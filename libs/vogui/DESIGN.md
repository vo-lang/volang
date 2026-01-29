# VoGUI Design Document

VoGUI is a declarative UI library for building web applications in Vo.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      User Vo Code                           │
│            State struct + view() + action functions         │
└─────────────────────────────────────────────────────────────┘
                            │
┌─────────────────────────────────────────────────────────────┐
│                    vogui (Vo Library)                       │
│   Components │ Styling │ Events │ Router │ State Mgmt       │
│              Handler Registry │ Event Channel               │
│                                                             │
│   var eventChan = make(chan Event, 100)                     │
│   func Run(app App) {                                       │
│       state = app.Init(); render()                          │
│       registerEventChan(eventChan)  // extern               │
│       for event := range eventChan { ... }                  │
│   }                                                         │
└─────────────────────────────────────────────────────────────┘
                            │ JSON Protocol + extern
┌─────────────────────────────────────────────────────────────┐
│                 vo-web (Rust/WASM)                          │
│   Global State: VM + eventChan GcRef + fiber ID             │
│                                                             │
│   JS API:                                                   │
│   - initGuiApp(source) → initial render JSON                │
│   - handleGuiEvent(id, payload) → new render JSON           │
│                                                             │
│   Extern impl:                                              │
│   - gui_registerEventChan: save channel ref                 │
│   - gui_emitRender: output JSON via stdout                  │
└─────────────────────────────────────────────────────────────┘
                            │
┌─────────────────────────────────────────────────────────────┐
│                  Svelte Rendering Layer                     │
│       DOM Render │ Virtual DOM Diff │ Event Capture         │
└─────────────────────────────────────────────────────────────┘
```

**Design Principle**: All application logic (state management, handler dispatch) lives in `vogui` (Vo). The `vo-web` layer provides:
1. JS API for initialization and event dispatch
2. Extern function implementations for channel registration and render output
3. Direct channel send to wake blocked fibers on events

## Programming Model

**State → View → Action**

| Concept | Description |
|---------|-------------|
| **State** | User-defined struct, single source of truth |
| **View** | Pure function: `func(state any) Node` |
| **Action** | Mutation function: `func(s *State, ...)` |

```go
type State struct { Count int }

func view(state any) gui.Node {
    s := state.(*State)
    return gui.Column(
        gui.Text("Count: ", s.Count),
        gui.Button("+1", gui.On(increment)),
    )
}

func increment(s *State) { s.Count++ }

func main() {
    gui.Run(gui.App{
        Init: func() any { return &State{} },
        View: view,
    })
}
```

**Note**: `Init` must return a pointer (`&State{}`). This allows actions to mutate the state.

---

## Communication Protocol

### Vo → JS

```json
// Render
{"type": "render", "tree": Node, "handlers": N}

// Navigation
{"type": "navigate", "path": "/foo"}

// HTTP Request
{"type": "http", "id": 1, "method": "GET", "url": "..."}

// Timer
{"type": "setTimeout", "id": 2, "ms": 1000}
{"type": "setInterval", "id": 3, "ms": 500}
{"type": "clearInterval", "id": 3}
```

### JS → Vo

```json
// User Event (value)
{"type": "event", "handler": 3, "payload": {"value": "hello"}}

// User Event (checked)
{"type": "event", "handler": 4, "payload": {"checked": true}}

// User Event (slider)
{"type": "event", "handler": 5, "payload": {"value": 42}}

// User Event (files)
{"type": "event", "handler": 6, "payload": {"files": [{"name": "a.txt", "size": 123, "type": "text/plain"}]}}

// HTTP Response
{"type": "http", "id": 1, "status": 200, "body": "..."}

// Timer Tick
{"type": "timer", "id": 2}
```

### Handler Registration

Handlers are registered at render time. Each call to `On*()` returns a `Handler` with a unique ID.
The runtime maintains a `handlers` map that maps ID → action function.

- `On(action)`: Registers action, returns `Handler{ID: N, Type: "click"}`
- `OnInt(action, val)`: Pre-binds integer `val` into `Handler.IntVal`, action receives it as parameter
- `OnIntValue(action, val)`: Pre-binds `val`, action also receives input value from payload
- `OnIntChecked(action, val)`: Pre-binds `val`, action also receives checked bool from payload
- When JS dispatches event, Vo runtime looks up `handlers[id]` and invokes the action

**Handler Lifecycle**: Handlers are rebuilt on every render. Before each `view()` call, the handler registry is cleared and IDs restart from 0. This ensures no memory leaks and predictable behavior.

### Event Loop

The event loop uses standard Go/Vo concurrency: a goroutine blocked on channel receive.

```
┌──────────────────────────────────────────────────────────────┐
│                        JS Runtime                            │
│                                                              │
│  1. initGuiApp(source)                                       │
│     └─> compile, vm.run() until fiber blocks on eventChan   │
│     └─> return initial render JSON                          │
│                                                              │
│  2. User clicks button                                       │
│     └─> handleGuiEvent(handlerId, payload)                  │
│         └─> channel.try_send(Event{...})                    │
│         └─> scheduler.wake_fiber(gui_fiber_id)              │
│         └─> vm.run() until fiber blocks again               │
│         └─> return new render JSON                          │
│                                                              │
│  3. Repeat step 2 for each event                            │
└──────────────────────────────────────────────────────────────┘

┌──────────────────────────────────────────────────────────────┐
│                   vogui (Vo) - Event Loop                    │
│                                                              │
│  func Run(app App) {                                         │
│      currentState = app.Init()                               │
│      render()                                                │
│      registerEventChan(eventChan)  // extern: save chan ref  │
│                                                              │
│      for event := range eventChan {  // blocks here ──────┐ │
│          handler := handlers[event.HandlerID]              │ │
│          invokeHandler(handler, event.Payload)             │ │
│          render()                   // emits new JSON      │ │
│      }  // <── vm.run() returns when blocked ──────────────┘ │
│  }                                                           │
└──────────────────────────────────────────────────────────────┘
```

**Key Insight**: When all fibers are blocked (waiting on channel), `vm.run()` returns control to JS. JS can then send events to the channel and resume execution.

---

## Core Types

```go
// Node represents a UI element
type Node struct {
    Type     string
    Props    map[string]any
    Children []Node
    Styles   map[string]any
}

// Handler represents an event binding
type Handler struct {
    ID    int
    Type  string  // Event type that determines payload format:
                  //   "click"      - no payload
                  //   "value"      - {value: string}
                  //   "checked"    - {checked: bool}
                  //   "int"        - uses IntVal field (no payload)
                  //   "intValue"   - uses IntVal + {value: string}
                  //   "intChecked" - uses IntVal + {checked: bool}
                  //   "key"        - {key: string}
                  //   "submit"     - no payload
                  //   "select"     - {value: string}
                  //   "slider"     - {value: int}
                  //   "files"      - {files: []FileInfo}
    IntVal int    // Pre-bound integer value for OnInt, OnIntValue, OnIntChecked
}

// App defines the application
type App struct {
    Init   func() any
    View   func(state any) Node
    Routes []Route  // optional, for SPA routing
}

// Route defines a page route
type Route struct {
    Path string
    View func(state any, params map[string]string) Node
}
```

---

## Styling System

All styling is done through Node methods (chainable):

```go
Button("OK", onClick).P(16).Bg("#007bff").Rounded(4)

// Complex styles
Button("OK", onClick).Style(map[string]any{
    "boxShadow": "0 4px 6px rgba(0,0,0,0.1)",
})
```

### Style Methods

| Method | CSS Property | Example |
|--------|-------------|---------|
| `W(v any)` | width | `.W(100)` `.W("50%")` |
| `H(v any)` | height | `.H(200)` |
| `P(v int)` | padding | `.P(16)` |
| `Px(v int)` | padding-left, padding-right | `.Px(8)` |
| `Py(v int)` | padding-top, padding-bottom | `.Py(12)` |
| `M(v int)` | margin | `.M(8)` |
| `Mx(v int)` | margin-left, margin-right | `.Mx(4)` |
| `My(v int)` | margin-top, margin-bottom | `.My(8)` |
| `Bg(c string)` | background | `.Bg("#f0f0f0")` |
| `Fg(c string)` | color | `.Fg("#333")` |
| `Flex(v int)` | flex | `.Flex(1)` |
| `Gap(v int)` | gap | `.Gap(8)` |
| `Rounded(v int)` | border-radius | `.Rounded(4)` |
| `Border(v string)` | border | `.Border("1px solid #ccc")` |
| `Shadow(v string)` | box-shadow | `.Shadow("0 2px 4px rgba(0,0,0,0.1)")` |
| `Opacity(v float64)` | opacity | `.Opacity(0.5)` |
| `Font(v int)` | font-size | `.Font(14)` |
| `Bold()` | font-weight: bold | `.Bold()` |
| `Italic()` | font-style: italic | `.Italic()` |
| `Center()` | text-align: center | `.Center()` |
| `Cursor(v string)` | cursor | `.Cursor("pointer")` |
| `Overflow(v string)` | overflow | `.Overflow("hidden")` |
| `Style(m map[string]any)` | any CSS | `.Style(map[string]any{...})` |

---

## Event Handlers

| Function | Action Signature | Use Case |
|----------|-----------------|----------|
| `On(action)` | `func(s *State)` | Button click |
| `OnValue(action)` | `func(s *State, value string)` | Input, TextArea |
| `OnChecked(action)` | `func(s *State, checked bool)` | Checkbox, Switch |
| `OnInt(action, val)` | `func(s *State, val int)` | Pre-bound int (list index, enum) |
| `OnIntValue(action, val)` | `func(s *State, val int, value string)` | Edit list item (pre-bound int + input value) |
| `OnIntChecked(action, val)` | `func(s *State, val int, checked bool)` | Toggle list item (pre-bound int + checked) |
| `OnKey(action)` | `func(s *State, key string)` | Keyboard event |
| `OnSubmit(action)` | `func(s *State)` | Form submit |
| `OnSelect(action)` | `func(s *State, value string)` | Select, Radio |
| `OnSlider(action)` | `func(s *State, value int)` | Slider |
| `OnFiles(action)` | `func(s *State, files []FileInfo)` | File picker |

---

## Components

### Layout

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Column` | `(children ...Node) Node` | Vertical flex |
| `Row` | `(children ...Node) Node` | Horizontal flex |
| `Center` | `(child Node) Node` | Center content |
| `Stack` | `(children ...Node) Node` | Z-axis stacking |
| `Scroll` | `(child Node) Node` | Scrollable container |
| `Wrap` | `(children ...Node) Node` | Flex wrap |
| `Grid` | `(cols int, children ...Node) Node` | CSS Grid |
| `GridItem` | `(span int, child Node) Node` | Grid column span |
| `Spacer` | `() Node` | Flexible space |
| `Divider` | `() Node` | Horizontal line |
| `Fragment` | `(children ...Node) Node` | No wrapper |

### Text

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Text` | `(content ...any) Node` | Inline text |
| `H1` - `H6` | `(text string) Node` | Headings |
| `P` | `(text string) Node` | Paragraph |
| `Code` | `(code string) Node` | Inline code |
| `Pre` | `(code string) Node` | Code block |
| `Strong` | `(text string) Node` | Bold |
| `Em` | `(text string) Node` | Italic |
| `Link` | `(href string, text string) Node` | Hyperlink |

### Media

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Image` | `(src string) Node` | Image |
| `Icon` | `(name string) Node` | Lucide icon |
| `Avatar` | `(src string) Node` | Avatar image |
| `Video` | `(src string) Node` | Video |

### Display

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Badge` | `(text string) Node` | Badge |
| `Tag` | `(text string) Node` | Tag |
| `Progress` | `(value float64) Node` | Progress bar (0-1) |
| `Spinner` | `() Node` | Loading spinner |
| `Empty` | `(message string) Node` | Empty state |
| `Alert` | `(kind string, message string) Node` | Alert (info/success/warning/error) |

### Input

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Button` | `(text string, onClick Handler) Node` | Button |
| `IconButton` | `(icon string, onClick Handler) Node` | Icon button |
| `Input` | `(value string, onChange Handler) Node` | Text input |
| `Password` | `(value string, onChange Handler) Node` | Password input |
| `TextArea` | `(value string, onChange Handler) Node` | Multiline text |
| `Checkbox` | `(checked bool, onChange Handler) Node` | Checkbox |
| `CheckboxLabel` | `(label string, checked bool, onChange Handler) Node` | Labeled checkbox |
| `Radio` | `(value, group, selected string, onChange Handler) Node` | Radio button |
| `Switch` | `(on bool, onChange Handler) Node` | Toggle switch |
| `Select` | `(value string, options []string, onChange Handler) Node` | Dropdown |
| `SelectLabeled` | `(value string, options []SelectOption, onChange Handler) Node` | Labeled dropdown |
| `Slider` | `(value, min, max int, onChange Handler) Node` | Slider |
| `NumberInput` | `(value int, onChange Handler) Node` | Number input |
| `DateInput` | `(value string, onChange Handler) Node` | Date picker |
| `TimeInput` | `(value string, onChange Handler) Node` | Time picker |
| `ColorInput` | `(value string, onChange Handler) Node` | Color picker |
| `FileInput` | `(accept string, onFiles Handler) Node` | File picker |
| `SearchInput` | `(value string, onChange, onSubmit Handler) Node` | Search box |

### Container

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Card` | `(children ...Node) Node` | Card |
| `CardHeader` | `(title string) Node` | Card header |
| `CardBody` | `(children ...Node) Node` | Card body |
| `CardFooter` | `(children ...Node) Node` | Card footer |
| `Panel` | `(title string, children ...Node) Node` | Panel with title |
| `Accordion` | `(items []AccordionItem) Node` | Collapsible panels |
| `Tabs` | `(active int, tabs []TabItem, onChange Handler) Node` | Tab panels |

### Overlay

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Modal` | `(open bool, onClose Handler, children ...Node) Node` | Modal dialog |
| `ModalHeader` | `(title string) Node` | Modal header |
| `ModalBody` | `(children ...Node) Node` | Modal body |
| `ModalFooter` | `(children ...Node) Node` | Modal footer |
| `Drawer` | `(open bool, side string, onClose Handler, children ...Node) Node` | Side drawer |
| `Tooltip` | `(content string, child Node) Node` | Tooltip |
| `Popover` | `(content Node, child Node) Node` | Popover |
| `Dropdown` | `(items []DropdownItem, child Node) Node` | Dropdown menu |
| `ContextMenu` | `(items []MenuItem, child Node) Node` | Right-click menu |

### Form

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Form` | `(onSubmit Handler, children ...Node) Node` | Form |
| `FormField` | `(label string, child Node) Node` | Form field |
| `FormError` | `(message string) Node` | Error message |
| `FormHelp` | `(message string) Node` | Help text |
| `FormSection` | `(title string, children ...Node) Node` | Form section |

### List & Table

| Component | Signature | Description |
|-----------|-----------|-------------|
| `List` | `(children ...Node) Node` | List container |
| `ListItem` | `(children ...Node) Node` | List item |
| `OrderedList` | `(children ...Node) Node` | Numbered list |
| `Table` | `(children ...Node) Node` | Table |
| `TableHead` | `(children ...Node) Node` | Table head |
| `TableBody` | `(children ...Node) Node` | Table body |
| `TableRow` | `(children ...Node) Node` | Table row |
| `TableCell` | `(content any) Node` | Table cell |
| `TableHeaderCell` | `(content string) Node` | Header cell |

### Navigation

| Component | Signature | Description |
|-----------|-----------|-------------|
| `Nav` | `(children ...Node) Node` | Navigation bar |
| `NavItem` | `(text string, active bool, onClick Handler) Node` | Nav item |
| `NavLink` | `(to string, text string) Node` | Nav link (router) |
| `Breadcrumb` | `(items []BreadcrumbItem) Node` | Breadcrumb |
| `Pagination` | `(current, total int, onChange Handler) Node` | Pagination |
| `Steps` | `(current int, items []string) Node` | Step indicator |

### Control Flow

| Component | Signature | Description |
|-----------|-----------|-------------|
| `If` | `(cond bool, then Node) Node` | Conditional |
| `IfElse` | `(cond bool, then, els Node) Node` | If-else |
| `Show` | `(visible bool, child Node) Node` | Show/hide (keep DOM) |
| `ForEach` | `(items any, render func(item any, i int) Node) Node` | List render (accepts any slice) |
| `ForEachKeyed` | `(items any, keyFn func(item any) string, render func(item any, i int) Node) Node` | Keyed list |
| `ForRange` | `(start, end int, render func(int) Node) Node` | Range render |

**Note on Keys**: `ForEach` uses array index as implicit key. Use `ForEachKeyed` when items may be reordered or filtered, otherwise DOM state (input focus, animations) may be incorrect.
`ForEach` expects a slice; if `items` is not a slice, the runtime fails.

**Rendering**: Keys are passed to Svelte via `props.key`. Svelte performs virtual DOM diff using these keys for efficient updates.

---

## Helper Types

```go
type SelectOption struct {
    Value string
    Label string
}

type AccordionItem struct {
    Title   string
    Content Node
    Open    bool
}

type TabItem struct {
    Label   string
    Content Node
}

type DropdownItem struct {
    Label   string
    OnClick Handler
    Icon    string
}

type MenuItem struct {
    Label    string
    OnClick  Handler
    Icon     string
    Disabled bool
    Children []MenuItem
}

type BreadcrumbItem struct {
    Label string
    Href  string
}

type FileInfo struct {
    Name string
    Size int
    Type string
}
```

---

## Async API

### HTTP

```go
func Fetch(url string, callback func(s *State, resp Response))
func Post(url string, body string, callback func(s *State, resp Response))

type Response struct {
    Status int
    Body   string
    Error  string
}
```

### Timers

```go
func SetTimeout(ms int, callback func(s *State))
func SetInterval(ms int, callback func(s *State)) int
func ClearInterval(id int)
```

**Note**: Async callbacks receive `*State` directly, same as action functions. The runtime knows the State type and passes the pointer.

```go
Fetch("/api/data", func(s *State, resp Response) {
    s.Data = resp.Body
})
```

---

## Routing

```go
func main() {
    gui.Run(gui.App{
        Init: func() any { return State{} },
        Routes: []gui.Route{
            {Path: "/", View: homePage},
            {Path: "/users/:id", View: userPage},
            {Path: "/about", View: aboutPage},
        },
    })
}

func homePage(state any, params map[string]string) gui.Node {
    return gui.Column(
        gui.H1("Home"),
        gui.NavLink("/about", "About"),
    )
}

func userPage(state any, params map[string]string) gui.Node {
    userId := params["id"]
    return gui.Text("User: ", userId)
}

// Programmatic navigation
gui.Navigate("/users/123")
```

---

## Example: Todo App

```go
package main

import "gui"

type State struct {
    Items  []Todo
    Input  string
    Filter string
    NextID int
}

type Todo struct {
    ID   int
    Text string
    Done bool
}

func view(state any) gui.Node {
    s := state.(State)

    return gui.Column(
        gui.H1("Todo App"),

        // Input row
        gui.Row(
            gui.Input(s.Input, gui.OnValue(setInput)).Flex(1),
            gui.Button("Add", gui.On(addTodo)),
        ).Gap(8),

        // Filter buttons
        gui.Row(
            filterBtn(s, "all", "All"),
            filterBtn(s, "active", "Active"),
            filterBtn(s, "done", "Done"),
        ).Gap(4),

        // Todo list
        gui.ForEach(filtered(s), func(item any, i int) gui.Node {
            todo := item.(Todo)
            return gui.Row(
                gui.Checkbox(todo.Done, gui.OnIntChecked(setTodoDone, todo.ID)),
                gui.Text(todo.Text).Flex(1).Fg(doneColor(todo.Done)),
                gui.IconButton("x", gui.OnInt(removeTodo, todo.ID)),
            ).Gap(8)
        }),

        // Stats
        gui.Text("Total: ", len(s.Items), " | Active: ", countActive(s)).
            Fg("#888").Font(12),
    ).P(16).Gap(12)
}

func filterBtn(s State, value string, label string) gui.Node {
    active := s.Filter == value
    return gui.Button(label, gui.OnInt(setFilter, filterIndex(value))).
        Bg(ifStr(active, "#007bff", "#e0e0e0")).
        Fg(ifStr(active, "#fff", "#333"))
}

func filterIndex(value string) int {
    switch value {
    case "all":    return 0
    case "active": return 1
    case "done":   return 2
    }
    return 0
}

func doneColor(done bool) string {
    if done {
        return "#999"
    }
    return "#000"
}

func ifStr(cond bool, t string, f string) string {
    if cond {
        return t
    }
    return f
}

// Actions
func setInput(s *State, value string) { s.Input = value }

func setFilter(s *State, i int) {
    filters := []string{"all", "active", "done"}
    s.Filter = filters[i]
}

func addTodo(s *State) {
    if s.Input != "" {
        s.Items = append(s.Items, Todo{ID: s.NextID, Text: s.Input})
        s.NextID++
        s.Input = ""
    }
}

func setTodoDone(s *State, id int, done bool) {
    for i := range s.Items {
        if s.Items[i].ID == id {
            s.Items[i].Done = done
            return
        }
    }
}

func removeTodo(s *State, id int) {
    for i := range s.Items {
        if s.Items[i].ID == id {
            s.Items = append(s.Items[:i], s.Items[i+1:]...)
            return
        }
    }
}

func filtered(s State) []any {
    var result []any
    for _, item := range s.Items {
        if s.Filter == "all" ||
            (s.Filter == "active" && !item.Done) ||
            (s.Filter == "done" && item.Done) {
            result = append(result, item)
        }
    }
    return result
}

func countActive(s State) int {
    n := 0
    for _, item := range s.Items {
        if !item.Done {
            n++
        }
    }
    return n
}

func main() {
    gui.Run(gui.App{
        Init: func() any { return State{Filter: "all"} },
        View: view,
    })
}
```

---

## Implementation Phases

| Phase | Content | Priority |
|-------|---------|----------|
| P1 | Minimal Counter demo: `Text`, `Button`, `Column`, `Row`, full event loop | Core |
| P2 | Complete component library | High |
| P3 | Style system with chainable methods | High |
| P4 | SPA routing | Medium |
| P5 | Async (HTTP, timers) | Medium |
| P6 | Advanced (themes, animations) | Low |

### P1 Scope (Minimal Viable Demo)

Goal: Verify the complete chain works end-to-end.

**Components**: `Text`, `Button`, `Column`, `Row`  
**Events**: `On(action)` only  
**Flow**: Vo → JSON tree → Svelte render → Click event → Vo action → State mutation → Re-render

```go
// P1 target: working counter
type State struct { Count int }

func view(state any) gui.Node {
    s := state.(*State)
    return gui.Column(
        gui.Text("Count: ", s.Count),
        gui.Button("+1", gui.On(increment)),
    )
}

func increment(s *State) { s.Count++ }

func main() {
    gui.Run(gui.App{
        Init: func() any { return &State{} },
        View: view,
    })
}
```

---

## Notes

### Error Handling

**View panic**: If `view()` panics, the runtime catches it and displays an error overlay in development mode. In production, it shows a generic error message and logs the stack trace.

**Action panic**: If an action function panics, the same error handling applies. State is not modified on panic.

**HTTP errors**: `Response.Error` contains the error message. Check `resp.Status` for HTTP status codes, or `resp.Error != ""` for network/timeout errors.

### DevTools

Future: State inspector and time-travel debugging for development builds.

### Component-Local State

All state lives in the single global `State` struct. For UI-only state (accordion open/close, modal visibility), this can be verbose. Two patterns:

1. **Explicit in State**: Add fields like `AccordionOpen []bool` to State
2. **Derived from data**: Use item properties (e.g., `todo.Expanded`) instead of separate UI state

True component-local state is not supported to keep the model simple and predictable.

---

## File Structure

```
libs/vogui/
├── DESIGN.md           # This document
├── README.md           # User-facing docs
├── gui.vo              # Vo API (components, Run, extern declarations)
└── examples/
    ├── counter.vo
    ├── todo.vo
    └── form.vo

lang/crates/vo-web/src/
├── lib.rs              # Existing WASM API
└── gui.rs              # GUI extern impl + initGuiApp/handleGuiEvent

playground/
├── src/
│   ├── components/
│   │   └── GuiPreview.svelte   # Render JSON tree as DOM
│   ├── lib/
│   │   └── vogui-runtime.ts    # Optional: helper for JSON → Svelte
│   └── wasm/
│       └── vo.ts               # WASM bindings (add GUI API)
```

**Note**: VoGUI does NOT have a native extension. The extern functions (`registerEventChan`, `emitRender`) are implemented directly in `vo-web` because:
1. They are WASM-specific (need access to vo-web's global VM state)
2. They use channel manipulation which requires direct access to VM internals
3. No separate `.so/.dylib` is needed for the WASM target
