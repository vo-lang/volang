# VoGUI - Declarative UI Library for Vo

VoGUI is a declarative UI library that runs on top of `vo-web`. It provides a simple, guided programming model for building web applications in Vo.

## Architecture

```
┌─────────────────────────────────────────┐
│  User Vo Code (State + View + Actions)  │
└─────────────────────────────────────────┘
                    │
┌─────────────────────────────────────────┐
│  VoGUI Library (libs/vogui)             │
│  - Node construction                    │
│  - Event binding                        │
│  - Render loop                          │
└─────────────────────────────────────────┘
                    │
┌─────────────────────────────────────────┐
│  vo-web WASM Runtime                    │
│  - Vo VM in browser                     │
│  - JS interop via extern                │
└─────────────────────────────────────────┘
                    │
┌─────────────────────────────────────────┐
│  Svelte Rendering Layer                 │
│  - Virtual DOM diffing                  │
│  - DOM updates                          │
│  - Event dispatching                    │
└─────────────────────────────────────────┘
```

## Programming Model

VoGUI follows a simple **State → View → Action** pattern:

| Concept | Description |
|---------|-------------|
| **State** | User-defined struct, single source of truth |
| **View** | Pure function: `state → Node tree` |
| **Action** | Functions that mutate state: `func(s *State, ...)` |

### Design Principles

1. **Guidance over enforcement** - API shape guides correct usage
2. **View receives value** - `state any` (read-only by convention)
3. **Action receives pointer** - `func(s *State)` (explicit mutation)
4. **No hidden state** - All state in user's struct

## API Reference

### Core Types

```go
package gui

// App defines the application structure
type App struct {
    Init func() any                     // Returns initial state
    View func(state any, ctx Context) Node  // Renders UI
}

// Context provides event binding capabilities
type Context struct { ... }

// Node represents a UI element (opaque)
type Node any

// Handler represents an event handler (opaque)
type Handler any
```

### Event Binding

```go
// On wraps an action for event handling
// action signature: func(s *State)
func On(action any) Handler

// OnValue wraps an action that receives input value
// action signature: func(s *State, value string)
func OnValue(action any) Handler

// OnIndex wraps an action that receives loop index
// action signature: func(s *State, index int)
func OnIndex(action any) Handler
```

### Layout Components

```go
// Column arranges children vertically
func Column(children ...Node) Node

// Row arranges children horizontally
func Row(children ...Node) Node

// Stack overlays children
func Stack(children ...Node) Node

// Scroll creates a scrollable container
func Scroll(children ...Node) Node

// Spacer fills available space
func Spacer() Node
```

### Display Components

```go
// Text displays text content
// content items are converted to string and concatenated
func Text(content ...any) Node

// Image displays an image
func Image(src string) Node

// Divider renders a horizontal line
func Divider() Node
```

### Input Components

```go
// Button renders a clickable button
func Button(text string, onClick Handler) Node

// Input renders a text input field
func Input(value string, onChange Handler) Node

// Checkbox renders a checkbox
func Checkbox(checked bool, onChange Handler) Node

// Select renders a dropdown
func Select(value string, options []string, onChange Handler) Node
```

### Control Flow

```go
// If conditionally renders a node
func If(cond bool, then Node) Node

// IfElse conditionally renders one of two nodes
func IfElse(cond bool, then Node, els Node) Node

// For renders a list of items
// items: slice of any type
// render: func(item T) Node  or  func(item T, index int) Node
func For(items any, render any) Node
```

### Styling

```go
// Style applies CSS-like properties to a node
func Style(node Node, props map[string]any) Node

// Common style helpers
func Padding(node Node, value int) Node
func Margin(node Node, value int) Node
func Width(node Node, value any) Node   // int (px) or string ("100%")
func Height(node Node, value any) Node
func Background(node Node, color string) Node
```

### App Lifecycle

```go
// Run starts the application
func Run(app App)
```

## Example

```go
package main

import "gui"

// 1. Define State
type State struct {
    Count int
    Text  string
    Items []string
}

// 2. Define View
func view(state any, ctx gui.Context) gui.Node {
    s := state.(State)
    
    return gui.Column(
        gui.Text("Counter: ", s.Count),
        
        gui.Row(
            gui.Button("-", gui.On(decrement)),
            gui.Button("+", gui.On(increment)),
        ),
        
        gui.Input(s.Text, gui.OnValue(setText)),
        gui.Button("Add Item", gui.On(addItem)),
        
        gui.If(len(s.Items) > 0,
            gui.Column(
                gui.Text("Items:"),
                gui.For(s.Items, func(item string, i int) gui.Node {
                    return gui.Row(
                        gui.Text(i+1, ". ", item),
                        gui.Button("×", gui.OnIndex(removeItem)),
                    )
                }),
            ),
        ),
    )
}

// 3. Define Actions
func increment(s *State) {
    s.Count++
}

func decrement(s *State) {
    s.Count--
}

func setText(s *State, value string) {
    s.Text = value
}

func addItem(s *State) {
    if s.Text != "" {
        s.Items = append(s.Items, s.Text)
        s.Text = ""
    }
}

func removeItem(s *State, index int) {
    s.Items = append(s.Items[:index], s.Items[index+1:]...)
}

// 4. Run App
func main() {
    gui.Run(gui.App{
        Init: func() any {
            return State{Count: 0, Items: []string{}}
        },
        View: view,
    })
}
```

## Implementation Notes

### Runtime Flow

1. `gui.Run(app)` initializes state via `app.Init()`
2. Calls `app.View(state, ctx)` to build Node tree
3. Serializes Node tree to JSON, sends to Svelte layer
4. Svelte renders DOM
5. User interaction triggers event → JS calls back to Vo
6. VoGUI invokes registered action with state pointer
7. After action completes, re-render View
8. Diff and update DOM

### Event Handling

```
User Click → Svelte event → JS bridge → vo-web extern
    → VoGUI looks up Handler → reflect-call action(state, args)
    → state mutated → re-render
```

### Node Serialization

Nodes are serialized as JSON for Svelte:

```json
{
  "type": "Column",
  "children": [
    {"type": "Text", "content": "Count: 5"},
    {"type": "Button", "text": "+", "onClick": "handler_1"}
  ]
}
```

## File Structure

```
libs/vogui/
├── README.md           # This file
├── gui.vo              # Vo API definitions
├── rust/               # Native implementation
│   ├── Cargo.toml
│   └── src/
│       └── lib.rs      # Runtime + extern functions
├── svelte/             # Svelte rendering layer
│   ├── package.json
│   ├── src/
│   │   ├── VoApp.svelte
│   │   └── components/
│   └── vite.config.js
└── examples/
    ├── counter.vo
    ├── todo.vo
    └── form.vo
```

## Dependencies

- `vo-web`: WASM runtime for Vo in browser
- Svelte 5: Rendering engine
- Vite: Build tool for Svelte layer

## Future Considerations

1. **Component System** - User-defined reusable components
2. **Routing** - SPA navigation
3. **Async Actions** - HTTP requests, timers
4. **Animations** - Transition effects
5. **Themes** - Styling system
