# VoGUI v3: Performance, Components, Canvas & Style System

> Architectural evolution beyond v2. Addresses performance ceiling, scalability limits,
> game development support, and style ergonomics.
> Date: 2026-02-27

---

## Table of Contents

1. [Motivation](#1-motivation)
2. [Binary Render Protocol](#2-binary-render-protocol)
3. [Canvas 2D API & Game Loop](#3-canvas-2d-api--game-loop)
4. [Style Combinator System](#4-style-combinator-system)
5. [Component Model](#5-component-model)
6. [Implementation Order & Dependencies](#6-implementation-order--dependencies)
7. [Revision of v2 "Explicitly Don't Do" Decisions](#7-revision-of-v2-decisions)

---

## 1. Motivation

### v2 achievements

VoGUI v2 (2026-02-25 design) successfully delivered:
- Two-layer state model (Vo app state + JS UI behavior state)
- Managed components (Dialog, Dropdown, Tabs, etc.)
- Theme system with CSS custom properties
- Event modifiers (Prevent, Stop, Debounce, Throttle)
- Transitions and TransitionGroups
- Refs, Portals, UnsafeHTML
- Utility CSS classes

### Remaining gaps for complex SPAs & mini-games

1. **Performance ceiling**: Every event triggers full Node tree rebuild + `json.Marshal`
   of the entire tree. For a 2000-node SPA, this is ~10ms serialization + ~8ms morphdom
   = 18ms per interaction. Perceptible lag.

2. **No Canvas 2D API**: `canvas.vo` creates a `<canvas>` element but provides zero
   drawing commands. Tetris example uses 200+ Div elements as grid cells instead.
   No `requestAnimationFrame` support.

3. **Style ergonomics**: Inline style chains (`.P(20).Bg("#fff").Rounded(8)`) have no
   reuse mechanism, no pseudo-class support (`:hover`, `:focus`), and every style
   becomes a runtime map insertion.

4. **State scalability**: Single global State struct doesn't scale. workspace.vo has
   28 fields. Real SPAs would have hundreds. No component-local state, no lifecycle.

### Quantified performance analysis

| Scenario            | Nodes | JSON size (est) | Marshal (est) | morphdom (est) | Total  |
|---------------------|-------|-----------------|---------------|----------------|--------|
| Counter             | ~10   | <1KB            | <0.1ms        | <0.1ms         | <1ms   |
| Todo (50 items)     | ~200  | ~20KB           | ~1ms          | ~1ms           | ~2ms   |
| Workspace           | ~500  | ~50KB           | ~3ms          | ~2ms           | ~5ms   |
| Complex SPA         | ~2000 | ~200KB          | ~10ms         | ~8ms           | ~18ms  |
| Tetris (per frame)  | ~250  | ~25KB           | ~1.5ms        | ~1ms           | ~2.5ms |
| Game (60fps budget) | N/A   | N/A             | N/A           | N/A            | 16ms   |

With binary protocol: JSON→binary cuts serialization 5-10x. Combined with Component
memo, most interactions touch <10% of the tree.

---

## 2. Binary Render Protocol

### Problem

`json.Marshal(map[string]any{...})` is the single biggest bottleneck:
- Node.Type is a string (e.g., "vo-button") — 9 bytes + quotes + key
- Props is `map[string]any` — every key is a string, every value boxed
- Handler metadata is full JSON objects
- The entire tree is serialized even if only one button changed

### Design

Replace JSON with a compact binary encoding transmitted via SharedArrayBuffer (WASM)
or string (non-WASM fallback).

#### Node Type Encoding

Assign a `u16` enum to each node type:

```
0x0000 = Fragment
0x0001 = div
0x0002 = span
0x0003 = vo-text
0x0004 = vo-button
0x0005 = vo-input
0x0006 = vo-row
0x0007 = vo-column
...
0x00FF = Canvas
0x0100+ = custom/user types (string-encoded as fallback)
```

#### Binary Format (per node)

```
┌──────────────────────────────────────────────────────┐
│ u16: node_type                                        │
│ u16: child_count                                      │
│ u16: prop_count                                       │
│ [props...]                                            │
│   u16: prop_key_id (enum: style, class, ref, ...)    │
│   u8:  value_type (string=1, int=2, float=3, bool=4, │
│                     map=5, handler=6)                  │
│   [value bytes]                                       │
│ [children... (recursive)]                             │
└──────────────────────────────────────────────────────┘
```

#### Prop Key Encoding

Most-used prop keys get numeric IDs:

```
0x0001 = style
0x0002 = class
0x0003 = ref
0x0004 = key
0x0005 = variant
0x0006 = transition
0x0007 = text
0x0008 = value
0x0009 = placeholder
0x000A = disabled
0x000B = handler_1 (click)
0x000C = handler_2 (value)
...
0x00FF = attrs
0x0100+ = custom key (length-prefixed string)
```

#### Style Encoding

Instead of `map[string]any` for styles, use a flat list of `(style_prop_id, value)`:

```
style_prop_id: u8
  0x01 = width
  0x02 = height
  0x03 = padding
  0x04 = margin
  0x05 = background
  0x06 = color
  0x07 = fontSize
  0x08 = borderRadius
  0x09 = border
  0x0A = gap
  ...

value: type-prefixed
  int (px assumed): i32
  string: length-prefixed UTF-8
  float: f32
```

#### Estimated savings

| Component  | JSON      | Binary (est) | Reduction |
|------------|-----------|--------------|-----------|
| Node type  | ~15 bytes | 2 bytes      | 87%       |
| Style prop | ~25 bytes | 5 bytes      | 80%       |
| Handler ref| ~40 bytes | 6 bytes      | 85%       |
| Total tree | ~200KB    | ~25-30KB     | 85%       |

#### Implementation architecture

**Vo side** (`libs/vogui/encode.vo`):
- `encodeBinary(tree Node, handlers []Handler) []byte`
- Uses `bytes.Buffer` or pre-allocated `[]byte` buffer
- Writes nodes depth-first (pre-order traversal)

**Rust bridge** (`libs/vogui/rust/src/externs.rs`):
- `emitRender` accepts `[]byte` instead of string
- For WASM: write to SharedArrayBuffer, signal JS via postMessage
- For non-WASM: base64 encode or pass as byte array

**JS side** (`libs/vogui/js/src/decoder.ts`):
- New module: `decodeBinary(buffer: ArrayBuffer): RenderMessage`
- Reads the binary format, reconstructs `VoNode` tree
- Existing renderer consumes `VoNode` as before (no renderer changes)

#### Fallback

Keep JSON path available via build flag for debugging. Binary is default for production.

#### Migration

This is **backward compatible at the JS renderer level** — the decoder produces the
same `VoNode` / `RenderMessage` that JSON parsing produces. Only the transport changes.

---

## 3. Canvas 2D API & Game Loop

### Problem

Current `canvas.vo` creates a `<canvas>` element but provides no drawing API. The Tetris
example fakes a grid with 200+ `Div` nodes. This is:
- 200x more DOM nodes than needed
- No access to Canvas 2D primitives (lines, arcs, images, text)
- No `requestAnimationFrame` for smooth 60fps loops

### Design: Command Buffer Architecture

Vo collects drawing commands into a buffer, then flushes them to JS in a single batch.
This minimizes cross-boundary calls.

#### Vo API (`libs/vogui/canvas.vo`)

```go
// Canvas drawing context (obtained from a named canvas)
type CanvasCtx struct {
    commands []any  // internal command buffer
    name     string
}

// GetCanvas returns a drawing context for the named canvas element.
func GetCanvas(refName string) *CanvasCtx

// Drawing commands (append to buffer)
func (c *CanvasCtx) Clear()
func (c *CanvasCtx) SetFill(color string)
func (c *CanvasCtx) SetStroke(color string)
func (c *CanvasCtx) SetLineWidth(w float64)
func (c *CanvasCtx) SetFont(font string)
func (c *CanvasCtx) SetAlpha(a float64)

// Shapes
func (c *CanvasCtx) FillRect(x, y, w, h float64)
func (c *CanvasCtx) StrokeRect(x, y, w, h float64)
func (c *CanvasCtx) ClearRect(x, y, w, h float64)
func (c *CanvasCtx) FillCircle(cx, cy, r float64)
func (c *CanvasCtx) StrokeCircle(cx, cy, r float64)
func (c *CanvasCtx) FillRoundRect(x, y, w, h, r float64)

// Path
func (c *CanvasCtx) BeginPath()
func (c *CanvasCtx) MoveTo(x, y float64)
func (c *CanvasCtx) LineTo(x, y float64)
func (c *CanvasCtx) ArcTo(x1, y1, x2, y2, r float64)
func (c *CanvasCtx) ClosePath()
func (c *CanvasCtx) Fill()
func (c *CanvasCtx) Stroke()

// Text
func (c *CanvasCtx) FillText(text string, x, y float64)
func (c *CanvasCtx) StrokeText(text string, x, y float64)

// Images (preloaded via LoadImage)
func (c *CanvasCtx) DrawImage(imgID int, x, y float64)
func (c *CanvasCtx) DrawImageScaled(imgID int, x, y, w, h float64)
func (c *CanvasCtx) DrawImageSub(imgID int, sx, sy, sw, sh, dx, dy, dw, dh float64)

// Transform
func (c *CanvasCtx) Save()
func (c *CanvasCtx) Restore()
func (c *CanvasCtx) Translate(x, y float64)
func (c *CanvasCtx) Rotate(angle float64)
func (c *CanvasCtx) Scale(sx, sy float64)

// Flush sends all buffered commands to JS for execution.
func (c *CanvasCtx) Flush()
```

#### Command encoding

Each command is encoded as a compact binary message:

```
u8: command_id
  0x01 = clear
  0x02 = setFill (+ string color)
  0x03 = setStroke (+ string color)
  0x04 = setLineWidth (+ f32)
  0x05 = fillRect (+ 4x f32)
  0x06 = strokeRect (+ 4x f32)
  0x07 = fillCircle (+ 3x f32)
  0x08 = fillText (+ string + 2x f32)
  0x09 = drawImage (+ i32 imgID + 2x f32)
  0x0A = drawImageScaled (+ i32 imgID + 4x f32)
  0x0B = drawImageSub (+ i32 imgID + 8x f32)
  0x0C = save
  0x0D = restore
  0x0E = translate (+ 2x f32)
  0x0F = rotate (+ f32)
  0x10 = scale (+ 2x f32)
  0x11 = beginPath
  0x12 = moveTo (+ 2x f32)
  0x13 = lineTo (+ 2x f32)
  0x14 = closePath
  0x15 = fill
  0x16 = stroke
  0x17 = setFont (+ string)
  0x18 = setAlpha (+ f32)
  0x19 = clearRect (+ 4x f32)
  0x1A = strokeCircle (+ 3x f32)
  0x1B = fillRoundRect (+ 5x f32)
  0x1C = strokeText (+ string + 2x f32)
  0x1D = arcTo (+ 5x f32)
```

`Flush()` sends the entire command buffer as a single byte array to JS.

#### JS side (`libs/vogui/js/src/canvas.ts`)

```typescript
function executeCanvasCommands(
    canvas: HTMLCanvasElement,
    commands: ArrayBuffer
): void {
    const ctx = canvas.getContext('2d')!;
    const view = new DataView(commands);
    let offset = 0;
    while (offset < view.byteLength) {
        const cmd = view.getUint8(offset++);
        switch (cmd) {
            case 0x01: ctx.clearRect(0, 0, canvas.width, canvas.height); break;
            case 0x05: {
                const x = view.getFloat32(offset, true); offset += 4;
                const y = view.getFloat32(offset, true); offset += 4;
                const w = view.getFloat32(offset, true); offset += 4;
                const h = view.getFloat32(offset, true); offset += 4;
                ctx.fillRect(x, y, w, h);
                break;
            }
            // ... etc
        }
    }
}
```

#### requestAnimationFrame support

New Vo API:

```go
// RequestAnimationFrame schedules a callback for the next frame.
// The callback receives the application state.
// Returns a cancel ID.
func RequestAnimationFrame(handler func(state any)) int

// CancelAnimationFrame cancels a pending frame request.
func CancelAnimationFrame(id int)

// RunGameLoop starts a continuous game loop that calls handler every frame.
// dt is the delta time in milliseconds since the last frame.
// Returns a cancel ID.
func RunGameLoop(handler func(state any, dt float64)) int

// StopGameLoop stops a running game loop.
func StopGameLoop(id int)
```

#### Implementation:

- `RequestAnimationFrame` → Rust extern → JS `requestAnimationFrame`
- Callback fires as a special event (like timer events) with `eventIDAnimFrame`
- `RunGameLoop` is sugar: internally uses `requestAnimationFrame` in a loop,
  calculates dt, dispatches to Vo handler

#### Image loading

```go
// LoadImage loads an image from a URL and returns an image ID for canvas drawing.
func LoadImage(url string, onLoad func(state any, imgID int)) int
```

JS side loads the image, stores in a registry, fires callback with the assigned ID.
Canvas commands reference images by ID.

#### Example: Simple game

```go
type GameState struct {
    PlayerX float64
    PlayerY float64
    Score   int
    LoopID  int
    SpriteID int
}

func initGame() any {
    s := &GameState{PlayerX: 100, PlayerY: 200}
    s.SpriteID = vogui.LoadImage("/sprites/player.png",
        func(state any, imgID int) {
            state.(*GameState).SpriteID = imgID
        })
    s.LoopID = vogui.RunGameLoop(gameLoop)
    vogui.SetGlobalKeyHandler(handleInput)
    return s
}

func gameLoop(state any, dt float64) {
    s := state.(*GameState)
    // Update logic
    s.PlayerY += 0.1 * dt  // gravity

    // Draw
    c := vogui.GetCanvas("game")
    c.Clear()
    c.SetFill("#87CEEB")
    c.FillRect(0, 0, 800, 600)  // sky
    if s.SpriteID > 0 {
        c.DrawImage(s.SpriteID, s.PlayerX, s.PlayerY)
    }
    c.SetFont("20px monospace")
    c.SetFill("#000")
    c.FillText(fmt.Sprintf("Score: %d", s.Score), 10, 30)
    c.Flush()
}

func view(state any) vogui.Node {
    return vogui.Canvas(vogui.CanvasOpts{Width: 800, Height: 600}).Ref("game")
}
```

---

## 4. Style Combinator System

### Problem

Current inline styles:
- No reuse: `.P(16).Bg("#fff").Rounded(8)` repeated everywhere
- No pseudo-classes: `:hover`, `:focus`, `:active` impossible with inline styles
- Runtime overhead: each `.P(16)` inserts into `map[string]any` at runtime
- No media queries per-element

### Design: Style as a first-class value

```go
// Style is a reusable, composable style definition.
// Under the hood it compiles to a CSS class name.
type Style struct {
    id    string            // generated unique class name
    props map[string]any    // base CSS properties
    hover *Style            // :hover overrides
    focus *Style            // :focus overrides
    active *Style           // :active overrides
    media map[string]*Style // media query overrides
}
```

#### Creating styles

```go
var cardStyle = vogui.NewStyle().
    P(16).Rounded(8).
    Border("1px solid var(--vo-border)").
    Bg("var(--vo-surface)")

var hoverCard = vogui.NewStyle().
    P(16).Rounded(8).
    Border("1px solid var(--vo-border)").
    Bg("var(--vo-surface)").
    Hover(vogui.NewStyle().Shadow("0 4px 12px rgba(0,0,0,0.1)")).
    Focus(vogui.NewStyle().Border("2px solid var(--vo-primary)"))
```

#### Applying styles

```go
func productCard(p Product) vogui.Node {
    return vogui.Card(
        vogui.Text(p.Name),
        vogui.Text(p.Price),
    ).ApplyStyle(hoverCard)
}
```

#### Composing styles

```go
// Extend creates a new Style inheriting from parent
var dangerCard = cardStyle.Extend().
    Border("1px solid var(--vo-danger)").
    Hover(vogui.NewStyle().Bg("var(--vo-danger)").Fg("#fff"))
```

#### How it works

1. **First render**: When `ApplyStyle(s)` is called, the Style's `id` is generated
   (hash of props), and the CSS rule is included in the render output as a new
   `"styles"` field.

2. **JS side**: The decoder reads the `"styles"` array, injects any new CSS rules
   into a `<style>` tag. Each Style gets a class name like `.vo-s-a1b2c3`.

3. **Node**: `ApplyStyle` sets `Props["class"]` to the generated class name.
   No inline styles needed.

4. **Caching**: Once a Style's CSS is injected, subsequent renders skip re-injection.
   The JS side tracks which style IDs have been injected.

#### Style CSS generation example

```go
var btn = vogui.NewStyle().
    P(8).Px(16).Rounded(4).
    Bg("var(--vo-primary)").Fg("#fff").
    Cursor("pointer").
    Hover(vogui.NewStyle().Bg("var(--vo-primary-hover)")).
    Active(vogui.NewStyle().Bg("var(--vo-primary-active)"))
```

Generates:
```css
.vo-s-x7f3a2 {
    padding: 8px;
    padding-left: 16px;
    padding-right: 16px;
    border-radius: 4px;
    background: var(--vo-primary);
    color: #fff;
    cursor: pointer;
}
.vo-s-x7f3a2:hover {
    background: var(--vo-primary-hover);
}
.vo-s-x7f3a2:active {
    background: var(--vo-primary-active);
}
```

#### Responsive styles

```go
var layout = vogui.NewStyle().
    Style("display", "flex").
    Style("flexDirection", "column").
    Media("(min-width: 768px)", vogui.NewStyle().
        Style("flexDirection", "row"))
```

Generates:
```css
.vo-s-y8g4b3 { display: flex; flex-direction: column; }
@media (min-width: 768px) {
    .vo-s-y8g4b3 { flex-direction: row; }
}
```

#### Coexistence with existing system

- `.P(16)`, `.Bg(...)` etc. still work as inline styles (backward compatible)
- `.Class("...")` still works for external CSS classes
- `.ApplyStyle(s)` is the new recommended way for app-level styling
- Built-in component styles (vo-button, vo-input) remain in `styles.ts`
- Users can override built-in styles via Style combinators or CSS

---

## 5. Component Model

### Why this reverses v2's "Explicitly Don't Do" decision

v2 Section 10.2 says: "Component-Local State in Vo — Why not: This would require
either a component identity/lifecycle system (complex runtime) or Hooks-like API."

**This was correct for UI behavior state** — dropdown open/close, tooltip visibility,
etc. remain in JS (managed components). That decision stands.

**But it was wrong for application-level component state.** The problem:

1. workspace.vo: 28 fields, all flat. Real SPA: hundreds of fields.
2. Every handler receives the entire `*State`. A "delete note" handler can
   accidentally mutate task data. No encapsulation.
3. No lifecycle: timers/subscriptions started in handler X must be manually
   cleaned up. No mount/unmount hooks.
4. No memo: changing a counter re-renders the entire 2000-node tree.

**Managed components solve UI behavior state.
   Component Model solves application state scalability.**

These are orthogonal. Both are needed.

### Design Principles

1. **Components are structs with a View method** — no hooks, no magic
2. **Component state lives inside parent state** — no separate heap allocation
3. **Props are struct fields set by parent** — explicit, typed, no prop drilling
4. **Lifecycle is explicit** — Init() and Destroy() methods, not useEffect
5. **Memo is opt-in** — ShouldUpdate() method for skip optimization
6. **TEA preserved for simple apps** — Component Model is additive, not mandatory

### Component interface

```go
// A Component is any struct that implements View().
// Init() and Destroy() are optional lifecycle methods.
// ShouldUpdate() is optional for memo optimization.

// Required:
func (c *MyComponent) View() vogui.Node

// Optional:
func (c *MyComponent) Init()                          // called on first mount
func (c *MyComponent) Destroy()                       // called on unmount
func (c *MyComponent) ShouldUpdate(prev *MyComponent) bool  // memo
```

The framework detects these methods via `~>` dynamic dispatch. No interface
registration needed.

### Mount

```go
// Mount renders a Component, managing its lifecycle.
func Mount(component any) Node
```

`Mount` wraps the component pointer into a special Node:
```go
func Mount(component any) Node {
    return Node{
        Type: "__component",
        Props: map[string]any{
            "ptr": component,
        },
    }
}
```

During rendering, when the renderer encounters a `__component` node:
1. Check if this component was seen in the previous render (by pointer address)
2. If new: call `Init()` if present, then call `View()`
3. If existing: call `ShouldUpdate(prev)` if present. If false, reuse cached subtree
4. If removed (was in prev render, not in current): call `Destroy()` if present

### Handler scoping

Handlers inside a Component's `View()` receive the Component pointer, not the
global State:

```go
type Counter struct {
    Count int
}

func (c *Counter) View() vogui.Node {
    return vogui.Row(
        vogui.Button("-", vogui.On(func(c *Counter) { c.Count-- })),
        vogui.Text(fmt.Sprint(c.Count)),
        vogui.Button("+", vogui.On(func(c *Counter) { c.Count++ })),
    ).Gap(8)
}
```

**Implementation**: When `Mount(component)` is called, it sets a thread-local
"current component context". All `On(handler)` calls within `View()` capture
this context. When the handler is invoked, the framework passes the component
pointer instead of the global state.

### Parent-child communication

**Props (parent → child)**: Parent sets struct fields before Mount:

```go
type App struct {
    Tasks    []Task
    TaskList TaskList  // child component
}

func (a *App) View() vogui.Node {
    a.TaskList.Tasks = a.Tasks  // pass data down
    return vogui.Mount(&a.TaskList)
}
```

**Callbacks (child → parent)**: Use function fields:

```go
type TaskList struct {
    Tasks    []Task
    OnDelete func(id string)  // callback to parent
}

func (t *TaskList) View() vogui.Node {
    return vogui.ForEachKeyed(t.Tasks, keyFn, func(task Task) vogui.Node {
        return vogui.Row(
            vogui.Text(task.Title),
            vogui.Button("Delete", vogui.On(func(t *TaskList) {
                if t.OnDelete != nil {
                    t.OnDelete(task.ID)
                }
            })),
        )
    })
}
```

Parent wires the callback:
```go
func (a *App) View() vogui.Node {
    a.TaskList.Tasks = a.Tasks
    a.TaskList.OnDelete = func(id string) {
        // remove from a.Tasks
    }
    return vogui.Mount(&a.TaskList)
}
```

**Important**: The `OnDelete` callback captures `a` (the parent), so it can mutate
parent state. This is safe because Vo is single-threaded and handlers execute
sequentially.

### Memo (replacing P3 / incremental rendering)

```go
func (t *TaskList) ShouldUpdate(prev *TaskList) bool {
    return len(t.Tasks) != len(prev.Tasks) || t.Filter != prev.Filter
}
```

Framework behavior:
1. Before calling `View()`, framework clones the component (shallow copy)
2. Parent sets new props (e.g., `a.TaskList.Tasks = newTasks`)
3. Framework calls `ShouldUpdate(prevClone)` — if false, reuse cached Node subtree
4. Cached subtree is re-serialized but NOT rebuilt (fast)

For binary protocol: cached subtrees can use a `CACHED_SUBTREE` marker:
```
u16: 0xFFFF (cached subtree marker)
u32: subtree_id
```
JS side keeps the previous DOM subtree and skips morphdom for it.

**This is why P3 (incremental rendering) doesn't need to be a separate feature.**
It falls out naturally from Component memo.

### Lifecycle management

```go
type Chat struct {
    Messages []Message
    timerID  int  // internal, not a prop
}

func (c *Chat) Init() {
    // Called once when component first appears in the tree
    c.timerID = vogui.SetInterval(func(state any) {
        // poll for new messages
    }, 5000)
}

func (c *Chat) Destroy() {
    // Called when component is removed from the tree
    vogui.ClearInterval(c.timerID)
}
```

Framework tracks component presence via pointer identity across renders.

### Component tree tracking

The framework maintains a `map[uintptr]componentState`:

```go
type componentState struct {
    prevPtr    uintptr       // pointer address for identity
    prevCopy   any           // shallow copy for ShouldUpdate
    cachedTree Node          // cached View() output
    inited     bool          // has Init() been called
}
```

Each render:
1. Walk the new Node tree, collect all `__component` nodes
2. For each: check if `prevPtr` exists in the map
   - New: call Init(), call View(), store in map
   - Existing: call ShouldUpdate(), either call View() or reuse cache
3. After render: any entries in map NOT seen this render → call Destroy(), remove

### Full example

```go
type App struct {
    CurrentView string
    Tasks       []Task
    TaskPanel   TaskPanel
    ChatPanel   ChatPanel
    Settings    SettingsPanel
}

func (a *App) View() vogui.Node {
    a.TaskPanel.Tasks = a.Tasks
    a.TaskPanel.OnUpdate = func(tasks []Task) { a.Tasks = tasks }

    return vogui.Row(
        sidebar(a),
        vogui.Column(
            vogui.If(a.CurrentView == "tasks", vogui.Mount(&a.TaskPanel)),
            vogui.If(a.CurrentView == "chat", vogui.Mount(&a.ChatPanel)),
            vogui.If(a.CurrentView == "settings", vogui.Mount(&a.Settings)),
        ).Flex(1),
    )
}
```

When user switches from "chat" to "tasks":
- ChatPanel.Destroy() called → clears intervals, etc.
- TaskPanel.Init() called (if first time) → sets up resources

### Nested components

Components can contain other components:

```go
type TaskPanel struct {
    Tasks    []Task
    OnUpdate func([]Task)
    Filter   TaskFilter  // child component
    List     TaskList    // child component
}

func (t *TaskPanel) View() vogui.Node {
    t.Filter.OnChange = func(f string) { t.filterBy = f }
    t.List.Tasks = filterTasks(t.Tasks, t.filterBy)
    t.List.OnDelete = func(id string) { /* ... */ }
    return vogui.Column(
        vogui.Mount(&t.Filter),
        vogui.Mount(&t.List),
    )
}
```

Lifecycle is recursive: destroying TaskPanel also destroys Filter and List.

---

## 6. Implementation Order & Dependencies

### Dependency graph

```
Binary Protocol ──────────────┐
                               ├──→ [Both needed for games]
Canvas 2D API ────────────────┘

Style Combinators ──────────── [Independent, can be done in parallel]

Component Model ──────────────→ [Benefits from binary protocol's
                                  cached subtree marker, but not
                                  strictly dependent]
```

### Phase 1: Binary Protocol (est. 3-5 days)

1. Define type/prop/style enums in shared constants (Vo + JS)
2. Implement `encodeBinary()` in Vo (`encode.vo`)
3. Implement `decodeBinary()` in JS (`decoder.ts`)
4. Update Rust `emitRender` to pass bytes
5. Add decode path in JS renderer entry point
6. Benchmark: measure improvement on workspace example
7. Keep JSON path as debug fallback

### Phase 2: Canvas 2D API (est. 3-4 days)

1. Implement `CanvasCtx` and command buffer in Vo
2. Implement canvas command encoding (binary)
3. Add `flushCanvas` Rust extern
4. Implement `executeCanvasCommands()` in JS
5. Add `requestAnimationFrame` / `RunGameLoop` API
6. Add Rust externs for rAF
7. Port Tetris example to use Canvas API (validation)

### Phase 3: Style Combinators (est. 2-3 days)

1. Implement `Style` struct in Vo with builder methods
2. Implement CSS generation from Style (Vo-side)
3. Add `"styles"` field to render output
4. Implement style injection in JS
5. Add style dedup/caching (by hash)
6. Update one example to use Style combinators (validation)

### Phase 4: Component Model (est. 4-6 days)

1. Implement `Mount()` function and `__component` node type
2. Implement component context for handler scoping
3. Implement lifecycle tracking (Init/Destroy)
4. Implement ShouldUpdate / memo
5. Implement cached subtree optimization (with binary protocol)
6. Rewrite workspace example using Components (validation)

### Total estimate: 12-18 days

---

## 7. Revision of v2 Decisions

### Section 10.2: "No Component-Local State" → **Reversed**

v2 rationale: "Would require component identity/lifecycle system (complex runtime)
or Hooks-like API."

v3 response: We implement component identity via pointer tracking (simple map),
lifecycle via convention methods (Init/Destroy), and scoping via thread-local
context. No hooks, no call-order sensitivity. Complexity is bounded.

The key insight v2 missed: **managed components solve UI state, but application state
needs its own encapsulation.** A 500-line State struct is not maintainable.

### Section 10.1: "No Fine-Grained Reactivity" → **Still correct**

We don't implement signals/proxies. Component memo (`ShouldUpdate`) provides
coarse-grained skip optimization, which is sufficient when combined with binary
protocol efficiency.

### Section 10.6: "No Full CSS-in-Vo" → **Partially reversed**

v2 said no CSS-in-Vo. v3 introduces Style combinators which ARE CSS-in-Vo,
but intentionally limited: no CSS parser, no runtime CSS generation. Style
objects compile to CSS at render time, not at parse time. This is similar to
Emotion's `css()` function but simpler.

---

*End of document.*
