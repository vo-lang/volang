# vui System Design

A comprehensive Elm-style UI framework for Vo.

---

## 1. Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Applications                                │
│   ┌─────────────┐    ┌─────────────┐    ┌─────────────┐            │
│   │ vibe-studio │    │  Other App  │    │  Other App  │            │
│   └──────┬──────┘    └──────┬──────┘    └──────┬──────┘            │
│          │                  │                  │                    │
│          ▼                  ▼                  ▼                    │
├─────────────────────────────────────────────────────────────────────┤
│                         vui (Vo)                                  │
│                                                                     │
│   Two-Fiber UI framework with Event/Signal model                    │
│   - UI Fiber: Apply Signal, View, Send Event                        │
│   - App Fiber: Handle Event, Render Signal                          │
│   - Widget library                                                  │
│                                                                     │
├─────────────────────────────────────────────────────────────────────┤
│                    egui Backend (Rust)                              │
│                                                                     │
│   Platform rendering layer (internal, user-invisible)               │
│   - egui + winit + wgpu (Desktop)                                   │
│   - egui + web-sys (Web/WASM)                                       │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

**Key Design Decisions:**
- vui uses **Two-Fiber Architecture**: UI Fiber (渲染) + App Fiber (业务逻辑)
- **Event/Signal Model**: Event (UI → App) + Signal (App → UI) 消息驱动
- Fully leverages Vo's **goroutine + channel** for async operations
- Rust backend is internal implementation detail, users only see Vo API
- Same Vo code runs on Desktop and Web

---

## 2. Core Architecture

### 2.1 Two-Fiber Model

```
┌─────────────┐    Event     ┌─────────────┐
│             │ ───────────► │             │
│  UI Fiber   │              │  App Fiber  │
│             │ ◄─────────── │             │
└─────────────┘   Signal     └─────────────┘
```

| Fiber | 职责 |
|-------|------|
| **UI Fiber** | 渲染 UI，收集用户操作，应用 Signal 更新 Model |
| **App Fiber** | 处理业务逻辑，执行异步任务，产生 Signal |

### 2.2 Message Types

| 消息 | 方向 | 类型 | 含义 |
|------|------|------|------|
| **Event** | UI → App | 数据 | 用户操作（点击、输入、键盘） |
| **Signal** | App → UI | 数据 | 状态变更描述 |

```vo
package vui

type Event any   // UI → App
type Signal any  // App → UI
```

### 2.3 App & UI Structure

```vo
package vui

type App struct {
    Init    func() any                              // Initialize App state
    Handle  func(state any, event Event) any        // Handle Event, return new state
    Render  func(state any) Signal                  // Produce Signal from state
}

type UI struct {
    Init    func() any                              // Initialize UI Model
    Apply   func(model any, signal Signal) any      // Apply Signal, return new Model
    View    func(model any)                         // Render UI (calls egui API)
}

type Config struct {
    Title   string
    Width   int
    Height  int
}

func Run(config Config, app App, ui UI)

// Send Event from View (collected and sent to App after View returns)
func Send(event Event)
```

### 2.4 Runtime Loop

```vo
func Run(config Config, app App, ui UI) {
    eventChan := make(chan Event, 256)
    signalChan := make(chan Signal, 256)
    
    // App Fiber: handles business logic (can block)
    go func() {
        appState := app.Init()
        for event := range eventChan {
            appState = app.Handle(appState, event)
            signal := app.Render(appState)
            signalChan <- signal
        }
    }()
    
    // UI Fiber: renders UI (each frame, never blocks)
    model := ui.Init()
    egui_run(config, func() {
        // 1. Apply pending Signals (non-blocking drain)
        for {
            select {
            case signal := <-signalChan:
                model = ui.Apply(model, signal)
            default:
                goto render
            }
        }
        
    render:
        // 2. Render UI
        ui.View(model)
        
        // 3. Collect Events from View, send to App
        for _, event := range drainViewEvents() {
            eventChan <- event
        }
    })
}
```

### 2.5 Fiber Model

```
┌─────────────────────────────────────────────────────────────────┐
│                      Vo Scheduler                                │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│   UI Fiber (每帧执行，不阻塞):                                    │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  1. Apply Signal → 更新 Model                            │   │
│   │  2. View(Model) → 渲染 UI                                │   │
│   │  3. 收集 Event → 发送给 App                              │   │
│   └─────────────────────────────────────────────────────────┘   │
│                              ▲                                   │
│                   signalChan │ │ eventChan                       │
│                              │ ▼                                 │
│   App Fiber (可阻塞):                                            │
│   ┌─────────────────────────────────────────────────────────┐   │
│   │  1. 收 Event                                             │   │
│   │  2. Handle(state, event) → 业务逻辑                      │   │
│   │  3. Render(state) → 产生 Signal                          │   │
│   │  4. 发送 Signal → UI                                     │   │
│   └─────────────────────────────────────────────────────────┘   │
│                                                                  │
│   Background Fibers (从 App Fiber spawn，可阻塞):                │
│   ┌──────────┐  ┌──────────┐  ┌──────────┐                      │
│   │ HTTP     │  │ File IO  │  │ Timer    │  ...                 │
│   └──────────┘  └──────────┘  └──────────┘                      │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## 3. Complete Example

```vo
package main

import "vui"

// ============ Events (UI → App) ============
type ClickIncrement struct{}
type ClickDecrement struct{}
type ClickFetch struct{}

// ============ Signals (App → UI) ============
type SetCount struct{ value int }
type SetLoading struct{ value bool }
type SetData struct{ value string }

// ============ App State ============
type AppState struct {
    count int
    data  string
}

func appInit() any {
    return AppState{count: 0}
}

func appHandle(state any, event vui.Event) any {
    s := state.(AppState)
    
    switch event.(type) {
    case ClickIncrement:
        s.count++
    case ClickDecrement:
        s.count--
    case ClickFetch:
        // Start async task (runs in background fiber)
        go func() {
            data := http.Get("https://api.example.com/data")
            // Send result back through internal mechanism
            vui.EmitSignal(SetData{value: data})
            vui.EmitSignal(SetLoading{value: false})
        }()
    }
    
    return s
}

func appRender(state any) vui.Signal {
    s := state.(AppState)
    // Produce Signal based on current state
    return SetCount{value: s.count}
}

// ============ UI Model ============
type Model struct {
    count   int
    loading bool
    data    string
}

func uiInit() any {
    return Model{count: 0}
}

func uiApply(model any, signal vui.Signal) any {
    m := model.(Model)
    
    switch sig := signal.(type) {
    case SetCount:
        m.count = sig.value
    case SetLoading:
        m.loading = sig.value
    case SetData:
        m.data = sig.value
    }
    
    return m
}

func uiView(model any) {
    m := model.(Model)
    
    vui.CentralPanel(func() {
        vui.Heading("Counter Example")
        vui.Separator()
        
        vui.Horizontal(func() {
            if vui.Button("-") {
                vui.Send(ClickDecrement{})
            }
            vui.Label(fmt.Sprintf("  %d  ", m.count))
            if vui.Button("+") {
                vui.Send(ClickIncrement{})
            }
        })
        
        vui.Space(20)
        
        if m.loading {
            vui.Spinner()
        } else {
            if vui.Button("Fetch Data") {
                vui.Send(ClickFetch{})
            }
            if m.data != "" {
                vui.Label("Data: " + m.data)
            }
        }
    })
}

// ============ Main ============
func main() {
    vui.Run(
        vui.Config{
            Title:  "My App",
            Width:  800,
            Height: 600,
        },
        vui.App{
            Init:   appInit,
            Handle: appHandle,
            Render: appRender,
        },
        vui.UI{
            Init:  uiInit,
            Apply: uiApply,
            View:  uiView,
        },
    )
}
```

---

## 4. Widget State Management

### 4.1 State Categories

| Category | Managed By | Examples |
|----------|-----------|----------|
| **Application State** | Model (user) | Business data, UI flags |
| **Complex Widget State** | Model (user) | DockState (layout persistence) |
| **Simple Widget State** | Framework (by ID) | TextInput cursor, ScrollArea position, Collapsing open/close |

### 4.2 Stateful Widgets

Widgets that need internal state require an ID:

```vo
// ID required - framework manages internal state
vui.TextInput("input-id", value)      // cursor position
vui.ScrollArea("scroll-id", func(){}) // scroll position
vui.Collapsing("section-id", "Title", func(){}) // open/close

// No ID needed - stateless
vui.Label("text")
vui.Button("click")
```

### 4.3 User-Managed Complex State

For complex state that users want to persist/control:

```vo
type Model struct {
    dock DockState  // User manages dock layout
}

func view(model any) {
    m := model.(Model)
    vui.DockArea("dock", &m.dock, func(nodeId int, tabId string) {
        // Render tab content
    })
}
```

---

## 5. Package Structure

```
libs/vui/
├── vo/                       # Vo API (what users import)
│   ├── vo.mod
│   ├── app.vo                # App, Config, Run, Cmd, Sub
│   ├── widget.vo             # All widgets
│   └── types.vo              # Color, Key, Modifiers, etc.
│
├── rust/                     # Rust backend (internal)
│   ├── src/
│   │   ├── lib.rs
│   │   ├── runtime.rs        # Main loop, FFI bridge
│   │   ├── widgets/          # Widget implementations
│   │   └── platform/         # Desktop/Web specific
│   └── Cargo.toml
│
└── vo.ext.toml               # Extension manifest
```

---

## 6. Widget API Reference

### App Lifecycle

```vo
package vui

type Config struct {
    Title       string
    Width       int
    Height      int
    Resizable   bool
    Maximized   bool
    Decorated   bool      // window decorations (title bar, etc.)
    Transparent bool
    VSync       bool
}

func Run(config Config, app App, ui UI)
func Quit()
func RequestRepaint()
```

### Panels (Top-level Layout)

```vo
type Side int
const (
    Left  Side = 0
    Right Side = 1
)

func CentralPanel(body func())
func SidePanel(id string, side Side, width float64, body func())
func SidePanelResizable(id string, side Side, defaultWidth float64, minWidth float64, maxWidth float64, body func())
func TopPanel(id string, body func())
func BottomPanel(id string, body func())

// Floating window
func Window(title string, body func())
func WindowWithConfig(title string, open *bool, resizable bool, collapsible bool, body func())

// Modal dialog (blocks interaction with background)
func Modal(id string, body func())
```

### Dock Area System (Arbitrary Panel Splitting)

The DockArea system allows VS Code-like arbitrary panel splitting and docking.

```vo
// DockArea is a container that supports arbitrary splitting and tab docking.
// It manages a tree of DockNodes, each of which can be:
// - A leaf node containing tabs
// - A horizontal split (left/right children)
// - A vertical split (top/bottom children)

type DockState struct {
    // Opaque state managed by egui runtime.
    // User passes this to DockArea and receives updates.
}

type DockNodeId int

type SplitDirection int
const (
    SplitHorizontal SplitDirection = 0  // left | right
    SplitVertical   SplitDirection = 1  // top / bottom
)

// Create a new DockState with an initial root node
func NewDockState() DockState

// Main dock area container
// - id: unique identifier for this dock area
// - state: current dock state (pass pointer for updates)
// - tabContent: called for each visible tab, receives (nodeId, tabIndex, tabId)
func DockArea(id string, state *DockState, tabContent func(nodeId DockNodeId, tabIndex int, tabId string))

// Dock operations (call these to modify the dock layout)

// Add a new tab to an existing node
func DockAddTab(state *DockState, nodeId DockNodeId, tabId string, tabTitle string)

// Remove a tab from a node
func DockRemoveTab(state *DockState, nodeId DockNodeId, tabId string)

// Split a node into two
// Returns the IDs of the two new child nodes
func DockSplit(state *DockState, nodeId DockNodeId, direction SplitDirection, ratio float64) (DockNodeId, DockNodeId)

// Get the active tab ID for a node (which tab is currently visible)
func DockActiveTab(state DockState, nodeId DockNodeId) string

// Set the active tab for a node
func DockSetActiveTab(state *DockState, nodeId DockNodeId, tabId string)

// Get root node ID
func DockRootNode(state DockState) DockNodeId

// Query node structure
func DockNodeTabs(state DockState, nodeId DockNodeId) []string           // tab IDs in this node
func DockNodeChildren(state DockState, nodeId DockNodeId) (DockNodeId, DockNodeId, bool)  // (child1, child2, isSplit)

// Close a node (removes it and potentially merges parent)
func DockCloseNode(state *DockState, nodeId DockNodeId)
```

**Dock Area Usage Example:**

```vo
package main

import "vui"

func main() {
    // Initialize dock state in Init
    dockState := vui.NewDockState()
    root := vui.DockRootNode(dockState)
    vui.DockAddTab(&dockState, root, "file1", "main.vo")
    vui.DockAddTab(&dockState, root, "file2", "utils.vo")
    
    vui.Run(vui.Config{Title: "Editor"}, vui.App{
        Init: func() (any, vui.Cmd) {
            return Model{dock: dockState}, vui.None
        },
        Update: updateEditor,
        View: func(model any) {
            m := model.(Model)
            vui.CentralPanel(func() {
                vui.DockArea("main-dock", &m.dock, func(nodeId vui.DockNodeId, tabIndex int, tabId string) {
                    switch tabId {
                    case "file1":
                        renderEditor("main.vo content...")
                    case "file2":
                        renderEditor("utils.vo content...")
                    }
                })
            })
            
            // Handle split via keyboard
            if vui.IsKeyPressed(vui.KeyBackslash) && vui.GetModifiers().Ctrl {
                vui.Send(SplitPane{})
            }
        },
    })
}
```

**Visual representation:**

```
Initial state:
┌─────────────────────────────────────────┐
│ [main.vo] [utils.vo]                    │  <- tabs
├─────────────────────────────────────────┤
│                                         │
│            Editor Content               │
│                                         │
└─────────────────────────────────────────┘

After vertical split:
┌─────────────────────────────────────────┐
│ [main.vo] [utils.vo]                    │
├─────────────────────────────────────────┤
│                                         │
│            Editor Content               │
│                                         │
├─────────────────────────────────────────┤
│ [main.vo]                               │  <- new node with copied tab
├─────────────────────────────────────────┤
│                                         │
│            Editor Content               │
│                                         │
└─────────────────────────────────────────┘

After horizontal split on bottom pane:
┌─────────────────────────────────────────┐
│ [main.vo] [utils.vo]                    │
├─────────────────────────────────────────┤
│                                         │
│            Editor Content               │
│                                         │
├───────────────────┬─────────────────────┤
│ [main.vo]         │ [main.vo]           │
├───────────────────┼─────────────────────┤
│                   │                     │
│    Content        │     Content         │
│                   │                     │
└───────────────────┴─────────────────────┘
```

### Layout Containers

```vo
func Horizontal(body func())
func HorizontalCentered(body func())
func HorizontalWrapped(body func())
func Vertical(body func())
func VerticalCentered(body func())
func Grid(id string, columns int, body func())

// Scroll area
func ScrollArea(id string, body func())
func ScrollAreaWithSize(id string, width float64, height float64, body func())
func ScrollToBottom(id string)
func ScrollToTop(id string)

// Simple split pane (fixed structure, resizable divider)
func HorizontalSplit(id string, defaultRatio float64, left func(), right func())
func VerticalSplit(id string, defaultRatio float64, top func(), bottom func())

// Spacing
func Space(size float64)
func Indent(amount float64, body func())

// Sizing constraints
func WithMinWidth(width float64, body func())
func WithMaxWidth(width float64, body func())
func WithMinHeight(height float64, body func())
func WithExactWidth(width float64, body func())
func WithExactHeight(height float64, body func())
```

### Text Display

```vo
func Label(text string)
func LabelColored(text string, color Color)
func LabelSelectable(text string)       // can be selected/copied
func Heading(text string)
func Monospace(text string)
func RichText(text string)              // supports **bold**, *italic*, `code`
```

### Buttons

```vo
func Button(text string) bool
func ButtonEnabled(text string, enabled bool) bool
func SmallButton(text string) bool
func IconButton(icon string) bool               // icon name or emoji
func ImageButton(imageId string) bool
func ToggleButton(text string, selected bool) (bool, bool)  // (newSelected, clicked)
```

### Input Widgets

```vo
// Single-line text input
func TextInput(id string, value string) (string, bool)
func TextInputWithHint(id string, hint string, value string) (string, bool)
func TextInputPassword(id string, value string) (string, bool)

// Multi-line text editor
func TextEdit(id string, value string) (string, bool)
func TextEditWithRows(id string, value string, rows int) (string, bool)

// Checkbox
func Checkbox(label string, checked bool) (bool, bool)

// Radio button
func RadioButton(label string, selected bool) bool

// Sliders
func SliderInt(label string, value int, min int, max int) (int, bool)
func SliderFloat(label string, value float64, min float64, max float64) (float64, bool)

// Drag values
func DragInt(label string, value int, speed float64) (int, bool)
func DragFloat(label string, value float64, speed float64) (float64, bool)

// Color picker
func ColorPicker(id string, color Color) (Color, bool)

// ComboBox / Dropdown
func ComboBox(id string, selected int, items []string) (int, bool)
```

### Containers

```vo
// Collapsing header
func Collapsing(header string, body func()) bool
func CollapsingDefaultOpen(header string, body func()) bool

// Tab bar (simple, for non-dock scenarios)
func TabBar(id string, tabs []string, selected int, body func(tabIndex int)) int

// Tree view (for file explorer, etc.)
func TreeNode(id string, label string, leaf bool, body func()) bool
func TreeNodeSelected(id string, label string, leaf bool, selected bool, body func()) (bool, bool)
func TreeNodeWithIcon(id string, icon string, label string, leaf bool, body func()) bool
```

### Menu System

```vo
func MenuBar(body func())
func Menu(title string, body func())
func MenuItem(label string) bool
func MenuItemWithShortcut(label string, shortcut string) bool
func MenuItemEnabled(label string, enabled bool) bool
func MenuSeparator()
func SubMenu(label string, body func())

// Context menu (right-click)
func ContextMenuArea(id string, content func(), menu func()) bool
```

### Display Widgets

```vo
func Separator()
func ProgressBar(fraction float64)
func ProgressBarWithText(fraction float64, text string)
func Spinner()
func SpinnerWithSize(size float64)
func Image(id string, width float64, height float64)

// Tooltip (attach to previous widget)
func Tooltip(text string)
func TooltipOnHover(body func())
```

### Style & Theming

```vo
type Color struct {
    R, G, B, A uint8
}

func RGB(r, g, b uint8) Color
func RGBA(r, g, b, a uint8) Color
func Hex(hex string) Color

// Predefined colors
var (
    White, Black, Red, Green, Blue, Yellow, Transparent Color
)

// Style scopes
func WithTextColor(color Color, body func())
func WithBackgroundColor(color Color, body func())
func WithFont(size float64, body func())
func WithMonoFont(body func())

// Themes
type Theme int
const (
    ThemeDark  Theme = 0
    ThemeLight Theme = 1
)
func SetTheme(theme Theme)
```

### Context & Input

```vo
// Available space
func AvailableWidth() float64
func AvailableHeight() float64
func AvailableSize() (float64, float64)

// Cursor
func CursorPos() (float64, float64)

// Focus
func RequestFocus(id string)
func HasFocus(id string) bool

// Keyboard
type Key int
const (
    KeyA Key = iota
    KeyB
    // ... KeyZ
    Key0
    // ... Key9
    KeyEnter
    KeyEscape
    KeyTab
    KeyBackspace
    KeyDelete
    KeyUp, KeyDown, KeyLeft, KeyRight
    KeyHome, KeyEnd
    KeyPageUp, KeyPageDown
    KeyF1, KeyF2, KeyF3, KeyF4, KeyF5, KeyF6, KeyF7, KeyF8, KeyF9, KeyF10, KeyF11, KeyF12
    KeySpace
    KeyBackslash
    KeySlash
    KeyMinus, KeyEquals
    KeyLeftBracket, KeyRightBracket
)

type Modifiers struct {
    Ctrl  bool
    Shift bool
    Alt   bool
    Cmd   bool  // macOS Command / Windows Super
}

func IsKeyPressed(key Key) bool       // just pressed this frame
func IsKeyDown(key Key) bool          // currently held
func IsKeyReleased(key Key) bool
func GetModifiers() Modifiers
func ConsumeKey(key Key)              // prevent further handling

// Mouse
func IsMouseButtonPressed(button int) bool   // 0=left, 1=right, 2=middle
func IsMouseButtonDown(button int) bool
func IsMouseButtonReleased(button int) bool

// Clipboard
func GetClipboard() string
func SetClipboard(text string)
```

### Code Editor (IDE-specific)

```vo
type Language int
const (
    LangPlain Language = iota
    LangVo
    LangGo
    LangRust
    LangPython
    LangJavaScript
    LangJSON
    LangMarkdown
    LangTOML
    LangYAML
)

type CursorPosition struct {
    Line   int
    Column int
}

type Selection struct {
    Start CursorPosition
    End   CursorPosition
}

type CodeEditorState struct {
    Text      string
    Cursor    CursorPosition
    Selection Selection
    Modified  bool
}

func CodeEditor(id string, state CodeEditorState, lang Language) (CodeEditorState, bool)
func CodeEditorWithLineNumbers(id string, state CodeEditorState, lang Language, showLineNumbers bool) (CodeEditorState, bool)
func CodeEditorReadOnly(id string, text string, lang Language)
```

### Utility

```vo
// Unique ID generation for dynamic content
func PushId(id string)
func PopId()
func WithId(id string, body func())

// Frame info
func FrameTime() float64       // seconds since last frame
func TotalTime() float64       // seconds since app start
func FrameCount() int
```

---

## 7. Testing

```vo
// Test App logic (Event → State)
func TestAppHandle(t *testing.T) {
    state := AppState{count: 0}
    
    newState := appHandle(state, ClickIncrement{})
    
    assert(newState.(AppState).count == 1)
}

// Test UI logic (Signal → Model)
func TestUIApply(t *testing.T) {
    model := Model{count: 0}
    
    newModel := uiApply(model, SetCount{value: 5})
    
    assert(newModel.(Model).count == 5)
}
```

---

## 8. Design Principles

1. **Two-Fiber Architecture** - UI Fiber (渲染) + App Fiber (业务逻辑) 分离
2. **Event/Signal Model** - Event (UI → App) + Signal (App → UI) 消息驱动
3. **Non-blocking UI** - UI Fiber 永不阻塞，App Fiber 可阻塞
4. **Goroutine-Powered** - Background tasks use goroutine for async operations
5. **Platform Agnostic** - Same Vo code for Desktop and Web
6. **ID-Based Widget State** - Stateful widgets identified by string ID

---

## 9. Implementation Phases

### Phase 1: Core Runtime
- [ ] Two-fiber model (UI Fiber + App Fiber)
- [ ] Event/Signal channels
- [ ] Send() and EmitSignal() API
- [ ] egui_run() integration

### Phase 2: Basic Widgets
- [ ] Label, Heading, Button
- [ ] TextInput, TextEdit
- [ ] Checkbox, Slider
- [ ] Horizontal, Vertical, ScrollArea

### Phase 3: Layout System
- [ ] CentralPanel, SidePanel, TopPanel, BottomPanel
- [ ] Split (resizable)
- [ ] Window, Modal

### Phase 4: Advanced Widgets
- [ ] MenuBar, Menu, MenuItem, ContextMenu
- [ ] TabBar, TreeNode, Collapsing
- [ ] DockArea (arbitrary splitting)

### Phase 5: Platform
- [ ] Desktop backend (egui + winit + wgpu)
- [ ] Web backend (egui + wasm)
