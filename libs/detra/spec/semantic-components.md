# Detra Semantic Components Specification

## Overview

Detra adopts a **semantic component model** where components describe *what* they are, not *how* to render them. Each renderer (egui, TUI, Web) interprets these semantic components according to its platform's best practices.

This is similar to React Native's approach: `<View>` becomes `UIView` on iOS, `android.View` on Android, and `<div>` on Web.

## Design Principles

1. **Semantic over Visual**: Components describe purpose, not appearance
2. **Renderer Freedom**: Renderers decide implementation details (animations, exact sizing, platform idioms)
3. **Progressive Enhancement**: Basic functionality guaranteed, advanced features optional per renderer
4. **Cross-Platform Consistency**: Same `.detra` file works on all renderers

## Component Hierarchy

```
Detra Semantic Components
├── Layout Components
│   ├── AppShell
│   ├── Sidebar
│   ├── MainArea
│   └── BottomPanel
├── Navigation Components
│   ├── TabBar
│   ├── ActivityBar
│   └── Breadcrumb
├── Content Components
│   ├── FileTree
│   ├── CodeEditor
│   ├── Terminal
│   ├── SearchPanel
│   └── ListView
├── Input Components
│   ├── TextInput
│   ├── Button
│   ├── Checkbox
│   ├── Select
│   └── Slider
└── Feedback Components
    ├── StatusBar
    ├── Notification
    └── ProgressBar
```

---

## Layout Components

### AppShell

The root container for an application. Manages the overall layout structure.

```detra
AppShell(
    showActivityBar: bool = true,
    showSidebar: bool = true,
    showBottomPanel: bool = false,
    sidebarPosition: "left" | "right" = "left"
) {
    // Children: ActivityBar?, Sidebar?, MainArea, BottomPanel?
}
```

**Renderer Interpretation:**
| Renderer | Implementation |
|----------|----------------|
| egui | `SidePanel` + `CentralPanel` + `TopBottomPanel` |
| TUI | ratatui `Layout::split()` |
| Web | CSS Grid / Flexbox |

---

### Sidebar

A collapsible side panel, typically for navigation or file trees.

```detra
Sidebar(
    width: int = 250,
    minWidth: int = 150,
    maxWidth: int = 500,
    resizable: bool = true,
    collapsible: bool = true,
    collapsed: bool = false
) {
    // Children: any content
}
```

**Events:**
- `onResize(width: int)` - When user resizes
- `onCollapse(collapsed: bool)` - When collapse state changes

**Renderer Interpretation:**
| Renderer | Implementation |
|----------|----------------|
| egui | `SidePanel::left().resizable().min_width().max_width()` |
| TUI | Fixed or percentage-based split |
| Web | Resizable div with drag handle |

---

### MainArea

The primary content area of the application.

```detra
MainArea {
    // Children: typically TabBar + content
}
```

---

### BottomPanel

A panel at the bottom, typically for terminal, output, or problems.

```detra
BottomPanel(
    height: int = 200,
    minHeight: int = 100,
    resizable: bool = true,
    visible: bool = true
) {
    // Children: typically TabBar + content
}
```

---

## Navigation Components

### TabBar

A horizontal bar of selectable tabs.

```detra
TabBar(
    tabs: []Tab,
    active: string,
    closable: bool = true,
    reorderable: bool = false
)

struct Tab {
    id: string
    label: string
    icon: string?
    dirty: bool = false  // Show unsaved indicator
}
```

**Events:**
- `onSelect(id: string)` - Tab selected
- `onClose(id: string)` - Tab close button clicked
- `onReorder(from: int, to: int)` - Tab dragged to new position

**Renderer Interpretation:**
| Renderer | Implementation |
|----------|----------------|
| egui | Custom tab bar with `ui.selectable_label()` |
| TUI | Horizontal list with highlight |
| Web | Native tabs or custom implementation |

---

### ActivityBar

A vertical bar of icon buttons, typically on the far left (VS Code style).

```detra
ActivityBar(
    items: []ActivityItem,
    active: string
)

struct ActivityItem {
    id: string
    icon: string
    label: string  // Tooltip
    badge: int?    // Notification count
}
```

**Events:**
- `onSelect(id: string)`

---

## Content Components

### FileTree

A hierarchical file/folder tree with expand/collapse.

```detra
FileTree(
    items: []FileItem,
    selected: string?,
    expanded: []string = [],
    multiSelect: bool = false
)

struct FileItem {
    path: string
    name: string
    isDir: bool
    children: []FileItem?
    icon: string?
}
```

**Events:**
- `onSelect(path: string)` - Item clicked
- `onExpand(path: string, expanded: bool)` - Folder expand/collapse
- `onContextMenu(path: string)` - Right-click

**Renderer Interpretation:**
| Renderer | Implementation |
|----------|----------------|
| egui | `CollapsingHeader` + `ui.selectable_label()` with indentation |
| TUI | Tree widget with +/- expand indicators |
| Web | Native `<details>` or custom tree |

---

### CodeEditor

A code editing area with optional syntax highlighting.

```detra
CodeEditor(
    content: string,
    language: string = "plain",
    readonly: bool = false,
    showLineNumbers: bool = true,
    wordWrap: bool = false,
    highlightLine: int? = nil
)
```

**Events:**
- `onChange(content: string)` - Content modified
- `onCursorChange(line: int, column: int)` - Cursor moved
- `onSave()` - Ctrl+S pressed

**Renderer Interpretation:**
| Renderer | Implementation |
|----------|----------------|
| egui | `TextEdit::multiline()` + `syntect` for highlighting |
| TUI | Custom text area with syntax highlighting |
| Web | Monaco Editor or CodeMirror |

**Language Support (renderer-dependent):**
- `"vo"`, `"rust"`, `"go"`, `"javascript"`, `"python"`, `"json"`, `"markdown"`, `"plain"`

---

### Terminal

A terminal/console output display.

```detra
Terminal(
    lines: []TerminalLine,
    maxLines: int = 1000,
    followOutput: bool = true
)

struct TerminalLine {
    text: string
    style: "normal" | "error" | "warning" | "success" = "normal"
    timestamp: int?
}
```

**Events:**
- `onInput(text: string)` - If interactive terminal

---

### SearchPanel

A search interface with results.

```detra
SearchPanel(
    query: string,
    results: []SearchResult,
    searching: bool = false,
    caseSensitive: bool = false,
    useRegex: bool = false
)

struct SearchResult {
    file: string
    line: int
    column: int
    preview: string
    matchStart: int
    matchEnd: int
}
```

**Events:**
- `onQueryChange(query: string)`
- `onResultSelect(file: string, line: int)`
- `onOptionsChange(caseSensitive: bool, useRegex: bool)`

---

### ListView

A generic scrollable list of items.

```detra
ListView(
    items: []ListItem,
    selected: string?,
    multiSelect: bool = false,
    virtualized: bool = true  // For large lists
)

struct ListItem {
    id: string
    primary: string
    secondary: string?
    icon: string?
}
```

**Events:**
- `onSelect(id: string)`
- `onActivate(id: string)` - Double-click or Enter

---

## Input Components

### TextInput

A single-line text input.

```detra
TextInput(
    value: string,
    placeholder: string = "",
    disabled: bool = false,
    password: bool = false,
    maxLength: int?
)
```

**Events:**
- `onChange(value: string)`
- `onSubmit()` - Enter pressed

---

### Button

A clickable button.

```detra
Button(
    label: string,
    icon: string?,
    variant: "primary" | "secondary" | "ghost" | "danger" = "secondary",
    disabled: bool = false,
    loading: bool = false
)
```

**Events:**
- `onClick()`

---

### Checkbox

A boolean toggle.

```detra
Checkbox(
    checked: bool,
    label: string?,
    disabled: bool = false,
    indeterminate: bool = false
)
```

**Events:**
- `onChange(checked: bool)`

---

### Select

A dropdown selection.

```detra
Select(
    value: string,
    options: []SelectOption,
    placeholder: string = "Select...",
    disabled: bool = false
)

struct SelectOption {
    value: string
    label: string
}
```

**Events:**
- `onChange(value: string)`

---

## Feedback Components

### StatusBar

Application status bar, typically at the bottom.

```detra
StatusBar {
    // Children: StatusBarItem components
}

StatusBarItem(
    text: string,
    icon: string?,
    align: "left" | "right" = "left",
    clickable: bool = false
)
```

**Events (StatusBarItem):**
- `onClick()`

---

### Notification

A toast/notification message.

```detra
Notification(
    message: string,
    type: "info" | "success" | "warning" | "error" = "info",
    duration: int = 5000,  // ms, 0 = persistent
    dismissible: bool = true
)
```

**Events:**
- `onDismiss()`

---

### ProgressBar

Progress indicator.

```detra
ProgressBar(
    value: float,  // 0.0 to 1.0
    indeterminate: bool = false,
    label: string?
)
```

---

## Complete Example: IDE Layout

```detra
state App {
    // External data (host-injected)
    external files []FileItem
    external currentFile string
    external editorContent string
    external terminalLines []TerminalLine
    
    // UI state
    sidebarVisible bool = true
    bottomVisible bool = true
    activeBottomTab string = "terminal"
    searchQuery string = ""
    searchResults []SearchResult = []
}

command OpenFile(path string)
command SaveFile(path string, content string)
command RunCommand(cmd string)

action ToggleSidebar() {
    set state.sidebarVisible = !state.sidebarVisible
}

action ToggleBottom() {
    set state.bottomVisible = !state.bottomVisible
}

action SetBottomTab(id string) {
    set state.activeBottomTab = id
}

action UpdateContent(content string) {
    set state.editorContent = content
}

action Search(query string) {
    set state.searchQuery = query
    emit SearchFiles(query: query)
}

view Main {
    AppShell(showSidebar: state.sidebarVisible, showBottomPanel: state.bottomVisible) {
        ActivityBar(
            items: [
                {id: "files", icon: "📁", label: "Explorer"},
                {id: "search", icon: "🔍", label: "Search"},
                {id: "settings", icon: "⚙", label: "Settings"}
            ],
            active: "files"
        )
        
        Sidebar(width: 250, resizable: true) {
            FileTree(
                items: state.files,
                selected: state.currentFile,
                onSelect: OpenFile
            )
        }
        
        MainArea {
            TabBar(
                tabs: [{id: state.currentFile, label: state.currentFile, dirty: false}],
                active: state.currentFile
            )
            CodeEditor(
                content: state.editorContent,
                language: "vo",
                onChange: UpdateContent,
                onSave: SaveFile(path: state.currentFile, content: state.editorContent)
            )
        }
        
        BottomPanel(height: 200, resizable: true) {
            TabBar(
                tabs: [
                    {id: "terminal", label: "TERMINAL"},
                    {id: "output", label: "OUTPUT"},
                    {id: "problems", label: "PROBLEMS"}
                ],
                active: state.activeBottomTab,
                onSelect: SetBottomTab
            )
            
            switch state.activeBottomTab {
            case "terminal":
                Terminal(lines: state.terminalLines)
            case "output":
                Terminal(lines: state.outputLines)
            default:
                ListView(items: state.problems)
            }
        }
        
        StatusBar {
            StatusBarItem(text: "Ln 1, Col 1")
            StatusBarItem(text: "UTF-8", align: "right")
            StatusBarItem(text: "Vo", align: "right")
        }
    }
}
```

---

## Renderer Implementation Guide

### Required Components (MVP)

Renderers MUST implement:
- `AppShell`, `Sidebar`, `MainArea`, `BottomPanel`
- `TabBar`
- `FileTree`
- `CodeEditor` (basic text editing, highlighting optional)
- `Button`, `TextInput`
- `StatusBar`

### Optional Components

Renderers MAY implement with fallbacks:
- `ActivityBar` → fallback to `TabBar`
- `Terminal` → fallback to `ListView`
- `SearchPanel` → fallback to `TextInput` + `ListView`
- `Notification` → fallback to console log

### Graceful Degradation

If a renderer doesn't support a component:
1. Use documented fallback
2. Log warning in debug mode
3. Never crash

---

## Migration from Current System

### Before (Low-level)

```detra
Column(width: 200, style: "sidebar") {
    Column(padding: 8) {
        Text(text: "EXPLORER", size: 11, bold: true)
        Button(text: "📄 main.vo", style: "treeitem", onClick: OpenFile(path: "main.vo"))
    }
}
```

### After (Semantic)

```detra
Sidebar(width: 200) {
    FileTree(items: state.files, onSelect: OpenFile)
}
```

**Benefits:**
- Renderer can use native tree component
- Automatic expand/collapse behavior
- Platform-appropriate icons
- Accessibility built-in
