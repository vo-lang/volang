# VoGUI v2: Approaching Vue/Svelte Expressiveness

> Research report and architectural design for evolving vogui into a full-featured web application framework.
> Date: 2026-02-25

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Current State Analysis](#2-current-state-analysis)
3. [Gap Analysis: vogui vs Vue/Svelte](#3-gap-analysis)
4. [Vo Language Constraints](#4-vo-language-constraints)
5. [Design Principles](#5-design-principles)
6. [Architecture Design](#6-architecture-design)
7. [Detailed API Design](#7-detailed-api-design)
8. [JS Runtime Changes](#8-js-runtime-changes)
9. [Implementation Phases](#9-implementation-phases)
10. [What We Explicitly Don't Do](#10-what-we-explicitly-dont-do)
11. [Appendix: Framework Comparison](#appendix-framework-comparison)

---

## 1. Executive Summary

### Problem

vogui currently implements a pure Elm-style TEA (The Elm Architecture) with a single global
State struct, a pure view function, and action functions. While this model is clean and
predictable, it falls short of Vue/Svelte expressiveness in several critical areas:

- **No component encapsulation** — all UI state (dropdown open, tooltip visible, accordion
  expanded) must be manually managed in the global State struct
- **Weak styling** — only inline styles, no CSS classes, no theming, no scoped styles
- **Rigid event system** — fixed handler types, no arbitrary DOM events, no modifiers
- **No transitions/animations** — no enter/leave animations, no list reordering animation
- **No DOM access** — no refs, no programmatic focus/scroll/measure
- **Full re-render per event** — entire view tree rebuilt on every event

### Proposed Solution

A **two-layer state model** that preserves TEA's simplicity for application state while
delegating UI-only state to the JS runtime:

```
Vo Layer:  Application state (data, business logic, routing)
JS Layer:  UI behavior state (dropdown open, tooltip visible, focus, animation)
```

This is NOT abandoning TEA. It's recognizing that **UI behavior state** (is this dropdown
open? is this tooltip visible?) is VIEW-layer concern, not MODEL-layer concern. Just as
HTML `<details>` manages its own open/close, vogui's managed components manage their own
UI behavior.

### Key Changes

| Area | Current | Proposed |
|------|---------|----------|
| Component model | Functions returning Node | Functions + Managed Components |
| UI state | All in global State | App state in Vo, UI state in JS |
| Styling | Inline only (.Style()) | Classes + Themes + Inline |
| Events | Fixed handler types | Any DOM event + modifiers |
| Transitions | None | Built-in enter/leave/list |
| DOM access | None | Refs + imperative actions |
| Theming | None | CSS custom properties |

---

## 2. Current State Analysis

### 2.1 Architecture

```
Vo App → view(state) → Node tree → JSON → JS renderer → morphdom → DOM
                                                ↑
DOM event → JSON payload → Vo handler → mutate State → re-render
```

- **State**: Single user-defined struct, pointer passed to all handlers
- **View**: Pure function `func(state any) Node` rebuilds entire tree
- **Handlers**: Registered per render cycle, invoked via dynamic dispatch (`~>`)
- **Rendering**: Full tree → JSON → morphdom diffing
- **Styling**: Inline styles via chainable `.Style()` methods on Node
- **Events**: Fixed set of 14 handler types (click, value, checked, int, etc.)

### 2.2 What Works Well

1. **Mental model is simple**: State → View → Action is easy to understand
2. **No hidden state**: Everything is in one place, easy to debug
3. **Morphdom is efficient**: DOM updates are minimal despite full tree rebuild
4. **Component library is rich**: 60+ node types cover most UI needs
5. **Chainable styles are ergonomic**: `.P(16).Bg("#fff").Rounded(4)` reads well
6. **ForEach/ForEachKeyed**: List rendering with keys for stable DOM
7. **Router**: Basic SPA routing with path params
8. **Timers**: SetTimeout/SetInterval with proper cleanup
9. **ExternalWidget**: Escape hatch for JS-native components (CodeMirror, etc.)

### 2.3 Pain Points (from real examples)

**workspace.vo** (922 lines) — the most complex example — demonstrates these issues:

1. **State bloat**: 30+ fields in State, of which ~15 are UI-only state
   (`ShowTaskModal`, `ShowSettings`, `ShowAddTxModal`, `ShowNoteModal`, etc.)
2. **Handler boilerplate**: Every open/close pair needs 2 functions
   (`openTaskModal`/`closeTaskModal`, `openSettings`/`closeSettings`, etc.)
3. **Style repetition**: `.Style("padding", "24px").Style("gap", "16px")` repeated
   across many view functions with no way to define reusable style sets
4. **No transitions**: Modals appear/disappear instantly, no animation
5. **No keyboard handling**: Accordion, tabs, dropdown have no keyboard navigation
6. **Accessibility**: No ARIA attributes, no focus management

---

## 3. Gap Analysis

### 3.1 Critical Gaps (blocks real SPA development)

| # | Capability | Vue/Svelte | vogui | Impact |
|---|-----------|-----------|-------|--------|
| G1 | Component-local UI state | Components own their UI state | All in global State | State bloat, poor reusability |
| G2 | CSS classes & theming | Scoped CSS, CSS modules, themes | Inline styles only | Style repetition, no design system |
| G3 | Arbitrary DOM events | Any event, with modifiers | 14 fixed handler types | Can't handle drag, scroll, resize, focus, blur |
| G4 | Transitions/animations | Built-in transition system | None | Jarring UX, feels like a prototype |
| G5 | DOM refs & imperative | Template refs, programmatic focus | None | Can't focus inputs, scroll to elements, measure |
| G6 | Accessible components | ARIA built into component libs | No ARIA | Fails accessibility standards |
| G7 | Keyboard navigation | Built into interactive components | None | Dropdowns, menus, tabs not keyboard-usable |

### 3.2 Important Gaps (limits app quality)

| # | Capability | Vue/Svelte | vogui | Impact |
|---|-----------|-----------|-------|--------|
| G8 | Portals/Teleport | Render into arbitrary DOM node | None | Toast/notification positioning |
| G9 | Form validation | Libraries + reactive validation | Manual | Verbose form code |
| G10 | Raw HTML | v-html / {@html} | None | Can't render markdown, rich text |
| G11 | Head/meta management | vue-head, svelte:head | None | No document title, SEO |
| G12 | Responsive design | Media queries, CSS breakpoints | None | No mobile adaptation |
| G13 | Error boundaries | errorCaptured / onError | View panic = crash | No graceful degradation |
| G14 | Suspense/loading | Suspense component, await blocks | Manual | Verbose async patterns |

### 3.3 Non-Gaps (vogui already handles these)

- ✅ Declarative UI
- ✅ Reactive rendering (state change → auto re-render)
- ✅ List rendering with keys
- ✅ Conditional rendering (If/IfElse/Show)
- ✅ SPA routing with params
- ✅ Async HTTP (Fetch/Post)
- ✅ Timers (setTimeout/setInterval)
- ✅ Modal/Drawer overlays (functional, lacks animation)

---

## 4. Vo Language Constraints

These constraints shape our design:

| Constraint | Impact on UI Framework |
|-----------|----------------------|
| **No generics** | Can't have `Component[T]`. Must use `any` + type assertions. |
| **Restricted pointers** | Only `*Struct`, no `*int`. Component state must be struct. |
| **No sum types / enums** | No `Option<T>`. Use zero values + bools for optional state. |
| **Interface-only polymorphism** | Component APIs use `any` or interfaces, not generic params. |
| **Dynamic access (~>)** | Duck-typing for handler dispatch. Enables flexible callbacks. |
| **No macros / metaprogramming** | No JSX, no template compilation. UI is function calls. |

### Key Insight

Vo's `~>` operator (dynamic access) is a superpower for UI frameworks. It enables:
- Handler dispatch without knowing State type at compile time
- Property access on arbitrary structs
- Method calls on unknown types

This means vogui can have reusable components that work with ANY State type, because
handlers use `~>` for dynamic dispatch. This is equivalent to React's callback props
or Vue's `$emit` — but built into the language.

---

## 5. Design Principles

### P1: Vo Declares Intent, JS Manages Behavior

The Vo code describes WHAT should appear and WHAT should happen on business events.
The JS runtime handles HOW things appear, animate, and respond to micro-interactions.

```go
// Vo says: "there's a tooltip here with this content"
gui.Tooltip("Copy to clipboard",
    gui.IconButton("copy", gui.On(copyAction)),
)
// JS decides: show on hover, position above, fade in 150ms, hide on mouse leave
```

### P2: Application State in Vo, UI Behavior State in JS

- **Vo State**: Data models, form values, loading flags, selected items, user preferences
- **JS State**: Dropdown open/close, tooltip visible, focus ring, animation progress,
  scroll position, hover state

The boundary: **if the server/business logic cares about it, it's Vo state.
If only the UI cares, it's JS state.**

### P3: Clean Slate Rewrite

vogui v2 is a full rewrite. There is no backward compatibility with v1. Existing
vogui apps will be rewritten to use the new API. This gives us freedom to make
breaking changes and design the best possible API without legacy constraints.
The core TEA model (State → View → Action) is preserved as the foundation.

### P4: Platform-Native Where Possible

Use HTML/CSS/browser capabilities instead of reinventing them:
- CSS transitions instead of JS animation framework
- CSS custom properties instead of Vo-side theme engine
- HTML `<dialog>` instead of custom modal positioning
- CSS Grid/Flexbox instead of Vo-side layout engine
- ARIA attributes instead of custom accessibility system

### P5: Structure via Composition, Behavior via Props

Compound components use **composition** (child components) for visual structure,
and **Props structs** for behavioral configuration. This is NOT a contradiction —
it's a clear separation of concerns:

```go
// Structure = composition (DialogTitle, DialogContent, DialogActions)
// Behavior = Props (Open, OnClose)
gui.Dialog(gui.DialogProps{Open: s.Show, OnClose: gui.On(close)},
    gui.DialogTitle("Settings"),
    gui.DialogContent(form),
    gui.DialogActions(buttons),
)
```

Never put structural children into Props. Never put behavioral flags into children.

### P6: Vo Props Are Authoritative

When Vo explicitly provides a prop to a managed component, that prop **always**
overrides JS-managed internal state. JS state is only authoritative when Vo does
not provide the corresponding prop.

```go
// Vo controls visibility → JS MUST obey
gui.Dialog(gui.DialogProps{Open: false, OnClose: handler}, ...)
// Even if JS internal state says "open", the dialog is closed.

// Vo omits control → JS manages freely
gui.DropdownMenu(
    gui.Button("Actions"),   // trigger
    gui.MenuItem("Edit", gui.On(edit)),
)
// JS decides open/close on its own. Vo doesn't provide Open prop.
```

Sync timing: Vo props are applied on every render. If JS state diverges from
Vo props between renders, the next render corrects it.

---

## 6. Architecture Design

### 6.1 Two-Layer State Model

```
┌─────────────────────────────────────────────────────────┐
│                     Vo Application                       │
│  State struct: business data, form values, route state   │
│  View: declares UI structure and business event handlers │
│  Actions: mutate State on business events                │
└────────────────────────┬────────────────────────────────┘
                         │ Node tree (enhanced)
┌────────────────────────▼────────────────────────────────┐
│                    vogui Vo Library                       │
│  Pure view components: layout, display, text, etc.       │
│  Managed components: Dropdown, Dialog, Tooltip, etc.     │
│  Style API: .Class(), .Variant(), .Style()               │
│  Event API: .On(), .Handle(), modifiers                  │
│  Transition API: Transition(), TransitionGroup()         │
└────────────────────────┬────────────────────────────────┘
                         │ JSON (enhanced node format)
┌────────────────────────▼────────────────────────────────┐
│                   vogui JS Runtime                        │
│  Renderer: node → DOM with managed behaviors             │
│  Behavior engine: open/close, focus, keyboard nav        │
│  Transition engine: CSS class-based enter/leave          │
│  Theme engine: CSS custom properties                     │
│  Ref registry: named DOM references                      │
│  Portal manager: render into named containers            │
│  morphdom: smart diffing with component preservation     │
└─────────────────────────────────────────────────────────┘
```

### 6.2 Enhanced Node Model

Current:
```go
type Node struct {
    Type     string
    Props    map[string]any
    Children []Node
}
```

New design:
```go
type Node struct {
    Type     string
    Props    map[string]any
    Children []Node
}

// Chainable methods (all store into Props):
func (n Node) Class(cls string) Node          // CSS class names
func (n Node) Variant(v string) Node          // Component variant
func (n Node) Key(k string) Node              // Diffing key
func (n Node) Ref(name string) Node           // DOM reference name
func (n Node) Attr(key string, val any) Node  // HTML attribute
func (n Node) On(event string, h Handler) Node // Bind arbitrary DOM event
func (n Node) Transition(name string) Node    // Transition name
```

Everything serializes to the Props map → JSON. The `On` method on Node specifies
WHICH event to listen for; the Handler specifies WHAT to do and HOW (modifiers).

### 6.3 Managed Components

Managed components are Node types where the JS renderer manages interactive behavior
internally. The Vo side only declares structure and business handlers.

#### Categories:

**Trigger-based** (JS manages open/close):
- `Tooltip` — show on hover
- `Popover` — show on click, close on outside click
- `DropdownMenu` — show on click, close on select/outside/escape
- `Collapsible` — toggle open/close on click
- `HoverCard` — show on hover with delay

**Enhanced existing** (add keyboard nav, ARIA, animation):
- `Tabs` — arrow key navigation, ARIA roles
- `Accordion` — keyboard expand/collapse, ARIA
- `Select` — keyboard navigation, search, ARIA
- `Dialog` — focus trap, escape to close, ARIA
- `Drawer` — focus trap, escape to close, ARIA

**New managed**:
- `Toast` / `Notification` — auto-dismiss, stack management
- `CommandPalette` — keyboard-driven search/action
- `Combobox` — input + dropdown with filtering

#### Design: Trigger-Based Components

The pattern for trigger-based managed components:

```go
// First child = trigger, remaining children = content
gui.DropdownMenu(
    gui.Button("Actions"),             // trigger
    gui.MenuItem("Edit", gui.On(edit)),   // content item
    gui.MenuItem("Delete", gui.On(del)),  // content item
)
```

The JS renderer:
1. Renders the trigger normally
2. Wraps content in a positioned container (hidden by default)
3. Attaches click/hover/focus listeners to trigger
4. Manages open/close state, animation, positioning, keyboard navigation
5. On item click: fires the Vo handler, then closes

#### Design: State-Driven Components

For cases where Vo needs to control visibility programmatically:

```go
gui.Dialog(gui.DialogProps{Open: s.ShowSettings, OnClose: gui.On(closeSettings)},
    gui.DialogTitle("Settings"),
    gui.DialogContent(settingsForm(s)),
)
```

Per **P6** (Vo Props Are Authoritative): when `Open` is provided, JS MUST respect it.
If `Open: false`, the dialog is closed regardless of JS internal state. The next render
always reconciles.

Rule: **If a business event should trigger visibility (API response, timer, navigation),
use state-driven. If user interaction triggers visibility (click, hover), use managed.**

### 6.4 CSS & Theming

#### Layer 1: Theme (CSS Custom Properties)

```go
// In Init, set the theme:
func main() {
    gui.SetTheme(gui.Theme{
        Primary:    "#3b82f6",
        Secondary:  "#6b7280",
        Success:    "#22c55e",
        Danger:     "#ef4444",
        Warning:    "#f59e0b",
        Background: "#ffffff",
        Surface:    "#f8fafc",
        Text:       "#0f172a",
        TextMuted:  "#64748b",
        Border:     "#e2e8f0",
        Radius:     "6px",
        FontFamily: "system-ui, -apple-system, sans-serif",
    })
    gui.Run(gui.App{...})
}
```

This emits CSS custom properties:
```css
:root {
    --vo-primary: #3b82f6;
    --vo-secondary: #6b7280;
    /* etc. */
}
```

All built-in component styles use these variables. Dark mode is just a different theme.

#### Layer 2: Component Variants

Built-in components support variants:

```go
gui.Button("OK", handler).Variant("primary")     // --vo-primary bg
gui.Button("Cancel", handler).Variant("outline")  // bordered, no bg
gui.Button("Delete", handler).Variant("danger")   // --vo-danger bg
gui.Button("Ghost", handler).Variant("ghost")     // no bg, no border

gui.Alert("success", "Saved!").Variant("soft")    // soft bg
gui.Badge("New").Variant("success")               // green
```

#### Layer 3: CSS Classes

```go
// Add class names (user writes CSS externally or uses built-in utility classes)
gui.Div(children...).Class("sidebar")
gui.Button("OK", handler).Class("btn-lg")

// Multiple classes
gui.Row(children...).Class("header sticky")
```

#### Layer 4: Inline Styles (Existing)

```go
// One-off customization
gui.Text("Hello").Style("letterSpacing", "2px")
```

#### Utility Classes (Built-in)

A small set of commonly needed utilities shipped with vogui:

```css
/* Spacing */
.p-0 through .p-12  (padding: 0-48px, step 4px)
.m-0 through .m-12
.px-*, .py-*, .mx-*, .my-*
.gap-0 through .gap-8

/* Layout */
.flex, .flex-col, .flex-row, .flex-wrap
.items-center, .items-start, .items-end
.justify-center, .justify-between, .justify-end
.flex-1, .flex-none

/* Sizing */
.w-full, .h-full, .min-h-screen
.w-1/2, .w-1/3, .w-2/3

/* Text */
.text-sm, .text-base, .text-lg, .text-xl
.font-bold, .font-medium
.text-center, .text-right
.truncate, .line-clamp-2, .line-clamp-3

/* Display */
.hidden, .block, .inline, .inline-block

/* Overflow */
.overflow-hidden, .overflow-auto, .overflow-scroll

/* Position */
.relative, .absolute, .fixed, .sticky
.top-0, .right-0, .bottom-0, .left-0, .inset-0

/* Border */
.border, .border-t, .border-b
.rounded, .rounded-lg, .rounded-full

/* Cursor */
.cursor-pointer, .cursor-default, .cursor-not-allowed

/* Responsive (media query prefixes) */
.sm:hidden, .md:flex, .lg:grid-cols-3
```

These are emitted as part of the vogui CSS injection. NOT a full Tailwind — just the
most commonly needed utilities for layout and spacing.

### 6.5 Event System

#### Current Limitations

- Fixed set: click, value, checked, int, intValue, intChecked, key, submit, select,
  slider, files, pointer, resize, widget
- No arbitrary DOM events (focus, blur, scroll, drag, etc.)
- No event modifiers (prevent default, stop propagation, debounce)
- No multiple events on one element

#### New Design

Two concepts: **Handler constructors** (create a Handler) and **Node binding**
(attach a Handler to a DOM event on a Node).

**Handler constructors** — create a Handler specifying WHAT to do:
```go
gui.On(action)                    // generic handler (click for buttons, change for inputs)
gui.OnValue(action)               // handler that passes input value as string
gui.OnChecked(action)             // handler that passes checkbox state as bool
gui.OnKey(action)                 // handler that passes key name as string
gui.OnSubmit(action)              // handler for form submission
gui.Handle(action)                // generic handler for arbitrary DOM events
```

**Event modifiers** — chainable on any Handler:
```go
gui.On(action).Prevent()          // preventDefault
gui.On(action).Stop()             // stopPropagation
gui.On(action).Once()             // fire once, then remove
gui.On(action).Debounce(300)      // debounce 300ms
gui.On(action).Throttle(100)      // throttle 100ms
gui.OnKey(action).Filter("Enter") // only fire on specific key
```

**Node binding** — attach a Handler to a specific DOM event on any Node:
```go
// Components with built-in events use handler constructors directly:
gui.Button("OK", gui.On(save))             // click is implicit
gui.Input(value, gui.OnValue(setValue))     // change is implicit

// Arbitrary DOM events use node.On(eventName, handler):
gui.Div(children...).On("scroll", gui.Handle(onScroll).Throttle(100))
gui.Input(v, gui.OnValue(setV)).On("focus", gui.Handle(onFocus))
gui.Div(children...).On("mouseenter", gui.Handle(onEnter))
```

This separation is clean: `gui.Handle(action)` says WHAT happens, `node.On(event, h)`
says WHEN it happens. No redundant event name specification.

#### Implementation

```go
type Handler struct {
    ID        int
    Gen       int      // render generation (monotonically increasing)
    Type      int      // handler type: click, value, checked, key, submit, generic
    IntVal    int
    Modifiers []string // "prevent", "stop", "once", "debounce:300", "throttle:100"
    KeyFilter string   // for key events: specific key name
}
```

**Handler generation protocol** (fixes stale handler correctness bug):

1. Each render cycle increments a monotonic `renderGen` counter
2. All handlers registered during that render carry `Gen = renderGen`
3. The JSON output includes `"gen": renderGen` at the top level
4. When JS dispatches an event, it sends `{"gen": N, "id": handlerID, "payload": ...}`
5. Vo discards events where `gen != currentGen`

This guarantees that debounced/throttled events firing after a re-render are safely
discarded instead of invoking the wrong handler. Without this, any async modifier
(Debounce, Throttle, Once with delay) is a correctness hazard.

The JS renderer reads modifiers and applies them when attaching event listeners.

### 6.6 Transitions

#### API

```go
// Wrap a conditional node with a transition
gui.Transition("fade",
    gui.If(s.ShowPanel, panelContent),
)

// Transition with name
gui.Transition("slide-up",
    gui.Show(s.MenuOpen, menuContent),
)

// List transitions (for ForEachKeyed)
gui.TransitionGroup("fade-slide",
    gui.ForEachKeyed(s.Items, keyFn, renderItem),
)
```

#### Built-in Transitions

| Name | Enter | Leave |
|------|-------|-------|
| `fade` | opacity 0→1 | opacity 1→0 |
| `slide-down` | translateY(-10px)→0, opacity | reverse |
| `slide-up` | translateY(10px)→0, opacity | reverse |
| `slide-left` | translateX(10px)→0, opacity | reverse |
| `slide-right` | translateX(-10px)→0, opacity | reverse |
| `scale` | scale(0.95)→1, opacity | reverse |
| `collapse` | max-height 0→auto | reverse |

#### Custom Transitions

Users define transitions via CSS classes:

```css
/* Custom transition "my-spin" */
.vo-enter-my-spin { transform: rotate(-90deg); opacity: 0; }
.vo-enter-active-my-spin { transition: all 0.3s ease; }
.vo-leave-active-my-spin { transition: all 0.2s ease; }
.vo-leave-to-my-spin { transform: rotate(90deg); opacity: 0; }
```

```go
gui.Transition("my-spin", gui.If(cond, child))
```

#### Implementation

The JS runtime:
1. Detects when a conditional node's child appears/disappears via morphdom
2. On enter: add `.vo-enter-from-{name}` → next frame: add `.vo-enter-to-{name}`,
   remove `-from` → on transition end: remove all transition classes
3. On leave: clone element, add `.vo-leave-from-{name}` → next frame: add
   `.vo-leave-to-{name}` → on transition end: remove cloned element
4. For lists: FLIP animation (First, Last, Invert, Play) for smooth reordering

### 6.7 DOM Access (Refs)

#### API

```go
// Name a DOM element
gui.Input(s.Query, gui.OnValue(setQuery)).Ref("search-input")
gui.Div(content).Ref("scroll-container")

// Imperative actions (called in action functions)
gui.Focus("search-input")           // focus element
gui.Blur("search-input")            // blur element
gui.ScrollTo("scroll-container", 0) // scroll to top
gui.ScrollIntoView("item-42")       // scroll element into view
gui.Select("search-input")          // select all text
```

#### Implementation

- Refs stored in a JS-side registry: `Map<string, HTMLElement>`
- `gui.Focus()` etc. are extern functions that emit commands to JS
- Commands executed after the current render cycle completes
- Refs are stable across morphdom updates (elements with `data-ref` attribute)

### 6.8 Portal System

Portals render content into a fixed DOM location, independent of the component tree.
Used for: toasts, notifications, popovers that need to escape overflow:hidden.

```go
// Render a toast notification at the top-right of the viewport
gui.Portal("toast",
    gui.If(s.ShowToast,
        gui.Alert("success", "Saved successfully!"),
    ),
)
```

JS runtime creates named containers:
```html
<div id="vo-portal-toast" class="vo-portal vo-portal-toast"></div>
```

CSS positions these containers appropriately.

### 6.9 Head Management

```go
// Set document title (reactive — updates on every render if different)
gui.SetTitle("My App - " + s.CurrentPage)

// Future: meta tags
gui.SetMeta("description", "My awesome app")
```

Implementation: `gui.SetTitle()` stores the title in the render output JSON. JS
applies it via `document.title = title`.

### 6.10 Unsafe HTML

```go
// Render trusted HTML content (e.g., from markdown parser)
gui.UnsafeHTML(markdownToHTML(s.Content))

// With container class
gui.UnsafeHTML(htmlContent).Class("prose")
```

The name `UnsafeHTML` makes the XSS risk explicit. JS renderer uses `innerHTML`.
The caller is responsible for sanitization (same as Vue's `v-html` / Svelte's `{@html}`).

### 6.11 HTML Attributes (ARIA, data-*, role, etc.)

```go
gui.Button("Menu", handler).
    Attr("aria-label", "Open main menu").
    Attr("aria-expanded", s.MenuOpen).
    Attr("role", "menubutton")

gui.Div(content).
    Attr("data-testid", "user-list").
    Attr("tabindex", 0)
```

Managed components automatically set appropriate ARIA attributes.

---

## 7. Detailed API Design

### 7.1 New Managed Components

#### DropdownMenu

```go
// Trigger-based: JS manages open/close
gui.DropdownMenu(
    gui.Button("Actions"),                          // trigger (first child)
    gui.MenuItem("Edit", gui.On(editItem)),         // menu items
    gui.MenuItemWithIcon("trash", "Delete", gui.On(deleteItem)),
    gui.MenuDivider(),
    gui.MenuItemDisabled("Archive"),
)
```

Behavior (JS-managed):
- Opens on trigger click
- Closes on: item click, outside click, Escape key
- Arrow keys navigate items
- Focus trapped within menu
- ARIA: role=menu, role=menuitem, aria-expanded

#### Combobox (Searchable Select)

```go
gui.Combobox(gui.ComboboxProps{
    Value:       s.SelectedCity,
    Placeholder: "Select a city...",
    OnSelect:    gui.OnValue(setCity),
},
    gui.ComboboxOption("New York", "nyc"),
    gui.ComboboxOption("San Francisco", "sfo"),
    gui.ComboboxOption("Tokyo", "tyo"),
)
```

Behavior (JS-managed):
- Input field with dropdown
- Filters options as user types
- Arrow keys navigate, Enter selects
- Escape closes dropdown
- ARIA: role=combobox, aria-autocomplete

#### Toast / Notification

```go
// In an action function:
func saveItem(s *State) {
    // ... save logic
    gui.Toast("Item saved successfully", "success", 3000) // auto-dismiss 3s
}

// Or in view (state-driven):
gui.Portal("notifications",
    gui.ForEach(s.Notifications, func(item any, i int) gui.Node {
        n := item.(Notification)
        return gui.Alert(n.Type, n.Message).
            Key(fmt.Sprint(n.ID)).
            Transition("slide-right")
    }),
)
```

#### Collapsible

```go
// JS manages open/close toggle
gui.Collapsible(
    gui.Row(gui.Text("Advanced Settings"), gui.Icon("chevron-down")), // trigger
    gui.Column(advancedSettings(s)...),  // content (collapsed by default)
)

// Start expanded
gui.CollapsibleOpen(
    gui.Text("Details"),      // trigger
    gui.Column(details...),   // content
)
```

#### HoverCard

```go
gui.HoverCard(
    gui.Link("@username", "/user/123"),  // trigger
    gui.Card(                             // card shown on hover
        gui.Row(
            gui.Avatar(user.AvatarURL),
            gui.Column(
                gui.Strong(user.Name),
                gui.Text(user.Bio),
            ),
        ),
    ),
)
```

### 7.2 Enhanced Existing Components

#### Tabs (with keyboard nav + ARIA)

```go
// Tabs with keyboard navigation and ARIA
gui.Tabs(s.ActiveTab, []gui.TabItem{
    {Label: "Overview", Content: overviewView(s)},
    {Label: "Settings", Content: settingsView(s)},
    {Label: "Members", Content: membersView(s)},
}, gui.OnSlider(setActiveTab))
```

New JS behaviors:
- Arrow keys switch tabs
- Home/End go to first/last tab
- ARIA: role=tablist, role=tab, role=tabpanel, aria-selected

#### Accordion (with keyboard nav + transitions)

```go
gui.Accordion([]gui.AccordionItem{
    {Title: "Section 1", Content: section1(s)},
    {Title: "Section 2", Content: section2(s)},
}, gui.OnSlider(toggleAccordion)).
    Transition("collapse")  // animate open/close
```

New JS behaviors:
- Arrow keys navigate panels
- Enter/Space toggle panel
- ARIA: role=region, aria-expanded

#### Dialog (enhanced Modal)

```go
// State-driven: Vo controls visibility via Open prop
gui.Dialog(gui.DialogProps{
    Open:    s.ShowSettings,
    OnClose: gui.On(closeSettings),
},
    gui.DialogTitle("Settings"),
    gui.DialogContent(
        settingsForm(s),
    ),
    gui.DialogActions(
        gui.Button("Cancel", gui.On(closeSettings)).Variant("outline"),
        gui.Button("Save", gui.On(saveSettings)).Variant("primary"),
    ),
).Transition("scale")
```

New JS behaviors:
- Uses HTML `<dialog>` element
- Focus trap (Tab cycles within dialog)
- Escape closes dialog
- Backdrop click closes dialog
- Enter/leave transitions
- ARIA: role=dialog, aria-modal, aria-labelledby

### 7.3 Div / Block (Generic Container)

```go
// Generic container for when layout components are too specific
gui.Div(children...).Class("sidebar").Attr("role", "navigation")
gui.Div(children...).Class("hero-section").Style("minHeight", "50vh")
```

The current `Block` type is similar but less clear. `Div` is explicit.

### 7.4 Responsive Helpers

```go
// Render different layouts based on screen size
// (using CSS media queries, not Vo logic)
gui.Div(
    gui.Div(sidebar(s)).Class("hidden md:block md:w-64"),
    gui.Div(content(s)).Class("flex-1"),
).Class("flex")
```

Responsive design is handled entirely through CSS utility classes with responsive
prefixes. No Vo-side screen size detection needed.

---

## 8. JS Runtime Changes

### 8.1 Behavior Engine

New module: `behavior.ts`

Manages interactive behaviors for managed components:

```typescript
interface BehaviorController {
    attach(trigger: HTMLElement, content: HTMLElement): void;
    detach(): void;
}

const behaviors: Record<string, (config: any) => BehaviorController> = {
    'dropdown-menu': createDropdownBehavior,
    'tooltip': createTooltipBehavior,
    'popover': createPopoverBehavior,
    'collapsible': createCollapsibleBehavior,
    'hover-card': createHoverCardBehavior,
    'combobox': createComboboxBehavior,
};
```

Each behavior controller:
- Attaches event listeners (click, hover, focus, keyboard)
- Manages CSS classes for open/close state
- Handles focus trapping
- Sets ARIA attributes
- Cleans up on detach

### 8.2 Transition Engine

New module: `transitions.ts`

```typescript
function enterTransition(el: HTMLElement, name: string): Promise<void>;
function leaveTransition(el: HTMLElement, name: string): Promise<void>;
function flipAnimation(elements: HTMLElement[]): void;
```

Integrated into morphdom callbacks:
- `onBeforeNodeAdded`: start enter transition
- `onBeforeNodeRemoved`: start leave transition, delay removal until animation ends
- `onElUpdated`: FLIP for list reordering

**Note**: The current renderer uses `onBeforeElUpdated` and `onNodeDiscarded` but
not `onBeforeNodeAdded`/`onBeforeNodeRemoved`. These additional hooks must be
wired up and tested for correct behavior with conditional rendering (If/Show).

### 8.3 Ref Registry

```typescript
const refRegistry = new Map<string, HTMLElement>();

// In renderer: when node has ref prop
el.dataset.ref = refName;
refRegistry.set(refName, el);

// Cleanup on removal
onNodeDiscarded: if (el.dataset.ref) refRegistry.delete(el.dataset.ref);
```

### 8.4 Portal Manager

```typescript
const portalContainers = new Map<string, HTMLElement>();

function getOrCreatePortal(name: string): HTMLElement {
    if (!portalContainers.has(name)) {
        const container = document.createElement('div');
        container.id = `vo-portal-${name}`;
        container.className = `vo-portal vo-portal-${name}`;
        document.body.appendChild(container);
        portalContainers.set(name, container);
    }
    return portalContainers.get(name)!;
}
```

### 8.5 Theme Engine

```typescript
function applyTheme(theme: Record<string, string>): void {
    const root = document.documentElement;
    for (const [key, value] of Object.entries(theme)) {
        root.style.setProperty(`--vo-${kebabCase(key)}`, value);
    }
}
```

### 8.6 Enhanced Renderer Changes

The `renderNode()` function needs these additions:
1. Read `class` from props and apply to element's className
2. Read `ref` from props and register in ref registry
3. Read `transition` from props and wire up transition engine
4. Read `attrs` from props and set as HTML attributes
5. Read event modifiers from handler metadata and apply (prevent, stop, debounce, etc.)
6. Attach `gen` (render generation) to all event dispatches for stale event discard
7. Support `node.On(event, handler)` — bind arbitrary DOM events from props
8. Handle new managed component types (DropdownMenu, Combobox, etc.)
9. Handle Portal nodes (render into portal container instead of parent)
10. Enforce P6: when Vo props provide explicit values, override JS-managed state

**Implementation risk**: Transition engine relies on morphdom's `onBeforeNodeAdded` /
`onBeforeNodeRemoved` hooks. These hooks exist in morphdom's API but the current
renderer primarily uses `onBeforeElUpdated` / `onNodeDiscarded`. The transition
integration needs careful testing to ensure these hooks fire reliably with our
node tree structure and keying strategy.

---

## 9. Implementation Phases

### Phase 1: CSS Foundation (Priority: Highest)

**Goal**: Proper styling system that replaces inline-style-only approach.

| Task | Description |
|------|-------------|
| 1.1 | `.Class(cls string)` method on Node |
| 1.2 | `.Variant(v string)` method on Node |
| 1.3 | `.Attr(key, val)` method on Node |
| 1.4 | Theme system: `gui.SetTheme()` → CSS custom properties |
| 1.5 | Component variants for Button, Alert, Badge, Card |
| 1.6 | Utility CSS classes (spacing, flex, text, display) |
| 1.7 | JS renderer reads `class`, `variant`, `attrs` from props |
| 1.8 | Responsive utility prefixes (sm:, md:, lg:) |

**Acceptance**:
- Test app uses `.Class()`, `.Variant()`, `.Attr()` on 5+ component types
- Dark mode toggle via `gui.SetTheme()` works
- Utility classes produce correct CSS (spot-check 10 classes)

### Phase 2: Event System Enhancement (Priority: High)

| Task | Description |
|------|-------------|
| 2.1 | `gui.Handle(action)` for generic handlers |
| 2.2 | `node.On(event, handler)` for arbitrary DOM event binding |
| 2.3 | Handler modifiers: `.Prevent()`, `.Stop()`, `.Once()` |
| 2.4 | `.Debounce(ms)` and `.Throttle(ms)` modifiers |
| 2.5 | `gui.OnKey(action).Filter(key)` for key-specific events |
| 2.6 | Handler generation protocol (renderGen counter, stale event discard) |
| 2.7 | JS renderer applies event modifiers |

**Acceptance**:
- Test: scroll, focus, blur, mouseenter events fire correctly
- Test: `.Debounce(300)` on input only fires once after 300ms pause
- Test: stale debounced event after re-render is discarded (generation mismatch)
- Test: `.Prevent()` actually prevents default browser behavior

### Phase 3: Transitions (Priority: High)

| Task | Description |
|------|-------------|
| 3.1 | `gui.Transition(name, child)` wrapper component |
| 3.2 | JS transition engine: enter/leave CSS class management |
| 3.3 | Built-in transitions: fade, slide-*, scale, collapse |
| 3.4 | Integration with morphdom for enter/leave detection |
| 3.5 | `gui.TransitionGroup(name, list)` for list animations |
| 3.6 | FLIP animation for list reordering |

**Acceptance**:
- Test: `gui.If` with `Transition("fade")` animates in/out
- Test: `ForEachKeyed` with `TransitionGroup` animates item add/remove
- Test: custom CSS transition class works
- Visual: no layout jump during enter/leave

### Phase 4: Managed Components (Priority: High)

| Task | Description |
|------|-------------|
| 4.1 | Behavior engine framework in JS |
| 4.2 | `DropdownMenu` managed component |
| 4.3 | `Combobox` managed component |
| 4.4 | `Collapsible` managed component |
| 4.5 | `Tooltip` enhancement (managed hover, delay, positioning) |
| 4.6 | `HoverCard` managed component |
| 4.7 | Enhanced `Dialog` with `<dialog>`, focus trap, transitions |
| 4.8 | Enhanced `Drawer` with focus trap, transitions |
| 4.9 | Enhanced `Tabs` with keyboard nav, ARIA |
| 4.10 | Enhanced `Accordion` with keyboard nav, ARIA, transitions |
| 4.11 | `Toast` imperative API + portal rendering |

**Acceptance**:
- Each managed component has a test app exercising all interactions
- Keyboard nav: Tab/Arrow/Enter/Escape work on DropdownMenu, Tabs, Accordion, Combobox
- P6 compliance: Dialog with `Open: false` from Vo overrides JS state
- ARIA: axe-core audit on each managed component passes

### Phase 5: DOM Access & Utilities (Priority: Medium)

| Task | Description |
|------|-------------|
| 5.1 | `.Ref(name)` method on Node |
| 5.2 | JS ref registry |
| 5.3 | `gui.Focus(ref)`, `gui.Blur(ref)` extern functions |
| 5.4 | `gui.ScrollTo(ref, pos)`, `gui.ScrollIntoView(ref)` |
| 5.5 | Portal system: `gui.Portal(name, children)` |
| 5.6 | Portal CSS positioning (toast, notification containers) |
| 5.7 | `gui.SetTitle(title)` for document head |
| 5.8 | `gui.UnsafeHTML(content)` for trusted HTML |
| 5.9 | `gui.Div(children...)` generic container |

**Acceptance**:
- Test: `gui.Focus("input-ref")` actually focuses the element after render
- Test: Portal content renders outside parent overflow:hidden
- Test: `gui.SetTitle()` updates document.title
- Test: `gui.UnsafeHTML` renders HTML string as DOM

### Phase 6: Accessibility (Priority: Medium)

| Task | Description |
|------|-------------|
| 6.1 | ARIA attributes on all managed components |
| 6.2 | Focus management in Dialog, Drawer |
| 6.3 | Keyboard navigation in all interactive components |
| 6.4 | Screen reader testing and fixes |
| 6.5 | Color contrast verification in default theme |

**Acceptance**:
- axe-core automated audit on full test app: 0 critical/serious violations
- Manual screen reader test (VoiceOver or NVDA) on Dialog, Menu, Tabs
- All interactive components reachable and operable via keyboard only

---

## 10. What We Explicitly Don't Do

### 10.1 Fine-Grained Reactivity (Signals)

**Why not**: Signals (SolidJS, Svelte 5 runes, Leptos) track individual value changes
for surgical DOM updates. This requires compiler support or a runtime signal graph.
Vo has neither. The morphdom approach (full tree diff) is good enough for the vast
majority of applications, and the simplicity of TEA is worth preserving.

**If needed later**: Performance optimization via subtree memoization (skip unchanged
subtrees during JSON serialization) is a much simpler approach.

### 10.2 Component-Local State in Vo

**Why not**: This would require either:
- A component identity/lifecycle system (complex runtime)
- Hooks-like API (requires call-order stability, easy to misuse without compiler checks)
- Both require departure from TEA's clean "all state in one place" model

**Instead**: Managed components (JS-side UI state) solve the actual problem (dropdown/
tooltip/animation state) without complicating the Vo model.

### 10.3 Virtual DOM

**Why not**: We already have morphdom, which diffs real DOM. A virtual DOM would add
complexity (double representation) for marginal benefit. morphdom is battle-tested and
efficient.

### 10.4 Server-Side Rendering (SSR)

**Why not**: Vo runs in WASM, which is client-side by nature. SSR would require running
the Vo VM on a server and serializing HTML output. This is possible but complex and
not a priority for the playground/desktop use cases.

### 10.5 Compile-Time Template Optimization

**Why not**: Vo views are runtime function calls, not templates. There's no compile-time
template analysis. Vue and Svelte can optimize because they have a template compiler.
Vo's approach is more like React — runtime tree construction. The tradeoff (no compile-
time optimization) is accepted for the simplicity of "UI is just function calls."

### 10.6 Full CSS-in-Vo

**Why not**: A full CSS-in-Vo solution (like styled-components or Emotion) would require
CSS parser/generator in Vo, which is unnecessary complexity. The combination of utility
classes + inline styles + external CSS files covers all use cases.

---

## Appendix: Framework Comparison

### A.1 Architecture Models

| Framework | Model | State Location | Reactivity |
|-----------|-------|---------------|------------|
| **Vue 3** | Component tree | Per-component (reactive refs) | Fine-grained (Proxy-based) |
| **Svelte 5** | Component tree | Per-component (runes/$state) | Compile-time signals |
| **React** | Component tree | Per-component (useState/hooks) | Coarse (re-render subtree) |
| **Elm** | TEA | Single global Model | Full re-render + vdom diff |
| **SolidJS** | Component tree | Signals (fine-grained) | Fine-grained (no vdom) |
| **Dioxus** | Component tree | Signals + hooks | Fine-grained |
| **Leptos** | Component tree | Signals (fine-grained) | Fine-grained |
| **vogui v1** | TEA | Single global State | Full re-render + morphdom |
| **vogui v2** | TEA + managed | App: global State, UI: JS | Full re-render + morphdom |

### A.2 What We Learn From Each

**From Vue/Svelte**: Component encapsulation matters. Users want to compose self-
contained UI pieces without wiring all the internal state. Our managed components
achieve this by pushing UI behavior to JS.

**From Elm**: TEA scales well for application logic. The single state is a feature
for debugging and time-travel. Don't abandon it for application state.

**From React**: Hooks prove that function-based components work. But hooks also prove
that implicit state ordering is a footgun. We avoid this by keeping local state in JS
where it's managed by the framework, not the user.

**From SolidJS/Leptos**: Fine-grained reactivity is the performance future, but it
requires language/compiler support. Not practical for Vo today. morphdom is good enough.

**From Dioxus**: Rust constraints (ownership, no GC) lead to different tradeoffs than
Go/Vo. Dioxus uses RSX (macro-based templates). Vo uses function calls. Both work.

### A.3 vogui v2 Positioning

vogui v2 aims to be:
- **Simpler than React** (no hooks complexity, no state management libraries)
- **More capable than Elm** (managed components, transitions, theming)
- **Approaching Vue/Svelte UX** (proper animations, keyboard nav, accessibility)
- **Unique to Vo** (leverages ~> dynamic access, TEA simplicity, WASM deployment)

The key differentiator: **TEA simplicity for application logic + managed components
for UI behavior = best of both worlds.**

---

*End of document.*
