# VoGUI v4: Preact Renderer + Radix UI + Tailwind CSS

> Complete renderer rewrite to achieve web-framework-grade expressiveness while keeping
> the Vo API zero-web-knowledge.
> Date: 2026-03-02

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Current State (v2/v3)](#2-current-state-v2v3)
3. [Design Goals](#3-design-goals)
4. [Architecture](#4-architecture)
5. [Tech Stack Selection](#5-tech-stack-selection)
6. [Layer 1: Vo API Design](#6-layer-1-vo-api-design)
7. [Layer 2: Render Protocol](#7-layer-2-render-protocol)
8. [Layer 3: Preact Renderer](#8-layer-3-preact-renderer)
9. [Layer 4: Component Library](#9-layer-4-component-library)
10. [Layer 5: Design Token & Styling](#10-layer-5-design-token--styling)
11. [Implementation Plan (Fresh Start)](#11-implementation-plan-fresh-start)
12. [Risk Assessment](#12-risk-assessment)
13. [Appendix: Full Component Inventory](#appendix-full-component-inventory)

---

## 1. Executive Summary

### Problem

VoGUI v2/v3 achieved a working two-layer state model, binary render protocol, canvas 2D API,
style combinators, and a component model with memo. However the **JS renderer** remains a
hand-rolled morphdom-based system with ~1600 lines of imperative DOM manipulation. Every
new component (Tooltip, DropdownMenu, Combobox, etc.) requires writing 50-200 lines of
custom DOM-building + event-wiring code in `renderer.ts`. This approach:

- **Does not scale**: Adding a proper Select with keyboard nav, ARIA, search, virtual scroll
  requires 500+ lines of hand-written JS. We already have 17 managed component renderers
  and each is incomplete in accessibility and edge-case handling.
- **Lacks ecosystem leverage**: The entire React/Preact ecosystem of battle-tested headless
  components (Radix UI, Floating UI, etc.) is inaccessible.
- **Styling is limited**: `styles.ts` is 30KB of hand-written CSS. Adding responsive variants,
  dark mode, animations, and new component styles means growing this file indefinitely.

### Proposed Solution

Replace the morphdom renderer with **Preact** (3KB React-compatible runtime) as the
rendering engine. This unlocks:

- **Radix UI** (via `preact/compat`) for complex interactive components
- **Tailwind CSS** (pre-compiled) for comprehensive, utility-based styling
- **Preact component model** for cleaner renderer code

The Vo side (`libs/vogui/*.vo`) changes are **minimal** — `VoNode` tree structure stays the
same, binary encode stays the same. The change is entirely in `libs/vogui/js/`.

### Key Metrics

| Metric | Current (morphdom) | Target (Preact) |
|--------|-------------------|-----------------|
| renderer.ts LOC | ~1620 | ~400 (mapping layer) |
| styles.ts LOC | ~900 | 0 (Tailwind replaces it) |
| Component behavior code | Hand-written per component | Radix UI / headless |
| ARIA compliance | Partial (~30%) | Full (Radix handles it) |
| Bundle size (renderer) | ~65KB (morphdom + custom CSS) | ~80KB (Preact + Radix + Tailwind) |
| New component effort | 50-200 lines JS + CSS | Vo wrapper + Preact component (~30 lines) |

### Licensing

All dependencies are MIT-licensed, compatible with the project's MIT license:

- Preact: MIT
- Radix UI (`@radix-ui/*`): MIT
- Tailwind CSS: MIT
- Lucide Icons: ISC (MIT-equivalent)
- Floating UI: MIT

---

## 2. Current State (v2/v3)

### What v2 delivered

- Two-layer state: Vo app state + JS UI behavior state
- Managed components: Dialog, Drawer, Tooltip, Popover, DropdownMenu, Tabs, Accordion
- Theme system with CSS custom properties (`--vo-primary`, etc.)
- Event modifiers: Prevent, Stop, Debounce, Throttle
- Refs, Portals, UnsafeHTML
- Component model with `Mount()`, `ShouldUpdate()` memo, lifecycle

### What v3 delivered

- Binary render protocol (5-10x smaller than JSON)
- Canvas 2D command buffer API
- Style combinator system (`NewStyle().W(100).Bg("#fff").Hover(...)`)
- Component subtree caching (`__comp__` / `__cached__` markers)

### Current Vo-side file inventory

```
node.vo        — Node struct, style methods (W, H, P, M, Gap, Flex, etc.)
layout.vo      — Row, Column, Center, Stack, Grid, Spacer, Divider, Scroll, Wrap
display.vo     — Text, H1-H6, P, Code, Pre, Link, Badge, Tag, Progress, Spinner, Alert, Avatar, Icon
input.vo       — Button, Input, Password, TextArea, Checkbox, Switch, Radio, Select, Slider, etc.
form.vo        — Form, FormField, FormError, FormHelp, FormSection
nav.vo         — Tabs, Accordion, Breadcrumb, Pagination, Steps, Nav, Sidebar
overlay.vo     — Dialog, Drawer, Tooltip, Popover, DropdownMenu, HoverCard, Collapsible, ContextMenu
event.vo       — Handler registration, modifiers, invocation dispatch
component.vo   — Mount(), component lifecycle, memo
style.vo       — Style combinator (NewStyle, Extend, Hover, Focus, Media)
theme.vo       — Theme struct, DefaultTheme, DarkTheme, CSS custom properties
encode.vo      — Binary encoding of VoNode tree
canvas.vo      — Canvas 2D command buffer
transition.vo  — Transition, TransitionGroup
timer.vo       — SetInterval, SetTimeout
router.vo      — Route, Navigate
gui.vo         — App entry point (gui.Run)
app.vo         — App lifecycle
```

### Current JS-side renderer architecture

```
decoder.ts     — Binary → RenderMessage (VoNode tree)
renderer.ts    — morphdom-based: VoNode → DOM
                 - renderNode(): 60-line switch on node.type
                 - createElementForType(): 120-line switch
                 - applyProps(): 120-line type-specific prop handling
                 - 17 managed component renderers (renderTabs, renderTooltip, etc.)
                 - attachHandlers(): click/change/input/submit binding
                 - morphdom onBeforeElUpdated hooks for widget/canvas/cached preservation
styles.ts      — 900 lines of hand-written CSS injected into <style>
canvas.ts      — Canvas 2D command execution
types.ts       — VoNode, VoHandler, RenderMessage, StylePropertyMap, WidgetFactory
index.ts       — Public exports
```

### Pain points

1. **renderer.ts is monolithic**: Adding ContextMenu required ~150 lines. Combobox ~200 lines.
   Each managed component is a self-contained DOM builder with its own keyboard handling,
   ARIA attributes, positioning logic, and event wiring. All incomplete.

2. **styles.ts is fragile**: Hand-written CSS with no design system. Adding a variant to Button
   means finding the right CSS rule, duplicating selectors, hoping nothing breaks. No
   responsive utilities, no dark mode toggle, no animation library.

3. **Accessibility is superficial**: Some ARIA roles set, but no focus trap, no roving
   tabindex, no screen reader testing. Radix UI provides this for free.

4. **No component encapsulation on JS side**: All 17 render functions share global state
   (`currentHandlers`, `currentGen`). Preact components get proper encapsulation via
   hooks/state.

---

## 3. Design Goals

### G1: User writes only Vo — zero web knowledge required

```vo
// This is ALL the user writes. No HTML, no CSS, no JS, no npm, no build step.
package main

import "vogui"

func main() {
    vogui.Run(myApp)
}

func myApp(state *AppState) vogui.Node {
    return vogui.Card(
        vogui.H2("Hello"),
        vogui.Button("Click", vogui.On(func(s *AppState) { s.Count++ })),
    ).P(24)
}
```

### G2: Expressiveness approaching shadcn/ui

- Every component shadcn/ui offers should have a vogui equivalent
- Variants (primary/secondary/destructive/outline/ghost)
- Sizes (sm/md/lg)
- Dark/light theme switching
- Animations and transitions
- Responsive layout

### G3: Renderer is invisible infrastructure

- Preact, Radix, Tailwind are **never exposed** to the user
- The entire JS bundle is compiled once and shipped with Studio/Playground
- User apps are `.vo` files only

### G4: Vo-side API changes are minimal

- `VoNode` struct stays the same: `{Type, Props, Children}`
- Binary encode stays the same
- Event/handler system stays the same
- New: `.Variant()`, `.Size()` methods on Node (already partially exists)
- New: expanded theme tokens

### G5: Clean-slate JS renderer

- Old `libs/vogui/js/` renamed to `libs/vogui/js-reference/` as reference
- New `libs/vogui/js/` built from scratch with Preact + Radix + Tailwind
- No migration code, no temporary adapters, no morphdom
- Same public API surface (`render`, `decodeBinaryRender`, `injectStyles`) so Studio integration is unchanged

---

## 4. Architecture

### Current

```
Vo App
  │
  ▼
view(state) → VoNode tree → binary encode → Uint8Array
  │
  ▼
JS: decodeBinary() → RenderMessage { tree: VoNode, handlers: VoHandler[] }
  │
  ▼
renderer.ts: renderNode(VoNode) → HTMLElement (imperative DOM building)
  │
  ▼
morphdom(container, newDOM) → DOM patch
```

### Proposed

```
Vo App
  │
  ▼
view(state) → VoNode tree → binary encode → Uint8Array
  │
  ▼
JS: decodeBinary() → RenderMessage { tree: VoNode, handlers: VoHandler[] }
  │
  ▼
renderer.tsx: voNodeToPreact(VoNode) → Preact VNode
  │
  ▼
Preact reconciler → DOM patch
  │
  ├── Simple nodes: <div>, <span>, <button> with Tailwind classes
  ├── Styled components: VgButton, VgCard, VgBadge (Tailwind + variant logic)
  └── Complex components: VgSelect, VgDialog, VgTooltip (Radix UI primitives)
```

### Key difference

The VoNode → DOM translation moves from **imperative DOM building + morphdom diffing** to
**declarative Preact VNode tree + Preact reconciler**. This is a drop-in replacement at the
`render()` function boundary — everything upstream (Vo, binary encode, decode) is unchanged.

---

## 5. Tech Stack Selection

### Why Preact over React

| | Preact | React |
|---|---|---|
| Bundle size | **3KB** gzip | 45KB gzip |
| React API compat | ✓ via `preact/compat` | Native |
| Radix UI support | ✓ via compat alias | ✓ |
| Performance | Slightly faster (smaller vdom) | Excellent |
| Hooks | ✓ Full support | ✓ |

Preact is 15x smaller with equivalent functionality. Since the renderer is bundled into
Studio (not shipped to end users individually), absolute size matters less — but there's
no reason to pay the React tax when Preact is API-compatible.

### Why Radix UI

- **Most complete** headless component library: 30+ primitives
- **Best accessibility**: WAI-ARIA compliant, keyboard navigation, focus management
- **Unstyled**: We control all visual presentation via Tailwind
- **MIT licensed**
- **Works with Preact** via `preact/compat` aliasing

Alternatives considered:
- **Zag.js**: Framework-agnostic state machines. Fewer components, less battle-tested.
  Good fallback if Radix + preact/compat has issues.
- **Headless UI**: Tailwind Labs, but React/Vue only. Fewer components than Radix.
- **Ariakit**: React-based. Fewer components than Radix.

### Why Tailwind CSS (pre-compiled)

- **Utility-first**: Every visual property is a class, composable at render time
- **Design tokens built in**: Colors, spacing, typography, shadows, border-radius
- **Dark mode**: `dark:` prefix, trivial to implement
- **Responsive**: `sm:`, `md:`, `lg:` breakpoint prefixes
- **Pre-compile once**: Generate a static CSS file containing all classes used by vogui
  components. Ship with Studio. Never changes at runtime.
- **MIT licensed**

Alternative considered:
- **Hand-written CSS** (current): Does not scale. 900 lines already and growing.
- **CSS Modules**: Build-step complexity, no utility system.
- **UnoCSS**: Tailwind-compatible but less ecosystem.

### Why NOT Svelte for renderer

Svelte's compilation model is **static** — components are compiled at build time from
`.svelte` files. VoGUI's VoNode tree is **dynamic** (generated at runtime by Vo). Svelte
has no equivalent of `React.createElement(type, props, children)` for runtime tree building.
Svelte is correct for Studio's own UI, but fundamentally wrong for vogui's renderer.

---

## 6. Layer 1: Vo API Design

### 6.1 Core principle: method chaining on Node

Every component returns a `Node`. Styling and behavior are applied via method chains.
No new types needed — the existing `Node` struct with `Props map[string]any` carries
everything.

```vo
// Existing pattern (stays the same):
vogui.Button("Save", handler).Variant("primary").Size("lg").Disabled(saving)

// New convenience methods (add to node.vo):
func (n Node) Variant(v string) Node  { return n.setProp("variant", v) }
func (n Node) Size(s string) Node     { return n.setProp("size", s) }
func (n Node) Color(c string) Node    { return n.setProp("color", c) }
func (n Node) Weight(w string) Node   { return n.setProp("weight", w) }
func (n Node) Shadow(s string) Node   { return n.setProp("shadow", s) }
func (n Node) Rounded(r string) Node  { return n.setProp("rounded", r) }
func (n Node) Truncate(lines int) Node { return n.setProp("truncate", lines) }
```

### 6.2 Semantic variants replace raw CSS

**Before (v2/v3):**
```vo
vogui.Button("Delete", handler).Bg("#ef4444").Color("#fff").Rounded(6).Px(16).H(36)
```

**After (v4):**
```vo
vogui.Button("Delete", handler).Variant("destructive").Size("md")
```

The renderer maps `variant="destructive"` to the appropriate Tailwind classes. The Vo user
never knows Tailwind exists.

### 6.3 Complete Vo API surface

No changes to existing function signatures. Only additions:

**New Node methods** (add to `node.vo`):
```vo
func (n Node) Variant(v string) Node    // "primary", "secondary", "destructive", "outline", "ghost"
func (n Node) Size(s string) Node       // "xs", "sm", "md", "lg", "xl"
func (n Node) Color(c string) Node      // "primary", "success", "warning", "danger", "muted"
func (n Node) Weight(w string) Node     // "normal", "medium", "semibold", "bold"
func (n Node) Shadow(s string) Node     // "none", "sm", "md", "lg", "xl"
func (n Node) Rounded(r string) Node    // "none", "sm", "md", "lg", "full"
func (n Node) Truncate(lines int) Node  // text truncation with ellipsis
func (n Node) Animate(a string) Node    // "fade-in", "slide-up", "spin", etc.
```

**New theme tokens** (extend `theme.vo`):
```vo
type Theme struct {
    // Colors (existing)
    Primary, Secondary, Success, Danger, Warning, Info string
    Background, Surface, Text, TextMuted, Border       string

    // New: extended palette
    Accent        string
    Card          string
    Popover       string
    Ring          string   // focus ring color

    // New: sizing tokens
    RadiusSm      string   // "4px"
    RadiusMd      string   // "6px"
    RadiusLg      string   // "8px"
    RadiusFull    string   // "9999px"

    // Existing
    FontFamily    string
}
```

### 6.4 What does NOT change on Vo side

- `Node` struct: `{Type string, Props map[string]any, Children []Node}` — unchanged
- Binary encode protocol — unchanged
- Event/Handler system — unchanged
- Component model (`Mount`, lifecycle, memo) — unchanged
- Canvas 2D API — unchanged
- Router, Timer — unchanged
- Style combinator system — unchanged (still works, but semantic variants are preferred)

---

## 7. Layer 2: Render Protocol

**No changes.** The binary protocol from v3 is retained:

```
Vo App → VoNode tree → binary encode → Uint8Array → JS decode → RenderMessage
```

The `RenderMessage` interface stays the same:

```ts
interface RenderMessage {
    type: 'render';
    gen: number;
    tree: VoNode;
    handlers: VoHandler[];
    styles?: string[];
    canvas?: CanvasBatch[];
}
```

The only change is what consumes `RenderMessage.tree` — Preact instead of morphdom.

---

## 8. Layer 3: Preact Renderer

### 8.1 Core rendering function

Replace the entire morphdom-based `_renderTree()` with a single Preact root:

```tsx
// renderer.tsx — the new core (~100 lines replaces ~800 lines)
import { h, render as preactRender } from 'preact';
import { useRef, useEffect } from 'preact/hooks';

let preactRoot: HTMLElement | null = null;

export function render(container: HTMLElement, msg: RenderMessage, config: RendererConfig) {
    if (!msg?.tree) {
        preactRender(null, container);
        return;
    }

    // Store handlers/config in module scope for event emission
    setRenderContext(msg.gen, msg.handlers, config);

    // Inject theme CSS custom properties
    applyThemeVars(container);

    // Render VoNode tree as Preact VNodes
    preactRender(h(VoTreeRoot, { tree: msg.tree, canvas: msg.canvas }), container);
}

function VoTreeRoot({ tree, canvas }: { tree: VoNode; canvas?: CanvasBatch[] }) {
    const ref = useRef<HTMLDivElement>(null);

    useEffect(() => {
        if (canvas && ref.current) {
            for (const batch of canvas) {
                executeCanvasBatch(batch, refRegistry);
            }
        }
    }, [canvas]);

    return h('div', { ref, style: { display: 'contents' } }, voNodeToVNode(tree));
}
```

### 8.2 VoNode → Preact VNode mapping

```tsx
// mapping.tsx — the core translation (~200 lines replaces ~600 lines)
import { h, VNode } from 'preact';
import { componentMap } from './components';

function voNodeToVNode(node: VoNode): VNode | null {
    if (!node?.type) return null;

    const { type, props = {}, children = [] } = node;

    // Fragment
    if (type === 'Fragment') {
        return h('div', { style: 'display:contents' }, children.map(voNodeToVNode));
    }

    // Component markers (memo optimization)
    if (type === '__comp__' || type === '__cached__') {
        return renderComponentMarker(node);
    }

    // Look up registered Preact component for this node type
    const Component = componentMap[type];
    if (Component) {
        return h(Component, {
            ...props,
            voChildren: children,
            key: props.key,
        });
    }

    // Fallback: render as HTML element with Tailwind classes
    return renderGenericElement(type, props, children);
}

function renderGenericElement(type: string, props: Record<string, any>, children: VoNode[]): VNode {
    const tag = typeToTag(type);               // "vo-row" → "div"
    const className = typeToClass(type, props); // "vo-row" → "flex flex-row"
    const style = propsToStyle(props);          // { width: 100 } → { width: '100px' }
    const eventHandlers = propsToHandlers(props); // { onClick: 5 } → { onClick: () => emit(5, ...) }

    return h(tag, {
        className,
        style,
        ...eventHandlers,
        'data-key': props.key,
        'data-ref': props.ref,
    }, children.map(voNodeToVNode));
}
```

### 8.3 Component registration

```tsx
// components/index.ts — component registry
import { VgButton } from './VgButton';
import { VgInput } from './VgInput';
import { VgSelect } from './VgSelect';
import { VgDialog } from './VgDialog';
import { VgTooltip } from './VgTooltip';
import { VgDropdownMenu } from './VgDropdownMenu';
// ... more

export const componentMap: Record<string, any> = {
    'button':           VgButton,
    'input':            VgInput,
    'textarea':         VgTextarea,
    'select':           VgSelect,
    'vo-dialog':        VgDialog,
    'vo-tooltip':       VgTooltip,
    'vo-dropdown-menu': VgDropdownMenu,
    'vo-popover':       VgPopover,
    'vo-tabs':          VgTabs,
    'vo-accordion':     VgAccordion,
    'vo-combobox':      VgCombobox,
    'vo-context-menu':  VgContextMenu,
    'vo-slider':        VgSlider,
    'vo-checkbox':      VgCheckbox,
    'vo-switch':        VgSwitch,
    'vo-progress':      VgProgress,
    'vo-badge':         VgBadge,
    'vo-alert':         VgAlert,
    'vo-avatar':        VgAvatar,
    'vo-card':          VgCard,
    'vo-sidebar':       VgSidebar,
    'vo-sidebar-item':  VgSidebarItem,
    'Canvas':           VgCanvas,
    // layout types handled by renderGenericElement with class mapping
};
```

### 8.4 Example: VgButton component

```tsx
// components/VgButton.tsx
import { h } from 'preact';
import { cn } from '../utils';

const variantClasses: Record<string, string> = {
    default:     'bg-slate-100 text-slate-900 hover:bg-slate-200',
    primary:     'bg-blue-600 text-white hover:bg-blue-700',
    secondary:   'bg-slate-600 text-white hover:bg-slate-700',
    destructive: 'bg-red-600 text-white hover:bg-red-700',
    outline:     'border border-slate-300 bg-transparent hover:bg-slate-100',
    ghost:       'bg-transparent hover:bg-slate-100',
};

const sizeClasses: Record<string, string> = {
    xs: 'h-7 px-2 text-xs',
    sm: 'h-8 px-3 text-sm',
    md: 'h-9 px-4 text-sm',
    lg: 'h-10 px-6 text-base',
    xl: 'h-12 px-8 text-lg',
};

export function VgButton({ textContent, onClick, variant, size, disabled, voChildren }: any) {
    const cls = cn(
        'inline-flex items-center justify-center rounded-md font-medium',
        'transition-colors focus-visible:outline-none focus-visible:ring-2',
        'focus-visible:ring-blue-500 disabled:opacity-50 disabled:pointer-events-none',
        variantClasses[variant || 'default'],
        sizeClasses[size || 'md'],
    );

    return h('button', {
        className: cls,
        disabled,
        onClick: onClick != null ? () => emit(onClick, '{}') : undefined,
    }, textContent);
}
```

### 8.5 Example: VgSelect component (Radix UI)

```tsx
// components/VgSelect.tsx
import { h } from 'preact';
import * as Select from '@radix-ui/react-select';

export function VgSelect({ value, options, onChange, placeholder, disabled }: any) {
    const items = (options || []) as Array<{ label: string; value: string }>;

    return h(Select.Root, {
        value,
        onValueChange: (v: string) => onChange != null && emit(onChange, JSON.stringify({ Value: v })),
        disabled,
    },
        h(Select.Trigger, {
            className: 'inline-flex items-center justify-between rounded-md border border-slate-300 bg-white px-3 h-9 text-sm hover:bg-slate-50 focus:outline-none focus:ring-2 focus:ring-blue-500 min-w-[180px]',
        },
            h(Select.Value, { placeholder: placeholder || 'Select...' }),
            h(Select.Icon, { className: 'ml-2 text-slate-400' }, '▾'),
        ),
        h(Select.Portal, null,
            h(Select.Content, {
                className: 'bg-white rounded-md shadow-lg border border-slate-200 overflow-hidden z-50',
                position: 'popper',
                sideOffset: 4,
            },
                h(Select.Viewport, { className: 'p-1' },
                    items.map(item =>
                        h(Select.Item, {
                            key: item.value,
                            value: item.value,
                            className: 'relative flex items-center px-2 py-1.5 text-sm rounded cursor-pointer hover:bg-slate-100 focus:bg-slate-100 outline-none data-[highlighted]:bg-slate-100',
                        },
                            h(Select.ItemText, null, item.label),
                        ),
                    ),
                ),
            ),
        ),
    );
}
```

**This replaces ~200 lines of hand-written renderCombobox/renderSelect in renderer.ts with
production-grade keyboard navigation, ARIA, scroll-into-view, and positioning — for free.**

### 8.6 Event emission bridge

The event system stays the same. Preact components call `emit(handlerId, payload)` which
goes through the existing bridge back to Vo:

```tsx
// events.ts — thin adapter (stays largely the same as current)
let currentGen = 0;
let currentHandlers: VoHandler[] = [];
let currentConfig: RendererConfig | null = null;

export function setRenderContext(gen: number, handlers: VoHandler[], config: RendererConfig) {
    currentGen = gen;
    currentHandlers = handlers;
    currentConfig = config;
}

export function emit(handlerId: number, payload: string): void {
    currentConfig?.onEvent?.(handlerId, payload);
}

// Convert VoNode event props to Preact event handlers
export function propsToHandlers(props: Record<string, any>): Record<string, any> {
    const handlers: Record<string, any> = {};

    if (props.onClick != null)  handlers.onClick  = () => emit(props.onClick, '{}');
    if (props.onChange != null)  handlers.onChange  = (e: any) => emit(props.onChange, JSON.stringify({ Value: e.target?.value ?? '' }));
    if (props.onSubmit != null) handlers.onSubmit  = (e: any) => { e.preventDefault(); emit(props.onSubmit, '{}'); };
    // ... other event types

    return handlers;
}
```

### 8.7 Ref and portal handling

```tsx
// Refs: Preact callback refs update the refRegistry
function applyRef(name: string): (el: HTMLElement | null) => void {
    return (el) => {
        if (el) refRegistry.set(name, el);
        else refRegistry.delete(name);
    };
}

// Portals: use Preact's createPortal
import { createPortal } from 'preact/compat';

function VgPortal({ portalName, voChildren }: any) {
    const container = getOrCreatePortal(portalName);
    return createPortal(
        voChildren.map(voNodeToVNode),
        container,
    );
}
```

---

## 9. Layer 4: Component Library

### 9.1 Simple components (Tailwind class mapping only)

These need NO Radix/headless library. The Preact component just maps VoNode props to
Tailwind classes:

| Vo function | Node type | Preact component | Description |
|-------------|-----------|-----------------|-------------|
| `Button()` | `button` | `VgButton` | Variant/size/disabled |
| `Badge()` | `vo-badge` | `VgBadge` | Variant/color |
| `Alert()` | `vo-alert` | `VgAlert` | Type (success/warning/error/info) |
| `Card()` | `vo-card` | `VgCard` | Shadow/padding/rounded |
| `Avatar()` | `vo-avatar` | `VgAvatar` | Size/fallback |
| `Progress()` | `vo-progress` | `VgProgress` | Value/max/variant |
| `Spinner()` | `vo-spinner` | `VgSpinner` | Size/color |
| `Divider()` | `vo-divider` | — | `<hr>` with Tailwind |
| `Spacer()` | `vo-spacer` | — | `<div class="flex-1">` |

### 9.2 Layout components (class mapping)

| Vo function | Tailwind class output |
|-------------|----------------------|
| `Row()` | `flex flex-row` |
| `Column()` | `flex flex-col` |
| `Center()` | `flex items-center justify-center` |
| `Stack()` | `relative` (children absolute) |
| `Grid(n)` | `grid grid-cols-{n}` |
| `Wrap()` | `flex flex-wrap` |
| `Scroll()` | `overflow-auto` |

### 9.3 Complex components (Radix UI)

These use `@radix-ui/react-*` via `preact/compat`:

| Vo function | Radix primitive | Key features |
|-------------|----------------|--------------|
| `Select()` | `@radix-ui/react-select` | Keyboard nav, search, ARIA, portal |
| `Dialog()` | `@radix-ui/react-dialog` | Focus trap, ESC close, overlay |
| `Drawer()` | Custom (dialog + animation) | Slide-in, focus trap |
| `Tooltip()` | `@radix-ui/react-tooltip` | Delay, positioning, portal |
| `Popover()` | `@radix-ui/react-popover` | Click-outside, positioning |
| `DropdownMenu()` | `@radix-ui/react-dropdown-menu` | Sub-menus, keyboard nav |
| `ContextMenu()` | `@radix-ui/react-context-menu` | Right-click trigger |
| `Tabs()` | `@radix-ui/react-tabs` | ARIA tabs, keyboard nav |
| `Accordion()` | `@radix-ui/react-accordion` | Expand/collapse animation |
| `Checkbox()` | `@radix-ui/react-checkbox` | Indeterminate state, ARIA |
| `Switch()` | `@radix-ui/react-switch` | ARIA toggle |
| `Slider()` | `@radix-ui/react-slider` | Range, step, ARIA |
| `Combobox()` | `@radix-ui/react-combobox` | Filter, keyboard nav |
| `HoverCard()` | `@radix-ui/react-hover-card` | Hover trigger, delay |
| `Collapsible()` | `@radix-ui/react-collapsible` | Animated expand |

### 9.4 Future components (post-v4)

| Component | Radix or custom | Priority |
|-----------|----------------|----------|
| `Toast()` | `@radix-ui/react-toast` | High |
| `DataTable()` | Custom (TanStack Table) | High |
| `DatePicker()` | Custom or third-party | Medium |
| `Calendar()` | Custom | Medium |
| `Skeleton()` | Tailwind animation | Low |
| `Sheet()` | Radix dialog variant | Low |

---

## 10. Layer 5: Design Token & Styling

### 10.1 Tailwind pre-compilation

Generate a **static CSS file** containing all Tailwind utility classes used by vogui
components. This file is compiled once at vogui build time (not at user app runtime):

```bash
# During vogui JS build (one-time):
npx tailwindcss -i input.css -o dist/vogui.css --content './src/components/**/*.tsx'
```

The resulting `vogui.css` (~30-50KB gzip) is bundled into Studio/Playground. Users never
see it.

### 10.2 CSS custom properties for theming

Tailwind is configured to use CSS custom properties so the Vo-side `Theme` struct can
override colors at runtime:

```css
/* tailwind.config.js */
theme: {
    extend: {
        colors: {
            primary:    'var(--vo-primary)',
            secondary:  'var(--vo-secondary)',
            success:    'var(--vo-success)',
            danger:     'var(--vo-danger)',
            warning:    'var(--vo-warning)',
            surface:    'var(--vo-surface)',
            border:     'var(--vo-border)',
        },
        borderRadius: {
            DEFAULT: 'var(--vo-radius)',
        },
        fontFamily: {
            sans: 'var(--vo-font-family)',
        },
    },
}
```

The renderer injects CSS variables from the Vo `Theme` struct:

```tsx
function applyThemeVars(container: HTMLElement) {
    const vars = themeFromRenderMessage(); // from msg or default
    container.style.setProperty('--vo-primary', vars.primary);
    container.style.setProperty('--vo-surface', vars.surface);
    // ... etc
}
```

### 10.3 Dark mode

```vo
// Vo side: user chooses theme
vogui.SetTheme(vogui.DarkTheme())
```

The renderer sets `class="dark"` on the container. Tailwind's `dark:` variants activate:

```tsx
// In component:
className="bg-white dark:bg-slate-900 text-slate-900 dark:text-slate-100"
```

### 10.4 Style combinator coexistence

The existing `style.vo` Style combinator system continues to work — it generates CSS
class names injected via the `styles` field in `RenderMessage`. Tailwind classes and
Style combinator classes can coexist on the same element. Users who prefer raw CSS control
keep that option.

---

## 11. Implementation Plan (Fresh Start)

Old renderer renamed to `libs/vogui/js-reference/` as read-only reference.
New `libs/vogui/js/` is built from scratch. No migration, no temporary adapters.

### P0: Scaffold & Core ✅ COMPLETE

| Task | Files | Status |
|------|-------|--------|
| Create `libs/vogui/js/` with `package.json`, Preact, Tailwind | scaffold | ✅ |
| `vite.config.ts` with Preact aliases, library build | config | ✅ |
| `tailwind.config.js` + `vogui.css` with CSS var tokens | config | ✅ |
| `src/types.ts` — VoNode, VoHandler, RenderMessage, RendererConfig, WidgetFactory | types | ✅ |
| `src/decoder.ts` — binary protocol decoder with theme support (flag bit 4) | decoder | ✅ |
| `src/events.ts` — emit(), propsToHandlers(), wrapWithModifiers(), setupKeyHandler() | events | ✅ |
| `src/renderer.tsx` — render(), voNodeToVNode(), widget support, #text, attrs, icon | core | ✅ |
| `src/canvas.ts` — executeCanvasBatch() | canvas | ✅ |
| `src/styles.ts` — injectStyles(), applyTheme(), dark mode toggle | styles | ✅ |
| `src/refs.ts` — ref registry | refs | ✅ |
| `src/mapping.ts` — typeToTag, typeToBaseClass, variantClass, propsToStyle | mapping | ✅ |
| `src/index.ts` — public exports matching + extending removed API surface | entry | ✅ |

### P1: Components ✅ COMPLETE

| Task | Radix? | Status |
|------|--------|--------|
| VgButton (variants, sizes, Tailwind styling) | No | ✅ |
| VgCheckbox, VgSwitch, VgSlider | Yes | ✅ |
| VgSelect | Yes | ✅ |
| VgDialog, VgDrawer | Yes | ✅ |
| VgTooltip, VgPopover, VgHoverCard | Yes | ✅ |
| VgDropdownMenu, VgContextMenu | Yes | ✅ |
| VgTabs, VgAccordion | Yes | ✅ |
| VgCombobox (custom filtering, keyboard nav, ARIA) | Custom | ✅ |
| VgCollapsible | Yes | ✅ |
| VgRadio, VgBreadcrumb, VgPagination, VgSteps | No | ✅ |
| Progress, Spinner, Avatar, Alert, Badge, Card, Panel | No (generic) | ✅ |
| Canvas integration (fullscreen support) | No | ✅ |
| Portal, Ref handling | Preact built-in | ✅ |
| External widget system (registerWidget, destroyWidgets) | No | ✅ |

### P2: Vo API + Integration ✅ COMPLETE

| Task | Status |
|------|--------|
| `.Variant()`, `.Size()`, `.Color()` chainable methods on Node | ✅ |
| Extended `Theme` struct (25+ tokens: foreground variants, accent, card, popover, ring, radius) | ✅ |
| Theme flows through binary protocol (flag bit 4, encode → decode → applyTheme) | ✅ |
| Handler encoding fix (was silently null, now serialized as map with iD/modifiers/keyFilter) | ✅ |
| Event modifiers: prevent, stop, once, debounce, throttle, keyFilter | ✅ |
| Build and verify in Studio | ✅ |
| Build and verify in Playground (Preact aliases added) | ✅ |
| `vo check libs/vogui` passes | ✅ |

### P3: Polish (partial)

| Task | Status |
|------|--------|
| Dark mode toggle (toggleDarkMode, setDarkMode, isDarkMode) | ✅ |
| Animation keyframes (fade, slide, scale, spin in tailwind.config.js) | ✅ |
| Sub-type mapping (dialog-title/content/actions, menu-item/divider, transitions) | ✅ |
| New example: Dashboard app | TODO |
| Documentation: update vogui README | TODO |
| Performance benchmark vs v3 | TODO |

### Build output
- `dist/index.js`: ~299 KB (77.9 KB gzip)
- `dist/style.css`: 19.4 KB (4.2 KB gzip) — fully processed Tailwind utilities

---

## 12. Risk Assessment

### R1: Radix UI + preact/compat incompatibility

**Risk**: Some Radix components may use React internals not covered by preact/compat.

**Mitigation**: Test each Radix primitive individually before committing. If a specific
primitive fails, fall back to Zag.js for that component (framework-agnostic state machines).
The component registry architecture makes this a per-component decision, not all-or-nothing.

### R2: Bundle size growth

**Risk**: Preact (3KB) + Radix (varies per component) + Tailwind CSS may exceed current
~65KB bundle.

**Mitigation**: Tree-shake unused Radix primitives. Tailwind purges unused classes at
compile time. Expected total: ~80KB — acceptable for a bundled runtime.

### R3: Performance regression

**Risk**: Preact reconciler may be slower than morphdom for simple node trees.

**Mitigation**: Preact's reconciler is O(n) with keyed diffing, comparable to morphdom.
The `__comp__`/`__cached__` optimization can be preserved via `shouldComponentUpdate` /
`React.memo`. Benchmark after P0.

### R4: Canvas and WidgetFactory interop

**Risk**: Canvas elements and external WidgetFactory instances need special handling
during Preact reconciliation (cannot be freely replaced).

**Mitigation**: Use Preact `ref` callbacks to register canvas elements. Widget instances
use `useEffect` cleanup for lifecycle. Both patterns are well-established in React/Preact.

### R5: Tailwind class conflicts with user styles

**Risk**: If vogui users use the Style combinator to set inline styles, they might
conflict with Tailwind's utility classes.

**Mitigation**: Inline styles always win over class-based styles in CSS specificity.
This is a non-issue — inline styles from the Style combinator will override Tailwind
classes, which is the expected behavior.

### R6: Removed reference drift

**Risk**: `js-reference/` diverges from understanding as new renderer evolves.

**Mitigation**: `js-reference/` is read-only reference. Once new renderer passes all
example apps, `js-reference/` can be deleted.

---

## 13. Appendix: Full Component Inventory

### Layout (9 components)

| Component | Vo function | Status |
|-----------|------------|--------|
| Div | `Div()` | Exists |
| Row / HStack | `Row()` | Exists |
| Column / VStack | `Column()` | Exists |
| Center | `Center()` | Exists |
| Stack | `Stack()` | Exists |
| Grid | `Grid(cols)` | Exists |
| Spacer | `Spacer()` | Exists |
| Divider | `Divider()` | Exists |
| Scroll | `Scroll()` | Exists |
| Wrap | `Wrap()` | Exists |

### Typography (12 components)

| Component | Vo function | Status |
|-----------|------------|--------|
| Text | `Text()` | Exists |
| H1-H6 | `H1()` ... `H6()` | Exists |
| Paragraph | `P()` | Exists |
| Code | `Code()` | Exists |
| Pre | `Pre()` | Exists |
| Strong | `Strong()` | Exists |
| Em | `Em()` | Exists |
| Link | `Link()` | Exists |

### Input (16 components)

| Component | Vo function | Status | Radix? |
|-----------|------------|--------|--------|
| Button | `Button()` | Exists | No |
| IconButton | `IconButton()` | Exists | No |
| Input | `Input()` | Exists | No |
| Password | `Password()` | Exists | No |
| TextArea | `TextArea()` | Exists | No |
| Checkbox | `Checkbox()` | Exists | Yes |
| Switch | `Switch()` | Exists | Yes |
| Radio | `Radio()` | Exists | Yes |
| Select | `Select()` | Exists | Yes |
| Slider | `Slider()` | Exists | Yes |
| NumberInput | `NumberInput()` | Exists | No |
| DateInput | `DateInput()` | Exists | No |
| TimeInput | `TimeInput()` | Exists | No |
| ColorInput | `ColorInput()` | Exists | No |
| FileInput | `FileInput()` | Exists | No |
| SearchInput | `SearchInput()` | Exists | No |

### Data Display (8 components)

| Component | Vo function | Status | Radix? |
|-----------|------------|--------|--------|
| Badge | `Badge()` | Exists | No |
| Tag | `Tag()` | Exists | No |
| Progress | `Progress()` | Exists | No |
| Spinner | `Spinner()` | Exists | No |
| Alert | `Alert()` | Exists | No |
| Avatar | `Avatar()` | Exists | No |
| Icon | `Icon()` | Exists | No |
| Image | `Image()` | Exists | No |

### Navigation (10 components)

| Component | Vo function | Status | Radix? |
|-----------|------------|--------|--------|
| Tabs | `Tabs()` | Exists | Yes |
| Accordion | `Accordion()` | Exists | Yes |
| Breadcrumb | `Breadcrumb()` | Exists | No |
| Pagination | `Pagination()` | Exists | No |
| Steps | `Steps()` | Exists | No |
| Nav | `Nav()` | Exists | No |
| NavItem | `NavItem()` | Exists | No |
| Sidebar | `Sidebar()` | Exists | No |
| SidebarItem | `SidebarItem()` | Exists | No |
| SidebarSection | `SidebarSection()` | Exists | No |

### Overlay (8 components)

| Component | Vo function | Status | Radix? |
|-----------|------------|--------|--------|
| Dialog | `Dialog()` | Exists | Yes |
| Drawer | `Drawer()` | Exists | Custom |
| Tooltip | `Tooltip()` | Exists | Yes |
| Popover | `Popover()` | Exists | Yes |
| DropdownMenu | `DropdownMenu()` | Exists | Yes |
| HoverCard | `HoverCard()` | Exists | Yes |
| Collapsible | `Collapsible()` | Exists | Yes |
| ContextMenu | `ContextMenu()` | Exists | Yes |

### Form (5 components)

| Component | Vo function | Status |
|-----------|------------|--------|
| Form | `Form()` | Exists |
| FormField | `FormField()` | Exists |
| FormError | `FormError()` | Exists |
| FormHelp | `FormHelp()` | Exists |
| FormSection | `FormSection()` | Exists |

### Control Flow (6 components)

| Component | Vo function | Status |
|-----------|------------|--------|
| If | `If()` | Exists |
| IfElse | `IfElse()` | Exists |
| Show | `Show()` | Exists |
| ForEach | `ForEach()` | Exists |
| ForEachKeyed | `ForEachKeyed()` | Exists |
| ForRange | `ForRange()` | Exists |

### Special (7 components)

| Component | Vo function | Status |
|-----------|------------|--------|
| Mount (Component) | `Mount()` | Exists |
| Canvas | `Canvas()` | Exists |
| Portal | `Portal()` | Exists |
| UnsafeHTML | `UnsafeHTML()` | Exists |
| Transition | `Transition()` | Exists |
| TransitionGroup | `TransitionGroup()` | Exists |
| HostWidget | `Widget()` | Exists |

### Grand total: 81 components

- 81 existing Vo-side components (API stays the same)
- 0 new Vo-side components needed for v4 (only new methods on Node)
- ~25 need Preact component wrappers (the rest are class-mapping only)
- ~15 use Radix UI primitives

---

*End of document.*
