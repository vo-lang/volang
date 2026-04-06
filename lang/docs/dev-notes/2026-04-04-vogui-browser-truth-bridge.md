# vogui: Browser Truth Bridge

Date: 2026-04-04

## Problem

The `VirtualScroll` component attempts to predict item heights on the Vo/WASM side
(`ItemHeight` callback, `offsets[]`, `totalHeight`) and use those predictions to drive
scroll position. This creates a **dual-truth problem**:

- **Vo-side truth**: predicted `totalHeight`, `offsets[i]`, `maxScrollTop()`
- **Browser truth**: DOM `scrollHeight`, `clientHeight`, `scrollTop`

In any scenario where these two disagree — variable-height text, font metrics, subpixel
rounding, box-model differences, border widths — you get a feedback loop:

1. Vo predicts layout → emits `scrollToBottom`
2. Browser commits real layout (slightly different height)
3. Browser reports true `scrollTop` back via scroll event
4. Vo reacts, re-emits scroll command
5. Visible jitter

The worst combination is `streaming + variable-height + virtual scroll + bottom-follow +
manual near-bottom scroll`. This is exactly what the chat demo does, and it is why
`VirtualScroll` jitters there.

## Root Cause

The wrong abstraction boundary. **The browser is a complete layout engine.** Vo should not
replicate it. Vo should own state and structure; the browser should own layout and physics.
The bridge between them should be:

- **Observation channels**: browser truth → Vo (resize, scroll state, intersection)
- **Command channels**: Vo → browser (scroll to, focus, etc.)

## Architecture: Three Layers

```
┌─────────────────────────────────────────────────┐
│  Layer 3: Host Widgets                          │
│  JS-native complex components                   │
│  (virtual list, rich editor, chart, map...)     │
│  Vo only configures & observes                  │
├─────────────────────────────────────────────────┤
│  Layer 2: Observation & Command Bridge  ← THIS  │
│  ObserveResize, ObserveScroll,                  │
│  ObserveIntersect, ScrollTo*, Focus...          │
│  Browser truth flows UP, commands flow DOWN     │
├─────────────────────────────────────────────────┤
│  Layer 1: Declarative Core (current vogui)      │
│  Node tree, Style, Component, Event, Theme,     │
│  Canvas, Router, Timer                          │
└─────────────────────────────────────────────────┘
```

Layer 1 is solid. This note covers **Layer 2** implementation.

## Layer 2: New APIs

### Vo side (`event.vo`)

```vo
// ScrollEvent is the browser-reported scroll state for a scroll container.
// All fields are real DOM values, not predictions.
type ScrollEvent struct {
    ScrollTop    float64
    ClientHeight float64
    ScrollHeight float64
    AtBottom     bool    // scrollHeight - clientHeight - scrollTop <= 1px
    BottomGap    float64 // pixels remaining to true bottom
}

// IntersectEvent is reported when a sentinel element enters/leaves the viewport.
type IntersectEvent struct {
    IsIntersecting   bool
    IntersectionRatio float64
}

// OnScrollState creates a handler that receives ScrollEvent on scroll.
// Action: func(s *State, e ScrollEvent)
func OnScrollState(action any) Handler

// OnIntersect creates a handler backed by IntersectionObserver.
// Action: func(s *State, e IntersectEvent)
func OnIntersect(action any) Handler
```

### Vo side (`node.vo`)

```vo
// ObserveScroll attaches an OnScrollState-backed handler to a scroll container.
func (n Node) ObserveScroll(h Handler) Node

// ObserveIntersect attaches an IntersectionObserver-backed handler to a node.
func (n Node) ObserveIntersect(h Handler) Node
```

`ObserveResize` already exists on `Node` (line 322 of node.vo). No change needed.

### JS side (`events.ts` / `renderer.tsx`)

- `propsToHandlers`: when `props.onScrollState != null`, install a scroll listener that
  computes `AtBottom` and `BottomGap` from real DOM values before emitting.
- `renderGenericElement`: when `props.onIntersect != null`, install an `IntersectionObserver`
  (same pattern as `ResizeObserver` for `onResize`).

## Implementation Plan

### Step 1: `event.vo` — add types + handlers

- `handlerScroll = 16`, `handlerIntersect = 17`
- `ScrollEvent`, `IntersectEvent` structs
- `OnScrollState`, `OnIntersect` constructors
- `invokeHandler` cases for both

### Step 2: `node.vo` — add node methods

- `ObserveScroll(h Handler) Node` → `setProp("onScrollState", h.ID)`
- `ObserveIntersect(h Handler) Node` → `setProp("onIntersect", h.ID)`

### Step 3: `js/src/events.ts` — rich scroll payload

In `propsToHandlers`, handle `onScrollState`:
```typescript
if (props.onScrollState != null) {
    const id = props.onScrollState;
    handlers.onScroll = (e: Event) => {
        const t = e.currentTarget as HTMLElement;
        const scrollTop = t.scrollTop;
        const scrollHeight = t.scrollHeight;
        const clientHeight = t.clientHeight;
        const browserMax = Math.max(0, scrollHeight - clientHeight);
        const bottomGap = Math.max(0, browserMax - scrollTop);
        emit(id, JSON.stringify({
            ScrollTop: scrollTop,
            ScrollHeight: scrollHeight,
            ClientHeight: clientHeight,
            AtBottom: bottomGap <= 1.0,
            BottomGap: bottomGap,
        }));
    };
}
```

### Step 4: `js/src/renderer.tsx` — IntersectionObserver binding

Detect `props.onIntersect != null` in `renderGenericElement`, install `IntersectionObserver`.
Follow exact same pattern as `ResizeObserver` binding (lines 285-337 of renderer.tsx).

### Step 5: `gui_chat.vo` — remove VirtualScroll

Replace `VirtualScroll` with:
- `followTail bool` on State (init to `true`)
- `vogui.Scroll(items...).Ref("chat-scroll").ObserveScroll(...)` container
- Scroll handler sets `s.followTail = e.AtBottom`
- `tick()` calls `ScrollToBottomRef("chat-scroll")` when `followTail`
- Top/Bottom buttons use `ScrollTo` / `ScrollToBottomRef` directly

Remove: `itemHeight`, `bubbleMaxWidth`, `itemTextWidth`, VirtualScroll config, all
`vs.Recompute()` / `vs.SetItemCount()` / `vs.ScrollToBottom()` calls.

## Why This Fixes the Jitter

The chat demo no longer has a predicted `totalHeight`. The scroll container is a plain
browser-managed DOM node. The browser decides its own `scrollHeight`. Vo only reads what
the browser reports (`AtBottom`, `BottomGap`) and issues commands (`ScrollToBottomRef`)
based on intent (`followTail`). No feedback loop, no dual truth.

## Layer 2 additions: MeasureRef (Phase 2a) ✅

One-shot `getBoundingClientRect` bridge. Vo requests a measurement, JS measures
after DOM commit, result flows back as a `Rect` callback.

```vo
// Vo side (measure.vo)
RequestMeasure("anchor", func(s *State, r Rect) {
    s.tooltipX = r.X + r.Width/2
    s.tooltipY = r.Bottom + 8
})
```

Flow: Vo queues refAction(cmd="measure", measureID=N) → JS executes
`getBoundingClientRect` in useLayoutEffect → emits event(ID=-6, {ID:N, Rect:…})
→ Vo receives, invokes one-shot callback, deletes it.

Persistent callback registry (`measureCallbacks` map) — NOT reset between renders,
same pattern as `timerHandlers`.

## Layer 2 additions: Smooth Scroll (Phase 2b) ✅

Three new smooth-scroll ref commands:

```vo
ScrollToSmooth(refName, top)       // el.scrollTo({ top, behavior: 'smooth' })
ScrollToBottomSmooth(refName)      // smooth scroll to true DOM bottom
ScrollIntoViewSmooth(refName)      // el.scrollIntoView({ behavior: 'smooth' })
```

## Layer 3: Host Widget Protocol (Phase 3a) ✅

Upgraded `ExternalWidget` → `HostWidget` with children support and cleaner API:

```vo
// New API — typed config + child slots
HostWidget("markdown", map[string]any{"content": text})
HostWidget("virtual-list", config, renderedItems...).ObserveWidget(handler)

// ObserveWidget chains event handling onto any HostWidget node
func (n Node) ObserveWidget(h Handler) Node
```

Children passed to `HostWidget` are rendered as Preact-managed nodes inside the
widget container. The JS factory coexists with them — it can ignore, wrap, or
reposition the slot content.

`ExternalWidget` remains for backward compat.

## VirtualList: JS-native Preact Component (Phase 3b) ✅

First built-in host widget. Unlike the old `VirtualScroll` (which predicts heights
in Vo), `VirtualList` delegates all scroll physics to JS/browser:

```vo
// Vo side (virtual_list.vo)
visible := make([]vogui.Node, 0, s.rangeEnd-s.rangeStart)
for i := s.rangeStart; i < s.rangeEnd && i < len(s.items); i++ {
    visible = append(visible, renderItem(i).Key(fmt.Sprint(i)))
}
return vogui.VirtualList(vogui.VirtualListConfig{
    TotalCount:   len(s.items),
    ItemHeight:   40,
    VisibleStart: s.rangeStart,
}, visible...).
    Flex(1).
    OnRange(vogui.OnVirtualRange(func(st *State, e vogui.RangeEvent) {
        st.rangeStart = e.Start
        st.rangeEnd = e.End
    }))
```

JS side (`VgVirtualList.tsx`): Preact component registered as `vo-virtual-list`.
Creates a scroll viewport with a spacer element sized to `totalCount * itemHeight`.
Children (visible items from Vo) are absolutely positioned at `visibleStart * itemHeight`.
On scroll, computes new Start/End range and emits via `onRange` handler.

No dual-truth: JS owns scroll physics, Vo only renders what JS tells it to.

## Future: More Host Widgets

- **MarkdownView** — render markdown with syntax highlighting (needs `marked` dep)
- **CodeEditor** — lightweight code editing with language support
- **Chart** — data visualization

## Transition & TransitionGroup (Phase 4) ✅

### VgTransition — enter/leave for conditional children

Real CSS animation for `vo-transition`. When child appears → enter animation
(from-state → to-state). When child disappears → keeps DOM alive for leave
animation → removes after transitionend.

```vo
Transition("fade", If(s.showTooltip, tooltipNode))
Transition("slide-up", If(s.menuOpen, menuNode))
```

Built-in transitions: fade, scale, slide-down, slide-up, slide-left, slide-right.
Each defines enterFrom/enterTo/leaveFrom/leaveTo CSS states with configurable
duration and easing. Fallback timeout ensures cleanup even if transitionend
doesn't fire.

### VgTransitionGroup — FLIP + enter/move/leave for keyed lists

Full animation lifecycle for `vo-transition-group`:

- **ENTER**: New children fade+scale in
- **MOVE**: Repositioned children animate via FLIP (First-Last-Invert-Play)
- **LEAVE**: Removed children stay in DOM with `pointerEvents:none`, play
  fade-out animation, then are removed via `setLeavingChildren` state cleanup

```vo
TransitionGroup("fade", Row(Gap(8),
    items.Map(func(item Item) Node {
        return Card(item.Name).Key(item.ID)
    })...,
))
```

Leave detection: component tracks `prevKeysRef` and `prevChildMapRef` across
renders. When a key disappears, its last-rendered VNode is kept in a
`leavingChildren` state array. After the leave animation completes, the entry
is removed from the array.

Configurable timing per transition name (fade=200ms, scale=250ms, etc.).

## Files Changed

| File | Change |
|------|--------|
| `vogui/event.vo` | +ScrollEvent, +IntersectEvent, +OnScrollState, +OnIntersect, +invokeHandler cases |
| `vogui/node.vo` | +ObserveScroll, +ObserveIntersect |
| `vogui/measure.vo` | NEW: Rect, RequestMeasure, invokeMeasureCallback |
| `vogui/widget.vo` | +HostWidget (children), +ObserveWidget, ExternalWidget kept for compat |
| `vogui/ref.vo` | +MeasureID fields, +ScrollToSmooth, +ScrollToBottomSmooth, +ScrollIntoViewSmooth |
| `vogui/encode.vo` | Encode measureID in refAction wire format |
| `vogui/app.vo` | +eventIDMeasure case in processEvent |
| `vogui/gui.vo` | Updated API documentation for all new features |
| `vogui/js/src/events.ts` | +onScrollState handler in propsToHandlers |
| `vogui/js/src/renderer.tsx` | +IntersectionObserver binding, +children support in ExternalWidgetHost |
| `vogui/js/src/refs.ts` | +measure, +scrollToSmooth, +scrollToBottomSmooth, +scrollIntoViewSmooth |
| `vogui/js/src/types.ts` | +measure/smooth commands in RefActionCommand, +measureId in RefAction |
| `vogui/js/src/decoder.ts` | +measure/smooth in command validation, +measureId decoding |
| `vogui/virtual_list.vo` | NEW: VirtualList, VirtualListConfig, RangeEvent, OnVirtualRange |
| `vogui/js/src/components/VgVirtualList.tsx` | NEW: Preact virtual list component with scroll-driven range |
| `vogui/js/src/components/index.ts` | Register VgVirtualList + VgTransition + VgTransitionGroup |
| `vogui/js/src/components/VgTransition.tsx` | NEW: enter/leave CSS animation for conditional children |
| `vogui/js/src/components/VgTransitionGroup.tsx` | NEW: FLIP + enter/move/leave animation for keyed lists |
| `vogui/js/src/mapping.ts` | Remove stale transition-all class (now Preact components) |
| `vogui/virtual_scroll.vo` | DELETED: replaced by VirtualList + ObserveScroll |
| `vogui/tests/main.vo` | Rewritten: VirtualScroll tests → new API struct/node tests |
| `volang/studio/…/gui_chat.vo` | Remove VirtualScroll, use ObserveScroll + followTail |
