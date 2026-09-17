# Pointer interaction

`On` and `OnWith` support `pointerdown`, `pointermove`, `pointerup`,
`pointercancel`, `pointerover`, `pointerout`, `pointerenter`, `pointerleave`,
`gotpointercapture` and `lostpointercapture`. They use native propagation and
the same ordered, bounded input queue as other events.

For a native `PointerEvent`, `Event.Pointer` is a `*ui.PointerData` snapshot:

| Field | Meaning |
| --- | --- |
| `ID` | Native pointer identity; keep it to distinguish simultaneous pointers. |
| `ClientX`, `ClientY` | Finite `float64` coordinates in CSS pixels relative to the viewport. |
| `Buttons` | The native bitmask of currently pressed buttons. |
| `Pressure` | The native pressure value, including the browser's device fallback. |
| `IsPrimary` | Whether the pointer is primary for its device type. |

`Event.Button` describes the button whose state changed; `Event.PointerType`
contains the native device type. `Event.Target` identifies the listener owner.
The snapshot retains its values when the DOM or pointer subsequently moves.
Keyboard, input and other events have a nil pointer. A constructed generic
`Event("pointermove")` also has nil; handlers should check before dereferencing.
Native click events expose a pointer snapshot only when their browser interface
is `PointerEvent`. Mouse-only interfaces do not acquire fabricated coordinates.

Declare `EventOptions{CapturePointer: true}` on a `pointerdown` listener to
request capture on its owning element before guest work is queued. Capture
keeps subsequent movement/up events on the element when the pointer leaves its
bounds. This is independent of `EventOptions.Capture`, which selects the DOM
capture phase. Other event kinds reject `CapturePointer` before any DOM changes.
`gotpointercapture` confirms native capture; an unavailable native request still
delivers the down event. Constructed events cannot create an active device capture.

The browser releases capture on pointer up/cancel. Handle both `pointercancel`
and `lostpointercapture` to end component drag state. Capture follows retained
elements across keyed and Portal placement. Removing a capture declaration,
disposing its node or closing its root releases owned captures, including when
HTML is retained during a handoff. Rebinding a still-enabled handler preserves
capture. A later native owner takes precedence over the former owner.

The [resizable preview example](examples/interaction/pointer.vo) tracks one
active pointer, bounds its width, and supports arrows, Home and End through a
labelled separator. Its CSS declares `touch-action: none` on the drag handle;
applications should choose the appropriate native scrolling/zooming behavior
for their surface. Pointer capture alone does not prevent native scrolling.
Framework handlers start after activation; early pointer-down gestures are not
replayed during HTML adoption.

The example keeps its width in scalar state and its active gesture in an opaque
`ui.State` store. Development reload preserves width and creates fresh gesture
ownership. Native capture and an in-progress drag cannot transfer to a new root.
No framework-specific reload hook is needed.

The host installs a loss listener only on an element holding an owned capture.
It adds no document-wide movement listener, animation loop, coalescing or
unbounded queue. The optional record adds one presence byte to non-pointer
events and 41 payload bytes when present. The experimental wire is v19; rebuild
guest and host together. Finite floats, optional presence and complete frames
are validated by generated codecs on both sides.

The contract follows [W3C Pointer Events](https://www.w3.org/TR/pointerevents3/).
Automated tests exercise real mouse capture in Chromium, Firefox and WebKit,
plus constructed fractional pen/cancellation payloads. Actual touch/pen hardware,
operating-system gestures and assistive technology need their own device checks.

## Continuous position delivery

`OnWith("pointermove", ui.EventOptions{Latest: true}, handler)` permits the host to
merge consecutive pending samples from the same listener, phase, pointer and
button/modifier state. The final absolute position is retained. Any intervening
event is a barrier; pointer up/cancel, clicks, native prevention and capture keep
their existing semantics. Use ordinary `On` for drawing paths or handlers that
need every sample. This option does not impose a frame-rate guarantee. Bounded
input batches yield between turns even when FIFO delivery is selected.
