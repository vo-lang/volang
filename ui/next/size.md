# Native size observations

`view.OnSize(func(ui.Size))` observes the first CSS border-box fragment of an HTML
element. `Size.Inline` and `Size.Block` are fractional logical CSS pixels,
including padding and borders, before transforms. They follow the element's
writing mode. Use `ElementRef.Measure` for a transformed physical bounding
rectangle and `OnViewport` for native scroll offsets and client dimensions.

```vo
ui.Element("div", content).OnSize(func(size ui.Size) {
    // This is an ordinary UI handler; owned state can be updated here.
    inlineSize.Set(fmt.Sprint(size.Inline))
})
```

The browser coalesces a resize burst into one latest value per animation frame.
An unchanged value creates no guest event. Listening starts after the element is
committed, including an adopted SSR element; SSR itself performs no measurement.
Hidden boxes can report zero. Non-replaced inline elements do not emit sizes.
Fragmented layout exposes only its first fragment and requires a separate layout
policy; the variable-list helper uses ordinary block wrappers.

Replacing/removing a listener or disposing its element cancels observation and
pending delivery. Capture, passive, prevention and other native event options
are rejected. SVG elements are rejected; HTML inside an SVG foreignObject is
supported. Payload fields must be finite nonnegative numbers. The browser adapter
uses native ResizeObserver and the existing UI writer, with no scroll listener
or independent scheduling queue.

Experimental wire v22 adds the `size` event. Permanent native tests cover typed
payloads, phases and removal; three-engine boundary tests cover client mounting,
SSR identity, fractional borders, writing modes, transforms, hide/restore and
cancellation. The public [measured list](collection/README.md) uses the same API.

Geometry follows the [Resize Observer specification](https://drafts.csswg.org/resize-observer/).
This API reports layout size; it does not promise a paint timestamp or aggregate
measurements across CSS fragments.

Size observations share a border-box observer per root. Viewport observations
share a separate content-box observer so their box semantics remain distinct.
Both feed one pending-frame scheduler; all viewport layout reads finish before
callbacks publish values. Removing a listener drops its pending report. The last
listener releases the associated observer, and root close releases the frame and
all remaining observations.
