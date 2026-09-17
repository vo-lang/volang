# Element scroll positioning

`ElementRef.ScrollTo(left, top)` queues an instant scroll after the next DOM
commit. Coordinates are physical CSS-pixel offsets: native negative `left` values
are valid for RTL containers. The browser clamps positions to the final scrollable
extent. Focus remains unchanged, and the request overrides CSS smooth scrolling.
Use `ScrollIntoView()` to reveal a descendant's nearest edges instead.

```vo
viewport := ui.Ref(scope, "viewport")

// A handler can change content and request its new position in one turn.
height.Set(3000)
viewport.ScrollTo(0, 2500)
```

Bind the ref to the scroll container with `.Ref(viewport)`. The ref resolves after
reconciliation, so a handler can show a new container and scroll it in the same
commit. Missing bindings are ignored; nil or disposed refs return false. True
means the request was queued, without promising that a binding will exist or that
the browser can reach those exact coordinates. Nonfinite coordinates panic.
Requests belong in handlers or post-commit effects, as with Focus. Multiple
focus, reveal and position requests keep their declared order.

For automatic corrections, use `ScrollToIfUnchanged(previous, next)` with two
`ScrollPosition{Left, Top}` values:

```vo
viewport.ScrollToIfUnchanged(
    ui.ScrollPosition{Left: observed.Left, Top: observed.Top},
    ui.ScrollPosition{Left: observed.Left, Top: observed.Top + addedHeight},
)
```

Pass the unrounded offsets from `OnViewport` as the previous observation. The
host compares them with the native position before applying this batch's DOM
changes, then performs a matching correction after layout. A newer native scroll
makes an old correction a no-op. New element identities have no matching prior
observation. The return value still describes queueing; use `OnViewport` for
actual observed positions. This primitive supports scroll restoration and list
anchoring policies without adding an animation loop or another asynchronous
queue. Automatic variable-row measurement and anchoring remain separate work.

The protocol adds `scrollPosition` in experimental wire v21. Both ordinary and
conditional coordinates are validated before DOM mutation. Parsed element
operations share one optional ordered list with Focus and ScrollIntoView;
ordinary commits allocate no such list. A malformed frame or an imperative target
removed in the same raw batch is rejected atomically. Vo ref resolution omits
requests for removed bindings before producing a frame.

Development source reload also restores saved element/control scroll positions
instantly, including pages with `scroll-behavior: smooth`. It uses the existing
explicit-ID matching and retains its normal ownership and replacement rules.
The [standalone example](examples/scroll-position/app.vo) covers live content
growth, early SSR scroll, hide/restore, RTL and an old correction interleaved with
a newer native scroll.

Coordinate and bounds behavior follows the current
[CSSOM View scrolling algorithms](https://drafts.csswg.org/cssom-view/#dom-element-scrollto).
Keyboard and three-engine tests exercise browser behavior; physical touch/trackpad
and assistive-technology acceptance remain in the platform validation plan.
