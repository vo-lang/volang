# Canvas bitmaps (experimental Web pack)

`github.com/vo-lang/ui/next/web/canvas` displays an immutable RGBA8 bitmap through
one managed native Canvas 2D surface. Add `"features": ["canvas"]` to your
project's `ui-next.json`; the normal generated boot file stays unchanged.
Ordinary projects omit this host. `canvas` and `editor` can be enabled together.

```vo
import "github.com/vo-lang/ui/next/web/canvas"

// Snapshot once, in component state initialization or an input/task handler.
image := canvas.NewBitmap(2, 1, []byte{255, 0, 0, 255, 0, 255, 0, 255})
view := canvas.View(image, canvas.Options{
    Label: "A red pixel beside a green pixel",
    OnError: func(message string) { failure.Set(message) },
})
```

`NewBitmap(width, height, pixels)` copies the input into an immutable encoded
snapshot. Editing the original slice has no effect. Width and height must be
positive, at most 2,048 each, with at most 262,144 pixels in total (1 MiB RGBA).
The byte length must equal `width * height * 4`. Invalid input, a nil/zero bitmap
or an empty image label panics before acquiring browser resources.
`Width()` and `Height()` return the logical pixel dimensions.

Keep the bitmap in an application `ui.State` and replace it when the image
changes. Calling the constructor on every render repeats encoding work. An
unchanged bitmap creates no draw operation on an unrelated update. Updating
pixels or dimensions retains the native canvas; CSS resizing changes display
size without changing the logical pixel grid. The default host uses pixelated
scaling. It adds no animation loop, resize observer, graphics dependency or
separate state runtime. This bounded pack suits computed illustrations,
heatmaps and small pixel surfaces; it has no high-rate video or GPU contract.

`View` returns a normal UI view with `role="img"`, the required accessible label,
full available width and an aspect ratio derived from its bitmap. Use normal
keys, classes, attributes and event handlers. Keep important explanatory text
adjacent to the view. Server HTML preserves the labelled placeholder and aspect
ratio; actual pixels appear when the optional browser host activates. It does
not embed image bytes in HTML or produce a server PNG. Overriding the returned
style or accessibility attributes transfers those choices to the caller.

The host owns its canvas subtree. Removal, root disposal and source reload
abort the instance, remove listeners and clear the native backing dimensions.
Creation/update failures and context loss deliver `OnError`; without a handler
they propagate through the component's error boundary. Render a local fallback
and recreate the view with a new key to retry. The adapter does not retain an
extra pixel cache for automatic context restoration. Opaque bitmap state is
reinitialized during source reload: compute its initial value from the restored
scalar inputs to keep the result consistent.

Pixels follow the browser's [Canvas ImageData semantics](https://html.spec.whatwg.org/multipage/canvas.html#imagedata).
Input uses RGBA8 in the default sRGB color space. Native alpha premultiplication
and readback can round transparent colors; exact byte assertions use opaque
sRGB fixtures. Native DOM/accessibility semantics remain owned by the host.
Physical display calibration and assistive-technology validation are separate
from the automated engine checks.

## Complete application

Run `vo ui create my-landscape --template canvas`, then
`vo ui dev --project my-landscape` or `vo ui test --project my-landscape`.
The toolkit includes the application, its stylesheet and public browser tests.
The template calculates a landscape in Vo and demonstrates changing its palette,
hiding and restoring the view, failure recovery and source reload.
