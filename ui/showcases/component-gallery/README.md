# Official UIKit component gallery

This permanent E3 application is the interaction laboratory for the official
UIKit. It renders content, form, feedback, overlay, and navigation families
under light/dark/high-contrast themes, compact/comfortable density, LTR/RTL,
typed keyboard and pointer input, and renderer-neutral accessibility state.

Run the same source in development modes:

```sh
vo ui test ui/showcases/component-gallery --mode=vm --click=Primary --click="Open confirmation" --click=Cancel
vo ui test ui/showcases/component-gallery --mode=jit --click=Secondary --click="Toggle popover"
vo ui test ui/showcases/component-gallery --mode=vm --focus="Toggle tooltip" --wait-text="Context without interaction" --blur="Toggle tooltip" --wait-absent-text="Context without interaction"
```

Build the browser release with `vo ui build ui/showcases/component-gallery`.
The release gate builds that output and runs
`npm --prefix lang/crates/vo-web run test:uikit-gallery-browser`, which drives
the form, keyboard, overlay, live-region, theme, density, and RTL contracts in
a real browser. `eng/run-uikit-gallery-contracts.sh` runs the same application
through both development runtimes.
Build the packaged Native AOT application with
`vo build ui/showcases/component-gallery -o component-gallery`.

The gallery deliberately keeps popup subtrees mounted and toggles `ui.Hidden`.
That exercises stable renderer identity while requiring layout, paint, hit
testing, focus, and accessibility to exclude closed content on every target.
