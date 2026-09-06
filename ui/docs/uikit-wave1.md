# Official UIKit contract

Volang UIKit is organized as six ordinary Volang packages:

- `ui/kit/tokens` owns typed semantic tokens, themes, density, motion,
  interaction states, elevation, and visual recipes.
- `ui/kit/headless` owns reusable state machines, keyboard navigation, and
  semantics without prescribing appearance.
- `ui/kit/icons` owns bounded renderer-neutral vector symbols without an icon
  font or browser asset dependency.
- `ui/kit/components` composes headless behavior with official visual recipes.
- `ui/kit/data` owns data-dense table, tree, list, and chart recipes.
- `ui/kit` is the stable convenience facade used by applications.

Applications may use the facade, import a lower layer, or copy a component's
small Volang implementation into application source. There is no generated
JavaScript component graph and no private renderer hook in the public UIKit.
Custom visual systems can retain the headless behavior and replace only token
or recipe code.

## Governed component baseline

The 1.0 baseline contains 63 conformant components across six families:

| Family | Count | Representative components |
| --- | ---: | --- |
| Content | 11 | application, typography, card, avatar, code snippet |
| Form | 23 | field controls, combobox, multi-select, calendar, date picker, file uploader |
| Feedback | 8 | alert, progress, loading, skeleton, empty state, toast |
| Overlay | 8 | dialog, command palette, popover, drawer, menu, context menu |
| Navigation | 6 | tabs, accordion, breadcrumb, pagination, rail, navigation menu |
| Data | 7 | table, tree, list, scroll area, resizable, carousel, bar chart |

The machine-readable source of truth is `ui/kit/catalog.toml`. Each row
declares variants, states, keyboard behavior, semantics, and executable source
evidence. Renderer-neutral hover, pressed, focus-ring, and 0–5 elevation
properties keep those visual states equivalent across Web SSR/live DOM and
desktop retained paint.
`vo-dev ui-certify --check` requires exact baseline coverage and
rejects incomplete conformant rows. A valid declaration reports
`63 implemented, 0 governed gaps`; product certification additionally requires
the CI evidence bundle.

## Controlled interaction model

Complex components receive current state plus explicit callbacks. Applications
therefore own domain policy, persistence, routing, and asynchronous work, while
UIKit owns interaction, focus, semantics, and visual recipes. Stable `id`
values on comboboxes, multi-selects, and date pickers form their DOM and
accessibility relationships; callers must keep them unique within a mounted
application root.

- `kit.Combobox` and `kit.MultiSelect` provide filtering, loading and empty
  states, IME-safe keyboard traversal, stable active-descendant relationships,
  selection limits, and removable selections.
- `kit.Calendar` provides a deterministic six-week grid, bounds, unavailable
  dates, single/range selection, configurable week starts, and RTL-aware
  keyboard navigation. `kit.DatePicker` composes that grid with caller-selected
  `time.Parse` and `time.Format` layouts.
- `kit.FileUploader` owns picker/drop intake, admission limits, progress
  presentation, cancellation and retry controls. Transfer goroutines and
  storage remain in an application service that publishes immutable
  `[]kit.UploadFile` snapshots. This keeps renderer state on the UI writer and
  makes cancellation, retries, backpressure, and persistence testable. Picker
  and drop paths are checked case-insensitively against the declared extension
  filters before admission. Rejected rows do not consume the accepted-file
  quota. Because the portable drop event carries paths rather than trusted file
  metadata, `MaximumBytes` is enforced when the service publishes each accepted
  snapshot; an over-limit accepted snapshot fails the component contract.
- `kit.ContextMenu` receives pointer coordinates and caller-owned open,
  active-item, submenu, and focus tokens. It clamps to the viewport, supports
  one bounded submenu level, typeahead, RTL direction, disabled/checkable
  entries, and deterministic focus restoration.

Use `ui/task` resources or streams for remote filtering and uploads. Worker
goroutines publish bounded typed results; component state and renderer nodes
remain on the root UI writer.

## Behavior matrix

The shared contracts include:

- native Button and Toggle activation exactly once for Enter and Space;
- roving RadioGroup and Tabs navigation that skips disabled entries and
  reverses horizontal direction under RTL;
- Select navigation with Home, End, arrows, Escape, disabled-option skipping,
  combobox/listbox semantics, and stable popup identity;
- composite choice relationships rendered into SSR HTML before Wasm
  activation;
- Calendar arrows, Home/End and PageUp/PageDown with disabled-date and range
  behavior under LTR and RTL;
- file picker, drag enter/leave/drop, upload cancellation, retry and rejection
  states through the shared platform boundary, including filtered-extension
  rejection and accepted-file quota accounting;
- ContextMenu and Shift+F10 invocation, typeahead, nested-menu direction,
  collision clamping, Enter/Space activation, Escape dismissal and focus
  restoration;
- modal AlertDialog focus isolation and restoration;
- non-modal Popover Escape dismissal and trigger restoration;
- focus-driven Tooltip description without adding a focus stop;
- closed overlay subtrees excluded consistently from layout, paint, input,
  focus, accessibility, and visible semantic assertions through `ui.Hidden`.

## Development and release parity

The component gallery is the permanent UIKit laboratory. Run it through VM
and JIT semantic tests during development, serve its Wasm AOT build in a real
browser, and build the same source as a Native AOT executable for release:

```sh
./eng/run-uikit-gallery-contracts.sh
vo ui build ui/showcases/component-gallery -o target/ui-component-gallery-aot
npm --prefix lang/crates/vo-web run test:uikit-gallery-browser -- \
  --static-root ../../../target/ui-component-gallery-aot
vo-dev ui-certify --check
```

Theme, high contrast, density, locale, LTR/RTL, keyboard, focus, overlays, and
semantic snapshots are part of the gallery contract. The browser gate also
checks light/dark surface hierarchy, visible selected states, native-form
control styling, dark input contrast, mobile-width overflow, stacked compact
actions, and horizontally scrollable data grids. `kit.PageTitle` supplies the
display heading while `kit.Title` keeps section hierarchy quieter; compact
screens reduce page padding through the inherited viewport environment.

The pure algorithms in `ui/i18n/core` are renderer independent and safe for
server Native AOT. `ui/i18n` adds the mounted UI environment facade. This split
keeps formatting and direction logic reusable in future SSR work.
