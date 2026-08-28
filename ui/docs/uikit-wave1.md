# UIKit Wave 1 contract

Volang UIKit is organized as four ordinary Volang packages:

- `ui/kit/tokens` owns typed semantic tokens, themes, density, motion, and
  visual recipes.
- `ui/kit/headless` owns reusable state machines, keyboard navigation, and
  semantics without prescribing appearance.
- `ui/kit/components` composes headless behavior with official visual recipes.
- `ui/kit` is the stable convenience facade used by applications.

Applications may use the facade, import a lower layer, or copy a component's
small Volang implementation into application source. There is no generated
JavaScript component graph and no private renderer hook in the public UIKit.
Custom visual systems can retain the headless behavior and replace only token
or recipe code.

## Behavior matrix

Wave 1 covers content, form, feedback, overlay, and navigation families. The
machine-readable source of truth is `ui/kit/catalog.toml`. Each catalog row
declares variants, states, keyboard behavior, semantics, and executable source
evidence. `vo-dev ui-certify --check` rejects incomplete conformant rows.

The shared contracts include:

- native Button and Toggle activation exactly once for Enter and Space;
- roving RadioGroup and Tabs navigation that skips disabled entries and
  reverses horizontal direction under RTL;
- Select navigation with Home, End, arrows, Escape, disabled-option skipping,
  combobox/listbox semantics, and stable popup identity;
- modal AlertDialog focus isolation and restoration;
- non-modal Popover Escape dismissal and trigger restoration;
- focus-driven Tooltip description without adding a focus stop;
- closed overlay subtrees excluded consistently from layout, paint, input,
  focus, accessibility, and visible semantic assertions through `ui.Hidden`.

## Development and release parity

The component gallery is the permanent Wave 1 laboratory. Run it through VM
and JIT semantic tests during development, serve its Wasm AOT build in a real
browser, and build the same source as a Native AOT executable for release.
Theme, high contrast, density, locale, LTR/RTL, keyboard, focus, overlays, and
semantic snapshots are part of the gallery contract.

The pure algorithms in `ui/i18n/core` are renderer independent and safe for
server Native AOT. `ui/i18n` adds the mounted UI environment facade. This split
keeps formatting and direction logic reusable in future SSR work.
