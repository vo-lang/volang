# UI authoring guide

## Components and state

A component is an ordinary typed function returning `ui.View`. Keyed component
calls own independent state, handlers, effects and goroutines. Use immutable
inputs, explicit state setters and stable keys. The compiler records direct
update sites; a bounded reconciler covers dynamic shapes.

External work starts after commit. Component disposal cancels its tasks before
cleanup and rejects completions from an earlier generation. Worker goroutines
return typed messages through `ui/task`; mounted state and renderer nodes stay
on the root's single writer.

## Application layers

- `ui` supplies views, events, environment, state and mount.
- `ui/kit` supplies the official design system and application controls.
- `ui/navigation`, `ui/forms`, `ui/commands`, `ui/resource` and
  `ui/persistence` cover application flow and data.
- `ui/web`, `ui/desktop`, `ui/platform` and `ui/system` expose typed platform
  boundaries with explicit support and permission results.
- `ui/graphics`, `ui/assets`, `ui/animation`, `ui/chart` and `ui/media` cover
  visual and timed content.
- `ui/document`, `ui/editor`, `ui/language` and `ui/workspace` form the Studio
  surface.

## Design system extension

Use `kit.ProvideTheme` to scope semantic color, spacing, type, shape, motion,
elevation, interaction-state, and density tokens. `kit/tokens.ExportCSS` and `ExportManifest` produce
deterministic design interchange. `kit/icons` provides bounded vector symbols
without an icon font.

Headless behavior lives separately from visual recipes. Applications can wrap
behavior, replace recipes or export maintained source:

```sh
vo ui source --list
vo ui source kit/components -o components.vo
```

Use `kit.PageTitle` once for the screen heading and `kit.Title` for section
headings. `kit.ButtonGroup` follows the inherited viewport and stacks actions
on compact screens. Official data grids retain their semantic column model and
gain a bounded horizontal scroll surface when fixed tracks exceed the
available width.

Use `ui.HoverBackground`, `ui.PressedBackground`, `ui.FocusRing`, and
`ui.Elevation` when authoring a custom recipe. These properties stay in the
portable UI tree: Web maps them to pseudo states and bounded CSS shadows;
desktop resolves retained pointer/focus state and paint effects without an
application render transaction. Official UIKit recipes already apply them to
buttons, menus, tabs, calendars, lists, navigation, data controls, and overlays.

## Editor focus and diagnostics

`editor.Options.FocusToken` targets the editor's native text control. Increase
the token after an external action such as activating a diagnostic or opening
a search result. Update the document selection and viewport first, then publish
the new token in the same application turn. Web focuses the textarea; desktop
focuses the corresponding native editor surface. A stable token has no focus
side effect.

Diagnostic adapters should preserve one-based source line and column values at
the host boundary. Convert them to document offsets in product state, clamp
invalid ranges, reveal a few context lines above the target, and only then
request focus. This keeps Problems lists, compiler hosts, and the editor model
independent while producing one cross-platform navigation contract.

The sibling provenance receipt lets an upgrade tool identify the exact source
that was customized.

## Target boundaries

Keep portable state and behavior above a small context-aware host interface.
Declare authority, lifetime, quota, cancellation and unsupported behavior at
that interface. Query capability support before presenting a platform action;
do not infer support from an operating-system name.

Every untrusted collection, string, byte frame, path and queue needs a bound.
Asynchronous results need a generation or immutable input version. These two
rules keep Web and desktop behavior deterministic under cancellation, reload,
device loss and hostile input.
