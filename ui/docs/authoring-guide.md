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

Use `kit.ProvideTheme` to scope semantic color, spacing, type, shape, motion
and density tokens. `kit/tokens.ExportCSS` and `ExportManifest` produce
deterministic design interchange. `kit/icons` provides bounded vector symbols
without an icon font.

Headless behavior lives separately from visual recipes. Applications can wrap
behavior, replace recipes or export maintained source:

```sh
vo ui source --list
vo ui source kit/components -o components.vo
```

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
