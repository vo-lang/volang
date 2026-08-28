# Comfortable cross-platform foundation

The E2 public surface keeps ordinary application code in `ui`, `ui/kit`, and
`ui/task`. The same source executes in VM and JIT development sessions and in
Core Wasm AOT and Native AOT releases.

## Environment and typed callbacks

`kit.Application` supplies the current environment and theme once at the root.
Descendants read `ui.CurrentEnvironment()` and `kit.CurrentTheme()` or create a
nested scope with `ui.ProvideEnvironment`, `ui.ProvideLocale`, and
`kit.ProvideTheme`.

Common controls expose small callbacks:

```vo
name := ui.UseStringState("Ada")

kit.ActionButton("Save", false, func() {
	save()
})

kit.Input("Name", ui.StringStateValue(name), "Ada", false, func(value string) {
	ui.SetStringState(name, value)
})
```

Low-level views retain `ui.Event` for custom controls. Adapters such as
`ui.Action`, `ui.TextChange`, `ui.ToggleChange`, `ui.KeyInput`,
`ui.PointerInput`, `ui.CompositionInput`, and `ui.LayoutChange` keep common
application handlers typed.

## Tasks, streams, and effects

Every declaration owns an internal generational state handle. Work starts only
after the declaring renderer commit succeeds. A key change or component
disposal cancels the prior context. Worker goroutines publish through bounded
mailboxes; mounted state remains owned by the UI Island.

```vo
result := task.UseString(query, func(ctx context.Context) (string, error) {
	return search(ctx, query)
})

updates := task.UseStringStream(topic, func(ctx context.Context, emit func(string) bool) error {
	for value := range subscribe(ctx, topic) {
		if !emit(value) {
			return nil
		}
	}
	return nil
})

task.UseEffect(subscriptionKey, func(ctx context.Context) func() {
	handle := connect(ctx)
	return func() {
		handle.Close()
	}
})
```

`UseString`, `UseBool`, `UseInt`, and `UseFloat` expose `Status`, `Value`, and
`Error`. Their stream counterparts also expose `HasValue` and a monotonic
`Revision`. A stream mailbox admits at most 256 pending messages and applies
one message per UI revision. `emit` blocks with cooperative backpressure and
returns `false` after cancellation or replacement.

Effect cleanup observes an already-cancelled context. Cleanup may release an
external resource and must leave UI publication to a later task completion.

## Measurement and adaptation

Viewport inputs live in `ui.Environment`. Container feedback uses
`ui.OnLayout(view, ui.LayoutChange(handler))`. Hosts quantize sizes to 1/64 of
a point, suppress equal values, admit at most 256 observers per commit, and
reject a ninth consecutive feedback turn. Applications should derive layout
from the event and avoid measuring merely to mirror a fixed style value.

## Focus, portals, and commands

`ui.Portal` preserves logical ancestry while hosts place the subtree in an
overlay plane. `ui.FocusRequest` accepts an increasing positive token and
moves focus after commit. `ui.Modal` isolates background input and restores a
framework focus target on dismissal.

`commands.Bind` installs a capture-phase key scope. It resolves the highest
enabled priority, rejects equal-priority chord ambiguity, and reaches focused
controls inside portals before target or modal key handlers advance the UI
generation. Logical and physical shortcuts are both available:

```vo
save := commands.New("document.save", "Save", commands.Key("s", ui.ModifierControl), saveDocument)
physicalSave := commands.New("document.save.physical", "Save", commands.Physical("KeyS", ui.ModifierControl), saveDocument)
physicalSave.Priority = 1
view = commands.Bind(view, save, physicalSave)
```

## Target invariants

- All renderer mutations publish atomically with one monotonic revision.
- Reverse events carry live handler, node, session, and sequence identities.
- Layout, focus, accessibility, and listener state derive from the accepted
  revision.
- Portal event propagation follows logical ancestry on Web and desktop.
- Task, stream, effect, measurement, event, and automation queues have explicit
  limits and deterministic failure behavior.
