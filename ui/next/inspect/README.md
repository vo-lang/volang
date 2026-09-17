# Development inspection

Optional component, prop, dependency, render and task diagnostics. Import
`github.com/vo-lang/ui/next/inspect` in an explicit development entry. Normal
applications can keep using `host.Run`; the inspection collector, clock and
browser presentation are separate from that entry.

For an owned headless root:

```vo
root := ui.NewRoot(ui.Component(appType, App))
session := inspect.New(root, inspect.Options{})
defer session.Close()
batch := root.Flush()
// Apply the batch, then acknowledge its commit.
root.Commit(batch.Revision)
snapshot := session.Snapshot()
```

For a development browser entry, `inspect.Run(view)` owns a root and adds one
inspection bridge scope. Install `createInspectionServices()` from the optional
`ui_next/inspection.js` browser module and pass its `services` to `mountUi`.
After `application.ready`, `await inspector.snapshot()` requests a capture through
the same serialized input queue as application events. `inspector.close()` rejects
pending captures; close the application to release its root and subscriptions.
Each service set belongs to one root. Concurrent captures share one request,
requests expire after five seconds, and late responses cannot satisfy a new one.
No timer polls the component tree and no data is sent to an external service.

The [inspection example](../examples/inspection/main.vo) and
[browser loader/panel](../../../lang/crates/vo-web/js/ui_next/inspection-lab.js)
form a complete integration. After the normal lab build, open
`/?example=inspection&backend=vm` or `backend=vm` on the lab server.
Capture before and after changing one counter, then compare the two instances.
The panel lives outside the inspected root, uses native details/table semantics,
and preserves expanded components across captures.

New standalone projects already have separate shared-application, production and
development entries. `dev --project` selects `developmentEntry` from `ui-next.json`
and bundles the development mount adapter. It provides an **Inspect components**
panel in an isolated shadow tree, with one application selector per document.
Closing the last application removes the panel. Production builds use the regular
mount adapter and entry, and contain no collector package or panel code.
Older configurations without that field retain their original entry. Custom
entries explicitly call `inspect.Run` and share components through a regular Vo
package; the toolkit does not rewrite arbitrary source to add inspection hooks.

Snapshots include:

- Component identity, parent, definition name/key/source, render count and dirty
  or fallback state. IDs belong to the current root lifetime.
- Named scalar values, state kinds, component dependencies and derived input
  dependencies. Pending resources and invalidated derived values are marked.
- Explicitly declared props from each component's latest render attempt. These
  keep their declaration order; repeating a name replaces its previous preview.
- Owned effect, task and subscription counts; task counts include subscriptions.
- The last 64 render attempts, with state invalidation, parent, mount or fallback
  causes and elapsed nanoseconds. A derived invalidation points to that derived
  state; its dependency list explains the underlying inputs.
- Up to 512 active requests/subscriptions and the last 64 terminal records,
  sorted by task ID. Each record includes component ownership, service, timeout,
  waiting/completed/failed/cancelled state, accepted reply count, bounded error
  and elapsed time. A subscription remains waiting across values; its terminal
  error counts as an accepted reply. Disposal and explicit cancellation retire
  the same record. Late results and repeated cancellation cannot add history.

Snapshots are independent of the live graph. Reading them does not subscribe a
component, evaluate a derived computation or invoke an application `String`
method. Structured stores and computed results are intentionally opaque.
Undeclared closure captures, provider-internal phases, full stack profiling and
DOM highlighting are outside the current snapshot.
This snapshot is a diagnostic view; restoring it as application state is outside
its contract. [Development reload](../develop/README.md) uses its own complete,
bounded checkpoint and compatibility scheme.

Inspection is bounded to 512 components, 2,048 state cells, 4,096 dependency edges
and 20,000 visited nodes per snapshot. String previews and names are capped at
256 UTF-8 bytes; file paths at 1,024 bytes. Clipped strings are copied so they do
not keep large source strings alive. Individual values and partial graphs carry
truncation flags. A truncated subset may change when a large scope's map changes.
Pending causes are limited to 512 components and 16 distinct state identities per
component. JSON transport has a 4 MiB budget; a failed capture is reported locally
and leaves the application running.

Declare a typed input inside its component render:

```vo
func Counter(name string) ui.View {
    return ui.Component(counterType, func(scope *ui.Scope) ui.View {
        ui.InspectProp(scope, "name", name)
        // Render with the same value, using ordinary typed application code.
        return ui.Element("p", ui.Text(name))
    })
}
```

`InspectProp` borrows its value only during the optional observer callback. It
does not retain the object or add a reactive dependency. Supply an already
computed input; argument expressions still execute as ordinary application code.
Declarations belong to the current render scope and cannot run from handlers or
derived computations. The collector copies previews for built-in strings,
booleans, integer and floating-point types and nil. Integer previews remain text
through JSON so 64-bit values keep their precision. Objects, functions and named
application types display as opaque; declare relevant scalar fields individually.

Props are bounded to 64 names per component, 512 components and 2,048 total values.
Names and string values use copied, at most 256-byte UTF-8 previews. Clipped names
may merge with the same clipped prefix; the snapshot is explicitly partial.
Each render replaces that component's previous declarations, including removing
conditional declarations. Disposal frees its prop budget. Attaching later waits
for the next render to observe props; it never reruns a component to discover
earlier inputs. Snapshots describe the latest observed render attempt; dirty
state may precede the next committed render. Capture again after a visible update
when comparing props with that update's DOM.

The default project demonstrates the declaration with its `initial` input.
Production uses the same application source and the regular host entry; optional
collection, formatting and panel modules are omitted. There is no added payload
field on every `View`, and no diagnostic formatting when no props observer exists.

Elapsed time includes child rendering and reconciliation. Nested times overlap
and must not be summed. The default clock uses wall time, with negative intervals
clamped to zero; `Options.Now` accepts a deterministic nanosecond clock for tests.
Instrumentation adds cost while attached, so these samples locate expensive
renders and do not replace the separate production performance benchmark.

Task time starts when the guest queues a request and ends before its terminal
callback. It includes guest/host queueing; waiting does not assert that a provider
is executing. Capturing an active task computes its current elapsed time without
changing retained history. Durations above JavaScript's safe integer bound are
clipped and marked partial. Attaching to an existing root captures active task
identity, but cannot recover its earlier service, timeout or start time. Such
records have `observedStart: false`, an empty service and elapsed time measured
since attachment. The panel labels that distinction. Request bodies and result
values are never retained; errors remain visible as bounded diagnostic text.
The inspector's own request/publish transport is excluded from collected starts.

`Root.Inspect`, `Root.InspectTasks`, `Scope.InspectMetadata` and `Signal.InspectIdentity` are read-only
core entry points used by the collector. `Root.Observe` permits one trusted
`RuntimeObserver`; callbacks must read metadata without changing application state
or running application work. Ordinary execution only checks whether an observer
is attached; it allocates no diagnostic snapshots and reads no diagnostic clock.
`RuntimeTaskObserver` is an optional extension; existing three-method observers
continue to work. Core task identity extraction does not enumerate sibling tasks.
`RuntimePropsObserver` is a separate optional extension for declared inputs.

Native contracts live in [inspection.vo](../tests/runtime/inspection.vo) and
[inspection_tasks.vo](../tests/runtime/inspection_tasks.vo) and
[inspection_props.vo](../tests/runtime/inspection_props.vo). The same
contracts execute on Wasm VM. Browser service tests cover request
coalescing, generation matching, timeout, malformed results and teardown; the
three-engine panel contract additionally covers local update causes, stable
unrelated components, disposal, expansion, real request/subscription histories,
independent histories for multiple roots and narrow layout.
