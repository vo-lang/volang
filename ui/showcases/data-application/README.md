# Data application showcase

This permanent dogfood application composes the public E4 surface: typed
routing, scoped forms, one command registry projected as shortcuts, toolbar and
palette, joined/retried resources, optimistic mutation, offline state,
persistence history, accessible charting, a virtual tree, and a 100,000-row
logical data set with bounded row materialization.

Run it for Web development with `vo ui dev ui/showcases/data-application`.
Run the native development window with
`vo ui run ui/showcases/data-application --mode=vm` or `--mode=jit`. Release
verification uses `vo ui build` for Web Wasm AOT and `vo ui package` for
Native AOT.

The permanent cross-runtime gate is `./eng/run-data-application-contracts.sh`.
It drives the same 16 interactions through VM and JIT and requires a byte-exact
final snapshot. Build the Web release and run
`npm --prefix lang/crates/vo-web run test:data-application-browser` for the real
browser business flow. CI also starts the packaged application in real Linux,
macOS, and Windows windows and completes selection, optimistic commit, routing,
and presentation.

The toolbar and Ctrl+K command palette are two projections of one typed command
scope. The palette is modal, searchable, keyboard navigable, and removed from
the live tree after activation, so command identities stay unambiguous.
