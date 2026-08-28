# Migrating a JavaScript frontend to Volang UI

Volang UI covers the jobs commonly split across Node/Bun, a bundler, a
component compiler, a reactive runtime, a DOM framework, and a desktop wrapper.
Application source, state, handlers, asynchronous work, and components remain
typed `.vo` code. Small generated browser modules implement the platform ABI
inside the packaged runtime and are not an application extension surface.

## Concept map

| JavaScript ecosystem | Volang UI |
| --- | --- |
| Node or Bun command/runtime | `vo` CLI plus VM/JIT development runtime |
| npm package manifest and lock | `vo.mod` and `vo.lock` |
| Vite development server | `vo ui dev` |
| Svelte/Vue component compiler | typed UI compiler and component artifact graph |
| reactive refs/stores | component-scoped structured state and derived values |
| virtual DOM update | compiled direct component updates with a keyed generic fallback |
| browser event loop | one UI Island receiving typed VUE1 events |
| promises/workers | scoped goroutines, typed tasks, streams, resources, and UI reducers |
| DOM/CSS API | renderer-neutral views and typed modifiers |
| Electron/Tauri renderer | native retained tree, layout/text engine, WGPU, AccessKit |
| production bundle | Core Wasm AOT Web tree or linked Native AOT executable |

Components remain ordinary `.vo` functions returning `ui.View`. Component
Model V2 gives each nested call a stable instance and links local and imported
component artifacts into direct update plans. Dynamic library composition
keeps the generic keyed reconciler with the same lifecycle, event, and semantic
contracts.

The certified foundation currently uses a single-root VUA1 artifact, scalar
state cells, early UIKit theme arguments, generic event callbacks, and manual
invalidation for some resource completions. These are migration fixtures while
E1 and E2 land the component graph, structured state, inherited environment,
typed callbacks, and scoped task bridge. New reusable libraries should follow
the target contracts in `architecture.md` and declare experimental APIs until
their required contract probes pass.

## Library migration

Pure algorithms, validation logic, data models, protocol clients, and design
tokens can usually be translated directly into Volang packages. A JavaScript
library that owns DOM nodes, monkey-patches browser globals, depends on npm
lifecycle hooks, or executes arbitrary script needs a Volang implementation
against `ui`, `ui/system`, or a typed host extension. Binary WebAssembly
libraries can be integrated through governed Volang extension boundaries when
their ABI and target support are explicit.

Component libraries should expose pure-Volang functions and keep platform
calls at the edge. This lets the same package execute under VM, JIT, Native AOT,
and Core Wasm AOT. Copying source architecture and behavior from a mature
library is often useful; copying its JavaScript runtime assumptions gives poor
portability.

## Goroutine model

The mounted UI Island is the single owner of component state publication and
renderer commits. Event handlers and task reducers run there and may update
several values; one turn publishes at most one atomic revision. Long work
belongs in component-scoped goroutines:

1. A committed component scope starts a typed task or stream.
2. The worker runs I/O or computation without mounted state or renderer nodes.
3. The worker returns a typed result through a bounded completion channel.
4. The UI Island validates its scope generation and runs the reducer.
5. Disposal, navigation, reload, and teardown propagate cancellation.

The current resource cache already proves joining, cancellation, retry, expiry,
stale-result rejection, and bounded LRU eviction. E2 moves its completion path
onto the official scoped task bridge. Manual `ui.Invalidate` remains a baseline
escape hatch and leaves the normal application path.

## Suggested migration sequence

1. Recreate the application shell with UIKit and preserve route/data model
   names.
2. Move pure business logic into ordinary Volang packages and unit-test it.
3. Translate leaf components and forms, retaining stable `ui.Key` identities
   for collections.
4. Replace promises and stores with resources, goroutines, channels, and typed
   state cells.
5. Replace direct DOM/platform access with typed modifiers and `ui/system`;
   consult the platform capability matrix for fallbacks.
6. Exercise interactions with `vo ui test`, then run Web VM/JIT development.
7. certify Web and desktop release outputs through `vo ui build` and Native AOT
   linking.

The [architecture guide](architecture.md), [platform capability
contract](platform-capabilities.md), and [release policy](release-policy.md)
define the boundaries that reusable packages can rely on.
