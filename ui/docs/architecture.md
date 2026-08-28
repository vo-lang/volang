# Volang UI architecture

## Product contract

Volang UI builds browser and desktop applications from typed `.vo` source.
VM and JIT serve development; Core Wasm AOT and Native AOT serve releases.
Application projects have no npm dependency graph, JavaScript component
runtime, virtual DOM, or browser-specific component API.

The browser still requires a small framework-owned adapter for DOM and other
Web APIs. The toolchain generates, audits, versions, and packages that adapter.
It remains below the Volang application ABI and does not become an extension
surface for application code.

The top-level `ui/` workspace owns framework contracts independently from the
language implementation. Compiler, VM, AOT, Web, and native packages depend
inward through versioned plans, artifacts, events, renderer mutations, system
requests, and execution adapters. UI packages never become dependencies of the
language core.

## Product family

Everyday applications normally import `ui` and `ui/kit`. Advanced capabilities
remain opt-in and independently linkable:

- `ui` owns components, state, lifecycle, environment, views, style, and input;
- `ui/kit` owns the official design system, controls, and product patterns;
- `ui/resource` owns queries, mutations, streams, caching, and retries;
- `ui/navigation`, `ui/forms`, and `ui/commands` own application behavior;
- `ui/web` owns routes, server authority, SSR, activation, and Web metadata;
- `ui/desktop` owns windows, lifecycle, shell policy, and packaging;
- `ui/document`, `ui/editor`, `ui/chart`, and `ui/media` are advanced packs;
- `ui/testing` owns application-facing semantic tests and deterministic fakes;
- `ui/system` owns typed capability and platform-service requests.

The existing Volang module solver, lockfile, authenticated cache, and release
pipeline distribute these packages. UI package metadata extends that system
with backend, platform, capability, ABI, accessibility, provenance, and
maintenance information.

## Renderer-neutral foundation

The certified foundation remains the implementation base:

1. `vo-ui-core` owns values, properties, events, keys, public identities, and
   renderer primitives.
2. `vo-ui-reactive` owns signals, derived dependencies, batching, priorities,
   scopes, and disposal.
3. `vo-ui-plan` owns compiler-neutral templates and direct update sites.
4. `vo-ui-artifact` owns bounded deployable component metadata.
5. `vo-ui-runtime` owns transactional direct updates and generic keyed
   reconciliation.
6. `vo-ui-scheduler` owns bounded, generation-checked goroutine completions.
7. `vo-ui-session` joins one UI writer, scheduler turns, and renderer commits.
8. `vo-ui-protocol` owns atomic mutation and typed reverse-event frames.
9. `vo-ui-layout`, `vo-ui-paint`, and `vo-ui-accessibility` derive one revision
   of geometry, presentation, and semantics.
10. `vo-ui-headless` and `vo-ui-golden` provide deterministic conformance.
11. `vo-ui-web` and `vo-ui-desktop` implement the common renderer boundary.
12. `vo-ui-system` owns bounded service request and completion contracts.

Native text, WGPU, AccessKit, system-service, window-shell, VM, AOT, and browser
packages remain adapters around these renderer-neutral contracts.

The M0-M7 baseline is frozen in `ui/roadmap.toml`. Product work can replace
unreleased internal component and artifact assumptions while preserving the
certified invariants: generational identity, atomic commits, deterministic
ordering, bounded work, precise ownership, and backend verification.

## Component Model V2

The current runtime preserves the certified single-root path and adds a
compiled component artifact graph with persistent mounted instances.

A component artifact records:

- canonical package and component type identity;
- typed props and child/slot contracts;
- immutable template structure and dynamic blocks;
- state schema, logical type fingerprints, and initialization functions;
- direct binding evaluators and dependency edges;
- handler, effect, task, resource, and error-boundary entry points;
- imported component call sites and required artifact versions;
- source spans and reload metadata.

The active multi-component wire contract is VUB1
(`volang.ui.component-bundle`, artifact version 1, component ABI 2). VUA1 stays
as the frozen single-root baseline. Reachable local and imported source
components now execute through VUB1 in VM/JIT, and both AOT lowerers preserve
the same evaluator table and authenticated sidecar.

A mounted component instance records:

- component type, parent, local key, and generational instance identity;
- current props, children, environment snapshot, and mounted node range;
- state, derived values, handlers, effects, tasks, resources, and subscriptions;
- focus, portal, error, loading, and cancellation scope ownership;
- compatible hot-reload schema and disposal state.

Sibling keys are parent-local. Compatible keyed instances survive insertion,
movement, and reordering. Removed or replaced instances invalidate their task,
handler, effect, and resource generations before cleanup. Renderer nodes carry
no application ownership.

Known static component graphs link into direct plans across package boundaries.
Dynamic library output, data-dependent component types, custom renderers, and
other unproven shapes retain the bounded generic reconciler. Both paths share
component identity, lifecycle, events, accessibility, and target behavior.

## Lifecycle

Component execution has four phases:

1. Render reads state and declares views, bindings, effects, and task intent.
2. Commit atomically advances renderer, layout, paint, semantics, focus, and
   listener identity.
3. Post-commit starts effects, subscriptions, timers, and worker goroutines
   whose declaring commit succeeded.
4. Dispose cancels owned work and generations before invoking cleanup.

Render remains replayable. External I/O and other non-repeatable effects live
in events, loaders, mutations, tasks, or post-commit effects. Framework-aware
diagnostics use compiler effect information to flag unsafe component work.

## State and goroutines

One UI Island owns state publication and renderer commits for each root.
Structured state has explicit `Set`, `Update`, transaction, reducer, and
collection semantics; silent in-place mutations cannot bypass dependency
tracking.

Workers execute I/O or computation away from the UI writer. They return typed
messages through bounded channels. `vo-ui-scheduler` verifies task and scope
generations, limits completions per turn, and hands accepted messages to a UI
Island reducer. `vo-ui-session` batches resulting writes into at most one
renderer revision.

The public task layer supplies `Task`, `Stream`, `Effect`, query, mutation,
pagination, retry, debounce, latest, join, queue, and bounded-parallel policies.
Component disposal, key changes, navigation, reload, and root teardown
propagate cancellation automatically. Manual invalidation remains an internal
escape hatch.

## Environment, style, and layout

Theme, locale, writing direction, density, platform conventions, viewport,
container size, DPI, text scale, safe area, input mode, accessibility settings,
media capability, and permission state flow through typed inherited
environment values.

The style system owns typed logical dimensions, color, typography, borders,
radius, elevation, effects, transforms, state selectors, responsive conditions,
component anatomy, variants, and recipes. Web lowers supported semantics to
DOM/CSS; native targets lower them to retained layout and paint properties.

Portable conformance requires the same constraints, ordering, overflow,
scrolling, focus, hit-testing, and accessibility semantics. Font metrics and
platform presentation may vary within declared tolerances. Intrinsic and
container measurements are revisioned and bounded; feedback loops have an
explicit iteration limit and diagnostic.

## Input and accessibility

Keyboard, pointer, touch, pen, wheel, trackpad, gamepad, IME, drag/drop, and
assistive actions enter one typed event boundary. Low-level event payloads stay
available for custom interactions. Common UIKit controls expose short typed
callbacks such as actions, string changes, boolean changes, and selections.

Focus, overlays, portals, gestures, commands, shortcuts, menus, toolbars, and
context menus use reusable state machines. Headless UIKit behavior primitives
preserve these interactions and semantics when applications replace visual
recipes.

Semantic identity is generation checked and revision aligned with layout.
Official controls follow WAI-ARIA APG when a pattern exists and carry Web,
NSAccessibility, UI Automation, and AT-SPI evidence where relevant.

## Web authority and activation

The Web application compiler divides code into three authority domains:

```text
server-only -> shared -> client
```

Dependencies may flow only toward lower authority. Server secrets, files,
processes, credentials, and privileged extensions cannot enter client Wasm or
serialized activation data. Loaders, actions, cookies, sessions, headers,
redirects, caches, endpoints, and deployment adapters carry explicit request
limits and security policy.

SSG and streaming SSR execute component artifacts on the server. Activation
manifests carry bounded component identity, props, state, resource, listener,
and DOM correspondence data. The browser validates the manifest before
resuming selected component instances. Useful semantic HTML remains available
before activation and without client execution where the route contract allows.

## UIKit product logic

UIKit combines two layers:

- behavior primitives own APG interaction, focus, selection, overlay, command,
  validation, collection, and accessibility state machines;
- Volang recipes own semantic tokens, density, motion, visual variants,
  component anatomy, layout, and branded themes.

The official style favors information-rich desktop work while adapting to
touch, accessibility, and narrow surfaces. Token and recipe overrides cover
normal customization. A governed source-export workflow supports deep changes
while retaining provenance and upgrade diagnostics.

## Quality and governance

`ui/product-roadmap.toml` defines product outcomes. `ui/capabilities.toml`
defines capability maturity, API stability, dependencies, target support, and
evidence. `ui/delivery.toml` defines E0-E8 and permanent contract probes.
`ui/certification.toml` and CI bind executable gates to release evidence.

Every untrusted decoder, queue, collection, asset, path, URL, plugin, and
serialized state has explicit limits. Every release target has startup, size,
memory, allocation, frame, latency, jank, power, failure, accessibility,
security, and provenance evidence appropriate to that target.
