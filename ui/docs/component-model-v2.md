# Component Model V2 implementation brief

Component Model V2 is E1 and the first product implementation increment. It
replaces the certified single-root optimization boundary with separately
compiled component definitions and persistent mounted instances. It preserves
the foundation renderer, protocol, scheduler, transaction, and AOT invariants.

## Fixed decisions

- Components remain ordinary typed functions in `.vo` files.
- No template language, `.vui` source format, application JavaScript runtime,
  reflection-based component lookup, or string event dispatch is introduced.
- Top-level `ui/` owns compiler-neutral plans, artifacts, identities, runtime,
  lifecycle, scheduler, and tests.
- `lang/crates/vo-ui-compiler` is a one-directional adapter over typed compiler
  facts. `vo-ui-plan` and `vo-ui-artifact` never import language crates.
- Canonical module and object identity defines a component type. Paths, import
  aliases, source spelling, and declaration order cannot impersonate it.
- Static component calls compile and link. Genuinely dynamic output uses the
  generic keyed reconciler under the same public behavior.
- Each root has one UI writer. Worker goroutines return typed messages through
  bounded, generation-checked scheduler turns.
- VM, JIT, Native AOT, Core Wasm AOT, and headless consume one authenticated
  component bundle contract.

## Compiler-neutral identities

The model needs distinct identities for distinct lifetimes:

- `ComponentTypeId` identifies one canonical component definition.
- `ComponentCallSiteId` identifies one statically compiled call in its parent.
- `ComponentInstanceId` is generational and identifies one mounted invocation.
- `StateFieldId`, `BindingId`, `HandlerId`, `EffectId`, and `TaskSiteId` are
  definition-local stable identities.
- `NodeId` continues to identify renderer nodes and never becomes application
  ownership.

Sibling component keys are parent-local. An unkeyed static call uses its stable
call-site identity. A keyed dynamic call combines the call site, key, and
component type. Duplicate keys reject the candidate transaction.

## Component bundle

The successor to the single `ComponentArtifact` is a bounded bundle:

```text
ComponentBundle
  format and ABI version
  canonical module identity
  root component type
  component definition table
  imported bundle requirements
  capability requirements
  source and reload metadata
```

Each definition contains:

```text
ComponentDefinition
  component type identity
  typed props layout
  typed child and slot contracts
  immutable template and dynamic blocks
  component call sites
  state schema and initializers
  binding evaluators and dependencies
  handler entry points and captures
  effect and task declarations
  error, loading, portal, and lifecycle boundaries
```

All tables have explicit byte, count, nesting, fan-out, state, binding,
handler, task, effect, import, and source-map limits. Validation authenticates
the entire bundle before any component mounts.

VUB1 now implements this compiler-neutral contract in `vo-ui-artifact`. It
uses canonical module/object `ComponentTypeId` values, stable definition-local
identities, strictly ordered tables, authenticated import requirements,
capability requirements, source/reload metadata, bounded static-graph
validation, and a deterministic codec. VUA1 remains available during the
compiler and runtime transition.

## Mounted instances

`ComponentRuntime` owns a persistent forest, one tree per root. Each instance
stores:

- generational identity, type, parent, call site, key, and sibling position;
- props, child/slot bindings, and inherited environment snapshot;
- state storage, derived dependencies, handlers, effects, tasks, resources,
  subscriptions, and error state;
- mounted renderer node range and nested component children;
- reload schema, revision, lifecycle phase, and disposal generation.

Reconciliation first stages the next component graph, validates key and type
compatibility, computes instance reuse/disposal, then stages renderer
mutations. Component and renderer state advance only after the complete commit
succeeds. A failed render, layout, host, or renderer transaction retains the
last committed component forest.

## Lifecycle order

For a successful revision:

1. Reused instances receive staged props and environment.
2. New instances allocate admitted state and scope capacity.
3. Render evaluates affected definitions and bindings.
4. Component and renderer transactions validate together.
5. One atomic renderer revision commits.
6. Component state, handlers, focus ownership, and generations publish.
7. Post-commit effects and tasks start.
8. Replaced and removed scopes cancel; cleanup runs after invalidation.

For a failed revision, newly admitted instances, handlers, tasks, effects, and
state roll back without becoming externally visible.

## State and mutation

E1 must preserve current scalar state while introducing a storage contract that
can extend to structured values. Every state write occurs in a transaction and
marks explicit binding dependencies. Slices, maps, and structs require `Set`,
`Update`, reducer, or observable collection operations; arbitrary in-place
mutation cannot silently bypass invalidation.

State handles are instance-scoped and generation checked. Worker goroutines do
not receive state handles. They publish typed messages that an instance-scoped
UI reducer applies after the scheduler validates task and instance generations.

## Separate compilation and package linking

Each package may emit component definitions alongside bytecode and AOT
artifacts. The application linker resolves imports by canonical package,
component type, ABI, and authenticated digest. It rejects missing definitions,
duplicate identities, incompatible props/slots, unsupported capabilities, and
cycles that require unbounded static expansion.

Source-distributed packages compile with the application. Precompiled packages
remain an optimization and require a future stable component ABI. A package
without a usable component artifact follows the generic source path.

## Hot reload

Reload matches instances by parent identity, call site or key, component type,
and state field identity. Compatible state migrates by logical type
fingerprint. Changed props and bindings recompute. Added state initializes,
removed state disposes, incompatible state resets, and removed scopes cancel.

A candidate bundle mounts and commits off the visible tree. Publication assigns
a new session epoch only after bundle validation, state migration, initial
render, host validation, and renderer commit succeed. Failure restores the old
bundle, instance forest, handlers, state, tasks, replay phase, and visible UI.

## First implementation slices

### B1 — Bundle model and validation

Add component and call-site identities, bounded component definition tables,
import requirements, codecs, negative tests, fuzz targets, and artifact
preservation through VOB, cache, Native AOT, and Core Wasm AOT.

Status: complete. VUB1 round-trip and hostile truncation tests live with the
codec; backend-owned tests preserve the sidecar through VOB, the compile cache,
Native AOT objects, and Core Wasm AOT custom sections. The independent fuzz
target applies deliberately smaller allocation and traversal limits.

### B2 — Compiler discovery and linking

Discover reachable local and imported components from typed object identity,
emit definitions and call sites, resolve package bundles, preserve the generic
fallback, and report why any call leaves direct mode.

Status: complete for source-distributed packages. Local and imported component
definitions share canonical package/object identities, typed prop layouts,
package-scoped evaluator entrypoints, and static keys. Typed discovery also
instruments keyed component calls inside ordinary branches and collections, so
the generic path retains the same instance scopes. Authenticated precompiled
package bundles remain gated on a future public component ABI.

### B3 — Mounted component forest

Mount nested instances, stage keyed reconciliation, preserve renderer identity,
dispose removed scopes, and atomically roll back failed component transactions.

Status: complete. The generational forest stages keyed and call-site identity,
type replacement, postorder disposal, template nodes, and renderer mutations as
one candidate. Renderer rejection leaves the previous forest and node-owner
index live.

### B4 — Instance state and handlers

Move state, handler tables, dependency edges, and reload schemas from root
scope to instance scope. Verify closure and structured-state GC roots across
VM, JIT, AOT, unwind, disposal, and reload.

Status: complete. VM/JIT
allocate scalar state by canonical component call path, component type, field,
and logical fingerprint. Handler evaluator closures capture those handles,
survive complete GC cycles, resolve from renderer nodes to logical instances,
and retain independent values for equal component types. Generic VM and AOT
state handles carry slot generations, so disposed instances and late callbacks
cannot alias a replacement. The runtime forest stores structured
`ComponentValue` transactions while language adapters keep precise managed
values on their owning backend.

### B5 — Lifecycle and scoped completion bridge

Add post-commit effect/task declarations, cancellation propagation, typed
completion messages, UI reducers, bounded turns, and late-result rejection.

Status: implemented at the compiler-neutral runtime boundary. Component-owned
tasks use generational instance owners, bounded turns, cancellation-before-
cleanup ordering, and stale-completion rejection. Public Volang task/effect
authoring APIs are delivered in E2 on this contract.

### B6 — Cross-backend and reload certification

Run the nested-component and asynchronous-search probes through headless, Web
VM, Web Wasm AOT, desktop VM/JIT, and Native AOT. Add state-preserving reload,
failure rollback, performance, OOM, and fuzz evidence.

Status: complete. Headless, VM, and JIT cover nested keyed mounting,
instance-local state and handlers, imported package execution, GC retention,
incremental node reuse, generational disposal, and transactional
state-preserving reload. The authored contract probe now performs keyed move,
remove, reinsert, and replacement through VM, JIT, a real Core Wasm AOT
browser, and a linked Native AOT window on Linux, macOS, and Windows. The
optimized certification workload also enforces stable identities and an 8 ms
p95 budget while updating 256 keyed stateful component instances; the current
macOS ARM64 reference result is below 0.5 ms.

## E1 exit gate

E1 completes only when:

- two instances of one component retain independent state;
- keyed insertion, deletion, reorder, and type replacement preserve exactly
  the compatible instances;
- removed instances cannot receive events or asynchronous completion;
- imported components use the same compiled path as local components;
- failed candidate render and failed hot reload retain the old interactive UI;
- VM, JIT, Native AOT, Core Wasm AOT, and headless agree on component behavior;
- foundation certification, no-std checks, protocol validation, GC precision,
  and current renderer performance budgets continue to pass.

Status: passed. The permanent probe is
`ui/tests/nested-component-list`; browser and packaged-native execution are
gated by `.github/workflows/ci.yml`, while the compiler-neutral failure,
lifecycle, scheduler, reload, fuzz, and performance suites retain the lower
layers of the contract.
