# Volang UI workspace rules

- Keep this top-level package boundary independent from `lang/`.
- Keep `vo-ui-core`, `vo-ui-reactive`, and `vo-ui-protocol` compatible with
  `no_std + alloc`.
- Platform code depends inward through the renderer protocol; core crates never
  import a platform backend.
- Preserve generational identities, atomic mutation batches, deterministic
  ordering, scoped cancellation, and one UI writer per root.
- Prefer compiler-generated direct updates for static component structure and
  retain the runtime reconciler for genuinely dynamic views.
- Keep `vo-ui-plan` compiler-neutral. Language adapters may depend on the plan
  ABI; the plan package does not import compiler crates.
- Route goroutine results through bounded `vo-ui-scheduler` turns. Worker tasks
  never retain mounted nodes or mutate the reactive graph.
- Join scheduler and renderer ownership through `vo-ui-session`; one drained
  turn may publish at most one renderer revision.
- Add conformance tests before extending protocol operations.
- Treat `ui/roadmap.toml` as a frozen foundation record. Active product work
  changes `product-roadmap.toml`, `capabilities.toml`, and `delivery.toml` and
  keeps `vo-dev ui-certify` consistent with them.
- Component Model V2 uses canonical package/object identities, separately
  compiled component definitions, generational mounted instances, and
  instance-owned state, handlers, effects, tasks, resources, and environment.
- Preserve render, atomic commit, post-commit effect, and cancel-before-cleanup
  disposal phases across VM, JIT, Native AOT, Core Wasm AOT, and headless.
- Add or update the owning capability entry, target matrix, dependencies,
  acceptance, and evidence whenever product behavior advances.
