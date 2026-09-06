# Volang UI workspace rules

Paths below are relative to the repository root. Run Cargo and `vo-dev` there;
`ui/` is a member of the root Cargo workspace. Read `ui/docs/architecture.md`
and the relevant capability's implementation before changing its contract.

## Ownership

- Keep the framework boundary independent from the language implementation.
  `ui/crates` owns renderer-neutral contracts; `lang/crates/vo-ui-*` owns
  compiler, VM, native platform, and AOT runtime adapters. Framework crates
  must not acquire dependencies on language or platform implementation crates.
- Preserve `no_std + alloc` wherever declared, including `vo-ui-core`,
  `vo-ui-reactive`, `vo-ui-protocol`, `vo-ui-runtime`, and `vo-ui-session`.
- Keep `vo-ui-plan` compiler-neutral and `vo-ui-artifact` responsible for
  bounded component artifacts. Adapters depend inward through versioned
  plans, artifacts, events, renderer mutations, and system-service contracts.
- Keep application APIs in typed Vo packages. Browser DOM/system glue belongs
  to `lang/crates/vo-web/js`; application projects keep their current pure-Vo
  source and dependency model.

## Component and rendering contracts

- Preserve generational identities, atomic mutation batches, deterministic
  ordering, scoped cancellation, and one UI writer per root.
- Prefer compiler-generated direct updates for proven static component
  structure; retain bounded keyed reconciliation for dynamic views.
- Component Model V2 uses canonical package/component identities, separately
  compiled definitions, parent-local sibling keys, generational mounted
  instances, and instance-owned state, handlers, effects, tasks, resources,
  and environment. Preserve the VUB1 component-bundle path and the certified
  VUA1 baseline; read versions and limits from their owning codecs.
- Preserve render, atomic commit, post-commit effect, and cancel-before-cleanup
  disposal phases across the affected VM, JIT, Native AOT, Core Wasm AOT,
  browser VM, and headless paths. External effects belong in handlers, tasks,
  or post-commit work; keep render replayable.
- Route worker results through bounded, generation-checked `vo-ui-scheduler`
  turns. Workers never retain mounted nodes or mutate the reactive graph.
  `vo-ui-session` joins scheduler and renderer ownership; one drained turn
  publishes at most one renderer revision.
- Commit renderer, layout, paint, accessibility, focus, and listener identity
  consistently. Preserve transactional reload and invalidate disposed task,
  handler, effect, and resource generations before cleanup.
- Pair protocol changes with conformance tests for bounds, stale identities,
  malformed frames, and atomic rejection. Update relevant producers, decoders,
  and browser/native consumers together.

## Product governance and compatibility

- `ui/roadmap.toml` is the frozen foundation record. Active product declarations
  live in `ui/product-roadmap.toml`, `ui/capabilities.toml`, and `ui/delivery.toml`.
  Derive completion and maturity from those files and executable evidence.
- When product behavior changes, update affected capability ownership, target
  support, dependencies, acceptance, evidence, and permanent contract probes.
  Check `ui/module-profiles.toml`, `ui/kit/catalog.toml`, and Studio parity files
  when their corresponding package or product surface changes.
- Follow `ui/docs/release-policy.md` for API stability, deprecation, and format
  compatibility. Internal format evolution must preserve stable public source
  APIs and certified invariants.
- `ui/certification.toml` declares foundation gates; `ui/quality-matrix.toml`
  and `ui/product-certification.toml` declare product evidence and release
  identity. Keep their readers in `cmd/vo-dev/src/ui_certification.rs` and
  `cmd/vo-dev/src/ci` consistent with declaration changes.

## Validation

- Start with the owning crate and affected contract probe. Select backend and
  real-browser/real-window checks from the capability and quality matrix;
  headless coverage alone cannot establish platform behavior.
- Run `cargo run -q -p vo-dev --locked -- ui-certify --check` for declaration
  changes. It reports `declaration-valid`. Complete product certification uses
  `ui-certify --evidence <ci-bundle>` with verified evidence for the same source
  commit across the required Web, Linux, macOS, and Windows lanes.
- Use `VOWORK=off` for baseline regressions; enable workspace overrides when
  testing them deliberately. Read `docs/ci.md` and current command/scripts for
  the relevant lane, prerequisites, and generated outputs.
- For UI documentation included by `lang/docs/catalog.toml`, regenerate
  `apps/studio/documentation` through `vo-dev generate studio-docs --write`
  when the source edit requires it, then run `vo-dev lint docs`.
- Instruction-only edits need structural and reference checks. Report the
  checks actually run and any affected platform coverage left unverified.
