---
name: volang-dev
description: Develop, debug, review, and maintain the Volang language, execution backends, UI framework, Studio, and repository automation. Use for work in the Volang repository and its compiler/runtime or first-party integration contracts.
---

# Volang Development

## Work from current authority

- Read applicable `AGENTS.md` files and `git status --short`; use the current worktree and preserve unrelated changes. Paths use repository-root or crate-name prefixes; resolve crate prefixes through `Cargo.toml`.
- Specifications under `lang/docs/spec` define the public language and runtime contract. Source, tests, and machine-readable manifests establish implementation and coverage. Report disagreements explicitly; a failing implementation does not authorize silently changing the specification.
- Continue authorized fixes, local builds, and required regeneration within the user's scope. A review-only request stays read-only. Reuse authorization already given; this skill adds no separate confirmation step for ordinary local work. Publishing and other external mutations need authorization covering that action.
- Check the applicable API/format stability policy before redesigning internals. Unreleased internal protocols may evolve; preserve declared stable public contracts and required migration behavior, including `ui/docs/release-policy.md`.
- Read relevant command help or its parser before using unfamiliar automation. When related manifests and readers are dirty together, check their consistency before invoking them.

## Route by owner

- Compiler: `vo-syntax` parses; `vo-analysis` resolves dependency-ordered packages and per-package `TypeInfo`; `vo-codegen` emits portable bytecode; `vo-engine` freezes compile inputs, verifies output, and orchestrates execution and AOT preparation.
- Shared contracts: `vo-common-core` owns bytecode, runtime type metadata, extern identity, serialization, verification, `execution_effects`, and `instruction_effects`. `vo-common` owns sources, diagnostics, VFS, and compiler ABI helpers. `vo-target` owns canonical target capabilities and target-dependent verification.
- Native execution: `vo-runtime` owns slots, objects, GC, FFI, and JIT callbacks; `vo-vm` owns loading, interpretation, fibers, scheduling, roots, and runtime transitions; `vo-jit` owns Cranelift JIT/OSR and Native AOT lowering (`vo-jit/src/aot.rs`). `vo-aot-runtime-core` validates embedded native images; `vo-aot-runtime` supplies the CLI static runtime.
- Core Wasm AOT: `vo-wasm-aot` owns generated Wasm execution and its memory/frame machinery. `vo-web/js` owns the JavaScript host; `vo-aot-support-wasm` supplies optional semantic support. Read `docs/aot.md` and the actual producer/consumer constants when changing image or host ABI.
- Modules and release: `vo-module` owns identity, schemas, authority, solving, lock/cache/readiness, workspaces, and lifecycle; `vo-release` stages and publishes user modules; `cmd/vo-dev` owns repository releases.
- Stdlib and extensions: `lang/stdlib` is the canonical Vo source set, embedded by `vo-stdlib-source`; `vo-stdlib` supplies portable and host providers. `vo-ffi-macro` checks `#[vo_fn]` / `#[vostd_fn]` declarations; `vo-runtime::ffi` resolves and freezes providers; `vo-ext` is the extension SDK.
- Apps: `vo-app-protocol` owns wire identities/codecs; `vo-schema-compiler` compiles protocol schemas; `vo-app-runtime` owns host/session, service, provider, and scheduling contracts; `vo-app-host-native` owns native host adapters. `lang/protocol/app-runtime/app.schema.toml` owns the generated App protocol.
- UI: read `ui/AGENTS.md` and `ui/docs/architecture.md` for framework work, including adapters outside `ui/`. `ui/` owns renderer-neutral contracts, Rust crates, and Vo packages. `lang/crates/vo-ui-*` holds language/platform adapters; `vo-ui-aot-runtime-native` supplies the UI static runtime. These packages share the root Cargo workspace.
- Web and Studio: `vo-web` owns browser VM compilation/execution, VFS, and browser UI hosts; `vo-web/runtime-wasm` supplies Wasm VM host providers. `apps/studio` owns the application and its native hosts. Check `lang/crates/vo-web/package.json` before selecting Web scripts.
- Commands and tests: `cmd/vo` is the user CLI; `cmd/vo-embed` exercises embedding; `cmd/vo-dev` selects and orchestrates repository checks; `cmd/vo-test` executes generated native plans and backend differentials.
- Examples and benchmarks: `examples/manifest.toml` and `benchmarks/manifest.toml` own root catalogs; app/UI catalogs have separate owners. `vo-dev bench` owns measurement and transient results; inspect its backend coverage and prerequisites before reporting performance.

## Preserve compiler and backend contracts

- Carry canonical package identity through resolution, analysis, codegen, runtime type names, visibility, and extern identity. Host paths must not determine semantic identity. Keep per-package `TypeInfo`, initialization order, fallible layouts, physical slots, transfer metadata, and GC scan metadata consistent.
- Pass executable modules through common verification and the applicable `vo-target` / `vo-engine::verify_compile_output_for_target` boundary. Preserve the additional strict JIT/Native AOT verification and emitted-Wasm validation.
- For opcode changes, update instruction encoding, metadata, codegen, common verification, serialization, VM dispatch, JIT/OSR/Native AOT lowering, and Core Wasm AOT lowering. Canonical execution effects live in `vo-common-core/src/execution_effects.rs`; register/frame effects live in `vo-common-core/src/instruction_effects.rs`. JIT capability rows in `vo-jit/src/semantics` consume the common contract and describe backend routes. Avoid independent effect lists in backends.
- Keep optimization, frame elision, shadow roots, materialization, suspension, and unwind decisions derived from verified layouts and resolved effects. A transition must preserve roots, panic/return state, and exactly-once guest effects.
- Treat Wasm VM and Core Wasm AOT as separate implementations. Core Wasm AOT images execute generated code; unsupported lowering must fail at build time. Check both generated guest code and `vo-web/js` host behavior for memory, extern, scheduling, or UI changes.
- For AOT changes, align image versions, embedded metadata, target capabilities, runtime archives, extension identity, and `vo-engine/src/aot_cache.rs` cache keys. Use `vo-target` for supported target/artifact combinations; test a linked executable when linker or static-runtime behavior changes.
- Preserve transactional extern loading: canonical `(package, function)` identity, declared ABI/effects, provider ownership, complete-table resolution, then registry freeze. Read ABI versions/fingerprints from their owners; update all producers, loaders, and generated wrappers together.
- Keep module-aware builds read-only over `vo.mod` and `vo.lock`; build-time auto-install may authenticate locked bytes into cache. Use explicit `vo mod` lifecycle commands to change the dependency graph.

## Runtime memory and host boundaries

Apply the SpanHeap rules to `vo-runtime`-based VM/JIT/Native AOT and Wasm VM work. For Core Wasm AOT, also inspect its generated collector, allocator, roots, and host admission; shared semantics need coverage on both implementations.

- Read `docs/game-memory-architecture.md` and normative `lang/docs/spec/runtime-memory.md`. `vo-runtime/src/gc/heap.rs` owns SpanHeap; `vo-runtime/src/gc.rs` owns collector policy, leases, and telemetry; `vo-runtime/src/gc_types.rs` owns resumable tracing; `vo-vm/src/gc_roots.rs` owns VM roots and host controls.
- Preserve one stable-address managed heap and collector per Island. Child Islands inherit admission policy while retaining independent occupancy, roots, counters, and errors. Root scanning remains precise across globals, interfaces, frames, defer/unwind, scheduler/replay state, and native shadow/materialized frames.
- Keep every GC phase work-bounded. Root scans, nested inline layouts, remembered cards, remark, sweep, and large-span reclaim retain cursors across `gc_step_units` calls. Audit dirty roots whenever references move or mutate.
- Route GC-bearing stores through the typed new-value barrier, maintaining incremental shading and old-to-young card marking across interpreter, generated native code, containers, stdlib, and FFI.
- Allocation failures must reach the scheduler before generated native code dereferences a null result or performs later guest effects. Allocation/container helpers leave `Gc::last_memory_error` for the VM scheduler to convert to sticky `VmError::IslandMemory`.
- Keep map buckets and queued guest payload slots in managed backing. Waiters, providers, JIT code, GPU, audio, and JavaScript allocations remain separate telemetry/budget domains. A no-growth transition is fallible and pre-admits object/index/gray/lease capacity; the managed hard limit covers SpanHeap committed bytes.
- Extensions retaining managed objects across calls or safe points use `GcLease`; stale or foreign generations fail closed. Preserve the host-owned GC dispatch proxy in `vo-runtime::ffi`: collector-owned Rust allocations are created and destroyed by the same allocator image.
- Wasm VM admission in `vo-web/src/vm.rs` uses 64 KiB pages, pre-grows `current + reserve`, and verifies `current + hard_limit` against the declared maximum. Core Wasm AOT has its own image/host memory admission contract.
- For app protocol changes, align schema/codecs, generations, bounded lanes, wait/replay keys, exit behavior, render buffering, and provider ownership across stdlib, VM, `vo-app-runtime`, and active hosts. Preserve feature-gated native, `no_std`, and Wasm adapters.

## Automation, workspaces, and documentation

- Run repository commands from the root. Use `VOWORK=off` for baseline regressions and release reproduction; enable workspace overrides deliberately when testing them. `vo.work` declares local module members; `eng/project.toml` declares sibling repositories and exact pins. Use `vo-dev first-party` helpers and verify actual pins before relying on sibling source.
- `d.py` is a compatibility dispatcher; command policy belongs in `cmd/vo-dev` and its manifests. Inspect command outputs, environment, network/tool needs, and tracked writes. `tool bootstrap --apply` installs tools; first-party commands can provision checkouts.
- `eng/artifacts.toml` owns governed outputs and generators. Regenerate affected tracked artifacts through the declared generator when the authorized source change requires it, then validate provenance and bytes. Use `--check` for verification; avoid hand-editing generated outputs.
- Maintained language documentation lives in `lang/docs/guides`, `lang/docs/spec`, and `lang/docs/vo-for-gophers.md`; `lang/docs/catalog.toml` selects Studio content, including UI pages. `apps/studio/documentation` is generated by `vo-dev generate studio-docs --write`; `vo-dev lint docs` checks it. App protocol outputs use `vo-dev generate app-protocol --write|--check`.
- Treat `lang/docs/dev-notes` and `lang/docs/outdated` as history. Check status and current implementation before relying on `lang/docs/dev` plans. Use current manifests/readers for active automation; removed rewrite plans are historical context.
- `eng/ci.toml` and `cmd/vo-dev/src/ci` own CI planning and evidence; `.github/workflows` execute the lanes. Read `docs/ci.md` for local equivalents. `ci lint` and local plans inspect declarations; certifiable evidence is bound to the exact source commit, configuration, jobs, results, and artifact digests.
- Keep this repository skill in `skills/volang-dev/SKILL.md`. `vo-dev lint skill` enforces a single file, `name`/`description` frontmatter, and at most 160 lines. Link to maintained owners for conditional detail; check this policy before adding skill resources or metadata files.

## Validate the affected contract

- Start with the owning crate and focused behavioral regressions. Select language cases through `tests/lang/manifest.toml`; targets, aliases, matrices, environment, and timeouts come from `eng/tests.toml`. Keep discovered cases synchronized with the manifest.
- `vo test` runs user-project tests; `vo-dev test` and `./d.py test` run repository regressions. `vo-test` accepts generated `run-plan` input. Its JIT jobs must enter compiled code; interpreter-only success does not demonstrate JIT coverage.
- A `native` matrix selects VM/JIT/OSR; it does not establish Native AOT coverage. Use relevant AOT crate/engine tests and build/run probes from CI for Native AOT. Select `wasm-aot` explicitly for Core Wasm AOT; it can have different skips from `wasm`.
- For memory changes, cover affected limits/reuse, work bounds, roots/cards, OOM, managed backing, Island transfer/policy, leases, `runtime/mem`, and Wasm admission. Include `vo-runtime` and `vo-vm`; add JIT/AOT, engine, Web, SDK, and host tests where the boundary changes.
- For UI changes, use `ui/quality-matrix.toml`, `ui/certification.toml`, and `ui/product-certification.toml` to select contract probes and platform evidence. Update capability, delivery, acceptance, and evidence declarations when their behavior changes. `ui-certify --check` establishes `declaration-valid`; `product-certified` requires `--evidence <ci-bundle>` with verified complete platform evidence.
- Read `rust-toolchain.toml` and `eng/toolchains.toml` for tool versions. Use locked dependencies. Serialize Cargo commands in a shared output tree; if isolation is needed, prefer `--target-dir` so tests spawning Cargo can keep their own fixture outputs.

Useful focused commands, chosen by scope:

```sh
VOWORK=off cargo test -p <owning-crate> --locked
VOWORK=off cargo check -p <owning-crate> --all-targets --locked
cargo run -q -p vo-dev --locked -- test lint --suite lang --strict
cargo run -q -p vo-dev --locked -- test plan --suite lang --targets vm,jit --path <case-path>
cargo run -q -p vo-dev --locked -- test run --suite lang --targets vm,jit --path <case-path>
cargo run -q -p vo-dev --locked -- lint skill
cargo run -q -p vo-dev --locked -- ui-certify --check
```

- For wider Rust/automation changes, follow `CONTRIBUTING.md` and the relevant CI lane. For instruction-only edits, check skill structure, referenced owners/paths, and the diff. Avoid running unrelated backend suites or stacking overlapping aggregate gates.
- Reuse successful checks only while their inputs, command, configuration, pins, and environment remain unchanged. Report actual checks, outcomes, side effects, and material coverage gaps.

## Release and completion

- Bind certification, build identity, artifacts, and publication to the same clean tagged commit with verified protected-main CI evidence. Derive timestamps from that commit, verify the full declared asset set and digests, serialize publication, and keep retries idempotent. `eng/release.toml` and its readers own current release policy.
- SDK publication, Homebrew updates, Pages deployment, and public release publication each need matching external-action authorization. Complete authorized preparation and validation before seeking any missing approval.
- Re-read the final diff, preserve unrelated work, and verify generated-state policy. State what changed, why, what was checked, and any remaining limitation.
