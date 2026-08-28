---
name: volang-dev
description: Maintain and review the Volang repository across the compiler, module system, bytecode, VM, JIT, GC, stdlib, FFI, CLI, language tests, Web/WASM, app runtime, engineering automation, releases, docs, examples, benchmarks, and governed artifacts.
---

# Volang Development

## Work from current authority

1. Read repository instructions and `git status --short`; preserve unrelated user changes.
2. Prefer implementation, tests, and machine-readable manifests over README claims and dated plans. Treat `lang/docs/spec` as intended public behavior and verify shipped behavior in source and tests.
3. Confirm scope before mutation. Review and diagnosis stay read-only; publishing, deployment, commits, pushes, dependency installation, sibling checkout provisioning, and tracked generation require matching authorization.
4. When related control files are simultaneously dirty, audit their static protocol consistency before running their commands.
5. Remove obsolete internal protocols when a redesign is authorized; Volang has no compatibility obligation for unreleased internals.

## Route by owner

- Compiler flow: `vo-syntax` parses; `vo-analysis` resolves dependency-ordered packages and per-package `TypeInfo`; `vo-codegen` lowers them into bytecode; `vo-engine` freezes inputs and orchestrates compile, verification, and execution.
- Shared contracts: `vo-common-core` owns bytecode, runtime types, extern identity, serialization, and common verification; `vo-common` owns sources, spans, diagnostics, VFS, and compiler-side ABI helpers.
- Runtime layers: `vo-runtime` owns slots, objects, precise GC, FFI, and JIT callback ABI; `vo-vm` owns module loading, interpretation, fibers, scheduling, roots, and runtime transitions; `vo-jit` owns native/OSR compilation and strict post-common verification.
- Runtime memory: read `docs/game-memory-architecture.md` for the accepted architecture and `lang/docs/spec/runtime-memory.md` for the normative contract. `vo-runtime/src/gc/heap.rs` owns the Island SpanHeap; `gc.rs` owns policy, leases, telemetry, and collector state; `gc_types.rs` owns resumable object tracing; `vo-vm/src/gc_roots.rs` owns VM root scanning and host controls.
- Modules and release: `vo-module` owns identity, schemas, authority, solving, lock/cache/readiness, workspaces, and lifecycle; `vo-release` owns user module release staging and publication; `vo-dev release` owns repository release automation.
- Stdlib and extensions: `lang/stdlib` is the canonical Vo source set; `vo-stdlib` embeds it and supplies portable or `std` host providers; `vo-ffi-macro` validates wrappers against Vo declarations; `vo-runtime::ffi` resolves and freezes providers; `vo-ext` is the extension SDK.
- Web and apps: `vo-web` owns browser compilation/runtime and VFS; `vo-web/runtime-wasm` supplies WASM host providers; `vo-app-runtime` owns generic host/session protocol.
- Commands and tests: `cmd/vo` is the user CLI; `cmd/vo-embed` exercises the embedded path; `cmd/vo-dev` owns repository development commands; `cmd/vo-test` only executes generated native plans and backend differentials.
- Test and content catalogs: `tests/lang/manifest.toml` owns language cases; `eng/tests.toml` owns targets, aliases, matrices, and environment. Root examples and benchmarks use their manifests; app example catalogs have separate lint/format ownership.

## Preserve cross-layer contracts

- Carry canonical package identity through module resolution, analysis, codegen, runtime type names, visibility, and extern identity. Never derive semantic identity from incidental host paths.
- Treat per-package `TypeInfo`, package initialization order, fallible type-layout facts, physical slot layouts, transfer metadata, and GC scan metadata as one compiler/runtime ABI.
- Pass every executable module through `vo-common-core` verification. Strict JIT adds `vo-jit` verification after the common verifier.
- Route opcode changes through instruction encoding, bytecode metadata, codegen, common verification, VM dispatch, serialization, `vo-jit/src/semantics`, lowering/runtime-path policy, and applicable backend regressions. Keep JIT capability, effects, metadata, register effects, and verifier domain derived from the semantic row.
- Treat GC roots as precise `SlotType`-driven state across globals, interfaces, frames, defer/unwind, scheduler/replay state, and JIT materialization. Audit dirty-root transitions whenever references move or mutate.
- Preserve one stable-address managed heap and collector per Island. Child Islands inherit admission policy while keeping heap occupancy, roots, counters, and errors independent.
- Keep every GC phase work-bounded: root, object, nested inline layout, remembered cards, remark, sweep, and large-span reclaim must retain a cursor across `gc_step_units` calls.
- Route every GC-bearing mutation through the typed new-value barrier. Interpreter, JIT, stdlib, runtime containers, and FFI must maintain both incremental shading and old-to-young card marking.
- Keep JIT allocation failure paths side-effect safe: generated code must leave the current fiber before dereferencing a null result or performing a later guest-visible effect.
- Do not consume `Gc::last_memory_error` inside allocation or container constructors. The VM scheduler owns conversion to sticky `VmError::IslandMemory`.
- Keep map buckets and queued guest payload slots in managed runtime backing. Treat waiter, endpoint, provider, JIT, GPU, audio, and JavaScript memory as separate telemetry/budget domains.
- A no-growth transition is fallible and must pre-admit collector object/index/gray/lease capacity. The managed hard limit covers SpanHeap committed bytes; document any wider product budget separately.
- Native extensions retaining a managed object across a call or safe point must use ABI-v10 `GcLease`; stale or foreign generations fail closed.
- WASM admission uses 64KiB pages, pre-grows `current + reserve`, and conservatively verifies `current + hard_limit` against the declared maximum.
- Preserve transactional extern loading: canonical `(package, function)` identity, declared ABI/effects, provider ownership, complete-table resolution, then registry freeze. Current extension macros are `#[vo_fn]` and `#[vostd_fn]`.
- Keep module-aware builds read-only over `vo.mod` and `vo.lock`; build-time auto-install may authenticate locked bytes into cache. Use explicit `vo mod` lifecycle commands to change the dependency graph.
- Audit `no_std`, native, WASM, and feature-gated paths where they apply. Native engine and bare `vo-web` prepare projects and extensions through distinct adapters.
- For app/runtime protocol changes, align event IDs, wait/replay keys, exit codes, render buffering, and island envelopes across stdlib, VM, `vo-app-runtime`, and active hosts.

## Use repository automation deliberately

- Treat machine-readable manifests and their readers in `cmd/vo-dev` as combined contracts. Do not duplicate policy in `d.py` or ad hoc shell snippets.
- Read current command help before invocation. Inspect tools, repositories, cwd/env, outputs, timeout, browser/network needs, and tracked writes.
- `tool bootstrap --apply` can install tools. Repository provisioning can fetch sibling repositories, and some commands can replace declared generated outputs.
- Inspect both `vo.work` and `eng/project.toml` for sibling work. Validate local hints and exact repository pins before relying on sibling source.
- Treat `eng/artifacts.toml` as the governed-artifact registry. Only entries with declared generators are generated; run validators for read-only checks and generators only with authorization to change tracked bytes.
- Keep source docs in `lang/docs/spec` and `lang/docs/vo-for-gophers.md`; app-visible copies are generated mirrors. Treat `dev-notes` and `outdated` as history, and read status fields before interpreting `lang/docs/dev` plans.
- For Web and release work, inspect the owning workspace scripts and `vo-dev` command implementation before running automation.

## Validate proportionately

- Pair language cases with owning crate tests. Keep discovered language cases exactly synchronized with `tests/lang/manifest.toml`; select VM, JIT, OSR, GC, nostd, WASM, or compile targets from `eng/tests.toml` according to the contract.
- For runtime-memory changes, cover SpanHeap reuse/limits, strict GC work bounds, generational reachability, dirty roots/cards, Interpreter and generated-JIT OOM, container backing, cross-Island unpack, lease generation/capacity, child-Island policy, `runtime/mem`, and WASM admission. Run both `vo-runtime` and `vo-vm`; include `vo-jit`, `vo-engine`, `vo-web`, `vo-ext`, and active app owners when their boundary changes.
- Distinguish `vo test` for user projects from `./d.py test` or `vo-dev test` for repository regressions.
- Start narrow, then widen only for affected cross-layer contracts. Useful primitives include:

```sh
cargo fmt --all -- --check
cargo test -p <crate> --locked
cargo check -p <crate> --all-targets --locked
cargo run -q -p vo-dev --locked -- test lint --suite lang --strict
```

- Run only one Cargo command at a time in the shared worktree unless target directories and fixtures are isolated.
- Reuse a successful check only while its source, command, configuration, pins, and environment remain unchanged. Avoid stacking overlapping aggregate gates.
- Report checks run, results, side effects, and material checks left unrun.

## Release boundaries

- Bind certification, build identity, artifacts, and publication to the same clean tagged commit. Derive timestamps from that commit, verify the complete asset set and digests, serialize public publication, and keep retries idempotent.
- Treat SDK publication, Homebrew updates, Pages deployment, and public release publication as separately authorized external mutations.

## Finish

Re-read the diff, verify ownership and generated-state policy, confirm tests cover the changed contract, and state remaining risks or validation gaps directly.
