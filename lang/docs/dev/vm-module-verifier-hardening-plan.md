# VM Module Verifier Hardening Plan

Status, 2026-06-08: `vo_common_core::verifier::ModuleVerifier`
already exists and is wired into `Vm::load`. This document records the
remaining development work for making the verifier the shared bytecode gate
across VM, JIT, no_std/embed, WASM, Studio, caches, and debugging tools.

This is a development plan, not a replacement for the bytecode or JIT specs.
Treat current Rust source as the implementation truth.

Related context:

- [`lang/crates/vo-common-core/src/verifier.rs`](../../crates/vo-common-core/src/verifier.rs)
- [`lang/crates/vo-vm/src/vm/mod.rs`](../../crates/vo-vm/src/vm/mod.rs)
- [`lang/crates/vo-jit/src/verifier.rs`](../../crates/vo-jit/src/verifier.rs)
- [`jit-fact-source.md`](jit-fact-source.md)
- [`gc-correctness-testing-framework.md`](gc-correctness-testing-framework.md)
- [`eng/tests.toml`](../../../eng/tests.toml)

## Goals

- Keep one shared verifier for bytecode/module facts that every backend must
  trust before execution.
- Keep JIT-only policy in `vo-jit`, after the shared verifier has accepted the
  module.
- Reject malformed generated, cached, serialized, embedded, and WASM bytecode
  as close as possible to the producing or accepting boundary.
- Make FFI, host-event, island, and JIT bridge contracts explicit enough that
  verifier work does not hide runtime protocol bugs.
- Route verifier changes through the same VM/JIT/GC/WASM/no_std test surfaces
  that depend on bytecode correctness.

## Non-Goals

- Do not move strict JIT lowering capability, helper ABI, OSR metadata, side
  exits, direct-call eligibility, or frame materialization policy into
  `vo-common-core`.
- Do not turn best-effort JIT into silent runtime compile-error recovery unless
  that semantic change is designed explicitly.
- Do not add a new ad hoc test runner. Use crate tests, `eng/*.toml`,
  `cmd/vo-dev`, and `./d.py`.
- Do not rename `JitInstructionMetadata` as part of unrelated verifier work.
  Clarify the contract first; a rename can be a follow-up migration.

## Current Architecture

`vo-common-core` owns the bytecode ABI and shared metadata:

- `Instruction` and `Opcode` live in `vo-common-core/src/instruction.rs`.
- `Module`, `FunctionDef`, `ExternDef`, `GlobalDef`, slot metadata, and
  per-PC instruction metadata live in `vo-common-core/src/bytecode.rs`.
- `ModuleVerifier` lives in `vo-common-core/src/verifier.rs`.

`Vm::load` is the execution gate:

1. Verify the module through `vo_common_core::verifier::verify_module`.
2. Register stdlib, linkme, native extension, or WASM externs.
3. Validate extern registration.
4. If strict JIT is enabled, run `vo_jit::verify_module_after_common`.
5. Finish loading globals, itab cache, sentinel errors, GC state, and JIT tables.

`vo-jit` is now layered on top of the shared verifier:

- `vo-jit::verify_module` first calls the common verifier.
- `vo-jit::verify_module_after_common` keeps strict JIT metadata policy:
  metadata kind, required metadata, loop metadata, and JIT-specific consistency.
- Semantic rows in `vo-jit/src/semantics/` remain the JIT fact source for
  lowering owner, capability, runtime path, helper return policy, effect
  contract, frame/trap/fail-fast policy, and verifier domain.

## Shared Verifier Contract

The shared verifier should own any fact the interpreter, GC, WASM runtime,
embed runner, or JIT must trust before executing a module:

- module entry and island-init function indices
- itab and named-method function references
- `FunctionDef.code.len() == FunctionDef.jit_metadata.len()`
- no `Opcode::Invalid`
- local/global slot ranges and slot layouts
- branch and loop target ranges
- constant, function, extern, global, struct, interface, and runtime metadata
  references
- static, dynamic, extern, closure, defer, go, and island call shapes
- return flags and return slot layout
- `has_defer`, `has_calls`, `has_call_extern`, `gc_scan_slots`, and
  `borrowed_scan_slots_prefix`
- `Interface0`/`Interface1` pairing in locals, globals, returns, captures, and
  metadata-provided layouts
- GC layout and write-barrier-sensitive metadata

`vo-jit` should continue to own facts that only matter to strict JIT:

- opcode lowering capability and lowering owner
- strict metadata kind policy and legacy metadata rejection
- `Hint/HINT_LOOP` plus `LoopEnd` backedge and offset consistency
- Cranelift verifier failures
- helper/callback ABI and JitContext offset policy
- OSR live-in/live-out and loop metadata policy
- direct-call, frame elision, side-exit, and VM call materialization policy
- strict-vs-best-effort JIT failure classification

## Workstream A: Finish Shared Verifier Coverage

### A1. Module-Level Cross References

Current verifier coverage is strong for function bytecode and GC layout. Extend
module-level checks so malformed metadata cannot survive until runtime:

- Validate `runtime_types` references to struct/interface metadata.
- Validate `well_known` entries against the module tables they reference.
- Validate global `ValueKind`/`ValueMeta` consistency and metadata ids.
- Validate debug info function/file references and PC ordering where practical.
- Keep checks no_std-compatible inside `vo-common-core`.

Tests:

- Add focused unit tests in `vo-common-core/src/verifier.rs`.
- For each new invariant, include one rejecting case and one valid case.
- Add a single `Vm::load` rejection test only when the invariant materially
  changes user-visible load behavior.

### A2. Instruction Metadata Shape

`JitInstructionMetadata` is now consumed by the shared verifier for VM-visible
layout facts. Clarify and tighten that boundary:

- Document that the type currently stores instruction metadata, not purely JIT
  metadata.
- Validate metadata layout vectors themselves as legal `SlotType` layouts,
  especially interface pairs.
- Validate `CallExternLayout` width against encoded arg slots and
  `ExternDef.ret_slots`.
- Validate closure capture slot layout against capture transfer metadata.

Do not move semantic-row strict metadata policy into `vo-common-core`.

### A3. Error Surface

Keep `ModuleVerificationError` backend-neutral:

- Error messages should say `instruction metadata` or `module metadata`, not
  `JIT metadata`, unless the error is produced by `vo-jit`.
- Avoid converting module-level verifier failures into misleading function-level
  JIT errors where a clearer wrapper is possible.
- Keep `VerifiedModule<'m>` as a borrow token for immediate load/use. JIT may
  keep its digest-based `VerifiedModule` for mutation detection after the
  common borrow ends.

## Workstream B: Verify Bytecode At Producing And Accepting Boundaries

Execution paths already pass through `Vm::load`; compile-only and display paths
need an explicit policy.

### B1. Post-Codegen And Serialize Gates

Run `ModuleVerifier` before writing or caching bytecode:

- native `vo build`
- native `vo emit`
- compile cache save/load paths
- `vo-web` compile output
- Studio compile/check output that serializes bytecode
- term-handler and GUI bytecode caches

The point is not just safety. It also catches codegen regressions closer to the
producer and prevents cache artifacts from storing invalid modules.

### B2. Dump And Inspection Tools

When a tool accepts bytecode, it should be clear whether it verifies it.

- `vo dump` should verify by default or expose a clearly named raw mode.
- Studio `dumpEntry` should report verifier errors before formatting.
- The formatter should optionally include per-PC instruction metadata summaries:
  call layouts, extern layouts, heap-return facts, queue/map/slice/interface
  layouts, and return flags.

## Workstream C: Runtime Protocol Hardening

Some high-risk contracts cannot be solved by `ModuleVerifier` because they are
runtime or host-protocol facts.

### C1. Fiber And Island Correlation

Raw `fiber_id` values can be reused after a fiber dies. Add generation or
correlation data to host/island responses:

- `FiberId { index, generation }`, or equivalent encoded token.
- Remote queue/island request ids keyed by endpoint, operation, and fiber.
- Validation before wake or endpoint response handling.
- Regression tests for stale wake, delayed response, duplicate response, and
  reused fiber slot.

### C2. Host Event Replay Contract

`ExternCallContext` requires consumers to take host-event resume token/data, but
post-call verification currently gives stronger treatment to closure replay and
I/O resume than to host-event replay.

Add one-shot validation for:

- `resume_host_event_token`
- `resume_host_event_data`
- replay event token handling in app-runtime mailbox paths

### C3. JIT Extern Bridge Equivalence

Interpreter and JIT extern paths should agree for every `ExternResult`:

- `Ok`
- `Panic`
- `Yield`
- `Block`
- `WaitIo`
- `HostEventWait`
- `HostEventWaitAndReplay`
- `CallClosure`

In particular, the JIT bridge must keep host-event wait/replay and closure-call
payloads in VM-owned suspend state; contract tests should prove token/data and
resume PCs survive the transition without duplicating side effects.

### C4. Native Extension Loader

Harden native extension loading:

- Reject `entry_count > 0` with null `entries`.
- Reject duplicate extern names transactionally.
- Require every entry to carry the exact canonical module owner selected by
  the extension manifest and reject packages outside that owner.
- Keep ABI version and fingerprint checks mandatory.
- Fingerprint the size, alignment, and every field offset of `ExtensionTable`
  and `ExternEntry`; keep contract tests for both layouts and result payload
  obligations.

### C5. FFI Return Validation

Debug-only return validation is useful but not enough for unsafe native
extensions.

- Add release-like tests for bad `GcRef` and interface returns.
- Prefer safe return APIs such as `ret_ref` for GC-scanned returns.
- Decide whether raw `set_slot` into GC-scanned return slots should be guarded,
  documented unsafe, or validated at the VM boundary.

## Workstream D: JIT Boundary Discipline

The common verifier should not grow into a second JIT verifier.

- Keep semantic rows in `vo-jit/src/semantics/` as the JIT fact source.
- Keep lowering capability, helper ABI, OSR, and frame materialization in
  `vo-jit` and `vo-vm/src/vm/jit`.
- Clarify best-effort JIT semantics: initialization/load-time strict metadata
  policy is skipped, but runtime hot-call or OSR compile failures may still be
  `JitError` unless explicitly redesigned.
- Consider a JitContext ABI snapshot or version/hash gate when adding fields,
  offsets, helpers, or callbacks.
- Reduce duplicated helper symbol lists over time by deriving more from the
  runtime manifest.

## Workstream E: Tests And CI Coverage

### E1. Unit Test Placement

Use this placement rule:

- `vo-common-core`: shared verifier unit tests.
- `vo-vm`: `Vm::load` rejection tests and runtime scheduler/protocol tests.
- `vo-jit`: strict JIT metadata, semantic row, helper ABI, OSR, and
  materialization tests.
- `vo-runtime`: GC/FFI/native extension loader contract tests.
- `tests/lang`: language-level regressions for behavior visible from `.vo`.

### E2. Language Test Targets

Use manifest targets deliberately:

- ordinary VM-shared invariant: `vm,jit`
- GC-sensitive invariant: `gc-vm,gc-jit`
- JIT-only policy: native JIT/OSR targets
- no_std/embed behavior: `nostd`
- browser/host-event behavior: `wasm`

### E3. CI Coverage

The pull-request workflow has no changed-file routing. Every pull request runs
the same four lanes:

- `quality-rust`: repository lint, language-manifest lint and formatting,
  Action workflow lint, Clippy, and all root-workspace tests;
- `language-native`: the `smoke` language cases on VM, JIT, and compile targets;
- `wasm-web`: the complete WASM language target plus `vo-web`, Studio Web,
  and standalone Studio WASM checks;
- `studio-native`: standalone Tauri Clippy and tests on macOS.

A push to `main` expands `language-native` to VM, JIT, OSR, GC, no_std, and
compile targets. Nightly runs the release-profile native and WASM matrices,
stress repetitions, macOS and Windows workspace tests, and dependency audits.
Verifier changes therefore receive the same fixed CI surface regardless of the
path that changed. Start with the focused owner tests below, then reproduce the
affected fixed lanes locally before merging.

## Workstream F: Documentation Cleanup

Fix stale references that now mislead verifier and VM work:

- Update `lang/docs/spec/vm-bytecode.md`:
  - `Chan*` should be current `Queue*` terminology.
  - Include `IslandNew`, `GoIsland`, and `ForLoop`.
  - Refresh `FunctionDef` and `Module` fields for current GC/JIT/island data.
- Update GC docs that refer to old `vo-vm/src/gc_layout_validate.rs`; the
  shared verifier now lives in `vo-common-core/src/verifier.rs`.
- Update references to `vo-jit/src/semantics.rs`; the fact source is now the
  `vo-jit/src/semantics/` module directory.
- Reconcile WASM time docs and manifest skip reasons with the current
  `vo-web/runtime-wasm` implementation.
- Keep historical `dev-notes/` as context, but prefer `lang/docs/dev/` and
  `lang/docs/spec/` for active maintainer contracts.

## Suggested Implementation Order

1. Add direct `vo-common-core` verifier tests for the next missing invariant.
2. Extend `ModuleVerifier` for runtime/well-known/global/debug cross refs.
3. Add verifier calls before cache/artifact serialization in engine and
   `vo-web`.
4. Add verification to `vo dump` and Studio dump/cache acceptance points.
5. Harden native extension loader null/duplicate entries.
6. Add host-event token/data post-call validation.
7. Add VM/JIT extern bridge equivalence contract tests.
8. Add fiber generation/correlation tokens for host/island responses.
9. Keep `vo-common-core` covered by the fixed quality and language lanes.
10. Clean active docs and manifest skip reasons.

## Validation Commands

Start narrow:

```sh
cargo test -p vo-common-core verifier
cargo test -p vo-vm module
cargo test -p vo-jit verifier
```

When a change crosses runtime or JIT boundaries:

```sh
cargo test -p vo-runtime ffi
cargo test -p vo-vm --features jit
cargo test -p vo-jit
```

For language-level and platform coverage:

```sh
./d.py test both tests/lang/cases/<case>.vo
./d.py test osr tests/lang/cases/<case>.vo
./d.py test gc tests/lang/cases/<case>.vo
./d.py test nostd tests/lang/cases/<case>.vo
./d.py test wasm tests/lang/cases/<case>.vo
```

For PR-level confidence after verifier or bytecode changes:

```sh
cargo run -q -p vo-dev --locked -- lint all
cargo run -q -p vo-dev --locked -- test lint --suite lang --strict
cargo clippy --workspace --all-targets --locked -- -D warnings
cargo test --workspace --all-targets --locked
cargo run -q -p vo-dev --locked -- test run --suite lang --targets vm,jit,osr,gc-vm,gc-jit,nostd,compile
cargo run -q -p vo-dev --locked -- test run --suite lang --targets wasm
```

## Done Criteria

A verifier hardening change is complete when:

- invalid bytecode is rejected at the earliest relevant boundary
- the error is backend-neutral unless it is truly JIT-only
- execution paths still reject through `Vm::load`
- strict JIT failures remain fail-fast and are not confused with side exits
- no_std and WASM users see the same shared verifier behavior
- tests exist at the owning layer, not only as end-to-end language cases
- the fixed CI and nightly lanes cover the affected verifier consumers
- active docs point to current source paths and terminology
