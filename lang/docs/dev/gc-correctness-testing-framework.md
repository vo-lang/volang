# GC Correctness Testing Framework

Status, 2026-07-19: GC coverage is implemented through crate tests,
`eng/tests.toml`, `cmd/vo-dev test`, and `./d.py test`.

Implementation notes:

- `VO_GC_STRESS=1` enables `Vm::set_gc_stress_every_step(true)`.
- `VO_GC_VERIFY=1` enables precise VM GC verification after VM-driven GC steps.
- `VO_GC_DEBUG=1` remains as a compatibility alias for stress plus verify.
- `eng/tests.toml` uses the explicit `VO_GC_STRESS` and `VO_GC_VERIFY` flags
  for `gc-vm` and `gc-jit`.
- The VM verifier is `SlotType`-aware and phase-aware: mark phases reject
  black-to-white edges, while sweep rejects only black edges to dead-white
  objects because current-white objects may already be confirmed live.
- Runtime model tests are implemented directly in
  `lang/crates/vo-runtime/tests/gc_model.rs`; a separate support module was not
  needed for the initial focused model.
- Crate-level GC checks use `cargo test -p vo-runtime gc`,
  `cargo test -p vo-vm gc`, `cargo test -p vo-vm --features jit gc`, and
  `cargo test -p vo-jit gc` directly.
- `./d.py test gc` remains the language-level stress plus verify regression
  entry point for `gc-vm` and `gc-jit`.

This document tracks the current GC audit findings as an implementation plan for
the GC correctness contract layer. The goal is not to replace `cargo test`,
`./d.py test`, or the language-test manifest. The goal is to keep adding
GC-specific tests and validation inside the existing Volang workflows so root
scanning, slot metadata, write barriers, incremental collection, and JIT
materialization stay testable as explicit contracts.

Related context:

- [`lang/docs/dev-notes/gc-system-design.md`](../dev-notes/gc-system-design.md)
- [`lang/docs/dev-notes/2026-05-09-gc-v1-plan.md`](../dev-notes/2026-05-09-gc-v1-plan.md)
- [`lang/docs/dev-notes/2026-02-27-slot-types-gc-bug.md`](../dev-notes/2026-02-27-slot-types-gc-bug.md)
- [`jit-fact-source.md`](jit-fact-source.md)

## Goals

- Catch GC correctness bugs before they become language-level flakes.
- Make each GC-sensitive boundary independently testable:
  - runtime object graph marking and sweeping
  - `SlotType` and `ValueMeta` layout validation
  - VM root enumeration and dirty-root state
  - write barriers across VM, runtime, FFI, and JIT helpers
  - JIT spill, side-exit, and materialized-frame roots
- Keep the new framework inside existing repo workflow:
  - crate tests for fast contract checks
  - `eng/tests.toml` and `vo-dev` for language regressions
- no new ad hoc test runner unless measurement later proves one is needed
- Keep stress and verify behavior wired to explicit environment variables, with
  `VO_GC_DEBUG=1` only as a compatibility alias.

## Current Coverage

Completed in the current integration layer:

- `VO_GC_STRESS=1` and `VO_GC_VERIFY=1` drive VM stress and precise
  post-step verification; `VO_GC_DEBUG=1` remains a compatibility alias.
- Runtime model tests cover basic reachability, root removal, mixed slot
  layouts, interface-pair scanning, array reference elements, and a late
  write-barrier rescue case.
- Central VM GC layout validation rejects malformed globals, struct fields,
  interface pairs, underscanned function metadata, capture drift, and return
  layout width drift during module load.
- VM root matrix tests cover globals, live fiber stacks, call-frame scan slots,
  return values, defers, panic payloads, closure replay results, select roots,
  sentinel errors, endpoint registry live handles, dirty-fiber sweep rescue,
  bounded root snapshots, root-scan restart, stable sweep skips, load reset, and
  the JIT pending-transition GC-entry contract.
- JIT materialization tests cover nested shadow-frame materialization, empty
  shadow-frame restoration, borrowed parent frames, and nested
  extern-yield/block special-call materialization before scheduler return.
- Direct crate tests cover the GC contracts, while `./d.py test gc` keeps the
  language-level stress/verify regression path.

Still incomplete or intentionally narrow:

- JIT write-barrier coverage is not yet exhaustive for every lowering path:
  array, slice, map, queue/select, FFI, and callback writes need broader
  contract tests.
- Extern/FFI boundaries cover call replay, WaitIo, yield/block materialization,
  return validation, and sentinel roots, but not every host extension shape.
- Host-event and browser/WASM suspension paths still need root survival tests
  equivalent to the native WaitIo and closure replay cases.
- VM root matrix coverage is focused on positive survival and dirty-state
  contracts; stale/negative cases for every root source remain future work.
- Runtime model tests are intentionally small and do not yet model every
  container kind or every incremental phase transition.

## Non-Goals

- Do not redesign the collector algorithm.
- Do not introduce a moving, generational, or conservative collector.
- Do not make GC tests depend on wall-clock timing.
- Do not duplicate test policy outside `eng/*.toml` and `cmd/vo-dev`.
- Do not rely only on end-to-end `.vo` programs for GC correctness. Language
  regressions are valuable, but they are too coarse to prove object graph,
  root, and barrier invariants.

## Bug Classes To Prevent

The framework should make these bug classes cheap to reproduce:

1. Missing roots: globals, fiber stacks, call frames, returns, defers, panic
   state, endpoint registry entries, sentinel errors, scheduler queues, and JIT
   materialized frames.
2. Malformed metadata: wrong `SlotType` width, empty GC layouts where a precise
   layout is required, broken `Interface0`/`Interface1` pairing, stale
   `gc_scan_slots`, and mismatch between VM loader and JIT verifier rules.
3. Missing or weak write barriers: black-to-white edges, typed barrier fallback
   to raw barrier, array and interface value writes, FFI container writes, and
   JIT collection writes.
4. Incremental state bugs: pending start-cycle root scans, pending atomic root
   scans, sweep rescue, stable-root skips, dirty-root epochs, and root mutation
   during bounded scans.
5. JIT boundary bugs: live refs in registers or shadow frames not materialized
   before GC, panic/defer paths losing refs, side exits with stale frame
   metadata, pending runtime payloads reaching GC without attachment or typed
   roots, and verifier/runtime contract drift.

## Architecture

The testing framework has four layers. Each layer owns a different correctness
boundary and should be runnable independently.

| Layer | Location | Purpose |
| --- | --- | --- |
| Runtime model tests | `lang/crates/vo-runtime/tests/gc_model.rs` | Compare real GC survivors with a small independent object-graph model. |
| Layout validator tests | `lang/crates/vo-vm` and shared validation module | Reject malformed GC metadata at module load time. |
| VM root matrix tests | `lang/crates/vo-vm/src/vm/mod.rs` focused unit tests | Prove every VM root source is scanned and dirty-state aware. |
| JIT GC contract tests | `lang/crates/vo-jit` and `vo-vm --features jit` | Prove JIT lowering, verifier, materialization, and runtime helpers agree on GC contracts. |

The layers should then be exposed through the existing commands:

```sh
./d.py test gc
cargo test -p vo-runtime gc
cargo test -p vo-vm gc
cargo test -p vo-vm --features jit gc
cargo test -p vo-jit gc
```

## Layer 1: Runtime Object-Graph Model

The runtime-level model tests construct small object graphs without a VM. The
model keeps an independent reachability graph and compares it with the real
collector after full and incremental collection.

Current file:

- `lang/crates/vo-runtime/tests/gc_model.rs`

Core operations:

```rust
let mut m = GcModel::new();
let parent = m.alloc_struct(&[SlotType::GcRef]);
let child = m.alloc_struct(&[]);
m.root(parent);
m.write(parent, 0, child);
m.full_gc();
m.assert_survivors_match_model();
```

Current capabilities:

- Allocate test objects with explicit slot layouts.
- Track roots separately from heap edges.
- Write slots through the real typed barrier path.
- Run one GC step, run until pause, and run a complete collection.
- Assert exact survivor set.
- Assert freed objects are absent from model roots and edges.

Current scenarios:

- simple chain survives from one root
- disconnected graph is swept
- root removal makes graph collectible
- black parent writing white child requires barrier
- struct with mixed non-ref and ref slots scans only refs
- interface pair scans slot 1 only when slot 0 says the data is a GC ref
- array reference elements survive through the typed barrier path

Next useful extensions:

- negative raw-barrier tests that prove verification catches missing barriers
- direct incremental-phase assertions for black-to-white edge violations

## Layer 2: Central GC Layout Validation

The VM uses one central validation module from module load and focused unit
tests. It validates globals, structs, functions, captures, returns, and
interface pairs before execution.

Current file:

- `lang/crates/vo-common-core/src/verifier.rs`

Current API shape:

```rust
pub fn verify_module(module: &Module) -> Result<VerifiedModule<'_>, ModuleVerificationError>;
pub fn validate_module_gc_layout(module: &Module) -> Result<(), ModuleVerificationError>;
pub fn validate_slot_layout(label: &str, slots: usize, slot_types: &[SlotType]) -> Result<(), ModuleVerificationError>;
pub fn validate_interface_pairs(label: &str, slot_types: &[SlotType]) -> Result<(), ModuleVerificationError>;
```

Rules:

- Any layout that will be scanned must have exactly one `SlotType` per slot.
- `Interface0` must be followed by `Interface1`.
- `Interface1` must not appear without a preceding `Interface0`.
- `gc_scan_slots` must not exceed local slots.
- `gc_scan_slots` must cover every slot that may contain a GC ref.
- Global layouts must not silently accept empty `slot_types` when the global is
  scannable.
- Struct layouts must be validated centrally, not only at scan time.
- Closure capture and return-slot layouts must be validated before execution.
- VM loader and JIT verifier should derive equivalent rules. Prefer sharing
  helper logic or adding contract tests that prevent drift.

Acceptance tests:

- loader rejects global layout width mismatch
- loader rejects struct layout width mismatch
- loader rejects orphan `Interface1`
- loader rejects `Interface0` not followed by `Interface1`
- loader rejects `gc_scan_slots` that under-scans a GC slot
- JIT verifier and VM loader agree on array/interface layout constraints

## Layer 3: VM Root Matrix

The current VM root matrix lives in focused `#[cfg(test)]` unit tests under
`lang/crates/vo-vm/src/vm/mod.rs`. These tests build VM state, place GC objects
in root sources, force collection at specific phases, and assert survival.

Future larger matrices can move to an integration-test helper when the private
VM internals have a clean public test surface.

Core shape:

```rust
GcRootScenario::new()
    .with_root(RootKind::Global)
    .force_gc_at(GcPoint::Sweep)
    .expect_survives();
```

Root kinds to cover:

- globals
- current fiber stack
- non-current live fiber stack
- call frame locals up to `gc_scan_slots`
- return slots
- closure captures
- closure replay results with slot metadata
- defer arguments
- panic payload
- JIT panic message
- sentinel errors
- endpoint registry live handles
- select and queue wait state
- island or host callback roots that mark all roots dirty
- materialized JIT frames

GC points to cover:

- before cycle start
- during bounded start-cycle root scan
- during propagate
- during pending atomic root scan
- during sweep before the object is swept
- during sweep after stable-root skip

Dirty-root expectations:

- Host, I/O, island, endpoint, and sentinel mutations mark all roots dirty.
- Fiber-local mutations mark that fiber dirty.
- Root mutation while a bounded root scan is pending restarts from a fresh
  snapshot.
- Stable sweep skip is allowed only when the root snapshot is still fresh.

Important cleanup:

- Remove or privatize stale root-scanning entry points that do not use the same
  collection logic as the snapshot path.
- If an old entry point must remain, make it delegate to the same root collector
  as the snapshot path.

## Layer 4: JIT GC Contracts

The JIT must prove that all GC-sensitive lowering paths either preserve precise
roots or materialize enough VM state before any GC-visible transition.

Required test areas:

- `ArraySet`, `SliceSet`, map writes, queue/select writes, and FFI writes use
  typed barriers when metadata is available.
- Single-slot array barrier logic cannot accidentally treat interface values as
  one-slot references.
- Any instruction that can allocate, call a runtime helper, block, wait, panic,
  or side-exit must have a clear root materialization policy.
- Materialized frames must have valid `func_id`, `bp`, `scan_slots`, and stack
  extent.
- JIT panic/defer paths must materialize roots before control returns to VM
  panic handling.
- `vo_gc_typed_write_barrier_by_meta` remains the preferred helper when
  metadata is available.

Suggested tests:

- `cargo test -p vo-jit gc`
- `cargo test -p vo-vm --features jit gc_materialize`
- one or more language regressions in `tests/lang/cases/gc_*` that run under
  both `gc-vm` and `gc-jit`

## Stress And Verify Modes

`eng/tests.toml` uses explicit `VO_GC_STRESS=1` and `VO_GC_VERIFY=1` flags for
GC regression targets. `VO_GC_DEBUG=1` remains a compatibility alias that
enables both behaviors during VM construction.

Recommended variables:

| Variable | Behavior |
| --- | --- |
| `VO_GC_STRESS=1` | Force GC work at every safe boundary by calling `Vm::set_gc_stress_every_step(true)`. |
| `VO_GC_VERIFY=1` | Run precise invariant checks after important GC transitions. |
| `VO_GC_TRACE=1` | Emit diagnostic trace output for local debugging only. |

Compatibility rule:

- `VO_GC_DEBUG=1` maps to `VO_GC_STRESS=1` plus `VO_GC_VERIFY=1`.
- New workflow configuration should prefer the explicit variables.

Suggested target split:

- `gc-vm`: regular VM GC regressions
- `gc-jit`: regular JIT GC regressions
- `gc-stress-vm`: VM regressions with stress and verify enabled
- `gc-stress-jit`: JIT regressions with stress and verify enabled

The default `./d.py test gc` should stay fast. Longer stress suites can be
separate targets or CI-only tasks if needed.

## Precise GC Verification

The VM verifier is `SlotType`-aware and uses the same object-child tracing rules
as the real collector. The debug hooks remain lightweight allocation, barrier,
and free tracking; they do not conservatively scan every object word.

Current direction:

- Keep lightweight allocation, barrier, and free tracing optional.
- Run a `SlotType`-aware verification pass after VM-driven GC steps when
  `VO_GC_VERIFY=1` is enabled.
- Only scan object payloads through the same metadata rules used by the real
  collector.
- Treat invalid typed-barrier children as test failures in verify mode.

Verifier invariants:

- no black object points to a white object
- every object in the gray queue is valid
- every root snapshot entry is valid or zero
- every scanned object has metadata with matching slot width
- every interface slot pair is well-formed
- no freed object appears in roots, edges, gray queue, or pending snapshots
- sweep rescue preserves any old-white object that becomes reachable from a
  fresh root scan

## Test Naming Policy

GC-sensitive tests must be discoverable by the standard filters. Use names that
contain `gc`.

Preferred prefixes:

- `gc_model_*`
- `gc_layout_*`
- `gc_root_*`
- `gc_barrier_*`
- `gc_materialize_*`
- `gc_stress_*`

This avoids the current problem where important tests named `root_scan` or
`materialize` are missed by `cargo test -p vo-vm gc`.

## Implementation Phases

### Phase 1: Make Existing GC Test Flags Real

- Wire `VO_GC_STRESS` and `VO_GC_VERIFY` into engine or VM setup.
- Keep `VO_GC_DEBUG` as a compatibility alias for stress plus verify.
- Update `eng/tests.toml` to use real variables.
- Add a small regression proving the environment variable enables stress mode.

Validation:

```sh
./d.py test gc
cargo test -p vo-vm gc
```

### Phase 2: Add Layout Validator

- Add central GC layout validation module.
- Call it from module load.
- Move or wrap existing function metadata checks so validation rules have one
  owner.
- Add malformed metadata tests for globals, structs, functions, captures,
  returns, and interface pairs.

Validation:

```sh
cargo test -p vo-vm gc_layout
cargo test -p vo-jit gc
```

### Phase 3: Add Runtime Model Tests

- Add object-graph model helper.
- Cover roots, graph mutation, typed barriers, interface pairs, mixed slots,
  arrays, and incremental state.
- Add verifier assertions where available.

Validation:

```sh
cargo test -p vo-runtime gc_model
cargo test -p vo-runtime gc
```

### Phase 4: Add VM Root Matrix

- Add root matrix helper.
- Cover all root kinds listed in this document.
- Unify or remove stale root scan entry points.
- Add dirty-root restart and stable-skip assertions.

Validation:

```sh
cargo test -p vo-vm gc_root
cargo test -p vo-vm gc
```

### Phase 5: Add JIT GC Contract Coverage

- Add or rename JIT tests so GC-sensitive contract tests include `gc` in their
  names.
- Add materialization tests under `vo-vm --features jit`.
- Add language-level stress regressions for VM and JIT.

Validation:

```sh
cargo test -p vo-jit gc
cargo test -p vo-vm --features jit gc
./d.py test gc
```

## Final Acceptance Criteria

- `VO_GC_DEBUG`, `VO_GC_STRESS`, and `VO_GC_VERIFY` behavior is explicit and
  documented in code or test config.
- `./d.py test gc` runs meaningful VM and JIT GC regressions.
- `cargo test -p vo-runtime gc` covers runtime model and existing GC tests.
- `cargo test -p vo-vm gc` covers layout validation and root matrix tests.
- `cargo test -p vo-vm --features jit gc` covers materialized-frame root tests.
- `cargo test -p vo-jit gc` covers JIT GC contract tests.
- Malformed GC metadata fails during load or verification, not during an
  arbitrary GC scan.
- Every root source listed in this document has at least one positive survival
  test and, where practical, one stale/dirty-state test.
- Runtime model tests can catch a deliberately disabled write barrier.
- Verifier checks are precise and `SlotType`-aware.

## Implementation Goal Prompt

Use this prompt when starting a dedicated implementation goal:

```text
/goal Implement the GC correctness testing framework described in
lang/docs/dev/gc-correctness-testing-framework.md.

Use the volang-dev skill first, then inspect the current source before editing.
Keep the work inside existing Volang workflows: crate tests, eng/tests.toml,
cmd/vo-dev, and ./d.py. Do not create a separate ad hoc test runner.

Implementation order:
1. Wire real GC stress/verify environment behavior. VO_GC_STRESS=1 should enable
   Vm::set_gc_stress_every_step(true), and VO_GC_VERIFY=1 should enable precise
   VM post-step verification. Keep VO_GC_DEBUG as a compatibility alias while
   using explicit variables in eng/tests.toml.
2. Add central GC layout validation for module/global/struct/function/capture/
   return/interface slot metadata, and call it during module load.
3. Add runtime GC object-graph model tests under vo-runtime that compare real
   collector survivors against an independent reachability model.
4. Add VM GC root matrix tests that cover globals, fibers, stacks, call frames,
   returns, defers, panic state, sentinel errors, endpoint/live handles,
   queue/select roots, dirty-root restarts, stable-root skips, and JIT
   materialized frames where feature-gated.
5. Add or rename JIT GC contract tests so cargo test -p vo-jit gc and
   cargo test -p vo-vm --features jit gc cover write barriers, layout contracts,
   side exits, panic/defer materialization, and materialized-frame roots.
6. Rework gc_debug/gc verification only as far as needed for precise,
   SlotType-aware invariant checks. Avoid conservative word scanning.
7. Update eng/tests.toml and any test naming so GC-sensitive tests are reached
   by ./d.py test gc and cargo test filters containing gc.

Preserve existing behavior unless the document explicitly calls for tightening a
GC correctness contract. Prefer small, source-backed changes and add focused
tests with each phase. Never revert unrelated user changes.

Required validation before completing the goal:
- ./d.py test gc
- cargo test -p vo-runtime gc
- cargo test -p vo-vm gc
- cargo test -p vo-vm --features jit gc
- cargo test -p vo-jit gc

If a validation command cannot run, report the exact blocker and the narrower
commands that did run. Update the document's status or implementation notes if
the final implementation intentionally differs from the plan.
```
