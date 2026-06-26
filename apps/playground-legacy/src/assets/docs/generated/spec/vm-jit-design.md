<!--
Generated from lang/docs/spec/vm-jit-design.md
Generator: node scripts/ci/docs_sync.mjs
Source-Digest: sha256:7aba9c71d862a3efb599e62988e31adc68163a619abdd8a3b16965bb9266e4a2
Generated-At: 2026-06-08T12:42:04+08:00
-->
# Vo VM JIT Design

This document describes the implemented JIT boundary. Historical design notes
live in `docs/dev-notes`; this file is the current runtime contract.

## Architecture

The JIT is a synchronous Cranelift backend for Vo bytecode. JIT functions run on
the VM thread and return a `JitResult` to the VM scheduler:

```rust
extern "C" fn(ctx: *mut JitContext, args: *mut u64, ret: *mut u64) -> JitResult
```

`args` points at the active `Fiber` stack frame. Primitive slots may be promoted
to Cranelift SSA values, but GC-visible slots remain represented in the VM stack
at safepoints. If generated code calls a helper that may allocate, block,
materialize frames, or panic, the helper result is checked before local JIT
execution continues.

## Strict And Best-Effort Modes

Strict JIT mode is fail-fast. `vo_jit::verify_module` is the public verifier
entry, and strict VM load/init paths run it before dispatch tables are usable.
It returns a `VerifiedModule` digest token; `JitCompiler` caches that token and
re-verifies only when the serialized module fingerprint changes. Compile
failure, invalid metadata, missing helper/callback ABI, bad call shape, bad
return shape, and internal JIT ABI errors surface as `JitError` or
`VmError::Jit`; they are not semantic side exits.

Best-effort JIT mode exists only through explicitly named VM APIs. It is for
embedding compatibility and does not change strict behavior.

Legal runtime side exits are semantic scheduling boundaries:

- cold or not-hot code remains interpreted
- regular/prepared VM call materialization
- `WaitIo`, `WaitQueue`, `Yield`, and `Replay`
- OSR normal exit
- stack-capacity trampoline into VM frame setup
- explicitly requested best-effort embedding APIs

## Metadata Contract

`vo-jit/src/metadata_contract.rs` is the single source of truth for opcode JIT
metadata requirements. It defines which current metadata kind an opcode may
consume and which layouts are required before lowering.

Typed decoding used by JIT effects and loop analysis is centralized in
`vo-jit/src/metadata.rs`. New JIT consumers should use those facts instead of
introducing local `JitInstructionMetadata` decode tables.

`vo-common-core` owns bytecode serialization. Current-version bytecode that has
a `jit_metadata` table must keep `jit_metadata.len() == code.len()`. Older
bytecode versions and removed metadata tags are not accepted input; they are
rejected before a module can execute or enter strict JIT.

## Opcode Contract

`vo-jit/src/semantics/` is the opcode contract table for JIT semantics.
Each row records packed operands, VM semantic source, lowering owner, verifier
requirements, register effect shape, memory sync shape, runtime dependencies,
helper return policy, frame policy, trap policy, fail-fast conditions,
capability, and the opcode effect contract. `capability.rs` and `contract.rs`
keep the public API and data types, but their per-opcode answers delegate to the
semantic row instead of maintaining separate matches.

Metadata remains a specialized fact source in `metadata_contract.rs`.
The semantics table imports the opcode metadata requirement from that module, and the
tests reject any second metadata requirement table in the semantic row.
Concrete read/write slot lists still come from `effects.rs` because they depend
on instruction operands, module signatures, extern signatures, and typed
metadata payloads. The register-constant analysis uses those effect results for
kill sets and only keeps dedicated logic for constant folding.

## Lowering Responsibilities

`vo-codegen` owns metadata production and typed instruction construction. It
must emit precise layout metadata for dynamic element, map, pointer, slot, call,
extern call, queue, iterator, and interface assertion operations. It also owns
typed return buffers and uses `FuncBuilder` helpers for shared shapes such as
static calls, call buffers, fallthrough returns, and zero-slot initialization.
Loop hint patching and method-value wrappers must go through typed
`FuncBuilder` APIs so bytecode and `jit_metadata` cannot drift by pc-indexed
manual edits.

`vo-common-core` owns VM-shared bytecode/module validation through
`vo-common-core/src/verifier.rs`. It checks function invariants, per-PC metadata
shape, slot layouts, call shapes, return flags, transfer metadata, branch
targets, GC layouts, write barriers, and index validity before any VM or JIT
execution path accepts a module.

`vo-jit` owns strict JIT validation and lowering:

- `verifier/` checks strict JIT metadata kind policy and loop metadata
  consistency after the shared `ModuleVerifier` has accepted the module.
- JIT capability, helper dependencies, ABI contracts, frame materialization,
  side exits, OSR, and direct-call contracts are described by the semantic row,
  contract graph, helper manifests, and lowering tests.
- `vo-jit/src/semantics/` describes opcode effects, fail-fast policy, runtime
  dependencies, verifier requirements, and capability coverage. The contract
  graph consumes those rows rather than re-declaring opcode policy.
- `call_helpers/plan.rs` owns static/dynamic call route selection.
- `call_helpers/callback_abi.rs` owns JitContext callback ABI callsites.
- `call_helpers/result_flow.rs` owns checked helper result routing and non-OK
  JIT call materialization flow.
- `DynamicCallLowering` in `call_helpers.rs` owns the shared
  closure/interface dynamic-call skeleton: inline-cache lookup, hit/miss branch,
  prepare-callback workspace, IC update, JIT/VM call dispatch, and return copy.
  Closure nil checks, closure func-id keys, slot0/capture handling, interface
  receiver pairs, method keys, and method-index rules stay explicit at the
  callsite.
- `helpers.rs` declares runtime helper imports from one helper table plus the
  runtime ABI manifest; helper names, `FuncId` fields, and per-function refs are
  no longer maintained as separate lists.
- `compile_common.rs` owns common full-function/OSR compile facts and driver
  mechanics: `ControlPolicy`, jump-target discovery, basic-block transition,
  per-PC flow fact application, and the `CompileDriver` loop. Full-function and
  OSR compilers still own their prologues, return/call lowering, and OSR
  range-exit materialization.

## VM Boundary

`vo-vm` owns `JitResult` scheduling. `JitResult::Ok` is adapted separately for
full JIT and OSR because full JIT copies return slots while OSR publishes
`loop_exit_pc`. Every non-OK result goes through the shared JIT bridge
transition layer before being adapted to `ExecResult` or `OsrResult`.
`JitResult::Panic` requires either typed runtime trap payload or explicit user
panic payload. Missing payload/location is a JIT error.

`JitResult::Call` materializes prepared or regular VM frames and returns
`FrameChanged` to the VM scheduler. Strict mode may resolve/compile the callee at
this boundary to keep metadata errors fail-fast, but execution does not
recursively re-enter a newly materialized frame on the host stack.

`WaitIo`, `WaitQueue`, and `Replay` materialize pending JIT frames before
blocking or replaying. Runtime side-exit counters record only semantic runtime
side exits, not compile or metadata failures.

## OSR

OSR compiles loop ranges with the same slot layout as the interpreter. A normal
loop exit writes `ctx.loop_exit_pc` and returns `JitResult::Ok`; side exits use
the same materialization and fail-fast contracts as full-function JIT. Loop-end
metadata for large hints is required and verifier-checked.

`ControlPolicy::LoopOsr` constrains block creation to targets inside the loop
range and leaves out-of-range jump, return, call, wait, and replay behavior to
`loop_compiler.rs`. Fallthrough exits route through the OSR exit block, spill
live locals, publish `loop_exit_pc`, and return `JitResult::Ok`.

## Adding Or Changing An Opcode

Opcode maintenance is intentionally row-driven:

- Update the opcode definition and typed instruction accessors in
  `vo-common-core`.
- Add or update codegen metadata emission and typed builders when the opcode
  needs JIT metadata.
- Update `metadata_contract.rs` only if the opcode consumes per-PC JIT
  metadata.
- Update the semantic row in `vo-jit/src/semantics/`: packed operands, lowering
  owner, verifier requirements, register effect shape, runtime dependencies,
  helper/trap/fail-fast/frame policy, capability, and effect contract.
- Add VM-shared slot/layout validation in `vo-common-core/src/verifier.rs`.
- Add concrete read/write effect handling in `effects.rs` only when the opcode
  has operand- or metadata-dependent slot lists.
- Add translate lowering explicitly in the relevant `translate/` module or
  compiler/call-helper owner; do not macro-generate `translate_inst`.
- Extend focused tests first for behavior changes, then run the JIT and language
  parity suites listed below.

## Testing

JIT changes should cover the layer they touch:

- bytecode/serializer schema: `cargo test -p vo-common-core`
- verifier, semantics, effects, lowering contracts: `cargo test -p vo-jit`
- metadata production and typed builders: `cargo test -p vo-codegen`
- VM bridge, callback ABI, frame materialization: `cargo test -p vo-vm --features jit`
- engine strict mode: `cargo test -p vo-engine --features jit`
- language parity and OSR: repo `./d.py test` suites, especially `jit` and `osr`
