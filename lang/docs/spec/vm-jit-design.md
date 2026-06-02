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
consume, which layouts are required before lowering, and which serialized legacy
kinds are reader compatibility only.

Typed decoding is centralized in `vo-jit/src/metadata.rs`. Verifier, effects,
lowering, semantics, and contract graph code must use these typed accessors
instead of introducing local `JitInstructionMetadata` decode tables.

`vo-common-core` owns bytecode serialization. Current-version bytecode that has
a `jit_metadata` table must keep `jit_metadata.len() == code.len()`. Legacy map
metadata can be read for compatibility, but strict JIT rejects it before
lowering.

## Lowering Responsibilities

`vo-codegen` owns metadata production and typed instruction construction. It
must emit precise layout metadata for dynamic element, map, pointer, slot, call,
extern call, queue, iterator, and interface assertion operations. It also owns
typed return buffers and uses `FuncBuilder` helpers for shared shapes such as
static calls, call buffers, fallthrough returns, and zero-slot initialization.
Loop hint patching and method-value wrappers must go through typed
`FuncBuilder` APIs so bytecode and `jit_metadata` cannot drift by pc-indexed
manual edits.

`vo-jit` owns validation and lowering:

- `verifier/` checks function invariants, per-PC metadata, slot layouts, call
  shapes, return flags, transfer metadata, branch targets, and legacy rejection.
- `semantics.rs`, `capability.rs`, and `contract_graph.rs` describe opcode
  effects, fail-fast policy, runtime dependencies, and capability coverage.
- `call_helpers/plan.rs` owns static/dynamic call route selection.
- `call_helpers/callback_abi.rs` owns JitContext callback ABI callsites.
- `call_helpers/result_flow.rs` owns checked helper result routing and non-OK
  JIT call materialization flow.
- Dynamic closure/interface call lowering shares the IC scratch, IC entry,
  hit/miss branch, prepare-callback argument, and return-copy skeleton while
  keeping nil checks, receiver/key construction, slot0/capture handling, and
  method-index rules explicit.
- `helpers.rs` declares runtime helper imports from one helper table plus the
  runtime ABI manifest; helper names, `FuncId` fields, and per-function refs are
  no longer maintained as separate lists.
- `compile_common.rs` owns common full-function/OSR compile facts such as
  variable declaration, float-slot lookup, branch target validation, and flow
  fact application. OSR range-exit behavior remains in `loop_compiler.rs`.

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
blocking or replaying. Legacy-named fallback counters record only semantic
runtime side exits, not compile or metadata failures.

## OSR

OSR compiles loop ranges with the same slot layout as the interpreter. A normal
loop exit writes `ctx.loop_exit_pc` and returns `JitResult::Ok`; side exits use
the same materialization and fail-fast contracts as full-function JIT. Loop-end
metadata for large hints is required and verifier-checked.

## Testing

JIT changes should cover the layer they touch:

- bytecode/serializer schema: `cargo test -p vo-common-core`
- verifier, semantics, effects, lowering contracts: `cargo test -p vo-jit`
- metadata production and typed builders: `cargo test -p vo-codegen`
- VM bridge, callback ABI, frame materialization: `cargo test -p vo-vm --features jit`
- engine strict mode: `cargo test -p vo-engine --features jit`
- language parity and OSR: repo `./d.py test` suites, especially `jit` and `osr`
