# JIT Opcode Fact Source

This note documents the current maintainer contract for `vo-jit`. It is meant
to be checked before adding or changing bytecode lowering, metadata, verifier,
or call behavior.

Related context:

- [`vm-production-readiness.md`](vm-production-readiness.md)

## Single Source Of Truth

`lang/crates/vo-jit/src/semantics/` owns the opcode fact table:

- `OPCODE_SEMANTICS` declares one named-field `semantic_row!` per valid opcode.
- `INVALID_SEMANTICS` declares the strict invalid-opcode sentinel.
- Accessors such as `opcode_capability`, `opcode_effect_contract`,
  `opcode_register_effects`, runtime dependencies, verifier requirements,
  verifier domain, helper return policy, frame policy, trap policy, and
  fail-fast conditions all derive from that row.
- `metadata_contract.rs` delegates opcode metadata requirements back to the
  semantics row. Do not add a second opcode-to-metadata match there.

Each row should declare these facts once with named fields:

- packed operand contract
- VM semantic source and lowering owner
- metadata requirement, verifier requirements, and optional verifier domain
  override
- register effects and dynamic register exceptions
- runtime dependencies and helper return policy
- frame, trap, and fail-fast policy
- capability and runtime path policy
- effect contract

If a new opcode needs behavior that cannot be expressed in the row spec, add a
small explicit spec enum to the `semantics/` module, then derive from it in the
consumer. Do not duplicate opcode-family matches in each consumer.

## Effects

`effects.rs` interprets `OpcodeRegisterEffects` from the semantics row for
ordinary register reads, register writes, memory sync, and may-call facts.

Only true dynamic cases should remain outside the row interpreter:

- metadata-dependent indexed, slice, map, and iterator layouts
- module signature dependent static calls
- extern return slot counts
- multi-result shapes whose slots are not statically present in the opcode
- runtime-context values that are not encoded in bytecode or JIT metadata

Missing metadata or module context is an error, not an implicit register or
layout fallback.

## Verifier Modules

`vo-common-core/src/verifier.rs` is the VM-shared bytecode/module verifier. It
owns layout/range checks, metadata shape checks, branch targets, call/extern
shape checks, transfer metadata, GC layout validation, and write-barrier
contracts before a module can execute.

`vo-jit/src/verifier.rs` runs only after the shared verifier. It owns strict JIT
metadata policy and loop metadata consistency; lowering capability, helper ABI,
frame materialization, side-exit, OSR, and direct-call contracts are enforced by
the semantic row, contract graph, helper manifests, and lowering tests.

`VerifierRequirement` remains a semantic-row fact for fail-fast policy and
contract graph coverage. VM-shared enforcement lives in `ModuleVerifier`; JIT
uses the same semantic rows for lowering capability, runtime dependency, and
ABI coverage instead of maintaining a second slot/layout verifier tree.

Family verifiers should keep concrete layout shape, interface-pair,
write-barrier, call-shape, and opcode-specific contracts. If a helper is shared
by multiple verifier domains, keep only the shared helper in the root and call
it from submodules.

## Call Lowering Modules

`call_helpers.rs` is the shared call ABI and frame-guard surface. Concrete call
lowering lives in focused modules:

- `dynamic.rs`: `CallClosure`, `CallIface`, monomorphic inline caches, and
  `DynamicCallLowering`
- `prepared.rs`: dispatch after `prepare_*_call` returns a `PreparedCall`
- `externs.rs`: `CallExtern` and intrinsic fast paths
- `vm_materialization.rs`: ordinary static call VM materialization and direct
  JIT-to-JIT call routing
- `result_flow.rs`: `JitResult` OK/non-OK handling
- `callback_abi.rs`: checked/raw JitContext callback ABI wrappers
- `plan.rs`: static and dynamic call route planning

Keep `DynamicCallLowering`; it is the useful aggregation boundary for dynamic
call state. Avoid splitting it into tiny helpers unless a real repeated contract
emerges.

## Runtime Path Naming

JIT lowering contracts must not use "fallback" to mean legal VM materialization.
The capability API uses `RuntimePathPolicy`:

- `None`
- `RuntimePanic`
- `RuntimeHelper`
- `VmSideExit`
- `VmCallMaterialization`
- `InvalidOpcode`

`PreparedCall::vm_materialization` means the prepared callee has no direct JIT
entry and must return through the VM call trampoline. It is an explicit runtime
path, not an implicit interpreter path.

The VM exposes execution statistics as `JitSideExitReason`; those statistics
count semantic side exits such as cold/not-hot execution, WaitIo, Replay, queue
blocking, and regular/prepared VM call materialization. Historical
language-test manifest side-exit observation keys are parsed only at the test
boundary and must map into side-exit fields before they reach runtime APIs.

## Fail-Fast Rules

Strict JIT compile and verification must fail fast for:

- invalid opcodes
- unsupported lowering owners
- missing JIT metadata required by the semantics row
- metadata kind/layout drift
- helper or callback ABI contract drift
- local/global slot range or layout mismatches
- invalid function, extern, constant, or branch references
- call signature and return shape mismatches
- runtime helper contract omissions

Do not silently re-route these cases to the interpreter. Legal VM
materialization is only for explicit semantic scheduling/frame boundaries such
as ordinary calls that need VM-owned frames, stack capacity growth, prepared
dynamic calls without a JIT entry, WaitIo/WaitQueue/Yield/Replay, and normal OSR
exit.

## Maintenance Checklist

When adding or changing an opcode:

1. Add or update exactly one semantics row.
2. Add row spec types if the fact cannot already be represented.
3. Keep capability, effect contract, verifier requirements/domain, metadata,
   runtime dependency, and fail-fast policy in that row.
4. Ensure `effects.rs` derives ordinary reads/writes/memory sync/may-call from
   the row.
5. Add verifier helper code in the appropriate submodule.
6. Keep metadata requirements delegated through `metadata_contract.rs`.
7. Add focused tests for any behavior change; for refactors, add source-boundary
   or semantic consistency tests.
8. Run `cargo test -p vo-jit` and the relevant VM/codegen/language tests.
9. Re-scan for duplicated opcode fact matches and misleading fallback names.
