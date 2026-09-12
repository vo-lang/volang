# Vo VM JIT Design

This document describes the implemented JIT boundary. Historical design notes
live in `docs/dev-notes`; this file is the current runtime contract.

## Architecture

The JIT is a synchronous Cranelift backend for Vo bytecode. JIT functions run on
the VM thread and return a `JitResult` to the VM scheduler:

```rust
extern "C" fn(
    ctx: *mut JitContext, frame_bp: u64, ret: *mut u64,
    arg0: u64, arg1: u64, arg2: u64, arg3: u64, arg4: u64,
) -> JitResult
```

`frame_bp` is a stable index into the active Fiber stack. The first five
argument words are authoritative in raw native register lanes; their shadow
slots need not be initialized by a native caller. Wide argument tails remain
in the capacity-checked Fiber window. Ordinary callees initialize alias-backed
leading slots before any guest safepoint and publish leading arguments on the
cold tier-up path before a possible entry rejection. VM/prepared-call entries
load lanes from their initialized frames. Continuations use their separate
canonical-frame import contract and never treat the resume PC as an argument.
Verified slots may remain in Cranelift SSA; allocation polls spill live direct
references and complete tagged interface pairs into precise native shadow-root maps. The
collector interprets each interface payload using its adjacent runtime tag.
Exhausting the bounded native/root scan requires typed VM-frame materialization
before returning to the scheduler, so native stack addresses never escape that
execution. Stack reallocation rebuilds addresses from the stable index. Every
non-OK helper result exits before further guest effects.

## Strict And Best-Effort Modes

Strict JIT mode is fail-fast. `vo-common-core::verify_loaded_module` validates
the owned module once and returns an immutable `LoadedModule`. That image owns
the module, verified runtime type facts, and the dense dynamic-callsite map.
The VM retains the exact image for the JIT lifetime; `JitCompiler` binds by
pointer identity and reuses its derived facts without serializing, hashing, or
rescanning the module. Compile failure, invalid metadata, missing
helper/callback ABI, bad call/return shape, and internal JIT ABI errors surface
as `JitError` or `VmError::Jit`; they are not semantic side exits.

Best-effort JIT mode exists only through explicitly named VM APIs. A runtime
compile or OSR failure is recorded, disables that entry from later retries,
and resumes in the interpreter. It is for embedding compatibility and does
not change strict behavior.

Legal runtime side exits are semantic scheduling boundaries:

- cold or not-hot code remains interpreted
- regular/prepared VM call materialization
- `WaitIo`, `WaitQueue`, `Yield`, and `Replay`
- OSR normal exit
- stack-capacity trampoline into VM frame setup
- explicitly requested best-effort embedding APIs

## Metadata Contract

`vo-common-core` is the metadata authority for every executable module. Its
verifier checks metadata kind, width, slot layout, and opcode compatibility.
`vo-jit/src/metadata.rs` provides allocation-free typed views used by lowering
and effect analysis; it does not define a second acceptance policy.

Binary32 arithmetic and comparisons have distinct verified opcodes (`AddF32`,
`SubF32`, `MulF32`, `DivF32`, `NegF32`, and the six ordered/unordered comparison
operations). Their operands use the low 32 bits of scalar `Value` or `Float`
slots. Arithmetic outputs clear the upper 32 bits; comparison outputs are
canonical Boolean `Value` slots. Every arithmetic instruction preserves its
binary32 rounding boundary. Inlining and value propagation MUST NOT widen the
operation or combine rounding boundaries through reassociation or fused
multiply-add. Explicit width conversions retain their existing opcodes.

`vo-common-core` owns bytecode serialization. Current-version bytecode that has
a `instruction_metadata` table must keep `instruction_metadata.len() == code.len()`. Older
bytecode versions and removed metadata tags are not accepted input; they are
rejected before a module can execute or enter strict JIT.

## Opcode Contract

Canonical execution effects, including writes through heap aliases, live in
`vo-common-core::execution_effects`. Register reads, writes and aliased frame
ranges live in `vo-common-core::instruction_effects`. Optimizer invalidation,
frame eligibility and backend lowering consume these contracts. The
`vo-jit/src/semantics/` rows are test-only cross-checks of capability, metadata,
ABI and lowering coverage.

`vo-common-core::instruction_registers` owns encoded register identities for
frame transformations. Empty argument/result windows retain valid boundary
anchors. Dynamic calls also reserve a prefix cell before their argument window
for a borrowed callee's closure or receiver; this storage is part of the frame
ABI even though it carries no ordinary caller register value. Transformations
must preserve these prefixes, contiguous operand ranges, fixed entry slots,
interface pairs and implicit unwind roots, and remap select case metadata with
the case-building instructions. Compiler-owned PC relocation updates branches,
ForLoop edges, loop metadata and source locations before dynamic callsite IDs
are assigned and the resulting module is verified.

## Lowering Responsibilities

`vo-codegen` owns metadata production and typed instruction construction. It
must emit precise layout metadata for dynamic element, map, pointer, slot, call,
extern call, queue, iterator, and interface assertion operations. It also owns
typed return buffers and uses `FuncBuilder` helpers for shared shapes such as
static calls, call buffers, fallthrough returns, and zero-slot initialization.
Loop hint patching and method-value wrappers must go through typed
`FuncBuilder` APIs so bytecode and `instruction_metadata` cannot drift by pc-indexed
manual edits.

`vo-common-core` owns VM-shared bytecode/module validation through
`vo-common-core/src/verifier.rs`. It checks function invariants, per-PC metadata
shape, slot layouts, call shapes, return flags, transfer metadata, branch
targets, GC layouts, write barriers, and index validity before any VM or JIT
execution path accepts a module.

`vo-jit` owns strict JIT validation and lowering:

- `verifier.rs` is a compatibility surface over the shared `ModuleVerifier`;
  executable-input acceptance remains in common-core.
- JIT capability, helper dependencies, ABI contracts, frame materialization,
  side exits, OSR, and direct-call contracts are described by semantic rows,
  helper manifests, and lowering tests.
- `vo-jit/src/semantics/` describes opcode effects, fail-fast policy, runtime
  dependencies, verifier requirements, and capability coverage. Direct tests
  compare those rows with verifier, metadata, ABI, and lowering consumers.
- `call_helpers/plan.rs` owns static/dynamic call route selection.
- `call_helpers/callback_abi.rs` owns JitContext callback ABI callsites.
- `call_helpers/result_flow.rs` owns checked helper result routing and non-OK
  JIT call materialization flow.
- `DynamicCallLowering` in `call_helpers/dynamic/mod.rs` owns the shared
  closure/interface dynamic-call skeleton: inline-cache lookup, hit/miss branch,
  prepare-callback workspace, JIT/VM call dispatch, and return copy. The VM
  callback owns cache publication. Each callsite has four fixed cache lanes;
  native hits validate the exact key and dispatch generation. Full caches
  retain existing entries and resolve additional identities without eviction.
  Closure nil checks, closure func-id keys, slot0/capture handling, interface
  receiver pairs, method keys, and method-index rules stay explicit at the
  callsite.
- `helpers.rs` declares runtime helper imports from one helper table plus the
  runtime ABI manifest; helper names, `FuncId` fields, and per-function refs are
  no longer maintained as separate lists.
- `analysis.rs` caches one `FunctionAnalysis` shared by full JIT and every OSR
  loop. Dynamic calls carry verifier-proven module-global callsite identities into
  the shared cache table without retaining bytecode-PC-sized metadata.
- `compile_common/` owns common full-function/OSR compile facts and driver
  mechanics: `ControlPolicy`, jump-target discovery, basic-block transition,
  instruction selection, and the `CompileDriver` loop. Full-function and
  OSR compilers still own their prologues, return/call lowering, and OSR
  range-exit materialization.

### Optimization and resource ownership

`FunctionCompilePlan` supplies a complete baseline or optimizing configuration.
The immutable per-function graph and recovery states are shared with OSR.
Static Native AOT recovery bodies build an entry-specific graph: every exported
recovery PC has an external canonical-frame edge. Entry values and range facts
must not inherit assumptions established before that PC. Dominance uses a
common virtual predecessor for these entries, and sparse entry loads include
both value liveness and the complete direct/conditional root projection. An
analysis-budget fallback retains baseline slot semantics; recovery compilation
must not enable ordinary-entry object virtualization.
Module entry summaries scan literal definitions conservatively and do not build
cold-function SSA. Per-artifact compiler work is admitted separately from module
summary retention, so a large cold function cannot reject an unrelated hot one.

Leaf inlining admits only complete acyclic recipes with no residual calls.
Recursive functions retain ordinary native activations, whose side exits have
complete VM restoration. Reaching a native depth, stack-byte, or shadow-window
boundary enters a VM trampoline; the Fiber's guest stack and call-frame limits
govern language recursion. Each outer native activation sets a stack-pointer
floor with room for the largest admitted callee. Static, cached dynamic and
prepared calls consume the same depth and stack-byte guard. Frame-elided calls
also participate in native activation accounting and preserve the outer floor.

Typed instructions retain their source PC. The shared driver checks that it
matches the metadata and recovery position before lowering. Prepared-call
requests carry the caller resume PC and callee frame base in distinct fields;
their dedicated helper validates payload widths before publication, and the VM
admits the callee identity and frame when consuming the request.

Scalar replacement preserves the real allocation and its GC/OOM accounting.
Recovery materializes only objects with live rooted aliases at the resume PC.
Allocation sites with simultaneously live dynamic instances cannot share a
single virtual object record.

Container helpers share lifetime-scoped scratch slots with eight-byte alignment.
One-slot map lookup keeps runtime shape dispatch and its generic fallback in a
single helper. Conditional traps with identical recovery variables share a
cold block and pass their kind, arguments, PC and live values on its incoming
edges. This avoids duplicating prefix computations at every recovery site.
String allocation copies from the immutable loaded module's constant pool,
without per-byte generated stores or a literal-sized native stack buffer.
`artifact.rs` validates final machine frame size, including alignment, spills,
outgoing arguments and frame setup, and extracts precise stack-map metadata
for full JIT, OSR and Native AOT. Compile-time recovery states remain available
for emitted spill code. Runtime deoptimization snapshots are retained only for
actual deoptimization sites; current lowering emits direct recovery exits.

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

`RuntimeTransition` means the helper completed the current bytecode instruction
and published deferred VM effects, such as queue wakeups. The bridge
materializes at `call_resume_pc`, yields to the VM so those effects are applied,
and resumes at the following instruction. Generated code must not continue past
this result or replay the completed operation.

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
  needs per-instruction layout metadata.
- Update canonical execution and register effects in `vo-common-core`;
  update capability and semantic cross-checks in `vo-jit`.
- Add VM-shared slot/layout validation in `vo-common-core/src/verifier.rs`.
- Add shared write enumeration to `vo-common-core::instruction_effects` when the
  opcode has operand-, metadata-, or signature-dependent destinations. Keep
  reads and frame-memory effects in the same shared contract.
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
- language parity and OSR: repository test targets `vm`, `jit`, `osr`,
  `gc-vm`, and `gc-osr`; OSR contract cases must prove a native loop entry and
  carry the matching VM baseline.

The `jit-opt` and `gc-jit-opt` test targets force optimizing publication and
require proof of actual optimizing machine-code execution. Compilation success
and baseline execution alone do not satisfy those targets. Execution JSON
includes optimizing compilations, failures, deoptimizations and the number of
distinct functions entered through optimizing code.
