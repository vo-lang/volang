# VM Call Performance Redesign (2026-03-26)

## Status: STARTED

## Scope

This note is a VM-only call performance redesign plan.

In scope:
- Interpreter `Call`, `CallClosure`, and `CallIface`
- VM frame setup and return handling
- Codegen changes that remove unnecessary dynamic call overhead in VM mode
- Bytecode or metadata changes that make VM call execution structurally cheaper

Out of scope:
- JIT-specific optimization work
- Function inlining
- Narrow micro-optimizations that preserve the current high-overhead call ABI

## Problem Statement

The current VM call path pays too much per call even before the callee does meaningful work.
The dominant costs are structural:

- Every call allocates a fresh callee frame region
- The whole frame is zeroed even when the parameter window is overwritten immediately
- Arguments are copied from caller call-buffer to callee frame
- Return values are copied back into caller-owned slots
- Closure and interface calls add dispatch work on top of the same expensive frame protocol

This means the current system spends too much time on call mechanics instead of user code.
If the goal is a thorough VM improvement, the main target must be the call ABI and frame lifecycle,
not isolated helper-level tweaks.

## Current Invariants

The existing implementation already gives us the metadata needed for a correct staged redesign:

- `FunctionDef.param_slots` is the canonical size of the callee parameter window
- `FunctionDef.local_slots` is the total frame footprint
- `FunctionDef.recv_slots` describes receiver width for method-style calls
- `FunctionDef.slot_types` is the authoritative slot layout for future precise initialization
- `Closure::call_layout()` is the authoritative rule for closure slot-0 placement and user-arg offset

These invariants are enough to start reducing wasted work without changing language semantics.

## Root-Cause Ranking

### 1. Over-expensive frame initialization

The interpreter currently zeroes the whole frame for:
- `Call`
- `CallClosure`
- `CallIface`
- defer-call entry
- closure replay frame setup
- some VM/JIT shared frame materialization paths

This is overly conservative. The parameter window is written immediately after frame allocation,
so zeroing it first is pure waste.

### 2. Copy-heavy call ABI

The current ABI is still caller-buffer to callee-frame by copy, then callee-return to caller-buffer by copy.
This is the largest remaining architectural cost after frame-wide zeroing is reduced.

### 3. Return path is still too coupled to the unwind machinery

Functions without defer should take a direct return fast path.
Return should only enter the full unwind engine when defer / errdefer / panic / recover semantics require it.

### 4. Dynamic dispatch is paying on top of the expensive base ABI

`CallClosure` and `CallIface` are not primarily slow because of dispatch alone.
They are slow because dispatch is layered on top of the same copy-heavy frame protocol.

## Design Principles

- Make the common case cheaper by construction
- Do not preserve an expensive ABI just because helpers already exist
- Reuse existing metadata instead of introducing ad hoc runtime guesses
- Prefer fail-fast structural assumptions over hidden fallback logic
- Keep VM semantics authoritative; JIT can adapt later if needed

## Target Architecture

### Phase A: Clean frame preparation

Before changing the ABI, unify frame setup and remove obviously wasted work:

- Reserve the callee frame once
- Do not zero the parameter window that is overwritten immediately
- Zero only the tail of the reserved frame that remains uninitialized after argument placement
- Use bulk stack copies where the source is already contiguous
- Apply the same preparation rules to:
  - static calls
  - closure calls
  - interface calls
  - defer call entry
  - closure replay frame creation
  - VM/JIT shared regular-call setup

This is the correct first slice because it reduces cost immediately without changing semantics.

### Phase B: Return fast path split

Separate the ordinary return path from the unwind engine:

- no defer, no heap-return handling -> direct return fast path
- defer / errdefer / panic / recover -> unified slow path

This must be explicit. Ordinary call/return traffic should not pay unwind machinery costs.

### Phase C: VM call ABI redesign

The real architectural goal is to stop copying arguments into a second frame window.
The intended end state is:

- caller-owned call buffer becomes the callee parameter window
- callee `bp` points directly at that window
- only extra local space beyond parameters is reserved separately
- return values stay in the caller-visible destination window
- static, closure, and interface calls share one frame protocol and differ only in target resolution

This is the largest and most important change. Everything before it should prepare the codebase for it.

### Phase D: Dynamic call specialization

Once the base ABI is cheaper, make dynamic calls thinner:

- per-call-site interface dispatch cache in VM
- closure layout specialization by kind instead of repeatedly routing through a fully generic path
- codegen lowering that removes known-target dynamic calls

### Phase E: Interpreter-side prepared call metadata

After the ABI stabilizes, move hot call-site decoding work out of the dispatch loop:

- prepared call descriptors
- pre-decoded arg/ret counts
- attached dispatch cache state
- direct access to resolved metadata needed by hot call paths

## Immediate Implementation Order

### Step 1. Unified frame-tail initialization

Implement now:
- shared helpers for frame-tail zeroing
- bulk stack copies for contiguous argument transfer
- remove whole-frame zeroing from VM call entry where the prefix is overwritten immediately

Files expected in the first slice:
- `lang/crates/vo-vm/src/fiber.rs`
- `lang/crates/vo-vm/src/exec/call.rs`
- `lang/crates/vo-vm/src/exec/unwind.rs`
- `lang/crates/vo-vm/src/vm/mod.rs`
- `lang/crates/vo-vm/src/vm/jit/mod.rs` for shared frame setup consistency

### Step 2. Return fast-path extraction

Implement next:
- explicit no-defer / stack-return fast path
- keep current unwind engine only for the semantic slow path

### Step 3. ABI redesign

Implement after Step 1 and Step 2 are stable:
- redefine caller/callee ownership of the parameter window
- update interpreter call opcodes and frame helpers together
- keep closure/interface target resolution separate from frame layout

### Step 4. Compiler-side de-thunking

Implement after the ABI is settled:
- lower known closure targets to direct `Call`
- lower proven monomorphic interface targets to direct `Call`
- avoid generating unnecessary wrapper-based dynamic calls when the target is already known

## First-Slice Correctness Rules

The first slice must preserve these invariants:

- Any slot that may still contain stale GC-visible data after frame allocation must be explicitly initialized
- Any slot guaranteed to be overwritten before observation must not be zeroed preemptively
- Closure argument placement must continue to use `Closure::call_layout()` as the single source of truth
- Defer-entry calls and replay calls must use the same frame initialization rules as ordinary calls

## Validation Plan

For the first slice:

- targeted VM tests covering closure calls, interface calls, method values, defer, and replay paths
- targeted JIT regression tests only where shared VM frame setup is reused
- then broad `./d.py test vm`
- then `./d.py test jit --release` because shared frame setup code is still exercised by JIT fallback and mixed paths

## Success Criteria

The redesign is moving in the right direction when:

- call-heavy VM benchmarks improve before any ABI rewrite lands
- frame setup code becomes centralized instead of duplicated
- whole-frame zeroing disappears from common call entry paths
- the codebase is ready for the larger caller-window-as-callee-params ABI transition
