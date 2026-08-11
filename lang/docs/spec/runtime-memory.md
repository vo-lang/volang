# Volang Runtime Memory Specification

Version: 1.0
Status: Accepted
Date: 2026-07-30

## 1. Scope

This specification defines the observable runtime-memory contract shared by
the interpreter, JIT, native extensions, Native hosts, and WebAssembly hosts.
Implementation rationale and workload guidance live in
`docs/game-memory-architecture.md`.

The keywords MUST, MUST NOT, SHOULD, and MAY are normative.

## 2. Island ownership

1. Every Island MUST own an independent managed heap, collector, root set,
   lease table, memory telemetry snapshot, and terminal memory-error state.
2. A mutable managed object MUST have exactly one owning Island.
3. A `GcRef` MUST NOT cross an Island boundary as a raw pointer.
4. Cross-Island values MUST be reconstructed from validated typed packets,
   transferred as owned boundary bytes, or represented by a validated runtime
   handle.
5. A child Island MUST inherit the parent's memory admission and collector
   policy at creation. Live occupancy, roots, counters, and errors MUST remain
   child-local.
6. A managed object's address MUST remain stable for its complete lifetime.

## 3. Managed heap

The runtime managed heap uses 64KiB blocks.

- Small allocations use one of the power-of-two size classes from 16 bytes
  through 32768 bytes.
- A small block serves one size class.
- Larger allocations use a contiguous block run.
- Allocation size includes the collector header and object data.
- Newly allocated object storage is zero initialized.
- An empty small block becomes reusable free-block capacity.
- A dead large allocation enters bounded block-by-block reclaim before its
  complete extent becomes reusable.

`hard_limit_bytes`, when set, MUST bound managed heap committed bytes. It does
not include JIT code, Rust collector metadata, host memory, GPU memory, audio
memory, JavaScript objects, or extension-provider allocations.

Native JIT code MUST have a finite executable-page budget per Island family.
The JIT memory provider MUST reserve a bounded arena during construction and
release that arena when the complete Island family is destroyed. The artifact
budget check MUST use page-granular committed bytes after machine-code
generation and before executable allocation. Telemetry MUST report emitted
bytes separately from charged page bytes. Published entry points remain valid
until family destruction, so budget exhaustion disables the new artifact
without evicting live code. Best-effort JIT execution falls back to the
interpreter; strict JIT execution reports a typed resource rejection. Arena
reservation failure MUST fail strict JIT construction before guest execution.

JIT analysis retention and per-artifact compiler work MUST have independent
finite limits. Full-function and loop-OSR compilation MUST share one immutable
function analysis. Evictable analysis state MUST NOT be pinned by a second VM
manager cache.

JIT safepoint metadata MUST have a separate finite per-family limit and MUST be
retained for exactly the lifetime of its published code. Every complete
function and loop-OSR artifact that contains direct or conditional roots and
can reach a native GC safepoint MUST link an active native-frame record while
it executes. Scalar-only and safepoint-free artifacts MAY omit that record.
Direct `GcRef` values live across a safepoint MUST have precise SP-relative
stack-map entries in an explicit shadow-root area. Ordinary non-GC calls MUST
NOT require root maps. Conditional roots whose pointer interpretation depends
on runtime type tags MUST carry a machine-readable materialization requirement.
Collection MUST wait until those frames have been materialized into typed VM
frames.

Allocation-capable generated code MUST poll before consuming new managed-heap
capacity. The no-work path MAY use runtime-owned raw GC field offsets. The
taken path MUST publish the current bytecode pc and all materializable frame
state before returning to the scheduler. Native-frame validation and the
subsequent VM root scan MUST each have finite work budgets. Allocation helpers
MUST NOT start an implicit collection while generated frames remain active.
Helpers that allocate only on a structural slow path SHOULD defer that
allocation, let generated code poll, and retry with explicit allocation
permission. A replay credential MUST be scoped to one exact function and
instruction, MUST authorize at most one retry, and MUST be issued only after a
collector slice completed. That exact retry MAY allocate while debt remains or
a collection cycle is active; the next allocation-capable instruction MUST poll
again. This guarantees progress by alternating bounded collector work with at
least one mutator allocation.

Each VM MUST also enforce finite limits for scheduled Fiber identities, stack
slots per Fiber, call frames per Fiber, and aggregate native Fiber stack/frame
storage. A runtime transition that publishes multiple spawns MUST reserve all
identities and all required Fiber storage before it publishes the first spawn.
Allocation or limit failure MUST reject the whole transition without a
partially visible spawn batch.

`initial_reserve_bytes` MUST be admitted before guest execution starts. Reserve
is rounded to the allocator's block granularity.

When `growth_allowed` is false:

- the managed heap MUST NOT obtain another segment from its page provider;
- free cells and free blocks already owned by the Island remain usable;
- collector object and lease metadata MUST have an admitted capacity;
- exhaustion of admitted collector metadata MUST produce
  `MetadataExhausted`.

When `allocation_allowed` is false, every managed allocation entry point MUST
fail with `AllocationForbidden`.

## 4. Collector

The collector is precise, stable-address, single-threaded, and incremental.
It supports:

- `GcMode::Incremental`, in which every cycle covers all generations;
- `GcMode::Generational`, in which ordinary minor cycles collect young
  generations and major cycles cover all generations.

Both modes MUST share the same heap layout, object header, write barrier,
root semantics, and failure contract.

A mode change is valid only while the collector is idle. An attempted mode
change during an active cycle MUST return `CollectorBusy`.

### 4.1 Verified runtime type metadata

Before a module becomes executable, bytecode verification MUST produce the
runtime type facts used by object tracing and typed write barriers. The module,
its type metadata, and those facts MUST remain one immutable unit. Parent and
child Islands in one VM family MUST share that unit; their heaps, roots,
collectors, and mutable execution state remain Island-local.

A tracing or barrier operation MUST use the verified facts without rebuilding
a recursive type layout or allocating temporary storage. Missing facts, type
kind drift, and slot-width mismatches MUST fail closed instead of selecting an
approximate layout.

### 4.2 Bounded work

`gc_step_units(N)` MUST complete at most `N` collector work units. A call with
`N == 0` MUST perform no collector work.

The following work MUST be resumable:

- root scanning;
- object and nested inline-layout scanning;
- gray propagation;
- remembered-parent scanning and retirement;
- atomic remark and fixed-point detection;
- sweeping;
- large-span reclaim.

Collector cursors MUST survive between scheduler boundaries. A large object,
deep inline array, large container, or large root set MUST NOT force one call
to scan the complete structure. Runtime type lookup MUST take constant time per
physical slot and MUST NOT hide recursive layout work from the work-unit
budget.

### 4.3 Roots

The precise root set includes all live guest references in:

- fiber frames and registers;
- defer and panic state;
- globals;
- runtime queue, endpoint, transport, and scheduler state;
- active `GcLease` entries;
- pending runtime state that can retain guest objects.

A root mutation during remark or sweep rescue MUST invalidate the affected
root-domain scan and participate in fixed-point completion.

### 4.4 Write barrier

Every heap mutation that stores a GC-bearing value MUST execute the typed
new-value barrier.

The barrier MUST:

1. shade a white child written by a black parent during incremental marking;
2. record an old-to-young edge during generational collection.

Interpreter operations, JIT lowering, runtime containers, standard-library
native helpers, and native-extension host callbacks MUST preserve this
contract.

## 5. Runtime containers

Map bucket backing and queued guest payload slots MUST be represented by
managed runtime-backing objects and precisely traced.

An incremental map scan MUST record the backing generation. If resize replaces
and rehashes the backing between bounded scan chunks, scanning MUST restart at
the beginning of the new generation and remain work-budgeted.

Native protocol metadata, including waiter and endpoint bookkeeping, MAY
remain in Rust containers. Hosts that enforce a total-process limit MUST budget
this memory separately.

Container allocation failure MUST leave a pending Island memory error.
Container helpers MUST NOT consume that error before the VM scheduler observes
it.

## 6. Host configuration and control

The canonical creation configuration is:

```rust
pub struct VmMemoryConfig {
    pub initial_reserve_bytes: usize,
    pub hard_limit_bytes: Option<usize>,
    pub gc_mode: GcMode,
    pub automatic_gc: bool,
    pub oom_policy: OomPolicy,
    pub growth_allowed: bool,
    pub allocation_allowed: bool,
    pub max_objects: Option<usize>,
    pub max_leases: Option<usize>,
}
```

The default configuration uses generational automatic collection, permits
growth and allocation, has no hard limit, and uses
`CollectThenTerminateIsland`.

The host control surface consists of:

- reserve managed capacity;
- set the managed hard limit;
- allow or deny managed growth;
- allow or deny managed allocation;
- report external/provider memory;
- read memory statistics;
- select GC mode while idle;
- stop or restart automatic GC;
- request bounded GC work;
- request a complete major collection at a safe host boundary.

Disabling growth is fallible because the runtime may need to reserve collector
metadata first.

Guest code MUST NOT directly change reserve, hard limit, growth permission, or
allocation permission.

## 7. Standard library

Package `runtime/mem` exposes:

```vo
func ReadStats() Stats
func GCStep(workUnits uint64) bool
func GCCollect() bool
```

`GCStep` and `GCCollect` schedule work for a VM scheduler boundary. Their
boolean result reports whether a VM owner accepted the request.

The `Stats` layout is platform independent and includes:

- reserved, committed, live, young, old, large, and runtime-backing bytes;
- free blocks, partial spans, fragmentation, and reclaim backlog;
- externally reported bytes and unknown external-provider count;
- allocation totals and failures;
- minor/major cycle, work-unit, remembered-parent/root, remark, and lease counters;
- growth/allocation permissions, hard-limit presence, GC mode/state, and
  automatic-GC state;
- WebAssembly current/maximum pages.

Native and WebAssembly runtimes MUST preserve the same public field meanings.

## 8. Allocation failure and Island termination

The memory error classes are:

- `AllocationForbidden`;
- `GrowthDisabled`;
- `HardLimitExceeded`;
- `MetadataExhausted`;
- `SystemAllocationFailed`;
- `InvalidPointer`;
- `CollectorBusy`.

An allocation failure MUST be retained as the Island's pending memory error.
Generated and interpreted code MUST return to the scheduler before
dereferencing a null allocation result or executing a later guest side effect.

The scheduler MUST convert the pending error to a sticky
`VmError::IslandMemory`. Later host polls MUST observe the same terminal error.
The failed instruction MUST NOT be replayed.

Under `CollectThenTerminateIsland`, a scheduler at a clean collector boundary
MAY perform one final major collection before teardown. Its purpose is
reclamation and terminal telemetry. Under `TerminateIsland`, the scheduler
skips this collection.

Termination clears the affected Island's executable and runtime state. Other
Islands remain independent failure domains.

## 9. Native FFI roots

Native extension ABI 10 defines:

```rust
pub struct GcLease {
    pub index: u32,
    pub generation: u32,
}
```

A native extension that retains a managed object across a call or safe point
MUST create a lease through the host callback table. It MUST resolve the lease
before use and release it when finished.

A released, stale, foreign, or out-of-range lease MUST fail closed.
Extensions MUST NOT retain an unleased raw `GcRef` across a safe point.

The ABI exposes create, resolve, and release callbacks through
`ExtHostOpsV10`. Host services use the independently versioned
`ExtHostServicesV2` table.

## 10. WebAssembly admission

WebAssembly memory admission uses 64KiB pages.

Before guest execution:

1. reserve bytes are rounded up to pages;
2. `current_pages + reserve_pages` MUST fit the declared maximum, if present;
3. a configured hard limit MUST conservatively fit as
   `current_pages + hard_limit_pages <= maximum_pages`;
4. the runtime MUST request exactly `reserve_pages` of linear-memory growth;
5. a failed growth request MUST fail VM construction;
6. current and maximum pages MUST be published through memory telemetry.

WebAssembly maximum pages bound the complete linear memory. The Island hard
limit bounds the managed heap committed within that memory.

## 11. Conformance

A conforming implementation MUST test:

- small and large allocation, reuse, hard-limit, no-growth, and bounded
  reclaim;
- bounded root/object/card/remark/sweep work;
- minor/major reachability under mutation;
- Interpreter and generated JIT OOM exits;
- full-function and loop-OSR precise stack maps, nested native-frame chains,
  conditional-root materialization, and bounded safepoint scans;
- managed map and queue backing reachability;
- child-Island configuration inheritance and heap isolation;
- lease generation and capacity failures;
- cross-Island unpack failure without null dereference;
- Native and WebAssembly admission/stat field equivalence;
- CLI and `runtime/mem` surface contracts.
