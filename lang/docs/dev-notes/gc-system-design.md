---
date: 2026-08-10
status: implemented
area: runtime
owner: volang
supersedes: []
superseded_by: []
---

# Vo GC System Design

This note describes the collector implemented by `vo-runtime` and its VM root
boundary. Historical collector proposals belong in version control history.

## Ownership and heap model

Each Island owns one single-threaded collector and one stable-address span
heap. Objects do not move. A `GcRef` points at object data immediately after an
eight-byte `GcHeader`:

```text
marked: u8 | reserved: u8 | slots: u16 | ValueMeta: u32
```

`marked` contains age, dual-white, and black bits. `reserved` contains object
layout flags and collector-internal transient flags. The collector keeps:

- dense live-object and data-size vectors for bounded sweep;
- one allocation-extent map for pointer validation, object size, and the
  remembered-set index;
- a gray worklist;
- a precise set of old parents that may contain young references;
- a bounded retirement queue for stale remembered parents;
- resumable root, object, sweep, and reclaim cursors.

The allocation-extent record is the pointer-identity authority. Diagnostic
paths do not maintain a second object index.

## Modes and phases

`GcMode::Incremental` runs major cycles. `GcMode::Generational` normally runs
minor cycles and periodically forces a major cycle. A mode change is accepted
only while the collector is paused.

Every cycle advances through:

```text
Pause -> Propagate -> Atomic -> Sweep -> Reclaim -> Pause
```

`Reclaim` is skipped when the span heap has no pending block reclamation.
Start-cycle, atomic, and sweep root scans use explicit resumable root-scan
kinds. Atomic propagation is sliced by the same work budget as ordinary
propagation.

Every metadata operation, scanned slot, swept allocation, and reclaimed block
is charged. `gc_step_units(n)` cannot perform more than `n` work units. A root
or object scanner that reports pending work must consume a positive amount of
its budget.

## Marking and mutation

New allocations use the current white and enter the gray queue when a cycle is
active. `mark_gray` canonicalizes interior references to their allocation base
before changing color.

The typed write barrier is shared by interpreter, JIT, stdlib, and FFI paths.
It performs two duties:

1. During incremental marking, a white child stored into a gray or black parent
   is shaded immediately.
2. In generational mode, an old parent receiving a young child enters the
   remembered set. During an active minor cycle the child is also shaded
   immediately, including writes after the cycle's remembered prefix was
   frozen.

The caller validates and barriers the new value before publishing the store,
so a rejected value cannot leave a partial mutation.

## Remembered parents

Minor collection snapshots the remembered-set length at cycle start. New
old-to-young stores join the next snapshot and shade their child in the current
cycle.

Remembered parents are queued before object propagation begins. A scan records
whether the parent still contains a young reference. Parents with no young edge
enter a retirement queue. Retirement happens only after the stable prefix has
been queued and is charged one work unit per parent. This preserves cursor
validity when removal uses `swap_remove`.

A write barrier clears a parent's pending-retirement flag. This handles a young
store that occurs after the parent was scanned and before retirement commits.
Dead parents are removed from the set during sweep.

## Roots

The VM owns root discovery. One cursor-driven scanner enumerates:

- globals;
- live fiber frame slots;
- defer, panic, return, select, and replay state;
- queues, endpoints, transports, and pending runtime transitions;
- GC leases.

The same scanner is used by collection and debug verification. Empty domains,
dead entries, and dirty-fiber snapshot copying consume budget, preventing a
large metadata-only root graph from hiding unbounded work.

Root epochs tell the collector whether roots may have changed. Mutable roots
are rescanned at atomic and sweep boundaries before reclamation can pass them.

## Runtime type facts

`vo-common-core` verifies an owned module and produces `LoadedModule`.
`LoadedModule` keeps the module and its derived runtime type facts together.
Collectors and typed barriers consume those immutable facts without rebuilding
recursive layouts or allocating temporary vectors.

Missing facts, kind drift, invalid widths, and unsupported layouts fail closed.
Parent and child Islands share the immutable module image while retaining
separate heaps and collector state.

## Allocation failure and telemetry

Allocation admission checks heap policy and collector metadata capacity before
publishing an object. A failed allocation returns a null `GcRef` plus a typed
`MemoryError`; callers must not initialize the null result.

No-growth mode reserves all object worklists, the extent map, remembered and
retirement queues, and lease storage before disabling growth.

`memory_stats()` is constant-time with respect to live objects, heap blocks,
and leases. Allocation, promotion, release, and sweep maintain counters for:

- live objects and managed bytes by generation;
- large objects and runtime backing;
- committed blocks and free blocks;
- active leases and remembered parents;
- allocation failures and GC work.

The public `dirty_cards` field remains as an ABI name and reports the current
remembered-parent count.

## Required regression coverage

Collector changes must retain tests for:

- one-work-unit root, object, remembered, sweep, and reclaim progress;
- writes after a minor remembered frontier has passed;
- cancellation and bounded commit of remembered-parent retirement;
- roots introduced during propagate and sweep;
- GC stress combined with forced OSR entry and a matching VM baseline;
- managed OOM before any object initialization;
- no-growth metadata exhaustion;
- constant-time stats invariants and live-byte accounting.
