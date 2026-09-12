import { AotRunQueue } from './aot_run_queue.js';
import { allocationBarrierHint, frameBarrierHint, fiberBarrierHint, publishBarrierHint, type AotBarrierHint } from './aot_barriers.js';
/** One memory provider, with independent managed owners and incremental cursors. */
import { AotHeapProvider, AotSpanHeap, AotIslandMemoryError, type AotHeapPolicy, type AotHeapSpan } from './aot_span_heap.js';
import { AotCollector, type AotGcMode } from './aot_collector.js';
import { AotTraceCursor, type AotMemoryMetadata, type AotTraceOwner } from './aot_trace.js';

export interface AotMemoryOptions extends AotHeapPolicy {
  readonly collectorMode?: AotGcMode;
  readonly stepUnits?: number;
  readonly debtBytes?: number;
  readonly automaticGC?: boolean;
  /** Instance-wide capacity for host scheduler registrations, separate from managed bytes. */
  readonly maxSchedulerWaiters?: number;
}

export interface AotGcLease {
  resolve(): bigint;
  release(): void;
}

/** Host controls run between guest turns; guest runtime/mem only requests work. */
export interface AotMemoryControl {
  stats(): ReturnType<AotMemoryRuntime['stats']>;
  reserve(bytes: number, island?: number): boolean;
  setHardLimit(bytes: number | undefined, island?: number): boolean;
  setGrowthAllowed(allowed: boolean, island?: number): boolean;
  setAllocationAllowed(allowed: boolean, island?: number): void;
  setMode(mode: AotGcMode, island?: number): boolean;
  setAutomaticGC(enabled: boolean, island?: number): void;
  reportExternalBytes(bytes: number, island?: number): void;
  step(units: number, island?: number): number;
  collect(island?: number): boolean;
}

/** A borrowed host call retains allocations through asynchronous replay. */
export class AotHostCall {
  readonly roots = new Set<number>();
  raw = false;
  private retained = false;
  private closed = false;
  private writes?: Map<number, bigint>;
  constructor(readonly runtime: AotMemoryRuntime, readonly owner: IslandOwner,
    readonly frame: number, readonly destination: number, private readonly slots: ArrayLike<number>, private readonly deferred: boolean) { owner.hostCalls.add(this); }
  staged(frame: number, slot: number): bigint | undefined { return frame === this.frame ? this.writes?.get(slot) : undefined; }
  stage(frame: number, slot: number, value: bigint): boolean {
    if (!this.deferred || frame !== this.frame || slot < this.destination || slot >= this.destination + this.slots.length) return false;
    this.writes ??= new Map(); this.writes.set(slot, value);
    const kind = this.slots[slot - this.destination];
    let reference = 0;
    if (kind === 1 || kind === 2) reference = this.owner.reference(value);
    else if (kind === 3 && this.writes.has(slot + 1) && Number(value & 255n) >= 14) reference = this.owner.reference(this.writes.get(slot + 1)!);
    else if (kind === 4 && this.writes.has(slot - 1) && Number(this.writes.get(slot - 1)! & 255n) >= 14) reference = this.owner.reference(value);
    if (reference) { this.roots.add(reference); this.owner.collector.rootWrite(reference); }
    return true;
  }
  run<T>(action: () => T): T {
    if (this.closed) throw new Error('expired Volang host-call lease');
    if (this.owner.heap.error) throw this.owner.heap.observeFailure();
    return this.runtime.withCall(this, action);
  }
  borrowMemory(): WebAssembly.Memory {
    if (this.closed) throw new Error('expired Volang host-call lease');
    this.raw = true; this.owner.collector.rootsChanged();
    return this.runtime.provider.memory;
  }
  retain(): void {
    if (this.closed) throw new Error('expired Volang host-call lease');
    if (this.retained) return;
    if (!this.runtime.admitLease(this.owner)) throw this.owner.heap.observeFailure();
    this.retained = true; this.owner.retainedCalls += 1;
  }
  leave(): void { if (!this.retained) this.release(); }
  cancel(): void {
    this.writes = undefined;
    this.release();
  }
  release(): void {
    if (this.closed) return;
    try {
      if (this.writes && !this.owner.heap.error) {
        // Publish an entire asynchronous return tuple atomically with respect
        // to GC, including conditional interface header/payload pairs.
        const view = this.runtime.provider.view();
        for (const [slot, value] of this.writes) view.setBigUint64(this.frame + slot * 8, BigInt.asUintN(64, value), true);
        this.runtime.write(this.frame + this.destination * 8, this.slots.length * 8);
      }
    } finally {
      this.closed = true; this.owner.hostCalls.delete(this);
      if (this.retained) this.owner.retainedCalls -= 1;
      if (this.raw) {
        this.owner.coarseHold = true;
        this.owner.collector.requestMajor(); this.runtime.request(this.owner);
      }
    }
  }
}

class IslandOwner implements AotTraceOwner {
  state = 0;
  readonly heap: AotSpanHeap;
  readonly trace: AotTraceCursor;
  readonly collector: AotCollector;
  readonly leases = new Set<number>();
  readonly fiberRoots = new Set<number>();
  // The image entry frame is static; managed frames use admitted span metadata.
  private entryFrameFunction?: number;
  readonly hostCalls = new Set<AotHostCall>();
  retainedCalls = 0;
  readonly externalLeases = new Set<{ reference: bigint }>();
  externalBytes = 0;
  automaticGC: boolean;
  coarseHold = false;
  debt = 0;
  debtRetiredThrough = 0;
  manualUnits = 0;
  fullTarget = 0;
  lastWork = 0;
  maxWork = 0;
  terminalWork = 0;
  leaseEpoch = 0;
  cleanup?: Generator<void>;
  cleaned = false;
  private leaseCursor?: MapIterator<[number, number]>;
  readonly borrowed = new Map<number, number>();
  beginCycle(major: boolean): void {
    this.leaseCursor = undefined;
    if (major) { this.leaseEpoch += 1; this.leaseCursor = this.borrowed.entries(); }
  }
  finishCycle(major: boolean): void { if (major) this.coarseHold = false; }
  nextLease(): boolean {
    const next = this.leaseCursor?.next();
    if (!next || next.done) return false;
    const [pointer, seen] = next.value;
    if (seen !== this.leaseEpoch) this.runtime.releaseEndpoint(this, pointer);
    return true;
  }
  constructor(readonly runtime: AotMemoryRuntime, id: number, policy: AotMemoryOptions) {
    this.heap = new AotSpanHeap(runtime.provider, id, policy);
    this.trace = new AotTraceCursor(runtime.metadata, this);
    this.collector = new AotCollector(this.heap, this.trace, policy.collectorMode);
    this.automaticGC = policy.automaticGC !== false;
  }
  reference(value: bigint): number {
    return this.referenceWord(Number(BigInt.asUintN(32, value)), Number(BigInt.asUintN(32, value >> 32n)));
  }
  referenceWord(pointer: number, generation: number): number {
    // Only the low wasm32 address participates in root membership. Temporary
    // call slots can carry interface flags with a null payload while a tuple
    // is being moved; endpoint use validates both halves separately.
    if (pointer === 0) return 0;
    if (this.runtime.states.has(pointer)) return this.runtime.states.get(pointer) === this ? pointer : 0;
    const header = this.runtime.provider.findHeader(pointer);
    if (!header) {
      if (generation || pointer >= this.runtime.metadata.stackBase) this.heap.fail('InvalidPointer');
      return 0;
    }
    const span = this.runtime.provider.spanAtHeader(header)!;
    if (this.runtime.isEndpoint(pointer)) {
      if (!this.runtime.validateEndpoint(this, pointer, generation)) return 0;
      if (span.heap !== this.heap) { this.runtime.retainEndpoint(this, pointer); return 0; }
    } else if (generation || span.heap !== this.heap) { this.heap.fail('InvalidPointer'); return 0; }
    return pointer;
  }
  *frames(): Generator<number> {
    if (this.entryFrameFunction !== undefined) yield this.runtime.metadata.stackBase;
    yield* this.heap.frames();
  }
  frameFunction(raw: number, span?: AotHeapSpan, index = 0): number | undefined {
    if (raw === this.runtime.metadata.stackBase) return this.entryFrameFunction;
    if (!span || span.heap !== this.heap || raw !== span.header(index) + 32) return undefined;
    return span.frameFunction(index);
  }
  registerFrame(raw: number, functionId: number, span?: AotHeapSpan): boolean {
    if (span) return span.registerFrame(span.index(raw), functionId);
    this.entryFrameFunction = functionId;
    return true;
  }
  fibers(): Iterable<number> { return this.fiberRoots; }
  *extraRoots(): Generator<number> {
    yield* this.leases;
    for (const lease of this.externalLeases) yield this.reference(lease.reference);
    let raw = this.coarseHold;
    for (const call of this.hostCalls) { yield* call.roots; raw ||= call.raw; yield 0; }
    if (raw) {
      // Raw views borrow the whole owner for the call. The post-call major
      // rebuilds precise edges, one allocation/slot at a time, before reuse.
      for (const span of this.heap.spans) {
        for (let index = 0; index < span.slots; index += 1) yield span.has(index) ? span.header(index) + 32 : 0;
      }
    }
  }
  releaseObject(header: number): void { this.runtime.releaseObject(header); }
}

export class AotMemoryRuntime {
  readonly provider: AotHeapProvider;
  private readonly runQueue: AotRunQueue;
  readonly states = new Map<number, IslandOwner>();
  readonly owners: IslandOwner[] = [];
  private readonly fibers = new Map<number, IslandOwner>();
  private readonly endpoints = new Map<number, { owner: IslandOwner; generation: number; peers: Set<IslandOwner> }>();
  private endpointGeneration = 0;
  private readonly cloneMemo = new Map<number, number>();
  private readonly pending = new Set<IslandOwner>();
  private readonly failedOwners = new Set<IslandOwner>();
  private readonly activeCollectors = new Set<IslandOwner>();
  private readonly barrierOwners = new Set<IslandOwner>();
  private readonly policy: AotMemoryOptions;
  private readonly globals = new Map<string, WebAssembly.Global>();
  private readonly publishedGlobals = new Map<string, number>();
  private draining = false;
  private running = false;
  private scope?: IslandOwner;
  private activeCall?: AotHostCall;
  private readonly scopes: (IslandOwner | undefined)[] = [];
  private readonly root: IslandOwner;
  private readonly allocationHints: AotBarrierHint[];
  private readonly frameHints: AotBarrierHint[];
  private readonly fiberHint: AotBarrierHint;
  constructor(memory: WebAssembly.Memory, maximumPages: number, readonly metadata: AotMemoryMetadata, options: AotMemoryOptions = {}) {
    this.provider = new AotHeapProvider(memory, maximumPages, metadata.barrierPages);
    this.runQueue = new AotRunQueue(options.maxSchedulerWaiters);
    this.allocationHints = metadata.descriptors.map(allocationBarrierHint);
    this.frameHints = metadata.frames.map(frame => frameBarrierHint(metadata, frame.slots));
    this.fiberHint = fiberBarrierHint(metadata);
    for (const key of ['stepUnits', 'debtBytes'] as const) {
      const value = options[key];
      if (value !== undefined && (!Number.isSafeInteger(value) || value < 1)) throw new Error(`invalid memory ${key}`);
    }
    this.policy = { ...options };
    if (options.collectorMode !== undefined && !['incremental', 'generational'].includes(options.collectorMode)) throw new RangeError('invalid collector mode');
    if (options.hardLimitBytes !== undefined
      && Math.ceil(options.hardLimitBytes / 65536) + memory.buffer.byteLength / 65536 > maximumPages) {
      throw new RangeError('managed hard limit exceeds the admitted WebAssembly maximum');
    }
    this.root = this.makeOwner(this.policy);
    if (this.root.heap.error) throw this.root.heap.observeFailure();
  }
  private makeOwner(policy: AotMemoryOptions): IslandOwner {
    const owner = new IslandOwner(this, this.owners.length, policy);
    this.owners.push(owner);
    owner.heap.onFailure = () => { this.failedOwners.add(owner); this.setGlobal('vo_memory_failed', 1); };
    if (owner.heap.error) owner.heap.onFailure();
    return owner;
  }
  private hostOwner(island = 0): IslandOwner {
    if (this.running || this.activeCall) throw new Error('memory controls require a host scheduling boundary');
    const owner = this.owners[island];
    if (!owner) throw new RangeError('unknown Island');
    return owner;
  }
  private objectCapacity(owner: IslandOwner, bytes = owner.heap.stats().committedBytes): number {
    return Math.min(owner.heap.policy.maxObjects ?? Number.MAX_SAFE_INTEGER, Math.ceil(bytes / 65536) * 1024);
  }
  controls(): AotMemoryControl {
    return {
      stats: () => this.stats(),
      reserve: (bytes, island) => {
        const owner = this.hostOwner(island);
        if (!Number.isSafeInteger(bytes) || bytes < 0) throw new RangeError('invalid reserve');
        if (!owner.heap.policy.growthAllowed
          && !owner.collector.admitObjects(this.objectCapacity(owner, owner.heap.stats().committedBytes + bytes), true)) return false;
        return owner.heap.reserve(bytes);
      },
      setHardLimit: (bytes, island) => {
        const owner = this.hostOwner(island);
        if (bytes !== undefined && Math.ceil(bytes / 65536) + this.provider.memory.buffer.byteLength / 65536 > this.provider.maximumPages) return false;
        return owner.heap.setHardLimit(bytes);
      },
      setGrowthAllowed: (allowed, island) => {
        const owner = this.hostOwner(island);
        if (!allowed && !owner.collector.admitObjects(this.objectCapacity(owner), true)) return false;
        return owner.heap.setGrowthAllowed(allowed);
      },
      setAllocationAllowed: (allowed, island) => this.hostOwner(island).heap.setAllocationAllowed(allowed),
      setMode: (mode, island) => {
        if (mode !== 'incremental' && mode !== 'generational') throw new RangeError('invalid collector mode');
        const owner = this.hostOwner(island);
        const changed = owner.collector.setMode(mode);
        if (changed) this.updateBarrierState(owner);
        return changed;
      },
      setAutomaticGC: (enabled, island) => {
        const owner = this.hostOwner(island); owner.automaticGC = enabled;
        if (enabled && (owner.collector.phase !== 'idle' || owner.debt >= (this.policy.debtBytes ?? 8 * 1024 * 1024))) this.request(owner);
      },
      reportExternalBytes: (bytes, island) => {
        if (!Number.isSafeInteger(bytes) || bytes < 0) throw new RangeError('invalid external memory size');
        this.hostOwner(island).externalBytes = bytes;
      },
      step: (units, island) => {
        const owner = this.hostOwner(island);
        const used = owner.collector.step(units);
        if (units !== 0) {
          owner.lastWork = used; owner.maxWork = Math.max(owner.maxWork, used);
          this.updateBarrierState(owner);
        }
        return used;
      },
      collect: island => this.withOwner(this.hostOwner(island), () => this.requestCollection()),
    };
  }
  lease(reference: bigint): AotGcLease {
    const owner = this.current();
    const pointer = owner.reference(reference);
    if (owner.heap.error || !pointer || !this.admitLease(owner)) throw owner.heap.observeFailure() ?? new Error('lease requires an owned allocation');
    const entry = { reference };
    owner.externalLeases.add(entry); owner.collector.rootWrite(pointer);
    let released = false;
    return {
      resolve: () => {
        if (released) throw new Error('expired Volang GC lease');
        if (owner.heap.error) throw owner.heap.observeFailure();
        if (!owner.reference(reference)) throw new Error('invalid Volang GC lease');
        return reference;
      },
      release: () => { if (!released) { released = true; owner.externalLeases.delete(entry); } },
    };
  }
  attach(instance: WebAssembly.Instance): void {
    this.globals.clear(); this.publishedGlobals.clear();
    for (const [name, value] of Object.entries(instance.exports)) {
      if (value instanceof WebAssembly.Global) this.globals.set(name, value);
    }
  }
  global(name: string): number { return Number(this.globals.get(name)?.value ?? 0) >>> 0; }
  private updateBarrierState(owner: IslandOwner): void {
    if (!owner.heap.error && owner.collector.needsBarrier) this.barrierOwners.add(owner);
    else this.barrierOwners.delete(owner);
    if (owner.heap.error || owner.collector.phase === 'idle') this.activeCollectors.delete(owner);
    else this.activeCollectors.add(owner);
    this.setGlobal('vo_gc_barrier', Number(this.barrierOwners.size !== 0) | (this.activeCollectors.size ? 2 : 0));
  }
  private setGlobal(name: 'vo_gc_debt' | 'vo_gc_barrier' | 'vo_memory_failed', value: number): void {
    // These exports have a single host writer. Publish transitions, avoiding
    // a Wasm Global access for unchanged allocation debt and barrier state.
    if (this.publishedGlobals.get(name) === value) return;
    const global = this.globals.get(name);
    if (global) { global.value = value; this.publishedGlobals.set(name, value); }
  }
  private interruptQuantum(): void {
    // Generated code replenishes this shared counter on every dispatch.
    const quantum = this.globals.get('vo_execution_quantum');
    if (quantum) quantum.value = 0;
  }
  current(): IslandOwner {
    if (this.scope) return this.scope;
    const fiber = this.global('vo_current_fiber');
    const owner = this.fibers.get(fiber);
    if (owner) return owner;
    const state = fiber ? this.provider.view().getUint32(fiber + this.metadata.fiberIsland, true) : 0;
    return this.states.get(state) ?? this.root;
  }
  withOwner<T>(owner: IslandOwner, action: () => T): T {
    const previous = this.scope; this.scope = owner;
    try { return action(); } finally { this.scope = previous; }
  }
  openCall(frame: number, destination: number, count: number, types: Uint8Array, effects: bigint): AotHostCall {
    const raw = frame - this.metadata.frameBytes;
    const header = this.provider.findHeader(raw);
    const span = this.provider.spanAtHeader(header);
    const owner = span ? this.owners[span.heap.island] : this.root;
    const functionId = owner.frameFunction(raw, span, span?.index(raw));
    const slots = types.length ? types : this.metadata.frames[functionId ?? -1]?.slots.slice(destination, destination + count) ?? Array(count).fill(0);
    return new AotHostCall(this, this.current(), frame, destination, slots, (effects & 28n) !== 0n);
  }
  stagedSlot(frame: number, slot: number): bigint | undefined { return this.activeCall?.staged(frame, slot); }
  stageSlot(frame: number, slot: number, value: bigint): boolean { return this.activeCall?.stage(frame, slot, value) ?? false; }
  withCall<T>(call: AotHostCall, action: () => T): T {
    const previous = this.activeCall; this.activeCall = call;
    try {
      return this.withOwner(call.owner, () => {
        const value = action();
        if (call.owner.heap.error) throw call.owner.heap.observeFailure();
        return value;
      });
    } finally { this.activeCall = previous; }
  }
  request(owner: IslandOwner): void { this.pending.add(owner); this.setGlobal('vo_gc_debt', 8 * 1024 * 1024); }
  private allocate(owner: IslandOwner, bytes: number, descriptor: number, frameFunction?: number): number {
    const layout = this.metadata.descriptors[descriptor];
    if (!layout) { owner.heap.fail('InvalidPointer'); return 0; }
    const tag = layout.tag;
    if (tag === 6 && this.endpointGeneration === 0xffff_ffff) {
      owner.heap.fail('MetadataExhausted'); return 0;
    }
    if (!owner.collector.admitObjects(owner.heap.objectCount + 1)) return 0;
    const backingBytes = tag === 1 || tag === 5 ? bytes + 32 : tag === 6 ? Math.max(0, bytes - 120) : 0;
    const pointer = owner.heap.allocate(bytes, descriptor, 0, backingBytes, frameFunction);
    if (!pointer) return 0;
    publishBarrierHint(this.provider.view(), pointer - 32, this.allocationHints[descriptor]);
    if (tag === 6) {
      const generation = ++this.endpointGeneration;
      this.endpoints.set(pointer, { owner, generation, peers: new Set() });
      this.provider.view().setUint32(pointer - 4, generation, true);
    }
    if (this.activeCall?.owner === owner) this.activeCall.roots.add(pointer);
    owner.collector.publish(pointer);
    owner.debt += bytes + 32;
    if (owner.automaticGC && (owner.debt >= (this.policy.debtBytes ?? 8 * 1024 * 1024) || owner.collector.phase !== 'idle')) this.pending.add(owner);
    this.setGlobal('vo_gc_debt', this.pending.size ? 8 * 1024 * 1024 : 0);
    return pointer;
  }
  private collect(fallback: IslandOwner, units: number): number {
    const owner = this.pending.values().next().value ?? fallback;
    const automatic = owner.automaticGC;
    const full = owner.fullTarget !== 0 || owner.coarseHold;
    const budget = owner.manualUnits !== 0 && !full
      ? Math.min(owner.manualUnits, this.policy.stepUnits ?? units) : this.policy.stepUnits ?? units;
    let used = 0;
    if (owner.heap.error) {
      if (owner !== this.root && !owner.cleaned) {
        owner.cleanup ??= this.cleanup(owner);
        while (used < budget) {
          used += 1;
          if (owner.cleanup.next().done) {
            owner.cleaned = true;
            this.failedOwners.delete(owner);
            this.setGlobal('vo_memory_failed', Number(this.failedOwners.size !== 0));
            break;
          }
        }
      }
      owner.terminalWork += used;
    } else used = owner.collector.step(budget);
    owner.manualUnits = owner.collector.phase === 'idle' ? 0 : Math.max(0, owner.manualUnits - used);
    owner.lastWork = used; owner.maxWork = Math.max(owner.maxWork, used);
    if (owner.fullTarget && owner.collector.stats().majorCycles >= owner.fullTarget) owner.fullTarget = 0;
    this.pending.delete(owner);
    if (owner.heap.error) { if (owner !== this.root && !owner.cleaned) this.pending.add(owner); }
    else {
      if (owner.collector.phase === 'idle') {
        owner.debt = 0;
        owner.debtRetiredThrough = owner.heap.allocationIdentity;
      }
      if (owner.manualUnits || owner.fullTarget || owner.coarseHold || owner.collector.requested
        || (automatic && owner.collector.phase !== 'idle')) this.pending.add(owner);
    }
    this.setGlobal('vo_gc_debt', this.pending.size ? 8 * 1024 * 1024 : 0);
    this.updateBarrierState(owner);
    return used;
  }
  private terminal(owner: IslandOwner): void {
    owner.heap.observeFailure();
    this.updateBarrierState(owner);
    if (owner.state) this.provider.view().setBigUint64(owner.state, 0xffff_ffff_ffff_ffffn, true);
    if (owner !== this.root && !owner.cleaned) this.pending.add(owner);
    this.setGlobal('vo_gc_debt', this.pending.size ? 8 * 1024 * 1024 : 0);
  }
  private closeEndpoint(pointer: number): void {
    this.runQueue.close(pointer);
    const view = this.provider.view();
    view.setBigUint64(pointer + 48, 1n, true);
    for (const offset of [0, 56, 64, 88, 96, 104, 112]) view.setBigUint64(pointer + offset, 0n, true);
  }
  private *cleanup(owner: IslandOwner): Generator<void> {
    // Scheduler frame removal remains authoritative. Wait for every live
    // owner record, charging each inspected Fiber and each retry separately.
    for (; ;) {
      if (this.draining) {
        for (const fiber of owner.fiberRoots) {
          while (!this.runQueue.wakeForCleanup(fiber)) yield;
          yield;
        }
        break;
      }
      let fiber = this.global('vo_fiber_head');
      let active = false;
      while (fiber) {
        const view = this.provider.view();
        const next = view.getUint32(fiber + this.metadata.fiberNext, true);
        if (view.getUint32(fiber + this.metadata.fiberIsland, true) === owner.state) {
          active = true;
          while (!this.runQueue.wakeForCleanup(fiber)) yield;
        }
        fiber = next;
        yield;
      }
      if (!active) break;
      yield;
    }
    for (const pointer of owner.borrowed.keys()) { this.releaseEndpoint(owner, pointer); yield; }
    for (const call of owner.hostCalls) { call.cancel(); yield; }
    for (const lease of owner.externalLeases) { owner.externalLeases.delete(lease); yield; }
    for (const span of owner.heap.spans) {
      for (let index = 0; index < span.slots; index += 1) {
        if (span.has(index)) {
          const header = span.header(index);
          const pointer = header + 32;
          const endpoint = this.endpoints.get(pointer);
          if (endpoint) this.closeEndpoint(pointer);
          if (pointer !== owner.state && !endpoint?.peers.size) {
            this.releaseObject(header); owner.heap.release(span, index);
          }
        }
        yield;
      }
      while (span.reclaiming) { owner.heap.reclaimOne(span); yield; }
    }
    while (owner.heap.returnFreePage()) yield;
  }
  private status(status: number): number {
    const owner = this.current();
    if (status === 0xffff_ffff) return owner.heap.error ? 4 : 0;
    if (!owner.heap.error) return status;
    this.terminal(owner);
    if (owner === this.root) return 4;
    const fiber = this.global('vo_current_fiber');
    const frame = this.provider.view().getUint32(fiber + this.metadata.fiberFrame, true);
    const parent = frame ? this.provider.view().getUint32(frame - this.metadata.frameBytes + this.metadata.frameParent, true) : 0;
    // Pop nested frames without re-entering guest code, then let the normal
    // completion path remove the last Fiber record and run healthy peers.
    return parent ? 4 : 0;
  }
  private registerFrame(raw: number): void {
    const header = this.provider.findHeader(raw);
    if (header !== 0) {
      if (raw !== header + 32 || this.provider.view().getUint32(header + 12, true) !== this.metadata.frameDescriptor) return;
    } else if (raw !== this.metadata.stackBase) return;
    const span = this.provider.spanAtHeader(header);
    const owner = span ? this.owners[span.heap.island] : this.root;
    const capacity = span ? span.requested[span.index(raw)] : this.metadata.stackLimit - raw;
    if (capacity < this.metadata.frameBytes) { owner.heap.fail('InvalidPointer'); return; }
    const functionId = this.provider.view().getUint32(raw + this.metadata.frameFunction, true);
    const layout = this.metadata.frames[functionId];
    if (!layout || capacity < this.metadata.frameBytes + layout.slots.length * 8) {
      owner.heap.fail('InvalidPointer'); return;
    }
    const view = this.provider.view();
    if (!header) view.setUint32(raw - 20, this.metadata.frameDescriptor, true);
    if (!header && this.metadata.barrierPages !== undefined) {
      view.setUint32(this.metadata.barrierPages + (raw >>> 16) * 4, (raw - 32) | 1, true);
    }
    if (owner.registerFrame(raw, functionId, span)) this.publishFrame(raw, functionId, owner);
  }

  private publishFrame(raw: number, functionId: number, owner: IslandOwner): void {
    publishBarrierHint(this.provider.view(), raw - 32, this.frameHints[functionId]);
    owner.collector.rootWrite(raw);
  }
  releaseObject(header: number): void {
    // Frame identity retires with its heap cell; only non-frame registries remain.
    const fiber = header + 32 + this.metadata.frameBytes;
    const owner = this.fibers.get(fiber);
    if (owner) { owner.fiberRoots.delete(fiber); this.fibers.delete(fiber); this.runQueue.remove(fiber); }
    if (this.endpoints.delete(header + 32)) this.runQueue.forgetQueue(header + 32);
  }
  isEndpoint(pointer: number): boolean { return this.endpoints.has(pointer); }
  validateEndpoint(owner: IslandOwner, pointer: number, generation: number): boolean {
    const endpoint = this.endpoints.get(pointer);
    if (!endpoint || endpoint.generation !== generation || !this.provider.findHeader(pointer)) {
      owner.heap.fail('InvalidPointer'); return false;
    }
    return true;
  }
  retainEndpoint(borrower: IslandOwner, pointer: number): void {
    const endpoint = this.endpoints.get(pointer);
    if (!endpoint) { borrower.heap.fail('InvalidPointer'); return; }
    if (endpoint.owner === borrower) return;
    if (!borrower.borrowed.has(pointer)) {
      if (!this.admitLease(borrower)) return;
      if (!endpoint.owner.leases.has(pointer) && !this.canAdmitLease(endpoint.owner)) {
        borrower.heap.fail('MetadataExhausted'); return;
      }
    }
    borrower.borrowed.set(pointer, borrower.leaseEpoch);
    endpoint.peers.add(borrower);
    endpoint.owner.leases.add(pointer);
    endpoint.owner.collector.rootWrite(pointer);
  }
  private canAdmitLease(owner: IslandOwner): boolean {
    const count = owner.borrowed.size + owner.leases.size + owner.retainedCalls + owner.externalLeases.size;
    const capacity = owner.heap.policy.maxLeases ?? (owner.heap.policy.growthAllowed
      ? Number.MAX_SAFE_INTEGER : owner.heap.stats().committedBytes / 64);
    return count < capacity;
  }
  admitLease(owner: IslandOwner): boolean {
    if (!this.canAdmitLease(owner)) { owner.heap.fail('MetadataExhausted'); return false; }
    return true;
  }
  releaseEndpoint(borrower: IslandOwner, pointer: number): void {
    borrower.borrowed.delete(pointer);
    const endpoint = this.endpoints.get(pointer);
    endpoint?.peers.delete(borrower);
    if (endpoint?.peers.size === 0) {
      endpoint.owner.leases.delete(pointer);
      if (endpoint.owner.heap.error) {
        endpoint.owner.cleaned = false; endpoint.owner.cleanup = undefined;
        this.request(endpoint.owner);
      }
    }
  }
  write(address: number, bytes: number, transferCopy = false): void {
    if (!bytes) return;
    const header = this.provider.findHeader(address);
    const span = this.provider.spanAtHeader(header);
    const raw = header ? header + 32 : this.metadata.stackBase;
    const owner = span ? this.owners[span.heap.island] : this.root;
    const frame = owner.frameFunction(raw, span, span?.index(raw));
    const view = this.provider.view();
    for (let slot = Math.floor(address / 8) * 8; slot < address + bytes; slot += 8) {
      let kind: number;
      if (frame !== undefined) {
        const offset = slot - raw - this.metadata.frameBytes;
        kind = offset >= 0 ? this.metadata.frames[frame].slots[offset / 8] ?? 0 : 0;
        if (offset < 0) {
          for (const field of this.metadata.frameDefers) {
            if (Math.floor((raw + field) / 8) * 8 === slot) owner.collector.rootWrite(view.getUint32(raw + field, true));
          }
        }
      } else if (this.fibers.has(raw + this.metadata.frameBytes)) {
        const offset = slot - raw - this.metadata.frameBytes;
        kind = offset === this.metadata.fiberPanic ? 3 : offset === this.metadata.fiberPanic + 8 ? 4
          : offset === this.metadata.fiberPreviousPanic ? 2 : 0;

      } else kind = header ? owner.trace.slotKind(header, slot) : 0;
      const normalize = (value: bigint): number => {
        const transferring = this.scopes.length !== 0 || (transferCopy && (frame !== undefined
          || (header !== 0 && this.metadata.descriptors[view.getUint32(header + 12, true)].tag === 6)));
        // Ordinary stores use reference()'s ownership/generation validation.
        // Transfer staging alone needs the preliminary foreign-value filter.
        if (transferring) {
          const pointer = Number(BigInt.asUintN(32, value));
          const childHeader = this.provider.findHeader(pointer);
          const child = this.provider.spanAtHeader(childHeader);
          if (child && child.heap !== owner.heap && !this.isEndpoint(pointer)) return 0;
        }
        return owner.reference(value);
      };
      let reference = 0;
      if (kind === 1 || kind === 2) reference = normalize(view.getBigUint64(slot, true));
      // Interface stores publish the header first and the payload last.
      // Shade only the completed payload, never an intermediate header/data pair.
      else if (kind === 4 && view.getUint8(slot - 8) >= 14) reference = normalize(view.getBigUint64(slot, true));
      if (frame !== undefined) owner.collector.rootWrite(reference);
      else if (reference) {
        if (span) owner.collector.writeToAllocation(span, span.index(header), reference);
        else owner.collector.rootWrite(reference);
      }
    }
  }
  call(operation: number, a: number, b: number, c: number, _d: number): number {
    a >>>= 0; b >>>= 0; c >>>= 0;
    // Most memory operations already identify their owner by address. Resolve
    // the active Island only on paths that actually need its policy or error.
    switch (operation) {
      case -1: {
        const owner = this.current();
        const pointer = this.allocate(owner, a, b);
        if (pointer && b === this.metadata.frameDescriptor && c === 2) {
          publishBarrierHint(this.provider.view(), pointer - 32, this.fiberHint);
          const fiber = pointer + this.metadata.frameBytes;
          this.fibers.set(fiber, owner); owner.fiberRoots.add(fiber); owner.collector.rootWrite(pointer);
        }
        return pointer;
      }
      case -2: return this.provider.findHeader(a);
      case -3: return this.collect(this.current(), a);
      case -4: { const owner = this.current(); owner.collector.rootWrite(owner.reference(BigInt(a))); return 0; }
      case -5: {
        // Explicit destruction accepts an exact payload base. The directory
        // authenticates the corresponding header without an interior lookup.
        const header = a - 32;
        const span = this.provider.spanAtHeader(header);
        if (span) {
          const owner = this.owners[span.heap.island];
          const index = span.index(header);
          const bytes = span.identity[index] > owner.debtRetiredThrough
            ? span.requested[index] + 32 : 0;
          this.releaseObject(header);
          span.heap.release(span, index);
          // Retired frames repay only debt accrued after the last collection.
          // A surviving older frame must not cancel newer guest allocation debt.
          // Live frames and ordinary guest allocations still request GC.
          owner.debt = Math.max(0, owner.debt - bytes);
          if (owner.debt < (this.policy.debtBytes ?? 8 * 1024 * 1024)
            && owner.collector.phase === 'idle' && !owner.collector.requested
            && !owner.manualUnits && !owner.fullTarget && !owner.coarseHold && !owner.heap.error) {
            this.pending.delete(owner);
            this.setGlobal('vo_gc_debt', this.pending.size ? 8 * 1024 * 1024 : 0);
          }
          // Explicit frame destruction admits one page of reuse immediately.
          if (span.reclaiming) span.heap.reclaimOne(span);
        }
        return 0;
      }
      case -6: this.cloneMemo.clear(); return 1;
      case -7: {
        const owner = this.current();
        this.write(a, b, c === 1);
        if (owner.heap.error) throw owner.heap.observeFailure();
        return 0;
      }
      case -8: {
        const owner = this.current();
        const child = c === 1 ? this.root : this.makeOwner({
          ...this.policy, ...owner.heap.policy,
          automaticGC: owner.automaticGC, collectorMode: owner.collector.stats().mode
        });
        const state = this.allocate(child, a, b);
        if (state) { child.state = state; this.states.set(state, child); }
        return state;
      }
      case -9: {
        const previous = this.scope?.state ?? 0;
        if (a) { this.scopes.push(this.scope); this.scope = this.states.get(a); }
        else { if (this.scope?.heap.error) this.terminal(this.scope); this.scope = this.scopes.pop(); }
        return previous;
      }
      case -10: this.registerFrame(a); return 0;
      case -11: {
        const owner = this.current();
        if (!this.validateEndpoint(owner, a, b)) throw owner.heap.observeFailure();
        this.retainEndpoint(owner, a);
        if (owner.heap.error) throw owner.heap.observeFailure();
        if (this.endpoints.get(a)!.owner.heap.error) this.closeEndpoint(a);
        return a;
      }
      case -12: return this.status(a);
      case -13: return this.states.has(a + 32) ? a + 32 : this.cloneMemo.get(a) ?? 0;
      case -14:
        try { this.cloneMemo.set(a, b); return 1; }
        catch { this.current().heap.fail('MetadataExhausted'); return 0; }
      case -15: {
        if (a) this.runQueue.finish(a);
        if (b) return 0;
        const next = this.runQueue.next();
        return next === 0 && this.pending.size ? -1 : next;
      }
      case -16: return this.runQueue.previous(a);
      case -17:
        if (!this.runQueue.wait(this.global('vo_current_fiber'), a, b)) this.current().heap.fail('MetadataExhausted');
        return 0;
      case -18:
        if (b === 4) this.runQueue.close(a);
        else this.runQueue.notify(a, b, this.global('vo_current_fiber'));
        return 0;
      case -19: this.runQueue.wake(a); return 0;
      case -20: this.runQueue.park(this.global('vo_current_fiber')); return 0;
      case -21: this.runQueue.add(a); return 0;
      case -22: {
        const layout = this.metadata.frames[b];
        const owner = this.current();
        if (!layout || a < this.metadata.frameBytes + layout.slots.length * 8) {
          owner.heap.fail('InvalidPointer'); return 0;
        }
        const raw = this.allocate(owner, a, this.metadata.frameDescriptor, b);
        if (raw) {
          this.provider.view().setUint32(raw + this.metadata.frameFunction, b, true);
          // Both owner and function layout were authenticated before allocating.
          this.publishFrame(raw, b, owner);
        }
        return raw;
      }
      default: throw new Error(`unknown Island memory operation ${operation}`);
    }
  }
  parkHostFiber(): () => void {
    return this.runQueue.parkHost(this.global('vo_current_fiber'));
  }
  requestStep(units: bigint): boolean {
    const owner = this.current();
    if (owner.heap.error) return false;
    if (units === 0n) return true;
    const budget = Number(units > BigInt(Number.MAX_SAFE_INTEGER) ? BigInt(Number.MAX_SAFE_INTEGER) : units);
    owner.manualUnits = Math.max(owner.manualUnits, budget);
    this.request(owner); this.interruptQuantum();
    return true;
  }
  requestCollection(): boolean {
    const owner = this.current();
    if (owner.heap.error) return false;
    owner.fullTarget = owner.collector.stats().majorCycles
      + (owner.collector.phase !== 'idle' && owner.collector.isMajor ? 2 : 1);
    owner.collector.requestMajor(); this.request(owner); this.interruptQuantum();
    return true;
  }
  /** Slot order is the public runtime/mem.Stats ABI, shared with native VM. */
  publicStats(): readonly bigint[] {
    const owner = this.current();
    const heap = owner.heap.stats(); const gc = owner.collector.stats();
    const phase = { idle: 0, reset: 1, roots: 1, mark: 1, remark: 2, leases: 2, sweep: 3, reclaim: 4 }[gc.phase];
    return [heap.committedBytes, heap.committedBytes, heap.liveBytes, heap.youngBytes, heap.oldBytes,
    heap.largeBytes, heap.backingBytes, owner.externalBytes, 1, heap.freeBlocks, heap.fragmentationBytes,
    heap.fragmentationBytes, heap.allocationBytes, heap.allocationFailures,
    gc.majorCycles + gc.minorCycles + Number(gc.phase !== 'idle'), gc.minorCycles, gc.majorCycles,
    gc.workUnits + owner.terminalWork, owner.lastWork, owner.maxWork, gc.rememberedParents,
    Number(gc.dirtyRoots), gc.remarkRounds, heap.reclaimBlocks * 65536,
    owner.retainedCalls + owner.externalLeases.size + owner.borrowed.size + owner.leases.size,
    Number(owner.heap.policy.growthAllowed), Number(owner.heap.policy.allocationAllowed),
    owner.heap.policy.hardLimitBytes ?? 0, Number(owner.heap.policy.hardLimitBytes !== undefined),
    Number(gc.mode === 'incremental'), phase, Number(owner.automaticGC),
    this.provider.memory.buffer.byteLength / 65536, this.provider.maximumPages, 1].map(BigInt);
  }
  step(entry: () => number): number {
    this.running = true;
    try { return entry(); }
    catch (error) {
      const owner = error instanceof AotIslandMemoryError ? this.owners[error.island] : this.current();
      if (!owner?.heap.error) throw error;
      this.terminal(owner);
      // The next scheduler dispatch observes the sticky failure and removes
      // this owner's frames without retrying the failed instruction.
      return owner === this.root ? 4 : 17;
    } finally { this.running = false; }
  }
  /** Finish accepted requests and terminal cleanup after guest execution ends. */
  drainStep(): boolean {
    this.draining = true;
    const owner = this.pending.values().next().value;
    if (!owner) return false;
    this.collect(owner, 4096);
    return this.pending.size !== 0;
  }
  schedulerStats() { return this.runQueue.stats(); }
  stats() { return this.owners.map(owner => ({ island: owner.heap.island, ...owner.heap.stats(), collector: owner.collector.stats(), error: owner.heap.error?.kind })); }
}
