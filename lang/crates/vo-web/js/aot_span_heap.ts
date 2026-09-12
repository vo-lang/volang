/** Stable-address 64 KiB span storage for one generated-code Island.
 * Allocation membership and requested extents live in the block directory;
 * guest object headers never establish their own identity or logical range.
 */
export const AOT_BLOCK_BYTES = 65_536;
export const AOT_OBJECT_HEADER_BYTES = 32;
const MAX_SMALL_BYTES = 32_768;
const MIN_CELL_BYTES = 64;
const FREE_DESCRIPTOR = 0xffff_ffff;

export type AotMemoryErrorKind =
  | 'AllocationForbidden' | 'GrowthDisabled' | 'HardLimitExceeded'
  | 'MetadataExhausted' | 'SystemAllocationFailed' | 'InvalidPointer' | 'CollectorBusy';

export class AotIslandMemoryError extends Error {
  constructor(readonly island: number, readonly kind: AotMemoryErrorKind) {
    super(`Island ${island}: ${kind}`);
    this.name = 'AotIslandMemoryError';
  }
}

export interface AotHeapPolicy {
  readonly initialReserveBytes?: number;
  readonly hardLimitBytes?: number;
  readonly growthAllowed?: boolean;
  readonly allocationAllowed?: boolean;
  readonly maxObjects?: number;
  readonly maxLeases?: number;
  readonly oomPolicy?: 'collect-then-terminate-island' | 'terminate-island';
}

function natural(value: number, name: string, maximum = 0xffff_ffff): number {
  if (!Number.isSafeInteger(value) || value < 0 || value > maximum) {
    throw new RangeError(`invalid ${name}: ${value}`);
  }
  return value;
}

function trailingZeroes(word: number): number {
  return 31 - Math.clz32(word & -word);
}

/** Metadata belongs to the provider and never resides in guest object headers. */
export class AotHeapSpan {
  slots: number;
  readonly allocationBits: Uint32Array;
  readonly requested: Uint32Array;
  readonly markEpoch: Uint32Array;
  readonly age: Uint8Array;
  readonly youngBits: Uint32Array;
  young = 0;
  readonly identity: Float64Array;
  // Frame membership discriminates this union: a function ID for frames,
  // backing bytes for other cells. Frame backing is its exact extent + header.
  // Both are immutable until release.
  readonly cellData: Uint32Array;
  // Admitted with the rest of the span, including no-growth capacity.
  readonly frameBits: Uint32Array;
  frameWords = 0;
  private framesRegistered = false;
  readonly rememberedBits: Uint32Array;
  rememberedCount = 0;
  readonly queuedBits: Uint32Array;
  readonly grayPositions: Uint32Array;
  freeWords: number;
  live = 0;
  reclaimPage = 0;
  reclaiming = false;
  retired = false;

  constructor(
    readonly heap: AotSpanHeap,
    public firstPage: number,
    public pages: number,
    /** Zero denotes a large contiguous block run. */
    public cellBytes: number,
  ) {
    this.slots = cellBytes === 0 ? 1 : AOT_BLOCK_BYTES / cellBytes;
    this.allocationBits = new Uint32Array(Math.ceil(this.slots / 32));
    this.requested = new Uint32Array(this.slots);
    this.markEpoch = new Uint32Array(this.slots);
    this.age = new Uint8Array(this.slots);
    this.youngBits = new Uint32Array(this.allocationBits.length);
    this.identity = new Float64Array(this.slots);
    this.cellData = new Uint32Array(this.slots);
    this.frameBits = new Uint32Array(this.allocationBits.length);
    this.rememberedBits = new Uint32Array(this.allocationBits.length);
    this.queuedBits = new Uint32Array(this.allocationBits.length);
    this.grayPositions = new Uint32Array(this.slots);
    this.freeWords = this.allocationBits.length === 32
      ? 0xffff_ffff : (2 ** this.allocationBits.length - 1) >>> 0;
  }

  /** Reuse already admitted metadata after every old cell has been retired. */
  reset(pages: number, cellBytes: number): void {
    this.pages = pages; this.cellBytes = cellBytes;
    this.slots = cellBytes === 0 ? 1 : AOT_BLOCK_BYTES / cellBytes;
    if (this.slots > this.requested.length) throw new Error('span metadata capacity was not admitted');
    this.allocationBits.fill(0); this.requested.fill(0); this.markEpoch.fill(0);
    this.age.fill(0); this.youngBits.fill(0); this.young = 0;
    this.identity.fill(0); this.cellData.fill(0); this.frameBits.fill(0); this.frameWords = 0;
    this.framesRegistered = false;
    this.rememberedBits.fill(0); this.queuedBits.fill(0); this.grayPositions.fill(0);
    this.rememberedCount = 0;
    const words = Math.ceil(this.slots / 32);
    this.freeWords = words === 32 ? 0xffff_ffff : (2 ** words - 1) >>> 0;
    this.live = 0; this.reclaimPage = 0; this.reclaiming = false; this.retired = false;
  }

  header(index: number): number {
    return this.firstPage * AOT_BLOCK_BYTES + index * this.cellBytes;
  }

  index(pointer: number): number {
    return this.cellBytes === 0 ? 0
      : Math.floor((pointer - this.firstPage * AOT_BLOCK_BYTES) / this.cellBytes);
  }

  has(index: number): boolean {
    return index >= 0 && index < this.slots
      && (this.allocationBits[index >>> 5] & (1 << (index & 31))) !== 0;
  }

  take(requested: number, epoch: number): number {
    if (this.retired || this.reclaiming || this.live === this.slots) return -1;
    const word = trailingZeroes(this.freeWords);
    const available = ~this.allocationBits[word] >>> 0;
    const bit = trailingZeroes(available);
    const index = word * 32 + bit;
    this.allocationBits[word] |= 1 << bit;
    this.youngBits[word] |= 1 << bit; this.young += 1;
    if ((this.allocationBits[word] >>> 0) === 0xffff_ffff || index + 1 === this.slots) {
      this.freeWords &= ~(1 << word);
    }
    this.identity[index] = this.heap.nextIdentity();
    this.requested[index] = requested;
    this.markEpoch[index] = epoch;
    this.age[index] = 0;
    this.live += 1;
    return index;
  }

  isFrame(index: number): boolean {
    return (this.frameBits[index >>> 5] & (1 << (index & 31))) !== 0;
  }

  frameFunction(index: number): number | undefined {
    return this.isFrame(index) ? this.cellData[index] : undefined;
  }

  registerFrame(index: number, functionId: number): boolean {
    // Runtime frames account their complete exact extent as backing. The
    // descriptor owner validates Frame before registering this derived form.
    if (!this.has(index) || (!this.isFrame(index) && this.cellData[index] !== this.requested[index] + AOT_OBJECT_HEADER_BYTES)) {
      this.heap.fail('InvalidPointer'); return false;
    }
    const word = index >>> 5;
    this.cellData[index] = functionId;
    this.frameBits[word] |= 1 << (index & 31);
    this.frameWords |= 1 << word;
    if (!this.framesRegistered) {
      this.heap.frameSpans.add(this); this.framesRegistered = true;
    }
    return true;
  }

  release(index: number): void {
    if (!this.has(index)) throw new Error('duplicate span-cell release');
    const word = index >>> 5;
    const mask = 1 << (index & 31);
    if (this.age[index] < 2) this.young -= 1;
    this.youngBits[word] &= ~mask;
    this.allocationBits[word] &= ~mask;
    this.rememberedBits[word] &= ~mask;
    this.queuedBits[word] &= ~mask;
    this.cellData[index] = 0;
    if ((this.frameBits[word] & mask) !== 0) {
      this.frameBits[word] &= ~mask;
      if (this.frameBits[word] === 0) this.frameWords &= ~(1 << word);
    }
    this.freeWords |= 1 << word;
    this.requested[index] = 0;
    this.markEpoch[index] = 0;
    this.age[index] = 0;
    this.live -= 1;
  }
}

/** The instance supplies physical pages; ownership and admission remain Island-local. */
export class AotHeapProvider {
  readonly directory: (AotHeapSpan | undefined)[];
  private readonly reusable: Uint8Array;
  private reusableCount = 0;
  private buffer: ArrayBufferLike;
  private cachedView: DataView;
  private cachedBytes: Uint8Array;
  private readonly shared: boolean;

  constructor(readonly memory: WebAssembly.Memory, readonly maximumPages: number, private readonly barrierPages?: number) {
    natural(maximumPages, 'maximum pages', 65_536);
    if (maximumPages < memory.buffer.byteLength / AOT_BLOCK_BYTES) {
      throw new RangeError('maximum pages precede the admitted image');
    }
    this.directory = new Array(maximumPages);
    this.reusable = new Uint8Array(maximumPages);
    this.buffer = memory.buffer;
    this.shared = typeof SharedArrayBuffer !== 'undefined' && this.buffer instanceof SharedArrayBuffer;
    this.cachedView = new DataView(this.buffer);
    this.cachedBytes = new Uint8Array(this.buffer);
  }

  view(): DataView {
    // Unshared Wasm growth detaches the previous buffer, including grow(0).
    // Shared memory retains the old view and needs the explicit identity check.
    if (this.buffer.byteLength === 0 || (this.shared && this.buffer !== this.memory.buffer)) {
      this.buffer = this.memory.buffer;
      this.cachedView = new DataView(this.buffer);
      this.cachedBytes = new Uint8Array(this.buffer);
    }
    return this.cachedView;
  }

  zero(address: number, bytes: number): void {
    this.view();
    this.cachedBytes.fill(0, address, address + bytes);
  }

  /** An exact base/interior lookup; free cells and class padding never match. */
  findHeader(pointer: number): number {
    if (!Number.isInteger(pointer) || pointer <= 0 || pointer > 0xffff_ffff) return 0;
    const span = this.directory[Math.floor(pointer / AOT_BLOCK_BYTES)];
    if (!span || span.retired || span.reclaiming) return 0;
    const index = span.index(pointer);
    if (!span.has(index)) return 0;
    const header = span.header(index);
    const data = header + AOT_OBJECT_HEADER_BYTES;
    return pointer === data || (pointer > data && pointer - data < span.requested[index]) ? header : 0;
  }

  spanAtHeader(header: number): AotHeapSpan | undefined {
    const span = this.directory[Math.floor(header / AOT_BLOCK_BYTES)];
    if (!span || span.retired || span.reclaiming) return undefined;
    const index = span.index(header);
    return span.has(index) && span.header(index) === header ? span : undefined;
  }

  grow(heap: AotSpanHeap, pages: number): number | undefined {
    if (pages <= this.reusableCount) {
      let run = 0;
      for (let page = 0; page < this.maximumPages; page += 1) {
        run = this.reusable[page] ? run + 1 : 0;
        if (run === pages) {
          const first = page + 1 - pages;
          this.reusable.fill(0, first, page + 1);
          this.reusableCount -= pages;
          return first;
        }
      }
    }
    const current = this.memory.buffer.byteLength / AOT_BLOCK_BYTES;
    if (pages > this.maximumPages - current) {
      heap.fail('SystemAllocationFailed');
      return undefined;
    }
    try {
      return this.memory.grow(pages);
    } catch {
      heap.fail('SystemAllocationFailed');
      return undefined;
    }
  }

  publishPage(page: number, span?: AotHeapSpan): void {
    this.directory[page] = span;
    if (this.barrierPages !== undefined) {
      // Even entries are small-span cell sizes; odd entries hold a large-run
      // header. The host directory remains the authority for identity/extents.
      const hint = span ? span.cellBytes || (span.firstPage * AOT_BLOCK_BYTES + 1) : 0;
      this.view().setUint32(this.barrierPages + page * 4, hint, true);
    }
  }

  releasePage(page: number): void {
    if (this.directory[page] || this.reusable[page]) throw new Error('page remains owned or was returned twice');
    this.reusable[page] = 1;
    this.reusableCount += 1;
  }
}

export interface AotHeapStats {
  readonly committedBytes: number;
  readonly liveBytes: number;
  readonly objects: number;
  readonly freeBlocks: number;
  readonly allocationBytes: number;
  readonly allocationFailures: number;
  readonly reclaimBlocks: number;
  readonly oldBytes: number;
  readonly youngBytes: number;
  readonly largeBytes: number;
  readonly backingBytes: number;
  readonly fragmentationBytes: number;
}

/** Size-class cells plus reusable contiguous block runs, with sticky admission errors. */
export class AotSpanHeap {
  /** Collector bookkeeping is retired before an explicitly freed cell can be reused. */
  onRelease?: (span: AotHeapSpan, index: number) => void;
  onFailure?: () => void;
  readonly spans = new Set<AotHeapSpan>();
  // Retain membership until span retirement, amortizing single-frame calls.
  readonly frameSpans = new Set<AotHeapSpan>();

  *frames(): Generator<number> {
    for (const span of this.frameSpans) {
      yield 0; // Empty spans also consume one bounded root-scan unit.
      let words = span.frameWords;
      while (words && !span.retired && !span.reclaiming) {
        const word = trailingZeroes(words); words &= words - 1;
        let bits = span.frameBits[word];
        while (bits) {
          const bit = trailingZeroes(bits); bits &= bits - 1;
          const index = word * 32 + bit;
          // Release/reuse can occur between yields. New roots are separately
          // published through the collector root-write barrier.
          yield span.has(index) && span.isFrame(index)
            ? span.header(index) + AOT_OBJECT_HEADER_BYTES : 0;
        }
      }
    }
  }
  policy: Required<Pick<AotHeapPolicy, 'growthAllowed' | 'allocationAllowed'>> & AotHeapPolicy;
  private readonly partial = new Map<number, Set<AotHeapSpan>>();
  private readonly allocationSpans: (AotHeapSpan | undefined)[] = new Array(17);
  private readonly freePages = new Set<number>();
  private readonly admittedSpans: AotHeapSpan[] = [];
  // One retired metadata record per size class amortizes short-lived frame
  // bursts without retaining managed pages or extending object lifetimes.
  private readonly recycledSpans = new Map<number, AotHeapSpan>();
  private committed = 0;
  private live = 0;
  private count = 0;
  private allocated = 0;
  private failures = 0;
  private reclaim = 0;
  private old = 0;
  private large = 0;
  private backing = 0;
  private pending?: AotIslandMemoryError;
  private terminal?: AotIslandMemoryError;
  private terminated = false;
  private identity = 0;
  get allocationIdentity(): number { return this.identity; }
  nextIdentity(): number {
    if (this.identity === Number.MAX_SAFE_INTEGER) throw new Error("allocation identity exhausted");
    return ++this.identity;
  }

  constructor(readonly provider: AotHeapProvider, readonly island: number, policy: AotHeapPolicy = {}) {
    if (policy.hardLimitBytes !== undefined) natural(policy.hardLimitBytes, 'hard limit', 2 ** 32);
    if (policy.maxObjects !== undefined) natural(policy.maxObjects, 'object limit');
    if (policy.maxLeases !== undefined) natural(policy.maxLeases, 'lease limit');
    if (policy.oomPolicy !== undefined && !['collect-then-terminate-island', 'terminate-island'].includes(policy.oomPolicy)) throw new RangeError('invalid OOM policy');
    const reserve = natural(policy.initialReserveBytes ?? 0, 'initial reserve', 2 ** 32);
    this.policy = { ...policy, growthAllowed: policy.growthAllowed ?? true, allocationAllowed: policy.allocationAllowed ?? true };
    // Construction reserve is admitted even when subsequent growth is denied.
    if (reserve !== 0) this.reserve(reserve);

  }

  child(island: number): AotSpanHeap {
    return new AotSpanHeap(this.provider, island, this.policy);
  }

  get error(): AotIslandMemoryError | undefined { return this.terminal ?? this.pending; }
  get objectCount(): number { return this.count; }

  setHardLimit(bytes: number | undefined): boolean {
    if (bytes !== undefined) natural(bytes, 'hard limit', 2 ** 32);
    if (this.error || (bytes !== undefined && bytes < this.committed)) return false;
    this.policy = { ...this.policy, hardLimitBytes: bytes }; return true;
  }
  setAllocationAllowed(allowed: boolean): void {
    this.policy = { ...this.policy, allocationAllowed: allowed };
  }
  setGrowthAllowed(allowed: boolean): boolean {
    if (this.error) return false;
    if (!allowed && this.policy.growthAllowed) {
      try {
        const bound = this.committed / AOT_BLOCK_BYTES;
        while (this.admittedSpans.length < bound) this.admittedSpans.push(new AotHeapSpan(this, 0, 1, MIN_CELL_BYTES));
      } catch { this.fail('MetadataExhausted'); return false; }
    }
    this.policy = { ...this.policy, growthAllowed: allowed }; return true;
  }

  fail(kind: AotMemoryErrorKind): void {
    this.failures += 1;
    if (!this.pending) {
      this.pending = new AotIslandMemoryError(this.island, kind);
      this.onFailure?.();
    }
  }

  /** Only the scheduler promotes an allocation failure to the terminal state. */
  observeFailure(): AotIslandMemoryError | undefined {
    this.terminal ??= this.pending;
    return this.terminal;
  }

  stats(): AotHeapStats {
    return {
      committedBytes: this.committed, liveBytes: this.live, objects: this.count,
      freeBlocks: this.freePages.size, allocationBytes: this.allocated,
      allocationFailures: this.failures, reclaimBlocks: this.reclaim,
      oldBytes: this.old, youngBytes: this.live - this.old, largeBytes: this.large, backingBytes: this.backing,
      fragmentationBytes: this.committed - this.freePages.size * AOT_BLOCK_BYTES - this.live
    };
  }

  reserve(bytes: number): boolean {
    natural(bytes, 'reserve bytes', 2 ** 32);
    if (this.error || this.terminated) return false;
    const pages = Math.ceil(bytes / AOT_BLOCK_BYTES);
    if (pages === 0) return true;
    if (this.policy.hardLimitBytes !== undefined
      && pages * AOT_BLOCK_BYTES > this.policy.hardLimitBytes - this.committed) {
      this.fail('HardLimitExceeded');
      return false;
    }
    if (!this.policy.growthAllowed) {
      try {
        for (let page = 0; page < pages; page += 1) this.admittedSpans.push(new AotHeapSpan(this, 0, 1, MIN_CELL_BYTES));
      } catch { this.fail('MetadataExhausted'); return false; }
    }
    const first = this.provider.grow(this, pages);
    if (first === undefined) return false;
    this.committed += pages * AOT_BLOCK_BYTES;
    for (let page = first; page < first + pages; page += 1) this.freePages.add(page);
    return true;
  }

  private takePages(pages: number): number | undefined {
    // Small spans are the allocation hot path and consume one free block directly.
    if (pages === 1 && this.freePages.size !== 0) {
      const page = this.freePages.values().next().value as number;
      this.freePages.delete(page);
      return page;
    }
    if (pages <= this.freePages.size) {
      // Large-run search is limited by the wasm32 page directory. Reclaim uses
      // O(1) insertion and never pays for sorting or merging this capacity.
      let run = 0;
      for (let page = 0; page < this.provider.maximumPages; page += 1) {
        run = this.freePages.has(page) ? run + 1 : 0;
        if (run === pages) {
          const first = page + 1 - pages;
          for (let i = first; i <= page; i += 1) this.freePages.delete(i);
          return first;
        }
      }
    }
    if (!this.policy.growthAllowed) { this.fail('GrowthDisabled'); return undefined; }
    if (this.policy.hardLimitBytes !== undefined
      && pages * AOT_BLOCK_BYTES > this.policy.hardLimitBytes - this.committed) {
      this.fail('HardLimitExceeded'); return undefined;
    }
    const first = this.provider.grow(this, pages);
    if (first === undefined) return undefined;
    this.committed += pages * AOT_BLOCK_BYTES;
    return first;
  }

  /** Terminal cleanup transfers exactly one free block to the shared provider. */
  returnFreePage(): boolean {
    const page = this.freePages.values().next().value;
    if (page === undefined) return false;
    this.freePages.delete(page);
    this.provider.releasePage(page);
    this.committed -= AOT_BLOCK_BYTES;
    return true;
  }

  allocate(bytes: number, descriptor: number, epoch = 0, backingBytes = 0, frameFunction?: number): number {
    if (this.error || this.terminated) return 0;
    if (frameFunction !== undefined && backingBytes !== bytes + AOT_OBJECT_HEADER_BYTES) { this.fail('InvalidPointer'); return 0; }
    if (this.identity === Number.MAX_SAFE_INTEGER) { this.fail('MetadataExhausted'); return 0; }
    if (!this.policy.allocationAllowed) { this.fail('AllocationForbidden'); return 0; }
    if (!Number.isInteger(bytes) || bytes < 0 || bytes > 0xffff_ffff - AOT_OBJECT_HEADER_BYTES) {
      this.fail('SystemAllocationFailed'); return 0;
    }
    if (this.policy.maxObjects !== undefined && this.count >= this.policy.maxObjects) {
      this.fail('MetadataExhausted'); return 0;
    }
    const size = Math.max(bytes + AOT_OBJECT_HEADER_BYTES, MIN_CELL_BYTES);
    const sizeClass = size <= MAX_SMALL_BYTES ? 32 - Math.clz32(size - 1) : 0;
    const cell = sizeClass === 0 ? 0 : 1 << sizeClass;
    let span = this.allocationSpans[sizeClass];
    if (!span && cell !== 0) span = this.partial.get(cell)?.values().next().value;
    if (!span) {
      const pages = cell === 0 ? Math.ceil(size / AOT_BLOCK_BYTES) : 1;
      // All metadata is allocated before publishing managed capacity or identity.
      let metadata: AotHeapSpan;
      try {
        if (!this.policy.growthAllowed) {
          const admitted = this.admittedSpans.pop();
          if (!admitted) { this.fail('GrowthDisabled'); return 0; }
          metadata = admitted; metadata.reset(pages, cell);
        } else {
          const recycled = this.recycledSpans.get(cell);
          if (recycled) {
            this.recycledSpans.delete(cell); recycled.reset(pages, cell); metadata = recycled;
          } else metadata = new AotHeapSpan(this, 0, pages, cell);
        }
      }
      catch { this.fail('MetadataExhausted'); return 0; }
      const first = this.takePages(pages);
      if (first === undefined) {
        if (!this.policy.growthAllowed) this.admittedSpans.push(metadata);
        else this.recycledSpans.set(cell, metadata);
        return 0;
      }
      // The page base is assigned exactly once, before directory publication.
      metadata.firstPage = first;
      span = metadata;
      this.spans.add(span);
      for (let page = first; page < first + pages; page += 1) this.provider.publishPage(page, span);
      if (cell !== 0) {
        let candidates = this.partial.get(cell);
        if (!candidates) { candidates = new Set(); this.partial.set(cell, candidates); }
        candidates.add(span);
      }
    }
    const index = span.take(bytes, epoch);
    if (index < 0) throw new Error('full span remained in the partial list');
    if (cell !== 0) {
      if (span.live === span.slots) {
        this.partial.get(cell)!.delete(span);
        this.allocationSpans[sizeClass] = undefined;
      } else this.allocationSpans[sizeClass] = span;
    }
    const header = span.header(index);
    const data = header + AOT_OBJECT_HEADER_BYTES;
    this.provider.zero(header, AOT_OBJECT_HEADER_BYTES + bytes);
    const view = this.provider.view();
    view.setUint32(header, (cell || span.pages * AOT_BLOCK_BYTES) - AOT_OBJECT_HEADER_BYTES, true);
    view.setUint32(header + 4, 1, true);
    view.setUint32(header + 12, descriptor, true);
    view.setUint32(header + 16, bytes, true);
    this.live += bytes + AOT_OBJECT_HEADER_BYTES;
    if (span.cellBytes === 0) this.large += bytes + AOT_OBJECT_HEADER_BYTES;
    this.allocated += bytes + AOT_OBJECT_HEADER_BYTES;
    span.cellData[index] = backingBytes; this.backing += backingBytes;
    this.count += 1;
    if (frameFunction !== undefined && !span.registerFrame(index, frameFunction)) return 0;
    return data;
  }

  promote(span: AotHeapSpan, index: number): void {
    if (span.age[index] === 1) this.old += span.requested[index] + AOT_OBJECT_HEADER_BYTES;
    span.age[index] += 1;
    if (span.age[index] === 2) {
      span.youngBits[index >>> 5] &= ~(1 << (index & 31)); span.young -= 1;
    }
  }
  /** One cell is one sweep operation; large runs enter a separate reclaim phase. */
  release(span: AotHeapSpan, index: number): void {
    if (span.heap !== this || span.retired || !span.has(index)) throw new Error('foreign or dead allocation');
    this.onRelease?.(span, index);
    const bytes = span.requested[index] + AOT_OBJECT_HEADER_BYTES;
    this.live -= bytes;
    if (span.age[index] >= 2) this.old -= bytes;
    if (span.cellBytes === 0) this.large -= bytes;
    this.backing -= span.isFrame(index) ? bytes : span.cellData[index];
    this.count -= 1;
    const header = span.header(index);
    const wasFull = span.live === span.slots;
    span.release(index);
    const view = this.provider.view();
    view.setUint32(header + 4, 0, true);
    view.setUint32(header + 12, FREE_DESCRIPTOR, true);
    if (span.live === 0) {
      this.partial.get(span.cellBytes)?.delete(span);
      const sizeClass = span.cellBytes === 0 ? 0 : 31 - Math.clz32(span.cellBytes);
      if (this.allocationSpans[sizeClass] === span) this.allocationSpans[sizeClass] = undefined;
      span.reclaiming = true;
      this.reclaim += span.pages;
    } else if (wasFull) {
      this.partial.get(span.cellBytes)?.add(span);
    }
  }

  /** Exactly one physical block is published as free capacity per call. */
  reclaimOne(span: AotHeapSpan): boolean {
    if (span.heap !== this || !span.reclaiming || span.retired) return false;
    const page = span.firstPage + span.reclaimPage;
    this.provider.publishPage(page);
    this.freePages.add(page);
    span.reclaimPage += 1;
    this.reclaim -= 1;
    if (span.reclaimPage === span.pages) {
      span.retired = true;
      span.reclaiming = false;
      this.spans.delete(span);
      this.frameSpans.delete(span);
      if (!this.policy.growthAllowed && span.requested.length === AOT_BLOCK_BYTES / MIN_CELL_BYTES
        && this.admittedSpans.length < this.committed / AOT_BLOCK_BYTES) this.admittedSpans.push(span);
      else if (this.policy.growthAllowed && !this.recycledSpans.has(span.cellBytes)) this.recycledSpans.set(span.cellBytes, span);
    }
    return true;
  }
}
