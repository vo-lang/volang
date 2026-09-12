import { AotHeapSpan, AotSpanHeap } from './aot_span_heap.js';

export type AotGcMode = 'incremental' | 'generational';
export type AotGcPhase = 'idle' | 'reset' | 'roots' | 'mark' | 'remark' | 'leases' | 'sweep' | 'reclaim';

/** One call advances at most one root/layout/physical-slot operation.
 * Zero is a non-reference; -1 is completion. Cursors remain owner-local.
 */
export interface AotTraceSource {
  beginCycle?(major: boolean): void;
  finishCycle?(major: boolean): void;
  nextLease?(): boolean;
  resetRoots(): void;
  nextRoot(): number;
  resetObject(header: number): void;
  nextObjectReference(): number;
  /** Runtime handles may retain roots independently of guest field layouts. */
  releaseObject(header: number): void;
}

export interface AotCollectorStats {
  readonly phase: AotGcPhase;
  readonly mode: AotGcMode;
  readonly workUnits: number;
  readonly majorCycles: number;
  readonly minorCycles: number;
  readonly rememberedParents: number;
  readonly lastStepUnits: number;
  readonly maxStepUnits: number;
  readonly remarkRounds: number;
  readonly dirtyRoots: boolean;
}

/** Precise, stable-address collection; every transition owns a resumable cursor. */
export class AotCollector {
  private state: AotGcPhase = 'idle';
  private selectedMode: AotGcMode;
  private epoch = 0;
  private major = true;
  private majorCycles = 0;
  private minorCycles = 0;
  private work = 0;
  private lastStep = 0;
  private maxStep = 0;
  private remarks = 0;
  private gray = new Uint32Array(1024);
  private grayCount = 0;
  private currentHeader = 0;
  private currentIdentity = 0;
  private currentHasYoung = false;
  private rootsDirty = false;
  private spans?: SetIterator<AotHeapSpan>;
  private span?: AotHeapSpan;
  private index = 0;
  private rememberedSpans?: SetIterator<AotHeapSpan>;
  private rememberedSpan?: AotHeapSpan;
  private rememberedIndex = 0;
  private readonly remembered = new Set<AotHeapSpan>();
  private rememberedCount = 0;
  private oldObjects = 0;

  constructor(readonly heap: AotSpanHeap, private readonly source: AotTraceSource, mode: AotGcMode = 'generational') {
    this.selectedMode = mode;
    heap.onRelease = (span, index) => this.retire(span, index);
    const reserve = heap.policy.initialReserveBytes ?? 0;
    const capacity = Math.min(heap.policy.maxObjects ?? Number.MAX_SAFE_INTEGER,
      Math.ceil(reserve / 65536) * 1024);
    if (!heap.policy.growthAllowed && capacity > this.gray.length) {
      try { this.gray = new Uint32Array(capacity); }
      catch { heap.fail('MetadataExhausted'); }
    }
  }

  get phase(): AotGcPhase { return this.state; }
  get isMajor(): boolean { return this.major; }
  get requested(): boolean { return this.majorRequested; }
  get allocationEpoch(): number { return this.state === 'idle' ? 0 : this.epoch; }
  get needsBarrier(): boolean {
    // Promotion initially remembers every old object. If all old parents are
    // already covered, their mutations require no additional card insertion.
    // An active collector still needs new-value shading in either mode.
    return this.state !== 'idle' || (this.selectedMode === 'generational' && this.oldObjects > this.rememberedCount);
  }

  stats(): AotCollectorStats {
    return {
      phase: this.state, mode: this.selectedMode, workUnits: this.work,
      majorCycles: this.majorCycles, minorCycles: this.minorCycles,
      rememberedParents: this.rememberedCount, lastStepUnits: this.lastStep, maxStepUnits: this.maxStep, remarkRounds: this.remarks, dirtyRoots: this.rootsDirty
    };
  }

  setMode(mode: AotGcMode): boolean {
    if (this.state !== 'idle') return false;
    this.selectedMode = mode;
    return true;
  }

  /** Admit work storage before an allocation can publish new identity. */
  admitObjects(capacity: number, preparing = false): boolean {
    if (capacity <= this.gray.length) return true;
    if (!preparing && !this.heap.policy.growthAllowed && this.heap.stats().committedBytes !== 0) {
      this.heap.fail('MetadataExhausted'); return false;
    }
    try {
      const next = new Uint32Array(Math.max(capacity, this.gray.length * 2));
      next.set(this.gray.subarray(0, this.grayCount));
      this.gray = next;
      return true;
    } catch { this.heap.fail('MetadataExhausted'); return false; }
  }

  /** Newly initialized objects are scanned before sweep can advance. */
  publish(reference: number): void {
    if (this.state !== 'idle' && this.state !== 'reset') this.shade(reference);
  }

  /** New-value shading applies during marking and sweep rescue alike. */
  rootWrite(reference: number): void {
    if (this.state === 'idle' || this.state === 'reset') return;
    this.shade(reference);
  }

  rootsChanged(): void {
    if (this.state !== 'idle') this.rootsDirty = true;
  }

  write(parent: number, reference: number): void {
    const provider = this.heap.provider;
    const parentHeader = provider.findHeader(parent);
    const parentSpan = provider.spanAtHeader(parentHeader);
    if (parentSpan) this.writeToAllocation(parentSpan, parentSpan.index(parentHeader), reference);
    else this.rootWrite(reference);
  }

  /** The caller resolved the destination within this synchronous store.
   * Do not retain this span/index across allocation, collection or host turns.
   * Identity stays in the heap; no secondary object registry is introduced.
   */
  writeToAllocation(parentSpan: AotHeapSpan, parentIndex: number, reference: number): void {
    if (parentSpan.heap !== this.heap || parentSpan.retired || parentSpan.reclaiming || !parentSpan.has(parentIndex)) {
      this.rootWrite(reference);
      return;
    }
    const provider = this.heap.provider;
    const childHeader = provider.findHeader(reference);
    const childSpan = provider.spanAtHeader(childHeader);
    if (!childSpan) return;
    if (childSpan.heap !== this.heap) { this.heap.fail('InvalidPointer'); return; }
    const childIndex = childSpan.index(childHeader);
    if (parentSpan.age[parentIndex] >= 2 && childSpan.age[childIndex] < 2) {
      this.remember(parentSpan, parentIndex);
    }
    if (this.state !== 'idle' && this.state !== 'reset' && (parentSpan.markEpoch[parentIndex] === this.epoch
      || (!this.major && parentSpan.age[parentIndex] >= 2))) {
      this.shadeAllocation(childHeader, childSpan, childIndex);
    }
  }

  private remember(span: AotHeapSpan, index: number): void {
    const word = index >>> 5;
    const mask = 1 << (index & 31);
    if ((span.rememberedBits[word] & mask) === 0) {
      span.rememberedBits[word] |= mask;
      span.rememberedCount += 1;
      this.remembered.add(span);
      this.rememberedCount += 1;
    }
  }

  private forget(span: AotHeapSpan, index: number): void {
    const word = index >>> 5;
    const mask = 1 << (index & 31);
    if ((span.rememberedBits[word] & mask) !== 0) {
      span.rememberedBits[word] &= ~mask;
      if (--span.rememberedCount === 0) this.remembered.delete(span);
      this.rememberedCount -= 1;
    }
  }

  private enqueue(header: number, span: AotHeapSpan, index: number): void {
    const word = index >>> 5;
    const mask = 1 << (index & 31);
    if ((span.queuedBits[word] & mask) !== 0
      || (this.currentHeader === header && this.currentIdentity === span.identity[index])) return;
    if (this.grayCount === this.gray.length) { this.heap.fail('MetadataExhausted'); return; }
    span.queuedBits[word] |= mask;
    span.grayPositions[index] = this.grayCount + 1;
    this.gray[this.grayCount++] = header;
  }

  private retire(span: AotHeapSpan, index: number): void {
    this.forget(span, index);
    if (span.age[index] >= 2) this.oldObjects -= 1;
    const position = span.grayPositions[index];
    if (position !== 0) {
      const last = this.gray[--this.grayCount];
      if (position - 1 !== this.grayCount) {
        this.gray[position - 1] = last;
        const moved = this.heap.provider.spanAtHeader(last)!;
        moved.grayPositions[moved.index(last)] = position;
      }
      span.grayPositions[index] = 0;
      span.queuedBits[index >>> 5] &= ~(1 << (index & 31));
    }
    if (span.header(index) === this.currentHeader) this.currentHeader = 0;
    if (span.live === 1) this.remembered.delete(span);
  }

  private shade(reference: number): void {
    const header = this.heap.provider.findHeader(reference);
    if (header === 0) return;
    const span = this.heap.provider.spanAtHeader(header)!;
    if (span.heap !== this.heap) { this.heap.fail('InvalidPointer'); return; }
    this.shadeAllocation(header, span, span.index(header));
  }

  /** Both callers authenticate owner and membership before reaching this path. */
  private shadeAllocation(header: number, span: AotHeapSpan, index: number): void {
    if (this.currentHeader && span.age[index] < 2) this.currentHasYoung = true;
    if ((!this.major && span.age[index] >= 2) || span.markEpoch[index] === this.epoch) return;
    span.markEpoch[index] = this.epoch;
    this.enqueue(header, span, index);
  }

  requestMajor(): void {
    if (this.state !== 'idle') {
      // An active minor completes before a separately requested major begins.
      this.majorRequested = true;
      return;
    }
    this.majorRequested = true;
  }
  private majorRequested = false;

  private begin(): void {
    // Avoid epoch reuse: reset is itself handled by an ordinary bounded major
    // cycle before a 32-bit epoch could wrap into an older allocation mark.
    if (this.epoch === 0xffff_ffff) {
      this.state = 'reset'; this.spans = this.heap.spans.values(); this.span = undefined; this.index = 0;
      return;
    }
    this.epoch += 1;
    this.major = this.selectedMode === 'incremental' || this.majorRequested || (this.minorCycles + this.majorCycles) % 8 === 0;
    this.majorRequested = false;
    this.rootsDirty = false;
    this.source.beginCycle?.(this.major);
    this.source.resetRoots();
    this.rememberedSpans = this.remembered.values();
    this.rememberedSpan = undefined;
    this.rememberedIndex = 0;
    this.state = 'roots';
  }

  /** N == 0 changes no collector state; the return is the exact work performed. */
  step(units: number): number {
    if (!Number.isSafeInteger(units) || units < 0) throw new RangeError('invalid collector work budget');
    if (units === 0 || this.heap.error) return 0;
    let used = 0;
    while (used < units && !this.heap.error) {
      used += 1;
      this.work += 1;
      if (this.state === 'idle') { this.begin(); continue; }
      if (this.state === 'reset') {
        if (!this.span) {
          this.span = this.spans!.next().value; this.index = 0;
          if (!this.span) { this.spans = undefined; this.epoch = 0; this.begin(); }
        } else if (this.index < this.span.slots) this.span.markEpoch[this.index++] = 0;
        else this.span = undefined;
        continue;
      }
      if (this.state === 'roots' || this.state === 'remark') {
        const reference = this.source.nextRoot();
        if (reference >= 0) { this.shade(reference); continue; }
        if (!this.major && this.nextRemembered()) continue;
        this.state = 'mark';
        continue;
      }
      if (this.state === 'mark') {
        if (this.currentHeader) {
          const reference = this.source.nextObjectReference();
          if (reference >= 0) { this.shade(reference); continue; }
          const span = this.heap.provider.spanAtHeader(this.currentHeader);
          if (span && span.heap === this.heap) {
            const index = span.index(this.currentHeader);
            if (span.age[index] >= 2) {
              if (this.currentHasYoung) this.remember(span, index);
              else this.forget(span, index);
            }
          }
          this.currentHeader = 0;
          continue;
        }
        if (this.grayCount !== 0) {
          const header = this.gray[--this.grayCount];
          const span = this.heap.provider.spanAtHeader(header);
          if (!span || span.heap !== this.heap) continue;
          const index = span.index(header);
          span.queuedBits[index >>> 5] &= ~(1 << (index & 31));
          span.grayPositions[index] = 0;
          this.currentHeader = header;
          this.currentIdentity = span.identity[index];
          this.currentHasYoung = false;
          this.source.resetObject(header);
          continue;
        }
        if (this.rootsDirty) {
          this.rootsDirty = false;
          this.source.resetRoots();
          this.remarks += 1;
          this.state = 'remark';
          continue;
        }
        if (!this.spans) {
          this.spans = this.heap.spans.values(); this.span = undefined; this.index = 0;
          this.state = 'leases';
        } else this.state = 'sweep';
        continue;
      }
      if (this.state === 'leases') {
        if (!this.source.nextLease?.()) this.state = 'sweep';
        continue;
      }
      if (this.state === 'sweep') {
        if (this.grayCount || this.rootsDirty) { this.state = 'mark'; continue; }
        if (!this.span) {
          this.span = this.spans!.next().value;
          this.index = 0;
          if (!this.span) { this.finish(); break; }
          continue;
        }
        const span = this.span;
        if (span.reclaiming) { this.state = 'reclaim'; continue; }
        if (span.retired || this.index === span.slots) { this.span = undefined; continue; }
        if (!this.major && span.young === 0) { this.span = undefined; continue; }
        // One bitmap word is bounded work even when every physical slot is old
        // or free. Live young cells retain their own sweep/promotion unit.
        const word = this.index >>> 5;
        const bits = (this.major ? span.allocationBits[word] : span.youngBits[word])
          & (0xffff_ffff << (this.index & 31));
        if (bits === 0) { this.index = Math.min(span.slots, (word + 1) * 32); continue; }
        const index = word * 32 + 31 - Math.clz32(bits & -bits);
        this.index = index + 1;
        if (span.markEpoch[index] !== this.epoch) {
          const header = span.header(index);
          this.source.releaseObject(header);
          this.forget(span, index);
          this.heap.release(span, index);
        } else if (span.age[index] < 2) {
          this.heap.promote(span, index);
          // Promotion itself establishes remembered membership. A subsequent
          // bounded scan retires it only after all young children disappear.
          if (span.age[index] === 2) { this.oldObjects += 1; this.remember(span, index); }
        }
        continue;
      }
      if (this.state === 'reclaim') {
        if (this.grayCount || this.rootsDirty) { this.state = 'mark'; continue; }
        if (!this.heap.reclaimOne(this.span!)) { this.span = undefined; this.state = 'sweep'; }
        else if (this.span!.retired) { this.span = undefined; this.state = 'sweep'; }
      }
    }
    this.lastStep = used; this.maxStep = Math.max(this.maxStep, used);
    return used;
  }

  private nextRemembered(): boolean {
    if (!this.rememberedSpan) {
      this.rememberedSpan = this.rememberedSpans!.next().value;
      this.rememberedIndex = 0;
      return this.rememberedSpan !== undefined;
    }
    const span = this.rememberedSpan;
    if (span.retired || span.rememberedCount === 0 || this.rememberedIndex === span.slots) {
      if (span.retired) this.remembered.delete(span);
      this.rememberedSpan = undefined;
      return true;
    }
    const word = this.rememberedIndex >>> 5;
    const bits = span.rememberedBits[word] & (0xffff_ffff << (this.rememberedIndex & 31));
    if (bits === 0) {
      this.rememberedIndex = Math.min(span.slots, (word + 1) * 32);
      return true;
    }
    const index = word * 32 + 31 - Math.clz32(bits & -bits);
    this.rememberedIndex = index + 1;
    this.enqueue(span.header(index), span, index);
    return true;
  }

  private finish(): void {
    this.source.finishCycle?.(this.major);
    if (this.major) this.majorCycles += 1; else this.minorCycles += 1;
    this.state = 'idle';
    this.spans = undefined;
    this.span = undefined;
    this.rememberedSpans = undefined;
    this.rememberedSpan = undefined;
    this.currentHeader = 0;
  }
}
