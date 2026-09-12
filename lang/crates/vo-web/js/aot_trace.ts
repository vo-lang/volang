import type { AotTraceSource } from './aot_collector.js';
import { AotHeapSpan, AotSpanHeap, AOT_OBJECT_HEADER_BYTES } from './aot_span_heap.js';

export interface AotTraceLayout {
  readonly slots: readonly number[];
  /** Encoded byte offset, with bit zero denoting a conditional interface pair. */
  readonly roots: readonly number[];
}
export interface AotAllocationLayout {
  readonly tag: number;
  readonly stride: number;
  readonly first: AotTraceLayout;
  readonly second: AotTraceLayout;
  readonly entries: AotTraceLayout;
}
export interface AotMemoryMetadata {
  readonly version: number;
  readonly frameDescriptor: number;
  readonly islandDescriptor: number;
  readonly stackBase: number;
  readonly stackLimit: number;
  readonly barrierPages: number;
  readonly frameBytes: number;
  readonly frameFunction: number;
  readonly frameParent: number;
  readonly frameDefers: readonly number[];
  readonly fiberBytes: number;
  readonly fiberNext: number;
  readonly fiberFrame: number;
  readonly fiberIsland: number;
  readonly fiberPanicGeneration: number;
  readonly fiberPanic: number;
  readonly fiberPreviousPanic: number;
  readonly descriptors: readonly AotAllocationLayout[];
  readonly frames: readonly AotTraceLayout[];
}

function layout(value: unknown): AotTraceLayout {
  if (!Array.isArray(value) || value.length > 1_000_000
    || value.some(slot => !Number.isInteger(slot) || slot < 0 || slot > 5)) {
    throw new Error('invalid precise memory layout');
  }
  const roots: number[] = [];
  for (let slot = 0; slot < value.length; slot += 1) {
    if (value[slot] === 1 || value[slot] === 2) roots.push(slot * 8);
    if (value[slot] === 3) {
      if (value[slot + 1] !== 4) throw new Error('unpaired interface root');
      roots.push(slot * 8 + 1);
      slot += 1;
    } else if (value[slot] === 4) throw new Error('unpaired interface payload');
  }
  return { slots: value, roots };
}

export function parseAotMemoryMetadata(module: WebAssembly.Module): AotMemoryMetadata {
  const sections = WebAssembly.Module.customSections(module, 'volang.memory.v1');
  if (sections.length !== 1) throw new Error('missing precise Island memory metadata');
  const raw = JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(sections[0]));
  if (!raw || raw.version !== 1 || !Array.isArray(raw.descriptors)
    || !Array.isArray(raw.frames) || raw.descriptors.length > 1_000_000
    || raw.frames.length > 1_000_000) throw new Error('invalid Island memory metadata');
  for (const key of ['frameDescriptor', 'islandDescriptor', 'stackBase', 'stackLimit', 'barrierPages',
    'frameBytes', 'frameFunction', 'frameParent', 'fiberBytes', 'fiberNext', 'fiberFrame',
    'fiberIsland', 'fiberPanicGeneration', 'fiberPanic', 'fiberPreviousPanic']) {
    if (!Number.isInteger(raw[key]) || raw[key] < 0 || raw[key] > 0xffff_ffff) {
      throw new Error(`invalid Island memory field ${key}`);
    }
  }
  if (!Array.isArray(raw.frameDefers) || raw.frameDefers.length !== 2
    || raw.frameDefers.some((offset: unknown) => !Number.isInteger(offset)
      || (offset as number) < 0 || (offset as number) + 4 > raw.frameBytes)) {
    throw new Error('invalid defer root offsets');
  }
  const descriptors = raw.descriptors.map((record: unknown) => {
    if (!Array.isArray(record) || record.length !== 4 || !Number.isInteger(record[0])
      || record[0] < 0 || record[0] > 6 || !Number.isInteger(record[1])
      || record[1] < 0 || record[1] > 0xffff_ffff) throw new Error('invalid allocation layout');
    const first = layout(record[2]);
    const second = layout(record[3]);
    return {
      tag: record[0], stride: record[1], first, second,
      entries: layout([...first.slots, ...second.slots])
    };
  });
  if (descriptors[raw.frameDescriptor]?.tag !== 1
    || descriptors[raw.islandDescriptor]?.tag !== 2) throw new Error('invalid runtime-owned descriptors');
  if (raw.barrierPages % 4 || raw.barrierPages + 65536 * 4 + 32 > raw.stackBase) throw new Error('invalid barrier page map');
  if (raw.stackBase % 8 || raw.stackLimit < raw.stackBase + raw.frameBytes
    || raw.frameFunction + 4 > raw.frameBytes || raw.frameParent + 4 > raw.frameBytes
    || raw.frameBytes % 8 || raw.fiberBytes % 8
    || [raw.fiberNext, raw.fiberFrame, raw.fiberIsland, raw.fiberPanicGeneration,
    raw.fiberPreviousPanic].some(offset => offset % 8 || offset + 8 > raw.fiberBytes)
    || raw.fiberPanic % 8 || raw.fiberPanic + 16 > raw.fiberBytes) throw new Error('invalid memory frame bounds');
  return { ...raw, descriptors, frames: raw.frames.map(layout) };
}

export interface AotTraceOwner {
  readonly heap: AotSpanHeap;
  readonly state: number;
  /** Validate runtime handles and establish owner-local lease reachability. */
  reference(value: bigint): number;
  referenceWord?(pointer: number, generation: number): number;
  beginCycle?(major: boolean): void;
  finishCycle?(major: boolean): void;
  nextLease?(): boolean;
  frames(): Iterable<number>;
  frameFunction(raw: number, span?: AotHeapSpan, index?: number): number | undefined;
  fibers(): Iterable<number>;
  extraRoots(): Iterable<number>;
  releaseObject(header: number): void;
}

/** Immutable flattened layouts plus one root cursor and one object cursor. */
export class AotTraceCursor implements AotTraceSource {
  private roots?: Generator<number>;
  private objectSpan?: AotHeapSpan;
  private objectIndex = 0;
  private objectIdentity = 0;
  private objectData = 0;
  private objectLayout?: AotAllocationLayout;
  private objectPhase = 0;
  private entry = 0;
  private entryCount = 0;
  private entryBase = 0;
  private entryStride = 0;
  private entryHead = 0;
  private entryCapacity = 0;
  private layoutRoot = 0;
  private backingSpan?: AotHeapSpan;
  private backingIndex = 0;
  private backingIdentity = 0;
  constructor(private readonly metadata: AotMemoryMetadata, private readonly owner: AotTraceOwner) { }
  beginCycle(major: boolean): void { this.owner.beginCycle?.(major); }
  finishCycle(major: boolean): void { this.owner.finishCycle?.(major); }
  nextLease(): boolean { return this.owner.nextLease?.() ?? false; }
  resetRoots(): void { this.roots = this.scanRoots(); }
  nextRoot(): number { const next = this.roots!.next(); return next.done ? -1 : next.value; }
  resetObject(header: number): void {
    const span = this.owner.heap.provider.spanAtHeader(header);
    this.objectSpan = span;
    this.objectIndex = span?.index(header) ?? 0;
    this.objectIdentity = span?.identity[this.objectIndex] ?? 0;
    this.objectData = header + AOT_OBJECT_HEADER_BYTES;
    this.objectLayout = span ? this.metadata.descriptors[this.pointer(header + 12)] : undefined;
    this.objectPhase = 0;
    this.entry = 0; this.layoutRoot = 0;
    this.backingSpan = undefined;
    if (span && !this.objectLayout) this.owner.heap.fail('InvalidPointer');
  }
  nextObjectReference(): number {
    const span = this.objectSpan;
    if (!span || span.heap !== this.owner.heap || !span.has(this.objectIndex)
      || span.identity[this.objectIndex] !== this.objectIdentity) return -1;
    const descriptor = this.objectLayout;
    if (!descriptor) return -1;
    const data = this.objectData;
    const tag = descriptor.tag;
    if (this.objectPhase === 0) {
      this.objectPhase = 1;
      switch (tag) {
        case 0: case 1: return -1;
        case 2:
          if (!this.beginEntries(data, 1, descriptor.first.slots.length * 8)) return -1;
          break;
        case 3:
          if (!this.range(data, 32)) return -1;
          return this.pointer(data + 16) === 0 ? 0 : this.root(data);
        case 4:
          this.objectPhase = 3;
          return this.range(data, 40) ? this.root(data + 32) : -1;
        case 5:
          if (!this.range(data, 16)) return -1;
          return this.root(data);
        case 6:
          if (!this.range(data, 120)) return -1;
          return this.root(data + 72);
      }
    }
    const layout = tag === 5 ? descriptor.entries : descriptor.first;
    if (layout.roots.length === 0 || this.objectPhase === 3) return -1;
    if (this.objectPhase === 1 && tag !== 2) {
      if (tag === 3) {
        const stride = this.pointer(data + 24);
        if (stride < layout.slots.length * 8
          || !this.beginEntries(this.pointer(data), this.pointer(data + 8), stride)) {
          this.owner.heap.fail('InvalidPointer'); return -1;
        }
      } else if (tag === 5) {
        if (!this.beginEntries(data + 16, this.pointer(data + 8), (1 + layout.slots.length) * 8)) return -1;
      } else if (tag === 6) {
        const count = this.pointer(data), capacity = this.pointer(data + 8);
        const stride = this.pointer(data + 16), base = this.pointer(data + 24), head = this.pointer(data + 32);
        if (count > capacity || stride < layout.slots.length * 8 || (capacity && head >= capacity)
          || !this.beginEntries(base, Math.max(1, capacity), stride)) {
          this.owner.heap.fail('InvalidPointer'); return -1;
        }
        this.entryCount = count; this.entryHead = head; this.entryCapacity = capacity;
      }
      this.objectPhase = 2;
    }
    if (this.backingSpan && (!this.backingSpan.has(this.backingIndex)
      || this.backingSpan.identity[this.backingIndex] !== this.backingIdentity)) return -1;
    if (this.entry >= this.entryCount) {
      if (tag !== 6 || this.objectPhase === 4 || this.word(data + 56) === 0n) return -1;
      this.objectPhase = 4; this.entry = 0; this.entryCount = 1;
      this.entryHead = 0; this.entryCapacity = 0;
    }
    const index = this.entryCapacity ? (this.entryHead + this.entry) % this.entryCapacity : this.entry;
    let address = this.entryBase + index * this.entryStride;
    if (tag === 5) {
      if (this.layoutRoot === 0 && this.word(address) !== 1n) { this.entry += 1; return 0; }
      address += 8;
    }
    // beginEntries authenticates the complete physical range once; the flat
    // layout and stride bound every root offset inside that retained identity.
    const result = this.rootAtKnownRange(address, layout.roots[this.layoutRoot++]);
    if (this.layoutRoot === layout.roots.length) { this.layoutRoot = 0; this.entry += 1; }
    return result;
  }
  private beginEntries(base: number, count: number, stride: number): boolean {
    if (!this.range(base, count * stride)) return false;
    this.entryBase = base; this.entryCount = count; this.entryStride = stride;
    this.entryHead = 0; this.entryCapacity = 0;
    const provider = this.owner.heap.provider;
    const header = provider.findHeader(base);
    this.backingSpan = provider.spanAtHeader(header);
    this.backingIndex = this.backingSpan?.index(header) ?? 0;
    this.backingIdentity = this.backingSpan?.identity[this.backingIndex] ?? 0;
    return true;
  }
  releaseObject(header: number): void { this.owner.releaseObject(header); }
  private view(): DataView { return this.owner.heap.provider.view(); }
  private word(address: number): bigint { return this.view().getBigUint64(address, true); }
  private pointer(address: number): number { return this.view().getUint32(address, true); }
  private root(address: number): number {
    const view = this.view();
    return this.owner.referenceWord
      ? this.owner.referenceWord(view.getUint32(address, true), view.getUint32(address + 4, true))
      : this.owner.reference(view.getBigUint64(address, true));
  }

  private rootAt(base: number, encoded: number): number {
    const address = base + (encoded & ~1);
    if (!this.range(address, (encoded & 1) ? 16 : 8)) return 0;
    return this.rootAtKnownRange(base, encoded);
  }

  private rootAtKnownRange(base: number, encoded: number): number {
    const address = base + (encoded & ~1);
    return (encoded & 1) !== 0
      ? this.view().getUint8(address) >= 14 ? this.root(address + 8) : 0
      : this.root(address);
  }

  private range(address: number, bytes: number): boolean {
    const provider = this.owner.heap.provider;
    if (bytes === 0 && Number.isInteger(address) && address >= 0 && address <= provider.memory.buffer.byteLength) return true;
    const header = provider.findHeader(address);
    const span = provider.spanAtHeader(header);
    if (span && span.heap === this.owner.heap && bytes >= 0
      && bytes <= header + 32 + span.requested[span.index(header)] - address) return true;
    if (!span && address >= this.metadata.stackBase
      && address + bytes <= this.metadata.stackLimit) return true;
    this.owner.heap.fail('InvalidPointer'); return false;
  }

  private *scanRoots(): Generator<number> {
    const m = this.metadata;
    if (this.owner.state !== 0) yield this.owner.state;
    for (const reference of this.owner.extraRoots()) yield reference;
    for (const raw of this.owner.frames()) {
      if (raw === 0) { yield 0; continue; }
      const header = this.owner.heap.provider.findHeader(raw);
      const span = this.owner.heap.provider.spanAtHeader(header);
      const index = span?.index(raw) ?? 0;
      const identity = span?.identity[index];
      if (raw !== m.stackBase && (!span || span.heap !== this.owner.heap)) { yield 0; continue; }
      const valid = () => raw === m.stackBase || (span!.has(index) && span!.identity[index] === identity);
      yield raw;
      if (!valid()) continue;
      const functionId = this.owner.frameFunction(raw, span, index);
      const slots = m.frames[functionId ?? -1];
      if (!slots) { this.owner.heap.fail('InvalidPointer'); return; }
      for (const root of slots.roots) {
        if (!valid() || this.owner.frameFunction(raw, span, index) !== functionId) break;
        yield this.rootAt(raw + m.frameBytes, root);
      }
      for (const offset of m.frameDefers) {
        if (!valid()) break;
        yield this.pointer(raw + offset);
      }
    }
    for (const fiber of this.owner.fibers()) {
      const recordHeader = this.owner.heap.provider.findHeader(fiber);
      const recordSpan = this.owner.heap.provider.spanAtHeader(recordHeader);
      if (!recordSpan || recordSpan.heap !== this.owner.heap) { yield 0; continue; }
      const recordIndex = recordSpan.index(fiber);
      const recordIdentity = recordSpan.identity[recordIndex];
      yield 0; // Advancing a scheduler record is independently budgeted.
      if (!recordSpan.has(recordIndex) || recordSpan.identity[recordIndex] !== recordIdentity) continue;
      if (this.pointer(fiber + m.fiberIsland) === this.owner.state) {
        yield fiber;
        if (this.word(fiber + m.fiberPanicGeneration) !== 0n) {
          yield this.view().getUint8(fiber + m.fiberPanic) >= 14
            ? this.root(fiber + m.fiberPanic + 8) : 0;
        }
        yield this.root(fiber + m.fiberPreviousPanic);
      }
    }
  }

  /** Physical slot kind is a constant-time lookup in authenticated flat facts. */
  slotKind(header: number, address: number): number {
    const descriptor = this.metadata.descriptors[this.pointer(header + 12)];
    if (!descriptor) return 0;
    const data = header + AOT_OBJECT_HEADER_BYTES;
    let offset = address - data;
    if (offset < 0 || offset % 8 !== 0) return 0;
    switch (descriptor.tag) {
      case 2: return descriptor.first.slots[offset / 8] ?? 0;
      case 3: {
        if (offset === 0) return this.pointer(data + 16) === 0 ? 0 : 2;
        const entries = this.pointer(data);
        const stride = this.pointer(data + 24);
        offset = address - entries;
        return offset >= 0 && stride !== 0 ? descriptor.first.slots[(offset % stride) / 8] ?? 0 : 0;
      }
      case 4: return offset === 32 ? 2 : 0;
      case 5: {
        if (offset === 0) return 2;
        offset -= 16;
        const stride = (1 + descriptor.entries.slots.length) * 8;
        const field = (offset % stride) / 8 - 1;
        return offset >= 0 && field >= 0 ? descriptor.entries.slots[field] ?? 0 : 0;
      }
      case 6: {
        if (offset === 72) return 2;
        const entries = this.pointer(data + 24);
        const stride = this.pointer(data + 16);
        offset = address - entries;
        return offset >= 0 && stride !== 0 ? descriptor.first.slots[(offset % stride) / 8] ?? 0 : 0;
      }
      default: return 0;
    }
  }
}
