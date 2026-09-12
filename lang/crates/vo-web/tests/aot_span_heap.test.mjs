import assert from 'node:assert/strict';
import test from 'node:test';
import { AotHeapProvider, AotSpanHeap } from '../dist/aot_span_heap.js';
import { AotCollector } from '../dist/aot_collector.js';

const BLOCK = 65536;
const setup = (policy = {}) => {
  const memory = new WebAssembly.Memory({ initial: 1, maximum: 64 });
  const provider = new AotHeapProvider(memory, 64);
  return { memory, provider, heap: new AotSpanHeap(provider, 1, policy) };
};

function traceFixture(heap) {
  const roots = [];
  let root = 0, object = 0, slot = 0;
  const source = {
    resetRoots() { root = 0; },
    nextRoot() { return root < roots.length ? roots[root++] : -1; },
    resetObject(header) { object = header; slot = 0; },
    nextObjectReference() {
      const span = heap.provider.spanAtHeader(object);
      if (!span || slot * 8 >= span.requested[span.index(object)]) return -1;
      return heap.provider.view().getUint32(object + 32 + slot++ * 8, true);
    },
    releaseObject() {},
  };
  const gc = new AotCollector(heap, source);
  const allocate = (size) => heap.allocate(size, 1, gc.allocationEpoch);
  const write = (parent, offset, child) => {
    heap.provider.view().setBigUint64(parent + offset, BigInt(child), true);
    gc.write(parent, child);
  };
  const cycle = (budget = 3) => {
    let calls = 0;
    do { assert.ok(gc.step(budget) <= budget); assert.ok(++calls < 100000); }
    while (gc.phase !== 'idle');
  };
  return { roots, gc, allocate, write, cycle };
}

test('directory owns exact extents, membership, and zeroed class reuse', () => {
  const { heap, provider, memory } = setup();
  const pointer = heap.allocate(33, 2);
  assert.equal(provider.findHeader(pointer), pointer - 32);
  assert.equal(provider.findHeader(pointer + 32), pointer - 32);
  assert.equal(provider.findHeader(pointer + 33), 0);
  provider.view().setUint32(pointer - 32 + 16, 65535, true);
  assert.equal(provider.findHeader(pointer + 33), 0);
  new Uint8Array(memory.buffer, pointer, 33).fill(91);
  const span = provider.spanAtHeader(pointer - 32);
  heap.release(span, span.index(pointer));
  assert.equal(provider.findHeader(pointer), 0);
  assert.equal(heap.reclaimOne(span), true);
  const reused = heap.allocate(33, 2);
  assert.equal(reused, pointer);
  assert.deepEqual([...new Uint8Array(memory.buffer, reused, 33)], Array(33).fill(0));
});

test('empty small blocks serve a different class and large runs reclaim one block per step', () => {
  const { heap, provider } = setup({ initialReserveBytes: 4 * BLOCK, growthAllowed: false });
  const small = heap.allocate(8, 2);
  let span = provider.spanAtHeader(small - 32);
  heap.release(span, span.index(small));
  assert.ok(heap.reclaimOne(span));
  const large = heap.allocate(3 * BLOCK - 32, 2);
  assert.ok(large);
  span = provider.spanAtHeader(large - 32);
  heap.release(span, 0);
  assert.equal(heap.stats().reclaimBlocks, 3);
  for (let remaining = 2; remaining >= 0; remaining--) {
    assert.ok(heap.reclaimOne(span));
    assert.equal(heap.stats().reclaimBlocks, remaining);
  }
  assert.equal(heap.stats().freeBlocks, 4);
  assert.ok(heap.allocate(32736, 2));
  assert.equal(heap.stats().committedBytes, 4 * BLOCK);
});

test('Island policy inheritance preserves independent occupancy and sticky errors', () => {
  const { heap, provider } = setup({ initialReserveBytes: BLOCK, hardLimitBytes: BLOCK, growthAllowed: false });
  const child = heap.child(2);
  const a = heap.allocate(8, 2);
  const b = child.allocate(8, 2);
  assert.notEqual(Math.floor(a / BLOCK), Math.floor(b / BLOCK));
  assert.equal(provider.spanAtHeader(a - 32).heap, heap);
  assert.equal(provider.spanAtHeader(b - 32).heap, child);
  assert.equal(child.allocate(2 * BLOCK, 2), 0);
  const error = child.observeFailure();
  assert.equal(error.kind, 'GrowthDisabled');
  assert.equal(child.observeFailure(), error);
  assert.equal(child.allocate(8, 2), 0);
  assert.ok(heap.allocate(8, 2));
  assert.equal(heap.error, undefined);
});

test('collector budgets cover long roots, large objects, sweep, and reclaim', () => {
  const { heap, provider } = setup();
  const { roots, gc, allocate, cycle } = traceFixture(heap);
  const large = allocate(2 * BLOCK);
  const child = allocate(8);
  provider.view().setBigUint64(large + 2 * BLOCK - 8, BigInt(child), true);
  roots.push(...Array(2000).fill(large));
  const before = gc.stats();
  assert.equal(gc.step(0), 0);
  assert.deepEqual(gc.stats(), before);
  cycle(7);
  assert.equal(heap.objectCount, 2);
  roots.length = 0;
  gc.requestMajor();
  cycle(5);
  assert.equal(heap.objectCount, 0);
  assert.equal(heap.stats().reclaimBlocks, 0);
});

test('new-value barriers preserve white children and old-to-young edges', () => {
  const { heap, provider } = setup();
  const { roots, gc, allocate, write, cycle } = traceFixture(heap);
  const parent = allocate(8);
  roots.push(parent);
  cycle();
  cycle();
  const child = allocate(8);
  write(parent, 0, child);
  cycle(1);
  assert.ok(provider.findHeader(child));
  assert.ok(gc.stats().minorCycles > 0);
  gc.requestMajor();
  while (gc.phase !== 'sweep') gc.step(1);
  const fresh = allocate(8);
  write(parent, 0, fresh);
  while (gc.phase !== 'idle') assert.equal(gc.step(1), 1);
  assert.ok(provider.findHeader(fresh));
  roots.length = 0;
  gc.requestMajor(); cycle(1);
  assert.equal(heap.objectCount, 0);
});


test('explicit release cancels queued roots before immediate address reuse', () => {
  const { heap, provider } = setup({ initialReserveBytes: BLOCK, growthAllowed: false });
  const { roots, gc, allocate, cycle } = traceFixture(heap);
  const anchor = allocate(8);
  roots.push(anchor);
  gc.step(1);
  for (let i = 0; i < 5000; i++) {
    const pointer = allocate(8);
    gc.publish(pointer);
    const span = provider.spanAtHeader(pointer - 32);
    heap.release(span, span.index(pointer));
  }
  assert.equal(heap.error, undefined);
  cycle(1);
  assert.ok(provider.findHeader(anchor));
  assert.ok(gc.stats().rememberedParents <= heap.objectCount);
});

test('epoch normalization is bounded and cannot revive dead allocations', () => {
  const { heap, provider } = setup();
  const { roots, gc, allocate, cycle } = traceFixture(heap);
  const live = allocate(8), dead = allocate(8);
  roots.push(live);
  gc.epoch = 0xffffffff;
  assert.equal(gc.step(1), 1);
  assert.equal(gc.phase, 'reset');
  cycle(1);
  assert.ok(provider.findHeader(live));
  assert.equal(provider.findHeader(dead), 0);
});

test('no-growth span metadata is admitted once and reused across size classes', () => {
  const { heap, provider } = setup({ initialReserveBytes: BLOCK, growthAllowed: false });
  let metadata;
  for (const size of [8, 32736, 100, 65504, 8]) {
    const pointer = heap.allocate(size, 0);
    assert.ok(pointer);
    const span = provider.spanAtHeader(pointer - 32);
    if (metadata) assert.equal(span, metadata);
    metadata = span;
    heap.release(span, span.index(pointer));
    while (span.reclaiming) heap.reclaimOne(span);
  }
  assert.equal(heap.stats().committedBytes, BLOCK);
});

test('retired Island blocks are reusable without physical memory growth', () => {
  const { heap, provider, memory } = setup({ initialReserveBytes: 3 * BLOCK });
  const before = memory.buffer.byteLength;
  assert.ok(heap.returnFreePage());
  assert.ok(heap.returnFreePage());
  const child = new AotSpanHeap(provider, 2, { initialReserveBytes: 2 * BLOCK, growthAllowed: false });
  assert.equal(memory.buffer.byteLength, before);
  assert.equal(heap.stats().committedBytes, BLOCK);
  assert.equal(child.stats().committedBytes, 2 * BLOCK);
  assert.ok(child.allocate(BLOCK, 0));
});

test('idle barriers follow old-parent coverage and resume when remembered scans retire cards', () => {
  const { heap, provider } = setup();
  const { roots, gc, allocate, write, cycle } = traceFixture(heap);
  const parent = allocate(8);
  roots.push(parent);
  cycle();
  assert.equal(gc.needsBarrier, false, 'young-only heaps require no idle barrier');
  cycle();
  assert.equal(gc.needsBarrier, false, 'promotion already remembers every old parent');
  cycle();
  assert.equal(gc.needsBarrier, true, 'retired cards require barriers for subsequent young edges');
  assert.equal(gc.setMode('incremental'), true);
  assert.equal(gc.needsBarrier, false);
  assert.equal(gc.setMode('generational'), true);
  assert.equal(gc.needsBarrier, true);
  const child = allocate(8);
  write(parent, 0, child);
  assert.equal(gc.needsBarrier, false);
  cycle();
  assert.ok(provider.findHeader(child));
  const span = provider.spanAtHeader(parent - 32);
  roots.length = 0;
  heap.release(span, span.index(parent));
  gc.requestMajor(); cycle();
  assert.equal(heap.stats().objects, 0);
  assert.equal(gc.needsBarrier, false, 'released old objects cannot keep idle barriers enabled');
});

test('retired frame-span metadata is reused without retaining managed pages or identity', () => {
  const { heap, provider } = setup();
  let previousSpan, previousIdentity = 0;
  for (let iteration = 0; iteration < 100; iteration++) {
    const pointer = heap.allocate(120, 1), span = provider.spanAtHeader(pointer - 32);
    if (previousSpan) assert.equal(span, previousSpan);
    assert.ok(span.identity[span.index(pointer)] > previousIdentity);
    previousIdentity = span.identity[span.index(pointer)]; previousSpan = span;
    heap.release(span, span.index(pointer));
    assert.equal(provider.findHeader(pointer), 0);
    assert.ok(heap.reclaimOne(span));
    assert.equal(heap.stats().freeBlocks, 1);
  }
});

test('minor sweep skips old spans and charges bitmap words for empty slots', () => {
  const { heap } = setup();
  const { roots, gc, allocate, cycle } = traceFixture(heap);
  assert.ok(gc.admitObjects(2048));
  for (let index = 0; index < 2048; index++) roots.push(allocate(8));
  cycle(64); cycle(64); cycle(64);
  const cycleStart = gc.stats().workUnits;
  cycle(1);
  assert.ok(gc.stats().workUnits - cycleStart <= roots.length + 100,
    'retired remembered cards cannot keep empty old spans in the minor scan');
  do { gc.step(1); } while (gc.phase !== 'sweep');
  assert.equal(gc.isMajor, false);
  const before = gc.stats().workUnits;
  while (gc.phase !== 'idle') gc.step(1);
  assert.ok(gc.stats().workUnits - before <= heap.spans.size * 3 + 2);
  assert.equal(heap.objectCount, roots.length);
});

test('cached views follow external memory growth and zero newly admitted bytes', () => {
  const { provider, memory } = setup();
  provider.view().setUint32(8, 123, true);
  memory.grow(1);
  assert.equal(provider.view().getUint32(8, true), 123);
  provider.view().setUint32(65536, 456, true);
  provider.zero(65536, 4);
  assert.equal(provider.view().getUint32(65536, true), 0);
  memory.grow(0);
  assert.equal(provider.view().getUint32(8, true), 123);
});


test('frame root bitmaps cover every word, clear holes, and preserve owner isolation', () => {
  const { heap, provider } = setup();
  const pointers = Array.from({ length: 1024 }, (_, id) => heap.allocate(8, 1, 0, 8 + 32, id));
  assert.ok(pointers.every(Boolean));
  assert.deepEqual([...heap.frames()].filter(Boolean), pointers);
  const span = provider.spanAtHeader(pointers[0] - 32);
  assert.equal(span.frameFunction(0), 0, 'function zero has a distinct membership tag');
  assert.equal(span.frameFunction(1023), 1023);
  assert.equal(heap.frameSpans.size, 1);
  const holes = [0, 31, 32, 63, 1023];
  for (const index of holes) heap.release(span, index);
  assert.deepEqual([...heap.frames()].filter(Boolean), pointers.filter((_, i) => !holes.includes(i)));
  const ordinary = heap.allocate(8, 0);
  assert.equal(ordinary, pointers[0]);
  assert.equal(span.frameFunction(0), undefined, 'an ordinary reused cell cannot inherit frame identity');
  const child = heap.child(2);
  const other = child.allocate(8, 1, 0, 8 + 32, 0);
  assert.deepEqual([...child.frames()].filter(Boolean), [other]);
  assert.ok(![...heap.frames()].includes(other));
});

test('empty frame spans consume bounded root work until their ordinary objects retire', () => {
  const { heap, provider } = setup();
  const roots = [];
  for (const size of [8, 64, 128, 256, 512, 1024, 2048, 4096]) {
    const anchor = heap.allocate(size, 0);
    const frame = heap.allocate(size, 1, 0, size + 32, 0);
    const span = provider.spanAtHeader(frame - 32);
    heap.release(span, span.index(frame));
    roots.push([span, anchor]);
  }
  const iterator = heap.frames();
  for (const _ of roots) assert.deepEqual(iterator.next(), { value: 0, done: false });
  assert.equal(iterator.next().done, true);
  for (const [span, anchor] of roots) {
    heap.release(span, span.index(anchor));
    assert.ok(heap.reclaimOne(span));
  }
  assert.equal(heap.frameSpans.size, 0);
  assert.deepEqual([...heap.frames()], []);
});

test('no-growth admission reuses frame metadata across cell shapes and large runs', () => {
  const { heap, provider } = setup({ initialReserveBytes: BLOCK, growthAllowed: false });
  let previous;
  for (const size of [112, 40000, 8, 32700, 64]) {
    const pointer = heap.allocate(size, 1, 0, size + 32, 999999);
    assert.ok(pointer);
    const span = provider.spanAtHeader(pointer - 32);
    if (previous) {
      assert.equal(span, previous.span);
      assert.equal(span.cellData, previous.functions);
      assert.equal(span.frameBits, previous.bits);
    }
    assert.deepEqual([...heap.frames()].filter(Boolean), [pointer]);
    previous = { span, functions: span.cellData, bits: span.frameBits };
    heap.release(span, span.index(pointer));
    while (span.reclaiming) assert.ok(heap.reclaimOne(span));
    assert.equal(heap.frameSpans.size, 0);
  }
  assert.equal(heap.stats().committedBytes, BLOCK);
  assert.equal(heap.error, undefined);
});


test('frame identities derive exact backing bytes and preserve container accounting', () => {
  const memory = new WebAssembly.Memory({initial: 8, maximum: 16});
  const provider = new AotHeapProvider(memory, 16);
  const heap = new AotSpanHeap(provider, 0);
  const frame = heap.allocate(32, 7, 0, 64, 0);
  const backing = heap.allocate(32, 8, 0, 24);
  const span = provider.spanAtHeader(frame - 32);
  assert.equal(provider.spanAtHeader(backing - 32), span);
  assert.equal(span.frameFunction(span.index(frame)), 0);
  assert.equal(span.frameFunction(span.index(backing)), undefined);
  assert.equal(heap.stats().backingBytes, 88);
  heap.release(span, span.index(frame));
  assert.equal(heap.stats().backingBytes, 24, 'freeing function zero cannot subtract container bytes');
  const reused = heap.allocate(32, 8, 0, 16);
  assert.equal(reused, frame);
  assert.equal(span.frameFunction(span.index(reused)), undefined);
  assert.equal(heap.stats().backingBytes, 40);
  heap.release(span, span.index(reused));
  assert.equal(heap.stats().backingBytes, 24);
  assert.equal(span.registerFrame(span.index(backing), 6), false);
  assert.equal(heap.error.kind, 'InvalidPointer');
  assert.equal(span.frameFunction(span.index(backing)), undefined);
  assert.equal(heap.stats().backingBytes, 24);
  heap.release(span, span.index(backing));
  assert.equal(heap.stats().backingBytes, 0);
});
