import assert from 'node:assert/strict';
import test from 'node:test';
import { AotHeapProvider, AotSpanHeap } from '../dist/aot_span_heap.js';
import { AotTraceCursor } from '../dist/aot_trace.js';
const layout = (slots, roots) => ({ slots, roots });
const scalar = layout([0], []), reference = layout([1], [0]), pair = layout([3, 4], [1]);
function setup() {
  const memory = new WebAssembly.Memory({ initial: 1, maximum: 16 });
  const provider = new AotHeapProvider(memory, 16), heap = new AotSpanHeap(provider, 0);
  const descriptors = [
    { tag: 0, first: scalar, second: scalar, entries: scalar },
    { tag: 2, first: pair, second: scalar, entries: pair },
    { tag: 3, first: reference, second: scalar, entries: reference },
    { tag: 4, first: reference, second: reference, entries: pair },
    { tag: 5, first: scalar, second: reference, entries: layout([0, 1], [8]) },
    { tag: 6, first: reference, second: scalar, entries: reference },
    { tag: 2, first: layout([1, 1], [0, 8]), second: scalar, entries: scalar },
  ];
  const metadata = { descriptors, stackBase: 128, stackLimit: 256, frames: [] };
  const owner = { heap, state: 0, reference: value => Number(value), frames: () => [], fibers: () => [], extraRoots: () => [], releaseObject() {} };
  const trace = new AotTraceCursor(metadata, owner);
  const word = (address, value) => provider.view().setBigUint64(address, BigInt(value), true);
  const scan = (pointer) => {
    trace.resetObject(pointer - 32);
    const result = [];
    for (let steps = 0; steps < 100; steps++) {
      const root = trace.nextObjectReference();
      if (root < 0) return result;
      result.push(root);
    }
    assert.fail('trace did not finish');
  };
  return { heap, provider, trace, word, scan };
}

test('reusable cursor scans interfaces, slices, map entries and queue rings precisely', () => {
  const { heap, word, scan } = setup();
  const value = heap.allocate(16, 1);
  word(value, 14); word(value + 8, 99);
  assert.deepEqual(scan(value), [99]);
  word(value, 1); assert.deepEqual(scan(value), [0]);
  const backing = heap.allocate(24, 0);
  word(backing, 11); word(backing + 8, 22); word(backing + 16, 33);
  const slice = heap.allocate(32, 2);
  word(slice, backing); word(slice + 8, 2); word(slice + 16, 3); word(slice + 24, 8);
  assert.deepEqual(scan(slice), [backing, 11, 22]);
  const entries = heap.allocate(16 + 3 * 24, 4);
  word(entries, backing); word(entries + 8, 3);
  word(entries + 16, 1); word(entries + 32, 55);
  word(entries + 64, 1); word(entries + 80, 66);
  assert.deepEqual(scan(entries), [backing, 55, 0, 66]);
  const map = heap.allocate(40, 3); word(map + 32, entries);
  assert.deepEqual(scan(map), [entries]);
  const queue = heap.allocate(120, 5);
  for (const [offset, value] of [[0, 2], [8, 3], [16, 8], [24, backing], [32, 2], [56, 1], [72, 88]]) word(queue + offset, value);
  assert.deepEqual(scan(queue), [88, 33, 11, 11]);
  word(queue, 0); word(queue + 8, 0); word(queue + 32, 0);
  assert.deepEqual(scan(queue), [88, 11]);
});

test('object and backing identities survive suspension and reject address reuse', () => {
  const { heap, provider, trace, word } = setup();
  const anchor = heap.allocate(16, 0), object = heap.allocate(16, 6);
  word(object, 11); word(object + 8, 22);
  trace.resetObject(object - 32);
  assert.equal(trace.nextObjectReference(), 11);
  let span = provider.spanAtHeader(object - 32);
  heap.release(span, span.index(object));
  assert.equal(heap.allocate(16, 6), object);
  assert.equal(trace.nextObjectReference(), -1);
  const slice = heap.allocate(32, 2);
  word(slice, anchor); word(slice + 8, 2); word(slice + 16, 2); word(slice + 24, 8);
  trace.resetObject(slice - 32);
  assert.equal(trace.nextObjectReference(), anchor);
  assert.equal(trace.nextObjectReference(), 0);
  span = provider.spanAtHeader(anchor - 32); heap.release(span, span.index(anchor));
  assert.equal(heap.allocate(16, 0), anchor);
  assert.equal(trace.nextObjectReference(), -1);
});

test('range admission rejects a layout larger than its exact allocation extent', () => {
  const { heap, trace } = setup();
  const object = heap.allocate(8, 6);
  trace.resetObject(object - 32);
  assert.equal(trace.nextObjectReference(), -1);
  assert.equal(heap.error.kind, 'InvalidPointer');
});


test('frame roots use registered layouts and stop after the cell is reused', () => {
  const { heap, provider, word } = setup();
  const frameBytes = 32, frameFunction = 0;
  const metadata = {
    stackBase: 128, stackLimit: 256, frameBytes, frameFunction, frameDefers: [],
    frames: [layout([1, 1], [0, 8]), layout([0, 0], [])],
  };
  const owner = {
    heap, state: 0, reference: value => Number(value),
    frames: () => heap.frames(), fibers: () => [], extraRoots: () => [], releaseObject() {},
    frameFunction: (_raw, span, index) => span?.frameFunction(index),
  };
  const trace = new AotTraceCursor(metadata, owner);
  const anchor = heap.allocate(48, 0);
  const frame = heap.allocate(48, 1, 0, 48 + 32, 0);
  word(frame + frameFunction, 1); // Guest header cannot redefine the admitted layout.
  word(frame + frameBytes, 11); word(frame + frameBytes + 8, 22);
  trace.resetRoots();
  assert.equal(trace.nextRoot(), 0); // One span advances per budget unit.
  assert.equal(trace.nextRoot(), frame);
  assert.equal(trace.nextRoot(), 11);
  const span = provider.spanAtHeader(frame - 32);
  heap.release(span, span.index(frame));
  assert.equal(heap.allocate(48, 0), frame);
  assert.equal(trace.nextRoot(), -1, 'old frame cursor cannot scan the new ordinary cell');
  assert.ok(provider.findHeader(anchor));
  assert.equal(heap.error, undefined);
});
