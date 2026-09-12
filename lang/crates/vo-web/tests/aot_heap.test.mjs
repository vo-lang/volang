import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';
import { AotMemoryRuntime } from '../dist/aot_memory.js';
import { parseAotMemoryMetadata } from '../dist/aot_trace.js';

// Test-only exports let us exercise the real generated allocator and collector
// without extending the public host ABI or relying on guest root liveness.
function inspectableImage(bytes) {
  const read = (buffer, cursor) => {
    let value = 0, shift = 0, byte;
    do {
      byte = buffer[cursor.at++];
      assert.notEqual(byte, undefined);
      value += (byte & 127) * 2 ** shift;
      shift += 7;
    } while (byte & 128);
    return value;
  };
  const leb = (value) => {
    const result = [];
    do {
      const byte = value & 127;
      value = Math.floor(value / 128);
      result.push(byte | (value ? 128 : 0));
    } while (value);
    return result;
  };
  const module = new WebAssembly.Module(bytes);
  const names = new Uint8Array(WebAssembly.Module.customSections(module, 'name')[0]);
  const functions = new Map();
  const cursor = { at: 0 };
  while (cursor.at < names.length) {
    const id = names[cursor.at++];
    const size = read(names, cursor);
    const end = cursor.at + size;
    if (id === 1) {
      const count = read(names, cursor);
      for (let i = 0; i < count; i++) {
        const index = read(names, cursor);
        const length = read(names, cursor);
        const name = new TextDecoder().decode(names.subarray(cursor.at, cursor.at + length));
        cursor.at += length;
        functions.set(name, index);
      }
    }
    cursor.at = end;
  }
  const additions = ['volang.find_allocation', 'volang.gc_collect'];
  const encoded = additions.flatMap((name) => {
    assert.ok(functions.has(name), `missing generated helper ${name}`);
    const text = [...new TextEncoder().encode(name)];
    return [...leb(text.length), ...text, 0, ...leb(functions.get(name))];
  });
  const output = [bytes.subarray(0, 8)];
  cursor.at = 8;
  while (cursor.at < bytes.length) {
    const start = cursor.at;
    const id = bytes[cursor.at++];
    const size = read(bytes, cursor);
    const end = cursor.at + size;
    if (id === 7) {
      const count = read(bytes, cursor);
      const payload = Buffer.concat([
        Buffer.from(leb(count + additions.length)), bytes.subarray(cursor.at, end), Buffer.from(encoded),
      ]);
      output.push(Buffer.from([7, ...leb(payload.length)]), payload);
    } else {
      output.push(bytes.subarray(start, end));
    }
    cursor.at = end;
  }
  const manifest = new DataView(WebAssembly.Module.customSections(module, 'volang.aot.v8')[0]);
  return { bytes: Buffer.concat(output), pages: manifest.getUint32(16, true) };
}

test('generated heap keeps exact extents, isolates size classes, and indexes over one million objects', async () => {
  assert.ok(process.env.VO_AOT_IMAGE, 'VO_AOT_IMAGE must name the CLI-produced image');
  const image = inspectableImage(await readFile(process.env.VO_AOT_IMAGE));
  const memory = new WebAssembly.Memory({ initial: image.pages, maximum: 4096 });
  const module = new WebAssembly.Module(image.bytes);
  const runtime = new AotMemoryRuntime(memory, 4096, parseAotMemoryMetadata(module), { automaticGC: false });
  const instance = new WebAssembly.Instance(module, {
    'volang:runtime/v3': { memory, 'call-extern': (...args) => runtime.call(...args) },
  });
  runtime.attach(instance);
  const allocate = instance.exports.vo_alloc;
  const owner = instance.exports['volang.find_allocation'];
  const collect = () => {
    runtime.requestCollection();
    while (runtime.drainStep()) {}
    return 0;
  };
  for (const size of [0, 1, 8, 31, 32, 33, 32736, 32737, 65504, 65505]) {
    const pointer = allocate(size);
    assert.ok(pointer > 0, `allocation of ${size}`);
    assert.equal(owner(pointer), pointer - 32, 'zero-byte allocations retain exact identity');
    if (size) assert.equal(owner(pointer + size), 0, 'class padding is outside the requested extent');
    if (size) assert.equal(owner(pointer + size - 1), pointer - 32);
  }
  const large = allocate(200000);
  new Uint8Array(memory.buffer, large, 200000).fill(255);
  assert.equal(collect(), 0);
  assert.equal(owner(large), 0, 'dead allocation has no owner');
  assert.notEqual(allocate(8), large, 'small allocation cannot pin a large free block');
  const reused = allocate(200000);
  assert.ok(reused, 'large allocation reuses admitted block capacity');
  assert.ok(new Uint8Array(memory.buffer, reused, 200000).every((byte) => byte === 0));

  const probes = [];
  for (let i = 0; i < 1_060_000; i++) {
    const pointer = allocate(8);
    assert.ok(pointer > 0);
    if (i % 4096 === 0 || i >= 1_048_575) probes.push(pointer);
  }
  for (const pointer of probes) {
    assert.equal(owner(pointer + 7), pointer - 32);
    assert.equal(owner(pointer + 8), 0);
  }
  assert.equal(collect(), 0);
  for (const pointer of probes) assert.equal(owner(pointer), 0);
  const pages = memory.buffer.byteLength;
  for (let i = 0; i < 10000; i++) assert.ok(allocate(8));
  assert.equal(memory.buffer.byteLength, pages, 'freed small allocations reuse committed memory');
});
