import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';

import { runAot } from '../dist/index.js';
import { parseAotInlineSources, lookupAotLogicalSources } from '../dist/aot_inline_sources.js';
import { parseAotDebugMetadata } from '../dist/aot_metadata.js';

function sourceModule(payloads, sectionName = 'volang.inline-sources.v1') {
  const leb = (value) => {
    const bytes = [];
    do {
      let byte = value & 127;
      value >>>= 7;
      if (value !== 0) byte |= 128;
      bytes.push(byte);
    } while (value !== 0);
    return bytes;
  };
  const output = [0, 97, 115, 109, 1, 0, 0, 0];
  const name = new TextEncoder().encode(sectionName);
  for (const payload of payloads) {
    const section = [...leb(name.length), ...name, ...payload];
    output.push(0, ...leb(section.length), ...section);
  }
  return new WebAssembly.Module(Uint8Array.from(output));
}

function sourcePayload(words) {
  const output = Buffer.alloc(8 + words.length * 4);
  output.write('VOINS001', 0, 'ascii');
  words.forEach((word, index) => output.writeUInt32LE(word, 8 + index * 4));
  return output;
}

const logicalSourceWords = [2, 2, 1,
  0xffff_ffff, 0, 0xffff_ffff, 0, 0, 0,
  0, 1, 0, 10, 70_000, 80_000,
  0, 4, 1, 2, 1];

test('Core debug source ancestry is optional, exact and independent of physical frame walking', () => {
  const empty = parseAotInlineSources(sourceModule([]), [], 0);
  assert.equal(empty.frames.length, 0);
  const inlineSources = parseAotInlineSources(sourceModule([sourcePayload(logicalSourceWords)]), ['leaf.vo'], 2);
  const physical = { pc: 3, file: 'physical.vo', line: 30, col: 2, length: 3 };
  const debug = { inlineSources, functions: [new Map([[3, physical]])] };
  assert.deepEqual(lookupAotLogicalSources(debug, 0, 2), [
    { functionId: 1, location: { file: 'leaf.vo', line: 10, col: 70_000, length: 80_000 } },
    { functionId: 0, location: undefined },
  ]);
  assert.deepEqual(lookupAotLogicalSources(debug, 0, 3), [{ functionId: 0, location: physical }]);
  assert.deepEqual(lookupAotLogicalSources(debug, 0, 1), [{ functionId: 0, location: undefined }]);
});

test('Core source metadata rejects malformed ancestry and instruction ownership', () => {
  const payload = sourcePayload(logicalSourceWords);
  assert.throws(() => parseAotInlineSources(sourceModule([payload, payload]), ['leaf.vo'], 2), /duplicate/);
  for (const [index, value] of [
    [0, 3], [0, 1_000_001], [1, 131_073], [2, 3],
    [3, 0], [4, 1], [6, 1], [9, 1], [10, 2],
    [11, 1], [12, 0], [13, 0], [14, 0],
    [15, 1], [16, 2], [17, 131_073], [18, 4], [19, 2],
  ]) {
    const words = logicalSourceWords.slice();
    words[index] = value;
    assert.throws(() => parseAotInlineSources(sourceModule([sourcePayload(words)]), ['leaf.vo'], 2),
      /Volang|inline source/, `word ${index} = ${value}`);
  }
  assert.throws(() => parseAotInlineSources(sourceModule([payload.subarray(0, payload.length - 1)]), ['leaf.vo'], 2), /truncated|input|table/);
  assert.throws(() => parseAotInlineSources(sourceModule([Buffer.concat([payload, Buffer.from([0])])]), ['leaf.vo'], 2), /trailing/);
  const chain = [1, 33, 1];
  for (let index = 0; index < 33; index += 1) {
    chain.push(index === 0 ? 0xffff_ffff : index - 1, 0, 0xffff_ffff, 0, 0, 0);
  }
  chain.push(0, 1, 1, 0, 32);
  assert.throws(() => parseAotInlineSources(sourceModule([sourcePayload(chain)]), [], 1), /depth/);
});

function physicalSourcePayload(records, count) {
  const path = Buffer.from('source.vo');
  const header = Buffer.alloc(36);
  header.write('VODBG003', 0, 'ascii');
  [1, 3, 96, 16, 48, 80, path.length].forEach((word, i) => header.writeUInt32LE(word, 8 + i * 4));
  const countBytes = Buffer.alloc(4);
  countBytes.writeUInt32LE(count);
  return Buffer.concat([header, path, countBytes, Buffer.from(records), Buffer.alloc(8)]);
}

test('Core compact source locations preserve exact lookup, full coordinates and immutable iteration', () => {
  const max = [255, 255, 255, 255, 15];
  const records = [2, 0, 29, 13, 8, ...max, 0, ...max, ...max, ...max];
  const debug = parseAotDebugMetadata(sourceModule([physicalSourcePayload(records, 2)], 'volang.debug.v3'));
  assert.equal(debug.functions.length, 3);
  const entries = debug.functions[0];
  const first = { pc: 2, file: 'source.vo', line: 29, col: 13, length: 8 };
  const last = { pc: 0xffff_ffff, file: 'source.vo', line: 0xffff_ffff, col: 0xffff_ffff, length: 0xffff_ffff };
  assert.equal(entries.size, 2);
  assert.deepEqual([...entries.keys()], [2, 0xffff_ffff]);
  assert.deepEqual([...entries.values()], [first, last]);
  assert.deepEqual([...entries], [[2, first], [0xffff_ffff, last]]);
  const visited = [];
  const receiver = {};
  entries.forEach(function (value, key, map) {
    assert.equal(this, receiver);
    assert.equal(map, entries);
    visited.push([key, value]);
  }, receiver);
  assert.deepEqual(visited, [...entries]);
  for (const pc of [-1, 0, 1, 2.5, 3, NaN, Infinity, 0x1_0000_0000]) {
    assert.equal(entries.get(pc), undefined);
    assert.equal(entries.has(pc), false);
  }
  assert.equal(entries.has(2), true);
  entries.get(2).line = 100;
  assert.deepEqual(entries.get(2), first);
  assert.equal(debug.functions[1].size, 0);
  assert.equal(debug.functions[2].get(0), undefined);
});

test('Core compact source parser rejects noncanonical, overflowing and incomplete records', () => {
  for (const [records, count] of [
    [[130, 0, 0, 29, 13, 8], 1],
    [[255, 255, 255, 255, 16, 0, 29, 13, 8], 1],
    [[128, 128, 128, 128, 128, 0, 29, 13, 8], 1],
    [[2, 0, 0, 13, 8], 1],
    [[2, 1, 29, 13, 8], 1],
    [[2, 0, 29, 13, 8, 2, 0, 30, 14, 9], 2],
    [[2, 0, 29, 13, 8], 0xffff_ffff],
  ]) {
    assert.throws(() => parseAotDebugMetadata(sourceModule([physicalSourcePayload(records, count)], 'volang.debug.v3')), /debug/);
  }
  const complete = physicalSourcePayload([2, 0, 29, 13, 8], 1);
  for (let removed = 1; removed <= 13; removed++) {
    assert.throws(() => parseAotDebugMetadata(sourceModule([complete.subarray(0, complete.length - removed)], 'volang.debug.v3')), /truncated|debug/);
  }
});

async function readAotImage() {
  const imagePath = process.env.VO_AOT_IMAGE;
  assert.ok(imagePath, 'VO_AOT_IMAGE must name the CLI-produced AOT image');
  return readFile(imagePath);
}

test('browser runtime admits and executes a CLI Core Wasm AOT image', async () => {
  const image = await readAotImage();
  const execution = await runAot(image);
  assert.equal(execution.manifest.target, 'wasm32-unknown-unknown');
  assert.ok(execution.manifest.semanticModuleLength > 0);
  assert.equal(execution.result.status, 'ok');
  assert.equal(execution.result.stderr, '');
  assert.equal(execution.result.stdout, 'Hello, hello!\n');
  assert.equal(execution.exitCode, 0);
  assert.equal(execution.instance.exports.vo_alloc_typed(8, 0xffff_ffff), 0);
});

test('Core Wasm host rejects an unsupported Volang ABI before instantiation', async () => {
  const image = Buffer.from(await readAotImage());
  const manifest = image.indexOf(Buffer.from('VOAOTW09', 'ascii'));
  assert.ok(manifest >= 0, 'CLI image must contain the AOT manifest');
  image.writeUInt16LE(2, manifest + 8);

  await assert.rejects(runAot(image), /unsupported Volang AOT ABI 2/);
});

test('Core Wasm host rejects an image with the wrong import and export shape', async () => {
  const emptyModule = Uint8Array.from([0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]);
  await assert.rejects(runAot(emptyModule), /imports do not match AOT ABI v9/);
});

test('Core Wasm host rejects corrupted compiler runtime metadata', async () => {
  const image = Buffer.from(await readAotImage());
  const metadata = image.indexOf(Buffer.from('VORT0001', 'ascii'));
  assert.ok(metadata >= 0, 'CLI image must contain compiler runtime metadata');
  image[metadata] ^= 0x01;

  await assert.rejects(runAot(image), /invalid Volang runtime metadata magic/);
});

test('Core Wasm host rejects an invalid debug frame-walk layout', async () => {
  const image = Buffer.from(await readAotImage());
  const metadata = image.indexOf(Buffer.from('VODBG003', 'ascii'));
  assert.ok(metadata >= 0, 'CLI image must contain compiler debug metadata');
  image.writeUInt32LE(0, metadata + 16);

  await assert.rejects(runAot(image), /invalid Volang debug frame layout/);
});

test('Core Wasm host still requires flat descriptors for ordinary runtime values', async () => {
  const image = Buffer.from(await readAotImage());
  const metadata = image.indexOf(Buffer.from('VORT0001', 'ascii'));
  assert.ok(metadata >= 0);
  assert.ok(image.readUInt32LE(metadata + 12) > 0, 'fixture contains runtime types');
  const firstRecord = metadata + 36;
  image.writeUInt32LE(0xffff_ffff, firstRecord + 20);
  await assert.rejects(runAot(image), /invalid Volang runtime type layout/);
});

test('Core Wasm host rejects oversized flat layouts for non-array values', async () => {
  const image = Buffer.from(await readAotImage());
  const metadata = image.indexOf(Buffer.from('VORT0001', 'ascii'));
  assert.ok(metadata >= 0);
  const firstRecord = metadata + 36;
  assert.notEqual(image[firstRecord + 9], 2, 'first fixture type is not an array');
  image.writeUInt32LE(0x1_0000, firstRecord + 12);
  await assert.rejects(runAot(image), /invalid Volang runtime type layout/);
});

test('Core Wasm host bounds process arguments before compiling the image', async () => {
  const image = await readAotImage();
  await assert.rejects(runAot(image, Array(1025).fill('x')), /arguments exceed the host contract/);
});

test('Core Wasm host enforces deterministic guest execution fuel', async () => {
  const image = await readAotImage();
  const execution = await runAot(image, { fuel: 0n });
  assert.equal(execution.exitCode, 15);
  assert.equal(execution.result.status, 'error');
  assert.match(execution.result.stderr, /fuel exhausted/);
});

test('Core Wasm host validates memory admission limits', async () => {
  const image = await readAotImage();
  await assert.rejects(runAot(image, { memoryLimitPages: 1 }), /memory limit must be within/);
});

test('Core Wasm admits the independent Island SpanHeap contract', async () => {
  const image = await readAotImage();
  await assert.rejects(
    runAot(image, { requireMemoryContract: 'instance-tracing' }),
    /required memory contract instance-tracing is unavailable/,
  );
  const execution = await runAot(image, { requireMemoryContract: 'island-span-heap' });
  assert.equal(execution.manifest.memoryContract, 'island-span-heap');
  assert.equal(execution.exitCode, 0);
  const corrupted = Buffer.from(image);
  const manifest = corrupted.indexOf(Buffer.from('VOAOTW09', 'ascii'));
  corrupted[manifest + 11] = 0xff;
  await assert.rejects(runAot(corrupted), /unsupported Core Wasm memory contract/);
});

test('Core Wasm rejects memory hints outside the admitted image', async () => {
  const image = Buffer.from(await readAotImage());
  const marker = Buffer.from('"stackLimit":');
  const start = image.indexOf(marker) + marker.length;
  assert.ok(start >= marker.length);
  let end = start;
  while (image[end] >= 48 && image[end] <= 57) end += 1;
  assert.ok(end > start);
  image.fill(57, start, end);
  await assert.rejects(runAot(image), /memory layout exceeds the admitted image/);
});


test('Core packed inline index preserves sparse order and immutable source queries', () => {
  const words = [...logicalSourceWords.slice(0, 15), 0, 0xffff_ffff, 3, 0, 1, 2, 1, 0xffff_fffe, 1];
  const inlineSources = parseAotInlineSources(sourceModule([sourcePayload(words)]), ['leaf.vo'], 2);
  const entries = inlineSources.functions.get(0);
  assert.equal(entries.size, 3);
  assert.deepEqual([...entries.keys()], [0, 2, 0xffff_fffe]);
  assert.deepEqual([...entries.values()], [1, 1, 1]);
  assert.deepEqual([...entries], [[0, 1], [2, 1], [0xffff_fffe, 1]]);
  assert.equal(entries.get(1), undefined);
  assert.equal(entries.get(0xffff_ffff), undefined);
  const seen = [];
  entries.forEach((value, key, map) => { assert.equal(map, entries); seen.push([key, value]); });
  assert.deepEqual(seen, [...entries]);
  const first = inlineSources.frames.get(1);
  first.location.line = 999;
  assert.equal(inlineSources.frames.get(1).location.line, 10);
  for (const index of [-1, 0.5, 2, NaN, Infinity]) assert.equal(inlineSources.frames.get(index), undefined);
  const debug = { inlineSources, functions: [] };
  assert.deepEqual(lookupAotLogicalSources(debug, 0, 0xffff_fffe).map(frame => frame.functionId), [1, 0]);
});
