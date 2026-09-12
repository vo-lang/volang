import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';
import { parseAotManifest } from '../dist/aot_metadata.js';

async function mapFixture(kind, capacity = 1024) {
  assert.ok(process.env.VO_AOT_IMAGE, 'VO_AOT_IMAGE must name the CLI-produced image');
  const module = new WebAssembly.Module(await readFile(process.env.VO_AOT_IMAGE));
  const pages = parseAotManifest(module).memoryPages;
  // Lookup touches only caller-owned records. Keep diagnostic backing outside
  // the image's heap and stack; no allocator or collector executes here.
  const memory = new WebAssembly.Memory({ initial: pages + 3, maximum: pages + 3 });
  const instance = new WebAssembly.Instance(module, {
    'volang:runtime/v3': { memory, 'call-extern': () => { throw new Error('Map lookup crossed a host boundary'); } },
  });
  const lookup = instance.exports.vo_map_lookup;
  assert.equal(typeof lookup, 'function');
  const bytes = new Uint8Array(memory.buffer), view = new DataView(memory.buffer);
  const map = pages * 65536, key = map + 128, backing = map + 4096, empty = backing + 65536;
  const bucketBytes = 24;
  assert.ok(capacity > 0 && capacity <= 1024 && (capacity & (capacity - 1)) === 0);
  view.setBigUint64(map + 8, BigInt(capacity), true);
  view.setBigUint64(map + 16, 8n, true);
  view.setBigUint64(map + 24, 8n, true);
  view.setBigUint64(map + 40, BigInt(kind), true);
  view.setUint32(map + 48, kind, true);
  const reset = () => { bytes.fill(0, backing, backing + capacity * bucketBytes); view.setBigUint64(map, 0n, true); };
  const query = (table, insert) => {
    view.setBigUint64(map + 32, BigInt(table), true);
    return lookup(map, key, Number(insert)) >>> 0;
  };
  const encode = value => {
    view.setBigUint64(key, 0n, true);
    if (kind === 12) view.setFloat32(key, value, true);
    else if (kind === 13) view.setFloat64(key, value, true);
    else view.setBigUint64(key, BigInt.asUintN(64, value), true);
  };
  const insert = value => {
    const address = query(backing, true);
    assert.ok(address >= backing && address < backing + capacity * bucketBytes);
    assert.equal((address - backing) % bucketBytes, 0);
    assert.equal(view.getBigUint64(address, true), 0n);
    view.setBigUint64(address, 1n, true);
    bytes.copyWithin(address + 8, key, key + 8);
    view.setBigUint64(address + 16, BigInt(value), true);
    view.setBigUint64(map, view.getBigUint64(map, true) + 1n, true);
    return address;
  };
  return { view, key, backing, empty, capacity, bucketBytes, query, encode, insert, reset };
}

test('generated floating Map keys retain high-bit entropy at 75 percent occupancy', async () => {
  for (const kind of [12, 13]) {
    const f = await mapFixture(kind);
    for (const stride of [1, 17]) {
      f.reset();
      for (let i = 0; i < 768; i++) { f.encode(i * stride + 0.25); f.insert(i * 3 + 1); }
      let probes = 0;
      for (let i = 0; i < 768; i++) {
        f.encode(i * stride + 0.25);
        const home = (f.query(f.empty, true) - f.empty) / f.bucketBytes;
        const address = f.query(f.backing, false);
        assert.ok(address);
        assert.equal(f.view.getBigUint64(address + 16, true), BigInt(i * 3 + 1));
        const found = (address - f.backing) / f.bucketBytes;
        probes += ((found - home + f.capacity) % f.capacity) + 1;
      }
      assert.ok(probes / 768 <= 8, `kind ${kind}, stride ${stride}: ${probes / 768} probes per hit`);
      for (let i = 768; i < 1024; i++) { f.encode(i * stride + 0.25); assert.equal(f.query(f.backing, false), 0); }
    }
  }
});

test('generated floating Map hashing preserves zero, NaN and float32 slot semantics', async () => {
  for (const kind of [12, 13]) {
    const f = await mapFixture(kind);
    f.encode(0); const zero = f.insert(7);
    f.encode(-0); assert.equal(f.query(f.backing, false), zero);
    f.view.setBigUint64(f.key, kind === 12 ? 0x7fc00001n : 0x7ff8000000000001n, true);
    const first = f.insert(11);
    assert.equal(f.query(f.backing, false), 0);
    assert.notEqual(f.insert(13), first, 'NaN never equals an existing key');
    if (kind === 12) {
      f.encode(1.25); const expected = f.insert(17);
      f.view.setUint32(f.key + 4, 0xdeadbeef, true);
      assert.equal(f.query(f.backing, false), expected, 'unused high bits cannot change a float32 key');
    }
  }
});


test('generated single-slot Map equality preserves canonical integer and reference bits', async () => {
  const kinds = [
    [1, 1, false],
    [2, 64, true], [3, 8, true], [4, 16, true], [5, 32, true], [6, 64, true],
    [7, 64, false], [8, 8, false], [9, 16, false], [10, 32, false], [11, 64, false],
    // Reference identities are compared without dereferencing their contents.
    [20, 32, false], [22, 32, false], [23, 32, false], [24, 32, false],
  ];
  for (const [kind, width, signed] of kinds) {
    const canonical = n => BigInt.asUintN(64, signed ? BigInt.asIntN(width, n) : BigInt.asUintN(width, n));
    const seeds = [0n, 1n, -1n, (1n << BigInt(width - 1)) - 1n, 1n << BigInt(width - 1)];
    for (let i = 1; i < 64; i++) seeds.push(1n << BigInt(i), (1n << BigInt(i)) | 1n);
    const values = [...new Set(seeds.map(canonical))];
    for (const capacity of [1, 2, 16, 1024]) {
      const f = await mapFixture(kind, capacity);
      const count = Math.min(capacity, values.length - 1), addresses = [];
      for (let i = 0; i < count; i++) {
        f.encode(values[i]);
        assert.equal(f.query(f.backing, false), 0);
        addresses.push(f.insert(i + 1));
      }
      for (let i = 0; i < count; i++) {
        f.encode(values[i]);
        assert.equal(f.query(f.backing, false), addresses[i]);
        assert.equal(f.query(f.backing, true), addresses[i]);
      }
      f.encode(values[count]);
      assert.equal(f.query(f.backing, false), 0);
      if (count === capacity) assert.equal(f.query(f.backing, true), 0, 'a full table must terminate');
      for (let i = 0; i < count; i += 2) f.view.setBigUint64(addresses[i], 2n, true);
      for (let i = 0; i < count; i++) {
        f.encode(values[i]);
        assert.equal(f.query(f.backing, false), i % 2 ? addresses[i] : 0, 'tombstones retain the probe chain');
      }
    }
  }
});

test('generated integer Map lookup distributes dense, strided and high-bit keys', async () => {
  const f = await mapFixture(11);
  const patterns = [i => i, i => -i - 1n, i => i * 17n, i => i * 1024n,
    i => i << 32n, i => i << 48n, i => (1n << 63n) + i];
  for (const pattern of patterns) {
    f.reset();
    for (let i = 0; i < 768; i++) { f.encode(pattern(BigInt(i))); f.insert(i + 1); }
    let probes = 0;
    for (let i = 0; i < 768; i++) {
      f.encode(pattern(BigInt(i)));
      const home = (f.query(f.empty, true) - f.empty) / f.bucketBytes;
      const address = f.query(f.backing, false);
      assert.ok(address);
      assert.equal(f.view.getBigUint64(address + 16, true), BigInt(i + 1));
      probes += (((address - f.backing) / f.bucketBytes - home + f.capacity) % f.capacity) + 1;
    }
    assert.ok(probes / 768 <= 8, `${probes / 768} probes per integer hit`);
    for (let i = 768; i < 1024; i++) { f.encode(pattern(BigInt(i))); assert.equal(f.query(f.backing, false), 0); }
  }
});
