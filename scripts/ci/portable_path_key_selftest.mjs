import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import {
  ICU_CASEMAP_VERSION,
  UNICODE_CASE_FOLD_EXCEPTIONS,
  UNICODE_CASE_FOLD_MAPPING_COUNT,
  UNICODE_CASE_FOLD_MAPPING_SHA256,
  UNICODE_CASE_FOLD_RANGES,
  UNICODE_CASE_FOLD_VERSION,
} from './unicode_casefold_data.mjs';
import { portableCaseKey, portablePathCollisionKey } from './portable_path_key.mjs';

assert.equal(UNICODE_CASE_FOLD_VERSION, '16.0.0');
assert.equal(ICU_CASEMAP_VERSION, '2.0.1');
assert.equal(UNICODE_CASE_FOLD_MAPPING_COUNT, 1557);
assert.equal(
  UNICODE_CASE_FOLD_MAPPING_SHA256,
  '20ad219e32f8b34b983fc00e75d6b4b9a33286f3ccd6a0851f6beab19a81b91a',
);
assert.equal(UNICODE_CASE_FOLD_RANGES.length % 4, 0);
assert.equal(UNICODE_CASE_FOLD_EXCEPTIONS.length % 2, 0);

const mappings = new Map();
for (let index = 0; index < UNICODE_CASE_FOLD_RANGES.length; index += 4) {
  const start = UNICODE_CASE_FOLD_RANGES[index];
  const end = UNICODE_CASE_FOLD_RANGES[index + 1];
  const step = UNICODE_CASE_FOLD_RANGES[index + 2];
  const delta = UNICODE_CASE_FOLD_RANGES[index + 3];
  assert.ok(Number.isInteger(start) && Number.isInteger(end) && start <= end);
  assert.ok(Number.isInteger(step) && step > 0);
  for (let source = start; source <= end; source += step) {
    assert.equal(mappings.has(source), false, `duplicate range mapping U+${source.toString(16)}`);
    mappings.set(source, String.fromCodePoint(source + delta));
  }
}
for (let index = 0; index < UNICODE_CASE_FOLD_EXCEPTIONS.length; index += 2) {
  const source = UNICODE_CASE_FOLD_EXCEPTIONS[index];
  const folded = UNICODE_CASE_FOLD_EXCEPTIONS[index + 1];
  assert.ok(Number.isInteger(source));
  assert.equal(typeof folded, 'string');
  assert.equal(mappings.has(source), false, `duplicate exception mapping U+${source.toString(16)}`);
  mappings.set(source, folded);
}
assert.equal(mappings.size, UNICODE_CASE_FOLD_MAPPING_COUNT);

const canonical = [...mappings]
  .sort(([left], [right]) => left - right)
  .map(([source, folded]) => {
    const targets = [...folded]
      .map((scalar) => scalar.codePointAt(0).toString(16).toUpperCase().padStart(6, '0'))
      .join(' ');
    return `${source.toString(16).toUpperCase().padStart(6, '0')}\t${targets}\n`;
  })
  .join('');
assert.equal(
  createHash('sha256').update(canonical).digest('hex'),
  UNICODE_CASE_FOLD_MAPPING_SHA256,
);
for (const [source, folded] of mappings) {
  assert.equal(
    portableCaseKey(String.fromCodePoint(source)),
    folded.normalize('NFC'),
    `full-fold mapping drift at U+${source.toString(16).toUpperCase()}`,
  );
}

assert.equal(portableCaseKey('Hello'), 'hello');
assert.equal(portableCaseKey('ß'), 'ss');
assert.equal(portableCaseKey('ẞ'), 'ss');
assert.equal(portableCaseKey('ı'), 'ı');
assert.notEqual(portableCaseKey('ı'), portableCaseKey('i'));
assert.equal(portableCaseKey('İ'), 'i\u0307');
assert.equal(portableCaseKey('İ'), portableCaseKey('i\u0307'));
assert.equal(portableCaseKey('Σ'), 'σ');
assert.equal(portableCaseKey('σ'), 'σ');
assert.equal(portableCaseKey('ς'), 'σ');
assert.equal(portableCaseKey('ﬃ'), 'ffi');
assert.equal(portableCaseKey('assets/game.vpaK'), 'assets/game.vpak');
assert.equal(portableCaseKey('A\u030A'), 'å');
assert.equal(portableCaseKey('\u{10400}'), '\u{10428}');
assert.equal(portablePathCollisionKey('Dir/ẞ.vo'), 'dir/ss.vo');

assert.throws(() => portableCaseKey('\ud800'), /isolated surrogate at UTF-16 index 0/);
assert.throws(() => portableCaseKey('\udc00'), /isolated surrogate at UTF-16 index 0/);
assert.throws(() => portableCaseKey('ok\ud800x'), /isolated surrogate at UTF-16 index 2/);
assert.throws(() => portablePathCollisionKey('ok/\udc00'), /isolated surrogate at UTF-16 index 3/);
assert.throws(() => portableCaseKey(null), /input must be a string/);

console.log(`portable path key selftest passed (${mappings.size} full-fold mappings)`);
