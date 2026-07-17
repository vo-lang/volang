#!/usr/bin/env node
import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';
import { parseBoundedJsonBytes } from './bounded_json.mjs';

const encode = (value) => new TextEncoder().encode(value);
const parse = (source, options = {}) => parseBoundedJsonBytes(
  encode(source),
  'bounded JSON fixture',
  options,
);

assert.deepEqual(parse('{"array":[true,null],"text":"数据"}'), {
  array: [true, null],
  text: '数据',
});
assert.deepEqual(parse('[[]]', { maxDepth: 2 }), [[]]);
assert.throws(() => parse('[[]]', { maxDepth: 1 }), /1-level JSON depth limit/);

assert.deepEqual(parse('{"a":1}', { maxTokens: 5 }), { a: 1 });
assert.throws(() => parse('{"a":1}', { maxTokens: 4 }), /4-token JSON limit/);

assert.deepEqual(parse('{"a":1,"b":2}', { maxObjectKeys: 2 }), { a: 1, b: 2 });
assert.throws(
  () => parse('{"a":1,"b":2}', { maxObjectKeys: 1 }),
  /1-key JSON limit/,
);

assert.deepEqual(parse('{"é":1}', { maxObjectKeyBytes: 2 }), { é: 1 });
assert.throws(
  () => parse('{"é":1}', { maxObjectKeyBytes: 1 }),
  /1-byte object-key limit/,
);

const exactBytes = encode('{"value":1}');
assert.deepEqual(
  parseBoundedJsonBytes(exactBytes, 'exact byte fixture', { maxBytes: exactBytes.byteLength }),
  { value: 1 },
);
assert.throws(
  () => parseBoundedJsonBytes(exactBytes, 'oversized byte fixture', {
    maxBytes: exactBytes.byteLength - 1,
  }),
  /at most .* bytes/,
);

assert.throws(
  () => parse('{"a":1,"\\u0061":2}'),
  /duplicate object key "a"/,
);
assert.throws(
  () => parse('{"value":"\\ud800"}'),
  /JSON string must contain only Unicode scalar values/,
);
assert.throws(
  () => parse('{"\\ud800":1}'),
  /object key must contain only Unicode scalar values/,
);
assert.throws(
  () => parseBoundedJsonBytes(new Uint8Array([0xff]), 'invalid UTF-8 fixture'),
  /must be valid UTF-8/,
);
assert.throws(() => parse('1e400'), /outside the finite f64 range/);
assert.throws(() => parse('{"a":1} trailing'), /trailing JSON data/);
assert.throws(() => parse('', { maxBytes: 1 }), /contains no JSON value/);

for (const option of [
  'maxBytes',
  'maxDepth',
  'maxTokens',
  'maxObjectKeys',
  'maxObjectKeyBytes',
]) {
  assert.throws(
    () => parseBoundedJsonBytes(encode('null'), 'invalid option fixture', { [option]: 0 }),
    new RegExp(`${option} must be a positive safe integer`),
  );
}
assert.throws(
  () => parseBoundedJsonBytes('null', 'non-byte fixture'),
  /must be a Uint8Array/,
);

const moduleSource = readFileSync(
  fileURLToPath(new URL('./bounded_json.mjs', import.meta.url)),
  'utf8',
);
assert.doesNotMatch(moduleSource, /(?:from\s+|require\()['"]node:/u);
assert.doesNotMatch(moduleSource, /\bBuffer\b|\bprocess\b/u);

console.log('bounded JSON parser selftest: ok');
