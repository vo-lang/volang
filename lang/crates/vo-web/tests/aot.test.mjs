import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import test from 'node:test';

import { runAot } from '../dist/index.js';

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
  const manifest = image.indexOf(Buffer.from('VOAOTW05', 'ascii'));
  assert.ok(manifest >= 0, 'CLI image must contain the AOT manifest');
  image.writeUInt16LE(2, manifest + 8);

  await assert.rejects(runAot(image), /unsupported Volang AOT ABI 2/);
});

test('Core Wasm host rejects an image with the wrong import and export shape', async () => {
  const emptyModule = Uint8Array.from([0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00]);
  await assert.rejects(runAot(emptyModule), /imports do not match AOT ABI v5/);
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
  const metadata = image.indexOf(Buffer.from('VODBG002', 'ascii'));
  assert.ok(metadata >= 0, 'CLI image must contain compiler debug metadata');
  image.writeUInt32LE(0, metadata + 16);

  await assert.rejects(runAot(image), /invalid Volang debug frame layout/);
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
