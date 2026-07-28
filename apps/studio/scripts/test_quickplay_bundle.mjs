import assert from 'node:assert/strict';
import { gzipSync, gunzipSync } from 'node:zlib';

import {
  addBlockKartArtifactAliases,
  buildTar,
  selectQuickPlayFiles,
  selectWorkspaceModuleFiles,
} from './build_quickplay_bundle.mjs';

assert.deepEqual(
  selectQuickPlayFiles([
    'world.vo',
    'art/blender/source.blend',
    'art/exports/scene.glb',
    'tools/build.mjs',
    'docs/readme.md',
    'assets/blockkart.vpak',
    'assets/raw.png',
    'terrain/recipe.json',
  ]),
  ['assets/blockkart.vpak', 'terrain/recipe.json', 'world.vo'],
);

assert.deepEqual(
  selectWorkspaceModuleFiles([
    'docs/readme.md',
    'js/dist/renderer.js',
    'js/src/renderer.ts',
    'protocol/generated/runtime.ts',
    'rust/src/lib.rs',
    'scene/main.vo',
    'vo.mod',
    'web-artifacts/extension.wasm',
  ]),
  [
    'js/dist/renderer.js',
    'js/src/renderer.ts',
    'protocol/generated/runtime.ts',
    'scene/main.vo',
    'vo.mod',
    'web-artifacts/extension.wasm',
  ],
);

const audioFiles = [
  'kart_engine.wav',
  'kart_boost.wav',
  'kart_hit.wav',
  'kart_skid.wav',
  'kart_grass.wav',
].map((name, index) => ({
  path: `assets/audio/${name}`,
  content: Buffer.from([index + 1]),
  mode: 0o644,
}));
const aliases = addBlockKartArtifactAliases(audioFiles);
assert.deepEqual(
  aliases.map((file) => file.path),
  [
    'assets/424c4f434b4b4152542d415353450102.bin',
    'assets/424c4f434b4b4152542d415353450202.bin',
    'assets/424c4f434b4b4152542d415353450302.bin',
    'assets/424c4f434b4b4152542d415353450402.bin',
    'assets/424c4f434b4b4152542d415353450502.bin',
  ],
);
assert.deepEqual(aliases.map((file) => [...file.content]), [[1], [2], [3], [4], [5]]);

const payload = Buffer.from('func main() {}\n');
const archive = buildTar([{ path: 'main.vo', content: payload, mode: 0o644 }]);
assert.equal(archive.byteLength % 512, 0);
assert.equal(archive.subarray(0, 'main.vo'.length).toString(), 'main.vo');
assert.equal(archive.subarray(512, 512 + payload.length).toString(), payload.toString());
assert.ok(archive.subarray(archive.length - 1024).every((byte) => byte === 0));
assert.deepEqual(gunzipSync(gzipSync(archive, { mtime: 0 })), archive);

console.log('quick play bundle contracts: ok');
