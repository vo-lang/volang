import assert from 'node:assert/strict';
import { gzipSync, gunzipSync } from 'node:zlib';

import { buildTar, selectQuickPlayFiles, selectWorkspaceModuleFiles } from './build_quickplay_bundle.mjs';

assert.deepEqual(
  selectQuickPlayFiles([
    'world.vo',
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
    'rust/src/lib.rs',
    'scene/main.vo',
    'vo.mod',
    'web-artifacts/extension.wasm',
  ]),
  ['js/dist/renderer.js', 'scene/main.vo', 'vo.mod', 'web-artifacts/extension.wasm'],
);

const payload = Buffer.from('func main() {}\n');
const archive = buildTar([{ path: 'main.vo', content: payload, mode: 0o644 }]);
assert.equal(archive.byteLength % 512, 0);
assert.equal(archive.subarray(0, 'main.vo'.length).toString(), 'main.vo');
assert.equal(archive.subarray(512, 512 + payload.length).toString(), payload.toString());
assert.ok(archive.subarray(archive.length - 1024).every((byte) => byte === 0));
assert.deepEqual(gunzipSync(gzipSync(archive, { mtime: 0 })), archive);

console.log('quick play bundle contracts: ok');
