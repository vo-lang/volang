import { mkdirSync, writeFileSync, existsSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';
import { resolveStudioBuildId } from './studio_build_id.mjs';

const scriptDir = dirname(fileURLToPath(import.meta.url));
const studioDir = resolve(scriptDir, '..');
const publicWasmDir = resolve(studioDir, 'public', 'wasm');
const buildIdFile = resolve(publicWasmDir, 'vo_studio_wasm.build_id');
const wasmFile = resolve(publicWasmDir, 'vo_studio_wasm_bg.wasm');
const jsFile = resolve(publicWasmDir, 'vo_studio_wasm.js');

const env = { ...process.env };
const buildId = resolveStudioBuildId(env, { studioRoot: studioDir });
env.VIBE_STUDIO_BUILD_ID = buildId;

const result = spawnSync(
  'wasm-pack',
  ['build', 'wasm', '--target', 'web', '--out-dir', '../public/wasm', '--out-name', 'vo_studio_wasm', '--release'],
  {
    cwd: studioDir,
    env,
    stdio: 'inherit',
  },
);
if (result.status !== 0) {
  process.exit(result.status ?? 1);
}
if (!existsSync(wasmFile)) {
  throw new Error(`missing wasm output: ${wasmFile}`);
}
if (!existsSync(jsFile)) {
  throw new Error(`missing JS output: ${jsFile}`);
}
mkdirSync(publicWasmDir, { recursive: true });
writeFileSync(buildIdFile, `${buildId}\n`);
console.log(`studio wasm build id: ${buildId}`);
