import { mkdirSync, writeFileSync, existsSync } from 'node:fs';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawnSync } from 'node:child_process';

const scriptDir = dirname(fileURLToPath(import.meta.url));
const studioDir = resolve(scriptDir, '..');
const publicWasmDir = resolve(studioDir, 'public', 'wasm');
const buildIdFile = resolve(publicWasmDir, 'vo_studio_wasm.build_id');
const wasmFile = resolve(publicWasmDir, 'vo_studio_wasm_bg.wasm');
const jsFile = resolve(publicWasmDir, 'vo_studio_wasm.js');

function resolveBuildId(env) {
  const explicit = (env.VIBE_STUDIO_BUILD_ID ?? '').trim();
  if (explicit.length > 0) {
    return explicit;
  }
  const githubParts = [env.GITHUB_SHA, env.GITHUB_RUN_ID, env.GITHUB_RUN_ATTEMPT]
    .map((value) => (value ?? '').trim())
    .filter((value) => value.length > 0);
  if (githubParts.length > 0) {
    return githubParts.join('-');
  }
  return `local-${Date.now().toString(16)}`;
}

const env = { ...process.env };
const buildId = resolveBuildId(env);
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
