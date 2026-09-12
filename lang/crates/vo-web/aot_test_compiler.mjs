import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

export const repositoryRoot = fileURLToPath(new URL('../../../', import.meta.url));

export function compilerPath() {
  if (process.env.VO_TEST_WASM_AOT_COMPILER) {
    return resolve(repositoryRoot, process.env.VO_TEST_WASM_AOT_COMPILER);
  }
  const profile = process.env.VO_TEST_PROFILE === 'release' ? 'release' : 'debug';
  const executable = process.platform === 'win32' ? 'vo.exe' : 'vo';
  const targetDirectory = resolve(repositoryRoot, process.env.CARGO_TARGET_DIR || 'target');
  return join(targetDirectory, profile, executable);
}

export function compileAotSource(source) {
  const work = mkdtempSync(join(tmpdir(), 'volang-aot-test-'));
  try {
    const input = join(work, 'main.vo'), image = join(work, 'main.wasm');
    writeFileSync(input, source);
    const build = spawnSync(compilerPath(), ['build', input, '--kind=wasm', '--no-cache', '-o', image],
      { encoding: 'utf8', timeout: 60000, env: { ...process.env, VOWORK: 'off' } });
    assert.ifError(build.error);
    assert.equal(build.status, 0, build.stderr);
    return readFileSync(image);
  } finally {
    rmSync(work, { recursive: true, force: true });
  }
}
