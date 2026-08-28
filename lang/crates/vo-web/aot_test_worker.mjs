#!/usr/bin/env node

import { mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join, resolve } from 'node:path';
import { spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';

import { runAot } from './dist/index.js';

const scriptDirectory = dirname(fileURLToPath(import.meta.url));
const repositoryRoot = resolve(scriptDirectory, '../../..');

function readRequest() {
  const request = JSON.parse(readFileSync(0, 'utf8'));
  if (typeof request?.path !== 'string' || request.path.length === 0) {
    throw new Error('AOT test worker requires a source path');
  }
  return request;
}

function compilerPath() {
  const profile = process.env.VO_TEST_PROFILE === 'release' ? 'release' : 'debug';
  const executable = process.platform === 'win32' ? 'vo.exe' : 'vo';
  return join(repositoryRoot, 'target', profile, executable);
}

async function main() {
  const request = readRequest();
  const source = resolve(repositoryRoot, request.path);
  const work = mkdtempSync(join(tmpdir(), 'volang-wasm-aot-test-'));
  const image = join(work, 'program.wasm');
  try {
    const compile = spawnSync(
      compilerPath(),
      ['build', source, '--kind=wasm', '--no-cache', '-o', image],
      {
        cwd: repositoryRoot,
        env: process.env,
        encoding: 'utf8',
        maxBuffer: 16 * 1024 * 1024,
      },
    );
    if (compile.error) throw compile.error;
    if (compile.status !== 0) {
      process.stdout.write(JSON.stringify({
        phase: 'compile',
        status: 'error',
        stdout: compile.stdout ?? '',
        stderr: compile.stderr ?? '',
        exitCode: compile.status,
      }));
      return;
    }

    const imageBytes = readFileSync(image);
    const needsSupport = imageBytes.includes(Buffer.from('vo1:6:regexp:', 'utf8'));
    const supportModule = needsSupport
      ? readFileSync(join(scriptDirectory, 'aot-support', 'vo_aot_support_wasm_bg.wasm'))
      : undefined;
    const aot = await runAot(imageBytes, {
      memoryLimitPages: 4096,
      fuel: 100_000_000n,
      ...(supportModule === undefined ? {} : { supportModule }),
    });
    process.stdout.write(JSON.stringify({
      phase: 'run',
      status: aot.result.status,
      stdout: aot.result.stdout,
      stderr: aot.result.stderr,
      exitCode: aot.exitCode,
    }));
  } finally {
    rmSync(work, { recursive: true, force: true });
  }
}

main().catch((error) => {
  process.stderr.write(`${error?.stack ?? error}\n`);
  process.exit(1);
});
