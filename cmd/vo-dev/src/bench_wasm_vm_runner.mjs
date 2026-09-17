#!/usr/bin/env node

import { readFile } from 'node:fs/promises';
import { relative, resolve, sep } from 'node:path';
import init, { compileAndRun } from '../../../lang/crates/vo-web/pkg/vo_web.js';

const [sourcePath] = process.argv.slice(2);
if (!sourcePath) {
  throw new Error('usage: node bench_wasm_vm_runner.mjs SOURCE');
}
await init({ module_or_path: await readFile(new URL(
  '../../../lang/crates/vo-web/pkg/vo_web_bg.wasm', import.meta.url,
)) });
const sourceName = relative(process.cwd(), resolve(sourcePath)).split(sep).join('/');
const result = await compileAndRun(await readFile(sourcePath, 'utf8'), sourceName);
try {
  process.stdout.write(result.stdout);
  if (result.stderr) process.stderr.write(`${result.stderr}\n`);
  if (result.status !== 'ok' || (result.exitCode ?? 0) !== 0) {
    process.exitCode = result.exitCode || 1;
  }
} finally {
  result.free?.();
}
