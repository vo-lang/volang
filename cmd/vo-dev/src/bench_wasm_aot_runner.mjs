#!/usr/bin/env node

import { readFile } from 'node:fs/promises';

import { runAot } from '../../../lang/crates/vo-web/dist/index.js';

const [imagePath] = process.argv.slice(2);
if (!imagePath) {
  throw new Error('usage: node bench_wasm_aot_runner.mjs AOT_IMAGE');
}

const image = await readFile(imagePath);
const needsSupport = image.includes(Buffer.from('vo1:6:regexp:', 'utf8'));
const supportModule = needsSupport
  ? await readFile(new URL('../../../lang/crates/vo-web/aot-support/vo_aot_support_wasm_bg.wasm', import.meta.url))
  : undefined;
const execution = await runAot(image, {
  memoryLimitPages: 4096,
  ...(supportModule === undefined ? {} : { supportModule }),
});
process.stdout.write(execution.result.stdout);
if (execution.result.stderr) process.stderr.write(`${execution.result.stderr}\n`);
if (execution.exitCode !== 0) process.exit(execution.exitCode);
