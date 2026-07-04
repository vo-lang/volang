#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));
const suite = argValue('--suite') || 'renderer_frame';
const localVoplayRoot = path.resolve(root, '..', 'voplay');
const voplayRoot = path.resolve(process.env.VOPLAY_ROOT || (existsSync(path.join(localVoplayRoot, 'vo.mod')) ? localVoplayRoot : path.join(root, 'ci_modules/voplay')));
const rustRoot = path.join(voplayRoot, 'rust');
const filters = {
  renderer_frame: 'renderer_frame',
  render_world: 'render_world',
};

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : process.argv[index + 1] ?? '';
}

function fail(message) {
  console.error(`voplay rust unit: ${message}`);
  process.exit(1);
}

if (!filters[suite]) {
  fail(`unknown --suite ${suite}; expected ${Object.keys(filters).join(', ')}`);
}
if (!existsSync(path.join(rustRoot, 'Cargo.toml'))) {
  fail(`voplay rust workspace is missing at ${rustRoot}`);
}

const args = ['test', '-p', 'vo-voplay', filters[suite]];
const result = spawnSync('cargo', args, {
  cwd: rustRoot,
  env: { ...process.env },
  stdio: 'inherit',
});
if (result.status !== 0) {
  fail(`cargo ${args.join(' ')} failed with exit ${result.status}`);
}
console.log(`voplay rust unit: ok suite=${suite} root=${voplayRoot}`);
