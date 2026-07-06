#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const suite = argValue('--suite') || 'renderer_frame';
const gate = argValue('--gate') || (suite === 'render_world' ? 'voplay-batch-planner-unit' : 'voplay-rust-unit');
const outDir = path.resolve(argValue('--out-dir') || process.env.VOPLAY_RUST_UNIT_OUT_DIR || path.join(root, 'target', gate));
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
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
mkdirSync(outDir, { recursive: true });
const logPath = path.join(outDir, 'rust-unit.log');
const result = spawnSync('cargo', args, {
  cwd: rustRoot,
  env: { ...process.env },
  encoding: 'utf8',
  stdio: ['ignore', 'pipe', 'pipe'],
});
process.stdout.write(result.stdout ?? '');
process.stderr.write(result.stderr ?? '');
writeFileSync(logPath, `${result.stdout ?? ''}${result.stderr ?? ''}`);
if (result.status !== 0) {
  fail(`cargo ${args.join(' ')} failed with exit ${result.status}`);
}
const generatedAt = new Date().toISOString();
writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify({
  schemaVersion: 1,
  kind: 'voplay.rustUnitReport',
  gate,
  suite,
  status: 'pass',
  generatedAt,
  freshEvidence: sourceBoundEvidence({
    gate,
    generatedAt,
    root,
    repos: [
      { name: 'volang', root },
      { name: 'voplay', root: voplayRoot },
    ],
    gateFiles: [
      'scripts/ci/voplay_rust_unit.mjs',
      'scripts/ci/repo_roots.mjs',
      'scripts/ci/source_bound_evidence.mjs',
      path.join(voplayRoot, 'rust/src/renderer_frame.rs'),
      path.join(voplayRoot, 'rust/src/render_world.rs'),
      'eng/tasks.toml',
      'eng/ci.toml',
    ],
    artifacts: [logPath],
  }),
  command: ['cargo', ...args],
  log: path.relative(root, logPath),
}, null, 2)}\n`);
console.log(`voplay rust unit: ok suite=${suite} root=${voplayRoot}`);
