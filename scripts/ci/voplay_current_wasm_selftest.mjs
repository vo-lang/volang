#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {
  gitCommit,
  gitDirty,
  sourceTreeDigest,
} from './source_bound_evidence.mjs';
import {
  currentVoplayWasmBuildPlatform,
  verifyCurrentVoplayWasm,
  VOPLAY_WASM_PRODUCER_COMMAND,
  VOPLAY_WASM_REQUIRED_OUTPUTS,
} from './voplay_current_wasm.mjs';

const tempRoot = mkdtempSync(path.join(os.tmpdir(), 'voplay-current-wasm-selftest-'));
const fixtureRoot = path.join(tempRoot, 'repo');
const outDir = path.join(tempRoot, 'out');

function git(args) {
  return execFileSync('git', args, { cwd: fixtureRoot, encoding: 'utf8' }).trim();
}

function digest(file) {
  return `sha256:${createHash('sha256').update(readFileSync(file)).digest('hex')}`;
}

try {
  mkdirSync(fixtureRoot);
  writeFileSync(path.join(fixtureRoot, 'renderer.rs'), 'pub fn render() {}\n');
  git(['init', '-q']);
  git(['add', 'renderer.rs']);
  git(['-c', 'user.name=WASM Test', '-c', 'user.email=wasm@example.invalid', 'commit', '-qm', 'fixture']);
  mkdirSync(outDir);
  for (const name of VOPLAY_WASM_REQUIRED_OUTPUTS) {
    writeFileSync(path.join(outDir, name), `fixture:${name}\n`);
  }
  const outputs = VOPLAY_WASM_REQUIRED_OUTPUTS.map((name) => {
    const file = path.join(outDir, name);
    return { name, size: readFileSync(file).byteLength, digest: digest(file) };
  });
  writeFileSync(path.join(outDir, 'producer-manifest.json'), `${JSON.stringify({
    schemaVersion: 1,
    kind: 'voplay.currentSourceWasm',
    generatedAt: new Date().toISOString(),
    ciRunId: 'wasm-selftest',
    command: VOPLAY_WASM_PRODUCER_COMMAND,
    source: {
      commit: gitCommit(fixtureRoot),
      dirty: gitDirty(fixtureRoot),
      digest: sourceTreeDigest(fixtureRoot),
    },
    toolchain: { rustc: 'fixture', wasmPack: 'fixture' },
    buildPlatform: currentVoplayWasmBuildPlatform(),
    outputs,
  }, null, 2)}\n`);

  assert.deepEqual(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
  }).issues, []);

  writeFileSync(path.join(fixtureRoot, 'renderer.rs'), 'pub fn render_changed() {}\n');
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
  }).issues.some((issue) => issue.includes('source.')));

  git(['checkout', '--', 'renderer.rs']);
  writeFileSync(path.join(outDir, 'voplay_island_bg.wasm'), 'stale artifact\n');
  assert(verifyCurrentVoplayWasm({
    voplayRoot: fixtureRoot,
    outDir,
    expectedCiRunId: 'wasm-selftest',
  }).issues.some((issue) => issue.includes('output digest')));

  console.log('voplay current-source WASM selftest: ok');
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
