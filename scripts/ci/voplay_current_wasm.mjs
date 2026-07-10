#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  existsSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import {
  gitCommit,
  gitDirty,
  sourceTreeDigest,
} from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const defaultOutDir = path.join(root, 'target', 'voplay-current-wasm');
export const VOPLAY_WASM_PRODUCER_COMMAND = [
  'wasm-pack',
  'build',
  '--target',
  'web',
  '--release',
  '--out-dir',
  '<OUT_DIR>',
  '--out-name',
  'voplay_island',
  '<VOPLAY_ROOT>/rust',
  '--no-default-features',
  '--features',
  'wasm-island',
];
export const VOPLAY_WASM_REQUIRED_OUTPUTS = ['voplay_island.js', 'voplay_island_bg.wasm'];

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function outputEntry(outDir, name) {
  const file = path.join(outDir, name);
  if (!existsSync(file)) return { name, missing: true };
  const bytes = readFileSync(file);
  return { name, size: bytes.byteLength, digest: sha256(bytes) };
}

function toolVersion(command, args) {
  return execFileSync(command, args, { encoding: 'utf8' }).trim();
}

export function verifyCurrentVoplayWasm({
  voplayRoot,
  outDir = defaultOutDir,
  expectedCiRunId = null,
  requireClean = false,
}) {
  const issues = [];
  const manifestPath = path.join(outDir, 'producer-manifest.json');
  if (!existsSync(manifestPath)) {
    return { issues: ['producer-manifest.json missing'], manifest: null };
  }
  let manifest;
  try {
    manifest = JSON.parse(readFileSync(manifestPath, 'utf8'));
  } catch (error) {
    return { issues: [`producer manifest parse failed: ${error.message}`], manifest: null };
  }
  if (manifest.schemaVersion !== 1) issues.push('schemaVersion must be 1');
  if (JSON.stringify(manifest.command) !== JSON.stringify(VOPLAY_WASM_PRODUCER_COMMAND)) {
    issues.push('producer command does not match canonical wasm-pack command');
  }
  if (expectedCiRunId && manifest.ciRunId !== expectedCiRunId) {
    issues.push(`ciRunId ${manifest.ciRunId ?? '(missing)'} did not match ${expectedCiRunId}`);
  }
  const currentSource = {
    commit: gitCommit(voplayRoot),
    dirty: gitDirty(voplayRoot),
    digest: sourceTreeDigest(voplayRoot),
  };
  for (const field of ['commit', 'dirty', 'digest']) {
    if (manifest.source?.[field] !== currentSource[field]) {
      issues.push(`source.${field} does not match current voplay source`);
    }
  }
  if (requireClean && currentSource.dirty) {
    issues.push('current voplay source is dirty');
  }
  const outputs = VOPLAY_WASM_REQUIRED_OUTPUTS.map((name) => outputEntry(outDir, name));
  if (JSON.stringify(manifest.outputs) !== JSON.stringify(outputs)) {
    issues.push('output digest set does not match current WASM artifacts');
  }
  if (outputs.some((output) => output.missing)) {
    issues.push('required current-source WASM output missing');
  }
  return { issues, manifest, outputs, currentSource };
}

function buildCurrentVoplayWasm() {
  const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
  const outDir = path.resolve(process.env.VOPLAY_CURRENT_WASM_OUT_DIR ?? defaultOutDir);
  rmSync(outDir, { recursive: true, force: true });
  mkdirSync(outDir, { recursive: true });
  execFileSync('wasm-pack', [
    'build',
    '--target',
    'web',
    '--release',
    '--out-dir',
    outDir,
    '--out-name',
    'voplay_island',
    path.join(voplayRoot, 'rust'),
    '--no-default-features',
    '--features',
    'wasm-island',
  ], { cwd: root, stdio: 'inherit' });
  const manifest = {
    schemaVersion: 1,
    kind: 'voplay.currentSourceWasm',
    generatedAt: new Date().toISOString(),
    ciRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
    command: VOPLAY_WASM_PRODUCER_COMMAND,
    source: {
      commit: gitCommit(voplayRoot),
      dirty: gitDirty(voplayRoot),
      digest: sourceTreeDigest(voplayRoot),
    },
    toolchain: {
      rustc: toolVersion('rustc', ['--version']),
      wasmPack: toolVersion('wasm-pack', ['--version']),
    },
    outputs: VOPLAY_WASM_REQUIRED_OUTPUTS.map((name) => outputEntry(outDir, name)),
  };
  writeFileSync(path.join(outDir, 'producer-manifest.json'), `${JSON.stringify(manifest, null, 2)}\n`);
  const verification = verifyCurrentVoplayWasm({
    voplayRoot,
    outDir,
    expectedCiRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
  });
  if (verification.issues.length > 0) {
    throw new Error(`current-source voplay WASM verification failed: ${verification.issues.join('; ')}`);
  }
  console.log(`voplay current-source WASM: ok (${path.relative(root, outDir)})`);
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? '').href) {
  buildCurrentVoplayWasm();
}
