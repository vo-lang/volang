#!/usr/bin/env node
import { existsSync, readFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { parseBoundedJsonBytes } from './bounded_json.mjs';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const quickplayDeps = path.join(root, 'apps/studio/public/quickplay/blockkart/deps.json');
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const sampleRoot = path.join(voplayRoot, 'examples/empty_smoke');
const samplePath = path.join(sampleRoot, 'main.vo');
const sampleManifestPath = path.join(sampleRoot, 'vo.mod');

function fail(message) {
  console.error(`voplay sample validate: ${message}`);
  process.exit(1);
}

function assert(condition, message) {
  if (!condition) fail(message);
}

assert(existsSync(samplePath), `missing non-BlockKart voplay sample: ${samplePath}`);
assert(existsSync(sampleManifestPath), `missing non-BlockKart voplay sample manifest: ${sampleManifestPath}`);
const source = readFileSync(samplePath, 'utf8');
const manifest = readFileSync(sampleManifestPath, 'utf8');
assert(
  /^module = "github\.com\/vo-lang\/voplay\/examples\/empty_smoke"\nvo = "\^0\.1\.0"\n\n\[dependencies\]\n"github\.com\/vo-lang\/voplay" = "\^0\.1\.0"\n$/.test(manifest),
  'sample must remain a canonical nested module with one explicit voplay dependency',
);
assert(source.includes('package main'), 'sample must be an executable app package');
assert(source.includes('"github.com/vo-lang/voplay"'), 'sample must import voplay directly');
assert(source.includes('voplay.Run(voplay.Game'), 'sample must use the generic voplay Game contract');
assert(source.includes('FixedUpdate(g *voplay.GameCtx'), 'sample must exercise fixed-step State contract');
assert(source.includes('Draw(g *voplay.GameCtx'), 'sample must exercise render State contract');
assert(!/BlockKart|blockkart|Race|Kart|Vehicle/.test(source), 'sample must not depend on BlockKart or racing concepts');

const deps = parseBoundedJsonBytes(
  readFileSync(quickplayDeps),
  'BlockKart quickplay deps',
  { maxBytes: 128 * 1024 * 1024 },
);
const voplay = deps.modules?.find((mod) => mod.module === 'github.com/vo-lang/voplay');
assert(voplay, 'quickplay deps must include voplay');
const nestedSampleFiles = voplay.files?.filter((file) => (
  file.path === 'examples/empty_smoke'
  || file.path.startsWith('examples/empty_smoke/')
)) ?? [];
assert(
  nestedSampleFiles.length === 0,
  'parent voplay package must prune the complete nested empty_smoke module boundary',
);

console.log('voplay sample validate: ok');
