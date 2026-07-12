#!/usr/bin/env node
import { existsSync, readFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const quickplayDeps = path.join(root, 'apps/studio/public/quickplay/blockkart/deps.json');
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const samplePath = path.join(voplayRoot, 'examples/empty_smoke/main.vo');

function fail(message) {
  console.error(`voplay sample validate: ${message}`);
  process.exit(1);
}

function assert(condition, message) {
  if (!condition) fail(message);
}

assert(existsSync(samplePath), `missing non-BlockKart voplay sample: ${samplePath}`);
const source = readFileSync(samplePath, 'utf8');
assert(source.includes('package main'), 'sample must be an executable app package');
assert(source.includes('"github.com/vo-lang/voplay"'), 'sample must import voplay directly');
assert(source.includes('voplay.Run(voplay.Game'), 'sample must use the generic voplay Game contract');
assert(source.includes('FixedUpdate(g *voplay.GameCtx'), 'sample must exercise fixed-step State contract');
assert(source.includes('Draw(g *voplay.GameCtx'), 'sample must exercise render State contract');
assert(!/BlockKart|blockkart|Race|Kart|Vehicle/.test(source), 'sample must not depend on BlockKart or racing concepts');

const deps = JSON.parse(readFileSync(quickplayDeps, 'utf8'));
const voplay = deps.modules?.find((mod) => mod.module === 'github.com/vo-lang/voplay');
assert(voplay, 'quickplay deps must include voplay');
const packagedSample = voplay.files?.find((file) => file.path === 'examples/empty_smoke/main.vo');
assert(packagedSample?.content === source, 'packaged voplay sample must match source checkout');

console.log('voplay sample validate: ok');
