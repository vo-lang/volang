#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const suite = argValue('--suite') || 'vehicle-dynamics';
const outDir = path.resolve(argValue('--out-dir') || process.env.VOPLAY_SCENE3D_UNIT_OUT_DIR || path.join(root, `target/voplay-${suite}-unit`));
const gate = argValue('--gate') || path.basename(outDir);
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const voguiRoot = requireRepoRoot('VOGUI_ROOT', 'vogui');
const vopackRoot = requireRepoRoot('VOPACK_ROOT', 'vopack');
const sourceFile = path.join(voplayRoot, 'tests/main.vo');
const sourceDir = path.dirname(sourceFile);
const projectDir = path.join(outDir, 'project');
const projectEntry = path.join(projectDir, 'main.vo');

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : process.argv[index + 1] ?? '';
}

function fail(message) {
  console.error(`voplay scene3d unit: ${message}`);
  process.exit(1);
}

function checkSourceCoverage(source) {
  const suites = {
    'vehicle-dynamics': [
      'scene3d kart dynamics resolves boost and drift controls',
      'scene3d kart drift charges mini turbo',
      'scene3d kart releases turbo after charged drift',
      'scene3d kart offroad engine force lower than road',
      'scene3d vehicle intent clamps steering',
    ],
    'physics-backend': [
      'scene3d vehicle sends wheel controls',
      'scene3d physics backend apply command maps every wheel',
      'scene3d physics backend apply command emits debug hash',
      'scene3d physics backend apply command maps rigid body force',
      'scene3d physics backend apply command distributes engine force',
      'scene3d physics backend apply command maps brake force',
      'scene3d vehicle wheel state contact',
      'scene3d vehicle suspension compression',
      'scene3d vehicle longitudinal slip uses body local velocity',
      'scene3d contact events decode backend pair',
      'scene3d malformed body packet records backend packet error',
      'scene3d malformed contact packet records backend packet error',
    ],
  };
  const required = suites[suite];
  if (!required) {
    fail(`unknown --suite ${suite}; expected ${Object.keys(suites).join(', ')}`);
  }
  const missing = required.filter((token) => !source.includes(token));
  if (missing.length > 0) {
    fail(`suite ${suite} is missing expected assertions: ${missing.join(', ')}`);
  }
}

function prepareProject(source) {
  const localModules = [
    { module: 'github.com/vo-lang/voplay', dir: voplayRoot },
    { module: 'github.com/vo-lang/vogui', dir: voguiRoot },
    { module: 'github.com/vo-lang/vopack', dir: vopackRoot },
  ];
  for (const local of localModules) {
    if (!existsSync(path.join(local.dir, 'vo.mod'))) {
      fail(`local workspace module ${local.module} is missing vo.mod at ${local.dir}`);
    }
  }
  mkdirSync(projectDir, { recursive: true });
  writeFileSync(
    path.join(projectDir, 'vo.mod'),
    'module github.com/vo-lang/voplay-scene3d-unit\n\nvo ^0.1.0\n\nrequire github.com/vo-lang/voplay v0.1.31\n',
  );
  const workLines = ['version = 1', ''];
  for (const local of localModules) {
    workLines.push('[[use]]');
    workLines.push(`path = ${JSON.stringify(path.relative(projectDir, local.dir).replaceAll(path.sep, '/'))}`);
    workLines.push('');
  }
  writeFileSync(path.join(projectDir, 'vo.work'), workLines.join('\n'));
  for (const entry of readdirSync(sourceDir)) {
    if (entry.endsWith('.vo')) {
      writeFileSync(path.join(projectDir, entry), readFileSync(path.join(sourceDir, entry), 'utf8'));
    }
  }
  writeFileSync(projectEntry, source);
}

function tail(text) {
  const value = String(text ?? '');
  return value.length > 6000 ? value.slice(value.length - 6000) : value;
}

if (!existsSync(sourceFile)) {
  fail(`test source is missing at ${sourceFile}`);
}
const source = readFileSync(sourceFile, 'utf8');
checkSourceCoverage(source);
prepareProject(source);
const command = ['./d.py', 'run', path.relative(root, projectDir)];
const result = spawnSync(command[0], command.slice(1), {
  cwd: root,
  env: { ...process.env },
  encoding: 'utf8',
  stdio: ['ignore', 'pipe', 'pipe'],
});
const logPath = path.join(outDir, 'scene3d-unit.log');
writeFileSync(logPath, `${result.stdout ?? ''}${result.stderr ?? ''}`);
if (result.status !== 0) {
  fail(`suite ${suite} failed with exit ${result.status}; see ${logPath}\n${tail(result.stdout + result.stderr)}`);
}

const generatedAt = new Date().toISOString();
writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify({
  schemaVersion: 1,
  kind: 'voplay.scene3dUnitReport',
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
      { name: 'vogui', root: voguiRoot },
      { name: 'vopack', root: vopackRoot },
    ],
    gateFiles: [
      'scripts/ci/voplay_scene3d_unit.mjs',
      'scripts/ci/repo_roots.mjs',
      'scripts/ci/source_bound_evidence.mjs',
      sourceFile,
      'eng/tasks.toml',
      'eng/ci.toml',
    ],
    artifacts: [logPath],
  }),
  source: path.relative(root, sourceFile),
  project: path.relative(root, projectDir),
  log: path.relative(root, logPath),
}, null, 2)}\n`);
console.log(`voplay scene3d unit: ok suite=${suite} ${path.join(outDir, 'report.json')}`);
