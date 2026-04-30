#!/usr/bin/env node
import { existsSync, readFileSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));
const quickplayDir = join(root, 'studio/public/quickplay/blockkart');
const projectPath = join(quickplayDir, 'project.json');
const depsPath = join(quickplayDir, 'deps.json');
const quickplayTsPath = join(root, 'studio/src/lib/quickplay.ts');

const expected = {
  projectName: 'BlockKart',
  projectModule: 'github.com/vo-lang/blockkart',
  voplayVersion: 'v0.1.26',
  voguiVersion: 'v0.1.14',
};

function fail(message) {
  console.error(`quickplay validate: ${message}`);
  process.exit(1);
}

function readJson(path) {
  try {
    return JSON.parse(readFileSync(path, 'utf8'));
  } catch (error) {
    fail(`cannot read JSON ${path}: ${error.message}`);
  }
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function collectStrings(value, out = []) {
  if (typeof value === 'string') {
    out.push(value);
  } else if (Array.isArray(value)) {
    for (const item of value) {
      collectStrings(item, out);
    }
  } else if (value && typeof value === 'object') {
    for (const item of Object.values(value)) {
      collectStrings(item, out);
    }
  }
  return out;
}

function localPathForArtifact(url) {
  const prefix = '/quickplay/blockkart/';
  assert(url.startsWith(prefix), `artifact URL must be quickplay-local: ${url}`);
  return join(root, 'studio/public/quickplay/blockkart', url.slice(prefix.length));
}

const project = readJson(projectPath);
const deps = readJson(depsPath);
const quickplayTs = readFileSync(quickplayTsPath, 'utf8');

assert(project.schemaVersion === 1, 'project schemaVersion must be 1');
assert(deps.schemaVersion === 1, 'deps schemaVersion must be 1');
assert(project.name === expected.projectName, `project name must be ${expected.projectName}`);
assert(project.module === expected.projectModule, `project module must be ${expected.projectModule}`);
assert(Array.isArray(project.files) && project.files.length > 0, 'project files must be embedded');
assert(project.files.some((file) => file.path === 'main.vo'), 'project package must include main.vo');

assert(Array.isArray(deps.modules) && deps.modules.length > 0, 'deps modules must be embedded');
const modules = new Map(deps.modules.map((mod) => [mod.module, mod]));
assert(modules.get('github.com/vo-lang/voplay')?.version === expected.voplayVersion, `voplay must be ${expected.voplayVersion}`);
assert(modules.get('github.com/vo-lang/vogui')?.version === expected.voguiVersion, `vogui must be ${expected.voguiVersion}`);

for (const mod of deps.modules) {
  assert(typeof mod.cacheDir === 'string' && mod.cacheDir.length > 0, `${mod.module} must have cacheDir`);
  assert(Array.isArray(mod.files) && mod.files.length > 0, `${mod.module} must embed files`);
  for (const artifact of mod.artifacts ?? []) {
    assert(typeof artifact.url === 'string', `${mod.module} artifact must have url`);
    const localPath = localPathForArtifact(artifact.url);
    assert(existsSync(localPath), `missing packaged artifact: ${artifact.url}`);
    assert(statSync(localPath).size > 0, `empty packaged artifact: ${artifact.url}`);
  }
}

const allStrings = collectStrings({ project, deps, quickplayTs });
assert(!allStrings.some((value) => value.includes('v0.1.23')), 'stale voplay v0.1.23 reference found');
assert(quickplayTs.includes('staticPackageUrl'), 'quickplay manifests must be build-versioned');
assert(quickplayTs.includes('/quickplay/blockkart/project.json'), 'quickplay project URL missing');
assert(quickplayTs.includes('/quickplay/blockkart/deps.json'), 'quickplay deps URL missing');
assert(quickplayTs.includes(`/quickplay/blockkart/artifacts/github.com@vo-lang@voplay/${expected.voplayVersion}/voplay_island.js`), 'voplay JS prefetch missing');
assert(quickplayTs.includes(`/quickplay/blockkart/artifacts/github.com@vo-lang@voplay/${expected.voplayVersion}/voplay_island_bg.wasm`), 'voplay WASM prefetch missing');

console.log('quickplay validate: ok');
