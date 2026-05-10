#!/usr/bin/env node
import { createHash } from 'node:crypto';
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
  voplayVersion: 'v0.1.27',
  voguiVersion: 'v0.1.15',
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

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function moduleFileBytes(file) {
  if (file.content != null) return Buffer.from(file.content, 'utf8');
  if (file.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  fail(`module file is missing content: ${file.path}`);
}

function validatePackagedWebManifest(mod) {
  const manifestFile = mod.files.find((file) => file.path === 'vo.web.json');
  if (!manifestFile || mod.files.some((file) => file.path === 'vo.release.json')) return;

  let manifest;
  try {
    manifest = JSON.parse(manifestFile.content);
  } catch (error) {
    fail(`${mod.module} vo.web.json is invalid JSON: ${error.message}`);
  }

  assert(Array.isArray(manifest.source), `${mod.module} vo.web.json source must be an array`);
  const files = new Map(mod.files.map((file) => [file.path, file]));
  for (const entry of manifest.source) {
    const file = files.get(entry.path);
    assert(file, `${mod.module} vo.web.json declares missing source ${entry.path}`);
    const bytes = moduleFileBytes(file);
    assert(bytes.byteLength === entry.size, `${mod.module} source size mismatch for ${entry.path}`);
    assert(sha256Digest(bytes) === entry.digest, `${mod.module} source digest mismatch for ${entry.path}`);
  }
  assert(
    sha256Digest(Buffer.from(JSON.stringify(manifest.source), 'utf8')) === manifest.source_digest,
    `${mod.module} vo.web.json source_digest mismatch`,
  );

  const webArtifacts = new Map((manifest.artifacts ?? []).map((artifact) => [artifact.path, artifact]));
  for (const artifact of mod.artifacts ?? []) {
    const webArtifact = webArtifacts.get(artifact.path);
    assert(webArtifact, `${mod.module} vo.web.json does not declare packaged artifact ${artifact.path}`);
    const localPath = localPathForArtifact(artifact.url);
    const bytes = readFileSync(localPath);
    assert(bytes.byteLength === webArtifact.size, `${mod.module} artifact size mismatch for ${artifact.path}`);
    assert(sha256Digest(bytes) === webArtifact.digest, `${mod.module} artifact digest mismatch for ${artifact.path}`);
  }
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
  for (const file of mod.files) {
    assert(!file.path.includes('.codex-release-stage'), `${mod.module} packaged transient file: ${file.path}`);
  }
  for (const artifact of mod.artifacts ?? []) {
    assert(typeof artifact.url === 'string', `${mod.module} artifact must have url`);
    const localPath = localPathForArtifact(artifact.url);
    assert(existsSync(localPath), `missing packaged artifact: ${artifact.url}`);
    assert(statSync(localPath).size > 0, `empty packaged artifact: ${artifact.url}`);
  }
  validatePackagedWebManifest(mod);
}

const allStrings = collectStrings({ project, deps, quickplayTs });
assert(!allStrings.some((value) => value.includes('v0.1.23')), 'stale voplay v0.1.23 reference found');
assert(quickplayTs.includes('staticPackageUrl'), 'quickplay manifests must be build-versioned');
assert(quickplayTs.includes('/quickplay/blockkart/project.json'), 'quickplay project URL missing');
assert(quickplayTs.includes('/quickplay/blockkart/deps.json'), 'quickplay deps URL missing');
assert(quickplayTs.includes(`/quickplay/blockkart/artifacts/github.com@vo-lang@voplay/${expected.voplayVersion}/voplay_island.js`), 'voplay JS prefetch missing');
assert(quickplayTs.includes(`/quickplay/blockkart/artifacts/github.com@vo-lang@voplay/${expected.voplayVersion}/voplay_island_bg.wasm`), 'voplay WASM prefetch missing');

console.log('quickplay validate: ok');
