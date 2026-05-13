#!/usr/bin/env node
import { createHash } from 'node:crypto';
import { existsSync, readFileSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));
const quickplayDir = join(root, 'apps/studio/public/quickplay/blockkart');
const projectPath = join(quickplayDir, 'project.json');
const depsPath = join(quickplayDir, 'deps.json');
const provenancePath = join(quickplayDir, 'provenance.json');
const quickplayTsPath = join(root, 'apps/studio/src/lib/quickplay.ts');

const expected = {
  projectName: 'BlockKart',
  projectModule: 'github.com/vo-lang/blockkart',
  requiredModules: [
    'github.com/vo-lang/vogui',
    'github.com/vo-lang/voplay',
  ],
  artifactName: 'studio.quickplay.blockkart',
  artifactPath: 'apps/studio/public/quickplay/blockkart',
  generatorCommand: ['vo-dev', 'task', 'run', 'task:quickplay-blockkart-package'],
  generatorInputs: [
    'apps/studio/scripts/package_blockkart_quickplay.mjs',
    'eng/project.toml',
    'external:BlockKart',
    'module-cache:voplay',
    'module-cache:vogui',
  ],
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

function localPathForArtifact(url) {
  const prefix = '/quickplay/blockkart/';
  assert(url.startsWith(prefix), `artifact URL must be quickplay-local: ${url}`);
  return join(root, 'apps/studio/public/quickplay/blockkart', url.slice(prefix.length));
}

function moduleByName(modules, moduleName) {
  const mod = modules.get(moduleName);
  assert(mod, `deps modules must include ${moduleName}`);
  assert(typeof mod.version === 'string' && mod.version.length > 0, `${moduleName} must have a version`);
  return mod;
}

function requiredVoplayArtifacts(voplay) {
  const artifacts = voplay.artifacts ?? [];
  const js = artifacts.find((artifact) => artifact.url?.endsWith('/voplay_island.js'));
  const wasm = artifacts.find((artifact) => artifact.url?.endsWith('/voplay_island_bg.wasm'));
  assert(js, 'voplay JS prefetch artifact missing from deps');
  assert(wasm, 'voplay WASM prefetch artifact missing from deps');
  return [js, wasm];
}

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function jsonFileDigest(path) {
  return sha256Digest(readFileSync(path));
}

function moduleFileBytes(file) {
  if (file.content != null) return Buffer.from(file.content, 'utf8');
  if (file.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  fail(`module file is missing content: ${file.path}`);
}

function sourceSetDigest(entries) {
  return sha256Digest(Buffer.from(JSON.stringify(entries), 'utf8'));
}

function packagedFilesDigest(files) {
  const entries = files
    .map((file) => {
      const bytes = moduleFileBytes(file);
      return {
        digest: sha256Digest(bytes),
        path: file.path,
        size: bytes.byteLength,
      };
    })
    .sort((a, b) => a.path.localeCompare(b.path));
  return sourceSetDigest(entries);
}

function sameStringArray(actual, wanted) {
  return Array.isArray(actual)
    && actual.length === wanted.length
    && actual.every((value, index) => value === wanted[index]);
}

function quickplayArtifactUrlsFromSource(source) {
  return [...source.matchAll(/\/quickplay\/blockkart\/artifacts\/[^'"`)\\\s]+/g)].map((match) => match[0]);
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

function validateProvenance(project, deps) {
  const provenance = readJson(provenancePath);
  assert(provenance.schemaVersion === 1, 'provenance schemaVersion must be 1');
  assert(provenance.artifact === expected.artifactName, `provenance artifact must be ${expected.artifactName}`);
  assert(provenance.path === expected.artifactPath, `provenance path must be ${expected.artifactPath}`);
  assert(
    sameStringArray(provenance.generator?.command, expected.generatorCommand),
    'provenance generator command mismatch',
  );
  assert(
    sameStringArray(provenance.inputs, expected.generatorInputs),
    'provenance generator inputs mismatch',
  );
  assert(provenance.project?.module === project.module, 'provenance project module mismatch');
  assert(provenance.project?.commit === project.commit, 'provenance project commit mismatch');
  assert(provenance.project?.dirty === false, 'checked-in quickplay package must not be generated from a dirty BlockKart tree');
  assert(
    provenance.project?.filesDigest === packagedFilesDigest(project.files),
    'provenance project files digest mismatch',
  );

  const outputMap = new Map((provenance.outputs ?? []).map((output) => [output.path, output]));
  const projectOutput = outputMap.get('project.json');
  const depsOutput = outputMap.get('deps.json');
  assert(projectOutput?.digest === jsonFileDigest(projectPath), 'provenance project.json digest mismatch');
  assert(projectOutput?.size === statSync(projectPath).size, 'provenance project.json size mismatch');
  assert(depsOutput?.digest === jsonFileDigest(depsPath), 'provenance deps.json digest mismatch');
  assert(depsOutput?.size === statSync(depsPath).size, 'provenance deps.json size mismatch');

  const provenanceDeps = new Map((provenance.dependencies ?? []).map((mod) => [mod.module, mod]));
  for (const mod of deps.modules) {
    const provenanceMod = provenanceDeps.get(mod.module);
    assert(provenanceMod, `provenance missing dependency ${mod.module}`);
    assert(provenanceMod.version === mod.version, `provenance version mismatch for ${mod.module}`);
    assert(provenanceMod.cacheDir === mod.cacheDir, `provenance cacheDir mismatch for ${mod.module}`);
    assert(provenanceMod.filesDigest === packagedFilesDigest(mod.files), `provenance files digest mismatch for ${mod.module}`);

    const artifactDigests = new Map((provenanceMod.artifacts ?? []).map((artifact) => [artifact.url, artifact]));
    for (const artifact of mod.artifacts ?? []) {
      const provenanceArtifact = artifactDigests.get(artifact.url);
      assert(provenanceArtifact, `provenance missing artifact ${artifact.url}`);
      assert(provenanceArtifact.path === artifact.path, `provenance artifact path mismatch for ${artifact.url}`);
      const localPath = localPathForArtifact(artifact.url);
      const bytes = readFileSync(localPath);
      assert(provenanceArtifact.size === bytes.byteLength, `provenance artifact size mismatch for ${artifact.url}`);
      assert(provenanceArtifact.digest === sha256Digest(bytes), `provenance artifact digest mismatch for ${artifact.url}`);
    }
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
const projectFiles = new Map(project.files.map((file) => [file.path, file]));
const runtimeAsset = projectFiles.get('assets/blockkart.vpak');
assert(runtimeAsset, 'project package must include assets/blockkart.vpak');
assert(moduleFileBytes(runtimeAsset).byteLength > 1024 * 1024, 'assets/blockkart.vpak must contain the runtime asset pack');
assert(!projectFiles.has('apps/studio/fixtures/blockkart/blockkart.vpak'), 'project package must not embed the Studio fixture path');

assert(Array.isArray(deps.modules) && deps.modules.length > 0, 'deps modules must be embedded');
const modules = new Map(deps.modules.map((mod) => [mod.module, mod]));
for (const moduleName of expected.requiredModules) {
  moduleByName(modules, moduleName);
}
validateProvenance(project, deps);

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

assert(quickplayTs.includes('staticPackageUrl'), 'quickplay manifests must be build-versioned');
assert(quickplayTs.includes('/quickplay/blockkart/project.json'), 'quickplay project URL missing');
assert(quickplayTs.includes('/quickplay/blockkart/deps.json'), 'quickplay deps URL missing');
for (const url of quickplayArtifactUrlsFromSource(quickplayTs)) {
  assert(false, `quickplay.ts must derive artifact URLs from deps.json, not hard-code ${url}`);
}
const declaredArtifactUrls = new Set();
for (const mod of deps.modules) {
  for (const artifact of mod.artifacts ?? []) {
    declaredArtifactUrls.add(artifact.url);
  }
}
const voplay = moduleByName(modules, 'github.com/vo-lang/voplay');
for (const artifact of requiredVoplayArtifacts(voplay)) {
  assert(declaredArtifactUrls.has(artifact.url), `voplay artifact is not declared: ${artifact.url}`);
}

console.log('quickplay validate: ok');
