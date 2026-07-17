#!/usr/bin/env node
import assert from 'node:assert/strict';
import { mkdirSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  BLOCKKART_VPAK_PATH,
  BLOCKKART_VPAK_PRODUCER_PATH,
  QUICKPLAY_ARTIFACT_INPUTS,
  QUICKPLAY_GENERATOR_VERSION,
  MAX_MODULE_ARTIFACT_BYTES,
  QUICKPLAY_PROJECT_VFS_ROOT,
  QUICKPLAY_WORKSPACE_MODULE_VFS_ROOT,
  canonicalProtocolJsonText,
  canonicalVoLock,
  encodeStaticFile,
  outputFacts,
  quickplaySourceDigests,
  quickplayWorkspaceFile,
  sha256Digest,
  validateProjectSnapshot,
  validateRelease,
} from './quickplay_vnext.mjs';
import {
  modulePathCacheKey,
  validateExactVersion,
  validateModulePath,
} from './quickplay_artifact_paths.mjs';
import { validateQuickplay } from './quickplay_validate.mjs';
import { protocolWasmFixture } from './wasm_protocol_v3_fixtures.mjs';

const command = ['vo-dev', 'task', 'run', 'task:quickplay-blockkart-package'];
const VALID_STANDALONE_WASM = protocolWasmFixture({ standaloneExportKeys: [] });
const repoRoot = fileURLToPath(new URL('../..', import.meta.url));
const studioProtocolSource = readFileSync(
  path.join(repoRoot, 'apps/studio/src/lib/quickplay_package_validation.ts'),
  'utf8',
);
for (const [name, expected] of [
  ['QUICKPLAY_PROJECT_VFS_ROOT', QUICKPLAY_PROJECT_VFS_ROOT],
  ['QUICKPLAY_WORKSPACE_MODULE_VFS_ROOT', QUICKPLAY_WORKSPACE_MODULE_VFS_ROOT],
]) {
  const match = new RegExp(`^export const ${name} = '([^']+)';$`, 'mu').exec(studioProtocolSource);
  assert.equal(match?.[1], expected, `${name} must match between generator and Studio`);
}

for (const repository of ['v0', 'v1', 'v2', 'v02']) {
  const module = `github.com/acme/${repository}`;
  assert.equal(modulePathCacheKey(module).suffixMajor, null);
  assert.equal(validateModulePath(module, 1n), module.replaceAll('/', '@'));
}
assert.equal(modulePathCacheKey('github.com/acme/lib/v2').suffixMajor, 2n);
for (const module of [
  'github.com/acme/lib/foo..bar',
  'github.com/acme/lib/foo.lock',
  'github.com/acme/lib/v0',
  'github.com/acme/lib/v1',
  'github.com/acme/lib/v02',
  'github.com/acme/lib/v18446744073709551616',
]) {
  assert.throws(() => modulePathCacheKey(module), /Invalid/);
}
const maxExactVersion = `0.1.0-${'a'.repeat(254 - '0.1.0-'.length)}`;
assert.equal(Buffer.byteLength(maxExactVersion), 254);
assert.equal(validateExactVersion(maxExactVersion), 0n);
assert.throws(() => validateExactVersion(`${maxExactVersion}a`), /Invalid exact module version/);
for (const version of ['1.0.0-RC.1', '1.0.0-alpha.lock']) {
  assert.throws(() => validateExactVersion(version), /Invalid exact module version/);
}
assert.equal(
  canonicalProtocolJsonText({ value: '数据\u0001\n' }),
  '{\n  "value": "数据\\u0001\\n"\n}',
);
assert.throws(
  () => canonicalProtocolJsonText({ value: '\ud800' }),
  /Unicode scalar values/,
);
let overlyDeepJson = null;
for (let depth = 0; depth <= 127; depth += 1) overlyDeepJson = [overlyDeepJson];
assert.throws(() => canonicalProtocolJsonText(overlyDeepJson), /depth exceeds 127/);

function writeJson(file, value) {
  writeFileSync(file, `${JSON.stringify(value, null, 2)}\n`);
}

const BLOCKKART_VPAK_FIXTURE_BYTES = Buffer.from('validated BlockKart VPAK fixture', 'utf8');

function blockKartVpakProjectFiles() {
  const producerIntent = {
    schemaVersion: 1,
    kind: 'blockkart.vpakProducerManifest',
    owner: 'BlockKart',
    command: ['vo', 'run', 'tools/pack_primitive_assets.vo'],
    pack: {
      path: BLOCKKART_VPAK_PATH,
      sha256: sha256Digest(BLOCKKART_VPAK_FIXTURE_BYTES).slice('sha256:'.length),
      size: BLOCKKART_VPAK_FIXTURE_BYTES.byteLength,
    },
    inputs: [{ path: 'main.vo' }],
    workspaceSourceInputCount: 1,
    payloadInputCount: 37,
    archiveEntryCount: 37,
    archiveEntries: Array.from({ length: 37 }, (_, index) => ({ path: `entry-${index}` })),
    internalManifest: {},
    upstream: [],
  };
  const producer = {
    ...producerIntent,
    producerDigest: sha256Digest(Buffer.from(JSON.stringify(producerIntent), 'utf8'))
      .slice('sha256:'.length),
  };
  return [
    encodeStaticFile(BLOCKKART_VPAK_PATH, BLOCKKART_VPAK_FIXTURE_BYTES),
    encodeStaticFile(
      BLOCKKART_VPAK_PRODUCER_PATH,
      Buffer.from(`${JSON.stringify(producer, null, 2)}\n`, 'utf8'),
    ),
  ];
}

function refreshProvenance(root) {
  const project = JSON.parse(readFileSync(path.join(root, 'project.json'), 'utf8'));
  const deps = JSON.parse(readFileSync(path.join(root, 'deps.json'), 'utf8'));
  const provenancePath = path.join(root, 'provenance.json');
  const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
  provenance.sourceDigests = quickplaySourceDigests(project, deps, repoRoot);
  provenance.outputs = outputFacts(root, provenance.outputs.map((entry) => entry.path));
  writeJson(provenancePath, provenance);
}

function replaceWorkspaceFixtureWasm(root, bytes) {
  const artifactRelative = 'artifacts/workspace/github.com@acme@lib/extension-wasm/wasm32-unknown-unknown/lib.wasm';
  const artifactPath = path.join(root, ...artifactRelative.split('/'));
  writeFileSync(artifactPath, bytes);
  const depsPath = path.join(root, 'deps.json');
  const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
  deps.modules[0].artifacts[0].size = bytes.byteLength;
  deps.modules[0].artifacts[0].digest = sha256Digest(bytes);
  writeJson(depsPath, deps);
  refreshProvenance(root);
}

function fixture(root) {
  rmSync(root, { recursive: true, force: true });
  mkdirSync(root, { recursive: true });
  const module = 'github.com/acme/lib';
  const snapshot = {
    schema_version: 2,
    mode: 'effective',
    authority: 'workspace',
    root: {
      module: 'github.com/vo-lang/blockkart',
      vo: '^0.2.0',
      dependencies: [{ module, constraint: '0.1.0' }],
    },
    workspace: { file: 'vo.work' },
    modules: [{
      module,
      vo: '^0.2.0',
      source: {
        kind: 'workspace',
        directory: '../modules/github.com@acme@lib',
      },
      dependencies: [],
    }],
  };
  const project = {
    schemaVersion: 2,
    name: 'BlockKart',
    module: 'github.com/vo-lang/blockkart',
    baseCommit: '1'.repeat(40),
    snapshot,
    files: [
      ...blockKartVpakProjectFiles(),
      encodeStaticFile('main.vo', Buffer.from('package main\n', 'utf8')),
      encodeStaticFile('vo.mod', Buffer.from('module = "github.com/vo-lang/blockkart"\nvo = "^0.2.0"\n\n[dependencies]\n"github.com/acme/lib" = "0.1.0"\n', 'utf8')),
      encodeStaticFile('vo.work', Buffer.from('version = 1\nmembers = ["../modules/github.com@acme@lib"]\n', 'utf8')),
    ],
  };
  const deps = {
    schemaVersion: 2,
    name: 'BlockKart dependencies',
    snapshotDigest: sha256Digest(Buffer.from(JSON.stringify(snapshot), 'utf8')),
    modules: [{
      source: 'workspace',
      module,
      cacheDir: 'workspace/github.com@acme@lib',
      files: [encodeStaticFile('vo.mod', Buffer.from('module = "github.com/acme/lib"\nvo = "^0.2.0"\n', 'utf8'))],
      artifacts: [{
        kind: 'extension-wasm',
        target: 'wasm32-unknown-unknown',
        name: 'lib.wasm',
        size: VALID_STANDALONE_WASM.byteLength,
        digest: sha256Digest(VALID_STANDALONE_WASM),
        path: 'web-artifacts/lib.wasm',
        url: '/quickplay/blockkart/artifacts/workspace/github.com@acme@lib/extension-wasm/wasm32-unknown-unknown/lib.wasm',
      }],
    }],
  };
  const artifactPath = path.join(root, 'artifacts', 'workspace', 'github.com@acme@lib', 'extension-wasm', 'wasm32-unknown-unknown', 'lib.wasm');
  mkdirSync(path.dirname(artifactPath), { recursive: true });
  writeFileSync(artifactPath, VALID_STANDALONE_WASM);
  writeJson(path.join(root, 'project.json'), project);
  writeJson(path.join(root, 'deps.json'), deps);
  const outputs = outputFacts(root, [
    'artifacts/workspace/github.com@acme@lib/extension-wasm/wasm32-unknown-unknown/lib.wasm',
    'deps.json',
    'project.json',
  ]);
  writeJson(path.join(root, 'provenance.json'), {
    schemaVersion: 2,
    artifact: 'studio.quickplay.blockkart',
    path: 'apps/studio/public/quickplay/blockkart',
    task: { id: 'quickplay-blockkart-package', command },
    generator: { version: QUICKPLAY_GENERATOR_VERSION, command },
    toolchain: { snapshotSchema: 2, releaseSchema: 2, packageSchema: 1 },
    sourceDigests: quickplaySourceDigests(project, deps, repoRoot),
    inputs: [...QUICKPLAY_ARTIFACT_INPUTS],
    outputs,
  });
}

function registryFixture(root) {
  rmSync(root, { recursive: true, force: true });
  mkdirSync(root, { recursive: true });
  const module = 'github.com/acme/lib';
  const version = '0.1.0';
  const wasm = VALID_STANDALONE_WASM;
  const modBytes = Buffer.from('module = "github.com/acme/lib"\nvo = "^0.2.0"\n', 'utf8');
  const packageManifest = {
    schema_version: 1,
    files: [{
      path: 'vo.mod',
      mode: 'regular',
      size: modBytes.byteLength,
      digest: sha256Digest(modBytes),
    }],
  };
  const packageBytes = Buffer.from(`${canonicalProtocolJsonText(packageManifest)}\n`, 'utf8');
  const release = {
    schema_version: 2,
    module,
    version,
    commit: '2'.repeat(40),
    vo: '^0.2.0',
    dependencies: [],
    source: { name: 'source.tar.gz', size: 1, digest: `sha256:${'3'.repeat(64)}` },
    package: { size: packageBytes.byteLength, digest: sha256Digest(packageBytes) },
    artifacts: [{
      kind: 'extension-wasm',
      target: 'wasm32-unknown-unknown',
      name: 'lib.wasm',
      size: wasm.byteLength,
      digest: sha256Digest(wasm),
    }],
  };
  const releaseBytes = Buffer.from(`${canonicalProtocolJsonText(release)}\n`, 'utf8');
  const snapshot = {
    schema_version: 2,
    mode: 'effective',
    authority: 'lock',
    root: {
      module: 'github.com/vo-lang/blockkart',
      vo: '^0.2.0',
      dependencies: [{ module, constraint: version }],
    },
    modules: [{
      module,
      version,
      vo: '^0.2.0',
      release: sha256Digest(releaseBytes),
      source: { kind: 'registry', directory: 'github.com@acme@lib/0.1.0' },
      dependencies: [],
    }],
  };
  const project = {
    schemaVersion: 2,
    name: 'BlockKart',
    module: 'github.com/vo-lang/blockkart',
    baseCommit: '1'.repeat(40),
    snapshot,
    files: [
      ...blockKartVpakProjectFiles(),
      encodeStaticFile('main.vo', Buffer.from('package main\n', 'utf8')),
      encodeStaticFile('vo.lock', Buffer.from(canonicalVoLock(snapshot), 'utf8')),
      encodeStaticFile('vo.mod', Buffer.from('module = "github.com/vo-lang/blockkart"\nvo = "^0.2.0"\n\n[dependencies]\n"github.com/acme/lib" = "0.1.0"\n', 'utf8')),
    ],
  };
  const cacheDir = 'github.com@acme@lib/0.1.0';
  const artifactRelative = `artifacts/${cacheDir}/extension-wasm/wasm32-unknown-unknown/lib.wasm`;
  const deps = {
    schemaVersion: 2,
    name: 'BlockKart dependencies',
    snapshotDigest: sha256Digest(Buffer.from(JSON.stringify(snapshot), 'utf8')),
    modules: [{
      source: 'registry',
      module,
      version,
      cacheDir,
      files: [
        encodeStaticFile('.vo-source-digest', Buffer.from(`${release.source.digest}\n`, 'utf8')),
        encodeStaticFile('.vo-version', Buffer.from(`${version}\n`, 'utf8')),
        encodeStaticFile('vo.mod', modBytes),
        encodeStaticFile('vo.package.json', packageBytes),
        encodeStaticFile('vo.release.json', releaseBytes),
      ].sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path))),
      artifacts: [{
        ...release.artifacts[0],
        path: 'artifacts/extension-wasm/wasm32-unknown-unknown/lib.wasm',
        url: '/quickplay/blockkart/artifacts/github.com@acme@lib/0.1.0/extension-wasm/wasm32-unknown-unknown/lib.wasm',
      }],
    }],
  };
  const artifactPath = path.join(root, ...artifactRelative.split('/'));
  mkdirSync(path.dirname(artifactPath), { recursive: true });
  writeFileSync(artifactPath, wasm);
  writeJson(path.join(root, 'project.json'), project);
  writeJson(path.join(root, 'deps.json'), deps);
  writeJson(path.join(root, 'provenance.json'), {
    schemaVersion: 2,
    artifact: 'studio.quickplay.blockkart',
    path: 'apps/studio/public/quickplay/blockkart',
    task: { id: 'quickplay-blockkart-package', command },
    generator: { version: QUICKPLAY_GENERATOR_VERSION, command },
    toolchain: { snapshotSchema: 2, releaseSchema: 2, packageSchema: 1 },
    sourceDigests: quickplaySourceDigests(project, deps, repoRoot),
    inputs: [...QUICKPLAY_ARTIFACT_INPUTS],
    outputs: outputFacts(root, [artifactRelative, 'deps.json', 'project.json']),
  });
}

function releaseValidationFixture(artifacts) {
  const module = {
    module: 'github.com/acme/lib',
    version: '0.1.0',
    vo: '^0.2.0',
    dependencies: [],
  };
  const release = {
    schema_version: 2,
    module: module.module,
    version: module.version,
    commit: '2'.repeat(40),
    vo: module.vo,
    dependencies: [],
    source: { name: 'source.tar.gz', size: 1, digest: `sha256:${'3'.repeat(64)}` },
    package: { size: 1, digest: `sha256:${'4'.repeat(64)}` },
    artifacts,
  };
  return { module, release };
}

function graphEdgeLimitFixture() {
  const moduleNames = Array.from(
    { length: 10_000 },
    (_, index) => `github.com/acme/m${String(index).padStart(5, '0')}`,
  );
  const dependency = (module) => ({ module, constraint: '0.1.0' });
  return {
    schema_version: 2,
    mode: 'effective',
    authority: 'lock',
    root: {
      module: 'github.com/vo-lang/blockkart',
      vo: '^0.2.0',
      dependencies: moduleNames.map(dependency),
    },
    modules: moduleNames.map((module, index) => ({
      module,
      version: '0.1.0',
      vo: '^0.2.0',
      release: `sha256:${'9'.repeat(64)}`,
      source: { kind: 'registry', directory: `registry/${index}` },
      dependencies: moduleNames
        .slice(0, 10)
        .filter((candidate) => candidate !== module)
        .slice(0, 9)
        .map(dependency),
    })),
  };
}

const root = mkdtempSync(path.join(os.tmpdir(), 'quickplay-vnext-selftest-'));
try {
  const projectPath = path.join(root, 'project.json');
  const depsPath = path.join(root, 'deps.json');
  fixture(root);
  assert.equal(validateQuickplay(root).modules, 1);

  fixture(root);
  replaceWorkspaceFixtureWasm(root, protocolWasmFixture({
    includeProtocol: false,
    standaloneExportKeys: [],
    customSectionText: 'vo_ext_protocol_version',
  }));
  assert.throws(
    () => validateQuickplay(root),
    /missing vo_ext_protocol_version/,
  );

  fixture(root);
  replaceWorkspaceFixtureWasm(root, protocolWasmFixture({
    protocolVersion: 2,
    standaloneExportKeys: [],
  }));
  assert.throws(
    () => validateQuickplay(root),
    /pure body i32\.const 3/,
  );

  fixture(root);
  const missingExactWrapper = JSON.parse(readFileSync(depsPath, 'utf8'));
  missingExactWrapper.modules[0].files.push(
    encodeStaticFile('api.vo', Buffer.from('package lib\n\nfunc missingWrapper()\n', 'utf8')),
  );
  missingExactWrapper.modules[0].files.sort(
    (left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)),
  );
  writeJson(depsPath, missingExactWrapper);
  refreshProvenance(root);
  assert.throws(
    () => validateQuickplay(root),
    /exact standalone export allowlist differs from its Vo extern catalog/,
  );

  fixture(root);
  replaceWorkspaceFixtureWasm(root, protocolWasmFixture({
    standaloneExportKeys: [],
    extraFunctionExports: ['legacyShortExport'],
  }));
  assert.throws(
    () => validateQuickplay(root),
    /exact standalone export allowlist differs from its Vo extern catalog/,
  );

  fixture(root);
  const mismatchedVpak = JSON.parse(readFileSync(projectPath, 'utf8'));
  const vpakIndex = mismatchedVpak.files.findIndex((entry) => entry.path === BLOCKKART_VPAK_PATH);
  assert.notEqual(vpakIndex, -1);
  mismatchedVpak.files[vpakIndex] = encodeStaticFile(
    BLOCKKART_VPAK_PATH,
    Buffer.from('different VPAK bytes', 'utf8'),
  );
  writeJson(projectPath, mismatchedVpak);
  assert.throws(
    () => validateQuickplay(root),
    /VPAK bytes do not match their producer manifest/,
  );

  fixture(root);
  const mismatchedProducer = JSON.parse(readFileSync(projectPath, 'utf8'));
  const producerIndex = mismatchedProducer.files.findIndex(
    (entry) => entry.path === BLOCKKART_VPAK_PRODUCER_PATH,
  );
  assert.notEqual(producerIndex, -1);
  const producerEntry = blockKartVpakProjectFiles().find(
    (entry) => entry.path === BLOCKKART_VPAK_PRODUCER_PATH,
  );
  const producerBytes = producerEntry.content === undefined
    ? Buffer.from(producerEntry.contentBase64, 'base64')
    : Buffer.from(producerEntry.content, 'utf8');
  const producerValue = JSON.parse(producerBytes.toString('utf8'));
  producerValue.producerDigest = '0'.repeat(64);
  mismatchedProducer.files[producerIndex] = encodeStaticFile(
    BLOCKKART_VPAK_PRODUCER_PATH,
    Buffer.from(`${JSON.stringify(producerValue, null, 2)}\n`, 'utf8'),
  );
  writeJson(projectPath, mismatchedProducer);
  assert.throws(() => validateQuickplay(root), /producerDigest does not bind/);

  fixture(root);
  const missingStaticMode = JSON.parse(readFileSync(projectPath, 'utf8'));
  delete missingStaticMode.files[0].mode;
  writeJson(projectPath, missingStaticMode);
  assert.throws(() => validateQuickplay(root), /unexpected fields/);

  fixture(root);
  const unknownStaticMode = JSON.parse(readFileSync(projectPath, 'utf8'));
  unknownStaticMode.files[0].mode = 0o640;
  writeJson(projectPath, unknownStaticMode);
  assert.throws(() => validateQuickplay(root), /mode is invalid/);
  const missingAuthority = JSON.parse(readFileSync(projectPath, 'utf8'));
  delete missingAuthority.snapshot.authority;
  writeJson(projectPath, missingAuthority);
  assert.throws(() => validateQuickplay(root), /authority|unexpected fields/);

  registryFixture(root);
  const missingRegistryLock = JSON.parse(readFileSync(projectPath, 'utf8'));
  missingRegistryLock.files = missingRegistryLock.files.filter((entry) => entry.path !== 'vo.lock');
  writeJson(projectPath, missingRegistryLock);
  assert.throws(() => validateQuickplay(root), /must carry vo\.lock v3/);

  registryFixture(root);
  const mismatchedLock = JSON.parse(readFileSync(projectPath, 'utf8'));
  const lockIndex = mismatchedLock.files.findIndex((entry) => entry.path === 'vo.lock');
  mismatchedLock.files[lockIndex] = encodeStaticFile(
    'vo.lock',
    Buffer.from(`${canonicalVoLock(mismatchedLock.snapshot)}# forged\n`, 'utf8'),
  );
  writeJson(projectPath, mismatchedLock);
  assert.throws(() => validateQuickplay(root), /canonical v3.*project snapshot/);

  fixture(root);
  const rootVoDrift = JSON.parse(readFileSync(projectPath, 'utf8'));
  const rootVoModIndex = rootVoDrift.files.findIndex((entry) => entry.path === 'vo.mod');
  rootVoDrift.files[rootVoModIndex] = encodeStaticFile(
    'vo.mod',
    Buffer.from('module = "github.com/vo-lang/blockkart"\nvo = "^0.3.0"\n\n[dependencies]\n"github.com/acme/lib" = "0.1.0"\n', 'utf8'),
  );
  writeJson(projectPath, rootVoDrift);
  assert.throws(() => validateQuickplay(root), /canonically match snapshot root intent/);

  fixture(root);
  const rootDependencyDrift = JSON.parse(readFileSync(projectPath, 'utf8'));
  const rootDependencyModIndex = rootDependencyDrift.files.findIndex((entry) => entry.path === 'vo.mod');
  rootDependencyDrift.files[rootDependencyModIndex] = encodeStaticFile(
    'vo.mod',
    Buffer.from('module = "github.com/vo-lang/blockkart"\nvo = "^0.2.0"\n\n[dependencies]\n"github.com/acme/lib" = "0.2.0"\n', 'utf8'),
  );
  writeJson(projectPath, rootDependencyDrift);
  assert.throws(() => validateQuickplay(root), /canonically match snapshot root intent/);

  fixture(root);
  const workspaceIdentityDrift = JSON.parse(readFileSync(path.join(root, 'deps.json'), 'utf8'));
  workspaceIdentityDrift.modules[0].files[0] = encodeStaticFile(
    'vo.mod',
    Buffer.from('module = "github.com/acme/other"\nvo = "^0.2.0"\n', 'utf8'),
  );
  writeJson(path.join(root, 'deps.json'), workspaceIdentityDrift);
  assert.throws(() => validateQuickplay(root), /workspace vo\.mod.*ProjectSnapshot module intent/);

  fixture(root);
  const workspaceVoDrift = JSON.parse(readFileSync(path.join(root, 'deps.json'), 'utf8'));
  workspaceVoDrift.modules[0].files[0] = encodeStaticFile(
    'vo.mod',
    Buffer.from('module = "github.com/acme/lib"\nvo = "^0.3.0"\n', 'utf8'),
  );
  writeJson(path.join(root, 'deps.json'), workspaceVoDrift);
  assert.throws(() => validateQuickplay(root), /workspace vo\.mod.*ProjectSnapshot module intent/);

  fixture(root);
  const workspaceDependencyDrift = JSON.parse(readFileSync(path.join(root, 'deps.json'), 'utf8'));
  workspaceDependencyDrift.modules[0].files[0] = encodeStaticFile(
    'vo.mod',
    Buffer.from(
      'module = "github.com/acme/lib"\nvo = "^0.2.0"\n\n[dependencies]\n"github.com/acme/other" = "0.1.0"\n',
      'utf8',
    ),
  );
  writeJson(path.join(root, 'deps.json'), workspaceDependencyDrift);
  assert.throws(
    () => validateQuickplay(root),
    /workspace vo\.mod declares dependencies outside the ProjectSnapshot module intent/,
  );

  fixture(root);

  const fullSnapshot = {
    schema_version: 2,
    mode: 'effective',
    authority: 'lock',
    root: {
      module: 'github.com/vo-lang/blockkart',
      vo: '^0.2.0',
      dependencies: [{ module: 'github.com/acme/lib', constraint: '0.1.0' }],
    },
    workspace: { file: '/tmp/vo.work' },
    modules: [{
      module: 'github.com/acme/lib',
      version: '0.1.0',
      vo: '^0.2.0',
      release: `sha256:${'4'.repeat(64)}`,
      source: { kind: 'workspace', directory: '/tmp/lib' },
      dependencies: [],
    }],
  };
  assert.doesNotThrow(() => validateProjectSnapshot(structuredClone(fullSnapshot), 'github.com/vo-lang/blockkart'));
  const invalidRootVo = structuredClone(fullSnapshot);
  invalidRootVo.root.vo = null;
  assert.throws(() => validateProjectSnapshot(invalidRootVo, 'github.com/vo-lang/blockkart'), /toolchain constraint/);
  const invalidWorkspace = structuredClone(fullSnapshot);
  invalidWorkspace.workspace = { garbage: true };
  assert.throws(() => validateProjectSnapshot(invalidWorkspace, 'github.com/vo-lang/blockkart'), /workspace.*unexpected fields/);
  const invalidAuthority = structuredClone(fullSnapshot);
  invalidAuthority.authority = 'empty';
  assert.throws(() => validateProjectSnapshot(invalidAuthority, 'github.com/vo-lang/blockkart'), /empty snapshot authority/);
  const unsatisfiedSelection = structuredClone(fullSnapshot);
  unsatisfiedSelection.root.dependencies[0].constraint = '0.2.0';
  assert.throws(() => validateProjectSnapshot(unsatisfiedSelection, 'github.com/vo-lang/blockkart'), /does not satisfy/);
  const legacyNestedSelection = structuredClone(fullSnapshot);
  legacyNestedSelection.modules[0].source.version = legacyNestedSelection.modules[0].version;
  delete legacyNestedSelection.modules[0].version;
  assert.throws(() => validateProjectSnapshot(legacyNestedSelection, 'github.com/vo-lang/blockkart'), /unexpected fields/);
  const emptySnapshot = {
    schema_version: 2,
    mode: 'effective',
    authority: 'empty',
    root: {
      module: 'github.com/vo-lang/blockkart',
      vo: '^0.2.0',
      dependencies: [],
    },
    modules: [],
  };
  assert.doesNotThrow(() => validateProjectSnapshot(emptySnapshot, 'github.com/vo-lang/blockkart'));
  assert.equal(canonicalVoLock(emptySnapshot), null);
  const emptyLockAuthority = structuredClone(emptySnapshot);
  emptyLockAuthority.authority = 'lock';
  assert.throws(() => validateProjectSnapshot(emptyLockAuthority, 'github.com/vo-lang/blockkart'), /lock snapshot authority/);
  assert.equal(
    quickplayWorkspaceFile([
      { module: 'github.com/acme/a', source: { kind: 'workspace' } },
      { module: 'github.com/acme/b', source: { kind: 'workspace' } },
    ]),
    'version = 1\nmembers = ["../modules/github.com@acme@a", "../modules/github.com@acme@b"]\n',
  );
  assert.equal(
    quickplayWorkspaceFile([
      { module: 'github.com/a/a/b', source: { kind: 'workspace' } },
      { module: 'github.com/a/a0', source: { kind: 'workspace' } },
    ]),
    'version = 1\nmembers = ["../modules/github.com@a@a0", "../modules/github.com@a@a@b"]\n',
  );

  fixture(root);
  const emptyQuickplayProject = JSON.parse(readFileSync(projectPath, 'utf8'));
  emptyQuickplayProject.snapshot = structuredClone(emptySnapshot);
  emptyQuickplayProject.files = [
    ...blockKartVpakProjectFiles(),
    encodeStaticFile('main.vo', Buffer.from('package main\n', 'utf8')),
    encodeStaticFile(
      'vo.mod',
      Buffer.from('module = "github.com/vo-lang/blockkart"\nvo = "^0.2.0"\n', 'utf8'),
    ),
  ];
  const emptyQuickplayDeps = {
    schemaVersion: 2,
    name: 'BlockKart dependencies',
    snapshotDigest: sha256Digest(Buffer.from(JSON.stringify(emptySnapshot), 'utf8')),
    modules: [],
  };
  writeJson(projectPath, emptyQuickplayProject);
  writeJson(depsPath, emptyQuickplayDeps);
  rmSync(path.join(root, 'artifacts'), { recursive: true, force: true });
  assert.throws(
    () => validateQuickplay(root),
    /requires a non-empty lock- or workspace-authoritative ProjectSnapshot/,
  );
  fixture(root);

  const canonicalArtifactOrder = releaseValidationFixture([
    {
      kind: 'extension-native',
      target: 'a-b-c',
      name: 'lib',
      size: 1,
      digest: `sha256:${'5'.repeat(64)}`,
    },
    {
      kind: 'extension-native',
      target: 'a-b-c-d',
      name: 'lib',
      size: 1,
      digest: `sha256:${'6'.repeat(64)}`,
    },
  ]);
  assert.doesNotThrow(() => validateRelease(
    structuredClone(canonicalArtifactOrder.release),
    canonicalArtifactOrder.module,
  ));
  for (const target of ['wasm32-unknown-unknown', 'wasm64-unknown-unknown']) {
    const nativeWasm = releaseValidationFixture([{
      kind: 'extension-native',
      target,
      name: 'lib.wasm',
      size: 1,
      digest: `sha256:${'5'.repeat(64)}`,
    }]);
    assert.throws(
      () => validateRelease(nativeWasm.release, nativeWasm.module),
      /extension-native target/,
    );
  }
  const reversedArtifactOrder = structuredClone(canonicalArtifactOrder.release);
  reversedArtifactOrder.artifacts.reverse();
  assert.throws(
    () => validateRelease(reversedArtifactOrder, canonicalArtifactOrder.module),
    /artifacts must be sorted and unique/,
  );

  const boundaryArtifactRelease = releaseValidationFixture([{
    kind: 'extension-wasm',
    target: 'wasm32-unknown-unknown',
    name: 'boundary.wasm',
    size: MAX_MODULE_ARTIFACT_BYTES,
    digest: `sha256:${'9'.repeat(64)}`,
  }]);
  assert.doesNotThrow(() => validateRelease(
    boundaryArtifactRelease.release,
    boundaryArtifactRelease.module,
  ));
  boundaryArtifactRelease.release.artifacts[0].size += 1;
  assert.throws(
    () => validateRelease(boundaryArtifactRelease.release, boundaryArtifactRelease.module),
    /artifacts\[0\]\.size is invalid/,
  );

  const boundedArtifacts = Array.from({ length: 997 }, (_, index) => ({
    kind: 'extension-native',
    target: `a-b-${String(index).padStart(4, '0')}`,
    name: 'lib',
    size: 1,
    digest: `sha256:${'7'.repeat(64)}`,
  }));
  const maxArtifactRelease = releaseValidationFixture(boundedArtifacts);
  assert.doesNotThrow(() => validateRelease(maxArtifactRelease.release, maxArtifactRelease.module));
  const excessiveArtifactRelease = releaseValidationFixture([
    ...boundedArtifacts,
    {
      kind: 'extension-native',
      target: 'a-b-0997',
      name: 'lib',
      size: 1,
      digest: `sha256:${'8'.repeat(64)}`,
    },
  ]);
  assert.throws(
    () => validateRelease(excessiveArtifactRelease.release, excessiveArtifactRelease.module),
    /artifacts must be a bounded array/,
  );

  const edgeBoundSnapshot = graphEdgeLimitFixture();
  assert.doesNotThrow(() => validateProjectSnapshot(
    edgeBoundSnapshot,
    'github.com/vo-lang/blockkart',
  ));
  edgeBoundSnapshot.modules.at(-1).dependencies.push({
    module: edgeBoundSnapshot.modules[9].module,
    constraint: '0.1.0',
  });
  assert.throws(
    () => validateProjectSnapshot(edgeBoundSnapshot, 'github.com/vo-lang/blockkart'),
    /more than 100000 dependency edges/,
  );

  const pristine = readFileSync(depsPath, 'utf8');
  const tampered = JSON.parse(pristine);
  tampered.modules[0].files[0].content += '# tampered\n';
  writeJson(depsPath, tampered);
  assert.throws(() => validateQuickplay(root), /size mismatch|digest mismatch/);

  writeFileSync(depsPath, pristine);
  const legacy = JSON.parse(pristine);
  legacy.modules[0].files.push(encodeStaticFile('vo.web.json', Buffer.from('{}', 'utf8')));
  writeJson(depsPath, legacy);
  assert.throws(() => validateQuickplay(root), /vo\.web\.json/);

  for (const internalName of ['.vo-project.lock', '.vo-project.transaction']) {
    fixture(root);
    const internalWorkspaceState = JSON.parse(readFileSync(depsPath, 'utf8'));
    internalWorkspaceState.modules[0].files.push(
      encodeStaticFile(`nested/${internalName}`, Buffer.from('internal\n', 'utf8')),
    );
    internalWorkspaceState.modules[0].files.sort(
      (left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)),
    );
    writeJson(depsPath, internalWorkspaceState);
    refreshProvenance(root);
    assert.doesNotThrow(() => validateQuickplay(root));
  }

  fixture(root);
  const invalidModuleVo = JSON.parse(readFileSync(projectPath, 'utf8'));
  invalidModuleVo.snapshot.modules[0].vo = 'v0.2.0';
  writeJson(projectPath, invalidModuleVo);
  assert.throws(() => validateQuickplay(root), /toolchain constraint|exact module version/);

  fixture(root);
  const nonCanonicalWork = JSON.parse(readFileSync(projectPath, 'utf8'));
  const workIndex = nonCanonicalWork.files.findIndex((entry) => entry.path === 'vo.work');
  assert.notEqual(workIndex, -1);
  nonCanonicalWork.files[workIndex] = encodeStaticFile(
    'vo.work',
    Buffer.from('version = 1\nmembers = ["modules/github.com@acme@lib"]\n', 'utf8'),
  );
  writeJson(projectPath, nonCanonicalWork);
  assert.throws(() => validateQuickplay(root), /canonical member closure/);

  for (const removedManifest of ['nested/vo.mod', 'nested/VO.MOD']) {
    fixture(root);
    const reserved = JSON.parse(readFileSync(projectPath, 'utf8'));
    const reservedEntry = encodeStaticFile('safe.json', Buffer.from('{}', 'utf8'));
    reservedEntry.path = removedManifest;
    reserved.files.push(reservedEntry);
    reserved.files.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
    writeJson(projectPath, reserved);
    assert.throws(() => validateQuickplay(root), /reserved protocol or internal metadata/);
  }

  for (const ordinaryNestedName of [
    'nested/vo.lock',
    'nested/vo.sum',
    'nested/vo.web.json',
    'nested/vo.work',
    'nested/VO.WEB.JSON',
  ]) {
    fixture(root);
    const project = JSON.parse(readFileSync(projectPath, 'utf8'));
    const entry = encodeStaticFile('safe.json', Buffer.from('{}', 'utf8'));
    entry.path = ordinaryNestedName;
    project.files.push(entry);
    project.files.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
    writeJson(projectPath, project);
    refreshProvenance(root);
    assert.doesNotThrow(() => validateQuickplay(root));
  }

  for (const internalName of ['.vo-project.lock', '.vo-project.transaction']) {
    fixture(root);
    const internalProjectState = JSON.parse(readFileSync(projectPath, 'utf8'));
    const internalEntry = encodeStaticFile('safe.json', Buffer.from('{}', 'utf8'));
    internalEntry.path = `nested/${internalName}`;
    internalProjectState.files.push(internalEntry);
    internalProjectState.files.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
    writeJson(projectPath, internalProjectState);
    refreshProvenance(root);
    assert.doesNotThrow(() => validateQuickplay(root));
  }

  fixture(root);
  const aliases = JSON.parse(readFileSync(projectPath, 'utf8'));
  aliases.files.push(encodeStaticFile('Case.vo', Buffer.from('package main\n', 'utf8')));
  aliases.files.push(encodeStaticFile('case.vo', Buffer.from('package main\n', 'utf8')));
  aliases.files.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
  writeJson(projectPath, aliases);
  assert.throws(() => validateQuickplay(root), /portable path alias/);

  fixture(root);
  const prefixAliases = JSON.parse(readFileSync(projectPath, 'utf8'));
  prefixAliases.files.push(encodeStaticFile('Sources/one.vo', Buffer.from('package main\n', 'utf8')));
  prefixAliases.files.push(encodeStaticFile('sources/two.vo', Buffer.from('package main\n', 'utf8')));
  prefixAliases.files.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
  writeJson(projectPath, prefixAliases);
  assert.throws(() => validateQuickplay(root), /portable path alias/);

  fixture(root);
  const deepPath = (scope, index) => [
    `${scope}-${index.toString(36)}`,
    ...Array.from({ length: 244 }, () => 'd'),
    'f.vo',
  ].join('/');
  const amplifiedProject = JSON.parse(readFileSync(projectPath, 'utf8'));
  const amplifiedDeps = JSON.parse(readFileSync(depsPath, 'utf8'));
  for (let index = 0; index < 204; index += 1) {
    amplifiedProject.files.push(encodeStaticFile(
      deepPath('project', index),
      Buffer.from('package main\n', 'utf8'),
    ));
    amplifiedDeps.modules[0].files.push(encodeStaticFile(
      deepPath('module', index),
      Buffer.from('package lib\n', 'utf8'),
    ));
  }
  amplifiedProject.files.sort(
    (left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)),
  );
  amplifiedDeps.modules[0].files.sort(
    (left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)),
  );
  writeJson(projectPath, amplifiedProject);
  writeJson(depsPath, amplifiedDeps);
  assert.throws(() => validateQuickplay(root), /aggregate install closure.*(?:path-closure|path-key) limit/);

  fixture(root);
  const reservedName = JSON.parse(readFileSync(projectPath, 'utf8'));
  const con = encodeStaticFile('safe.vo', Buffer.from('package main\n', 'utf8'));
  con.path = 'CON';
  reservedName.files.push(con);
  reservedName.files.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
  writeJson(projectPath, reservedName);
  assert.throws(() => validateQuickplay(root), /Invalid .*component|CON/);

  fixture(root);
  const artifactAncestor = JSON.parse(readFileSync(depsPath, 'utf8'));
  artifactAncestor.modules[0].files.push(encodeStaticFile('web-artifacts', Buffer.from('occupied\n', 'utf8')));
  artifactAncestor.modules[0].files.sort(
    (left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)),
  );
  writeJson(depsPath, artifactAncestor);
  assert.throws(() => validateQuickplay(root), /file\/directory collision/);

  fixture(root);
  writeFileSync(path.join(root, 'vo.web.json'), '{}\n');
  const provenancePath = path.join(root, 'provenance.json');
  const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
  provenance.outputs.push(outputFacts(root, ['vo.web.json'])[0]);
  provenance.outputs.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
  writeJson(provenancePath, provenance);
  assert.throws(() => validateQuickplay(root), /physical file closure/);

  registryFixture(root);
  assert.equal(validateQuickplay(root).artifacts, 1);
  const registryDepsPath = path.join(root, 'deps.json');

  const modeDrift = JSON.parse(readFileSync(registryDepsPath, 'utf8'));
  modeDrift.modules[0].files.find((entry) => entry.path === 'vo.mod').mode = 0o755;
  writeJson(registryDepsPath, modeDrift);
  refreshProvenance(root);
  assert.throws(() => validateQuickplay(root), /differs from vo\.package\.json/);

  registryFixture(root);
  const nonCanonicalRelease = JSON.parse(readFileSync(registryDepsPath, 'utf8'));
  const releaseIndex = nonCanonicalRelease.modules[0].files.findIndex((entry) => entry.path === 'vo.release.json');
  const releaseValue = JSON.parse(nonCanonicalRelease.modules[0].files[releaseIndex].content);
  const nonCanonicalReleaseBytes = Buffer.from(`${JSON.stringify(releaseValue)}\n`, 'utf8');
  nonCanonicalRelease.modules[0].files[releaseIndex] = encodeStaticFile(
    'vo.release.json',
    nonCanonicalReleaseBytes,
  );
  const releaseProject = JSON.parse(readFileSync(projectPath, 'utf8'));
  releaseProject.snapshot.modules[0].release = sha256Digest(nonCanonicalReleaseBytes);
  const releaseLockIndex = releaseProject.files.findIndex((entry) => entry.path === 'vo.lock');
  releaseProject.files[releaseLockIndex] = encodeStaticFile(
    'vo.lock',
    Buffer.from(canonicalVoLock(releaseProject.snapshot), 'utf8'),
  );
  nonCanonicalRelease.snapshotDigest = sha256Digest(Buffer.from(JSON.stringify(releaseProject.snapshot), 'utf8'));
  writeJson(projectPath, releaseProject);
  writeJson(registryDepsPath, nonCanonicalRelease);
  assert.throws(() => validateQuickplay(root), /canonical pretty JSON.*trailing LF/);

  registryFixture(root);
  const nonCanonicalPackage = JSON.parse(readFileSync(registryDepsPath, 'utf8'));
  const packageIndex = nonCanonicalPackage.modules[0].files.findIndex((entry) => entry.path === 'vo.package.json');
  const nonCanonicalPackageBytes = Buffer.from(
    `${nonCanonicalPackage.modules[0].files[packageIndex].content}\n`,
    'utf8',
  );
  nonCanonicalPackage.modules[0].files[packageIndex] = encodeStaticFile(
    'vo.package.json',
    nonCanonicalPackageBytes,
  );
  const packageReleaseIndex = nonCanonicalPackage.modules[0].files.findIndex((entry) => entry.path === 'vo.release.json');
  const packageRelease = JSON.parse(nonCanonicalPackage.modules[0].files[packageReleaseIndex].content);
  packageRelease.package = {
    size: nonCanonicalPackageBytes.byteLength,
    digest: sha256Digest(nonCanonicalPackageBytes),
  };
  const packageReleaseBytes = Buffer.from(`${canonicalProtocolJsonText(packageRelease)}\n`, 'utf8');
  nonCanonicalPackage.modules[0].files[packageReleaseIndex] = encodeStaticFile(
    'vo.release.json',
    packageReleaseBytes,
  );
  const packageProject = JSON.parse(readFileSync(projectPath, 'utf8'));
  packageProject.snapshot.modules[0].release = sha256Digest(packageReleaseBytes);
  const packageLockIndex = packageProject.files.findIndex((entry) => entry.path === 'vo.lock');
  packageProject.files[packageLockIndex] = encodeStaticFile(
    'vo.lock',
    Buffer.from(canonicalVoLock(packageProject.snapshot), 'utf8'),
  );
  nonCanonicalPackage.snapshotDigest = sha256Digest(Buffer.from(JSON.stringify(packageProject.snapshot), 'utf8'));
  writeJson(projectPath, packageProject);
  writeJson(registryDepsPath, nonCanonicalPackage);
  assert.throws(() => validateQuickplay(root), /canonical pretty JSON.*trailing LF/);

  registryFixture(root);
  const missingReleasedArtifact = JSON.parse(readFileSync(registryDepsPath, 'utf8'));
  missingReleasedArtifact.modules[0].artifacts = [];
  writeJson(registryDepsPath, missingReleasedArtifact);
  assert.throws(() => validateQuickplay(root), /browser artifacts do not match its authenticated release/);
  console.log('Quickplay vNext validator selftest: ok');
} finally {
  rmSync(root, { recursive: true, force: true });
}
