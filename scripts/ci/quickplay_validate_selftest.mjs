#!/usr/bin/env node
import { execFileSync, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
  symlinkSync,
  truncateSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requiredVpakProducerInputPaths } from './blockkart_vpak_policy.mjs';
import {
  artifactCachePath,
  quickplayArtifactRelativePathFromUrl,
  quickplayArtifactUrl,
} from './quickplay_artifact_paths.mjs';
import {
  QUICKPLAY_GENERATOR_COMMAND,
  QUICKPLAY_GENERATOR_INPUTS,
  QUICKPLAY_GENERATOR_SOURCE_INPUTS,
  QUICKPLAY_GENERATOR_VERSION,
  QUICKPLAY_SOURCE_ROOTS,
  QUICKPLAY_TASK_ID,
  quickplayGeneratorSourceDigest,
} from './quickplay_generator_contract.mjs';
import {
  currentVoCliToolchain,
  currentVoCliBuildInputs,
  voCliExecutionDigest,
  VO_CLI_PRODUCER_TASK_INPUTS,
} from './quickplay_cli_producer_contract.mjs';
import { compareUtf8 } from './utf8_order.mjs';
import { portablePathCollisionKey } from './portable_path_key.mjs';
import {
  decodeModuleTextUtf8,
  parseVoLockForV2Migration,
  parseVoLockV2,
  parseVoModRootContract,
  renderVoLockV2,
  selectLockedModuleSnapshotForCache,
  validatePackagedModuleSet,
  validateVoLockV2RootGraph,
} from './vo_lock_v2.mjs';
import {
  createVoplayVolangBuildInputs,
  currentVoplayWasmBuildPlatform,
  voplayFfiSourceFingerprint,
  VOPLAY_WASM_PRODUCER_COMMAND,
} from './voplay_current_wasm.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const validator = path.join(root, 'scripts/ci/quickplay_validate.mjs');
const sourceAudit = path.join(root, 'scripts/ci/quickplay_source_audit.mjs');
const syntheticVoCliBuildInputs = currentVoCliBuildInputs(root);
const syntheticVoCliToolchain = currentVoCliToolchain(root);
const syntheticVoBinary = {
  path: syntheticVoCliToolchain.target.includes('windows')
    ? 'target/blockkart-vpak-build/vo.exe'
    : 'target/blockkart-vpak-build/vo',
  digest: `sha256:${'8'.repeat(64)}`,
  size: 4096,
};
const syntheticVoCliExecutionDigest = voCliExecutionDigest(
  syntheticVoCliBuildInputs,
  syntheticVoCliToolchain,
  syntheticVoBinary,
);
const syntheticVolangBuildInputs = createVoplayVolangBuildInputs(root, [
  {
    name: 'vo-common-core',
    version: '0.1.0',
    path: 'lang/crates/vo-common-core',
  },
  {
    name: 'vo-runtime',
    version: '0.1.0',
    path: 'lang/crates/vo-runtime',
  },
]);
const syntheticSourceClosure = (() => {
  const closure = {
    schemaVersion: 1,
    kind: 'voplay.localSourceClosure',
    workspace: {
      repository: 'github.com/vo-lang/voplay',
      path: 'vo.work',
      digest: `sha256:${createHash('sha256').update('voplay-workspace').digest('hex')}`,
      modules: ['github.com/vo-lang/vogui', 'github.com/vo-lang/vopack'],
    },
    repositories: [
      {
        roles: ['cargo-path', 'vo-workspace'],
        name: 'github.com/vo-lang/vogui',
        commit: 'a'.repeat(40),
        dirty: false,
        digest: `sha256:${createHash('sha256').update('vogui-source-tree').digest('hex')}`,
        cargoPackages: [{ name: 'vogui', version: '0.1.0', manifest: 'rust/Cargo.toml' }],
        workspaceModules: ['github.com/vo-lang/vogui'],
      },
      {
        roles: ['vo-workspace'],
        name: 'github.com/vo-lang/vopack',
        commit: 'b'.repeat(40),
        dirty: false,
        digest: `sha256:${createHash('sha256').update('vopack-source-tree').digest('hex')}`,
        cargoPackages: [],
        workspaceModules: ['github.com/vo-lang/vopack'],
      },
      {
        roles: ['cargo-path', 'root', 'workspace-owner'],
        name: 'github.com/vo-lang/voplay',
        commit: 'c'.repeat(40),
        dirty: false,
        digest: `sha256:${createHash('sha256').update('voplay-source-tree').digest('hex')}`,
        cargoPackages: [{ name: 'vo-voplay', version: '0.1.0', manifest: 'rust/Cargo.toml' }],
        workspaceModules: [],
      },
    ],
  };
  closure.digest = `sha256:${createHash('sha256').update(JSON.stringify(closure)).digest('hex')}`;
  return closure;
})();

function fail(message) {
  console.error(`quickplay validate selftest: ${message}`);
  process.exit(1);
}

function engineeringTaskInputs(taskName) {
  const source = readFileSync(path.join(root, 'eng/tasks.toml'), 'utf8');
  const block = source.split('[[task]]').find((candidate) => (
    new RegExp(`(?:^|\\n)name = ${JSON.stringify(taskName)}(?:\\n|$)`).test(candidate)
  ));
  if (!block) fail(`engineering task is missing: ${taskName}`);
  const match = /(?:^|\n)inputs\s*=\s*\[([\s\S]*?)\](?:\n|$)/.exec(block);
  if (!match) fail(`engineering task has no inputs: ${taskName}`);
  try {
    return new Set(JSON.parse(`[${match[1]}]`));
  } catch (error) {
    fail(`could not parse engineering task inputs for ${taskName}: ${error.message}`);
  }
}

for (const taskName of [
  'quickplay-blockkart-package',
  'quickplay-regenerate-check',
  'quickplay-validate-selftest',
  'quickplay-validate',
]) {
  const inputs = engineeringTaskInputs(taskName);
  const missing = QUICKPLAY_GENERATOR_SOURCE_INPUTS.filter((input) => !inputs.has(input));
  if (missing.length > 0) {
    fail(`${taskName} task inputs omit generator contract files: ${missing.join(', ')}`);
  }
  const missingCliInputs = VO_CLI_PRODUCER_TASK_INPUTS.filter((input) => !inputs.has(input));
  if (missingCliInputs.length > 0) {
    fail(`${taskName} task inputs omit Vo CLI producer paths: ${missingCliInputs.join(', ')}`);
  }
}

const unicodeOrderProbe = ['中.vo', '😀.vo', 'é.vo', 'a.vo'];
const unicodeOrderFound = [...unicodeOrderProbe].sort(compareUtf8);
const unicodeOrderExpected = ['a.vo', 'é.vo', '中.vo', '😀.vo'];
if (JSON.stringify(unicodeOrderFound) !== JSON.stringify(unicodeOrderExpected)) {
  fail(`UTF-8 comparator mismatch: ${JSON.stringify(unicodeOrderFound)}`);
}
if (portablePathCollisionKey('ϑ.vo') !== portablePathCollisionKey('ϴ.vo')) {
  fail('portable path collision keys must apply full Unicode case equivalence');
}

const lockProtocolProbe = {
  version: 2,
  created_by: 'quickplay "v2"\\selftest\n\u0085e\u0301🙂',
  root: { module: 'github.com/acme/root', vo: '^0.1.0' },
  resolved: [
    {
      path: 'github.com/acme/z',
      version: 'v1.0.0',
      vo: '^0.1.0',
      commit: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
      release_manifest: `sha256:${'1'.repeat(64)}`,
      source: `sha256:${'2'.repeat(64)}`,
      deps: [
        { module: 'github.com/acme/b', constraint: '^1.0.0' },
        { module: 'github.com/acme/a', constraint: 'v1.2.3' },
      ],
      artifacts: [],
    },
    {
      path: 'github.com/acme/b',
      version: 'v1.0.0',
      vo: '^0.1.0',
      commit: 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
      release_manifest: `sha256:${'3'.repeat(64)}`,
      source: `sha256:${'4'.repeat(64)}`,
      deps: [],
      artifacts: [],
    },
    {
      path: 'github.com/acme/a',
      version: 'v1.2.3',
      vo: '^0.1.0',
      commit: 'cccccccccccccccccccccccccccccccccccccccc',
      release_manifest: `sha256:${'5'.repeat(64)}`,
      source: `sha256:${'6'.repeat(64)}`,
      deps: [],
      artifacts: [],
    },
  ],
};
const renderedLockProbe = renderVoLockV2(lockProtocolProbe);
const parsedLockProbe = parseVoLockV2(renderedLockProbe, 'quickplay lock v2 selftest');
const parsedZProbe = parsedLockProbe.resolved.find((module) => module.path === 'github.com/acme/z');
const cacheLockProbe = selectLockedModuleSnapshotForCache(
  renderedLockProbe,
  'github.com/acme/z',
  'quickplay module cache lock selftest',
);
if (
  parsedLockProbe.created_by !== lockProtocolProbe.created_by
  || parsedZProbe.deps.map((edge) => edge.module).join(',')
    !== 'github.com/acme/a,github.com/acme/b'
  || !renderedLockProbe.includes('deps = [\n  { module =')
  || !renderedLockProbe.includes('\\n\\u0085é🙂')
  || Object.hasOwn(cacheLockProbe, 'deps')
  || cacheLockProbe.module !== 'github.com/acme/z'
  || cacheLockProbe.commit !== 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
  || cacheLockProbe.release_manifest !== `sha256:${'1'.repeat(64)}`
  || cacheLockProbe.source !== `sha256:${'2'.repeat(64)}`
) {
  fail('vo.lock v2 multiline, cache projection, escaping, or canonical sorting mismatch');
}
try {
  renderVoLockV2({
    ...lockProtocolProbe,
    resolved: lockProtocolProbe.resolved.map((module) => (
      module.path === 'github.com/acme/z'
        ? {
            ...module,
            deps: [
              { module: 'github.com/acme/a', constraint: '^1.0.0' },
              { module: 'github.com/acme/a', constraint: '^2.0.0' },
            ],
          }
        : module
    )),
  });
  fail('vo.lock v2 renderer accepted duplicate dependency edges');
} catch (error) {
  if (String(error.message).startsWith('quickplay validate selftest:')) throw error;
}
const legacyLockProbe = renderedLockProbe
  .replace(/^version = 2$/m, 'version = 1')
  .replace(
    /deps = \[\n(?:  \{.*\},\n)+\]/,
    'deps = ["github.com/acme/b", "github.com/acme/a"]',
  );
const migratedLockProbe = parseVoLockForV2Migration(legacyLockProbe, 'legacy lock selftest');
if (
  migratedLockProbe.migratedFromVersion !== 1
  || migratedLockProbe.resolved.find((module) => module.path === 'github.com/acme/z').deps.some(
    (edge) => edge.constraint !== '__legacy_requires_replacement__',
  )
) {
  fail('vo.lock v1 migration did not mark every legacy edge for authenticated replacement');
}

function expectRejected(label, action) {
  try {
    action();
  } catch {
    return;
  }
  fail(`vo.lock v2 accepted ${label}`);
}

function expectRejectedMessage(label, messagePart, action) {
  try {
    action();
  } catch (error) {
    if (String(error.message).includes(messagePart)) return;
    fail(`${label} failed with an unexpected diagnostic: ${error.message}`);
  }
  fail(`vo.lock v2 accepted ${label}`);
}

expectRejected('invalid UTF-8 bytes at a module-text boundary', () =>
  decodeModuleTextUtf8(Buffer.from([0x66, 0x6f, 0x80]), 'invalid UTF-8 selftest'));
expectRejectedMessage('a non-text lock input', 'expected text', () =>
  parseVoLockV2(Buffer.from(renderedLockProbe), 'non-text lock selftest'));
expectRejectedMessage('a lock newline bomb', 'line count exceeds', () =>
  parseVoLockV2('\n'.repeat(1_000_000), 'lock newline bomb selftest'));
expectRejectedMessage('a migration newline bomb', 'line count exceeds', () =>
  parseVoLockForV2Migration(
    `version = 1\n${'\n'.repeat(1_000_000)}`,
    'migration newline bomb selftest',
  ));
expectRejectedMessage('a vo.mod newline bomb', 'line count exceeds', () =>
  parseVoModRootContract(
    `module github.com/acme/root\nvo ^0.1.0\n${'\n'.repeat(100_001)}`,
    'vo.mod newline bomb selftest',
  ));
expectRejectedMessage('a vo.mod token bomb', 'directive exceeds', () =>
  parseVoModRootContract(
    `module github.com/acme/root extra extra${'\tignored'.repeat(100_000)}\nvo ^0.1.0\n`,
    'vo.mod token bomb selftest',
  ));
expectRejectedMessage('an oversized integer token', 'integer exceeds JavaScript safe range', () =>
  parseVoLockV2(
    renderedLockProbe.replace('version = 2', `version = ${'9'.repeat(1_000_000)}`),
    'integer token bomb selftest',
  ));

expectRejected('v1 at a strict consumer boundary', () =>
  parseVoLockV2(renderedLockProbe.replace(/^version = 2$/m, 'version = 1'), 'strict v1 rejection'));
expectRejected('the JSON-only \\/ escape', () =>
  parseVoLockV2(
    renderedLockProbe.replace(
      'module = "github.com/acme/root"',
      'module = "github.com\\/acme/root"',
    ),
    'JSON escape rejection',
  ));
expectRejected('an isolated Unicode surrogate', () =>
  parseVoLockV2(
    renderedLockProbe.replace(/^created_by = .*$/m, 'created_by = "\\uD800"'),
    'surrogate rejection',
  ));
expectRejected('a non-canonical Unicode escape for a scalar', () =>
  parseVoLockV2(
    renderedLockProbe.replace('🙂', '\\U0001F642'),
    'non-canonical scalar escape rejection',
  ));
expectRejected('CRLF in canonical wire bytes', () =>
  parseVoLockV2(renderedLockProbe.replaceAll('\n', '\r\n'), 'CRLF canonical rejection'));

const missingTransitive = {
  ...lockProtocolProbe,
  resolved: lockProtocolProbe.resolved.filter((module) => module.path !== 'github.com/acme/a'),
};
expectRejected('a missing transitive module', () => renderVoLockV2(missingTransitive));
const unsatisfiedTransitive = {
  ...lockProtocolProbe,
  resolved: lockProtocolProbe.resolved.map((module) => (
    module.path === 'github.com/acme/a' ? { ...module, version: 'v1.2.4' } : module
  )),
};
expectRejected('an unsatisfied transitive edge', () => renderVoLockV2(unsatisfiedTransitive));
expectRejected('an invalid commit', () => renderVoLockV2({
  ...lockProtocolProbe,
  resolved: lockProtocolProbe.resolved.map((module) => (
    module.path === 'github.com/acme/a' ? { ...module, commit: 'A'.repeat(40) } : module
  )),
}));
expectRejected('an invalid digest', () => renderVoLockV2({
  ...lockProtocolProbe,
  resolved: lockProtocolProbe.resolved.map((module) => (
    module.path === 'github.com/acme/a' ? { ...module, source: `sha256:${'A'.repeat(64)}` } : module
  )),
}));
expectRejected('an incompatible module major suffix', () => renderVoLockV2({
  ...lockProtocolProbe,
  resolved: lockProtocolProbe.resolved.map((module) => (
    module.path === 'github.com/acme/a'
      ? { ...module, path: 'github.com/acme/a/v2' }
      : module
  )),
}));

const rootContract = parseVoModRootContract(
  'module github.com/acme/root\nvo ^0.1.0\n\nrequire github.com/acme/z ^1.0.0\n',
  'root graph vo.mod',
);
validateVoLockV2RootGraph(parsedLockProbe, rootContract, 'root graph vo.lock');
validatePackagedModuleSet(
  parsedLockProbe,
  parsedLockProbe.resolved.map((module) => ({ module: module.path, version: module.version })),
  'root graph deps.modules',
);

const aggregateModules = Array.from({ length: 10_000 }, (_, index) =>
  `github.com/acme/m${String(index).padStart(5, '0')}`);
const aggregateLock = {
  root: { module: 'github.com/acme/root', vo: '^0.1.0' },
  resolved: aggregateModules.map((modulePath, index) => ({
    path: modulePath,
    version: 'v1.0.0',
    deps: Array.from({ length: 9 }, (_, offset) => ({
      module: aggregateModules[(index + offset + 1) % aggregateModules.length],
    })),
  })),
};
const aggregateContract = {
  module: 'github.com/acme/root',
  vo: '^0.1.0',
  require: aggregateModules.map((modulePath) => ({
    module: modulePath,
    constraint: 'v1.0.0',
  })),
};
validateVoLockV2RootGraph(aggregateLock, aggregateContract, '100000-edge boundary');
expectRejected('10,000 root plus 90,001 transitive edges', () =>
  validateVoLockV2RootGraph(
    {
      ...aggregateLock,
      resolved: aggregateLock.resolved.map((module, index) => (
        index === 0
          ? { ...module, deps: [...module.deps, { module: aggregateModules[10] }] }
          : module
      )),
    },
    aggregateContract,
    '100001-edge boundary',
  ));
expectRejected('a root module mismatch', () => validateVoLockV2RootGraph(
  parsedLockProbe,
  { ...rootContract, module: 'github.com/acme/other' },
  'root mismatch',
));
expectRejected('a missing root requirement', () => validateVoLockV2RootGraph(
  parsedLockProbe,
  parseVoModRootContract(
    'module github.com/acme/root\nvo ^0.1.0\nrequire github.com/acme/missing ^1.0.0\n',
    'missing root requirement vo.mod',
  ),
  'missing root requirement vo.lock',
));
expectRejected('an orphaned resolved module', () => validateVoLockV2RootGraph(
  parsedLockProbe,
  parseVoModRootContract(
    'module github.com/acme/root\nvo ^0.1.0\nrequire github.com/acme/a v1.2.3\n',
    'orphan vo.mod',
  ),
  'orphan vo.lock',
));
expectRejected('a packaged module omission', () => validatePackagedModuleSet(
  parsedLockProbe,
  parsedLockProbe.resolved.slice(1).map((module) => ({ module: module.path, version: module.version })),
  'incomplete deps.modules',
));

const singletonToolchainLock = {
  ...lockProtocolProbe,
  root: { ...lockProtocolProbe.root, vo: '^0.0.3' },
  resolved: lockProtocolProbe.resolved.map((module) => ({ ...module, vo: '0.0.3' })),
};
renderVoLockV2(singletonToolchainLock);
const maxU64 = '18446744073709551615';
renderVoLockV2({
  ...lockProtocolProbe,
  root: { ...lockProtocolProbe.root, vo: `~1.2.${maxU64}` },
  resolved: lockProtocolProbe.resolved.map((module) => ({ ...module, vo: `1.2.${maxU64}` })),
});
expectRejected('a prerelease range narrowed to one exact prerelease', () => renderVoLockV2({
  ...lockProtocolProbe,
  root: { ...lockProtocolProbe.root, vo: '^0.0.3-alpha.1' },
  resolved: lockProtocolProbe.resolved.map((module) => ({ ...module, vo: '0.0.3-alpha.1' })),
}));

const encodedArtifact = {
  kind: 'extension-native',
  target: 'aarch64-apple-darwin',
  name: 'lib #100% 中.dylib',
};
const encodedArtifactUrl = quickplayArtifactUrl('github.com@vo-lang@demo/v1.2.3', encodedArtifact);
const expectedEncodedArtifactUrl = '/quickplay/blockkart/artifacts/github.com%40vo-lang%40demo/v1.2.3/extension-native/aarch64-apple-darwin/lib%20%23100%25%20%E4%B8%AD.dylib';
if (encodedArtifactUrl !== expectedEncodedArtifactUrl) {
  fail(`artifact URL encoding mismatch: ${encodedArtifactUrl}`);
}
const rfc3986Url = quickplayArtifactUrl('github.com@vo-lang@demo/v1.2.3', {
  ...encodedArtifact,
  name: "lib!'().dylib",
});
if (!rfc3986Url.endsWith('/lib%21%27%28%29.dylib')) {
  fail(`artifact RFC 3986 encoding mismatch: ${rfc3986Url}`);
}
if (
  quickplayArtifactRelativePathFromUrl(encodedArtifactUrl)
  !== `artifacts/github.com@vo-lang@demo/v1.2.3/${encodedArtifact.kind}/${encodedArtifact.target}/${encodedArtifact.name}`
) {
  fail('artifact URL decoding mismatch');
}
for (const invalidName of ['../escape.wasm', 'NUL.wasm', 'bad\\name.wasm', 'bad/name.wasm']) {
  try {
    artifactCachePath({ kind: 'extension-wasm', target: 'wasm32-unknown-unknown', name: invalidName });
    fail(`artifact path accepted invalid name ${JSON.stringify(invalidName)}`);
  } catch (error) {
    if (String(error.message).startsWith('quickplay validate selftest:')) throw error;
  }
}

const sourceAuditIdentitySelftest = spawnSync(process.execPath, [sourceAudit], {
  cwd: root,
  env: {
    ...process.env,
    QUICKPLAY_SOURCE_AUDIT_ARTIFACT_SELFTEST: '1',
  },
  encoding: 'utf8',
  maxBuffer: 20 * 1024 * 1024,
});
if (
  sourceAuditIdentitySelftest.status !== 0
  || !sourceAuditIdentitySelftest.stdout.includes('quickplay source audit artifact identity selftest: ok')
) {
  fail(`source-audit artifact identity diagnostics selftest failed\n${sourceAuditIdentitySelftest.stdout}${sourceAuditIdentitySelftest.stderr}`);
}

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function currentGeneratorSourceDigest() {
  return quickplayGeneratorSourceDigest(QUICKPLAY_GENERATOR_SOURCE_INPUTS.map((relativePath) => {
    const bytes = readFileSync(path.join(root, relativePath));
    return { digest: sha256Digest(bytes), path: relativePath, size: bytes.byteLength };
  }));
}

function jsonText(value) {
  return `${JSON.stringify(value, null, 2)}\n`;
}

function moduleFileBytes(file) {
  if (file.content != null) return Buffer.from(file.content, 'utf8');
  if (file.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  throw new Error(`file has no bytes: ${file.path}`);
}

function sourceSetDigest(entries) {
  const canonical = entries
    .map((entry) => ({ path: entry.path, size: entry.size, digest: entry.digest }))
    .sort((a, b) => compareUtf8(a.path, b.path));
  return sha256Digest(Buffer.from(JSON.stringify(canonical), 'utf8'));
}

const sourceSetGolden = sourceSetDigest([
  {
    digest: 'sha256:e55cffc81a5ad8cfe85239d944a3ae9513645a9eed79bc884f51b80b2760fc46',
    size: 3,
    path: 'vo.mod',
  },
  {
    size: 0,
    path: 'empty.vo',
    digest: 'sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855',
  },
]);
if (sourceSetGolden !== 'sha256:99e9a1fb7265851bf1de4e0be09198396256ba485afacf74c0c54eeb836afa50') {
  fail(`source-set digest protocol mismatch: ${sourceSetGolden}`);
}

function packagedFilesDigest(files) {
  return sourceSetDigest(files
    .map((file) => {
      const bytes = moduleFileBytes(file);
      return { digest: sha256Digest(bytes), path: file.path, size: bytes.byteLength };
    })
    .sort((a, b) => compareUtf8(a.path, b.path)));
}

function sourceEntry(path, content) {
  const bytes = Buffer.from(content, 'utf8');
  return { digest: sha256Digest(bytes), path, size: bytes.byteLength };
}

function projectSourceEntry(path, content) {
  const bytes = Buffer.from(content, 'utf8');
  return { path, digest: sha256Digest(bytes), size: bytes.byteLength };
}

function repoFileEntry(repoRoot, relative) {
  const bytes = readFileSync(path.join(repoRoot, relative));
  return { path: relative, digest: sha256Digest(bytes), size: bytes.byteLength };
}

function repoFileEntries(repoRoot, relatives) {
  return relatives
    .map((relative) => repoFileEntry(repoRoot, relative))
    .sort((a, b) => compareUtf8(a.path, b.path));
}

function writeFile(filePath, bytes) {
  mkdirSync(path.dirname(filePath), { recursive: true });
  writeFileSync(filePath, bytes);
}

function git(cwd, args) {
  execFileSync('git', args, { cwd, stdio: 'pipe' });
}

function initBlockKartRepo(repoRoot) {
  mkdirSync(repoRoot, { recursive: true });
  const asset = Buffer.alloc(1024 * 1024 + 17, 7);
  writeFile(path.join(repoRoot, 'main.vo'), 'package main\n\nfunc main() {}\n\n// valid scalar: 😀\n');
  writeFile(path.join(repoRoot, 'vo.mod'), 'module github.com/vo-lang/blockkart\nvo ^0.1.0\n\nrequire github.com/vo-lang/vogui v0.1.15\nrequire github.com/vo-lang/voplay v0.1.28\n');
  writeFile(path.join(repoRoot, 'assets/blockkart.vpak'), asset);
  writeFile(path.join(repoRoot, 'tools/pack_primitive_assets.vo'), 'package main\n\nfunc main() {}\n');
  writeFile(path.join(repoRoot, 'tools/vpak_provenance.mjs'), 'export const provenance = true;\n');
  writeFile(path.join(repoRoot, 'tools/generate_primitive_terrain.mjs'), 'export const generated = true;\n');
  writeFile(path.join(repoRoot, 'tools/paint_terrain_textures.mjs'), 'export const painted = true;\n');
  writeFile(path.join(repoRoot, 'tools/terrain_heightfield_spec.mjs'), 'export const heightmapSize = 8;\n');
  writeFile(path.join(repoRoot, 'tools/terrain_recipe.mjs'), 'export const recipe = {};\n');
  writeFile(path.join(repoRoot, 'terrain/recipes/primitive_concept_v1.json'), '{"name":"selftest"}\n');
  writeFile(path.join(repoRoot, 'docs/images/terrain-upgrade-concept-v1.png'), Buffer.from('concept'));
  for (const relative of [
    'assets/source/terrain_painted/grass_painted_v1.png',
    'assets/source/terrain_painted/meadow_painted_v1.png',
    'assets/source/terrain_painted/dirt_painted_v1.png',
    'assets/source/terrain_painted/rock_painted_v1.png',
    'assets/effects/grass_card_atlas.png',
    'assets/maps/primitive_track/blockkart.map.json',
    'assets/maps/primitive_track/lowpoly_terrain.glb',
    'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
    'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
    'assets/maps/primitive_track/terrain_splat_large.png',
  ]) {
    writeFile(path.join(repoRoot, relative), Buffer.from(`selftest ${relative}\n`, 'utf8'));
  }
  for (const relative of requiredVpakProducerInputPaths.filter((entry) => !entry.startsWith('workspace:'))) {
    if (!existsSync(path.join(repoRoot, relative))) {
      writeFile(path.join(repoRoot, relative), Buffer.from(`selftest ${relative}\n`, 'utf8'));
    }
  }
  git(repoRoot, ['init']);
  git(repoRoot, ['config', 'user.email', 'quickplay-selftest@example.invalid']);
  git(repoRoot, ['config', 'user.name', 'Quickplay Selftest']);
  return asset;
}

function buildModule({
  module,
  version,
  commit,
  sourceFiles,
  sourceDigest,
  artifacts = [],
  release = true,
  extension = null,
  require = [],
}) {
  const sourceEntries = sourceFiles
    .map(([filePath, content]) => sourceEntry(filePath, content))
    .sort((left, right) => compareUtf8(left.path, right.path));
  const webManifest = {
    schema_version: 1,
    module,
    version,
    commit,
    module_root: '.',
    vo: '^0.1.0',
    require,
    source_digest: sourceSetDigest(sourceEntries),
    source: sourceEntries,
    web: null,
    extension,
    artifacts: artifacts.map((artifact) => ({
      digest: artifact.digest,
      kind: artifact.kind,
      name: artifact.name,
      path: artifact.path,
      size: artifact.size,
      target: artifact.target,
    })),
  };
  const webManifestContent = jsonText(webManifest);
  const files = [
    ...sourceFiles.map(([filePath, content]) => ({ path: filePath, content })),
    { path: 'vo.web.json', content: webManifestContent },
  ];
  if (release) {
    const releaseManifest = {
      schema_version: 1,
      module,
      version,
      commit,
      module_root: '.',
      vo: '^0.1.0',
      require,
      source: {
        name: `${module.split('/').pop()}-${version}.tar.gz`,
        size: 123,
        digest: sourceDigest,
        files_size: sourceEntries.reduce((total, entry) => total + entry.size, 0),
        files_digest: sourceSetDigest(sourceEntries),
      },
      web_manifest: {
        size: Buffer.byteLength(webManifestContent, 'utf8'),
        digest: sha256Digest(Buffer.from(webManifestContent, 'utf8')),
      },
      artifacts: artifacts.map((artifact) => ({
        digest: artifact.digest,
        kind: artifact.kind,
        name: artifact.name,
        size: artifact.size,
        target: artifact.target,
      })),
    };
    files.push({ path: 'vo.release.json', content: jsonText(releaseManifest) });
    files.push({ path: '.vo-source-digest', content: `${sourceDigest}\n` });
    files.push({ path: '.vo-version', content: `${version}\n` });
  }
  files.sort((a, b) => compareUtf8(a.path, b.path));
  return {
    module,
    version,
    commit,
    cacheDir: module.replaceAll('/', '@') + `/${version}`,
    dirty: false,
    source: 'module-cache',
    files,
    artifacts: artifacts.map((artifact) => ({
      digest: artifact.digest,
      kind: artifact.kind,
      name: artifact.name,
      path: artifact.path,
      size: artifact.size,
      target: artifact.target,
      url: artifact.url,
    })),
  };
}

function writeSyntheticPackage(
  tempQuickplay,
  blockKartRoot,
  assetBytes,
  {
    transformCanonicalManifest = (bytes) => bytes,
    transformProjectDocument = (bytes) => bytes,
  } = {},
) {
  rmSync(tempQuickplay, { recursive: true, force: true });
  const voplayJs = Buffer.from('export const island = true;\n', 'utf8');
  const voplayWasm = Buffer.from([0, 97, 115, 109, 1, 0, 0, 0]);
  const voplayJsArtifact = {
    kind: 'extension-js-glue',
    target: 'wasm32-unknown-unknown',
    name: 'voplay_island.js',
    path: 'artifacts/extension-js-glue/wasm32-unknown-unknown/voplay_island.js',
    url: quickplayArtifactUrl('github.com@vo-lang@voplay/v0.1.28', {
      kind: 'extension-js-glue', target: 'wasm32-unknown-unknown', name: 'voplay_island.js',
    }),
    size: voplayJs.byteLength,
    digest: sha256Digest(voplayJs),
  };
  const voplayWasmArtifact = {
    kind: 'extension-wasm',
    target: 'wasm32-unknown-unknown',
    name: 'voplay_island_bg.wasm',
    path: 'artifacts/extension-wasm/wasm32-unknown-unknown/voplay_island_bg.wasm',
    url: quickplayArtifactUrl('github.com@vo-lang@voplay/v0.1.28', {
      kind: 'extension-wasm', target: 'wasm32-unknown-unknown', name: 'voplay_island_bg.wasm',
    }),
    size: voplayWasm.byteLength,
    digest: sha256Digest(voplayWasm),
  };
  writeFile(path.join(tempQuickplay, 'artifacts/github.com@vo-lang@voplay/v0.1.28/extension-js-glue/wasm32-unknown-unknown/voplay_island.js'), voplayJs);
  writeFile(path.join(tempQuickplay, 'artifacts/github.com@vo-lang@voplay/v0.1.28/extension-wasm/wasm32-unknown-unknown/voplay_island_bg.wasm'), voplayWasm);

  const voplayRenderer = [
    "export { RenderIsland } from './render_bootstrap.js';",
    '',
  ].join('\n');
  const voplayBootstrap = [
    'export class RenderIsland {',
    '  constructor() {',
    '    this.hostTimers = new Map();',
    '    this.displayPulseWaiters = new Map();',
    '  }',
    '',
    '  scheduleHostEvents() {',
    '    const events = this.vm.takePendingHostEvents();',
    '    for (const ev of events) {',
    '      if (this.hostTimers.has(ev.key)) continue;',
    '      if (ev.delayMs === DISPLAY_PULSE_DELAY_MS) {',
    '        this.displayPulseWaiters.set(ev.key, { afterSerial: 1 });',
    '        this.hostTimers.set(ev.key, { kind: "displayPulse" });',
    '      } else {',
    '        const id = window.setTimeout(() => this.wakeHostEvent(ev.key, ev.delayMs), ev.delayMs);',
    '        this.hostTimers.set(ev.key, { kind: "timeout", id });',
    '      }',
    '    }',
    '  }',
    '',
    '  wakeHostEvent(key, delayMs) {',
    '    this.vm.wakeHostEvent(key);',
    '    this.vm.runScheduled();',
    '  }',
    '}',
    '',
  ].join('\n');
  const voplayDts = [
    'export interface VoVm {',
    '  takePendingHostEvents(): Array<{',
    '    key: string;',
    '    delayMs: number;',
    '  }>;',
    '  wakeHostEvent(key: string): void;',
    '}',
    '',
  ].join('\n');

  const fakeSource = (label) => `sha256:${createHash('sha256').update(label).digest('hex')}`;
  const vogui = buildModule({
    module: 'github.com/vo-lang/vogui',
    version: 'v0.1.15',
    commit: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
    sourceDigest: fakeSource('vogui-source'),
    sourceFiles: [['vo.mod', 'module github.com/vo-lang/vogui\nvo ^0.1.0\n']],
  });
  const vopack = buildModule({
    module: 'github.com/vo-lang/vopack',
    version: 'v0.1.2',
    commit: 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb',
    sourceDigest: fakeSource('vopack-source'),
    sourceFiles: [
      ['vo.mod', 'module github.com/vo-lang/vopack\nvo ^0.1.0\n'],
      ['mount.vo', 'package vopack\n\nfunc MountSelfTest() int { return 1 }\n'],
    ],
  });
  const voplay = buildModule({
    module: 'github.com/vo-lang/voplay',
    version: 'v0.1.28',
    commit: 'cccccccccccccccccccccccccccccccccccccccc',
    sourceDigest: fakeSource('voplay-source'),
    artifacts: [voplayJsArtifact, voplayWasmArtifact],
    extension: {
      name: 'voplay',
      include: [],
      wasm: {
        kind: 'Bindgen',
        wasm: 'voplay_island_bg.wasm',
        js_glue: 'voplay_island.js',
      },
      web: {
        capabilities: [],
        js_modules: { renderer: 'js/dist/voplay-render-island.js' },
      },
    },
    require: [
      { module: 'github.com/vo-lang/vogui', constraint: 'v0.1.15' },
      { module: 'github.com/vo-lang/vopack', constraint: 'v0.1.2' },
    ],
    sourceFiles: [
      [
        'vo.mod',
        'module github.com/vo-lang/voplay\nvo ^0.1.0\n\nrequire github.com/vo-lang/vogui v0.1.15\nrequire github.com/vo-lang/vopack v0.1.2\n\n[extension]\nname = "voplay"\ninclude = []\n\n[extension.wasm]\ntype = "bindgen"\nwasm = "voplay_island_bg.wasm"\njs_glue = "voplay_island.js"\n\n[extension.web]\ncapabilities = []\n\n[extension.web.js]\nrenderer = "js/dist/voplay-render-island.js"\n',
      ],
      ['js/dist/render_bootstrap.d.ts', voplayDts],
      ['js/dist/render_bootstrap.js', voplayBootstrap],
      ['js/dist/voplay-render-island.js', voplayRenderer],
      ['scene3d/blockkart_pack.vo', 'package scene3d\n\nfunc PackBlockKart() {}\n'],
    ],
  });

  const lock = `version = 2
created_by = "quickplay validate selftest"

[root]
module = "github.com/vo-lang/blockkart"
vo = "^0.1.0"

[[resolved]]
path = "github.com/vo-lang/vogui"
version = "v0.1.15"
vo = "^0.1.0"
commit = "${vogui.files.find((file) => file.path === 'vo.web.json').content.match(/"commit": "([^"]+)"/)[1]}"
release_manifest = "${sha256Digest(Buffer.from(vogui.files.find((file) => file.path === 'vo.release.json').content, 'utf8'))}"
source = "${vogui.files.find((file) => file.path === '.vo-source-digest').content.trim()}"
deps = []

[[resolved]]
path = "github.com/vo-lang/vopack"
version = "v0.1.2"
vo = "^0.1.0"
commit = "${vopack.files.find((file) => file.path === 'vo.web.json').content.match(/"commit": "([^"]+)"/)[1]}"
release_manifest = "${sha256Digest(Buffer.from(vopack.files.find((file) => file.path === 'vo.release.json').content, 'utf8'))}"
source = "${vopack.files.find((file) => file.path === '.vo-source-digest').content.trim()}"
deps = []

[[resolved]]
path = "github.com/vo-lang/voplay"
version = "v0.1.28"
vo = "^0.1.0"
commit = "${voplay.files.find((file) => file.path === 'vo.web.json').content.match(/"commit": "([^"]+)"/)[1]}"
release_manifest = "${sha256Digest(Buffer.from(voplay.files.find((file) => file.path === 'vo.release.json').content, 'utf8'))}"
source = "${voplay.files.find((file) => file.path === '.vo-source-digest').content.trim()}"
deps = [
  { module = "github.com/vo-lang/vogui", constraint = "v0.1.15" },
  { module = "github.com/vo-lang/vopack", constraint = "v0.1.2" },
]

[[resolved.artifact]]
kind = "extension-js-glue"
target = "wasm32-unknown-unknown"
name = "voplay_island.js"
size = ${voplayJs.byteLength}
digest = "${sha256Digest(voplayJs)}"

[[resolved.artifact]]
kind = "extension-wasm"
target = "wasm32-unknown-unknown"
name = "voplay_island_bg.wasm"
size = ${voplayWasm.byteLength}
digest = "${sha256Digest(voplayWasm)}"
`;
  writeFile(path.join(blockKartRoot, 'vo.lock'), lock);
  const sourceAllowlist = [
    {
      path: 'tools/pack_primitive_assets.vo',
      reason: 'Asset-pack generation tool; quickplay runtime embeds the generated assets/blockkart.vpak payload.',
      expiresAt: '2027-01-31T00:00:00.000Z',
    },
  ];
  const projectSourceFiles = [
    projectSourceEntry('main.vo', readFileSync(path.join(blockKartRoot, 'main.vo'), 'utf8')),
    projectSourceEntry('tools/pack_primitive_assets.vo', readFileSync(path.join(blockKartRoot, 'tools/pack_primitive_assets.vo'), 'utf8')),
  ];
  const vpakInputs = [
    'tools/pack_primitive_assets.vo',
    'tools/vpak_provenance.mjs',
    'assets/maps/primitive_track/blockkart.map.json',
    'assets/maps/primitive_track/lowpoly_terrain.glb',
    'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
    'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
    'assets/maps/primitive_track/terrain_splat_large.png',
    'assets/effects/grass_card_atlas.png',
  ];
  const paintInputs = [
    'tools/paint_terrain_textures.mjs',
    'docs/images/terrain-upgrade-concept-v1.png',
  ];
  const paintOutputs = [
    'assets/source/terrain_painted/grass_painted_v1.png',
    'assets/source/terrain_painted/meadow_painted_v1.png',
    'assets/source/terrain_painted/dirt_painted_v1.png',
    'assets/source/terrain_painted/rock_painted_v1.png',
    'assets/effects/grass_card_atlas.png',
  ];
  const terrainInputs = [
    'tools/generate_primitive_terrain.mjs',
    'tools/terrain_heightfield_spec.mjs',
    'tools/terrain_recipe.mjs',
    'terrain/recipes/primitive_concept_v1.json',
    ...paintOutputs,
  ];
  const terrainOutputs = [
    'assets/maps/primitive_track/blockkart.map.json',
    'assets/maps/primitive_track/lowpoly_terrain.glb',
    'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
    'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
    'assets/maps/primitive_track/terrain_splat_large.png',
  ];
  const canonicalInputPaths = [...new Set([
    ...requiredVpakProducerInputPaths.filter((entry) => !entry.startsWith('workspace:')),
    ...vpakInputs,
    ...paintInputs,
    ...paintOutputs,
    ...terrainInputs,
    ...terrainOutputs,
  ])].sort(compareUtf8);
  const canonicalInputs = repoFileEntries(blockKartRoot, canonicalInputPaths).map((entry) => ({
    path: entry.path,
    sha256: entry.digest.slice('sha256:'.length),
    size: entry.size,
  }));
  const workspacePaths = [
    ...requiredVpakProducerInputPaths.filter((entry) => entry.startsWith('workspace:')),
    'workspace:github.com/vo-lang/voplay/scene3d/blockkart_pack.vo',
  ];
  const workspaceModules = [vogui, vopack, voplay].sort((left, right) => (
    right.module.length - left.module.length
  ));
  for (const workspacePath of workspacePaths) {
    const workspaceRelative = workspacePath.slice('workspace:'.length);
    const workspaceModule = workspaceModules.find((mod) => workspaceRelative.startsWith(`${mod.module}/`));
    const relative = workspaceModule ? workspaceRelative.slice(workspaceModule.module.length + 1) : null;
    const workspaceFile = workspaceModule?.files.find((file) => file.path === relative);
    if (!workspaceFile) fail(`selftest workspace input is missing from its module: ${workspacePath}`);
    const bytes = moduleFileBytes(workspaceFile);
    canonicalInputs.push({
      path: workspacePath,
      sha256: sha256Digest(bytes).slice('sha256:'.length),
      size: bytes.byteLength,
    });
  }
  canonicalInputs.sort((a, b) => compareUtf8(a.path, b.path));
  const archiveEntries = Array.from({ length: 37 }, (_, index) => ({
    path: `assets/selftest-${String(index).padStart(2, '0')}.bin`,
    kind: 'selftest',
    sourcePath: `assets/selftest-${String(index).padStart(2, '0')}.bin`,
    sourceSha256: createHash('sha256').update(`selftest-${index}`).digest('hex'),
    sourceSize: index + 1,
    contentHash: `crc32:${index.toString(16).padStart(8, '0')}`,
    dependencies: [],
    compression: 0,
    rawSize: index + 1,
    storedSize: index + 1,
    storedChecksum: index,
  }));
  const canonicalManifest = {
    schemaVersion: 1,
    kind: 'blockkart.vpakProducerManifest',
    owner: 'BlockKart',
    command: ['vo', 'run', 'tools/pack_primitive_assets.vo'],
    pack: {
      path: 'assets/blockkart.vpak',
      sha256: sha256Digest(assetBytes).slice('sha256:'.length),
      size: assetBytes.byteLength,
    },
    inputs: canonicalInputs,
    payloadInputCount: 37,
    archiveEntryCount: 37,
    workspaceSourceInputCount: workspacePaths.length,
    archiveEntries,
    internalManifest: { pack: 'BlockKart', version: 'selftest', assetCount: 37, sha256: createHash('sha256').update('selftest manifest').digest('hex') },
    upstream: [],
  };
  canonicalManifest.producerDigest = createHash('sha256').update(JSON.stringify(canonicalManifest)).digest('hex');
  const canonicalManifestBytes = transformCanonicalManifest(
    Buffer.from(jsonText(canonicalManifest), 'utf8'),
  );
  if (!Buffer.isBuffer(canonicalManifestBytes)) {
    fail('canonical manifest selftest transform must return a Buffer');
  }
  writeFile(path.join(blockKartRoot, 'assets/blockkart.vpak.provenance.json'), canonicalManifestBytes);
  const status = execFileSync('git', ['status', '--porcelain'], {
    cwd: blockKartRoot,
    encoding: 'utf8',
  }).trim();
  if (status !== '') {
    git(blockKartRoot, ['add', '.']);
    git(blockKartRoot, ['commit', '-m', 'selftest fixture']);
  }
  const commit = execFileSync('git', ['rev-parse', 'HEAD'], { cwd: blockKartRoot, encoding: 'utf8' }).trim();

  const project = {
    schemaVersion: 2,
    name: 'BlockKart',
    module: 'github.com/vo-lang/blockkart',
    commit,
    dirty: false,
    sourceFiles: projectSourceFiles,
    sourceAllowlist,
    files: [
      { path: 'assets/blockkart.vpak', contentBase64: assetBytes.toString('base64') },
      { path: 'assets/blockkart.vpak.provenance.json', content: canonicalManifestBytes.toString('utf8') },
      { path: 'main.vo', content: readFileSync(path.join(blockKartRoot, 'main.vo'), 'utf8') },
      { path: 'vo.lock', content: lock },
      { path: 'vo.mod', content: readFileSync(path.join(blockKartRoot, 'vo.mod'), 'utf8') },
    ].sort((a, b) => compareUtf8(a.path, b.path)),
  };
  const deps = {
    schemaVersion: 2,
    name: 'BlockKart dependencies',
    modules: [vogui, vopack, voplay].sort((a, b) => compareUtf8(a.module, b.module)),
  };

  const projectBytes = transformProjectDocument(Buffer.from(jsonText(project), 'utf8'));
  if (!Buffer.isBuffer(projectBytes)) {
    fail('project document selftest transform must return a Buffer');
  }
  const depsBytes = Buffer.from(jsonText(deps), 'utf8');
  writeFile(path.join(tempQuickplay, 'project.json'), projectBytes);
  writeFile(path.join(tempQuickplay, 'deps.json'), depsBytes);

  const provenance = {
    schemaVersion: 3,
    artifact: 'studio.quickplay.blockkart',
    path: 'apps/studio/public/quickplay/blockkart',
    task: {
      id: QUICKPLAY_TASK_ID,
      command: QUICKPLAY_GENERATOR_COMMAND,
    },
    generator: {
      command: QUICKPLAY_GENERATOR_COMMAND,
      script: 'apps/studio/scripts/package_blockkart_quickplay.mjs',
      version: QUICKPLAY_GENERATOR_VERSION,
    },
    toolchain: {
      node: `v${process.versions.node.split('.')[0]}`,
      voDevSourceDigest: currentGeneratorSourceDigest(),
      wasmTarget: 'wasm32-unknown-unknown',
    },
    sourceRoots: QUICKPLAY_SOURCE_ROOTS,
    inputs: QUICKPLAY_GENERATOR_INPUTS,
    project: {
      commit,
      dirty: false,
      filesDigest: packagedFilesDigest(project.files),
      module: project.module,
      sourceFiles: projectSourceFiles,
      sourceAllowlist,
      sourceFilesDigest: sourceSetDigest(projectSourceFiles),
    },
    producers: [
      {
        id: 'voplay-current-source-wasm',
        owner: 'voplay/rust',
        kind: 'wasm-bindgen',
        command: VOPLAY_WASM_PRODUCER_COMMAND,
        sourceClosure: syntheticSourceClosure,
        volangBuildInputs: syntheticVolangBuildInputs,
        ffiSourceFingerprint: voplayFfiSourceFingerprint(
          syntheticSourceClosure.digest,
          syntheticVolangBuildInputs.digest,
        ),
        toolchain: {
          rustc: 'rustc selftest',
          wasmPack: 'wasm-pack selftest',
        },
        buildPlatform: currentVoplayWasmBuildPlatform(),
        outputs: [voplayJsArtifact, voplayWasmArtifact].map((artifact) => ({
          name: artifact.name,
          size: artifact.size,
          digest: artifact.digest,
        })),
      },
      {
        id: 'blockkart-runtime-vpak',
        owner: 'BlockKart',
        kind: 'vpak',
        output: 'assets/blockkart.vpak',
        command: ['vo', 'run', 'tools/pack_primitive_assets.vo'],
        toolchain: syntheticVoCliToolchain,
        voBinary: syntheticVoBinary,
        voCliBuildInputs: syntheticVoCliBuildInputs,
        voCliExecutionDigest: syntheticVoCliExecutionDigest,
        inputs: canonicalInputs.map((entry) => ({ path: entry.path, digest: `sha256:${entry.sha256}`, size: entry.size })),
        outputs: repoFileEntries(blockKartRoot, ['assets/blockkart.vpak']),
        producerManifest: {
          path: 'assets/blockkart.vpak.provenance.json',
          sha256: sha256Digest(canonicalManifestBytes),
          size: canonicalManifestBytes.byteLength,
          producerDigest: canonicalManifest.producerDigest,
        },
        archiveEntryCount: 37,
        payloadInputCount: 37,
        workspaceSourceInputCount: workspacePaths.length,
        archiveEntries,
        upstream: [
          {
            id: 'painted-terrain-textures',
            owner: 'BlockKart',
            kind: 'offline-texture-generation',
            command: ['node', 'tools/paint_terrain_textures.mjs'],
            inputs: repoFileEntries(blockKartRoot, paintInputs),
            outputs: repoFileEntries(blockKartRoot, paintOutputs),
          },
          {
            id: 'primitive-terrain-assets',
            owner: 'BlockKart',
            kind: 'offline-terrain-generation',
            command: ['node', 'tools/generate_primitive_terrain.mjs'],
            inputs: repoFileEntries(blockKartRoot, terrainInputs),
            outputs: repoFileEntries(blockKartRoot, terrainOutputs),
          },
        ],
      },
    ],
    dependencies: deps.modules.map((mod) => ({
      artifacts: (mod.artifacts ?? []).map((artifact) => {
        const localPath = path.join(
          tempQuickplay,
          ...quickplayArtifactRelativePathFromUrl(artifact.url).split('/'),
        );
        const bytes = readFileSync(localPath);
        return {
          digest: sha256Digest(bytes),
          kind: artifact.kind,
          name: artifact.name,
          path: artifact.path,
          size: bytes.byteLength,
          target: artifact.target,
          url: artifact.url,
        };
      }),
      cacheDir: mod.cacheDir,
      commit: mod.commit,
      dirty: false,
      filesDigest: packagedFilesDigest(mod.files),
      module: mod.module,
      source: 'module-cache',
      version: mod.version,
    })),
    outputs: [
      { digest: sha256Digest(projectBytes), path: 'project.json', size: projectBytes.byteLength },
      { digest: sha256Digest(depsBytes), path: 'deps.json', size: depsBytes.byteLength },
    ],
  };
  writeFile(path.join(tempQuickplay, 'provenance.json'), jsonText(provenance));
}

function runValidator(dir, blockKartRoot, environment = {}) {
  const blockKartExpectedCommit = execFileSync('git', ['rev-parse', 'HEAD'], {
    cwd: blockKartRoot,
    encoding: 'utf8',
  }).trim();
  const reportDir = Object.hasOwn(environment, 'QUICKPLAY_VALIDATE_OUT_DIR')
    ? path.resolve(environment.QUICKPLAY_VALIDATE_OUT_DIR)
    : mkdtempSync(path.join(tempRoot, 'validator-report-'));
  const env = {
    ...process.env,
    QUICKPLAY_DIR: dir,
    BLOCKKART_ROOT: blockKartRoot,
    BLOCKKART_EXPECTED_COMMIT: blockKartExpectedCommit,
    QUICKPLAY_VALIDATE_SELFTEST: '1',
    QUICKPLAY_VALIDATE_SELFTEST_EXPECTED_VOLANG_BUILD_INPUTS: JSON.stringify(
      syntheticVolangBuildInputs,
    ),
    QUICKPLAY_VALIDATE_SELFTEST_EXPECTED_SOURCE_CLOSURE: JSON.stringify(
      syntheticSourceClosure,
    ),
    QUICKPLAY_VALIDATE_OUT_DIR: reportDir,
    ...environment,
  };
  delete env.QUICKPLAY_ALLOW_LEGACY_V1;
  for (const variable of [
    'QUICKPLAY_VALIDATE_REPORT_SELFTEST_ONLY',
    'QUICKPLAY_VALIDATE_REPORT_SELFTEST_FAILURE',
    'QUICKPLAY_VALIDATE_REPORT_SELFTEST_MAX_BYTES',
  ]) {
    if (!Object.hasOwn(environment, variable)) delete env[variable];
  }
  const result = spawnSync(process.execPath, [validator], {
    cwd: root,
    env,
    encoding: 'utf8',
    maxBuffer: 20 * 1024 * 1024,
  });
  return Object.assign(result, { reportPath: path.join(reportDir, 'report.json') });
}

function reportTemporaries(reportDir) {
  return readdirSync(reportDir).filter((name) => (
    name.startsWith('.report.json.') && name.endsWith('.tmp')
  ));
}

function runReportPublisher(dir, blockKartRoot, reportDir, environment = {}) {
  return runValidator(dir, blockKartRoot, {
    QUICKPLAY_VALIDATE_OUT_DIR: reportDir,
    QUICKPLAY_VALIDATE_REPORT_SELFTEST_ONLY: '1',
    ...environment,
  });
}

function writePackageDocuments(tempQuickplay, project, deps, provenance) {
  const projectBytes = Buffer.from(jsonText(project), 'utf8');
  const depsBytes = Buffer.from(jsonText(deps), 'utf8');
  const outputMap = new Map((provenance.outputs ?? []).map((entry) => [entry.path, entry]));
  const projectOutput = outputMap.get('project.json');
  const depsOutput = outputMap.get('deps.json');
  if (!projectOutput || !depsOutput) fail('package provenance outputs are incomplete');
  Object.assign(projectOutput, { digest: sha256Digest(projectBytes), size: projectBytes.byteLength });
  Object.assign(depsOutput, { digest: sha256Digest(depsBytes), size: depsBytes.byteLength });
  writeFileSync(path.join(tempQuickplay, 'project.json'), projectBytes);
  writeFileSync(path.join(tempQuickplay, 'deps.json'), depsBytes);
  writeFileSync(path.join(tempQuickplay, 'provenance.json'), jsonText(provenance));
}

function writeDepsAndRefreshProvenance(tempQuickplay, deps, refreshedModules = []) {
  const depsBytes = Buffer.from(jsonText(deps), 'utf8');
  writeFileSync(path.join(tempQuickplay, 'deps.json'), depsBytes);
  const provenancePath = path.join(tempQuickplay, 'provenance.json');
  const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
  const output = provenance.outputs?.find((entry) => entry.path === 'deps.json');
  if (!output) {
    fail('could not find deps.json provenance output');
  }
  output.digest = sha256Digest(depsBytes);
  output.size = depsBytes.byteLength;
  for (const moduleName of refreshedModules) {
    const mod = deps.modules.find((entry) => entry.module === moduleName);
    const provenanceMod = provenance.dependencies?.find((entry) => entry.module === moduleName);
    if (!mod || !provenanceMod) {
      fail(`could not refresh dependency provenance for ${moduleName}`);
    }
    provenanceMod.filesDigest = packagedFilesDigest(mod.files);
  }
  writeFileSync(provenancePath, jsonText(provenance));
}

function refreshModuleReleaseBinding(tempQuickplay, moduleName) {
  const project = JSON.parse(readFileSync(path.join(tempQuickplay, 'project.json'), 'utf8'));
  const deps = JSON.parse(readFileSync(path.join(tempQuickplay, 'deps.json'), 'utf8'));
  const provenance = JSON.parse(readFileSync(path.join(tempQuickplay, 'provenance.json'), 'utf8'));
  const mod = deps.modules.find((entry) => entry.module === moduleName);
  if (!mod) fail(`could not refresh missing dependency ${moduleName}`);
  const releaseFile = mod.files.find((file) => file.path === 'vo.release.json');
  const sourceMarker = mod.files.find((file) => file.path === '.vo-source-digest');
  if (typeof releaseFile?.content !== 'string' || typeof sourceMarker?.content !== 'string') {
    fail(`could not refresh release metadata for ${moduleName}`);
  }
  const lockFile = project.files.find((file) => file.path === 'vo.lock');
  if (typeof lockFile?.content !== 'string') fail('project package has no text vo.lock');
  const lock = parseVoLockV2(lockFile.content, 'quickplay selftest refresh lock');
  const locked = lock.resolved.find((entry) => entry.path === moduleName);
  if (!locked) fail(`project lock does not resolve ${moduleName}`);
  locked.release_manifest = sha256Digest(Buffer.from(releaseFile.content, 'utf8'));
  locked.source = sourceMarker.content.trim();
  lockFile.content = renderVoLockV2(lock);
  provenance.project.filesDigest = packagedFilesDigest(project.files);
  const provenanceMod = provenance.dependencies.find((entry) => entry.module === moduleName);
  if (!provenanceMod) fail(`provenance does not contain ${moduleName}`);
  provenanceMod.filesDigest = packagedFilesDigest(mod.files);
  writePackageDocuments(tempQuickplay, project, deps, provenance);
}

function refreshModuleSourceEvidence(tempQuickplay, moduleName) {
  const depsPath = path.join(tempQuickplay, 'deps.json');
  const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
  const mod = deps.modules.find((entry) => entry.module === moduleName);
  if (!mod) fail(`could not refresh missing dependency source ${moduleName}`);
  const webFile = mod.files.find((file) => file.path === 'vo.web.json');
  const releaseFile = mod.files.find((file) => file.path === 'vo.release.json');
  const sourceMarker = mod.files.find((file) => file.path === '.vo-source-digest');
  if (
    typeof webFile?.content !== 'string'
    || typeof releaseFile?.content !== 'string'
    || typeof sourceMarker?.content !== 'string'
  ) {
    fail(`could not refresh source-bound manifests for ${moduleName}`);
  }
  const sourceFiles = new Map(mod.files.map((file) => [file.path, file]));
  const web = JSON.parse(webFile.content);
  web.source = web.source.map((entry) => {
    const file = sourceFiles.get(entry.path);
    if (!file) fail(`could not refresh missing ${moduleName} source file ${entry.path}`);
    const bytes = moduleFileBytes(file);
    return { path: entry.path, size: bytes.byteLength, digest: sha256Digest(bytes) };
  }).sort((left, right) => compareUtf8(left.path, right.path));
  web.source_digest = sourceSetDigest(web.source);
  webFile.content = jsonText(web);

  const release = JSON.parse(releaseFile.content);
  const reboundSourceDigest = sha256Digest(Buffer.from(`selftest-source:${web.source_digest}`, 'utf8'));
  release.source.digest = reboundSourceDigest;
  release.source.files_size = web.source.reduce((total, entry) => total + entry.size, 0);
  release.source.files_digest = web.source_digest;
  const webBytes = Buffer.from(webFile.content, 'utf8');
  release.web_manifest.size = webBytes.byteLength;
  release.web_manifest.digest = sha256Digest(webBytes);
  releaseFile.content = jsonText(release);
  sourceMarker.content = `${reboundSourceDigest}\n`;
  writeFileSync(depsPath, jsonText(deps));
  refreshModuleReleaseBinding(tempQuickplay, moduleName);
}

const tempRoot = mkdtempSync(path.join(os.tmpdir(), 'quickplay-validate-selftest-'));
const tempQuickplay = path.join(tempRoot, 'quickplay-package');
const blockKartRoot = path.join(tempRoot, 'BlockKart');

try {
  const assetBytes = initBlockKartRepo(blockKartRoot);
  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);

  const baseline = runValidator(tempQuickplay, blockKartRoot);
  if (baseline.status !== 0) {
    fail(`baseline fixture must validate before drift mutation\n${baseline.stdout}${baseline.stderr}`);
  }

  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'blockkart-runtime-vpak');
    delete producer.voCliBuildInputs;
    writeFileSync(provenancePath, jsonText(provenance));
    const missingCliInputs = runValidator(tempQuickplay, blockKartRoot);
    if (
      missingCliInputs.status === 0
      || !/assets\/blockkart\.vpak producer fields must be exactly/s.test(
        `${missingCliInputs.stdout}${missingCliInputs.stderr}`,
      )
    ) {
      fail(`validator accepted missing Vo CLI producer inputs\n${missingCliInputs.stdout}${missingCliInputs.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'blockkart-runtime-vpak');
    producer.voCliBuildInputs.inputs[0].digest = `sha256:${'0'.repeat(64)}`;
    writeFileSync(provenancePath, jsonText(provenance));
    const staleCliInputs = runValidator(tempQuickplay, blockKartRoot);
    if (
      staleCliInputs.status === 0
      || !/Vo CLI producer inputs are invalid:.*digest does not match/s.test(
        `${staleCliInputs.stdout}${staleCliInputs.stderr}`,
      )
    ) {
      fail(`validator accepted stale Vo CLI producer inputs\n${staleCliInputs.stdout}${staleCliInputs.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'blockkart-runtime-vpak');
    delete producer.toolchain;
    writeFileSync(provenancePath, jsonText(provenance));
    const missingCliToolchain = runValidator(tempQuickplay, blockKartRoot);
    if (
      missingCliToolchain.status === 0
      || !/assets\/blockkart\.vpak producer fields must be exactly/s.test(
        `${missingCliToolchain.stdout}${missingCliToolchain.stderr}`,
      )
    ) {
      fail(`validator accepted missing Vo CLI toolchain identity\n${missingCliToolchain.stdout}${missingCliToolchain.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'blockkart-runtime-vpak');
    delete producer.voCliExecutionDigest;
    writeFileSync(provenancePath, jsonText(provenance));
    const missingCliExecutionDigest = runValidator(tempQuickplay, blockKartRoot);
    if (
      missingCliExecutionDigest.status === 0
      || !/assets\/blockkart\.vpak producer fields must be exactly|Vo CLI execution identity is invalid/s.test(
        `${missingCliExecutionDigest.stdout}${missingCliExecutionDigest.stderr}`,
      )
    ) {
      fail(`validator accepted missing Vo CLI execution digest\n${missingCliExecutionDigest.stdout}${missingCliExecutionDigest.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'blockkart-runtime-vpak');
    producer.voBinary.digest = `sha256:${'0'.repeat(64)}`;
    writeFileSync(provenancePath, jsonText(provenance));
    const driftedCliBinary = runValidator(tempQuickplay, blockKartRoot);
    if (
      driftedCliBinary.status === 0
      || !/Vo CLI execution identity is invalid:.*execution digest does not match/s.test(
        `${driftedCliBinary.stdout}${driftedCliBinary.stderr}`,
      )
    ) {
      fail(`validator accepted drifted Vo CLI binary identity\n${driftedCliBinary.stdout}${driftedCliBinary.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    delete producer.sourceClosure;
    writeFileSync(provenancePath, jsonText(provenance));
    const missingClosure = runValidator(tempQuickplay, blockKartRoot);
    if (
      missingClosure.status === 0
      || !/voplay WASM producer.*exact|source closure is invalid/s.test(
        `${missingClosure.stdout}${missingClosure.stderr}`,
      )
    ) {
      fail(`validator accepted missing sibling source-closure provenance\n${missingClosure.stdout}${missingClosure.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    producer.sourceClosure.repositories = producer.sourceClosure.repositories.filter(
      (repository) => repository.name !== 'github.com/vo-lang/vopack',
    );
    writeFileSync(provenancePath, jsonText(provenance));
    const omittedRepository = runValidator(tempQuickplay, blockKartRoot);
    if (
      omittedRepository.status === 0
      || !/source closure is invalid:.*(?:digest|current locked local source graph|workspace\.modules)/s.test(
        `${omittedRepository.stdout}${omittedRepository.stderr}`,
      )
    ) {
      fail(`validator accepted a source closure with vopack omitted\n${omittedRepository.stdout}${omittedRepository.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    producer.sourceClosure.repositories[0].dirty = true;
    writeFileSync(provenancePath, jsonText(provenance));
    const dirtyRepository = runValidator(tempQuickplay, blockKartRoot);
    if (
      dirtyRepository.status === 0
      || !/source closure is invalid:.*dirty must be false/s.test(
        `${dirtyRepository.stdout}${dirtyRepository.stderr}`,
      )
    ) {
      fail(`validator accepted dirty sibling source provenance\n${dirtyRepository.stdout}${dirtyRepository.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    producer.sourceClosure.repositories[0].root = '/forged/host/path';
    writeFileSync(provenancePath, jsonText(provenance));
    const forgedHostPath = runValidator(tempQuickplay, blockKartRoot);
    if (
      forgedHostPath.status === 0
      || !/source closure is invalid:.*exact repository fields/s.test(
        `${forgedHostPath.stdout}${forgedHostPath.stderr}`,
      )
    ) {
      fail(`validator accepted a forged absolute repository root\n${forgedHostPath.stdout}${forgedHostPath.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    producer.sourceClosure.workspace.path = ['..', 'voplay', 'vo.work'].join('/');
    writeFileSync(provenancePath, jsonText(provenance));
    const layoutAlias = runValidator(tempQuickplay, blockKartRoot);
    if (
      layoutAlias.status === 0
      || !/source closure is invalid:.*(?:normalized|workspace\.path must be vo\.work)/s.test(
        `${layoutAlias.stdout}${layoutAlias.stderr}`,
      )
    ) {
      fail(`validator accepted a persisted workspace layout alias\n${layoutAlias.stdout}${layoutAlias.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);

  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    delete producer.volangBuildInputs;
    writeFileSync(provenancePath, jsonText(provenance));
    const missingVolangInputs = runValidator(tempQuickplay, blockKartRoot);
    if (
      missingVolangInputs.status === 0
      || !/(?:voplay WASM producer fields must be exactly|Volang build inputs are invalid: volangBuildInputs must contain the exact scoped-input fields)/.test(
        `${missingVolangInputs.stdout}${missingVolangInputs.stderr}`,
      )
    ) {
      fail(`validator accepted missing Volang build-input provenance\n${missingVolangInputs.stdout}${missingVolangInputs.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    producer.volangBuildInputs.digest = sha256Digest(Buffer.from('stale Volang inputs', 'utf8'));
    writeFileSync(provenancePath, jsonText(provenance));
    const staleVolangInputs = runValidator(tempQuickplay, blockKartRoot);
    if (
      staleVolangInputs.status === 0
      || !/Volang build inputs are invalid: volangBuildInputs\.digest does not match current scoped Volang inputs/.test(
        `${staleVolangInputs.stdout}${staleVolangInputs.stderr}`,
      )
    ) {
      fail(`validator accepted stale Volang build-input provenance\n${staleVolangInputs.stdout}${staleVolangInputs.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    producer.ffiSourceFingerprint = sha256Digest(Buffer.from('stale FFI source fingerprint', 'utf8'));
    writeFileSync(provenancePath, jsonText(provenance));
    const staleFfiFingerprint = runValidator(tempQuickplay, blockKartRoot);
    if (
      staleFfiFingerprint.status === 0
      || !/FFI source fingerprint does not match current source inputs/.test(
        `${staleFfiFingerprint.stdout}${staleFfiFingerprint.stderr}`,
      )
    ) {
      fail(`validator accepted a stale FFI source fingerprint\n${staleFfiFingerprint.stdout}${staleFfiFingerprint.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const producer = provenance.producers.find((entry) => entry.id === 'voplay-current-source-wasm');
    producer.volangBuildInputs = createVoplayVolangBuildInputs(
      root,
      syntheticVolangBuildInputs.packages.slice(0, 1),
    );
    writeFileSync(provenancePath, jsonText(provenance));
    const omittedPackage = runValidator(tempQuickplay, blockKartRoot);
    if (
      omittedPackage.status === 0
      || !/Volang build inputs are invalid: volangBuildInputs do not match locked Cargo metadata/.test(
        `${omittedPackage.stdout}${omittedPackage.stderr}`,
      )
    ) {
      fail(`validator accepted a rehashed Volang build-input set with one path package omitted\n${omittedPackage.stdout}${omittedPackage.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  const linkedQuickplay = path.join(tempRoot, 'quickplay-link');
  const linkedBlockKart = path.join(tempRoot, 'blockkart-link');
  symlinkSync(tempQuickplay, linkedQuickplay, 'dir');
  symlinkSync(blockKartRoot, linkedBlockKart, 'dir');
  const canonicalRoot = runValidator(linkedQuickplay, linkedBlockKart);
  if (canonicalRoot.status !== 0) {
    fail(`validator must canonicalize an authorized symlinked root\n${canonicalRoot.stdout}${canonicalRoot.stderr}`);
  }

  {
    const reportDir = mkdtempSync(path.join(tempRoot, 'atomic-report-'));
    const reportPath = path.join(reportDir, 'report.json');
    writeFileSync(reportPath, 'stale report must be replaced atomically\n');
    const published = runReportPublisher(tempQuickplay, blockKartRoot, reportDir);
    if (published.status !== 0) {
      fail(`validator could not atomically replace an existing report\n${published.stdout}${published.stderr}`);
    }
    const report = JSON.parse(readFileSync(reportPath, 'utf8'));
    if (report.kind !== 'quickplay.validateReport' || report.status !== 'ok') {
      fail('atomically published report has the wrong contract or status');
    }
    if (reportTemporaries(reportDir).length !== 0) {
      fail('successful report publication left a temporary file behind');
    }

    const sentinel = Buffer.from('complete prior report survives interrupted publication\n');
    writeFileSync(reportPath, sentinel);
    const interrupted = runReportPublisher(tempQuickplay, blockKartRoot, reportDir, {
      QUICKPLAY_VALIDATE_REPORT_SELFTEST_FAILURE: 'after-temp-fsync',
    });
    if (
      interrupted.status === 0
      || !/selftest injected report publication failure after temporary fsync/.test(
        `${interrupted.stdout}${interrupted.stderr}`,
      )
    ) {
      fail(`validator did not exercise the interrupted atomic publication path\n${interrupted.stdout}${interrupted.stderr}`);
    }
    if (!readFileSync(reportPath).equals(sentinel)) {
      fail('interrupted report publication tore or replaced the prior report');
    }
    if (reportTemporaries(reportDir).length !== 0) {
      fail('interrupted report publication left a temporary file behind');
    }

    const bounded = runReportPublisher(tempQuickplay, blockKartRoot, reportDir, {
      QUICKPLAY_VALIDATE_REPORT_SELFTEST_MAX_BYTES: '1',
    });
    if (
      bounded.status === 0
      || !/quickplay validation report exceeds the 1-byte report limit/.test(
        `${bounded.stdout}${bounded.stderr}`,
      )
    ) {
      fail(`validator did not enforce the report byte budget\n${bounded.stdout}${bounded.stderr}`);
    }
    if (!readFileSync(reportPath).equals(sentinel)) {
      fail('report byte-limit failure changed the prior report');
    }
    if (reportTemporaries(reportDir).length !== 0) {
      fail('report byte-limit failure created a temporary file');
    }
  }

  {
    const reportDir = path.join(tempRoot, 'created-report-parent', 'nested-report-dir');
    const createdDirectory = runReportPublisher(tempQuickplay, blockKartRoot, reportDir);
    if (createdDirectory.status !== 0 || !existsSync(path.join(reportDir, 'report.json'))) {
      fail(`validator could not securely create its report directory\n${createdDirectory.stdout}${createdDirectory.stderr}`);
    }
    if (reportTemporaries(reportDir).length !== 0) {
      fail('secure report directory creation left a temporary file');
    }
  }

  {
    const reportDir = mkdtempSync(path.join(tempRoot, 'symlink-report-'));
    const victimPath = path.join(tempRoot, 'report-symlink-victim.json');
    const victim = Buffer.from('report symlink victim must remain unchanged\n');
    writeFileSync(victimPath, victim);
    symlinkSync(victimPath, path.join(reportDir, 'report.json'), 'file');
    const symlinkTarget = runReportPublisher(tempQuickplay, blockKartRoot, reportDir);
    if (
      symlinkTarget.status === 0
      || !/existing report target must be a regular file without symbolic links/.test(
        `${symlinkTarget.stdout}${symlinkTarget.stderr}`,
      )
    ) {
      fail(`validator accepted a symbolic-link report target\n${symlinkTarget.stdout}${symlinkTarget.stderr}`);
    }
    if (!readFileSync(victimPath).equals(victim) || reportTemporaries(reportDir).length !== 0) {
      fail('symbolic-link report rejection changed the victim or left a temporary file');
    }
  }

  {
    const reportDir = mkdtempSync(path.join(tempRoot, 'special-report-'));
    mkdirSync(path.join(reportDir, 'report.json'));
    const specialTarget = runReportPublisher(tempQuickplay, blockKartRoot, reportDir);
    if (
      specialTarget.status === 0
      || !/existing report target must be a regular file without symbolic links/.test(
        `${specialTarget.stdout}${specialTarget.stderr}`,
      )
    ) {
      fail(`validator accepted a non-regular report target\n${specialTarget.stdout}${specialTarget.stderr}`);
    }
    if (reportTemporaries(reportDir).length !== 0) {
      fail('non-regular report rejection left a temporary file');
    }
  }

  {
    const realReportDir = mkdtempSync(path.join(tempRoot, 'real-report-dir-'));
    const linkedReportDir = path.join(tempRoot, 'linked-report-dir');
    symlinkSync(realReportDir, linkedReportDir, 'dir');
    const symlinkDirectory = runReportPublisher(
      tempQuickplay,
      blockKartRoot,
      linkedReportDir,
    );
    if (
      symlinkDirectory.status === 0
      || !/report output directory must not be a symbolic link or special file/.test(
        `${symlinkDirectory.stdout}${symlinkDirectory.stderr}`,
      )
    ) {
      fail(`validator accepted a symbolic-link report directory\n${symlinkDirectory.stdout}${symlinkDirectory.stderr}`);
    }
    if (existsSync(path.join(realReportDir, 'report.json'))) {
      fail('symbolic-link report directory rejection published into its target');
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes, {
    transformProjectDocument(bytes) {
      const source = bytes.toString('utf8');
      if (!source.includes('😀')) {
        fail('could not locate non-BMP project scalar for JSON selftest');
      }
      return Buffer.from(source.replace('😀', '\\ud83d\\ude00'), 'utf8');
    },
  });
  {
    const pairedSurrogate = runValidator(tempQuickplay, blockKartRoot);
    if (pairedSurrogate.status !== 0) {
      fail(`validator rejected a valid escaped non-BMP scalar\n${pairedSurrogate.stdout}${pairedSurrogate.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const source = readFileSync(depsPath, 'utf8').replace(
      '{\n',
      '{\n  "nonFiniteProbe": 1e400,\n',
    );
    writeFileSync(depsPath, source);
    const nonFiniteNumber = runValidator(tempQuickplay, blockKartRoot);
    if (
      nonFiniteNumber.status === 0
      || !/outside the finite f64 range/.test(
        `${nonFiniteNumber.stdout}${nonFiniteNumber.stderr}`,
      )
    ) {
      fail(`validator accepted an out-of-range JSON number\n${nonFiniteNumber.stdout}${nonFiniteNumber.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const source = readFileSync(depsPath, 'utf8').replace(
      '{\n',
      '{\n  "surrogateProbe": { "\\ud800": "nested" },\n',
    );
    writeFileSync(depsPath, source);
    const surrogateKey = runValidator(tempQuickplay, blockKartRoot);
    if (
      surrogateKey.status === 0
      || !/isolated Unicode surrogate in a JSON string/.test(
        `${surrogateKey.stdout}${surrogateKey.stderr}`,
      )
    ) {
      fail(`validator accepted a nested isolated-surrogate JSON key\n${surrogateKey.stdout}${surrogateKey.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const source = readFileSync(depsPath, 'utf8').replace(
      '{\n',
      '{\n  "surrogateProbe": { "nested": ["\\udc00"] },\n',
    );
    writeFileSync(depsPath, source);
    const surrogateValue = runValidator(tempQuickplay, blockKartRoot);
    if (
      surrogateValue.status === 0
      || !/isolated Unicode surrogate in a JSON string/.test(
        `${surrogateValue.stdout}${surrogateValue.stderr}`,
      )
    ) {
      fail(`validator accepted a nested isolated-surrogate JSON value\n${surrogateValue.stdout}${surrogateValue.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes, {
    transformCanonicalManifest(bytes) {
      const source = bytes.toString('utf8');
      const needle = '  "pack": {\n    "path": "assets/blockkart.vpak",';
      if (!source.includes(needle)) {
        fail('could not locate nested canonical vpak manifest key for duplicate-key selftest');
      }
      return Buffer.from(source.replace(
        needle,
        `${needle}\n    "path": "assets/blockkart.vpak",`,
      ), 'utf8');
    },
  });
  {
    const duplicateProducerKey = runValidator(tempQuickplay, blockKartRoot);
    if (
      duplicateProducerKey.status === 0
      || !/canonical vpak producer manifest contains duplicate object key "path"/.test(
        `${duplicateProducerKey.stdout}${duplicateProducerKey.stderr}`,
      )
    ) {
      fail(`validator accepted a nested duplicate canonical vpak producer key\n${duplicateProducerKey.stdout}${duplicateProducerKey.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes, {
    transformCanonicalManifest(bytes) {
      const manifest = JSON.parse(bytes.toString('utf8'));
      const input = manifest.inputs.find((entry) => !entry.path.startsWith('workspace:'));
      if (!input) fail('could not locate canonical vpak input for traversal selftest');
      input.path = '../escape-from-blockkart';
      delete manifest.producerDigest;
      manifest.producerDigest = createHash('sha256')
        .update(JSON.stringify(manifest))
        .digest('hex');
      return Buffer.from(jsonText(manifest), 'utf8');
    },
  });
  {
    const traversalProducerInput = runValidator(tempQuickplay, blockKartRoot);
    if (
      traversalProducerInput.status === 0
      || !/Invalid BlockKart producer source path component|portable relative path/.test(
        `${traversalProducerInput.stdout}${traversalProducerInput.stderr}`,
      )
    ) {
      fail(`validator accepted a traversing canonical vpak input\n${traversalProducerInput.stdout}${traversalProducerInput.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    provenance.generator.version = QUICKPLAY_GENERATOR_VERSION - 1;
    writeFileSync(provenancePath, jsonText(provenance));
    const staleGenerator = runValidator(tempQuickplay, blockKartRoot);
    if (staleGenerator.status === 0 || !/generator version must be/.test(`${staleGenerator.stdout}${staleGenerator.stderr}`)) {
      fail(`validator accepted a stale generator version\n${staleGenerator.stdout}${staleGenerator.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const provenancePath = path.join(tempQuickplay, 'provenance.json');
    const provenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    provenance.toolchain.voDevSourceDigest = sha256Digest(Buffer.from('forged generator source'));
    writeFileSync(provenancePath, jsonText(provenance));
    const forgedGenerator = runValidator(tempQuickplay, blockKartRoot);
    if (forgedGenerator.status === 0 || !/generator source digest must match/.test(`${forgedGenerator.stdout}${forgedGenerator.stderr}`)) {
      fail(`validator accepted a forged generator source digest\n${forgedGenerator.stdout}${forgedGenerator.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const deps = JSON.parse(readFileSync(path.join(tempQuickplay, 'deps.json'), 'utf8'));
    deps.name = 'Unbound dependencies';
    writeDepsAndRefreshProvenance(tempQuickplay, deps);
    const wrongName = runValidator(tempQuickplay, blockKartRoot);
    if (wrongName.status === 0 || !/deps name must be BlockKart dependencies/.test(`${wrongName.stdout}${wrongName.stderr}`)) {
      fail(`validator accepted an unbound dependency package name\n${wrongName.stdout}${wrongName.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const project = JSON.parse(readFileSync(path.join(tempQuickplay, 'project.json'), 'utf8'));
    const deps = JSON.parse(readFileSync(path.join(tempQuickplay, 'deps.json'), 'utf8'));
    const provenance = JSON.parse(readFileSync(path.join(tempQuickplay, 'provenance.json'), 'utf8'));
    const forged = {
      path: 'main.vo',
      reason: 'Forged runtime omission that attempts to self-authorize an unreviewed source exclusion.',
      expiresAt: '2099-01-01T00:00:00.000Z',
    };
    project.sourceAllowlist.push(forged);
    project.sourceAllowlist.sort((left, right) => compareUtf8(left.path, right.path));
    provenance.project.sourceAllowlist = structuredClone(project.sourceAllowlist);
    writePackageDocuments(tempQuickplay, project, deps, provenance);
    const forgedAllowlist = runValidator(tempQuickplay, blockKartRoot);
    if (
      forgedAllowlist.status === 0
      || !/sourceAllowlist must exactly match the authenticated Quickplay policy/.test(`${forgedAllowlist.stdout}${forgedAllowlist.stderr}`)
    ) {
      fail(`validator accepted a self-authorized source allowlist entry\n${forgedAllowlist.stdout}${forgedAllowlist.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const project = JSON.parse(readFileSync(path.join(tempQuickplay, 'project.json'), 'utf8'));
    const deps = JSON.parse(readFileSync(path.join(tempQuickplay, 'deps.json'), 'utf8'));
    const provenance = JSON.parse(readFileSync(path.join(tempQuickplay, 'provenance.json'), 'utf8'));
    provenance.project.sourceAllowlist = [];
    writePackageDocuments(tempQuickplay, project, deps, provenance);
    const divergentAllowlist = runValidator(tempQuickplay, blockKartRoot);
    if (
      divergentAllowlist.status === 0
      || !/provenance project sourceAllowlist must exactly match/.test(`${divergentAllowlist.stdout}${divergentAllowlist.stderr}`)
    ) {
      fail(`validator accepted divergent project and provenance allowlists\n${divergentAllowlist.stdout}${divergentAllowlist.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
    const mod = deps.modules.find((entry) => entry.module === 'github.com/vo-lang/vogui');
    const voMod = mod?.files.find((file) => file.path === 'vo.mod');
    if (!mod || typeof voMod?.content !== 'string') fail('could not find vogui vo.mod for module drift');
    voMod.content = voMod.content.replace(
      'module github.com/vo-lang/vogui',
      'module github.com/vo-lang/forged-vogui',
    );
    writeFileSync(depsPath, jsonText(deps));
    refreshModuleSourceEvidence(tempQuickplay, mod.module);
    const forgedModule = runValidator(tempQuickplay, blockKartRoot);
    if (
      forgedModule.status === 0
      || !/vo\.mod module must match dependency package module/.test(`${forgedModule.stdout}${forgedModule.stderr}`)
    ) {
      fail(`validator accepted source-rebound dependency module drift\n${forgedModule.stdout}${forgedModule.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
    const mod = deps.modules.find((entry) => entry.module === 'github.com/vo-lang/vopack');
    const voMod = mod?.files.find((file) => file.path === 'vo.mod');
    if (!mod || typeof voMod?.content !== 'string') fail('could not find vopack vo.mod for toolchain drift');
    voMod.content = voMod.content.replace('vo ^0.1.0', 'vo ~0.1.0');
    writeFileSync(depsPath, jsonText(deps));
    refreshModuleSourceEvidence(tempQuickplay, mod.module);
    const forgedToolchain = runValidator(tempQuickplay, blockKartRoot);
    if (
      forgedToolchain.status === 0
      || !/vo\.mod vo constraint must match project vo\.lock/.test(`${forgedToolchain.stdout}${forgedToolchain.stderr}`)
    ) {
      fail(`validator accepted source-rebound dependency toolchain drift\n${forgedToolchain.stdout}${forgedToolchain.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
    const mod = deps.modules.find((entry) => entry.module === 'github.com/vo-lang/voplay');
    const voMod = mod?.files.find((file) => file.path === 'vo.mod');
    if (!mod || typeof voMod?.content !== 'string') fail('could not find voplay vo.mod for requirement drift');
    voMod.content = voMod.content.replace(
      'require github.com/vo-lang/vogui v0.1.15',
      'require github.com/vo-lang/vogui v0.1.14',
    );
    writeFileSync(depsPath, jsonText(deps));
    refreshModuleSourceEvidence(tempQuickplay, mod.module);
    const forgedRequirement = runValidator(tempQuickplay, blockKartRoot);
    if (
      forgedRequirement.status === 0
      || !/vo\.mod requirements must exactly match project vo\.lock/.test(`${forgedRequirement.stdout}${forgedRequirement.stderr}`)
    ) {
      fail(`validator accepted source-rebound dependency requirement drift\n${forgedRequirement.stdout}${forgedRequirement.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const project = JSON.parse(readFileSync(path.join(tempQuickplay, 'project.json'), 'utf8'));
    const deps = JSON.parse(readFileSync(path.join(tempQuickplay, 'deps.json'), 'utf8'));
    const provenance = JSON.parse(readFileSync(path.join(tempQuickplay, 'provenance.json'), 'utf8'));
    const voMod = project.files.find((file) => file.path === 'vo.mod');
    if (typeof voMod?.content !== 'string') fail('could not find project vo.mod for metadata drift');
    voMod.content += '\n[unsupported]\nvalue = "forged"\n';
    provenance.project.filesDigest = packagedFilesDigest(project.files);
    writePackageDocuments(tempQuickplay, project, deps, provenance);
    const forgedMetadata = runValidator(tempQuickplay, blockKartRoot);
    if (
      forgedMetadata.status === 0
      || !/contains unsupported field\(s\): "unsupported"/.test(
        `${forgedMetadata.stdout}${forgedMetadata.stderr}`,
      )
    ) {
      fail(`validator accepted evidence-rebound project metadata drift\n${forgedMetadata.stdout}${forgedMetadata.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const projectPath = path.join(tempQuickplay, 'project.json');
    const source = readFileSync(projectPath, 'utf8');
    writeFileSync(projectPath, source.replace(
      '"name": "BlockKart",',
      '"name": "BlockKart",\n  "name": "ShadowKart",',
    ));
    const duplicateKey = runValidator(tempQuickplay, blockKartRoot);
    if (duplicateKey.status === 0 || !/duplicate object key "name"/.test(`${duplicateKey.stdout}${duplicateKey.stderr}`)) {
      fail(`validator accepted a duplicate JSON field\n${duplicateKey.stdout}${duplicateKey.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const projectPath = path.join(tempQuickplay, 'project.json');
    const project = JSON.parse(readFileSync(projectPath, 'utf8'));
    project.files.push({ path: 'unicode/ϑ.vo', content: 'package unicode\n' });
    project.files.push({ path: 'unicode/ϴ.vo', content: 'package unicode\n' });
    writeFileSync(projectPath, jsonText(project));
    const unicodeCollision = runValidator(tempQuickplay, blockKartRoot);
    if (unicodeCollision.status === 0 || !/duplicate portable path/.test(`${unicodeCollision.stdout}${unicodeCollision.stderr}`)) {
      fail(`validator accepted a full-case-fold path collision\n${unicodeCollision.stdout}${unicodeCollision.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const deps = JSON.parse(readFileSync(path.join(tempQuickplay, 'deps.json'), 'utf8'));
    const mod = deps.modules.find((entry) => entry.module === 'github.com/vo-lang/vopack');
    const marker = mod?.files.find((file) => file.path === '.vo-version');
    if (!mod || typeof marker?.content !== 'string') fail('could not find vopack .vo-version marker');
    marker.content = mod.version;
    writeDepsAndRefreshProvenance(tempQuickplay, deps, [mod.module]);
    const nonCanonicalMarker = runValidator(tempQuickplay, blockKartRoot);
    if (nonCanonicalMarker.status === 0 || !/\.vo-version must be the canonical/.test(`${nonCanonicalMarker.stdout}${nonCanonicalMarker.stderr}`)) {
      fail(`validator accepted a non-canonical module version marker\n${nonCanonicalMarker.stdout}${nonCanonicalMarker.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
    const mod = deps.modules.find((entry) => entry.module === 'github.com/vo-lang/vogui');
    const releaseFile = mod?.files.find((file) => file.path === 'vo.release.json');
    if (!mod || typeof releaseFile?.content !== 'string') fail('could not find vogui release manifest');
    const release = JSON.parse(releaseFile.content);
    release.untrusted = true;
    releaseFile.content = jsonText(release);
    writeFileSync(depsPath, jsonText(deps));
    refreshModuleReleaseBinding(tempQuickplay, mod.module);
    const unknownReleaseField = runValidator(tempQuickplay, blockKartRoot);
    if (unknownReleaseField.status === 0 || !/unknown field untrusted/.test(`${unknownReleaseField.stdout}${unknownReleaseField.stderr}`)) {
      fail(`validator accepted an unknown release field\n${unknownReleaseField.stdout}${unknownReleaseField.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
    const mod = deps.modules.find((entry) => entry.module === 'github.com/vo-lang/vogui');
    const releaseFile = mod?.files.find((file) => file.path === 'vo.release.json');
    if (!mod || typeof releaseFile?.content !== 'string') fail('could not find vogui release manifest');
    const release = JSON.parse(releaseFile.content);
    release.source.name = 'NUL.tar.gz';
    releaseFile.content = jsonText(release);
    writeFileSync(depsPath, jsonText(deps));
    refreshModuleReleaseBinding(tempQuickplay, mod.module);
    const reservedSourceName = runValidator(tempQuickplay, blockKartRoot);
    if (
      reservedSourceName.status === 0
      || !/source\.name.*portable path component/.test(`${reservedSourceName.stdout}${reservedSourceName.stderr}`)
    ) {
      fail(`validator accepted a reserved release source file name\n${reservedSourceName.stdout}${reservedSourceName.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
    const mod = deps.modules.find((entry) => entry.module === 'github.com/vo-lang/vogui');
    const releaseFile = mod?.files.find((file) => file.path === 'vo.release.json');
    if (!mod || typeof releaseFile?.content !== 'string') fail('could not find vogui release manifest');
    const release = JSON.parse(releaseFile.content);
    release.source.size = 256 * 1024 * 1024 + 1;
    releaseFile.content = jsonText(release);
    writeFileSync(depsPath, jsonText(deps));
    refreshModuleReleaseBinding(tempQuickplay, mod.module);
    const oversizedSourceArchive = runValidator(tempQuickplay, blockKartRoot);
    if (
      oversizedSourceArchive.status === 0
      || !/source\.size must be within 1\.\.=268435456/.test(`${oversizedSourceArchive.stdout}${oversizedSourceArchive.stderr}`)
    ) {
      fail(`validator accepted an oversized release source archive\n${oversizedSourceArchive.stdout}${oversizedSourceArchive.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
    const mod = deps.modules.find((entry) => entry.module === 'github.com/vo-lang/vogui');
    const releaseFile = mod?.files.find((file) => file.path === 'vo.release.json');
    if (!mod || typeof releaseFile?.content !== 'string') fail('could not find vogui release manifest');
    releaseFile.content += ' '.repeat(16 * 1024 * 1024);
    writeFileSync(depsPath, jsonText(deps));
    refreshModuleReleaseBinding(tempQuickplay, mod.module);
    const oversizedReleaseManifest = runValidator(tempQuickplay, blockKartRoot);
    if (
      oversizedReleaseManifest.status === 0
      || !/vo\.release\.json exceeds the 16777216-byte text limit/.test(`${oversizedReleaseManifest.stdout}${oversizedReleaseManifest.stderr}`)
    ) {
      fail(`validator accepted an oversized release manifest\n${oversizedReleaseManifest.stdout}${oversizedReleaseManifest.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const project = JSON.parse(readFileSync(path.join(tempQuickplay, 'project.json'), 'utf8'));
    const deps = JSON.parse(readFileSync(path.join(tempQuickplay, 'deps.json'), 'utf8'));
    const provenance = JSON.parse(readFileSync(path.join(tempQuickplay, 'provenance.json'), 'utf8'));
    const lockFile = project.files.find((file) => file.path === 'vo.lock');
    if (typeof lockFile?.content !== 'string') fail('could not find project vo.lock for rewrite mutation');
    const digest = sha256Digest(Buffer.from(lockFile.content, 'utf8'));
    project.lockRewrite = {
      schemaVersion: 1,
      path: 'vo.lock',
      sourceDigest: digest,
      packagedDigest: digest,
      modules: [],
    };
    provenance.project.lockRewrite = project.lockRewrite;
    provenance.project.filesDigest = packagedFilesDigest(project.files);
    writePackageDocuments(tempQuickplay, project, deps, provenance);
    const incompleteRewrite = runValidator(tempQuickplay, blockKartRoot);
    if (incompleteRewrite.status === 0 || !/lockRewrite modules must exactly project/.test(`${incompleteRewrite.stdout}${incompleteRewrite.stderr}`)) {
      fail(`validator accepted incomplete lock rewrite evidence\n${incompleteRewrite.stdout}${incompleteRewrite.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    writeFile(path.join(tempQuickplay, 'evil.js'), 'export const undeclared = true;\n');
    const undeclaredOutput = runValidator(tempQuickplay, blockKartRoot);
    if (undeclaredOutput.status === 0 || !/undeclared file: evil\.js/.test(`${undeclaredOutput.stdout}${undeclaredOutput.stderr}`)) {
      fail(`validator accepted an undeclared Quickplay output\n${undeclaredOutput.stdout}${undeclaredOutput.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const projectFile = path.join(tempQuickplay, 'project.json');
    const depsFile = path.join(tempQuickplay, 'deps.json');
    const legacyProject = JSON.parse(readFileSync(projectFile, 'utf8'));
    const legacyDeps = JSON.parse(readFileSync(depsFile, 'utf8'));
    legacyProject.schemaVersion = 1;
    legacyDeps.schemaVersion = 1;
    writeFileSync(projectFile, jsonText(legacyProject));
    writeFileSync(depsFile, jsonText(legacyDeps));
    const legacy = runValidator(tempQuickplay, blockKartRoot, {
      QUICKPLAY_ALLOW_LEGACY_V1: '1',
    });
    if (legacy.status === 0 || !/schemaVersion must be 2/.test(`${legacy.stdout}${legacy.stderr}`)) {
      fail(`validator accepted retired schema-v1 input\n${legacy.stdout}${legacy.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsFile = path.join(tempQuickplay, 'deps.json');
    const provenanceFile = path.join(tempQuickplay, 'provenance.json');
    const rawAtDeps = JSON.parse(readFileSync(depsFile, 'utf8'));
    const rawAtProvenance = JSON.parse(readFileSync(provenanceFile, 'utf8'));
    const mod = rawAtDeps.modules.find((entry) => entry.artifacts.length > 0);
    const artifact = mod?.artifacts?.[0];
    const provenanceMod = rawAtProvenance.dependencies.find((entry) => entry.module === mod?.module);
    const provenanceArtifact = provenanceMod?.artifacts?.find((entry) => entry.name === artifact?.name);
    if (!artifact || !provenanceArtifact) fail('could not build schema-v2 raw-@ URL mutation');
    artifact.url = artifact.url.replaceAll('%40', '@');
    provenanceArtifact.url = artifact.url;
    writePackageDocuments(
      tempQuickplay,
      JSON.parse(readFileSync(path.join(tempQuickplay, 'project.json'), 'utf8')),
      rawAtDeps,
      rawAtProvenance,
    );
    const rawAt = runValidator(tempQuickplay, blockKartRoot);
    if (rawAt.status === 0 || !/(non-canonical|URL must be|URL mismatch|Invalid quickplay)/i.test(`${rawAt.stdout}${rawAt.stderr}`)) {
      fail(`validator accepted a schema-v2 raw-@ artifact URL\n${rawAt.stdout}${rawAt.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const malformedProjectPath = path.join(tempQuickplay, 'project.json');
    const malformedProject = JSON.parse(readFileSync(malformedProjectPath, 'utf8'));
    const malformedAsset = malformedProject.files.find((file) => file.path === 'assets/blockkart.vpak');
    if (!malformedAsset?.contentBase64) fail('could not find base64 project asset');
    malformedAsset.contentBase64 += '\n';
    writeFileSync(malformedProjectPath, jsonText(malformedProject));
    const malformedBase64 = runValidator(tempQuickplay, blockKartRoot);
    if (malformedBase64.status === 0 || !/non-canonical base64/.test(`${malformedBase64.stdout}${malformedBase64.stderr}`)) {
      fail(`validator accepted non-canonical base64\n${malformedBase64.stdout}${malformedBase64.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const tooManyFilesPath = path.join(tempQuickplay, 'project.json');
    const tooManyFiles = JSON.parse(readFileSync(tooManyFilesPath, 'utf8'));
    tooManyFiles.files = Array.from({ length: 20_001 }, (_, index) => ({
      path: `generated/${String(index).padStart(5, '0')}.vo`,
      content: '',
    }));
    writeFileSync(tooManyFilesPath, jsonText(tooManyFiles));
    const fileOverflow = runValidator(tempQuickplay, blockKartRoot);
    if (fileOverflow.status === 0 || !/20000(?:-file|[- ]entry) limit/.test(`${fileOverflow.stdout}${fileOverflow.stderr}`)) {
      fail(`validator accepted more than 20,000 package files\n${fileOverflow.stdout}${fileOverflow.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const tooManyModulesPath = path.join(tempQuickplay, 'deps.json');
    const tooManyModules = JSON.parse(readFileSync(tooManyModulesPath, 'utf8'));
    tooManyModules.modules = Array.from({ length: 10_001 }, () => ({}));
    writeFileSync(tooManyModulesPath, jsonText(tooManyModules));
    const moduleOverflow = runValidator(tempQuickplay, blockKartRoot);
    if (moduleOverflow.status === 0 || !/10000-module limit/.test(`${moduleOverflow.stdout}${moduleOverflow.stderr}`)) {
      fail(`validator accepted more than 10,000 modules\n${moduleOverflow.stdout}${moduleOverflow.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const oversizedProjectPath = path.join(tempQuickplay, 'project.json');
    truncateSync(oversizedProjectPath, 128 * 1024 * 1024 + 1);
    const oversizedJson = runValidator(tempQuickplay, blockKartRoot);
    if (oversizedJson.status === 0 || !/134217728-byte limit/.test(`${oversizedJson.stdout}${oversizedJson.stderr}`)) {
      fail(`validator accepted oversized package JSON\n${oversizedJson.stdout}${oversizedJson.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsFile = path.join(tempQuickplay, 'deps.json');
    const actualDeps = JSON.parse(readFileSync(depsFile, 'utf8'));
    const artifact = actualDeps.modules.find((entry) => entry.artifacts.length > 0)?.artifacts?.[0];
    if (!artifact) fail('could not find artifact for byte-drift mutation');
    const artifactRelative = quickplayArtifactRelativePathFromUrl(artifact.url);
    const artifactFile = path.join(tempQuickplay, ...artifactRelative.split('/'));
    const bytes = readFileSync(artifactFile);
    bytes[0] ^= 0xff;
    writeFileSync(artifactFile, bytes);
    const byteDrift = runValidator(tempQuickplay, blockKartRoot);
    if (
      byteDrift.status === 0
      || !/(?:actual bytes digest mismatch|declared digest does not match artifact bytes)/.test(`${byteDrift.stdout}${byteDrift.stderr}`)
    ) {
      fail(`validator accepted artifact byte drift\n${byteDrift.stdout}${byteDrift.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsFile = path.join(tempQuickplay, 'deps.json');
    const oversizedArtifactDeps = JSON.parse(readFileSync(depsFile, 'utf8'));
    const artifact = oversizedArtifactDeps.modules.find((entry) => entry.artifacts.length > 0)?.artifacts?.[0];
    if (!artifact) fail('could not find artifact for size-limit mutation');
    const artifactRelative = quickplayArtifactRelativePathFromUrl(artifact.url);
    truncateSync(path.join(tempQuickplay, ...artifactRelative.split('/')), 256 * 1024 * 1024 + 1);
    const oversizedArtifact = runValidator(tempQuickplay, blockKartRoot);
    if (oversizedArtifact.status === 0 || !/268435456-byte limit/.test(`${oversizedArtifact.stdout}${oversizedArtifact.stderr}`)) {
      fail(`validator accepted an oversized artifact file\n${oversizedArtifact.stdout}${oversizedArtifact.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsFile = path.join(tempQuickplay, 'deps.json');
    const unpackagedDeps = JSON.parse(readFileSync(depsFile, 'utf8'));
    const mod = unpackagedDeps.modules.find((entry) => entry.module === 'github.com/vo-lang/voplay');
    if (!mod || mod.artifacts.length < 2) fail('could not find voplay artifact for unpackaged-web mutation');
    mod.artifacts = mod.artifacts.slice(1);
    writeDepsAndRefreshProvenance(tempQuickplay, unpackagedDeps);
    const unpackaged = runValidator(tempQuickplay, blockKartRoot);
    if (
      unpackaged.status === 0
      || !/(?:packaged.*vo\.web\.json artifact identities differ|omits artifact declared by vo\.web\.json)/.test(`${unpackaged.stdout}${unpackaged.stderr}`)
    ) {
      fail(`validator accepted an unpackaged web artifact\n${unpackaged.stdout}${unpackaged.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);

  const invalidUtf8ProjectPath = path.join(tempQuickplay, 'project.json');
  const invalidUtf8Project = JSON.parse(readFileSync(invalidUtf8ProjectPath, 'utf8'));
  const invalidUtf8Lock = invalidUtf8Project.files.find((file) => file.path === 'vo.lock');
  if (!invalidUtf8Lock) fail('could not find embedded vo.lock for invalid UTF-8 mutation');
  delete invalidUtf8Lock.content;
  invalidUtf8Lock.contentBase64 = Buffer.from([0xff]).toString('base64');
  writeFileSync(invalidUtf8ProjectPath, jsonText(invalidUtf8Project));
  const invalidUtf8 = runValidator(tempQuickplay, blockKartRoot);
  if (invalidUtf8.status === 0) {
    fail('validator accepted invalid UTF-8 vo.lock bytes');
  }
  if (!/vo\.lock.*invalid UTF-8/i.test(`${invalidUtf8.stdout}${invalidUtf8.stderr}`)) {
    fail(`validator rejected invalid UTF-8 vo.lock for the wrong reason\n${invalidUtf8.stdout}${invalidUtf8.stderr}`);
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  const depsPath = path.join(tempQuickplay, 'deps.json');
  const invalidWebDeps = JSON.parse(readFileSync(depsPath, 'utf8'));
  const invalidWebModule = invalidWebDeps.modules.find(
    (mod) => mod.module === 'github.com/vo-lang/vogui',
  );
  const invalidWebFile = invalidWebModule?.files?.find((file) => file.path === 'vo.web.json');
  if (!invalidWebFile || typeof invalidWebFile.content !== 'string') {
    fail('could not find embedded vo.web.json to mutate');
  }
  invalidWebFile.content = `{${' '.repeat(invalidWebFile.content.length - 1)}`;
  writeDepsAndRefreshProvenance(
    tempQuickplay,
    invalidWebDeps,
    [invalidWebModule.module],
  );
  const invalidWeb = runValidator(tempQuickplay, blockKartRoot);
  const invalidWebOutput = `${invalidWeb.stdout}${invalidWeb.stderr}`;
  if (invalidWeb.status === 0) {
    fail('validator accepted vo.web.json bytes that do not match vo.release.json');
  }
  if (!/vo\.release\.json web_manifest digest mismatch/.test(invalidWebOutput)) {
    fail(`validator rejected unbound vo.web.json for the wrong reason\n${invalidWebOutput}`);
  }
  if (/vo\.web\.json is invalid JSON/.test(invalidWebOutput)) {
    fail(`validator parsed vo.web.json before verifying its release binding\n${invalidWebOutput}`);
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  const traversalDeps = JSON.parse(readFileSync(depsPath, 'utf8'));
  const traversalArtifact = traversalDeps.modules
    .find((mod) => mod.module === 'github.com/vo-lang/voplay')
    ?.artifacts?.[0];
  if (!traversalArtifact) {
    fail('could not find embedded artifact to mutate');
  }
  traversalArtifact.path = 'artifacts/extension-js-glue/wasm32-unknown-unknown/../escape.js';
  writeDepsAndRefreshProvenance(tempQuickplay, traversalDeps);
  const traversal = runValidator(tempQuickplay, blockKartRoot);
  if (traversal.status === 0) {
    fail('validator accepted artifact path traversal');
  }
  if (!/artifact.*(path|name)|Invalid.*artifact/i.test(`${traversal.stdout}${traversal.stderr}`)) {
    fail(`validator rejected artifact traversal for the wrong reason\n${traversal.stdout}${traversal.stderr}`);
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  const provenancePath = path.join(tempQuickplay, 'provenance.json');
  const identityProvenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
  const identityArtifact = identityProvenance.dependencies
    .find((mod) => mod.module === 'github.com/vo-lang/voplay')
    ?.artifacts?.[0];
  if (!identityArtifact) {
    fail('could not find provenance artifact to mutate');
  }
  identityArtifact.name = 'forged-name.js';
  writeFileSync(provenancePath, jsonText(identityProvenance));
  const identityDrift = runValidator(tempQuickplay, blockKartRoot);
  if (identityDrift.status === 0) {
    fail('validator accepted provenance artifact identity drift');
  }
  if (!/provenance missing artifact|artifact.*identit/i.test(`${identityDrift.stdout}${identityDrift.stderr}`)) {
    fail(`validator rejected provenance identity drift for the wrong reason\n${identityDrift.stdout}${identityDrift.stderr}`);
  }

  for (const missingField of ['kind', 'target', 'name']) {
    writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
    const malformedProvenance = JSON.parse(readFileSync(provenancePath, 'utf8'));
    const malformedArtifact = malformedProvenance.dependencies
      .find((mod) => mod.module === 'github.com/vo-lang/voplay')
      ?.artifacts?.[0];
    if (!malformedArtifact) {
      fail(`could not find provenance artifact for missing-${missingField} mutation`);
    }
    delete malformedArtifact[missingField];
    writeFileSync(provenancePath, jsonText(malformedProvenance));
    const malformed = runValidator(tempQuickplay, blockKartRoot);
    const malformedOutput = `${malformed.stdout}${malformed.stderr}`;
    if (malformed.status === 0) {
      fail(`validator accepted provenance artifact missing ${missingField}`);
    }
    if (!new RegExp(`invalid identity: Invalid artifact ${missingField}`).test(malformedOutput)) {
      fail(`validator rejected provenance artifact missing ${missingField} for the wrong reason\n${malformedOutput}`);
    }
    const report = JSON.parse(readFileSync(malformed.reportPath, 'utf8'));
    const diagnostic = report.issues?.[0];
    if (
      report.status !== 'failed'
      || diagnostic?.subsystem !== 'ProducerProvenance'
      || diagnostic?.file !== 'apps/studio/public/quickplay/blockkart/provenance.json'
      || !Number.isInteger(diagnostic?.line)
      || diagnostic.line < 1
      || diagnostic?.evidence?.found == null
      || diagnostic?.evidence?.found?.[missingField] !== undefined
      || !diagnostic?.reason?.includes(`artifact ${missingField}`)
    ) {
      fail(`validator did not emit a stable structured diagnostic for missing ${missingField}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  {
    const depsPath = path.join(tempQuickplay, 'deps.json');
    const missingSourceDeps = JSON.parse(readFileSync(depsPath, 'utf8'));
    const vopack = missingSourceDeps.modules.find((mod) => mod.module === 'github.com/vo-lang/vopack');
    const mountIndex = vopack?.files?.findIndex((file) => file.path === 'mount.vo') ?? -1;
    if (!vopack || mountIndex < 0) fail('could not find embedded vopack mount.vo to remove');
    vopack.files.splice(mountIndex, 1);
    writeDepsAndRefreshProvenance(tempQuickplay, missingSourceDeps, [vopack.module]);
    const missingSource = runValidator(tempQuickplay, blockKartRoot);
    if (
      missingSource.status === 0
      || !/omits (?:runtime )?source declared by vo\.web\.json: mount\.vo/.test(`${missingSource.stdout}${missingSource.stderr}`)
    ) {
      fail(`validator accepted a missing runtime source\n${missingSource.stdout}${missingSource.stderr}`);
    }
  }

  writeSyntheticPackage(tempQuickplay, blockKartRoot, assetBytes);
  const deps = JSON.parse(readFileSync(depsPath, 'utf8'));
  const vopack = deps.modules.find((mod) => mod.module === 'github.com/vo-lang/vopack');
  const mount = vopack?.files?.find((file) => file.path === 'mount.vo');
  if (!mount || typeof mount.content !== 'string') {
    fail('could not find embedded vopack mount.vo to mutate');
  }
  mount.content += '\n// source drift self-test mutation\n';
  writeFileSync(depsPath, jsonText(deps));

  const drift = runValidator(tempQuickplay, blockKartRoot);
  if (drift.status === 0) {
    fail('validator accepted mutated embedded dependency source');
  }
  const output = `${drift.stdout}${drift.stderr}`;
  if (!/(?:embedded source (?:size|digest) mismatch|declared (?:size|digest) does not match source bytes)/.test(output)) {
    fail(`validator failed for the wrong reason\n${output}`);
  }
console.log('quickplay validate selftest: ok (5 Vo CLI lineage rejections plus full suite)');
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
