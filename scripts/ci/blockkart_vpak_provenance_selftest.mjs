#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  lstatSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  realpathSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  currentVoCliToolchain,
  currentVoCliBuildInputs,
  verifyVoCliExecutionIdentity,
  verifyVoCliBuildInputs,
} from './quickplay_cli_producer_contract.mjs';
import {
  assertSameBlockKartVpakProducerState,
  observeBlockKartVpakProducer,
  parseBoundedJsonBytes,
} from './quickplay_vnext.mjs';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const sourceRoots = [
  blockKartRoot,
  requireRepoRoot('VOPLAY_ROOT', 'voplay'),
  requireRepoRoot('VOGUI_ROOT', 'vogui'),
  requireRepoRoot('VOPACK_ROOT', 'vopack'),
];
const outDir = path.resolve(process.env.BLOCKKART_VPAK_SELFTEST_OUT_DIR ?? path.join(root, 'target/blockkart-vpak-provenance-selftest'));
const MAX_FIXTURE_FILE_BYTES = 256 * 1024 * 1024;
const MAX_FIXTURE_FILES = 20_000;
const MAX_FIXTURE_TOTAL_BYTES = 512 * 1024 * 1024;
const MAX_BUILD_REPORT_BYTES = 16 * 1024 * 1024;
const MAX_ASSET_DIRECTORY_ENTRIES = 100_000;
const VPAK_TRANSACTION_PREFIX = '.blockkart-vpak-transaction-';
const UTF8_DECODER = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });
const blockKartAssetsDirectory = path.join(blockKartRoot, 'assets');
const transactionBoundaryBeforeReport = observeEmptyVpakTransactionBoundary(
  blockKartAssetsDirectory,
  'BlockKart assets before VPAK report observation',
);
const producerState = observeBlockKartVpakProducer(
  blockKartRoot,
  'BlockKart VPAK provenance selftest producer',
);
const manifest = producerState.producer;
const buildReportBytes = readStableFile(
  root,
  'target/blockkart-vpak-build/report.json',
  'BlockKart VPAK build report',
  null,
  MAX_BUILD_REPORT_BYTES,
);
assert(
  buildReportBytes.byteLength <= MAX_BUILD_REPORT_BYTES,
  `BlockKart VPAK build report exceeds ${MAX_BUILD_REPORT_BYTES} bytes`,
);
const buildReport = parseBoundedJsonBytes(
  buildReportBytes,
  'BlockKart VPAK build report',
  {
    maxBytes: MAX_BUILD_REPORT_BYTES,
    maxDepth: 128,
    maxTokens: 1_000_000,
    maxObjectKeys: 200_000,
    maxObjectKeyBytes: 1024 * 1024,
  },
);
const producerStateAfterBuildReport = observeBlockKartVpakProducer(
  blockKartRoot,
  'BlockKart VPAK provenance selftest producer after build report read',
);
assertSameBlockKartVpakProducerState(
  producerState,
  producerStateAfterBuildReport,
  'BlockKart VPAK producer across build report observation',
);
const transactionBoundaryAfterReport = observeEmptyVpakTransactionBoundary(
  blockKartAssetsDirectory,
  'BlockKart assets after VPAK report observation',
);
assertSameVpakTransactionBoundary(
  transactionBoundaryBeforeReport,
  transactionBoundaryAfterReport,
  'BlockKart VPAK transaction boundary across report and pair observation',
);
const tempRoot = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'blockkart-vpak-provenance-selftest-')),
);
const fixtureRoot = path.join(tempRoot, 'BlockKart');

function sameNativePath(left, right) {
  const normalizedLeft = path.normalize(path.resolve(left));
  const normalizedRight = path.normalize(path.resolve(right));
  return process.platform === 'win32'
    ? normalizedLeft.toUpperCase() === normalizedRight.toUpperCase()
    : normalizedLeft === normalizedRight;
}

function sameStat(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.nlink === right.nlink
    && left.size === right.size
    && left.mtimeNs === right.mtimeNs
    && left.ctimeNs === right.ctimeNs;
}

function directoryEntrySnapshot(entries, label) {
  assert(
    entries.length <= MAX_ASSET_DIRECTORY_ENTRIES,
    `${label} exceeds the ${MAX_ASSET_DIRECTORY_ENTRIES}-entry limit`,
  );
  return entries
    .map((entry) => {
      assert(
        typeof entry.name === 'string' && !entry.name.includes('\0'),
        `${label} contains an invalid entry name`,
      );
      const kind = entry.isDirectory()
        ? 'directory'
        : entry.isFile()
          ? 'file'
          : entry.isSymbolicLink()
            ? 'symlink'
            : 'special';
      return { name: entry.name, kind };
    })
    .sort((left, right) => Buffer.compare(Buffer.from(left.name), Buffer.from(right.name)));
}

function observeEmptyVpakTransactionBoundary(directory, label, operations = {}) {
  const stat = operations.lstatSync ?? lstatSync;
  const readDirectory = operations.readdirSync ?? readdirSync;
  const resolved = path.resolve(directory);
  assert(
    sameNativePath(realpathSync.native(resolved), resolved),
    `${label} must be a canonical directory without symbolic-link path components`,
  );
  const before = stat(resolved, { bigint: true });
  assert(
    before.isDirectory() && !before.isSymbolicLink(),
    `${label} must be a real directory`,
  );
  const firstEntries = directoryEntrySnapshot(
    readDirectory(resolved, { withFileTypes: true }),
    `${label} first scan`,
  );
  operations.afterFirstRead?.(resolved);
  const secondEntries = directoryEntrySnapshot(
    readDirectory(resolved, { withFileTypes: true }),
    `${label} second scan`,
  );
  const after = stat(resolved, { bigint: true });
  assert(
    sameStat(before, after) && JSON.stringify(firstEntries) === JSON.stringify(secondEntries),
    `${label} changed while it was scanned`,
  );
  const transactions = secondEntries.filter((entry) => (
    entry.name.startsWith(VPAK_TRANSACTION_PREFIX)
  ));
  assert.equal(
    transactions.length,
    0,
    `${label} contains VPAK transaction state; run canonical producer recovery first: `
      + transactions.map((entry) => `${entry.name} (${entry.kind})`).join(', '),
  );
  return Object.freeze({
    directory: resolved,
    entries: Object.freeze(secondEntries.map((entry) => Object.freeze(entry))),
    metadata: after,
  });
}

function assertSameVpakTransactionBoundary(left, right, label) {
  assert(
    sameNativePath(left.directory, right.directory)
      && sameStat(left.metadata, right.metadata)
      && JSON.stringify(left.entries) === JSON.stringify(right.entries),
    `${label} changed`,
  );
}

function sha256(bytes) {
  return createHash('sha256').update(bytes).digest('hex');
}

function validateRelative(relative, label) {
  assert.equal(typeof relative, 'string', `${label} must be a string`);
  assert(
    relative.length > 0
      && relative.length <= 1024
      && !relative.includes('\\')
      && !relative.includes('\0')
      && !relative.startsWith('/')
      && !path.posix.isAbsolute(relative),
    `${label} must be a bounded portable relative path`,
  );
  const components = relative.split('/');
  assert(
    components.every((component) => /^[A-Za-z0-9._-]+$/u.test(component) && component !== '.' && component !== '..'),
    `${label} contains a non-portable component`,
  );
  return components;
}

function readStableFile(
  sourceRoot,
  relative,
  label,
  expected = null,
  maxBytes = MAX_FIXTURE_FILE_BYTES,
) {
  const components = validateRelative(relative, label);
  const source = path.resolve(sourceRoot, ...components);
  const rootRelative = path.relative(sourceRoot, source);
  assert(
    rootRelative !== '..' && !rootRelative.startsWith(`..${path.sep}`) && !path.isAbsolute(rootRelative),
    `${label} escapes its source root`,
  );
  assert(sameNativePath(realpathSync.native(source), source), `${label} traverses a symbolic-link path`);
  const before = lstatSync(source, { bigint: true });
  assert(before.isFile() && !before.isSymbolicLink(), `${label} must be a real regular file`);
  assert(before.size <= BigInt(maxBytes), `${label} exceeds the ${maxBytes}-byte limit`);
  const bytes = readFileSync(source);
  const after = lstatSync(source, { bigint: true });
  assert(sameStat(before, after), `${label} changed while it was copied`);
  if (expected !== null) {
    assert.equal(expected.size, bytes.byteLength, `${label} size differs from provenance`);
    assert.equal(expected.sha256, sha256(bytes), `${label} digest differs from provenance`);
  }
  return bytes;
}

function moduleIdentity(sourceRoot) {
  const source = UTF8_DECODER.decode(readStableFile(sourceRoot, 'vo.mod', `${sourceRoot} vo.mod`));
  const matches = [...source.matchAll(/^module = "([a-z0-9][a-z0-9._/-]*)"$/gmu)];
  assert.equal(matches.length, 1, `${sourceRoot}/vo.mod must contain one canonical module declaration`);
  return matches[0][1];
}

function assertBuildReportProducerBinding(report, currentProducer, label) {
  assert(
    report !== null && typeof report === 'object' && !Array.isArray(report),
    `${label} must be an object`,
  );
  assert.deepEqual(
    Object.keys(report).sort(),
    [
      'archiveEntryCount',
      'ciRunId',
      'kind',
      'pack',
      'payloadInputCount',
      'producerDigest',
      'producerManifest',
      'schemaVersion',
      'toolchain',
      'voBinary',
      'voCliBuildInputs',
      'voCliExecutionDigest',
      'workspaceSourceInputCount',
    ],
    `${label} must contain exact fields`,
  );
  assert.equal(report.schemaVersion, 2, `${label} schema version is invalid`);
  assert.equal(report.kind, 'blockkart.vpakBuildReport', `${label} kind is invalid`);
  assert.deepEqual(
    Object.keys(report.producerManifest ?? {}).sort(),
    ['digest', 'path', 'size'],
    `${label} producerManifest must contain exact fields`,
  );
  assert.equal(
    report.producerManifest.path,
    'assets/blockkart.vpak.provenance.json',
    `${label} producerManifest path is invalid`,
  );
  assert.equal(
    report.producerManifest.digest,
    currentProducer.manifest.digest,
    `${label} producerManifest digest does not bind the current stable provenance`,
  );
  assert.equal(
    report.producerManifest.size,
    currentProducer.manifest.size,
    `${label} producerManifest size does not bind the current stable provenance`,
  );
  assert.deepEqual(
    Object.keys(report.pack ?? {}).sort(),
    ['path', 'sha256', 'size'],
    `${label} pack must contain exact fields`,
  );
  assert.deepEqual(
    report.pack,
    currentProducer.producer.pack,
    `${label} pack does not bind the current producer manifest`,
  );
  assert.equal(
    `sha256:${report.pack.sha256}`,
    currentProducer.pack.digest,
    `${label} pack digest does not bind the current stable VPAK`,
  );
  assert.equal(
    report.pack.size,
    currentProducer.pack.size,
    `${label} pack size does not bind the current stable VPAK`,
  );
  assert.equal(
    report.producerDigest,
    currentProducer.producer.producerDigest,
    `${label} producerDigest does not bind the current producer`,
  );
  for (const field of [
    'archiveEntryCount',
    'payloadInputCount',
    'workspaceSourceInputCount',
  ]) {
    assert.equal(
      report[field],
      currentProducer.producer[field],
      `${label} ${field} does not bind the current producer`,
    );
  }
}

const sourcesByModule = new Map();
for (const sourceRoot of sourceRoots) {
  const module = moduleIdentity(sourceRoot);
  assert(!sourcesByModule.has(module), `duplicate fixture source module ${module}`);
  sourcesByModule.set(module, sourceRoot);
}
const fixtureRootsByModule = new Map([...sourcesByModule].map(([module, sourceRoot]) => [
  module,
  sameNativePath(sourceRoot, blockKartRoot)
    ? fixtureRoot
    : path.join(tempRoot, path.basename(sourceRoot)),
]));
assert.equal(
  new Set([...fixtureRootsByModule.values()].map((value) => path.resolve(value).toLowerCase())).size,
  fixtureRootsByModule.size,
  'fixture workspace roots must have distinct real directories',
);
for (const fixtureModuleRoot of fixtureRootsByModule.values()) {
  mkdirSync(fixtureModuleRoot, { recursive: true });
  const metadata = lstatSync(fixtureModuleRoot, { bigint: true });
  assert(metadata.isDirectory() && !metadata.isSymbolicLink(), 'fixture workspace members must be real directories');
}

const orderedModules = [...sourcesByModule.keys()].sort((left, right) => right.length - left.length);
function workspaceFactLocation(logicalPath) {
  assert(logicalPath.startsWith('workspace:'), `invalid workspace fact ${logicalPath}`);
  const module = orderedModules.find((candidate) => logicalPath.startsWith(`workspace:${candidate}/`));
  assert(module, `workspace fact has no declared fixture source: ${logicalPath}`);
  const relative = logicalPath.slice(`workspace:${module}/`.length);
  validateRelative(relative, `workspace fact ${logicalPath}`);
  return {
    destinationRoot: fixtureRootsByModule.get(module),
    module,
    relative,
    sourceRoot: sourcesByModule.get(module),
  };
}

const copiedDestinations = new Map();
let copiedBytes = 0;
function copyToFixture({ destinationRoot, expected = null, relative, sourceRoot }) {
  const destination = path.resolve(destinationRoot, ...validateRelative(relative, `fixture ${relative}`));
  const destinationKey = destination.toLowerCase();
  const previous = copiedDestinations.get(destinationKey);
  if (previous !== undefined) {
    assert.deepEqual(previous, expected, `fixture destination has conflicting provenance ${destination}`);
    return;
  }
  if (copiedDestinations.size >= MAX_FIXTURE_FILES) throw new Error('fixture exceeds its file-count limit');
  copiedDestinations.set(destinationKey, expected);
  const bytes = readStableFile(sourceRoot, relative, `fixture source ${relative}`, expected);
  if (copiedBytes > MAX_FIXTURE_TOTAL_BYTES - bytes.byteLength) {
    throw new Error(`fixture exceeds the ${MAX_FIXTURE_TOTAL_BYTES}-byte aggregate limit`);
  }
  copiedBytes += bytes.byteLength;
  mkdirSync(path.dirname(destination), { recursive: true });
  writeFileSync(destination, bytes, { flag: 'wx' });
  assert(readFileSync(destination).equals(bytes), `fixture destination differs after writing ${relative}`);
}

function runVpakTransactionBoundarySelftest() {
  const boundaryFixture = path.join(tempRoot, 'transaction-boundary');
  const reset = () => {
    rmSync(boundaryFixture, { recursive: true, force: true });
    mkdirSync(boundaryFixture, { mode: 0o755 });
  };
  const transactionName = (suffix) => (
    `${VPAK_TRANSACTION_PREFIX}00000000-0000-4000-8000-${suffix.padStart(12, '0')}`
  );
  const seedJournal = (suffix, contents) => {
    const transaction = path.join(boundaryFixture, transactionName(suffix));
    mkdirSync(transaction, { mode: 0o700 });
    writeFileSync(path.join(transaction, 'transaction.json'), contents, { flag: 'wx' });
  };

  for (const [phase, suffix] of [['prepared', '1'], ['committed', '2']]) {
    reset();
    const emptyBinding = { digest: `sha256:${'0'.repeat(64)}`, size: 0 };
    seedJournal(suffix, `${JSON.stringify({
      schemaVersion: 1,
      kind: 'blockkart.vpakPairTransaction',
      phase,
      hadPrevious: false,
      previous: null,
      next: { pack: emptyBinding, provenance: emptyBinding },
    })}\n`);
    assert.throws(
      () => observeEmptyVpakTransactionBoundary(
        boundaryFixture,
        `${phase} transaction boundary fixture`,
      ),
      /contains VPAK transaction state/u,
    );
  }

  reset();
  seedJournal('3', '{corrupt transaction journal');
  assert.throws(
    () => observeEmptyVpakTransactionBoundary(
      boundaryFixture,
      'corrupt transaction boundary fixture',
    ),
    /contains VPAK transaction state/u,
  );

  reset();
  writeFileSync(
    path.join(boundaryFixture, `${VPAK_TRANSACTION_PREFIX}invalid-entry`),
    'invalid transaction entry',
    { flag: 'wx' },
  );
  assert.throws(
    () => observeEmptyVpakTransactionBoundary(
      boundaryFixture,
      'invalid transaction entry boundary fixture',
    ),
    /contains VPAK transaction state/u,
  );

  reset();
  let scanMutationInjected = false;
  assert.throws(
    () => observeEmptyVpakTransactionBoundary(
      boundaryFixture,
      'changing transaction boundary fixture',
      {
        afterFirstRead(directory) {
          scanMutationInjected = true;
          mkdirSync(path.join(directory, transactionName('4')), { mode: 0o700 });
        },
      },
    ),
    /changed while it was scanned/u,
  );
  assert.equal(scanMutationInjected, true);
  reset();
  observeEmptyVpakTransactionBoundary(
    boundaryFixture,
    'empty transaction boundary fixture',
  );
}

try {
  runVpakTransactionBoundarySelftest();
  assertBuildReportProducerBinding(
    buildReport,
    producerState,
    'BlockKart VPAK build report',
  );
  const producerBindingMutations = [
    ['producerManifest.path', (report) => {
      report.producerManifest.path = 'assets/stale.vpak.provenance.json';
    }],
    ['producerManifest.digest', (report) => {
      report.producerManifest.digest = `sha256:${'0'.repeat(64)}`;
    }],
    ['producerManifest.size', (report) => {
      report.producerManifest.size += 1;
    }],
    ['pack.path', (report) => {
      report.pack.path = 'assets/stale.vpak';
    }],
    ['pack.sha256', (report) => {
      report.pack.sha256 = '0'.repeat(64);
    }],
    ['pack.size', (report) => {
      report.pack.size += 1;
    }],
    ['producerDigest', (report) => {
      report.producerDigest = '0'.repeat(64);
    }],
    ['archiveEntryCount', (report) => {
      report.archiveEntryCount += 1;
    }],
    ['payloadInputCount', (report) => {
      report.payloadInputCount += 1;
    }],
    ['workspaceSourceInputCount', (report) => {
      report.workspaceSourceInputCount += 1;
    }],
  ];
  for (const [field, mutate] of producerBindingMutations) {
    const drifted = structuredClone(buildReport);
    mutate(drifted);
    assert.throws(
      () => assertBuildReportProducerBinding(
        drifted,
        producerState,
        `drifted BlockKart VPAK build report ${field}`,
      ),
      `BlockKart VPAK build report must reject ${field} drift`,
    );
  }

  const currentCliInputs = currentVoCliBuildInputs(root);
  const currentCliToolchain = currentVoCliToolchain(root);
  assert.deepEqual(
    verifyVoCliBuildInputs(buildReport.voCliBuildInputs, { expected: currentCliInputs }),
    [],
    'BlockKart build report must bind the current locked Vo CLI source closure',
  );
  assert.equal(
    buildReport.ciRunId,
    process.env.VO_DEV_CI_RUN_ID ?? null,
    'BlockKart build report must come from the current task graph run',
  );
  assert.deepEqual(
    verifyVoCliExecutionIdentity(buildReport.toolchain, buildReport.voBinary, {
      buildInputs: buildReport.voCliBuildInputs,
      executionDigest: buildReport.voCliExecutionDigest,
      expectedToolchain: currentCliToolchain,
    }),
    [],
    'BlockKart build report must bind its actual pinned toolchain and stable Vo binary',
  );
  const missingCliInput = structuredClone(buildReport.voCliBuildInputs);
  missingCliInput.inputs.pop();
  assert(
    verifyVoCliBuildInputs(missingCliInput, { expected: currentCliInputs }).length > 0,
    'Vo CLI build-input validation must reject an omitted producer input',
  );
  assert(
    verifyVoCliExecutionIdentity(undefined, buildReport.voBinary, {
      buildInputs: buildReport.voCliBuildInputs,
      executionDigest: buildReport.voCliExecutionDigest,
      expectedToolchain: currentCliToolchain,
    }).length > 0,
    'Vo CLI execution validation must reject a missing toolchain identity',
  );
  assert(
    verifyVoCliExecutionIdentity(buildReport.toolchain, buildReport.voBinary, {
      buildInputs: buildReport.voCliBuildInputs,
      executionDigest: undefined,
      expectedToolchain: currentCliToolchain,
    }).length > 0,
    'Vo CLI execution validation must reject a missing execution digest',
  );
  assert(
    verifyVoCliExecutionIdentity(buildReport.toolchain, {
      ...buildReport.voBinary,
      digest: `sha256:${'0'.repeat(64)}`,
    }, {
      buildInputs: buildReport.voCliBuildInputs,
      executionDigest: buildReport.voCliExecutionDigest,
      expectedToolchain: currentCliToolchain,
    }).length > 0,
    'Vo CLI execution validation must reject binary digest drift',
  );
  const driftedCliInput = structuredClone(buildReport.voCliBuildInputs);
  driftedCliInput.inputs[0].digest = `sha256:${'0'.repeat(64)}`;
  assert(
    verifyVoCliBuildInputs(driftedCliInput, { expected: currentCliInputs }).length > 0,
    'Vo CLI build-input validation must reject producer source drift',
  );
  const voBinaryRelative = buildReport.voBinary?.path;
  const voBinaryComponents = validateRelative(voBinaryRelative, 'build report Vo binary path');
  const voBinary = path.resolve(root, ...voBinaryComponents);
  const voBinaryBytes = readStableFile(root, voBinaryRelative, 'built Vo binary', {
    sha256: String(buildReport.voBinary.digest).replace(/^sha256:/u, ''),
    size: buildReport.voBinary.size,
  });
  assert(voBinaryBytes.byteLength > 0, 'built Vo binary must not be empty');
  const voBinaryMetadata = lstatSync(voBinary, { bigint: true });
  assert(
    voBinaryMetadata.isFile()
      && !voBinaryMetadata.isSymbolicLink()
      && voBinaryMetadata.nlink === 1n
      && sameNativePath(realpathSync.native(voBinary), voBinary),
    'built Vo binary must be a singly-linked real file',
  );

  assert(Array.isArray(manifest.inputs), 'canonical producer manifest must contain an input array');
  assert(manifest.inputs.length > 0 && manifest.inputs.length <= MAX_FIXTURE_FILES, 'producer input count is invalid');
  for (const [index, entry] of manifest.inputs.entries()) {
    assert.deepEqual(
      Object.keys(entry).sort(),
      ['path', 'sha256', 'size'],
      `producer input ${index} must contain exact fields`,
    );
    assert(/^[0-9a-f]{64}$/u.test(entry.sha256), `producer input ${index} digest is invalid`);
    assert(
      Number.isSafeInteger(entry.size) && entry.size >= 0 && entry.size <= MAX_FIXTURE_FILE_BYTES,
      `producer input ${index} size is invalid`,
    );
    const location = entry.path.startsWith('workspace:')
      ? workspaceFactLocation(entry.path)
      : {
          destinationRoot: fixtureRoot,
          relative: entry.path,
          sourceRoot: blockKartRoot,
        };
    assert.notEqual(location.relative, 'vo.lock', 'VPAK provenance fixture must not depend on vo.lock');
    copyToFixture({ ...location, expected: { sha256: entry.sha256, size: entry.size } });
  }
  for (const [module, fixtureModuleRoot] of fixtureRootsByModule) {
    const moduleManifest = path.resolve(fixtureModuleRoot, 'vo.mod');
    if (!copiedDestinations.has(moduleManifest.toLowerCase())) {
      copyToFixture({
        destinationRoot: fixtureModuleRoot,
        relative: 'vo.mod',
        sourceRoot: sourcesByModule.get(module),
      });
    }
    const moduleFile = lstatSync(path.join(fixtureModuleRoot, 'vo.mod'), { bigint: true });
    assert(
      moduleFile.isFile() && !moduleFile.isSymbolicLink(),
      `fixture workspace module ${module} must be bound by a copied vo.mod`,
    );
  }
  assert.deepEqual(
    Object.keys(manifest.pack).sort(),
    ['path', 'sha256', 'size'],
    'canonical producer pack must contain exact fields',
  );
  copyToFixture({
    destinationRoot: fixtureRoot,
    expected: { sha256: manifest.pack.sha256, size: manifest.pack.size },
    relative: manifest.pack.path,
    sourceRoot: blockKartRoot,
  });
  copyToFixture({
    destinationRoot: fixtureRoot,
    expected: {
      sha256: producerState.manifest.digest.replace(/^sha256:/u, ''),
      size: producerState.manifest.size,
    },
    relative: 'assets/blockkart.vpak.provenance.json',
    sourceRoot: blockKartRoot,
  });

  const fixtureEnvironment = {
    ...process.env,
    VO_BIN: voBinary,
    VO_MOD_CACHE: path.join(tempRoot, 'mod-cache'),
  };
  mkdirSync(fixtureEnvironment.VO_MOD_CACHE, { recursive: true });
  execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
    cwd: fixtureRoot,
    env: fixtureEnvironment,
    stdio: 'pipe',
    timeout: 60_000,
  });
  const payload = manifest.archiveEntries[0]?.sourcePath;
  if (!payload) throw new Error('canonical manifest has no payload fixture');
  const payloadPath = path.join(fixtureRoot, payload);
  writeFileSync(payloadPath, Buffer.concat([readFileSync(payloadPath), Buffer.from('\nnegative provenance mutation\n')]));
  const negative = spawnSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
    cwd: fixtureRoot,
    encoding: 'utf8',
    env: fixtureEnvironment,
    maxBuffer: 4 * 1024 * 1024,
    timeout: 60_000,
  });
  if (negative.error) throw negative.error;
  if (negative.status === 0) {
    throw new Error('vpak provenance checker accepted a mutated payload source');
  }
  const transactionBoundaryBeforeFinalProducer = observeEmptyVpakTransactionBoundary(
    blockKartAssetsDirectory,
    'BlockKart assets before final VPAK producer observation',
  );
  assertSameVpakTransactionBoundary(
    transactionBoundaryBeforeReport,
    transactionBoundaryBeforeFinalProducer,
    'BlockKart VPAK transaction boundary across provenance selftest',
  );
  const finalProducerState = observeBlockKartVpakProducer(
    blockKartRoot,
    'BlockKart VPAK provenance selftest final producer',
  );
  assertSameBlockKartVpakProducerState(
    producerState,
    finalProducerState,
    'BlockKart VPAK producer across provenance selftest',
  );
  const transactionBoundaryAfterFinalProducer = observeEmptyVpakTransactionBoundary(
    blockKartAssetsDirectory,
    'BlockKart assets after final VPAK producer observation',
  );
  assertSameVpakTransactionBoundary(
    transactionBoundaryBeforeFinalProducer,
    transactionBoundaryAfterFinalProducer,
    'BlockKart VPAK transaction boundary across final producer observation',
  );
  mkdirSync(outDir, { recursive: true });
  writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify({
    schemaVersion: 1,
    kind: 'blockkart.vpakProvenanceSelftest',
    status: 'pass',
    mutatedPath: payload,
    fixtureModuleCount: fixtureRootsByModule.size,
    fixtureFileCount: copiedDestinations.size,
    fixtureBytes: copiedBytes,
    voCliPackageCount: currentCliInputs.packages.length,
    voCliInputCount: currentCliInputs.inputs.length,
    rejection: `${negative.stdout ?? ''}${negative.stderr ?? ''}`.slice(-2000),
  }, null, 2)}\n`);
  console.log(`blockkart vpak provenance selftest: ok rejected ${payload}`);
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
