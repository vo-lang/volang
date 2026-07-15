#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { createHash, randomBytes } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  existsSync,
  fstatSync,
  fsyncSync,
  lstatSync,
  mkdirSync,
  openSync,
  opendirSync,
  readSync,
  realpathSync,
  renameSync,
  unlinkSync,
  writeSync,
} from 'node:fs';
import {
  basename,
  dirname,
  isAbsolute,
  join,
  parse as parsePath,
  posix,
  relative,
  resolve,
  sep,
} from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { portablePathCollisionKey } from './portable_path_key.mjs';
import { requiredVpakProducerInputPaths } from './blockkart_vpak_policy.mjs';
import {
  artifactCachePath,
  artifactKey,
  moduleCacheDir,
  quickplayArtifactRelativePathFromUrl,
  quickplayArtifactUrl,
  validatePortableRelativePath,
} from './quickplay_artifact_paths.mjs';
import {
  currentVoCliToolchain,
  currentVoCliBuildInputs,
  VO_CLI_GUEST_ENVIRONMENT,
  verifyVoCliExecutionIdentity,
  verifyVoCliBuildInputs,
} from './quickplay_cli_producer_contract.mjs';
import {
  QUICKPLAY_ARTIFACT_NAME,
  QUICKPLAY_ARTIFACT_PATH,
  QUICKPLAY_GENERATOR_COMMAND,
  QUICKPLAY_GENERATOR_INPUTS,
  QUICKPLAY_GENERATOR_SOURCE_INPUTS,
  QUICKPLAY_GENERATOR_VERSION,
  QUICKPLAY_SOURCE_ROOTS,
  QUICKPLAY_TASK_ID,
  quickplayBlockKartSourceAllowlist,
  quickplayGeneratorSourceDigest,
} from './quickplay_generator_contract.mjs';
import { validateVoplayRendererContract } from './quickplay_renderer_contract.mjs';
import { validateStudioQuickplayTypeScriptContract } from './quickplay_typescript_contract.mjs';
import {
  parseVoModWebMetadata,
  validatePortablePathComponent,
  validateWebManifestVoModContract,
} from './quickplay_web_manifest_contract.mjs';
import {
  sourceBoundEvidence,
  verifySourceBoundEvidence,
} from './source_bound_evidence.mjs';
import { compareUtf8 } from './utf8_order.mjs';
import {
  decodeModuleTextUtf8,
  parseVoLockForV2Migration,
  parseVoLockV2,
  parseVoModRootContract,
  validatePackagedModuleSet,
  validateVoLockV2RootGraph,
} from './vo_lock_v2.mjs';
import {
  currentVoplayWasmBuildPlatform,
  lockedVoplayBuildInputs,
  verifyVoplaySourceClosure,
  verifyVoplayVolangBuildInputs,
  voplayFfiSourceFingerprint,
  VOPLAY_WASM_PRODUCER_COMMAND,
  VOPLAY_WASM_REQUIRED_OUTPUTS,
} from './voplay_current_wasm.mjs';

function canonicalExistingDirectory(path, label) {
  const canonical = realpathSync.native(resolve(path));
  const metadata = lstatSync(canonical);
  if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
    throw new Error(`${label} must resolve to a real directory: ${canonical}`);
  }
  return canonical;
}

const root = canonicalExistingDirectory(fileURLToPath(new URL('../..', import.meta.url)), 'Volang root');
const quickplayDir = canonicalExistingDirectory(
  process.env.QUICKPLAY_DIR ?? join(root, 'apps/studio/public/quickplay/blockkart'),
  'Quickplay package root',
);
const projectPath = join(quickplayDir, 'project.json');
const depsPath = join(quickplayDir, 'deps.json');
const provenancePath = join(quickplayDir, 'provenance.json');
const quickplayTsPath = join(root, 'apps/studio/src/lib/quickplay.ts');
const blockKartRoot = canonicalExistingDirectory(
  requireRepoRoot('BLOCKKART_ROOT', 'BlockKart'),
  'BlockKart root',
);
const outDir = resolve(process.env.QUICKPLAY_VALIDATE_OUT_DIR || join(root, 'target/quickplay-validate'));
const selftestMode = process.env.QUICKPLAY_VALIDATE_SELFTEST === '1';
let packageSchemaVersion = null;
const dependencyRepos = selftestMode
  ? []
  : [
      {
        name: 'github.com/vo-lang/vogui',
        root: canonicalExistingDirectory(requireRepoRoot('VOGUI_ROOT', 'vogui'), 'vogui root'),
      },
      {
        name: 'github.com/vo-lang/vopack',
        root: canonicalExistingDirectory(requireRepoRoot('VOPACK_ROOT', 'vopack'), 'vopack root'),
      },
      {
        name: 'github.com/vo-lang/voplay',
        root: canonicalExistingDirectory(requireRepoRoot('VOPLAY_ROOT', 'voplay'), 'voplay root'),
      },
    ];

function quickplayEvidenceScope() {
  return {
    repos: [
      { name: 'volang', root },
      { name: 'BlockKart', root: blockKartRoot },
      ...dependencyRepos,
    ],
    gateFiles: [
      'scripts/ci/quickplay_validate.mjs',
      'scripts/ci/blockkart_vpak_policy.mjs',
      'scripts/ci/repo_roots.mjs',
      'scripts/ci/source_bound_evidence.mjs',
      'scripts/ci/quickplay_artifact_paths.mjs',
      'scripts/ci/quickplay_cli_producer_contract.mjs',
      'scripts/ci/blockkart_vpak_build.mjs',
      'scripts/ci/blockkart_vpak_provenance_selftest.mjs',
      'scripts/ci/quickplay_generator_contract.mjs',
      'scripts/ci/quickplay_renderer_contract.mjs',
      'scripts/ci/quickplay_typescript_contract.mjs',
      'scripts/ci/quickplay_web_manifest_contract.mjs',
      'scripts/ci/portable_path_key.mjs',
      'scripts/ci/unicode_casefold_data.mjs',
      'scripts/ci/utf8_order.mjs',
      'scripts/ci/vo_lock_v2.mjs',
      'apps/studio/scripts/package_blockkart_quickplay.mjs',
      'apps/studio/src/lib/quickplay.ts',
      'eng/artifacts.toml',
      'eng/tasks.toml',
      'eng/project.toml',
    ],
    artifacts: [quickplayDir],
  };
}

const MAX_JSON_BYTES = 128 * 1024 * 1024;
const MAX_METADATA_BYTES = 16 * 1024 * 1024;
const MAX_MODULES = 10_000;
const MAX_FILES = 20_000;
const MAX_WALK_ENTRIES = 100_000;
const MAX_FILE_BYTES = 256 * 1024 * 1024;
const MAX_TOTAL_BYTES = 512 * 1024 * 1024;
const MAX_SOURCE_BYTES = 64 * 1024 * 1024;
const MAX_DEPTH = 256;
const MAX_PATH_BYTES = 4 * 1024;
const MAX_VALIDATION_IO_BYTES = 1024 * 1024 * 1024;
const IO_BUFFER_BYTES = 1024 * 1024;
const MAX_JSON_TOKENS = 2_000_000;
const MAX_JSON_OBJECT_KEYS = 50_000;
const MAX_REPORT_BYTES = 64 * 1024 * 1024;
const MAX_REPORT_TEMP_ATTEMPTS = 16;
const QUICKPLAY_PROJECT_VFS_ROOT = '/home/vo/.studio/sessions/quickplay/BlockKart/current';
const REPORT_FILE_NAME = 'report.json';
const REPORT_TEMP_PREFIX = '.report.json.';
const FORBIDDEN_PACKAGE_DIRECTORIES = new Set([
  '.codex-release-stage',
  '.git',
  'node_modules',
  'target',
  'tmp_checks',
]);
const stableFiles = new Map();
const moduleFileByteCache = new WeakMap();
let validationIoBytes = 0;

const expected = {
  projectName: 'BlockKart',
  projectModule: 'github.com/vo-lang/blockkart',
  requiredModules: [
    'github.com/vo-lang/vogui',
    'github.com/vo-lang/voplay',
  ],
  artifactName: QUICKPLAY_ARTIFACT_NAME,
  artifactPath: QUICKPLAY_ARTIFACT_PATH,
  generatorCommand: QUICKPLAY_GENERATOR_COMMAND,
  sourceRoots: QUICKPLAY_SOURCE_ROOTS,
  generatorInputs: QUICKPLAY_GENERATOR_INPUTS,
};

const requiredVpakProducerOutput = 'assets/blockkart.vpak';
const requiredVpakProducerManifest = 'assets/blockkart.vpak.provenance.json';
const requiredTerrainProducerInputs = [
  'tools/generate_primitive_terrain.mjs',
  'tools/terrain_heightfield_spec.mjs',
  'tools/terrain_recipe.mjs',
  'terrain/recipes/primitive_concept_v1.json',
  'assets/source/terrain_painted/grass_painted_v1.png',
  'assets/source/terrain_painted/meadow_painted_v1.png',
  'assets/source/terrain_painted/dirt_painted_v1.png',
  'assets/source/terrain_painted/rock_painted_v1.png',
  'assets/effects/grass_card_atlas.png',
];
const requiredTerrainProducerOutputs = [
  'assets/maps/primitive_track/lowpoly_terrain.glb',
  'assets/maps/primitive_track/lowpoly_terrain_lod.glb',
  'assets/maps/primitive_track/lowpoly_terrain_height_grid.bin',
  'assets/maps/primitive_track/terrain_splat_large.png',
];
const requiredPaintProducerInputs = [
  'tools/paint_terrain_textures.mjs',
  'docs/images/terrain-upgrade-concept-v1.png',
];
const requiredPaintProducerOutputs = [
  'assets/source/terrain_painted/grass_painted_v1.png',
  'assets/source/terrain_painted/meadow_painted_v1.png',
  'assets/source/terrain_painted/dirt_painted_v1.png',
  'assets/source/terrain_painted/rock_painted_v1.png',
  'assets/effects/grass_card_atlas.png',
];

function structuredIssue(message, evidence = {}) {
  return {
    owner: evidence.owner ?? 'Volang',
    subsystem: evidence.subsystem ?? 'Quickplay',
    severity: evidence.severity ?? 'P0',
    message,
    file: evidence.file ?? null,
    line: evidence.line ?? 1,
    reason: evidence.reason ?? message,
    requiredFix: evidence.requiredFix ?? 'Update the current source/provenance and rerun this gate from fresh checked-out sources.',
    evidence,
  };
}

function fileFingerprint(stat) {
  return {
    dev: stat.dev,
    ino: stat.ino,
    size: stat.size,
    mtimeMs: stat.mtimeMs,
    ctimeMs: stat.ctimeMs,
  };
}

function sameFingerprint(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.size === right.size
    && left.mtimeMs === right.mtimeMs
    && left.ctimeMs === right.ctimeMs;
}

function chargeValidationIo(size, label) {
  validationIoBytes += size;
  if (!Number.isSafeInteger(validationIoBytes) || validationIoBytes > MAX_VALIDATION_IO_BYTES) {
    throw new Error(`${label} exceeds the ${MAX_VALIDATION_IO_BYTES}-byte validation I/O budget`);
  }
}

function pathIsWithin(path, candidateRoot) {
  const suffix = relative(candidateRoot, path);
  return suffix === '' || (!suffix.startsWith(`..${sep}`) && suffix !== '..' && !isAbsolute(suffix));
}

function trustedRootForPath(path) {
  const candidates = [quickplayDir, blockKartRoot, ...dependencyRepos.map((repo) => repo.root), root]
    .map((candidate) => resolve(candidate))
    .filter((candidate) => pathIsWithin(path, candidate))
    .sort((left, right) => right.length - left.length);
  if (candidates.length === 0) {
    throw new Error(`file is outside every trusted validation root: ${path}`);
  }
  return candidates[0];
}

function assertNoSymlinkAncestors(path, label) {
  const trustedRoot = trustedRootForPath(path);
  const rootMetadata = lstatSync(trustedRoot);
  if (!rootMetadata.isDirectory() || rootMetadata.isSymbolicLink()) {
    throw new Error(`${label} trusted root must be a real directory: ${trustedRoot}`);
  }
  const suffix = relative(trustedRoot, path);
  const components = suffix.split(sep).filter(Boolean);
  let current = trustedRoot;
  for (const component of components.slice(0, -1)) {
    current = join(current, component);
    const metadata = lstatSync(current);
    if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
      throw new Error(`${label} has a symlink or non-directory ancestor: ${current}`);
    }
  }
}

function openStableRegularFile(path, label, maxBytes) {
  assertNoSymlinkAncestors(path, label);
  let before;
  try {
    before = lstatSync(path);
  } catch (error) {
    throw new Error(`${label} cannot be inspected: ${error.message}`);
  }
  if (!before.isFile() || before.isSymbolicLink()) {
    throw new Error(`${label} must be a regular file without symlinks`);
  }
  if (!Number.isSafeInteger(before.size) || before.size < 0 || before.size > maxBytes) {
    throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
  }

  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  let fd;
  try {
    fd = openSync(path, fsConstants.O_RDONLY | noFollow);
    const opened = fstatSync(fd);
    if (!opened.isFile() || !sameFingerprint(fileFingerprint(before), fileFingerprint(opened))) {
      throw new Error(`${label} changed while it was opened`);
    }
    return { before: fileFingerprint(opened), fd, size: opened.size };
  } catch (error) {
    if (fd !== undefined) closeSync(fd);
    throw error;
  }
}

function finishStableRead(fd, before, size, label) {
  const extra = Buffer.allocUnsafe(1);
  if (readSync(fd, extra, 0, 1, size) !== 0) {
    throw new Error(`${label} grew while it was being read`);
  }
  const after = fileFingerprint(fstatSync(fd));
  if (!sameFingerprint(before, after)) {
    throw new Error(`${label} changed while it was being read`);
  }
  return after;
}

function revalidateStableFile(absolute, record, label, maxBytes) {
  const { before, fd } = openStableRegularFile(absolute, label, maxBytes);
  try {
    if (!sameFingerprint(record.fingerprint, before)) {
      throw new Error(`${label} changed after it was validated`);
    }
  } finally {
    closeSync(fd);
  }
}

function revalidateAllStableFiles() {
  for (const [absolute, record] of stableFiles) {
    revalidateStableFile(absolute, record, `validated file ${absolute}`, record.size);
  }
}

function readRegularFileLimited(path, label, maxBytes = MAX_FILE_BYTES) {
  const absolute = resolve(path);
  const cached = stableFiles.get(absolute);
  if (cached?.bytes) {
    if (cached.size > maxBytes) throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
    revalidateStableFile(absolute, cached, label, maxBytes);
    return cached.bytes;
  }

  const { before, fd, size } = openStableRegularFile(absolute, label, maxBytes);
  try {
    chargeValidationIo(size, label);
    const bytes = Buffer.allocUnsafe(size);
    let offset = 0;
    while (offset < size) {
      const count = readSync(fd, bytes, offset, size - offset, offset);
      if (count === 0) throw new Error(`${label} was truncated while it was being read`);
      offset += count;
    }
    const fingerprint = finishStableRead(fd, before, size, label);
    const digest = sha256Digest(bytes);
    if (cached && (cached.size !== size || cached.digest !== digest)) {
      throw new Error(`${label} changed between validation reads`);
    }
    stableFiles.set(absolute, { bytes, digest, fingerprint, size });
    return bytes;
  } finally {
    closeSync(fd);
  }
}

function digestRegularFileLimited(path, label, maxBytes = MAX_FILE_BYTES) {
  const absolute = resolve(path);
  const cached = stableFiles.get(absolute);
  if (cached) {
    if (cached.size > maxBytes) throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
    revalidateStableFile(absolute, cached, label, maxBytes);
    return { digest: cached.digest, size: cached.size };
  }

  const { before, fd, size } = openStableRegularFile(absolute, label, maxBytes);
  try {
    chargeValidationIo(size, label);
    const hash = createHash('sha256');
    const chunk = Buffer.allocUnsafe(Math.min(IO_BUFFER_BYTES, Math.max(1, size)));
    let offset = 0;
    while (offset < size) {
      const count = readSync(fd, chunk, 0, Math.min(chunk.byteLength, size - offset), offset);
      if (count === 0) throw new Error(`${label} was truncated while it was being read`);
      hash.update(chunk.subarray(0, count));
      offset += count;
    }
    const fingerprint = finishStableRead(fd, before, size, label);
    const record = { digest: `sha256:${hash.digest('hex')}`, fingerprint, size };
    stableFiles.set(absolute, record);
    return record;
  } finally {
    closeSync(fd);
  }
}

function minimalFreshEvidence(status) {
  return {
    schemaVersion: 2,
    gate: 'quickplay-validate',
    generatedAt: new Date().toISOString(),
    skipped: true,
    reason: selftestMode ? 'selftest mode' : `validation ${status}`,
    verdict: {
      status: 'fail',
      dirtyProvenance: false,
      missingCommitRepos: [],
      dirtyRepos: [],
    },
  };
}

function lineInFile(filePath, needle) {
  try {
    if (!existsSync(filePath)) return 1;
    const source = manifestUtf8(
      readRegularFileLimited(filePath, `diagnostic source ${filePath}`, MAX_METADATA_BYTES),
      `diagnostic source ${filePath}`,
    );
    const lines = source.split(/\r?\n/);
    const index = lines.findIndex((line) => line.includes(needle));
    return index === -1 ? 1 : index + 1;
  } catch {
    return 1;
  }
}

function buildReport(status, details = {}) {
  const generatedAt = new Date().toISOString();
  const evidenceScope = quickplayEvidenceScope();
  const freshEvidence = status === 'ok' && !selftestMode
    ? sourceBoundEvidence({
        gate: 'quickplay-validate',
        generatedAt,
        root,
        repos: evidenceScope.repos,
        gateFiles: evidenceScope.gateFiles,
        artifacts: evidenceScope.artifacts,
      })
    : minimalFreshEvidence(status);
  const issues = [...(details.issues ?? [])];
  if (status === 'ok' && freshEvidence.verdict.status !== 'pass' && !selftestMode) {
    issues.push(structuredIssue('quickplay validate freshEvidence verdict must pass', {
      owner: 'Volang',
      subsystem: 'FreshEvidence',
      file: 'scripts/ci/quickplay_validate.mjs',
      line: 73,
      reason: 'Current quickplay validate report was produced from dirty or missing-commit repositories.',
      dirtyRepos: freshEvidence.verdict.dirtyRepos,
      missingCommitRepos: freshEvidence.verdict.missingCommitRepos,
      requiredFix: 'Commit or clean all source/provenance changes, then rerun quickplay-validate so its report binds to fresh source digests.',
    }));
  }
  const finalStatus = issues.length > 0 ? 'failed' : status;
  const report = {
    schemaVersion: 1,
    kind: 'quickplay.validateReport',
    gate: 'quickplay-validate',
    status: finalStatus,
    generatedAt,
    freshEvidence,
    selftestMode,
    quickplayDir,
    ...details,
    issues,
  };
  return report;
}

function sameNodeIdentity(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode;
}

function assertBoundedReportPath(path, label) {
  if (typeof path !== 'string' || path.length === 0 || path.includes('\0')) {
    throw new Error(`${label} must be a non-empty path without NUL bytes`);
  }
  if (Buffer.byteLength(path, 'utf8') > MAX_PATH_BYTES) {
    throw new Error(`${label} exceeds the ${MAX_PATH_BYTES}-byte path limit`);
  }
  const parsed = parsePath(path);
  const components = path.slice(parsed.root.length).split(sep).filter(Boolean);
  if (components.length > MAX_DEPTH) {
    throw new Error(`${label} exceeds the ${MAX_DEPTH}-component path limit`);
  }
}

function canonicalReportOutputDirectory(directory) {
  const requested = resolve(directory);
  assertBoundedReportPath(requested, 'report output directory');
  const parsed = parsePath(requested);
  const missingComponents = [];
  let candidate = requested;

  while (true) {
    let metadata;
    try {
      metadata = lstatSync(candidate, { bigint: true });
    } catch (error) {
      if (error?.code !== 'ENOENT' && error?.code !== 'ENOTDIR') throw error;
      if (candidate === parsed.root) {
        throw new Error(`report output directory has no existing directory ancestor: ${requested}`);
      }
      missingComponents.unshift(basename(candidate));
      candidate = dirname(candidate);
      continue;
    }

    if (candidate === requested
      && (!metadata.isDirectory() || metadata.isSymbolicLink())) {
      throw new Error(`report output directory must not be a symbolic link or special file: ${requested}`);
    }

    const canonicalBase = realpathSync.native(candidate);
    assertBoundedReportPath(canonicalBase, 'canonical report output ancestor');
    const canonicalMetadata = lstatSync(canonicalBase, { bigint: true });
    if (!canonicalMetadata.isDirectory() || canonicalMetadata.isSymbolicLink()) {
      throw new Error(`report output ancestor must resolve to a real directory: ${candidate}`);
    }
    if (metadata.isDirectory()
      && !metadata.isSymbolicLink()
      && !sameNodeIdentity(metadata, canonicalMetadata)) {
      throw new Error(`report output ancestor changed while it was canonicalized: ${candidate}`);
    }

    let current = canonicalBase;
    for (const component of missingComponents) {
      current = join(current, component);
      assertBoundedReportPath(current, 'report output directory');
      try {
        mkdirSync(current, { mode: 0o755 });
      } catch (error) {
        if (error?.code !== 'EEXIST') throw error;
      }
      const currentMetadata = lstatSync(current, { bigint: true });
      if (!currentMetadata.isDirectory() || currentMetadata.isSymbolicLink()) {
        throw new Error(`report output path must contain only real directories: ${current}`);
      }
    }
    const canonicalCurrent = realpathSync.native(current);
    if (canonicalCurrent !== current) {
      throw new Error(`report output path changed or contains a symbolic link: ${current}`);
    }
    return current;
  }
}

function openPinnedReportDirectory(directory) {
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const directoryOnly = typeof fsConstants.O_DIRECTORY === 'number' ? fsConstants.O_DIRECTORY : 0;
  const descriptor = openSync(directory, fsConstants.O_RDONLY | directoryOnly | noFollow);
  try {
    const opened = fstatSync(descriptor, { bigint: true });
    const byPath = lstatSync(directory, { bigint: true });
    if (!opened.isDirectory()
      || !byPath.isDirectory()
      || byPath.isSymbolicLink()
      || !sameNodeIdentity(opened, byPath)) {
      throw new Error(`report output directory changed while it was opened: ${directory}`);
    }
    return { descriptor, identity: opened };
  } catch (error) {
    closeSync(descriptor);
    throw error;
  }
}

function assertPinnedReportDirectory(directory, descriptor, identity) {
  const opened = fstatSync(descriptor, { bigint: true });
  const byPath = lstatSync(directory, { bigint: true });
  if (!opened.isDirectory()
    || !byPath.isDirectory()
    || byPath.isSymbolicLink()
    || !sameNodeIdentity(identity, opened)
    || !sameNodeIdentity(identity, byPath)) {
    throw new Error(`report output directory changed during publication: ${directory}`);
  }
}

function assertSafeReportDestination(destination) {
  let metadata;
  try {
    metadata = lstatSync(destination, { bigint: true });
  } catch (error) {
    if (error?.code === 'ENOENT') return;
    throw error;
  }
  if (!metadata.isFile() || metadata.isSymbolicLink()) {
    throw new Error(`existing report target must be a regular file without symbolic links: ${destination}`);
  }
}

function effectiveReportByteLimit() {
  const configured = selftestMode
    ? process.env.QUICKPLAY_VALIDATE_REPORT_SELFTEST_MAX_BYTES
    : undefined;
  if (configured === undefined) return MAX_REPORT_BYTES;
  if (!/^[1-9][0-9]*$/u.test(configured)) {
    throw new Error('QUICKPLAY_VALIDATE_REPORT_SELFTEST_MAX_BYTES must be a positive integer');
  }
  const limit = Number(configured);
  if (!Number.isSafeInteger(limit) || limit > MAX_REPORT_BYTES) {
    throw new Error(`QUICKPLAY_VALIDATE_REPORT_SELFTEST_MAX_BYTES must not exceed ${MAX_REPORT_BYTES}`);
  }
  return limit;
}

function reportPublicationFailurePoint() {
  if (!selftestMode) return null;
  const point = process.env.QUICKPLAY_VALIDATE_REPORT_SELFTEST_FAILURE;
  if (point === undefined) return null;
  if (point !== 'after-temp-fsync') {
    throw new Error(`unsupported report publication selftest failure point: ${point}`);
  }
  return point;
}

function serializeReport(report) {
  const serialized = JSON.stringify(report, null, 2);
  if (typeof serialized !== 'string') {
    throw new Error('quickplay validation report must serialize to a JSON value');
  }
  const source = `${serialized}\n`;
  assertNoDuplicateJsonKeys(source, 'quickplay validation report');
  const size = Buffer.byteLength(source, 'utf8');
  const limit = effectiveReportByteLimit();
  if (!Number.isSafeInteger(size) || size > limit) {
    throw new Error(`quickplay validation report exceeds the ${limit}-byte report limit`);
  }
  return Buffer.from(source, 'utf8');
}

function createReportTemporary(directory) {
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  for (let attempt = 0; attempt < MAX_REPORT_TEMP_ATTEMPTS; attempt += 1) {
    const name = `${REPORT_TEMP_PREFIX}${process.pid}.${randomBytes(16).toString('hex')}.tmp`;
    const path = join(directory, name);
    assertBoundedReportPath(path, 'temporary report path');
    try {
      const descriptor = openSync(
        path,
        fsConstants.O_WRONLY
          | fsConstants.O_CREAT
          | fsConstants.O_EXCL
          | noFollow,
        0o644,
      );
      const metadata = fstatSync(descriptor, { bigint: true });
      if (!metadata.isFile() || metadata.isSymbolicLink()
        || metadata.nlink !== 1n || metadata.size !== 0n) {
        closeSync(descriptor);
        throw new Error(`temporary report is not a new regular file: ${path}`);
      }
      return { descriptor, identity: metadata, path };
    } catch (error) {
      if (error?.code !== 'EEXIST') throw error;
    }
  }
  throw new Error(`could not allocate a unique report temporary after ${MAX_REPORT_TEMP_ATTEMPTS} attempts`);
}

function assertOwnedReportTemporary(path, identity, expectedSize) {
  const metadata = lstatSync(path, { bigint: true });
  if (!metadata.isFile()
    || metadata.isSymbolicLink()
    || metadata.nlink !== 1n
    || metadata.size !== BigInt(expectedSize)
    || !sameNodeIdentity(identity, metadata)) {
    throw new Error(`temporary report changed during publication: ${path}`);
  }
  return metadata;
}

function cleanupOwnedReportTemporary(directory, directoryDescriptor, directoryIdentity, temporary) {
  if (temporary === undefined) return;
  try {
    assertPinnedReportDirectory(directory, directoryDescriptor, directoryIdentity);
    const metadata = lstatSync(temporary.path, { bigint: true });
    if (metadata.isFile()
      && !metadata.isSymbolicLink()
      && sameNodeIdentity(temporary.identity, metadata)) {
      unlinkSync(temporary.path);
    }
  } catch {
    // Leave an untrusted or replaced path untouched. The random name prevents reuse by this run.
  }
}

function publishReport(report) {
  const payload = serializeReport(report);
  const failurePoint = reportPublicationFailurePoint();
  const directory = canonicalReportOutputDirectory(outDir);
  const destination = join(directory, REPORT_FILE_NAME);
  assertBoundedReportPath(destination, 'report destination');
  const pinned = openPinnedReportDirectory(directory);
  let temporary;
  let published = false;

  try {
    assertSafeReportDestination(destination);
    temporary = createReportTemporary(directory);
    let offset = 0;
    while (offset < payload.byteLength) {
      const written = writeSync(
        temporary.descriptor,
        payload,
        offset,
        payload.byteLength - offset,
        offset,
      );
      if (written <= 0) {
        throw new Error(`report write made no progress: ${destination}`);
      }
      offset += written;
    }
    fsyncSync(temporary.descriptor);
    const writtenMetadata = fstatSync(temporary.descriptor, { bigint: true });
    if (!writtenMetadata.isFile()
      || writtenMetadata.nlink !== 1n
      || writtenMetadata.size !== BigInt(payload.byteLength)
      || !sameNodeIdentity(temporary.identity, writtenMetadata)) {
      throw new Error(`temporary report changed while it was written: ${temporary.path}`);
    }
    temporary.identity = writtenMetadata;
    assertOwnedReportTemporary(temporary.path, temporary.identity, payload.byteLength);
    if (failurePoint === 'after-temp-fsync') {
      throw new Error('selftest injected report publication failure after temporary fsync');
    }

    closeSync(temporary.descriptor);
    temporary.descriptor = undefined;
    assertPinnedReportDirectory(directory, pinned.descriptor, pinned.identity);
    assertSafeReportDestination(destination);
    assertOwnedReportTemporary(temporary.path, temporary.identity, payload.byteLength);
    renameSync(temporary.path, destination);
    published = true;

    assertPinnedReportDirectory(directory, pinned.descriptor, pinned.identity);
    const publishedMetadata = lstatSync(destination, { bigint: true });
    if (!publishedMetadata.isFile()
      || publishedMetadata.isSymbolicLink()
      || publishedMetadata.nlink !== 1n
      || publishedMetadata.size !== BigInt(payload.byteLength)
      || !sameNodeIdentity(temporary.identity, publishedMetadata)) {
      throw new Error(`atomic report publication failed: ${destination}`);
    }
    fsyncSync(pinned.descriptor);
    return destination;
  } finally {
    if (temporary?.descriptor !== undefined) {
      closeSync(temporary.descriptor);
      temporary.descriptor = undefined;
    }
    if (!published) {
      cleanupOwnedReportTemporary(
        directory,
        pinned.descriptor,
        pinned.identity,
        temporary,
      );
    }
    closeSync(pinned.descriptor);
  }
}

function writeReport(status, details = {}) {
  const report = buildReport(status, details);
  publishReport(report);
  return report;
}

function fail(message, evidence = {}) {
  writeReport('failed', { message, issues: [structuredIssue(message, evidence)] });
  console.error(`quickplay validate: ${message}`);
  process.exit(1);
}

if (selftestMode && process.env.QUICKPLAY_VALIDATE_REPORT_SELFTEST_ONLY === '1') {
  const report = writeReport('ok', { reportPublicationSelftest: true });
  if (report.status !== 'ok') {
    throw new Error('report publication selftest must produce an ok report');
  }
  console.log('quickplay validate: report publication selftest ok');
  process.exit(0);
}

function provenanceArtifactKey(moduleName, artifact) {
  try {
    return artifactKey(artifact);
  } catch (error) {
    fail(`provenance artifact for ${moduleName} has an invalid identity: ${error.message}`, {
      owner: 'studio/artifacts',
      subsystem: 'ProducerProvenance',
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      line: lineInFile(provenancePath, artifact?.name ?? artifact?.path ?? '"artifacts"'),
      reason: error.message,
      found: artifact,
      requiredFix: 'Regenerate quickplay so every provenance artifact records a valid kind, target, and name.',
    });
  }
}

function assertNoDuplicateJsonKeys(source, label) {
  const stack = [];
  let rootState = 'value';
  let index = 0;
  let tokens = 0;

  const charge = () => {
    tokens += 1;
    if (tokens > MAX_JSON_TOKENS) {
      throw new Error(`${label} exceeds the ${MAX_JSON_TOKENS}-token JSON limit`);
    }
  };
  const skipWhitespace = () => {
    while (index < source.length && /[\u0009\u000A\u000D\u0020]/.test(source[index])) index += 1;
  };
  const decodeScalarString = (token) => {
    let value;
    try {
      value = JSON.parse(token);
    } catch (error) {
      throw new Error(`${label} contains an invalid JSON string: ${error.message}`);
    }
    for (let offset = 0; offset < value.length; offset += 1) {
      const unit = value.charCodeAt(offset);
      if (unit >= 0xd800 && unit <= 0xdbff) {
        const next = value.charCodeAt(offset + 1);
        if (!(next >= 0xdc00 && next <= 0xdfff)) {
          throw new Error(`${label} contains an isolated Unicode surrogate in a JSON string`);
        }
        offset += 1;
      } else if (unit >= 0xdc00 && unit <= 0xdfff) {
        throw new Error(`${label} contains an isolated Unicode surrogate in a JSON string`);
      }
    }
    return value;
  };
  const scanString = () => {
    const start = index;
    index += 1;
    while (index < source.length) {
      const character = source[index];
      if (character === '"') {
        index += 1;
        return decodeScalarString(source.slice(start, index));
      }
      if (character === '\\') {
        index += 2;
      } else {
        index += 1;
      }
    }
    throw new Error(`${label} contains an unterminated JSON string`);
  };
  const scanScalar = () => {
    const start = index;
    while (index < source.length && !/[\u0009\u000A\u000D\u0020,\]}]/.test(source[index])) index += 1;
    if (index === start) throw new Error(`${label} contains a missing JSON value`);
    const token = source.slice(start, index);
    let value;
    try {
      value = JSON.parse(token);
    } catch (error) {
      throw new Error(`${label} contains an invalid JSON scalar: ${error.message}`);
    }
    if (typeof value === 'number' && !Number.isFinite(value)) {
      throw new Error(`${label} contains a JSON number outside the finite f64 range`);
    }
  };
  const beginValue = () => {
    charge();
    if (source[index] === '{') {
      index += 1;
      if (stack.length >= MAX_DEPTH) throw new Error(`${label} exceeds the ${MAX_DEPTH}-level JSON depth limit`);
      stack.push({ kind: 'object', keys: new Set(), state: 'keyOrEnd' });
    } else if (source[index] === '[') {
      index += 1;
      if (stack.length >= MAX_DEPTH) throw new Error(`${label} exceeds the ${MAX_DEPTH}-level JSON depth limit`);
      stack.push({ kind: 'array', state: 'valueOrEnd' });
    } else if (source[index] === '"') {
      scanString();
    } else {
      scanScalar();
    }
  };

  while (true) {
    skipWhitespace();
    if (stack.length === 0) {
      if (rootState === 'done') {
        if (index !== source.length) throw new Error(`${label} has trailing JSON data`);
        return;
      }
      if (index === source.length) throw new Error(`${label} contains no JSON value`);
      rootState = 'done';
      beginValue();
      continue;
    }

    const context = stack.at(-1);
    if (context.kind === 'object') {
      if (context.state === 'keyOrEnd') {
        if (source[index] === '}') {
          charge();
          index += 1;
          stack.pop();
          continue;
        }
        if (source[index] !== '"') throw new Error(`${label} contains a non-string JSON object key`);
        charge();
        const key = scanString();
        if (Buffer.byteLength(key, 'utf8') > MAX_PATH_BYTES) {
          throw new Error(`${label} contains an object key exceeding ${MAX_PATH_BYTES} bytes`);
        }
        if (context.keys.size >= MAX_JSON_OBJECT_KEYS) {
          throw new Error(`${label} exceeds the ${MAX_JSON_OBJECT_KEYS}-key JSON object limit`);
        }
        if (context.keys.has(key)) throw new Error(`${label} contains duplicate object key ${JSON.stringify(key)}`);
        context.keys.add(key);
        context.state = 'colon';
        continue;
      }
      if (context.state === 'colon') {
        if (source[index] !== ':') throw new Error(`${label} JSON object key is missing a colon`);
        charge();
        index += 1;
        context.state = 'value';
        continue;
      }
      if (context.state === 'value') {
        if (index === source.length) throw new Error(`${label} JSON object is missing a value`);
        context.state = 'commaOrEnd';
        beginValue();
        continue;
      }
      if (source[index] === ',') {
        charge();
        index += 1;
        context.state = 'keyOrEnd';
      } else if (source[index] === '}') {
        charge();
        index += 1;
        stack.pop();
      } else {
        throw new Error(`${label} JSON object is missing a comma or closing brace`);
      }
      continue;
    }

    if (context.state === 'valueOrEnd') {
      if (source[index] === ']') {
        charge();
        index += 1;
        stack.pop();
      } else {
        if (index === source.length) throw new Error(`${label} JSON array is missing a value`);
        context.state = 'commaOrEnd';
        beginValue();
      }
      continue;
    }
    if (source[index] === ',') {
      charge();
      index += 1;
      context.state = 'valueOrEnd';
    } else if (source[index] === ']') {
      charge();
      index += 1;
      stack.pop();
    } else {
      throw new Error(`${label} JSON array is missing a comma or closing bracket`);
    }
  }
}

function readJson(path, maxBytes = MAX_JSON_BYTES) {
  try {
    const bytes = readRegularFileLimited(path, `JSON file ${path}`, maxBytes);
    const source = manifestUtf8(bytes, `JSON file ${path}`);
    assertNoDuplicateJsonKeys(source, `JSON file ${path}`);
    return JSON.parse(source);
  } catch (error) {
    fail(`cannot read JSON ${path}: ${error.message}`);
  }
}

function gitOutput(args, cwd) {
  try {
    return execFileSync('git', args, {
      cwd,
      encoding: 'utf8',
      env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' },
      maxBuffer: MAX_METADATA_BYTES,
      timeout: 30_000,
      stdio: ['ignore', 'pipe', 'pipe'],
    }).trim();
  } catch (error) {
    const stderr = error?.stderr ? String(error.stderr).trim() : '';
    fail(`git ${args.join(' ')} failed in ${cwd}: ${stderr || error.message}`);
  }
}

function gitStatus(cwd) {
  return gitOutput(['status', '--porcelain=v2', '--untracked-files=all'], cwd);
}

function gitDirty(cwd) {
  return gitStatus(cwd) !== '';
}

function assert(condition, message, evidence = {}) {
  if (!condition) {
    fail(message, evidence);
  }
}

function artifactRelativePathForUrl(url) {
  try {
    return quickplayArtifactRelativePathFromUrl(url);
  } catch (error) {
    fail(error.message);
  }
}

function localPathForArtifact(url) {
  return join(quickplayDir, ...artifactRelativePathForUrl(url).split('/'));
}

function moduleByName(modules, moduleName) {
  const mod = modules.get(moduleName);
  assert(mod, `deps modules must include ${moduleName}`);
  assert(typeof mod.version === 'string' && mod.version.length > 0, `${moduleName} must have a version`);
  return mod;
}

function requiredVoplayArtifacts(voplay) {
  const artifacts = voplay.artifacts ?? [];
  const js = artifacts.find((artifact) => artifactKey(artifact) === artifactKey({
    kind: 'extension-js-glue', target: 'wasm32-unknown-unknown', name: 'voplay_island.js',
  }));
  const wasm = artifacts.find((artifact) => artifactKey(artifact) === artifactKey({
    kind: 'extension-wasm', target: 'wasm32-unknown-unknown', name: 'voplay_island_bg.wasm',
  }));
  assert(js, 'voplay JS prefetch artifact missing from deps');
  assert(wasm, 'voplay WASM prefetch artifact missing from deps');
  return [js, wasm];
}

function sha256Digest(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function jsonFileDigest(path) {
  return sha256Digest(readRegularFileLimited(path, `JSON file ${path}`, MAX_JSON_BYTES));
}

function moduleFileBytes(file) {
  const cached = moduleFileByteCache.get(file);
  if (cached) return cached;
  const label = typeof file?.path === 'string' ? file.path : '<invalid path>';
  const hasText = typeof file?.content === 'string';
  const hasBase64 = typeof file?.contentBase64 === 'string';
  assert(
    hasText !== hasBase64
      && (file.content == null || hasText)
      && (file.contentBase64 == null || hasBase64),
    `module file must contain exactly one string payload: ${label}`,
  );

  let bytes;
  if (hasText) {
    const size = Buffer.byteLength(file.content, 'utf8');
    assert(size <= MAX_FILE_BYTES, `module file ${label} exceeds the ${MAX_FILE_BYTES}-byte limit`);
    bytes = Buffer.from(file.content, 'utf8');
  } else {
    assert(!/\s/.test(file.contentBase64), `module file has non-canonical base64: ${label}`);
    const maximumEncodedLength = Math.ceil(MAX_FILE_BYTES / 3) * 4;
    assert(
      file.contentBase64.length <= maximumEncodedLength,
      `module file ${label} exceeds the ${MAX_FILE_BYTES}-byte limit`,
    );
    bytes = Buffer.from(file.contentBase64, 'base64');
    assert(
      bytes.toString('base64') === file.contentBase64,
      `module file has invalid or non-canonical base64: ${label}`,
    );
    assert(bytes.byteLength <= MAX_FILE_BYTES, `module file ${label} exceeds the ${MAX_FILE_BYTES}-byte limit`);
  }
  moduleFileByteCache.set(file, bytes);
  return bytes;
}

function validateMode(mode, label) {
  assert(
    mode == null || (Number.isInteger(mode) && mode >= 0 && mode <= 0o7777),
    `${label} has an invalid file mode`,
  );
}

function portableCollisionKey(path) {
  return portablePathCollisionKey(path);
}

function validateBrowserPortablePathSet(paths, destinationRoot, label) {
  assert(Array.isArray(paths), `${label} paths must be an array`);
  assert(paths.length <= MAX_FILES, `${label} paths exceed the ${MAX_FILES}-entry limit`);
  const rootComponents = destinationRoot.split('/').filter(Boolean);
  const collisionKeys = new Map();
  for (const relative of paths) {
    let components;
    try {
      components = validatePortableRelativePath(relative, `${label} path`);
    } catch (error) {
      fail(error.message);
    }
    const finalPath = `${destinationRoot}/${relative}`;
    assert(
      rootComponents.length + components.length <= MAX_DEPTH
        && Buffer.byteLength(finalPath, 'utf8') <= MAX_PATH_BYTES,
      `${label} final browser path exceeds the VFS path limit: ${finalPath}`,
    );
    const key = portableCollisionKey(relative);
    assert(!collisionKeys.has(key), `${label} contains a duplicate portable path: ${relative}`);
    collisionKeys.set(key, relative);
  }
  for (const [key, relative] of collisionKeys) {
    const components = key.split('/');
    for (let length = 1; length < components.length; length += 1) {
      const ancestor = components.slice(0, length).join('/');
      assert(
        !collisionKeys.has(ancestor),
        `${label} contains a file/directory ancestor collision: ${collisionKeys.get(ancestor)} and ${relative}`,
      );
    }
  }
}

function validatePackagedFileSet(files, label, reserveArtifactTree) {
  assert(Array.isArray(files), `${label} files must be an array`);
  assert(files.length > 0, `${label} must embed files`);
  assert(files.length <= MAX_FILES, `${label} exceeds the ${MAX_FILES}-file limit`);
  const destinations = new Set();
  let bytes = 0;
  for (const file of files) {
    assert(file && typeof file === 'object' && !Array.isArray(file), `${label} contains an invalid file entry`);
    assertAllowedObjectKeys(file, ['path'], ['content', 'contentBase64', 'mode'], `${label} file ${file?.path ?? '<invalid>'}`);
    try {
      validatePortableRelativePath(file.path, `${label} file path`);
    } catch (error) {
      fail(error.message);
    }
    assert(
      file.path.split('/').every((component) => !FORBIDDEN_PACKAGE_DIRECTORIES.has(component)),
      `${label} file uses a forbidden generated/control directory: ${file.path}`,
    );
    if (reserveArtifactTree) {
      assert(
        file.path !== 'artifacts' && !file.path.startsWith('artifacts/'),
        `${label} source file uses the reserved artifact subtree: ${file.path}`,
      );
    }
    const key = portableCollisionKey(file.path);
    assert(!destinations.has(key), `${label} contains a duplicate portable path: ${file.path}`);
    destinations.add(key);
    validateMode(file.mode, `${label} file ${file.path}`);
    const payload = moduleFileBytes(file);
    bytes += payload.byteLength;
    assert(
      Number.isSafeInteger(bytes) && bytes <= MAX_SOURCE_BYTES,
      `${label} exceeds the ${MAX_SOURCE_BYTES}-byte source payload limit`,
    );
  }
  return bytes;
}

function validateArtifactDescriptor(artifact, label, requireLocation) {
  validateArtifactDescriptorWithLimit(artifact, label, requireLocation, MAX_FILE_BYTES);
}

function validatePackageBudgets(project, deps) {
  assert(project && typeof project === 'object' && !Array.isArray(project), 'project package must be an object');
  assert(deps && typeof deps === 'object' && !Array.isArray(deps), 'dependency package must be an object');
  assertAllowedObjectKeys(
    project,
    ['schemaVersion', 'name', 'module', 'commit', 'dirty', 'sourceFiles', 'sourceAllowlist', 'files'],
    ['lockRewrite'],
    'project package',
  );
  assertExactObjectKeys(deps, ['schemaVersion', 'name', 'modules'], 'dependency package');
  assert(project.schemaVersion === deps.schemaVersion, 'project and deps schemaVersion must match');
  assert(
    project.schemaVersion === 2,
    'project and deps schemaVersion must be 2',
  );
  packageSchemaVersion = project.schemaVersion;
  assert(/^[0-9a-f]{40}$/.test(project.commit ?? ''), 'project commit must be a lowercase 40-hex Git commit');
  assert(project.dirty === false, 'project dirty flag must be false');
  assert(Array.isArray(project.sourceFiles), 'project sourceFiles must be an array');
  assert(project.sourceFiles.length <= MAX_FILES, `project sourceFiles exceed the ${MAX_FILES}-entry limit`);
  for (const [index, entry] of project.sourceFiles.entries()) {
    assertExactObjectKeys(entry, ['path', 'size', 'digest'], `project sourceFiles[${index}]`);
    validateSourceEntry(entry, 'project');
  }
  validateBrowserPortablePathSet(
    project.sourceFiles.map((entry) => entry.path),
    '/',
    'project sourceFiles',
  );
  const expectedSourceAllowlist = quickplayBlockKartSourceAllowlist(
    project.sourceFiles.map((entry) => entry.path),
  );
  assert(Array.isArray(project.sourceAllowlist), 'project sourceAllowlist must be an array');
  assert(project.sourceAllowlist.length <= MAX_FILES, `project sourceAllowlist exceeds the ${MAX_FILES}-entry limit`);
  for (const [index, entry] of project.sourceAllowlist.entries()) {
    assertExactObjectKeys(entry, ['path', 'reason', 'expiresAt'], `project sourceAllowlist[${index}]`);
    try {
      validatePortableRelativePath(entry.path, `project sourceAllowlist[${index}] path`);
    } catch (error) {
      fail(error.message);
    }
    assert(typeof entry.reason === 'string', `project sourceAllowlist[${index}] reason must be a string`);
    assert(
      typeof entry.expiresAt === 'string'
        && !Number.isNaN(Date.parse(entry.expiresAt))
        && Date.parse(entry.expiresAt) > Date.now(),
      `project sourceAllowlist[${index}] expiresAt must be a future date`,
    );
  }
  validateBrowserPortablePathSet(
    project.sourceAllowlist.map((entry) => entry.path),
    '/',
    'project sourceAllowlist',
  );
  assert(
    JSON.stringify(project.sourceAllowlist) === JSON.stringify(expectedSourceAllowlist),
    'project sourceAllowlist must exactly match the authenticated Quickplay policy',
  );
  assert(Array.isArray(deps.modules), 'deps modules must be an array');
  assert(deps.modules.length > 0, 'deps modules must be embedded');
  assert(deps.modules.length <= MAX_MODULES, `deps package exceeds the ${MAX_MODULES}-module limit`);
  assert(Array.isArray(project.files), 'project package files must be an array');

  validateBrowserPortablePathSet(
    project.files.map((file) => file?.path),
    QUICKPLAY_PROJECT_VFS_ROOT,
    'project package',
  );
  let totalFiles = project.files?.length ?? 0;
  let totalBytes = validatePackagedFileSet(project.files, 'project package', false);
  const moduleNames = new Set();
  const cacheDirs = new Set();
  const artifactUrls = new Set();
  for (const mod of deps.modules) {
    assert(mod && typeof mod === 'object' && !Array.isArray(mod), 'deps package contains an invalid module');
    assertExactObjectKeys(
      mod,
      ['module', 'version', 'commit', 'cacheDir', 'dirty', 'source', 'files', 'artifacts'],
      `dependency package ${mod?.module ?? '<invalid>'}`,
    );
    assert(typeof mod.module === 'string' && typeof mod.version === 'string', 'deps package contains an invalid module identity');
    assert(/^[0-9a-f]{40}$/.test(mod.commit ?? ''), `${mod.module} commit must be a lowercase 40-hex Git commit`);
    assert(mod.dirty === false, `${mod.module} dirty flag must be false`);
    assert(mod.source === 'module-cache' || mod.source === 'external', `${mod.module} source must identify a clean package source`);
    let expectedCacheDir;
    try {
      expectedCacheDir = moduleCacheDir(mod.module, mod.version);
    } catch (error) {
      fail(`invalid packaged module identity: ${error.message}`);
    }
    assert(mod.cacheDir === expectedCacheDir, `${mod.module} cacheDir must be ${expectedCacheDir}`);
    const cacheKey = portableCollisionKey(mod.cacheDir);
    assert(!moduleNames.has(mod.module), `deps modules contain duplicate ${mod.module}`);
    assert(!cacheDirs.has(cacheKey), `deps modules contain colliding cache directory ${mod.cacheDir}`);
    moduleNames.add(mod.module);
    cacheDirs.add(cacheKey);

    totalFiles += mod.files?.length ?? 0;
    totalBytes += validatePackagedFileSet(mod.files, `dependency package ${mod.module}`, true);
    assert(Array.isArray(mod.artifacts), `${mod.module} artifacts must be an array`);
    assert(mod.artifacts.length <= MAX_FILES, `${mod.module} artifacts exceed the ${MAX_FILES}-entry limit`);
    totalFiles += mod.artifacts.length;
    const identities = new Set();
    for (const artifact of mod.artifacts) {
      const label = `${mod.module} artifact ${artifact?.name ?? '<invalid>'}`;
      assertAllowedObjectKeys(
        artifact,
        ['kind', 'target', 'name', 'size', 'digest', 'path', 'url'],
        ['mode'],
        label,
      );
      validateArtifactDescriptor(artifact, label, true);
      const key = artifactKey(artifact);
      assert(!identities.has(key), `${mod.module} contains duplicate artifact identity ${artifactLabel(artifact)}`);
      identities.add(key);
      assert(!artifactUrls.has(artifact.url), `deps package contains duplicate artifact URL ${artifact.url}`);
      artifactUrls.add(artifact.url);
      totalBytes += artifact.size;
    }
    validateBrowserPortablePathSet(
      [...mod.files.map((file) => file.path), ...mod.artifacts.map((artifact) => artifact.path)],
      `/${mod.cacheDir}`,
      `dependency package ${mod.module}`,
    );
    assert(
      Number.isSafeInteger(totalFiles) && totalFiles <= MAX_FILES,
      `combined Quickplay package exceeds the ${MAX_FILES}-file limit`,
    );
    assert(
      Number.isSafeInteger(totalBytes) && totalBytes <= MAX_TOTAL_BYTES,
      `combined Quickplay package exceeds the ${MAX_TOTAL_BYTES}-byte payload limit`,
    );
  }
  return { bytes: totalBytes, files: totalFiles };
}

function boundedArray(value, label, maxEntries = MAX_FILES) {
  assert(Array.isArray(value), `${label} must be an array`);
  assert(value.length <= maxEntries, `${label} exceeds the ${maxEntries}-entry limit`);
  return value;
}

function assertExactObjectKeys(value, expectedKeys, label) {
  assert(value && typeof value === 'object' && !Array.isArray(value), `${label} must be an object`);
  const actual = Object.keys(value).sort(compareUtf8);
  const expectedKeysSorted = [...expectedKeys].sort(compareUtf8);
  assert(
    JSON.stringify(actual) === JSON.stringify(expectedKeysSorted),
    `${label} fields must be exactly ${expectedKeysSorted.join(', ')}`,
  );
}

function assertAllowedObjectKeys(value, requiredKeys, optionalKeys, label) {
  assert(value && typeof value === 'object' && !Array.isArray(value), `${label} must be an object`);
  const allowed = new Set([...requiredKeys, ...optionalKeys]);
  for (const key of Object.keys(value)) {
    assert(allowed.has(key), `${label} contains unknown field ${key}`);
  }
  for (const key of requiredKeys) {
    assert(Object.hasOwn(value, key), `${label} is missing field ${key}`);
  }
}

function validateProvenanceStructure(provenance, deps) {
  assert(provenance && typeof provenance === 'object' && !Array.isArray(provenance), 'provenance must be an object');
  assertExactObjectKeys(
    provenance,
    ['schemaVersion', 'artifact', 'path', 'task', 'generator', 'toolchain', 'sourceRoots', 'inputs', 'project', 'producers', 'dependencies', 'outputs'],
    'provenance',
  );
  assertExactObjectKeys(provenance.task, ['id', 'command'], 'provenance task');
  assertExactObjectKeys(provenance.generator, ['command', 'script', 'version'], 'provenance generator');
  assertExactObjectKeys(provenance.toolchain, ['node', 'voDevSourceDigest', 'wasmTarget'], 'provenance toolchain');
  assertExactObjectKeys(
    provenance.sourceRoots,
    ['volang', 'blockKart', 'vogui', 'vopack', 'voplay'],
    'provenance sourceRoots',
  );
  assertAllowedObjectKeys(
    provenance.project,
    ['commit', 'dirty', 'filesDigest', 'module', 'sourceFiles', 'sourceAllowlist', 'sourceFilesDigest'],
    ['lockRewrite'],
    'provenance project',
  );
  const dependencyNames = new Set();
  for (const dependency of boundedArray(provenance.dependencies, 'provenance dependencies', MAX_MODULES)) {
    assertExactObjectKeys(
      dependency,
      ['artifacts', 'cacheDir', 'commit', 'dirty', 'filesDigest', 'module', 'source', 'version'],
      `provenance dependency ${dependency?.module ?? '<invalid>'}`,
    );
    assert(typeof dependency.module === 'string', 'provenance dependency module must be a string');
    assert(!dependencyNames.has(dependency.module), `provenance contains duplicate dependency ${dependency.module}`);
    dependencyNames.add(dependency.module);
    for (const [index, artifact] of boundedArray(
      dependency.artifacts,
      `${dependency.module} provenance artifacts`,
    ).entries()) {
      provenanceArtifactKey(dependency.module, artifact);
      assertExactObjectKeys(
        artifact,
        ['digest', 'kind', 'name', 'path', 'size', 'target', 'url'],
        `${dependency.module} provenance artifacts[${index}]`,
      );
      validateArtifactDescriptor(artifact, `${dependency.module} provenance artifacts[${index}]`, true);
    }
  }
  const packagedNames = new Set(deps.modules.map((mod) => mod.module));
  assert(
    dependencyNames.size === packagedNames.size
      && [...dependencyNames].every((moduleName) => packagedNames.has(moduleName)),
    'provenance dependency set must exactly match deps modules',
  );

  const outputPaths = new Set();
  for (const output of boundedArray(provenance.outputs, 'provenance outputs')) {
    assertExactObjectKeys(output, ['digest', 'path', 'size'], 'provenance output');
    assert(typeof output.path === 'string', 'provenance output path must be a string');
    assert(sha256Field(output.digest), `provenance output ${output.path} digest must be sha256`);
    assert(
      Number.isSafeInteger(output.size) && output.size >= 0 && output.size <= MAX_JSON_BYTES,
      `provenance output ${output.path} size must be within the ${MAX_JSON_BYTES}-byte limit`,
    );
    assert(!outputPaths.has(output.path), `provenance contains duplicate output ${output.path}`);
    outputPaths.add(output.path);
  }
  assert(
    outputPaths.size === 2 && outputPaths.has('project.json') && outputPaths.has('deps.json'),
    'provenance outputs must contain exactly project.json and deps.json',
  );

  boundedArray(provenance.inputs, 'provenance generator inputs');
  boundedArray(provenance.project?.sourceFiles, 'provenance project sourceFiles');
  boundedArray(provenance.project?.sourceAllowlist, 'provenance project sourceAllowlist');
  for (const producer of boundedArray(provenance.producers, 'provenance producers')) {
    assert(producer && typeof producer === 'object' && !Array.isArray(producer), 'provenance producer must be an object');
    boundedArray(producer.inputs ?? [], 'provenance producer inputs');
    boundedArray(producer.outputs, 'provenance producer outputs');
    boundedArray(producer.archiveEntries ?? [], 'provenance producer archiveEntries');
    for (const upstream of boundedArray(producer.upstream ?? [], 'provenance producer upstream')) {
      assert(upstream && typeof upstream === 'object' && !Array.isArray(upstream), 'provenance upstream producer must be an object');
      boundedArray(upstream.inputs, 'provenance upstream inputs');
      boundedArray(upstream.outputs, 'provenance upstream outputs');
    }
  }
}

function manifestUtf8(bytes, label) {
  try {
    return new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(bytes);
  } catch (error) {
    fail(`${label} is invalid UTF-8: ${error.message}`);
  }
}

function sourceSetDigest(entries) {
  assert(Array.isArray(entries), 'source digest entries must be an array');
  assert(entries.length <= MAX_FILES, `source digest entries exceed the ${MAX_FILES}-entry limit`);
  const canonical = entries
    .map((entry) => ({ path: entry.path, size: entry.size, digest: entry.digest }))
    .sort((a, b) => compareUtf8(a.path, b.path));
  return sha256Digest(Buffer.from(JSON.stringify(canonical), 'utf8'));
}

function listProjectSourceFiles(projectRoot, extension) {
  const files = [];
  let entriesSeen = 0;
  let totalBytes = 0;
  const visit = (dir, depth) => {
    assert(depth <= MAX_DEPTH, `BlockKart source tree exceeds the ${MAX_DEPTH}-level depth limit`);
    const directoryMetadata = lstatSync(dir);
    assert(
      directoryMetadata.isDirectory() && !directoryMetadata.isSymbolicLink(),
      `BlockKart source tree directory must not be a symlink: ${dir}`,
    );
    const handle = opendirSync(dir);
    const entries = [];
    try {
      for (let entry = handle.readSync(); entry != null; entry = handle.readSync()) {
        entriesSeen += 1;
        assert(
          entriesSeen <= MAX_WALK_ENTRIES,
          `BlockKart source tree exceeds the ${MAX_WALK_ENTRIES}-entry traversal limit`,
        );
        entries.push(entry);
      }
    } finally {
      handle.closeSync();
    }
    entries.sort((left, right) => compareUtf8(left.name, right.name));
    for (const entry of entries) {
      if (entry.name === '.git' || entry.name === 'target' || entry.name === 'node_modules' || entry.name === 'tmp_checks') {
        continue;
      }
      const file = join(dir, entry.name);
      const metadata = lstatSync(file);
      assert(!entry.isSymbolicLink() && !metadata.isSymbolicLink(), `BlockKart source tree contains a symlink: ${file}`);
      if (entry.isDirectory() && metadata.isDirectory()) {
        visit(file, depth + 1);
      } else if (entry.isFile() && metadata.isFile() && file.endsWith(extension)) {
        assert(files.length < MAX_FILES, `BlockKart source tree exceeds the ${MAX_FILES}-file limit`);
        const relative = posix.normalize(file.slice(projectRoot.length + 1).split(/[\\/]/).join('/'));
        assert(
          relative.split('/').length <= MAX_DEPTH,
          `BlockKart source path exceeds the ${MAX_DEPTH}-component limit: ${relative}`,
        );
        try {
          validatePortableRelativePath(relative, 'BlockKart source path');
        } catch (error) {
          fail(error.message);
        }
        const found = digestRegularFileLimited(file, `BlockKart source ${relative}`, MAX_FILE_BYTES);
        totalBytes += found.size;
        assert(
          Number.isSafeInteger(totalBytes) && totalBytes <= MAX_SOURCE_BYTES,
          `BlockKart source tree exceeds the ${MAX_SOURCE_BYTES}-byte source limit`,
        );
        files.push({ path: relative, digest: found.digest, size: found.size });
      } else if (!entry.isFile() || !metadata.isFile()) {
        fail(`BlockKart source tree contains an unsupported entry: ${file}`);
      }
    }
  };
  visit(projectRoot, 0);
  return files.sort((a, b) => compareUtf8(a.path, b.path));
}

function validateQuickplayOutputClosure(artifactUrls) {
  const allowedFiles = new Set(['project.json', 'deps.json', 'provenance.json']);
  for (const url of artifactUrls) {
    allowedFiles.add(artifactRelativePathForUrl(url));
  }
  const allowedDirectories = new Set();
  for (const file of allowedFiles) {
    const components = file.split('/');
    for (let length = 1; length < components.length; length += 1) {
      allowedDirectories.add(components.slice(0, length).join('/'));
    }
  }

  const found = new Set();
  let entriesSeen = 0;
  const visit = (directory, relativeDirectory, depth) => {
    assert(depth <= MAX_DEPTH, `Quickplay output exceeds the ${MAX_DEPTH}-level depth limit`);
    const directoryMetadata = lstatSync(directory);
    assert(
      directoryMetadata.isDirectory() && !directoryMetadata.isSymbolicLink(),
      `Quickplay output directory must not be a symlink: ${directory}`,
    );
    const handle = opendirSync(directory);
    const entries = [];
    try {
      for (let entry = handle.readSync(); entry != null; entry = handle.readSync()) {
        entriesSeen += 1;
        assert(
          entriesSeen <= MAX_WALK_ENTRIES,
          `Quickplay output exceeds the ${MAX_WALK_ENTRIES}-entry traversal limit`,
        );
        entries.push(entry);
      }
    } finally {
      handle.closeSync();
    }
    entries.sort((left, right) => compareUtf8(left.name, right.name));
    for (const entry of entries) {
      const relativePath = relativeDirectory ? `${relativeDirectory}/${entry.name}` : entry.name;
      try {
        validatePortableRelativePath(relativePath, 'Quickplay output path');
      } catch (error) {
        fail(error.message);
      }
      const absolute = join(directory, entry.name);
      const metadata = lstatSync(absolute);
      assert(!entry.isSymbolicLink() && !metadata.isSymbolicLink(), `Quickplay output contains a symlink: ${relativePath}`);
      if (entry.isDirectory() && metadata.isDirectory()) {
        assert(allowedDirectories.has(relativePath), `Quickplay output contains an undeclared directory: ${relativePath}`);
        visit(absolute, relativePath, depth + 1);
      } else if (entry.isFile() && metadata.isFile()) {
        assert(allowedFiles.has(relativePath), `Quickplay output contains an undeclared file: ${relativePath}`);
        found.add(relativePath);
      } else {
        fail(`Quickplay output contains an unsupported entry: ${relativePath}`);
      }
    }
  };
  visit(quickplayDir, '', 0);
  const missing = [...allowedFiles].filter((path) => !found.has(path));
  assert(missing.length === 0, `Quickplay output is missing declared files: ${missing.join(', ')}`);
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
    .sort((a, b) => compareUtf8(a.path, b.path));
  return sourceSetDigest(entries);
}

function entryMap(entries) {
  assert(Array.isArray(entries), 'digest entries must be an array');
  assert(entries.length <= MAX_FILES, `digest entries exceed the ${MAX_FILES}-entry limit`);
  const out = new Map();
  for (const entry of entries) {
    assert(entry && typeof entry === 'object' && !Array.isArray(entry), 'digest entries must be objects');
    assert(typeof entry.path === 'string' && entry.path.length > 0, 'digest entry path must be a string');
    assert(!out.has(entry.path), `duplicate digest entry: ${entry.path}`);
    out.set(entry.path, entry);
  }
  return out;
}

function blockKartSourcePath(relativePath, label) {
  let components;
  try {
    components = validatePortableRelativePath(relativePath, label);
  } catch (error) {
    fail(error.message);
  }
  const absolute = resolve(blockKartRoot, ...components);
  assert(
    pathIsWithin(absolute, blockKartRoot),
    `${label} escapes the BlockKart source root: ${relativePath}`,
  );
  return absolute;
}

function sourceDigestEntry(relative) {
  const absolute = blockKartSourcePath(relative, 'BlockKart producer source path');
  assert(existsSync(absolute), `BlockKart producer source is missing: ${relative}`, {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: relative,
    reason: 'A required producer input/output path is absent from the BlockKart checkout.',
    requiredFix: 'Regenerate or restore the producer source/output before packaging quickplay.',
  });
  const file = digestRegularFileLimited(absolute, `BlockKart producer source ${relative}`);
  return { path: relative, digest: file.digest, size: file.size };
}

function validateProducerDigestEntries(entries, requiredPaths, label) {
  const entriesByPath = entryMap(entries);
  for (const requiredPath of requiredPaths) {
    const found = entriesByPath.get(requiredPath);
    assert(found, `${label} producer provenance missing digest entry for ${requiredPath}`, {
      owner: 'BlockKart',
      subsystem: 'ProducerProvenance',
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      reason: 'The quickplay provenance does not record a required producer input/output digest.',
      requiredFix: 'Regenerate quickplay provenance with full vpak and terrain producer inputs/outputs.',
      requiredPath,
    });
    if (requiredPath.startsWith('workspace:')) {
      continue;
    }
    const expectedEntry = sourceDigestEntry(requiredPath);
    assert(
      found.digest === expectedEntry.digest && found.size === expectedEntry.size,
      `${label} producer provenance stale for ${requiredPath}`,
      {
        owner: 'BlockKart',
        subsystem: 'ProducerProvenance',
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        reason: 'A recorded producer digest no longer matches the current source checkout.',
        requiredFix: 'Regenerate quickplay provenance from the current BlockKart producer files.',
        requiredPath,
        expected: expectedEntry,
        found,
      },
    );
  }
}

function canonicalWorkspaceInputFile(entry, project, deps) {
  const workspacePath = entry.path.slice('workspace:'.length);
  const modules = [project.module, ...deps.modules.map((mod) => mod.module)]
    .sort((left, right) => right.length - left.length);
  const moduleName = modules.find((candidate) => workspacePath.startsWith(`${candidate}/`));
  assert(moduleName, `unknown workspace producer module: ${entry.path}`);
  const relativePath = workspacePath.slice(moduleName.length + 1);
  try {
    validatePortableRelativePath(relativePath, 'workspace producer input path');
  } catch (error) {
    fail(error.message);
  }
  if (moduleName === project.module) {
    return digestRegularFileLimited(
      blockKartSourcePath(relativePath, 'workspace producer project input path'),
      `workspace producer input ${entry.path}`,
    );
  }
  const mod = deps.modules.find((candidate) => candidate.module === moduleName);
  const file = mod?.files?.find((candidate) => candidate.path === relativePath);
  assert(file, `workspace producer input is not embedded in ${moduleName}: ${relativePath}`);
  const bytes = moduleFileBytes(file);
  return { digest: sha256Digest(bytes), size: bytes.byteLength };
}

function validateCanonicalVpakManifest(manifest, packagedVpakBytes, project, deps) {
  assert(manifest && typeof manifest === 'object' && !Array.isArray(manifest), 'canonical vpak producer manifest must be an object');
  assert(manifest.schemaVersion === 1, 'canonical vpak producer manifest schemaVersion must be 1');
  assert(manifest.kind === 'blockkart.vpakProducerManifest', 'canonical vpak producer manifest kind mismatch');
  assert(manifest.owner === 'BlockKart', 'canonical vpak producer manifest owner mismatch');
  assert(
    JSON.stringify(manifest.command) === JSON.stringify(VO_CLI_GUEST_ENVIRONMENT.command),
    'canonical vpak producer command must match the authenticated Vo CLI guest command',
  );
  assert(manifest.pack?.path === requiredVpakProducerOutput, 'canonical vpak producer pack path mismatch');
  assert(manifest.pack?.size === packagedVpakBytes.byteLength, 'canonical vpak producer pack size mismatch');
  assert(
    manifest.pack?.sha256 === sha256Digest(packagedVpakBytes).slice('sha256:'.length),
    'canonical vpak producer pack digest mismatch',
  );

  const inputs = boundedArray(manifest.inputs, 'canonical vpak producer inputs');
  const inputPaths = new Set();
  let workspaceCount = 0;
  for (const entry of inputs) {
    assert(entry && typeof entry === 'object' && !Array.isArray(entry), 'canonical vpak producer input must be an object');
    assert(typeof entry.path === 'string' && entry.path.length > 0, 'canonical vpak producer input path must be a string');
    assert(!inputPaths.has(entry.path), `canonical vpak producer contains duplicate input ${entry.path}`);
    inputPaths.add(entry.path);
    assert(/^[0-9a-f]{64}$/.test(entry.sha256 ?? ''), `canonical vpak producer input digest is invalid: ${entry.path}`);
    assert(
      Number.isSafeInteger(entry.size) && entry.size >= 0 && entry.size <= MAX_FILE_BYTES,
      `canonical vpak producer input size is invalid: ${entry.path}`,
    );
    let actual;
    if (entry.path.startsWith('workspace:')) {
      workspaceCount += 1;
      actual = canonicalWorkspaceInputFile(entry, project, deps);
    } else {
      actual = sourceDigestEntry(entry.path);
    }
    assert(actual.size === entry.size, `canonical vpak producer input size is stale: ${entry.path}`);
    assert(
      actual.digest === `sha256:${entry.sha256}`,
      `canonical vpak producer input digest is stale: ${entry.path}`,
    );
  }
  assert(
    manifest.workspaceSourceInputCount === workspaceCount,
    'canonical vpak producer workspaceSourceInputCount mismatch',
  );

  const archiveEntries = boundedArray(manifest.archiveEntries, 'canonical vpak archive entries');
  assert(manifest.archiveEntryCount === archiveEntries.length, 'canonical vpak archiveEntryCount mismatch');
  assert(manifest.payloadInputCount === archiveEntries.length, 'canonical vpak payloadInputCount mismatch');
  const archivePaths = new Set();
  for (const entry of archiveEntries) {
    assert(entry && typeof entry === 'object' && !Array.isArray(entry), 'canonical vpak archive entry must be an object');
    try {
      validatePortableRelativePath(entry.path, 'canonical vpak archive path');
    } catch (error) {
      fail(error.message);
    }
    assert(!archivePaths.has(entry.path), `canonical vpak archive contains duplicate path ${entry.path}`);
    archivePaths.add(entry.path);
  }

  const digestSource = { ...manifest };
  delete digestSource.producerDigest;
  assert(/^[0-9a-f]{64}$/.test(manifest.producerDigest ?? ''), 'canonical vpak producerDigest must be sha256');
  assert(
    manifest.producerDigest === createHash('sha256').update(JSON.stringify(digestSource)).digest('hex'),
    'canonical vpak producerDigest mismatch',
  );
}

function validateVoplayWasmProducer(provenance, deps) {
  const producers = boundedArray(provenance.producers, 'provenance producers');
  const producerIds = new Set();
  for (const producer of producers) {
    assert(typeof producer.id === 'string' && producer.id.length > 0, 'provenance producer id must be a string');
    assert(!producerIds.has(producer.id), `provenance contains duplicate producer ${producer.id}`);
    producerIds.add(producer.id);
  }
  assert(
    producerIds.size === 2
      && producerIds.has('voplay-current-source-wasm')
      && producerIds.has('blockkart-runtime-vpak'),
    'provenance producers must contain exactly voplay-current-source-wasm and blockkart-runtime-vpak',
  );

  const producer = producers.find((entry) => entry.id === 'voplay-current-source-wasm');
  assertExactObjectKeys(producer, [
    'buildPlatform',
    'command',
    'ffiSourceFingerprint',
    'id',
    'kind',
    'outputs',
    'owner',
    'sourceClosure',
    'toolchain',
    'volangBuildInputs',
  ], 'voplay WASM producer');
  assert(producer.owner === 'voplay/rust', 'voplay WASM producer owner mismatch');
  assert(producer.kind === 'wasm-bindgen', 'voplay WASM producer kind mismatch');
  assert(
    JSON.stringify(producer.command) === JSON.stringify(VOPLAY_WASM_PRODUCER_COMMAND),
    'voplay WASM producer command mismatch',
  );
  assert(
    JSON.stringify(producer.buildPlatform) === JSON.stringify(currentVoplayWasmBuildPlatform()),
    'voplay WASM producer buildPlatform mismatch',
  );
  assert(
    producer.toolchain
      && typeof producer.toolchain === 'object'
      && !Array.isArray(producer.toolchain)
      && typeof producer.toolchain.rustc === 'string'
      && producer.toolchain.rustc.length > 0
      && typeof producer.toolchain.wasmPack === 'string'
      && producer.toolchain.wasmPack.length > 0,
    'voplay WASM producer toolchain is incomplete',
  );
  const voplay = deps.modules.find((mod) => mod.module === 'github.com/vo-lang/voplay');
  assert(voplay, 'voplay WASM producer requires the packaged voplay module');
  const voplayRepo = selftestMode
    ? null
    : dependencyRepos.find((repo) => repo.name === voplay.module);
  let expectedVolangBuildInputs;
  let expectedSourceClosure;
  if (selftestMode) {
    const source = process.env.QUICKPLAY_VALIDATE_SELFTEST_EXPECTED_VOLANG_BUILD_INPUTS;
    assert(
      typeof source === 'string' && Buffer.byteLength(source, 'utf8') <= 1024 * 1024,
      'quickplay validator selftest expected Volang build inputs are missing or oversized',
    );
    try {
      expectedVolangBuildInputs = JSON.parse(source);
    } catch (error) {
      fail(`quickplay validator selftest expected Volang build inputs are invalid JSON: ${error.message}`);
    }
    const closureSource = process.env.QUICKPLAY_VALIDATE_SELFTEST_EXPECTED_SOURCE_CLOSURE;
    assert(
      typeof closureSource === 'string' && Buffer.byteLength(closureSource, 'utf8') <= 4 * 1024 * 1024,
      'quickplay validator selftest expected source closure is missing or oversized',
    );
    try {
      expectedSourceClosure = JSON.parse(closureSource);
    } catch (error) {
      fail(`quickplay validator selftest expected source closure is invalid JSON: ${error.message}`);
    }
  } else {
    assert(voplayRepo, 'voplay WASM producer source repository is unavailable');
    try {
      const buildInputs = lockedVoplayBuildInputs(voplayRepo.root, {
        volangRoot: root,
        requireClean: false,
      });
      expectedVolangBuildInputs = buildInputs.volangBuildInputs;
      expectedSourceClosure = buildInputs.sourceClosure;
    } catch (error) {
      fail(`voplay WASM producer locked local source graph cannot be derived: ${error.message}`);
    }
  }
  const sourceClosureIssues = verifyVoplaySourceClosure(producer.sourceClosure, {
    expected: expectedSourceClosure,
  });
  assert(
    sourceClosureIssues.length === 0,
    `voplay WASM producer source closure is invalid: ${sourceClosureIssues.join('; ')}`,
  );
  const rootRepository = producer.sourceClosure.repositories.find((repository) => (
    repository.roles.includes('root')
  ));
  assert(rootRepository.name === voplay.module, 'voplay WASM producer root repository identity mismatch');
  assert(rootRepository.commit === voplay.commit, 'voplay WASM producer root repository commit mismatch');
  const volangBuildInputIssues = verifyVoplayVolangBuildInputs(
    producer.volangBuildInputs,
    root,
    { expected: expectedVolangBuildInputs },
  );
  assert(
    volangBuildInputIssues.length === 0,
    `voplay WASM producer Volang build inputs are invalid: ${volangBuildInputIssues.join('; ')}`,
  );
  assert(
    producer.ffiSourceFingerprint === voplayFfiSourceFingerprint(
      producer.sourceClosure?.digest,
      expectedVolangBuildInputs.digest,
    ),
    'voplay WASM producer FFI source fingerprint does not match current source inputs',
  );

  const outputs = boundedArray(producer.outputs, 'voplay WASM producer outputs');
  assert(outputs.length === VOPLAY_WASM_REQUIRED_OUTPUTS.length, 'voplay WASM producer output count mismatch');
  const outputNames = new Set();
  const artifactsByName = new Map(voplay.artifacts.map((artifact) => [artifact.name, artifact]));
  for (const output of outputs) {
    assert(output && typeof output === 'object' && !Array.isArray(output), 'voplay WASM producer output must be an object');
    assert(VOPLAY_WASM_REQUIRED_OUTPUTS.includes(output.name), `unexpected voplay WASM producer output: ${output.name}`);
    assert(!outputNames.has(output.name), `duplicate voplay WASM producer output: ${output.name}`);
    outputNames.add(output.name);
    const artifact = artifactsByName.get(output.name);
    assert(artifact, `voplay WASM producer output is not packaged: ${output.name}`);
    assertArtifactBytesMatch(artifact, output, `voplay WASM producer output ${output.name}`);
  }
  assert(
    VOPLAY_WASM_REQUIRED_OUTPUTS.every((name) => outputNames.has(name)),
    'voplay WASM producer output set is incomplete',
  );
}

function validateBlockKartRuntimeProducer(project, provenance, deps) {
  const projectFiles = new Map((project.files ?? []).map((file) => [file.path, file]));
  const packagedVpak = projectFiles.get(requiredVpakProducerOutput);
  assert(packagedVpak, 'project package missing assets/blockkart.vpak', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/project.json',
    reason: 'The runtime vpak must be embedded and source-bound.',
    requiredFix: 'Regenerate the quickplay package with assets/blockkart.vpak included.',
  });
  const packagedVpakBytes = moduleFileBytes(packagedVpak);
  const packagedManifest = projectFiles.get(requiredVpakProducerManifest);
  assert(packagedManifest, 'project package missing canonical vpak producer manifest', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/project.json',
    reason: 'The packaged vpak must carry its canonical producer manifest sidecar.',
    requiredFix: 'Rebuild and package assets/blockkart.vpak.provenance.json.',
  });
  const packagedManifestBytes = moduleFileBytes(packagedManifest);
  const canonicalManifestPath = join(blockKartRoot, requiredVpakProducerManifest);
  const canonicalManifestBytes = readRegularFileLimited(
    canonicalManifestPath,
    'canonical BlockKart vpak producer manifest',
    MAX_METADATA_BYTES,
  );
  assert(sha256Digest(packagedManifestBytes) === sha256Digest(canonicalManifestBytes), 'packaged canonical vpak producer manifest is stale', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/project.json',
    reason: 'The packaged producer manifest differs from the current BlockKart manifest.',
    requiredFix: 'Regenerate quickplay from the current vpak producer manifest.',
  });
  let canonicalManifest;
  try {
    const source = manifestUtf8(canonicalManifestBytes, 'canonical vpak producer manifest');
    assertNoDuplicateJsonKeys(source, 'canonical vpak producer manifest');
    canonicalManifest = JSON.parse(source);
  } catch (error) {
    fail(`canonical vpak producer manifest is invalid JSON: ${error.message}`);
  }
  validateCanonicalVpakManifest(canonicalManifest, packagedVpakBytes, project, deps);
  const producer = (provenance.producers ?? []).find((entry) => entry?.output === requiredVpakProducerOutput);
  assert(producer, 'provenance missing assets/blockkart.vpak producer record', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The runtime vpak is packaged without first-party producer lineage.',
    requiredFix: 'Regenerate quickplay provenance with tools/pack_primitive_assets.vo, terrain generation inputs, output digests, and commands.',
  });
  assertExactObjectKeys(
    producer,
    [
      'id',
      'owner',
      'kind',
      'output',
      'command',
      'toolchain',
      'voBinary',
      'voCliBuildInputs',
      'voCliExecutionDigest',
      'inputs',
      'outputs',
      'upstream',
      'producerManifest',
      'archiveEntryCount',
      'payloadInputCount',
      'workspaceSourceInputCount',
      'archiveEntries',
    ],
    'assets/blockkart.vpak producer',
  );
  const currentCliInputs = currentVoCliBuildInputs(root);
  const cliInputIssues = verifyVoCliBuildInputs(producer.voCliBuildInputs, {
    expected: currentCliInputs,
  });
  assert(
    cliInputIssues.length === 0,
    `assets/blockkart.vpak Vo CLI producer inputs are invalid: ${cliInputIssues.join('; ')}`,
    {
      owner: 'Volang/cmd/vo',
      subsystem: 'ProducerProvenance',
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      line: lineInFile(provenancePath, '"voCliBuildInputs"'),
      reason: 'The VPAK producer must bind the exact locked local Cargo source closure of the Vo CLI binary that ran the pack script.',
      requiredFix: 'Run task:quickplay-blockkart-package so blockkart-vpak-build records and packages the current Vo CLI build inputs.',
      issues: cliInputIssues,
    },
  );
  const cliExecutionIssues = verifyVoCliExecutionIdentity(
    producer.toolchain,
    producer.voBinary,
    {
      buildInputs: producer.voCliBuildInputs,
      executionDigest: producer.voCliExecutionDigest,
      expectedToolchain: currentVoCliToolchain(root),
    },
  );
  assert(
    cliExecutionIssues.length === 0,
    `assets/blockkart.vpak Vo CLI execution identity is invalid: ${cliExecutionIssues.join('; ')}`,
    {
      owner: 'Volang/cmd/vo',
      subsystem: 'ProducerProvenance',
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      line: lineInFile(provenancePath, '"voBinary"'),
      reason: 'The VPAK producer must retain the actual frozen Rust toolchain and stable Vo binary identity verified around guest execution.',
      requiredFix: 'Run task:quickplay-blockkart-package so blockkart-vpak-build records the current toolchain and pre/post guest binary digest.',
      issues: cliExecutionIssues,
    },
  );
  assert(producer.owner === 'BlockKart', 'assets/blockkart.vpak producer owner mismatch', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The producer owner must identify the accountable first-party source.',
    requiredFix: 'Set producer.owner to BlockKart for the runtime vpak producer.',
  });
  assert(producer.kind === 'vpak', 'assets/blockkart.vpak producer kind mismatch', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The producer kind must identify the runtime asset pack contract.',
    requiredFix: 'Set producer.kind to vpak for assets/blockkart.vpak.',
  });
  assert(JSON.stringify(producer.command) === JSON.stringify(VO_CLI_GUEST_ENVIRONMENT.command), 'assets/blockkart.vpak producer command must match the authenticated Vo CLI guest command', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The provenance must name the command that generated the vpak.',
    requiredFix: 'Record the exact authenticated vpak pack command in producer.command.',
  });
  validateProducerDigestEntries(producer.inputs, requiredVpakProducerInputPaths, 'assets/blockkart.vpak');
  validateProducerDigestEntries(producer.outputs, [requiredVpakProducerOutput], 'assets/blockkart.vpak');
  const outputEntry = entryMap(producer.outputs).get(requiredVpakProducerOutput);
  assert(
    outputEntry.digest === sha256Digest(packagedVpakBytes) && outputEntry.size === packagedVpakBytes.byteLength,
    'assets/blockkart.vpak producer output digest must match packaged bytes',
    {
      owner: 'BlockKart',
      subsystem: 'ProducerProvenance',
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      reason: 'The recorded vpak output digest does not match the quickplay package bytes.',
      requiredFix: 'Regenerate quickplay provenance from the same vpak bytes embedded in project.json.',
      expected: { digest: sha256Digest(packagedVpakBytes), size: packagedVpakBytes.byteLength },
      found: outputEntry,
    },
  );
  const canonicalInputs = canonicalManifest.inputs
    .map((entry) => ({ path: entry.path, digest: `sha256:${entry.sha256}`, size: entry.size }))
    .sort((a, b) => compareUtf8(a.path, b.path));
  const producerInputs = [...(producer.inputs ?? [])].sort((a, b) => compareUtf8(a.path, b.path));
  assert(JSON.stringify(producerInputs) === JSON.stringify(canonicalInputs), 'vpak producer input set must exactly match the canonical manifest', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The producer input set omits or adds files relative to the actual vpak and generator closure.',
    requiredFix: 'Regenerate quickplay from assets/blockkart.vpak.provenance.json.',
  });
  assert(producer.archiveEntryCount === 37
    && producer.payloadInputCount === 37
    && Number(producer.workspaceSourceInputCount ?? 0) > 0
    && producer.workspaceSourceInputCount === canonicalManifest.workspaceSourceInputCount
    && producer.archiveEntryCount === canonicalManifest.archiveEntryCount
    && JSON.stringify(producer.archiveEntries) === JSON.stringify(canonicalManifest.archiveEntries), 'vpak producer archive entry closure mismatch', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The producer record must cover all 37 public archive entries and source inputs.',
    requiredFix: 'Rebuild the vpak producer manifest and regenerate quickplay.',
  });
  assert(producer.producerManifest?.path === requiredVpakProducerManifest
    && producer.producerManifest?.sha256 === sha256Digest(canonicalManifestBytes)
    && producer.producerManifest?.size === canonicalManifestBytes.byteLength
    && producer.producerManifest?.producerDigest === canonicalManifest.producerDigest, 'vpak producer manifest binding mismatch', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The producer record must bind to the exact packaged canonical manifest.',
    requiredFix: 'Regenerate quickplay after rebuilding the canonical vpak manifest.',
  });

  const upstream = Array.isArray(producer.upstream) ? producer.upstream : [];
  const terrainProducer = upstream.find((entry) => entry?.id === 'primitive-terrain-assets');
  const paintProducer = upstream.find((entry) => entry?.id === 'painted-terrain-textures');
  assert(terrainProducer, 'assets/blockkart.vpak provenance missing primitive terrain producer', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The vpak producer must include the generated terrain asset lineage.',
    requiredFix: 'Record tools/generate_primitive_terrain.mjs inputs and generated terrain output digests.',
  });
  validateProducerDigestEntries(terrainProducer.inputs, requiredTerrainProducerInputs, 'primitive-terrain-assets');
  validateProducerDigestEntries(terrainProducer.outputs, requiredTerrainProducerOutputs, 'primitive-terrain-assets');
  assert(paintProducer, 'assets/blockkart.vpak provenance missing painted terrain producer', {
    owner: 'BlockKart',
    subsystem: 'ProducerProvenance',
    file: 'apps/studio/public/quickplay/blockkart/provenance.json',
    reason: 'The vpak producer must include the baked terrain texture lineage.',
    requiredFix: 'Record tools/paint_terrain_textures.mjs inputs and baked texture output digests.',
  });
  validateProducerDigestEntries(paintProducer.inputs, requiredPaintProducerInputs, 'painted-terrain-textures');
  validateProducerDigestEntries(paintProducer.outputs, requiredPaintProducerOutputs, 'painted-terrain-textures');
}

function lockRewriteEvidence(entry) {
  return {
    module: entry.path,
    version: entry.version,
    commit: entry.commit,
    releaseManifestDigest: entry.release_manifest,
    sourceDigest: entry.source,
    dependencies: entry.deps.map((dependency) => ({
      module: dependency.module,
      constraint: dependency.constraint,
    })),
    artifacts: entry.artifacts.map((artifact) => ({
      kind: artifact.kind,
      target: artifact.target,
      name: artifact.name,
      size: artifact.size,
      digest: artifact.digest,
    })),
  };
}

function validateProjectLockRewrite(project, sourceBytes, packagedBytes) {
  const rewrite = project.lockRewrite;
  const sourceDigest = sha256Digest(sourceBytes);
  const packagedDigest = sha256Digest(packagedBytes);
  const sourceLock = parseVoLockForV2Migration(
    decodeModuleTextUtf8(sourceBytes, 'BlockKart source vo.lock'),
    'BlockKart source vo.lock',
  );
  const packagedLock = parseVoLockV2(
    decodeModuleTextUtf8(packagedBytes, 'BlockKart packaged vo.lock'),
    'BlockKart packaged vo.lock',
  );

  const sourceSelection = sourceLock.resolved
    .map((entry) => ({ module: entry.path, version: entry.version }))
    .sort((left, right) => compareUtf8(left.module, right.module));
  const packagedSelection = packagedLock.resolved
    .map((entry) => ({ module: entry.path, version: entry.version }))
    .sort((left, right) => compareUtf8(left.module, right.module));
  assert(
    JSON.stringify(sourceLock.root) === JSON.stringify(packagedLock.root),
    'project lock rewrite must preserve the root module and toolchain constraint',
  );
  assert(
    JSON.stringify(sourceSelection) === JSON.stringify(packagedSelection),
    'project lock rewrite must preserve the exact resolved module versions',
  );

  if (sourceDigest === packagedDigest && rewrite == null) return;
  assertExactObjectKeys(
    rewrite,
    ['schemaVersion', 'path', 'sourceDigest', 'packagedDigest', 'modules'],
    'project lockRewrite',
  );
  assert(rewrite?.schemaVersion === 1, 'project lockRewrite schemaVersion must be 1 when packaged vo.lock differs from source');
  assert(rewrite.path === 'vo.lock', 'project lockRewrite path must be vo.lock');
  assert(rewrite.sourceDigest === sourceDigest, 'project lockRewrite sourceDigest must match source vo.lock');
  assert(rewrite.packagedDigest === packagedDigest, 'project lockRewrite packagedDigest must match packaged vo.lock');
  const expectedModules = packagedLock.resolved
    .map(lockRewriteEvidence)
    .sort((left, right) => compareUtf8(left.module, right.module));
  assert(
    JSON.stringify(rewrite.modules) === JSON.stringify(expectedModules),
    'project lockRewrite modules must exactly project the packaged vo.lock graph',
  );
}

function sameStringArray(actual, wanted) {
  return Array.isArray(actual)
    && actual.length === wanted.length
    && actual.every((value, index) => value === wanted[index]);
}

function artifactMap(entries, label, options = {}) {
  const {
    maxBytes = MAX_FILE_BYTES,
    requireDigest = true,
    requireLocation = false,
  } = options;
  assert(Array.isArray(entries), `${label} must be an array`);
  assert(entries.length <= MAX_FILES, `${label} exceeds the ${MAX_FILES}-entry limit`);
  const out = new Map();
  for (const artifact of entries) {
    if (requireDigest) {
      validateArtifactDescriptorWithLimit(artifact, `${label} ${artifact?.name ?? '<invalid>'}`, requireLocation, maxBytes);
    } else {
      assert(artifact && typeof artifact === 'object' && !Array.isArray(artifact), `${label} entry must be an object`);
      try {
        artifactKey(artifact);
      } catch (error) {
        fail(`${label} has an invalid artifact identity: ${error.message}`);
      }
    }
    const key = artifactKey(artifact);
    assert(!out.has(key), `${label} contains duplicate artifact identity ${artifactLabel(artifact)}`);
    out.set(key, artifact);
  }
  return out;
}

function validateArtifactDescriptorWithLimit(artifact, label, requireLocation, maxBytes) {
  assert(artifact && typeof artifact === 'object' && !Array.isArray(artifact), `${label} must be an object`);
  try {
    artifactKey(artifact);
  } catch (error) {
    fail(`${label} has an invalid identity: ${error.message}`);
  }
  assert(
    Number.isSafeInteger(artifact.size) && artifact.size >= 0 && artifact.size <= maxBytes,
    `${label} size must be a safe integer within the ${maxBytes}-byte limit`,
  );
  assert(sha256Field(artifact.digest), `${label} digest must be sha256`);
  if (artifact.path != null) {
    try {
      validatePortableRelativePath(artifact.path, `${label} path`);
    } catch (error) {
      fail(error.message);
    }
  }
  if (requireLocation) {
    assert(typeof artifact.path === 'string', `${label} path must be a string`);
    assert(typeof artifact.url === 'string', `${label} URL must be a string`);
    validateMode(artifact.mode, label);
  }
}

function assertExactArtifactKeys(left, right, leftLabel, rightLabel) {
  const missing = [...left.keys()].filter((key) => !right.has(key));
  const extra = [...right.keys()].filter((key) => !left.has(key));
  assert(
    missing.length === 0 && extra.length === 0,
    `${leftLabel} and ${rightLabel} artifact identities differ`,
    { missing, extra },
  );
}

function assertArtifactBytesMatch(expected, found, label) {
  assert(found.size === expected.size, `${label} size mismatch`);
  assert(found.digest === expected.digest, `${label} digest mismatch`);
}

function validateActualModuleArtifacts(
  mod,
  lockedModule,
  release,
  webManifest,
  provenanceDependency,
  declaredArtifacts,
) {
  assert(provenanceDependency, `provenance missing dependency ${mod.module}`);
  const packaged = artifactMap(mod.artifacts, `${mod.module} deps artifacts`, { requireLocation: true });
  const locked = artifactMap(lockedModule.artifacts ?? [], `${mod.module} locked artifacts`);
  const published = artifactMap(release.artifacts ?? [], `${mod.module} release artifacts`, {
    maxBytes: MAX_TOTAL_BYTES,
  });
  const web = artifactMap(webManifest.artifacts ?? [], `${mod.module} web artifacts`);
  for (const artifact of provenanceDependency.artifacts ?? []) {
    provenanceArtifactKey(mod.module, artifact);
  }
  const provenance = artifactMap(
    provenanceDependency.artifacts ?? [],
    `${mod.module} provenance artifacts`,
    { requireLocation: true },
  );
  const declared = artifactMap(declaredArtifacts, `${mod.module} vo.mod artifacts`, {
    requireDigest: false,
  });
  const declaredBrowser = new Map(
    [...declared].filter(([, artifact]) => (
      artifact.kind === 'extension-wasm' || artifact.kind === 'extension-js-glue'
    )),
  );

  assertExactArtifactKeys(packaged, web, `${mod.module} packaged`, `${mod.module} vo.web.json`);
  assertExactArtifactKeys(packaged, declaredBrowser, `${mod.module} packaged`, `${mod.module} browser vo.mod`);
  assertExactArtifactKeys(packaged, locked, `${mod.module} packaged`, `${mod.module} locked`);
  assertExactArtifactKeys(packaged, provenance, `${mod.module} packaged`, `${mod.module} provenance`);
  for (const [key, artifact] of packaged) {
    const label = `${mod.module} artifact ${artifactLabel(artifact)}`;
    assert(declared.has(key), `${label} is not declared by vo.mod`);
    const lockedArtifact = locked.get(key);
    const releaseArtifact = published.get(key);
    const webArtifact = web.get(key);
    const provenanceArtifact = provenance.get(key);
    assert(releaseArtifact, `${label} is missing from vo.release.json`);
    assertArtifactBytesMatch(artifact, lockedArtifact, `${label} lock contract`);
    assertArtifactBytesMatch(artifact, releaseArtifact, `${label} release contract`);
    assertArtifactBytesMatch(artifact, webArtifact, `${label} web contract`);
    assertArtifactBytesMatch(artifact, provenanceArtifact, `${label} provenance contract`);
    assert(provenanceArtifact.path === artifact.path, `${label} provenance path mismatch`);
    assert(provenanceArtifact.url === artifact.url, `${label} provenance URL mismatch`);
    const actual = digestRegularFileLimited(
      localPathForArtifact(artifact.url),
      `${label} packaged file`,
      MAX_FILE_BYTES,
    );
    assert(actual.size > 0, `${label} packaged file must not be empty`);
    assertArtifactBytesMatch(artifact, actual, `${label} actual bytes`);
  }
}

function validatePackagedWebManifest(mod) {
  const manifestFile = mod.files.find((file) => file.path === 'vo.web.json');
  if (!manifestFile || mod.files.some((file) => file.path === 'vo.release.json')) return;
  const manifest = parseVoWebManifest(mod, moduleFileBytes(manifestFile));

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
    sourceSetDigest(manifest.source) === manifest.source_digest,
    `${mod.module} vo.web.json source_digest mismatch`,
  );

  const packaged = artifactMap(mod.artifacts ?? [], `${mod.module} packaged artifacts`, { requireLocation: true });
  const webArtifacts = artifactMap(manifest.artifacts ?? [], `${mod.module} web artifacts`);
  assertExactArtifactKeys(packaged, webArtifacts, `${mod.module} packaged`, `${mod.module} vo.web.json`);
  for (const [key, artifact] of packaged) {
    const webArtifact = webArtifacts.get(key);
    assertArtifactBytesMatch(artifact, webArtifact, `${mod.module} artifact ${artifactLabel(artifact)} web contract`);
    const actual = digestRegularFileLimited(
      localPathForArtifact(artifact.url),
      `${mod.module} artifact ${artifactLabel(artifact)} packaged file`,
      MAX_FILE_BYTES,
    );
    assertArtifactBytesMatch(artifact, actual, `${mod.module} artifact ${artifactLabel(artifact)} actual bytes`);
  }
}

function parseReleaseManifest(mod, source) {
  assert(
    source.byteLength <= MAX_METADATA_BYTES,
    `${mod.module} vo.release.json exceeds the ${MAX_METADATA_BYTES}-byte text limit`,
  );
  const text = manifestUtf8(source, `${mod.module} vo.release.json`);
  let manifest;
  try {
    assertNoDuplicateJsonKeys(text, `${mod.module} vo.release.json`);
    manifest = JSON.parse(text);
  } catch (error) {
    fail(`${mod.module} vo.release.json is invalid JSON: ${error.message}`);
  }
  assert(
    manifest && typeof manifest === 'object' && !Array.isArray(manifest),
    `${mod.module} vo.release.json must contain a JSON object`,
  );
  return manifest;
}

function validateReleaseManifestShape(mod, release) {
  assertAllowedObjectKeys(
    release,
    ['schema_version', 'module', 'version', 'commit', 'module_root', 'vo', 'require', 'source', 'web_manifest'],
    ['artifacts'],
    `${mod.module} vo.release.json`,
  );
  assertExactObjectKeys(
    release.source,
    ['name', 'size', 'digest', 'files_size', 'files_digest'],
    `${mod.module} vo.release.json source`,
  );
  assertExactObjectKeys(
    release.web_manifest,
    ['size', 'digest'],
    `${mod.module} vo.release.json web_manifest`,
  );
  try {
    validatePortablePathComponent(
      release.source.name,
      `${mod.module} vo.release.json source.name`,
    );
  } catch (error) {
    fail(error instanceof Error ? error.message : String(error));
  }
  assert(
    Number.isSafeInteger(release.source.size)
      && release.source.size > 0
      && release.source.size <= MAX_FILE_BYTES,
    `${mod.module} vo.release.json source.size must be within 1..=${MAX_FILE_BYTES}`,
  );
  assert(
    Number.isSafeInteger(release.source.files_size)
      && release.source.files_size > 0
      && release.source.files_size <= MAX_SOURCE_BYTES,
    `${mod.module} vo.release.json source.files_size must be within 1..=${MAX_SOURCE_BYTES}`,
  );
  assert(sha256Field(release.source.digest), `${mod.module} vo.release.json source.digest must be sha256`);
  assert(sha256Field(release.source.files_digest), `${mod.module} vo.release.json source.files_digest must be sha256`);
  assert(
    Number.isSafeInteger(release.web_manifest.size)
      && release.web_manifest.size > 0
      && release.web_manifest.size <= MAX_METADATA_BYTES,
    `${mod.module} vo.release.json web_manifest.size must be within 1..=${MAX_METADATA_BYTES}`,
  );
  assert(sha256Field(release.web_manifest.digest), `${mod.module} vo.release.json web_manifest.digest must be sha256`);

  const requirements = boundedArray(release.require, `${mod.module} vo.release.json require`, MAX_MODULES);
  let previousModule = null;
  for (const [index, requirement] of requirements.entries()) {
    assertExactObjectKeys(
      requirement,
      ['module', 'constraint'],
      `${mod.module} vo.release.json require[${index}]`,
    );
    assert(
      typeof requirement.module === 'string' && typeof requirement.constraint === 'string',
      `${mod.module} vo.release.json require[${index}] must contain strings`,
    );
    assert(
      previousModule == null || compareUtf8(previousModule, requirement.module) < 0,
      `${mod.module} vo.release.json require must be unique and sorted by module path`,
    );
    previousModule = requirement.module;
  }

  const artifacts = boundedArray(release.artifacts ?? [], `${mod.module} vo.release.json artifacts`, MAX_MODULES);
  let previousArtifact = null;
  for (const [index, artifact] of artifacts.entries()) {
    assertExactObjectKeys(
      artifact,
      ['kind', 'target', 'name', 'size', 'digest'],
      `${mod.module} vo.release.json artifacts[${index}]`,
    );
    validateArtifactDescriptorWithLimit(
      artifact,
      `${mod.module} vo.release.json artifacts[${index}]`,
      false,
      MAX_TOTAL_BYTES,
    );
    const key = artifactKey(artifact);
    assert(
      previousArtifact == null || compareUtf8(previousArtifact, key) < 0,
      `${mod.module} vo.release.json artifacts must be unique and sorted by identity`,
    );
    previousArtifact = key;
  }
}

function verifyWebManifestBinding(moduleName, release, webManifestSource) {
  assert(
    release.web_manifest && typeof release.web_manifest === 'object' && !Array.isArray(release.web_manifest),
    `${moduleName} vo.release.json web_manifest must be an object`,
  );
  assert(
    Number.isSafeInteger(release.web_manifest.size) && release.web_manifest.size >= 0,
    `${moduleName} vo.release.json web_manifest size must be a non-negative safe integer`,
  );
  assert(
    sha256Field(release.web_manifest.digest),
    `${moduleName} vo.release.json web_manifest digest must be sha256`,
  );
  assert(
    release.web_manifest.size === webManifestSource.byteLength,
    `${moduleName} vo.release.json web_manifest size mismatch`,
  );
  assert(
    release.web_manifest.digest === sha256Digest(webManifestSource),
    `${moduleName} vo.release.json web_manifest digest mismatch`,
  );
}

function validateSourceEntry(entry, moduleName) {
  assert(entry && typeof entry === 'object' && !Array.isArray(entry), `${moduleName} vo.web.json source entries must be objects`);
  assert(typeof entry.path === 'string' && entry.path.length > 0, `${moduleName} vo.web.json source entry path must be a string`);
  try {
    validatePortableRelativePath(entry.path, `${moduleName} vo.web.json source path`);
  } catch (error) {
    fail(error.message);
  }
  assert(
    Number.isSafeInteger(entry.size) && entry.size >= 0 && entry.size <= MAX_METADATA_BYTES,
    `${moduleName} vo.web.json source entry ${entry.path} size must be within the ${MAX_METADATA_BYTES}-byte text limit`,
  );
  assert(sha256Field(entry.digest), `${moduleName} vo.web.json source entry ${entry.path} digest must be sha256`);
}

function shouldValidateEmbeddedSourceFile(file) {
  return ![
    '.vo-source-digest',
    '.vo-version',
    'vo.release.json',
    'vo.web.json',
  ].includes(file.path);
}

function normalizedRequirementEdges(value, label) {
  assert(Array.isArray(value), `${label} must be an array`);
  assert(value.length <= MAX_MODULES, `${label} exceeds the ${MAX_MODULES}-edge limit`);
  const edges = value.map((entry, index) => {
    assert(
      entry
      && typeof entry === 'object'
      && !Array.isArray(entry)
      && typeof entry.module === 'string'
      && typeof entry.constraint === 'string',
      `${label}[${index}] must contain module and constraint strings`,
    );
    return { module: entry.module, constraint: entry.constraint };
  }).sort((left, right) => compareUtf8(left.module, right.module));
  for (let index = 1; index < edges.length; index += 1) {
    assert(edges[index - 1].module !== edges[index].module, `${label} contains duplicate ${edges[index].module}`);
  }
  return edges;
}

function moduleRootForCanonicalModule(module) {
  const components = module.split('/');
  return components.length === 3 ? '.' : components.slice(3).join('/');
}

function validateReleaseSourceContracts(mod, lockedModule, provenanceDependency) {
  const dependencyDirty = provenanceDependency?.dirty === true;
  assert(!dependencyDirty, `${mod.module} provenance dirty flag must be false in strict validation`);
  const voModSource = requireModuleFile(mod, 'vo.mod');
  const modContract = parseCompleteVoModContract(voModSource, `${mod.module} vo.mod`);
  assert(
    modContract.module === mod.module,
    `${mod.module} vo.mod module must match dependency package module`,
  );
  assert(
    modContract.vo === lockedModule.vo,
    `${mod.module} vo.mod vo constraint must match project vo.lock`,
  );
  const lockedEdges = normalizedRequirementEdges(
    lockedModule.deps,
    `${mod.module} project vo.lock deps`,
  );
  const modEdges = normalizedRequirementEdges(
    modContract.require,
    `${mod.module} vo.mod require`,
  );
  assert(
    JSON.stringify(modEdges) === JSON.stringify(lockedEdges),
    `${mod.module} vo.mod requirements must exactly match project vo.lock dependency edges`,
  );
  const expectedModuleRoot = moduleRootForCanonicalModule(modContract.module);
  const releaseFile = mod.files.find((file) => file.path === 'vo.release.json');
  assert(releaseFile, `${mod.module} must include vo.release.json`);

  const releaseSource = moduleFileBytes(releaseFile);
  assert(
    sha256Digest(releaseSource) === lockedModule.release_manifest,
    `${mod.module} vo.release.json digest must match project vo.lock`,
  );
  const release = parseReleaseManifest(mod, releaseSource);
  validateReleaseManifestShape(mod, release);
  const webManifestFile = mod.files.find((file) => file.path === 'vo.web.json');
  assert(webManifestFile, `${mod.module} must include vo.web.json`);
  const webManifestSource = moduleFileBytes(webManifestFile);
  const sourceMarker = requireModuleFile(mod, '.vo-source-digest');
  const versionMarker = requireModuleFile(mod, '.vo-version');

  assert(release.schema_version === 1, `${mod.module} vo.release.json schema_version must be 1`);
  assert(release.module === mod.module, `${mod.module} vo.release.json module mismatch`);
  assert(release.version === mod.version, `${mod.module} vo.release.json version mismatch`);
  assert(
    release.module_root === expectedModuleRoot,
    `${mod.module} vo.release.json module_root must match the module path root`,
  );
  assert(release.vo === lockedModule.vo, `${mod.module} vo.release.json vo constraint must match project vo.lock`);
  assert(release.commit === lockedModule.commit, `${mod.module} vo.release.json commit must match project vo.lock`);
  assert(release.source && typeof release.source === 'object' && !Array.isArray(release.source), `${mod.module} vo.release.json source must be an object`);
  assert(sha256Field(release.source.digest), `${mod.module} vo.release.json source digest must be sha256`);
  assert(
    Number.isSafeInteger(release.source.size) && release.source.size > 0 && release.source.size <= MAX_FILE_BYTES,
    `${mod.module} vo.release.json source size must be within the ${MAX_FILE_BYTES}-byte limit`,
  );
  assert(release.source?.digest === lockedModule.source, `${mod.module} vo.release.json source digest must match project vo.lock`);
  assert(sha256Field(release.source.files_digest), `${mod.module} vo.release.json files_digest must be sha256`);
  assert(
    Number.isSafeInteger(release.source.files_size) && release.source.files_size > 0 && release.source.files_size <= MAX_SOURCE_BYTES,
    `${mod.module} vo.release.json files_size must be within the ${MAX_SOURCE_BYTES}-byte limit`,
  );
  assert(
    sourceMarker === `${lockedModule.source}\n`,
    `${mod.module} .vo-source-digest must be the canonical lock digest line`,
  );
  assert(
    sourceMarker === `${release.source?.digest}\n`,
    `${mod.module} .vo-source-digest must match vo.release.json source digest`,
  );
  assert(versionMarker === `${mod.version}\n`, `${mod.module} .vo-version must be the canonical module version line`);
  const releaseEdges = normalizedRequirementEdges(release.require, `${mod.module} vo.release.json require`);
  assert(
    JSON.stringify(lockedEdges) === JSON.stringify(releaseEdges),
    `${mod.module} project vo.lock dependency edges must match vo.release.json require`,
  );
  verifyWebManifestBinding(mod.module, release, webManifestSource);
  const declaredArtifacts = validateReleaseArtifactContract(mod, release);

  const rawWebManifest = parseVoWebManifest(mod, webManifestSource);
  const packagedSources = new Map(mod.files.map((file) => [file.path, file]));
  const packagedArtifacts = new Map(mod.artifacts.map((artifact) => [artifactKey(artifact), artifact]));
  const { manifest: webManifest } = validateWebManifestVoModContract(
    rawWebManifest,
    voModSource,
    `${mod.module} browser release contract`,
    {
      expectedModule: mod.module,
      expectedVersion: mod.version,
      expectedCommit: lockedModule.commit,
      expectedVo: release.vo,
      sourceBytes(path) {
        const file = packagedSources.get(path);
        assert(file, `${mod.module} omits source declared by vo.web.json: ${path}`);
        return moduleFileBytes(file);
      },
      artifactBytes(artifact) {
        const packaged = packagedArtifacts.get(artifactKey(artifact));
        assert(packaged, `${mod.module} omits artifact declared by vo.web.json: ${artifactLabel(artifact)}`);
        return readRegularFileLimited(
          localPathForArtifact(packaged.url),
          `${mod.module} browser artifact ${artifactLabel(artifact)}`,
          MAX_FILE_BYTES,
        );
      },
    },
  );
  assert(webManifest.schema_version === 1, `${mod.module} vo.web.json schema_version must be 1`);
  assert(webManifest.module === mod.module, `${mod.module} vo.web.json module mismatch`);
  assert(webManifest.version === mod.version, `${mod.module} vo.web.json version mismatch`);
  assert(
    webManifest.module_root === expectedModuleRoot,
    `${mod.module} vo.web.json module_root must match the module path root`,
  );
  assert(webManifest.vo === release.vo, `${mod.module} vo.web.json vo constraint must match vo.release.json`);
  assert(webManifest.commit === lockedModule.commit, `${mod.module} vo.web.json commit must match project vo.lock`);
  const webEdges = normalizedRequirementEdges(webManifest.require, `${mod.module} vo.web.json require`);
  assert(
    JSON.stringify(webEdges) === JSON.stringify(releaseEdges),
    `${mod.module} vo.web.json dependency edges must match vo.release.json require`,
  );
  assert(Array.isArray(webManifest.source), `${mod.module} vo.web.json source must be an array`);
  assert(webManifest.source.length <= MAX_FILES, `${mod.module} vo.web.json source exceeds the ${MAX_FILES}-entry limit`);
  const seen = new Set();
  let sourceBytes = 0;
  for (const entry of webManifest.source) {
    validateSourceEntry(entry, mod.module);
    const collisionKey = portableCollisionKey(entry.path);
    assert(!seen.has(collisionKey), `${mod.module} vo.web.json duplicate portable source entry ${entry.path}`);
    seen.add(collisionKey);
    sourceBytes += entry.size;
    assert(
      Number.isSafeInteger(sourceBytes) && sourceBytes <= MAX_SOURCE_BYTES,
      `${mod.module} vo.web.json source exceeds the ${MAX_SOURCE_BYTES}-byte payload limit`,
    );
  }
  assert(sourceSetDigest(webManifest.source) === webManifest.source_digest, `${mod.module} vo.web.json source_digest mismatch`);
  assert(
    release.source?.files_digest === webManifest.source_digest,
    `${mod.module} vo.release.json files_digest must match vo.web.json source_digest`,
  );
  assert(
    release.source?.files_size === sourceBytes,
    `${mod.module} vo.release.json files_size must match vo.web.json source bytes`,
  );

  const sourceByPath = new Map(webManifest.source.map((entry) => [entry.path, entry]));
  const embeddedByPath = new Map(mod.files.map((file) => [file.path, file]));
  for (const entry of webManifest.source) {
    assert(
      embeddedByPath.has(entry.path),
      `${mod.module} omits source declared by vo.web.json: ${entry.path}`,
    );
  }
  for (const file of mod.files) {
    if (!shouldValidateEmbeddedSourceFile(file)) continue;
    const entry = sourceByPath.get(file.path);
    assert(entry, `${mod.module} embeds source file not declared by vo.web.json: ${file.path}`);
    const bytes = moduleFileBytes(file);
    assert(bytes.byteLength === entry.size, `${mod.module} embedded source size mismatch for ${file.path}`);
    assert(sha256Digest(bytes) === entry.digest, `${mod.module} embedded source digest mismatch for ${file.path}`);
  }
  validateActualModuleArtifacts(
    mod,
    lockedModule,
    release,
    webManifest,
    provenanceDependency,
    declaredArtifacts,
  );
  return webManifest;
}

function requireModuleFile(mod, path) {
  const file = mod.files.find((file) => file.path === path);
  assert(file, `${mod.module} must include ${path}`);
  return decodeModuleTextUtf8(moduleFileBytes(file), `${mod.module} ${path}`);
}

function requireProjectFile(project, path) {
  const file = project.files.find((file) => file.path === path);
  assert(file, `project package must include ${path}`);
  return decodeModuleTextUtf8(moduleFileBytes(file), `quickplay project ${path}`);
}

function parseCompleteVoModContract(source, label) {
  const rootContract = parseVoModRootContract(source, label);
  const metadata = parseVoModWebMetadata(source, label);
  return { ...rootContract, metadata };
}

function parseVoModDeclaredArtifacts(source, label) {
  return parseVoModWebMetadata(source, label).declaredArtifacts;
}

function parseVoWebManifest(mod, manifestSource) {
  const text = manifestUtf8(manifestSource, `${mod.module} vo.web.json`);
  let manifest;
  try {
    assertNoDuplicateJsonKeys(text, `${mod.module} vo.web.json`);
    manifest = JSON.parse(text);
  } catch (error) {
    fail(`${mod.module} vo.web.json is invalid JSON: ${error.message}`);
  }
  assert(
    manifest && typeof manifest === 'object' && !Array.isArray(manifest),
    `${mod.module} vo.web.json must contain a JSON object`,
  );
  return manifest;
}

function sha256Field(value) {
  return typeof value === 'string' && /^sha256:[0-9a-f]{64}$/.test(value);
}

function artifactLabel(artifact) {
  return `${artifact.kind} ${artifact.target} ${artifact.name}`;
}

function validateReleaseArtifactContract(mod, release) {
  const declared = parseVoModDeclaredArtifacts(requireModuleFile(mod, 'vo.mod'), `${mod.module} vo.mod`);
  const declaredMap = artifactMap(declared, `${mod.module} vo.mod artifacts`, { requireDigest: false });
  const publishedMap = artifactMap(release.artifacts ?? [], `${mod.module} release artifacts`, {
    maxBytes: MAX_TOTAL_BYTES,
  });
  assertExactArtifactKeys(declaredMap, publishedMap, `${mod.module} vo.mod`, `${mod.module} vo.release.json`);
  return declared;
}

function validateProjectDependencyContracts(project, deps, provenance) {
  const modContract = parseCompleteVoModContract(
    requireProjectFile(project, 'vo.mod'),
    'quickplay project vo.mod',
  );
  const lock = parseVoLockV2(
    requireProjectFile(project, 'vo.lock'),
    'quickplay project vo.lock',
  );
  assert(modContract.module === project.module, 'project vo.mod module must match project package module');
  assert(lock.root.module === project.module, 'project vo.lock root module must match project package module');
  validateVoLockV2RootGraph(lock, modContract, 'quickplay project vo.lock');
  validatePackagedModuleSet(lock, deps.modules, 'quickplay deps.modules');
  const directRequires = new Map(
    modContract.require.map((requirement) => [requirement.module, requirement.constraint]),
  );
  const locked = new Map(lock.resolved.map((entry) => [entry.path, entry]));
  const provenanceDeps = new Map((provenance.dependencies ?? []).map((mod) => [mod.module, mod]));
  const verifiedWebManifests = new Map();

  for (const moduleName of expected.requiredModules) {
    const requiredVersion = directRequires.get(moduleName);
    assert(requiredVersion, `project vo.mod must directly require ${moduleName}`);
    const mod = moduleByName(new Map(deps.modules.map((entry) => [entry.module, entry])), moduleName);
    assert(typeof mod.version === 'string', `deps package must identify ${moduleName} version`);
  }

  for (const mod of deps.modules) {
    const lockedModule = locked.get(mod.module);
    const provenanceDependency = provenanceDeps.get(mod.module);
    assert(lockedModule, `project vo.lock must resolve packaged dependency ${mod.module}`);
    assert(
      lockedModule.version === mod.version,
      `project vo.lock resolves ${mod.module} ${lockedModule.version}, but deps package embeds ${mod.version}`,
    );
    assert(mod.commit === lockedModule.commit, `${mod.module} deps commit must match project vo.lock`);
    assert(provenanceDependency, `provenance missing dependency ${mod.module}`);
    assert(provenanceDependency.commit === mod.commit, `${mod.module} provenance commit must match deps`);
    assert(provenanceDependency.dirty === mod.dirty, `${mod.module} provenance dirty flag must match deps`);
    assert(provenanceDependency.source === mod.source, `${mod.module} provenance source must match deps`);
    assert(sha256Field(lockedModule.source), `project vo.lock must bind ${mod.module} source digest`);
    assert(sha256Field(lockedModule.release_manifest), `project vo.lock must bind ${mod.module} release manifest digest`);

    const webManifest = validateReleaseSourceContracts(
      mod,
      lockedModule,
      provenanceDependency,
    );
    verifiedWebManifests.set(mod.module, webManifest);
    assert(webManifest.module === mod.module, `${mod.module} vo.web.json module mismatch`);
    assert(webManifest.version === mod.version, `${mod.module} vo.web.json version mismatch`);
    assert(webManifest.commit === lockedModule.commit, `${mod.module} vo.web.json commit must match project vo.lock`);

    const webArtifacts = new Map((webManifest.artifacts ?? []).map((artifact) => [artifactKey(artifact), artifact]));
    for (const lockedArtifact of lockedModule.artifacts ?? []) {
      const webArtifact = webArtifacts.get(artifactKey(lockedArtifact));
      assert(webArtifact, `${mod.module} vo.web.json missing locked artifact ${lockedArtifact.name}`);
      assert(
        webArtifact.size === lockedArtifact.size,
        `${mod.module} artifact size mismatch for ${lockedArtifact.name}`,
      );
      assert(
        webArtifact.digest === lockedArtifact.digest,
        `${mod.module} artifact digest mismatch for ${lockedArtifact.name}`,
      );
    }
  }

  return verifiedWebManifests;
}

function validateProjectSourceCheckout(project, provenance) {
  assert(existsSync(blockKartRoot), `BlockKart source checkout is required at ${blockKartRoot}`);
  assert(gitOutput(['rev-parse', '--is-inside-work-tree'], blockKartRoot) === 'true', `BlockKart source is not a git checkout: ${blockKartRoot}`);
  const head = gitOutput(['rev-parse', 'HEAD'], blockKartRoot);
  assert(head === project.commit, `BlockKart source HEAD ${head} does not match project commit ${project.commit}`);
  const status = gitStatus(blockKartRoot);
  assert((status !== '') === provenance.project?.dirty, 'BlockKart source dirty state must match provenance project.dirty');
  assert(status === '', 'BlockKart source checkout must be clean for strict quickplay validation');
  assert(provenance.project?.dirty === false, 'provenance project.dirty must be false for strict quickplay validation');

  for (const file of project.files ?? []) {
    const sourcePath = blockKartSourcePath(file.path, 'BlockKart packaged source path');
    assert(existsSync(sourcePath), `BlockKart source checkout is missing packaged file ${file.path}`);
    const sourceFile = digestRegularFileLimited(sourcePath, `BlockKart packaged source ${file.path}`);
    const packagedBytes = moduleFileBytes(file);
    if (file.path === 'vo.lock') {
      const sourceBytes = readRegularFileLimited(
        sourcePath,
        'BlockKart source vo.lock',
        MAX_JSON_BYTES,
      );
      validateProjectLockRewrite(project, sourceBytes, packagedBytes);
      continue;
    }
    if (sourceFile.size !== packagedBytes.byteLength || sourceFile.digest !== sha256Digest(packagedBytes)) {
      assert(sourceFile.size === packagedBytes.byteLength, `BlockKart source size mismatch for ${file.path}`);
      assert(sourceFile.digest === sha256Digest(packagedBytes), `BlockKart source digest mismatch for ${file.path}`);
    }
  }

  const sourceFiles = listProjectSourceFiles(blockKartRoot, '.vo');
  const manifestSourceFiles = project.sourceFiles ?? provenance.project?.sourceFiles;
  assert(Array.isArray(manifestSourceFiles), 'quickplay package must record BlockKart sourceFiles digest list');
  assert(
    sourceSetDigest(manifestSourceFiles) === sourceSetDigest(sourceFiles),
    'quickplay package sourceFiles digest list must match BlockKart source checkout',
  );
  assert(
    provenance.project?.sourceFilesDigest === sourceSetDigest(sourceFiles),
    'provenance project sourceFilesDigest must match BlockKart source checkout',
  );

  const packagedVo = new Set((project.files ?? [])
    .map((file) => file.path)
    .filter((file) => file.endsWith('.vo')));
  const sourceAllowlist = new Map(project.sourceAllowlist
    .map((entry) => [entry.path, entry]));
  const unpackaged = sourceFiles
    .filter((entry) => !packagedVo.has(entry.path))
    .filter((entry) => !sourceAllowlist.has(entry.path));
  assert(unpackaged.length === 0, `BlockKart source files missing from quickplay package or allowlist: ${unpackaged.map((entry) => entry.path).join(', ')}`);
  for (const entry of sourceAllowlist.values()) {
    assert(
      typeof entry.reason === 'string'
        && entry.reason.trim().length >= 12
        && sourceFiles.some((source) => source.path === entry.path),
      `invalid BlockKart source allowlist entry: ${entry.path ?? '<missing>'}`,
    );
  }
}

function revalidateRepositoryState(project, deps, freshEvidence) {
  assert(
    gitOutput(['rev-parse', 'HEAD'], blockKartRoot) === project.commit,
    'BlockKart source HEAD changed during validation',
  );
  assert(gitStatus(blockKartRoot) === '', 'BlockKart source became dirty during validation');
  if (selftestMode) return;
  const volangEvidence = freshEvidence?.repos?.find((repo) => repo.name === 'volang');
  assert(volangEvidence, 'freshEvidence must bind the Volang repository');
  assert(
    gitOutput(['rev-parse', 'HEAD'], root) === volangEvidence.commit,
    'Volang source HEAD changed during validation',
  );
  assert(
    gitDirty(root) === volangEvidence.dirty,
    'Volang source dirty state changed during validation',
  );
  const packaged = new Map(deps.modules.map((mod) => [mod.module, mod]));
  for (const repo of dependencyRepos) {
    const mod = packaged.get(repo.name);
    assert(mod, `validated repository ${repo.name} is absent from deps`);
    assert(
      gitOutput(['rev-parse', 'HEAD'], repo.root) === mod.commit,
      `${repo.name} source HEAD changed during validation`,
    );
    assert(gitStatus(repo.root) === '', `${repo.name} source became dirty during validation`);
  }
}

function moduleFileMap(mod) {
  return new Map(mod.files.map((file) => [file.path, file]));
}

function validateVoplayHostWakeKeyContract(voplay, manifest) {
  assert(manifest, 'voplay must have a release-verified vo.web.json manifest');
  const rendererEntry = manifest.extension?.web?.js_modules?.renderer;
  assert(typeof rendererEntry === 'string' && rendererEntry.length > 0, 'voplay vo.web.json must declare a renderer JS module');
  const files = moduleFileMap(voplay);
  validateVoplayRendererContract({
    entryPath: rendererEntry,
    label: `${voplay.module} renderer contract`,
    declarations: {
      path: 'js/dist/render_bootstrap.d.ts',
      source: requireModuleFile(voplay, 'js/dist/render_bootstrap.d.ts'),
    },
    readSource(path) {
      const file = files.get(path);
      if (!file) return undefined;
      return decodeModuleTextUtf8(moduleFileBytes(file), `${voplay.module} ${path}`);
    },
  });
}

function currentGeneratorSourceDigest() {
  const entries = QUICKPLAY_GENERATOR_SOURCE_INPUTS.map((relativePath) => {
    const file = digestRegularFileLimited(
      join(root, relativePath),
      `quickplay generator input ${relativePath}`,
      MAX_METADATA_BYTES,
    );
    return { digest: file.digest, path: relativePath, size: file.size };
  });
  return quickplayGeneratorSourceDigest(entries);
}

function validateProvenance(project, deps) {
  const provenance = readJson(provenancePath);
  assert(provenance.schemaVersion === 3, 'provenance schemaVersion must be 3');
  assert(provenance.artifact === expected.artifactName, `provenance artifact must be ${expected.artifactName}`);
  assert(provenance.path === expected.artifactPath, `provenance path must be ${expected.artifactPath}`);
  assert(provenance.task?.id === QUICKPLAY_TASK_ID, 'provenance task id mismatch');
  assert(
    sameStringArray(provenance.task?.command, expected.generatorCommand),
    'provenance task command mismatch',
  );
  assert(
    sameStringArray(provenance.generator?.command, expected.generatorCommand),
    'provenance generator command mismatch',
  );
  assert(
    provenance.generator?.script === 'apps/studio/scripts/package_blockkart_quickplay.mjs',
    'provenance generator script mismatch',
  );
  assert(
    provenance.generator?.version === QUICKPLAY_GENERATOR_VERSION,
    `provenance generator version must be ${QUICKPLAY_GENERATOR_VERSION}`,
  );
  assert(/^v\d+$/.test(provenance.toolchain?.node ?? ''), 'provenance node toolchain must use a reproducible major version');
  assert(
    provenance.toolchain?.voDevSourceDigest === currentGeneratorSourceDigest(),
    'provenance vo-dev generator source digest must match the current generator contract',
  );
  assert(provenance.toolchain?.wasmTarget === 'wasm32-unknown-unknown', 'provenance wasm target mismatch');
  assert(
    JSON.stringify(provenance.sourceRoots) === JSON.stringify(expected.sourceRoots),
    'provenance source roots must use canonical repository identities',
  );
  assert(
    sameStringArray(provenance.inputs, expected.generatorInputs),
    'provenance generator inputs mismatch',
    {
      owner: 'studio/artifacts',
      subsystem: 'ProducerProvenance',
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      line: lineInFile(provenancePath, '"inputs"'),
      reason: 'The current quickplay provenance was produced by an older generator input contract.',
      requiredFix: 'Regenerate quickplay provenance so generator.inputs matches the complete declared generator contract.',
      expected: expected.generatorInputs,
      found: provenance.inputs ?? null,
    },
  );
  assert(provenance.project?.module === project.module, 'provenance project module mismatch');
  assert(provenance.project?.commit === project.commit, 'provenance project commit mismatch');
  assert(provenance.project?.dirty === project.dirty, 'provenance project dirty flag must match project package');
  assert(provenance.project?.dirty === gitDirty(blockKartRoot), 'provenance project dirty flag mismatch');
  assert(provenance.project?.dirty === false, 'provenance project dirty flag must be false');
  assert(
    provenance.project?.filesDigest === packagedFilesDigest(project.files),
    'provenance project files digest mismatch',
  );
  assert(
    JSON.stringify(provenance.project?.lockRewrite ?? null) === JSON.stringify(project.lockRewrite ?? null),
    'provenance project lockRewrite mismatch',
  );
  assert(
    sourceSetDigest(provenance.project?.sourceFiles ?? []) === sourceSetDigest(project.sourceFiles ?? []),
    'provenance project sourceFiles must match project sourceFiles',
  );
  assert(
    JSON.stringify(provenance.project?.sourceAllowlist) === JSON.stringify(project.sourceAllowlist),
    'provenance project sourceAllowlist must exactly match the project package policy',
  );
  assert(
    provenance.project?.sourceFilesDigest === sourceSetDigest(project.sourceFiles ?? []),
    'provenance project sourceFilesDigest mismatch',
  );
  validateVoplayWasmProducer(provenance, deps);
  validateBlockKartRuntimeProducer(project, provenance, deps);

  const outputMap = new Map((provenance.outputs ?? []).map((output) => [output.path, output]));
  const projectOutput = outputMap.get('project.json');
  const depsOutput = outputMap.get('deps.json');
  const projectBytes = readRegularFileLimited(projectPath, 'quickplay project.json', MAX_JSON_BYTES);
  const depsBytes = readRegularFileLimited(depsPath, 'quickplay deps.json', MAX_JSON_BYTES);
  assert(projectOutput?.digest === jsonFileDigest(projectPath), 'provenance project.json digest mismatch');
  assert(projectOutput?.size === projectBytes.byteLength, 'provenance project.json size mismatch');
  assert(depsOutput?.digest === jsonFileDigest(depsPath), 'provenance deps.json digest mismatch');
  assert(depsOutput?.size === depsBytes.byteLength, 'provenance deps.json size mismatch');

  const provenanceDeps = new Map((provenance.dependencies ?? []).map((mod) => [mod.module, mod]));
  for (const mod of deps.modules) {
    const provenanceMod = provenanceDeps.get(mod.module);
    assert(provenanceMod, `provenance missing dependency ${mod.module}`);
    assert(provenanceMod.version === mod.version, `provenance version mismatch for ${mod.module}`);
    assert(provenanceMod.cacheDir === mod.cacheDir, `provenance cacheDir mismatch for ${mod.module}`);
    assert(provenanceMod.commit === mod.commit, `provenance commit mismatch for ${mod.module}`);
    assert(provenanceMod.source === mod.source, `provenance source mismatch for ${mod.module}`);
    assert(typeof provenanceMod.dirty === 'boolean', `provenance dirty flag missing for ${mod.module}`);
    assert(provenanceMod.dirty === mod.dirty, `provenance dirty flag mismatch for ${mod.module}`);
    assert(provenanceMod.dirty === false, `provenance dirty flag must be false for ${mod.module}`);
    if (!selftestMode && provenanceMod.source === 'external') {
      const voplayRepo = dependencyRepos.find((repo) => repo.name === mod.module);
      assert(voplayRepo, `validator repo root missing for ${mod.module}`);
      const dependencyHead = gitOutput(['rev-parse', 'HEAD'], voplayRepo.root);
      const dependencyDirty = gitDirty(voplayRepo.root);
      assert(dependencyHead === mod.commit, `${mod.module} checkout HEAD must match packaged commit`);
      assert(provenanceMod.commit === dependencyHead, `${mod.module} provenance commit must match checkout HEAD`);
      assert(provenanceMod.dirty === dependencyDirty, `provenance dirty flag mismatch for ${mod.module}`);
      assert(!dependencyDirty, `${mod.module} checkout must be clean for strict quickplay validation`);
    }
    assert(provenanceMod.filesDigest === packagedFilesDigest(mod.files), `provenance files digest mismatch for ${mod.module}`);

    const artifactDigests = new Map();
    for (const artifact of provenanceMod.artifacts ?? []) {
      const key = provenanceArtifactKey(mod.module, artifact);
      assert(
        !artifactDigests.has(key),
        `provenance contains duplicate artifact identity for ${artifactLabel(artifact)}`,
        {
          owner: 'studio/artifacts',
          subsystem: 'ProducerProvenance',
          file: 'apps/studio/public/quickplay/blockkart/provenance.json',
          line: lineInFile(provenancePath, artifact.name),
          found: artifact,
          requiredFix: 'Regenerate quickplay so each provenance artifact identity is unique.',
        },
      );
      artifactDigests.set(key, artifact);
    }
    for (const artifact of mod.artifacts ?? []) {
      const key = artifactKey(artifact);
      const provenanceArtifact = artifactDigests.get(key);
      assert(provenanceArtifact, `provenance missing artifact ${artifactLabel(artifact)}`);
      assert(provenanceArtifact.url === artifact.url, `provenance artifact URL mismatch for ${artifactLabel(artifact)}`);
      assert(provenanceArtifact.path === artifact.path, `provenance artifact path mismatch for ${artifact.url}`);
      const localPath = localPathForArtifact(artifact.url);
      const actual = digestRegularFileLimited(localPath, `provenance artifact ${artifact.url}`);
      assert(provenanceArtifact.size === actual.size, `provenance artifact size mismatch for ${artifact.url}`);
      assert(provenanceArtifact.digest === actual.digest, `provenance artifact digest mismatch for ${artifact.url}`);
    }
  }
}

const project = readJson(projectPath);
const deps = readJson(depsPath);
validatePackageBudgets(project, deps);
const provenance = readJson(provenancePath, MAX_METADATA_BYTES);
validateProvenanceStructure(provenance, deps);
const quickplayTs = manifestUtf8(
  readRegularFileLimited(quickplayTsPath, 'Studio quickplay.ts', MAX_METADATA_BYTES),
  'Studio quickplay.ts',
);

assert(project.schemaVersion === packageSchemaVersion, 'project schemaVersion changed during validation');
assert(deps.schemaVersion === packageSchemaVersion, 'deps schemaVersion changed during validation');
assert(project.name === expected.projectName, `project name must be ${expected.projectName}`);
assert(project.module === expected.projectModule, `project module must be ${expected.projectModule}`);
assert(deps.name === 'BlockKart dependencies', 'deps name must be BlockKart dependencies');
assert(Array.isArray(project.files) && project.files.length > 0, 'project files must be embedded');
assert(project.files.some((file) => file.path === 'main.vo'), 'project package must include main.vo');
const projectFiles = new Map(project.files.map((file) => [file.path, file]));
const runtimeAsset = projectFiles.get('assets/blockkart.vpak');
assert(runtimeAsset, 'project package must include assets/blockkart.vpak');
assert(moduleFileBytes(runtimeAsset).byteLength > 1024 * 1024, 'assets/blockkart.vpak must contain the runtime asset pack');
assert(!projectFiles.has('apps/studio/fixtures/blockkart/blockkart.vpak'), 'project package must not embed the Studio fixture path');

assert(Array.isArray(deps.modules) && deps.modules.length > 0, 'deps modules must be embedded');
for (const mod of deps.modules) {
  let expectedCacheDir;
  try {
    expectedCacheDir = moduleCacheDir(mod.module, mod.version);
  } catch (error) {
    fail(`invalid packaged module identity: ${error.message}`);
  }
  assert(
    mod.cacheDir === expectedCacheDir,
    `${mod.module} cacheDir must be ${expectedCacheDir}`,
  );
}
const modules = new Map(deps.modules.map((mod) => [mod.module, mod]));
assert(modules.size === deps.modules.length, 'deps modules must be unique');
for (const moduleName of expected.requiredModules) {
  moduleByName(modules, moduleName);
}
const verifiedWebManifests = validateProjectDependencyContracts(project, deps, provenance);
validateProvenance(project, deps);
validateProjectSourceCheckout(project, provenance);

for (const mod of deps.modules) {
  assert(Array.isArray(mod.files) && mod.files.length > 0, `${mod.module} must embed files`);
  for (const file of mod.files) {
    assert(!file.path.includes('.codex-release-stage'), `${mod.module} packaged transient file: ${file.path}`);
  }
  for (const artifact of mod.artifacts ?? []) {
    assert(typeof artifact.url === 'string', `${mod.module} artifact must have url`);
    const expectedPath = artifactCachePath(artifact);
    assert(
      artifact.path === expectedPath,
      `${mod.module} artifact ${artifactLabel(artifact)} path must be ${expectedPath}`,
    );
    let expectedUrl;
    try {
      expectedUrl = quickplayArtifactUrl(mod.cacheDir, artifact);
    } catch (error) {
      fail(`${mod.module} has invalid artifact identity: ${error.message}`);
    }
    assert(
      artifact.url === expectedUrl,
      `${mod.module} artifact ${artifactLabel(artifact)} URL must be ${expectedUrl}`,
    );
    const localPath = localPathForArtifact(artifact.url);
    assert(existsSync(localPath), `missing packaged artifact: ${artifact.url}`);
    assert(
      digestRegularFileLimited(localPath, `packaged artifact ${artifact.url}`).size > 0,
      `empty packaged artifact: ${artifact.url}`,
    );
  }
  validatePackagedWebManifest(mod);
}

validateStudioQuickplayTypeScriptContract(quickplayTs);
const declaredArtifactUrls = new Set();
for (const mod of deps.modules) {
  for (const artifact of mod.artifacts ?? []) {
    declaredArtifactUrls.add(artifact.url);
  }
}
validateQuickplayOutputClosure(declaredArtifactUrls);
const voplay = moduleByName(modules, 'github.com/vo-lang/voplay');
validateVoplayHostWakeKeyContract(voplay, verifiedWebManifests.get(voplay.module));
for (const artifact of requiredVoplayArtifacts(voplay)) {
  assert(declaredArtifactUrls.has(artifact.url), `voplay artifact is not declared: ${artifact.url}`);
}

try {
  revalidateAllStableFiles();
} catch (error) {
  fail(`validated input changed before report publication: ${error.message}`);
}

const finalReport = buildReport('ok', {
  project: {
    module: project.module,
    commit: project.commit ?? null,
    provenanceCommit: provenance.project?.commit ?? null,
  },
  dependencies: (deps.modules ?? []).map((mod) => ({
    module: mod.module,
    version: mod.version,
    commit: mod.commit ?? null,
  })),
});
try {
  if (!selftestMode) {
    const evidenceScope = quickplayEvidenceScope();
    const evidenceIssues = verifySourceBoundEvidence({
      evidence: finalReport.freshEvidence,
      expectedGate: 'quickplay-validate',
      expectedCiRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
      root,
      expectedGateFiles: evidenceScope.gateFiles,
      expectedArtifacts: evidenceScope.artifacts,
      expectedRepos: evidenceScope.repos,
    });
    assert(
      evidenceIssues.length === 0,
      `quickplay freshEvidence changed before publication: ${evidenceIssues.join('; ')}`,
    );
  }
  revalidateAllStableFiles();
  validateQuickplayOutputClosure(declaredArtifactUrls);
  revalidateRepositoryState(project, deps, finalReport.freshEvidence);
} catch (error) {
  fail(`validated inputs changed during evidence publication: ${error.message}`);
}
publishReport(finalReport);
if (finalReport.status !== 'ok') {
  console.error(`quickplay validate: failed with ${finalReport.issues.length} issue(s); wrote ${outDir}`);
  process.exit(1);
}
console.log('quickplay validate: ok');
