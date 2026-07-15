#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  existsSync,
  fstatSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  openSync,
  opendirSync,
  readSync,
  realpathSync,
  renameSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from 'node:fs';
import { tmpdir } from 'node:os';
import { basename, dirname, isAbsolute, join, relative, resolve, sep } from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { requiredVpakProducerInputPaths } from './blockkart_vpak_policy.mjs';
import { artifactKey } from './quickplay_artifact_paths.mjs';
import {
  currentVoCliToolchain,
  currentVoCliBuildInputs,
  VO_CLI_GUEST_ENVIRONMENT,
  voCliExecutionDigest,
  verifyVoCliExecutionIdentity,
  verifyVoCliBuildInputs,
} from './quickplay_cli_producer_contract.mjs';
import { quickplayBlockKartSourceAllowlist } from './quickplay_generator_contract.mjs';
import { QUICKPLAY_SOURCE_AUDIT_GATE_FILES } from './quickplay_source_audit_scope.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';
import { verifyCurrentVoplayWasm } from './voplay_current_wasm.mjs';
import { compareUtf8 } from './utf8_order.mjs';
import {
  decodeModuleTextUtf8,
  parseVoLockV2,
  parseVoModRootContract,
  validatePackagedModuleSet,
  validateVoLockV2RootGraph,
} from './vo_lock_v2.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const quickplayDir = resolve(process.env.QUICKPLAY_DIR ?? join(root, 'apps/studio/public/quickplay/blockkart'));
const outDir = resolve(argValue('--out-dir') || process.env.QUICKPLAY_SOURCE_AUDIT_OUT_DIR || join(root, 'target/quickplay-source-audit'));
const voplayCurrentWasmRoot = resolve(process.env.VOPLAY_CURRENT_WASM_OUT_DIR ?? join(root, 'target/voplay-current-wasm'));
const WASM_TARGET = 'wasm32-unknown-unknown';
const UTF8_DECODER = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });
const READ_CHUNK_BYTES = 1024 * 1024;
const SOURCE_AUDIT_LIMITS = Object.freeze({
  maxJsonBytes: 128 * 1024 * 1024,
  maxMetadataJsonBytes: 16 * 1024 * 1024,
  maxJsonDepth: 256,
  maxJsonTokens: 2_000_000,
  maxJsonObjectKeys: 50_000,
  maxJsonCollectionEntries: 50_000,
  maxJsonTotalCollectionEntries: 200_000,
  maxObjectKeyBytes: 4 * 1024,
  maxJsonTotalObjectKeyBytes: 32 * 1024 * 1024,
  maxTreeEntries: 100_000,
  maxTreeDepth: 256,
  maxPathBytes: 4 * 1024,
  maxTreePathBytes: 32 * 1024 * 1024,
  maxFileBytes: 256 * 1024 * 1024,
  maxTreeBytes: 512 * 1024 * 1024,
  maxAuditReadBytes: 1024 * 1024 * 1024,
  maxDiagnosticBytes: 2 * 1024 * 1024,
  maxReportBytes: 64 * 1024 * 1024,
  maxGitTextBytes: 4 * 1024 * 1024,
  maxGitShowCalls: 20_000,
  maxGitShowFileBytes: 64 * 1024 * 1024,
  maxGitShowBytes: 512 * 1024 * 1024,
});
let blockKartRoot;
const dependencyRepos = new Map();

class ByteBudget {
  constructor(maxBytes, label) {
    this.maxBytes = maxBytes;
    this.label = label;
    this.bytes = 0;
  }

  charge(size, item) {
    if (!Number.isSafeInteger(size) || size < 0) {
      throw new Error(`${item} has an unsupported byte size`);
    }
    if (this.bytes > this.maxBytes - size) {
      throw new Error(`${this.label} exceeds the ${this.maxBytes}-byte limit while reading ${item}`);
    }
    this.bytes += size;
  }
}

class GitShowBudget {
  constructor(limits = SOURCE_AUDIT_LIMITS) {
    this.maxCalls = limits.maxGitShowCalls;
    this.maxFileBytes = limits.maxGitShowFileBytes;
    this.maxBytes = limits.maxGitShowBytes;
    this.calls = 0;
    this.bytes = 0;
  }

  begin(label, expectedBytes) {
    if (!Number.isSafeInteger(expectedBytes) || expectedBytes < 0) {
      throw new Error(`${label} has an invalid expected size`);
    }
    if (expectedBytes > this.maxFileBytes) {
      throw new Error(`${label} exceeds the ${this.maxFileBytes}-byte per-file git show limit`);
    }
    if (this.calls >= this.maxCalls) {
      throw new Error(`git show exceeds the ${this.maxCalls}-call limit`);
    }
    if (this.bytes > this.maxBytes - expectedBytes) {
      throw new Error(`git show exceeds the ${this.maxBytes}-byte aggregate output limit`);
    }
    this.calls += 1;
    return Math.min(this.maxFileBytes, this.maxBytes - this.bytes, expectedBytes + 1);
  }

  finish(label, size) {
    if (!Number.isSafeInteger(size) || size < 0 || size > this.maxFileBytes) {
      throw new Error(`${label} returned an unsupported git show output size`);
    }
    if (this.bytes > this.maxBytes - size) {
      throw new Error(`git show exceeds the ${this.maxBytes}-byte aggregate output limit`);
    }
    this.bytes += size;
  }
}

const auditReadBudget = new ByteBudget(SOURCE_AUDIT_LIMITS.maxAuditReadBytes, 'quickplay source audit file reads');
const gitShowBudget = new GitShowBudget();
const stableAuditFiles = new Map();
const stableAuditTrees = new Map();
let stableAuditFileTrackingEnabled = false;
let stableAuditTreeTrackingEnabled = false;

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : (process.argv[index + 1] ?? '');
}

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function statFingerprint(stat) {
  return [stat.dev, stat.ino, stat.mode, stat.size, stat.mtimeNs, stat.ctimeNs]
    .map((value) => String(value))
    .join(':');
}

function sameStat(left, right) {
  return statFingerprint(left) === statFingerprint(right);
}

function rememberStableAuditFile(path, label, record) {
  if (!stableAuditFileTrackingEnabled) return;
  const absolute = resolve(path);
  const previous = stableAuditFiles.get(absolute);
  if (
    previous
    && (
      previous.digest !== record.digest
      || previous.fingerprint !== record.fingerprint
      || previous.size !== record.size
    )
  ) {
    throw new Error(`${label} changed between audit reads`);
  }
  stableAuditFiles.set(absolute, {
    digest: record.digest,
    fingerprint: record.fingerprint,
    label,
    size: record.size,
  });
}

function inspectRegularFile(path, label, maxBytes) {
  let before;
  try {
    before = lstatSync(path, { bigint: true });
  } catch (error) {
    throw new Error(`${label} cannot be inspected: ${error.message}`);
  }
  if (!before.isFile() || before.isSymbolicLink()) {
    throw new Error(`${label} must be a regular file without symbolic links`);
  }
  if (before.size > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} has an unsupported file size`);
  }
  const size = Number(before.size);
  if (size > maxBytes) {
    throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
  }
  return { before, size };
}

function openStableRegularFile(path, label, maxBytes, budget = null) {
  const { before, size } = inspectRegularFile(path, label, maxBytes);
  budget?.charge(size, label);
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  let fd;
  try {
    fd = openSync(path, fsConstants.O_RDONLY | noFollow);
    const opened = fstatSync(fd, { bigint: true });
    if (!opened.isFile() || !sameStat(before, opened)) {
      throw new Error(`${label} changed while it was opened`);
    }
    return { before: opened, fd, size };
  } catch (error) {
    if (fd !== undefined) closeSync(fd);
    throw error;
  }
}

function finishStableRead(path, label, fd, before, size) {
  const extra = Buffer.allocUnsafe(1);
  if (readSync(fd, extra, 0, 1, size) !== 0) {
    throw new Error(`${label} grew while it was being read`);
  }
  const openedAfter = fstatSync(fd, { bigint: true });
  if (!sameStat(before, openedAfter)) {
    throw new Error(`${label} changed while it was being read`);
  }
  const pathAfter = lstatSync(path, { bigint: true });
  if (!pathAfter.isFile() || pathAfter.isSymbolicLink() || !sameStat(before, pathAfter)) {
    throw new Error(`${label} was replaced while it was being read`);
  }
  return statFingerprint(openedAfter);
}

function readRegularFileLimited(path, label, maxBytes = SOURCE_AUDIT_LIMITS.maxFileBytes, budget = auditReadBudget) {
  const absolute = resolve(path);
  const { before, fd, size } = openStableRegularFile(absolute, label, maxBytes, budget);
  try {
    const bytes = Buffer.allocUnsafe(size);
    let offset = 0;
    while (offset < size) {
      const count = readSync(fd, bytes, offset, size - offset, offset);
      if (count === 0) throw new Error(`${label} was truncated while it was being read`);
      offset += count;
    }
    const fingerprint = finishStableRead(absolute, label, fd, before, size);
    const digest = sha256(bytes);
    rememberStableAuditFile(absolute, label, { digest, fingerprint, size });
    return { bytes, digest, fingerprint, size };
  } finally {
    closeSync(fd);
  }
}

function digestRegularFileLimited(path, label, maxBytes, budget) {
  const absolute = resolve(path);
  const { before, fd, size } = openStableRegularFile(absolute, label, maxBytes, budget);
  try {
    const hash = createHash('sha256');
    const chunk = Buffer.allocUnsafe(Math.min(READ_CHUNK_BYTES, Math.max(1, size)));
    let offset = 0;
    while (offset < size) {
      const count = readSync(fd, chunk, 0, Math.min(chunk.byteLength, size - offset), offset);
      if (count === 0) throw new Error(`${label} was truncated while it was being read`);
      hash.update(chunk.subarray(0, count));
      offset += count;
    }
    const fingerprint = finishStableRead(absolute, label, fd, before, size);
    const digest = `sha256:${hash.digest('hex')}`;
    rememberStableAuditFile(absolute, label, { digest, fingerprint, size });
    return { digest, fingerprint, size };
  } finally {
    closeSync(fd);
  }
}

function revalidateStableAuditFiles() {
  const budget = new ByteBudget(
    SOURCE_AUDIT_LIMITS.maxAuditReadBytes,
    'stable audit file revalidation',
  );
  const tracking = stableAuditFileTrackingEnabled;
  stableAuditFileTrackingEnabled = false;
  try {
    for (const [path, expected] of stableAuditFiles) {
      const found = digestRegularFileLimited(
        path,
        `revalidated ${expected.label}`,
        expected.size,
        budget,
      );
      if (
        found.digest !== expected.digest
        || found.fingerprint !== expected.fingerprint
        || found.size !== expected.size
      ) {
        throw new Error(`${expected.label} changed after it was audited`);
      }
    }
  } finally {
    stableAuditFileTrackingEnabled = tracking;
  }
}

function decodeUtf8(bytes, label) {
  try {
    return UTF8_DECODER.decode(bytes);
  } catch (error) {
    throw new Error(`${label} is not valid UTF-8: ${error.message}`);
  }
}

function assertStrictJsonShape(source, label, limits = SOURCE_AUDIT_LIMITS) {
  const stack = [];
  let rootState = 'value';
  let index = 0;
  let tokens = 0;
  let totalCollectionEntries = 0;
  let totalObjectKeyBytes = 0;

  const chargeToken = () => {
    tokens += 1;
    if (tokens > limits.maxJsonTokens) {
      throw new Error(`${label} exceeds the ${limits.maxJsonTokens}-token JSON limit`);
    }
  };
  const chargeCollectionEntry = (context) => {
    context.entries += 1;
    totalCollectionEntries += 1;
    if (context.entries > limits.maxJsonCollectionEntries) {
      throw new Error(`${label} exceeds the ${limits.maxJsonCollectionEntries}-entry JSON collection limit`);
    }
    if (totalCollectionEntries > limits.maxJsonTotalCollectionEntries) {
      throw new Error(`${label} exceeds the ${limits.maxJsonTotalCollectionEntries}-entry aggregate JSON collection limit`);
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
      index += character === '\\' ? 2 : 1;
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
    chargeToken();
    if (source[index] === '{') {
      index += 1;
      if (stack.length >= limits.maxJsonDepth) {
        throw new Error(`${label} exceeds the ${limits.maxJsonDepth}-level JSON depth limit`);
      }
      stack.push({ entries: 0, kind: 'object', keys: new Set(), state: 'keyOrEnd' });
    } else if (source[index] === '[') {
      index += 1;
      if (stack.length >= limits.maxJsonDepth) {
        throw new Error(`${label} exceeds the ${limits.maxJsonDepth}-level JSON depth limit`);
      }
      stack.push({ entries: 0, kind: 'array', state: 'valueOrEnd' });
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
          chargeToken();
          index += 1;
          stack.pop();
          continue;
        }
        if (source[index] !== '"') throw new Error(`${label} contains a non-string JSON object key`);
        chargeToken();
        const key = scanString();
        const keyBytes = Buffer.byteLength(key, 'utf8');
        if (keyBytes > limits.maxObjectKeyBytes) {
          throw new Error(`${label} contains an object key exceeding ${limits.maxObjectKeyBytes} bytes`);
        }
        if (totalObjectKeyBytes > limits.maxJsonTotalObjectKeyBytes - keyBytes) {
          throw new Error(`${label} exceeds the ${limits.maxJsonTotalObjectKeyBytes}-byte aggregate JSON object-key limit`);
        }
        totalObjectKeyBytes += keyBytes;
        if (context.keys.size >= limits.maxJsonObjectKeys) {
          throw new Error(`${label} exceeds the ${limits.maxJsonObjectKeys}-key JSON object limit`);
        }
        if (context.keys.has(key)) {
          throw new Error(`${label} contains duplicate object key ${JSON.stringify(key)}`);
        }
        context.keys.add(key);
        chargeCollectionEntry(context);
        context.state = 'colon';
        continue;
      }
      if (context.state === 'colon') {
        if (source[index] !== ':') throw new Error(`${label} JSON object key is missing a colon`);
        chargeToken();
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
        chargeToken();
        index += 1;
        context.state = 'keyOrEnd';
      } else if (source[index] === '}') {
        chargeToken();
        index += 1;
        stack.pop();
      } else {
        throw new Error(`${label} JSON object is missing a comma or closing brace`);
      }
      continue;
    }

    if (context.state === 'valueOrEnd') {
      if (source[index] === ']') {
        chargeToken();
        index += 1;
        stack.pop();
      } else {
        if (index === source.length) throw new Error(`${label} JSON array is missing a value`);
        chargeCollectionEntry(context);
        context.state = 'commaOrEnd';
        beginValue();
      }
      continue;
    }
    if (source[index] === ',') {
      chargeToken();
      index += 1;
      context.state = 'valueOrEnd';
    } else if (source[index] === ']') {
      chargeToken();
      index += 1;
      stack.pop();
    } else {
      throw new Error(`${label} JSON array is missing a comma or closing bracket`);
    }
  }
}

function parseJsonBytes(bytes, label, limits = SOURCE_AUDIT_LIMITS) {
  if (bytes.byteLength > limits.maxJsonBytes) {
    throw new Error(`${label} exceeds the ${limits.maxJsonBytes}-byte JSON limit`);
  }
  const source = decodeUtf8(bytes, label);
  assertStrictJsonShape(source, label, limits);
  try {
    return JSON.parse(source);
  } catch (error) {
    throw new Error(`${label} is invalid JSON: ${error.message}`);
  }
}

function readJson(path, maxBytes = SOURCE_AUDIT_LIMITS.maxJsonBytes, limits = SOURCE_AUDIT_LIMITS) {
  const label = `JSON file ${path}`;
  const { bytes } = readRegularFileLimited(path, label, maxBytes);
  return parseJsonBytes(bytes, label, { ...limits, maxJsonBytes: maxBytes });
}

function mergedTreeLimits(overrides = {}) {
  return {
    maxEntries: overrides.maxEntries ?? SOURCE_AUDIT_LIMITS.maxTreeEntries,
    maxDepth: overrides.maxDepth ?? SOURCE_AUDIT_LIMITS.maxTreeDepth,
    maxPathBytes: overrides.maxPathBytes ?? SOURCE_AUDIT_LIMITS.maxPathBytes,
    maxPathTotalBytes: overrides.maxPathTotalBytes ?? SOURCE_AUDIT_LIMITS.maxTreePathBytes,
    maxFileBytes: overrides.maxFileBytes ?? SOURCE_AUDIT_LIMITS.maxFileBytes,
    maxTotalBytes: overrides.maxTotalBytes ?? SOURCE_AUDIT_LIMITS.maxTreeBytes,
  };
}

class TreeBudget extends ByteBudget {
  constructor(limits, label) {
    super(limits.maxTotalBytes, `${label} file bytes`);
    this.limits = limits;
    this.entryCount = 0;
    this.fileCount = 0;
    this.pathBytes = 0;
  }

  addEntry(path) {
    const pathBytes = Buffer.byteLength(path, 'utf8');
    if (pathBytes > this.limits.maxPathBytes) {
      throw new Error(`${path} exceeds the ${this.limits.maxPathBytes}-byte path limit`);
    }
    if (this.pathBytes > this.limits.maxPathTotalBytes - pathBytes) {
      throw new Error(`directory traversal exceeds the ${this.limits.maxPathTotalBytes}-byte aggregate path limit`);
    }
    this.pathBytes += pathBytes;
    this.entryCount += 1;
    if (this.entryCount > this.limits.maxEntries) {
      throw new Error(`directory traversal exceeds the ${this.limits.maxEntries}-entry limit`);
    }
  }

  addFile() {
    this.fileCount += 1;
  }
}

function stableTreeSummary(snapshot) {
  return {
    digest: snapshot.digest,
    entryCount: snapshot.entryCount,
    fileCount: snapshot.fileCount,
    pathBytes: snapshot.pathBytes,
    stabilityDigest: snapshot.stabilityDigest,
    totalBytes: snapshot.totalBytes,
  };
}

function rememberStableAuditTree(rootPath, label, snapshot, options) {
  if (!stableAuditTreeTrackingEnabled) return;
  const key = `${resolve(rootPath)}\0${label}`;
  const summary = stableTreeSummary(snapshot);
  const previous = stableAuditTrees.get(key);
  if (previous && JSON.stringify(previous.summary) !== JSON.stringify(summary)) {
    throw new Error(`${label} changed between audit traversals`);
  }
  if (!previous) {
    stableAuditTrees.set(key, {
      label,
      options,
      root: resolve(rootPath),
      summary,
    });
  }
}

function canonicalTreeRoot(path, label) {
  const requested = resolve(path);
  const requestedStat = lstatSync(requested, { bigint: true });
  if (!requestedStat.isDirectory() || requestedStat.isSymbolicLink()) {
    throw new Error(`${label} must be a real directory without a symbolic-link root`);
  }
  const canonical = realpathSync.native(requested);
  const canonicalStat = lstatSync(canonical, { bigint: true });
  if (!canonicalStat.isDirectory() || canonicalStat.isSymbolicLink() || !sameStat(requestedStat, canonicalStat)) {
    throw new Error(`${label} changed while its canonical root was resolved`);
  }
  return canonical;
}

function assertContainedPath(treeRoot, path, label) {
  const suffix = relative(treeRoot, path);
  if (isAbsolute(suffix) || suffix === '..' || suffix.startsWith(`..${sep}`)) {
    throw new Error(`${label} escapes traversal root ${treeRoot}`);
  }
}

function pathIsWithin(parent, child) {
  const suffix = relative(resolve(parent), resolve(child));
  return suffix === '' || (!isAbsolute(suffix) && suffix !== '..' && !suffix.startsWith(`..${sep}`));
}

function assertSafeReportOutputLocation(outputPath, artifactPath, repositories, volangTarget) {
  if (pathIsWithin(artifactPath, outputPath)) {
    throw new Error('quickplay source audit output directory must be outside the Quickplay artifact tree');
  }
  for (const repository of repositories) {
    if (!pathIsWithin(repository.root, outputPath)) continue;
    if (repository.name === 'volang' && pathIsWithin(volangTarget, outputPath)) continue;
    throw new Error(`quickplay source audit output directory must not modify source repository ${repository.name}`);
  }
}

function ensureSingleRealDirectory(path, label) {
  try {
    mkdirSync(path);
  } catch (error) {
    if (error?.code !== 'EEXIST') throw error;
  }
  const metadata = lstatSync(path, { bigint: true });
  if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
    throw new Error(`${label} must be a real directory without symbolic links`);
  }
  return realpathSync.native(path);
}

function planCanonicalDirectoryCreation(path, label) {
  const requested = resolve(path);
  const missing = [];
  let cursor = requested;
  while (true) {
    let metadata;
    try {
      metadata = lstatSync(cursor, { bigint: true });
    } catch (error) {
      if (error?.code !== 'ENOENT') throw error;
      const parent = dirname(cursor);
      if (parent === cursor) throw new Error(`${label} has no existing directory ancestor`);
      missing.unshift(basename(cursor));
      cursor = parent;
      continue;
    }
    let canonical;
    try {
      canonical = realpathSync.native(cursor);
    } catch (error) {
      throw new Error(`${label} existing ancestor cannot be resolved safely: ${error.message}`);
    }
    const canonicalMetadata = lstatSync(canonical, { bigint: true });
    if (!canonicalMetadata.isDirectory() || canonicalMetadata.isSymbolicLink()) {
      throw new Error(`${label} existing ancestor must resolve to a real directory`);
    }
    if (!metadata.isDirectory() && !metadata.isSymbolicLink()) {
      throw new Error(`${label} existing ancestor is not a directory`);
    }
    return {
      canonicalParent: canonical,
      canonicalPath: resolve(canonical, ...missing),
      missing,
    };
  }
}

function prepareReportOutputDirectory(outputPath, artifactPath, repositories) {
  const volangTarget = resolve(root, 'target');
  const canonicalArtifact = canonicalTreeRoot(artifactPath, 'Quickplay artifact directory');
  const canonicalTarget = ensureSingleRealDirectory(volangTarget, 'Volang target directory');
  const canonicalRepositories = repositories.map((repository) => ({
    ...repository,
    root: canonicalTreeRoot(repository.root, `${repository.name} source repository`),
  }));
  const plan = planCanonicalDirectoryCreation(outputPath, 'quickplay source audit output directory');
  assertSafeReportOutputLocation(plan.canonicalPath, canonicalArtifact, canonicalRepositories, canonicalTarget);
  let current = plan.canonicalParent;
  for (const component of plan.missing) {
    const next = join(current, component);
    current = ensureSingleRealDirectory(next, `quickplay source audit output component ${component}`);
    assertSafeReportOutputLocation(current, canonicalArtifact, canonicalRepositories, canonicalTarget);
  }
  if (current !== plan.canonicalPath) {
    throw new Error('quickplay source audit output directory changed while it was created');
  }
  return canonicalTreeRoot(current, 'quickplay source audit output directory');
}

function isBoundedPortableRelativePath(packagedPath) {
  if (typeof packagedPath !== 'string' || packagedPath.length === 0) return false;
  return !packagedPath.includes('\0')
    && !packagedPath.includes('\\')
    && !packagedPath.startsWith('/')
    && !packagedPath.split('/').some((component) => component === '' || component === '.' || component === '..')
    && Buffer.byteLength(packagedPath, 'utf8') <= SOURCE_AUDIT_LIMITS.maxPathBytes;
}

function resolveContainedSourcePath(repoRoot, packagedPath, label) {
  if (!isBoundedPortableRelativePath(packagedPath)) {
    throw new Error(`${label} is not a bounded portable relative path`);
  }
  const canonicalRoot = resolve(repoRoot);
  const absolute = resolve(canonicalRoot, ...packagedPath.split('/'));
  assertContainedPath(canonicalRoot, absolute, label);
  let current = canonicalRoot;
  const components = relative(canonicalRoot, absolute).split(sep).filter(Boolean);
  for (const component of components.slice(0, -1)) {
    current = join(current, component);
    const metadata = lstatSync(current, { bigint: true });
    if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
      throw new Error(`${label} has a non-directory or symbolic-link ancestor at ${current}`);
    }
  }
  return absolute;
}

function readDirectoryNamesStable(path, label, relativePath, before, budget) {
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const directoryOnly = typeof fsConstants.O_DIRECTORY === 'number' ? fsConstants.O_DIRECTORY : 0;
  const fd = openSync(path, fsConstants.O_RDONLY | noFollow | directoryOnly);
  const names = [];
  let directory;
  try {
    const opened = fstatSync(fd, { bigint: true });
    if (!opened.isDirectory() || !sameStat(before, opened)) {
      throw new Error(`${label} changed while it was opened`);
    }
    directory = opendirSync(path, { encoding: 'buffer' });
    while (true) {
      const entry = directory.readSync();
      if (entry === null) break;
      if (!Buffer.isBuffer(entry.name)) {
        throw new Error(`${label} did not return a directory entry name as bytes`);
      }
      const name = decodeUtf8(entry.name, `directory entry name in ${label}`);
      if (name === '.' || name === '..' || name.includes('/') || name.includes('\0')) {
        throw new Error(`${label} contains an invalid directory entry name`);
      }
      budget.addEntry(relativePath ? `${relativePath}/${name}` : name);
      names.push(name);
    }
    directory.closeSync();
    directory = null;
    const openedAfter = fstatSync(fd, { bigint: true });
    const pathAfter = lstatSync(path, { bigint: true });
    if (!sameStat(before, openedAfter) || !sameStat(before, pathAfter)) {
      throw new Error(`${label} changed while it was enumerated`);
    }
  } finally {
    if (directory) directory.closeSync();
    closeSync(fd);
  }
  names.sort(compareUtf8);
  return names;
}

function walkBoundedTree(treeRoot, {
  includeFile = () => true,
  label = 'directory tree',
  limits: limitOverrides = {},
  skipDirectory = () => false,
} = {}) {
  const limits = mergedTreeLimits(limitOverrides);
  const canonicalRoot = canonicalTreeRoot(treeRoot, label);
  const budget = new TreeBudget(limits, label);
  const contentHash = createHash('sha256');
  const stabilityHash = createHash('sha256');
  const files = [];
  const visitedDirectories = new Set();

  const visit = (absolute, relativePath, depth) => {
    if (depth > limits.maxDepth) {
      throw new Error(`${label} exceeds the ${limits.maxDepth}-level directory depth limit at ${relativePath || '.'}`);
    }
    assertContainedPath(canonicalRoot, absolute, label);
    const before = lstatSync(absolute, { bigint: true });
    if (!before.isDirectory() || before.isSymbolicLink()) {
      throw new Error(`${label} contains a non-directory or symbolic-link traversal node at ${relativePath || '.'}`);
    }
    const directoryIdentity = `${before.dev}:${before.ino}`;
    if (visitedDirectories.has(directoryIdentity)) {
      throw new Error(`${label} contains a directory cycle at ${relativePath || '.'}`);
    }
    visitedDirectories.add(directoryIdentity);
    const display = relativePath || '.';
    contentHash.update(`D\0${display}\0${before.mode}\0`);
    stabilityHash.update(`D\0${display}\0${statFingerprint(before)}\0`);

    const names = readDirectoryNamesStable(absolute, `${label} directory ${display}`, relativePath, before, budget);
    for (const name of names) {
      const childAbsolute = join(absolute, name);
      assertContainedPath(canonicalRoot, childAbsolute, label);
      const childRelative = relativePath ? `${relativePath}/${name}` : name;
      const child = lstatSync(childAbsolute, { bigint: true });
      if (child.isSymbolicLink()) {
        throw new Error(`${label} contains forbidden symbolic link ${childRelative}; links may escape the root or form a cycle`);
      }
      if (child.isDirectory()) {
        if (skipDirectory(childRelative, name)) {
          contentHash.update(`X\0${childRelative}\0${child.mode}\0`);
          stabilityHash.update(`X\0${childRelative}\0${statFingerprint(child)}\0`);
          continue;
        }
        visit(childAbsolute, childRelative, depth + 1);
        continue;
      }
      if (!child.isFile()) {
        throw new Error(`${label} contains unsupported filesystem entry ${childRelative}`);
      }
      if (!includeFile(childRelative)) {
        continue;
      }
      const result = digestRegularFileLimited(
        childAbsolute,
        `${label} file ${childRelative}`,
        limits.maxFileBytes,
        budget,
      );
      if (result.fingerprint !== statFingerprint(child)) {
        throw new Error(`${label} file ${childRelative} changed before it was read`);
      }
      budget.addFile();
      contentHash.update(`F\0${childRelative}\0${child.mode}\0${result.size}\0${result.digest}\0`);
      stabilityHash.update(`F\0${childRelative}\0${result.fingerprint}\0`);
      files.push({ path: childRelative, digest: result.digest, size: result.size });
    }

    const after = lstatSync(absolute, { bigint: true });
    if (!sameStat(before, after)) {
      throw new Error(`${label} directory ${display} changed while it was traversed`);
    }
  };

  visit(canonicalRoot, '', 0);
  const snapshot = {
    digest: `sha256:${contentHash.digest('hex')}`,
    entryCount: budget.entryCount,
    fileCount: budget.fileCount,
    files,
    pathBytes: budget.pathBytes,
    root: canonicalRoot,
    stabilityDigest: `sha256:${stabilityHash.digest('hex')}`,
    totalBytes: budget.bytes,
  };
  rememberStableAuditTree(canonicalRoot, label, snapshot, {
    includeFile,
    label,
    limits: { ...limitOverrides },
    skipDirectory,
  });
  return snapshot;
}

function revalidateStableAuditTrees() {
  const fileTracking = stableAuditFileTrackingEnabled;
  const treeTracking = stableAuditTreeTrackingEnabled;
  stableAuditFileTrackingEnabled = false;
  stableAuditTreeTrackingEnabled = false;
  try {
    for (const record of stableAuditTrees.values()) {
      const found = walkBoundedTree(record.root, record.options);
      if (JSON.stringify(stableTreeSummary(found)) !== JSON.stringify(record.summary)) {
        throw new Error(`${record.label} changed after it was audited`);
      }
    }
  } finally {
    stableAuditFileTrackingEnabled = fileTracking;
    stableAuditTreeTrackingEnabled = treeTracking;
  }
}

function revalidateStableAuditInputs() {
  revalidateStableAuditTrees();
  revalidateStableAuditFiles();
}

function quickplayArtifactSnapshot(path, limitOverrides = {}) {
  const snapshot = walkBoundedTree(path, {
    label: 'Quickplay artifact snapshot',
    limits: limitOverrides,
  });
  return {
    digest: snapshot.digest,
    entryCount: snapshot.entryCount,
    fileCount: snapshot.fileCount,
    stabilityDigest: snapshot.stabilityDigest,
    totalBytes: snapshot.totalBytes,
  };
}

function assertArtifactSnapshotUnchanged(before, after) {
  if (
    before.digest !== after.digest
    || before.stabilityDigest !== after.stabilityDigest
    || before.entryCount !== after.entryCount
    || before.fileCount !== after.fileCount
    || before.totalBytes !== after.totalBytes
  ) {
    throw new Error('Quickplay artifact snapshot changed while the source audit was running');
  }
}

function fileBytes(file) {
  if (file?.content != null) return Buffer.from(file.content, 'utf8');
  if (file?.contentBase64 != null) return Buffer.from(file.contentBase64, 'base64');
  return null;
}

function lineInFile(filePath, needle) {
  if (!filePath || !existsSync(filePath)) return 1;
  try {
    const { bytes } = readRegularFileLimited(
      filePath,
      `diagnostic source ${filePath}`,
      SOURCE_AUDIT_LIMITS.maxDiagnosticBytes,
      null,
    );
    const source = decodeUtf8(bytes, `diagnostic source ${filePath}`);
    const index = source.indexOf(needle);
    if (index === -1) return 1;
    let line = 1;
    for (let offset = 0; offset < index; offset += 1) {
      if (source.charCodeAt(offset) === 0x0a) line += 1;
    }
    return line;
  } catch {
    return 1;
  }
}

function issue(issues, owner, subsystem, severity, message, evidence = {}) {
  const file = evidence.file ?? evidence.path ?? null;
  let line = evidence.line ?? 1;
  if (evidence.line === undefined && typeof file === 'string') {
    const diagnosticPath = resolve(root, file);
    const suffix = relative(root, diagnosticPath);
    if (!isAbsolute(suffix) && suffix !== '..' && !suffix.startsWith(`..${sep}`)) {
      line = lineInFile(diagnosticPath, evidence.needle ?? '');
    }
  }
  issues.push({
    owner,
    subsystem,
    severity,
    message,
    file,
    line,
    reason: evidence.reason ?? message,
    requiredFix: evidence.requiredFix ?? 'Update the current source/provenance so the gate can verify this contract from first-party evidence.',
    evidence,
  });
}

function indexVoplayProvenanceArtifacts(artifacts, issues) {
  const artifactsByKey = new Map();
  for (const artifact of artifacts ?? []) {
    let key;
    try {
      key = artifactKey(artifact);
    } catch (error) {
      issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', 'Quickplay voplay artifact has an invalid identity', {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: artifact?.name ?? artifact?.path ?? 'artifacts',
        found: artifact,
        reason: error.message,
        requiredFix: 'Regenerate quickplay so every provenance artifact records a valid kind, target, and name.',
      });
      continue;
    }
    if (artifactsByKey.has(key)) {
      issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', 'Quickplay voplay provenance contains a duplicate artifact identity', {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: artifact.name,
        found: artifact,
        requiredFix: 'Regenerate quickplay so each provenance artifact identity is unique.',
      });
      continue;
    }
    artifactsByKey.set(key, artifact);
  }
  return artifactsByKey;
}

function selftestArtifactIdentityDiagnostics() {
  const complete = {
    kind: 'extension-wasm',
    target: WASM_TARGET,
    name: 'voplay_island_bg.wasm',
  };
  for (const missing of ['kind', 'target', 'name']) {
    const artifact = { ...complete };
    delete artifact[missing];
    const issues = [];
    const indexed = indexVoplayProvenanceArtifacts([artifact], issues);
    const diagnostic = issues[0];
    if (
      indexed.size !== 0
      || issues.length !== 1
      || diagnostic.message !== 'Quickplay voplay artifact has an invalid identity'
      || !diagnostic.reason.includes(`artifact ${missing}`)
      || diagnostic.evidence.found !== artifact
    ) {
      throw new Error(`missing ${missing} did not produce the expected structured artifact diagnostic`);
    }
  }

  const duplicateIssues = [];
  const indexed = indexVoplayProvenanceArtifacts([complete, { ...complete }], duplicateIssues);
  if (
    indexed.size !== 1
    || duplicateIssues.length !== 1
    || duplicateIssues[0].message !== 'Quickplay voplay provenance contains a duplicate artifact identity'
  ) {
    throw new Error('duplicate artifact identities did not produce the expected structured diagnostic');
  }
  console.log('quickplay source audit artifact identity selftest: ok');
}

function selftestDependencyRepoAudit() {
  const repoRoot = mkdtempSync(join(tmpdir(), 'quickplay-source-audit-repo-selftest-'));
  const moduleName = 'selftest.example/module';
  try {
    const source = 'package fixture\n';
    writeFileSync(join(repoRoot, 'main.vo'), source);
    execFileSync('git', ['init', '-q'], { cwd: repoRoot });
    execFileSync('git', ['add', 'main.vo'], { cwd: repoRoot });
    execFileSync('git', [
      '-c',
      'user.name=Quickplay Audit Test',
      '-c',
      'user.email=quickplay-audit@example.invalid',
      'commit',
      '-qm',
      'fixture',
    ], { cwd: repoRoot });
    const commit = git(['rev-parse', 'HEAD'], repoRoot);
    const bytes = Buffer.from(source, 'utf8');
    const issues = [];
    dependencyRepos.set(moduleName, repoRoot);
    auditDependencyRepoCommit(
      {
        module: moduleName,
        files: [{ path: 'main.vo', content: source }],
      },
      { commit },
      { dirty: false },
      {
        source: [{ path: 'main.vo', size: bytes.byteLength, digest: sha256(bytes) }],
      },
      issues,
    );
    if (issues.length !== 0) {
      throw new Error(`clean dependency repo produced audit issues: ${JSON.stringify(issues)}`);
    }
    console.log('quickplay source audit dependency repo selftest: ok');
  } finally {
    dependencyRepos.delete(moduleName);
    rmSync(repoRoot, { recursive: true, force: true });
  }
}

function git(args, cwd) {
  return execFileSync('git', args, {
    cwd,
    encoding: 'utf8',
    env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' },
    maxBuffer: SOURCE_AUDIT_LIMITS.maxGitTextBytes,
    stdio: ['ignore', 'pipe', 'pipe'],
    timeout: 30_000,
  }).trim();
}

function tryGit(args, cwd) {
  try {
    return { ok: true, stdout: git(args, cwd) };
  } catch (error) {
    return {
      ok: false,
      error: String(error?.stderr || error?.message || error).trim(),
    };
  }
}

function tryGitBytes(args, cwd, expectedBytes, label) {
  let maxBuffer;
  try {
    maxBuffer = gitShowBudget.begin(label, expectedBytes);
  } catch (error) {
    return { ok: false, error: error.message };
  }
  try {
    const bytes = execFileSync('git', args, {
      cwd,
      env: { ...process.env, GIT_OPTIONAL_LOCKS: '0' },
      maxBuffer: Math.max(1, maxBuffer),
      stdio: ['ignore', 'pipe', 'pipe'],
      timeout: 30_000,
    });
    gitShowBudget.finish(label, bytes.byteLength);
    return { ok: true, bytes };
  } catch (error) {
    const captured = Buffer.isBuffer(error?.stdout) ? error.stdout.byteLength : 0;
    let budgetError = '';
    try {
      gitShowBudget.finish(label, captured);
    } catch (inner) {
      budgetError = `; ${inner.message}`;
    }
    return {
      ok: false,
      error: `${String(error?.stderr || error?.message || error).trim()}${budgetError}`,
    };
  }
}

function repositoryStateSnapshot(repositories) {
  return repositories.map(({ name, root: repoRoot }) => {
    const head = git(['rev-parse', 'HEAD'], repoRoot);
    const status = git(['status', '--porcelain=v1', '--untracked-files=all'], repoRoot);
    const confirmedHead = git(['rev-parse', 'HEAD'], repoRoot);
    const confirmedStatus = git(['status', '--porcelain=v1', '--untracked-files=all'], repoRoot);
    if (head !== confirmedHead || status !== confirmedStatus) {
      throw new Error(`${name} repository changed while its state snapshot was captured`);
    }
    return {
      name,
      root: repoRoot,
      head,
      dirty: status.length > 0,
      statusDigest: sha256(Buffer.from(status, 'utf8')),
    };
  });
}

function assertRepositoryStatesUnchanged(before, after) {
  if (JSON.stringify(before) !== JSON.stringify(after)) {
    throw new Error('one or more source repositories changed while the Quickplay source audit was running');
  }
}

function auditProjectLockGraph(projectFiles, deps, issues) {
  const modBytes = fileBytes(projectFiles.get('vo.mod'));
  const lockBytes = fileBytes(projectFiles.get('vo.lock'));
  if (!modBytes || !lockBytes) {
    issue(issues, 'BlockKart', 'Lock', 'P0', 'Quickplay project must embed vo.mod and vo.lock bytes', {
      file: 'apps/studio/public/quickplay/blockkart/project.json',
      expected: ['vo.mod', 'vo.lock'],
      requiredFix: 'Regenerate Quickplay with the complete project module contract and canonical v2 lock.',
    });
    return new Map();
  }
  try {
    const modContract = parseVoModRootContract(
      decodeModuleTextUtf8(modBytes, 'quickplay project vo.mod'),
      'quickplay project vo.mod',
    );
    const lock = parseVoLockV2(
      decodeModuleTextUtf8(lockBytes, 'quickplay project vo.lock'),
      'quickplay project vo.lock',
    );
    validateVoLockV2RootGraph(lock, modContract, 'quickplay project vo.lock');
    validatePackagedModuleSet(lock, deps.modules, 'quickplay deps.modules');
    return new Map(lock.resolved.map((entry) => [entry.path, entry]));
  } catch (error) {
    issue(issues, 'BlockKart', 'Lock', 'P0', 'Quickplay project lock graph is invalid or incomplete', {
      file: 'apps/studio/public/quickplay/blockkart/project.json',
      needle: 'vo.lock',
      reason: error.message,
      requiredFix: 'Regenerate Quickplay from a canonical v2 vo.lock whose root and complete dependency closure match vo.mod and deps.json.',
    });
    return new Map();
  }
}

function sourceSetDigest(entries) {
  const canonical = entries
    .map((entry) => ({ path: entry.path, size: entry.size, digest: entry.digest }))
    .sort((a, b) => compareUtf8(a.path, b.path));
  return sha256(Buffer.from(JSON.stringify(canonical), 'utf8'));
}

function packagedFilesDigest(files) {
  const entries = (files ?? [])
    .map((file) => {
      const bytes = fileBytes(file);
      return {
        digest: sha256(bytes),
        path: file.path,
        size: bytes.byteLength,
      };
    })
    .sort((a, b) => compareUtf8(a.path, b.path));
  return sourceSetDigest(entries);
}

function validatePackagedLockRewrite(project, sourceBytes, packagedBytes, issues) {
  const rewrite = project.lockRewrite;
  if (!rewrite || rewrite.schemaVersion !== 1 || rewrite.path !== 'vo.lock') {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'Packaged vo.lock differs from source without a structured lockRewrite contract', {
      expected: 'project.lockRewrite schemaVersion=1 path=vo.lock',
    });
    return false;
  }
  const sourceDigest = sha256(sourceBytes);
  const packagedDigest = sha256(packagedBytes);
  let ok = true;
  if (rewrite.sourceDigest !== sourceDigest) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite sourceDigest does not match source vo.lock', { expected: sourceDigest, found: rewrite.sourceDigest ?? null });
    ok = false;
  }
  if (rewrite.packagedDigest !== packagedDigest) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite packagedDigest does not match packaged vo.lock', { expected: packagedDigest, found: rewrite.packagedDigest ?? null });
    ok = false;
  }
  if (!Array.isArray(rewrite.modules) || rewrite.modules.length === 0) {
    issue(issues, 'BlockKart', 'LockRewrite', 'P0', 'lockRewrite must list rewritten dependency modules', { modules: rewrite.modules ?? null });
    ok = false;
  }
  return ok;
}

function listSourceFiles(projectRoot, extension) {
  const ignoredDirectories = new Set(['.git', 'target', 'node_modules', 'tmp_checks']);
  const traversal = walkBoundedTree(projectRoot, {
    includeFile: (path) => path.endsWith(extension),
    label: `source tree ${projectRoot}`,
    skipDirectory: (_path, name) => ignoredDirectories.has(name),
  });
  return traversal.files.sort((a, b) => compareUtf8(a.path, b.path));
}

function shouldValidateEmbeddedSource(file) {
  if (file.content == null) return false;
  return !['.vo-source-digest', '.vo-version', 'vo.release.json', 'vo.web.json'].includes(file.path);
}

function dropNativeExtensionTables(voModSource) {
  const lines = voModSource.split(/\r?\n/);
  const out = [];
  let skip = false;
  for (const line of lines) {
    const table = line.trim().match(/^\[\[?([^\]]+)\]\]?$/);
    if (table) {
      const name = table[1].trim();
      skip = name === 'extension.native' || name.startsWith('extension.native.');
    }
    if (!skip) {
      out.push(line);
    }
  }
  return `${out.join('\n').replace(/\n{3,}/g, '\n\n').trimEnd()}\n`;
}

function repoSourceBytesForAudit(mod, files, entry, sourceBytes) {
  if (entry.path !== 'vo.mod') {
    return sourceBytes;
  }
  const packaged = files.get('vo.mod');
  const packagedBytes = fileBytes(packaged);
  if (!packagedBytes) {
    return sourceBytes;
  }
  const rewritten = Buffer.from(
    dropNativeExtensionTables(decodeModuleTextUtf8(sourceBytes, `${mod.module} source vo.mod`)),
    'utf8',
  );
  decodeModuleTextUtf8(packagedBytes, `${mod.module} packaged vo.mod`);
  if (rewritten.byteLength === packagedBytes.byteLength && sha256(rewritten) === sha256(packagedBytes)) {
    return rewritten;
  }
  return sourceBytes;
}

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

function entryMap(entries) {
  return new Map((entries ?? []).map((entry) => [entry.path, entry]));
}

function sourceDigestEntry(relative) {
  let absolute;
  try {
    absolute = resolveContainedSourcePath(
      blockKartRoot,
      relative,
      `BlockKart producer input ${relative}`,
    );
  } catch (error) {
    return { path: relative, invalid: true, reason: error.message };
  }
  if (!existsSync(absolute)) return { path: relative, missing: true };
  const result = digestRegularFileLimited(
    absolute,
    `BlockKart producer input ${relative}`,
    SOURCE_AUDIT_LIMITS.maxFileBytes,
    auditReadBudget,
  );
  return { path: relative, digest: result.digest, size: result.size };
}

function validateDigestEntries(entries, requiredPaths, issues, owner, subsystem, producerLabel, requiredFix) {
  const entriesByPath = entryMap(entries);
  for (const requiredPath of requiredPaths) {
    const actual = entriesByPath.get(requiredPath);
    if (!actual) {
      issue(issues, owner, subsystem, 'P0', `${producerLabel} missing producer digest entry for ${requiredPath}`, {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: '"producers"',
        requiredPath,
        requiredFix,
      });
      continue;
    }
    if (requiredPath.startsWith('workspace:')) {
      continue;
    }
    const expected = sourceDigestEntry(requiredPath);
    if (expected.missing || actual.digest !== expected.digest || actual.size !== expected.size) {
      issue(issues, owner, subsystem, 'P0', `${producerLabel} producer digest entry is stale for ${requiredPath}`, {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: requiredPath,
        requiredPath,
        expected,
        found: actual,
        requiredFix,
      });
    }
  }
}

function validateBlockKartProducerProvenance(project, provenance, packaged, issues) {
  try {
    execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
      cwd: blockKartRoot,
      env: process.env,
      maxBuffer: SOURCE_AUDIT_LIMITS.maxGitTextBytes,
      stdio: 'pipe',
      timeout: 30_000,
    });
  } catch (error) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Current BlockKart vpak failed canonical manifest verification', {
      file: 'assets/blockkart.vpak.provenance.json',
      needle: '"producerDigest"',
      detail: String(error?.stderr || error?.message || error),
      requiredFix: 'Rebuild assets/blockkart.vpak and its canonical producer manifest before quickplay packaging.',
    });
  }
  const packagedVpak = packaged.get(requiredVpakProducerOutput);
  const packagedVpakBytes = fileBytes(packagedVpak);
  if (!packagedVpakBytes) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Quickplay package must include assets/blockkart.vpak bytes', {
      file: 'apps/studio/public/quickplay/blockkart/project.json',
      needle: requiredVpakProducerOutput,
      requiredFix: 'Regenerate the quickplay package with the BlockKart runtime vpak embedded.',
    });
    return;
  }
  const packagedManifestBytes = fileBytes(packaged.get(requiredVpakProducerManifest));
  const currentManifestPath = join(blockKartRoot, requiredVpakProducerManifest);
  let currentManifestBytes = null;
  if (existsSync(currentManifestPath)) {
    try {
      currentManifestBytes = readRegularFileLimited(
        currentManifestPath,
        'BlockKart vpak producer manifest',
        SOURCE_AUDIT_LIMITS.maxMetadataJsonBytes,
      ).bytes;
    } catch (error) {
      issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Current BlockKart vpak producer manifest cannot be read safely', {
        file: requiredVpakProducerManifest,
        reason: error.message,
        requiredFix: 'Rebuild the canonical vpak producer manifest as a bounded regular JSON file.',
      });
    }
  }
  let canonicalManifest = null;
  if (!packagedManifestBytes || !currentManifestBytes) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Quickplay package must include the canonical vpak producer manifest', {
      file: 'apps/studio/public/quickplay/blockkart/project.json',
      needle: requiredVpakProducerManifest,
      requiredFix: 'Rebuild assets/blockkart.vpak and package its canonical producer manifest sidecar.',
    });
  } else if (sha256(packagedManifestBytes) !== sha256(currentManifestBytes)) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Packaged vpak producer manifest differs from current BlockKart source', {
      file: 'apps/studio/public/quickplay/blockkart/project.json',
      needle: requiredVpakProducerManifest,
      requiredFix: 'Regenerate quickplay from the current canonical vpak producer manifest.',
    });
  } else {
    try {
      canonicalManifest = parseJsonBytes(
        currentManifestBytes,
        'BlockKart vpak producer manifest',
        { ...SOURCE_AUDIT_LIMITS, maxJsonBytes: SOURCE_AUDIT_LIMITS.maxMetadataJsonBytes },
      );
    } catch (error) {
      issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Current BlockKart vpak producer manifest is invalid', {
        file: requiredVpakProducerManifest,
        reason: error.message,
        requiredFix: 'Regenerate the canonical vpak producer manifest as strict bounded UTF-8 JSON without duplicate keys.',
      });
    }
  }

  const producer = (provenance.producers ?? []).find((entry) => entry?.output === requiredVpakProducerOutput);
  if (!producer) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak must declare first-party producer provenance', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: '"outputs"',
      requiredFix: 'Regenerate quickplay provenance with a producer record for tools/pack_primitive_assets.vo, terrain generation inputs, output digests, and toolchain command.',
    });
    return;
  }
  let currentCliInputs;
  let currentCliToolchain;
  try {
    currentCliInputs = currentVoCliBuildInputs(root);
    currentCliToolchain = currentVoCliToolchain(root);
  } catch (error) {
    issue(issues, 'Volang/cmd/vo', 'ProducerProvenance', 'P0', 'Current Vo CLI producer input closure cannot be derived', {
      file: 'scripts/ci/quickplay_cli_producer_contract.mjs',
      reason: error.message,
      requiredFix: 'Restore a locked offline cmd/vo Cargo graph and rerun quickplay source audit.',
    });
  }
  const cliInputIssues = verifyVoCliBuildInputs(producer.voCliBuildInputs, {
    expected: currentCliInputs ?? null,
  });
  if (cliInputIssues.length > 0) {
    issue(issues, 'Volang/cmd/vo', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak Vo CLI producer input closure is missing or stale', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: 'voCliBuildInputs',
      found: producer.voCliBuildInputs ?? null,
      issues: cliInputIssues,
      requiredFix: 'Run task:quickplay-blockkart-package so the VPAK producer records the current locked Vo CLI Cargo/source closure.',
    });
  }
  const cliExecutionIssues = verifyVoCliExecutionIdentity(
    producer.toolchain,
    producer.voBinary,
    {
      buildInputs: producer.voCliBuildInputs,
      executionDigest: producer.voCliExecutionDigest,
      expectedToolchain: currentCliToolchain ?? null,
    },
  );
  if (cliExecutionIssues.length > 0) {
    issue(issues, 'Volang/cmd/vo', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak Vo CLI execution identity is missing or stale', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: 'voBinary',
      toolchain: producer.toolchain ?? null,
      voBinary: producer.voBinary ?? null,
      issues: cliExecutionIssues,
      requiredFix: 'Run task:quickplay-blockkart-package so the VPAK producer records the actual pinned Rust toolchain and stable Vo binary digest.',
    });
  }

  if (producer.owner !== 'BlockKart' || producer.kind !== 'vpak') {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak producer must name the BlockKart vpak owner contract', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      producer,
      requiredFix: 'Set producer.owner=BlockKart and producer.kind=vpak for the runtime asset pack producer.',
    });
  }
  if (JSON.stringify(producer.command) !== JSON.stringify(VO_CLI_GUEST_ENVIRONMENT.command)) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak producer command must match the authenticated Vo CLI guest command', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      command: producer.command ?? null,
      requiredFix: 'Record the exact authenticated pack command that generated assets/blockkart.vpak.',
    });
  }
  validateDigestEntries(
    producer.inputs,
    requiredVpakProducerInputPaths,
    issues,
    'BlockKart',
    'ProducerProvenance',
    'assets/blockkart.vpak',
    'Record current source digests for the vpak pack script and map manifest inputs.',
  );
  validateDigestEntries(
    producer.outputs,
    [requiredVpakProducerOutput],
    issues,
    'BlockKart',
    'ProducerProvenance',
    'assets/blockkart.vpak',
    'Record the current output digest for assets/blockkart.vpak.',
  );
  const producerOutput = entryMap(producer.outputs).get(requiredVpakProducerOutput);
  if (producerOutput?.digest !== sha256(packagedVpakBytes) || producerOutput?.size !== packagedVpakBytes.byteLength) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak producer output must match packaged bytes', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      expected: { digest: sha256(packagedVpakBytes), size: packagedVpakBytes.byteLength },
      found: producerOutput ?? null,
      requiredFix: 'Regenerate quickplay provenance from the current packaged vpak bytes.',
    });
  }
  if (canonicalManifest) {
    const expectedInputs = canonicalManifest.inputs
      .map((entry) => ({ path: entry.path, digest: `sha256:${entry.sha256}`, size: entry.size }))
      .sort((a, b) => compareUtf8(a.path, b.path));
    const foundInputs = [...(producer.inputs ?? [])].sort((a, b) => compareUtf8(a.path, b.path));
    const manifestFact = producer.producerManifest;
    const manifestMatches = manifestFact?.path === requiredVpakProducerManifest
      && manifestFact?.sha256 === sha256(currentManifestBytes)
      && manifestFact?.size === currentManifestBytes.byteLength
      && manifestFact?.producerDigest === canonicalManifest.producerDigest;
    if (JSON.stringify(foundInputs) !== JSON.stringify(expectedInputs)
      || producer.archiveEntryCount !== canonicalManifest.archiveEntryCount
      || producer.payloadInputCount !== canonicalManifest.payloadInputCount
      || producer.workspaceSourceInputCount !== canonicalManifest.workspaceSourceInputCount
      || JSON.stringify(producer.archiveEntries) !== JSON.stringify(canonicalManifest.archiveEntries)
      || !manifestMatches) {
      issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'Quickplay vpak producer record is not the canonical 37-entry manifest', {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: '"archiveEntries"',
        expectedArchiveEntryCount: canonicalManifest.archiveEntryCount,
        foundArchiveEntryCount: producer.archiveEntryCount ?? null,
        manifestMatches,
        requiredFix: 'Regenerate quickplay after rebuilding and checking the BlockKart canonical vpak producer manifest.',
      });
    }
  }

  const upstream = Array.isArray(producer.upstream) ? producer.upstream : [];
  const terrainProducer = upstream.find((entry) => entry?.id === 'primitive-terrain-assets');
  const paintProducer = upstream.find((entry) => entry?.id === 'painted-terrain-textures');
  if (!terrainProducer) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak provenance must include primitive terrain generator lineage', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      requiredFix: 'Record the tools/generate_primitive_terrain.mjs producer with terrain inputs and generated output digests.',
    });
  } else {
    validateDigestEntries(
      terrainProducer.inputs,
      requiredTerrainProducerInputs,
      issues,
      'BlockKart',
      'ProducerProvenance',
      'primitive-terrain-assets',
      'Record current terrain generator input digests.',
    );
    validateDigestEntries(
      terrainProducer.outputs,
      requiredTerrainProducerOutputs,
      issues,
      'BlockKart',
      'ProducerProvenance',
      'primitive-terrain-assets',
      'Record current generated terrain output digests.',
    );
  }
  if (!paintProducer) {
    issue(issues, 'BlockKart', 'ProducerProvenance', 'P0', 'assets/blockkart.vpak provenance must include painted terrain texture lineage', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: requiredVpakProducerOutput,
      requiredFix: 'Record the tools/paint_terrain_textures.mjs producer with source concept and baked texture output digests.',
    });
  } else {
    validateDigestEntries(
      paintProducer.inputs,
      requiredPaintProducerInputs,
      issues,
      'BlockKart',
      'ProducerProvenance',
      'painted-terrain-textures',
      'Record current painted texture producer input digests.',
    );
    validateDigestEntries(
      paintProducer.outputs,
      requiredPaintProducerOutputs,
      issues,
      'BlockKart',
      'ProducerProvenance',
      'painted-terrain-textures',
      'Record current painted texture output digests.',
    );
  }
}

function validateVoplayWasmProducerProvenance(provenance, issues) {
  const voplayRoot = dependencyRepos.get('github.com/vo-lang/voplay');
  const verification = verifyCurrentVoplayWasm({
    voplayRoot,
    outDir: voplayCurrentWasmRoot,
    expectedCiRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
  });
  if (verification.issues.length > 0) {
    const manifestMissing = verification.issues.includes('producer-manifest.json missing');
    issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', manifestMissing
      ? 'Current-source voplay WASM last-known-good output is missing'
      : 'Current-source voplay WASM producer evidence is stale', {
      file: 'target/voplay-current-wasm/producer-manifest.json',
      freshnessIssues: verification.issues,
      outputPolicy: 'transactional',
      expectedFailureBehavior: 'A failed preflight or build retains the previous verified payload and producer manifest as one directory generation.',
      lastKnownGood: verification.manifest
        ? {
            generatedAt: verification.manifest.generatedAt ?? null,
            sourceClosure: verification.manifest.sourceClosure ?? null,
            volangBuildInputs: verification.manifest.volangBuildInputs ?? null,
            ffiSourceFingerprint: verification.manifest.ffiSourceFingerprint ?? null,
            outputs: verification.manifest.outputs ?? null,
          }
        : null,
      requiredFix: manifestMissing
        ? 'Run the transactional voplay-current-wasm producer after its source preconditions are satisfied; only a fully verified staging generation may repopulate this output.'
        : 'Satisfy the voplay source preconditions, then replace the retained last-known-good output through the producer staged publish and recovery protocol in the same task run.',
    });
    return;
  }
  const producer = (provenance.producers ?? []).find((entry) => entry?.id === 'voplay-current-source-wasm');
  if (!producer) {
    issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', 'Quickplay provenance must include the current-source voplay WASM producer', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: '"producers"',
      requiredFix: 'Regenerate quickplay with the voplay-current-wasm-build producer output.',
    });
    return;
  }
  const producerKeysMatch = JSON.stringify(Object.keys(producer).sort()) === JSON.stringify([
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
  ].sort());
  const stableProducerMatches = producerKeysMatch
    && JSON.stringify(producer.command) === JSON.stringify(verification.manifest.command)
    && JSON.stringify(producer.sourceClosure) === JSON.stringify(verification.manifest.sourceClosure)
    && JSON.stringify(producer.volangBuildInputs) === JSON.stringify(verification.manifest.volangBuildInputs)
    && producer.ffiSourceFingerprint === verification.manifest.ffiSourceFingerprint
    && JSON.stringify(producer.toolchain) === JSON.stringify(verification.manifest.toolchain);
  const producerPlatformValid = typeof producer.buildPlatform?.os === 'string'
    && typeof producer.buildPlatform?.arch === 'string';
  const sameBuildPlatform = producerPlatformValid
    && JSON.stringify(producer.buildPlatform) === JSON.stringify(verification.manifest.buildPlatform);
  const outputNamesMatch = JSON.stringify((producer.outputs ?? []).map((output) => output.name))
    === JSON.stringify(verification.manifest.outputs.map((output) => output.name));
  if (!stableProducerMatches || !producerPlatformValid || !outputNamesMatch
      || (sameBuildPlatform && JSON.stringify(producer.outputs) !== JSON.stringify(verification.manifest.outputs))) {
    issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', 'Quickplay voplay WASM producer record does not match the current build manifest', {
      file: 'apps/studio/public/quickplay/blockkart/provenance.json',
      needle: 'voplay-current-source-wasm',
      expected: verification.manifest,
      found: producer,
      requiredFix: 'Regenerate quickplay after rebuilding voplay-current-wasm on the package producer platform.',
    });
  }
  if (stableProducerMatches && producerPlatformValid && outputNamesMatch && !sameBuildPlatform) {
    console.log(`quickplay source audit: verified cross-platform voplay build ${producer.buildPlatform.os}/${producer.buildPlatform.arch} -> ${verification.manifest.buildPlatform.os}/${verification.manifest.buildPlatform.arch}`);
  }
  const dependency = (provenance.dependencies ?? []).find((entry) => entry?.module === 'github.com/vo-lang/voplay');
  const artifactsByKey = indexVoplayProvenanceArtifacts(dependency?.artifacts, issues);
  for (const output of producer.outputs ?? []) {
    const kind = output.name.endsWith('.js') ? 'extension-js-glue' : 'extension-wasm';
    const packaged = artifactsByKey.get(artifactKey({ kind, target: WASM_TARGET, name: output.name }));
    if (!packaged || packaged.digest !== output.digest || packaged.size !== output.size) {
      issue(issues, 'voplay/rust', 'ProducerProvenance', 'P0', `Packaged ${output.name} does not match its producer output`, {
        file: 'apps/studio/public/quickplay/blockkart/provenance.json',
        needle: output.name,
        expected: output,
        found: packaged ?? null,
        requiredFix: 'Regenerate quickplay from the recorded producer output.',
      });
    }
  }
}

function auditBlockKart(project, provenance, issues) {
  if (!existsSync(blockKartRoot)) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout is missing', { blockKartRoot });
    return;
  }
  if (provenance?.schemaVersion !== 3) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Quickplay provenance must use schema v3', { schemaVersion: provenance?.schemaVersion ?? null });
  }
  if (provenance?.project?.commit && provenance.project.commit !== project.commit) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance commit does not match project package', { expected: project.commit, found: provenance.project.commit });
  }
  if (provenance?.project?.filesDigest && provenance.project.filesDigest !== packagedFilesDigest(project.files)) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance filesDigest does not match project package files', { expected: packagedFilesDigest(project.files), found: provenance.project.filesDigest });
  }
  if (JSON.stringify(provenance?.project?.lockRewrite ?? null) !== JSON.stringify(project.lockRewrite ?? null)) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'Project provenance lockRewrite must match project package lockRewrite', {
      provenance: provenance?.project?.lockRewrite ?? null,
      project: project.lockRewrite ?? null,
    });
  }
  const inside = tryGit(['rev-parse', '--is-inside-work-tree'], blockKartRoot);
  if (!inside.ok || inside.stdout !== 'true') {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source path is not a git checkout', { blockKartRoot, error: inside.error });
    return;
  }
  const head = git(['rev-parse', 'HEAD'], blockKartRoot);
  if (head !== project.commit) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source HEAD does not match packaged commit', { expected: project.commit, found: head });
  }
  const status = git(['status', '--porcelain'], blockKartRoot);
  const dirty = status !== '';
  if (dirty) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout must be clean for strict quickplay source audit', { status });
  }
  if (dirty && provenance?.project?.dirty !== true) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart source checkout is dirty but provenance project.dirty is not true', { status, provenanceDirty: provenance?.project?.dirty ?? null });
  }
  if (!dirty && provenance?.project?.dirty === true) {
    issue(issues, 'BlockKart', 'Source', 'P0', 'BlockKart provenance project.dirty is true but source checkout is clean');
  }
  if (provenance?.project?.dirty === true) {
    issue(issues, 'BlockKart', 'Provenance', 'P0', 'BlockKart provenance project.dirty must be false for strict source audit');
  }

  const packaged = new Map((project.files ?? []).map((file) => [file.path, file]));
  validateBlockKartProducerProvenance(project, provenance, packaged, issues);
  const sourceFiles = listSourceFiles(blockKartRoot, '.vo');
  const packagedVo = new Set([...packaged.keys()].filter((file) => file.endsWith('.vo')));
  const expectedSourceAllowlist = quickplayBlockKartSourceAllowlist(
    sourceFiles.map((entry) => entry.path),
  );
  if (JSON.stringify(project.sourceAllowlist ?? null) !== JSON.stringify(expectedSourceAllowlist)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay project sourceAllowlist does not match the authenticated generator policy', {
      expected: expectedSourceAllowlist,
      found: project.sourceAllowlist ?? null,
      requiredFix: 'Regenerate Quickplay so project.sourceAllowlist is derived from the fixed generator contract and current BlockKart source set.',
    });
  }
  if (JSON.stringify(provenance?.project?.sourceAllowlist ?? null) !== JSON.stringify(expectedSourceAllowlist)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay provenance sourceAllowlist does not match the authenticated generator policy', {
      expected: expectedSourceAllowlist,
      found: provenance?.project?.sourceAllowlist ?? null,
      requiredFix: 'Regenerate Quickplay so provenance.project.sourceAllowlist exactly mirrors the fixed generator contract.',
    });
  }
  if (JSON.stringify(project.sourceAllowlist ?? null) !== JSON.stringify(provenance?.project?.sourceAllowlist ?? null)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay project and provenance sourceAllowlist records differ', {
      project: project.sourceAllowlist ?? null,
      provenance: provenance?.project?.sourceAllowlist ?? null,
      requiredFix: 'Regenerate the project and provenance manifests in one package generation.',
    });
  }
  const sourceAllowlist = new Map(expectedSourceAllowlist.map((entry) => [entry.path, entry]));
  const unpackaged = sourceFiles
    .filter((entry) => !packagedVo.has(entry.path))
    .filter((entry) => !sourceAllowlist.has(entry.path));
  if (unpackaged.length > 0) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'BlockKart source files are missing from quickplay package and have no allowlist reason', { unpackaged });
  }
  const invalidAllowlist = [...sourceAllowlist.values()].filter((entry) => (
    !entry?.path
    || typeof entry.reason !== 'string'
    || entry.reason.trim().length < 12
    || typeof entry.expiresAt !== 'string'
    || Number.isNaN(Date.parse(entry.expiresAt))
    || Date.parse(entry.expiresAt) <= Date.now()
    || !sourceFiles.some((source) => source.path === entry.path)
  ));
  if (invalidAllowlist.length > 0) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'BlockKart quickplay source allowlist entries must name an existing source file and include a reason plus future expiresAt', { invalidAllowlist });
  }
  const manifestSourceFiles = project.sourceFiles ?? provenance?.project?.sourceFiles ?? null;
  if (!Array.isArray(manifestSourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay package manifest must record BlockKart source file digest list', { expected: 'project.sourceFiles[] or provenance.project.sourceFiles[]' });
  } else if (sourceSetDigest(manifestSourceFiles) !== sourceSetDigest(sourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay package source digest list does not match BlockKart/**/*.vo', {
      expected: sourceSetDigest(sourceFiles),
      found: sourceSetDigest(manifestSourceFiles),
    });
  }
  if (provenance?.project?.sourceFilesDigest !== sourceSetDigest(sourceFiles)) {
    issue(issues, 'BlockKart', 'SourceCoverage', 'P0', 'Quickplay provenance sourceFilesDigest does not match BlockKart/**/*.vo', {
      expected: sourceSetDigest(sourceFiles),
      found: provenance?.project?.sourceFilesDigest ?? null,
    });
  }
  for (const [path, file] of packaged) {
    const packagedBytes = fileBytes(file);
    if (!packagedBytes) {
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file has no bytes', { path });
      continue;
    }
    let source;
    try {
      const sourcePath = resolveContainedSourcePath(blockKartRoot, path, `packaged BlockKart source path ${path}`);
      if (!existsSync(sourcePath)) {
        issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file is missing from source checkout', { path });
        continue;
      }
      source = readRegularFileLimited(
        sourcePath,
        `BlockKart source file ${path}`,
        SOURCE_AUDIT_LIMITS.maxFileBytes,
      ).bytes;
    } catch (error) {
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart source path cannot be read safely', {
        path,
        reason: error.message,
        requiredFix: 'Regenerate Quickplay with bounded portable source paths backed by regular files inside the BlockKart checkout.',
      });
      continue;
    }
    if (source.byteLength !== packagedBytes.byteLength || sha256(source) !== sha256(packagedBytes)) {
      if (path === 'vo.lock' && validatePackagedLockRewrite(project, source, packagedBytes, issues)) {
        continue;
      }
      issue(issues, 'BlockKart', 'Source', 'P0', 'Packaged BlockKart file differs from source checkout', {
        path,
        source: { digest: sha256(source), size: source.byteLength },
        packaged: { digest: sha256(packagedBytes), size: packagedBytes.byteLength },
      });
    }
  }
}

function auditDependencyRelease(mod, locked, provenanceDependency, issues) {
  const dependencyDirty = provenanceDependency?.dirty === true;
  if (dependencyDirty) {
    issue(issues, mod.module, 'Provenance', 'P0', 'Dependency provenance dirty flag must be false for strict source audit', { module: mod.module });
  }
  const files = new Map((mod.files ?? []).map((file) => [file.path, file]));
  const releaseFile = files.get('vo.release.json');
  const webFile = files.get('vo.web.json');
  if (!releaseFile) {
    issue(issues, mod.module, 'Release', 'P0', 'Dependency package is missing vo.release.json', { module: mod.module, version: mod.version });
    return null;
  }
  if (!webFile) {
    issue(issues, mod.module, 'Release', 'P0', 'Dependency package is missing vo.web.json', { module: mod.module, version: mod.version });
    return null;
  }

  const releaseBytes = fileBytes(releaseFile);
  if (!releaseBytes) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json has no bytes');
    return null;
  }
  const releaseDigest = sha256(releaseBytes);
  if (releaseDigest !== locked.release_manifest) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json digest does not match project vo.lock', { expected: locked.release_manifest, found: releaseDigest });
    return null;
  }

  let release;
  try {
    release = parseJsonBytes(
      releaseBytes,
      `${mod.module} vo.release.json`,
      { ...SOURCE_AUDIT_LIMITS, maxJsonBytes: SOURCE_AUDIT_LIMITS.maxMetadataJsonBytes },
    );
  } catch (error) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json is invalid bounded strict JSON', { error: error.message });
    return null;
  }
  if (!release || typeof release !== 'object' || Array.isArray(release)) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json must contain a JSON object');
    return null;
  }
  if (release.schema_version !== 1) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json schema_version mismatch', { expected: 1, found: release.schema_version ?? null });
  if (release.module !== mod.module) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json module mismatch', { expected: mod.module, found: release.module ?? null });
  if (release.version !== mod.version) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json version mismatch', { expected: mod.version, found: release.version ?? null });
  if (locked.commit && release.commit !== locked.commit) issue(issues, mod.module, 'Release', 'P0', 'vo.release.json commit mismatch', { expected: locked.commit, found: release.commit ?? null });
  if (locked.source && release.source?.digest !== locked.source) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json source digest does not match project vo.lock', { expected: locked.source, found: release.source?.digest ?? null });
  }
  const normalizeEdges = (value) => {
    if (!Array.isArray(value)) return null;
    const edges = [];
    for (const entry of value) {
      if (
        !entry
        || typeof entry !== 'object'
        || Array.isArray(entry)
        || typeof entry.module !== 'string'
        || typeof entry.constraint !== 'string'
      ) {
        return null;
      }
      edges.push({ module: entry.module, constraint: entry.constraint });
    }
    edges.sort((left, right) => compareUtf8(left.module, right.module));
    for (let index = 1; index < edges.length; index += 1) {
      if (edges[index - 1].module === edges[index].module) return null;
    }
    return edges;
  };
  const releaseEdges = normalizeEdges(release.require);
  const lockedEdges = normalizeEdges(locked.deps);
  if (!releaseEdges || !lockedEdges || JSON.stringify(releaseEdges) !== JSON.stringify(lockedEdges)) {
    issue(issues, mod.module, 'Release', 'P0', 'Project vo.lock dependency edges do not match vo.release.json require', {
      expected: releaseEdges,
      found: locked.deps ?? null,
    });
  }

  const webBytes = fileBytes(webFile);
  if (!webBytes) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json has no bytes');
    return null;
  }
  if (
    !release.web_manifest
    || typeof release.web_manifest !== 'object'
    || Array.isArray(release.web_manifest)
    || !Number.isSafeInteger(release.web_manifest.size)
    || release.web_manifest.size < 0
    || !/^sha256:[0-9a-f]{64}$/.test(release.web_manifest.digest ?? '')
  ) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json web_manifest metadata is invalid', {
      path: join(quickplayDir, 'deps.json'),
      needle: mod.module,
      expected: {
        size: 'non-negative safe integer',
        digest: 'sha256:<64 lowercase hex digits>',
      },
      found: release.web_manifest ?? null,
      requiredFix: 'Regenerate Quickplay from the locked module snapshots so the package carries a current release contract bound to vo.web.json.',
    });
    return null;
  }
  let webBindingValid = true;
  if (release.web_manifest.size !== webBytes.byteLength) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json web_manifest size does not match vo.web.json', { expected: webBytes.byteLength, found: release.web_manifest.size });
    webBindingValid = false;
  }
  const webDigest = sha256(webBytes);
  if (release.web_manifest.digest !== webDigest) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json web_manifest digest does not match vo.web.json', { expected: webDigest, found: release.web_manifest.digest });
    webBindingValid = false;
  }
  if (!webBindingValid) return null;

  let web;
  try {
    web = parseJsonBytes(
      webBytes,
      `${mod.module} vo.web.json`,
      { ...SOURCE_AUDIT_LIMITS, maxJsonBytes: SOURCE_AUDIT_LIMITS.maxMetadataJsonBytes },
    );
  } catch (error) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json is invalid bounded strict JSON', { error: error.message });
    return null;
  }
  if (!web || typeof web !== 'object' || Array.isArray(web)) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json must contain a JSON object');
    return null;
  }

  if (web.schema_version !== 1) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json schema_version mismatch', { expected: 1, found: web.schema_version ?? null });
  if (web.module !== mod.module) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json module mismatch', { expected: mod.module, found: web.module ?? null });
  if (web.version !== mod.version) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json version mismatch', { expected: mod.version, found: web.version ?? null });
  if (locked.commit && web.commit !== locked.commit) issue(issues, mod.module, 'Release', 'P0', 'vo.web.json commit mismatch', { expected: locked.commit, found: web.commit ?? null });
  const webEdges = normalizeEdges(web.require);
  if (!releaseEdges || !webEdges || JSON.stringify(releaseEdges) !== JSON.stringify(webEdges)) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json dependency edges do not match vo.release.json require', {
      expected: releaseEdges,
      found: web.require ?? null,
    });
  }
  let sourceBytes = 0;
  const invalidSourceEntries = [];
  const sourcePaths = new Set();
  if (!Array.isArray(web.source)) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json source is not an array');
    return null;
  }
  if (web.source.length > SOURCE_AUDIT_LIMITS.maxGitShowCalls) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json source exceeds the bounded repository audit file count', {
      expectedAtMost: SOURCE_AUDIT_LIMITS.maxGitShowCalls,
      found: web.source.length,
    });
    return null;
  }
  for (const entry of web.source) {
    const valid = entry
      && typeof entry === 'object'
      && !Array.isArray(entry)
      && isBoundedPortableRelativePath(entry.path)
      && Number.isSafeInteger(entry.size)
      && entry.size >= 0
      && entry.size <= SOURCE_AUDIT_LIMITS.maxGitShowFileBytes
      && /^sha256:[0-9a-f]{64}$/.test(entry.digest ?? '')
      && !sourcePaths.has(entry.path)
      && sourceBytes <= SOURCE_AUDIT_LIMITS.maxGitShowBytes - entry.size;
    if (!valid) {
      if (invalidSourceEntries.length < 20) invalidSourceEntries.push(entry);
      continue;
    }
    sourcePaths.add(entry.path);
    sourceBytes += entry.size;
  }
  if (invalidSourceEntries.length > 0 || sourcePaths.size !== web.source.length) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json source entries exceed path, identity, or byte budgets', {
      invalidSourceEntries,
      maxFileBytes: SOURCE_AUDIT_LIMITS.maxGitShowFileBytes,
      maxTotalBytes: SOURCE_AUDIT_LIMITS.maxGitShowBytes,
    });
    return null;
  }
  if (sourceSetDigest(web.source) !== web.source_digest) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.web.json source_digest mismatch', { expected: web.source_digest, found: sourceSetDigest(web.source) });
  }
  if (release.source?.files_digest !== web.source_digest) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json files_digest does not match vo.web.json source_digest', { expected: web.source_digest ?? null, found: release.source?.files_digest ?? null });
  }
  if (release.source?.files_size !== sourceBytes) {
    issue(issues, mod.module, 'Release', 'P0', 'vo.release.json files_size does not match vo.web.json source bytes', { expected: sourceBytes, found: release.source?.files_size ?? null });
  }
  const marker = files.get('.vo-source-digest')?.content?.trim() ?? '';
  if (locked.source && marker !== locked.source) {
    issue(issues, mod.module, 'Release', 'P0', '.vo-source-digest does not match project vo.lock', { expected: locked.source, found: marker });
  }

  const sourceByPath = new Map((web.source ?? []).map((entry) => [entry.path, entry]));
  for (const file of mod.files ?? []) {
    if (!shouldValidateEmbeddedSource(file)) continue;
    const entry = sourceByPath.get(file.path);
    if (!entry) {
      issue(issues, mod.module, 'Release', 'P0', 'Embedded source file is not declared by vo.web.json', { path: file.path });
      continue;
    }
    const bytes = fileBytes(file);
    const digest = sha256(bytes);
    if (bytes.byteLength !== entry.size || digest !== entry.digest) {
      issue(issues, mod.module, 'Release', 'P0', 'Embedded source file differs from vo.web.json source entry', {
        path: file.path,
        expected: { digest: entry.digest, size: entry.size },
        found: { digest, size: bytes.byteLength },
      });
    }
  }
  return web;
}

function auditDependencyRepoCommit(mod, locked, provenanceDependency, web, issues) {
  if (!locked?.commit) return;
  if (!web) return;
  const files = new Map((mod.files ?? []).map((file) => [file.path, file]));
  const repoRoot = dependencyRepos.get(mod.module);
  if (!repoRoot || !existsSync(repoRoot)) {
    issue(issues, mod.module, 'SourceRepo', 'P1', 'Dependency source repo checkout is missing', { repoRoot });
    return;
  }
  const head = tryGit(['rev-parse', 'HEAD'], repoRoot);
  const dirty = provenanceDependency?.dirty === true;
  if (dirty) {
    if (!head.ok || head.stdout !== locked.commit) {
      issue(issues, mod.module, 'SourceRepo', 'P0', 'Dirty dependency source repo HEAD does not match locked commit', { expected: locked.commit, found: head.ok ? head.stdout : null, repoRoot, error: head.error });
      return;
    }
  }
  const commitType = tryGit(['cat-file', '-t', locked.commit], repoRoot);
  if (!commitType.ok || commitType.stdout !== 'commit') {
    issue(issues, mod.module, 'SourceRepo', 'P1', 'Dependency source repo does not contain locked commit', { commit: locked.commit, repoRoot, error: commitType.error });
    return;
  }
  for (const entry of web.source ?? []) {
    let source;
    if (dirty) {
      try {
        const workingTreePath = resolveContainedSourcePath(
          repoRoot,
          entry.path,
          `${mod.module} working-tree source ${entry.path}`,
        );
        source = {
          ok: true,
          bytes: readRegularFileLimited(
            workingTreePath,
            `${mod.module} working-tree source ${entry.path}`,
            SOURCE_AUDIT_LIMITS.maxGitShowFileBytes,
          ).bytes,
        };
      } catch (error) {
        source = { ok: false, error: error.message };
      }
    } else {
      source = tryGitBytes(
        ['show', `${locked.commit}:${entry.path}`],
        repoRoot,
        entry.size,
        `${mod.module}@${locked.commit}:${entry.path}`,
      );
    }
    if (!source.ok) {
      issue(issues, mod.module, 'SourceRepo', 'P0', dirty
        ? 'Dirty dependency working tree is missing packaged source file'
        : 'Locked dependency commit is missing release source file', { commit: locked.commit, path: entry.path, error: source.error });
      continue;
    }
    const sourceBytes = source.bytes;
    const expectedBytes = repoSourceBytesForAudit(mod, files, entry, sourceBytes);
    if (expectedBytes.byteLength !== entry.size || sha256(expectedBytes) !== entry.digest) {
      issue(issues, mod.module, 'SourceRepo', 'P0', dirty
        ? 'Dirty dependency working-tree source differs from vo.web.json source entry'
        : 'Locked dependency commit source differs from vo.web.json source entry', {
        commit: locked.commit,
        path: entry.path,
        expected: { digest: entry.digest, size: entry.size },
        found: { digest: sha256(expectedBytes), size: expectedBytes.byteLength },
      });
    }
  }
}

function markdown(report) {
  const lines = ['# Quickplay Source Audit', '', `- Status: ${report.status}`, `- Issues: ${report.issues.length}`, `- Quickplay dir: ${report.quickplayDir}`, `- BlockKart root: ${report.blockKartRoot}`, ''];
  if (report.issues.length > 0) {
    lines.push('## Issues', '');
    for (const item of report.issues) {
      lines.push(`- ${item.severity} ${item.owner}/${item.subsystem}: ${item.message}`);
      const evidence = JSON.stringify(item.evidence ?? {});
      if (evidence !== '{}') lines.push(`  - evidence: \`${evidence}\``);
    }
    lines.push('');
  }
  return `${lines.join('\n')}\n`;
}

function publishReportsAtomically(outputDir, outputs) {
  mkdirSync(outputDir, { recursive: true });
  const canonicalOutputDir = canonicalTreeRoot(outputDir, 'quickplay source audit output directory');
  const prepared = outputs.map(({ name, content }) => {
    if (typeof name !== 'string' || name.length === 0 || name.includes('/') || name.includes('\\') || name.includes('\0')) {
      throw new Error(`invalid report output name ${JSON.stringify(name)}`);
    }
    const bytes = Buffer.from(content, 'utf8');
    if (bytes.byteLength > SOURCE_AUDIT_LIMITS.maxReportBytes) {
      throw new Error(`${name} exceeds the ${SOURCE_AUDIT_LIMITS.maxReportBytes}-byte report limit`);
    }
    return { bytes, name };
  });
  const staging = mkdtempSync(join(canonicalOutputDir, '.quickplay-source-audit-publish-'));
  try {
    for (const output of prepared) {
      const staged = join(staging, output.name);
      writeFileSync(staged, output.bytes, { flag: 'wx', mode: 0o644 });
      const metadata = lstatSync(staged, { bigint: true });
      if (!metadata.isFile() || metadata.isSymbolicLink() || metadata.size !== BigInt(output.bytes.byteLength)) {
        throw new Error(`staged report ${output.name} is not a stable regular file`);
      }
    }
    for (const output of prepared) {
      const destination = join(canonicalOutputDir, output.name);
      renameSync(join(staging, output.name), destination);
      const tracking = stableAuditFileTrackingEnabled;
      stableAuditFileTrackingEnabled = false;
      let published;
      try {
        published = readRegularFileLimited(
          destination,
          `published report ${output.name}`,
          SOURCE_AUDIT_LIMITS.maxReportBytes,
          null,
        ).bytes;
      } finally {
        stableAuditFileTrackingEnabled = tracking;
      }
      if (!published.equals(output.bytes)) {
        throw new Error(`published report ${output.name} differs from its staged bytes`);
      }
    }
  } finally {
    rmSync(staging, { recursive: true, force: true });
  }
}

function publishSourceAuditReport(outputDir, report) {
  publishReportsAtomically(outputDir, [
    { name: 'quickplay-source-audit.md', content: markdown(report) },
    // JSON is the authoritative machine-readable publication marker, so it is
    // renamed only after the companion Markdown report is fully in place.
    { name: 'quickplay-source-audit.json', content: `${JSON.stringify(report, null, 2)}\n` },
  ]);
}

function probeSourceAuditReportPublication(outputDir, report) {
  const probeDir = mkdtempSync(join(outputDir, '.quickplay-source-audit-probe-'));
  try {
    publishSourceAuditReport(probeDir, report);
  } finally {
    rmSync(probeDir, { recursive: true, force: true });
  }
}

function expectSelftestFailure(label, action, pattern) {
  try {
    action();
  } catch (error) {
    if (!pattern.test(error.message)) {
      throw new Error(`${label} failed with an unexpected diagnostic: ${error.message}`);
    }
    return;
  }
  throw new Error(`${label} unexpectedly succeeded`);
}

function selftestBoundaryLimits() {
  const fixtureRoot = mkdtempSync(join(tmpdir(), 'quickplay-source-audit-boundary-selftest-'));
  try {
    const jsonLimits = (overrides = {}) => ({ ...SOURCE_AUDIT_LIMITS, ...overrides });
    const oversizedJson = join(fixtureRoot, 'oversized.json');
    writeFileSync(oversizedJson, Buffer.alloc(65, 0x20));
    expectSelftestFailure(
      'oversized JSON',
      () => readJson(oversizedJson, 64, jsonLimits({ maxJsonBytes: 64 })),
      /64-byte limit/,
    );

    const invalidUtf8 = join(fixtureRoot, 'invalid-utf8.json');
    writeFileSync(invalidUtf8, Buffer.from([0x7b, 0x22, 0x78, 0x22, 0x3a, 0xff, 0x7d]));
    expectSelftestFailure(
      'invalid UTF-8 JSON',
      () => readJson(invalidUtf8),
      /not valid UTF-8/,
    );

    const duplicateJson = join(fixtureRoot, 'duplicate.json');
    writeFileSync(duplicateJson, '{"value":1,"value":2}\n');
    expectSelftestFailure(
      'duplicate-key JSON',
      () => readJson(duplicateJson),
      /duplicate object key "value"/,
    );

    const surrogateKeyJson = join(fixtureRoot, 'surrogate-key.json');
    writeFileSync(surrogateKeyJson, '{"\\ud800":1}\n');
    expectSelftestFailure(
      'isolated-surrogate JSON key',
      () => readJson(surrogateKeyJson),
      /isolated Unicode surrogate/,
    );

    const surrogateValueJson = join(fixtureRoot, 'surrogate-value.json');
    writeFileSync(surrogateValueJson, '{"nested":["\\udc00"]}\n');
    expectSelftestFailure(
      'isolated-surrogate JSON value',
      () => readJson(surrogateValueJson),
      /isolated Unicode surrogate/,
    );

    const scalarJson = join(fixtureRoot, 'scalar.json');
    writeFileSync(scalarJson, '{"scalar":"\\ud83d\\ude42"}\n');
    const scalar = readJson(scalarJson);
    if (scalar.scalar !== '🙂') {
      throw new Error('valid non-BMP JSON scalar did not roundtrip');
    }

    const nonFiniteJson = join(fixtureRoot, 'non-finite.json');
    writeFileSync(nonFiniteJson, '{"value":1e400}\n');
    expectSelftestFailure(
      'out-of-range JSON number',
      () => readJson(nonFiniteJson),
      /outside the finite f64 range/,
    );

    const deepJson = join(fixtureRoot, 'deep.json');
    writeFileSync(deepJson, '[[[]]]\n');
    expectSelftestFailure(
      'deep JSON',
      () => readJson(deepJson, 1024, jsonLimits({ maxJsonDepth: 2 })),
      /2-level JSON depth limit/,
    );

    const tokenJson = join(fixtureRoot, 'tokens.json');
    writeFileSync(tokenJson, '[0]\n');
    expectSelftestFailure(
      'JSON token budget',
      () => readJson(tokenJson, 1024, jsonLimits({ maxJsonTokens: 2 })),
      /2-token JSON limit/,
    );

    const collectionJson = join(fixtureRoot, 'collection.json');
    writeFileSync(collectionJson, '[0,1]\n');
    expectSelftestFailure(
      'JSON collection budget',
      () => readJson(collectionJson, 1024, jsonLimits({ maxJsonCollectionEntries: 1 })),
      /1-entry JSON collection limit/,
    );

    const excessiveEntries = join(fixtureRoot, 'excessive-entries');
    mkdirSync(excessiveEntries);
    for (const name of ['a', 'b', 'c']) writeFileSync(join(excessiveEntries, name), name);
    expectSelftestFailure(
      'directory entry budget',
      () => quickplayArtifactSnapshot(excessiveEntries, { maxEntries: 2 }),
      /2-entry limit/,
    );

    const deepTree = join(fixtureRoot, 'deep-tree');
    mkdirSync(join(deepTree, 'a', 'b', 'c'), { recursive: true });
    expectSelftestFailure(
      'directory depth budget',
      () => quickplayArtifactSnapshot(deepTree, { maxDepth: 2 }),
      /2-level directory depth limit/,
    );

    const oversizedTreeFile = join(fixtureRoot, 'oversized-tree-file');
    mkdirSync(oversizedTreeFile);
    writeFileSync(join(oversizedTreeFile, 'large.bin'), 'abc');
    expectSelftestFailure(
      'directory single-file budget',
      () => quickplayArtifactSnapshot(oversizedTreeFile, { maxFileBytes: 2 }),
      /2-byte limit/,
    );

    const excessiveTreeBytes = join(fixtureRoot, 'excessive-tree-bytes');
    mkdirSync(excessiveTreeBytes);
    writeFileSync(join(excessiveTreeBytes, 'a'), 'ab');
    writeFileSync(join(excessiveTreeBytes, 'b'), 'cd');
    expectSelftestFailure(
      'directory aggregate byte budget',
      () => quickplayArtifactSnapshot(excessiveTreeBytes, { maxTotalBytes: 3 }),
      /3-byte limit/,
    );

    const excessivePath = join(fixtureRoot, 'excessive-path');
    mkdirSync(excessivePath);
    writeFileSync(join(excessivePath, 'abcde'), 'x');
    expectSelftestFailure(
      'directory path budget',
      () => quickplayArtifactSnapshot(excessivePath, { maxPathBytes: 4 }),
      /4-byte path limit/,
    );

    const loopTree = join(fixtureRoot, 'loop-tree');
    mkdirSync(loopTree);
    symlinkSync('.', join(loopTree, 'again'));
    expectSelftestFailure(
      'symbolic-link loop',
      () => quickplayArtifactSnapshot(loopTree),
      /forbidden symbolic link/,
    );

    const outside = join(fixtureRoot, 'outside.txt');
    writeFileSync(outside, 'outside\n');
    const escapingTree = join(fixtureRoot, 'escaping-tree');
    mkdirSync(escapingTree);
    symlinkSync(outside, join(escapingTree, 'outside.txt'));
    expectSelftestFailure(
      'escaping symbolic link',
      () => quickplayArtifactSnapshot(escapingTree),
      /forbidden symbolic link/,
    );

    const changingTree = join(fixtureRoot, 'changing-tree');
    mkdirSync(changingTree);
    const changingFile = join(changingTree, 'project.json');
    writeFileSync(changingFile, '{"generation":1}\n');
    const before = quickplayArtifactSnapshot(changingTree);
    writeFileSync(changingFile, '{"generation":2}\n');
    const after = quickplayArtifactSnapshot(changingTree);
    expectSelftestFailure(
      'audit-time artifact mutation',
      () => assertArtifactSnapshotUnchanged(before, after),
      /changed while the source audit was running/,
    );

    const restoredSource = join(fixtureRoot, 'restored-source.vo');
    writeFileSync(restoredSource, 'package original\n');
    stableAuditFiles.clear();
    stableAuditFileTrackingEnabled = true;
    try {
      writeFileSync(restoredSource, 'package transient\n');
      readRegularFileLimited(restoredSource, 'transient source audit read', 1024);
      writeFileSync(restoredSource, 'package original\n');
      expectSelftestFailure(
        'mutate-read-restore source binding',
        () => revalidateStableAuditFiles(),
        /changed after it was audited/,
      );
    } finally {
      stableAuditFileTrackingEnabled = false;
      stableAuditFiles.clear();
    }

    const restoredTree = join(fixtureRoot, 'restored-tree');
    const restoredTreeFile = join(restoredTree, 'hidden.vo');
    const restoredTreeBackup = join(fixtureRoot, 'hidden.vo.backup');
    mkdirSync(restoredTree);
    writeFileSync(restoredTreeFile, 'package hidden\n');
    renameSync(restoredTreeFile, restoredTreeBackup);
    stableAuditTrees.clear();
    stableAuditTreeTrackingEnabled = true;
    try {
      walkBoundedTree(restoredTree, { label: 'transient source tree audit' });
      renameSync(restoredTreeBackup, restoredTreeFile);
      expectSelftestFailure(
        'remove-traverse-restore source tree binding',
        () => revalidateStableAuditTrees(),
        /changed after it was audited/,
      );
    } finally {
      stableAuditTreeTrackingEnabled = false;
      stableAuditTrees.clear();
      if (existsSync(restoredTreeBackup) && !existsSync(restoredTreeFile)) {
        renameSync(restoredTreeBackup, restoredTreeFile);
      }
    }

    const reportDir = join(fixtureRoot, 'reports');
    mkdirSync(reportDir);
    probeSourceAuditReportPublication(reportDir, {
      status: 'ok',
      issues: [],
      quickplayDir: changingTree,
      blockKartRoot: restoredTree,
    });
    if (
      existsSync(join(reportDir, 'quickplay-source-audit.json'))
      || existsSync(join(reportDir, 'quickplay-source-audit.md'))
    ) {
      throw new Error('private publication preflight exposed a formal report path');
    }
    publishReportsAtomically(reportDir, [
      { name: 'quickplay-source-audit.json', content: '{"status":"ok"}\n' },
      { name: 'quickplay-source-audit.md', content: '# ok\n' },
    ]);
    const published = readRegularFileLimited(
      join(reportDir, 'quickplay-source-audit.json'),
      'atomic report selftest output',
      1024,
      null,
    ).bytes.toString('utf8');
    if (published !== '{"status":"ok"}\n') {
      throw new Error('atomic report publication selftest produced unexpected bytes');
    }

    const protectedRepo = join(fixtureRoot, 'protected-repo');
    const protectedArtifact = join(fixtureRoot, 'protected-artifact');
    mkdirSync(protectedRepo);
    mkdirSync(protectedArtifact);
    expectSelftestFailure(
      'report output inside source repository',
      () => prepareReportOutputDirectory(
        join(protectedRepo, 'reports'),
        protectedArtifact,
        [{ name: 'fixture', root: protectedRepo }],
      ),
      /must not modify source repository fixture/,
    );
    expectSelftestFailure(
      'report output inside Quickplay artifact',
      () => prepareReportOutputDirectory(
        join(protectedArtifact, 'reports'),
        protectedArtifact,
        [],
      ),
      /must be outside the Quickplay artifact tree/,
    );
    const protectedRepoLink = join(fixtureRoot, 'protected-repo-link');
    symlinkSync(protectedRepo, protectedRepoLink);
    expectSelftestFailure(
      'report output through source-repository symlink ancestor',
      () => prepareReportOutputDirectory(
        join(protectedRepoLink, 'must-not-exist'),
        protectedArtifact,
        [{ name: 'fixture', root: protectedRepo }],
      ),
      /must not modify source repository fixture/,
    );
    if (existsSync(join(protectedRepo, 'must-not-exist'))) {
      throw new Error('unsafe report output validation created a directory through a symbolic-link ancestor');
    }

    const gitBudget = new GitShowBudget({
      ...SOURCE_AUDIT_LIMITS,
      maxGitShowCalls: 1,
      maxGitShowFileBytes: 2,
      maxGitShowBytes: 2,
    });
    const allowance = gitBudget.begin('selftest git object', 2);
    if (allowance !== 2) throw new Error(`unexpected git show allowance ${allowance}`);
    gitBudget.finish('selftest git object', 2);
    expectSelftestFailure(
      'git show call budget',
      () => gitBudget.begin('second git object', 0),
      /1-call limit/,
    );

    const expectedAllowlist = quickplayBlockKartSourceAllowlist([
      'runtimepack/runtime_pack.vo',
      'tools/pack_primitive_assets.vo',
    ]);
    if (
      expectedAllowlist.length !== 2
      || expectedAllowlist[0].path !== 'runtimepack/runtime_pack.vo'
      || expectedAllowlist[1].path !== 'tools/pack_primitive_assets.vo'
    ) {
      throw new Error('authenticated BlockKart source allowlist selftest failed');
    }
    console.log('quickplay source audit boundary selftest: ok');
  } finally {
    rmSync(fixtureRoot, { recursive: true, force: true });
  }
}

if (process.env.QUICKPLAY_SOURCE_AUDIT_BOUNDARY_SELFTEST === '1') {
  selftestBoundaryLimits();
  process.exit(0);
}

if (process.env.QUICKPLAY_SOURCE_AUDIT_CLI_PRODUCER_SELFTEST === '1') {
  const current = currentVoCliBuildInputs(root);
  const toolchain = currentVoCliToolchain(root);
  const binary = {
    path: toolchain.target.includes('windows')
      ? 'target/blockkart-vpak-build/vo.exe'
      : 'target/blockkart-vpak-build/vo',
    digest: `sha256:${'1'.repeat(64)}`,
    size: 1024,
  };
  const executionDigest = voCliExecutionDigest(current, toolchain, binary);
  if (verifyVoCliExecutionIdentity(toolchain, binary, {
    buildInputs: current,
    executionDigest,
    expectedToolchain: toolchain,
  }).length > 0) {
    throw new Error('quickplay source audit rejected a valid Vo CLI execution identity fixture');
  }
  const missing = structuredClone(current);
  missing.inputs.pop();
  const drifted = structuredClone(current);
  drifted.inputs[0].digest = `sha256:${'0'.repeat(64)}`;
  for (const [label, candidate] of [['missing', missing], ['drifted', drifted]]) {
    const found = verifyVoCliBuildInputs(candidate, { expected: current });
    if (found.length === 0) {
      throw new Error(`quickplay source audit accepted ${label} Vo CLI producer inputs`);
    }
  }
  if (
    verifyVoCliExecutionIdentity(undefined, binary, {
      buildInputs: current,
      executionDigest,
      expectedToolchain: toolchain,
    }).length === 0
    || verifyVoCliExecutionIdentity(toolchain, binary, {
      buildInputs: current,
      executionDigest: undefined,
      expectedToolchain: toolchain,
    }).length === 0
    || verifyVoCliExecutionIdentity(toolchain, {
      ...binary,
      digest: `sha256:${'0'.repeat(64)}`,
    }, {
      buildInputs: current,
      executionDigest,
      expectedToolchain: toolchain,
    }).length === 0
  ) {
    throw new Error('quickplay source audit accepted a missing or drifted Vo CLI execution identity');
  }
  console.log(
    'quickplay source audit Vo CLI producer selftest: ok '
    + '(2 source-lineage rejections, 3 execution-identity rejections)',
  );
  process.exit(0);
}

if (process.env.QUICKPLAY_SOURCE_AUDIT_ARTIFACT_SELFTEST === '1') {
  selftestArtifactIdentityDiagnostics();
  selftestDependencyRepoAudit();
  process.exit(0);
}

blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
dependencyRepos.set('github.com/vo-lang/vogui', requireRepoRoot('VOGUI_ROOT', 'vogui'));
dependencyRepos.set('github.com/vo-lang/vopack', requireRepoRoot('VOPACK_ROOT', 'vopack'));
dependencyRepos.set('github.com/vo-lang/voplay', requireRepoRoot('VOPLAY_ROOT', 'voplay'));

const sourceRepositories = [
  { name: 'volang', root },
  { name: 'BlockKart', root: blockKartRoot },
  ...[...dependencyRepos.entries()].map(([module, repoRoot]) => ({ name: module, root: repoRoot })),
];
const reportOutputDir = prepareReportOutputDirectory(outDir, quickplayDir, sourceRepositories);
stableAuditFileTrackingEnabled = true;
stableAuditTreeTrackingEnabled = true;
const repositoryStatesBefore = repositoryStateSnapshot(sourceRepositories);
const quickplaySnapshotBefore = quickplayArtifactSnapshot(quickplayDir);
const project = readJson(join(quickplayDir, 'project.json'));
const deps = readJson(join(quickplayDir, 'deps.json'));
const provenance = readJson(join(quickplayDir, 'provenance.json'));
const issues = [];
auditBlockKart(project, provenance, issues);
validateVoplayWasmProducerProvenance(provenance, issues);
const projectFiles = new Map((project.files ?? []).map((file) => [file.path, file]));
const locked = auditProjectLockGraph(projectFiles, deps, issues);
const provenanceDependencies = new Map((provenance.dependencies ?? []).map((entry) => [entry.module, entry]));
for (const mod of deps.modules ?? []) {
  const lockedModule = locked.get(mod.module);
  if (!lockedModule) {
    issue(issues, mod.module, 'Lock', 'P0', 'Dependency module is missing from project vo.lock', { version: mod.version });
    continue;
  }
  const provenanceDependency = provenanceDependencies.get(mod.module);
  const web = auditDependencyRelease(mod, lockedModule, provenanceDependency, issues);
  auditDependencyRepoCommit(mod, lockedModule, provenanceDependency, web, issues);
}

const generatedAt = new Date().toISOString();
const freshEvidence = sourceBoundEvidence({
  gate: 'quickplay-source-audit',
  generatedAt,
  root,
  repos: sourceRepositories,
  gateFiles: QUICKPLAY_SOURCE_AUDIT_GATE_FILES,
  artifacts: [quickplayDir],
});
if (freshEvidence.verdict.status !== 'pass') {
  issue(issues, 'Volang', 'FreshEvidence', 'P0', 'quickplay source audit freshEvidence verdict must pass', {
    file: 'scripts/ci/quickplay_source_audit.mjs',
    needle: 'sourceBoundEvidence',
    reason: 'Current source-bound report was produced from dirty or missing-commit repositories.',
    dirtyRepos: freshEvidence.verdict.dirtyRepos,
    missingCommitRepos: freshEvidence.verdict.missingCommitRepos,
    requiredFix: 'Regenerate the report from clean checked-out sources after all quickplay/provenance changes are committed.',
  });
}
const reportSourceFiles = listSourceFiles(blockKartRoot, '.vo');
const packagedSources = (project.files ?? [])
  .filter((file) => file.path.endsWith('.vo'))
  .map((file) => {
    const bytes = fileBytes(file);
    return {
      path: file.path,
      digest: sha256(bytes),
      size: bytes.byteLength,
    };
  })
  .sort((a, b) => compareUtf8(a.path, b.path));

try {
  revalidateStableAuditInputs();
} catch (error) {
  issue(issues, 'Volang', 'StableSnapshot', 'P0', 'An audited source input changed before report publication', {
    file: 'scripts/ci/quickplay_source_audit.mjs',
    needle: 'revalidateStableAuditInputs',
    reason: error.message,
    requiredFix: 'Rerun the audit while every source and artifact file remains byte-for-byte identical to the version read by the audit.',
  });
}

let repositoryStatesAfter = null;
try {
  repositoryStatesAfter = repositoryStateSnapshot(sourceRepositories);
  assertRepositoryStatesUnchanged(repositoryStatesBefore, repositoryStatesAfter);
} catch (error) {
  issue(issues, 'Volang', 'StableSnapshot', 'P0', 'Source repository state changed during the Quickplay source audit', {
    file: 'scripts/ci/quickplay_source_audit.mjs',
    needle: 'assertRepositoryStatesUnchanged',
    before: repositoryStatesBefore,
    after: repositoryStatesAfter,
    reason: error.message,
    requiredFix: 'Rerun the audit while Volang, BlockKart, and every dependency checkout remain unchanged.',
  });
}

let quickplaySnapshotAfter = null;
try {
  quickplaySnapshotAfter = quickplayArtifactSnapshot(quickplayDir);
  assertArtifactSnapshotUnchanged(quickplaySnapshotBefore, quickplaySnapshotAfter);
} catch (error) {
  issue(issues, 'studio/artifacts', 'StableSnapshot', 'P0', 'Quickplay artifact changed during the source audit', {
    file: 'scripts/ci/quickplay_source_audit.mjs',
    needle: 'assertArtifactSnapshotUnchanged',
    before: quickplaySnapshotBefore,
    after: quickplaySnapshotAfter,
    reason: error.message,
    requiredFix: 'Publish one complete Quickplay generation before auditing and keep it immutable until the report is atomically published.',
  });
}

const report = {
  schemaVersion: 1,
  kind: 'quickplay.sourceAuditReport',
  status: issues.length === 0 ? 'ok' : 'failed',
  generatedAt,
  freshEvidence,
  quickplayDir,
  outDir: reportOutputDir,
  blockKartRoot,
  checkedAt: generatedAt,
  artifactSnapshot: {
    before: quickplaySnapshotBefore,
    after: quickplaySnapshotAfter,
  },
  repositorySnapshots: {
    before: repositoryStatesBefore,
    after: repositoryStatesAfter,
  },
  sourceFiles: reportSourceFiles,
  packagedSources,
  packageManifest: {
    sourceFiles: project.sourceFiles ?? null,
    sourceAllowlist: project.sourceAllowlist ?? null,
    projectSourceAllowlist: project.sourceAllowlist ?? null,
    provenanceSourceAllowlist: provenance.project?.sourceAllowlist ?? null,
    expectedSourceAllowlist: quickplayBlockKartSourceAllowlist(
      reportSourceFiles.map((entry) => entry.path),
    ),
  },
  issues,
};

probeSourceAuditReportPublication(reportOutputDir, report);

let repositoryStatesPublished = null;
let quickplaySnapshotPublished = null;
try {
  revalidateStableAuditInputs();
} catch (error) {
  issue(issues, 'Volang', 'StableSnapshot', 'P0', 'An audited source input changed across the report publication preflight', {
    file: 'scripts/ci/quickplay_source_audit.mjs',
    needle: 'probeSourceAuditReportPublication',
    reason: error.message,
    requiredFix: 'Discard the report and rerun the audit from source files that remain immutable through atomic publication.',
  });
}
try {
  repositoryStatesPublished = repositoryStateSnapshot(sourceRepositories);
  assertRepositoryStatesUnchanged(repositoryStatesBefore, repositoryStatesPublished);
} catch (error) {
  issue(issues, 'Volang', 'StableSnapshot', 'P0', 'Source repository state changed across the report publication preflight', {
    file: 'scripts/ci/quickplay_source_audit.mjs',
    needle: 'probeSourceAuditReportPublication',
    before: repositoryStatesBefore,
    after: repositoryStatesPublished,
    reason: error.message,
    requiredFix: 'Use the Volang target tree or a directory outside all source repositories for reports, then rerun from stable checkouts.',
  });
}
try {
  quickplaySnapshotPublished = quickplayArtifactSnapshot(quickplayDir);
  assertArtifactSnapshotUnchanged(quickplaySnapshotBefore, quickplaySnapshotPublished);
} catch (error) {
  issue(issues, 'studio/artifacts', 'StableSnapshot', 'P0', 'Quickplay artifact changed across the report publication preflight', {
    file: 'scripts/ci/quickplay_source_audit.mjs',
    needle: 'probeSourceAuditReportPublication',
    before: quickplaySnapshotBefore,
    after: quickplaySnapshotPublished,
    reason: error.message,
    requiredFix: 'Keep the authenticated Quickplay generation immutable through report publication and rerun the audit.',
  });
}
report.repositorySnapshots.published = repositoryStatesPublished;
report.artifactSnapshot.published = quickplaySnapshotPublished;
report.status = issues.length === 0 ? 'ok' : 'failed';
publishSourceAuditReport(reportOutputDir, report);

if (issues.length > 0) {
  for (const item of issues) {
    console.error(JSON.stringify({
      owner: item.owner,
      subsystem: item.subsystem,
      severity: item.severity,
      message: item.message,
      evidence: item.evidence ?? null,
    }));
  }
  console.error(`quickplay source audit: failed with ${issues.length} issue(s); wrote ${reportOutputDir}`);
  process.exit(1);
}
console.log(`quickplay source audit: ok; wrote ${reportOutputDir}`);
