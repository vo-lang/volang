import { execFileSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  fstatSync,
  lstatSync,
  openSync,
  opendirSync,
  readlinkSync,
  readSync,
  realpathSync,
} from 'node:fs';
import path from 'node:path';
import {
  canonicalExistingDirectory,
  canonicalGitRepositoryRoot,
  cleanGitEnvironment,
} from './repo_roots.mjs';

export const SOURCE_BOUND_EVIDENCE_LIMITS = Object.freeze({
  maxRepositories: 64,
  maxInputPaths: 10_000,
  maxTreeEntries: 100_000,
  maxDirectoryObservationEntries: 100_000,
  maxFiles: 20_000,
  maxFileBytes: 256 * 1024 * 1024,
  maxTotalBytes: 512 * 1024 * 1024,
  maxPathBytes: 4 * 1024,
  maxMetadataBytes: 32 * 1024 * 1024,
  maxSymlinkBytes: 64 * 1024,
  maxDepth: 256,
  maxGitOutputBytes: 32 * 1024 * 1024,
  gitTimeoutMs: 30_000,
});

export const SOURCE_TREE_OUTPUT_DECLARATIONS = Object.freeze({
  'vogui.current-source-wasm': Object.freeze([
    'web-artifacts/vogui.wasm',
  ]),
  'voplay.current-source-wasm': Object.freeze([
    'web-artifacts/voplay_island.js',
    'web-artifacts/voplay_island_bg.wasm',
  ]),
});

const READ_CHUNK_BYTES = 64 * 1024;
const MAX_ID_BYTES = 4 * 1024;
const UTF8_DECODER = new TextDecoder('utf-8', { fatal: true });

function stricterLimit(value, fallback, label) {
  if (value === undefined) {
    return fallback;
  }
  if (!Number.isSafeInteger(value) || value < 0 || value > fallback) {
    throw new Error(`${label} must be an integer between 0 and ${fallback}`);
  }
  return value;
}

class ResourceBudget {
  constructor(label, {
    maxDirectoryEntries,
    maxDirectoryObservationEntries,
  } = {}) {
    this.label = label;
    this.maxDirectoryEntries = stricterLimit(
      maxDirectoryEntries,
      SOURCE_BOUND_EVIDENCE_LIMITS.maxTreeEntries,
      'maxDirectoryEntries',
    );
    this.maxDirectoryObservationEntries = stricterLimit(
      maxDirectoryObservationEntries,
      SOURCE_BOUND_EVIDENCE_LIMITS.maxDirectoryObservationEntries,
      'maxDirectoryObservationEntries',
    );
    this.inputPaths = 0;
    this.files = 0;
    this.filesystemEntries = 0;
    this.directoryEntries = 0;
    this.directoryObservationEntries = 0;
    this.payloadBytes = 0;
    this.metadataBytes = 0;
    this.observationMetadataBytes = 0;
  }

  addMetadata(value, label) {
    const bytes = Buffer.byteLength(value, 'utf8');
    if (bytes > SOURCE_BOUND_EVIDENCE_LIMITS.maxPathBytes) {
      throw new Error(`${label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxPathBytes}-byte path limit`);
    }
    this.metadataBytes += bytes;
    if (this.metadataBytes > SOURCE_BOUND_EVIDENCE_LIMITS.maxMetadataBytes) {
      throw new Error(`${this.label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxMetadataBytes}-byte metadata limit`);
    }
  }

  addInputPath(value, label) {
    this.inputPaths += 1;
    if (this.inputPaths > SOURCE_BOUND_EVIDENCE_LIMITS.maxInputPaths) {
      throw new Error(`${this.label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxInputPaths}-path input limit`);
    }
    this.addMetadata(value, label);
  }

  addFilesystemEntry(value) {
    this.filesystemEntries += 1;
    if (this.filesystemEntries > SOURCE_BOUND_EVIDENCE_LIMITS.maxTreeEntries) {
      throw new Error(`${this.label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxTreeEntries}-entry filesystem limit`);
    }
    this.addMetadata(value, `filesystem path ${value}`);
  }

  addDirectoryEntry(nameBytes, directory) {
    this.directoryEntries += 1;
    if (this.directoryEntries > this.maxDirectoryEntries) {
      throw new Error(`${this.label} exceeds the ${this.maxDirectoryEntries}-entry traversal limit`);
    }
    this.metadataBytes += nameBytes;
    if (this.metadataBytes > SOURCE_BOUND_EVIDENCE_LIMITS.maxMetadataBytes) {
      throw new Error(`${this.label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxMetadataBytes}-byte metadata limit while reading ${directory}`);
    }
  }

  addDirectoryObservation(nameBytes, directory) {
    this.directoryObservationEntries += 1;
    if (this.directoryObservationEntries > this.maxDirectoryObservationEntries) {
      throw new Error(`${this.label} exceeds the ${this.maxDirectoryObservationEntries}-entry directory observation limit`);
    }
    this.observationMetadataBytes += nameBytes;
    if (this.observationMetadataBytes > SOURCE_BOUND_EVIDENCE_LIMITS.maxMetadataBytes) {
      throw new Error(`${this.label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxMetadataBytes}-byte observation metadata limit while rereading ${directory}`);
    }
  }

  addLeaf(label) {
    this.files += 1;
    if (this.files > SOURCE_BOUND_EVIDENCE_LIMITS.maxFiles) {
      throw new Error(`${this.label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxFiles}-file limit at ${label}`);
    }
  }

  reserveFile(size, label) {
    if (!Number.isSafeInteger(size) || size < 0 || size > SOURCE_BOUND_EVIDENCE_LIMITS.maxFileBytes) {
      throw new Error(`${label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxFileBytes}-byte file limit`);
    }
    this.addLeaf(label);
    this.addPayload(size, label);
  }

  reserveSymlink(size, label) {
    if (!Number.isSafeInteger(size) || size < 0 || size > SOURCE_BOUND_EVIDENCE_LIMITS.maxSymlinkBytes) {
      throw new Error(`${label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxSymlinkBytes}-byte symbolic-link limit`);
    }
    this.addPayload(size, label);
  }

  addPayload(size, label) {
    const next = this.payloadBytes + size;
    if (!Number.isSafeInteger(next) || next > SOURCE_BOUND_EVIDENCE_LIMITS.maxTotalBytes) {
      throw new Error(`${this.label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxTotalBytes}-byte aggregate limit at ${label}`);
    }
    this.payloadBytes = next;
  }
}

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function digestJson(value) {
  return sha256(Buffer.from(JSON.stringify(value), 'utf8'));
}

function errorMessage(error) {
  return error instanceof Error ? error.message : String(error);
}

function validateString(value, label, { allowEmpty = false, maxBytes = MAX_ID_BYTES } = {}) {
  if (typeof value !== 'string' || (!allowEmpty && value.length === 0)) {
    throw new Error(`${label} must be ${allowEmpty ? 'a string' : 'a non-empty string'}`);
  }
  if (value.includes('\0')) {
    throw new Error(`${label} contains a NUL byte`);
  }
  if (Buffer.byteLength(value, 'utf8') > maxBytes) {
    throw new Error(`${label} exceeds the ${maxBytes}-byte limit`);
  }
  return value;
}

function validatePath(value, label) {
  return validateString(value, label, {
    maxBytes: SOURCE_BOUND_EVIDENCE_LIMITS.maxPathBytes,
  });
}

function normalizeRoot(value, label) {
  return canonicalExistingDirectory(validatePath(value, label), label);
}

function normalizedPaths(values, {
  label,
  allowNull = false,
  requireNonEmpty = false,
  budget,
}) {
  if (!Array.isArray(values)) {
    throw new Error(`${label} must be an array`);
  }
  if (values.length > SOURCE_BOUND_EVIDENCE_LIMITS.maxInputPaths) {
    throw new Error(`${label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxInputPaths}-path input limit`);
  }
  const normalized = [];
  for (let index = 0; index < values.length; index += 1) {
    const value = values[index];
    if (allowNull && (value === null || value === undefined || value === false)) {
      continue;
    }
    const checked = validatePath(value, `${label}[${index}]`);
    budget?.addInputPath(checked, `${label}[${index}]`);
    normalized.push(checked);
  }
  const canonical = [...new Set(normalized)].sort();
  if (requireNonEmpty && canonical.length === 0) {
    throw new Error(`${label} must contain at least one path`);
  }
  return canonical;
}

function pathEscapesRoot(root, absolutePath) {
  const relative = path.relative(root, absolutePath);
  return relative === '..'
    || relative.startsWith(`..${path.sep}`)
    || path.isAbsolute(relative);
}

function normalizeScopedPath(root, value, label) {
  const checked = validatePath(value, label);
  if (checked.split(/[\\/]/u).includes('..')) {
    throw new Error(`${label} must not contain a parent-directory segment`);
  }
  const absolute = path.resolve(root, checked);
  if (pathEscapesRoot(root, absolute)) {
    throw new Error(`${label} escapes evidence root ${root}: ${checked}`);
  }
  return toDisplayPath(root, absolute);
}

function normalizedScopedPaths(root, values, {
  label,
  allowNull = false,
  requireNonEmpty = false,
  budget,
}) {
  if (!Array.isArray(values)) {
    throw new Error(`${label} must be an array`);
  }
  if (values.length > SOURCE_BOUND_EVIDENCE_LIMITS.maxInputPaths) {
    throw new Error(`${label} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxInputPaths}-path input limit`);
  }
  const normalized = [];
  for (let index = 0; index < values.length; index += 1) {
    const value = values[index];
    if (allowNull && (value === null || value === undefined || value === false)) {
      continue;
    }
    const itemLabel = `${label}[${index}]`;
    const checked = validatePath(value, itemLabel);
    budget?.addInputPath(checked, itemLabel);
    normalized.push(normalizeScopedPath(root, checked, itemLabel));
  }
  const canonical = [...new Set(normalized)].sort();
  if (requireNonEmpty && canonical.length === 0) {
    throw new Error(`${label} must contain at least one path`);
  }
  return canonical;
}

function sameArray(left, right) {
  return Array.isArray(left)
    && left.length === right.length
    && left.every((value, index) => value === right[index]);
}

function sameRecord(actual, expected) {
  if (!actual || typeof actual !== 'object' || Array.isArray(actual)) {
    return false;
  }
  const expectedKeys = Object.keys(expected).sort();
  let actualKeyCount = 0;
  for (const key in actual) {
    if (!Object.hasOwn(actual, key)) {
      continue;
    }
    actualKeyCount += 1;
    if (actualKeyCount > SOURCE_BOUND_EVIDENCE_LIMITS.maxRepositories
      || !Object.hasOwn(expected, key)
      || actual[key] !== expected[key]) {
      return false;
    }
  }
  return actualKeyCount === expectedKeys.length;
}

function repoDigestEntries(repoEntries) {
  return repoEntries.map(({ name, commit, dirty, sourceDigest }) => ({
    name,
    commit,
    dirty,
    sourceDigest,
  }));
}

function repoSummary(repoEntries) {
  return {
    testedCommits: Object.fromEntries(repoEntries.map((repo) => [repo.name, repo.commit])),
    dirtyFlags: Object.fromEntries(repoEntries.map((repo) => [repo.name, repo.dirty])),
  };
}

function bindingDigest({ gate, ciRunId, sourceDigest, gateDigest, artifactDigest, testedCommits, dirtyFlags }) {
  return digestJson({
    gate,
    ciRunId,
    sourceDigest,
    gateDigest,
    artifactDigest,
    testedCommits,
    dirtyFlags,
  });
}

function gitOutput(args, cwd, maxBuffer = SOURCE_BOUND_EVIDENCE_LIMITS.maxGitOutputBytes) {
  try {
    return execFileSync('git', args, {
      cwd,
      encoding: 'buffer',
      env: cleanGitEnvironment(),
      maxBuffer,
      timeout: SOURCE_BOUND_EVIDENCE_LIMITS.gitTimeoutMs,
      stdio: ['ignore', 'pipe', 'ignore'],
    });
  } catch {
    return null;
  }
}

function decodeUtf8(bytes, label) {
  try {
    return UTF8_DECODER.decode(bytes);
  } catch {
    throw new Error(`${label} is not valid UTF-8`);
  }
}

export function parseGitFileList(output, repoRoot) {
  if (!Buffer.isBuffer(output)) {
    throw new Error('Git file list must be a Buffer');
  }
  if (output.length > SOURCE_BOUND_EVIDENCE_LIMITS.maxGitOutputBytes) {
    throw new Error(`Git file list exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxGitOutputBytes}-byte output limit`);
  }
  const normalizedRoot = normalizeRoot(repoRoot, 'repository root');
  if (output.length === 0) {
    return [];
  }
  const files = [];
  const seen = new Set();
  let itemStart = 0;
  for (let index = 0; index < output.length; index += 1) {
    if (output[index] !== 0) {
      if (index - itemStart + 1 > SOURCE_BOUND_EVIDENCE_LIMITS.maxPathBytes) {
        throw new Error(`Git source path in ${normalizedRoot} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxPathBytes}-byte path limit`);
      }
      continue;
    }
    if (files.length >= SOURCE_BOUND_EVIDENCE_LIMITS.maxTreeEntries) {
      throw new Error(`Git source tree ${normalizedRoot} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxTreeEntries}-entry limit`);
    }
    const relative = decodeUtf8(
      output.subarray(itemStart, index),
      `Git source path in ${normalizedRoot}`,
    );
    validatePath(relative, `Git source path in ${normalizedRoot}`);
    const absolute = path.resolve(normalizedRoot, relative);
    const back = path.relative(normalizedRoot, absolute);
    if (back === '..' || back.startsWith(`..${path.sep}`) || path.isAbsolute(back)) {
      throw new Error(`Git source path escapes ${normalizedRoot}: ${relative}`);
    }
    if (seen.has(relative)) {
      throw new Error(`Git source path is duplicated in ${normalizedRoot}: ${relative}`);
    }
    seen.add(relative);
    files.push(relative);
    itemStart = index + 1;
  }
  if (itemStart !== output.length) {
    throw new Error(`Git file list for ${normalizedRoot} was not NUL-terminated`);
  }
  return files.sort();
}

function gitFiles(repoRoot) {
  const output = gitOutput(
    ['ls-files', '-z', '--cached', '--others', '--exclude-standard'],
    repoRoot,
  );
  if (output === null) {
    throw new Error(`could not enumerate Git source files in ${repoRoot}`);
  }
  return parseGitFileList(output, repoRoot);
}

function gitTrackedFiles(repoRoot) {
  const output = gitOutput(['ls-files', '-z', '--cached'], repoRoot);
  if (output === null) {
    throw new Error(`could not enumerate tracked Git source files in ${repoRoot}`);
  }
  return parseGitFileList(output, repoRoot);
}

export function sourceTreeDeclaredOutputPaths(declaration) {
  const outputs = SOURCE_TREE_OUTPUT_DECLARATIONS[declaration];
  if (!outputs) {
    throw new Error(`unknown source-tree output declaration: ${declaration}`);
  }
  return [...outputs];
}

function gitStatusSnapshot(repoRoot) {
  const output = gitOutput(
    ['status', '--porcelain=v1', '-z', '--untracked-files=all'],
    repoRoot,
  );
  if (output === null) {
    return null;
  }
  return {
    dirty: output.length > 0,
    size: output.length,
    digest: sha256(output),
  };
}

function sameGitStatus(left, right) {
  return left !== null
    && right !== null
    && left.dirty === right.dirty
    && left.size === right.size
    && left.digest === right.digest;
}

function toDisplayPath(root, absolutePath) {
  const relative = path.relative(root, absolutePath).split(path.sep).join('/');
  return relative || '.';
}

function isMissingError(error) {
  return error?.code === 'ENOENT' || error?.code === 'ENOTDIR';
}

function tryLstat(absolutePath) {
  try {
    return lstatSync(absolutePath, { bigint: true });
  } catch (error) {
    if (isMissingError(error)) {
      return null;
    }
    throw error;
  }
}

function statKind(stat) {
  if (stat.isFile()) return 'file';
  if (stat.isDirectory()) return 'directory';
  if (stat.isSymbolicLink()) return 'symlink';
  if (stat.isFIFO()) return 'fifo';
  if (stat.isSocket()) return 'socket';
  if (stat.isBlockDevice()) return 'block-device';
  if (stat.isCharacterDevice()) return 'character-device';
  return 'other';
}

function sameStat(left, right) {
  if (left === null || right === null) {
    return left === right;
  }
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.nlink === right.nlink
    && left.uid === right.uid
    && left.gid === right.gid
    && left.rdev === right.rdev
    && left.size === right.size
    && left.mtimeNs === right.mtimeNs
    && left.ctimeNs === right.ctimeNs
    && statKind(left) === statKind(right);
}

function assertPlanStable(plan) {
  const current = tryLstat(plan.absolute);
  if (!sameStat(plan.stat, current)) {
    throw new Error(`filesystem entry changed while evidence was collected: ${plan.path}`);
  }
}

function planAncestorDirectories(root, absolutePath, label) {
  if (pathEscapesRoot(root, absolutePath)) {
    throw new Error(`${label} escapes evidence root ${root}`);
  }
  if (absolutePath === root) {
    return [];
  }
  const ancestors = [];
  let current = path.dirname(absolutePath);
  while (true) {
    const stat = tryLstat(current);
    const displayPath = toDisplayPath(root, current);
    if (stat === null || !stat.isDirectory() || stat.isSymbolicLink()) {
      throw new Error(`${label} has a missing, non-directory, or symbolic-link ancestor: ${displayPath}`);
    }
    ancestors.push({
      absolute: current,
      path: displayPath,
      stat,
      kind: 'directory',
    });
    if (current === root) {
      break;
    }
    const parent = path.dirname(current);
    if (parent === current || pathEscapesRoot(root, parent)) {
      throw new Error(`${label} escapes evidence root ${root}`);
    }
    current = parent;
  }
  return ancestors;
}

function assertPlansStable(plans) {
  for (const plan of plans) {
    assertPlanStable(plan);
  }
}

function planEntry(root, absolutePath, budget, { explicitInput = false } = {}) {
  validatePath(absolutePath, 'filesystem path');
  const displayPath = toDisplayPath(root, absolutePath);
  if (!explicitInput) {
    budget.addFilesystemEntry(displayPath);
  }
  const stat = tryLstat(absolutePath);
  const kind = stat === null ? 'missing' : statKind(stat);
  const plan = {
    absolute: absolutePath,
    path: displayPath,
    stat,
    kind,
  };
  if (kind === 'file') {
    if (stat.size > BigInt(Number.MAX_SAFE_INTEGER)) {
      throw new Error(`${displayPath} has an unsupported file size`);
    }
    budget.reserveFile(Number(stat.size), displayPath);
  } else if (kind === 'symlink') {
    budget.addLeaf(displayPath);
    if (stat.size > BigInt(SOURCE_BOUND_EVIDENCE_LIMITS.maxSymlinkBytes)) {
      throw new Error(`${displayPath} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxSymlinkBytes}-byte symbolic-link limit`);
    }
  } else if (kind !== 'missing' && kind !== 'directory') {
    budget.addLeaf(displayPath);
  }
  return plan;
}

function digestRegularFile(plan) {
  assertPlanStable(plan);
  const expectedSize = Number(plan.stat.size);
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const fd = openSync(plan.absolute, fsConstants.O_RDONLY | noFollow);
  try {
    const opened = fstatSync(fd, { bigint: true });
    if (!sameStat(plan.stat, opened) || !opened.isFile()) {
      throw new Error(`filesystem entry changed before it was read: ${plan.path}`);
    }
    const hash = createHash('sha256');
    const chunk = Buffer.allocUnsafe(Math.min(READ_CHUNK_BYTES, Math.max(1, expectedSize)));
    let total = 0;
    while (total < expectedSize) {
      const requested = Math.min(chunk.length, expectedSize - total);
      const read = readSync(fd, chunk, 0, requested, null);
      if (read === 0) {
        throw new Error(`file was truncated while evidence was collected: ${plan.path}`);
      }
      hash.update(chunk.subarray(0, read));
      total += read;
    }
    if (readSync(fd, chunk, 0, 1, null) !== 0) {
      throw new Error(`file grew while evidence was collected: ${plan.path}`);
    }
    const completed = fstatSync(fd, { bigint: true });
    if (!sameStat(plan.stat, completed)) {
      throw new Error(`file changed while evidence was collected: ${plan.path}`);
    }
    return {
      path: plan.path,
      size: total,
      digest: `sha256:${hash.digest('hex')}`,
    };
  } finally {
    closeSync(fd);
    assertPlanStable(plan);
  }
}

function digestSymlink(plan, budget) {
  assertPlanStable(plan);
  const firstTarget = readlinkSync(plan.absolute, { encoding: 'buffer' });
  if (!Buffer.isBuffer(firstTarget)) {
    throw new Error(`could not read symbolic link as bytes: ${plan.path}`);
  }
  assertPlanStable(plan);
  const secondTarget = readlinkSync(plan.absolute, { encoding: 'buffer' });
  if (!Buffer.isBuffer(secondTarget) || !firstTarget.equals(secondTarget)) {
    throw new Error(`symbolic link changed while evidence was collected: ${plan.path}`);
  }
  budget.reserveSymlink(firstTarget.length, plan.path);
  assertPlanStable(plan);
  return {
    path: plan.path,
    kind: 'symlink',
    size: firstTarget.length,
    digest: sha256(firstTarget),
  };
}

function materializePlan(plan, budget) {
  if (plan.kind === 'missing') {
    return { path: plan.path, missing: true };
  }
  if (plan.kind === 'file') {
    return digestRegularFile(plan);
  }
  if (plan.kind === 'symlink') {
    return digestSymlink(plan, budget);
  }
  return { path: plan.path, kind: plan.kind };
}

function materializePlans(plans, budget) {
  const entries = plans.map((plan) => materializePlan(plan, budget));
  for (const plan of plans) {
    assertPlanStable(plan);
  }
  return entries;
}

function readDirectoryNamesOnce(plan, budget, observationOnly) {
  assertPlanStable(plan);
  const directory = opendirSync(plan.absolute, { encoding: 'buffer' });
  const names = [];
  try {
    assertPlanStable(plan);
    while (true) {
      const entry = directory.readSync();
      if (entry === null) {
        break;
      }
      if (!Buffer.isBuffer(entry.name)) {
        throw new Error(`directory entry name was not returned as bytes in ${plan.path}`);
      }
      if (observationOnly) {
        budget.addDirectoryObservation(entry.name.length, plan.path);
      } else {
        budget.addDirectoryEntry(entry.name.length, plan.path);
      }
      const name = decodeUtf8(entry.name, `directory entry name in ${plan.path}`);
      if (name === '.' || name === '..' || name.includes('/') || name.includes('\0')) {
        throw new Error(`directory contains an invalid entry name: ${plan.path}`);
      }
      names.push(name);
    }
  } finally {
    directory.closeSync();
  }
  assertPlanStable(plan);
  return names.sort();
}

function readDirectoryNames(plan, budget) {
  const first = readDirectoryNamesOnce(plan, budget, false);
  const second = readDirectoryNamesOnce(plan, budget, true);
  if (!sameArray(first, second)) {
    throw new Error(`directory changed while evidence was collected: ${plan.path}`);
  }
  return first;
}

function collectArtifactPlans(root, artifactPaths, budget) {
  const plans = [];
  const ancestorPlans = [];
  const directories = [];
  for (const artifact of artifactPaths) {
    const absolute = path.resolve(root, artifact);
    ancestorPlans.push(...planAncestorDirectories(root, absolute, `artifact ${artifact}`));
    const rootPlan = planEntry(root, absolute, budget, {
      explicitInput: true,
    });
    if (rootPlan.kind !== 'file' && rootPlan.kind !== 'directory') {
      throw new Error(`artifact ${rootPlan.path} must be an existing regular file or directory`);
    }
    plans.push(rootPlan);
    if (rootPlan.kind === 'directory') {
      directories.push({ plan: rootPlan, depth: 0 });
    }
  }

  for (let index = 0; index < directories.length; index += 1) {
    const { plan, depth } = directories[index];
    if (depth > SOURCE_BOUND_EVIDENCE_LIMITS.maxDepth) {
      throw new Error(`${plan.path} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxDepth}-level traversal limit`);
    }
    const names = readDirectoryNames(plan, budget);
    for (const name of names) {
      const child = planEntry(root, path.join(plan.absolute, name), budget);
      if (child.kind !== 'file' && child.kind !== 'directory') {
        throw new Error(`artifact tree entry ${child.path} must be a regular file or directory`);
      }
      plans.push(child);
      if (child.kind === 'directory') {
        if (depth >= SOURCE_BOUND_EVIDENCE_LIMITS.maxDepth) {
          throw new Error(`${child.path} exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxDepth}-level traversal limit`);
        }
        directories.push({ plan: child, depth: depth + 1 });
      }
    }
  }
  assertPlansStable(ancestorPlans);
  return { plans, ancestorPlans };
}

function compareEntries(left, right) {
  if (left.path < right.path) return -1;
  if (left.path > right.path) return 1;
  const leftKind = left.kind ?? '';
  const rightKind = right.kind ?? '';
  if (leftKind < rightKind) return -1;
  if (leftKind > rightKind) return 1;
  return 0;
}

function sameGitFiles(left, right) {
  return sameArray(left, right);
}

function sourceTreeEntriesWithBudget(repoRoot, budget, excludedPaths = []) {
  const excluded = new Set(excludedPaths);
  const filesBefore = gitFiles(repoRoot);
  const plans = filesBefore
    .filter((relative) => !excluded.has(relative))
    .map((relative) => planEntry(
      repoRoot,
      path.resolve(repoRoot, relative),
      budget,
    ));
  const entries = materializePlans(plans, budget);
  const filesAfter = gitFiles(repoRoot);
  if (!sameGitFiles(filesBefore, filesAfter)) {
    throw new Error(`Git source tree changed while evidence was collected: ${repoRoot}`);
  }
  return entries;
}

function sourceTreeDigestWithBudget(repoRoot, budget) {
  return digestJson(sourceTreeEntriesWithBudget(repoRoot, budget));
}

function declaredOutputFacts(repoRoot, declaration, outputPaths) {
  const outputRoot = path.join(repoRoot, 'web-artifacts');
  let canonicalOutputRoot;
  let outputRootMetadata;
  try {
    canonicalOutputRoot = realpathSync.native(outputRoot);
    outputRootMetadata = lstatSync(outputRoot, { bigint: true });
  } catch (error) {
    throw new Error(`${declaration} output directory cannot be read: ${errorMessage(error)}`);
  }
  if (
    canonicalOutputRoot !== outputRoot
    || outputRootMetadata.isSymbolicLink()
    || !outputRootMetadata.isDirectory()
  ) {
    throw new Error(`${declaration} output directory must be a canonical real directory`);
  }
  return outputPaths.map((relative) => {
    if (!/^web-artifacts\/[A-Za-z0-9][A-Za-z0-9._-]*$/u.test(relative)) {
      throw new Error(`${declaration} contains an invalid declared output path: ${relative}`);
    }
    const file = path.join(repoRoot, ...relative.split('/'));
    let canonical;
    let metadata;
    try {
      canonical = realpathSync.native(file);
      metadata = lstatSync(file, { bigint: true });
    } catch (error) {
      throw new Error(`${declaration} output cannot be read: ${relative}: ${errorMessage(error)}`);
    }
    if (canonical !== file || metadata.isSymbolicLink() || !metadata.isFile()) {
      throw new Error(`${declaration} output must be a canonical tracked regular file: ${relative}`);
    }
    return {
      path: relative,
      dev: String(metadata.dev),
      ino: String(metadata.ino),
      mode: String(metadata.mode),
      nlink: String(metadata.nlink),
      size: String(metadata.size),
      mtimeNs: String(metadata.mtimeNs),
      ctimeNs: String(metadata.ctimeNs),
    };
  });
}

function fileSetDigestWithBudget(root, files, budget) {
  const ancestorPlans = [];
  const plans = files.map((file) => {
    const absolute = path.resolve(root, file);
    ancestorPlans.push(...planAncestorDirectories(root, absolute, `file ${file}`));
    const plan = planEntry(root, absolute, budget, { explicitInput: true });
    if (plan.kind !== 'file') {
      throw new Error(`file-set entry ${plan.path} must be an existing regular file`);
    }
    return plan;
  });
  const entries = materializePlans(plans, budget).sort(compareEntries);
  assertPlansStable(ancestorPlans);
  return digestJson(entries);
}

function artifactSetDigestWithBudget(root, artifacts, budget) {
  const { plans, ancestorPlans } = collectArtifactPlans(root, artifacts, budget);
  const entries = materializePlans(plans, budget).sort(compareEntries);
  assertPlansStable(ancestorPlans);
  return digestJson(entries);
}

function artifactSetSnapshotWithBudget(root, artifacts, budget) {
  const { plans, ancestorPlans } = collectArtifactPlans(root, artifacts, budget);
  const entries = materializePlans(plans, budget).sort(compareEntries);
  assertPlansStable(ancestorPlans);
  return entries;
}

export function sourceTreeDigest(repoRoot) {
  const normalizedRoot = normalizeRoot(repoRoot, 'repository root');
  return sourceTreeDigestWithBudget(
    normalizedRoot,
    new ResourceBudget(`source tree ${normalizedRoot}`),
  );
}

export function sourceTreeDigestExcludingDeclaredOutputs(repoRoot, declaration) {
  const normalizedRoot = normalizeRoot(repoRoot, 'repository root');
  const outputPaths = sourceTreeDeclaredOutputPaths(declaration);
  const trackedBefore = gitTrackedFiles(normalizedRoot);
  for (const relative of outputPaths) {
    if (!trackedBefore.includes(relative)) {
      throw new Error(`${declaration} output must be tracked by Git: ${relative}`);
    }
  }
  const factsBefore = declaredOutputFacts(normalizedRoot, declaration, outputPaths);
  const entries = sourceTreeEntriesWithBudget(
    normalizedRoot,
    new ResourceBudget(`source tree ${normalizedRoot} excluding ${declaration}`),
    outputPaths,
  );
  const factsAfter = declaredOutputFacts(normalizedRoot, declaration, outputPaths);
  const trackedAfter = gitTrackedFiles(normalizedRoot);
  if (!sameGitFiles(trackedBefore, trackedAfter)) {
    throw new Error(`tracked Git source tree changed while evidence was collected: ${normalizedRoot}`);
  }
  if (JSON.stringify(factsBefore) !== JSON.stringify(factsAfter)) {
    throw new Error(`${declaration} outputs changed while evidence was collected`);
  }
  return digestJson({
    schema: 'source-tree-excluding-declared-outputs-v1',
    declaration,
    excludedOutputs: outputPaths,
    entries,
  });
}

export function fileSetDigest(root, files) {
  const normalizedRoot = normalizeRoot(root, 'file-set root');
  const budget = new ResourceBudget(`file set ${normalizedRoot}`);
  const normalizedFiles = normalizedScopedPaths(normalizedRoot, files, {
    label: 'files',
    budget,
  });
  return fileSetDigestWithBudget(normalizedRoot, normalizedFiles, budget);
}

export function artifactSetDigest(root, artifacts, limits = {}) {
  const normalizedRoot = normalizeRoot(root, 'artifact-set root');
  const budget = new ResourceBudget(`artifact set ${normalizedRoot}`, limits);
  const normalizedArtifacts = normalizedScopedPaths(normalizedRoot, artifacts, {
    label: 'artifacts',
    allowNull: true,
    budget,
  });
  return artifactSetDigestWithBudget(normalizedRoot, normalizedArtifacts, budget);
}

export function artifactSetSnapshot(root, artifacts, limits = {}) {
  const normalizedRoot = normalizeRoot(root, 'artifact-set root');
  const budget = new ResourceBudget(`artifact snapshot ${normalizedRoot}`, limits);
  const normalizedArtifacts = normalizedScopedPaths(normalizedRoot, artifacts, {
    label: 'artifacts',
    allowNull: true,
    budget,
  });
  return artifactSetSnapshotWithBudget(normalizedRoot, normalizedArtifacts, budget);
}

export function gitCommit(repoRoot) {
  let normalizedRoot;
  try {
    normalizedRoot = normalizeRoot(repoRoot, 'repository root');
  } catch {
    return null;
  }
  const output = gitOutput(
    ['rev-parse', '--verify', 'HEAD^{commit}'],
    normalizedRoot,
    1024,
  );
  if (output === null) {
    return null;
  }
  let commit;
  try {
    commit = decodeUtf8(output, `Git commit for ${normalizedRoot}`).trim();
  } catch {
    return null;
  }
  return /^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(commit) ? commit : null;
}

export function gitDirty(repoRoot) {
  let normalizedRoot;
  try {
    normalizedRoot = normalizeRoot(repoRoot, 'repository root');
  } catch {
    return true;
  }
  return gitStatusSnapshot(normalizedRoot)?.dirty ?? true;
}

function validateRepos(repos) {
  if (!Array.isArray(repos) || repos.length === 0) {
    throw new Error('repos must contain at least one repository');
  }
  if (repos.length > SOURCE_BOUND_EVIDENCE_LIMITS.maxRepositories) {
    throw new Error(`repos exceeds the ${SOURCE_BOUND_EVIDENCE_LIMITS.maxRepositories}-repository limit`);
  }
  const names = new Set();
  return repos.map((repo, index) => {
    if (!repo || typeof repo !== 'object' || Array.isArray(repo)) {
      throw new Error(`repos[${index}] must be an object`);
    }
    const name = validateString(repo.name, `repos[${index}].name`, { maxBytes: MAX_ID_BYTES });
    if (names.has(name)) {
      throw new Error(`repository name is duplicated: ${name}`);
    }
    names.add(name);
    return {
      name,
      root: canonicalGitRepositoryRoot(repo.root, `repos[${index}].root`),
    };
  });
}

function observeRepo(repo, budget) {
  const commitBefore = gitCommit(repo.root);
  const statusBefore = gitStatusSnapshot(repo.root);
  if (statusBefore === null) {
    throw new Error(`could not read Git status in ${repo.root}`);
  }
  const sourceDigest = sourceTreeDigestWithBudget(repo.root, budget);
  const commitAfter = gitCommit(repo.root);
  const statusAfter = gitStatusSnapshot(repo.root);
  if (commitBefore !== commitAfter || !sameGitStatus(statusBefore, statusAfter)) {
    throw new Error(`repository changed while evidence was collected: ${repo.root}`);
  }
  return {
    name: repo.name,
    root: repo.root,
    commit: commitAfter,
    dirty: statusAfter.dirty,
    sourceDigest,
  };
}

export function sourceBoundEvidence({
  gate,
  generatedAt,
  root,
  repos,
  gateFiles,
  artifacts = [],
}) {
  const normalizedGate = validateString(gate, 'gate');
  validateString(generatedAt, 'generatedAt');
  if (Number.isNaN(Date.parse(generatedAt))) {
    throw new Error('generatedAt must be a valid date');
  }
  const normalizedRoot = normalizeRoot(root, 'evidence root');
  const normalizedRepos = validateRepos(repos);
  const budget = new ResourceBudget(`source-bound evidence ${normalizedGate}`);
  const normalizedGateFiles = normalizedScopedPaths(normalizedRoot, gateFiles, {
    label: 'gateFiles',
    requireNonEmpty: true,
    budget,
  });
  const normalizedArtifacts = normalizedScopedPaths(normalizedRoot, artifacts, {
    label: 'artifacts',
    allowNull: true,
    budget,
  });
  const repoEntries = normalizedRepos.map((repo) => observeRepo(repo, budget));
  const { testedCommits, dirtyFlags } = repoSummary(repoEntries);
  const sourceDigest = digestJson(repoDigestEntries(repoEntries));
  const gateDigest = fileSetDigestWithBudget(normalizedRoot, normalizedGateFiles, budget);
  const artifactDigest = artifactSetDigestWithBudget(normalizedRoot, normalizedArtifacts, budget);
  const missingCommitRepos = repoEntries
    .filter((repo) => !repo.commit)
    .map((repo) => repo.name);
  const dirtyRepos = repoEntries
    .filter((repo) => repo.dirty)
    .map((repo) => repo.name);
  const verdictStatus = missingCommitRepos.length === 0 && dirtyRepos.length === 0 ? 'pass' : 'fail';
  const ciRunId = validateString(
    process.env.VO_DEV_CI_RUN_ID || `standalone:${randomUUID()}`,
    'ciRunId',
  );
  const runBindingDigest = bindingDigest({
    gate: normalizedGate,
    ciRunId,
    sourceDigest,
    gateDigest,
    artifactDigest,
    testedCommits,
    dirtyFlags,
  });
  return {
    schemaVersion: 2,
    ciRunId,
    taskRunId: `${normalizedGate}:${ciRunId}`,
    gate: normalizedGate,
    generatedAt,
    sourceDigest,
    gateDigest,
    artifactDigest,
    runBindingDigest,
    testedCommits,
    dirtyFlags,
    inputs: {
      gateFiles: normalizedGateFiles,
      artifacts: normalizedArtifacts,
    },
    verdict: {
      status: verdictStatus,
      dirtyProvenance: dirtyRepos.length > 0,
      missingCommitRepos,
      dirtyRepos,
    },
    repos: repoEntries,
  };
}

function validDigest(value) {
  return typeof value === 'string' && /^sha256:[0-9a-f]{64}$/.test(value);
}

function validCommit(value) {
  return value === null
    || (typeof value === 'string'
      && value.length <= 64
      && /^[0-9a-f]{40}(?:[0-9a-f]{24})?$/.test(value));
}

function validBoundedString(value, { allowEmpty = false, maxBytes = MAX_ID_BYTES } = {}) {
  return typeof value === 'string'
    && (allowEmpty || value.length > 0)
    && !value.includes('\0')
    && Buffer.byteLength(value, 'utf8') <= maxBytes;
}

function validRepoVerdictList(value, expected) {
  return Array.isArray(value)
    && value.length <= SOURCE_BOUND_EVIDENCE_LIMITS.maxRepositories
    && sameArray(value, expected);
}

function sameRepoScope(actual, expected) {
  return actual.length === expected.length
    && actual.every((repo, index) => (
      repo.name === expected[index].name
      && repo.root === expected[index].root
    ));
}

export function verifySourceBoundEvidence({
  evidence,
  expectedGate,
  root,
  expectedGateFiles,
  expectedArtifacts,
  expectedRepos,
  expectedCiRunId = null,
}) {
  const issues = [];
  if (!validBoundedString(expectedGate)) {
    return ['expectedGate missing or exceeds the evidence identifier limit'];
  }
  let normalizedRoot;
  try {
    normalizedRoot = normalizeRoot(root, 'evidence root');
  } catch (error) {
    return [`evidence root is invalid: ${errorMessage(error)}`];
  }

  let expectedGateFileScope;
  let expectedArtifactScope;
  let expectedRepoScope;
  try {
    const expectedInputBudget = new ResourceBudget('expected freshEvidence scope');
    expectedGateFileScope = normalizedScopedPaths(normalizedRoot, expectedGateFiles, {
      label: 'expectedGateFiles',
      requireNonEmpty: true,
      budget: expectedInputBudget,
    });
    expectedArtifactScope = normalizedScopedPaths(normalizedRoot, expectedArtifacts, {
      label: 'expectedArtifacts',
      allowNull: true,
      budget: expectedInputBudget,
    });
    expectedRepoScope = validateRepos(expectedRepos);
    for (let index = 0; index < expectedRepos.length; index += 1) {
      if (expectedRepos[index].root !== expectedRepoScope[index].root) {
        throw new Error(`expectedRepos[${index}].root must be canonical: ${expectedRepoScope[index].root}`);
      }
    }
  } catch (error) {
    return [`expected freshEvidence scope is invalid: ${errorMessage(error)}`];
  }

  if (!evidence || typeof evidence !== 'object' || Array.isArray(evidence)) {
    return ['missing freshEvidence'];
  }
  if (evidence.schemaVersion !== 2) {
    issues.push(`schemaVersion ${evidence.schemaVersion ?? '(missing)'} did not match 2`);
  }
  if (!validBoundedString(evidence.gate)) {
    issues.push('gate missing or exceeds the evidence identifier limit');
  } else if (evidence.gate !== expectedGate) {
    issues.push(`gate ${evidence.gate} did not match ${expectedGate}`);
  }
  if (!validBoundedString(evidence.ciRunId)) {
    issues.push('ciRunId missing or exceeds the evidence identifier limit');
  } else if (expectedCiRunId !== null) {
    if (!validBoundedString(expectedCiRunId)) {
      issues.push('current ciRunId missing or exceeds the evidence identifier limit');
    } else if (evidence.ciRunId !== expectedCiRunId) {
      issues.push(`ciRunId ${evidence.ciRunId} did not match current run ${expectedCiRunId}`);
    }
  }
  if (validBoundedString(evidence.gate) && validBoundedString(evidence.ciRunId)) {
    if (evidence.taskRunId !== `${evidence.gate}:${evidence.ciRunId}`) {
      issues.push('taskRunId does not bind gate and ciRunId');
    }
  } else {
    issues.push('taskRunId cannot be validated without bounded gate and ciRunId values');
  }
  if (!validBoundedString(evidence.generatedAt) || Number.isNaN(Date.parse(evidence.generatedAt))) {
    issues.push('generatedAt missing or invalid');
  }
  for (const field of ['sourceDigest', 'gateDigest', 'artifactDigest', 'runBindingDigest']) {
    if (!validDigest(evidence[field])) {
      issues.push(`${field} missing or invalid`);
    }
  }

  let gateFiles = [];
  let artifacts = [];
  try {
    const inputBudget = new ResourceBudget('freshEvidence inputs');
    gateFiles = normalizedScopedPaths(normalizedRoot, evidence.inputs?.gateFiles, {
      label: 'gateFiles',
      requireNonEmpty: true,
      budget: inputBudget,
    });
    artifacts = normalizedScopedPaths(normalizedRoot, evidence.inputs?.artifacts, {
      label: 'artifacts',
      budget: inputBudget,
    });
    if (!sameArray(evidence.inputs.gateFiles, gateFiles)) {
      issues.push('gateFiles are not canonical');
    }
    if (!sameArray(evidence.inputs.artifacts, artifacts)) {
      issues.push('artifacts are not canonical');
    }
    if (!sameArray(gateFiles, expectedGateFileScope)) {
      issues.push('gateFiles do not match expected verification scope');
    }
    if (!sameArray(artifacts, expectedArtifactScope)) {
      issues.push('artifacts do not match expected verification scope');
    }
  } catch (error) {
    issues.push(`freshEvidence inputs are invalid: ${errorMessage(error)}`);
  }

  const declaredRepos = Array.isArray(evidence.repos) ? evidence.repos : [];
  let normalizedRepos = [];
  let declaredRepoScopeMatches = false;
  try {
    normalizedRepos = validateRepos(declaredRepos);
    for (let index = 0; index < declaredRepos.length; index += 1) {
      if (declaredRepos[index].root !== normalizedRepos[index].root) {
        issues.push(`${normalizedRepos[index].name} repository root is not canonical`);
      }
      if (!validCommit(declaredRepos[index].commit)) {
        issues.push(`${normalizedRepos[index].name} commit is invalid`);
      }
      if (typeof declaredRepos[index].dirty !== 'boolean') {
        issues.push(`${normalizedRepos[index].name} dirty flag is invalid`);
      }
      if (!validDigest(declaredRepos[index].sourceDigest)) {
        issues.push(`${normalizedRepos[index].name} sourceDigest is invalid`);
      }
    }
    declaredRepoScopeMatches = sameRepoScope(normalizedRepos, expectedRepoScope);
    if (!declaredRepoScopeMatches) {
      issues.push('repos do not match expected verification scope');
    }
  } catch (error) {
    issues.push(`repos are invalid: ${errorMessage(error)}`);
  }

  let currentRepos;
  let gateDigest;
  let artifactDigest;
  try {
    const budget = new ResourceBudget(`verify source-bound evidence ${expectedGate}`);
    for (const gateFile of expectedGateFileScope) {
      budget.addInputPath(gateFile, 'gateFiles');
    }
    for (const artifact of expectedArtifactScope) {
      budget.addInputPath(artifact, 'artifacts');
    }
    currentRepos = expectedRepoScope.map((repo) => observeRepo(repo, budget));
    gateDigest = fileSetDigestWithBudget(normalizedRoot, expectedGateFileScope, budget);
    artifactDigest = artifactSetDigestWithBudget(normalizedRoot, expectedArtifactScope, budget);
  } catch (error) {
    issues.push(`could not recompute freshEvidence within resource limits: ${errorMessage(error)}`);
    return issues;
  }

  if (declaredRepoScopeMatches) {
    for (let index = 0; index < declaredRepos.length; index += 1) {
      const declared = declaredRepos[index];
      const current = currentRepos[index];
      for (const field of ['commit', 'dirty', 'sourceDigest']) {
        if (declared[field] !== current[field]) {
          issues.push(`${current.name} ${field} does not match current source`);
        }
      }
    }
  }

  const { testedCommits, dirtyFlags } = repoSummary(currentRepos);
  const sourceDigest = digestJson(repoDigestEntries(currentRepos));
  for (const [field, expected] of Object.entries({
    sourceDigest,
    gateDigest,
    artifactDigest,
  })) {
    if (evidence[field] !== expected) {
      issues.push(`${field} does not match recomputed value`);
    }
  }
  if (validBoundedString(evidence.gate) && validBoundedString(evidence.ciRunId)) {
    const runBindingDigest = bindingDigest({
      gate: evidence.gate,
      ciRunId: evidence.ciRunId,
      sourceDigest,
      gateDigest,
      artifactDigest,
      testedCommits,
      dirtyFlags,
    });
    if (evidence.runBindingDigest !== runBindingDigest) {
      issues.push('runBindingDigest does not match recomputed value');
    }
  }
  if (!sameRecord(evidence.testedCommits, testedCommits)) {
    issues.push('testedCommits do not match current repos');
  }
  if (!sameRecord(evidence.dirtyFlags, dirtyFlags)) {
    issues.push('dirtyFlags do not match current repos');
  }

  const missingCommitRepos = currentRepos.filter((repo) => !repo.commit).map((repo) => repo.name);
  const dirtyRepos = currentRepos.filter((repo) => repo.dirty).map((repo) => repo.name);
  const verdictStatus = missingCommitRepos.length === 0 && dirtyRepos.length === 0 ? 'pass' : 'fail';
  if (evidence.verdict?.status !== verdictStatus) {
    const declaredStatus = ['pass', 'fail'].includes(evidence.verdict?.status)
      ? evidence.verdict.status
      : '(missing or invalid)';
    issues.push(`freshEvidence verdict ${declaredStatus} did not match ${verdictStatus}`);
  }
  if (typeof evidence.verdict?.dirtyProvenance !== 'boolean'
    || evidence.verdict.dirtyProvenance !== (dirtyRepos.length > 0)) {
    issues.push('freshEvidence dirtyProvenance does not match current repos');
  }
  if (!validRepoVerdictList(evidence.verdict?.missingCommitRepos, missingCommitRepos)) {
    issues.push('freshEvidence missingCommitRepos do not match current repos');
  }
  if (!validRepoVerdictList(evidence.verdict?.dirtyRepos, dirtyRepos)) {
    issues.push('freshEvidence dirtyRepos do not match current repos');
  }
  return issues;
}
