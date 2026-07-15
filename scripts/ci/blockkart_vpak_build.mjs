#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { createHash, randomUUID } from 'node:crypto';
import {
  closeSync,
  constants as fsConstants,
  fchmodSync,
  fstatSync,
  fsyncSync,
  lstatSync,
  mkdtempSync,
  mkdirSync,
  openSync,
  readdirSync,
  readSync,
  realpathSync,
  renameSync,
  rmSync,
  unlinkSync,
  writeSync,
} from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  assertVoCliCargoConfigBoundary,
  assertVoCliBuildInputs,
  currentVoCliToolchain,
  currentVoCliBuildInputs,
  voCliExecutionDigest,
  VO_CLI_GUEST_ENVIRONMENT,
  voCliBuildCommand,
  voCliBuildEnvironment,
  voCliGuestEnvironment,
} from './quickplay_cli_producer_contract.mjs';
import { parseBoundedStrictJsonBytes } from './quickplay_web_manifest_contract.mjs';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const targetRoot = path.join(root, 'target');
const outDir = path.join(targetRoot, 'blockkart-vpak-build');
const voBin = path.join(outDir, process.platform === 'win32' ? 'vo.exe' : 'vo');
const reportPath = path.join(outDir, 'report.json');
const guestRoot = path.join(outDir, 'guest');
const producerLeasePath = path.join(blockKartRoot, 'target', 'blockkart-vpak-producer.lock');
const MAX_REPORT_BYTES = 16 * 1024 * 1024;
const MAX_VPAK_BYTES = 64 * 1024 * 1024;
const MAX_VO_BINARY_BYTES = 256 * 1024 * 1024;
const MAX_CARGO_SEED_BYTES = 512 * 1024 * 1024;
const MAX_CARGO_SEED_ENTRIES = 100_000;
const MAX_CARGO_SEED_DEPTH = 64;
const MAX_CARGO_SEED_PATH_BYTES = 8 * 1024 * 1024;
const CARGO_BUILD_TIMEOUT_MS = 25 * 60 * 1000;
const VPAK_GUEST_TIMEOUT_MS = 5 * 60 * 1000;

function pathWithin(parent, candidate) {
  const relative = path.relative(parent, candidate);
  return relative === ''
    || (!relative.startsWith(`..${path.sep}`) && relative !== '..' && !path.isAbsolute(relative));
}

function sameNativePath(left, right) {
  const normalizedLeft = path.normalize(path.resolve(left));
  const normalizedRight = path.normalize(path.resolve(right));
  return process.platform === 'win32'
    ? normalizedLeft.toUpperCase() === normalizedRight.toUpperCase()
    : normalizedLeft === normalizedRight;
}

function ensureRealChildDirectoryUnder(base, candidate, label) {
  const canonicalRoot = realpathSync.native(base);
  const resolved = path.resolve(candidate);
  if (!pathWithin(canonicalRoot, resolved) || resolved === canonicalRoot) {
    throw new Error(`${label} must be a child of its declared root`);
  }
  const components = path.relative(canonicalRoot, resolved).split(path.sep).filter(Boolean);
  let current = canonicalRoot;
  for (const component of components) {
    current = path.join(current, component);
    try {
      const metadata = lstatSync(current);
      if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
        throw new Error(`${label} component must be a real directory: ${current}`);
      }
    } catch (error) {
      if (error?.code !== 'ENOENT') throw error;
      try {
        mkdirSync(current, { mode: 0o755 });
      } catch (mkdirError) {
        if (mkdirError?.code !== 'EEXIST') throw mkdirError;
      }
      const created = lstatSync(current);
      if (!created.isDirectory() || created.isSymbolicLink()) {
        throw new Error(`${label} component was replaced: ${current}`);
      }
    }
  }
  const canonicalOutput = realpathSync.native(resolved);
  if (!sameNativePath(canonicalOutput, resolved) || !pathWithin(canonicalRoot, canonicalOutput)) {
    throw new Error(`${label} resolves outside the Volang root`);
  }
  return canonicalOutput;
}

function ensureRealChildDirectory(candidate, label) {
  return ensureRealChildDirectoryUnder(root, candidate, label);
}

function prepareOutputDirectory() {
  const canonicalTarget = ensureRealChildDirectory(targetRoot, 'Volang Cargo target root');
  const output = ensureRealChildDirectory(outDir, 'BlockKart VPAK report output');
  if (!pathWithin(canonicalTarget, output) || output === canonicalTarget) {
    throw new Error('BlockKart VPAK report output must be a dedicated child of the Volang target directory');
  }
  return output;
}

function sameStat(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.size === right.size
    && left.mtimeNs === right.mtimeNs
    && left.ctimeNs === right.ctimeNs;
}

function syncDirectory(directory) {
  if (process.platform === 'win32') return;
  const descriptor = openSync(directory, fsConstants.O_RDONLY);
  try {
    fsyncSync(descriptor);
  } finally {
    closeSync(descriptor);
  }
}

function readDescriptorExactly(descriptor, size, label) {
  if (!Number.isSafeInteger(size) || size < 0) throw new Error(`${label} has an invalid size`);
  const bytes = Buffer.allocUnsafe(size);
  let offset = 0;
  while (offset < bytes.byteLength) {
    const count = readSync(descriptor, bytes, offset, bytes.byteLength - offset, offset);
    if (count === 0) throw new Error(`${label} was truncated while it was read`);
    offset += count;
  }
  const extra = Buffer.allocUnsafe(1);
  if (readSync(descriptor, extra, 0, 1, bytes.byteLength) !== 0) {
    throw new Error(`${label} grew while it was read`);
  }
  return bytes;
}

// A crashed producer leaves this lease behind intentionally. Automatic expiry
// can overlap a paused build or a reused PID; explicit recovery keeps the
// single-writer guarantee fail-closed on every supported host.
function acquireProducerLease(
  leasePath = producerLeasePath,
  declaredRoot = blockKartRoot,
) {
  const parent = ensureRealChildDirectoryUnder(
    declaredRoot,
    path.dirname(leasePath),
    'BlockKart VPAK producer lease directory',
  );
  if (!sameNativePath(parent, path.dirname(leasePath))) {
    throw new Error('BlockKart VPAK producer lease must be published in its canonical directory');
  }
  const token = randomUUID();
  const bytes = Buffer.from(`${JSON.stringify({
    schemaVersion: 1,
    kind: 'blockkart.vpakProducerLease',
    pid: process.pid,
    token,
  })}\n`, 'utf8');
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  let descriptor;
  let created = false;
  try {
    try {
      descriptor = openSync(
        leasePath,
        fsConstants.O_RDWR | fsConstants.O_CREAT | fsConstants.O_EXCL | noFollow,
        0o600,
      );
      created = true;
    } catch (error) {
      if (error?.code === 'EEXIST') {
        throw new Error(
          `BlockKart VPAK production is already leased: ${leasePath}. `
          + 'After confirming that no producer is active, remove the stale lease explicitly.',
        );
      }
      throw error;
    }
    let offset = 0;
    while (offset < bytes.byteLength) {
      const written = writeSync(descriptor, bytes, offset, bytes.byteLength - offset, offset);
      if (written <= 0) throw new Error('BlockKart VPAK producer lease write made no progress');
      offset += written;
    }
    if (process.platform !== 'win32') fchmodSync(descriptor, 0o400);
    fsyncSync(descriptor);
    const metadata = fstatSync(descriptor, { bigint: true });
    const pathMetadata = lstatSync(leasePath, { bigint: true });
    if (
      !metadata.isFile()
      || metadata.isSymbolicLink()
      || metadata.nlink !== 1n
      || metadata.size !== BigInt(bytes.byteLength)
      || !sameStat(metadata, pathMetadata)
    ) {
      throw new Error('BlockKart VPAK producer lease changed while it was acquired');
    }
    syncDirectory(parent);
    return { bytes, descriptor, leasePath, metadata, parent, token };
  } catch (error) {
    if (descriptor !== undefined) closeSync(descriptor);
    if (created) {
      try {
        unlinkSync(leasePath);
        syncDirectory(parent);
      } catch (cleanupError) {
        if (cleanupError?.code !== 'ENOENT') {
          throw new AggregateError(
            [error, cleanupError],
            'could not acquire and clean up the BlockKart VPAK producer lease',
          );
        }
      }
    }
    throw error;
  }
}

function assertProducerLease(lease) {
  const opened = fstatSync(lease.descriptor, { bigint: true });
  let current;
  try {
    current = lstatSync(lease.leasePath, { bigint: true });
  } catch (error) {
    if (error?.code === 'ENOENT') throw new Error('BlockKart VPAK producer lease disappeared');
    throw error;
  }
  if (
    opened.nlink !== 1n
    || !sameStat(lease.metadata, opened)
    || !sameStat(opened, current)
  ) {
    throw new Error('BlockKart VPAK producer lease was replaced or modified');
  }
  const bytes = readDescriptorExactly(
    lease.descriptor,
    Number(opened.size),
    'BlockKart VPAK producer lease',
  );
  if (!bytes.equals(lease.bytes)) {
    throw new Error('BlockKart VPAK producer lease contents changed');
  }
}

function releaseProducerLease(lease) {
  let ownershipError = null;
  try {
    assertProducerLease(lease);
  } catch (error) {
    ownershipError = error;
  }
  closeSync(lease.descriptor);
  lease.descriptor = undefined;
  if (ownershipError !== null) throw ownershipError;
  unlinkSync(lease.leasePath);
  syncDirectory(lease.parent);
}

function assertSameSnapshot(left, right, label) {
  if (
    left.digest !== right.digest
    || left.size !== right.size
    || !left.bytes.equals(right.bytes)
  ) {
    throw new Error(`${label} changed across its authenticated observation window`);
  }
}

function digestRegularFile(file, label, maxBytes = 128 * 1024 * 1024) {
  if (!sameNativePath(realpathSync.native(file), file)) {
    throw new Error(`${label} must not traverse symbolic-link path components`);
  }
  const before = lstatSync(file, { bigint: true });
  if (
    !before.isFile()
    || before.isSymbolicLink()
    || before.size > BigInt(maxBytes)
    || before.size > BigInt(Number.MAX_SAFE_INTEGER)
  ) {
    throw new Error(`${label} must be a bounded regular file`);
  }
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const descriptor = openSync(file, fsConstants.O_RDONLY | noFollow);
  try {
    const opened = fstatSync(descriptor, { bigint: true });
    if (!opened.isFile() || !sameStat(before, opened)) {
      throw new Error(`${label} changed before it was read`);
    }
    const bytes = Buffer.allocUnsafe(Number(opened.size));
    let offset = 0;
    while (offset < bytes.byteLength) {
      const count = readSync(descriptor, bytes, offset, bytes.byteLength - offset, offset);
      if (count === 0) throw new Error(`${label} was truncated while it was read`);
      offset += count;
    }
    const extra = Buffer.allocUnsafe(1);
    if (readSync(descriptor, extra, 0, 1, bytes.byteLength) !== 0) {
      throw new Error(`${label} grew while it was read`);
    }
    const openedAfter = fstatSync(descriptor, { bigint: true });
    const pathAfter = lstatSync(file, { bigint: true });
    if (!sameStat(opened, openedAfter) || !sameStat(opened, pathAfter)) {
      throw new Error(`${label} changed while it was read`);
    }
    return {
      bytes,
      digest: `sha256:${createHash('sha256').update(bytes).digest('hex')}`,
      size: bytes.byteLength,
    };
  } finally {
    closeSync(descriptor);
  }
}

function writeBytesAtomically(destination, bytes, mode, label) {
  const output = prepareOutputDirectory();
  if (path.dirname(destination) !== output) {
    throw new Error(`${label} must be published directly inside the BlockKart VPAK output directory`);
  }
  const temporary = path.join(output, `.${path.basename(destination)}.${process.pid}.${randomUUID()}.tmp`);
  let descriptor;
  try {
    descriptor = openSync(
      temporary,
      fsConstants.O_WRONLY | fsConstants.O_CREAT | fsConstants.O_EXCL,
      mode,
    );
    let offset = 0;
    while (offset < bytes.byteLength) {
      const written = writeSync(descriptor, bytes, offset, bytes.byteLength - offset, offset);
      if (written <= 0) throw new Error(`${label} atomic write made no progress`);
      offset += written;
    }
    fchmodSync(descriptor, mode);
    fsyncSync(descriptor);
    closeSync(descriptor);
    descriptor = undefined;
    renameSync(temporary, destination);
    if (process.platform !== 'win32') {
      const directoryDescriptor = openSync(output, fsConstants.O_RDONLY);
      try {
        fsyncSync(directoryDescriptor);
      } finally {
        closeSync(directoryDescriptor);
      }
    }
  } finally {
    if (descriptor !== undefined) closeSync(descriptor);
    try {
      unlinkSync(temporary);
    } catch (error) {
      if (error?.code !== 'ENOENT') throw error;
    }
  }
}

function publishVoBinary(source) {
  const sourceFact = digestRegularFile(
    source,
    'fresh Vo CLI producer binary',
    MAX_VO_BINARY_BYTES,
  );
  writeBytesAtomically(voBin, sourceFact.bytes, 0o755, 'Vo CLI producer binary');
  const published = digestRegularFile(
    voBin,
    'published Vo CLI producer binary',
    MAX_VO_BINARY_BYTES,
  );
  if (sourceFact.digest !== published.digest || sourceFact.size !== published.size) {
    throw new Error('published Vo CLI producer binary differs from the fresh Cargo output');
  }
  return published;
}

function writeReportAtomically(report) {
  const bytes = Buffer.from(`${JSON.stringify(report, null, 2)}\n`, 'utf8');
  if (bytes.byteLength > MAX_REPORT_BYTES) {
    throw new Error(`BlockKart VPAK build report exceeds the ${MAX_REPORT_BYTES}-byte limit`);
  }
  writeBytesAtomically(
    reportPath,
    bytes,
    0o644,
    'BlockKart VPAK build report',
  );
}

function removeOwnedOutputFile(file, label) {
  const output = prepareOutputDirectory();
  if (path.dirname(file) !== output) throw new Error(`${label} is outside the task output directory`);
  let metadata;
  try {
    metadata = lstatSync(file, { bigint: true });
  } catch (error) {
    if (error?.code === 'ENOENT') return;
    throw error;
  }
  if (metadata.isDirectory()) throw new Error(`${label} must not be a directory: ${file}`);
  unlinkSync(file);
}

function writeFreshFile(destination, bytes) {
  let descriptor;
  try {
    descriptor = openSync(
      destination,
      fsConstants.O_WRONLY | fsConstants.O_CREAT | fsConstants.O_EXCL,
      0o644,
    );
    let offset = 0;
    while (offset < bytes.byteLength) {
      const written = writeSync(descriptor, bytes, offset, bytes.byteLength - offset, offset);
      if (written <= 0) throw new Error(`Cargo seed copy made no progress: ${destination}`);
      offset += written;
    }
    fchmodSync(descriptor, 0o644);
    fsyncSync(descriptor);
    closeSync(descriptor);
    descriptor = undefined;
  } finally {
    if (descriptor !== undefined) closeSync(descriptor);
  }
}

function copyCargoSeedEntry(source, destination, relative, state, depth = 0) {
  if (depth > MAX_CARGO_SEED_DEPTH) {
    throw new Error(`Cargo seed exceeds the ${MAX_CARGO_SEED_DEPTH}-level depth limit`);
  }
  const pathBytes = Buffer.byteLength(relative, 'utf8');
  state.entries += 1;
  state.pathBytes += pathBytes;
  if (
    !Number.isSafeInteger(state.entries)
    || state.entries > MAX_CARGO_SEED_ENTRIES
    || !Number.isSafeInteger(state.pathBytes)
    || state.pathBytes > MAX_CARGO_SEED_PATH_BYTES
  ) {
    throw new Error('Cargo seed exceeds its traversal or aggregate path limit');
  }
  if (!sameNativePath(realpathSync.native(source), source)) {
    throw new Error(`Cargo seed source traverses a symbolic link: ${source}`);
  }
  const before = lstatSync(source, { bigint: true });
  if (before.isSymbolicLink()) {
    throw new Error(`Cargo seed source contains a symbolic link: ${source}`);
  }
  if (before.isFile()) {
    const sourceFact = digestRegularFile(source, `Cargo seed ${relative}`);
    state.bytes += sourceFact.size;
    if (!Number.isSafeInteger(state.bytes) || state.bytes > MAX_CARGO_SEED_BYTES) {
      throw new Error(`Cargo seed exceeds the ${MAX_CARGO_SEED_BYTES}-byte aggregate limit`);
    }
    writeFreshFile(destination, sourceFact.bytes);
    const copiedFact = digestRegularFile(destination, `copied Cargo seed ${relative}`);
    if (sourceFact.digest !== copiedFact.digest || sourceFact.size !== copiedFact.size) {
      throw new Error(`Cargo seed copy changed bytes: ${relative}`);
    }
    return;
  }
  if (!before.isDirectory()) {
    throw new Error(`Cargo seed source contains a special filesystem entry: ${source}`);
  }
  mkdirSync(destination, { mode: 0o755 });
  const entries = readdirSync(source, { withFileTypes: true })
    .sort((left, right) => Buffer.compare(Buffer.from(left.name), Buffer.from(right.name)));
  for (const entry of entries) {
    copyCargoSeedEntry(
      path.join(source, entry.name),
      path.join(destination, entry.name),
      `${relative}/${entry.name}`,
      state,
      depth + 1,
    );
  }
  const after = lstatSync(source, { bigint: true });
  if (!sameStat(before, after)) {
    throw new Error(`Cargo seed source directory changed while it was copied: ${source}`);
  }
}

function registryArchiveChecksums(lockBytes) {
  const source = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true }).decode(lockBytes);
  const checksums = new Map();
  let cargoPackage = null;
  const finishPackage = () => {
    if (!cargoPackage?.source?.startsWith('registry+')) return;
    if (
      typeof cargoPackage.name !== 'string'
      || typeof cargoPackage.version !== 'string'
      || !/^[0-9a-f]{64}$/u.test(cargoPackage.checksum ?? '')
      || !/^[A-Za-z0-9_.+-]+$/u.test(cargoPackage.name)
      || !/^[A-Za-z0-9_.+-]+$/u.test(cargoPackage.version)
    ) {
      throw new Error(`Cargo.lock contains an invalid registry package record: ${JSON.stringify(cargoPackage)}`);
    }
    const archive = `${cargoPackage.name}-${cargoPackage.version}.crate`;
    const previous = checksums.get(archive);
    if (previous !== undefined && previous !== cargoPackage.checksum) {
      throw new Error(`Cargo.lock assigns conflicting checksums to ${archive}`);
    }
    checksums.set(archive, cargoPackage.checksum);
  };
  for (const line of source.split('\n')) {
    if (line === '[[package]]') {
      finishPackage();
      cargoPackage = {};
      continue;
    }
    if (cargoPackage === null) continue;
    const match = line.match(/^(name|version|source|checksum) = ("(?:[^"\\]|\\.)*")$/u);
    if (!match) continue;
    if (Object.hasOwn(cargoPackage, match[1])) {
      throw new Error(`Cargo.lock package repeats ${match[1]}`);
    }
    cargoPackage[match[1]] = parseBoundedStrictJsonBytes(
      Buffer.from(match[2], 'utf8'),
      `Cargo.lock package ${match[1]}`,
      { maxBytes: 16 * 1024, maxDepth: 2, maxTokens: 4, maxObjectKeys: 1, maxObjectKeyBytes: 1 },
    );
  }
  finishPackage();
  if (checksums.size === 0) throw new Error('Cargo.lock contains no registry package checksums');
  return checksums;
}

function verifyCopiedRegistryArchives(cargoHome, lockBytes = null) {
  const authenticatedLockBytes = lockBytes ?? digestRegularFile(
    path.join(root, 'Cargo.lock'),
    'Volang Cargo.lock',
    MAX_REPORT_BYTES,
  ).bytes;
  const expected = registryArchiveChecksums(authenticatedLockBytes);
  const cacheRoot = path.join(cargoHome, 'registry', 'cache');
  const verified = new Map();
  for (const registry of readdirSync(cacheRoot, { withFileTypes: true })) {
    if (!registry.isDirectory()) {
      throw new Error(`Cargo registry cache contains a non-directory entry: ${registry.name}`);
    }
    const registryRoot = path.join(cacheRoot, registry.name);
    for (const archive of readdirSync(registryRoot, { withFileTypes: true })) {
      if (!archive.isFile()) {
        throw new Error(`Cargo registry cache contains a non-file entry: ${registry.name}/${archive.name}`);
      }
      const checksum = expected.get(archive.name);
      const archivePath = path.join(registryRoot, archive.name);
      if (checksum === undefined) {
        // The task Cargo home must contain code authorized by this Cargo.lock.
        unlinkSync(archivePath);
        continue;
      }
      const fact = digestRegularFile(
        archivePath,
        `locked Cargo registry archive ${archive.name}`,
      );
      if (fact.digest !== `sha256:${checksum}`) {
        throw new Error(`Cargo registry archive checksum does not match Cargo.lock: ${archive.name}`);
      }
      const key = `${registry.name}/${archive.name}`;
      if (verified.has(key)) throw new Error(`Cargo seed repeats authenticated archive ${key}`);
      verified.set(key, { digest: fact.digest, size: fact.size });
    }
  }
  if (verified.size === 0) throw new Error('Cargo seed contains no Cargo.lock-authenticated registry archives');
  return verified;
}

function hashLengthPrefixed(hash, bytes) {
  const length = Buffer.allocUnsafe(8);
  length.writeBigUInt64BE(BigInt(bytes.byteLength));
  hash.update(length);
  hash.update(bytes);
}

function snapshotExtractedSourceEntry(candidate, relative, state, depth = 0) {
  if (depth > MAX_CARGO_SEED_DEPTH) {
    throw new Error(`Cargo registry source exceeds the ${MAX_CARGO_SEED_DEPTH}-level depth limit`);
  }
  if (!sameNativePath(realpathSync.native(candidate), candidate)) {
    throw new Error(`Cargo registry source traverses a symbolic link: ${candidate}`);
  }
  const before = lstatSync(candidate, { bigint: true });
  if (before.isSymbolicLink()) throw new Error(`Cargo registry source contains a symbolic link: ${candidate}`);
  state.entries += 1;
  state.pathBytes += Buffer.byteLength(relative, 'utf8');
  if (
    !Number.isSafeInteger(state.entries)
    || state.entries > MAX_CARGO_SEED_ENTRIES
    || !Number.isSafeInteger(state.pathBytes)
    || state.pathBytes > MAX_CARGO_SEED_PATH_BYTES
  ) {
    throw new Error('Cargo registry source exceeds its traversal or aggregate path limit');
  }
  const relativeBytes = Buffer.from(relative, 'utf8');
  const permissions = Buffer.from((before.mode & 0o777n).toString(8), 'ascii');
  if (before.isFile()) {
    if (before.nlink !== 1n) throw new Error(`Cargo registry source file must be singly linked: ${candidate}`);
    const fact = digestRegularFile(candidate, `Cargo registry source ${relative}`, MAX_CARGO_SEED_BYTES);
    state.bytes += fact.size;
    if (!Number.isSafeInteger(state.bytes) || state.bytes > MAX_CARGO_SEED_BYTES) {
      throw new Error(`Cargo registry source exceeds the ${MAX_CARGO_SEED_BYTES}-byte aggregate limit`);
    }
    state.packageHash.update(Buffer.from([0x66]));
    hashLengthPrefixed(state.packageHash, relativeBytes);
    hashLengthPrefixed(state.packageHash, permissions);
    hashLengthPrefixed(state.packageHash, Buffer.from(fact.digest, 'ascii'));
    hashLengthPrefixed(state.packageHash, Buffer.from(String(fact.size), 'ascii'));
    return;
  }
  if (!before.isDirectory()) throw new Error(`Cargo registry source contains a special entry: ${candidate}`);
  state.packageHash.update(Buffer.from([0x64]));
  hashLengthPrefixed(state.packageHash, relativeBytes);
  hashLengthPrefixed(state.packageHash, permissions);
  const entries = readdirSync(candidate, { withFileTypes: true })
    .sort((left, right) => Buffer.compare(Buffer.from(left.name), Buffer.from(right.name)));
  for (const entry of entries) {
    const childRelative = relative === '' ? entry.name : `${relative}/${entry.name}`;
    snapshotExtractedSourceEntry(
      path.join(candidate, entry.name),
      childRelative,
      state,
      depth + 1,
    );
  }
  const after = lstatSync(candidate, { bigint: true });
  if (!sameStat(before, after)) throw new Error(`Cargo registry source directory changed: ${candidate}`);
}

function snapshotExtractedPackageSource(packageRoot, aggregateState) {
  const startEntries = aggregateState.entries;
  const startBytes = aggregateState.bytes;
  const packageState = {
    ...aggregateState,
    packageHash: createHash('sha256'),
  };
  snapshotExtractedSourceEntry(packageRoot, '', packageState);
  aggregateState.bytes = packageState.bytes;
  aggregateState.entries = packageState.entries;
  aggregateState.pathBytes = packageState.pathBytes;
  return {
    digest: `sha256:${packageState.packageHash.digest('hex')}`,
    entries: packageState.entries - startEntries,
    size: packageState.bytes - startBytes,
  };
}

function verifyExtractedRegistrySources(cargoHome, authenticatedArchives, label) {
  const sourceRoot = path.join(cargoHome, 'registry', 'src');
  const registries = readdirSync(sourceRoot, { withFileTypes: true })
    .sort((left, right) => Buffer.compare(Buffer.from(left.name), Buffer.from(right.name)));
  const extracted = new Map();
  const aggregateState = { bytes: 0, entries: 0, pathBytes: 0 };
  for (const registry of registries) {
    if (!registry.isDirectory()) {
      throw new Error(`${label} registry source root contains a non-directory: ${registry.name}`);
    }
    const registryRoot = path.join(sourceRoot, registry.name);
    if (!sameNativePath(realpathSync.native(registryRoot), registryRoot)) {
      throw new Error(`${label} registry source root traverses a symbolic link: ${registry.name}`);
    }
    const cargoPackages = readdirSync(registryRoot, { withFileTypes: true })
      .sort((left, right) => Buffer.compare(Buffer.from(left.name), Buffer.from(right.name)));
    for (const cargoPackage of cargoPackages) {
      if (!cargoPackage.isDirectory()) {
        throw new Error(`${label} registry source contains a non-directory: ${cargoPackage.name}`);
      }
      const packageRoot = path.join(registryRoot, cargoPackage.name);
      if (!sameNativePath(realpathSync.native(packageRoot), packageRoot)) {
        throw new Error(`${label} registry package traverses a symbolic link: ${cargoPackage.name}`);
      }
      const archiveKey = `${registry.name}/${cargoPackage.name}.crate`;
      if (!authenticatedArchives.has(archiveKey)) {
        throw new Error(
          `${label} extracted a registry package without a Cargo.lock-authenticated archive: `
          + cargoPackage.name,
        );
      }
      const marker = digestRegularFile(
        path.join(packageRoot, '.cargo-ok'),
        `${label} registry package completion marker ${cargoPackage.name}`,
        1024,
      );
      const markerValue = parseBoundedStrictJsonBytes(
        marker.bytes,
        `${label} registry package completion marker ${cargoPackage.name}`,
        { maxBytes: 1024, maxDepth: 4, maxTokens: 16, maxObjectKeys: 4, maxObjectKeyBytes: 16 },
      );
      if (
        markerValue === null
        || typeof markerValue !== 'object'
        || Array.isArray(markerValue)
        || JSON.stringify(Object.keys(markerValue).sort()) !== JSON.stringify(['v'])
        || markerValue.v !== 1
      ) {
        throw new Error(`${label} registry package has an invalid Cargo completion marker: ${cargoPackage.name}`);
      }
      const archive = authenticatedArchives.get(archiveKey);
      extracted.set(archiveKey, {
        archiveDigest: archive.digest,
        archiveSize: archive.size,
        markerDigest: marker.digest,
        markerSize: marker.size,
        source: snapshotExtractedPackageSource(packageRoot, aggregateState),
      });
    }
  }
  if (extracted.size === 0) throw new Error(`${label} extracted no authenticated registry sources`);
  return extracted;
}

function assertFactMapsEqual(left, right, label) {
  if (left.size !== right.size) throw new Error(`${label} set changed`);
  for (const [key, fact] of left) {
    const current = right.get(key);
    if (current === undefined || JSON.stringify(fact) !== JSON.stringify(current)) {
      throw new Error(`${label} changed: ${key}`);
    }
  }
}

function prepareTaskCargoHome(ambientCargoHome, taskCargoHome) {
  const state = { bytes: 0, entries: 0, pathBytes: 0 };
  mkdirSync(path.join(taskCargoHome, 'registry'), { mode: 0o755 });
  for (const component of ['cache', 'index']) {
    copyCargoSeedEntry(
      path.join(ambientCargoHome, 'registry', component),
      path.join(taskCargoHome, 'registry', component),
      `registry/${component}`,
      state,
    );
  }
  const verifiedArchives = verifyCopiedRegistryArchives(taskCargoHome);
  console.log(
    `blockkart vpak Cargo seed: ok entries=${state.entries} bytes=${state.bytes} `
    + `lockedArchives=${verifiedArchives.size}`,
  );
  return verifiedArchives;
}

function isPlainObject(value) {
  if (value === null || typeof value !== 'object' || Array.isArray(value)) return false;
  const prototype = Object.getPrototypeOf(value);
  return prototype === Object.prototype || prototype === null;
}

function parseProducerManifestBytes(bytes, label) {
  const producer = parseBoundedStrictJsonBytes(bytes, label, {
    maxBytes: MAX_REPORT_BYTES,
    maxDepth: 127,
    maxTokens: 1_000_000,
    maxObjectKeys: 200_000,
    maxObjectKeyBytes: MAX_REPORT_BYTES,
  });
  if (!isPlainObject(producer)) throw new Error(`${label} must contain a JSON object`);
  if (
    producer.schemaVersion !== 1
    || producer.kind !== 'blockkart.vpakProducerManifest'
    || producer.owner !== 'BlockKart'
  ) {
    throw new Error(`${label} identity is invalid`);
  }
  if (JSON.stringify(producer.command) !== JSON.stringify(VO_CLI_GUEST_ENVIRONMENT.command)) {
    throw new Error(`${label} command does not match the authenticated Vo CLI guest command`);
  }
  if (
    producer.archiveEntryCount !== 37
    || producer.payloadInputCount !== 37
    || !Number.isSafeInteger(producer.workspaceSourceInputCount)
    || producer.workspaceSourceInputCount <= 0
  ) {
    throw new Error(
      'BlockKart vpak closure must contain 37 payloads and current workspace sources, '
      + `found entries=${producer.archiveEntryCount} inputs=${producer.payloadInputCount} `
      + `workspace=${producer.workspaceSourceInputCount ?? 0}`,
    );
  }
  if (
    !isPlainObject(producer.pack)
    || JSON.stringify(Object.keys(producer.pack).sort()) !== JSON.stringify(['path', 'sha256', 'size'])
    || producer.pack.path !== 'assets/blockkart.vpak'
    || !/^[0-9a-f]{64}$/u.test(producer.pack.sha256 ?? '')
    || !Number.isSafeInteger(producer.pack.size)
    || producer.pack.size <= 0
    || producer.pack.size > MAX_VPAK_BYTES
    || !/^[0-9a-f]{64}$/u.test(producer.producerDigest ?? '')
  ) {
    throw new Error(`${label} pack or producer digest identity is invalid`);
  }
  return producer;
}

function observeProducerState(manifestPath, label) {
  const manifest = digestRegularFile(manifestPath, `${label} manifest`, MAX_REPORT_BYTES);
  const producer = parseProducerManifestBytes(manifest.bytes, `${label} manifest`);
  const pack = digestRegularFile(
    path.join(blockKartRoot, producer.pack.path),
    `${label} VPAK`,
    MAX_VPAK_BYTES,
  );
  if (pack.digest !== `sha256:${producer.pack.sha256}` || pack.size !== producer.pack.size) {
    throw new Error(`${label} VPAK bytes do not match the producer manifest`);
  }
  return { manifest, pack, producer };
}

function assertSameProducerState(left, right, label) {
  assertSameSnapshot(left.manifest, right.manifest, `${label} manifest`);
  assertSameSnapshot(left.pack, right.pack, `${label} VPAK`);
}

function runVpakProvenanceCheck(guestEnvironment, lease) {
  assertProducerLease(lease);
  execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
    cwd: blockKartRoot,
    env: guestEnvironment,
    stdio: 'inherit',
    timeout: VPAK_GUEST_TIMEOUT_MS,
  });
  assertProducerLease(lease);
}

function runProtocolSelftest() {
  const fixture = mkdtempSync(path.join(targetRoot, '.blockkart-vpak-protocol-selftest-'));
  try {
    assert.throws(
      () => parseProducerManifestBytes(
        Buffer.from('{"schemaVersion":1,"schemaVersion":1}'),
        'duplicate-key producer fixture',
      ),
      /duplicate object key "schemaVersion"/,
    );
    assert.throws(
      () => parseProducerManifestBytes(Buffer.from([0xff]), 'invalid UTF-8 producer fixture'),
      /must be valid UTF-8/,
    );

    const leaseRoot = path.join(fixture, 'lease-root');
    mkdirSync(leaseRoot, { mode: 0o755 });
    const leasePath = path.join(leaseRoot, 'target', 'producer.lock');
    const lease = acquireProducerLease(leasePath, leaseRoot);
    assertProducerLease(lease);
    assert.throws(
      () => acquireProducerLease(leasePath, leaseRoot),
      /already leased/,
    );
    releaseProducerLease(lease);
    const reacquired = acquireProducerLease(leasePath, leaseRoot);
    releaseProducerLease(reacquired);

    const cargoHome = path.join(fixture, 'cargo-home');
    const registryName = 'index.example.invalid-0123456789abcdef';
    const cacheRoot = path.join(cargoHome, 'registry', 'cache', registryName);
    const sourceRoot = path.join(cargoHome, 'registry', 'src', registryName, 'fixture-1.0.0');
    mkdirSync(cacheRoot, { recursive: true, mode: 0o755 });
    mkdirSync(sourceRoot, { recursive: true, mode: 0o755 });
    const archiveBytes = Buffer.from('authenticated Cargo archive fixture', 'utf8');
    const archiveChecksum = createHash('sha256').update(archiveBytes).digest('hex');
    writeFreshFile(path.join(cacheRoot, 'fixture-1.0.0.crate'), archiveBytes);
    writeFreshFile(path.join(cacheRoot, 'unlocked-9.9.9.crate'), Buffer.from('unlocked'));
    writeFreshFile(path.join(sourceRoot, '.cargo-ok'), Buffer.from('{"v":1}', 'utf8'));
    writeFreshFile(path.join(sourceRoot, 'lib.rs'), Buffer.from('pub fn fixture() {}\n', 'utf8'));
    const lockBytes = Buffer.from(
      `version = 4\n\n[[package]]\nname = "fixture"\nversion = "1.0.0"\n`
      + `source = "registry+https://example.invalid/index"\nchecksum = "${archiveChecksum}"\n`,
      'utf8',
    );
    const authenticated = verifyCopiedRegistryArchives(cargoHome, lockBytes);
    assert.equal(authenticated.size, 1);
    assert.throws(
      () => lstatSync(path.join(cacheRoot, 'unlocked-9.9.9.crate')),
      (error) => error?.code === 'ENOENT',
    );
    const extractedBefore = verifyExtractedRegistrySources(
      cargoHome,
      authenticated,
      'registry protocol selftest',
    );
    unlinkSync(path.join(sourceRoot, 'lib.rs'));
    writeFreshFile(path.join(sourceRoot, 'lib.rs'), Buffer.from('pub fn drifted() {}\n', 'utf8'));
    const extractedAfter = verifyExtractedRegistrySources(
      cargoHome,
      authenticated,
      'registry protocol selftest after mutation',
    );
    assert.throws(
      () => assertFactMapsEqual(extractedBefore, extractedAfter, 'registry source fixture'),
      /registry source fixture changed/,
    );
    const wrongRegistrySource = path.join(
      cargoHome,
      'registry',
      'src',
      'other.example.invalid-fedcba9876543210',
      'fixture-1.0.0',
    );
    mkdirSync(wrongRegistrySource, { recursive: true, mode: 0o755 });
    writeFreshFile(path.join(wrongRegistrySource, '.cargo-ok'), Buffer.from('{"v":1}', 'utf8'));
    assert.throws(
      () => verifyExtractedRegistrySources(
        cargoHome,
        authenticated,
        'registry slot mismatch selftest',
      ),
      /without a Cargo.lock-authenticated archive/,
    );

    const same = { bytes: Buffer.from('same'), digest: 'sha256:same', size: 4 };
    assertSameSnapshot(same, { ...same, bytes: Buffer.from('same') }, 'snapshot fixture');
    assert.throws(
      () => assertSameSnapshot(
        same,
        { bytes: Buffer.from('drift'), digest: 'sha256:drift', size: 5 },
        'snapshot fixture',
      ),
      /changed across its authenticated observation window/,
    );
  } finally {
    rmSync(fixture, { recursive: true, force: true });
  }
  console.log('blockkart vpak build protocol selftest: ok');
}

function runBuild() {
  prepareOutputDirectory();
  runProtocolSelftest();
  const lease = acquireProducerLease();
  let freshCargoTarget = null;
  let freshCargoHome = null;
  let completed = false;
  let failure = null;
  try {
    assertProducerLease(lease);
    rmSync(guestRoot, { force: true, recursive: true });
    removeOwnedOutputFile(reportPath, 'BlockKart VPAK build report');
    removeOwnedOutputFile(voBin, 'Vo CLI producer binary');
    freshCargoTarget = mkdtempSync(path.join(targetRoot, '.blockkart-vpak-cargo-'));

    const toolchain = currentVoCliToolchain(root);
    const ambientCargoEnvironment = voCliBuildEnvironment(process.env, root);
    freshCargoHome = mkdtempSync(path.join(targetRoot, '.blockkart-vpak-cargo-home-'));
    const authenticatedArchives = prepareTaskCargoHome(
      ambientCargoEnvironment.CARGO_HOME,
      freshCargoHome,
    );
    const preflightInputs = currentVoCliBuildInputs(root, freshCargoHome);
    const metadataSources = verifyExtractedRegistrySources(
      freshCargoHome,
      authenticatedArchives,
      'Vo CLI metadata',
    );
    const cargoEnvironment = voCliBuildEnvironment(
      process.env,
      root,
      freshCargoTarget,
      freshCargoHome,
    );
    const buildCommand = voCliBuildCommand(toolchain.host);
    assertProducerLease(lease);
    execFileSync(buildCommand[0], buildCommand.slice(1), {
      cwd: root,
      env: cargoEnvironment,
      stdio: 'inherit',
      timeout: CARGO_BUILD_TIMEOUT_MS,
    });
    assertProducerLease(lease);
    const archivesAfterBuild = verifyCopiedRegistryArchives(freshCargoHome);
    assertFactMapsEqual(
      authenticatedArchives,
      archivesAfterBuild,
      'Cargo.lock-authenticated registry archive closure',
    );
    const buildSources = verifyExtractedRegistrySources(
      freshCargoHome,
      archivesAfterBuild,
      'Vo CLI build',
    );
    assertFactMapsEqual(
      metadataSources,
      buildSources,
      'Cargo registry source closure across the Vo CLI build',
    );
    assertVoCliCargoConfigBoundary(root, cargoEnvironment);
    const freshVoBin = path.join(
      freshCargoTarget,
      toolchain.host,
      'release-native',
      process.platform === 'win32' ? 'vo.exe' : 'vo',
    );
    const voBinaryBeforeGuest = publishVoBinary(freshVoBin);
    const guestEnvironment = voCliGuestEnvironment(process.env, root, voBin);
    for (const directory of [
      guestEnvironment.HOME,
      guestEnvironment.TMPDIR,
      guestEnvironment.VO_MOD_CACHE,
      guestEnvironment.XDG_CACHE_HOME,
    ]) {
      ensureRealChildDirectory(directory, 'Vo CLI guest directory');
    }
    assertProducerLease(lease);
    execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--build'], {
      cwd: blockKartRoot,
      env: guestEnvironment,
      stdio: 'inherit',
      timeout: VPAK_GUEST_TIMEOUT_MS,
    });
    runVpakProvenanceCheck(guestEnvironment, lease);

    const voBinaryAfterGuest = digestRegularFile(
      voBin,
      'Vo CLI producer binary after guest execution',
      MAX_VO_BINARY_BYTES,
    );
    assertSameSnapshot(
      voBinaryBeforeGuest,
      voBinaryAfterGuest,
      'Vo CLI producer binary across BlockKart VPAK generation',
    );
    const producerManifestPath = path.join(blockKartRoot, 'assets/blockkart.vpak.provenance.json');
    const producerAfterFirstCheck = observeProducerState(
      producerManifestPath,
      'BlockKart VPAK producer after first provenance check',
    );
    runVpakProvenanceCheck(guestEnvironment, lease);
    const producerAfterSecondCheck = observeProducerState(
      producerManifestPath,
      'BlockKart VPAK producer after second provenance check',
    );
    assertSameProducerState(
      producerAfterFirstCheck,
      producerAfterSecondCheck,
      'BlockKart VPAK producer across the second provenance check',
    );

    const postflightInputs = currentVoCliBuildInputs(root, freshCargoHome);
    assertVoCliBuildInputs(preflightInputs, {
      expected: postflightInputs,
      label: 'Vo CLI inputs across BlockKart VPAK production',
    });
    const finalArchives = verifyCopiedRegistryArchives(freshCargoHome);
    assertFactMapsEqual(
      authenticatedArchives,
      finalArchives,
      'Cargo.lock-authenticated registry archive closure after VPAK production',
    );
    const finalSources = verifyExtractedRegistrySources(
      freshCargoHome,
      finalArchives,
      'Vo CLI postflight',
    );
    assertFactMapsEqual(
      metadataSources,
      finalSources,
      'Cargo registry source closure across VPAK production',
    );
    const producerBeforeReport = observeProducerState(
      producerManifestPath,
      'BlockKart VPAK producer before report publication',
    );
    assertSameProducerState(
      producerAfterSecondCheck,
      producerBeforeReport,
      'BlockKart VPAK producer after provenance validation',
    );
    const voBinaryBeforeReport = digestRegularFile(
      voBin,
      'Vo CLI producer binary before report publication',
      MAX_VO_BINARY_BYTES,
    );
    assertSameSnapshot(
      voBinaryAfterGuest,
      voBinaryBeforeReport,
      'Vo CLI producer binary before report publication',
    );
    const voBinary = {
      path: path.relative(root, voBin).split(path.sep).join('/'),
      digest: voBinaryBeforeReport.digest,
      size: voBinaryBeforeReport.size,
    };
    assertProducerLease(lease);
    writeReportAtomically({
      schemaVersion: 2,
      kind: 'blockkart.vpakBuildReport',
      ciRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
      toolchain,
      voCliBuildInputs: postflightInputs,
      voBinary,
      voCliExecutionDigest: voCliExecutionDigest(postflightInputs, toolchain, voBinary),
      producerManifest: {
        path: 'assets/blockkart.vpak.provenance.json',
        digest: producerBeforeReport.manifest.digest,
        size: producerBeforeReport.manifest.size,
      },
      archiveEntryCount: producerBeforeReport.producer.archiveEntryCount,
      payloadInputCount: producerBeforeReport.producer.payloadInputCount,
      workspaceSourceInputCount: producerBeforeReport.producer.workspaceSourceInputCount,
      producerDigest: producerBeforeReport.producer.producerDigest,
      pack: producerBeforeReport.producer.pack,
    });
    const producerAfterReport = observeProducerState(
      producerManifestPath,
      'BlockKart VPAK producer after report publication',
    );
    assertSameProducerState(
      producerBeforeReport,
      producerAfterReport,
      'BlockKart VPAK producer across report publication',
    );
    assertProducerLease(lease);
    completed = true;
    console.log(
      `blockkart vpak build: ok entries=${producerBeforeReport.producer.archiveEntryCount} `
      + `producer=${producerBeforeReport.producer.producerDigest}`,
    );
  } catch (error) {
    failure = error;
  }

  const cleanupFailures = [];
  const clean = (action) => {
    try {
      action();
    } catch (error) {
      cleanupFailures.push(error);
    }
  };
  if (freshCargoTarget !== null) {
    clean(() => rmSync(freshCargoTarget, { force: true, recursive: true }));
  }
  if (freshCargoHome !== null) {
    clean(() => rmSync(freshCargoHome, { force: true, recursive: true }));
  }
  clean(() => rmSync(guestRoot, { force: true, recursive: true }));
  if (!completed) {
    clean(() => removeOwnedOutputFile(reportPath, 'BlockKart VPAK build report'));
    clean(() => removeOwnedOutputFile(voBin, 'Vo CLI producer binary'));
  }
  clean(() => releaseProducerLease(lease));

  const failures = failure === null ? cleanupFailures : [failure, ...cleanupFailures];
  if (failures.length === 1) throw failures[0];
  if (failures.length > 1) {
    throw new AggregateError(failures, 'BlockKart VPAK build and cleanup reported multiple failures');
  }
}

if (process.argv.length > 3) throw new Error('blockkart_vpak_build accepts at most one mode argument');
const mode = process.argv[2] ?? '--build';
ensureRealChildDirectory(targetRoot, 'Volang Cargo target root');
if (mode === '--self-test') {
  runProtocolSelftest();
} else if (mode === '--build') {
  runBuild();
} else {
  throw new Error(`unknown blockkart_vpak_build mode: ${mode}`);
}
