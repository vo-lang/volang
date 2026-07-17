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
  voCliBuildCommand,
  voCliBuildEnvironment,
  voCliGuestEnvironment,
} from './quickplay_cli_producer_contract.mjs';
import {
  assertSameBlockKartVpakProducerState,
  observeBlockKartVpakProducer,
  parseBlockKartVpakProducerManifestBytes,
  parseBoundedJsonBytes as parseBoundedStrictJsonBytes,
} from './quickplay_vnext.mjs';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const targetRoot = path.join(root, 'target');
const outDir = path.join(targetRoot, 'blockkart-vpak-build');
const voBin = path.join(outDir, process.platform === 'win32' ? 'vo.exe' : 'vo');
const reportPath = path.join(outDir, 'report.json');
const guestRoot = path.join(outDir, 'guest');
const producerLeasePath = path.join(blockKartRoot, 'target', 'blockkart-vpak-producer.lock');
const blockKartAssetsDirectory = path.join(blockKartRoot, 'assets');
const trackedVpakPath = path.join(blockKartAssetsDirectory, 'blockkart.vpak');
const trackedVpakProvenancePath = path.join(
  blockKartAssetsDirectory,
  'blockkart.vpak.provenance.json',
);
const VPAK_TRANSACTION_DIRECTORY_ENV = 'BLOCKKART_VPAK_TRANSACTION_DIR';
const VPAK_TRANSACTION_PREFIX = '.blockkart-vpak-transaction-';
const VPAK_TRANSACTION_NAME = /^\.blockkart-vpak-transaction-[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/u;
const VPAK_TRANSACTION_JOURNAL = 'transaction.json';
const MAX_REPORT_BYTES = 16 * 1024 * 1024;
const MAX_VPAK_BYTES = 64 * 1024 * 1024;
const MAX_VO_BINARY_BYTES = 256 * 1024 * 1024;
const MAX_CARGO_SEED_BYTES = 512 * 1024 * 1024;
const MAX_CARGO_SEED_ENTRIES = 100_000;
const MAX_CARGO_SEED_DEPTH = 64;
const MAX_CARGO_SEED_PATH_BYTES = 8 * 1024 * 1024;
const CARGO_BUILD_TIMEOUT_MS = 25 * 60 * 1000;
const VPAK_GUEST_TIMEOUT_MS = 5 * 60 * 1000;

class VpakCommitUncertainError extends Error {
  constructor(transaction, cause) {
    super(
      `BlockKart VPAK commit marker could not be made durable; recovery journal retained at ${transaction.journal}`,
      { cause },
    );
    this.name = 'VpakCommitUncertainError';
    this.code = 'BLOCKKART_VPAK_COMMIT_UNCERTAIN';
    this.transactionDirectory = transaction.directory;
  }
}

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

function metadataOrNull(candidate) {
  try {
    return lstatSync(candidate, { bigint: true });
  } catch (error) {
    if (error?.code === 'ENOENT') return null;
    throw error;
  }
}

function syncRegularFile(file, label) {
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const descriptor = openSync(file, fsConstants.O_RDONLY | noFollow);
  try {
    const metadata = fstatSync(descriptor, { bigint: true });
    if (!metadata.isFile() || metadata.isSymbolicLink() || metadata.nlink !== 1n) {
      throw new Error(`${label} must be a singly-linked regular file before it is synchronized`);
    }
    fsyncSync(descriptor);
  } finally {
    closeSync(descriptor);
  }
}

function vpakPairFileFact(file, label, maxBytes) {
  const metadata = metadataOrNull(file);
  if (
    metadata === null
    || !metadata.isFile()
    || metadata.isSymbolicLink()
    || metadata.nlink !== 1n
  ) {
    throw new Error(`${label} must be a singly-linked regular file`);
  }
  return digestRegularFile(file, label, maxBytes);
}

function vpakPairFileFactOrNull(file, label, maxBytes) {
  return metadataOrNull(file) === null ? null : vpakPairFileFact(file, label, maxBytes);
}

function journalFact(fact) {
  return { digest: fact.digest, size: fact.size };
}

function validateJournalFact(value, label) {
  if (
    value === null
    || typeof value !== 'object'
    || Array.isArray(value)
    || JSON.stringify(Object.keys(value).sort()) !== JSON.stringify(['digest', 'size'])
    || !/^sha256:[0-9a-f]{64}$/u.test(value.digest ?? '')
    || !Number.isSafeInteger(value.size)
    || value.size < 0
  ) {
    throw new Error(`${label} is invalid`);
  }
  return { digest: value.digest, size: value.size };
}

function assertVpakFileBinding(file, expected, label, maxBytes) {
  const actual = vpakPairFileFact(file, label, maxBytes);
  if (actual.digest !== expected.digest || actual.size !== expected.size) {
    throw new Error(`${label} differs from its transaction journal binding`);
  }
  return actual;
}

function canonicalDirectory(candidate, label) {
  const resolved = path.resolve(candidate);
  const canonical = realpathSync.native(resolved);
  const metadata = lstatSync(resolved, { bigint: true });
  if (
    !sameNativePath(canonical, resolved)
    || !metadata.isDirectory()
    || metadata.isSymbolicLink()
  ) {
    throw new Error(`${label} must be a real canonical directory`);
  }
  return canonical;
}

function vpakPairTransactionFromDirectory(assetsDirectory, transactionDirectory) {
  const assets = canonicalDirectory(assetsDirectory, 'BlockKart VPAK assets directory');
  const transaction = canonicalDirectory(
    transactionDirectory,
    'BlockKart VPAK transaction directory',
  );
  if (
    !sameNativePath(path.dirname(transaction), assets)
    || !VPAK_TRANSACTION_NAME.test(path.basename(transaction))
  ) {
    throw new Error('BlockKart VPAK transaction must be a named direct child of its assets directory');
  }
  return Object.freeze({
    assetsDirectory: assets,
    directory: transaction,
    destinationPack: path.join(assets, 'blockkart.vpak'),
    destinationProvenance: path.join(assets, 'blockkart.vpak.provenance.json'),
    journal: path.join(transaction, VPAK_TRANSACTION_JOURNAL),
    newPack: path.join(transaction, 'blockkart.vpak'),
    newProvenance: path.join(transaction, 'blockkart.vpak.provenance.json'),
    previousPack: path.join(transaction, 'previous.blockkart.vpak'),
    previousProvenance: path.join(transaction, 'previous.blockkart.vpak.provenance.json'),
  });
}

function createVpakPairTransaction(assetsDirectory = blockKartAssetsDirectory) {
  const assets = canonicalDirectory(assetsDirectory, 'BlockKart VPAK assets directory');
  const transactionDirectory = path.join(
    assets,
    `${VPAK_TRANSACTION_PREFIX}${randomUUID()}`,
  );
  mkdirSync(transactionDirectory, { mode: 0o700 });
  const metadata = lstatSync(transactionDirectory, { bigint: true });
  if (!metadata.isDirectory() || metadata.isSymbolicLink()) {
    throw new Error('BlockKart VPAK transaction directory was replaced while it was created');
  }
  syncDirectory(assets);
  return vpakPairTransactionFromDirectory(assets, transactionDirectory);
}

function encodeVpakPairJournal(journal) {
  const bytes = Buffer.from(`${JSON.stringify(journal, null, 2)}\n`, 'utf8');
  if (bytes.byteLength > 64 * 1024) {
    throw new Error('BlockKart VPAK transaction journal exceeds 64 KiB');
  }
  return bytes;
}

function writeVpakPairJournal(transaction, journal, replace = false, operations = {}) {
  const rename = operations.renameSync ?? renameSync;
  const sync = operations.syncDirectory ?? syncDirectory;
  const bytes = encodeVpakPairJournal(journal);
  if (!replace) {
    writeFreshFile(transaction.journal, bytes);
    sync(transaction.directory);
    sync(transaction.assetsDirectory);
    return {
      published: true,
      durable: true,
      postCommitError: null,
    };
  }
  vpakPairFileFact(
    transaction.journal,
    'BlockKart VPAK transaction journal before replacement',
    64 * 1024,
  );
  const temporary = path.join(
    transaction.directory,
    `.transaction.${process.pid}.${randomUUID()}.tmp`,
  );
  let published = false;
  let durable = false;
  let publicationError = null;
  try {
    writeFreshFile(temporary, bytes);
    rename(temporary, transaction.journal);
    published = true;
    sync(transaction.directory);
    durable = true;
    sync(transaction.assetsDirectory);
  } catch (error) {
    publicationError = error;
  } finally {
    try {
      rmSync(temporary, { force: true });
    } catch (error) {
      publicationError = publicationError === null
        ? error
        : new AggregateError(
          [publicationError, error],
          'BlockKart VPAK transaction journal publication and cleanup both failed',
        );
    }
  }
  if (!published && publicationError !== null) throw publicationError;
  return {
    published,
    durable,
    postCommitError: publicationError,
  };
}

function readVpakPairJournal(transaction) {
  const fact = vpakPairFileFact(
    transaction.journal,
    'BlockKart VPAK transaction journal',
    64 * 1024,
  );
  const value = parseBoundedStrictJsonBytes(
    fact.bytes,
    'BlockKart VPAK transaction journal',
    {
      maxBytes: 64 * 1024,
      maxDepth: 8,
      maxTokens: 128,
      maxObjectKeys: 32,
      maxObjectKeyBytes: 256,
    },
  );
  if (
    value === null
    || typeof value !== 'object'
    || Array.isArray(value)
    || JSON.stringify(Object.keys(value).sort())
      !== JSON.stringify(['hadPrevious', 'kind', 'next', 'phase', 'previous', 'schemaVersion'])
    || value.schemaVersion !== 1
    || value.kind !== 'blockkart.vpakPairTransaction'
    || !['prepared', 'committed'].includes(value.phase)
    || typeof value.hadPrevious !== 'boolean'
    || value.next === null
    || typeof value.next !== 'object'
    || Array.isArray(value.next)
    || JSON.stringify(Object.keys(value.next).sort()) !== JSON.stringify(['pack', 'provenance'])
  ) {
    throw new Error('BlockKart VPAK transaction journal shape is invalid');
  }
  const previous = value.previous;
  if (
    value.hadPrevious
      ? (
        previous === null
        || typeof previous !== 'object'
        || Array.isArray(previous)
        || JSON.stringify(Object.keys(previous).sort()) !== JSON.stringify(['pack', 'provenance'])
      )
      : previous !== null
  ) {
    throw new Error('BlockKart VPAK transaction journal previous generation is invalid');
  }
  return Object.freeze({
    schemaVersion: 1,
    kind: 'blockkart.vpakPairTransaction',
    phase: value.phase,
    hadPrevious: value.hadPrevious,
    previous: value.hadPrevious ? {
      pack: validateJournalFact(previous.pack, 'transaction previous pack'),
      provenance: validateJournalFact(previous.provenance, 'transaction previous provenance'),
    } : null,
    next: {
      pack: validateJournalFact(value.next.pack, 'transaction next pack'),
      provenance: validateJournalFact(value.next.provenance, 'transaction next provenance'),
    },
  });
}

function prepareVpakPairTransaction(transaction) {
  syncRegularFile(transaction.newPack, 'staged BlockKart VPAK');
  syncRegularFile(transaction.newProvenance, 'staged BlockKart VPAK provenance');
  const next = {
    pack: journalFact(vpakPairFileFact(
      transaction.newPack,
      'staged BlockKart VPAK',
      MAX_VPAK_BYTES,
    )),
    provenance: journalFact(vpakPairFileFact(
      transaction.newProvenance,
      'staged BlockKart VPAK provenance',
      MAX_REPORT_BYTES,
    )),
  };
  const previousPack = vpakPairFileFactOrNull(
    transaction.destinationPack,
    'current tracked BlockKart VPAK',
    MAX_VPAK_BYTES,
  );
  const previousProvenance = vpakPairFileFactOrNull(
    transaction.destinationProvenance,
    'current tracked BlockKart VPAK provenance',
    MAX_REPORT_BYTES,
  );
  if ((previousPack === null) !== (previousProvenance === null)) {
    throw new Error('tracked BlockKart VPAK and provenance must both exist or both be absent');
  }
  syncDirectory(transaction.directory);
  const journal = Object.freeze({
    schemaVersion: 1,
    kind: 'blockkart.vpakPairTransaction',
    phase: 'prepared',
    hadPrevious: previousPack !== null,
    previous: previousPack === null ? null : {
      pack: journalFact(previousPack),
      provenance: journalFact(previousProvenance),
    },
    next,
  });
  writeVpakPairJournal(transaction, journal);
  return journal;
}

function syncVpakTransactionDirectories(transaction) {
  syncDirectory(transaction.directory);
  syncDirectory(transaction.assetsDirectory);
}

function removePublishedTransactionFile(file, expected, label, maxBytes) {
  if (metadataOrNull(file) === null) return;
  assertVpakFileBinding(file, expected, label, maxBytes);
  unlinkSync(file);
}

function rollbackPreparedVpakPairTransaction(transaction, journal, operations = {}) {
  const rename = operations.renameSync ?? renameSync;
  const sync = operations.syncDirectory ?? syncDirectory;
  const restore = (destination, backup, previous, next, label, maxBytes) => {
    if (metadataOrNull(backup) !== null) {
      assertVpakFileBinding(backup, previous, `${label} rollback backup`, maxBytes);
      removePublishedTransactionFile(
        destination,
        next,
        `${label} partially published generation`,
        maxBytes,
      );
      rename(backup, destination);
      syncVpakTransactionDirectories(transaction);
    } else {
      assertVpakFileBinding(destination, previous, `${label} retained generation`, maxBytes);
    }
  };

  if (journal.hadPrevious) {
    restore(
      transaction.destinationPack,
      transaction.previousPack,
      journal.previous.pack,
      journal.next.pack,
      'BlockKart VPAK',
      MAX_VPAK_BYTES,
    );
    restore(
      transaction.destinationProvenance,
      transaction.previousProvenance,
      journal.previous.provenance,
      journal.next.provenance,
      'BlockKart VPAK provenance',
      MAX_REPORT_BYTES,
    );
    assertVpakFileBinding(
      transaction.destinationPack,
      journal.previous.pack,
      'rolled-back BlockKart VPAK',
      MAX_VPAK_BYTES,
    );
    assertVpakFileBinding(
      transaction.destinationProvenance,
      journal.previous.provenance,
      'rolled-back BlockKart VPAK provenance',
      MAX_REPORT_BYTES,
    );
  } else {
    if (metadataOrNull(transaction.previousPack) !== null
      || metadataOrNull(transaction.previousProvenance) !== null) {
      throw new Error('BlockKart VPAK transaction unexpectedly backed up an absent generation');
    }
    removePublishedTransactionFile(
      transaction.destinationPack,
      journal.next.pack,
      'new BlockKart VPAK during rollback',
      MAX_VPAK_BYTES,
    );
    removePublishedTransactionFile(
      transaction.destinationProvenance,
      journal.next.provenance,
      'new BlockKart VPAK provenance during rollback',
      MAX_REPORT_BYTES,
    );
    sync(transaction.assetsDirectory);
  }
  rmSync(transaction.directory, { recursive: true, force: true });
  sync(transaction.assetsDirectory);
}

function publishVpakPairWithRollback(transaction, postPublishValidation, operations = {}) {
  if (typeof postPublishValidation !== 'function') {
    throw new Error('BlockKart VPAK pair publication requires a post-publication validator');
  }
  const rename = operations.renameSync ?? renameSync;
  const warn = operations.warn ?? console.warn;
  const writeCommittedJournal = operations.writeCommittedJournal ?? writeVpakPairJournal;
  const journal = prepareVpakPairTransaction(transaction);
  let result;
  let commitMarkerPublished = false;
  let commitMarkerDurable = false;
  let deferredCommitCleanupError = null;
  try {
    if (journal.hadPrevious) {
      assertVpakFileBinding(
        transaction.destinationPack,
        journal.previous.pack,
        'tracked BlockKart VPAK before displacement',
        MAX_VPAK_BYTES,
      );
      rename(transaction.destinationPack, transaction.previousPack);
      syncVpakTransactionDirectories(transaction);
      assertVpakFileBinding(
        transaction.destinationProvenance,
        journal.previous.provenance,
        'tracked BlockKart VPAK provenance before displacement',
        MAX_REPORT_BYTES,
      );
      rename(transaction.destinationProvenance, transaction.previousProvenance);
      syncVpakTransactionDirectories(transaction);
    }
    assertVpakFileBinding(
      transaction.newPack,
      journal.next.pack,
      'staged BlockKart VPAK before publication',
      MAX_VPAK_BYTES,
    );
    rename(transaction.newPack, transaction.destinationPack);
    syncVpakTransactionDirectories(transaction);
    assertVpakFileBinding(
      transaction.newProvenance,
      journal.next.provenance,
      'staged BlockKart VPAK provenance before publication',
      MAX_REPORT_BYTES,
    );
    rename(transaction.newProvenance, transaction.destinationProvenance);
    syncVpakTransactionDirectories(transaction);
    result = postPublishValidation();
    assertVpakFileBinding(
      transaction.destinationPack,
      journal.next.pack,
      'published BlockKart VPAK after validation',
      MAX_VPAK_BYTES,
    );
    assertVpakFileBinding(
      transaction.destinationProvenance,
      journal.next.provenance,
      'published BlockKart VPAK provenance after validation',
      MAX_REPORT_BYTES,
    );
    const commitPublication = writeCommittedJournal(
      transaction,
      { ...journal, phase: 'committed' },
      true,
    );
    commitMarkerPublished = commitPublication?.published === true;
    commitMarkerDurable = commitPublication?.durable === true;
    if (!commitMarkerPublished) {
      throw new Error('BlockKart VPAK committed journal was not published');
    }
    if (!commitMarkerDurable) {
      throw new VpakCommitUncertainError(
        transaction,
        commitPublication.postCommitError
          ?? new Error('BlockKart VPAK commit journal directory was not synchronized'),
      );
    }
    if (commitPublication.postCommitError !== null) {
      deferredCommitCleanupError = commitPublication.postCommitError;
    }
  } catch (error) {
    if (commitMarkerPublished && !commitMarkerDurable) {
      throw error instanceof VpakCommitUncertainError
        ? error
        : new VpakCommitUncertainError(transaction, error);
    }
    if (commitMarkerDurable) {
      deferredCommitCleanupError = error;
    } else {
      try {
        rollbackPreparedVpakPairTransaction(transaction, journal, {
          renameSync: rename,
          syncDirectory: operations.rollbackSyncDirectory ?? syncDirectory,
        });
      } catch (rollbackError) {
        throw new AggregateError(
          [error, rollbackError],
          'BlockKart VPAK pair publication failed and rollback did not complete',
        );
      }
      throw error;
    }
  }

  if (deferredCommitCleanupError !== null) {
    try {
      warn(
        'BlockKart VPAK pair committed; transaction cleanup will be retried '
        + `on the next run: ${deferredCommitCleanupError.message}`,
      );
    } catch {
      // Publication crossed its durable commit marker; logging cannot reverse it.
    }
    return result;
  }

  try {
    rmSync(transaction.directory, { recursive: true, force: true });
    syncDirectory(transaction.assetsDirectory);
  } catch (error) {
    warn(
      `BlockKart VPAK pair committed; transaction cleanup will be retried on the next run: ${error.message}`,
    );
  }
  return result;
}

function recoverVpakPairTransactions(assetsDirectory = blockKartAssetsDirectory) {
  const assets = canonicalDirectory(assetsDirectory, 'BlockKart VPAK assets directory');
  const transactions = readdirSync(assets, { withFileTypes: true })
    .filter((entry) => entry.name.startsWith(VPAK_TRANSACTION_PREFIX))
    .sort((left, right) => Buffer.compare(Buffer.from(left.name), Buffer.from(right.name)));
  for (const entry of transactions) {
    if (!entry.isDirectory() || !VPAK_TRANSACTION_NAME.test(entry.name)) {
      throw new Error(`invalid BlockKart VPAK transaction entry: ${entry.name}`);
    }
  }
  const withJournal = transactions.filter((entry) => (
    metadataOrNull(path.join(assets, entry.name, VPAK_TRANSACTION_JOURNAL)) !== null
  ));
  if (withJournal.length > 1) {
    throw new Error('multiple journaled BlockKart VPAK transactions require manual inspection');
  }
  for (const entry of transactions) {
    const transaction = vpakPairTransactionFromDirectory(assets, path.join(assets, entry.name));
    if (metadataOrNull(transaction.journal) === null) {
      const pack = vpakPairFileFactOrNull(
        transaction.destinationPack,
        'tracked BlockKart VPAK during pre-publication recovery',
        MAX_VPAK_BYTES,
      );
      const provenance = vpakPairFileFactOrNull(
        transaction.destinationProvenance,
        'tracked BlockKart VPAK provenance during pre-publication recovery',
        MAX_REPORT_BYTES,
      );
      if ((pack === null) !== (provenance === null)) {
        throw new Error('tracked BlockKart VPAK pair is split while recovering an unprepared transaction');
      }
      rmSync(transaction.directory, { recursive: true, force: true });
      syncDirectory(assets);
      continue;
    }
    const journal = readVpakPairJournal(transaction);
    if (journal.phase === 'committed') {
      assertVpakFileBinding(
        transaction.destinationPack,
        journal.next.pack,
        'committed BlockKart VPAK during recovery',
        MAX_VPAK_BYTES,
      );
      assertVpakFileBinding(
        transaction.destinationProvenance,
        journal.next.provenance,
        'committed BlockKart VPAK provenance during recovery',
        MAX_REPORT_BYTES,
      );
      rmSync(transaction.directory, { recursive: true, force: true });
      syncDirectory(assets);
      continue;
    }
    rollbackPreparedVpakPairTransaction(transaction, journal);
  }
}

function observeVpakProducerPair(packPath, provenancePath, label) {
  const manifest = vpakPairFileFact(
    provenancePath,
    `${label} manifest`,
    MAX_REPORT_BYTES,
  );
  const producer = parseBlockKartVpakProducerManifestBytes(
    manifest.bytes,
    `${label} manifest`,
  );
  const pack = vpakPairFileFact(packPath, `${label} VPAK`, MAX_VPAK_BYTES);
  if (pack.digest !== `sha256:${producer.pack.sha256}` || pack.size !== producer.pack.size) {
    throw new Error(`${label} VPAK bytes do not match the producer manifest`);
  }
  const manifestAfter = vpakPairFileFact(
    provenancePath,
    `${label} manifest after pack verification`,
    MAX_REPORT_BYTES,
  );
  const packAfter = vpakPairFileFact(
    packPath,
    `${label} VPAK after manifest verification`,
    MAX_VPAK_BYTES,
  );
  assertSameSnapshot(manifest, manifestAfter, `${label} manifest observation`);
  assertSameSnapshot(pack, packAfter, `${label} VPAK observation`);
  return { manifest, pack, producer };
}

function assertSameVpakProducerPair(left, right, label) {
  assertSameSnapshot(left.manifest, right.manifest, `${label} manifest`);
  assertSameSnapshot(left.pack, right.pack, `${label} VPAK`);
  if (left.producer.producerDigest !== right.producer.producerDigest) {
    throw new Error(`${label} producer intent changed`);
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

function removeOutputFileAndSync(file, output, label, operations = {}) {
  const unlink = operations.unlinkSync ?? unlinkSync;
  const sync = operations.syncDirectory ?? syncDirectory;
  if (!sameNativePath(path.dirname(file), output)) {
    throw new Error(`${label} is outside its task output directory`);
  }
  let metadata;
  try {
    metadata = lstatSync(file, { bigint: true });
  } catch (error) {
    if (error?.code === 'ENOENT') {
      sync(output);
      return;
    }
    throw error;
  }
  if (metadata.isDirectory()) throw new Error(`${label} must not be a directory: ${file}`);
  unlink(file);
  sync(output);
}

function removeOwnedOutputFile(file, label) {
  removeOutputFileAndSync(file, prepareOutputDirectory(), label);
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
      () => parseBlockKartVpakProducerManifestBytes(
        Buffer.from('{"schemaVersion":1,"schemaVersion":1}'),
        'duplicate-key producer fixture',
      ),
      /duplicate object key "schemaVersion"/,
    );
    assert.throws(
      () => parseBlockKartVpakProducerManifestBytes(
        Buffer.from([0xff]),
        'invalid UTF-8 producer fixture',
      ),
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

    const pairAssets = path.join(fixture, 'pair-assets');
    const resetPair = () => {
      rmSync(pairAssets, { recursive: true, force: true });
      mkdirSync(pairAssets, { mode: 0o755 });
    };
    const trackedPair = () => ({
      pack: path.join(pairAssets, 'blockkart.vpak'),
      provenance: path.join(pairAssets, 'blockkart.vpak.provenance.json'),
    });
    const seedTrackedPair = (generation) => {
      const tracked = trackedPair();
      writeFreshFile(tracked.pack, Buffer.from(`${generation} pack`, 'utf8'));
      writeFreshFile(tracked.provenance, Buffer.from(`${generation} provenance`, 'utf8'));
    };
    const stagePair = (generation) => {
      const transaction = createVpakPairTransaction(pairAssets);
      writeFreshFile(transaction.newPack, Buffer.from(`${generation} pack`, 'utf8'));
      writeFreshFile(
        transaction.newProvenance,
        Buffer.from(`${generation} provenance`, 'utf8'),
      );
      return transaction;
    };
    const assertTrackedPair = (generation) => {
      const tracked = trackedPair();
      assert.equal(
        vpakPairFileFact(tracked.pack, 'fixture tracked pack', 1024).bytes.toString('utf8'),
        `${generation} pack`,
      );
      assert.equal(
        vpakPairFileFact(
          tracked.provenance,
          'fixture tracked provenance',
          1024,
        ).bytes.toString('utf8'),
        `${generation} provenance`,
      );
    };

    resetPair();
    seedTrackedPair('old-success');
    const successfulTransaction = stagePair('new-success');
    let successValidationCalls = 0;
    publishVpakPairWithRollback(successfulTransaction, () => {
      successValidationCalls += 1;
      assertTrackedPair('new-success');
      return 'published';
    }, { warn() {} });
    assert.equal(successValidationCalls, 1);
    assertTrackedPair('new-success');
    assert.equal(metadataOrNull(successfulTransaction.directory), null);

    resetPair();
    seedTrackedPair('old-uncertain-commit');
    const uncertainCommitTransaction = stagePair('new-uncertain-commit');
    const uncertainTaskOutput = path.join(fixture, 'uncertain-task-output');
    mkdirSync(uncertainTaskOutput, { mode: 0o755 });
    const uncertainReport = path.join(uncertainTaskOutput, 'report.json');
    const uncertainVoBinary = path.join(uncertainTaskOutput, 'vo');
    writeFreshFile(uncertainReport, Buffer.from('new report', 'utf8'));
    writeFreshFile(uncertainVoBinary, Buffer.from('new Vo binary', 'utf8'));
    let transactionSyncFailureInjected = false;
    assert.throws(
      () => publishVpakPairWithRollback(
        uncertainCommitTransaction,
        () => 'must-not-report-success',
        {
          warn() {},
          writeCommittedJournal(transaction, journal, replace) {
            return writeVpakPairJournal(transaction, journal, replace, {
              syncDirectory(directory) {
                if (
                  !transactionSyncFailureInjected
                  && sameNativePath(directory, transaction.directory)
                ) {
                  transactionSyncFailureInjected = true;
                  throw new Error('injected commit transaction-directory sync failure');
                }
                syncDirectory(directory);
              },
            });
          },
        },
      ),
      (error) => (
        error instanceof VpakCommitUncertainError
        && /injected commit transaction-directory sync failure/u.test(error.cause?.message ?? '')
      ),
    );
    assert.equal(transactionSyncFailureInjected, true);
    removeOutputFileAndSync(
      uncertainReport,
      uncertainTaskOutput,
      'uncertain fixture report',
    );
    removeOutputFileAndSync(
      uncertainVoBinary,
      uncertainTaskOutput,
      'uncertain fixture Vo binary',
    );
    assert.equal(metadataOrNull(uncertainReport), null);
    assert.equal(metadataOrNull(uncertainVoBinary), null);
    assertTrackedPair('new-uncertain-commit');
    assert.notEqual(metadataOrNull(uncertainCommitTransaction.directory), null);
    const visibleUncertainJournal = readVpakPairJournal(uncertainCommitTransaction);
    assert.equal(visibleUncertainJournal.phase, 'committed');
    writeVpakPairJournal(
      uncertainCommitTransaction,
      { ...visibleUncertainJournal, phase: 'prepared' },
      true,
    );
    recoverVpakPairTransactions(pairAssets);
    assertTrackedPair('old-uncertain-commit');
    assert.equal(metadataOrNull(uncertainCommitTransaction.directory), null);

    resetPair();
    seedTrackedPair('old-durable-commit');
    const durableCommitTransaction = stagePair('new-durable-commit');
    const durabilityWarnings = [];
    assert.equal(
      publishVpakPairWithRollback(
        durableCommitTransaction,
        () => 'committed-with-deferred-cleanup',
        {
          warn(message) {
            durabilityWarnings.push(message);
          },
          writeCommittedJournal(transaction, journal, replace) {
            return writeVpakPairJournal(transaction, journal, replace, {
              syncDirectory(directory) {
                if (sameNativePath(directory, transaction.assetsDirectory)) {
                  throw new Error('injected post-commit assets-directory sync failure');
                }
                syncDirectory(directory);
              },
            });
          },
        },
      ),
      'committed-with-deferred-cleanup',
    );
    assertTrackedPair('new-durable-commit');
    assert.notEqual(metadataOrNull(durableCommitTransaction.directory), null);
    assert.equal(durabilityWarnings.length, 1);
    assert.match(durabilityWarnings[0], /injected post-commit assets-directory sync failure/);
    recoverVpakPairTransactions(pairAssets);
    assertTrackedPair('new-durable-commit');
    assert.equal(metadataOrNull(durableCommitTransaction.directory), null);

    resetPair();
    seedTrackedPair('old-postcheck');
    const postcheckTransaction = stagePair('new-postcheck');
    assert.throws(
      () => publishVpakPairWithRollback(postcheckTransaction, () => {
        assertTrackedPair('new-postcheck');
        throw new Error('injected post-publication validation failure');
      }, { warn() {} }),
      /injected post-publication validation failure/,
    );
    assertTrackedPair('old-postcheck');
    assert.equal(metadataOrNull(postcheckTransaction.directory), null);

    resetPair();
    const absentPostcheckTransaction = stagePair('new-absent-postcheck');
    assert.throws(
      () => publishVpakPairWithRollback(absentPostcheckTransaction, () => {
        assertTrackedPair('new-absent-postcheck');
        throw new Error('injected first-publication validation failure');
      }, { warn() {} }),
      /injected first-publication validation failure/,
    );
    assert.equal(metadataOrNull(trackedPair().pack), null);
    assert.equal(metadataOrNull(trackedPair().provenance), null);
    assert.equal(metadataOrNull(absentPostcheckTransaction.directory), null);

    resetPair();
    const absentSyncFailureTransaction = stagePair('new-absent-sync-failure');
    let rollbackAssetsSyncFailureInjected = false;
    assert.throws(
      () => publishVpakPairWithRollback(absentSyncFailureTransaction, () => {
        throw new Error('injected first-publication postcheck failure before rollback sync');
      }, {
        rollbackSyncDirectory(directory) {
          if (
            !rollbackAssetsSyncFailureInjected
            && sameNativePath(directory, absentSyncFailureTransaction.assetsDirectory)
          ) {
            rollbackAssetsSyncFailureInjected = true;
            throw new Error('injected rollback assets-directory sync failure');
          }
          syncDirectory(directory);
        },
        warn() {},
      }),
      (error) => (
        error instanceof AggregateError
        && error.errors.some((entry) => (
          /injected rollback assets-directory sync failure/u.test(entry.message)
        ))
      ),
    );
    assert.equal(rollbackAssetsSyncFailureInjected, true);
    assert.equal(metadataOrNull(trackedPair().pack), null);
    assert.equal(metadataOrNull(trackedPair().provenance), null);
    assert.notEqual(metadataOrNull(absentSyncFailureTransaction.directory), null);
    assert.equal(readVpakPairJournal(absentSyncFailureTransaction).phase, 'prepared');
    recoverVpakPairTransactions(pairAssets);
    assert.equal(metadataOrNull(absentSyncFailureTransaction.directory), null);

    resetPair();
    seedTrackedPair('old-rename');
    const renameFailureTransaction = stagePair('new-rename');
    let renameCalls = 0;
    assert.throws(
      () => publishVpakPairWithRollback(renameFailureTransaction, () => {
        throw new Error('validator must not run after an incomplete pair rename');
      }, {
        renameSync(from, to) {
          renameCalls += 1;
          if (renameCalls === 4) {
            throw new Error('injected provenance publication rename failure');
          }
          renameSync(from, to);
        },
        warn() {},
      }),
      /injected provenance publication rename failure/,
    );
    assertTrackedPair('old-rename');
    assert.equal(metadataOrNull(renameFailureTransaction.directory), null);

    resetPair();
    seedTrackedPair('old-recovery');
    const interruptedTransaction = stagePair('new-recovery');
    prepareVpakPairTransaction(interruptedTransaction);
    renameSync(interruptedTransaction.destinationPack, interruptedTransaction.previousPack);
    syncVpakTransactionDirectories(interruptedTransaction);
    renameSync(
      interruptedTransaction.destinationProvenance,
      interruptedTransaction.previousProvenance,
    );
    syncVpakTransactionDirectories(interruptedTransaction);
    renameSync(interruptedTransaction.newPack, interruptedTransaction.destinationPack);
    syncVpakTransactionDirectories(interruptedTransaction);
    recoverVpakPairTransactions(pairAssets);
    assertTrackedPair('old-recovery');
    assert.equal(metadataOrNull(interruptedTransaction.directory), null);

    resetPair();
    seedTrackedPair('old-committed-recovery');
    const committedTransaction = stagePair('new-committed-recovery');
    const committedJournal = prepareVpakPairTransaction(committedTransaction);
    renameSync(committedTransaction.destinationPack, committedTransaction.previousPack);
    renameSync(
      committedTransaction.destinationProvenance,
      committedTransaction.previousProvenance,
    );
    renameSync(committedTransaction.newPack, committedTransaction.destinationPack);
    renameSync(
      committedTransaction.newProvenance,
      committedTransaction.destinationProvenance,
    );
    syncVpakTransactionDirectories(committedTransaction);
    writeVpakPairJournal(
      committedTransaction,
      { ...committedJournal, phase: 'committed' },
      true,
    );
    recoverVpakPairTransactions(pairAssets);
    assertTrackedPair('new-committed-recovery');
    assert.equal(metadataOrNull(committedTransaction.directory), null);

    resetPair();
    seedTrackedPair('old-unprepared');
    const unpreparedTransaction = stagePair('new-unprepared');
    recoverVpakPairTransactions(pairAssets);
    assertTrackedPair('old-unprepared');
    assert.equal(metadataOrNull(unpreparedTransaction.directory), null);
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
  let vpakTransaction = null;
  let completed = false;
  let preserveUncertainVpakTransaction = false;
  let failure = null;
  try {
    assertProducerLease(lease);
    recoverVpakPairTransactions(blockKartAssetsDirectory);
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
    vpakTransaction = createVpakPairTransaction(blockKartAssetsDirectory);
    if (
      !sameNativePath(vpakTransaction.destinationPack, trackedVpakPath)
      || !sameNativePath(
        vpakTransaction.destinationProvenance,
        trackedVpakProvenancePath,
      )
    ) {
      throw new Error('BlockKart VPAK transaction destinations do not match the tracked pair');
    }
    const stagedGuestEnvironment = {
      ...guestEnvironment,
      [VPAK_TRANSACTION_DIRECTORY_ENV]: vpakTransaction.directory,
    };
    assertProducerLease(lease);
    execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--build'], {
      cwd: blockKartRoot,
      env: stagedGuestEnvironment,
      stdio: 'inherit',
      timeout: VPAK_GUEST_TIMEOUT_MS,
    });
    runVpakProvenanceCheck(stagedGuestEnvironment, lease);

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
    const producerAfterFirstCheck = observeVpakProducerPair(
      vpakTransaction.newPack,
      vpakTransaction.newProvenance,
      'staged BlockKart VPAK producer after first provenance check',
    );
    runVpakProvenanceCheck(stagedGuestEnvironment, lease);
    const producerAfterSecondCheck = observeVpakProducerPair(
      vpakTransaction.newPack,
      vpakTransaction.newProvenance,
      'staged BlockKart VPAK producer after second provenance check',
    );
    assertSameVpakProducerPair(
      producerAfterFirstCheck,
      producerAfterSecondCheck,
      'staged BlockKart VPAK producer across the second provenance check',
    );

    assertProducerLease(lease);
    const producerBeforeReport = publishVpakPairWithRollback(vpakTransaction, () => {
      assertProducerLease(lease);
      runVpakProvenanceCheck(guestEnvironment, lease);
      const publishedAfterCheck = observeBlockKartVpakProducer(
        blockKartRoot,
        'published BlockKart VPAK producer after provenance check',
      );
      assertSameVpakProducerPair(
        producerAfterSecondCheck,
        publishedAfterCheck,
        'staged and published BlockKart VPAK producer',
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
      const producerBeforePublicationReport = observeBlockKartVpakProducer(
        blockKartRoot,
        'BlockKart VPAK producer before report publication',
      );
      assertSameBlockKartVpakProducerState(
        publishedAfterCheck,
        producerBeforePublicationReport,
        'BlockKart VPAK producer after postflight validation',
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
          digest: producerBeforePublicationReport.manifest.digest,
          size: producerBeforePublicationReport.manifest.size,
        },
        archiveEntryCount: producerBeforePublicationReport.producer.archiveEntryCount,
        payloadInputCount: producerBeforePublicationReport.producer.payloadInputCount,
        workspaceSourceInputCount: producerBeforePublicationReport.producer.workspaceSourceInputCount,
        producerDigest: producerBeforePublicationReport.producer.producerDigest,
        pack: producerBeforePublicationReport.producer.pack,
      });
      const producerAfterReport = observeBlockKartVpakProducer(
        blockKartRoot,
        'BlockKart VPAK producer after report publication',
      );
      assertSameBlockKartVpakProducerState(
        producerBeforePublicationReport,
        producerAfterReport,
        'BlockKart VPAK producer across report publication',
      );
      assertProducerLease(lease);
      return producerBeforePublicationReport;
    });
    vpakTransaction = null;
    completed = true;
    console.log(
      `blockkart vpak build: ok entries=${producerBeforeReport.producer.archiveEntryCount} `
      + `producer=${producerBeforeReport.producer.producerDigest}`,
    );
  } catch (error) {
    failure = error;
    preserveUncertainVpakTransaction = error instanceof VpakCommitUncertainError
      && vpakTransaction !== null
      && sameNativePath(error.transactionDirectory, vpakTransaction.directory);
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
  if (vpakTransaction !== null && !preserveUncertainVpakTransaction) {
    clean(() => recoverVpakPairTransactions(blockKartAssetsDirectory));
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
