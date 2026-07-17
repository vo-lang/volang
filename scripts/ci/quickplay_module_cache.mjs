#!/usr/bin/env node
import {
  existsSync,
  lstatSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import path from 'node:path';
import { fileURLToPath, pathToFileURL } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import {
  captureCleanEffectiveSnapshot,
  createVoBinaryAuthority,
  fetchAuthenticatedModules,
  portableProjectSnapshot,
  selectModuleCacheRoot,
  validatePortableProjectSnapshot,
} from './quickplay_vnext.mjs';
import {
  cleanupDirectoryReplacementBackups,
  publishStagedDirectoryWithRollback,
  recoverInterruptedDirectoryReplacement,
  withDirectoryReplacementLock,
} from './voplay_current_wasm.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const defaultReportRoot = path.join(root, 'target', 'quickplay-module-cache');
const SNAPSHOT_FILE = 'snapshot.json';
const MODULE_CACHE_DIRECTORY = 'mod';
const MAX_SNAPSHOT_BYTES = 128 * 1024 * 1024;
const OUTPUT_LABEL = 'quickplay module cache';
const STRICT_UTF8 = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });

function sameNativePath(left, right) {
  const normalizedLeft = path.normalize(path.resolve(left));
  const normalizedRight = path.normalize(path.resolve(right));
  return process.platform === 'win32'
    ? normalizedLeft.toUpperCase() === normalizedRight.toUpperCase()
    : normalizedLeft === normalizedRight;
}

function realDirectory(directory) {
  try {
    const metadata = lstatSync(directory);
    return metadata.isDirectory() && !metadata.isSymbolicLink();
  } catch (error) {
    if (error?.code === 'ENOENT') return false;
    throw error;
  }
}

function realDirectoryPath(rootDirectory, portableRelative) {
  let current = rootDirectory;
  for (const component of portableRelative.split('/')) {
    current = path.join(current, component);
    if (!realDirectory(current)) return false;
  }
  return true;
}

function sameStableMetadata(left, right) {
  return left.dev === right.dev
    && left.ino === right.ino
    && left.mode === right.mode
    && left.nlink === right.nlink
    && left.size === right.size
    && left.mtimeNs === right.mtimeNs
    && left.ctimeNs === right.ctimeNs;
}

/** A published output is complete only when both the report and cache root exist. */
export function completeQuickplayModuleCacheDirectory(directory) {
  try {
    if (!realDirectory(directory)) return false;
    const cache = path.join(directory, MODULE_CACHE_DIRECTORY);
    if (!realDirectory(cache)) return false;
    const snapshot = path.join(directory, SNAPSHOT_FILE);
    const metadata = lstatSync(snapshot, { bigint: true });
    if (
      !metadata.isFile()
      || metadata.isSymbolicLink()
      || metadata.size <= 0n
      || metadata.size > BigInt(MAX_SNAPSHOT_BYTES)
    ) return false;
    const bytes = readFileSync(snapshot);
    const after = lstatSync(snapshot, { bigint: true });
    if (
      !sameStableMetadata(metadata, after)
      || bytes.byteLength !== Number(metadata.size)
      || bytes.at(-1) !== 0x0a
    ) return false;
    const value = validatePortableProjectSnapshot(
      JSON.parse(STRICT_UTF8.decode(bytes)),
      'github.com/vo-lang/blockkart',
    );
    const canonicalBytes = Buffer.from(`${JSON.stringify(value, null, 2)}\n`, 'utf8');
    return canonicalBytes.equals(bytes)
      && value.modules.every((module) => (
        module.source.kind !== 'registry'
        || realDirectoryPath(cache, module.source.directory)
      ));
  } catch {
    return false;
  }
}

function publishCompleteDirectory(
  stagingDirectory,
  outputDirectory,
  { completeDirectory, label, operations = {} },
) {
  if (!completeDirectory(stagingDirectory)) {
    throw new Error(`${label} staging directory is incomplete`);
  }
  publishStagedDirectoryWithRollback(stagingDirectory, outputDirectory, {
    ...operations,
    label,
  });
}

/** Publish one fully-produced sibling directory while preserving the prior output on failure. */
export function publishQuickplayModuleCacheDirectory(
  stagingDirectory,
  outputDirectory,
  operations = {},
) {
  publishCompleteDirectory(stagingDirectory, outputDirectory, {
    completeDirectory: completeQuickplayModuleCacheDirectory,
    label: OUTPUT_LABEL,
    operations,
  });
}

function prepareTransactionalDirectory(
  outputDirectory,
  produce,
) {
  const output = path.resolve(outputDirectory);
  return withDirectoryReplacementLock(output, () => {
    recoverInterruptedDirectoryReplacement(output, {
      completeDirectory: completeQuickplayModuleCacheDirectory,
      label: OUTPUT_LABEL,
    });
    if (existsSync(output) && completeQuickplayModuleCacheDirectory(output)) {
      cleanupDirectoryReplacementBackups(output, {
        completeDirectory: completeQuickplayModuleCacheDirectory,
        label: OUTPUT_LABEL,
      });
    }
    mkdirSync(path.dirname(output), { recursive: true });
    const staging = mkdtempSync(
      path.join(path.dirname(output), `.${path.basename(output)}.staging-`),
    );
    try {
      const result = produce(staging);
      publishQuickplayModuleCacheDirectory(staging, output);
      cleanupDirectoryReplacementBackups(output, {
        completeDirectory: completeQuickplayModuleCacheDirectory,
        label: OUTPUT_LABEL,
      });
      return result;
    } finally {
      rmSync(staging, { recursive: true, force: true });
    }
  });
}

function captureModuleSnapshot({ blockKartRoot, workspaceRoots, cacheRoot }) {
  const voAuthority = createVoBinaryAuthority({ root });
  let snapshot = captureCleanEffectiveSnapshot({
    root,
    projectRoot: blockKartRoot,
    workspaceRoots,
    cacheRoot,
    voAuthority,
  });
  if (snapshot.modules.some((module) => module.source.kind === 'registry')) {
    fetchAuthenticatedModules({
      root,
      projectRoot: blockKartRoot,
      cacheRoot,
      voAuthority,
    });
    snapshot = captureCleanEffectiveSnapshot({
      root,
      projectRoot: blockKartRoot,
      workspaceRoots,
      cacheRoot,
      voAuthority,
    });
  }
  return snapshot;
}

function writeSnapshotReport(reportRoot, snapshot) {
  mkdirSync(reportRoot, { recursive: true });
  writeFileSync(
    path.join(reportRoot, SNAPSHOT_FILE),
    `${JSON.stringify(portableProjectSnapshot(snapshot), null, 2)}\n`,
    { flag: 'wx' },
  );
}

function repositoryRoots() {
  return {
    blockKartRoot: requireRepoRoot('BLOCKKART_ROOT', 'BlockKart'),
    workspaceRoots: [
      requireRepoRoot('VOGUI_ROOT', 'vogui'),
      requireRepoRoot('VOPACK_ROOT', 'vopack'),
      requireRepoRoot('VOPLAY_ROOT', 'voplay'),
    ],
  };
}

function main() {
  const reportRoot = path.resolve(
    process.env.QUICKPLAY_MODULE_CACHE_OUT_DIR ?? defaultReportRoot,
  );
  const defaultCacheRoot = path.join(reportRoot, MODULE_CACHE_DIRECTORY);
  const cacheRoot = selectModuleCacheRoot(defaultCacheRoot);
  if (!sameNativePath(cacheRoot, defaultCacheRoot)) {
    throw new Error(
      'quickplay-module-cache requires VO_MOD_CACHE to be unset or equal its task-local mod directory',
    );
  }
  const repositories = repositoryRoots();
  const snapshot = prepareTransactionalDirectory(reportRoot, (stagingReportRoot) => {
    const stagingCacheRoot = path.join(stagingReportRoot, MODULE_CACHE_DIRECTORY);
    mkdirSync(stagingCacheRoot, { recursive: true });
    const current = captureModuleSnapshot({
      ...repositories,
      cacheRoot: stagingCacheRoot,
    });
    writeSnapshotReport(stagingReportRoot, current);
    return current;
  });

  const registry = snapshot.modules.filter((module) => module.source.kind === 'registry').length;
  const workspace = snapshot.modules.length - registry;
  console.log(`Quickplay module sources: ${registry} authenticated registry, ${workspace} workspace`);
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? '').href) {
  try {
    main();
  } catch (error) {
    console.error(error instanceof Error ? error.message : String(error));
    process.exitCode = 1;
  }
}
