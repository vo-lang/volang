#!/usr/bin/env node
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  renameSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  completeQuickplayModuleCacheDirectory,
  publishQuickplayModuleCacheDirectory,
} from './quickplay_module_cache.mjs';
import { withDirectoryReplacementLock } from './voplay_current_wasm.mjs';

const temporaryRoot = mkdtempSync(path.join(os.tmpdir(), 'volang-quickplay-cache-atomicity-'));
const output = path.join(temporaryRoot, 'quickplay-module-cache');
const cacheScript = fileURLToPath(new URL('./quickplay_module_cache.mjs', import.meta.url));

function snapshotFixture(generation) {
  const constraint = generation === 'previous' ? '^0.1.0' : '^0.1.1';
  return {
    schema_version: 2,
    mode: 'effective',
    authority: 'workspace',
    root: {
      module: 'github.com/vo-lang/blockkart',
      vo: '^0.1.0',
      dependencies: [{ module: 'github.com/vo-lang/voplay', constraint }],
    },
    workspace: { file: 'vo.work' },
    modules: [{
      module: 'github.com/vo-lang/voplay',
      vo: '^0.1.0',
      source: {
        kind: 'workspace',
        directory: '../modules/github.com@vo-lang@voplay',
      },
      dependencies: [],
    }],
  };
}

function writeCompleteOutput(directory, generation) {
  mkdirSync(path.join(directory, 'mod'), { recursive: true });
  writeFileSync(path.join(directory, 'mod', 'generation.txt'), `${generation}\n`);
  writeFileSync(
    path.join(directory, 'snapshot.json'),
    `${JSON.stringify(snapshotFixture(generation), null, 2)}\n`,
  );
  assert.equal(completeQuickplayModuleCacheDirectory(directory), true);
}

function generation(directory) {
  return readFileSync(path.join(directory, 'mod', 'generation.txt'), 'utf8').trim();
}

function snapshotBytes(directory) {
  return readFileSync(path.join(directory, 'snapshot.json'));
}

function replacementDebris() {
  const prefix = `${path.basename(output)}.backup-`;
  return readdirSync(temporaryRoot)
    .filter((entry) => entry.startsWith(prefix) || entry === `${path.basename(output)}.publication-locks`);
}

try {
  const rejectedReport = path.join(temporaryRoot, 'external-cache-rejection-report');
  const rejectedExternalCache = path.join(temporaryRoot, 'external-cache');
  const rejected = spawnSync(process.execPath, [cacheScript], {
    cwd: temporaryRoot,
    env: {
      ...process.env,
      QUICKPLAY_MODULE_CACHE_OUT_DIR: rejectedReport,
      VO_MOD_CACHE: rejectedExternalCache,
    },
    encoding: 'utf8',
  });
  assert.notEqual(rejected.status, 0, 'external VO_MOD_CACHE unexpectedly succeeded');
  assert.match(
    `${rejected.stdout}\n${rejected.stderr}`,
    /requires VO_MOD_CACHE to be unset or equal its task-local mod directory/,
  );
  assert.equal(existsSync(rejectedReport), false, 'rejected report path must stay absent');
  assert.equal(existsSync(rejectedExternalCache), false, 'rejected external cache must stay absent');

  writeCompleteOutput(output, 'previous');
  const obsolete = path.join(output, 'mod', 'obsolete-module', 'stale.txt');
  mkdirSync(path.dirname(obsolete), { recursive: true });
  writeFileSync(obsolete, 'stale\n');
  const previousSnapshot = snapshotBytes(output);
  const staging = path.join(temporaryRoot, '.quickplay-module-cache.staging-next');
  writeCompleteOutput(staging, 'next');

  let renameCount = 0;
  assert.throws(
    () => withDirectoryReplacementLock(output, () => {
      publishQuickplayModuleCacheDirectory(staging, output, {
        renameSync(source, destination) {
          renameCount += 1;
          if (renameCount === 2) throw new Error('injected second-rename failure');
          renameSync(source, destination);
        },
      });
    }),
    /previous output was restored.*injected second-rename failure/,
  );
  assert.equal(generation(output), 'previous', 'failed publication must restore the old output');
  assert.deepEqual(snapshotBytes(output), previousSnapshot, 'failed publication must restore the old snapshot bytes');
  assert.equal(existsSync(obsolete), true, 'failed publication must restore every old cache entry');
  assert.equal(generation(staging), 'next', 'failed publication must preserve the complete staging tree');
  assert.deepEqual(replacementDebris(), [], 'rollback must leave no backup or claim debris');

  withDirectoryReplacementLock(output, () => {
    publishQuickplayModuleCacheDirectory(staging, output);
  });
  assert.equal(generation(output), 'next', 'successful publication must replace the whole directory');
  assert.equal(existsSync(obsolete), false, 'successful publication must remove old extra cache entries');
  assert.equal(existsSync(staging), false, 'successful publication must consume the staging directory');
  assert.deepEqual(replacementDebris(), [], 'successful publication must clean replacement state');

  const incomplete = path.join(temporaryRoot, '.quickplay-module-cache.staging-incomplete');
  mkdirSync(path.join(incomplete, 'mod'), { recursive: true });
  assert.equal(completeQuickplayModuleCacheDirectory(incomplete), false);
  assert.throws(
    () => withDirectoryReplacementLock(output, () => {
      publishQuickplayModuleCacheDirectory(incomplete, output);
    }),
    /staging directory is incomplete/,
  );
  assert.equal(generation(output), 'next', 'incomplete staging must leave the output untouched');
  assert.deepEqual(replacementDebris(), [], 'rejected staging must leave no replacement state');

  console.log('quickplay module cache transactional selftest: ok');
} finally {
  rmSync(temporaryRoot, { recursive: true, force: true });
}
