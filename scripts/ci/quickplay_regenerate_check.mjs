#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { mkdtempSync, realpathSync, rmSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import {
  acceptedCrossPlatformVoplayVariant,
  assertQuickplayVoCliProducerInputs,
  assertQuickplaySnapshotUnchanged,
  compareQuickplaySnapshots,
  QUICKPLAY_REGENERATE_GATE_FILES,
  snapshotQuickplayDirectory,
  writeFileAtomically,
} from './quickplay_regenerate_contract.mjs';
import {
  currentVoCliBuildInputs,
  currentVoCliToolchain,
} from './quickplay_cli_producer_contract.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const checkedDir = path.join(root, 'apps/studio/public/quickplay/blockkart');
const generator = path.join(root, 'apps/studio/scripts/package_blockkart_quickplay.mjs');
const reportDir = path.resolve(
  process.env.QUICKPLAY_REGENERATE_CHECK_OUT_DIR
    || path.join(root, 'target/quickplay-regenerate-check'),
);
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const dependencyRepos = [
  { name: 'github.com/vo-lang/vogui', root: requireRepoRoot('VOGUI_ROOT', 'vogui') },
  { name: 'github.com/vo-lang/voplay', root: requireRepoRoot('VOPLAY_ROOT', 'voplay') },
  { name: 'github.com/vo-lang/vopack', root: requireRepoRoot('VOPACK_ROOT', 'vopack') },
];

class RegenerateCheckFailure extends Error {}

function currentCheckedSnapshot(expected) {
  const found = snapshotQuickplayDirectory(checkedDir);
  if (expected) assertQuickplaySnapshotUnchanged(expected, found, 'checked Quickplay package');
  return found;
}

function currentGeneratedSnapshot(directory, expected) {
  if (!directory) return null;
  const found = snapshotQuickplayDirectory(directory);
  if (expected) assertQuickplaySnapshotUnchanged(expected, found, 'regenerated Quickplay package');
  return found;
}

function publishReport(status, details = {}, bindings = {}) {
  const checkedSnapshot = currentCheckedSnapshot(bindings.checkedSnapshot);
  const generatedSnapshot = currentGeneratedSnapshot(
    bindings.generatedDir,
    bindings.generatedSnapshot,
  );
  const generatedAt = new Date().toISOString();
  const freshEvidence = sourceBoundEvidence({
    gate: 'quickplay-regenerate-check',
    generatedAt,
    root,
    repos: [
      { name: 'volang', root },
      { name: 'BlockKart', root: blockKartRoot },
      ...dependencyRepos,
    ],
    gateFiles: QUICKPLAY_REGENERATE_GATE_FILES,
    artifacts: [checkedDir],
  });
  currentCheckedSnapshot(checkedSnapshot);
  if (generatedSnapshot) currentGeneratedSnapshot(bindings.generatedDir, generatedSnapshot);
  const evidencePassed = freshEvidence.verdict.status === 'pass';
  const finalStatus = status === 'ok' && !evidencePassed ? 'failed' : status;
  const report = {
    schemaVersion: 2,
    kind: 'quickplay.regenerateCheckReport',
    gate: 'quickplay-regenerate-check',
    status: finalStatus,
    generatedAt,
    freshEvidence,
    checkedDir,
    snapshot: {
      checkedEntries: checkedSnapshot.length,
      generatedEntries: generatedSnapshot?.length ?? null,
    },
    ...(status === 'ok' && !evidencePassed
      ? { message: 'source-bound evidence requires clean committed repositories' }
      : {}),
    ...details,
  };
  writeFileAtomically(reportDir, 'report.json', `${JSON.stringify(report, null, 2)}\n`);
  currentCheckedSnapshot(checkedSnapshot);
  if (generatedSnapshot) currentGeneratedSnapshot(bindings.generatedDir, generatedSnapshot);
  return report;
}

function fail(message, details = {}, bindings = {}) {
  try {
    publishReport('failed', { message, ...details }, bindings);
  } catch (error) {
    console.error(`quickplay regenerate check: report publication failed: ${error.message}`);
  }
  throw new RegenerateCheckFailure(message);
}

function run() {
let checkedSnapshot;
let currentCliInputs;
let currentCliToolchain;
try {
  currentCliInputs = currentVoCliBuildInputs(root);
  currentCliToolchain = currentVoCliToolchain(root);
  assertQuickplayVoCliProducerInputs(
    checkedDir,
    currentCliInputs,
    'checked Quickplay package',
    currentCliToolchain,
  );
  checkedSnapshot = currentCheckedSnapshot();
} catch (error) {
  fail(`checked-in quickplay package is invalid: ${error.message}`);
}

const tempRoot = realpathSync.native(
  mkdtempSync(path.join(os.tmpdir(), 'blockkart-quickplay-regenerate-')),
);
const outDir = path.join(tempRoot, 'blockkart');
try {
  const result = spawnSync(process.execPath, [generator], {
    cwd: root,
    env: {
      ...process.env,
      BLOCKKART_QUICKPLAY_OUT_ROOT: outDir,
    },
    encoding: 'utf8',
    maxBuffer: 20 * 1024 * 1024,
    timeout: 540_000,
  });
  if (result.error || result.status !== 0) {
    fail(
      `generator failed${result.error ? `: ${result.error.message}` : ` with status ${result.status}`}`,
      {
        generatorStatus: result.status,
        signal: result.signal,
        stdout: result.stdout,
        stderr: result.stderr,
      },
      { checkedSnapshot },
    );
  }

  let generatedSnapshot;
  try {
    assertQuickplayVoCliProducerInputs(
      outDir,
      currentCliInputs,
      'regenerated Quickplay package',
      currentCliToolchain,
    );
    generatedSnapshot = currentGeneratedSnapshot(outDir);
    currentCheckedSnapshot(checkedSnapshot);
  } catch (error) {
    fail(
      `package snapshot failed: ${error.message}`,
      {},
      { checkedSnapshot },
    );
  }
  const diff = compareQuickplaySnapshots(generatedSnapshot, checkedSnapshot);
  let platformVariant = null;
  if (diff.missing.length > 0 || diff.extra.length > 0 || diff.changed.length > 0) {
    try {
      platformVariant = acceptedCrossPlatformVoplayVariant(diff, outDir, checkedDir);
    } catch (error) {
      fail(
        `cross-platform comparison failed: ${error.message}`,
        { diff },
        { checkedSnapshot, generatedDir: outDir, generatedSnapshot },
      );
    }
    if (!platformVariant) {
      fail(
        `regenerated package differs from checked-in artifact\n${JSON.stringify(diff, null, 2)}`,
        { diff },
        { checkedSnapshot, generatedDir: outDir, generatedSnapshot },
      );
    }
  }

  let report;
  try {
    report = publishReport(
      'ok',
      platformVariant ? { diff, platformVariant } : { diff },
      { checkedSnapshot, generatedDir: outDir, generatedSnapshot },
    );
  } catch (error) {
    fail(
      `report publication preflight failed: ${error.message}`,
      { diff },
      { checkedSnapshot, generatedDir: outDir, generatedSnapshot },
    );
  }
  if (report.status !== 'ok') {
    throw new RegenerateCheckFailure(report.message);
  }
  if (platformVariant) {
    console.log(
      `quickplay regenerate check: ok (metadata-only platform label ${platformVariant.checkedPlatform.os}/${platformVariant.checkedPlatform.arch} -> ${platformVariant.generatedPlatform.os}/${platformVariant.generatedPlatform.arch})`,
    );
  } else {
    console.log('quickplay regenerate check: ok');
  }
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
}

try {
  run();
} catch (error) {
  if (error instanceof RegenerateCheckFailure) {
    console.error(`quickplay regenerate check: ${error.message}`);
  } else {
    console.error(`quickplay regenerate check: unexpected failure: ${error.message}`);
  }
  process.exitCode = 1;
}
