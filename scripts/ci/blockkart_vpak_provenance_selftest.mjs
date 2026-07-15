#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync, spawnSync } from 'node:child_process';
import { copyFileSync, mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  currentVoCliToolchain,
  currentVoCliBuildInputs,
  verifyVoCliExecutionIdentity,
  verifyVoCliBuildInputs,
} from './quickplay_cli_producer_contract.mjs';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const moduleRoots = {
  voplay: requireRepoRoot('VOPLAY_ROOT', 'voplay'),
  vogui: requireRepoRoot('VOGUI_ROOT', 'vogui'),
  vopack: requireRepoRoot('VOPACK_ROOT', 'vopack'),
};
const outDir = path.resolve(process.env.BLOCKKART_VPAK_SELFTEST_OUT_DIR ?? path.join(root, 'target/blockkart-vpak-provenance-selftest'));
const manifest = JSON.parse(readFileSync(path.join(blockKartRoot, 'assets/blockkart.vpak.provenance.json'), 'utf8'));
const buildReport = JSON.parse(readFileSync(path.join(root, 'target/blockkart-vpak-build/report.json'), 'utf8'));
const tempRoot = mkdtempSync(path.join(os.tmpdir(), 'blockkart-vpak-provenance-selftest-'));
const fixtureRoot = path.join(tempRoot, 'BlockKart');
const blockKartWorkspacePrefix = 'workspace:github.com/vo-lang/blockkart/';

function copy(relative) {
  const destination = path.join(fixtureRoot, relative);
  mkdirSync(path.dirname(destination), { recursive: true });
  copyFileSync(path.join(blockKartRoot, relative), destination);
}

try {
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
  const fixtureInputs = manifest.inputs.flatMap((entry) => {
    if (!entry.path.startsWith('workspace:')) return [entry.path];
    if (entry.path.startsWith(blockKartWorkspacePrefix)) {
      return [entry.path.slice(blockKartWorkspacePrefix.length)];
    }
    return [];
  });
  for (const relative of [
    'assets/blockkart.vpak',
    'assets/blockkart.vpak.provenance.json',
    ...fixtureInputs,
  ]) {
    copy(relative);
  }
  for (const [name, source] of Object.entries(moduleRoots)) {
    symlinkSync(source, path.join(tempRoot, name), 'dir');
  }
  execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
    cwd: fixtureRoot,
    stdio: 'pipe',
  });
  const payload = manifest.archiveEntries[0]?.sourcePath;
  if (!payload) throw new Error('canonical manifest has no payload fixture');
  const payloadPath = path.join(fixtureRoot, payload);
  writeFileSync(payloadPath, Buffer.concat([readFileSync(payloadPath), Buffer.from('\nnegative provenance mutation\n')]));
  const negative = spawnSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
    cwd: fixtureRoot,
    encoding: 'utf8',
  });
  if (negative.status === 0) {
    throw new Error('vpak provenance checker accepted a mutated payload source');
  }
  mkdirSync(outDir, { recursive: true });
  writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify({
    schemaVersion: 1,
    kind: 'blockkart.vpakProvenanceSelftest',
    status: 'pass',
    mutatedPath: payload,
    voCliPackageCount: currentCliInputs.packages.length,
    voCliInputCount: currentCliInputs.inputs.length,
    rejection: `${negative.stdout ?? ''}${negative.stderr ?? ''}`.slice(-2000),
  }, null, 2)}\n`);
  console.log(`blockkart vpak provenance selftest: ok rejected ${payload}`);
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
