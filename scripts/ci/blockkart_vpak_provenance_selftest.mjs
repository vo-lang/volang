#!/usr/bin/env node
import { execFileSync, spawnSync } from 'node:child_process';
import { copyFileSync, mkdirSync, mkdtempSync, readFileSync, rmSync, symlinkSync, writeFileSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
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
const tempRoot = mkdtempSync(path.join(os.tmpdir(), 'blockkart-vpak-provenance-selftest-'));
const fixtureRoot = path.join(tempRoot, 'BlockKart');
const blockKartWorkspacePrefix = 'workspace:github.com/vo-lang/blockkart/';

function copy(relative) {
  const destination = path.join(fixtureRoot, relative);
  mkdirSync(path.dirname(destination), { recursive: true });
  copyFileSync(path.join(blockKartRoot, relative), destination);
}

try {
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
    rejection: `${negative.stdout ?? ''}${negative.stderr ?? ''}`.slice(-2000),
  }, null, 2)}\n`);
  console.log(`blockkart vpak provenance selftest: ok rejected ${payload}`);
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
