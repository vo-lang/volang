#!/usr/bin/env node
import { execFileSync } from 'node:child_process';
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const voBin = path.join(root, 'target', 'debug', 'vo');
const outDir = path.resolve(process.env.BLOCKKART_VPAK_OUT_DIR ?? path.join(root, 'target', 'blockkart-vpak-build'));

execFileSync('cargo', ['build', '-q', '-p', 'vo'], { cwd: root, env: process.env, stdio: 'inherit' });
execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--build'], {
  cwd: blockKartRoot,
  env: { ...process.env, VO_BIN: voBin },
  stdio: 'inherit',
});
execFileSync(process.execPath, ['tools/vpak_provenance.mjs', '--check'], {
  cwd: blockKartRoot,
  env: process.env,
  stdio: 'inherit',
});

const producer = JSON.parse(readFileSync(path.join(blockKartRoot, 'assets/blockkart.vpak.provenance.json'), 'utf8'));
if (producer.archiveEntryCount !== 37 || producer.payloadInputCount !== 37 || Number(producer.workspaceSourceInputCount ?? 0) <= 0) {
  throw new Error(`BlockKart vpak closure must contain 37 payloads and current workspace sources, found entries=${producer.archiveEntryCount} inputs=${producer.payloadInputCount} workspace=${producer.workspaceSourceInputCount ?? 0}`);
}
mkdirSync(outDir, { recursive: true });
writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify({
  schemaVersion: 1,
  kind: 'blockkart.vpakBuildReport',
  ciRunId: process.env.VO_DEV_CI_RUN_ID ?? null,
  archiveEntryCount: producer.archiveEntryCount,
  payloadInputCount: producer.payloadInputCount,
  workspaceSourceInputCount: producer.workspaceSourceInputCount,
  producerDigest: producer.producerDigest,
  pack: producer.pack,
}, null, 2)}\n`);
console.log(`blockkart vpak build: ok entries=${producer.archiveEntryCount} producer=${producer.producerDigest}`);
