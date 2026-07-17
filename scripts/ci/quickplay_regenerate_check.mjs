#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, readdirSync, rmSync, writeFileSync, mkdirSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  CURRENT_VO_CLI_BUILD_TIMEOUT_MS,
  sha256Digest,
} from './quickplay_vnext.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const checked = path.join(root, 'apps', 'studio', 'public', 'quickplay', 'blockkart');
const generator = path.join(root, 'apps', 'studio', 'scripts', 'package_blockkart_quickplay.mjs');
const reportRoot = path.resolve(
  process.env.QUICKPLAY_REGENERATE_CHECK_OUT_DIR
    ?? path.join(root, 'target', 'quickplay-regenerate-check'),
);

function snapshot(directory, relative = '') {
  const current = relative === '' ? directory : path.join(directory, ...relative.split('/'));
  const entries = [];
  for (const entry of readdirSync(current, { withFileTypes: true })) {
    const child = relative === '' ? entry.name : `${relative}/${entry.name}`;
    if (entry.isDirectory()) entries.push(...snapshot(directory, child));
    else if (entry.isFile()) {
      const bytes = readFileSync(path.join(directory, ...child.split('/')));
      entries.push({ path: child, size: bytes.byteLength, digest: sha256Digest(bytes) });
    } else throw new Error(`unsupported Quickplay output entry: ${child}`);
  }
  return entries.sort((left, right) => Buffer.compare(Buffer.from(left.path), Buffer.from(right.path)));
}

const temporary = mkdtempSync(path.join(os.tmpdir(), 'quickplay-regenerate-'));
const generated = path.join(temporary, 'blockkart');
let report;
try {
  const result = spawnSync(process.execPath, [generator], {
    cwd: root,
    env: { ...process.env, BLOCKKART_QUICKPLAY_OUT_ROOT: generated },
    encoding: 'utf8',
    maxBuffer: 32 * 1024 * 1024,
    // The generator may need one cold current-root Vo CLI build. Leave time
    // for snapshotting, packaging, and verification after that producer exits.
    timeout: CURRENT_VO_CLI_BUILD_TIMEOUT_MS + 15 * 60 * 1000,
  });
  if (result.error || result.status !== 0) {
    throw new Error(result.error?.message || result.stderr.trim() || `generator exited ${result.status}`);
  }
  const expected = snapshot(checked);
  const found = snapshot(generated);
  if (JSON.stringify(found) !== JSON.stringify(expected)) {
    const expectedByPath = new Map(expected.map((entry) => [entry.path, entry]));
    const foundByPath = new Map(found.map((entry) => [entry.path, entry]));
    const paths = new Set([...expectedByPath.keys(), ...foundByPath.keys()]);
    const changed = [...paths].filter((item) => (
      JSON.stringify(expectedByPath.get(item)) !== JSON.stringify(foundByPath.get(item))
    )).sort();
    throw new Error(`checked Quickplay package is stale: ${changed.join(', ')}`);
  }
  report = { schemaVersion: 1, status: 'ok', files: found.length };
  console.log(`Quickplay regenerate check: ok (${found.length} files)`);
} catch (error) {
  report = { schemaVersion: 1, status: 'failed', message: String(error?.message ?? error) };
  console.error(report.message);
  process.exitCode = 1;
} finally {
  mkdirSync(reportRoot, { recursive: true });
  writeFileSync(path.join(reportRoot, 'report.json'), `${JSON.stringify(report, null, 2)}\n`);
  rmSync(temporary, { recursive: true, force: true });
}
