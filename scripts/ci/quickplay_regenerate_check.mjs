#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const root = fileURLToPath(new URL('../..', import.meta.url));
const checkedDir = path.join(root, 'apps/studio/public/quickplay/blockkart');
const generator = path.join(root, 'apps/studio/scripts/package_blockkart_quickplay.mjs');

function fail(message) {
  console.error(`quickplay regenerate check: ${message}`);
  process.exit(1);
}

function sha256(bytes) {
  return `sha256:${createHash('sha256').update(bytes).digest('hex')}`;
}

function walkFiles(dir) {
  const files = [];
  function walk(current) {
    for (const entry of readdirSync(current, { withFileTypes: true })) {
      const absolute = path.join(current, entry.name);
      if (entry.isDirectory()) {
        walk(absolute);
      } else if (entry.isFile()) {
        files.push(path.relative(dir, absolute).split(path.sep).join('/'));
      }
    }
  }
  walk(dir);
  files.sort();
  return files;
}

function compareDirs(actualDir, expectedDir) {
  const actual = walkFiles(actualDir);
  const expected = walkFiles(expectedDir);
  const actualSet = new Set(actual);
  const expectedSet = new Set(expected);
  const missing = expected.filter((file) => !actualSet.has(file));
  const extra = actual.filter((file) => !expectedSet.has(file));
  const changed = [];
  for (const file of expected) {
    if (!actualSet.has(file)) continue;
    const actualPath = path.join(actualDir, file);
    const expectedPath = path.join(expectedDir, file);
    const actualBytes = readFileSync(actualPath);
    const expectedBytes = readFileSync(expectedPath);
    if (actualBytes.byteLength !== expectedBytes.byteLength || sha256(actualBytes) !== sha256(expectedBytes)) {
      changed.push({
        path: file,
        actual: { digest: sha256(actualBytes), size: actualBytes.byteLength },
        expected: { digest: sha256(expectedBytes), size: expectedBytes.byteLength },
      });
    }
  }
  return { missing, extra, changed };
}

if (!existsSync(checkedDir) || !statSync(checkedDir).isDirectory()) {
  fail(`checked-in quickplay package is missing: ${checkedDir}`);
}

const tempRoot = mkdtempSync(path.join(os.tmpdir(), 'blockkart-quickplay-regenerate-'));
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
  });
  if (result.status !== 0) {
    fail(`generator failed with status ${result.status}\n${result.stdout}${result.stderr}`);
  }
  const diff = compareDirs(outDir, checkedDir);
  if (diff.missing.length > 0 || diff.extra.length > 0 || diff.changed.length > 0) {
    fail(`regenerated package differs from checked-in artifact\n${JSON.stringify(diff, null, 2)}`);
  }
  console.log('quickplay regenerate check: ok');
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
