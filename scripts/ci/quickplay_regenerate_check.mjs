#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const checkedDir = path.join(root, 'apps/studio/public/quickplay/blockkart');
const generator = path.join(root, 'apps/studio/scripts/package_blockkart_quickplay.mjs');
const reportDir = path.resolve(process.env.QUICKPLAY_REGENERATE_CHECK_OUT_DIR || path.join(root, 'target/quickplay-regenerate-check'));
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const dependencyRepos = [
  { name: 'github.com/vo-lang/vogui', root: requireRepoRoot('VOGUI_ROOT', 'vogui') },
  { name: 'github.com/vo-lang/voplay', root: requireRepoRoot('VOPLAY_ROOT', 'voplay') },
  { name: 'github.com/vo-lang/vopack', root: requireRepoRoot('VOPACK_ROOT', 'vopack') },
];

function writeReport(status, details = {}) {
  const generatedAt = new Date().toISOString();
  mkdirSync(reportDir, { recursive: true });
  writeFileSync(path.join(reportDir, 'report.json'), `${JSON.stringify({
    schemaVersion: 1,
    kind: 'quickplay.regenerateCheckReport',
    gate: 'quickplay-regenerate-check',
    status,
    generatedAt,
    freshEvidence: sourceBoundEvidence({
      gate: 'quickplay-regenerate-check',
      generatedAt,
      root,
      repos: [
        { name: 'volang', root },
        { name: 'BlockKart', root: blockKartRoot },
        ...dependencyRepos,
      ],
      gateFiles: [
        'scripts/ci/quickplay_regenerate_check.mjs',
        'scripts/ci/repo_roots.mjs',
        'scripts/ci/source_bound_evidence.mjs',
        'apps/studio/scripts/package_blockkart_quickplay.mjs',
        'eng/tasks.toml',
        'eng/project.toml',
      ],
      artifacts: [checkedDir],
    }),
    checkedDir,
    ...details,
  }, null, 2)}\n`);
}

function fail(message, details = {}) {
  writeReport('failed', { message, ...details });
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

function voplayPackageFacts(dir) {
  const provenance = JSON.parse(readFileSync(path.join(dir, 'provenance.json'), 'utf8'));
  const producer = (provenance.producers ?? []).find((entry) => entry?.id === 'voplay-current-source-wasm') ?? null;
  const dependency = (provenance.dependencies ?? []).find((entry) => entry?.module === 'github.com/vo-lang/voplay');
  const artifactPaths = (dependency?.artifacts ?? [])
    .map((artifact) => artifact.url?.replace(/^\/quickplay\/blockkart\//, ''))
    .filter(Boolean);
  return { artifactPaths, producer };
}

function stableProducerIdentity(producer) {
  return {
    command: producer?.command ?? null,
    source: producer?.source ?? null,
    toolchain: producer?.toolchain ?? null,
    outputNames: (producer?.outputs ?? []).map((output) => output.name),
  };
}

function acceptedCrossPlatformVoplayVariant(diff, actualDir, expectedDir) {
  if (diff.missing.length > 0 || diff.extra.length > 0 || diff.changed.length === 0) return null;
  const actual = voplayPackageFacts(actualDir);
  const expected = voplayPackageFacts(expectedDir);
  const allowedFiles = new Set([
    'deps.json',
    'project.json',
    'provenance.json',
    ...actual.artifactPaths,
    ...expected.artifactPaths,
  ]);
  if (!diff.changed.every(({ path: file }) => allowedFiles.has(file))) return null;
  const actualProducer = actual.producer;
  const expectedProducer = expected.producer;
  if (!actualProducer?.buildPlatform || !expectedProducer?.buildPlatform) return null;
  if (JSON.stringify(actualProducer.buildPlatform) === JSON.stringify(expectedProducer.buildPlatform)) return null;
  if (JSON.stringify(stableProducerIdentity(actualProducer)) !== JSON.stringify(stableProducerIdentity(expectedProducer))) return null;
  return {
    generatedPlatform: actualProducer.buildPlatform,
    checkedPlatform: expectedProducer.buildPlatform,
    changedFiles: diff.changed.map(({ path: file }) => file),
  };
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
    fail(`generator failed with status ${result.status}\n${result.stdout}${result.stderr}`, {
      generatorStatus: result.status,
      stdout: result.stdout,
      stderr: result.stderr,
    });
  }
  const diff = compareDirs(outDir, checkedDir);
  if (diff.missing.length > 0 || diff.extra.length > 0 || diff.changed.length > 0) {
    const platformVariant = acceptedCrossPlatformVoplayVariant(diff, outDir, checkedDir);
    if (!platformVariant) {
      fail(`regenerated package differs from checked-in artifact\n${JSON.stringify(diff, null, 2)}`, { diff });
    }
    writeReport('ok', { diff, platformVariant });
    console.log(`quickplay regenerate check: ok (cross-platform voplay variant ${platformVariant.checkedPlatform.os}/${platformVariant.checkedPlatform.arch} -> ${platformVariant.generatedPlatform.os}/${platformVariant.generatedPlatform.arch})`);
  } else {
    writeReport('ok', { diff });
    console.log('quickplay regenerate check: ok');
  }
} finally {
  rmSync(tempRoot, { recursive: true, force: true });
}
