#!/usr/bin/env node
import assert from 'node:assert/strict';
import { execFileSync } from 'node:child_process';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import {
  sourceBoundEvidence,
  verifySourceBoundEvidence,
} from './source_bound_evidence.mjs';

const root = mkdtempSync(path.join(os.tmpdir(), 'volang-source-bound-evidence-'));
const previousRunId = process.env.VO_DEV_CI_RUN_ID;

function git(args) {
  return execFileSync('git', args, { cwd: root, encoding: 'utf8' }).trim();
}

try {
  writeFileSync(path.join(root, 'gate.mjs'), 'export const gate = true;\n');
  writeFileSync(path.join(root, 'artifact.json'), '{"status":"pass"}\n');
  git(['init', '-q']);
  git(['add', '.']);
  git(['-c', 'user.name=Evidence Test', '-c', 'user.email=evidence@example.invalid', 'commit', '-qm', 'fixture']);
  process.env.VO_DEV_CI_RUN_ID = 'selftest-run';

  const evidence = sourceBoundEvidence({
    gate: 'selftest-gate',
    generatedAt: new Date().toISOString(),
    root,
    repos: [{ name: 'fixture', root }],
    gateFiles: ['gate.mjs'],
    artifacts: ['artifact.json'],
  });
  assert.deepEqual(verifySourceBoundEvidence({
    evidence,
    expectedGate: 'selftest-gate',
    expectedCiRunId: 'selftest-run',
    root,
  }), []);

  const forgedDigest = structuredClone(evidence);
  forgedDigest.gateDigest = `sha256:${'0'.repeat(64)}`;
  assert(verifySourceBoundEvidence({
    evidence: forgedDigest,
    expectedGate: 'selftest-gate',
    expectedCiRunId: 'selftest-run',
    root,
  }).some((issue) => issue.includes('gateDigest')));

  assert(verifySourceBoundEvidence({
    evidence,
    expectedGate: 'selftest-gate',
    expectedCiRunId: 'different-run',
    root,
  }).some((issue) => issue.includes('ciRunId')));

  writeFileSync(path.join(root, 'artifact.json'), '{"status":"forged-pass"}\n');
  assert(verifySourceBoundEvidence({
    evidence,
    expectedGate: 'selftest-gate',
    expectedCiRunId: 'selftest-run',
    root,
  }).some((issue) => issue.includes('artifactDigest')));

  console.log('source-bound-evidence selftest: ok');
} finally {
  if (previousRunId === undefined) {
    delete process.env.VO_DEV_CI_RUN_ID;
  } else {
    process.env.VO_DEV_CI_RUN_ID = previousRunId;
  }
  rmSync(root, { recursive: true, force: true });
}
