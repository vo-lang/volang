#!/usr/bin/env node
import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdtempSync, readFileSync, readdirSync, rmSync } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  STUDIO_BROWSER_SMOKE_KIND,
  STUDIO_BROWSER_SMOKE_SCHEMA_VERSION,
  isBlockKartPerfFrameRecord,
  selectLatestBlockKartPerfFrame,
  validateStudioBrowserSmokeEvidence,
  webGpuRequirementFailure,
} from './studio_browser_smoke_contract.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));

function perfRecord(
  frame,
  receivedAt = '2026-07-17T00:00:01.000Z',
  studioPerfEpoch = 0,
) {
  return {
    receivedAt,
    payload: {
      schemaVersion: 1,
      source: 'blockkart',
      kind: 'perf-summary',
      frame,
      studioSessionId: 1,
      studioPerfEpoch,
      screen: { pixelWidth: 1280, pixelHeight: 720 },
      window: { frames: 300 },
      current: { frameMs: 8.2 },
      renderer: { submitFrameMs: 1.1 },
    },
  };
}

function screenshot(path) {
  return {
    path,
    bytes: 1024,
    sha256: 'a'.repeat(64),
    mediaType: 'image/png',
  };
}

function validEvidence() {
  return {
    schemaVersion: STUDIO_BROWSER_SMOKE_SCHEMA_VERSION,
    kind: STUDIO_BROWSER_SMOKE_KIND,
    status: 'ok',
    startedAt: '2026-07-17T00:00:00.000Z',
    quickplayStartedAt: '2026-07-17T00:00:00.000Z',
    visualCaptureCompletedAt: '2026-07-17T00:00:01.999Z',
    finishedAt: '2026-07-17T00:00:03.000Z',
    webGpu: {
      required: true,
      supported: true,
      adapter: true,
      probeExecutedInPage: true,
      reason: 'adapter available',
    },
    runtime: {
      ready: true,
      status: 'ready',
      kind: 'gui',
      isRunning: true,
      lastErrorPresent: true,
      lastError: null,
      moduleBytes: 32,
      renderBytes: 64,
      sessionId: 1,
    },
    renderer: { active: true, count: 1, sessionId: 1, capabilities: [{}] },
    quickplay: {
      href: 'http://127.0.0.1:4173/?proj=vo%3Aquickplay%3Ablockkart#/runner',
      entryPath: 'cmd/blockkart/main.vo',
      canvasWidth: 1280,
      canvasHeight: 720,
    },
    firstFrame: {
      ok: true,
      ready: true,
      skipped: false,
      source: 'voplay-perf-endpoint',
      frame: 300,
      reportCount: 1,
      record: perfRecord(300),
    },
    finalFrame: {
      ok: true,
      ready: true,
      skipped: false,
      source: 'voplay-perf-endpoint',
      frame: 301,
      reportCount: 1,
      notBefore: '2026-07-17T00:00:02.000Z',
      expectedPerfEpoch: 1,
      minimumFrameExclusive: 300,
      record: perfRecord(301, '2026-07-17T00:00:02.001Z', 1),
    },
    bytecodeContract: { ok: true, skipped: false, closureCount: 1, wrongCount: 0 },
    pageFailures: [],
    screenshots: {
      viewport: screenshot('target/studio-browser-smoke/viewport.png'),
      canvas: screenshot('target/studio-browser-smoke/canvas.png'),
    },
  };
}

const old = perfRecord(299, '2026-07-16T23:59:59.999Z');
const malformed = perfRecord(300);
delete malformed.payload.renderer;
const latest = perfRecord(301, '2026-07-17T00:00:02.000Z', 1);
const lateOldEpoch = perfRecord(400, '2026-07-17T00:00:03.000Z', 0);
assert.equal(isBlockKartPerfFrameRecord(old, '2026-07-17T00:00:00.000Z'), false);
assert.equal(isBlockKartPerfFrameRecord(malformed), false);
assert.equal(isBlockKartPerfFrameRecord(latest, 'not-a-timestamp'), false);
assert.equal(selectLatestBlockKartPerfFrame([old, malformed, latest], '2026-07-17T00:00:00.000Z'), latest);
assert.equal(
  selectLatestBlockKartPerfFrame(
    [latest, lateOldEpoch],
    '2026-07-17T00:00:00.000Z',
    latest.payload.studioSessionId,
    latest.payload.studioPerfEpoch,
    300,
  ),
  latest,
  'a late old-epoch record must not hide an already received matching frame',
);

assert.equal(webGpuRequirementFailure(false, { adapter: false }), null);
assert.match(webGpuRequirementFailure(true, { adapter: false, reason: 'requestAdapter returned null' }), /required/);
assert.deepEqual(validateStudioBrowserSmokeEvidence(validEvidence()), []);
const closedTimeWindowEvidence = validEvidence();
closedTimeWindowEvidence.firstFrame.record.receivedAt = closedTimeWindowEvidence.quickplayStartedAt;
closedTimeWindowEvidence.finalFrame.notBefore = closedTimeWindowEvidence.visualCaptureCompletedAt;
closedTimeWindowEvidence.finalFrame.record.receivedAt = closedTimeWindowEvidence.finishedAt;
assert.deepEqual(
  validateStudioBrowserSmokeEvidence(closedTimeWindowEvidence),
  [],
  'perf records at the closed runtime-window boundaries must remain valid',
);

for (const mutate of [
  (evidence) => { evidence.webGpu.adapter = false; },
  (evidence) => { evidence.webGpu.required = false; },
  (evidence) => { evidence.webGpu.probeExecutedInPage = false; },
  (evidence) => { evidence.renderer.active = false; },
  (evidence) => { evidence.renderer.sessionId = 2; },
  (evidence) => { evidence.renderer.capabilities = []; },
  (evidence) => { evidence.runtime.renderBytes = 0; },
  (evidence) => { evidence.runtime.moduleBytes = Number.NaN; },
  (evidence) => { evidence.runtime.lastErrorPresent = false; },
  (evidence) => { delete evidence.runtime.lastError; },
  (evidence) => { evidence.runtime.lastError = undefined; },
  (evidence) => { evidence.firstFrame.record.receivedAt = '2026-07-16T23:59:59.999Z'; },
  (evidence) => { evidence.firstFrame.record.receivedAt = '2026-07-17T00:00:01+00:00'; },
  (evidence) => { evidence.firstFrame.record.receivedAt = '2026-07-17T00:00:03.001Z'; },
  (evidence) => { evidence.firstFrame.frame = 301; },
  (evidence) => { evidence.firstFrame.record.payload.studioSessionId = 2; },
  (evidence) => { evidence.firstFrame.record.payload.studioPerfEpoch = -1; },
  (evidence) => { delete evidence.finalFrame; },
  (evidence) => { evidence.finalFrame.record.payload.studioSessionId = 2; },
  (evidence) => { evidence.finalFrame.record.payload.studioPerfEpoch = 0; },
  (evidence) => { evidence.finalFrame.expectedPerfEpoch = 0; },
  (evidence) => { evidence.finalFrame.frame = 300; evidence.finalFrame.record.payload.frame = 300; },
  (evidence) => { evidence.finalFrame.minimumFrameExclusive = 299; },
  (evidence) => { evidence.finalFrame.notBefore = '2026-07-17T00:00:00.999Z'; },
  (evidence) => { evidence.finalFrame.record.receivedAt = '2026-07-17T00:00:02+00:00'; },
  (evidence) => { evidence.finalFrame.record.receivedAt = '2026-07-17T00:00:03.001Z'; },
  (evidence) => { evidence.visualCaptureCompletedAt = 'not-a-timestamp'; },
  (evidence) => { evidence.visualCaptureCompletedAt = '2026-07-17T00:00:01+00:00'; },
  (evidence) => { evidence.finalFrame.notBefore = '2026-07-17T00:00:01.998Z'; },
  (evidence) => { evidence.firstFrame.record.payload.renderer.submitFrameMs = null; },
  (evidence) => { evidence.bytecodeContract.skipped = true; },
  (evidence) => { evidence.bytecodeContract.closureCount = 0; },
  (evidence) => { evidence.quickplay.canvasWidth = 0; },
  (evidence) => { evidence.finishedAt = 'not-a-timestamp'; },
  (evidence) => { evidence.screenshots.canvas.sha256 = 'bad'; },
  (evidence) => { evidence.screenshots.canvas.path = '../canvas.png'; },
  (evidence) => { evidence.pageFailures.push('console.error: boom'); },
]) {
  const evidence = validEvidence();
  mutate(evidence);
  assert.ok(validateStudioBrowserSmokeEvidence(evidence).length > 0);
}

const signalOutDir = mkdtempSync(path.join(os.tmpdir(), 'studio-browser-smoke-signal-'));
try {
  const signalResult = spawnSync(
    process.execPath,
    [path.join(root, 'scripts/ci/studio_browser_smoke.mjs'), '--selftest-signal-failure-evidence'],
    {
      cwd: root,
      env: { ...process.env, STUDIO_BROWSER_SMOKE_OUT_DIR: signalOutDir },
      encoding: 'utf8',
      timeout: 10000,
    },
  );
  assert.equal(signalResult.error, undefined, signalResult.error?.message);
  assert.equal(signalResult.signal, null);
  assert.equal(signalResult.status, 143, signalResult.stderr);
  const failure = JSON.parse(readFileSync(path.join(signalOutDir, 'studio-browser-smoke.json'), 'utf8'));
  assert.equal(failure.schemaVersion, STUDIO_BROWSER_SMOKE_SCHEMA_VERSION);
  assert.equal(failure.kind, `${STUDIO_BROWSER_SMOKE_KIND}.failure`);
  assert.equal(failure.status, 'failed');
  assert.equal(failure.stage, 'received SIGTERM');
  assert.equal(failure.error, 'terminated by SIGTERM');
  assert.deepEqual(
    readdirSync(signalOutDir).filter((entry) => entry.includes('.tmp-')),
    [],
    'signal failure evidence must not leave an atomic-write temporary file',
  );
} finally {
  rmSync(signalOutDir, { recursive: true, force: true });
}

const timeoutOutDir = mkdtempSync(path.join(os.tmpdir(), 'studio-browser-smoke-timeout-'));
try {
  const timeoutResult = spawnSync(
    process.execPath,
    [path.join(root, 'scripts/ci/studio_browser_smoke.mjs'), '--selftest-timeout-failure-evidence'],
    {
      cwd: root,
      env: {
        ...process.env,
        STUDIO_BROWSER_SMOKE_OUT_DIR: timeoutOutDir,
        STUDIO_BROWSER_SMOKE_TOTAL_TIMEOUT_MS: '25',
      },
      encoding: 'utf8',
      timeout: 10000,
    },
  );
  assert.equal(timeoutResult.error, undefined, timeoutResult.error?.message);
  assert.equal(timeoutResult.signal, null);
  assert.equal(timeoutResult.status, 124, timeoutResult.stderr);
  const failure = JSON.parse(readFileSync(path.join(timeoutOutDir, 'studio-browser-smoke.json'), 'utf8'));
  assert.equal(failure.schemaVersion, STUDIO_BROWSER_SMOKE_SCHEMA_VERSION);
  assert.equal(failure.kind, `${STUDIO_BROWSER_SMOKE_KIND}.failure`);
  assert.equal(failure.status, 'failed');
  assert.equal(failure.stage, 'total deadline exceeded');
  assert.equal(failure.error, 'total timeout after 25ms');
  assert.deepEqual(
    readdirSync(timeoutOutDir).filter((entry) => entry.includes('.tmp-')),
    [],
    'timeout failure evidence must not leave an atomic-write temporary file',
  );
} finally {
  rmSync(timeoutOutDir, { recursive: true, force: true });
}

console.log('Studio browser smoke contract selftest: ok');
