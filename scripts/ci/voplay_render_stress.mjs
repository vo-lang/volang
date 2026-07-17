#!/usr/bin/env node
import { spawn, spawnSync } from 'node:child_process';
import { existsSync, mkdirSync, readFileSync, readdirSync, renameSync, rmSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  BLOCKKART_BASELINE_BUDGET_DEFAULTS,
  BLOCKKART_BASELINE_MAX_TIMER_MS,
  blockKartBaselineObserverTimeoutBudget,
  blockKartBaselineTimeoutBreakdown,
  blockKartBaselineTimeoutBudget,
  configuredBlockKartBaselineTimeout,
} from './blockkart_baseline_budget.mjs';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const outDir = path.resolve(argValue('--out-dir') || process.env.VOPLAY_RENDER_STRESS_OUT_DIR || path.join(root, 'target/voplay-render-stress'));
const restartCount = nonNegativeInt(argValue('--restart-count') || process.env.VOPLAY_RENDER_STRESS_RESTART_COUNT, 50);
const captureMs = positiveInt(argValue('--capture-ms') || process.env.VOPLAY_RENDER_STRESS_CAPTURE_MS, 36000);
const pulseMode = normalizePulseMode(argValue('--pulse-mode') || process.env.VOPLAY_RENDER_STRESS_PULSE_MODE || 'hybrid');
const budgetAllScenes = boolOption('--budget-all', process.env.VOPLAY_RENDER_STRESS_BUDGET_ALL, false);
const coverageCaptureDefaultMs = budgetAllScenes ? captureMs : Math.min(captureMs, 14000);
const coverageCaptureMs = positiveInt(argValue('--coverage-capture-ms') || process.env.VOPLAY_RENDER_STRESS_COVERAGE_CAPTURE_MS, coverageCaptureDefaultMs);
const soakOnly = budgetAllScenes && restartCount === 0 && captureMs >= 600000 && coverageCaptureMs >= 600000;
const budgetPath = path.resolve(argValue('--budget') || process.env.VOPLAY_RENDER_STRESS_BUDGET || path.join(root, 'eng/perf-budgets/blockkart-voplay.medium.json'));
if (!existsSync(budgetPath)) fail(`budget file missing: ${budgetPath}`);
const perfBudget = readJson(budgetPath);
const targetFps = positiveInt(perfBudget?.target?.fps, 60);
const targetFrameMs = 1000 / targetFps;
const renderBudget = perfBudget?.render ?? {};
let renderStressConfigurationError = null;
let heartbeatIntervalMs = 15000;
try {
  heartbeatIntervalMs = canonicalPositiveTimer(
    argValue('--heartbeat-ms') || process.env.VOPLAY_RENDER_STRESS_HEARTBEAT_MS,
    15000,
    'VOPLAY_RENDER_STRESS_HEARTBEAT_MS/--heartbeat-ms',
  );
} catch (error) {
  renderStressConfigurationError = error instanceof Error ? error.message : String(error);
}
let activeObservedProcess = null;
let renderStressTermination = null;
let renderStressFailureEvidenceWritten = false;

function argValue(name) {
  const index = process.argv.indexOf(name);
  return index === -1 ? '' : process.argv[index + 1] ?? '';
}

function nonNegativeInt(value, fallback) {
  const parsed = Number.parseInt(String(value ?? ''), 10);
  return Number.isFinite(parsed) && parsed >= 0 ? parsed : fallback;
}

function positiveInt(value, fallback) {
  const parsed = Number.parseInt(String(value ?? ''), 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function canonicalPositiveTimer(value, fallback, name) {
  if (value == null || String(value).trim() === '') return fallback;
  const raw = String(value).trim();
  if (!/^[1-9][0-9]*$/.test(raw)) {
    throw new Error(`${name} must be a canonical positive integer`);
  }
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed > BLOCKKART_BASELINE_MAX_TIMER_MS) {
    throw new Error(`${name} exceeds the Node timer limit (${BLOCKKART_BASELINE_MAX_TIMER_MS}ms)`);
  }
  return parsed;
}

function boolOption(name, envValue, fallback) {
  const index = process.argv.indexOf(name);
  if (index !== -1) {
    const raw = process.argv[index + 1];
    if (raw && !raw.startsWith('--')) {
      return parseBool(raw, true);
    }
    return true;
  }
  if (envValue != null && String(envValue) !== '') {
    return parseBool(envValue, fallback);
  }
  return fallback;
}

function parseBool(value, fallback) {
  const raw = String(value ?? '').trim().toLowerCase();
  if (raw === '1' || raw === 'true' || raw === 'yes' || raw === 'on') return true;
  if (raw === '0' || raw === 'false' || raw === 'no' || raw === 'off') return false;
  return fallback;
}

function normalizePulseMode(value) {
  const mode = String(value ?? '').trim().toLowerCase();
  if (mode === '' || mode === 'default') {
    return '';
  }
  if (mode === 'raf' || mode === 'timer' || mode === 'hybrid') {
    return mode;
  }
  fail(`unsupported --pulse-mode "${value}"; expected raf, timer, hybrid, or default`);
}

function readJson(file) {
  return JSON.parse(readFileSync(file, 'utf8'));
}

function writeJsonAtomic(file, value) {
  mkdirSync(path.dirname(file), { recursive: true });
  const temporary = `${file}.tmp-${process.pid}`;
  try {
    writeFileSync(temporary, `${JSON.stringify(value, null, 2)}\n`);
    renameSync(temporary, file);
  } finally {
    rmSync(temporary, { force: true });
  }
}

function writeRenderStressFailureEvidence(stage, error, detail = {}) {
  if (renderStressFailureEvidenceWritten) return;
  renderStressFailureEvidenceWritten = true;
  writeJsonAtomic(path.join(outDir, 'report.json'), {
    schemaVersion: 1,
    kind: 'voplay.renderStressReport.failure',
    status: 'failed',
    generatedAt: new Date().toISOString(),
    stage,
    error,
    ...detail,
  });
}

function writeRenderStressInterruptionEvidence(signal) {
  writeRenderStressFailureEvidence('received process signal', `terminated by ${signal}`, {
    signal,
    activeScene: activeObservedProcess?.scene ?? null,
    activeChildPid: activeObservedProcess?.child?.pid ?? null,
  });
}

function handleRenderStressSignal(signal, exitCode) {
  if (renderStressTermination) return;
  renderStressTermination = { signal, exitCode };
  try {
    writeRenderStressInterruptionEvidence(signal);
    activeObservedProcess?.markInterrupted?.(signal);
  } catch (error) {
    console.error(`voplay render stress: could not write interruption evidence: ${error instanceof Error ? error.message : String(error)}`);
  }
  const child = activeObservedProcess?.child ?? null;
  if (!child || child.exitCode !== null || child.signalCode !== null) {
    process.exit(exitCode);
  }
  child.kill(signal);
  const forceKill = setTimeout(() => {
    if (child.exitCode === null && child.signalCode === null) {
      child.kill('SIGKILL');
    }
  }, 10000);
  const forceExit = setTimeout(() => process.exit(exitCode), 12000);
  child.once('close', () => {
    clearTimeout(forceKill);
    clearTimeout(forceExit);
    process.exit(exitCode);
  });
}

for (const [signal, exitCode] of [['SIGINT', 130], ['SIGTERM', 143]]) {
  process.on(signal, () => handleRenderStressSignal(signal, exitCode));
}

function fail(message) {
  console.error(`voplay render stress: ${message}`);
  process.exit(1);
}

if (renderStressConfigurationError) {
  rmSync(outDir, { recursive: true, force: true });
  mkdirSync(outDir, { recursive: true });
  writeRenderStressFailureEvidence(
    'configuration validation',
    renderStressConfigurationError,
  );
  fail(renderStressConfigurationError);
}

async function runBaseline(name, args, options = {}) {
  const sceneOut = path.join(outDir, name);
  rmSync(sceneOut, { recursive: true, force: true });
  mkdirSync(sceneOut, { recursive: true });
  const command = [
    'scripts/ci/blockkart_baseline.mjs',
    '--out-dir',
    sceneOut,
    '--capture-ms',
    String(options.captureMs ?? captureMs),
    ...(pulseMode ? ['--pulse-mode', pulseMode] : []),
    ...args,
  ];
  const sceneCaptureMs = Number(options.captureMs ?? captureMs);
  const restartArgumentIndex = args.indexOf('--restart-count');
  const sceneRestartCount = restartArgumentIndex === -1
    ? 0
    : nonNegativeInt(args[restartArgumentIndex + 1], 0);
  const budgetOptions = {
    captureMs: sceneCaptureMs,
    restartCount: sceneRestartCount,
    renderStressScenario: args.includes('--stress-profile'),
    startRaceScenario: args.includes('--start-race'),
    storageReloadScenario: args.includes('--verify-storage-reload'),
    resizeScenario: args.includes('--resize-cycle'),
  };
  const childTimeoutMs = blockKartBaselineTimeoutBudget(budgetOptions);
  const timeoutMs = positiveInt(
    options.timeoutMs,
    blockKartBaselineObserverTimeoutBudget(budgetOptions),
  );
  if (timeoutMs <= childTimeoutMs) {
    throw new Error(`baseline observer timeout ${timeoutMs}ms must exceed child timeout ${childTimeoutMs}ms`);
  }
  const heartbeatPath = path.join(sceneOut, 'heartbeat.json');
  const result = await runObservedProcess(
    name,
    command,
    heartbeatPath,
    timeoutMs,
    formalBaselineEnvironment(childTimeoutMs, sceneCaptureMs),
  );
  const logPath = path.join(sceneOut, 'baseline-run.log');
  writeFileSync(logPath, `${result.stdout ?? ''}${result.stderr ?? ''}`);
  const jsonPath = path.join(sceneOut, 'blockkart-baseline.json');
  const markdownPath = path.join(sceneOut, 'blockkart-baseline.md');
  if (result.status !== 0 && !(options.allowBaselineFailureWithJson && existsSync(jsonPath))) {
    const issue = result.timedOut
      ? { code: 'baseline.timeout', severity: 0, detail: JSON.stringify(result.timeoutDiagnostic) }
      : { code: 'baseline.command_failed', severity: 0, detail: tail(`${result.stdout ?? ''}${result.stderr ?? ''}`) };
    return {
      name,
      status: 'fail',
      command: [process.execPath, ...command],
      artifacts: { directory: sceneOut, log: logPath, heartbeat: heartbeatPath, json: existsSync(jsonPath) ? jsonPath : null, markdown: existsSync(markdownPath) ? markdownPath : null },
      observability: result.observability,
      issues: [issue],
    };
  }
  if (!existsSync(jsonPath)) {
    return {
      name,
      status: 'fail',
      command: [process.execPath, ...command],
      artifacts: { directory: sceneOut, log: logPath, heartbeat: heartbeatPath, json: null, markdown: existsSync(markdownPath) ? markdownPath : null },
      observability: result.observability,
      issues: [{ code: 'baseline.missing_json', severity: 0, detail: `${jsonPath} was not written` }],
    };
  }
  return summarizeBaselineScene(name, readJson(jsonPath), { sceneOut, logPath, heartbeatPath, jsonPath, markdownPath, command: [process.execPath, ...command], observability: result.observability }, options);
}

function formalBaselineEnvironment(childTimeoutMs, sceneCaptureMs, baseEnvironment = process.env) {
  return {
    ...baseEnvironment,
    BLOCKKART_BASELINE_VIEWPORT_WIDTH: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.viewportWidth),
    BLOCKKART_BASELINE_VIEWPORT_HEIGHT: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.viewportHeight),
    BLOCKKART_BASELINE_EXPECT_LIFECYCLE: 'Running',
    BLOCKKART_BASELINE_FIRST_FRAME_TIMEOUT_MS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.firstFrameTimeoutMs),
    BLOCKKART_BASELINE_CAPTURE_MS: String(sceneCaptureMs),
    BLOCKKART_BASELINE_LONG_RUN_TELEMETRY_THRESHOLD_MS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.longRunTelemetryThresholdMs),
    BLOCKKART_BASELINE_LONG_RUN_TELEMETRY_MAX_AGE_MS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.longRunTelemetryMaxAgeMs),
    BLOCKKART_BASELINE_LONG_RUN_TELEMETRY_SPAN_GRACE_MS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.longRunTelemetrySpanGraceMs),
    BLOCKKART_BASELINE_STARTUP_WARN_MS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.startupWarnMs),
    BLOCKKART_BASELINE_MAX_SLOW_FRAMES: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.maxSlowFrames),
    BLOCKKART_BASELINE_RESTART_COUNT: '0',
    BLOCKKART_BASELINE_RESTART_TIMEOUT_MS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.restartWaitTimeoutMs),
    BLOCKKART_BASELINE_START_RACE: '0',
    BLOCKKART_BASELINE_VERIFY_STORAGE_RELOAD: '0',
    BLOCKKART_BASELINE_STRESS_PROFILE: '',
    BLOCKKART_BASELINE_RESIZE_CYCLE: '0',
    BLOCKKART_BASELINE_CDP_SCREENSHOT_ATTEMPTS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.cdpScreenshotAttempts),
    BLOCKKART_BASELINE_CDP_SCREENSHOT_TIMEOUT_MS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.cdpScreenshotTimeoutMs),
    BLOCKKART_BASELINE_CANVAS_DATA_URL_ATTEMPTS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.canvasDataUrlAttempts),
    BLOCKKART_BASELINE_CANVAS_DATA_URL_TIMEOUT_MS: String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.canvasDataUrlTimeoutMs),
    BLOCKKART_BASELINE_TIMEOUT_MS: String(childTimeoutMs),
    BLOCKKART_BASELINE_NO_FAIL: '0',
    BLOCKKART_BASELINE_REQUIRE_WEBGPU: '1',
    BLOCKKART_BASELINE_SIMULATE_FAILURE: '',
    BLOCKKART_BASELINE_VISUAL_CAPTURE: '1',
    BLOCKKART_BASELINE_PULSE_MODE: '',
    BLOCKKART_BASELINE_PERF_MODE: 'stats',
    BLOCKKART_BASELINE_PERF_CONSOLE: '0',
    BLOCKKART_BASELINE_PERF_DIAG: 'pulseHybrid',
    BLOCKKART_BASELINE_PERF_GPU_PROBE: '0',
  };
}

function runObservedProcess(scene, command, heartbeatPath, timeoutMs, environment = process.env) {
  const startedAt = Date.now();
  let stdout = '';
  let stderr = '';
  let stage = 'spawn';
  let lastTelemetry = null;
  let lastOutputAt = startedAt;
  let timedOut = false;
  let settled = false;
  const timeline = [{ at: new Date(startedAt).toISOString(), elapsedMs: 0, stage, detail: command.join(' ') }];
  const child = spawn(process.execPath, command, {
    cwd: root,
    env: { ...environment },
    stdio: ['ignore', 'pipe', 'pipe'],
  });
  const activeToken = { child, scene, heartbeatPath, markInterrupted: null };
  activeObservedProcess = activeToken;
  const recordStage = (nextStage, detail = '') => {
    if (!nextStage || (nextStage === stage && detail === '')) return;
    stage = nextStage;
    timeline.push({ at: new Date().toISOString(), elapsedMs: Date.now() - startedAt, stage, detail });
    writeHeartbeat('running');
  };
  const consume = (stream, chunk) => {
    const text = chunk.toString();
    if (stream === 'stdout') stdout += text;
    else stderr += text;
    lastOutputAt = Date.now();
    for (const line of text.split(/\r?\n/)) {
      const match = line.match(/BlockKart baseline progress:\s*(.+)/);
      if (!match) continue;
      const detail = match[1].trim();
      const telemetryMatch = detail.match(/^telemetry\s+(.+)$/);
      if (telemetryMatch) {
        try {
          lastTelemetry = JSON.parse(telemetryMatch[1]);
        } catch {
          lastTelemetry = { telemetryError: 'malformed baseline telemetry progress', raw: tail(telemetryMatch[1]) };
        }
      }
      recordStage(baselineProgressStage(detail, lastTelemetry), detail);
    }
  };
  function heartbeat(status) {
    return {
      schemaVersion: 1,
      kind: 'voplay.renderStressHeartbeat',
      scene,
      status,
      stage,
      elapsedMs: Date.now() - startedAt,
      timeoutMs,
      pid: child.pid ?? null,
      lastOutputAgoMs: Date.now() - lastOutputAt,
      stdoutBytes: Buffer.byteLength(stdout),
      stderrBytes: Buffer.byteLength(stderr),
      frameIndex: lastTelemetry?.frameIndex ?? null,
      lastPass: lastTelemetry?.pass ?? null,
      frameP90Ms: lastTelemetry?.frameP90Ms ?? null,
      frameP99Ms: lastTelemetry?.frameP99Ms ?? null,
      resourceChurn: lastTelemetry?.resourceChurn ?? null,
      telemetrySource: lastTelemetry?.telemetrySource ?? null,
      telemetryStatus: lastTelemetry?.telemetryStatus ?? null,
      telemetryReportCount: lastTelemetry?.telemetryReportCount ?? null,
      telemetryReportAgeMs: lastTelemetry?.telemetryReportAgeMs ?? null,
      telemetryObservedSpanMs: lastTelemetry?.telemetryObservedSpanMs ?? null,
      telemetryFrameProgress: lastTelemetry?.telemetryFrameProgress ?? null,
      telemetryFailure: lastTelemetry?.telemetryFailure ?? null,
      telemetryError: lastTelemetry?.telemetryError ?? null,
      perfEndpointError: lastTelemetry?.perfEndpointError ?? null,
      lastTelemetryPacket: lastTelemetry?.lastTelemetryPacket ?? null,
      updatedAt: new Date().toISOString(),
      timeline,
    };
  }
  function writeHeartbeat(status) {
    writeJsonAtomic(heartbeatPath, heartbeat(status));
  }
  activeToken.markInterrupted = (signal) => {
    stage = 'interrupted';
    timeline.push({
      at: new Date().toISOString(),
      elapsedMs: Date.now() - startedAt,
      stage,
      detail: signal,
    });
    writeHeartbeat('interrupted');
  };
  child.stdout.on('data', (chunk) => consume('stdout', chunk));
  child.stderr.on('data', (chunk) => consume('stderr', chunk));
  writeHeartbeat('running');
  const interval = setInterval(() => {
    writeHeartbeat('running');
    console.error(`voplay render stress heartbeat: scene=${scene} stage=${stage} elapsedMs=${Date.now() - startedAt} lastOutputAgoMs=${Date.now() - lastOutputAt}`);
  }, heartbeatIntervalMs);
  return new Promise((resolve) => {
    const finish = (status, signal, error = null) => {
      if (settled) return;
      settled = true;
      clearInterval(interval);
      clearTimeout(timeout);
      const interrupted = renderStressTermination != null;
      if (!interrupted) {
        recordStage(timedOut ? 'timeout' : (status === 0 ? 'complete' : 'failed'), error?.message ?? signal ?? '');
      }
      const terminalStatus = interrupted
        ? 'interrupted'
        : (timedOut ? 'timeout' : (status === 0 ? 'pass' : 'fail'));
      writeHeartbeat(terminalStatus);
      const observability = heartbeat(terminalStatus);
      if (activeObservedProcess === activeToken) {
        activeObservedProcess = null;
      }
      resolve({
        status: Number.isInteger(status) ? status : 1,
        signal,
        stdout,
        stderr: `${stderr}${error ? `\n${error.stack ?? error.message}` : ''}`,
        timedOut,
        observability,
        timeoutDiagnostic: timedOut ? {
          code: 'voplay.renderStress.timeout',
          scene,
          stage,
          elapsedMs: Date.now() - startedAt,
          timeoutMs,
          lastOutputAgoMs: Date.now() - lastOutputAt,
          frameIndex: lastTelemetry?.frameIndex ?? null,
          lastPass: lastTelemetry?.pass ?? null,
          frameP90Ms: lastTelemetry?.frameP90Ms ?? null,
          frameP99Ms: lastTelemetry?.frameP99Ms ?? null,
          resourceChurn: lastTelemetry?.resourceChurn ?? null,
          telemetrySource: lastTelemetry?.telemetrySource ?? null,
          telemetryStatus: lastTelemetry?.telemetryStatus ?? null,
          telemetryReportCount: lastTelemetry?.telemetryReportCount ?? null,
          telemetryReportAgeMs: lastTelemetry?.telemetryReportAgeMs ?? null,
          telemetryObservedSpanMs: lastTelemetry?.telemetryObservedSpanMs ?? null,
          telemetryFrameProgress: lastTelemetry?.telemetryFrameProgress ?? null,
          telemetryFailure: lastTelemetry?.telemetryFailure ?? null,
          telemetryError: lastTelemetry?.telemetryError ?? null,
          perfEndpointError: lastTelemetry?.perfEndpointError ?? null,
          lastTelemetryPacket: lastTelemetry?.lastTelemetryPacket ?? null,
          timeline,
        } : null,
      });
    };
    const timeout = setTimeout(() => {
      timedOut = true;
      recordStage('timeout-signal', `SIGTERM after ${timeoutMs}ms`);
      child.kill('SIGTERM');
      setTimeout(() => {
        if (!settled) child.kill('SIGKILL');
      }, 3000).unref();
    }, timeoutMs);
    child.once('error', (error) => finish(1, null, error));
    child.once('close', (status, signal) => finish(status, signal));
  });
}

function baselineProgressStage(detail, telemetry) {
  if (detail.startsWith('telemetry ')) {
    return `baseline.${telemetry?.stage ?? 'telemetry'}`;
  }
  const normalized = detail
    .replace(/\b(?:port|debugPort|ms|count|profile|completed|ok|skipped|entry|reason|renderers|quiesce|stopped)=\S+/g, '')
    .trim()
    .replace(/[^A-Za-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '')
    .toLowerCase();
  return `baseline.${normalized || 'progress'}`;
}

function summarizeBaselineScene(name, baseline, meta, options = {}) {
  const strictBudget = options.strictBudget !== false;
  const requirePerfSummary = options.requirePerfSummary !== false;
  const sceneBudget = sceneRenderBudget(name);
  const perfSummary = latestPerfSummary(baseline);
  const renderer = perfSummary?.renderer ?? latestRendererFromReports(baseline.perfReports);
  const workload = perfSummary?.workload ?? {};
  const latestSceneReport = baseline.diagnostics?.latestSceneReport ?? null;
  const stressSceneReport = baseline.renderStress?.latestSceneReport ?? null;
  const primitiveUploadBytes = Math.max(
    numberOr(workload.uploadBytes, 0),
    numberOr(latestSceneReport?.primitiveUploadBytes, 0),
    numberOr(stressSceneReport?.primitiveUploadBytes, 0),
  );
  const bufferCreates = numberOr(workload.bufferCreates, 0);
  const bindGroupCreates = numberOr(workload.bindGroupCreates, 0);
  const textureUploads = numberOr(workload.textureUploads, 0);
  const residentChunkRebuilds = numberOr(workload.residentChunkRebuilds, 0);
  const resourceChurnEvents = bufferCreates + bindGroupCreates + textureUploads + residentChunkRebuilds + (primitiveUploadBytes > 0 ? 1 : 0);
  const pulse = latestPerfReport(baseline.perfReports, 'pulse')?.metrics?.wait ?? {};
  const webgpu = latestPerfReport(baseline.perfReports, 'webgpu')?.metrics ?? {};
  const frameP90Ms = numberOr(perfSummary?.window?.frameP90Ms, pulse.p90);
  const frameP99Ms = numberOr(perfSummary?.window?.frameP99Ms, pulse.p99);
  const windowSlowFrames = numberOr(perfSummary?.window?.slowFrames, null);
  const activeWorkP90Ms = numberOr(perfSummary?.window?.activeWorkP90Ms, perfSummary?.gate?.activeWorkP90Ms);
  const activeWorkP99Ms = numberOr(perfSummary?.window?.activeWorkP99Ms, perfSummary?.gate?.activeWorkP99Ms);
  const renderWorkP90Ms = numberOr(perfSummary?.window?.renderWorkP90Ms, perfSummary?.current?.renderWorkMs);
  const renderSubmitP90Ms = numberOr(perfSummary?.window?.renderSubmitP90Ms, webgpu.submitCpu?.p90);
  const gpuWorkDoneP90Ms = numberOr(perfSummary?.window?.gpuWorkDoneP90Ms, webgpu.workDoneRaw?.p90);
  const passTimings = rendererPassTimings(renderer);
  const slowestPass = Object.entries(passTimings)
    .sort((a, b) => Number(b[1] ?? 0) - Number(a[1] ?? 0))[0] ?? ['', 0];
  const issues = [];
  for (const issue of baseline.issues ?? []) {
    if (shouldIgnoreBaselineIssue(issue, options)) {
      continue;
    }
    if (issue.severity === 'P0' || issue.severity === 'P1') {
      issues.push({ code: `baseline.${issue.severity}`, severity: issue.severity === 'P0' ? 0 : 1, detail: `${issue.owner}: ${issue.title}: ${issue.detail ?? ''}` });
    }
  }
  if (baseline.visual?.enabled !== false && !baseline.visual?.canvas?.nonEmpty) {
    issues.push({ code: 'canvas.blank', severity: 0, detail: JSON.stringify(baseline.visual?.canvas ?? {}) });
  }
  if (baseline.lifecycle?.state !== 'Running' || baseline.lifecycle?.reachedRunning !== true) {
    issues.push({ code: 'lifecycle.not_running', severity: 0, detail: JSON.stringify(baseline.lifecycle ?? {}) });
  }
  const warnings = baseline.warnings?.length ?? 0;
  const errors = baseline.errors?.length ?? 0;
  const resourceFailures = baseline.resourceFailures?.length ?? 0;
  if (warnings > 0) issues.push({ code: 'console.warning', severity: 1, detail: `${warnings} warnings` });
  if (errors > 0) issues.push({ code: 'console.error', severity: 0, detail: `${errors} errors` });
  if (resourceFailures > 0) issues.push({ code: 'resource.failure', severity: 0, detail: `${resourceFailures} resource failures` });
  if (!Number.isFinite(frameP90Ms) || !Number.isFinite(frameP99Ms)) {
    issues.push({ code: 'render.missing_real_samples', severity: 1, detail: 'baseline did not expose frame p90/p99 samples' });
  }
  if (requirePerfSummary && !perfSummary) {
    issues.push({ code: 'render.missing_perf_summary', severity: 1, detail: 'BlockKart did not emit a perf-summary report with renderer pass diagnostics' });
  }
  const budgetP90Ms = numberOr(sceneBudget.p90Ms, renderBudget.p90Ms, targetFrameMs);
  const budgetP99Ms = numberOr(sceneBudget.p99Ms, renderBudget.p99Ms, targetFrameMs);
  const budgetSlowFrames = numberOr(sceneBudget.maxSlowFrames, renderBudget.maxSlowFrames, null);
  const budgetResourceChurn = numberOr(sceneBudget.maxResourceChurnEvents, renderBudget.maxResourceChurnEvents, null);
  const budgetUploadBytes = numberOr(sceneBudget.maxUploadBytes, renderBudget.maxUploadBytes, null);
  const enforceRenderFrameBudget = strictBudget && (perfSummary || requirePerfSummary);
  const p90OverBudget = enforceRenderFrameBudget && Number.isFinite(frameP90Ms) && Number.isFinite(budgetP90Ms) && frameP90Ms > budgetP90Ms;
  const p99OverBudget = enforceRenderFrameBudget && Number.isFinite(frameP99Ms) && Number.isFinite(budgetP99Ms) && frameP99Ms > budgetP99Ms;
  const slowFramesOverBudget = Number.isFinite(windowSlowFrames) && Number.isFinite(budgetSlowFrames) && windowSlowFrames > budgetSlowFrames;
  const resourceChurnOverBudget = Number.isFinite(resourceChurnEvents) && Number.isFinite(budgetResourceChurn) && resourceChurnEvents > budgetResourceChurn;
  const uploadOverBudget = Number.isFinite(primitiveUploadBytes) && Number.isFinite(budgetUploadBytes) && primitiveUploadBytes > budgetUploadBytes;
  const hostPacingOnly = isHostPacingOnly(perfSummary, {
    frameP90Ms,
    frameP99Ms,
    activeWorkP90Ms,
    activeWorkP99Ms,
    renderWorkP90Ms,
    renderSubmitP90Ms,
    gpuWorkDoneP90Ms,
    passTimings,
    budgetP90Ms,
    budgetP99Ms,
  });
  if (strictBudget && perfSummary && perfSummary.gatePassed === false) {
    issues.push({ code: 'render.perf_gate_failed', severity: 1, detail: `status=${perfSummary.status ?? 'unknown'} failure=${perfSummary.failure ?? 'unknown'} hostPacingOnly=${hostPacingOnly}` });
  }
  if (requirePerfSummary && ((workload.frameGraphPasses ?? 0) <= 0 || (workload.frameGraphResources ?? 0) <= 0)) {
    issues.push({ code: 'render.missing_frame_graph_diagnostics', severity: 1, detail: JSON.stringify(workload) });
  }
  if (p90OverBudget) {
    issues.push({ code: 'render.p90_over_budget', severity: 1, detail: `${frameP90Ms.toFixed(2)}ms > ${budgetP90Ms.toFixed(2)}ms` });
  }
  if (p99OverBudget) {
    issues.push({ code: 'render.p99_over_budget', severity: 1, detail: `${frameP99Ms.toFixed(2)}ms > ${budgetP99Ms.toFixed(2)}ms hostPacingOnly=${hostPacingOnly}` });
  }
  if (strictBudget && slowFramesOverBudget) {
    issues.push({ code: 'render.slow_frames_over_budget', severity: 1, detail: `${windowSlowFrames} > ${budgetSlowFrames} hostPacingOnly=${hostPacingOnly}` });
  }
  if ((workload.frameGraphTargets ?? 0) > 0 && workload.frameGraphReadyTargets !== workload.frameGraphTargets) {
    issues.push({ code: 'render.target_not_ready', severity: 0, detail: `${workload.frameGraphReadyTargets}/${workload.frameGraphTargets} targets ready` });
  }
  for (const [field, code] of [
    ['frameGraphFailures', 'render.frame_graph_failure'],
    ['missingResources', 'render.resource_miss'],
    ['invalidBatches', 'render.invalid_batch'],
    ['fallbackPaths', 'render.fallback_path'],
  ]) {
    const count = numberOr(workload[field], 0);
    if (count > 0) {
      issues.push({ code, severity: 0, detail: `${field}=${count}` });
    }
  }
  if (strictBudget && resourceChurnOverBudget) {
    issues.push({ code: 'render.resource_churn_over_budget', severity: 1, detail: `${resourceChurnEvents} > ${budgetResourceChurn}` });
  }
  if (strictBudget && uploadOverBudget) {
    issues.push({ code: 'render.upload_over_budget', severity: 1, detail: `${primitiveUploadBytes} > ${budgetUploadBytes}` });
  }
  const waterSurfaceCount = Math.max(
    numberOr(latestSceneReport?.waterSurfaces, 0),
    numberOr(latestSceneReport?.visibleWaterSurfaces, 0),
    numberOr(stressSceneReport?.waterSurfaces, 0),
    numberOr(stressSceneReport?.visibleWaterSurfaces, 0),
  );
  if (/water/i.test(name) && waterSurfaceCount > 0 && numberOr(workload.waterDraws, 0) <= 0) {
    issues.push({ code: 'render.water_pass_not_submitted', severity: 0, detail: `waterSurfaces=${waterSurfaceCount} waterDraws=${numberOr(workload.waterDraws, 0)}` });
  }
  return {
    name,
    status: issues.some((issue) => issue.severity <= 1) ? 'fail' : 'pass',
    command: meta.command,
    budgetApplied: strictBudget,
    budget: sceneBudget,
    source: {
      project: baseline.project ?? null,
      dependencies: baseline.dependencies ?? [],
    },
    artifacts: {
      directory: meta.sceneOut,
      log: meta.logPath,
      heartbeat: meta.heartbeatPath,
      json: meta.jsonPath,
      markdown: meta.markdownPath,
      viewportScreenshot: baseline.artifacts?.viewportScreenshot ?? null,
      canvasScreenshot: baseline.artifacts?.canvasScreenshot ?? null,
    },
    viewport: baseline.viewport,
    canvas: {
      width: baseline.finalState?.canvasWidth ?? 0,
      height: baseline.finalState?.canvasHeight ?? 0,
      nonEmpty: baseline.visual?.canvas?.nonEmpty === true,
      uniqueSampledColors: baseline.visual?.canvas?.uniqueSampledColors ?? 0,
      lumaStdDev: baseline.visual?.canvas?.lumaStdDev ?? 0,
    },
    lifecycle: baseline.lifecycle,
    timings: {
      frameId: perfSummary?.frame ?? null,
      frameP90Ms,
      frameP99Ms,
      windowSlowFrames,
      activeWorkP90Ms,
      activeWorkP99Ms,
      renderWorkP90Ms,
      renderSubmitP90Ms,
      gpuWorkDoneP90Ms,
      slowFrames: baseline.voplaySlowFrames?.length ?? 0,
      passTimings,
      slowestPass: { name: slowestPass[0], ms: Number(slowestPass[1] ?? 0) },
      classification: perfSummary?.classification ?? null,
    },
    frameGraph: {
      passes: workload.frameGraphPasses ?? 0,
      resources: workload.frameGraphResources ?? 0,
      targets: workload.frameGraphTargets ?? 0,
      readyTargets: workload.frameGraphReadyTargets ?? 0,
    },
    workload: {
      drawCalls: workload.drawCalls ?? 0,
      modelDraws: workload.modelDraws ?? 0,
      primitiveDraws: workload.primitiveDraws ?? 0,
      primitiveChunks: workload.primitiveChunks ?? 0,
      instances: workload.instances ?? 0,
      triangles: workload.triangles ?? 0,
      postEffects: workload.postEffects ?? 0,
      frameGraphSkippedPasses: workload.frameGraphSkippedPasses ?? 0,
      frameGraphFailures: workload.frameGraphFailures ?? 0,
      filteredDraws: workload.filteredDraws ?? 0,
      missingResources: workload.missingResources ?? 0,
      invalidBatches: workload.invalidBatches ?? 0,
      fallbackPaths: workload.fallbackPaths ?? 0,
      missingModels: workload.missingModels ?? 0,
      missingMeshes: workload.missingMeshes ?? 0,
      missingTextures: workload.missingTextures ?? 0,
      missingBindGroups: workload.missingBindGroups ?? 0,
      missingChunks: workload.missingChunks ?? 0,
      missingTargets: workload.missingTargets ?? 0,
      invalidBatchIndices: workload.invalidBatchIndices ?? 0,
      incompatibleDraws: workload.incompatibleDraws ?? 0,
      bufferCreates,
      bindGroupCreates,
      textureUploads,
      residentChunkRebuilds,
      resourceChurnEvents,
      primitiveUploadBytes,
      waterDraws: workload.waterDraws ?? 0,
      diagnosticFlags: workload.diagnosticFlags ?? 0,
    },
    attribution: {
      slowFrames: baseline.performanceAttribution?.slowFrames ?? null,
      render: baseline.performanceAttribution?.render ?? null,
      sceneComplexity: baseline.performanceAttribution?.sceneComplexity ?? null,
      optimizationTarget: baseline.performanceAttribution?.optimizationTarget ?? null,
    },
    diagnostics: {
      perfGate: perfSummary ? {
        passed: perfSummary.gatePassed === true,
        status: perfSummary.status ?? '',
        failure: perfSummary.failure ?? '',
        hostPacingOnly,
      } : null,
      latestSceneReport,
      latestVehicleReport: baseline.diagnostics?.latestVehicleReport ?? null,
      renderStress: baseline.renderStress ?? null,
      resize: baseline.resize ?? null,
      perfReportCount: baseline.perfReports?.length ?? 0,
      warnings,
      errors,
      resourceFailures,
      restart: baseline.restart,
      captureTelemetry: baseline.captureTelemetry ?? null,
    },
    observability: meta.observability,
    issues,
  };
}

function latestPerfSummary(baseline) {
  const reports = (baseline.perfReports ?? []).filter((report) => report?.kind === 'perf-summary');
  const summary = reports.at(-1) ?? null;
  const skips = (baseline.perfReports ?? []).filter((report) => report?.kind === 'perf-skip-summary').at(-1) ?? null;
  return summary ? { ...summary, workload: { ...(summary.workload ?? {}), ...(skips?.workload ?? {}) } } : null;
}

function shouldIgnoreBaselineIssue(issue, options) {
  if (!options.ignoreBaselineSlowFrameIssue) {
    return false;
  }
  return issue?.severity === 'P1'
    && String(issue?.owner ?? '').toLowerCase() === 'voplay'
    && /slow frames/i.test(String(issue?.title ?? ''));
}

function latestPerfReport(reports, kind) {
  return (reports ?? []).filter((report) => report?.kind === kind).at(-1) ?? null;
}

function latestRendererFromReports(reports) {
  const renderer = latestPerfSummary({ perfReports: reports })?.renderer;
  return renderer ?? {};
}

function rendererPassTimings(renderer) {
  return {
    depth: numberOr(renderer?.depthPassMs, 0),
    shadow: numberOr(renderer?.shadowPassMs, 0),
    main: numberOr(renderer?.mainPassMs, 0),
    post: numberOr(renderer?.postPassMs, 0),
    overlay: numberOr(renderer?.overlayPassMs, 0),
    backendSubmit: numberOr(renderer?.queueSubmitCpuMs, 0) + numberOr(renderer?.presentCpuMs, 0),
  };
}

function isHostPacingOnly(perfSummary, metrics) {
  if (!perfSummary) {
    return false;
  }
  const status = String(perfSummary.status ?? '');
  if (status !== 'pacing-limited' && status !== 'cadence-jitter') {
    return false;
  }
  const failure = String(perfSummary.failure ?? '');
  if (failure !== 'slow_frames' && failure !== 'frame_max' && failure !== 'frame_p99' && !(failure === '' && status === 'cadence-jitter')) {
    return false;
  }
  const budgetP90Ms = Number(metrics.budgetP90Ms);
  if (!Number.isFinite(budgetP90Ms) || budgetP90Ms <= 0) {
    return false;
  }
  const frameP90Ms = Number(metrics.frameP90Ms);
  const activeWorkP90Ms = Number(metrics.activeWorkP90Ms);
  const activeWorkP99Ms = Number(metrics.activeWorkP99Ms);
  const renderWorkP90Ms = Number(metrics.renderWorkP90Ms);
  const renderSubmitP90Ms = Number(metrics.renderSubmitP90Ms);
  const gpuWorkDoneP90Ms = Number(metrics.gpuWorkDoneP90Ms);
  const slowestPassMs = Math.max(...Object.values(metrics.passTimings ?? {}).map((value) => Number(value)).filter(Number.isFinite), 0);
  const renderLaneBudget = Math.max(3, budgetP90Ms * 0.35);
  return Number.isFinite(frameP90Ms)
    && frameP90Ms <= budgetP90Ms
    && (!Number.isFinite(activeWorkP90Ms) || activeWorkP90Ms <= budgetP90Ms)
    && (!Number.isFinite(activeWorkP99Ms) || activeWorkP99Ms <= budgetP90Ms)
    && (!Number.isFinite(renderWorkP90Ms) || renderWorkP90Ms <= renderLaneBudget)
    && (!Number.isFinite(renderSubmitP90Ms) || renderSubmitP90Ms <= renderLaneBudget)
    && (!Number.isFinite(gpuWorkDoneP90Ms) || gpuWorkDoneP90Ms <= renderLaneBudget)
    && slowestPassMs <= renderLaneBudget
    && hasPaceDominatedSpike(perfSummary.classification, budgetP90Ms, renderLaneBudget);
}

function hasPaceDominatedSpike(classification, budgetP90Ms, renderLaneBudget) {
  return paceDominatedFrame(classification?.latestSpike, budgetP90Ms, renderLaneBudget)
    || paceDominatedFrame(classification?.windowWorst, budgetP90Ms, renderLaneBudget);
}

function paceDominatedFrame(frame, budgetP90Ms, renderLaneBudget) {
  const frameMs = Number(frame?.frameMs);
  const paceWaitMs = Number(frame?.paceWaitMs);
  if (!Number.isFinite(frameMs) || !Number.isFinite(paceWaitMs) || frameMs <= budgetP90Ms) {
    return false;
  }
  const renderWorkMs = Number(frame?.renderWorkMs);
  const submitCpuMs = Number(frame?.submitCpuMs ?? frame?.rendererQueueSubmitCpuMs);
  const gpuWorkDoneMs = Number(frame?.gpuWorkDoneMs);
  return paceWaitMs >= Math.max(frameMs * 0.7, budgetP90Ms * 0.5)
    && (!Number.isFinite(renderWorkMs) || renderWorkMs <= renderLaneBudget)
    && (!Number.isFinite(submitCpuMs) || submitCpuMs <= renderLaneBudget)
    && (!Number.isFinite(gpuWorkDoneMs) || gpuWorkDoneMs <= renderLaneBudget);
}

function numberOr(...values) {
  for (const value of values) {
    const number = Number(value);
    if (Number.isFinite(number)) return number;
  }
  return null;
}

function sceneRenderBudget(name) {
  return {
    ...(renderBudget ?? {}),
    ...((renderBudget.scenes ?? {})[name] ?? {}),
  };
}

function maxSceneDiagnostic(scene, key) {
  return Math.max(
    numberOr(scene.diagnostics?.latestSceneReport?.[key], 0),
    numberOr(scene.diagnostics?.renderStress?.latestSceneReport?.[key], 0),
  );
}

function tail(text) {
  const value = String(text ?? '');
  return value.length > 4000 ? value.slice(value.length - 4000) : value;
}

function markdownReport(report) {
  const lines = ['# voplay render stress', ''];
  lines.push(`- Status: ${report.status}`);
  lines.push(`- Target: ${report.target.width}x${report.target.height} @ ${report.target.fps} FPS`);
  lines.push(`- Budget: ${path.relative(root, report.budget.path)} p90=${formatMs(report.budget.render.p90Ms)} p99=${formatMs(report.budget.render.p99Ms)} slowFrames=${report.budget.render.maxSlowFrames}`);
  lines.push(`- Budget all scenes: ${report.budgetAllScenes === true}`);
  lines.push(`- Scene count: ${report.summary.sceneCount}`);
  lines.push(`- P0/P1: ${report.summary.p0}/${report.summary.p1}`);
  lines.push(`- Frame p90/p99: ${formatMs(report.summary.frameP90Ms)} / ${formatMs(report.summary.frameP99Ms)}`);
  lines.push(`- Coverage: ${Object.entries(report.coverage ?? {}).map(([name, ok]) => `${name}=${ok === true}`).join(' ')}`);
  lines.push('');
  for (const scene of report.scenes) {
    const artifacts = scene.artifacts ?? {};
    const canvas = scene.canvas ?? {};
    const lifecycle = scene.lifecycle ?? {};
    const timings = scene.timings ?? {};
    const frameGraph = scene.frameGraph ?? {};
    const workload = scene.workload ?? {};
    const diagnostics = scene.diagnostics ?? {};
    const slowestPass = timings.slowestPass ?? {};
    lines.push(`## ${scene.name}`);
    lines.push(`- Status: ${scene.status} budgetApplied=${scene.budgetApplied === true}`);
    lines.push(`- Baseline JSON: ${path.relative(root, artifacts.json ?? artifacts.directory ?? outDir)}`);
    lines.push(`- Baseline MD: ${path.relative(root, artifacts.markdown ?? artifacts.directory ?? outDir)}`);
    lines.push(`- Canvas screenshot: ${artifacts.canvasScreenshot ? path.relative(root, artifacts.canvasScreenshot) : 'missing'}`);
    lines.push(`- Canvas: ${canvas.width ?? 0}x${canvas.height ?? 0} nonEmpty=${canvas.nonEmpty === true} colors=${canvas.uniqueSampledColors ?? 0}`);
    lines.push(`- Lifecycle: ${lifecycle.state ?? 'missing'} reachedRunning=${lifecycle.reachedRunning === true}`);
    lines.push(`- Timings: frame=${timings.frameId ?? 'n/a'} p90/p99=${formatMs(timings.frameP90Ms)}/${formatMs(timings.frameP99Ms)} activeP90/P99=${formatMs(timings.activeWorkP90Ms)}/${formatMs(timings.activeWorkP99Ms)} renderWorkP90=${formatMs(timings.renderWorkP90Ms)} windowSlow=${timings.windowSlowFrames ?? 'n/a'} submitP90=${formatMs(timings.renderSubmitP90Ms)} gpuP90=${formatMs(timings.gpuWorkDoneP90Ms)}`);
    if (timings.classification) {
      const classification = timings.classification;
      const worst = classification.windowWorst ?? {};
      lines.push(`- Classification: current=${classification.primaryCause ?? 'n/a'} worst=${classification.windowWorstCause ?? worst.cause ?? 'n/a'} worstFrame=${classification.windowWorstFrame ?? worst.frame ?? 'n/a'} worstMs=${formatMs(classification.windowWorstMs ?? worst.frameMs)} evidence=${worst.evidence ?? 'n/a'}`);
    }
    lines.push(`- Passes: slowest=${slowestPass.name || 'unknown'} ${formatMs(slowestPass.ms)} graph=${frameGraph.passes ?? 0} passes ${frameGraph.resources ?? 0} resources targets=${frameGraph.readyTargets ?? 0}/${frameGraph.targets ?? 0}`);
    lines.push(`- Workload: draws=${workload.drawCalls ?? 0} models=${workload.modelDraws ?? 0} primitives=${workload.primitiveDraws ?? 0} chunks=${workload.primitiveChunks ?? 0} instances=${workload.instances ?? 0} churn=${workload.resourceChurnEvents ?? 0} upload=${workload.primitiveUploadBytes ?? 0}`);
    if (scene.attribution?.slowFrames) {
      const slow = scene.attribution.slowFrames;
      lines.push(`- Slow attribution: count=${slow.count ?? 0} steady=${slow.steadyState ?? 0} restart=${slow.restartRebuild ?? 0} reasons=${JSON.stringify(slow.byReason ?? {})}`);
    }
    if (scene.attribution?.optimizationTarget) {
      const target = scene.attribution.optimizationTarget;
      lines.push(`- Optimization target: ${target.owner ?? 'n/a'} ${target.subsystem ?? 'n/a'} ${target.name ?? 'n/a'} ${formatMs(target.durationMs)}`);
    }
    lines.push(`- Diagnostics: warnings=${diagnostics.warnings ?? 0} errors=${diagnostics.errors ?? 0} resourceFailures=${diagnostics.resourceFailures ?? 0} perfReports=${diagnostics.perfReportCount ?? 0} gate=${diagnostics.perfGate?.passed === true ? 'pass' : diagnostics.perfGate?.failure ?? 'missing'}`);
    for (const issue of scene.issues ?? []) {
      lines.push(`- Issue ${issue.code}: ${issue.detail}`);
    }
    lines.push('');
  }
  for (const issue of report.coverageIssues ?? []) {
    lines.push(`- Coverage issue ${issue.detail}`);
  }
  for (const issue of report.summaryIssues ?? []) {
    lines.push(`- Summary issue ${issue.code}: ${issue.detail}`);
  }
  return `${lines.join('\n')}\n`;
}

function formatMs(value) {
  const number = Number(value);
  return Number.isFinite(number) ? `${number.toFixed(2)}ms` : 'n/a';
}

function portableRepoRelativePath(file) {
  return path.relative(root, file).split(path.sep).join('/');
}

async function waitForJsonFile(file, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let lastError = null;
  while (Date.now() < deadline) {
    try {
      return readJson(file);
    } catch (error) {
      lastError = error;
    }
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
  throw new Error(`timed out waiting for ${file}: ${lastError instanceof Error ? lastError.message : String(lastError)}`);
}

function waitForChildClose(child, timeoutMs) {
  return new Promise((resolve, reject) => {
    let settled = false;
    const timeout = setTimeout(() => {
      if (settled) return;
      settled = true;
      reject(new Error(`child ${child.pid ?? 'unknown'} did not exit within ${timeoutMs}ms`));
    }, timeoutMs);
    child.once('close', (status, signal) => {
      if (settled) return;
      settled = true;
      clearTimeout(timeout);
      resolve({ status, signal });
    });
  });
}

function processIsAlive(pid) {
  if (!Number.isSafeInteger(pid) || pid < 1) return false;
  try {
    process.kill(pid, 0);
    return true;
  } catch {
    return false;
  }
}

function atomicTemporaryFiles(directory, prefix = '') {
  if (!existsSync(directory)) return [];
  return readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const relative = prefix ? path.join(prefix, entry.name) : entry.name;
    const absolute = path.join(directory, entry.name);
    if (entry.isDirectory()) return atomicTemporaryFiles(absolute, relative);
    return entry.name.includes('.tmp-') ? [relative] : [];
  });
}

async function runParentSignalSelftest(directory, signal) {
  const expectedExitCode = signal === 'SIGINT' ? 130 : 143;
  rmSync(directory, { recursive: true, force: true });
  mkdirSync(directory, { recursive: true });
  const heartbeatPath = path.join(directory, 'parent-signal-fixture', 'heartbeat.json');
  let stdout = '';
  let stderr = '';
  const parent = spawn(
    process.execPath,
    [
      path.join(root, 'scripts/ci/voplay_render_stress.mjs'),
      '--selftest-parent-signal-fixture',
      '--out-dir',
      directory,
    ],
    {
      cwd: root,
      env: { ...process.env },
      stdio: ['ignore', 'pipe', 'pipe'],
    },
  );
  parent.stdout.on('data', (chunk) => { stdout = appendLog(stdout, chunk); });
  parent.stderr.on('data', (chunk) => { stderr = appendLog(stderr, chunk); });
  let grandchildPid = null;
  let close = null;
  let error = null;
  try {
    const runningHeartbeat = await waitForJsonFile(heartbeatPath, 10000);
    grandchildPid = runningHeartbeat?.pid ?? null;
    parent.kill(signal);
    close = await waitForChildClose(parent, 15000);
  } catch (caught) {
    error = caught instanceof Error ? caught.message : String(caught);
    parent.kill('SIGKILL');
  }
  const orphanDeadline = Date.now() + 2000;
  while (processIsAlive(grandchildPid) && Date.now() < orphanDeadline) {
    await new Promise((resolve) => setTimeout(resolve, 25));
  }
  const grandchildAlive = processIsAlive(grandchildPid);
  if (grandchildAlive) {
    try { process.kill(grandchildPid, 'SIGKILL'); } catch { /* best-effort selftest cleanup */ }
  }
  let report = null;
  let heartbeat = null;
  try { report = readJson(path.join(directory, 'report.json')); } catch { /* reported below */ }
  try { heartbeat = readJson(heartbeatPath); } catch { /* reported below */ }
  const temporaryFiles = atomicTemporaryFiles(directory);
  const passed = error == null
    && close?.status === expectedExitCode
    && close?.signal == null
    && !grandchildAlive
    && report?.kind === 'voplay.renderStressReport.failure'
    && report?.status === 'failed'
    && report?.stage === 'received process signal'
    && report?.signal === signal
    && report?.error === `terminated by ${signal}`
    && heartbeat?.status === 'interrupted'
    && heartbeat?.stage === 'interrupted'
    && temporaryFiles.length === 0;
  return {
    passed,
    signal,
    expectedExitCode,
    error,
    close,
    parentPid: parent.pid ?? null,
    grandchildPid,
    grandchildAlive,
    report,
    heartbeat,
    temporaryFiles,
    stdout,
    stderr,
  };
}

if (process.argv.includes('--selftest-parent-signal-fixture')) {
  rmSync(outDir, { recursive: true, force: true });
  mkdirSync(outDir, { recursive: true });
  const fixtureHeartbeat = path.join(outDir, 'parent-signal-fixture', 'heartbeat.json');
  const fixture = await runObservedProcess(
    'parent-signal-fixture',
    ['-e', 'setInterval(() => {}, 1000)'],
    fixtureHeartbeat,
    60000,
  );
  process.exit(fixture.status);
}

if (process.argv.includes('--selftest-observability')) {
  const selftestDir = path.join(outDir, 'observability-selftest');
  const heartbeatPath = path.join(selftestDir, 'heartbeat.json');
  rmSync(selftestDir, { recursive: true, force: true });
  mkdirSync(selftestDir, { recursive: true });
  const parentSignalSelftests = [];
  for (const signal of ['SIGINT', 'SIGTERM']) {
    parentSignalSelftests.push(await runParentSignalSelftest(
      path.join(selftestDir, `parent-${signal.toLowerCase()}`),
      signal,
    ));
  }
  const result = await runObservedProcess(
    'timeout-negative-fixture',
    ['-e', `console.error('BlockKart baseline progress: telemetry ${JSON.stringify({ stage: 'capture', frameIndex: 42, pass: 'main', frameP90Ms: 16.7, frameP99Ms: 16.7, resourceChurn: 3, telemetrySource: 'negative-fixture', telemetryStatus: 'failed', telemetryReportCount: 12, telemetryReportAgeMs: 50001, telemetryObservedSpanMs: 90000, telemetryFrameProgress: 2400, telemetryFailure: { code: 'voplay.renderTelemetry.stale' }, telemetryError: 'fixture transport closed', perfEndpointError: null, lastTelemetryPacket: { status: 'pass', frameGraphFailures: 0 } })}'); setInterval(() => {}, 1000)`],
    heartbeatPath,
    100,
  );
  const timeoutStageRecorded = result.observability?.timeline?.some((entry) => entry.stage === 'timeout-signal') === true;
  const telemetryRecorded = result.timeoutDiagnostic?.frameIndex === 42
    && result.timeoutDiagnostic?.lastPass === 'main'
    && result.timeoutDiagnostic?.telemetrySource === 'negative-fixture'
    && result.timeoutDiagnostic?.telemetryStatus === 'failed'
    && result.timeoutDiagnostic?.telemetryFailure?.code === 'voplay.renderTelemetry.stale'
    && result.timeoutDiagnostic?.telemetryError === 'fixture transport closed'
    && result.timeoutDiagnostic?.lastTelemetryPacket?.frameGraphFailures === 0;
  const longRunTelemetrySelftest = spawnSync(process.execPath, [path.join(root, 'scripts/ci/blockkart_baseline.mjs'), '--selftest-long-run-telemetry'], {
    cwd: root,
    env: { ...process.env },
    encoding: 'utf8',
  });
  const signalOutDir = path.join(selftestDir, 'baseline-signal');
  const signalSelftest = spawnSync(
    process.execPath,
    [path.join(root, 'scripts/ci/blockkart_baseline.mjs'), '--selftest-signal-failure-evidence'],
    {
      cwd: root,
      env: { ...process.env, BLOCKKART_BASELINE_OUT_DIR: signalOutDir },
      encoding: 'utf8',
      timeout: 10000,
    },
  );
  let signalFailureEvidence = null;
  try {
    signalFailureEvidence = readJson(path.join(signalOutDir, 'blockkart-baseline.json'));
  } catch {
    signalFailureEvidence = null;
  }
  const signalTemporaryFiles = existsSync(signalOutDir)
    ? readdirSync(signalOutDir).filter((entry) => entry.includes('.tmp-'))
    : ['missing output directory'];
  const signalSelftestPassed = signalSelftest.error == null
    && signalSelftest.signal == null
    && signalSelftest.status === 143
    && signalFailureEvidence?.schemaVersion === 1
    && signalFailureEvidence?.kind === 'blockkart.baseline.failure'
    && signalFailureEvidence?.status === 'failed'
    && signalFailureEvidence?.stage === 'received SIGTERM'
    && signalFailureEvidence?.error === 'terminated by SIGTERM'
    && signalTemporaryFiles.length === 0;
  const timeoutPolicyFixtures = [
    { name: 'underbudget', value: '1', message: 'below the required' },
    { name: 'overflow', value: '2147000001', message: 'exceeds the Node timer limit' },
    { name: 'noncanonical', value: '0900000', message: 'canonical positive integer' },
  ].map((fixture) => {
    const fixtureOutDir = path.join(selftestDir, `baseline-timeout-${fixture.name}`);
    const child = spawnSync(
      process.execPath,
      [path.join(root, 'scripts/ci/blockkart_baseline.mjs'), '--selftest-long-run-telemetry'],
      {
        cwd: root,
        env: {
          ...process.env,
          BLOCKKART_BASELINE_OUT_DIR: fixtureOutDir,
          BLOCKKART_BASELINE_TIMEOUT_MS: fixture.value,
        },
        encoding: 'utf8',
        timeout: 10000,
      },
    );
    let evidence = null;
    try {
      evidence = readJson(path.join(fixtureOutDir, 'blockkart-baseline.json'));
    } catch {
      evidence = null;
    }
    const temporaryFiles = existsSync(fixtureOutDir)
      ? readdirSync(fixtureOutDir).filter((entry) => entry.includes('.tmp-'))
      : ['missing output directory'];
    return {
      ...fixture,
      status: child.status,
      signal: child.signal,
      processError: child.error?.message ?? null,
      evidence,
      temporaryFiles,
      passed: child.error == null
        && child.signal == null
        && child.status === 1
        && evidence?.kind === 'blockkart.baseline.failure'
        && evidence?.status === 'failed'
        && evidence?.stage === 'timeout policy validation'
        && String(evidence?.error ?? '').includes(fixture.message)
        && temporaryFiles.length === 0,
    };
  });
  const timeoutPolicyEvidencePassed = timeoutPolicyFixtures.every((fixture) => fixture.passed);
  const heartbeatPolicyFixtures = [
    { name: 'noncanonical', value: '015000', message: 'canonical positive integer' },
    { name: 'overflow', value: String(BLOCKKART_BASELINE_MAX_TIMER_MS + 1), message: 'exceeds the Node timer limit' },
  ].map((fixture) => {
    const fixtureOutDir = path.join(selftestDir, `render-heartbeat-${fixture.name}`);
    const child = spawnSync(
      process.execPath,
      [
        path.join(root, 'scripts/ci/voplay_render_stress.mjs'),
        '--selftest-parent-signal-fixture',
        '--out-dir',
        fixtureOutDir,
      ],
      {
        cwd: root,
        env: {
          ...process.env,
          VOPLAY_RENDER_STRESS_HEARTBEAT_MS: fixture.value,
        },
        encoding: 'utf8',
        timeout: 10000,
      },
    );
    let evidence = null;
    try {
      evidence = readJson(path.join(fixtureOutDir, 'report.json'));
    } catch {
      evidence = null;
    }
    const temporaryFiles = atomicTemporaryFiles(fixtureOutDir);
    return {
      ...fixture,
      status: child.status,
      signal: child.signal,
      processError: child.error?.message ?? null,
      evidence,
      temporaryFiles,
      passed: child.error == null
        && child.signal == null
        && child.status === 1
        && evidence?.schemaVersion === 1
        && evidence?.kind === 'voplay.renderStressReport.failure'
        && evidence?.status === 'failed'
        && evidence?.stage === 'configuration validation'
        && String(evidence?.error ?? '').includes(fixture.message)
        && temporaryFiles.length === 0,
    };
  });
  const heartbeatPolicyEvidencePassed = heartbeatPolicyFixtures.every((fixture) => fixture.passed);
  const simpleChildBudgetMs = blockKartBaselineTimeoutBudget();
  const simpleObserverBudgetMs = blockKartBaselineObserverTimeoutBudget();
  const soakChildBudgetMs = blockKartBaselineTimeoutBudget({ captureMs: 600000 });
  const soakObserverBudgetMs = blockKartBaselineObserverTimeoutBudget({ captureMs: 600000 });
  const restart50ChildBudgetMs = blockKartBaselineTimeoutBudget({ captureMs: 36000, restartCount: 50 });
  const restart50ObserverBudgetMs = blockKartBaselineObserverTimeoutBudget({ captureMs: 36000, restartCount: 50 });
  const restart50BudgetBreakdown = blockKartBaselineTimeoutBreakdown({ captureMs: 36000, restartCount: 50 });
  const renderStressBudgetBreakdown = blockKartBaselineTimeoutBreakdown({
    captureMs: 14000,
    renderStressScenario: true,
  });
  const storageReloadBudgetBreakdown = blockKartBaselineTimeoutBreakdown({ storageReloadScenario: true });
  const resizeBudgetBreakdown = blockKartBaselineTimeoutBreakdown({ resizeScenario: true });
  let overflowRejected = false;
  try {
    blockKartBaselineTimeoutBudget({ restartCount: Number.MAX_SAFE_INTEGER });
  } catch {
    overflowRejected = true;
  }
  let minimumTimeoutOverflowRejected = false;
  try {
    blockKartBaselineTimeoutBudget({ minimumTimeoutMs: 2148000000 });
  } catch {
    minimumTimeoutOverflowRejected = true;
  }
  let configuredUnderbudgetRejected = false;
  try {
    configuredBlockKartBaselineTimeout('1');
  } catch {
    configuredUnderbudgetRejected = true;
  }
  const ciBaselineEnvironment = formalBaselineEnvironment(
    simpleChildBudgetMs,
    BLOCKKART_BASELINE_BUDGET_DEFAULTS.captureMs,
    {
      CI: 'true',
      BLOCKKART_BASELINE_LONG_RUN_TELEMETRY_MAX_AGE_MS: '600000',
      BLOCKKART_BASELINE_LONG_RUN_TELEMETRY_SPAN_GRACE_MS: '400000',
      BLOCKKART_BASELINE_MAX_SLOW_FRAMES: '999999',
    },
  );
  const formalPolicyContract = {
    visualCapture: ciBaselineEnvironment.BLOCKKART_BASELINE_VISUAL_CAPTURE === '1',
    requireWebGpu: ciBaselineEnvironment.BLOCKKART_BASELINE_REQUIRE_WEBGPU === '1',
    failOnIssues: ciBaselineEnvironment.BLOCKKART_BASELINE_NO_FAIL === '0',
    lifecycle: ciBaselineEnvironment.BLOCKKART_BASELINE_EXPECT_LIFECYCLE === 'Running',
    noSimulation: ciBaselineEnvironment.BLOCKKART_BASELINE_SIMULATE_FAILURE === '',
    telemetryMaxAge: ciBaselineEnvironment.BLOCKKART_BASELINE_LONG_RUN_TELEMETRY_MAX_AGE_MS
      === String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.longRunTelemetryMaxAgeMs),
    telemetrySpanGrace: ciBaselineEnvironment.BLOCKKART_BASELINE_LONG_RUN_TELEMETRY_SPAN_GRACE_MS
      === String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.longRunTelemetrySpanGraceMs),
    slowFrameBudget: ciBaselineEnvironment.BLOCKKART_BASELINE_MAX_SLOW_FRAMES
      === String(BLOCKKART_BASELINE_BUDGET_DEFAULTS.maxSlowFrames),
    childTimeoutBound: Number(ciBaselineEnvironment.BLOCKKART_BASELINE_TIMEOUT_MS) === simpleChildBudgetMs,
    simpleDeadlineOrder: simpleObserverBudgetMs > simpleChildBudgetMs,
    soakDeadlineOrder: soakObserverBudgetMs > soakChildBudgetMs,
    restart50DeadlineOrder: restart50ObserverBudgetMs > restart50ChildBudgetMs,
    restart50ReadinessWaitsComplete: restart50BudgetBreakdown.readinessWaitsMs
      === BLOCKKART_BASELINE_BUDGET_DEFAULTS.firstFrameTimeoutMs * 4,
    restart50IterationBudgetComplete: restart50BudgetBreakdown.restartWaitsMs
      === BLOCKKART_BASELINE_BUDGET_DEFAULTS.restartWaitTimeoutMs * 50
      && restart50BudgetBreakdown.restartCooldownsMs
        === BLOCKKART_BASELINE_BUDGET_DEFAULTS.restartCooldownMs * 50,
    renderStressScenarioBudgetComplete: renderStressBudgetBreakdown.scenarioWaitsMs
      === BLOCKKART_BASELINE_BUDGET_DEFAULTS.restartWaitTimeoutMs,
    storageReloadScenarioBudgetComplete: storageReloadBudgetBreakdown.scenarioWaitsMs
      === BLOCKKART_BASELINE_BUDGET_DEFAULTS.restartWaitTimeoutMs * 2,
    resizeScenarioBudgetComplete: resizeBudgetBreakdown.resizeWaitsMs
      === BLOCKKART_BASELINE_BUDGET_DEFAULTS.resizeStepCount
        * BLOCKKART_BASELINE_BUDGET_DEFAULTS.resizeStepWaitMs,
    overflowRejected,
    minimumTimeoutOverflowRejected,
    configuredUnderbudgetRejected,
  };
  const formalPolicyContractPassed = Object.values(formalPolicyContract).every((value) => value === true);
  const voplayTransportSource = readFileSync(path.join(voplayRoot, 'js/render_bootstrap.ts'), 'utf8');
  const voplayTransportBody = voplayTransportSource.slice(
    voplayTransportSource.indexOf('function postVoplayPerfReports('),
    voplayTransportSource.indexOf('function readLocationParam('),
  );
  const studioTransportSource = readFileSync(path.join(root, 'apps/studio/src/lib/perf_report_bridge.ts'), 'utf8');
  const studioTransportBody = studioTransportSource.slice(
    studioTransportSource.indexOf('function flushVoplayPerfPayloads('),
    studioTransportSource.indexOf('function isLocalHost('),
  );
  const transportSourceContract = {
    voplayBatchPost: voplayTransportBody.includes('JSON.stringify(payloads)'),
    voplayResponseConsumed: voplayTransportBody.includes('response.arrayBuffer()'),
    voplayBoundedTransport: !voplayTransportBody.includes('sendBeacon') && !voplayTransportBody.includes('keepalive'),
    studioBatchPost: studioTransportBody.includes('pending.map(parseVoplayPerfPayload)'),
    studioResponseConsumed: studioTransportBody.includes('response.arrayBuffer()'),
    studioBoundedTransport: !studioTransportBody.includes('sendBeacon') && !studioTransportBody.includes('keepalive'),
  };
  const transportSourceContractPassed = Object.values(transportSourceContract).every((value) => value === true);
  if (!result.timedOut
    || result.timeoutDiagnostic?.code !== 'voplay.renderStress.timeout'
    || !timeoutStageRecorded
    || !telemetryRecorded
    || !existsSync(heartbeatPath)
    || longRunTelemetrySelftest.status !== 0
    || !signalSelftestPassed
    || !parentSignalSelftests.every((selftest) => selftest.passed)
    || !timeoutPolicyEvidencePassed
    || !heartbeatPolicyEvidencePassed
    || !transportSourceContractPassed
    || !formalPolicyContractPassed) {
    result.longRunTelemetrySelftest = {
      status: longRunTelemetrySelftest.status,
      stdout: longRunTelemetrySelftest.stdout,
      stderr: longRunTelemetrySelftest.stderr,
    };
    result.transportSourceContract = transportSourceContract;
    result.signalSelftest = {
      status: signalSelftest.status,
      signal: signalSelftest.signal,
      error: signalSelftest.error?.message ?? null,
      stdout: signalSelftest.stdout,
      stderr: signalSelftest.stderr,
      evidence: signalFailureEvidence,
      temporaryFiles: signalTemporaryFiles,
    };
    result.formalPolicyContract = formalPolicyContract;
    result.timeoutPolicyFixtures = timeoutPolicyFixtures;
    result.parentSignalSelftests = parentSignalSelftests;
    result.heartbeatPolicyFixtures = heartbeatPolicyFixtures;
    fail(`observability selftest failed: ${JSON.stringify(result)}`);
  }
  writeFileSync(path.join(outDir, 'report.json'), `${JSON.stringify({
    schemaVersion: 1,
    kind: 'voplay.renderStressObservabilitySelftest',
    status: 'pass',
    timeoutDiagnostic: result.timeoutDiagnostic,
    heartbeat: result.observability,
    transportSourceContract,
    signalFailureEvidence: {
      exitCode: signalSelftest.status,
      kind: signalFailureEvidence.kind,
      stage: signalFailureEvidence.stage,
      temporaryFiles: signalTemporaryFiles,
    },
    parentSignalEvidence: parentSignalSelftests.map((selftest) => ({
      signal: selftest.signal,
      exitCode: selftest.close?.status ?? null,
      grandchildPid: selftest.grandchildPid,
      grandchildAlive: selftest.grandchildAlive,
      reportKind: selftest.report?.kind ?? null,
      heartbeatStatus: selftest.heartbeat?.status ?? null,
      heartbeatStage: selftest.heartbeat?.stage ?? null,
      temporaryFiles: selftest.temporaryFiles,
    })),
    timeoutPolicyFailureEvidence: timeoutPolicyFixtures.map((fixture) => ({
      name: fixture.name,
      exitCode: fixture.status,
      stage: fixture.evidence?.stage ?? null,
      error: fixture.evidence?.error ?? null,
      temporaryFiles: fixture.temporaryFiles,
    })),
    heartbeatPolicyFailureEvidence: heartbeatPolicyFixtures.map((fixture) => ({
      name: fixture.name,
      exitCode: fixture.status,
      stage: fixture.evidence?.stage ?? null,
      error: fixture.evidence?.error ?? null,
      temporaryFiles: fixture.temporaryFiles,
    })),
    formalPolicyContract: {
      ...formalPolicyContract,
      budgetsMs: {
        simple: { child: simpleChildBudgetMs, observer: simpleObserverBudgetMs },
        soak10m: { child: soakChildBudgetMs, observer: soakObserverBudgetMs },
        restart50: { child: restart50ChildBudgetMs, observer: restart50ObserverBudgetMs },
      },
      restart50Breakdown: restart50BudgetBreakdown,
      scenarioBreakdowns: {
        renderStress: renderStressBudgetBreakdown,
        storageReload: storageReloadBudgetBreakdown,
        resize: resizeBudgetBreakdown,
      },
    },
  }, null, 2)}\n`);
  console.log('voplay render stress observability selftest: ok');
  process.exit(0);
}

rmSync(outDir, { recursive: true, force: true });
mkdirSync(outDir, { recursive: true });
const baselinePerfArgs = ['--perf-mode', 'stats', '--perf-console', '0', '--perf-gpu-probe', '0', '--perf-diag', 'pulseHybrid'];
const coverageArgs = ['--no-fail-on-issues', ...baselinePerfArgs];
const coverageBudgetOptions = {
  strictBudget: budgetAllScenes,
  ignoreBaselineSlowFrameIssue: !budgetAllScenes,
};
const scenes = [
  await runBaseline(soakOnly ? 'blockkart-quickplay-baseline-soak-10m' : 'blockkart-quickplay-baseline', baselinePerfArgs, { strictBudget: true, captureMs }),
];
if (!soakOnly) {
  scenes.push(await runBaseline('blockkart-primitive-10k', [...coverageArgs, '--stress-profile', 'primitive10k'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }));
  scenes.push(await runBaseline('blockkart-water', [...coverageArgs, '--stress-profile', 'water'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }));
  scenes.push(await runBaseline('blockkart-resource-churn-soak', [...coverageArgs, '--stress-profile', 'resource-churn'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }));
  scenes.push(await runBaseline('blockkart-chunked-world-drive', [...coverageArgs, '--stress-profile', 'chunked-world-drive'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }));
  scenes.push(await runBaseline('blockkart-shadow-post-matrix', [...coverageArgs, '--stress-profile', 'shadow-post-matrix'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }));
  scenes.push(await runBaseline('blockkart-resize-recreate-targets', [...coverageArgs, '--resize-cycle'], { ...coverageBudgetOptions, captureMs: coverageCaptureMs }));
}
if (!soakOnly && restartCount > 0) {
  scenes.push(await runBaseline(`blockkart-restart-${restartCount}`, ['--no-fail-on-issues', ...baselinePerfArgs, '--restart-count', String(restartCount)], { ...coverageBudgetOptions, requirePerfSummary: false, allowBaselineFailureWithJson: true }));
}

const p0 = scenes.flatMap((scene) => scene.issues.filter((issue) => issue.severity === 0)).length;
let p1 = scenes.flatMap((scene) => scene.issues.filter((issue) => issue.severity === 1)).length;
const frameP90Values = scenes.map((scene) => scene.timings?.frameP90Ms).filter(Number.isFinite);
const frameP99Values = scenes.map((scene) => scene.timings?.frameP99Ms).filter(Number.isFinite);
const summaryFrameP90Ms = frameP90Values.length ? Math.max(...frameP90Values) : null;
const summaryFrameP99Ms = frameP99Values.length ? Math.max(...frameP99Values) : null;
const summarySlowFrames = scenes.reduce((sum, scene) => sum + (scene.timings?.slowFrames ?? 0), 0);
const coverage = {
  blockKartBaseline: scenes.some((scene) => scene.name === 'blockkart-quickplay-baseline' && scene.status === 'pass'),
  primitive10k: scenes.some((scene) => (scene.workload?.instances ?? 0) >= 10000 || (scene.workload?.primitiveDraws ?? 0) >= 10000 || maxSceneDiagnostic(scene, 'primitiveInstances') >= 10000),
  chunkedWorld: scenes.some((scene) => (scene.workload?.primitiveChunks ?? 0) > 0),
  water: scenes.some((scene) => maxSceneDiagnostic(scene, 'waterSurfaces') > 0 || maxSceneDiagnostic(scene, 'visibleWaterSurfaces') > 0 || (scene.workload?.waterDraws ?? 0) > 0),
  shadowPostMatrix: scenes.some((scene) => Number(scene.timings?.passTimings?.shadow ?? 0) > 0 && Number(scene.workload?.postEffects ?? 0) > 0),
  restart50: restartCount >= 50 && scenes.some((scene) => scene.diagnostics?.restart?.requested === restartCount && scene.diagnostics?.restart?.completed === restartCount),
  resizeRecreateTargets: scenes.some((scene) => scene.diagnostics?.resize?.completed === true || scene.diagnostics?.latestSceneReport?.resizeRecreateTargets === true),
  resourceChurnSoak: scenes.some((scene) => scene.name === 'blockkart-resource-churn-soak' && scene.diagnostics?.renderStress?.completed === true && Number(scene.workload?.resourceChurnEvents ?? 0) > 0),
  realCanvasCapture: scenes.every((scene) => scene.canvas?.nonEmpty === true),
  realPerfSamples: scenes.every((scene) => Number.isFinite(scene.timings?.frameP90Ms) && Number.isFinite(scene.timings?.frameP99Ms)),
};
const coverageIssues = soakOnly ? [] : Object.entries(coverage)
  .filter(([, covered]) => covered !== true)
  .map(([name]) => ({ code: 'coverage.missing', severity: 1, detail: name }));
p1 += coverageIssues.length;
const summaryIssues = [];
const summaryBudgetP90Ms = numberOr(renderBudget.p90Ms, targetFrameMs);
const summaryBudgetP99Ms = numberOr(renderBudget.p99Ms, targetFrameMs);
const summaryBudgetSlowFrames = numberOr(renderBudget.maxSlowFrames, null);
if (Number.isFinite(summaryFrameP90Ms) && Number.isFinite(summaryBudgetP90Ms) && summaryFrameP90Ms > summaryBudgetP90Ms) {
  summaryIssues.push({ code: 'summary.p90_over_budget', severity: 1, detail: `${summaryFrameP90Ms.toFixed(2)}ms > ${summaryBudgetP90Ms.toFixed(2)}ms` });
}
if (Number.isFinite(summaryFrameP99Ms) && Number.isFinite(summaryBudgetP99Ms) && summaryFrameP99Ms > summaryBudgetP99Ms) {
  summaryIssues.push({ code: 'summary.p99_over_budget', severity: 1, detail: `${summaryFrameP99Ms.toFixed(2)}ms > ${summaryBudgetP99Ms.toFixed(2)}ms` });
}
if (Number.isFinite(summarySlowFrames) && Number.isFinite(summaryBudgetSlowFrames) && summarySlowFrames > summaryBudgetSlowFrames) {
  summaryIssues.push({ code: 'summary.slow_frames_over_budget', severity: 1, detail: `${summarySlowFrames} > ${summaryBudgetSlowFrames}` });
}
p1 += summaryIssues.filter((issue) => issue.severity === 1).length;
const generatedAt = new Date().toISOString();
const sceneArtifacts = scenes.map((scene) => scene.artifacts?.directory);
if (sceneArtifacts.some((directory) => typeof directory !== 'string')) {
  fail('every render stress scene must expose one stable artifact directory');
}
const freshEvidence = sourceBoundEvidence({
  gate: soakOnly ? 'voplay-render-soak-10m' : (budgetAllScenes ? 'voplay-render-stress-budgeted' : 'voplay-render-stress'),
  generatedAt,
  root,
  repos: [
    { name: 'volang', root },
    { name: 'voplay', root: voplayRoot },
    { name: 'BlockKart', root: blockKartRoot },
  ],
  gateFiles: [
    'scripts/ci/voplay_render_stress.mjs',
    'scripts/ci/blockkart_baseline.mjs',
    'scripts/ci/blockkart_baseline_budget.mjs',
    'scripts/ci/studio_browser_smoke_contract.mjs',
    'scripts/ci/repo_roots.mjs',
    'scripts/ci/source_bound_evidence.mjs',
    budgetPath,
    'eng/tasks.toml',
    'eng/ci.toml',
  ],
  artifacts: [
    'apps/studio/public/quickplay/blockkart/project.json',
    'apps/studio/public/quickplay/blockkart/deps.json',
    'apps/studio/public/quickplay/blockkart/provenance.json',
    ...sceneArtifacts,
  ],
});
const report = {
  schemaVersion: 1,
  kind: 'voplay.renderStressReport',
  mode: soakOnly ? 'soak-10m' : 'stress',
  generatedAt,
  freshEvidence,
  target: { width: perfBudget?.target?.width ?? 1280, height: perfBudget?.target?.height ?? 720, fps: targetFps, frameBudgetMs: Number(targetFrameMs.toFixed(4)) },
  budget: { path: budgetPath, render: renderBudget },
  budgetAllScenes,
  policy: {
    budgetAllScenes,
    soakOnly,
    restartCount,
    captureMs,
    coverageCaptureMs,
    pulseMode,
    heartbeatIntervalMs,
    budgetPath: portableRepoRelativePath(budgetPath),
  },
  scenes,
  summary: {
    sceneCount: scenes.length,
    passCount: scenes.filter((scene) => scene.status === 'pass').length,
    p0,
    p1,
    frameP90Ms: summaryFrameP90Ms,
    frameP99Ms: summaryFrameP99Ms,
    slowFrames: summarySlowFrames,
  },
  coverage,
  coverageIssues,
  summaryIssues,
  status: p0 === 0 && p1 === 0 ? 'pass' : 'fail',
};
writeJsonAtomic(path.join(outDir, 'report.json'), report);
writeFileSync(path.join(outDir, 'report.md'), markdownReport(report));

if (report.status !== 'pass') {
  fail(`stress thresholds failed: p0=${p0} p1=${p1}; see ${path.join(outDir, 'report.json')}`);
}
console.log(`voplay render stress: ok ${path.join(outDir, 'report.json')}`);
