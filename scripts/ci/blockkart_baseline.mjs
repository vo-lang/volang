#!/usr/bin/env node
import { spawn, spawnSync } from 'node:child_process';
import {
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import net from 'node:net';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { deflateSync, inflateSync } from 'node:zlib';
import { requireRepoRoot } from './repo_roots.mjs';
import { sourceBoundEvidence } from './source_bound_evidence.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const voplayRoot = requireRepoRoot('VOPLAY_ROOT', 'voplay');
const blockKartRoot = requireRepoRoot('BLOCKKART_ROOT', 'BlockKart');
const studioDir = path.join(root, 'apps/studio');
const studioDistIndex = path.join(studioDir, 'dist/index.html');
const quickplayDir = path.join(root, 'apps/studio/public/quickplay/blockkart');
const voplayPerfReportRoute = '/__voplay_perf_report';

const outDir = path.resolve(argValue('--out-dir') || process.env.BLOCKKART_BASELINE_OUT_DIR || path.join(root, 'target/blockkart-baseline'));
const viewportWidth = positiveInt(argValue('--viewport-width') || process.env.BLOCKKART_BASELINE_VIEWPORT_WIDTH, 1280);
const viewportHeight = positiveInt(argValue('--viewport-height') || process.env.BLOCKKART_BASELINE_VIEWPORT_HEIGHT, 720);
const firstFrameTimeoutMs = positiveInt(process.env.BLOCKKART_BASELINE_FIRST_FRAME_TIMEOUT_MS, 90000);
const captureMs = positiveInt(argValue('--capture-ms') || process.env.BLOCKKART_BASELINE_CAPTURE_MS, 6000);
const cdpScreenshotAttempts = positiveInt(process.env.BLOCKKART_BASELINE_CDP_SCREENSHOT_ATTEMPTS, 3);
const cdpScreenshotTimeoutMs = positiveInt(process.env.BLOCKKART_BASELINE_CDP_SCREENSHOT_TIMEOUT_MS, 15000);
const canvasDataUrlAttempts = positiveInt(process.env.BLOCKKART_BASELINE_CANVAS_DATA_URL_ATTEMPTS, 3);
const canvasDataUrlTimeoutMs = positiveInt(process.env.BLOCKKART_BASELINE_CANVAS_DATA_URL_TIMEOUT_MS, 60000);
const startupWarnMs = positiveInt(process.env.BLOCKKART_BASELINE_STARTUP_WARN_MS, 20000);
const maxSlowFrames = positiveInt(process.env.BLOCKKART_BASELINE_MAX_SLOW_FRAMES, 2);
const restartCount = nonNegativeInt(argValue('--restart-count') || process.env.BLOCKKART_BASELINE_RESTART_COUNT, 0);
const restartWaitTimeoutMs = positiveInt(process.env.BLOCKKART_BASELINE_RESTART_TIMEOUT_MS, 30000);
const baselineTimeoutMs = positiveInt(
  process.env.BLOCKKART_BASELINE_TIMEOUT_MS,
  Math.max(300000, firstFrameTimeoutMs + captureMs + restartCount * restartWaitTimeoutMs + 180000),
);
const resetKey = { key: 'r', code: 'KeyR', keyCode: 82 };
const startRaceKey = { key: 'w', code: 'KeyW', keyCode: 87 };
const renderStressKeys = {
  primitive10k: { key: 'F7', code: 'F7', keyCode: 118 },
  water: { key: 'F8', code: 'F8', keyCode: 119 },
  'resource-churn': { key: 'F9', code: 'F9', keyCode: 120 },
  'chunked-world-drive': { key: 'F10', code: 'F10', keyCode: 121 },
  'shadow-post-matrix': { key: 'F11', code: 'F11', keyCode: 122 },
};
const requireWebGpuAdapter = process.env.BLOCKKART_BASELINE_REQUIRE_WEBGPU === '1';
const failOnIssues = !process.argv.includes('--no-fail-on-issues') && process.env.BLOCKKART_BASELINE_NO_FAIL !== '1';
const noWebGpuAdapterPattern = /no suitable GPU adapter|requestAdapter returned null|navigator\.gpu is unavailable/i;
const simulatedFailure = argValue('--simulate-failure') || process.env.BLOCKKART_BASELINE_SIMULATE_FAILURE || '';
const expectedLifecycleState = argValue('--expect-lifecycle-state') || process.env.BLOCKKART_BASELINE_EXPECT_LIFECYCLE || (simulatedFailure ? 'Failed' : 'Running');
const startRaceRequested = process.argv.includes('--start-race') || process.env.BLOCKKART_BASELINE_START_RACE === '1';
const verifyStorageReload = process.argv.includes('--verify-storage-reload') || process.env.BLOCKKART_BASELINE_VERIFY_STORAGE_RELOAD === '1';
const renderStressProfile = argValue('--stress-profile') || process.env.BLOCKKART_BASELINE_STRESS_PROFILE || '';
const resizeCycleRequested = boolOption('--resize-cycle', process.env.BLOCKKART_BASELINE_RESIZE_CYCLE, false);
const perfMode = argValue('--perf-mode') || process.env.BLOCKKART_BASELINE_PERF_MODE || 'trace';
const perfConsole = boolOption('--perf-console', process.env.BLOCKKART_BASELINE_PERF_CONSOLE, true);
const pulseMode = normalizePulseMode(argValue('--pulse-mode') || process.env.BLOCKKART_BASELINE_PULSE_MODE || '');
const perfDiag = perfDiagForPulseMode(argValue('--perf-diag') || process.env.BLOCKKART_BASELINE_PERF_DIAG || 'gpu,pulseHybrid', pulseMode);
const perfGpuProbe = boolOption('--perf-gpu-probe', process.env.BLOCKKART_BASELINE_PERF_GPU_PROBE, true);
const blockKartLifecycleMarker = '__BLOCKKART_LIFECYCLE__';
const blockKartFailureReportMarker = '__BLOCKKART_FAILURE_REPORT__';
const blockKartAssetReportMarker = '__BLOCKKART_ASSET_REPORT__';
const blockKartSceneReportMarker = '__BLOCKKART_SCENE_REPORT__';
const blockKartVehicleReportMarker = '__BLOCKKART_VEHICLE_REPORT__';
const blockKartRaceReportMarker = '__BLOCKKART_RACE_REPORT__';

const cleanupCallbacks = [];
let cleanedUp = false;

function fail(message) {
  console.error(`BlockKart baseline: ${message}`);
  cleanup();
  process.exit(1);
}

function argValue(name) {
  const index = process.argv.indexOf(name);
  if (index === -1) {
    return '';
  }
  return process.argv[index + 1] ?? '';
}

function positiveInt(value, fallback) {
  const parsed = Number.parseInt(String(value ?? ''), 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
}

function nonNegativeInt(value, fallback) {
  const parsed = Number.parseInt(String(value ?? ''), 10);
  return Number.isFinite(parsed) && parsed >= 0 ? parsed : fallback;
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

function baselineGateName() {
  if (restartCount === 50) {
    return 'blockkart-baseline-restart-50';
  }
  if (restartCount === 10) {
    return 'blockkart-baseline-restart-10';
  }
  if (startRaceRequested) {
    return 'blockkart-baseline-start-race';
  }
  if (verifyStorageReload) {
    return 'blockkart-baseline-storage-reload';
  }
  return 'blockkart-baseline';
}

function parseBool(value, fallback) {
  const raw = String(value ?? '').trim().toLowerCase();
  if (raw === '1' || raw === 'true' || raw === 'yes' || raw === 'on') {
    return true;
  }
  if (raw === '0' || raw === 'false' || raw === 'no' || raw === 'off') {
    return false;
  }
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

function perfDiagForPulseMode(value, mode) {
  if (!mode) {
    return value;
  }
  const pulseToken = `pulse${mode[0].toUpperCase()}${mode.slice(1)}`;
  const tokens = String(value ?? '')
    .split(/[,\s;&|]+/)
    .map((token) => token.trim())
    .filter(Boolean);
  let replaced = false;
  const next = tokens.map((token) => {
    if (/^pulse(?:raf|timer|hybrid)$/i.test(token)) {
      replaced = true;
      return pulseToken;
    }
    return token;
  });
  if (!replaced) {
    next.push(pulseToken);
  }
  return next.join(',');
}

function readJson(file) {
  return JSON.parse(readFileSync(file, 'utf8'));
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function withTimeout(promise, timeoutMs, label) {
  let timer = null;
  const timeout = new Promise((_, reject) => {
    timer = setTimeout(() => {
      reject(new Error(`${label} timed out after ${timeoutMs}ms`));
    }, timeoutMs);
  });
  return Promise.race([promise, timeout]).finally(() => {
    if (timer) {
      clearTimeout(timer);
    }
  });
}

function cleanup() {
  if (cleanedUp) {
    return;
  }
  cleanedUp = true;
  for (const callback of cleanupCallbacks.reverse()) {
    try {
      callback();
    } catch {
      // Best-effort cleanup only.
    }
  }
}

for (const signal of ['SIGINT', 'SIGTERM']) {
  process.on(signal, () => {
    cleanup();
    process.exit(130);
  });
}
process.on('exit', cleanup);

function installSimulatedFailure(kind) {
  if (!kind) {
    return null;
  }
  if (kind !== 'asset-pack' && kind !== 'missing-asset-pack') {
    fail(`unsupported simulated failure "${kind}"; expected asset-pack`);
  }
  const distProjectPath = path.join(studioDir, 'dist/quickplay/blockkart/project.json');
  if (!existsSync(distProjectPath)) {
    fail('apps/studio/dist quickplay project package is missing; run the studio-build task first');
  }
  const original = readFileSync(distProjectPath, 'utf8');
  cleanupCallbacks.push(() => writeFileSync(distProjectPath, original));
  const pack = JSON.parse(original);
  const asset = pack.files?.find((file) => file.path === 'assets/blockkart.vpak');
  if (!asset) {
    fail('BlockKart dist quickplay package is missing assets/blockkart.vpak');
  }
  asset.path = 'assets/blockkart_missing.vpak';
  writeFileSync(distProjectPath, `${JSON.stringify(pack, null, 2)}\n`);
  return {
    kind,
    target: 'assets/blockkart.vpak',
    mutation: 'renamed packaged VFS entry to assets/blockkart_missing.vpak',
  };
}

function syncDistQuickplayPackage() {
  if (!existsSync(path.join(quickplayDir, 'project.json')) || !existsSync(path.join(quickplayDir, 'deps.json'))) {
    fail('apps/studio/public quickplay package is missing; run the quickplay-validate task first');
  }
  const distQuickplayDir = path.join(studioDir, 'dist/quickplay/blockkart');
  const backupRoot = mkdtempSync(path.join(os.tmpdir(), 'blockkart-dist-quickplay-'));
  const backupDir = path.join(backupRoot, 'blockkart');
  const hadDistQuickplay = existsSync(distQuickplayDir);
  if (hadDistQuickplay) {
    cpSync(distQuickplayDir, backupDir, { recursive: true });
  }
  cleanupCallbacks.push(() => {
    rmSync(distQuickplayDir, { recursive: true, force: true });
    if (hadDistQuickplay) {
      cpSync(backupDir, distQuickplayDir, { recursive: true });
    }
    rmSync(backupRoot, { recursive: true, force: true });
  });
  rmSync(distQuickplayDir, { recursive: true, force: true });
  mkdirSync(path.dirname(distQuickplayDir), { recursive: true });
  cpSync(quickplayDir, distQuickplayDir, { recursive: true });
}

function trimLog(log) {
  return log.length > 12000 ? log.slice(log.length - 12000) : log;
}

function appendLog(current, chunk) {
  return trimLog(current + chunk.toString());
}

function reservePort() {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.once('error', reject);
    server.listen(0, '127.0.0.1', () => {
      const address = server.address();
      const port = typeof address === 'object' && address ? address.port : 0;
      server.close(() => resolve(port));
    });
  });
}

async function fetchOk(url, options = {}) {
  let lastError = 'not attempted';
  for (let attempt = 1; attempt <= 40; attempt++) {
    try {
      const response = await fetch(url, { cache: 'no-store', ...options });
      if (response.ok) {
        return response;
      }
      lastError = `HTTP ${response.status}`;
    } catch (error) {
      lastError = error instanceof Error ? error.message : String(error);
    }
    await sleep(Math.min(1000, attempt * 100));
  }
  throw new Error(`${url} did not become available: ${lastError}`);
}

async function fetchVoplayPerfEndpoint(baseUrl) {
  const url = new URL(voplayPerfReportRoute, baseUrl);
  const response = await fetch(url, { cache: 'no-store' });
  if (!response.ok) {
    throw new Error(`${url} returned HTTP ${response.status}`);
  }
  const body = await response.json();
  return {
    count: Number.isFinite(body?.count) ? body.count : 0,
    reports: Array.isArray(body?.reports) ? body.reports : [],
  };
}

function commandWorks(command) {
  const result = spawnSync(command, ['--version'], { stdio: 'ignore' });
  return !result.error && result.status === 0;
}

function findBrowserBinary() {
  const explicit = [
    process.env.BLOCKKART_BASELINE_BROWSER_BIN,
    process.env.STUDIO_BROWSER_BIN,
    process.env.CHROME_BIN,
    process.env.PUPPETEER_EXECUTABLE_PATH,
    process.env.PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH,
  ].filter(Boolean);
  for (const candidate of explicit) {
    if (existsSync(candidate) || commandWorks(candidate)) {
      return candidate;
    }
  }

  const pathCandidates = process.platform === 'darwin'
    ? [
        '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
        '/Applications/Chromium.app/Contents/MacOS/Chromium',
        '/Applications/Microsoft Edge.app/Contents/MacOS/Microsoft Edge',
      ]
    : [
        '/usr/bin/google-chrome',
        '/usr/bin/google-chrome-stable',
        '/usr/bin/chromium',
        '/usr/bin/chromium-browser',
        '/usr/bin/microsoft-edge',
      ];
  for (const candidate of pathCandidates) {
    if (existsSync(candidate)) {
      return candidate;
    }
  }

  for (const command of ['google-chrome', 'google-chrome-stable', 'chromium', 'chromium-browser', 'microsoft-edge']) {
    if (commandWorks(command)) {
      return command;
    }
  }
  return null;
}

function stopProcess(child) {
  if (!child || child.exitCode != null || child.signalCode != null) {
    return;
  }
  signalProcess(child, 'SIGTERM');
  child.stdout?.destroy();
  child.stderr?.destroy();
  child.stdin?.destroy?.();
  child.unref?.();
}

function signalProcess(child, signal) {
  if (!child || child.exitCode != null || child.signalCode != null) {
    return;
  }
  if (process.platform !== 'win32' && child.pid) {
    try {
      process.kill(-child.pid, signal);
      return;
    } catch {
      // Fall through to direct kill when the child is not a process group leader.
    }
  }
  try {
    child.kill(signal);
  } catch {
    // The process may have exited between the status check and the signal.
  }
}

async function stopProcessAndWait(child, timeoutMs = 3000) {
  if (!child) {
    return;
  }
  if (child.exitCode != null || child.signalCode != null) {
    child.stdout?.destroy();
    child.stderr?.destroy();
    child.stdin?.destroy?.();
    return;
  }
  let exited = false;
  const exitPromise = new Promise((resolve) => {
    child.once('exit', () => {
      exited = true;
      resolve();
    });
  });
  signalProcess(child, 'SIGTERM');
  child.stdout?.destroy();
  child.stderr?.destroy();
  child.stdin?.destroy?.();
  await Promise.race([exitPromise, sleep(timeoutMs)]);
  if (!exited && child.exitCode == null && child.signalCode == null) {
    signalProcess(child, 'SIGKILL');
    await Promise.race([exitPromise, sleep(500)]);
  }
  child.unref?.();
}

function startPreviewServer(port) {
  let log = '';
  const child = spawn(
    'npm',
    ['run', 'preview', '--', '--host', '127.0.0.1', '--port', String(port), '--strictPort'],
    {
      cwd: studioDir,
      detached: process.platform !== 'win32',
      env: { ...process.env, BROWSER: 'none' },
      stdio: ['ignore', 'pipe', 'pipe'],
    },
  );
  child.stdout.on('data', (chunk) => {
    log = appendLog(log, chunk);
  });
  child.stderr.on('data', (chunk) => {
    log = appendLog(log, chunk);
  });
  cleanupCallbacks.push(() => stopProcess(child));
  child.once('exit', (code, signal) => {
    if (!cleanedUp) {
      fail(`vite preview exited early with ${signal ?? code}\n${log}`);
    }
  });
  return { child, log: () => log };
}

async function startBrowser(debugPort) {
  const browserBin = findBrowserBinary();
  if (!browserBin) {
    fail('could not find Chrome/Chromium. Set BLOCKKART_BASELINE_BROWSER_BIN, STUDIO_BROWSER_BIN, or CHROME_BIN.');
  }
  const profileDir = mkdtempSync(path.join(os.tmpdir(), 'volang-blockkart-baseline-'));
  cleanupCallbacks.push(() => rmSync(profileDir, { recursive: true, force: true }));
  let log = '';
  const child = spawn(
    browserBin,
    [
      '--headless=new',
      '--disable-dev-shm-usage',
      '--disable-background-timer-throttling',
      '--disable-backgrounding-occluded-windows',
      '--disable-renderer-backgrounding',
      '--disable-features=CalculateNativeWinOcclusion',
      '--enable-unsafe-webgpu',
      '--ignore-gpu-blocklist',
      '--no-sandbox',
      `--remote-debugging-port=${debugPort}`,
      `--user-data-dir=${profileDir}`,
      'about:blank',
    ],
    {
      detached: process.platform !== 'win32',
      stdio: ['ignore', 'pipe', 'pipe'],
    },
  );
  child.stdout.on('data', (chunk) => {
    log = appendLog(log, chunk);
  });
  child.stderr.on('data', (chunk) => {
    log = appendLog(log, chunk);
  });
  cleanupCallbacks.push(() => stopProcess(child));
  child.once('exit', (code, signal) => {
    if (!cleanedUp) {
      fail(`browser exited early with ${signal ?? code}\n${log}`);
    }
  });
  await fetchOk(`http://127.0.0.1:${debugPort}/json/version`);
  return { child, browserBin, log: () => log };
}

function openWebSocket(url) {
  if (typeof WebSocket !== 'function') {
    fail('Node global WebSocket is unavailable; use Node 24 or newer.');
  }
  return new Promise((resolve, reject) => {
    const ws = new WebSocket(url);
    const timer = setTimeout(() => reject(new Error(`timed out connecting to ${url}`)), 10000);
    ws.addEventListener('open', () => {
      clearTimeout(timer);
      resolve(ws);
    }, { once: true });
    ws.addEventListener('error', () => {
      clearTimeout(timer);
      reject(new Error(`could not connect to ${url}`));
    }, { once: true });
  });
}

class CdpClient {
  constructor(ws) {
    this.ws = ws;
    this.nextId = 1;
    this.pending = new Map();
    this.handlers = new Map();
    ws.addEventListener('message', (event) => this.onMessage(event));
    ws.addEventListener('close', () => {
      for (const { reject, timer } of this.pending.values()) {
        clearTimeout(timer);
        reject(new Error('CDP websocket closed'));
      }
      this.pending.clear();
    });
  }

  on(method, handler) {
    const handlers = this.handlers.get(method) ?? [];
    handlers.push(handler);
    this.handlers.set(method, handlers);
    return () => {
      const current = this.handlers.get(method) ?? [];
      this.handlers.set(method, current.filter((entry) => entry !== handler));
    };
  }

  waitFor(method, predicate = () => true, timeoutMs = 30000) {
    return new Promise((resolve, reject) => {
      const off = this.on(method, (params) => {
        if (!predicate(params)) {
          return;
        }
        clearTimeout(timer);
        off();
        resolve(params);
      });
      const timer = setTimeout(() => {
        off();
        reject(new Error(`timed out waiting for ${method}`));
      }, timeoutMs);
    });
  }

  send(method, params = {}, timeoutMs = 30000) {
    const id = this.nextId++;
    const message = JSON.stringify({ id, method, params });
    return new Promise((resolve, reject) => {
      const timer = setTimeout(() => {
        this.pending.delete(id);
        reject(new Error(`CDP ${method} timed out`));
      }, timeoutMs);
      this.pending.set(id, { resolve, reject, timer, method });
      this.ws.send(message);
    });
  }

  async evaluate(expression, timeoutMs = 30000) {
    const response = await this.send('Runtime.evaluate', {
      expression,
      awaitPromise: true,
      returnByValue: true,
      userGesture: true,
      timeout: timeoutMs,
    }, timeoutMs + 1000);
    if (response.exceptionDetails) {
      throw new Error(formatExceptionDetails(response.exceptionDetails));
    }
    return response.result?.value;
  }

  close() {
    this.ws.close();
  }

  onMessage(event) {
    let message;
    try {
      message = JSON.parse(String(event.data));
    } catch {
      return;
    }
    if (message.id != null) {
      const pending = this.pending.get(message.id);
      if (!pending) {
        return;
      }
      clearTimeout(pending.timer);
      this.pending.delete(message.id);
      if (message.error) {
        pending.reject(new Error(`CDP ${pending.method} failed: ${message.error.message}`));
      } else {
        pending.resolve(message.result ?? {});
      }
      return;
    }
    const handlers = this.handlers.get(message.method) ?? [];
    for (const handler of handlers) {
      handler(message.params ?? {});
    }
  }
}

function formatExceptionDetails(details) {
  const text = details.text ?? 'exception';
  const value = details.exception?.description ?? details.exception?.value ?? '';
  return `${text}${value ? `: ${value}` : ''}`;
}

async function openPage(debugPort) {
  const target = await (await fetchOk(
    `http://127.0.0.1:${debugPort}/json/new?${encodeURIComponent('about:blank')}`,
    { method: 'PUT' },
  )).json();
  if (!target.webSocketDebuggerUrl) {
    fail('Chrome did not return a page websocket URL');
  }
  const ws = await openWebSocket(target.webSocketDebuggerUrl);
  return { targetId: target.id, client: new CdpClient(ws) };
}

async function closePage(debugPort, targetId) {
  if (!targetId) {
    return;
  }
  await fetch(`http://127.0.0.1:${debugPort}/json/close/${targetId}`).catch(() => undefined);
}

async function navigate(client, url) {
  const loaded = client.waitFor('Page.loadEventFired', () => true, 45000).catch(() => null);
  await client.send('Page.navigate', { url });
  await loaded;
  await waitForPredicate(
    client,
    'document ready',
    'document.readyState === "complete"',
    (value) => value === true,
    45000,
  );
}

async function waitForPredicate(client, label, expression, predicate, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let lastValue = null;
  let lastError = null;
  while (Date.now() < deadline) {
    try {
      lastValue = await client.evaluate(expression, 10000);
      lastError = null;
      if (predicate(lastValue)) {
        return lastValue;
      }
    } catch (error) {
      lastError = error instanceof Error ? error.message : String(error);
    }
    await sleep(300);
  }
  const detail = lastError ?? JSON.stringify(lastValue);
  throw new Error(`${label} timed out after ${timeoutMs}ms; last state: ${detail}`);
}

function quickplayStateExpression() {
  return `(() => {
    const app = document.querySelector('#app');
    const text = document.body.innerText || '';
    const canvas = document.querySelector('.runner-surface canvas, .renderer-surface canvas, canvas');
    const canvasRect = canvas ? canvas.getBoundingClientRect() : null;
    const runner = document.querySelector('.runner-surface');
    const error = document.querySelector('.runner-error, .splash-card.error')?.innerText || '';
    const loading = document.querySelector('.runner-loading')?.innerText || document.querySelector('.splash-loading')?.innerText || '';
    return {
      href: location.href,
      appChildren: app ? app.children.length : -1,
      runner: Boolean(runner),
      hasCanvas: Boolean(canvas),
      canvasId: canvas?.id || null,
      canvasWidth: canvasRect ? Math.round(canvasRect.width) : 0,
      canvasHeight: canvasRect ? Math.round(canvasRect.height) : 0,
      canvasRect: canvasRect ? {
        x: Math.max(0, canvasRect.left),
        y: Math.max(0, canvasRect.top),
        width: Math.max(0, canvasRect.width),
        height: Math.max(0, canvasRect.height),
      } : null,
      loading,
      error,
      textSample: text.slice(0, 1000),
    };
  })()`;
}

function webGpuStateExpression() {
  return `(async () => {
    const gpu = globalThis.navigator?.gpu;
    if (!gpu) {
      return { supported: false, adapter: false, reason: 'navigator.gpu is unavailable' };
    }
    try {
      const adapter = await gpu.requestAdapter();
      return {
        supported: true,
        adapter: Boolean(adapter),
        reason: adapter ? 'adapter available' : 'requestAdapter returned null',
      };
    } catch (error) {
      return {
        supported: true,
        adapter: false,
        reason: error instanceof Error ? error.message : String(error),
      };
    }
  })()`;
}

function debugSnapshotExpression() {
  return `(() => {
    const hook = globalThis.__voStudioBrowserSmoke;
    const perfReports = globalThis.__voplayPerfReports ?? [];
    return {
      entryPath: hook?.entryPath?.() ?? null,
      consoleLines: hook?.consoleLines?.() ?? [],
      runtimeState: hook?.runtimeState?.() ?? null,
      perfReports,
    };
  })()`;
}

function studioBrowserSmokeHookReadyExpression() {
  return `(() => {
    const hook = globalThis.__voStudioBrowserSmoke;
    const entryPath = hook?.entryPath?.() ?? null;
    return {
      ready: Boolean(hook?.dumpCurrent && entryPath),
      entryPath,
    };
  })()`;
}

function installCollectors(client) {
  const requests = new Map();
  const events = {
    console: [],
    exceptions: [],
    browserLog: [],
    resourceFailures: [],
  };

  client.on('Runtime.consoleAPICalled', (params) => {
    const text = (params.args ?? [])
      .map((arg) => arg.value ?? arg.description ?? arg.unserializableValue ?? '')
      .filter((value) => value !== '')
      .join(' ');
    events.console.push({
      type: params.type ?? 'log',
      text,
      timestamp: params.timestamp ?? Date.now(),
    });
  });
  client.on('Runtime.exceptionThrown', (params) => {
    events.exceptions.push({
      text: formatExceptionDetails(params.exceptionDetails ?? params),
      timestamp: Date.now(),
    });
  });
  client.on('Log.entryAdded', (params) => {
    const entry = params.entry ?? {};
    events.browserLog.push({
      level: entry.level ?? 'info',
      source: entry.source ?? '',
      text: entry.text ?? '',
      url: entry.url ?? '',
      timestamp: entry.timestamp ?? Date.now(),
    });
  });
  client.on('Network.requestWillBeSent', (params) => {
    if (params.requestId && params.request?.url) {
      requests.set(params.requestId, params.request.url);
    }
  });
  client.on('Network.responseReceived', (params) => {
    const status = params.response?.status ?? 0;
    if (status >= 400) {
      events.resourceFailures.push({
        kind: 'http',
        status,
        url: params.response?.url ?? requests.get(params.requestId) ?? '',
        resourceType: params.type ?? '',
      });
    }
  });
  client.on('Network.loadingFailed', (params) => {
    const url = requests.get(params.requestId) ?? '';
    if (url.endsWith('/favicon.ico')) {
      return;
    }
    events.resourceFailures.push({
      kind: 'loadingFailed',
      errorText: params.errorText ?? '',
      canceled: Boolean(params.canceled),
      url,
      resourceType: params.type ?? '',
    });
  });
  return events;
}

function hasNoWebGpuAdapterState(state) {
  const text = [state?.error, state?.textSample, state?.loading]
    .filter(Boolean)
    .join('\n');
  return noWebGpuAdapterPattern.test(text);
}

async function waitForQuickplayFirstFrame(client, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let lastState = null;
  let lastError = null;
  while (Date.now() < deadline) {
    try {
      const state = await client.evaluate(quickplayStateExpression(), 10000);
      lastState = state;
      lastError = null;
      if (state.runner && state.hasCanvas && state.canvasWidth > 0 && state.canvasHeight > 0 && !state.error) {
        return { ok: true, skipped: false, state };
      }
      if (hasNoWebGpuAdapterState(state)) {
        const webGpu = await client.evaluate(webGpuStateExpression(), 10000).catch((error) => ({
          supported: false,
          adapter: false,
          reason: error instanceof Error ? error.message : String(error),
        }));
        if (!webGpu.adapter) {
          const reason = `no WebGPU adapter available (${webGpu.reason})`;
          return { ok: !requireWebGpuAdapter, skipped: !requireWebGpuAdapter, reason, state, webGpu };
        }
      }
    } catch (error) {
      lastError = error instanceof Error ? error.message : String(error);
    }
    await sleep(300);
  }
  const detail = lastError ?? JSON.stringify(lastState);
  return { ok: false, skipped: false, reason: `timed out after ${timeoutMs}ms; last state: ${detail}`, state: lastState };
}

function countLifecycleState(events, state) {
  return collectBlockKartLifecycle(events.console, []).events.filter((event) => event.state === state).length;
}

async function focusQuickplaySurface(client) {
  await client.send('Page.bringToFront', {}, 5000).catch(() => undefined);
  await client.evaluate(`(() => {
    window.focus();
    const canvas = document.querySelector('.runner-surface canvas, .renderer-surface canvas, canvas');
    if (canvas && typeof canvas.focus === 'function') {
      canvas.focus();
    }
    return true;
  })()`, 5000).catch(() => undefined);
}

async function dispatchResetKey(client) {
  await dispatchKey(client, resetKey);
}

async function dispatchStartRaceKey(client) {
  await dispatchKey(client, startRaceKey);
}

async function dispatchRenderStressKey(client, profile) {
  const key = renderStressKeys[profile];
  if (!key) {
    throw new Error(`unsupported render stress profile "${profile}"`);
  }
  await dispatchKey(client, key);
}

async function dispatchKey(client, key) {
  let lastFailure = 'not attempted';
  for (let attempt = 1; attempt <= 5; attempt++) {
    const result = await tryDispatchKey(client, key);
    if (result.ok) {
      return;
    }
    lastFailure = result.failure;
    await sleep(250 * attempt);
  }
  throw new Error(`${lastFailure} after retries for ${key.code}`);
}

async function tryDispatchKey(client, key) {
  const keyDownDispatched = await dispatchKeyPhase(client, 'keyDown', key);
  if (!keyDownDispatched) {
    return { ok: false, failure: 'keyDown dispatch failed' };
  }
  await sleep(120);
  const keyUpDispatched = await dispatchKeyPhase(client, 'keyUp', key);
  if (!keyUpDispatched) {
    return { ok: false, failure: 'keyUp dispatch failed' };
  }
  return { ok: true, failure: null };
}

async function dispatchKeyPhase(client, type, key) {
  await focusQuickplaySurface(client);
  const domType = type === 'keyDown' ? 'keydown' : 'keyup';
  const domDispatched = await dispatchDomKeyEvent(client, domType, key);
  if (domDispatched) {
    return true;
  }
  return await dispatchCdpKeyEvent(client, type, key);
}

async function dispatchCdpKeyEvent(client, type, key) {
  try {
    await client.send('Input.dispatchKeyEvent', {
      type,
      key: key.key,
      code: key.code,
      windowsVirtualKeyCode: key.keyCode,
      nativeVirtualKeyCode: key.keyCode,
      unmodifiedText: key.key.length === 1 ? key.key : '',
      text: type === 'keyDown' && key.key.length === 1 ? key.key : '',
    }, 10000);
    return true;
  } catch {
    return false;
  }
}

async function dispatchDomKeyEvent(client, type, key) {
  try {
    await client.evaluate(keyEventExpression(type, key), 10000);
    return true;
  } catch {
    return false;
  }
}

function keyEventExpression(type, key) {
  return `(() => {
    const init = {
      key: ${JSON.stringify(key.key)},
      code: ${JSON.stringify(key.code)},
      keyCode: ${Number(key.keyCode)},
      which: ${Number(key.keyCode)},
      bubbles: true,
      cancelable: true,
      composed: true,
    };
    const canvas = document.querySelector('.runner-surface canvas, .renderer-surface canvas, canvas');
    if (canvas && typeof canvas.focus === 'function') {
      canvas.focus();
    }
    const targets = [canvas, document, window].filter(Boolean);
    for (const target of targets) {
      target.dispatchEvent(new KeyboardEvent(${JSON.stringify(type)}, init));
    }
    return targets.length > 0;
  })()`;
}

async function waitForLifecycleStateCount(events, state, minCount, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let count = countLifecycleState(events, state);
  while (Date.now() < deadline) {
    count = countLifecycleState(events, state);
    if (count >= minCount) {
      return { ok: true, count };
    }
    await sleep(100);
  }
  return { ok: false, count };
}

async function waitForRestartProgress(events, minRestartingCount, minRunningCount, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let restartingCount = countLifecycleState(events, 'Restarting');
  let runningCount = countLifecycleState(events, 'Running');
  while (Date.now() < deadline) {
    restartingCount = countLifecycleState(events, 'Restarting');
    runningCount = countLifecycleState(events, 'Running');
    if (restartingCount >= minRestartingCount) {
      return { ok: true, phase: 'Restarting', restartingCount, runningCount };
    }
    if (runningCount >= minRunningCount) {
      return { ok: true, phase: 'Running', restartingCount, runningCount };
    }
    await sleep(100);
  }
  return { ok: false, phase: 'none', restartingCount, runningCount };
}

async function requestRestartWithConfirmation(client, events, beforeRestartingEvents, beforeRunningEvents, timeoutMs) {
  const attempts = [];
  const confirmTimeoutMs = Math.min(5000, Math.max(1500, Math.floor(timeoutMs / 6)));
  for (let attempt = 1; attempt <= 3; attempt++) {
    await dispatchKeyPhase(client, 'keyUp', resetKey).catch(() => false);
    await sleep(50);
    const keyDownDispatched = await dispatchKeyPhase(client, 'keyDown', resetKey);
    if (!keyDownDispatched) {
      const progress = {
        ok: false,
        phase: 'dispatch-failed',
        restartingCount: countLifecycleState(events, 'Restarting'),
        runningCount: countLifecycleState(events, 'Running'),
      };
      attempts.push({ attempt, ...progress });
      await sleep(250 * attempt);
      continue;
    }
    let progress = null;
    try {
      progress = await waitForRestartProgress(events, beforeRestartingEvents + 1, beforeRunningEvents + 1, confirmTimeoutMs);
      attempts.push({
        attempt,
        ok: progress.ok,
        phase: progress.phase,
        restartingCount: progress.restartingCount,
        runningCount: progress.runningCount,
      });
      if (progress.ok) {
        return { ok: true, attempts, progress };
      }
    } finally {
      await dispatchKeyPhase(client, 'keyUp', resetKey).catch(() => false);
    }
    await sleep(250 * attempt);
  }
  return { ok: false, attempts, progress: attempts[attempts.length - 1] ?? null };
}

function countRaceState(events, state) {
  return collectBlockKartDiagnostics(events.console, []).raceReports.filter((report) => report.raceState === state).length;
}

function countRaceReportsLoadedFromStorage(events) {
  return collectBlockKartDiagnostics(events.console, []).raceReports.filter((report) => report.settingsLoadedFromStorage === true).length;
}

function countRenderStressReports(events, profile) {
  return collectBlockKartDiagnostics(events.console, []).sceneReports.filter((report) => report.renderStressProfile === profile && report.renderStressApplied === true).length;
}

async function waitForRaceStateCount(events, state, minCount, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let count = countRaceState(events, state);
  while (Date.now() < deadline) {
    count = countRaceState(events, state);
    if (count >= minCount) {
      return { ok: true, count };
    }
    await sleep(100);
  }
  return { ok: false, count };
}

async function waitForRenderStressProfile(events, profile, minCount, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let count = countRenderStressReports(events, profile);
  while (Date.now() < deadline) {
    count = countRenderStressReports(events, profile);
    if (count >= minCount) {
      return { ok: true, count };
    }
    await sleep(100);
  }
  return { ok: false, count };
}

async function waitForRaceStorageLoadedCount(events, minCount, timeoutMs) {
  const deadline = Date.now() + timeoutMs;
  let count = countRaceReportsLoadedFromStorage(events);
  while (Date.now() < deadline) {
    count = countRaceReportsLoadedFromStorage(events);
    if (count >= minCount) {
      return { ok: true, count };
    }
    await sleep(100);
  }
  return { ok: false, count };
}

async function runRenderStressScenario(client, events, profile, timeoutMs) {
  const requested = profile !== '';
  const initialReports = requested ? countRenderStressReports(events, profile) : 0;
  const scenario = {
    requested,
    profile,
    completed: false,
    skipped: !requested,
    skipReason: requested ? null : 'render stress mode disabled',
    initialReports,
    finalReports: initialReports,
    latestSceneReport: collectBlockKartDiagnostics(events.console, []).latestSceneReport,
  };
  if (!requested) {
    return scenario;
  }
  if (!renderStressKeys[profile]) {
    scenario.skipped = false;
    scenario.failure = `unsupported render stress profile "${profile}"`;
    return scenario;
  }
  await dispatchRenderStressKey(client, profile);
  const wait = await waitForRenderStressProfile(events, profile, initialReports + 1, timeoutMs);
  scenario.completed = wait.ok;
  scenario.skipped = false;
  scenario.skipReason = null;
  scenario.finalReports = countRenderStressReports(events, profile);
  scenario.latestSceneReport = collectBlockKartDiagnostics(events.console, []).latestSceneReport;
  if (!wait.ok) {
    scenario.failure = `render stress profile ${profile} did not report applied=true within ${timeoutMs}ms`;
  }
  return scenario;
}

async function runResizeCycleScenario(client, requested) {
  const scenario = {
    requested,
    completed: false,
    skipped: !requested,
    skipReason: requested ? null : 'resize cycle disabled',
    steps: [],
  };
  if (!requested) {
    return scenario;
  }
  const sizes = [
    { width: Math.max(640, Math.floor(viewportWidth * 0.75)), height: Math.max(360, Math.floor(viewportHeight * 0.75)) },
    { width: viewportWidth, height: viewportHeight },
  ];
  try {
    for (const size of sizes) {
      await client.send('Emulation.setDeviceMetricsOverride', {
        width: size.width,
        height: size.height,
        deviceScaleFactor: 1,
        mobile: false,
        screenWidth: size.width,
        screenHeight: size.height,
      });
      scenario.steps.push(size);
      await sleep(900);
    }
    scenario.completed = true;
    scenario.skipped = false;
    scenario.skipReason = null;
  } catch (error) {
    scenario.skipped = false;
    scenario.failure = error instanceof Error ? error.message : String(error);
  }
  return scenario;
}

async function runStartRaceScenario(client, events, requested, timeoutMs) {
  const initialRunningRaceReports = countRaceState(events, 'Running');
  const scenario = {
    requested,
    completed: false,
    skipped: !requested,
    skipReason: !requested ? 'start-race mode disabled' : null,
    initialRunningRaceReports,
    finalRunningRaceReports: initialRunningRaceReports,
  };
  if (!requested) {
    return scenario;
  }
  await dispatchStartRaceKey(client);
  const wait = await waitForRaceStateCount(events, 'Running', initialRunningRaceReports + 1, timeoutMs);
  scenario.completed = wait.ok;
  scenario.finalRunningRaceReports = countRaceState(events, 'Running');
  scenario.skipped = false;
  scenario.skipReason = null;
  if (!wait.ok) {
    scenario.failure = `race did not enter Running within ${timeoutMs}ms`;
  }
  return scenario;
}

async function runStorageReloadScenario(client, events, quickplayUrl, requested, timeoutMs) {
  const initialRunningEvents = countLifecycleState(events, 'Running');
  const initialLoadedReports = countRaceReportsLoadedFromStorage(events);
  const scenario = {
    requested,
    completed: false,
    skipped: !requested,
    skipReason: !requested ? 'storage reload mode disabled' : null,
    initialRunningEvents,
    finalRunningEvents: initialRunningEvents,
    initialLoadedReports,
    finalLoadedReports: initialLoadedReports,
    latestRaceReport: collectBlockKartDiagnostics(events.console, []).latestRaceReport,
  };
  if (!requested) {
    return scenario;
  }
  const reloadUrl = new URL(quickplayUrl.toString());
  reloadUrl.searchParams.set('blockkartBaselineStorageReload', String(Date.now()));
  await navigate(client, reloadUrl.toString());
  const runningWait = await waitForLifecycleStateCount(events, 'Running', initialRunningEvents + 1, timeoutMs);
  scenario.finalRunningEvents = countLifecycleState(events, 'Running');
  if (!runningWait.ok) {
    scenario.skipped = false;
    scenario.skipReason = null;
    scenario.failure = `reload did not return to lifecycle Running within ${timeoutMs}ms`;
    return scenario;
  }
  const loadedWait = await waitForRaceStorageLoadedCount(events, initialLoadedReports + 1, timeoutMs);
  scenario.finalLoadedReports = countRaceReportsLoadedFromStorage(events);
  scenario.latestRaceReport = collectBlockKartDiagnostics(events.console, []).latestRaceReport;
  scenario.completed = loadedWait.ok;
  scenario.skipped = false;
  scenario.skipReason = null;
  if (!loadedWait.ok) {
    scenario.failure = `reload did not report settingsLoadedFromStorage=true within ${timeoutMs}ms`;
  }
  return scenario;
}

async function runRestartScenario(client, events, requested, timeoutMs) {
  const initialRunningEvents = countLifecycleState(events, 'Running');
  const scenario = {
    requested,
    completed: 0,
    skipped: requested === 0,
    skipReason: requested === 0 ? 'restart mode disabled' : null,
    initialRunningEvents,
    finalRunningEvents: initialRunningEvents,
    iterations: [],
  };
  if (requested <= 0) {
    return scenario;
  }
  for (let index = 1; index <= requested; index++) {
    const beforeRunningEvents = countLifecycleState(events, 'Running');
    const beforeRestartingEvents = countLifecycleState(events, 'Restarting');
    if (requested >= 10) {
      console.log(`BlockKart baseline: restart ${index}/${requested} request`);
    }
    const request = await requestRestartWithConfirmation(client, events, beforeRestartingEvents, beforeRunningEvents, timeoutMs);
    const wait = countLifecycleState(events, 'Running') >= beforeRunningEvents + 1
      ? { ok: true, count: countLifecycleState(events, 'Running') }
      : await waitForLifecycleStateCount(events, 'Running', beforeRunningEvents + 1, timeoutMs);
    const afterRunningEvents = countLifecycleState(events, 'Running');
    const afterRestartingEvents = countLifecycleState(events, 'Restarting');
    const iteration = {
      index,
      ok: wait.ok,
      restartRequestConfirmed: request.ok,
      restartRequestAttempts: request.attempts,
      beforeRunningEvents,
      afterRunningEvents,
      beforeRestartingEvents,
      afterRestartingEvents,
    };
    scenario.iterations.push(iteration);
    scenario.finalRunningEvents = afterRunningEvents;
    if (!wait.ok) {
      const requestDetail = request.ok ? `request confirmed via ${request.progress?.phase ?? 'progress'}` : `request was not confirmed after ${request.attempts.length} attempts`;
      scenario.failure = `restart ${index} did not return to Running within ${timeoutMs}ms (${requestDetail})`;
      if (requested >= 10) {
        console.log(`BlockKart baseline: restart ${index}/${requested} failed running=${afterRunningEvents} restarting=${afterRestartingEvents}`);
      }
      break;
    }
    scenario.completed = index;
    if (requested >= 10) {
      console.log(`BlockKart baseline: restart ${index}/${requested} ok running=${afterRunningEvents} restarting=${afterRestartingEvents}`);
    }
    await sleep(150);
  }
  scenario.skipped = false;
  scenario.skipReason = null;
  return scenario;
}

async function captureScreenshot(client, file, clip = null) {
  const normalizedClip = clip ? {
    x: Math.max(0, clip.x),
    y: Math.max(0, clip.y),
    width: Math.max(1, clip.width),
    height: Math.max(1, clip.height),
    scale: 1,
  } : null;
  const attempts = [
    { format: 'png', fromSurface: false, captureBeyondViewport: false },
    { format: 'png', fromSurface: true, captureBeyondViewport: false },
  ];
  let result = null;
  let lastError = null;
  await client.send('Page.bringToFront', {}, 5000).catch(() => undefined);
  for (let attemptIndex = 1; attemptIndex <= cdpScreenshotAttempts && !result; attemptIndex++) {
    for (const attempt of attempts) {
      const params = normalizedClip ? { ...attempt, clip: normalizedClip } : attempt;
      try {
        result = await client.send('Page.captureScreenshot', params, cdpScreenshotTimeoutMs);
        break;
      } catch (error) {
        lastError = error;
      }
    }
    if (!result && attemptIndex < cdpScreenshotAttempts) {
      await sleep(500);
    }
  }
  if (!result) {
    throw new Error(`Page.captureScreenshot failed after ${cdpScreenshotAttempts} attempts: ${lastError instanceof Error ? lastError.message : String(lastError ?? 'unknown error')}`);
  }
  const bytes = Buffer.from(result.data, 'base64');
  writeFileSync(file, bytes);
  return analyzePng(bytes);
}

async function captureCanvasDataUrl(client, file) {
  let lastError = null;
  for (let attempt = 1; attempt <= canvasDataUrlAttempts; attempt++) {
    try {
      const dataUrl = await client.evaluate(`(() => {
        const canvas = document.querySelector('.runner-surface canvas, .renderer-surface canvas, canvas');
        if (!canvas) {
          return { error: 'canvas not found' };
        }
        try {
          return { dataUrl: canvas.toDataURL('image/png') };
        } catch (error) {
          return { error: error instanceof Error ? error.message : String(error) };
        }
      })()`, canvasDataUrlTimeoutMs);
      if (!dataUrl?.dataUrl || typeof dataUrl.dataUrl !== 'string') {
        throw new Error(dataUrl?.error ?? 'canvas toDataURL returned no data');
      }
      const match = dataUrl.dataUrl.match(/^data:image\/png;base64,(.+)$/);
      if (!match) {
        throw new Error('canvas toDataURL did not return a PNG data URL');
      }
      const bytes = Buffer.from(match[1], 'base64');
      writeFileSync(file, bytes);
      return analyzePng(bytes);
    } catch (error) {
      lastError = error;
      if (attempt < canvasDataUrlAttempts) {
        await sleep(500);
      }
    }
  }
  throw new Error(`canvas toDataURL capture failed after ${canvasDataUrlAttempts} attempts: ${lastError instanceof Error ? lastError.message : String(lastError ?? 'unknown error')}`);
}

async function captureFreshBrowserCanvas(url, viewportFile, canvasFile, browserBin) {
  const bin = browserBin ?? findBrowserBinary();
  if (!bin) {
    throw new Error('could not find Chrome/Chromium for fresh browser canvas capture');
  }
  const debugPort = await reservePort();
  const profileDir = mkdtempSync(path.join(os.tmpdir(), 'volang-blockkart-fresh-capture-'));
  let log = '';
  const child = spawn(
    bin,
    [
      '--headless=new',
      '--disable-dev-shm-usage',
      '--disable-background-timer-throttling',
      '--disable-backgrounding-occluded-windows',
      '--disable-renderer-backgrounding',
      '--disable-features=CalculateNativeWinOcclusion',
      '--enable-unsafe-webgpu',
      '--ignore-gpu-blocklist',
      '--no-sandbox',
      `--remote-debugging-port=${debugPort}`,
      `--user-data-dir=${profileDir}`,
      'about:blank',
    ],
    {
      detached: process.platform !== 'win32',
      stdio: ['ignore', 'pipe', 'pipe'],
    },
  );
  child.stdout.on('data', (chunk) => {
    log = appendLog(log, chunk);
  });
  child.stderr.on('data', (chunk) => {
    log = appendLog(log, chunk);
  });
  let client = null;
  let targetId = null;
  try {
    await fetchOk(`http://127.0.0.1:${debugPort}/json/version`);
    ({ targetId, client } = await openPage(debugPort));
    await client.send('Page.enable');
    await client.send('Runtime.enable');
    await client.send('Log.enable');
    await client.send('Network.enable');
    await client.send('Emulation.setDeviceMetricsOverride', {
      width: viewportWidth,
      height: viewportHeight,
      deviceScaleFactor: 1,
      mobile: false,
      screenWidth: viewportWidth,
      screenHeight: viewportHeight,
    });
    await navigate(client, url);
    const firstFrame = await waitForQuickplayFirstFrame(client, firstFrameTimeoutMs);
    if (!firstFrame.ok || firstFrame.skipped) {
      throw new Error(firstFrame.reason ?? 'fresh browser did not reach first frame');
    }
    await sleep(1000);
    const state = await client.evaluate(quickplayStateExpression(), 30000).catch(() => firstFrame.state ?? {});
    const viewportAnalysis = await captureScreenshot(client, viewportFile);
    let canvasAnalysis = null;
    let canvasMode = 'fresh-browser-crop';
    if (state.canvasRect && state.canvasRect.width > 0 && state.canvasRect.height > 0) {
      const canvasBytes = cropPng(readFileSync(viewportFile), state.canvasRect, viewportWidth, viewportHeight);
      writeFileSync(canvasFile, canvasBytes);
      canvasAnalysis = analyzePng(canvasBytes);
    }
    if (!canvasAnalysis?.nonEmpty) {
      canvasAnalysis = await captureCanvasDataUrl(client, canvasFile);
      canvasMode = 'fresh-browser-data-url';
    }
    return { viewportAnalysis, canvasAnalysis, viewportMode: 'fresh-browser', canvasMode };
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    throw new Error(log ? `${detail}\nfresh browser log:\n${log}` : detail);
  } finally {
    client?.close();
    await closePage(debugPort, targetId).catch(() => undefined);
    stopProcess(child);
    try {
      rmSync(profileDir, { recursive: true, force: true });
    } catch {
      // Chrome may release profile files after the fallback screenshot is already usable.
    }
  }
}

async function captureViewportScreencast(client, file) {
  const capture = waitForScreencastFrame(client);
  await client.send('Page.bringToFront', {}, 5000).catch(() => undefined);
  try {
    await client.send('Page.startScreencast', {
      format: 'png',
      maxWidth: viewportWidth,
      maxHeight: viewportHeight,
      everyNthFrame: 1,
    }, 5000);
    const result = await capture.promise;
    writeFileSync(file, result.bytes);
    return result;
  } finally {
    capture.cancel();
    await client.send('Page.stopScreencast', {}, 5000).catch(() => undefined);
  }
}

async function captureViewportBrowserCli(url, file, browserBin) {
  const attempts = positiveInt(process.env.BLOCKKART_BASELINE_SCREENSHOT_ATTEMPTS, 3);
  const failures = [];
  let best = null;
  for (let attempt = 1; attempt <= attempts; attempt++) {
    try {
      const result = await captureViewportBrowserCliAttempt(url, file, browserBin);
      result.metadata = { ...result.metadata, attempt };
      if (!best || visualScore(result.analysis) > visualScore(best.analysis)) {
        best = result;
      }
      if (result.analysis.nonEmpty) {
        return result;
      }
      failures.push(`attempt ${attempt} produced a visually blank screenshot`);
    } catch (error) {
      failures.push(`attempt ${attempt} failed: ${error instanceof Error ? error.message : String(error)}`);
    }
    if (attempt < attempts) {
      rmSync(file, { force: true });
      await sleep(1000);
    }
  }
  if (best) {
    throw new Error(failures.join('\n') || 'browser CLI screenshot produced only visually blank frames');
  }
  throw new Error(failures.join('\n') || 'browser CLI screenshot failed');
}

async function captureViewportBrowserCliAttempt(url, file, browserBin) {
  const bin = browserBin ?? findBrowserBinary();
  if (!bin) {
    throw new Error('could not find Chrome/Chromium for browser CLI screenshot');
  }
  rmSync(file, { force: true });
  const profileDir = mkdtempSync(path.join(os.tmpdir(), 'volang-blockkart-screenshot-'));
  let log = '';
  const child = spawn(
    bin,
    [
      '--headless=new',
      '--disable-dev-shm-usage',
      '--enable-unsafe-webgpu',
      '--ignore-gpu-blocklist',
      '--no-sandbox',
      '--hide-scrollbars',
      '--run-all-compositor-stages-before-draw',
      '--force-device-scale-factor=1',
      `--user-data-dir=${profileDir}`,
      `--window-size=${viewportWidth},${viewportHeight}`,
      `--screenshot=${file}`,
      '--timeout=15000',
      '--virtual-time-budget=12000',
      url,
    ],
    {
      detached: process.platform !== 'win32',
      stdio: ['ignore', 'pipe', 'pipe'],
    },
  );
  child.stdout.on('data', (chunk) => {
    log = appendLog(log, chunk);
  });
  child.stderr.on('data', (chunk) => {
    log = appendLog(log, chunk);
  });
  try {
    return await waitForScreenshotFile(file, child, 30000, () => log);
  } finally {
    await stopProcessAndWait(child);
    try {
      rmSync(profileDir, { recursive: true, force: true });
    } catch {
      // Chrome can release profile files after the screenshot is already usable.
    }
  }
}

function visualScore(analysis) {
  return (analysis?.nonEmpty ? 1000000 : 0)
    + (analysis?.uniqueSampledColors ?? 0) * 10
    + (analysis?.lumaStdDev ?? 0);
}

async function waitForScreenshotFile(file, child, timeoutMs, log) {
  const deadline = Date.now() + timeoutMs;
  let lastError = 'not attempted';
  while (Date.now() < deadline) {
    if (existsSync(file)) {
      try {
        const bytes = readFileSync(file);
        return {
          bytes,
          analysis: analyzePng(bytes),
          metadata: { source: 'browser-cli' },
        };
      } catch (error) {
        lastError = error instanceof Error ? error.message : String(error);
      }
    }
    if ((child.exitCode !== null || child.signalCode !== null) && !existsSync(file)) {
      throw new Error(`browser CLI exited before writing ${file}\n${log()}`);
    }
    await sleep(250);
  }
  throw new Error(`browser CLI screenshot did not become readable after ${timeoutMs}ms; last error: ${lastError}\n${log()}`);
}

function waitForScreencastFrame(client) {
  let off = () => undefined;
  let timeout = null;
  let sampleTimeout = null;
  let settled = false;
  let best = null;
  let lastError = null;
  let frameCount = 0;

  const promise = new Promise((resolve, reject) => {
    const finish = (callback, value) => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timeout);
      clearTimeout(sampleTimeout);
      off();
      callback(value);
    };
    const maybeResolve = () => {
      if (best) {
        finish(resolve, best);
      }
    };
    timeout = setTimeout(() => {
      if (best) {
        finish(resolve, best);
      } else {
        finish(reject, new Error(lastError ?? 'timed out waiting for Page.screencastFrame'));
      }
    }, 15000);
    off = client.on('Page.screencastFrame', (params) => {
      void client.send('Page.screencastFrameAck', { sessionId: params.sessionId }, 5000).catch(() => undefined);
      try {
        const bytes = Buffer.from(params.data ?? '', 'base64');
        const analysis = analyzePng(bytes);
        const score = (analysis.nonEmpty ? 1000000 : 0) + analysis.uniqueSampledColors * 10 + analysis.lumaStdDev;
        const frame = {
          bytes,
          analysis,
          metadata: params.metadata ?? null,
          score,
        };
        if (!best || frame.score > best.score) {
          best = frame;
        }
        frameCount++;
        if (analysis.nonEmpty && frameCount >= 2) {
          maybeResolve();
          return;
        }
        if (frameCount >= 8) {
          maybeResolve();
        } else if (!sampleTimeout) {
          sampleTimeout = setTimeout(maybeResolve, 1500);
        }
      } catch (error) {
        lastError = error instanceof Error ? error.message : String(error);
      }
    });
  });

  return {
    promise,
    cancel: () => {
      if (settled) {
        return;
      }
      settled = true;
      clearTimeout(timeout);
      clearTimeout(sampleTimeout);
      off();
    },
  };
}

function decodePng(bytes) {
  const signature = '89504e470d0a1a0a';
  if (bytes.subarray(0, 8).toString('hex') !== signature) {
    throw new Error('not a PNG file');
  }
  let offset = 8;
  let width = 0;
  let height = 0;
  let bitDepth = 0;
  let colorType = 0;
  const idat = [];
  while (offset < bytes.length) {
    const length = bytes.readUInt32BE(offset);
    const type = bytes.subarray(offset + 4, offset + 8).toString('ascii');
    const data = bytes.subarray(offset + 8, offset + 8 + length);
    offset += 12 + length;
    if (type === 'IHDR') {
      width = data.readUInt32BE(0);
      height = data.readUInt32BE(4);
      bitDepth = data[8];
      colorType = data[9];
    } else if (type === 'IDAT') {
      idat.push(data);
    } else if (type === 'IEND') {
      break;
    }
  }
  if (bitDepth !== 8) {
    throw new Error(`unsupported PNG bit depth ${bitDepth}`);
  }
  const bytesPerPixel = colorType === 6 ? 4 : colorType === 2 ? 3 : colorType === 0 ? 1 : colorType === 4 ? 2 : 0;
  if (!bytesPerPixel) {
    throw new Error(`unsupported PNG color type ${colorType}`);
  }
  const inflated = inflateSync(Buffer.concat(idat));
  const rowBytes = width * bytesPerPixel;
  const pixels = Buffer.alloc(rowBytes * height);
  let inOffset = 0;
  for (let y = 0; y < height; y++) {
    const filter = inflated[inOffset++];
    const rowOffset = y * rowBytes;
    for (let x = 0; x < rowBytes; x++) {
      const raw = inflated[inOffset++];
      const left = x >= bytesPerPixel ? pixels[rowOffset + x - bytesPerPixel] : 0;
      const up = y > 0 ? pixels[rowOffset - rowBytes + x] : 0;
      const upLeft = y > 0 && x >= bytesPerPixel ? pixels[rowOffset - rowBytes + x - bytesPerPixel] : 0;
      let value;
      if (filter === 0) {
        value = raw;
      } else if (filter === 1) {
        value = raw + left;
      } else if (filter === 2) {
        value = raw + up;
      } else if (filter === 3) {
        value = raw + Math.floor((left + up) / 2);
      } else if (filter === 4) {
        value = raw + paeth(left, up, upLeft);
      } else {
        throw new Error(`unsupported PNG filter ${filter}`);
      }
      pixels[rowOffset + x] = value & 0xff;
    }
  }
  return { width, height, colorType, bytesPerPixel, pixels };
}

function cropPng(bytes, rect, cssViewportWidth, cssViewportHeight) {
  if (!rect || rect.width <= 0 || rect.height <= 0) {
    throw new Error('canvas rect is empty');
  }
  const image = decodePng(bytes);
  const scaleX = image.width / Math.max(1, cssViewportWidth);
  const scaleY = image.height / Math.max(1, cssViewportHeight);
  const x = clamp(Math.floor(rect.x * scaleX), 0, image.width - 1);
  const y = clamp(Math.floor(rect.y * scaleY), 0, image.height - 1);
  const width = clamp(Math.ceil(rect.width * scaleX), 1, image.width - x);
  const height = clamp(Math.ceil(rect.height * scaleY), 1, image.height - y);
  const rgba = Buffer.alloc(width * height * 4);
  for (let row = 0; row < height; row++) {
    for (let col = 0; col < width; col++) {
      const [r, g, b, a] = pixelRgba(image, x + col, y + row);
      const offset = (row * width + col) * 4;
      rgba[offset] = r;
      rgba[offset + 1] = g;
      rgba[offset + 2] = b;
      rgba[offset + 3] = a;
    }
  }
  return encodeRgbaPng(width, height, rgba);
}

function pixelRgba(image, x, y) {
  const offset = (y * image.width + x) * image.bytesPerPixel;
  if (image.colorType === 6) {
    return [
      image.pixels[offset],
      image.pixels[offset + 1],
      image.pixels[offset + 2],
      image.pixels[offset + 3],
    ];
  }
  if (image.colorType === 2) {
    return [
      image.pixels[offset],
      image.pixels[offset + 1],
      image.pixels[offset + 2],
      255,
    ];
  }
  if (image.colorType === 4) {
    const value = image.pixels[offset];
    return [value, value, value, image.pixels[offset + 1]];
  }
  const value = image.pixels[offset];
  return [value, value, value, 255];
}

function encodeRgbaPng(width, height, rgba) {
  const signature = Buffer.from('89504e470d0a1a0a', 'hex');
  const ihdr = Buffer.alloc(13);
  ihdr.writeUInt32BE(width, 0);
  ihdr.writeUInt32BE(height, 4);
  ihdr[8] = 8;
  ihdr[9] = 6;
  ihdr[10] = 0;
  ihdr[11] = 0;
  ihdr[12] = 0;
  const rowBytes = width * 4;
  const scanlines = Buffer.alloc((rowBytes + 1) * height);
  for (let row = 0; row < height; row++) {
    const scanlineOffset = row * (rowBytes + 1);
    scanlines[scanlineOffset] = 0;
    rgba.copy(scanlines, scanlineOffset + 1, row * rowBytes, row * rowBytes + rowBytes);
  }
  return Buffer.concat([
    signature,
    pngChunk('IHDR', ihdr),
    pngChunk('IDAT', deflateSync(scanlines)),
    pngChunk('IEND', Buffer.alloc(0)),
  ]);
}

function pngChunk(type, data) {
  const typeBytes = Buffer.from(type, 'ascii');
  const chunk = Buffer.alloc(12 + data.length);
  chunk.writeUInt32BE(data.length, 0);
  typeBytes.copy(chunk, 4);
  data.copy(chunk, 8);
  chunk.writeUInt32BE(crc32(Buffer.concat([typeBytes, data])), 8 + data.length);
  return chunk;
}

const crcTable = makeCrcTable();

function makeCrcTable() {
  const table = new Uint32Array(256);
  for (let n = 0; n < 256; n++) {
    let c = n;
    for (let k = 0; k < 8; k++) {
      c = (c & 1) ? (0xedb88320 ^ (c >>> 1)) : (c >>> 1);
    }
    table[n] = c >>> 0;
  }
  return table;
}

function crc32(bytes) {
  let crc = 0xffffffff;
  for (const byte of bytes) {
    crc = crcTable[(crc ^ byte) & 0xff] ^ (crc >>> 8);
  }
  return (crc ^ 0xffffffff) >>> 0;
}

function clamp(value, min, max) {
  return Math.min(max, Math.max(min, value));
}

function paeth(a, b, c) {
  const p = a + b - c;
  const pa = Math.abs(p - a);
  const pb = Math.abs(p - b);
  const pc = Math.abs(p - c);
  if (pa <= pb && pa <= pc) return a;
  if (pb <= pc) return b;
  return c;
}

function analyzePng(bytes) {
  const image = decodePng(bytes);
  const totalPixels = image.width * image.height;
  const step = Math.max(1, Math.floor(Math.sqrt(totalPixels / 20000)));
  const colors = new Set();
  let count = 0;
  let alphaNonZero = 0;
  let sum = 0;
  let sumSq = 0;
  let minLuma = 255;
  let maxLuma = 0;
  for (let y = 0; y < image.height; y += step) {
    for (let x = 0; x < image.width; x += step) {
      const offset = (y * image.width + x) * image.bytesPerPixel;
      let r;
      let g;
      let b;
      let a = 255;
      if (image.colorType === 6) {
        r = image.pixels[offset];
        g = image.pixels[offset + 1];
        b = image.pixels[offset + 2];
        a = image.pixels[offset + 3];
      } else if (image.colorType === 2) {
        r = image.pixels[offset];
        g = image.pixels[offset + 1];
        b = image.pixels[offset + 2];
      } else if (image.colorType === 4) {
        r = image.pixels[offset];
        g = r;
        b = r;
        a = image.pixels[offset + 1];
      } else {
        r = image.pixels[offset];
        g = r;
        b = r;
      }
      const luma = 0.2126 * r + 0.7152 * g + 0.0722 * b;
      sum += luma;
      sumSq += luma * luma;
      minLuma = Math.min(minLuma, luma);
      maxLuma = Math.max(maxLuma, luma);
      if (a > 0) {
        alphaNonZero++;
      }
      colors.add(`${r >> 3},${g >> 3},${b >> 3},${a >> 6}`);
      count++;
    }
  }
  const meanLuma = count > 0 ? sum / count : 0;
  const variance = count > 0 ? Math.max(0, sumSq / count - meanLuma * meanLuma) : 0;
  const lumaStdDev = Math.sqrt(variance);
  const uniqueSampledColors = colors.size;
  return {
    width: image.width,
    height: image.height,
    sampleStep: step,
    sampleCount: count,
    uniqueSampledColors,
    meanLuma: round(meanLuma),
    lumaStdDev: round(lumaStdDev),
    minLuma: round(minLuma),
    maxLuma: round(maxLuma),
    alphaNonZeroRatio: count > 0 ? round(alphaNonZero / count) : 0,
    nonEmpty: count > 0 && uniqueSampledColors >= 64 && lumaStdDev >= 5,
  };
}

function round(value) {
  return Math.round(value * 1000) / 1000;
}

function parseDurationMs(text) {
  const match = String(text).match(/\b(\d+(?:\.\d+)?)(ms|s)\b/);
  if (!match) {
    return null;
  }
  const value = Number.parseFloat(match[1]);
  return match[2] === 's' ? value * 1000 : value;
}

function classifyStartupPhases(lines) {
  return (lines ?? [])
    .filter((line) => line && isStartupPhaseLine(line.text))
    .map((line) => ({
      kind: line.kind,
      text: line.text,
      durationMs: parseDurationMs(line.text),
      ts: line.ts,
    }));
}

function isStartupPhaseLine(text) {
  return /\bstartup phase=|\bstudio-wasm:|^\[studio-gui\]|\bOpened GUI\b|\btotal open\b|\bcompileGui\b|\bpreload wasm extension\b/i.test(String(text ?? ''));
}

function collectVoplaySlowFrames(consoleEvents, consoleLines) {
  const combined = [
    ...(consoleEvents ?? []).map((event) => ({ source: 'browser-console', type: event.type, text: event.text, ts: event.timestamp ?? null })),
    ...(consoleLines ?? []).map((line) => ({ source: 'studio-console', type: line.kind, text: line.text, ts: line.ts ?? null })),
  ];
  return combined.filter((entry) => /voplay (render )?slow frame|voplay work slow|voplay renderer slow submit/i.test(entry.text));
}

function parseBlockKartMarker(text, marker) {
  const raw = String(text ?? '');
  const index = raw.indexOf(marker);
  if (index === -1) {
    return null;
  }
  const payload = raw.slice(index + marker.length).trim();
  if (!payload) {
    return null;
  }
  try {
    return JSON.parse(payload);
  } catch {
    return null;
  }
}

function collectBlockKartLifecycle(consoleEvents, consoleLines) {
  const combined = [
    ...(consoleEvents ?? []).map((event) => ({
      source: 'browser-console',
      kind: event.type,
      text: event.text,
      ts: event.timestamp,
    })),
    ...(consoleLines ?? []).map((line) => ({
      source: 'studio-console',
      kind: line.kind,
      text: line.text,
      ts: line.ts,
    })),
  ];
  const events = [];
  const failureReports = [];
  for (const line of combined) {
    const lifecycle = parseBlockKartMarker(line.text, blockKartLifecycleMarker);
    if (lifecycle) {
      events.push({
        state: lifecycle.state ?? null,
        startupPhase: lifecycle.startupPhase ?? null,
        source: line.source,
        kind: line.kind,
        ts: line.ts ?? null,
        text: line.text,
      });
    }
    const failure = parseBlockKartMarker(line.text, blockKartFailureReportMarker);
    if (failure) {
      failureReports.push({
        ...failure,
        source: line.source,
        kind: line.kind,
        ts: line.ts ?? null,
      });
    }
  }
  const last = events.length > 0 ? events[events.length - 1] : null;
  return {
    state: last?.state ?? null,
    startupPhase: last?.startupPhase ?? null,
    reachedRunning: events.some((event) => event.state === 'Running'),
    events,
    failureReport: failureReports.length > 0 ? failureReports[failureReports.length - 1] : null,
    failureReports,
  };
}

function combinedConsoleLines(consoleEvents, consoleLines) {
  return [
    ...(consoleEvents ?? []).map((event) => ({
      source: 'browser-console',
      kind: event.type,
      text: event.text,
      ts: event.timestamp,
    })),
    ...(consoleLines ?? []).map((line) => ({
      source: 'studio-console',
      kind: line.kind,
      text: line.text,
      ts: line.ts,
    })),
  ];
}

function collectStructuredMarkerReports(consoleEvents, consoleLines, marker) {
  const reports = [];
  for (const line of combinedConsoleLines(consoleEvents, consoleLines)) {
    const parsed = parseBlockKartMarker(line.text, marker);
    if (!parsed) {
      continue;
    }
    reports.push({
      ...parsed,
      source: line.source,
      kind: line.kind,
      ts: line.ts ?? null,
    });
  }
  return reports;
}

function collectBlockKartDiagnostics(consoleEvents, consoleLines) {
  const assetReports = collectStructuredMarkerReports(consoleEvents, consoleLines, blockKartAssetReportMarker);
  const sceneReports = collectStructuredMarkerReports(consoleEvents, consoleLines, blockKartSceneReportMarker);
  const vehicleReports = collectStructuredMarkerReports(consoleEvents, consoleLines, blockKartVehicleReportMarker);
  const raceReports = collectStructuredMarkerReports(consoleEvents, consoleLines, blockKartRaceReportMarker);
  const restartDiagnosticReports = (reports) => reports.filter((report) => report.phase === 'worldReady' || report.phase === 'raceRestart');
  return {
    assetReports,
    sceneReports,
    vehicleReports,
    raceReports,
    latestAssetReport: assetReports.length > 0 ? assetReports[assetReports.length - 1] : null,
    latestSceneReport: sceneReports.length > 0 ? sceneReports[sceneReports.length - 1] : null,
    latestVehicleReport: vehicleReports.length > 0 ? vehicleReports[vehicleReports.length - 1] : null,
    latestRaceReport: raceReports.length > 0 ? raceReports[raceReports.length - 1] : null,
    worldReadyAssetReports: assetReports.filter((report) => report.phase === 'worldReady'),
    worldReadySceneReports: sceneReports.filter((report) => report.phase === 'worldReady'),
    worldReadyVehicleReports: vehicleReports.filter((report) => report.phase === 'worldReady'),
    worldReadyRaceReports: raceReports.filter((report) => report.phase === 'worldReady'),
    restartAssetReports: restartDiagnosticReports(assetReports),
    restartSceneReports: restartDiagnosticReports(sceneReports),
    restartVehicleReports: restartDiagnosticReports(vehicleReports),
    restartRaceReports: restartDiagnosticReports(raceReports),
  };
}

function startupPhaseRecord(phase) {
  const text = String(phase?.text ?? '');
  const startupMatch = text.match(/\bstartup phase=([A-Za-z0-9_.:-]+)/);
  const studioMatch = text.match(/\[studio-wasm:([^\]]+)\]/);
  const guiMatch = text.match(/\[studio-gui\]\s+([A-Za-z0-9_.:-]+)/);
  const name = startupMatch?.[1] ?? studioMatch?.[1] ?? guiMatch?.[1] ?? (text.startsWith('Opened GUI ') ? 'openGui' : 'unknown');
  const record = {
    name,
    label: startupMatch ? startupLabel(text) : 'Studio startup',
    owner: 'Studio',
    subsystem: 'Runtime',
    kind: phase?.kind ?? null,
    durationMs: phase?.durationMs ?? null,
    ts: phase?.ts ?? null,
    text,
  };
  if (/BlockKart primitive startup/.test(text)) {
    record.label = 'BlockKart primitive';
    record.owner = 'BlockKart';
    record.subsystem = primitiveStartupSubsystem(name);
  } else if (/BlockKart world startup/.test(text)) {
    record.label = 'BlockKart world';
    record.owner = 'BlockKart';
    record.subsystem = worldStartupSubsystem(name);
  } else if (/preload wasm extension|studio-wasm|compileGui|total open|Opened GUI/.test(text)) {
    record.owner = /voplay/i.test(text) ? 'voplay' : 'Studio';
    record.subsystem = /wasm|compile|open/i.test(text) ? 'Runtime' : 'Renderer';
  }
  return record;
}

function startupLabel(text) {
  if (/BlockKart primitive startup/.test(text)) {
    return 'BlockKart primitive';
  }
  if (/BlockKart world startup/.test(text)) {
    return 'BlockKart world';
  }
  return 'startup';
}

function primitiveStartupSubsystem(name) {
  if (/Map|Terrain|TrackAsset|NewTrack|trackSpawnAndCheckpoints/i.test(name)) {
    return 'Map';
  }
  if (/Visual|Layer|Scene|Scatter|Primitive/i.test(name)) {
    return 'Scene';
  }
  if (/Physics/i.test(name)) {
    return 'Physics';
  }
  if (/Gameplay/i.test(name)) {
    return 'Gameplay';
  }
  return 'Scene';
}

function worldStartupSubsystem(name) {
  if (/loadAsset|loadModel/i.test(name)) {
    return 'Asset';
  }
  if (/buildLevel|Map|Track|Terrain/i.test(name)) {
    return 'Map';
  }
  if (/spawnPlayer|Vehicle/i.test(name)) {
    return 'Vehicle';
  }
  if (/Audio/i.test(name)) {
    return 'Audio';
  }
  if (/Input/i.test(name)) {
    return 'Input';
  }
  if (/Camera|Scene|configure/i.test(name)) {
    return 'Scene';
  }
  return 'Runtime';
}

function summarizeStartupAttribution(startupPhases) {
  const records = (startupPhases ?? []).map(startupPhaseRecord);
  const timed = records.filter((record) => Number.isFinite(record.durationMs));
  const slowest = [...timed].sort((a, b) => b.durationMs - a.durationMs).slice(0, 12);
  const byOwner = sumDurationBy(timed, 'owner');
  const bySubsystem = sumDurationBy(timed, 'subsystem');
  const critical = Object.fromEntries(['preparePrimitiveMapAsset', 'buildPrimitiveTrackVisuals'].map((name) => [
    name,
    timed.find((record) => record.name === name) ?? null,
  ]));
  return { records, slowest, byOwner, bySubsystem, critical };
}

function sumDurationBy(records, key) {
  const sums = new Map();
  for (const record of records) {
    const value = record[key] ?? 'unknown';
    sums.set(value, round((sums.get(value) ?? 0) + record.durationMs));
  }
  return Object.fromEntries([...sums.entries()].sort(([a], [b]) => String(a).localeCompare(String(b))));
}

function collectSlowFrameAttribution(slowFrames, lifecycle) {
  const events = lifecycle?.events ?? [];
  const runningTs = events.find((event) => event.state === 'Running')?.ts ?? null;
  const restartWindows = lifecycleRestartWindows(events);
  const frames = (slowFrames ?? []).map((frame, index) => {
    const text = String(frame.text ?? '');
    const reason = slowFrameReason(text);
    const subsystem = /submit|renderer|render slow|reason=(loop|submit|cadence|pacing)/i.test(text) ? 'Renderer' : 'Runtime';
    const phase = classifySlowFramePhase(frame.ts ?? null, runningTs, restartWindows);
    return {
      index,
      phase,
      owner: 'voplay',
      subsystem,
      reason,
      text,
      ts: frame.ts ?? null,
      source: frame.source ?? null,
    };
  });
  return {
    count: frames.length,
    firstFrame: frames.filter((frame) => frame.phase === 'first-frame-warmup').length,
    firstFrameWarmup: frames.filter((frame) => frame.phase === 'first-frame-warmup').length,
    restartRebuild: frames.filter((frame) => frame.phase === 'restart-rebuild').length,
    steadyState: frames.filter((frame) => frame.phase === 'steady-state').length,
    byPhase: countBy(frames, 'phase'),
    byReason: countBy(frames, 'reason'),
    frames,
  };
}

function lifecycleRestartWindows(events) {
  const windows = [];
  let active = null;
  for (const event of events ?? []) {
    if (event.state === 'Restarting') {
      active = { start: event.ts ?? null, end: null };
      continue;
    }
    if (event.state === 'Running' && active) {
      active.end = event.ts ?? null;
      windows.push(active);
      active = null;
    }
  }
  if (active) {
    windows.push(active);
  }
  return windows;
}

function classifySlowFramePhase(ts, runningTs, restartWindows) {
  if (ts != null) {
    for (const window of restartWindows ?? []) {
      if (window.start == null) {
        continue;
      }
      const end = window.end == null ? window.start + 5000 : window.end + 1000;
      if (ts >= window.start && ts <= end) {
        return 'restart-rebuild';
      }
    }
  }
  if (runningTs == null || ts == null || ts <= runningTs) {
    return 'first-frame-warmup';
  }
  return 'steady-state';
}

function slowFrameReason(text) {
  const match = String(text ?? '').match(/\breason=([A-Za-z0-9_.:-]+)/);
  if (match) {
    return match[1];
  }
  if (/submit/i.test(text)) {
    return 'submit';
  }
  if (/work slow/i.test(text)) {
    return 'work';
  }
  if (/render/i.test(text)) {
    return 'render';
  }
  return 'unknown';
}

function actionableRenderSlowFrames(performanceAttribution, rawSlowFrames) {
  const attributed = performanceAttribution?.slowFrames?.frames;
  const frames = Array.isArray(attributed) && attributed.length > 0
    ? attributed
    : (rawSlowFrames ?? []).map((frame, index) => ({
      index,
      phase: 'unknown',
      reason: slowFrameReason(frame?.text ?? ''),
      text: frame?.text ?? '',
      ts: frame?.ts ?? null,
      source: frame?.source ?? null,
    }));
  return frames.filter(isActionableRenderSlowFrame);
}

function isActionableRenderSlowFrame(frame) {
  const phase = frame?.phase ?? 'unknown';
  if (phase === 'first-frame-warmup') {
    return false;
  }
  const metrics = parseSlowFrameMetrics(frame?.text ?? '');
  if (isHostLoopSlowFrame(frame, metrics)) {
    return false;
  }
  return true;
}

function isHostLoopSlowFrame(frame, metrics) {
  const reason = String(frame?.reason ?? slowFrameReason(frame?.text ?? '')).toLowerCase();
  if (reason !== 'loop' && reason !== 'pacing' && reason !== 'cadence') {
    return false;
  }
  return metrics.submitMs === 0
    && metrics.drawBytes === 0
    && metrics.cachedFrames > 0;
}

function parseSlowFrameMetrics(text) {
  return {
    frame: numericTextMetric(text, 'frame'),
    loopMs: numericTextMetric(text, 'loopMs'),
    submitMs: numericTextMetric(text, 'submitMs'),
    drawBytes: numericTextMetric(text, 'drawBytes'),
    cachedFrames: numericTextMetric(text, 'cached'),
  };
}

function numericTextMetric(text, key) {
  const match = String(text ?? '').match(new RegExp(`\\b${key}=([0-9]+(?:\\.[0-9]+)?)`));
  if (!match) {
    return null;
  }
  const value = Number(match[1]);
  return Number.isFinite(value) ? value : null;
}

function countBy(records, key) {
  const counts = new Map();
  for (const record of records ?? []) {
    const value = record?.[key] ?? 'unknown';
    counts.set(value, (counts.get(value) ?? 0) + 1);
  }
  return Object.fromEntries([...counts.entries()].sort(([a], [b]) => String(a).localeCompare(String(b))));
}

function buildPerformanceAttribution({ startupPhases, slowFrames, lifecycle, perf, diagnostics }) {
  const startup = summarizeStartupAttribution(startupPhases);
  const slow = collectSlowFrameAttribution(slowFrames, lifecycle);
  const latestPerf = perf?.latestPerfSummary ?? (perf?.last?.length ? perf.last[perf.last.length - 1] : null);
  const scene = diagnostics?.latestSceneReport ?? null;
  const asset = diagnostics?.latestAssetReport ?? null;
  const render = extractRenderPerf(latestPerf);
  const sceneComplexity = scene ? {
    activeEntities: numericReportValue(scene, 'activeEntities'),
    physicsBodies: numericReportValue(scene, 'physicsBodies'),
    primitiveInstances: numericReportValue(scene, 'primitiveInstances'),
    primitiveVisibleInstances: numericReportValue(scene, 'primitiveVisibleInstances'),
    primitiveChunks: numericReportValue(scene, 'primitiveChunks'),
    primitiveVisibleChunks: numericReportValue(scene, 'primitiveVisibleChunks'),
    primitiveDrawCalls: numericReportValue(scene, 'primitiveDrawCalls'),
    primitiveUploadBytes: numericReportValue(scene, 'primitiveUploadBytes'),
    waterSurfaces: numericReportValue(scene, 'waterSurfaces'),
    visibleWaterSurfaces: numericReportValue(scene, 'visibleWaterSurfaces'),
  } : null;
  const assetComplexity = asset ? {
    mountCount: numericReportValue(asset, 'mountCount'),
    groupCount: numericReportValue(asset, 'groupCount'),
    textureCount: numericReportValue(asset, 'textureCount'),
    linearTextureCount: numericReportValue(asset, 'linearTextureCount'),
    cubemapCount: numericReportValue(asset, 'cubemapCount'),
    modelCount: numericReportValue(asset, 'modelCount'),
    audioCount: numericReportValue(asset, 'audioCount'),
    preparedMapBuilds: numericReportValue(asset, 'preparedMapBuilds'),
    preparedMapReuses: numericReportValue(asset, 'preparedMapReuses'),
    primitiveTemplateBuilds: numericReportValue(asset, 'primitiveTemplateBuilds'),
    primitiveTemplateReuses: numericReportValue(asset, 'primitiveTemplateReuses'),
  } : null;
  const optimizationTarget = startup.slowest[0] ? {
    owner: startup.slowest[0].owner,
    subsystem: startup.slowest[0].subsystem,
    name: startup.slowest[0].name,
    durationMs: startup.slowest[0].durationMs,
  } : null;
  return {
    startup,
    slowFrames: slow,
    render,
    sceneComplexity,
    assetComplexity,
    budgets: {
      startupWarnMs,
      maxSlowFrames,
      restartAdjustedSlowFrameBudget: maxSlowFrames,
      targetFps: 60,
    },
    optimizationTarget,
  };
}

function extractRenderPerf(latestPerf) {
  if (!latestPerf) {
    return null;
  }
  return {
    kind: latestPerf.kind ?? null,
    status: latestPerf.status ?? latestPerf.summary?.status ?? null,
    frameP90Ms: latestPerf.window?.frameP90Ms ?? latestPerf.frameP90Ms ?? null,
    frameP99Ms: latestPerf.window?.frameP99Ms ?? latestPerf.frameP99Ms ?? null,
    renderLoopP90Ms: latestPerf.window?.renderLoopP90Ms ?? latestPerf.renderLoopP90Ms ?? null,
    renderWorkP90Ms: latestPerf.window?.renderWorkP90Ms ?? latestPerf.renderWorkP90Ms ?? null,
    renderSubmitP90Ms: latestPerf.window?.renderSubmitP90Ms ?? latestPerf.renderSubmitP90Ms ?? null,
    gpuWorkDoneP90Ms: latestPerf.window?.gpuWorkDoneP90Ms ?? latestPerf.gpuWorkDoneP90Ms ?? null,
    activeWorkP90Ms: latestPerf.window?.activeWorkP90Ms ?? latestPerf.gate?.activeWorkP90Ms ?? null,
    activeWorkP99Ms: latestPerf.window?.activeWorkP99Ms ?? latestPerf.gate?.activeWorkP99Ms ?? null,
    drawUploadBytes: latestPerf.workload?.uploadBytes ?? latestPerf.current?.uploadBytes ?? latestPerf.current?.drawBytes ?? null,
    currentCause: latestPerf.current?.classification?.primaryCause ?? latestPerf.classification?.primaryCause ?? null,
  };
}

const sceneGrowthKeys = [
  'activeEntities',
  'modelEntities',
  'staticRenderableEntities',
  'physicsBodies',
  'terrainBodies',
  'primitiveLayers',
  'primitiveInstances',
  'primitiveChunks',
  'primitiveGroups',
  'collectibles',
  'checkpoints',
  'boostPads',
  'obstacles',
  'trackEntities',
  'kartWheelPrimitives',
];

const assetGrowthKeys = [
  'mountCount',
  'groupCount',
  'activeGroupDepth',
  'textureCount',
  'linearTextureCount',
  'cubemapCount',
  'fontCount',
  'modelCount',
  'audioCount',
];

function numericReportValue(report, key) {
  const value = Number(report?.[key]);
  return Number.isFinite(value) ? value : 0;
}

function reportGrowth(first, last, keys) {
  const growth = [];
  for (const key of keys) {
    const before = numericReportValue(first, key);
    const after = numericReportValue(last, key);
    if (after > before) {
      growth.push({ key, before, after });
    }
  }
  return growth;
}

function collectWarnings(events) {
  return [
    ...events.console.filter((event) => event.type === 'warning' || event.type === 'warn'),
    ...events.browserLog.filter((event) => event.level === 'warning'),
  ];
}

function collectErrors(events, consoleLines, state) {
  const consoleErrors = events.console.filter((event) => event.type === 'error' || event.type === 'assert');
  const logErrors = events.browserLog.filter((event) => event.level === 'error' && !isIgnoredBrowserLogError(event));
  const stderr = (consoleLines ?? []).filter((line) => line.kind === 'stderr');
  const runnerError = state?.error ? [{ text: state.error, source: 'runner' }] : [];
  return [
    ...consoleErrors.map((event) => ({ source: 'browser-console', text: event.text })),
    ...logErrors.map((event) => ({ source: `browser-log:${event.source}`, text: event.text })),
    ...events.exceptions.map((event) => ({ source: 'exception', text: event.text })),
    ...stderr.map((line) => ({ source: 'studio-console', text: line.text })),
    ...runnerError,
  ];
}

function isIgnoredBrowserLogError(event) {
  return /\/favicon\.ico(?:[?#]|$)/i.test(event?.url ?? '');
}

function isIgnoredResourceFailure(failure) {
  if (failure?.kind === 'loadingFailed' && failure?.canceled === true && failure?.errorText === 'net::ERR_ABORTED') {
    return true;
  }
  return /\/favicon\.ico(?:[?#]|$)/i.test(failure?.url ?? '');
}

function buildIssueList({ firstFrame, canvasAnalysis, errors, warnings, resourceFailures, startupPhases, slowFrames, perfReports, lifecycle, expectedLifecycleState, diagnostics, restartScenario, startRaceScenario, storageReloadScenario, renderStressScenario, resizeScenario, performanceAttribution }) {
  const issues = [];
  const add = (severity, owner, title, evidence) => issues.push({ severity, owner, title, evidence });
  if (!firstFrame.ok) {
    add('P0', 'voplay/studio', 'BlockKart quickplay did not reach first frame', firstFrame.reason ?? 'first frame wait failed');
  }
  if (firstFrame.ok && !firstFrame.skipped && !canvasAnalysis?.nonEmpty) {
    add('P0', 'voplay', 'BlockKart canvas is blank or visually uniform', canvasAnalysis ? JSON.stringify(canvasAnalysis) : 'canvas screenshot unavailable');
  }
  if (errors.length > 0) {
    add('P0', 'voplay/studio', 'Browser or Studio console reported errors', errors.slice(0, 8).map((entry) => `${entry.source}: ${entry.text}`).join('\n'));
  }
  const unexpectedResources = resourceFailures.filter((failure) => !isIgnoredResourceFailure(failure));
  if (unexpectedResources.length > 0) {
    add('P0', 'voplay/studio', 'Resource load failures during BlockKart startup', unexpectedResources.slice(0, 8).map((entry) => `${entry.status ?? entry.errorText} ${entry.url}`).join('\n'));
  }
  if (!firstFrame.skipped && expectedLifecycleState) {
    if (!lifecycle?.state) {
      add('P0', 'BlockKart', 'BlockKart lifecycle state was not reported', `expected final state ${expectedLifecycleState}`);
    } else if (lifecycle.state !== expectedLifecycleState) {
      add('P0', 'BlockKart', 'BlockKart lifecycle reached unexpected final state', `expected=${expectedLifecycleState} actual=${lifecycle.state} phase=${lifecycle.startupPhase ?? 'n/a'} failure=${JSON.stringify(lifecycle.failureReport ?? null)}`);
    }
    if (expectedLifecycleState === 'Running' && !lifecycle?.reachedRunning) {
      add('P0', 'BlockKart', 'BlockKart lifecycle did not reach Running', JSON.stringify(lifecycle?.events ?? []));
    }
    if (expectedLifecycleState === 'Failed' && !lifecycle?.failureReport) {
      add('P0', 'BlockKart', 'BlockKart failure path did not emit a structured failure report', JSON.stringify(lifecycle?.events ?? []));
    }
  }
  const opened = startupPhases.find((phase) => phase.text.startsWith('Opened GUI '));
  if (opened?.durationMs != null && opened.durationMs > startupWarnMs) {
    add('P1', 'voplay/studio', 'BlockKart startup phase is slower than baseline threshold', `${opened.text}; threshold=${startupWarnMs}ms`);
  }
  const slowFrameBudget = adjustedSlowFrameBudget(restartScenario, startRaceScenario, storageReloadScenario);
  const actionableSlowFrames = actionableRenderSlowFrames(performanceAttribution, slowFrames);
  if (actionableSlowFrames.length > slowFrameBudget) {
    add('P1', 'voplay', 'voplay render slow frames persisted during baseline capture', `actionable=${actionableSlowFrames.length} total=${slowFrames.length} budget=${slowFrameBudget}\n${actionableSlowFrames.slice(0, 8).map((entry) => entry.text).join('\n')}`);
  }
  if (!firstFrame.skipped && expectedLifecycleState !== 'Failed' && perfReports.length === 0) {
    add('P1', 'voplay', 'voplay perf reports were not captured under trace mode', 'globalThis.__voplayPerfReports was empty after capture window');
  }
  if (!firstFrame.skipped && expectedLifecycleState === 'Running') {
    if ((diagnostics?.assetReports?.length ?? 0) === 0) {
      add('P1', 'BlockKart', 'BlockKart asset diagnostics were not reported', 'expected at least one __BLOCKKART_ASSET_REPORT__ marker');
    }
    if ((diagnostics?.sceneReports?.length ?? 0) === 0) {
      add('P1', 'BlockKart', 'BlockKart scene diagnostics were not reported', 'expected at least one __BLOCKKART_SCENE_REPORT__ marker');
    }
    if ((diagnostics?.vehicleReports?.length ?? 0) === 0) {
      add('P1', 'BlockKart', 'BlockKart vehicle telemetry was not reported', 'expected at least one __BLOCKKART_VEHICLE_REPORT__ marker');
    }
    if ((diagnostics?.raceReports?.length ?? 0) === 0) {
      add('P1', 'BlockKart', 'BlockKart race telemetry was not reported', 'expected at least one __BLOCKKART_RACE_REPORT__ marker');
    }
    const latestRace = diagnostics?.latestRaceReport ?? null;
    if (latestRace && latestRace.settingsStorageStatus !== 'persistent') {
      add('P1', 'BlockKart', 'BlockKart settings persistence was not active', `status=${latestRace.settingsStorageStatus ?? 'n/a'} error=${latestRace.settingsStorageError ?? ''}`);
    }
    const critical = performanceAttribution?.startup?.critical ?? {};
    if (!critical.preparePrimitiveMapAsset) {
      add('P1', 'BlockKart', 'BlockKart map asset preparation was not separately attributable', 'missing startup phase preparePrimitiveMapAsset');
    }
    if (!critical.buildPrimitiveTrackVisuals) {
      add('P1', 'BlockKart', 'BlockKart primitive track visuals were not separately attributable', 'missing startup phase buildPrimitiveTrackVisuals');
    }
  }
  if (!firstFrame.skipped && startRaceScenario?.requested && !startRaceScenario.completed) {
    add('P0', 'BlockKart', 'BlockKart start flow did not enter Running race state', startRaceScenario.failure ?? JSON.stringify(startRaceScenario));
  }
  if (!firstFrame.skipped && storageReloadScenario?.requested && !storageReloadScenario.completed) {
    add('P0', 'BlockKart/Studio', 'BlockKart settings did not survive a reload', storageReloadScenario.failure ?? JSON.stringify(storageReloadScenario));
  }
  if (!firstFrame.skipped && renderStressScenario?.requested && !renderStressScenario.completed) {
    add('P0', 'BlockKart', 'BlockKart render stress profile did not apply', renderStressScenario.failure ?? JSON.stringify(renderStressScenario));
  }
  if (!firstFrame.skipped && resizeScenario?.requested && !resizeScenario.completed) {
    add('P1', 'voplay/studio', 'BlockKart resize cycle did not complete', resizeScenario.failure ?? JSON.stringify(resizeScenario));
  }
  if (!firstFrame.skipped && restartScenario?.requested > 0) {
    if (restartScenario.skipped) {
      add('P1', 'BlockKart', 'BlockKart restart diagnostic mode was skipped', restartScenario.skipReason ?? 'restart scenario skipped');
    } else if (restartScenario.completed < restartScenario.requested) {
      add('P0', 'BlockKart', 'BlockKart restart did not reliably return to Running', restartScenario.failure ?? JSON.stringify(restartScenario.iterations));
    }
    const sceneReports = diagnostics?.restartSceneReports ?? [];
    const assetReports = diagnostics?.restartAssetReports ?? [];
    if (sceneReports.length < 2) {
      add('P1', 'BlockKart', 'BlockKart restart mode had insufficient scene diagnostics', `restart scene reports=${sceneReports.length}`);
    } else {
      const growth = reportGrowth(sceneReports[0], sceneReports[sceneReports.length - 1], sceneGrowthKeys);
      if (growth.length > 0) {
        add('P0', 'BlockKart', 'BlockKart scene counts grew after restart', JSON.stringify(growth));
      }
    }
    if (assetReports.length < 2) {
      add('P1', 'BlockKart', 'BlockKart restart mode had insufficient asset diagnostics', `restart asset reports=${assetReports.length}`);
    } else {
      const growth = reportGrowth(assetReports[0], assetReports[assetReports.length - 1], assetGrowthKeys);
      if (growth.length > 0) {
        add('P0', 'voplay/BlockKart', 'BlockKart asset counts grew after restart', JSON.stringify(growth));
      }
      const latestAsset = assetReports[assetReports.length - 1];
      const maxMountCount = Math.max(...assetReports.map((report) => numericReportValue(report, 'mountCount')));
      if (maxMountCount > 1) {
        add('P0', 'voplay/Asset', 'BlockKart restart mounted duplicate asset packs', `max mountCount=${maxMountCount}`);
      }
      if (!latestAsset.preparedMapCached || !latestAsset.primitiveTemplateCached) {
        add('P1', 'BlockKart/Scene', 'BlockKart restart did not retain immutable map and primitive caches', JSON.stringify({
          preparedMapCached: latestAsset.preparedMapCached,
          primitiveTemplateCached: latestAsset.primitiveTemplateCached,
        }));
      }
      if (numericReportValue(latestAsset, 'preparedMapBuilds') > 1 || numericReportValue(latestAsset, 'primitiveTemplateBuilds') > 1) {
        add('P1', 'BlockKart/Scene', 'BlockKart restart rebuilt immutable cached resources', JSON.stringify({
          preparedMapBuilds: latestAsset.preparedMapBuilds,
          primitiveTemplateBuilds: latestAsset.primitiveTemplateBuilds,
        }));
      }
      if (restartScenario.completed > 0 && latestAsset.phase === 'worldReady' && numericReportValue(latestAsset, 'preparedMapReuses') < restartScenario.completed) {
        add('P1', 'BlockKart/Map', 'BlockKart restart did not report prepared map reuse for every completed restart', `preparedMapReuses=${latestAsset.preparedMapReuses} completed=${restartScenario.completed}`);
      }
      if (restartScenario.completed > 0 && latestAsset.phase === 'worldReady' && numericReportValue(latestAsset, 'primitiveTemplateReuses') < restartScenario.completed) {
        add('P1', 'BlockKart/Scene', 'BlockKart restart did not report primitive template reuse for every completed restart', `primitiveTemplateReuses=${latestAsset.primitiveTemplateReuses} completed=${restartScenario.completed}`);
      }
    }
  }
  if (warnings.length > 0 && errors.length === 0) {
    add('info', 'voplay/studio', 'Warnings were observed and retained for triage', warnings.slice(0, 8).map((entry) => entry.text).join('\n'));
  }
  return issues;
}

function adjustedSlowFrameBudget(restartScenario, startRaceScenario, storageReloadScenario) {
  let budget = maxSlowFrames;
  if (restartScenario?.requested > 0) {
    budget += restartScenario.completed > 0 ? 2 : 0;
  }
  if (startRaceScenario?.requested && !startRaceScenario.skipped) {
    budget += 3;
  }
  if (storageReloadScenario?.requested && !storageReloadScenario.skipped) {
    budget += 2;
  }
  return budget;
}

function normalizePerfReports(debugReports, endpointReports) {
  const reports = [];
  for (const report of debugReports ?? []) {
    reports.push(report);
  }
  for (const record of endpointReports ?? []) {
    reports.push(record?.payload ?? record);
  }
  return reports;
}

function summarizePerfReports(perfReports) {
  const byKind = new Map();
  let latestPerfSummary = null;
  for (const report of perfReports ?? []) {
    const kind = report?.kind ?? 'unknown';
    byKind.set(kind, (byKind.get(kind) ?? 0) + 1);
    if (kind === 'perf-summary') {
      latestPerfSummary = report;
    }
  }
  return {
    count: perfReports?.length ?? 0,
    byKind: Object.fromEntries([...byKind.entries()].sort(([a], [b]) => String(a).localeCompare(String(b)))),
    last: perfReports?.slice?.(-5) ?? [],
    latestPerfSummary,
  };
}

function markdownReport(report) {
  const lines = [];
  lines.push('# BlockKart Baseline');
  lines.push('');
  lines.push(`- Status: ${report.status}`);
  lines.push(`- URL: ${report.url}`);
  lines.push(`- Viewport: ${report.viewport.width}x${report.viewport.height}`);
  lines.push(`- Project: ${report.project.module}@${report.project.commit}`);
  lines.push(`- Screenshot: ${path.relative(root, report.artifacts.viewportScreenshot)}`);
  lines.push(`- Canvas crop: ${path.relative(root, report.artifacts.canvasScreenshot)}`);
  lines.push('');
  lines.push('## Smoke And Visual');
  lines.push('');
  lines.push(`- First frame: ${report.firstFrame.ok ? 'ok' : 'failed'}${report.firstFrame.skipped ? ' (skipped)' : ''}`);
  lines.push(`- Canvas: ${report.finalState.canvasWidth}x${report.finalState.canvasHeight}`);
  lines.push(`- Canvas non-empty: ${report.visual.canvas.nonEmpty}`);
  lines.push(`- Canvas luma stddev: ${report.visual.canvas.lumaStdDev}`);
  lines.push(`- Canvas sampled colors: ${report.visual.canvas.uniqueSampledColors}`);
  lines.push(`- Capture modes: viewport=${report.visual.capture.viewportMode}, canvas=${report.visual.capture.canvasMode}`);
  for (const warning of report.visual.capture.warnings) {
    lines.push(`- Capture warning: ${warning}`);
  }
  lines.push('');
  lines.push('## Startup Phases');
  lines.push('');
  if (report.startupPhases.length === 0) {
    lines.push('- No startup console phases were captured.');
  } else {
    for (const phase of report.startupPhases) {
      const duration = phase.durationMs == null ? '' : ` (${Math.round(phase.durationMs)}ms)`;
      lines.push(`- [${phase.kind}] ${phase.text}${duration}`);
    }
  }
  lines.push('');
  lines.push('## Performance Attribution');
  lines.push('');
  const attribution = report.performanceAttribution;
  if (!attribution) {
    lines.push('- Performance attribution was not built.');
  } else {
    lines.push(`- Startup budget: ${attribution.budgets.startupWarnMs}ms`);
    lines.push(`- Slow-frame budget: ${attribution.budgets.restartAdjustedSlowFrameBudget}`);
    if (attribution.optimizationTarget) {
      const target = attribution.optimizationTarget;
      lines.push(`- Current optimization target: ${target.owner}/${target.subsystem} ${target.name} ${Math.round(target.durationMs)}ms`);
    }
    const critical = attribution.startup.critical ?? {};
    for (const name of ['preparePrimitiveMapAsset', 'buildPrimitiveTrackVisuals']) {
      const phase = critical[name];
      lines.push(`- ${name}: ${phase ? `${Math.round(phase.durationMs)}ms owner=${phase.owner} subsystem=${phase.subsystem}` : 'not captured'}`);
    }
    if (attribution.startup.slowest.length > 0) {
      lines.push('- Slowest startup phases:');
      for (const phase of attribution.startup.slowest.slice(0, 8)) {
        lines.push(`  - ${Math.round(phase.durationMs)}ms ${phase.owner}/${phase.subsystem} ${phase.label}:${phase.name}`);
      }
    }
    lines.push(`- Slow frames: first-frame warmup=${attribution.slowFrames.firstFrameWarmup} restart rebuild=${attribution.slowFrames.restartRebuild} steady-state=${attribution.slowFrames.steadyState}`);
    lines.push(`- Slow-frame reasons: ${JSON.stringify(attribution.slowFrames.byReason)}`);
    if (attribution.render) {
      lines.push(`- Render perf: kind=${attribution.render.kind ?? 'n/a'} frameP90=${attribution.render.frameP90Ms ?? 'n/a'} renderP90=${attribution.render.renderLoopP90Ms ?? 'n/a'} renderWorkP90=${attribution.render.renderWorkP90Ms ?? 'n/a'} activeP90/P99=${attribution.render.activeWorkP90Ms ?? 'n/a'}/${attribution.render.activeWorkP99Ms ?? 'n/a'} submitP90=${attribution.render.renderSubmitP90Ms ?? 'n/a'} gpuP90=${attribution.render.gpuWorkDoneP90Ms ?? 'n/a'} upload=${attribution.render.drawUploadBytes ?? 'n/a'} cause=${attribution.render.currentCause ?? 'n/a'}`);
    }
    if (attribution.sceneComplexity) {
      const scene = attribution.sceneComplexity;
      lines.push(`- Scene complexity: entities=${scene.activeEntities} physics=${scene.physicsBodies} primitives=${scene.primitiveInstances} chunks=${scene.primitiveChunks} water=${scene.waterSurfaces}/${scene.visibleWaterSurfaces} drawCalls=${scene.primitiveDrawCalls} upload=${scene.primitiveUploadBytes}`);
    }
    if (attribution.assetComplexity) {
      const asset = attribution.assetComplexity;
      lines.push(`- Asset complexity: mounts=${asset.mountCount} groups=${asset.groupCount} textures=${asset.textureCount}/${asset.linearTextureCount} cubemaps=${asset.cubemapCount} models=${asset.modelCount} audio=${asset.audioCount}`);
      lines.push(`- Restart cache: preparedMap builds/reuses=${asset.preparedMapBuilds}/${asset.preparedMapReuses} primitiveTemplate builds/reuses=${asset.primitiveTemplateBuilds}/${asset.primitiveTemplateReuses}`);
    }
  }
  lines.push('');
  lines.push('## Lifecycle');
  lines.push('');
  lines.push(`- Expected final state: ${report.lifecycle.expectedState}`);
  lines.push(`- Final state: ${report.lifecycle.state ?? 'not reported'}`);
  lines.push(`- Startup phase: ${report.lifecycle.startupPhase ?? 'not reported'}`);
  lines.push(`- Reached Running: ${report.lifecycle.reachedRunning}`);
  lines.push(`- Lifecycle events: ${report.lifecycle.events.length}`);
  if (report.lifecycle.failureReport) {
    const failure = report.lifecycle.failureReport;
    lines.push(`- Failure report: ${failure.owner}/${failure.subsystem} phase=${failure.startupPhase} player=${oneLine(failure.playerMessage)}`);
    lines.push(`- Developer message: ${oneLine(failure.developerMessage)}`);
  }
  if (report.simulation) {
    lines.push(`- Simulation: ${report.simulation.kind} (${report.simulation.mutation})`);
  }
  lines.push('');
  lines.push('## Asset And Scene');
  lines.push('');
  lines.push(`- Asset reports: ${report.diagnostics.assetReports.length}`);
  lines.push(`- Scene reports: ${report.diagnostics.sceneReports.length}`);
  lines.push(`- Vehicle reports: ${report.diagnostics.vehicleReports.length}`);
  lines.push(`- Race reports: ${report.diagnostics.raceReports.length}`);
  if (report.diagnostics.latestAssetReport) {
    const asset = report.diagnostics.latestAssetReport;
    lines.push(`- Latest asset report: phase=${asset.phase} mounts=${asset.mountCount ?? 'n/a'} group=${asset.groupCount} textures=${asset.textureCount}/${asset.linearTextureCount ?? 'n/a'} cubemaps=${asset.cubemapCount} models=${asset.modelCount} audio=${asset.audioCount}`);
    lines.push(`- Latest cache report: runtime=${asset.runtimeCacheReady === true} preparedMap=${asset.preparedMapCached === true} primitiveTemplate=${asset.primitiveTemplateCached === true} preparedMap builds/reuses=${asset.preparedMapBuilds ?? 'n/a'}/${asset.preparedMapReuses ?? 'n/a'} primitiveTemplate builds/reuses=${asset.primitiveTemplateBuilds ?? 'n/a'}/${asset.primitiveTemplateReuses ?? 'n/a'}`);
    if (asset.restartRootCause) {
      lines.push(`- Restart root cause: ${oneLine(asset.restartRootCause)}`);
    }
  }
  if (report.diagnostics.latestSceneReport) {
    const scene = report.diagnostics.latestSceneReport;
    lines.push(`- Latest scene report: phase=${scene.phase} entities=${scene.activeEntities} physics=${scene.physicsBodies} primitives=${scene.primitiveInstances} chunks=${scene.primitiveChunks} water=${scene.waterSurfaces ?? 'n/a'}/${scene.visibleWaterSurfaces ?? 'n/a'}`);
  }
  if (report.diagnostics.latestVehicleReport) {
    const vehicle = report.diagnostics.latestVehicleReport;
    lines.push(`- Latest vehicle report: phase=${vehicle.phase} preset=${vehicle.tuningPreset} surface=${vehicle.surfaceKind}/${vehicle.surfaceID} speed=${vehicle.speed} input=${vehicle.throttle}/${vehicle.steer}`);
  }
  if (report.diagnostics.latestRaceReport) {
    const race = report.diagnostics.latestRaceReport;
    lines.push(`- Latest race report: phase=${race.phase} state=${race.raceState} lap=${race.lap}/${race.totalLaps} time=${race.time} best=${race.bestTimeSet ? race.bestTime : 'unset'} settings=${race.settingsStorageStatus} loaded=${race.settingsLoadedFromStorage === true}${race.settingsStorageError ? ` error=${oneLine(race.settingsStorageError)}` : ''}`);
  }
  lines.push(`- Start-race mode: requested=${report.startRace.requested} completed=${report.startRace.completed}${report.startRace.skipped ? ` skipped=${oneLine(report.startRace.skipReason)}` : ''}`);
  lines.push(`- Storage reload mode: requested=${report.storageReload.requested} completed=${report.storageReload.completed}${report.storageReload.skipped ? ` skipped=${oneLine(report.storageReload.skipReason)}` : ''}`);
  lines.push(`- Render stress mode: requested=${report.renderStress.requested} profile=${report.renderStress.profile || 'none'} completed=${report.renderStress.completed}${report.renderStress.skipped ? ` skipped=${oneLine(report.renderStress.skipReason)}` : ''}`);
  lines.push(`- Resize cycle mode: requested=${report.resize.requested} completed=${report.resize.completed}${report.resize.skipped ? ` skipped=${oneLine(report.resize.skipReason)}` : ''}`);
  lines.push(`- Restart mode: requested=${report.restart.requested} completed=${report.restart.completed}${report.restart.skipped ? ` skipped=${oneLine(report.restart.skipReason)}` : ''}`);
  lines.push('');
  lines.push('## Perf And Diagnostics');
  lines.push('');
  lines.push(`- voplay perf reports: ${report.perf.count}`);
  lines.push(`- voplay perf endpoint reports: ${report.perfEndpoint?.count ?? 0}`);
  if (report.perfEndpoint?.error) {
    lines.push(`- voplay perf endpoint error: ${report.perfEndpoint.error}`);
  }
  lines.push(`- voplay perf report kinds: ${JSON.stringify(report.perf.byKind)}`);
  lines.push(`- voplay render slow frames: ${report.voplaySlowFrames.length}`);
  lines.push(`- warnings: ${report.warnings.length}`);
  lines.push(`- errors: ${report.errors.length}`);
  lines.push(`- resource failures: ${report.resourceFailures.length}`);
  if (report.ignoredResourceFailures?.length > 0) {
    lines.push(`- ignored resource failures: ${report.ignoredResourceFailures.length}`);
  }
  lines.push('');
  lines.push('## P0/P1 Triage');
  lines.push('');
  const triage = report.issues.filter((issue) => issue.severity === 'P0' || issue.severity === 'P1');
  if (triage.length === 0) {
    lines.push('- No P0/P1 issues found in this baseline run.');
  } else {
    for (const issue of triage) {
      lines.push(`- ${issue.severity} [${issue.owner}] ${issue.title}: ${oneLine(issue.evidence)}`);
    }
  }
  const infos = report.issues.filter((issue) => issue.severity === 'info');
  if (infos.length > 0) {
    lines.push('');
    lines.push('## Informational');
    lines.push('');
    for (const issue of infos) {
      lines.push(`- [${issue.owner}] ${issue.title}: ${oneLine(issue.evidence)}`);
    }
  }
  lines.push('');
  lines.push('## Remaining Risks');
  lines.push('');
  for (const risk of report.remainingRisks) {
    lines.push(`- ${risk}`);
  }
  lines.push('');
  return `${lines.join('\n')}\n`;
}

function oneLine(value) {
  return String(value ?? '').replace(/\s+/g, ' ').slice(0, 500);
}

async function main() {
  if (!existsSync(studioDistIndex)) {
    fail('apps/studio/dist is missing; run the studio-build task first');
  }
  mkdirSync(outDir, { recursive: true });
  syncDistQuickplayPackage();
  const projectPackage = readJson(path.join(quickplayDir, 'project.json'));
  const depsPackage = readJson(path.join(quickplayDir, 'deps.json'));
  const provenance = readJson(path.join(quickplayDir, 'provenance.json'));
  const simulation = installSimulatedFailure(simulatedFailure);

  const previewPort = await reservePort();
  const debugPort = await reservePort();
  const baseUrl = `http://127.0.0.1:${previewPort}/`;
  const preview = startPreviewServer(previewPort);
  let browser = null;
  let client = null;
  let targetId = null;
  try {
    await fetchOk(baseUrl).catch((error) => {
      fail(`vite preview did not start: ${error.message}\n${preview.log()}`);
    });

    browser = await startBrowser(debugPort);
    ({ targetId, client } = await openPage(debugPort));
    cleanupCallbacks.push(() => client.close());
    cleanupCallbacks.push(() => {
      void closePage(debugPort, targetId);
    });

    await client.send('Page.enable');
    await client.send('Runtime.enable');
    await client.send('Log.enable');
    await client.send('Network.enable');
    await client.send('Emulation.setDeviceMetricsOverride', {
      width: viewportWidth,
      height: viewportHeight,
      deviceScaleFactor: 1,
      mobile: false,
      screenWidth: viewportWidth,
      screenHeight: viewportHeight,
    });
    const events = installCollectors(client);

    const quickplayUrl = new URL('/', baseUrl);
    quickplayUrl.searchParams.set('proj', 'vo:quickplay:blockkart');
    quickplayUrl.searchParams.set('mode', 'runner');
    quickplayUrl.searchParams.set('studioBrowserSmokeDebug', '1');
    if (perfMode) {
      quickplayUrl.searchParams.set('voplayPerf', perfMode);
    }
    if (perfConsole) {
      quickplayUrl.searchParams.set('voplayPerfConsole', '1');
    }
    if (perfDiag) {
      quickplayUrl.searchParams.set('voplayPerfDiag', perfDiag);
    }
    if (pulseMode) {
      quickplayUrl.searchParams.set('voplayPulseMode', pulseMode);
    }
    if (perfGpuProbe) {
      quickplayUrl.searchParams.set('voplayPerfGpu', '1');
    }
    quickplayUrl.searchParams.set('voplayRendererPerf', '1');
    if (renderStressProfile) {
      quickplayUrl.searchParams.set('blockkartRenderStress', renderStressProfile);
    }
    if (resizeCycleRequested) {
      quickplayUrl.searchParams.set('blockkartResizeCycle', '1');
    }
    quickplayUrl.hash = '#/runner';

    const startedAt = new Date().toISOString();
    const monotonicStartMs = Date.now();
    await navigate(client, quickplayUrl.toString());
    const hookState = await waitForPredicate(
      client,
      'BlockKart debug hook',
      studioBrowserSmokeHookReadyExpression(),
      (state) => state.ready,
      firstFrameTimeoutMs,
    );
    const firstFrame = await waitForQuickplayFirstFrame(client, firstFrameTimeoutMs);
    let renderStressScenario = {
      requested: renderStressProfile !== '',
      profile: renderStressProfile,
      completed: false,
      skipped: renderStressProfile === '',
      skipReason: renderStressProfile === '' ? 'render stress mode disabled' : null,
      initialReports: 0,
      finalReports: 0,
      latestSceneReport: collectBlockKartDiagnostics(events.console, []).latestSceneReport,
    };
    if (renderStressProfile !== '') {
      if (!firstFrame.ok || firstFrame.skipped) {
        renderStressScenario.skipped = true;
        renderStressScenario.skipReason = firstFrame.reason ?? 'first frame did not complete';
      } else if (expectedLifecycleState !== 'Running') {
        renderStressScenario.skipped = true;
        renderStressScenario.skipReason = `render stress mode requires expected lifecycle Running, got ${expectedLifecycleState}`;
      } else {
        renderStressScenario = await runRenderStressScenario(client, events, renderStressProfile, restartWaitTimeoutMs);
      }
    }
    let resizeScenario = {
      requested: resizeCycleRequested,
      completed: false,
      skipped: !resizeCycleRequested,
      skipReason: resizeCycleRequested ? null : 'resize cycle disabled',
      steps: [],
    };
    if (resizeCycleRequested) {
      if (!firstFrame.ok || firstFrame.skipped) {
        resizeScenario.skipped = true;
        resizeScenario.skipReason = firstFrame.reason ?? 'first frame did not complete';
      } else if (expectedLifecycleState !== 'Running') {
        resizeScenario.skipped = true;
        resizeScenario.skipReason = `resize cycle requires expected lifecycle Running, got ${expectedLifecycleState}`;
      } else {
        resizeScenario = await runResizeCycleScenario(client, true);
      }
    }
    let startRaceScenario = {
      requested: startRaceRequested,
      completed: false,
      skipped: !startRaceRequested,
      skipReason: startRaceRequested ? null : 'start-race mode disabled',
      initialRunningRaceReports: countRaceState(events, 'Running'),
      finalRunningRaceReports: countRaceState(events, 'Running'),
    };
    if (startRaceRequested) {
      if (!firstFrame.ok || firstFrame.skipped) {
        startRaceScenario.skipped = true;
        startRaceScenario.skipReason = firstFrame.reason ?? 'first frame did not complete';
      } else if (expectedLifecycleState !== 'Running') {
        startRaceScenario.skipped = true;
        startRaceScenario.skipReason = `start-race mode requires expected lifecycle Running, got ${expectedLifecycleState}`;
      } else {
        startRaceScenario = await runStartRaceScenario(client, events, true, restartWaitTimeoutMs);
      }
    }
    let restartScenario = {
      requested: restartCount,
      completed: 0,
      skipped: restartCount === 0,
      skipReason: restartCount === 0 ? 'restart mode disabled' : null,
      initialRunningEvents: countLifecycleState(events, 'Running'),
      finalRunningEvents: countLifecycleState(events, 'Running'),
      iterations: [],
    };
    if (restartCount > 0) {
      if (!firstFrame.ok || firstFrame.skipped) {
        restartScenario.skipped = true;
        restartScenario.skipReason = firstFrame.reason ?? 'first frame did not complete';
      } else if (expectedLifecycleState !== 'Running') {
        restartScenario.skipped = true;
        restartScenario.skipReason = `restart mode requires expected lifecycle Running, got ${expectedLifecycleState}`;
      } else {
        restartScenario = await runRestartScenario(client, events, restartCount, restartWaitTimeoutMs);
      }
    }
    let storageReloadScenario = {
      requested: verifyStorageReload,
      completed: false,
      skipped: !verifyStorageReload,
      skipReason: verifyStorageReload ? null : 'storage reload mode disabled',
      initialRunningEvents: countLifecycleState(events, 'Running'),
      finalRunningEvents: countLifecycleState(events, 'Running'),
      initialLoadedReports: countRaceReportsLoadedFromStorage(events),
      finalLoadedReports: countRaceReportsLoadedFromStorage(events),
      latestRaceReport: collectBlockKartDiagnostics(events.console, []).latestRaceReport,
    };
    if (verifyStorageReload) {
      if (!firstFrame.ok || firstFrame.skipped) {
        storageReloadScenario.skipped = true;
        storageReloadScenario.skipReason = firstFrame.reason ?? 'first frame did not complete';
      } else if (expectedLifecycleState !== 'Running') {
        storageReloadScenario.skipped = true;
        storageReloadScenario.skipReason = `storage reload mode requires expected lifecycle Running, got ${expectedLifecycleState}`;
      } else {
        storageReloadScenario = await runStorageReloadScenario(client, events, quickplayUrl, true, restartWaitTimeoutMs);
      }
    }
    if (firstFrame.ok && !firstFrame.skipped && captureMs > 0) {
      await sleep(captureMs);
    }
    const finalState = await client.evaluate(quickplayStateExpression(), 30000).catch(() => firstFrame.state ?? {});
    const debugSnapshot = await client.evaluate(debugSnapshotExpression(), 15000).catch(() => ({
      entryPath: hookState.entryPath,
      consoleLines: [],
      runtimeState: null,
      perfReports: [],
    }));
    await sleep(250);
    const perfEndpoint = await fetchVoplayPerfEndpoint(baseUrl).catch((error) => ({
      count: 0,
      reports: [],
      error: error instanceof Error ? error.message : String(error),
    }));
    const perfReports = normalizePerfReports(debugSnapshot.perfReports, perfEndpoint.reports);

    const viewportScreenshot = path.join(outDir, 'blockkart-baseline-viewport.png');
    const canvasScreenshot = path.join(outDir, 'blockkart-baseline-canvas.png');
    rmSync(canvasScreenshot, { force: true });
    const captureWarnings = [];
    let viewportAnalysis = null;
    let viewportCapture = null;
    let viewportCaptureMode = 'browser-cli';
    try {
      viewportCapture = await captureViewportBrowserCli(quickplayUrl.toString(), viewportScreenshot, browser?.browserBin);
      viewportAnalysis = viewportCapture.analysis;
    } catch (error) {
      viewportCaptureMode = 'screencast';
      captureWarnings.push(`viewport browser CLI screenshot failed: ${error instanceof Error ? error.message : String(error)}`);
      try {
        viewportCapture = await captureViewportScreencast(client, viewportScreenshot);
        viewportAnalysis = viewportCapture.analysis;
      } catch (screencastError) {
        viewportCaptureMode = 'failed';
        captureWarnings.push(`viewport screencast failed: ${screencastError instanceof Error ? screencastError.message : String(screencastError)}`);
      }
    }
    let canvasAnalysis = null;
    let canvasCaptureMode = 'viewport-crop';
    if (viewportCapture && finalState.canvasRect && finalState.canvasRect.width > 0 && finalState.canvasRect.height > 0) {
      try {
        const canvasBytes = cropPng(viewportCapture.bytes, finalState.canvasRect, viewportWidth, viewportHeight);
        writeFileSync(canvasScreenshot, canvasBytes);
        canvasAnalysis = analyzePng(canvasBytes);
      } catch (error) {
        canvasCaptureMode = 'failed';
        captureWarnings.push(`canvas viewport crop failed: ${error instanceof Error ? error.message : String(error)}`);
      }
    } else if (finalState.canvasRect && finalState.canvasRect.width > 0 && finalState.canvasRect.height > 0) {
      try {
        canvasAnalysis = await captureScreenshot(client, canvasScreenshot, finalState.canvasRect);
        canvasCaptureMode = 'cdp-canvas-crop';
      } catch (error) {
        canvasCaptureMode = 'failed';
        captureWarnings.push(`canvas CDP crop failed: ${error instanceof Error ? error.message : String(error)}`);
      }
    } else if (!viewportCapture) {
      canvasCaptureMode = 'not-captured';
    } else {
      canvasCaptureMode = 'not-found';
      captureWarnings.push('canvas crop skipped because no canvas rect was available');
    }
    if (!canvasAnalysis?.nonEmpty) {
      try {
        const dataUrlAnalysis = await captureCanvasDataUrl(client, canvasScreenshot);
        canvasAnalysis = dataUrlAnalysis;
        canvasCaptureMode = 'canvas-data-url';
      } catch (error) {
        captureWarnings.push(`canvas data-url capture failed: ${error instanceof Error ? error.message : String(error)}`);
      }
    }
    if (!canvasAnalysis?.nonEmpty) {
      try {
        const freshCapture = await captureFreshBrowserCanvas(quickplayUrl.toString(), viewportScreenshot, canvasScreenshot, browser?.browserBin);
        if (!viewportAnalysis?.nonEmpty) {
          viewportAnalysis = freshCapture.viewportAnalysis;
          viewportCapture = { bytes: readFileSync(viewportScreenshot), analysis: viewportAnalysis };
          viewportCaptureMode = freshCapture.viewportMode;
        }
        canvasAnalysis = freshCapture.canvasAnalysis;
        canvasCaptureMode = freshCapture.canvasMode;
      } catch (error) {
        captureWarnings.push(`fresh browser canvas capture failed: ${error instanceof Error ? error.message : String(error)}`);
      }
    }

    const startupPhases = classifyStartupPhases([
      ...(debugSnapshot.consoleLines ?? []),
      ...events.console.map((event) => ({
        kind: `browser-${event.type}`,
        text: event.text,
        ts: event.timestamp,
      })),
    ]);
    const voplaySlowFrames = collectVoplaySlowFrames(events.console, debugSnapshot.consoleLines);
    const lifecycle = {
      expectedState: expectedLifecycleState,
      ...collectBlockKartLifecycle(events.console, debugSnapshot.consoleLines),
    };
    const diagnostics = collectBlockKartDiagnostics(events.console, debugSnapshot.consoleLines);
    const warnings = collectWarnings(events);
    const errors = collectErrors(events, debugSnapshot.consoleLines, finalState);
    const resourceFailures = events.resourceFailures.filter((failure) => !isIgnoredResourceFailure(failure));
    const ignoredResourceFailures = events.resourceFailures.filter(isIgnoredResourceFailure);
    const perf = summarizePerfReports(perfReports);
    const performanceAttribution = buildPerformanceAttribution({
      startupPhases,
      slowFrames: voplaySlowFrames,
      lifecycle,
      perf,
      diagnostics,
    });
    performanceAttribution.budgets.restartAdjustedSlowFrameBudget = adjustedSlowFrameBudget(restartScenario, startRaceScenario, storageReloadScenario);
    const status = firstFrame.skipped ? 'skipped' : 'ok';
    const issues = buildIssueList({
      firstFrame,
      canvasAnalysis,
      errors,
      warnings,
      resourceFailures,
      startupPhases,
      slowFrames: voplaySlowFrames,
      perfReports,
      lifecycle,
      expectedLifecycleState,
      diagnostics,
      restartScenario,
      startRaceScenario,
      storageReloadScenario,
      renderStressScenario,
      resizeScenario,
      performanceAttribution,
    });
    const p0p1 = issues.filter((issue) => issue.severity === 'P0' || issue.severity === 'P1');
    const generatedAt = new Date().toISOString();
    const freshEvidence = sourceBoundEvidence({
      gate: baselineGateName(),
      generatedAt,
      root,
      repos: [
        { name: 'volang', root },
        { name: 'voplay', root: voplayRoot },
        { name: 'BlockKart', root: blockKartRoot },
      ],
      gateFiles: [
        'scripts/ci/blockkart_baseline.mjs',
        'scripts/ci/quickplay_validate.mjs',
        'scripts/ci/repo_roots.mjs',
        'scripts/ci/source_bound_evidence.mjs',
        'apps/studio/package.json',
        'apps/studio/vite.config.ts',
        'eng/tasks.toml',
        'eng/ci.toml',
      ],
      artifacts: [
        quickplayDir,
        studioDistIndex,
        viewportScreenshot,
        canvasScreenshot,
      ],
    });
    const report = {
      schemaVersion: 1,
      generatedAt,
      freshEvidence,
      startedAt,
      durationMs: Date.now() - monotonicStartMs,
      status: p0p1.length > 0 ? 'failed' : status,
      url: quickplayUrl.toString(),
      viewport: { width: viewportWidth, height: viewportHeight },
      project: {
        name: projectPackage.name,
        module: projectPackage.module,
        commit: projectPackage.commit,
        provenanceCommit: provenance.project?.commit ?? null,
      },
      dependencies: (depsPackage.modules ?? []).map((modulePack) => ({
        module: modulePack.module,
        version: modulePack.version,
        commit: modulePack.commit ?? null,
        provenanceCommit: (provenance.dependencies ?? []).find((dep) => dep.module === modulePack.module)?.commit ?? null,
        artifacts: (modulePack.artifacts ?? []).map((artifact) => artifact.url),
      })),
      artifacts: {
        directory: outDir,
        viewportScreenshot,
        canvasScreenshot,
      },
      hookState,
      firstFrame,
      finalState,
      runtimeState: debugSnapshot.runtimeState,
      startupPhases,
      lifecycle,
      simulation,
      startRace: startRaceScenario,
      storageReload: storageReloadScenario,
      renderStress: renderStressScenario,
      resize: resizeScenario,
      restart: restartScenario,
      diagnostics,
      performanceAttribution,
      visual: {
        viewport: viewportAnalysis ?? { nonEmpty: false, reason: 'viewport screenshot was not captured' },
        canvas: canvasAnalysis ?? { nonEmpty: false, reason: 'canvas crop was not captured' },
        capture: {
          viewportMode: viewportCaptureMode,
          canvasMode: canvasCaptureMode,
          warnings: captureWarnings,
        },
      },
      perf,
      perfEndpoint,
      perfReports,
      voplaySlowFrames,
      warnings,
      errors,
      resourceFailures,
      ignoredResourceFailures,
      consoleEvents: events.console,
      browserLog: events.browserLog,
      issues,
      remainingRisks: [
        'Baseline covers the checked-in quickplay package, not a local BlockKart source checkout.',
        'Headless WebGPU timing is machine-dependent; slow-frame thresholds are diagnostic, not a final product FPS promise.',
        'The capture window is intentionally short for CI; long-run race loop stability remains a follow-up product soak gate.',
        'BlockKart product persistence is scoped to the Studio web runtime /persist VFS root; native-host preference sync remains a later host integration gate.',
      ],
    };

    const jsonPath = path.join(outDir, 'blockkart-baseline.json');
    const markdownPath = path.join(outDir, 'blockkart-baseline.md');
    writeFileSync(jsonPath, `${JSON.stringify(report, null, 2)}\n`);
    writeFileSync(markdownPath, markdownReport(report));

    console.log(`BlockKart baseline: ${report.status}`);
    console.log(`BlockKart baseline: report ${path.relative(root, markdownPath)}`);
    console.log(`BlockKart baseline: json ${path.relative(root, jsonPath)}`);
    console.log(`BlockKart baseline: screenshot ${path.relative(root, viewportScreenshot)}`);
    console.log(`BlockKart baseline: canvas ${finalState.canvasWidth ?? 0}x${finalState.canvasHeight ?? 0} nonEmpty=${report.visual.canvas.nonEmpty}`);
    console.log(`BlockKart baseline: lifecycle ${lifecycle.state ?? 'not-reported'} expected=${expectedLifecycleState} reachedRunning=${lifecycle.reachedRunning}`);
    console.log(`BlockKart baseline: diagnostics assetReports=${diagnostics.assetReports.length} sceneReports=${diagnostics.sceneReports.length} vehicleReports=${diagnostics.vehicleReports.length} raceReports=${diagnostics.raceReports.length} startRace=${startRaceScenario.completed ? 'yes' : 'no'} storageReload=${storageReloadScenario.completed ? 'yes' : 'no'} restarts=${restartScenario.completed}/${restartScenario.requested}`);
    console.log(`BlockKart baseline: startup phases ${startupPhases.length}, perf reports ${perf.count}, slow frames ${voplaySlowFrames.length}, warnings ${warnings.length}, errors ${errors.length}, resource failures ${resourceFailures.length}`);

    client.close();
    await closePage(debugPort, targetId);
    stopProcess(browser.child);
    stopProcess(preview.child);
    cleanup();

    if (failOnIssues && p0p1.length > 0) {
      for (const issue of p0p1) {
        console.error(`BlockKart baseline: ${issue.severity} ${issue.owner}: ${issue.title}`);
      }
      process.exit(1);
    }
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    const logs = [
      ['vite preview', preview.log()],
      ['browser', browser?.log() ?? ''],
    ]
      .filter(([, log]) => log)
      .map(([label, log]) => `${label} log:\n${log}`)
      .join('\n');
    fail(logs ? `${detail}\n${logs}` : detail);
  }
}

await withTimeout(main(), baselineTimeoutMs, 'BlockKart baseline').catch((error) => {
  fail(error instanceof Error ? error.message : String(error));
});
