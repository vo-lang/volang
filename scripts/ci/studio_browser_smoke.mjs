#!/usr/bin/env node
import { spawn, spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  renameSync,
  rmSync,
  writeFileSync,
} from 'node:fs';
import net from 'node:net';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import {
  STUDIO_BROWSER_SMOKE_KIND,
  STUDIO_BROWSER_SMOKE_SCHEMA_VERSION,
  selectLatestBlockKartPerfFrame,
  validateStudioBrowserSmokeEvidence,
  webGpuRequirementFailure,
} from './studio_browser_smoke_contract.mjs';

const root = fileURLToPath(new URL('../..', import.meta.url));
const studioDir = path.join(root, 'apps/studio');
const studioDistIndex = path.join(studioDir, 'dist/index.html');
const smokeTimeoutMs = Number.parseInt(process.env.STUDIO_BROWSER_SMOKE_TIMEOUT_MS ?? '90000', 10);
const quickplayTimeoutMs = Number.parseInt(process.env.STUDIO_BROWSER_SMOKE_QUICKPLAY_TIMEOUT_MS ?? '90000', 10);
const totalTimeoutMs = Number.parseInt(process.env.STUDIO_BROWSER_SMOKE_TOTAL_TIMEOUT_MS ?? '840000', 10);
const requireWebGpuAdapter = process.env.STUDIO_BROWSER_SMOKE_REQUIRE_WEBGPU === '1';
const outDir = path.resolve(process.env.STUDIO_BROWSER_SMOKE_OUT_DIR ?? path.join(root, 'target/studio-browser-smoke'));
const evidencePath = path.join(outDir, 'studio-browser-smoke.json');
const viewportScreenshotPath = path.join(outDir, 'studio-browser-smoke-viewport.png');
const canvasScreenshotPath = path.join(outDir, 'studio-browser-smoke-canvas.png');
const voplayPerfReportRoute = '/__voplay_perf_report';
const noWebGpuAdapterPattern = /no suitable GPU adapter|requestAdapter returned null|navigator\.gpu is unavailable/i;
const pngSignature = Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]);

const cleanupCallbacks = [];
let cleanedUp = false;
let failureEvidenceWritten = false;
let failureStage = 'initialization';

function fail(message) {
  exitWithFailure(message, 1);
}

function exitWithFailure(message, exitCode) {
  const detail = String(message ?? 'unknown failure').slice(0, 32768);
  console.error(`Studio browser smoke: ${detail}`);
  try {
    writeFailureEvidence(detail);
  } catch (error) {
    console.error(`Studio browser smoke: could not write failure evidence: ${error instanceof Error ? error.message : String(error)}`);
  }
  cleanup();
  process.exit(exitCode);
}

function writeFailureEvidence(message) {
  if (failureEvidenceWritten) return;
  failureEvidenceWritten = true;
  mkdirSync(outDir, { recursive: true });
  writeJsonAtomic(evidencePath, {
    schemaVersion: STUDIO_BROWSER_SMOKE_SCHEMA_VERSION,
    kind: `${STUDIO_BROWSER_SMOKE_KIND}.failure`,
    status: 'failed',
    generatedAt: new Date().toISOString(),
    stage: String(failureStage).slice(0, 200),
    error: String(message).slice(0, 32768),
    policy: { requireWebGpuAdapter, totalTimeoutMs },
  });
}

function stage(value) {
  failureStage = String(value).slice(0, 200);
}

function assert(condition, message) {
  if (!condition) {
    fail(message);
  }
}

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function fetchWithTimeout(url, options = {}, timeoutMs = 10000) {
  const controller = new AbortController();
  const timeout = setTimeout(
    () => controller.abort(new Error(`fetch timed out after ${timeoutMs}ms`)),
    timeoutMs,
  );
  try {
    return await fetch(url, { ...options, signal: controller.signal });
  } finally {
    clearTimeout(timeout);
  }
}

async function fetchJsonWithTimeout(url, options = {}, timeoutMs = 10000) {
  const controller = new AbortController();
  const timeout = setTimeout(
    () => controller.abort(new Error(`fetch timed out after ${timeoutMs}ms`)),
    timeoutMs,
  );
  try {
    const response = await fetch(url, { ...options, signal: controller.signal });
    const body = await response.json();
    return { response, body };
  } finally {
    clearTimeout(timeout);
  }
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

for (const [signal, exitCode] of [['SIGINT', 130], ['SIGTERM', 143]]) {
  process.on(signal, () => {
    stage(`received ${signal}`);
    exitWithFailure(`terminated by ${signal}`, exitCode);
  });
}
process.on('exit', cleanup);

function trimLog(log) {
  return log.length > 12000 ? log.slice(log.length - 12000) : log;
}

function appendLog(current, chunk) {
  return trimLog(current + chunk.toString());
}

function portableRepoPath(file) {
  return path.relative(root, file).split(path.sep).join('/');
}

function sha256(bytes) {
  return createHash('sha256').update(bytes).digest('hex');
}

function writeJsonAtomic(file, value) {
  const temp = `${file}.tmp-${process.pid}`;
  try {
    writeFileSync(temp, `${JSON.stringify(value, null, 2)}\n`);
    renameSync(temp, file);
  } finally {
    rmSync(temp, { force: true });
  }
}

function pngArtifact(file, bytes) {
  assert(bytes.byteLength > pngSignature.byteLength, `${file} screenshot is empty`);
  assert(bytes.subarray(0, pngSignature.byteLength).equals(pngSignature), `${file} is not a PNG screenshot`);
  writeFileSync(file, bytes);
  return {
    path: portableRepoPath(file),
    bytes: bytes.byteLength,
    sha256: sha256(bytes),
    mediaType: 'image/png',
  };
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
      const response = await fetchWithTimeout(url, { cache: 'no-store', ...options });
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

async function fetchJsonOk(url, options = {}) {
  let lastError = 'not attempted';
  for (let attempt = 1; attempt <= 40; attempt++) {
    try {
      const { response, body } = await fetchJsonWithTimeout(url, { cache: 'no-store', ...options });
      if (response.ok) {
        return body;
      }
      lastError = `HTTP ${response.status}`;
    } catch (error) {
      lastError = error instanceof Error ? error.message : String(error);
    }
    await sleep(Math.min(1000, attempt * 100));
  }
  throw new Error(`${url} did not return JSON: ${lastError}`);
}

async function clearVoplayPerfReports(baseUrl) {
  const url = new URL(voplayPerfReportRoute, baseUrl);
  const { response, body } = await fetchJsonWithTimeout(url, { method: 'DELETE', cache: 'no-store' });
  if (!response.ok) {
    throw new Error(`${url} returned HTTP ${response.status} while clearing perf reports`);
  }
  if (body?.count !== 0) {
    throw new Error(`${url} did not clear perf reports: ${JSON.stringify(body)}`);
  }
}

async function fetchVoplayPerfReports(baseUrl) {
  const url = new URL(voplayPerfReportRoute, baseUrl);
  const { response, body } = await fetchJsonWithTimeout(url, { cache: 'no-store' });
  if (!response.ok) {
    throw new Error(`${url} returned HTTP ${response.status}`);
  }
  return {
    count: Number.isSafeInteger(body?.count) ? body.count : 0,
    reports: Array.isArray(body?.reports) ? body.reports : [],
  };
}

function commandWorks(command) {
  const result = spawnSync(command, ['--version'], { stdio: 'ignore' });
  return !result.error && result.status === 0;
}

function findBrowserBinary() {
  const explicit = [
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
  if (child.exitCode != null || child.signalCode != null) {
    return;
  }
  if (process.platform !== 'win32' && child.pid) {
    try {
      process.kill(-child.pid, 'SIGTERM');
      return;
    } catch {
      // Fall through to direct kill.
    }
  }
  child.kill('SIGTERM');
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
  return {
    child,
    log: () => log,
  };
}

async function startBrowser(debugPort) {
  const browserBin = findBrowserBinary();
  if (!browserBin) {
    fail('could not find Chrome/Chromium. Set STUDIO_BROWSER_BIN or CHROME_BIN for the browser smoke gate.');
  }
  const profileDir = mkdtempSync(path.join(os.tmpdir(), 'volang-studio-browser-smoke-'));
  cleanupCallbacks.push(() => rmSync(profileDir, { recursive: true, force: true }));
  let log = '';
  const child = spawn(
    browserBin,
    [
      '--headless=new',
      '--disable-dev-shm-usage',
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
  return {
    child,
    log: () => log,
  };
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
  const target = await fetchJsonOk(
    `http://127.0.0.1:${debugPort}/json/new?${encodeURIComponent('about:blank')}`,
    { method: 'PUT' },
  );
  assert(target.webSocketDebuggerUrl, 'Chrome did not return a page websocket URL');
  const ws = await openWebSocket(target.webSocketDebuggerUrl);
  return { targetId: target.id, client: new CdpClient(ws) };
}

async function closePage(debugPort, targetId) {
  if (!targetId) {
    return;
  }
  await fetchWithTimeout(
    `http://127.0.0.1:${debugPort}/json/close/${targetId}`,
    {},
    5000,
  ).catch(() => undefined);
}

async function navigate(client, url) {
  const loaded = client.waitFor('Page.loadEventFired', () => true, 45000).catch(() => null);
  await client.send('Page.navigate', { url });
  await loaded;
  await waitForExpression(
    client,
    'document.readyState complete',
    'document.readyState === "complete"',
    (value) => value === true,
    45000,
  );
}

async function capturePng(client, clip = null) {
  const normalizedClip = clip ? {
    x: Math.max(0, Number(clip.x)),
    y: Math.max(0, Number(clip.y)),
    width: Math.max(1, Number(clip.width)),
    height: Math.max(1, Number(clip.height)),
    scale: 1,
  } : null;
  let lastError = null;
  await client.send('Page.bringToFront', {}, 5000).catch(() => undefined);
  for (const fromSurface of [true, false]) {
    try {
      const result = await client.send('Page.captureScreenshot', {
        format: 'png',
        fromSurface,
        captureBeyondViewport: false,
        ...(normalizedClip ? { clip: normalizedClip } : {}),
      }, 30000);
      const bytes = Buffer.from(result.data ?? '', 'base64');
      if (bytes.byteLength > pngSignature.byteLength && bytes.subarray(0, pngSignature.byteLength).equals(pngSignature)) {
        return bytes;
      }
      lastError = new Error('Chrome returned an empty or malformed PNG');
    } catch (error) {
      lastError = error;
    }
  }
  throw new Error(`Page.captureScreenshot failed: ${lastError instanceof Error ? lastError.message : String(lastError)}`);
}

async function waitForExpression(
  client,
  label,
  expression,
  predicate,
  timeoutMs,
  terminalError = () => null,
) {
  const deadline = Date.now() + timeoutMs;
  let lastValue = null;
  let lastError = null;
  while (Date.now() < deadline) {
    try {
      lastValue = await client.evaluate(expression, 10000);
      lastError = null;
      const terminal = terminalError(lastValue);
      if (terminal) {
        fail(`${label} failed: ${terminal}; state: ${JSON.stringify(lastValue)}`);
      }
      if (predicate(lastValue)) {
        return lastValue;
      }
    } catch (error) {
      lastError = error instanceof Error ? error.message : String(error);
    }
    await sleep(300);
  }
  const detail = lastError ?? JSON.stringify(lastValue);
  fail(`${label} timed out after ${timeoutMs}ms; last state: ${detail}`);
}

function pageStateExpression() {
  return `(() => {
    const app = document.querySelector('#app');
    const text = document.body.innerText || '';
    const splashError = document.querySelector('.splash-card.error')?.innerText || '';
    const loading = document.querySelector('.splash-loading')?.innerText || '';
    return {
      href: location.href,
      appChildren: app ? app.children.length : -1,
      mounted: Boolean(app && app.children.length > 0),
      hasStudioText: text.includes('VoStudio') || text.includes('BlockKart') || text.includes('Open'),
      loading,
      error: splashError,
      textSample: text.slice(0, 500),
    };
  })()`;
}

function quickplayStateExpression() {
  return `(() => {
    const app = document.querySelector('#app');
    const text = document.body.innerText || '';
    const hook = globalThis.__voStudioBrowserSmoke;
    const runtime = hook?.runtimeState?.() ?? null;
    const renderer = hook?.rendererState?.() ?? null;
    const runtimeLastErrorPresent = runtime != null
      && Object.prototype.hasOwnProperty.call(runtime, 'lastError');
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
      runtimeReady: runtime?.status === 'ready'
        && runtime?.kind === 'gui'
        && runtimeLastErrorPresent
        && runtime.lastError === null,
      runtimeStatus: runtime?.status ?? null,
      runtimeKind: runtime?.kind ?? null,
      runtimeIsRunning: runtime?.isRunning === true,
      runtimeSessionId: runtime?.gui?.sessionId ?? null,
      moduleBytes: runtime?.gui?.moduleBytes?.byteLength ?? 0,
      renderBytes: runtime?.gui?.renderBytes?.byteLength ?? 0,
      rendererActive: renderer?.active === true,
      rendererCount: Array.isArray(renderer?.renderers) ? renderer.renderers.length : 0,
      rendererSessionId: renderer?.sessionId ?? null,
      rendererCapabilities: Array.isArray(renderer?.renderers) ? renderer.renderers : [],
      runtimeLastErrorPresent,
      runtimeError: runtimeLastErrorPresent ? runtime.lastError : null,
      loading,
      error,
      textSample: text.slice(0, 500),
    };
  })()`;
}

function webGpuStateExpression() {
  return `(async () => {
    const gpu = globalThis.navigator?.gpu;
    if (!gpu) {
      return {
        probeExecutedInPage: true,
        supported: false,
        adapter: false,
        reason: 'navigator.gpu is unavailable',
        features: [],
        info: null,
      };
    }
    try {
      const adapter = await gpu.requestAdapter();
      const info = adapter?.info ?? null;
      return {
        probeExecutedInPage: true,
        supported: true,
        adapter: Boolean(adapter),
        reason: adapter ? 'adapter available' : 'requestAdapter returned null',
        features: adapter ? Array.from(adapter.features ?? []).sort() : [],
        info: info ? {
          architecture: info.architecture ?? '',
          description: info.description ?? '',
          device: info.device ?? '',
          vendor: info.vendor ?? '',
        } : null,
      };
    } catch (error) {
      return {
        probeExecutedInPage: true,
        supported: true,
        adapter: false,
        reason: error instanceof Error ? error.message : String(error),
        features: [],
        info: null,
      };
    }
  })()`;
}

function studioBrowserSmokeHookReadyExpression() {
  return `(() => {
    const hook = globalThis.__voStudioBrowserSmoke;
    const entryPath = hook?.entryPath?.() ?? null;
    const lines = hook?.consoleLines?.() ?? [];
    const runtime = hook?.runtimeState?.() ?? null;
    const runtimeLastErrorPresent = runtime != null
      && Object.prototype.hasOwnProperty.call(runtime, 'lastError');
    const bodyText = document.body?.innerText ?? '';
    return {
      ready: Boolean(hook?.dumpCurrent && entryPath),
      entryPath,
      loading: document.querySelector('.runner-loading, .splash-loading')?.innerText || '',
      error: document.querySelector('.runner-error, .splash-card.error')?.innerText || '',
      consoleTail: Array.isArray(lines) ? lines.slice(-20) : [],
      runtimeState: runtime == null ? null : {
        status: runtime.status ?? null,
        kind: runtime.kind ?? null,
        isRunning: runtime.isRunning === true,
        lastErrorPresent: runtimeLastErrorPresent,
        lastError: runtimeLastErrorPresent ? runtime.lastError : null,
        moduleBytesLength: runtime.gui?.moduleBytes?.byteLength ?? 0,
        renderBytesLength: runtime.gui?.renderBytes?.byteLength ?? 0,
        sessionId: runtime.gui?.sessionId ?? null,
      },
      textSample: bodyText.slice(0, 1000),
    };
  })()`;
}

function quickplayBytecodeContractExpression() {
  return `(async () => {
    const hook = globalThis.__voStudioBrowserSmoke;
    if (!hook?.dumpCurrent) {
      return { ok: false, reason: 'Studio browser smoke debug hook is unavailable' };
    }
    const entryPath = hook.entryPath?.() ?? null;
    if (!entryPath) {
      return { ok: false, reason: 'Studio browser smoke debug hook has no entry path' };
    }
    const wait = (ms) => new Promise((resolve) => setTimeout(resolve, ms));
    const waitForDebugHook = async (name) => {
      for (let i = 0; i < 100; i++) {
        const debugHook = globalThis[name];
        if (debugHook?.dumpModuleBytes) {
          return debugHook;
        }
        await wait(100);
      }
      return null;
    };
    const analyzeDump = (label, dump, moduleBytesLength = null) => {
      const voplayExterns = dump
        .split('\\n')
        .filter((line) => line.startsWith('# [') && line.includes('voplay_'))
        .slice(0, 64);
      const textureExterns = dump
        .split('\\n')
        .filter((line) => line.startsWith('# [') && /texture|load/i.test(line))
        .slice(0, 64);
      const dumpHead = dump.slice(0, 1000);
      const functionBlocks = dump.split(/\\n(?=func_\\d+ )/g);
      const externMatch = /^# \\[(\\d+)\\] voplay_loadTextureBytes\\(1\\) ->/m.exec(dump);
      if (!externMatch) {
        return {
          label,
          ok: true,
          skipped: true,
          entryPath,
          moduleBytesLength,
          reason: 'voplay_loadTextureBytes extern is missing from bytecode dump',
          voplayExterns,
          textureExterns,
          dumpHead,
        };
      }
      const externId = Number(externMatch[1]);
      const externCallNeedle = 'extern_' + externId;
      const closureBlocks = functionBlocks.filter((block) => (
        /^func_\\d+ closure_/.test(block)
        && block.includes('CallExtern')
        && block.includes(externCallNeedle)
      ));
      const wrongBlocks = closureBlocks.filter((block) => !/^\\s*# param_types: .*\\bp\\d+=Slice\\b/m.test(block));
      const summarize = (block) => {
        const lines = block.split('\\n');
        const header = lines[0] ?? '';
        const paramTypes = lines.find((line) => line.includes('# param_types:')) ?? '';
        const call = lines.find((line) => line.includes('CallExtern') && line.includes(externCallNeedle)) ?? '';
        return [header, paramTypes, call].filter(Boolean).join(' | ');
      };
      return {
        label,
        ok: closureBlocks.length > 0 && wrongBlocks.length === 0,
        entryPath,
        moduleBytesLength,
        externId,
        closureCount: closureBlocks.length,
        wrongCount: wrongBlocks.length,
        closures: closureBlocks.slice(0, 8).map(summarize),
        wrongClosures: wrongBlocks.slice(0, 8).map(summarize),
      };
    };
    const current = analyzeDump('compile', await hook.dumpCurrent());
    const rendererHook = await waitForDebugHook('__voStudioBrowserSmokeRenderer');
    const renderer = rendererHook
      ? analyzeDump('renderer', await rendererHook.dumpModuleBytes(), rendererHook.moduleBytesLength?.() ?? null)
      : { label: 'renderer', ok: true, skipped: true, reason: 'Studio renderer module bytes debug hook is unavailable' };
    const renderIslandHook = await waitForDebugHook('__voStudioBrowserSmokeRenderIsland');
    const renderIsland = renderIslandHook
      ? analyzeDump('renderIsland', await renderIslandHook.dumpModuleBytes(), renderIslandHook.moduleBytesLength?.() ?? null)
      : { label: 'renderIsland', ok: true, skipped: true, reason: 'Studio render-island VM bytecode debug hook is unavailable' };
    const probes = [current, renderer, renderIsland];
    const applicable = probes.filter((probe) => probe.skipped !== true);
    const skippedReasons = probes
      .filter((probe) => probe.skipped)
      .map((probe) => probe.reason)
      .filter(Boolean);
    return {
      ok: applicable.length > 0 && applicable.every((probe) => probe.ok === true),
      entryPath,
      skipped: applicable.length === 0,
      reason: skippedReasons.join('; '),
      externId: renderIsland.externId ?? renderer.externId ?? current.externId,
      closureCount: applicable.reduce((sum, probe) => sum + (probe.closureCount ?? 0), 0),
      wrongCount: applicable.reduce((sum, probe) => sum + (probe.wrongCount ?? 0), 0),
      current,
      renderer,
      renderIsland,
    };
  })()`;
}

function assetProbeExpression() {
  return `(async () => {
    async function text(path) {
      const response = await fetch(path, { cache: 'no-store' });
      return { path, ok: response.ok, status: response.status, text: response.ok ? await response.text() : '' };
    }
    async function json(path) {
      const response = await fetch(path, { cache: 'no-store' });
      return { path, ok: response.ok, status: response.status, body: response.ok ? await response.json() : null };
    }
    const buildId = await text('/wasm/vo_studio_wasm.build_id');
    const wasmJs = await text('/wasm/vo_studio_wasm.js');
    const wasmBinary = await fetch('/wasm/vo_studio_wasm_bg.wasm', { cache: 'no-store' });
    const project = await json('/quickplay/blockkart/project.json');
    const deps = await json('/quickplay/blockkart/deps.json');
    return {
      buildId: { ok: buildId.ok, status: buildId.status, value: buildId.text.trim() },
      wasmJs: { ok: wasmJs.ok, status: wasmJs.status, referencesWasm: wasmJs.text.includes('vo_studio_wasm_bg.wasm') },
      wasmBinary: { ok: wasmBinary.ok, status: wasmBinary.status, bytes: Number(wasmBinary.headers.get('content-length') || '0') },
      project: {
        ok: project.ok,
        status: project.status,
        schemaVersion: project.body?.schemaVersion,
        name: project.body?.name,
        module: project.body?.module,
      },
      deps: {
        ok: deps.ok,
        status: deps.status,
        schemaVersion: deps.body?.schemaVersion,
        name: deps.body?.name,
        moduleCount: deps.ok && deps.body?.modules ? deps.body.modules.length : 0,
      },
    };
  })()`;
}

function installPageFailureCollectors(client, pageFailures) {
  client.on('Runtime.exceptionThrown', (params) => {
    pageFailures.push(formatExceptionDetails(params.exceptionDetails ?? params));
  });
  client.on('Runtime.consoleAPICalled', (params) => {
    if (params.type !== 'error') {
      return;
    }
    const text = (params.args ?? [])
      .map((arg) => arg.value ?? arg.description ?? '')
      .filter(Boolean)
      .join(' ');
    pageFailures.push(`console.error: ${text || 'no message'}`);
  });
  client.on('Log.entryAdded', (params) => {
    const entry = params.entry ?? {};
    if (entry.level === 'error' && entry.source !== 'network') {
      pageFailures.push(`browser log: ${entry.text ?? 'no message'}`);
    }
  });
}

function assertNoPageFailures(stage, pageFailures) {
  if (pageFailures.length === 0) {
    return;
  }
  fail(`${stage} reported browser errors:\n${pageFailures.join('\n')}`);
}

async function waitForQuickplayFirstFrame(
  client,
  baseUrl,
  notBefore,
  timeoutMs,
  expectedPerfEpoch = null,
  minimumFrameExclusive = null,
) {
  if (expectedPerfEpoch != null
    && (!Number.isSafeInteger(expectedPerfEpoch) || expectedPerfEpoch < 1)) {
    throw new Error(`expected perf epoch is invalid: ${expectedPerfEpoch}`);
  }
  if (minimumFrameExclusive != null
    && (!Number.isSafeInteger(minimumFrameExclusive) || minimumFrameExclusive < 1)) {
    throw new Error(`minimum frame is invalid: ${minimumFrameExclusive}`);
  }
  const deadline = Date.now() + timeoutMs;
  let lastState = null;
  let lastPerfEndpoint = { count: 0, reports: [] };
  let lastError = null;
  while (Date.now() < deadline) {
    try {
      const state = await client.evaluate(quickplayStateExpression(), 10000);
      lastState = state;
      lastError = null;
      const rendererBridgeReady = (
        state.runner
        && state.hasCanvas
        && state.canvasWidth > 0
        && state.canvasHeight > 0
        && state.runtimeReady
        && state.runtimeIsRunning
        && state.moduleBytes > 0
        && state.renderBytes > 0
        && state.rendererActive
        && state.rendererCount > 0
        && state.runtimeSessionId != null
        && state.rendererSessionId === state.runtimeSessionId
        && !state.error
        && state.runtimeLastErrorPresent
        && state.runtimeError === null
      );
      if (rendererBridgeReady) {
        lastPerfEndpoint = await fetchVoplayPerfReports(baseUrl);
        const perfRecord = selectLatestBlockKartPerfFrame(
          lastPerfEndpoint.reports,
          notBefore,
          state.runtimeSessionId,
          expectedPerfEpoch,
          minimumFrameExclusive,
        );
        if (perfRecord) {
          return {
            ok: true,
            skipped: false,
            state,
            rendererReadiness: {
              ready: true,
              source: 'voplay-perf-endpoint',
              reportCount: lastPerfEndpoint.count,
              frame: perfRecord.payload.frame,
              record: perfRecord,
              notBefore,
              expectedPerfEpoch,
              minimumFrameExclusive,
            },
          };
        }
      }
      if (state.error || !state.runtimeLastErrorPresent || state.runtimeError !== null) {
        return {
          ok: false,
          skipped: false,
          reason: state.error
            || (!state.runtimeLastErrorPresent ? 'GUI runtime lastError field is missing' : state.runtimeError),
          state,
        };
      }
    } catch (error) {
      lastError = error instanceof Error ? error.message : String(error);
    }
    await sleep(300);
  }
  const detail = lastError ?? JSON.stringify({ state: lastState, perfReportCount: lastPerfEndpoint.count });
  return { ok: false, skipped: false, reason: `timed out after ${timeoutMs}ms; last state: ${detail}`, state: lastState };
}

function assertNoUnexpectedQuickplayFailures(quickplay, pageFailures) {
  if (pageFailures.length === 0) {
    return;
  }
  const unexpected = quickplay.skipped
    ? pageFailures.filter((failure) => !noWebGpuAdapterPattern.test(failure))
    : pageFailures;
  if (unexpected.length === 0) {
    return;
  }
  fail(`BlockKart quickplay reported browser errors:\n${unexpected.join('\n')}`);
}

async function main() {
  stage('preflight');
  assert(existsSync(studioDistIndex), 'apps/studio/dist is missing; run the studio-build task first');
  rmSync(outDir, { recursive: true, force: true });
  mkdirSync(outDir, { recursive: true });
  const startedAt = new Date().toISOString();
  const previewPort = await reservePort();
  const debugPort = await reservePort();
  const baseUrl = `http://127.0.0.1:${previewPort}/`;
  const preview = startPreviewServer(previewPort);
  let browser = null;
  let client = null;
  let targetId = null;
  try {
    stage('preview startup');
    await fetchOk(baseUrl).catch((error) => {
      fail(`vite preview did not start: ${error.message}\n${preview.log()}`);
    });

    stage('browser startup');
    browser = await startBrowser(debugPort);
    ({ targetId, client } = await openPage(debugPort));
    cleanupCallbacks.push(() => client.close());
    cleanupCallbacks.push(() => {
      void closePage(debugPort, targetId);
    });

    const pageFailures = [];
    installPageFailureCollectors(client, pageFailures);
    await client.send('Page.enable');
    await client.send('Runtime.enable');
    await client.send('Log.enable');

    stage('Studio UI mount');
    await navigate(client, `${baseUrl}#/`);
    const mounted = await waitForExpression(
      client,
      'Studio main UI mount',
      pageStateExpression(),
      (state) => state.mounted && state.hasStudioText && !state.loading && !state.error,
      smokeTimeoutMs,
      (state) => state.error || null,
    );
    assertNoPageFailures('Studio main UI', pageFailures);

    const assets = await client.evaluate(assetProbeExpression(), 45000);
    assert(assets.buildId.ok && assets.buildId.value, `Studio WASM build id failed: ${JSON.stringify(assets.buildId)}`);
    assert(assets.wasmJs.ok && assets.wasmJs.referencesWasm, `Studio WASM JS probe failed: ${JSON.stringify(assets.wasmJs)}`);
    assert(assets.wasmBinary.ok, `Studio WASM binary probe failed: ${JSON.stringify(assets.wasmBinary)}`);
    assert(
      assets.project.ok
        && assets.project.schemaVersion === 2
        && assets.project.name === 'BlockKart'
        && assets.project.module === 'github.com/vo-lang/blockkart',
      `BlockKart project probe failed: ${JSON.stringify(assets.project)}`,
    );
    assert(
      assets.deps.ok
        && assets.deps.schemaVersion === 2
        && assets.deps.name === 'BlockKart dependencies'
        && assets.deps.moduleCount > 0,
      `BlockKart deps probe failed: ${JSON.stringify(assets.deps)}`,
    );
    assertNoPageFailures('Studio asset probes', pageFailures);

    stage('BlockKart quickplay startup');
    await clearVoplayPerfReports(baseUrl);
    pageFailures.length = 0;
    const quickplayUrl = new URL('/', baseUrl);
    quickplayUrl.searchParams.set('proj', 'vo:quickplay:blockkart');
    quickplayUrl.searchParams.set('mode', 'runner');
    quickplayUrl.searchParams.set('studioBrowserSmokeDebug', '1');
    quickplayUrl.searchParams.set('voplayPerf', 'stats');
    quickplayUrl.searchParams.set('voplayRendererPerf', '1');
    quickplayUrl.hash = '#/runner';
    const quickplayStartedAt = new Date().toISOString();
    await navigate(client, quickplayUrl.toString());

    const webGpuProbe = await client.evaluate(webGpuStateExpression(), 30000);
    const webGpu = { required: requireWebGpuAdapter, ...webGpuProbe };
    const webGpuFailure = webGpuRequirementFailure(requireWebGpuAdapter, webGpuProbe);
    assert(!webGpuFailure, webGpuFailure);
    if (!webGpuProbe.adapter) {
      const state = await client.evaluate(quickplayStateExpression(), 10000).catch(() => null);
      const reason = `no WebGPU adapter available (${webGpuProbe.reason})`;
      const quickplay = { ok: true, skipped: true, reason, state };
      assertNoUnexpectedQuickplayFailures(quickplay, pageFailures);
      const viewport = pngArtifact(viewportScreenshotPath, await capturePng(client));
      const visualCaptureCompletedAt = new Date().toISOString();
      writeJsonAtomic(evidencePath, {
        schemaVersion: STUDIO_BROWSER_SMOKE_SCHEMA_VERSION,
        kind: STUDIO_BROWSER_SMOKE_KIND,
        status: 'skipped',
        startedAt,
        quickplayStartedAt,
        visualCaptureCompletedAt,
        finishedAt: new Date().toISOString(),
        webGpu,
        reason,
        pageFailures: [...pageFailures],
        screenshots: { viewport, canvas: null },
      });
      console.log(`Studio browser smoke: quickplay first frame skipped (${reason})`);
      console.log(`Studio browser smoke: evidence ${portableRepoPath(evidencePath)}`);
      client.close();
      await closePage(debugPort, targetId);
      stopProcess(browser.child);
      stopProcess(preview.child);
      cleanup();
      return;
    }

    const hookState = await waitForExpression(
      client,
      'Studio browser smoke debug hook',
      studioBrowserSmokeHookReadyExpression(),
      (state) => state.ready,
      smokeTimeoutMs,
      (state) => state.error || state.runtimeState?.lastError || null,
    );
    const dumpPath = process.env.STUDIO_BROWSER_SMOKE_DUMP_PATH;
    if (dumpPath) {
      const dump = await client.evaluate('globalThis.__voStudioBrowserSmoke.dumpCurrent()', 60000);
      writeFileSync(dumpPath, dump);
      console.log(`Studio browser smoke: wrote quickplay bytecode dump ${dumpPath}`);
    }
    const quickplay = await waitForQuickplayFirstFrame(client, baseUrl, quickplayStartedAt, quickplayTimeoutMs);
    assert(quickplay.ok, `BlockKart quickplay first frame failed: ${quickplay.reason}`);
    assertNoUnexpectedQuickplayFailures(quickplay, pageFailures);
    const bytecodeContract = await client.evaluate(quickplayBytecodeContractExpression(), 60000);
    assert(
      bytecodeContract.ok && bytecodeContract.skipped !== true,
      `BlockKart quickplay bytecode contract failed or had no applicable probe: ${JSON.stringify(bytecodeContract)}`,
    );

    stage('visual capture');
    const viewport = pngArtifact(viewportScreenshotPath, await capturePng(client));
    assert(
      quickplay.state.canvasRect?.width > 0 && quickplay.state.canvasRect?.height > 0,
      `BlockKart quickplay canvas rect is invalid: ${JSON.stringify(quickplay.state.canvasRect)}`,
    );
    const canvas = pngArtifact(canvasScreenshotPath, await capturePng(client, quickplay.state.canvasRect));
    const visualCaptureCompletedAt = new Date().toISOString();
    assertNoUnexpectedQuickplayFailures(quickplay, pageFailures);
    stage('post-capture renderer liveness');
    await clearVoplayPerfReports(baseUrl);
    const finalPerfEpoch = await client.evaluate(`(() => {
      const hook = globalThis.__voStudioBrowserSmoke;
      if (!hook || typeof hook.rotatePerfEvidenceEpoch !== 'function') {
        throw new Error('Studio perf evidence epoch hook is unavailable');
      }
      return hook.rotatePerfEvidenceEpoch();
    })()`, 5000);
    assert(
      Number.isSafeInteger(finalPerfEpoch) && finalPerfEpoch > 0,
      `Studio perf evidence epoch is invalid: ${finalPerfEpoch}`,
    );
    const finalProbeStartedAt = new Date().toISOString();
    const finalQuickplay = await waitForQuickplayFirstFrame(
      client,
      baseUrl,
      finalProbeStartedAt,
      quickplayTimeoutMs,
      finalPerfEpoch,
      quickplay.rendererReadiness.frame,
    );
    assert(finalQuickplay.ok, `BlockKart post-capture renderer liveness failed: ${finalQuickplay.reason}`);
    assertNoUnexpectedQuickplayFailures(finalQuickplay, pageFailures);
    stage('formal evidence validation');
    const evidence = {
      schemaVersion: STUDIO_BROWSER_SMOKE_SCHEMA_VERSION,
      kind: STUDIO_BROWSER_SMOKE_KIND,
      status: 'ok',
      startedAt,
      quickplayStartedAt,
      visualCaptureCompletedAt,
      finishedAt: new Date().toISOString(),
      studio: { mounted, assets },
      quickplay: {
        href: finalQuickplay.state.href,
        entryPath: hookState.entryPath,
        canvasId: finalQuickplay.state.canvasId,
        canvasWidth: finalQuickplay.state.canvasWidth,
        canvasHeight: finalQuickplay.state.canvasHeight,
      },
      webGpu,
      runtime: {
        ready: finalQuickplay.state.runtimeReady,
        status: finalQuickplay.state.runtimeStatus,
        kind: finalQuickplay.state.runtimeKind,
        isRunning: finalQuickplay.state.runtimeIsRunning,
        lastErrorPresent: finalQuickplay.state.runtimeLastErrorPresent,
        lastError: finalQuickplay.state.runtimeError,
        sessionId: finalQuickplay.state.runtimeSessionId,
        moduleBytes: finalQuickplay.state.moduleBytes,
        renderBytes: finalQuickplay.state.renderBytes,
      },
      renderer: {
        active: finalQuickplay.state.rendererActive,
        count: finalQuickplay.state.rendererCount,
        sessionId: finalQuickplay.state.rendererSessionId,
        capabilities: finalQuickplay.state.rendererCapabilities,
      },
      firstFrame: {
        ok: true,
        skipped: false,
        ...quickplay.rendererReadiness,
      },
      finalFrame: {
        ok: true,
        skipped: false,
        ...finalQuickplay.rendererReadiness,
      },
      bytecodeContract,
      pageFailures: [...pageFailures],
      screenshots: { viewport, canvas },
    };
    const evidenceErrors = validateStudioBrowserSmokeEvidence(evidence);
    assert(evidenceErrors.length === 0, `formal evidence contract failed: ${evidenceErrors.join('; ')}`);
    writeJsonAtomic(evidencePath, evidence);

    console.log('Studio browser smoke: ok');
    console.log(`Studio browser smoke: mounted ${mounted.href}`);
    console.log(`Studio browser smoke: wasm build ${assets.buildId.value}`);
    console.log(`Studio browser smoke: quickplay entry ${hookState.entryPath}`);
    console.log(`Studio browser smoke: WebGPU ${webGpu.reason}`);
    console.log(`Studio browser smoke: renderer perf frames ${quickplay.rendererReadiness.frame}->${finalQuickplay.rendererReadiness.frame}`);
    console.log(`Studio browser smoke: quickplay canvas ${finalQuickplay.state.canvasWidth}x${finalQuickplay.state.canvasHeight}`);
    console.log(`Studio browser smoke: evidence ${portableRepoPath(evidencePath)}`);
    if (bytecodeContract.skipped) {
      console.log(`Studio browser smoke: quickplay bytecode contract skipped (${bytecodeContract.reason})`);
    } else {
      console.log(`Studio browser smoke: quickplay texture-byte closures ${bytecodeContract.closureCount}`);
    }

    client.close();
    await closePage(debugPort, targetId);
    stopProcess(browser.child);
    stopProcess(preview.child);
    cleanup();
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

for (const [name, value] of [
  ['STUDIO_BROWSER_SMOKE_TIMEOUT_MS', smokeTimeoutMs],
  ['STUDIO_BROWSER_SMOKE_QUICKPLAY_TIMEOUT_MS', quickplayTimeoutMs],
  ['STUDIO_BROWSER_SMOKE_TOTAL_TIMEOUT_MS', totalTimeoutMs],
]) {
  if (!Number.isSafeInteger(value) || value < 1 || value > 2_147_000_000) {
    fail(`${name} must be a positive safe integer within the Node timer limit`);
  }
}

const totalDeadline = setTimeout(() => {
  stage('total deadline exceeded');
  exitWithFailure(`total timeout after ${totalTimeoutMs}ms`, 124);
}, totalTimeoutMs);

try {
  if (process.argv.includes('--selftest-signal-failure-evidence')) {
    stage('signal failure evidence selftest');
    process.emit('SIGTERM');
    throw new Error('SIGTERM handler returned unexpectedly');
  }
  if (process.argv.includes('--selftest-timeout-failure-evidence')) {
    stage('total deadline selftest waiting');
    await new Promise(() => undefined);
  } else {
    await main();
  }
} catch (error) {
  fail(error instanceof Error ? error.message : String(error));
} finally {
  clearTimeout(totalDeadline);
}
