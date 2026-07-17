import { Buffer } from 'node:buffer';
import {
  closeSync,
  constants as fsConstants,
  existsSync,
  fstatSync,
  openSync,
  readSync,
  readdirSync,
  realpathSync,
  statSync,
} from 'node:fs';
import { dirname, extname, isAbsolute, join, relative, resolve, sep } from 'node:path';
import { defineConfig } from 'vite';
import { svelte, vitePreprocess } from '@sveltejs/vite-plugin-svelte';
import {
  resolveStudioBuildId as resolveSharedStudioBuildId,
  validateStudioWasmBuildId,
} from './scripts/studio_build_id.mjs';
import { planLocalProjectSources } from './scripts/local_project_snapshot_plan.mjs';

interface ProxyRequest {
  method?: string;
  url?: string;
  on(event: 'data', handler: (chunk: Uint8Array | string) => void): void;
  on(event: 'end', handler: () => void): void;
  on(event: 'error', handler: (error: Error) => void): void;
}

interface ProxyResponse {
  writeHead(status: number, headers?: Record<string, string>): void;
  end(body?: string | Uint8Array): void;
}

interface ProxyMiddlewares {
  use(route: string, handler: (req: ProxyRequest, res: ProxyResponse) => void): void;
}

interface ProxyServer {
  middlewares: ProxyMiddlewares;
  ws?: {
    send(payload: unknown): void;
  };
}

interface LocalProjectSnapshotFile {
  path: string;
  contentBase64: string;
  mode: number;
}

interface LocalProjectSnapshot {
  projectPath: string;
  projectRelativePath: string;
  files: LocalProjectSnapshotFile[];
}

interface VoplayPerfReport {
  receivedAt: string;
  payload: unknown;
}

interface LocalProjectSnapshotPlan {
  projectRoot: string;
  snapshotBase: string;
  roots: string[];
  files: string[];
}

const buildEnv = (globalThis as typeof globalThis & {
  process?: { env?: Record<string, string | undefined> };
}).process?.env ?? {};

const LOCAL_PROJECT_ROUTE = '/__vo_studio_local_project';
const VOPLAY_PERF_REPORT_ROUTE = '/__voplay_perf_report';
const VOPLAY_PERF_RELOAD_ROUTE = '/__voplay_perf_reload';
const VOPLAY_PERF_REPORT_LIMIT = 256;
const VOPLAY_PERF_REPORT_MAX_BYTES = 128 * 1024;
const LOCAL_PROJECT_MAX_FILES = Math.min(
  readPositiveIntEnv('VITE_STUDIO_LOCAL_PROJECT_MAX_FILES', 5000),
  20_000,
);
// The snapshot is transported as base64 JSON. Keep raw bytes below half of the
// browser's 128 MiB response ceiling so encoding and path metadata stay bounded.
const LOCAL_PROJECT_MAX_BYTES = Math.min(
  readByteSizeEnv('VITE_STUDIO_LOCAL_PROJECT_MAX_BYTES', 64 * 1024 * 1024),
  64 * 1024 * 1024,
);
const LOCAL_PROJECT_MAX_FILE_BYTES = Math.min(
  readByteSizeEnv('VITE_STUDIO_LOCAL_PROJECT_MAX_FILE_BYTES', 256 * 1024 * 1024),
  LOCAL_PROJECT_MAX_BYTES,
);
const LOCAL_PROJECT_MAX_ROOTS = 10_000;
const LOCAL_PROJECT_MAX_ENTRIES = 100_000;
const LOCAL_PROJECT_MAX_DEPTH = 256;
const LOCAL_PROJECT_MAX_PATH_BYTES = 4096;
const LOCAL_PROJECT_MAX_NAME_BYTES = 255;
const LOCAL_PROJECT_MAX_METADATA_BYTES = 16 * 1024 * 1024;
const LOCAL_PROJECT_MAX_RESPONSE_BYTES = 128 * 1024 * 1024;
const ROOT_PATH = sep;
const LOCAL_PROJECT_SKIP_DIRS = new Set([
  '.git',
  '.hg',
  '.svn',
  '.cache',
  '.turbo',
  'node_modules',
  'target',
]);
const voplayPerfReports: VoplayPerfReport[] = [];

function readPositiveIntEnv(name: string, fallback: number): number {
  const raw = buildEnv[name]?.trim();
  if (!raw) {
    return fallback;
  }
  const parsed = Number.parseInt(raw, 10);
  return Number.isSafeInteger(parsed) && parsed > 0 ? parsed : fallback;
}

function readByteSizeEnv(name: string, fallback: number): number {
  const raw = buildEnv[name]?.trim();
  if (!raw) {
    return fallback;
  }
  const match = raw.match(/^(\d+(?:\.\d+)?)\s*(b|kb|kib|mb|mib|gb|gib)?$/i);
  if (!match) {
    return fallback;
  }
  const value = Number.parseFloat(match[1]);
  if (!Number.isFinite(value) || value <= 0) {
    return fallback;
  }
  const unit = (match[2] ?? 'b').toLowerCase();
  const multiplier = unit === 'gb' || unit === 'gib'
    ? 1024 * 1024 * 1024
    : unit === 'mb' || unit === 'mib'
      ? 1024 * 1024
      : unit === 'kb' || unit === 'kib'
        ? 1024
        : 1;
  const bytes = Math.floor(value * multiplier);
  return Number.isSafeInteger(bytes) && bytes > 0 ? bytes : fallback;
}

function compareUtf8(left: string, right: string): number {
  return Buffer.compare(Buffer.from(left, 'utf8'), Buffer.from(right, 'utf8'));
}

function localProjectSnapshotsEnabled(): boolean {
  return buildEnv.VITE_STUDIO_LOCAL_PROJECTS === '1';
}

function localProjectSnapshot() {
  return {
    name: 'studio-local-project-snapshot',
    configureServer(server: ProxyServer) {
      installLocalProjectSnapshot(server.middlewares);
    },
    configurePreviewServer(server: ProxyServer) {
      installLocalProjectSnapshot(server.middlewares);
    },
  };
}

function voplayPerfReportEndpoint() {
  return {
    name: 'studio-voplay-perf-report-endpoint',
    configureServer(server: ProxyServer) {
      installVoplayPerfReportEndpoint(server.middlewares);
      installVoplayPerfReloadEndpoint(server);
    },
    configurePreviewServer(server: ProxyServer) {
      installVoplayPerfReportEndpoint(server.middlewares);
      installVoplayPerfReloadEndpoint(server);
    },
  };
}

function installLocalProjectSnapshot(middlewares: ProxyMiddlewares) {
  middlewares.use(LOCAL_PROJECT_ROUTE, (req: ProxyRequest, res: ProxyResponse) => {
    try {
      if (!localProjectSnapshotsEnabled()) {
        writeText(res, 404, 'local project bridge is disabled');
        return;
      }
      const url = new URL(req.url ?? '', 'http://studio.local');
      const rawPath = url.searchParams.get('path');
      if (!rawPath) {
        writeText(res, 400, 'missing path query parameter');
        return;
      }
      writeJson(res, readLocalProjectSnapshot(rawPath), LOCAL_PROJECT_MAX_RESPONSE_BYTES);
    } catch (error) {
      writeText(res, 500, localProjectErrorMessage(error));
    }
  });
}

function installVoplayPerfReportEndpoint(middlewares: ProxyMiddlewares) {
  middlewares.use(VOPLAY_PERF_REPORT_ROUTE, (req: ProxyRequest, res: ProxyResponse) => {
    const method = (req.method ?? 'GET').toUpperCase();
    if (method === 'GET') {
      writeJson(res, { count: voplayPerfReports.length, reports: voplayPerfReports });
      return;
    }
    if (method === 'DELETE') {
      voplayPerfReports.length = 0;
      writeJson(res, { ok: true, count: 0 });
      return;
    }
    if (method !== 'POST') {
      writeText(res, 405, 'method not allowed');
      return;
    }
    readRequestBody(req, VOPLAY_PERF_REPORT_MAX_BYTES).then((body) => {
      let payload: unknown = body;
      if (body.length > 0) {
        try {
          payload = JSON.parse(body);
        } catch {
          payload = { raw: body };
        }
      }
      pushVoplayPerfReportPayload(payload);
      writeJson(res, { ok: true, count: voplayPerfReports.length });
    }).catch((error: unknown) => {
      writeText(res, 400, localProjectErrorMessage(error));
    });
  });
}

function pushVoplayPerfReportPayload(payload: unknown) {
  const receivedAt = new Date().toISOString();
  const payloads = Array.isArray(payload) ? payload : [payload];
  for (const item of payloads) {
    voplayPerfReports.push({ receivedAt, payload: item });
  }
  if (voplayPerfReports.length > VOPLAY_PERF_REPORT_LIMIT) {
    voplayPerfReports.splice(0, voplayPerfReports.length - VOPLAY_PERF_REPORT_LIMIT);
  }
}

function installVoplayPerfReloadEndpoint(server: ProxyServer) {
  server.middlewares.use(VOPLAY_PERF_RELOAD_ROUTE, (req: ProxyRequest, res: ProxyResponse) => {
    const method = (req.method ?? 'GET').toUpperCase();
    if (method !== 'GET' && method !== 'POST') {
      writeText(res, 405, 'method not allowed');
      return;
    }
    server.ws?.send({ type: 'full-reload', path: '*' });
    voplayPerfReports.push({
      receivedAt: new Date().toISOString(),
      payload: { schemaVersion: 1, source: 'studio-dev-server', kind: 'reload-request' },
    });
    if (voplayPerfReports.length > VOPLAY_PERF_REPORT_LIMIT) {
      voplayPerfReports.splice(0, voplayPerfReports.length - VOPLAY_PERF_REPORT_LIMIT);
    }
    writeJson(res, { ok: true, count: voplayPerfReports.length });
  });
}

function readRequestBody(req: ProxyRequest, maxBytes: number): Promise<string> {
  return new Promise((resolveBody, rejectBody) => {
    const chunks: Uint8Array[] = [];
    let total = 0;
    let rejected = false;
    req.on('data', (chunk) => {
      if (rejected) {
        return;
      }
      const bytes = typeof chunk === 'string' ? Buffer.from(chunk) : Buffer.from(chunk);
      total += bytes.byteLength;
      if (total > maxBytes) {
        rejected = true;
        chunks.length = 0;
        rejectBody(new Error('request body too large'));
        return;
      }
      chunks.push(bytes);
    });
    req.on('end', () => {
      if (rejected) {
        return;
      }
      resolveBody(Buffer.concat(chunks).toString('utf8'));
    });
    req.on('error', (error) => {
      if (rejected) {
        return;
      }
      rejected = true;
      chunks.length = 0;
      rejectBody(error);
    });
  });
}

function writeText(res: ProxyResponse, status: number, body: string) {
  res.writeHead(status, { 'content-type': 'text/plain; charset=utf-8' });
  res.end(body);
}

function writeJson(res: ProxyResponse, body: unknown, maxBytes?: number) {
  const json = JSON.stringify(body);
  if (maxBytes !== undefined && Buffer.byteLength(json, 'utf8') > maxBytes) {
    throw new Error(`JSON response exceeds the ${formatBytes(maxBytes)} limit`);
  }
  res.writeHead(200, {
    'content-type': 'application/json; charset=utf-8',
    'cache-control': 'no-store',
  });
  res.end(json);
}

function localProjectErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function readLocalProjectSnapshot(rawPath: string): LocalProjectSnapshot {
  const projectRoot = resolveLocalProjectRoot(stripFileUrl(rawPath));
  const plan = planLocalProjectSnapshot(projectRoot);
  const projectRelativePath = toPosix(relative(plan.snapshotBase, plan.projectRoot));
  const files: LocalProjectSnapshotFile[] = [];
  const seen = new Set<string>();
  const totals = {
    entries: 0,
    files: 0,
    bytes: 0,
    jsonBytes: Buffer.byteLength(JSON.stringify({
      projectPath: plan.projectRoot,
      projectRelativePath,
      files: [],
    }), 'utf8'),
  };
  for (const root of plan.roots) {
    collectSnapshotFiles(root, plan.snapshotBase, files, seen, totals);
  }
  for (const file of plan.files) {
    collectSnapshotFile(file, plan.snapshotBase, files, seen, totals);
  }
  if (files.length === 0) {
    throw new Error(`No Studio-readable files found under ${projectRoot}`);
  }
  files.sort((left, right) => compareUtf8(left.path, right.path));
  return {
    projectPath: plan.projectRoot,
    projectRelativePath,
    files,
  };
}

function planLocalProjectSnapshot(projectRoot: string): LocalProjectSnapshotPlan {
  const sources = planLocalProjectSources(projectRoot, {
    voBin: buildEnv.VO_BIN,
    environment: buildEnv,
    maxOutputBytes: LOCAL_PROJECT_MAX_METADATA_BYTES,
    maxRoots: LOCAL_PROJECT_MAX_ROOTS,
  });
  return {
    projectRoot,
    snapshotBase: commonAncestor([
      ...sources.roots.map((root) => dirname(root)),
      ...sources.files.map((file) => dirname(file)),
    ]),
    roots: sources.roots,
    files: sources.files,
  };
}

function readTextFileLimited(path: string, maxBytes: number): string {
  const bytes = readStableRegularFile(path, maxBytes, `metadata file ${path}`).bytes;
  return new TextDecoder('utf-8', { fatal: true }).decode(bytes);
}

function readStableRegularFile(
  path: string,
  maxBytes: number,
  label: string,
): { bytes: Buffer; mode: number } {
  if (!Number.isSafeInteger(maxBytes) || maxBytes < 0) {
    throw new Error(`invalid byte limit while reading ${label}`);
  }
  const noFollow = typeof fsConstants.O_NOFOLLOW === 'number' ? fsConstants.O_NOFOLLOW : 0;
  const descriptor = openSync(path, fsConstants.O_RDONLY | noFollow);
  try {
    const before = fstatSync(descriptor);
    if (!before.isFile()) {
      throw new Error(`${label} must be a regular file`);
    }
    if (!Number.isSafeInteger(before.size) || before.size < 0 || before.size > maxBytes) {
      throw new Error(`${label} exceeds the ${formatBytes(maxBytes)} limit`);
    }
    const bytes = Buffer.allocUnsafe(before.size);
    let offset = 0;
    while (offset < before.size) {
      const count = readSync(descriptor, bytes, offset, before.size - offset, null);
      if (count <= 0) {
        throw new Error(`${label} changed while Studio was reading it`);
      }
      offset += count;
    }
    const probe = Buffer.allocUnsafe(1);
    if (readSync(descriptor, probe, 0, 1, null) > 0) {
      throw new Error(`${label} exceeds the ${formatBytes(maxBytes)} limit or changed while Studio was reading it`);
    }
    const after = fstatSync(descriptor);
    if (
      before.dev !== after.dev
      || before.ino !== after.ino
      || before.size !== after.size
      || before.mtimeMs !== after.mtimeMs
      || before.ctimeMs !== after.ctimeMs
      || before.size !== offset
    ) {
      throw new Error(`${label} changed while Studio was reading it`);
    }
    if (bytes.byteLength > maxBytes) {
      throw new Error(`${label} exceeds the ${formatBytes(maxBytes)} limit`);
    }
    return { bytes, mode: after.mode & 0o777 };
  } finally {
    closeSync(descriptor);
  }
}

function commonAncestor(paths: string[]): string {
  if (paths.length === 0) {
    return ROOT_PATH;
  }
  const splitPaths = paths.map((path) => realpathSync(path).split(sep).filter(Boolean));
  const first = splitPaths[0];
  let length = first.length;
  for (const parts of splitPaths.slice(1)) {
    length = Math.min(length, parts.length);
    for (let index = 0; index < length; index++) {
      if (parts[index] !== first[index]) {
        length = index;
        break;
      }
    }
  }
  const prefix = first.slice(0, length).join(sep);
  return paths[0].startsWith(sep) ? `${sep}${prefix}` : prefix || ROOT_PATH;
}

function stripFileUrl(value: string): string {
  if (!value.startsWith('file://')) {
    return value;
  }
  return decodeURIComponent(new URL(value).pathname);
}

function resolveLocalProjectRoot(inputPath: string): string {
  const abs = realpathSync(isAbsolute(inputPath) ? inputPath : resolve(inputPath));
  const stat = statSync(abs);
  if (stat.isDirectory()) {
    return abs;
  }
  if (stat.isFile() && extname(abs) === '.vo') {
    return findNearestVoModuleRoot(dirname(abs)) ?? dirname(abs);
  }
  throw new Error(`Unsupported local project path: ${inputPath}`);
}

function findNearestVoModuleRoot(start: string): string | null {
  let current = start;
  while (true) {
    if (existsSync(join(current, 'vo.mod'))) {
      return current;
    }
    const parent = dirname(current);
    if (parent === current) {
      return null;
    }
    current = parent;
  }
}

function collectSnapshotFiles(
  root: string,
  snapshotBase: string,
  files: LocalProjectSnapshotFile[],
  seen: Set<string>,
  totals: { entries: number; files: number; bytes: number; jsonBytes: number },
) {
  const walk = (dir: string, depth: number) => {
    const entries = readdirSync(dir, { withFileTypes: true })
      .sort((left, right) => compareUtf8(left.name, right.name));
    for (const entry of entries) {
      totals.entries += 1;
      if (totals.entries > LOCAL_PROJECT_MAX_ENTRIES) {
        throw new Error(`local project snapshot exceeds the ${LOCAL_PROJECT_MAX_ENTRIES}-entry traversal limit`);
      }
      const abs = join(dir, entry.name);
      const relFromRoot = toPosix(relative(root, abs));
      if (entry.isDirectory()) {
        if (shouldSkipLocalProjectDir(entry.name, relFromRoot)) {
          continue;
        }
        if (depth >= LOCAL_PROJECT_MAX_DEPTH) {
          throw new Error(`local project snapshot exceeds the ${LOCAL_PROJECT_MAX_DEPTH}-level depth limit at ${abs}`);
        }
        walk(abs, depth + 1);
        continue;
      }
      if (!entry.isFile() || !shouldIncludeLocalProjectFile(entry.name, relFromRoot)) {
        continue;
      }
      const snapshotPath = toPosix(relative(snapshotBase, abs));
      if (!snapshotPath || snapshotPath.startsWith('../') || seen.has(snapshotPath)) {
        continue;
      }
      const remainingBytes = Math.max(0, LOCAL_PROJECT_MAX_BYTES - totals.bytes);
      const file = readStableRegularFile(
        abs,
        Math.min(LOCAL_PROJECT_MAX_FILE_BYTES, remainingBytes),
        `local project file ${abs}`,
      );
      addSnapshotFile(snapshotPath, file.bytes, file.mode, files, seen, totals);
      if (!relFromRoot.includes('/') && (entry.name.endsWith('.wasm') || entry.name.endsWith('.js'))) {
        addSnapshotFile(
          toPosix(relative(snapshotBase, join(root, 'artifacts', entry.name))),
          file.bytes,
          file.mode,
          files,
          seen,
          totals,
        );
      }
    }
  };
  walk(root, 0);
}

function collectSnapshotFile(
  abs: string,
  snapshotBase: string,
  files: LocalProjectSnapshotFile[],
  seen: Set<string>,
  totals: { entries: number; files: number; bytes: number; jsonBytes: number },
) {
  const snapshotPath = toPosix(relative(snapshotBase, abs));
  if (!snapshotPath || snapshotPath.startsWith('../') || seen.has(snapshotPath)) {
    return;
  }
  totals.entries += 1;
  if (totals.entries > LOCAL_PROJECT_MAX_ENTRIES) {
    throw new Error(`local project snapshot exceeds the ${LOCAL_PROJECT_MAX_ENTRIES}-entry traversal limit`);
  }
  const remainingBytes = Math.max(0, LOCAL_PROJECT_MAX_BYTES - totals.bytes);
  const file = readStableRegularFile(
    abs,
    Math.min(LOCAL_PROJECT_MAX_FILE_BYTES, remainingBytes),
    `local project metadata file ${abs}`,
  );
  addSnapshotFile(snapshotPath, file.bytes, file.mode, files, seen, totals);
}

function addSnapshotFile(
  path: string,
  bytes: Buffer,
  mode: number,
  files: LocalProjectSnapshotFile[],
  seen: Set<string>,
  totals: { entries: number; files: number; bytes: number; jsonBytes: number },
) {
  if (!path || path.startsWith('../') || seen.has(path)) {
    return;
  }
  validateLocalSnapshotPath(path);
  const nextFiles = totals.files + 1;
  const nextBytes = totals.bytes + bytes.length;
  const encodedLength = 4 * Math.ceil(bytes.length / 3);
  const entryJsonBytes = Buffer.byteLength(JSON.stringify({
    path,
    contentBase64: '',
    mode,
  }), 'utf8') + encodedLength;
  const nextJsonBytes = totals.jsonBytes + entryJsonBytes + (totals.files > 0 ? 1 : 0);
  if (!Number.isSafeInteger(nextBytes)) {
    throw new Error('local project snapshot byte accounting overflowed');
  }
  if (!Number.isSafeInteger(nextJsonBytes) || nextJsonBytes > LOCAL_PROJECT_MAX_RESPONSE_BYTES) {
    throw new Error(`local project snapshot exceeds the ${formatBytes(LOCAL_PROJECT_MAX_RESPONSE_BYTES)} response limit`);
  }
  totals.files = nextFiles;
  totals.bytes = nextBytes;
  totals.jsonBytes = nextJsonBytes;
  if (totals.files > LOCAL_PROJECT_MAX_FILES || totals.bytes > LOCAL_PROJECT_MAX_BYTES) {
    throw new Error(
      `local project snapshot is too large for Studio web local mode `
      + `(${totals.files}/${LOCAL_PROJECT_MAX_FILES} files, `
      + `${formatBytes(totals.bytes)}/${formatBytes(LOCAL_PROJECT_MAX_BYTES)})`,
    );
  }
  seen.add(path);
  files.push({
    path,
    contentBase64: Buffer.from(bytes).toString('base64'),
    mode,
  });
}

function validateLocalSnapshotPath(path: string): void {
  const encodedPath = Buffer.from(path, 'utf8');
  const parts = path.split('/');
  if (
    path.length === 0
    || path.startsWith('/')
    || path.endsWith('/')
    || path.includes('\\')
    || encodedPath.byteLength > LOCAL_PROJECT_MAX_PATH_BYTES
    || parts.length > LOCAL_PROJECT_MAX_DEPTH
    || parts.some((part) => (
      part.length === 0
      || part === '.'
      || part === '..'
      || Buffer.byteLength(part, 'utf8') > LOCAL_PROJECT_MAX_NAME_BYTES
      || /[\u0000-\u001f\u007f]/u.test(part)
    ))
  ) {
    throw new Error(`local project snapshot contains an unsupported path: ${JSON.stringify(path)}`);
  }
}

function shouldSkipLocalProjectDir(name: string, relFromRoot: string): boolean {
  if (LOCAL_PROJECT_SKIP_DIRS.has(name) || name.startsWith('.dist')) {
    return true;
  }
  return relFromRoot.split('/').some((part) => LOCAL_PROJECT_SKIP_DIRS.has(part));
}

function shouldIncludeLocalProjectFile(name: string, relFromRoot: string): boolean {
  if (name.endsWith('.vo')) {
    return true;
  }
  if (
    name === 'vo.mod'
    || name === 'vo.lock'
    || name === 'vo.release.json'
    || name === 'vo.package.json'
    || name === 'vo.work'
  ) {
    return true;
  }
  if (name.endsWith('.vpak')) {
    return true;
  }
  if (relFromRoot.startsWith('assets/') && /\.(png|jpe?g|webp|glb|gltf|bin)$/i.test(name)) {
    return true;
  }
  if (relFromRoot.startsWith('js/dist/')) {
    return true;
  }
  if (/^web-artifacts\/.+\.(wasm|js)$/.test(relFromRoot)) {
    return true;
  }
  if (/^rust\/pkg[^/]*\/.+\.(wasm|js)$/.test(relFromRoot)) {
    return true;
  }
  return !relFromRoot.includes('/') && (name.endsWith('.wasm') || name.endsWith('.js'));
}

function formatBytes(bytes: number): string {
  if (bytes >= 1024 * 1024 * 1024) {
    return `${(bytes / (1024 * 1024 * 1024)).toFixed(1)}GiB`;
  }
  if (bytes >= 1024 * 1024) {
    return `${(bytes / (1024 * 1024)).toFixed(1)}MiB`;
  }
  if (bytes >= 1024) {
    return `${(bytes / 1024).toFixed(1)}KiB`;
  }
  return `${bytes}B`;
}

function toPosix(path: string): string {
  return path.split(sep).filter(Boolean).join('/');
}

function studioManualChunks(id: string): string | undefined {
  const normalized = id.replace(/\\/g, '/');
  if (!normalized.includes('/node_modules/')) {
    return undefined;
  }
  if (normalized.includes('/monaco-editor/esm/')) {
    return 'vendor-monaco';
  }
  if (normalized.includes('/golden-layout/')) {
    return 'vendor-layout';
  }
  if (normalized.includes('/marked/') || normalized.includes('/highlight.js/')) {
    return 'vendor-docs';
  }
  if (normalized.includes('/@tauri-apps/')) {
    return 'vendor-tauri';
  }
  if (normalized.includes('/svelte/')) {
    return 'vendor-svelte';
  }
  return 'vendor';
}

function readStudioWasmBuildId(): string | null {
  const buildIdPath = resolve('public/wasm/vo_studio_wasm.build_id');
  if (!existsSync(buildIdPath)) {
    return null;
  }
  const value = readTextFileLimited(buildIdPath, 4096).trim();
  if (!/^[A-Za-z0-9._-]{1,256}$/.test(value)) {
    throw new Error('Studio WASM build ID contains unsupported characters');
  }
  return value;
}

const studioWasmBuildId = readStudioWasmBuildId();
const studioBuildId = studioWasmBuildId
  ? validateStudioWasmBuildId(studioWasmBuildId, buildEnv, { studioRoot: resolve() })
  : resolveSharedStudioBuildId(buildEnv, { studioRoot: resolve() });

export default defineConfig({
  plugins: [localProjectSnapshot(), voplayPerfReportEndpoint(), svelte({ preprocess: vitePreprocess() })],
  server: {
    host: '127.0.0.1',
    port: 5174,
    strictPort: true,
    fs: {
      // Studio's renderer VFS reuses the repository's vo-web VFS core.
      allow: [resolve('../..')],
    },
  },
  define: {
    __STUDIO_BUILD_ID__: JSON.stringify(studioBuildId),
  },
  build: {
    outDir: 'dist',
    target: ['es2020', 'chrome105'],
    chunkSizeWarningLimit: 2800,
    rollupOptions: {
      output: {
        manualChunks: studioManualChunks,
      },
    },
  },
  // Allow .wasm files from public/ to be served as assets
  assetsInclude: ['**/*.wasm'],
  optimizeDeps: {
    // studio WASM module is loaded at runtime from public/wasm/, not bundled
    exclude: ['vo_studio_wasm'],
  },
});
