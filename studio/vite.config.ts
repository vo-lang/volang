import { Buffer } from 'node:buffer';
import { existsSync, readFileSync, readdirSync, realpathSync, statSync } from 'node:fs';
import { dirname, extname, isAbsolute, join, relative, resolve, sep } from 'node:path';
import { defineConfig } from 'vite';
import { svelte, vitePreprocess } from '@sveltejs/vite-plugin-svelte';

interface ProxyRequest {
  url?: string;
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

interface UpstreamHeaders {
  get(name: string): string | null;
}

interface UpstreamResponse {
  status: number;
  headers: UpstreamHeaders;
  arrayBuffer(): Promise<ArrayBuffer>;
}

function writeProxyResponseHeaders(res: ProxyResponse, upstream: UpstreamResponse) {
  const headers: Record<string, string> = {
    'content-type': upstream.headers.get('content-type') ?? 'application/octet-stream',
    'access-control-allow-origin': '*',
  };
  const contentLength = upstream.headers.get('content-length');
  if (contentLength !== null) {
    headers['content-length'] = contentLength;
  }
  res.writeHead(upstream.status, headers);
}

function proxyErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

async function proxyGhReleaseRequest(req: ProxyRequest, res: ProxyResponse): Promise<void> {
  const target = `https://github.com${req.url ?? ''}`;
  try {
    const upstream = await (globalThis as typeof globalThis & {
      fetch(url: string, init: { redirect: 'follow' }): Promise<UpstreamResponse>;
    }).fetch(target, { redirect: 'follow' });
    writeProxyResponseHeaders(res, upstream);
    const buf = new Uint8Array(await upstream.arrayBuffer());
    res.end(buf);
  } catch (err) {
    res.writeHead(502, { 'content-type': 'text/plain; charset=utf-8' });
    res.end(`gh-release proxy error: ${proxyErrorMessage(err)}`);
  }
}

function installGhReleaseProxy(middlewares: ProxyMiddlewares) {
  middlewares.use('/gh-release/', (req: ProxyRequest, res: ProxyResponse) => {
    void proxyGhReleaseRequest(req, res);
  });
}

function ghReleaseProxy() {
  return {
    name: 'gh-release-proxy',
    configureServer(server: ProxyServer) {
      installGhReleaseProxy(server.middlewares);
    },
    configurePreviewServer(server: ProxyServer) {
      installGhReleaseProxy(server.middlewares);
    },
  };
}

const LOCAL_PROJECT_ROUTE = '/__vo_studio_local_project';
const LOCAL_PROJECT_MAX_FILES = 5000;
const LOCAL_PROJECT_MAX_BYTES = 64 * 1024 * 1024;
const LOCAL_PROJECT_SKIP_DIRS = new Set([
  '.git',
  '.hg',
  '.svn',
  '.cache',
  '.turbo',
  'node_modules',
  'target',
]);

function localProjectSnapshotsEnabled(): boolean {
  return buildEnv.VITE_STUDIO_LOCAL_PROJECTS === '1';
}

function localProjectSnapshot() {
  return {
    name: 'studio-local-project-snapshot',
    configureServer(server: ProxyServer) {
      installLocalProjectSnapshot(server.middlewares);
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
      writeJson(res, readLocalProjectSnapshot(rawPath));
    } catch (error) {
      writeText(res, 500, localProjectErrorMessage(error));
    }
  });
}

function writeText(res: ProxyResponse, status: number, body: string) {
  res.writeHead(status, { 'content-type': 'text/plain; charset=utf-8' });
  res.end(body);
}

function writeJson(res: ProxyResponse, body: LocalProjectSnapshot) {
  res.writeHead(200, {
    'content-type': 'application/json; charset=utf-8',
    'cache-control': 'no-store',
  });
  res.end(JSON.stringify(body));
}

function localProjectErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function readLocalProjectSnapshot(rawPath: string): LocalProjectSnapshot {
  const projectRoot = resolveLocalProjectRoot(stripFileUrl(rawPath));
  const snapshotBase = dirname(projectRoot);
  const files: LocalProjectSnapshotFile[] = [];
  const seen = new Set<string>();
  const totals = { files: 0, bytes: 0 };
  collectSnapshotFiles(projectRoot, snapshotBase, files, seen, totals);
  if (files.length === 0) {
    throw new Error(`No Studio-readable files found under ${projectRoot}`);
  }
  return {
    projectPath: projectRoot,
    projectRelativePath: toPosix(relative(snapshotBase, projectRoot)),
    files,
  };
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
  totals: { files: number; bytes: number },
) {
  const walk = (dir: string) => {
    for (const entry of readdirSync(dir, { withFileTypes: true })) {
      const abs = join(dir, entry.name);
      const relFromRoot = toPosix(relative(root, abs));
      if (entry.isDirectory()) {
        if (shouldSkipLocalProjectDir(entry.name, relFromRoot)) {
          continue;
        }
        walk(abs);
        continue;
      }
      if (!entry.isFile() || !shouldIncludeLocalProjectFile(entry.name, relFromRoot)) {
        continue;
      }
      const snapshotPath = toPosix(relative(snapshotBase, abs));
      if (!snapshotPath || snapshotPath.startsWith('../') || seen.has(snapshotPath)) {
        continue;
      }
      const bytes = readFileSync(abs);
      const mode = statSync(abs).mode & 0o777;
      addSnapshotFile(snapshotPath, bytes, mode, files, seen, totals);
      if (!relFromRoot.includes('/') && (entry.name.endsWith('.wasm') || entry.name.endsWith('.js'))) {
        addSnapshotFile(
          toPosix(relative(snapshotBase, join(root, 'artifacts', entry.name))),
          bytes,
          mode,
          files,
          seen,
          totals,
        );
      }
    }
  };
  walk(root);
}

function addSnapshotFile(
  path: string,
  bytes: Buffer,
  mode: number,
  files: LocalProjectSnapshotFile[],
  seen: Set<string>,
  totals: { files: number; bytes: number },
) {
  if (!path || path.startsWith('../') || seen.has(path)) {
    return;
  }
  totals.files += 1;
  totals.bytes += bytes.length;
  if (totals.files > LOCAL_PROJECT_MAX_FILES || totals.bytes > LOCAL_PROJECT_MAX_BYTES) {
    throw new Error('local project snapshot is too large for Studio web local mode');
  }
  seen.add(path);
  files.push({
    path,
    contentBase64: Buffer.from(bytes).toString('base64'),
    mode,
  });
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
  if (name === 'vo.mod' || name === 'vo.lock' || name === 'vo.ext.toml') {
    return true;
  }
  if (relFromRoot.startsWith('js/dist/')) {
    return true;
  }
  if (/^rust\/pkg[^/]*\/.+\.(wasm|js)$/.test(relFromRoot)) {
    return true;
  }
  return !relFromRoot.includes('/') && (name.endsWith('.wasm') || name.endsWith('.js'));
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
  if (normalized.includes('/@xterm/')) {
    return 'vendor-terminal';
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

const buildEnv = (globalThis as typeof globalThis & {
  process?: { env?: Record<string, string | undefined> };
}).process?.env ?? {};

function readStudioWasmBuildId(): string | null {
  try {
    const value = readFileSync(resolve('public/wasm/vo_studio_wasm.build_id'), 'utf8').trim();
    return value.length > 0 ? value : null;
  } catch {
    return null;
  }
}

const studioBuildId = readStudioWasmBuildId()
  || [
    buildEnv.GITHUB_SHA,
    buildEnv.GITHUB_RUN_ID,
    buildEnv.GITHUB_RUN_ATTEMPT,
  ].filter((value): value is string => typeof value === 'string' && value.length > 0).join('-')
  || Date.now().toString(36);

export default defineConfig({
  plugins: [localProjectSnapshot(), ghReleaseProxy(), svelte({ preprocess: vitePreprocess() })],
  server: {
    port: 5174,
    strictPort: true,
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
