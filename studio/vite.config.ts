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
const LOCAL_PROJECT_MAX_FILES = readPositiveIntEnv('VITE_STUDIO_LOCAL_PROJECT_MAX_FILES', 5000);
const LOCAL_PROJECT_MAX_BYTES = readByteSizeEnv('VITE_STUDIO_LOCAL_PROJECT_MAX_BYTES', 512 * 1024 * 1024);
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

function readPositiveIntEnv(name: string, fallback: number): number {
  const raw = buildEnv[name]?.trim();
  if (!raw) {
    return fallback;
  }
  const parsed = Number.parseInt(raw, 10);
  return Number.isFinite(parsed) && parsed > 0 ? parsed : fallback;
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
  return Math.floor(value * multiplier);
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
  const plan = planLocalProjectSnapshot(projectRoot);
  const files: LocalProjectSnapshotFile[] = [];
  const seen = new Set<string>();
  const totals = { files: 0, bytes: 0 };
  for (const root of plan.roots) {
    collectSnapshotFiles(root, plan.snapshotBase, files, seen, totals);
  }
  for (const file of plan.files) {
    collectSnapshotFile(file, plan.snapshotBase, files, seen, totals);
  }
  if (files.length === 0) {
    throw new Error(`No Studio-readable files found under ${projectRoot}`);
  }
  return {
    projectPath: plan.projectRoot,
    projectRelativePath: toPosix(relative(plan.snapshotBase, plan.projectRoot)),
    files,
  };
}

function planLocalProjectSnapshot(projectRoot: string): LocalProjectSnapshotPlan {
  const roots = new Map<string, string>();
  const extraFiles = new Map<string, string>();
  const queue = [projectRoot];

  for (let index = 0; index < queue.length; index++) {
    const root = queue[index];
    if (roots.has(root)) {
      continue;
    }
    roots.set(root, root);
    for (const workPath of localVoWorkCandidates(root)) {
      if (existsSync(workPath)) {
        const realWorkPath = realpathSync(workPath);
        if (!isPathWithin(realWorkPath, root)) {
          extraFiles.set(realWorkPath, realWorkPath);
        }
      }
    }
    for (const moduleRoot of resolveVoWorkModuleRoots(root)) {
      if (!roots.has(moduleRoot)) {
        queue.push(moduleRoot);
      }
    }
  }

  const rootList = [...roots.values()];
  const fileList = [...extraFiles.values()];
  return {
    projectRoot,
    snapshotBase: commonAncestor([
      ...rootList.map((root) => dirname(root)),
      ...fileList.map((file) => dirname(file)),
    ]),
    roots: rootList,
    files: fileList,
  };
}

function resolveVoWorkModuleRoots(moduleRoot: string): string[] {
  const roots: string[] = [];
  for (const workPath of localVoWorkCandidates(moduleRoot)) {
    if (!existsSync(workPath)) {
      continue;
    }
    const workDir = dirname(workPath);
    for (const usePath of parseVoWorkUsePaths(readFileSync(workPath, 'utf8'))) {
      const resolved = resolve(workDir, usePath);
      if (!existsSync(resolved)) {
        continue;
      }
      const real = realpathSync(resolved);
      if (!statSync(real).isDirectory() || !existsSync(join(real, 'vo.mod'))) {
        continue;
      }
      roots.push(real);
    }
  }
  return roots;
}

function localVoWorkCandidates(moduleRoot: string): string[] {
  const candidates = [
    join(moduleRoot, 'vo.work'),
    join(dirname(moduleRoot), 'vo.work'),
  ];
  return [...new Set(candidates.map((path) => realpathIfExists(path) ?? path))];
}

function realpathIfExists(path: string): string | null {
  try {
    return realpathSync(path);
  } catch {
    return null;
  }
}

function parseVoWorkUsePaths(content: string): string[] {
  const paths: string[] = [];
  let inUseTable = false;
  for (const rawLine of content.split(/\r?\n/)) {
    const line = stripTomlComment(rawLine).trim();
    if (!line) {
      continue;
    }
    if (/^\[\[use\]\]$/.test(line)) {
      inUseTable = true;
      continue;
    }
    if (/^\[/.test(line)) {
      inUseTable = false;
      continue;
    }
    if (!inUseTable) {
      continue;
    }
    const match = /^path\s*=\s*(['"])(.*)\1\s*$/.exec(line);
    if (match) {
      paths.push(unescapeTomlString(match[2], match[1]));
    }
  }
  return paths;
}

function stripTomlComment(line: string): string {
  let quote: string | null = null;
  let escaped = false;
  for (let index = 0; index < line.length; index++) {
    const ch = line[index];
    if (quote) {
      if (quote === '"' && ch === '\\' && !escaped) {
        escaped = true;
        continue;
      }
      if (ch === quote && !escaped) {
        quote = null;
      }
      escaped = false;
      continue;
    }
    if (ch === '"' || ch === "'") {
      quote = ch;
      continue;
    }
    if (ch === '#') {
      return line.slice(0, index);
    }
  }
  return line;
}

function unescapeTomlString(value: string, quote: string): string {
  if (quote === "'") {
    return value;
  }
  return value
    .replace(/\\n/g, '\n')
    .replace(/\\r/g, '\r')
    .replace(/\\t/g, '\t')
    .replace(/\\"/g, '"')
    .replace(/\\\\/g, '\\');
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

function isPathWithin(path: string, root: string): boolean {
  const rel = relative(root, path);
  return rel === '' || (!rel.startsWith('..') && !isAbsolute(rel));
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

function collectSnapshotFile(
  abs: string,
  snapshotBase: string,
  files: LocalProjectSnapshotFile[],
  seen: Set<string>,
  totals: { files: number; bytes: number },
) {
  const snapshotPath = toPosix(relative(snapshotBase, abs));
  if (!snapshotPath || snapshotPath.startsWith('../') || seen.has(snapshotPath)) {
    return;
  }
  const bytes = readFileSync(abs);
  const mode = statSync(abs).mode & 0o777;
  addSnapshotFile(snapshotPath, bytes, mode, files, seen, totals);
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
  if (name === 'vo.mod' || name === 'vo.lock' || name === 'vo.web.json' || name === 'vo.work') {
    return true;
  }
  if (name.endsWith('.vpak')) {
    return true;
  }
  if (relFromRoot.startsWith('assets/') && /\.(png|jpe?g|webp)$/i.test(name)) {
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
  plugins: [localProjectSnapshot(), svelte({ preprocess: vitePreprocess() })],
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
