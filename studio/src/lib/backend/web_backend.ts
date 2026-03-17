import type { Backend } from './backend';
import type {
  BootstrapContext,
  BuildResult,
  CheckResult,
  CompileResult,
  DiscoveredProject,
  FsEntry,
  FsStat,
  GitOp,
  GitResult,
  GrepMatch,
  GrepOpts,
  GuiRunOutput,
  HttpOpts,
  HttpResult,
  InstallEvent,
  InstalledModule,
  ProcEvent,
  ReadManyResult,
  RenderIslandVfsSnapshot,
  RunEvent,
  RunOpts,
  SessionInfo,
  StreamHandle,
} from '../types';
import { makeErrorStreamHandle, makeResolvedStreamHandle } from './stream_handle';

const WORKSPACE_ROOT = '/workspace';
const ROOT = '/';
const URL_SESSION_ROOT = `${WORKSPACE_ROOT}/.studio-sessions/url`;
const defaultWorkspaceFiles = new Map<string, string>([
  [`${WORKSPACE_ROOT}/README.md`, '# Studio\n\nThe web backend is running in an in-memory workspace.\n'],
  [`${WORKSPACE_ROOT}/main.vo`, 'package main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Studio rewrite bootstrap")\n}\n'],
]);
const files = new Map<string, string>();
const directories = new Set<string>();

resetWorkspaceState();

export class WebBackend implements Backend {
  readonly platform = 'wasm' as const;

  async getBootstrapContext(): Promise<BootstrapContext> {
    const params = new URLSearchParams(window.location.search);
    const rawMode = params.get('mode');
    const mode: import('../types').StudioMode = rawMode === 'runner' ? 'runner' : 'dev';
    const initialUrl = mode === 'runner'
      ? params.get('url')
      : params.get('project') ?? params.get('url');
    const launchUrl = window.location.href.includes('?') ? window.location.href : null;
    return {
      workspaceRoot: WORKSPACE_ROOT,
      launchUrl,
      initialPath: null,
      initialUrl,
      initialRunTarget: null,
      mode,
      platform: 'wasm',
    };
  }

  async openWorkspaceSession(): Promise<SessionInfo> {
    if (!directories.has(WORKSPACE_ROOT)) {
      resetWorkspaceState();
    }
    return buildSessionInfo(WORKSPACE_ROOT, 'workspace');
  }

  async openRunSession(_path: string): Promise<SessionInfo> {
    throw new Error('Run sessions are not available in web mode');
  }

  async openUrlSession(url: string): Promise<SessionInfo> {
    const importedRoot = await importProjectFromUrl(url);
    return buildSessionInfo(importedRoot, 'url');
  }

  async discoverProjects(root: string): Promise<DiscoveredProject[]> {
    const entries = await this.listDir(root);
    const projects: DiscoveredProject[] = [];
    for (const entry of entries) {
      if (entry.name.startsWith('.')) continue;
      if (!entry.isDir && entry.name.endsWith('.vo')) {
        projects.push({ name: entry.name.slice(0, -3), type: 'single', localPath: entry.path, entryPath: entry.path });
      } else if (entry.isDir) {
        try {
          const children = await this.listDir(entry.path);
          if (children.some((c) => !c.isDir && c.name === 'vo.mod')) {
            const ep = children.some((c) => !c.isDir && c.name === 'main.vo') ? `${entry.path}/main.vo`
              : children.some((c) => !c.isDir && c.name === 'app.vo') ? `${entry.path}/app.vo`
              : `${entry.path}/vo.mod`;
            projects.push({ name: entry.name, type: 'module', localPath: entry.path, entryPath: ep });
          }
        } catch { /* skip */ }
      }
    }
    return projects;
  }

  async listDir(path: string): Promise<FsEntry[]> {
    const normalized = normalizePath(path);
    if (!directories.has(normalized)) throw new Error(`Directory not found: ${normalized}`);
    return listDirEntries(normalized);
  }

  async statPath(path: string): Promise<FsStat> {
    const normalized = normalizePath(path);
    const isDir = directories.has(normalized);
    const content = files.get(normalized);
    if (!isDir && content === undefined) throw new Error(`Path not found: ${normalized}`);
    return { path: normalized, isDir, isFile: !isDir, size: content?.length ?? 0, modifiedMs: 0 };
  }

  async readFile(path: string): Promise<string> {
    const normalized = normalizePath(path);
    const content = files.get(normalized);
    if (content === undefined) throw new Error(`File not found: ${normalized}`);
    return content;
  }

  async readMany(paths: string[]): Promise<ReadManyResult[]> {
    return paths.map((path) => {
      const normalized = normalizePath(path);
      const content = files.get(normalized);
      return content !== undefined
        ? { path: normalized, content, error: null }
        : { path, content: null, error: `File not found: ${path}` };
    });
  }

  async writeFile(path: string, content: string): Promise<void> {
    const normalized = normalizePath(path);
    setFile(normalized, content);
  }

  async mkdir(path: string): Promise<void> {
    ensureDir(normalizePath(path));
  }

  async removeEntry(path: string, recursive: boolean): Promise<void> {
    const normalized = normalizePath(path);
    if (files.has(normalized)) {
      files.delete(normalized);
      return;
    }
    if (!directories.has(normalized)) return;
    const children = listDirEntries(normalized);
    if (children.length > 0 && !recursive) {
      throw new Error(`Directory not empty: ${normalized}`);
    }
    const descendants = [...directories].filter((dir) => dir === normalized || dir.startsWith(`${normalized}/`));
    for (const filePath of [...files.keys()]) {
      if (filePath.startsWith(`${normalized}/`)) {
        files.delete(filePath);
      }
    }
    for (const dir of descendants.sort((a, b) => b.length - a.length)) {
      if (dir !== ROOT && dir !== WORKSPACE_ROOT) {
        directories.delete(dir);
      }
    }
  }

  async renameEntry(oldPath: string, newPath: string): Promise<void> {
    const oldNorm = normalizePath(oldPath);
    const newNorm = normalizePath(newPath);
    const content = files.get(oldNorm);
    if (content !== undefined) {
      files.delete(oldNorm);
      setFile(newNorm, content);
      return;
    }
    if (!directories.has(oldNorm)) {
      throw new Error(`Path not found: ${oldNorm}`);
    }
    ensureDir(newNorm);
    const dirMoves = [...directories]
      .filter((dir) => dir === oldNorm || dir.startsWith(`${oldNorm}/`))
      .sort((a, b) => a.length - b.length);
    const fileMoves = [...files.entries()]
      .filter(([filePath]) => filePath.startsWith(`${oldNorm}/`));
    for (const [filePath] of fileMoves) {
      files.delete(filePath);
    }
    for (const dir of dirMoves.sort((a, b) => b.length - a.length)) {
      directories.delete(dir);
    }
    for (const dir of dirMoves) {
      const moved = dir === oldNorm ? newNorm : `${newNorm}${dir.slice(oldNorm.length)}`;
      ensureDir(moved);
    }
    for (const [filePath, fileContent] of fileMoves) {
      const moved = `${newNorm}${filePath.slice(oldNorm.length)}`;
      setFile(moved, fileContent);
    }
  }

  async copyEntry(src: string, dst: string): Promise<void> {
    const srcNorm = normalizePath(src);
    const dstNorm = normalizePath(dst);
    const content = files.get(srcNorm);
    if (content !== undefined) {
      setFile(dstNorm, content);
      return;
    }
    if (!directories.has(srcNorm)) {
      throw new Error(`Path not found: ${srcNorm}`);
    }
    ensureDir(dstNorm);
    for (const dir of [...directories].filter((dir) => dir.startsWith(`${srcNorm}/`)).sort((a, b) => a.length - b.length)) {
      ensureDir(`${dstNorm}${dir.slice(srcNorm.length)}`);
    }
    for (const [filePath, fileContent] of files) {
      if (filePath.startsWith(`${srcNorm}/`)) {
        setFile(`${dstNorm}${filePath.slice(srcNorm.length)}`, fileContent);
      }
    }
  }

  async grep(path: string, pattern: string, opts?: GrepOpts): Promise<GrepMatch[]> {
    const results: GrepMatch[] = [];
    const maxResults = opts?.maxResults ?? 500;
    const caseSensitive = opts?.caseSensitive ?? false;
    const patternCmp = caseSensitive ? pattern : pattern.toLowerCase();
    for (const [filePath, content] of files) {
      if (!filePath.startsWith(normalizePath(path))) continue;
      content.split('\n').forEach((line, idx) => {
        if (results.length >= maxResults) return;
        const lineCmp = caseSensitive ? line : line.toLowerCase();
        const col = lineCmp.indexOf(patternCmp);
        if (col >= 0) results.push({ path: filePath, line: idx + 1, column: col, text: line });
      });
    }
    return results;
  }

  async checkVo(_path: string): Promise<CheckResult> {
    throw new Error('vo check is not wired in WASM mode yet');
  }

  async compileVo(_path: string): Promise<CompileResult> {
    throw new Error('vo compile is not wired in WASM mode yet');
  }

  async formatVo(_path: string): Promise<string> {
    throw new Error('vo format is not wired in WASM mode yet');
  }

  async buildVo(_path: string, _output?: string): Promise<BuildResult> {
    throw new Error('vo build is not wired in WASM mode yet');
  }

  async dumpVo(_path: string): Promise<string> {
    throw new Error('vo dump is not wired in WASM mode yet');
  }

  runVo(_path: string, _opts?: RunOpts): StreamHandle<RunEvent> {
    return makeErrorStreamHandle<RunEvent>('vo run is not wired in WASM mode yet');
  }

  async stopVoRun(): Promise<void> {
    return;
  }

  async runGui(_path: string): Promise<GuiRunOutput> {
    throw new Error('GUI runtime is not wired in WASM mode yet');
  }

  async sendGuiEvent(_handlerId: number, _payload: string): Promise<Uint8Array> {
    throw new Error('GUI runtime is not wired in WASM mode yet');
  }

  async sendGuiEventAsync(_handlerId: number, _payload: string): Promise<void> {
    throw new Error('GUI runtime is not wired in WASM mode yet');
  }

  async pushIslandTransport(_data: Uint8Array): Promise<void> {
    throw new Error('Render-island transport is not wired in WASM mode yet');
  }

  async pollGuiRender(): Promise<Uint8Array> {
    throw new Error('GUI runtime is not wired in WASM mode yet');
  }

  async stopGui(): Promise<void> {
    throw new Error('GUI runtime is not wired in WASM mode yet');
  }

  async getRenderIslandVfsSnapshot(_path: string): Promise<RenderIslandVfsSnapshot> {
    throw new Error('Render-island VFS is not wired in WASM mode yet');
  }

  voGet(_spec: string): StreamHandle<InstallEvent> {
    return makeErrorStreamHandle<InstallEvent>('vo get is not wired in WASM mode yet');
  }

  async voInit(_path: string, _name?: string): Promise<string> {
    throw new Error('vo init is not wired in WASM mode yet');
  }

  async voVersion(): Promise<string> {
    return 'wasm';
  }

  async listInstalledModules(): Promise<InstalledModule[]> {
    return [];
  }

  spawnProcess(_program: string, _args: string[], _cwd?: string, _env?: Record<string, string>): StreamHandle<ProcEvent> {
    return makeErrorStreamHandle<ProcEvent>('process spawn is not available in WASM mode');
  }

  async httpRequest(method: string, url: string, opts?: HttpOpts): Promise<HttpResult> {
    const fetchOpts: RequestInit = {
      method,
      headers: opts?.headers as Record<string, string> | undefined,
      body: opts?.body,
    };
    const response = await fetch(url, fetchOpts);
    const body = await response.text();
    const headers: Record<string, string> = {};
    response.headers.forEach((value, key) => { headers[key] = value; });
    return { status: response.status, headers, body };
  }

  async gitExec(_op: GitOp): Promise<GitResult> {
    throw new Error('git operations are not available in WASM mode');
  }
}

function normalizePath(path: string): string {
  const trimmed = path.trim();
  if (!trimmed || trimmed === '.') {
    return WORKSPACE_ROOT;
  }
  if (trimmed === ROOT) {
    return ROOT;
  }
  const absolute = trimmed.startsWith('/') ? trimmed : `/${trimmed}`;
  return absolute.endsWith('/') && absolute.length > 1 ? absolute.slice(0, -1) : absolute;
}

function sortEntries(entries: FsEntry[]): FsEntry[] {
  return [...entries].sort((a, b) => {
    if (a.isDir !== b.isDir) {
      return a.isDir ? -1 : 1;
    }
    return a.name.localeCompare(b.name);
  });
}

function resetWorkspaceState(): void {
  files.clear();
  directories.clear();
  directories.add(ROOT);
  ensureDir(WORKSPACE_ROOT);
  ensureDir(URL_SESSION_ROOT);
  for (const [path, content] of defaultWorkspaceFiles) {
    setFile(path, content);
  }
}

function ensureDir(path: string): void {
  const normalized = normalizePath(path);
  const parts = normalized.split('/').filter(Boolean);
  let current = ROOT;
  directories.add(ROOT);
  for (const part of parts) {
    current = current === ROOT ? `/${part}` : `${current}/${part}`;
    directories.add(current);
  }
}

function setFile(path: string, content: string): void {
  const normalized = normalizePath(path);
  const parent = dirname(normalized);
  ensureDir(parent);
  files.set(normalized, content);
}

function dirname(path: string): string {
  const normalized = normalizePath(path);
  const index = normalized.lastIndexOf('/');
  if (index <= 0) return ROOT;
  return normalized.slice(0, index);
}

function listDirEntries(path: string): FsEntry[] {
  const normalized = normalizePath(path);
  const entries = new Map<string, FsEntry>();
  for (const dir of directories) {
    if (dir === normalized || dirname(dir) !== normalized) continue;
    const name = dir.slice(dir.lastIndexOf('/') + 1);
    entries.set(name, { name, path: dir, isDir: true });
  }
  for (const [filePath, content] of files) {
    if (dirname(filePath) !== normalized) continue;
    const name = filePath.slice(filePath.lastIndexOf('/') + 1);
    entries.set(name, { name, path: filePath, isDir: false, size: content.length });
  }
  return sortEntries([...entries.values()]);
}

function buildSessionInfo(root: string, origin: SessionInfo['origin']): SessionInfo {
  const normalizedRoot = normalizePath(root);
  const entryPath = detectEntryPath(normalizedRoot);
  return {
    root: normalizedRoot,
    origin,
    projectMode: files.has(`${normalizedRoot}/vo.mod`) ? 'module' : 'single-file',
    entryPath,
    singleFileRun: false,
  };
}

function detectEntryPath(root: string): string | null {
  const main = `${root}/main.vo`;
  if (files.has(main)) return main;
  const first = [...files.keys()]
    .filter((path) => path.startsWith(`${root}/`) && path.endsWith('.vo'))
    .sort()[0];
  return first ?? null;
}

async function importProjectFromUrl(url: string): Promise<string> {
  const response = await fetch(url);
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${url}`);
  }
  const sessionRoot = buildImportedRoot(url);
  await clearImportedRoot(sessionRoot);
  ensureDir(sessionRoot);
  const bytes = new Uint8Array(await response.arrayBuffer());
  if (looksLikeTarGz(url, bytes)) {
    const extracted = await extractTarGzTextFiles(bytes);
    if (extracted.length === 0) {
      throw new Error('archive contains no UTF-8 files');
    }
    const prefix = sharedPrefix(extracted.map((file) => file.path));
    for (const file of extracted) {
      const relative = prefix && file.path.startsWith(`${prefix}/`)
        ? file.path.slice(prefix.length + 1)
        : file.path;
      if (!relative) continue;
      setFile(`${sessionRoot}/${relative}`, file.content);
    }
  } else {
    const decoder = new TextDecoder('utf-8', { fatal: true });
    const content = decoder.decode(bytes);
    let fileName = fileNameFromUrl(url) ?? 'main.vo';
    if (!fileName.includes('.')) {
      fileName += '.vo';
    }
    setFile(`${sessionRoot}/${fileName}`, content);
  }
  return sessionRoot;
}

async function clearImportedRoot(root: string): Promise<void> {
  for (const filePath of [...files.keys()]) {
    if (filePath.startsWith(`${root}/`)) {
      files.delete(filePath);
    }
  }
  for (const dir of [...directories].sort((a, b) => b.length - a.length)) {
    if (dir.startsWith(`${root}/`) || dir === root) {
      directories.delete(dir);
    }
  }
}

function buildImportedRoot(url: string): string {
  return `${URL_SESSION_ROOT}/${sanitizeSlug(fileNameFromUrl(url) ?? 'project')}-${hashString(url)}`;
}

function sanitizeSlug(input: string): string {
  const stem = input
    .replace(/\.tar\.gz$/i, '')
    .replace(/\.tgz$/i, '')
    .replace(/\.zip$/i, '')
    .replace(/\.vo$/i, '');
  const slug = stem
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '');
  return slug || 'project';
}

function hashString(input: string): string {
  let hash = 2166136261;
  for (let index = 0; index < input.length; index += 1) {
    hash ^= input.charCodeAt(index);
    hash = Math.imul(hash, 16777619);
  }
  return (hash >>> 0).toString(16).padStart(8, '0');
}

function fileNameFromUrl(url: string): string | null {
  const pathname = new URL(url, window.location.href).pathname;
  const name = pathname.split('/').pop() ?? '';
  return name || null;
}

function looksLikeTarGz(url: string, bytes: Uint8Array): boolean {
  const lowered = url.toLowerCase();
  return lowered.endsWith('.tar.gz') || lowered.endsWith('.tgz') || (bytes[0] === 0x1f && bytes[1] === 0x8b);
}

async function extractTarGzTextFiles(bytes: Uint8Array): Promise<Array<{ path: string; content: string }>> {
  const ab = new ArrayBuffer(bytes.byteLength);
  new Uint8Array(ab).set(bytes);
  const stream = new Response(ab).body;
  if (!stream || typeof DecompressionStream === 'undefined') {
    throw new Error('gzip archive import is not supported in this browser');
  }
  const decompressed = stream.pipeThrough(new DecompressionStream('gzip'));
  const tarBytes = new Uint8Array(await new Response(decompressed).arrayBuffer());
  return parseTarTextFiles(tarBytes);
}

function parseTarTextFiles(bytes: Uint8Array): Array<{ path: string; content: string }> {
  const decoder = new TextDecoder();
  const utf8 = new TextDecoder('utf-8', { fatal: true });
  const filesOut: Array<{ path: string; content: string }> = [];
  let offset = 0;
  while (offset + 512 <= bytes.length) {
    const header = bytes.subarray(offset, offset + 512);
    if (header.every((value) => value === 0)) break;
    const name = readTarString(decoder, header, 0, 100);
    const prefix = readTarString(decoder, header, 345, 155);
    const typeFlag = header[156];
    const size = parseTarSize(readTarString(decoder, header, 124, 12));
    const path = sanitizeTarPath(prefix ? `${prefix}/${name}` : name);
    const bodyStart = offset + 512;
    const bodyEnd = bodyStart + size;
    if (path && (typeFlag === 0 || typeFlag === 48)) {
      try {
        filesOut.push({ path, content: utf8.decode(bytes.subarray(bodyStart, bodyEnd)) });
      } catch {}
    }
    offset = bodyStart + Math.ceil(size / 512) * 512;
  }
  return filesOut;
}

function readTarString(decoder: TextDecoder, header: Uint8Array, start: number, length: number): string {
  const slice = header.subarray(start, start + length);
  const zeroIndex = slice.indexOf(0);
  return decoder.decode(zeroIndex >= 0 ? slice.subarray(0, zeroIndex) : slice).trim();
}

function parseTarSize(value: string): number {
  const trimmed = value.replace(/\0.*$/, '').trim();
  return trimmed ? Number.parseInt(trimmed, 8) : 0;
}

function sanitizeTarPath(path: string): string | null {
  const parts = path.split('/').filter(Boolean);
  if (parts.length === 0 || parts.some((part) => part === '.' || part === '..')) {
    return null;
  }
  return parts.join('/');
}

function sharedPrefix(paths: string[]): string | null {
  const first = paths[0]?.split('/')[0];
  if (!first) return null;
  for (const path of paths) {
    const parts = path.split('/').filter(Boolean);
    if (parts.length < 2 || parts[0] !== first) {
      return null;
    }
  }
  return first;
}
