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
import { loadStudioWasm, setStandaloneGuiEventDispatcher, type StudioWasm } from '../studio_wasm';
import { consolePush } from '../../stores/console';
import { formatDurationMs, pushUiConsole, renderStudioLogRecord, type StudioLogRecord } from './gui_console';
import { makeErrorStreamHandle, makeResolvedStreamHandle, makeStreamHandleFromProducer } from './stream_handle';

const WORKSPACE_ROOT = '/workspace';
const ROOT = '/';
const URL_SESSION_ROOT = `${WORKSPACE_ROOT}/.studio-sessions/url`;
const defaultWorkspaceFiles = new Map<string, string>([
  [`${WORKSPACE_ROOT}/README.md`, '# Studio\n\nThe web backend is running in an in-memory workspace.\n'],
  [`${WORKSPACE_ROOT}/main.vo`, 'package main\n\nimport "fmt"\n\nfunc main() {\n    fmt.Println("Studio rewrite bootstrap")\n}\n'],
]);
const files = new Map<string, string>();
const vfsFiles = new Map<string, Uint8Array>();
const directories = new Set<string>();
const vfsFileModes = new Map<string, number>();
const vfsFileModTimes = new Map<string, number>();
const vfsDirModes = new Map<string, number>();
const vfsDirModTimes = new Map<string, number>();
const textEncoder = new TextEncoder();
const utf8Decoder = new TextDecoder('utf-8', { fatal: true });
let studioWasmPromise: Promise<StudioWasm> | null = null;
let vfsBindingsInstalled = false;

function displayPath(path: string): string {
  const normalized = normalizePath(path);
  if (normalized === ROOT) {
    return ROOT;
  }
  if (normalized.startsWith(`${WORKSPACE_ROOT}/`)) {
    return normalized.slice(WORKSPACE_ROOT.length + 1);
  }
  return normalized.startsWith('/') ? normalized.slice(1) : normalized;
}

function pushStudioLogRecord(record: StudioLogRecord): void {
  pushUiConsole(renderStudioLogRecord(record, displayPath));
}

type OpenVfsFile = { path: string; flags: number; position: number };

const openVfsFiles = new Map<number, OpenVfsFile>();
let nextVfsFd = 100;

const O_RDONLY = 0;
const O_WRONLY = 1;
const O_RDWR = 2;
const O_APPEND = 8;
const O_CREATE = 16;
const O_EXCL = 32;
const O_TRUNC = 128;
const ERR_NOT_EXIST = 'file does not exist';
const ERR_EXIST = 'file already exists';
const ERR_NOT_DIR = 'not a directory';
const ERR_IS_DIR = 'is a directory';
const ERR_INVALID = 'invalid argument';
const ERR_BAD_FD = 'invalid file descriptor';

resetWorkspaceState();

export class WebBackend implements Backend {
  readonly platform = 'wasm' as const;

  private guiOperationChain: Promise<void> = Promise.resolve();
  private guiSessionId = 0;
  private guiFatalError: Error | null = null;

  private installStandaloneGuiDispatcher(sessionId: number): void {
    setStandaloneGuiEventDispatcher(async (handlerId, payload) => {
      if (sessionId !== this.guiSessionId) {
        return;
      }
      await this.dispatchGuiEventAsyncSerialized(handlerId, payload, sessionId);
    });
  }

  private async runGuiEventSerialized<T>(
    runWithWasm: (wasm: StudioWasm) => T | Promise<T>,
    staleValue: T,
    sessionId = this.guiSessionId,
  ): Promise<T> {
    if (sessionId !== this.guiSessionId) {
      return staleValue;
    }
    if (this.guiFatalError) {
      throw this.guiFatalError;
    }

    const run = async (): Promise<T> => {
      if (sessionId !== this.guiSessionId) {
        return staleValue;
      }
      if (this.guiFatalError) {
        throw this.guiFatalError;
      }
      const wasm = await getStudioWasm();
      if (sessionId !== this.guiSessionId) {
        return staleValue;
      }
      try {
        return await runWithWasm(wasm);
      } catch (error) {
        if (sessionId !== this.guiSessionId) {
          return staleValue;
        }
        this.guiFatalError = error instanceof Error ? error : new Error(String(error));
        setStandaloneGuiEventDispatcher(null);
        throw this.guiFatalError;
      }
    };

    return this.serializeGuiOperation(run);
  }

  private serializeGuiOperation<T>(run: () => Promise<T>): Promise<T> {
    const next = this.guiOperationChain.then(run, run);
    this.guiOperationChain = next.then(() => undefined, () => undefined);
    return next;
  }

  private assertGuiSessionCurrent(sessionId: number): void {
    if (sessionId !== this.guiSessionId) {
      throw new Error('GUI backend session superseded');
    }
  }

  private async dispatchGuiEventSerialized(
    handlerId: number,
    payload: string,
    sessionId = this.guiSessionId,
  ): Promise<Uint8Array> {
    return this.runGuiEventSerialized(
      (wasm) => wasm.sendGuiEvent(handlerId, payload),
      new Uint8Array(0),
      sessionId,
    );
  }

  private async dispatchGuiEventAsyncSerialized(
    handlerId: number,
    payload: string,
    sessionId = this.guiSessionId,
  ): Promise<void> {
    await this.runGuiEventSerialized<void>(
      (wasm) => {
        wasm.sendGuiEventAsync(handlerId, payload);
      },
      undefined,
      sessionId,
    );
  }

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

  async openRunSession(path: string): Promise<SessionInfo> {
    const normalized = normalizePath(path);
    if (normalized.endsWith('.vo') && hasVfsFile(normalized)) {
      const parent = normalized.substring(0, normalized.lastIndexOf('/')) || WORKSPACE_ROOT;
      return {
        root: parent,
        origin: 'workspace',
        projectMode: 'single-file',
        entryPath: normalized,
        singleFileRun: false,
      };
    }
    return buildSessionInfo(normalized, 'workspace');
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
    const content = vfsFiles.get(normalized);
    if (!isDir && content === undefined) throw new Error(`Path not found: ${normalized}`);
    return {
      path: normalized,
      isDir,
      isFile: !isDir,
      size: content?.length ?? 0,
      modifiedMs: isDir ? (vfsDirModTimes.get(normalized) ?? 0) : (vfsFileModTimes.get(normalized) ?? 0),
    };
  }

  async readFile(path: string): Promise<string> {
    const normalized = normalizePath(path);
    const content = readTextFile(normalized);
    if (content === null) throw new Error(`File not found: ${normalized}`);
    return content;
  }

  async readMany(paths: string[]): Promise<ReadManyResult[]> {
    return paths.map((path) => {
      const normalized = normalizePath(path);
      const content = readTextFile(normalized);
      return content !== null
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
    if (vfsFiles.has(normalized)) {
      deleteFile(normalized);
      return;
    }
    if (!directories.has(normalized)) return;
    const children = listDirEntries(normalized);
    if (children.length > 0 && !recursive) {
      throw new Error(`Directory not empty: ${normalized}`);
    }
    const descendants = [...directories].filter((dir) => dir === normalized || dir.startsWith(`${normalized}/`));
    for (const filePath of [...vfsFiles.keys()]) {
      if (filePath.startsWith(`${normalized}/`)) {
        deleteFile(filePath);
      }
    }
    for (const dir of descendants.sort((a, b) => b.length - a.length)) {
      if (dir !== ROOT && dir !== WORKSPACE_ROOT) {
        directories.delete(dir);
        vfsDirModes.delete(dir);
        vfsDirModTimes.delete(dir);
      }
    }
  }

  async renameEntry(oldPath: string, newPath: string): Promise<void> {
    const oldNorm = normalizePath(oldPath);
    const newNorm = normalizePath(newPath);
    const content = vfsFiles.get(oldNorm);
    if (content !== undefined) {
      const mode = vfsFileModes.get(oldNorm) ?? 0o644;
      deleteFile(oldNorm);
      setVfsFile(newNorm, content, mode);
      return;
    }
    if (!directories.has(oldNorm)) {
      throw new Error(`Path not found: ${oldNorm}`);
    }
    ensureDir(newNorm);
    const dirMoves = [...directories]
      .filter((dir) => dir === oldNorm || dir.startsWith(`${oldNorm}/`))
      .sort((a, b) => a.length - b.length);
    const fileMoves = [...vfsFiles.entries()]
      .filter(([filePath]) => filePath.startsWith(`${oldNorm}/`));
    for (const [filePath] of fileMoves) {
      deleteFile(filePath);
    }
    for (const dir of dirMoves.sort((a, b) => b.length - a.length)) {
      directories.delete(dir);
      vfsDirModes.delete(dir);
      vfsDirModTimes.delete(dir);
    }
    for (const dir of dirMoves) {
      const moved = dir === oldNorm ? newNorm : `${newNorm}${dir.slice(oldNorm.length)}`;
      ensureDir(moved);
    }
    for (const [filePath, fileContent] of fileMoves) {
      const moved = `${newNorm}${filePath.slice(oldNorm.length)}`;
      setVfsFile(moved, fileContent, vfsFileModes.get(filePath) ?? 0o644);
    }
  }

  async copyEntry(src: string, dst: string): Promise<void> {
    const srcNorm = normalizePath(src);
    const dstNorm = normalizePath(dst);
    const content = vfsFiles.get(srcNorm);
    if (content !== undefined) {
      setVfsFile(dstNorm, content, vfsFileModes.get(srcNorm) ?? 0o644);
      return;
    }
    if (!directories.has(srcNorm)) {
      throw new Error(`Path not found: ${srcNorm}`);
    }
    ensureDir(dstNorm);
    for (const dir of [...directories].filter((dir) => dir.startsWith(`${srcNorm}/`)).sort((a, b) => a.length - b.length)) {
      ensureDir(`${dstNorm}${dir.slice(srcNorm.length)}`);
    }
    for (const [filePath, fileContent] of vfsFiles) {
      if (filePath.startsWith(`${srcNorm}/`)) {
        setVfsFile(`${dstNorm}${filePath.slice(srcNorm.length)}`, fileContent, vfsFileModes.get(filePath) ?? 0o644);
      }
    }
  }

  async grep(path: string, pattern: string, opts?: GrepOpts): Promise<GrepMatch[]> {
    const results: GrepMatch[] = [];
    const maxResults = opts?.maxResults ?? 500;
    const caseSensitive = opts?.caseSensitive ?? false;
    const patternCmp = caseSensitive ? pattern : pattern.toLowerCase();
    for (const [filePath] of vfsFiles) {
      if (!filePath.startsWith(normalizePath(path))) continue;
      const content = readTextFile(filePath);
      if (content === null) continue;
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

  runVo(path: string, _opts?: RunOpts): StreamHandle<RunEvent> {
    const normalized = normalizePath(path);
    return makeStreamHandleFromProducer<RunEvent>((emit, onDone, onError) => {
      (async () => {
        const start = performance.now();
        const wasm = await getStudioWasm();
        await wasm.prepareEntry(normalized);
        const output = wasm.compileRunEntry(normalized);
        if (output) {
          for (const line of output.split('\n')) {
            emit({ kind: 'stdout', text: line });
          }
        }
        const durationMs = Math.round(performance.now() - start);
        emit({ kind: 'done', exitCode: 0, durationMs });
        onDone();
      })().catch((err) => {
        emit({ kind: 'error', message: err instanceof Error ? err.message : String(err) });
        onDone();
      });
    });
  }

  async stopVoRun(): Promise<void> {
    return;
  }

  async runGui(path: string): Promise<GuiRunOutput> {
    const normalized = normalizePath(path);
    const targetLabel = displayPath(normalized);
    const sessionId = this.guiSessionId + 1;
    this.guiSessionId = sessionId;
    this.guiFatalError = null;
    setStandaloneGuiEventDispatcher(null);
    return this.serializeGuiOperation(async () => {
      consolePush('system', `Opening GUI ${targetLabel}`);
      const totalStart = performance.now();
      const wasm = await getStudioWasm();
      this.assertGuiSessionCurrent(sessionId);
      consolePush('system', `Preparing dependencies for ${targetLabel}...`);
      const prepareStart = performance.now();
      await wasm.prepareEntry(normalized);
      const prepareDurationMs = performance.now() - prepareStart;
      consolePush('system', `Prepared dependencies for ${targetLabel} in ${formatDurationMs(prepareDurationMs)}`);
      console.info(`[studio-gui] prepareEntry ${normalized} ${Math.round(prepareDurationMs)}ms`);
      this.assertGuiSessionCurrent(sessionId);
      this.installStandaloneGuiDispatcher(sessionId);
      try {
        consolePush('system', `Compiling and starting GUI ${targetLabel}...`);
        const runStart = performance.now();
        const output = wasm.runGui(normalized);
        const runDurationMs = performance.now() - runStart;
        const totalDurationMs = performance.now() - totalStart;
        consolePush('success', `Opened GUI ${targetLabel} in ${formatDurationMs(totalDurationMs)}`);
        console.info(`[studio-gui] runGui ${normalized} ${Math.round(runDurationMs)}ms`);
        console.info(`[studio-gui] total open ${normalized} ${Math.round(totalDurationMs)}ms`);
        return output;
      } catch (error) {
        if (sessionId === this.guiSessionId) {
          setStandaloneGuiEventDispatcher(null);
        }
        throw error;
      }
    });
  }

  async sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array> {
    return this.dispatchGuiEventSerialized(handlerId, payload);
  }

  async sendGuiEventAsync(handlerId: number, payload: string): Promise<void> {
    await this.dispatchGuiEventAsyncSerialized(handlerId, payload);
  }

  async pushIslandTransport(data: Uint8Array): Promise<void> {
    await this.runGuiEventSerialized<void>(
      (wasm) => {
        wasm.pushIslandData(data);
      },
      undefined,
    );
  }

  async pollGuiRender(): Promise<Uint8Array> {
    return this.runGuiEventSerialized(
      (wasm) => wasm.pollGuiRender(),
      new Uint8Array(0),
    );
  }

  async stopGui(): Promise<void> {
    this.guiSessionId += 1;
    this.guiFatalError = null;
    setStandaloneGuiEventDispatcher(null);
    await this.serializeGuiOperation(async () => {
      const wasm = await getStudioWasm();
      wasm.stopGui();
    });
  }

  async getRenderIslandVfsSnapshot(path: string): Promise<RenderIslandVfsSnapshot> {
    const wasm = await getStudioWasm();
    return wasm.getRenderIslandVfsSnapshot(normalizePath(path));
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

function getStudioWasm(): Promise<StudioWasm> {
  ensureVfsBindings();
  if (!studioWasmPromise) {
    (globalThis as Record<string, unknown>).__voStudioLogRecord = (record: StudioLogRecord) => {
      pushStudioLogRecord(record);
      console.debug('[studio-log]', record);
    };
    studioWasmPromise = loadStudioWasm();
  }
  return studioWasmPromise;
}

function ensureVfsBindings(): void {
  if (vfsBindingsInstalled) {
    return;
  }
  vfsBindingsInstalled = true;
  const windowWithVfs = window as unknown as Record<string, unknown>;
  windowWithVfs._vfsOpenFile = (path: string, flags: number | bigint, mode: number | bigint) =>
    vfsOpenFile(path, toNum(flags), toNum(mode));
  windowWithVfs._vfsRead = (fd: number | bigint, length: number | bigint) =>
    vfsRead(toNum(fd), toNum(length));
  windowWithVfs._vfsWrite = (fd: number | bigint, data: Uint8Array) =>
    vfsWrite(toNum(fd), data);
  windowWithVfs._vfsReadAt = (fd: number | bigint, length: number | bigint, offset: number | bigint) =>
    vfsReadAt(toNum(fd), toNum(length), toNum(offset));
  windowWithVfs._vfsWriteAt = (fd: number | bigint, data: Uint8Array, offset: number | bigint) =>
    vfsWriteAt(toNum(fd), data, toNum(offset));
  windowWithVfs._vfsSeek = (fd: number | bigint, offset: number | bigint, whence: number | bigint) =>
    vfsSeek(toNum(fd), toNum(offset), toNum(whence));
  windowWithVfs._vfsClose = (fd: number | bigint) => vfsClose(toNum(fd));
  windowWithVfs._vfsSync = (fd: number | bigint) => vfsSync(toNum(fd));
  windowWithVfs._vfsFstat = (fd: number | bigint) => vfsFstat(toNum(fd));
  windowWithVfs._vfsFtruncate = (fd: number | bigint, size: number | bigint) =>
    vfsFtruncate(toNum(fd), toNum(size));
  windowWithVfs._vfsMkdir = (path: string, mode: number | bigint) => vfsMkdir(path, toNum(mode));
  windowWithVfs._vfsMkdirAll = (path: string, mode: number | bigint) => vfsMkdirAll(path, toNum(mode));
  windowWithVfs._vfsRemove = (path: string) => vfsRemove(path);
  windowWithVfs._vfsRemoveAll = (path: string) => vfsRemoveAll(path);
  windowWithVfs._vfsRename = (oldPath: string, newPath: string) => vfsRename(oldPath, newPath);
  windowWithVfs._vfsStat = (path: string) => vfsStat(path);
  windowWithVfs._vfsReadDir = (path: string) => vfsReadDir(path);
  windowWithVfs._vfsChmod = (path: string, mode: number | bigint) => vfsChmod(path, toNum(mode));
  windowWithVfs._vfsTruncate = (path: string, size: number | bigint) => vfsTruncate(path, toNum(size));
  windowWithVfs._vfsReadFile = (path: string) => vfsReadFile(path);
  windowWithVfs._vfsWriteFile = (path: string, data: Uint8Array, mode: number | bigint) =>
    vfsWriteFile(path, data, toNum(mode));
}

function toNum(value: number | bigint): number {
  return typeof value === 'bigint' ? Number(value) : value;
}

function readTextFile(path: string): string | null {
  const normalized = normalizePath(path);
  const cached = files.get(normalized);
  if (cached !== undefined) {
    return cached;
  }
  const bytes = vfsFiles.get(normalized);
  if (!bytes) {
    return null;
  }
  const decoded = tryDecodeUtf8(bytes);
  if (decoded === null) {
    return null;
  }
  files.set(normalized, decoded);
  return decoded;
}

function tryDecodeUtf8(bytes: Uint8Array): string | null {
  try {
    return utf8Decoder.decode(bytes);
  } catch {
    return null;
  }
}

function setVfsFile(path: string, bytes: Uint8Array, mode = 0o644): void {
  const normalized = normalizePath(path);
  const parent = dirname(normalized);
  ensureDir(parent);
  const copy = new Uint8Array(bytes);
  vfsFiles.set(normalized, copy);
  vfsFileModes.set(normalized, mode);
  vfsFileModTimes.set(normalized, Date.now());
  const decoded = tryDecodeUtf8(copy);
  if (decoded === null) {
    files.delete(normalized);
  } else {
    files.set(normalized, decoded);
  }
}

function deleteFile(path: string): void {
  const normalized = normalizePath(path);
  files.delete(normalized);
  vfsFiles.delete(normalized);
  vfsFileModes.delete(normalized);
  vfsFileModTimes.delete(normalized);
}

function hasVfsFile(path: string): boolean {
  return vfsFiles.has(normalizePath(path));
}

function resolveSnapshotRoot(path: string): string {
  const normalized = normalizePath(path);
  if (directories.has(normalized)) {
    return findProjectRoot(normalized) ?? normalized;
  }
  const parent = dirname(normalized);
  return findProjectRoot(parent) ?? parent;
}

function findProjectRoot(start: string): string | null {
  let current = normalizePath(start);
  while (true) {
    if (hasVfsFile(`${current}/vo.mod`)) {
      return current;
    }
    const parent = dirname(current);
    if (parent === current) {
      return null;
    }
    current = parent;
  }
}

function listVfsEntries(path: string): Array<[string, boolean, number]> {
  const normalized = normalizePath(path);
  const entries = new Map<string, [string, boolean, number]>();
  for (const dir of directories) {
    if (dir === normalized || dirname(dir) !== normalized) continue;
    const name = dir.slice(dir.lastIndexOf('/') + 1);
    entries.set(name, [name, true, vfsDirModes.get(dir) ?? 0o755]);
  }
  for (const [filePath] of vfsFiles) {
    if (dirname(filePath) !== normalized) continue;
    const name = filePath.slice(filePath.lastIndexOf('/') + 1);
    entries.set(name, [name, false, vfsFileModes.get(filePath) ?? 0o644]);
  }
  return [...entries.values()];
}

function vfsOpenFile(path: string, flags: number, mode: number): [number, string | null] {
  const normalized = normalizePath(path);
  const access = flags & 0x3;
  const create = (flags & O_CREATE) !== 0;
  const excl = (flags & O_EXCL) !== 0;
  const trunc = (flags & O_TRUNC) !== 0;
  let bytes = vfsFiles.get(normalized);
  if (!bytes) {
    if (!create) return [-1, ERR_NOT_EXIST];
    setVfsFile(normalized, new Uint8Array(0), mode);
    bytes = vfsFiles.get(normalized) ?? new Uint8Array(0);
  } else {
    if (excl) return [-1, ERR_EXIST];
    if (directories.has(normalized)) return [-1, ERR_IS_DIR];
    if (trunc && access !== O_RDONLY) {
      setVfsFile(normalized, new Uint8Array(0), vfsFileModes.get(normalized) ?? mode);
      bytes = vfsFiles.get(normalized) ?? new Uint8Array(0);
    }
  }
  const fd = nextVfsFd++;
  openVfsFiles.set(fd, {
    path: normalized,
    flags,
    position: (flags & O_APPEND) !== 0 ? bytes.length : 0,
  });
  return [fd, null];
}

function vfsRead(fd: number, length: number): [Uint8Array | null, string | null] {
  const file = openVfsFiles.get(fd);
  if (!file) return [null, ERR_BAD_FD];
  if (directories.has(file.path)) return [null, ERR_IS_DIR];
  const bytes = vfsFiles.get(file.path) ?? new Uint8Array(0);
  const start = file.position;
  const end = Math.min(start + length, bytes.length);
  const chunk = bytes.slice(start, end);
  file.position = end;
  return [chunk, null];
}

function writeBytes(path: string, offset: number, data: Uint8Array): number {
  const existing = vfsFiles.get(path) ?? new Uint8Array(0);
  const nextLength = Math.max(existing.length, offset + data.length);
  const next = new Uint8Array(nextLength);
  next.set(existing);
  next.set(data, offset);
  setVfsFile(path, next, vfsFileModes.get(path) ?? 0o644);
  return data.length;
}

function vfsWrite(fd: number, data: Uint8Array): [number, string | null] {
  const file = openVfsFiles.get(fd);
  if (!file) return [0, ERR_BAD_FD];
  if (directories.has(file.path)) return [0, ERR_IS_DIR];
  const access = file.flags & 0x3;
  if (access === O_RDONLY) return [0, 'file not open for writing'];
  const written = writeBytes(file.path, file.position, data);
  file.position += written;
  return [written, null];
}

function vfsReadAt(fd: number, length: number, offset: number): [Uint8Array | null, string | null] {
  const file = openVfsFiles.get(fd);
  if (!file) return [null, ERR_BAD_FD];
  if (directories.has(file.path)) return [null, ERR_IS_DIR];
  const bytes = vfsFiles.get(file.path) ?? new Uint8Array(0);
  if (offset >= bytes.length) return [new Uint8Array(0), null];
  return [bytes.slice(offset, Math.min(offset + length, bytes.length)), null];
}

function vfsWriteAt(fd: number, data: Uint8Array, offset: number): [number, string | null] {
  const file = openVfsFiles.get(fd);
  if (!file) return [0, ERR_BAD_FD];
  if (directories.has(file.path)) return [0, ERR_IS_DIR];
  const access = file.flags & 0x3;
  if (access === O_RDONLY) return [0, 'file not open for writing'];
  return [writeBytes(file.path, offset, data), null];
}

function vfsSeek(fd: number, offset: number, whence: number): [number, string | null] {
  const file = openVfsFiles.get(fd);
  if (!file) return [-1, ERR_BAD_FD];
  const bytes = vfsFiles.get(file.path) ?? new Uint8Array(0);
  let nextPosition: number;
  switch (whence) {
    case 0:
      nextPosition = offset;
      break;
    case 1:
      nextPosition = file.position + offset;
      break;
    case 2:
      nextPosition = bytes.length + offset;
      break;
    default:
      return [-1, ERR_INVALID];
  }
  if (nextPosition < 0) return [-1, ERR_INVALID];
  file.position = nextPosition;
  return [nextPosition, null];
}

function vfsClose(fd: number): string | null {
  return openVfsFiles.delete(fd) ? null : ERR_BAD_FD;
}

function vfsSync(_fd: number): string | null {
  return null;
}

function vfsFstat(fd: number): [number, number, number, boolean, string | null] {
  const file = openVfsFiles.get(fd);
  if (!file) return [0, 0, 0, false, ERR_BAD_FD];
  const isDir = directories.has(file.path);
  const size = isDir ? 0 : (vfsFiles.get(file.path)?.length ?? 0);
  return [
    size,
    isDir ? (vfsDirModes.get(file.path) ?? 0o755) : (vfsFileModes.get(file.path) ?? 0o644),
    isDir ? (vfsDirModTimes.get(file.path) ?? 0) : (vfsFileModTimes.get(file.path) ?? 0),
    isDir,
    null,
  ];
}

function vfsFtruncate(fd: number, size: number): string | null {
  const file = openVfsFiles.get(fd);
  if (!file) return ERR_BAD_FD;
  if (directories.has(file.path)) return ERR_IS_DIR;
  return vfsTruncate(file.path, size);
}

function vfsMkdir(path: string, mode: number): string | null {
  const normalized = normalizePath(path);
  const parent = dirname(normalized);
  if (!directories.has(parent)) return ERR_NOT_EXIST;
  if (directories.has(normalized) || vfsFiles.has(normalized)) return ERR_EXIST;
  directories.add(normalized);
  vfsDirModes.set(normalized, mode);
  vfsDirModTimes.set(normalized, Date.now());
  return null;
}

function vfsMkdirAll(path: string, mode: number): string | null {
  const normalized = normalizePath(path);
  const parts = normalized.split('/').filter(Boolean);
  let current = ROOT;
  for (const part of parts) {
    current = current === ROOT ? `/${part}` : `${current}/${part}`;
    if (vfsFiles.has(current)) return ERR_NOT_DIR;
    if (!directories.has(current)) {
      directories.add(current);
      vfsDirModes.set(current, mode);
      vfsDirModTimes.set(current, Date.now());
    }
  }
  return null;
}

function vfsRemove(path: string): string | null {
  const normalized = normalizePath(path);
  if (vfsFiles.has(normalized)) {
    deleteFile(normalized);
    return null;
  }
  if (!directories.has(normalized)) return ERR_NOT_EXIST;
  if (listVfsEntries(normalized).length > 0) return 'directory not empty';
  directories.delete(normalized);
  vfsDirModes.delete(normalized);
  vfsDirModTimes.delete(normalized);
  return null;
}

function vfsRemoveAll(path: string): string | null {
  const normalized = normalizePath(path);
  if (vfsFiles.has(normalized)) {
    deleteFile(normalized);
    return null;
  }
  if (!directories.has(normalized)) return ERR_NOT_EXIST;
  for (const filePath of [...vfsFiles.keys()]) {
    if (filePath === normalized || filePath.startsWith(`${normalized}/`)) {
      deleteFile(filePath);
    }
  }
  for (const dir of [...directories].sort((a, b) => b.length - a.length)) {
    if (dir === normalized || dir.startsWith(`${normalized}/`)) {
      directories.delete(dir);
      vfsDirModes.delete(dir);
      vfsDirModTimes.delete(dir);
    }
  }
  return null;
}

function vfsRename(oldPath: string, newPath: string): string | null {
  const oldNorm = normalizePath(oldPath);
  const newNorm = normalizePath(newPath);
  if (vfsFiles.has(oldNorm)) {
    setVfsFile(newNorm, vfsFiles.get(oldNorm) ?? new Uint8Array(0), vfsFileModes.get(oldNorm) ?? 0o644);
    deleteFile(oldNorm);
    return null;
  }
  if (!directories.has(oldNorm)) return ERR_NOT_EXIST;
  const children = listVfsEntries(oldNorm);
  vfsMkdirAll(newNorm, vfsDirModes.get(oldNorm) ?? 0o755);
  for (const [name, isDir] of children) {
    const from = oldNorm === ROOT ? `/${name}` : `${oldNorm}/${name}`;
    const to = newNorm === ROOT ? `/${name}` : `${newNorm}/${name}`;
    if (isDir) {
      vfsRename(from, to);
    } else {
      setVfsFile(to, vfsFiles.get(from) ?? new Uint8Array(0), vfsFileModes.get(from) ?? 0o644);
      deleteFile(from);
    }
  }
  directories.delete(oldNorm);
  vfsDirModes.delete(oldNorm);
  vfsDirModTimes.delete(oldNorm);
  return null;
}

function vfsStat(path: string): [string, number, number, number, boolean, string | null] {
  const normalized = normalizePath(path);
  const name = normalized === ROOT ? '' : normalized.slice(normalized.lastIndexOf('/') + 1);
  if (directories.has(normalized)) {
    return [name, 0, vfsDirModes.get(normalized) ?? 0o755, vfsDirModTimes.get(normalized) ?? 0, true, null];
  }
  const bytes = vfsFiles.get(normalized);
  if (!bytes) return ['', 0, 0, 0, false, ERR_NOT_EXIST];
  return [name, bytes.length, vfsFileModes.get(normalized) ?? 0o644, vfsFileModTimes.get(normalized) ?? 0, false, null];
}

function vfsReadDir(path: string): [Array<[string, boolean, number]>, string | null] {
  const normalized = normalizePath(path);
  if (!directories.has(normalized)) return [[], ERR_NOT_EXIST];
  return [listVfsEntries(normalized), null];
}

function vfsChmod(path: string, mode: number): string | null {
  const normalized = normalizePath(path);
  if (directories.has(normalized)) {
    vfsDirModes.set(normalized, mode);
    return null;
  }
  if (!vfsFiles.has(normalized)) return ERR_NOT_EXIST;
  vfsFileModes.set(normalized, mode);
  return null;
}

function vfsTruncate(path: string, size: number): string | null {
  const normalized = normalizePath(path);
  if (directories.has(normalized)) return ERR_IS_DIR;
  const existing = vfsFiles.get(normalized);
  if (!existing) return ERR_NOT_EXIST;
  let next: Uint8Array;
  if (size < existing.length) {
    next = existing.slice(0, size);
  } else if (size > existing.length) {
    next = new Uint8Array(size);
    next.set(existing);
  } else {
    next = existing;
  }
  setVfsFile(normalized, next, vfsFileModes.get(normalized) ?? 0o644);
  return null;
}

function vfsReadFile(path: string): [Uint8Array | null, string | null] {
  const normalized = normalizePath(path);
  if (directories.has(normalized)) return [null, ERR_IS_DIR];
  const bytes = vfsFiles.get(normalized);
  if (!bytes) return [null, ERR_NOT_EXIST];
  return [new Uint8Array(bytes), null];
}

function vfsWriteFile(path: string, data: Uint8Array, mode: number): string | null {
  setVfsFile(path, data, mode);
  return null;
}

function resetWorkspaceState(): void {
  files.clear();
  vfsFiles.clear();
  directories.clear();
  vfsFileModes.clear();
  vfsFileModTimes.clear();
  vfsDirModes.clear();
  vfsDirModTimes.clear();
  openVfsFiles.clear();
  nextVfsFd = 100;
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
  if (!vfsDirModes.has(ROOT)) {
    vfsDirModes.set(ROOT, 0o755);
    vfsDirModTimes.set(ROOT, Date.now());
  }
  for (const part of parts) {
    current = current === ROOT ? `/${part}` : `${current}/${part}`;
    if (!directories.has(current)) {
      directories.add(current);
      vfsDirModes.set(current, 0o755);
      vfsDirModTimes.set(current, Date.now());
    }
  }
}

function setFile(path: string, content: string): void {
  const normalized = normalizePath(path);
  const parent = dirname(normalized);
  ensureDir(parent);
  files.set(normalized, content);
  vfsFiles.set(normalized, textEncoder.encode(content));
  vfsFileModes.set(normalized, 0o644);
  vfsFileModTimes.set(normalized, Date.now());
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
  for (const [filePath, content] of vfsFiles) {
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
    projectMode: hasVfsFile(`${normalizedRoot}/vo.mod`) ? 'module' : 'single-file',
    entryPath,
    singleFileRun: false,
  };
}

function detectEntryPath(root: string): string | null {
  const main = `${root}/main.vo`;
  if (hasVfsFile(main)) return main;
  const first = [...vfsFiles.keys()]
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
  for (const filePath of [...vfsFiles.keys()]) {
    if (filePath.startsWith(`${root}/`)) {
      deleteFile(filePath);
    }
  }
  for (const dir of [...directories].sort((a, b) => b.length - a.length)) {
    if (dir.startsWith(`${root}/`) || dir === root) {
      directories.delete(dir);
      vfsDirModes.delete(dir);
      vfsDirModTimes.delete(dir);
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
