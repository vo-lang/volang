import type { Backend, FileDialogFilter } from './backend';
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
  LaunchSpec,
  ProcEvent,
  ReadManyResult,
  RendererBridgeVfsSnapshot,
  RunEvent,
  RunOpts,
  SessionInfo,
  StreamHandle,
} from '../types';
import { buildShareInfo } from '../session_share';
import { loadStudioWasm, setStandaloneGuiEventDispatcher, type StudioWasm } from '../studio_wasm';
import { executeGuiFromCompileOutput, type GuiCompileOutput } from '../gui/gui_pipeline';
import { consolePush } from '../../stores/console';
import { formatDurationMs, pushUiConsole, renderStudioLogRecord, type StudioLogRecord } from './gui_console';
import { makeErrorStreamHandle, makeResolvedStreamHandle, makeStreamHandleFromProducer } from './stream_handle';
import { installWindowVfsBackend } from '../window_vfs_bindings';

const WORKSPACE_ROOT = '/workspace';
const ROOT = '/';
const SOURCE_CACHE_ROOT = `${WORKSPACE_ROOT}/.studio-sources`;
const URL_SESSION_ROOT = `${WORKSPACE_ROOT}/.studio-sessions/url`;
const LOCAL_SESSION_ROOT = `${WORKSPACE_ROOT}/.studio-sessions/local`;
const LOCAL_PROJECT_ENDPOINT = '/__vo_studio_local_project';
const GITHUB_SOURCE_ROOT = `${SOURCE_CACHE_ROOT}/github`;
const GITHUB_SESSION_ROOT = `${WORKSPACE_ROOT}/.studio-sessions/github`;
const GITHUB_API_ROOT = 'https://api.github.com';
const GITHUB_BLOB_FETCH_CONCURRENCY = 8;
const DISPLAY_PULSE_DELAY_MS = 0xFFFFFFFF;
const MISSING_INITIAL_GUI_RENDER = 'guest app did not emit a render';
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

interface GitHubRepoInput {
  owner: string;
  repo: string;
  ref: string | null;
  commit: string | null;
  subdir: string | null;
}

interface GitHubRepoMetadata {
  default_branch?: string;
  html_url?: string;
}

interface GitHubCommitMetadata {
  sha?: string;
}

interface GitHubTreeItem {
  path?: string;
  type?: string;
  sha?: string;
  mode?: string;
}

interface GitHubTreeMetadata {
  tree?: GitHubTreeItem[];
  truncated?: boolean;
}

interface GitHubBlobMetadata {
  content?: string;
  encoding?: string;
}

interface ResolvedGitHubSource {
  owner: string;
  repo: string;
  requestedRef: string | null;
  resolvedCommit: string;
  subdir: string | null;
  htmlUrl: string;
  sourceCacheRoot: string;
  sessionRoot: string;
  projectRoot: string;
}

interface LocalProjectSnapshotFile {
  path: string;
  contentBase64: string;
  mode?: number;
}

interface LocalProjectSnapshot {
  projectPath: string;
  projectRelativePath: string;
  files: LocalProjectSnapshotFile[];
}

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
  private guiHostTimers = new Map<string, { kind: 'timeout' | 'raf'; id: number }>();
  private guiFirstRenderWaiter: { sessionId: number; resolve: (bytes: Uint8Array) => void; reject: (error: unknown) => void } | null = null;

  private clearGuiHostTimers(): void {
    for (const handle of this.guiHostTimers.values()) {
      if (handle.kind === 'raf') {
        cancelAnimationFrame(handle.id);
      } else {
        clearTimeout(handle.id);
      }
    }
    this.guiHostTimers.clear();
  }

  private rejectGuiFirstRenderWaiter(error: unknown): void {
    if (!this.guiFirstRenderWaiter) {
      return;
    }
    const waiter = this.guiFirstRenderWaiter;
    this.guiFirstRenderWaiter = null;
    waiter.reject(error);
  }

  private createGuiFirstRenderWaiter(sessionId: number): Promise<Uint8Array> {
    this.rejectGuiFirstRenderWaiter(new Error('GUI session superseded'));
    return new Promise<Uint8Array>((resolve, reject) => {
      this.guiFirstRenderWaiter = { sessionId, resolve, reject };
    });
  }

  private resolveGuiFirstRenderWaiter(sessionId: number, bytes: Uint8Array): void {
    if (bytes.length === 0 || !this.guiFirstRenderWaiter || this.guiFirstRenderWaiter.sessionId !== sessionId) {
      return;
    }
    const waiter = this.guiFirstRenderWaiter;
    this.guiFirstRenderWaiter = null;
    waiter.resolve(bytes);
  }

  private drainPendingGuiHostEvents(wasm: StudioWasm, sessionId = this.guiSessionId): void {
    while (true) {
      const event = wasm.pollPendingHostEvent();
      if (!event) {
        return;
      }
      this.scheduleGuiHostEvent(event.token, event.delayMs, sessionId);
    }
  }

  private scheduleGuiHostEvent(token: string, delayMs: number, sessionId: number): void {
    if (sessionId !== this.guiSessionId || this.guiFatalError) {
      return;
    }
    const existing = this.guiHostTimers.get(token);
    if (existing) {
      if (existing.kind === 'raf') {
        cancelAnimationFrame(existing.id);
      } else {
        clearTimeout(existing.id);
      }
    }
    const fire = (): void => {
      this.guiHostTimers.delete(token);
      void this.runGuiEventSerialized(
        (wasm) => {
          wasm.wakeHostEvent(token);
          this.drainPendingGuiHostEvents(wasm, sessionId);
          const renderBytes = this.guiFirstRenderWaiter?.sessionId === sessionId
            ? wasm.pollGuiRender()
            : new Uint8Array(0);
          if (
            renderBytes.length === 0
            && this.guiFirstRenderWaiter?.sessionId === sessionId
            && this.guiHostTimers.size === 0
          ) {
            throw new Error(MISSING_INITIAL_GUI_RENDER);
          }
          return renderBytes;
        },
        new Uint8Array(0),
        sessionId,
      ).then((renderBytes) => {
        this.resolveGuiFirstRenderWaiter(sessionId, renderBytes);
      }).catch(() => {});
    };
    const handle = delayMs === DISPLAY_PULSE_DELAY_MS
      ? { kind: 'raf' as const, id: requestAnimationFrame(() => fire()) }
      : { kind: 'timeout' as const, id: window.setTimeout(fire, Math.max(0, delayMs)) };
    this.guiHostTimers.set(token, handle);
  }

  private prepareFirstGuiRender(
    wasm: StudioWasm,
    initialRender: Uint8Array,
    sessionId: number,
  ): { renderBytes: Uint8Array; waitForRender: Promise<Uint8Array> | null } {
    if (initialRender.length > 0) {
      return { renderBytes: initialRender, waitForRender: null };
    }
    this.drainPendingGuiHostEvents(wasm, sessionId);
    const bufferedRender = wasm.pollGuiRender();
    if (bufferedRender.length > 0) {
      return { renderBytes: bufferedRender, waitForRender: null };
    }
    if (this.guiHostTimers.size === 0) {
      throw new Error(MISSING_INITIAL_GUI_RENDER);
    }
    return {
      renderBytes: new Uint8Array(0),
      waitForRender: this.createGuiFirstRenderWaiter(sessionId),
    };
  }

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
        this.clearGuiHostTimers();
        this.rejectGuiFirstRenderWaiter(this.guiFatalError);
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
      (wasm) => {
        const renderBytes = wasm.sendGuiEvent(handlerId, payload);
        this.drainPendingGuiHostEvents(wasm, sessionId);
        return renderBytes;
      },
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
        this.drainPendingGuiHostEvents(wasm, sessionId);
      },
      undefined,
      sessionId,
    );
  }

  async getBootstrapContext(): Promise<BootstrapContext> {
    const params = new URLSearchParams(window.location.search);
    const env = import.meta.env as Record<string, string | undefined>;
    const rawMode = params.get('mode') ?? env.VITE_STUDIO_MODE ?? null;
    const mode: import('../types').StudioMode = rawMode === 'runner' ? 'runner' : 'dev';
    const proj = params.get('proj') ?? env.VITE_STUDIO_PROJ ?? null;
    const launch: LaunchSpec | null = proj != null || rawMode != null
      ? { proj, mode }
      : null;
    return {
      workspaceRoot: WORKSPACE_ROOT,
      launch,
      mode,
      platform: 'wasm',
    };
  }

  async openSession(spec: LaunchSpec): Promise<SessionInfo> {
    if (spec.proj == null) {
      return openWorkspaceSession();
    }
    const filePath = localFileUrlPath(spec.proj);
    if (filePath) {
      return openLocalProjectSession(filePath);
    }
    if (spec.proj.startsWith('/')) {
      if (isWorkspaceVfsPath(spec.proj)) {
        return openPathSession(spec.proj);
      }
      return openLocalProjectSession(spec.proj);
    }
    const githubInput = parseGitHubRepoUrl(spec.proj);
    if (githubInput) {
      return openGitHubRepoSession(githubInput);
    }
    throw new Error(`Unsupported project URL: ${spec.proj}`);
  }

  async discoverWorkspaceProjects(): Promise<DiscoveredProject[]> {
    return this.discoverProjects(WORKSPACE_ROOT);
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
    this.clearGuiHostTimers();
    this.rejectGuiFirstRenderWaiter(new Error('GUI session superseded'));
    setStandaloneGuiEventDispatcher(null);
    const totalStart = performance.now();
    const startup = await this.serializeGuiOperation(async () => {
      consolePush('system', `Opening GUI ${targetLabel}`);
      const wasm = await getStudioWasm();
      this.assertGuiSessionCurrent(sessionId);
      consolePush('system', `Preparing dependencies for ${targetLabel}...`);
      const prepareStart = performance.now();
      await wasm.prepareEntry(normalized);
      const prepareDurationMs = performance.now() - prepareStart;
      consolePush('system', `Prepared dependencies for ${targetLabel} in ${formatDurationMs(prepareDurationMs)}`);
      this.assertGuiSessionCurrent(sessionId);
      this.installStandaloneGuiDispatcher(sessionId);
      consolePush('system', `Compiling and starting GUI ${targetLabel}...`);
      const compileStart = performance.now();
      const compileResult = wasm.compileGui(normalized);
      const compileDurationMs = performance.now() - compileStart;
      console.info(`[studio-gui] compileGui ${normalized} ${Math.round(compileDurationMs)}ms`);
      const wasmExtensionLabels = compileResult.wasmExtensions.map((ext) => `${ext.name}=>${ext.moduleKey ?? ext.name}`);
      const wasmExtensionSummary = wasmExtensionLabels.length > 0 ? wasmExtensionLabels.join(', ') : 'none';
      console.info(`[studio-gui] wasmExtensions ${normalized} count=${wasmExtensionLabels.length} names=${wasmExtensionSummary}`);
      consolePush(
        'system',
        wasmExtensionLabels.length > 0
          ? `GUI compile discovered WASM extensions: ${wasmExtensionSummary}`
          : 'GUI compile discovered no WASM extensions',
      );
      this.assertGuiSessionCurrent(sessionId);
      const compiled: GuiCompileOutput = {
        bytecode: compileResult.bytecode,
        entryPath: compileResult.entryPath,
        framework: compileResult.framework,
        providerFrameworks: compileResult.providerFrameworks,
        wasmExtensions: compileResult.wasmExtensions,
      };
      const assertCurrent = (id: number) => { this.assertGuiSessionCurrent(id); };
      const output = await executeGuiFromCompileOutput(compiled, this, wasm, sessionId, assertCurrent);
      this.assertGuiSessionCurrent(sessionId);
      const firstRender = this.prepareFirstGuiRender(wasm, output.renderBytes, sessionId);
      return {
        output,
        renderBytes: firstRender.renderBytes,
        waitForRender: firstRender.waitForRender,
      };
    });
    const renderBytes = startup.waitForRender ? await startup.waitForRender : startup.renderBytes;
    this.assertGuiSessionCurrent(sessionId);
    const totalDurationMs = performance.now() - totalStart;
    consolePush('success', `Opened GUI ${targetLabel} in ${formatDurationMs(totalDurationMs)}`);
    console.info(`[studio-gui] total open ${normalized} ${Math.round(totalDurationMs)}ms`);
    return {
      ...startup.output,
      renderBytes,
    };
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
        this.drainPendingGuiHostEvents(wasm);
      },
      undefined,
    );
  }

  async pollIslandTransport(): Promise<Uint8Array> {
    return this.runGuiEventSerialized(
      (wasm) => wasm.pollIslandData(),
      new Uint8Array(0),
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
    this.clearGuiHostTimers();
    this.rejectGuiFirstRenderWaiter(new Error('GUI session superseded'));
    setStandaloneGuiEventDispatcher(null);
    await this.serializeGuiOperation(async () => {
      const wasm = await getStudioWasm();
      wasm.stopGui();
    });
  }

  async getRendererBridgeVfsSnapshot(path: string): Promise<RendererBridgeVfsSnapshot> {
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

  async pickDirectory(_defaultPath?: string): Promise<string | null> {
    return null;
  }

  async pickFile(_defaultPath?: string, _filters?: FileDialogFilter[]): Promise<string | null> {
    return null;
  }

  async createProjectFiles(files: { path: string; content: string }[]): Promise<void> {
    for (const file of files) {
      setFile(normalizePath(file.path), file.content);
    }
  }

  async gitExec(_op: GitOp): Promise<GitResult> {
    return { ok: false, output: 'git is not available in WASM mode' };
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
  installWindowVfsBackend({
    openFile: vfsOpenFile,
    read: vfsRead,
    write: vfsWrite,
    readAt: vfsReadAt,
    writeAt: vfsWriteAt,
    seek: vfsSeek,
    close: vfsClose,
    sync: vfsSync,
    fstat: vfsFstat,
    ftruncate: vfsFtruncate,
    mkdir: vfsMkdir,
    mkdirAll: vfsMkdirAll,
    remove: vfsRemove,
    removeAll: vfsRemoveAll,
    rename: vfsRename,
    stat: vfsStat,
    readDir: vfsReadDir,
    chmod: vfsChmod,
    truncate: vfsTruncate,
    readFile: vfsReadFile,
    writeFile: vfsWriteFile,
  });
  vfsBindingsInstalled = true;
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

function buildSessionInfo(root: string, origin: SessionInfo['origin'], source: SessionInfo['source']): SessionInfo {
  const normalizedRoot = normalizePath(root);
  const entryPath = detectEntryPath(normalizedRoot);
  const share = buildShareInfo({ root: normalizedRoot, entryPath, source });
  return {
    root: normalizedRoot,
    origin,
    projectMode: hasVfsFile(`${normalizedRoot}/vo.mod`) ? 'module' : 'single-file',
    entryPath,
    singleFileRun: false,
    source,
    share,
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

function openWorkspaceSession(): SessionInfo {
  if (!directories.has(WORKSPACE_ROOT)) {
    resetWorkspaceState();
  }
  return buildSessionInfo(WORKSPACE_ROOT, 'workspace', { kind: 'workspace' });
}

function openPathSession(path: string): SessionInfo {
  const normalized = normalizePath(path);
  if (normalized.endsWith('.vo') && hasVfsFile(normalized)) {
    const parent = normalized.substring(0, normalized.lastIndexOf('/')) || WORKSPACE_ROOT;
    const source: SessionInfo['source'] = { kind: 'path', path: normalized };
    return {
      root: parent,
      origin: 'workspace',
      projectMode: 'single-file',
      entryPath: normalized,
      singleFileRun: false,
      source,
      share: buildShareInfo({ root: parent, entryPath: normalized, source }),
    };
  }
  return buildSessionInfo(normalized, 'workspace', { kind: 'path', path: normalized });
}

async function openLocalProjectSession(path: string): Promise<SessionInfo> {
  const snapshot = await fetchLocalProjectSnapshot(path);
  const projectName = snapshot.projectRelativePath.split('/').filter(Boolean).pop() ?? 'project';
  const sessionRoot = `${LOCAL_SESSION_ROOT}/${sanitizeSlug(projectName)}-${hashString(snapshot.projectPath)}-${sessionNonce()}`;
  clearImportedRootSync(sessionRoot);
  ensureDir(sessionRoot);
  for (const file of snapshot.files) {
    setVfsFile(`${sessionRoot}/${file.path}`, decodeBase64Bytes(file.contentBase64), file.mode ?? 0o644);
  }
  const projectRoot = normalizePath(`${sessionRoot}/${snapshot.projectRelativePath}`);
  if (!hasTreeAt(projectRoot)) {
    throw new Error(`Local project snapshot did not contain ${snapshot.projectRelativePath}`);
  }
  return buildSessionInfo(projectRoot, 'run-target', { kind: 'path', path: snapshot.projectPath });
}

async function fetchLocalProjectSnapshot(path: string): Promise<LocalProjectSnapshot> {
  const url = new URL(LOCAL_PROJECT_ENDPOINT, window.location.origin);
  url.searchParams.set('path', path);
  const response = await fetch(url.toString(), { cache: 'no-store' });
  if (!response.ok) {
    const detail = await response.text().catch(() => '');
    throw new Error(detail || `Local project bridge failed with HTTP ${response.status}`);
  }
  return response.json() as Promise<LocalProjectSnapshot>;
}

function localFileUrlPath(value: string): string | null {
  if (!value.startsWith('file://')) {
    return null;
  }
  return decodeURIComponent(new URL(value).pathname);
}

function isWorkspaceVfsPath(path: string): boolean {
  const normalized = normalizePath(path);
  return normalized === WORKSPACE_ROOT
    || normalized.startsWith(`${WORKSPACE_ROOT}/`)
    || hasTreeAt(normalized)
    || hasVfsFile(normalized);
}

function parseGitHubRepoUrl(value: string): GitHubRepoInput | null {
  let parsed: URL;
  try {
    parsed = new URL(value, window.location.href);
  } catch {
    return null;
  }
  const host = parsed.hostname.toLowerCase();
  if (host !== 'github.com' && host !== 'www.github.com') {
    return null;
  }
  const parts = parsed.pathname.split('/').filter(Boolean);
  if (parts.length < 2) {
    return null;
  }
  const owner = parts[0];
  const repo = parts[1].replace(/\.git$/i, '');
  if (!owner || !repo) {
    return null;
  }
  if (parts.length === 2) {
    return { owner, repo, ref: null, commit: null, subdir: null };
  }
  if (parts[2] !== 'tree' || !parts[3]) {
    return null;
  }
  return {
    owner,
    repo,
    ref: parts[3],
    commit: null,
    subdir: parts.length > 4 ? parts.slice(4).join('/') : null,
  };
}

async function openGitHubRepoSession(source: GitHubRepoInput): Promise<SessionInfo> {
  const resolved = await resolveGitHubSource(source);
  await populateGitHubSourceCache(resolved);
  materializeGitHubSession(resolved);
  return buildSessionInfo(resolved.projectRoot, 'url', {
    kind: 'github_repo',
    owner: resolved.owner,
    repo: resolved.repo,
    requestedRef: resolved.requestedRef,
    resolvedCommit: resolved.resolvedCommit,
    subdir: resolved.subdir,
    htmlUrl: resolved.htmlUrl,
    sourceCacheRoot: resolved.sourceCacheRoot,
  });
}

async function resolveGitHubSource(source: GitHubRepoInput): Promise<ResolvedGitHubSource> {
  const repoApi = `${GITHUB_API_ROOT}/repos/${encodeURIComponent(source.owner)}/${encodeURIComponent(source.repo)}`;
  const repoUrl = `https://github.com/${source.owner}/${source.repo}`;
  let requestedRef = source.ref ?? null;
  let resolvedCommit = source.commit ?? null;
  let htmlUrl = repoUrl;
  if (!resolvedCommit) {
    const repoInfo = await fetchGitHubJson<GitHubRepoMetadata>(repoApi);
    htmlUrl = typeof repoInfo.html_url === 'string' && repoInfo.html_url ? repoInfo.html_url : repoUrl;
    requestedRef = requestedRef ?? (typeof repoInfo.default_branch === 'string' ? repoInfo.default_branch : null);
    if (!requestedRef) {
      throw new Error(`Could not resolve a GitHub ref for ${source.owner}/${source.repo}`);
    }
    const commitInfo = await fetchGitHubJson<GitHubCommitMetadata>(`${repoApi}/commits/${encodeURIComponent(requestedRef)}`);
    resolvedCommit = typeof commitInfo.sha === 'string' ? commitInfo.sha : null;
    if (!resolvedCommit) {
      throw new Error(`Could not resolve commit for ${source.owner}/${source.repo}`);
    }
  }
  const sourceCacheRoot = `${GITHUB_SOURCE_ROOT}/${source.owner}/${source.repo}/${resolvedCommit}`;
  const sessionRoot = `${GITHUB_SESSION_ROOT}/${source.owner}/${source.repo}/${resolvedCommit}/${sessionNonce()}`;
  const projectRoot = source.subdir ? `${sessionRoot}/${normalizeRelativePath(source.subdir)}` : sessionRoot;
  return {
    owner: source.owner,
    repo: source.repo,
    requestedRef,
    resolvedCommit,
    subdir: source.subdir ?? null,
    htmlUrl,
    sourceCacheRoot,
    sessionRoot,
    projectRoot,
  };
}

async function populateGitHubSourceCache(resolved: ResolvedGitHubSource): Promise<void> {
  if (hasTreeAt(resolved.sourceCacheRoot)) {
    return;
  }
  await clearImportedRoot(resolved.sourceCacheRoot);
  ensureDir(resolved.sourceCacheRoot);
  const tree = await fetchGitHubTree(resolved.owner, resolved.repo, resolved.resolvedCommit);
  if (tree.truncated) {
    throw new Error(`GitHub repository is too large to open in the browser: ${resolved.owner}/${resolved.repo}`);
  }
  const blobs = (tree.tree ?? []).filter((entry): entry is GitHubTreeItem & { path: string; sha: string } => (
    entry.type === 'blob'
      && typeof entry.path === 'string'
      && entry.path.length > 0
      && typeof entry.sha === 'string'
      && entry.sha.length > 0
  ));
  if (blobs.length === 0) {
    throw new Error(`GitHub repository contains no files: ${resolved.owner}/${resolved.repo}`);
  }
  await runWithConcurrency(
    blobs,
    GITHUB_BLOB_FETCH_CONCURRENCY,
    async (entry) => {
      const bytes = await fetchGitHubBlobBytes(resolved.owner, resolved.repo, entry.sha);
      setVfsFile(
        `${resolved.sourceCacheRoot}/${entry.path}`,
        bytes,
        parseGitFileMode(entry.mode),
      );
    },
  );
}

function materializeGitHubSession(resolved: ResolvedGitHubSource): void {
  clearImportedRootSync(resolved.sessionRoot);
  copyVfsTree(resolved.sourceCacheRoot, resolved.sessionRoot);
  if (!hasTreeAt(resolved.projectRoot)) {
    throw new Error(`GitHub project root not found: ${resolved.projectRoot}`);
  }
}

async function fetchGitHubJson<T>(url: string): Promise<T> {
  const response = await fetch(url, {
    headers: {
      Accept: 'application/vnd.github+json',
    },
  });
  if (!response.ok) {
    throw new Error(`GitHub request failed with HTTP ${response.status}: ${url}`);
  }
  return response.json() as Promise<T>;
}

async function fetchGitHubTree(owner: string, repo: string, commit: string): Promise<GitHubTreeMetadata> {
  return fetchGitHubJson<GitHubTreeMetadata>(
    `${GITHUB_API_ROOT}/repos/${encodeURIComponent(owner)}/${encodeURIComponent(repo)}/git/trees/${encodeURIComponent(commit)}?recursive=1`,
  );
}

async function fetchGitHubBlobBytes(owner: string, repo: string, sha: string): Promise<Uint8Array> {
  const blob = await fetchGitHubJson<GitHubBlobMetadata>(
    `${GITHUB_API_ROOT}/repos/${encodeURIComponent(owner)}/${encodeURIComponent(repo)}/git/blobs/${encodeURIComponent(sha)}`,
  );
  if (blob.encoding !== 'base64' || typeof blob.content !== 'string') {
    throw new Error(`GitHub blob payload is not base64 encoded: ${owner}/${repo}@${sha}`);
  }
  return decodeBase64Bytes(blob.content);
}

async function fetchBytesFromUrl(url: string, headers?: Record<string, string>): Promise<Uint8Array> {
  const response = await fetch(url, { headers });
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${url}`);
  }
  return new Uint8Array(await response.arrayBuffer());
}

function hasTreeAt(root: string): boolean {
  const normalized = normalizePath(root);
  if (directories.has(normalized) || vfsFiles.has(normalized)) {
    return true;
  }
  for (const dir of directories) {
    if (dir.startsWith(`${normalized}/`)) {
      return true;
    }
  }
  for (const filePath of vfsFiles.keys()) {
    if (filePath.startsWith(`${normalized}/`)) {
      return true;
    }
  }
  return false;
}

function copyVfsTree(sourceRoot: string, sessionRoot: string): void {
  const srcNorm = normalizePath(sourceRoot);
  const dstNorm = normalizePath(sessionRoot);
  ensureDir(dstNorm);
  for (const dir of [...directories]
    .filter((dir) => dir === srcNorm || dir.startsWith(`${srcNorm}/`))
    .sort((a, b) => a.length - b.length)) {
    const moved = dir === srcNorm ? dstNorm : `${dstNorm}${dir.slice(srcNorm.length)}`;
    ensureDir(moved);
  }
  for (const [filePath, bytes] of vfsFiles) {
    if (filePath.startsWith(`${srcNorm}/`)) {
      setVfsFile(`${dstNorm}${filePath.slice(srcNorm.length)}`, bytes, vfsFileModes.get(filePath) ?? 0o644);
    }
  }
}

function clearImportedRootSync(root: string): void {
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

function normalizeRelativePath(path: string): string {
  const parts = path.split('/').filter(Boolean);
  if (parts.length === 0 || parts.some((part) => part === '.' || part === '..')) {
    throw new Error(`Invalid GitHub subdir: ${path}`);
  }
  return parts.join('/');
}

function sessionNonce(): string {
  return `${Date.now().toString(36)}-${Math.random().toString(36).slice(2, 8)}`;
}

async function importProjectFromUrl(url: string): Promise<string> {
  const sessionRoot = buildImportedRoot(url);
  await clearImportedRoot(sessionRoot);
  ensureDir(sessionRoot);
  const bytes = await fetchBytesFromUrl(url);
  if (looksLikeTarGz(url, bytes)) {
    const extracted = await extractTarGzFiles(bytes);
    if (extracted.length === 0) {
      throw new Error('archive contains no files');
    }
    const prefix = sharedPrefix(extracted.map((file) => file.path));
    for (const file of extracted) {
      const relative = prefix && file.path.startsWith(`${prefix}/`)
        ? file.path.slice(prefix.length + 1)
        : file.path;
      if (!relative) continue;
      setVfsFile(`${sessionRoot}/${relative}`, file.bytes);
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

async function extractTarGzFiles(bytes: Uint8Array): Promise<Array<{ path: string; bytes: Uint8Array }>> {
  const ab = new ArrayBuffer(bytes.byteLength);
  new Uint8Array(ab).set(bytes);
  const stream = new Response(ab).body;
  if (!stream || typeof DecompressionStream === 'undefined') {
    throw new Error('gzip archive import is not supported in this browser');
  }
  const decompressed = stream.pipeThrough(new DecompressionStream('gzip'));
  const tarBytes = new Uint8Array(await new Response(decompressed).arrayBuffer());
  return parseTarFiles(tarBytes);
}

function parseTarFiles(bytes: Uint8Array): Array<{ path: string; bytes: Uint8Array }> {
  const decoder = new TextDecoder();
  const filesOut: Array<{ path: string; bytes: Uint8Array }> = [];
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
      filesOut.push({ path, bytes: bytes.slice(bodyStart, bodyEnd) });
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
  const trimmed = value.replace(/\0/g, '').trim();
  if (!trimmed) {
    return 0;
  }
  const parsed = Number.parseInt(trimmed, 8);
  if (!Number.isFinite(parsed) || parsed < 0) {
    throw new Error(`Invalid tar entry size: ${value}`);
  }
  return parsed;
}

function sanitizeTarPath(path: string): string | null {
  const parts = path.split('/').filter(Boolean);
  if (parts.length === 0 || parts.some((part) => part === '.' || part === '..')) {
    return null;
  }
  return parts.join('/');
}

function parseGitFileMode(mode: string | undefined): number {
  if (!mode) {
    return 0o644;
  }
  const parsed = Number.parseInt(mode, 8);
  return Number.isFinite(parsed) ? parsed : 0o644;
}

function decodeBase64Bytes(value: string): Uint8Array {
  const normalized = value.replace(/\s+/g, '');
  const binary = atob(normalized);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i += 1) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}

async function runWithConcurrency<T>(
  values: T[],
  concurrency: number,
  worker: (value: T) => Promise<void>,
): Promise<void> {
  let nextIndex = 0;
  const runnerCount = Math.max(1, Math.min(concurrency, values.length));
  const runners = Array.from({ length: runnerCount }, async () => {
    while (nextIndex < values.length) {
      const index = nextIndex;
      nextIndex += 1;
      await worker(values[index]!);
    }
  });
  await Promise.all(runners);
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
