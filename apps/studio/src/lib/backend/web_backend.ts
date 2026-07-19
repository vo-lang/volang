import type { Backend, FileDialogFilter } from './backend';
import type {
  BootstrapContext,
  BuildResult,
  CheckResult,
  CompileResult,
  DiagnosticError,
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
  LaunchSpec,
  ProcEvent,
  ReadManyResult,
  RendererBridgeVfsSnapshot,
  RunEvent,
  RunOpts,
  SessionInfo,
  StreamHandle,
  WorkspaceDiscoveryMode,
} from '../types';
import { buildShareInfo } from '../session_share';
import { guestExitCode, loadStudioWasm, setStandaloneGuiEventDispatcher, type StudioWasm } from '../studio_wasm';
import {
  executeGuiFromCompileOutput,
  resetGuiHostBridge,
  type GuiCompileOutput,
} from '../gui/gui_pipeline';
import {
  handleVoplayPerfHostLog,
  setActiveVoplayPerfSessionId,
  shouldEmitVoplayPerfConsoleDiagnostics,
} from '../perf_report_bridge';
import { GuiSessionBinding, type GuiSessionToken } from '../gui_session';
import { consolePush } from '../../stores/console';
import { formatDurationMs, pushUiConsole, renderStudioLogRecord, type StudioLogRecord } from './gui_console';
import { makeErrorStreamHandle, makeResolvedStreamHandle, makeStreamHandleFromProducer } from './stream_handle';
import { installWindowVfsBackend } from '../window_vfs_bindings';
import { PortablePathTrie, portableCaseKey } from '../portable_path_key';
import { compareUtf8 } from '../utf8_order';

const WORKSPACE_ROOT = '/workspace';
const ROOT = '/';
// Host-owned compiler state must remain outside every guest-visible project
// namespace. Project roots that overlap this prefix are rejected below.
const HOST_PRIVATE_VFS_ROOT = '/__volang_studio_host';
const SOURCE_CACHE_ROOT = `${WORKSPACE_ROOT}/.volang/apps/studio/sources`;
const URL_SESSION_ROOT = `${WORKSPACE_ROOT}/.volang/apps/studio/sessions/url`;
const LOCAL_SESSION_ROOT = `${WORKSPACE_ROOT}/.volang/apps/studio/sessions/local`;
const LOCAL_PROJECT_ENDPOINT = '/__vo_studio_local_project';
const GITHUB_SOURCE_ROOT = `${SOURCE_CACHE_ROOT}/github`;
const GITHUB_SESSION_ROOT = `${WORKSPACE_ROOT}/.volang/apps/studio/sessions/github`;
const GITHUB_API_ROOT = 'https://api.github.com';
const GITHUB_FILE_FETCH_CONCURRENCY = 8;
const RUNTIME_PERSIST_ROOT = '/persist';
const RUNTIME_PERSIST_STORAGE_KEY = 'volang.studio.runtimeVfsPersist.v1';
const RUNTIME_PERSIST_MAX_FILE_BYTES = 256 * 1024;
const RUNTIME_PERSIST_MAX_TOTAL_BYTES = 1024 * 1024;
const RUNTIME_PERSIST_MAX_SNAPSHOT_CHARS = 4 * 1024 * 1024;
const sessionWorkspaceDiscovery = new Map<string, WorkspaceDiscoveryMode>();
const DISPLAY_PULSE_DELAY_MS = 0xFFFFFFFF;
const PERF_SAMPLE_WINDOW = 240;
const FRAME_BUDGET_120_MS = 1000 / 120;
const DISPLAY_PULSE_SLOW_120_MS = FRAME_BUDGET_120_MS * 1.25;
const DISPLAY_PULSE_SLOW_60_MS = (1000 / 60) * 1.1;
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
const readOnlyStaticRoots = new Set<string>();
const completedGitHubSourceCaches = new Set<string>();
const textEncoder = new TextEncoder();
const utf8Decoder = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });
let studioWasmPromise: Promise<StudioWasm> | null = null;
let vfsBindingsInstalled = false;
let runtimePersistLifecycleInstalled = false;

interface GitHubRepoInput {
  owner: string;
  repo: string;
  ref: string | null;
  commit: string | null;
  subdir: string | null;
}

interface GitHubTreeItem {
  path?: string;
  mode?: string;
  sha?: string;
  type?: string;
}

interface GitHubTreeMetadata {
  tree?: GitHubTreeItem[];
  truncated?: boolean;
}

interface GitHubSourceFile {
  path: string;
  mode: number;
  sha: string;
}

interface GitHubBlobMetadata {
  content?: string;
  encoding?: string;
}

interface GitHubCommitMetadata {
  sha?: string;
}

interface RuntimePersistFile {
  contentBase64: string;
  mode?: number;
  modTime?: number;
}

interface RuntimePersistDir {
  mode?: number;
  modTime?: number;
}

interface RuntimePersistSnapshot {
  schemaVersion?: number;
  files?: Record<string, RuntimePersistFile>;
  dirs?: Record<string, RuntimePersistDir>;
}

interface ResolvedGitHubSource {
  owner: string;
  repo: string;
  requestedRef: string | null;
  fetchRef: string;
  resolvedCommit: string | null;
  subdir: string | null;
  htmlUrl: string;
  sourceCacheRoot: string;
  sessionRoot: string;
  projectRoot: string;
}

interface PreparedLocalProjectSnapshot {
  projectPath: string;
  projectRelativePath: string;
  files: PreparedStaticFile[];
}

interface StaticPackageFile {
  path: string;
  size: number;
  digest: string;
  content?: string;
  contentBase64?: string;
  mode?: number;
}

interface PreparedStaticFile {
  relative: string;
  bytes: Uint8Array;
  mode: number;
}

interface PreparedStaticTree {
  root: string;
  files: PreparedStaticFile[];
  readOnly?: boolean;
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

interface DetachedOpenVfsNode {
  isDir: boolean;
  bytes: Uint8Array;
  pathBytes: number;
  mode: number;
  modTime: number;
  openCount: number;
}

type OpenVfsFile = {
  path: string;
  flags: number;
  position: number;
  detached?: DetachedOpenVfsNode;
};

const openVfsFiles = new Map<number, OpenVfsFile>();
let nextVfsFd = 100;
let runtimeVfsRoot = ROOT;
let runtimeVfsFloor: string | null = null;
let liveVfsBytes = 0;
let liveVfsPathBytes = 0;
let orphanVfsBytes = 0;
let orphanVfsPathBytes = 0;
let orphanVfsNodes = 0;
let openVfsPathBytes = 0;
let textCacheBytes = 0;

const O_RDONLY = 0;
const O_WRONLY = 1;
const O_RDWR = 2;
const O_APPEND = 8;
const O_CREATE = 16;
const O_EXCL = 32;
const O_SYNC = 64;
const O_TRUNC = 128;
const KNOWN_VFS_OPEN_FLAGS = O_WRONLY | O_RDWR | O_APPEND | O_CREATE | O_EXCL | O_SYNC | O_TRUNC;
const VFS_ACCESS_MASK = 0x3;
const VFS_MUTABLE_MODE_BITS = 0o777 | (1 << 23) | (1 << 22) | (1 << 20);
const MAX_VFS_FILE_BYTES = 256 * 1024 * 1024;
const MAX_VFS_IO_BYTES = 64 * 1024 * 1024;
const MAX_VFS_TOTAL_BYTES = 512 * 1024 * 1024;
const MAX_VFS_NODES = 100_000;
const MAX_VFS_PATH_KEY_BYTES = 16 * 1024 * 1024;
const MAX_VFS_TEXT_CACHE_BYTES = 16 * 1024 * 1024;
const MAX_OPEN_VFS_FILES = 65_536;
const MAX_VFS_PATH_DEPTH = 256;
const MAX_VFS_PATH_LENGTH = 4096;
const MAX_VFS_NAME_LENGTH = 255;
const MAX_VFS_DIRECTORY_ENTRIES = 100_000;
const MAX_STATIC_PACKAGE_FILES = 20_000;
const MAX_STATIC_PACKAGE_FILE_BYTES = 256 * 1024 * 1024;
const MAX_STATIC_PACKAGE_TOTAL_BYTES = 512 * 1024 * 1024;
const MAX_IMPORTED_TAR_BYTES = MAX_STATIC_PACKAGE_TOTAL_BYTES + (MAX_STATIC_PACKAGE_FILES * 1024);
const MAX_STATIC_SOURCE_PAYLOAD_BYTES = 64 * 1024 * 1024;
const MAX_LOCAL_PROJECT_SNAPSHOT_BYTES = 128 * 1024 * 1024;
const FIRST_VFS_FD = 100;
const LAST_VFS_FD = 0x7fffffff;
const ERR_NOT_EXIST = 'file does not exist';
const ERR_EXIST = 'file already exists';
const ERR_NOT_DIR = 'not a directory';
const ERR_IS_DIR = 'is a directory';
const ERR_INVALID = 'invalid argument';
const ERR_BAD_FD = 'file already closed';
const ERR_PERMISSION = 'permission denied';
const ERR_FILE_TOO_LARGE = 'file too large';
const ERR_OUT_OF_MEMORY = 'out of memory';

function invalidVfsArgument(detail: string): string {
  return `${ERR_INVALID}: ${detail}`;
}

function validVfsMode(mode: number): boolean {
  return Number.isSafeInteger(mode) && mode >= 0 && mode <= 0xffffffff;
}

function normalizedVfsMode(mode: number): number {
  return mode & VFS_MUTABLE_MODE_BITS;
}

function safeVfsInteger(value: number, min: number, max: number): boolean {
  return Number.isSafeInteger(value) && value >= min && value <= max;
}

function allocateVfsFd(): number | null {
  if (openVfsFiles.size >= MAX_OPEN_VFS_FILES) return null;
  for (let attempts = 0; attempts <= MAX_OPEN_VFS_FILES; attempts += 1) {
    const candidate = nextVfsFd;
    nextVfsFd = candidate >= LAST_VFS_FD ? FIRST_VFS_FD : candidate + 1;
    if (!openVfsFiles.has(candidate)) return candidate;
  }
  return null;
}

function tryTrackOpenVfsFile(fd: number, file: OpenVfsFile): boolean {
  if (openVfsFiles.has(fd)) throw new Error('VFS descriptor is already tracked');
  const pathBytes = vfsPathKeyBytes(file.path);
  if (!canAllocateVfs(0, 0, pathBytes)) return false;
  openVfsFiles.set(fd, file);
  openVfsPathBytes += pathBytes;
  return true;
}

function deleteTrackedOpenVfsFile(fd: number): OpenVfsFile | undefined {
  const file = openVfsFiles.get(fd);
  if (!file) return undefined;
  openVfsFiles.delete(fd);
  openVfsPathBytes -= vfsPathKeyBytes(file.path);
  return file;
}

function projectedOpenVfsPathBytesAfterRename(oldPath: string, newPath: string): number {
  let total = openVfsPathBytes;
  for (const file of openVfsFiles.values()) {
    if (!file.detached && (file.path === oldPath || file.path.startsWith(`${oldPath}/`))) {
      const moved = `${newPath}${file.path.slice(oldPath.length)}`;
      total += vfsPathKeyBytes(moved) - vfsPathKeyBytes(file.path);
    }
  }
  return total;
}

function vfsPathKeyBytes(path: string): number {
  return textEncoder.encode(path).byteLength;
}

function aggregateVfsPathKeyBytes(
  directoryPaths: Iterable<string>,
  filePaths: Iterable<string>,
): number {
  let total = 0;
  for (const path of directoryPaths) {
    total += vfsPathKeyBytes(path);
    if (!Number.isSafeInteger(total) || total > MAX_VFS_PATH_KEY_BYTES) return total;
  }
  for (const path of filePaths) {
    total += vfsPathKeyBytes(path);
    if (!Number.isSafeInteger(total) || total > MAX_VFS_PATH_KEY_BYTES) return total;
  }
  return total;
}

function addVfsDirectoryKey(path: string): boolean {
  if (directories.has(path)) return false;
  directories.add(path);
  liveVfsPathBytes += vfsPathKeyBytes(path);
  return true;
}

function deleteVfsDirectoryKey(path: string): boolean {
  if (!directories.delete(path)) return false;
  liveVfsPathBytes -= vfsPathKeyBytes(path);
  return true;
}

function setVfsFileKey(path: string, bytes: Uint8Array): void {
  if (!vfsFiles.has(path)) liveVfsPathBytes += vfsPathKeyBytes(path);
  vfsFiles.set(path, bytes);
}

function deleteVfsFileKey(path: string): boolean {
  if (!vfsFiles.delete(path)) return false;
  liveVfsPathBytes -= vfsPathKeyBytes(path);
  return true;
}

function canAllocateVfs(nodes: number, bytes: number, pathBytes = 0): boolean {
  return directories.size + vfsFiles.size + orphanVfsNodes + nodes <= MAX_VFS_NODES
    && liveVfsBytes + orphanVfsBytes + bytes <= MAX_VFS_TOTAL_BYTES
    && liveVfsPathBytes + orphanVfsPathBytes + openVfsPathBytes + pathBytes
      <= MAX_VFS_PATH_KEY_BYTES;
}

resetWorkspaceState();
installRuntimeVfsPersistenceLifecycle();

function percentile(sorted: number[], fraction: number): number {
  if (sorted.length === 0) return 0;
  const index = Math.min(sorted.length - 1, Math.max(0, Math.ceil(sorted.length * fraction) - 1));
  return sorted[index];
}

function countAbove(samples: number[], threshold: number): number {
  let count = 0;
  for (const sample of samples) {
    if (sample > threshold) count += 1;
  }
  return count;
}

function formatMs(value: number): string {
  return `${value.toFixed(2)}ms`;
}

export class WebBackend implements Backend {
  readonly platform = 'wasm' as const;

  private guiOperationChain: Promise<void> = Promise.resolve();
  private readonly guiSession = new GuiSessionBinding();
  private guiFatalError: Error | null = null;
  private guiGuestExitHandler: ((session: GuiSessionToken, exitCode: number) => void) | null = null;
  private guiHostTimers = new Map<string, { kind: 'timeout' | 'raf'; id: number }>();
  private guiFirstRenderWaiter: { sessionId: number; resolve: (bytes: Uint8Array) => void; reject: (error: unknown) => void } | null = null;
  private guiDisplayPulseWaitWindow: number[] = [];

  setGuiGuestExitHandler(handler: ((session: GuiSessionToken, exitCode: number) => void) | null): void {
    this.guiGuestExitHandler = handler;
  }

  private activeGuiSessionId(): number {
    return this.guiSession.active?.id ?? 0;
  }

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

  private drainPendingGuiHostEvents(wasm: StudioWasm, sessionId = this.activeGuiSessionId()): void {
    while (true) {
      const event = wasm.pollPendingHostEvent();
      if (!event) {
        return;
      }
      this.scheduleGuiHostEvent(event.key, event.delayMs, sessionId);
    }
  }

  private scheduleGuiHostEvent(key: string, delayMs: number, sessionId: number): void {
    if (!this.guiSession.isActiveId(sessionId) || this.guiFatalError) {
      return;
    }
    const existing = this.guiHostTimers.get(key);
    if (existing) {
      if (existing.kind === 'raf') {
        cancelAnimationFrame(existing.id);
      } else {
        clearTimeout(existing.id);
      }
    }
    const fire = (): void => {
      this.guiHostTimers.delete(key);
      void this.runGuiEventSerialized(
        (wasm) => {
          wasm.wakeHostEvent(key);
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
      }).catch((error) => {
        const fatalError = this.recordGuiFatalError(error, sessionId);
        console.error('[Vo Studio] GUI host event failed', fatalError);
      });
    };
    const handle = delayMs === DISPLAY_PULSE_DELAY_MS
      ? this.scheduleGuiDisplayPulse(fire)
      : { kind: 'timeout' as const, id: window.setTimeout(fire, Math.max(0, delayMs)) };
    this.guiHostTimers.set(key, handle);
  }

  private recordGuiFatalError(error: unknown, sessionId: number): Error {
    const fatalError = error instanceof Error ? error : new Error(String(error));
    const session = this.guiSession.active;
    if (!session || session.id !== sessionId) {
      return fatalError;
    }
    if (!this.guiFatalError) {
      this.guiFatalError = fatalError;
      const exitCode = guestExitCode(error);
      if (exitCode !== null) {
        this.guiGuestExitHandler?.(session, exitCode);
      }
    }
    this.clearGuiHostTimers();
    this.rejectGuiFirstRenderWaiter(this.guiFatalError);
    setStandaloneGuiEventDispatcher(null);
    return this.guiFatalError;
  }

  private scheduleGuiDisplayPulse(fire: () => void): { kind: 'raf'; id: number } {
    const scheduledAtMs = performance.now();
    const id = requestAnimationFrame(() => {
      this.recordGuiDisplayPulseWait(performance.now() - scheduledAtMs);
      fire();
    });
    return { kind: 'raf', id };
  }

  private recordGuiDisplayPulseWait(waitMs: number): void {
    if (!shouldEmitVoplayPerfConsoleDiagnostics()) {
      return;
    }
    this.guiDisplayPulseWaitWindow.push(waitMs);
    if (this.guiDisplayPulseWaitWindow.length < PERF_SAMPLE_WINDOW) {
      return;
    }
    const samples = this.guiDisplayPulseWaitWindow;
    const sorted = [...samples].sort((a, b) => a - b);
    const p50 = percentile(sorted, 0.5);
    const p90 = percentile(sorted, 0.9);
    const p99 = percentile(sorted, 0.99);
    const max = sorted[sorted.length - 1] ?? 0;
    console.info(
      `[studio-perf] gui display pulse window samples=${samples.length}` +
        ` wait p50/p90/p99/max=${formatMs(p50)}/${formatMs(p90)}/${formatMs(p99)}/${formatMs(max)}` +
        ` slow120=${countAbove(samples, DISPLAY_PULSE_SLOW_120_MS)}/${samples.length}` +
        ` slow60=${countAbove(samples, DISPLAY_PULSE_SLOW_60_MS)}/${samples.length}` +
        ` visibility=${document.visibilityState}` +
        ` focus=${document.hasFocus()}` +
        ` timers=${this.guiHostTimers.size}`,
    );
    this.guiDisplayPulseWaitWindow = [];
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
      if (!this.guiSession.isActiveId(sessionId)) {
        return;
      }
      await this.dispatchGuiEventAsyncSerialized(handlerId, payload, sessionId);
    });
  }

  private async runGuiEventSerialized<T>(
    runWithWasm: (wasm: StudioWasm) => T | Promise<T>,
    staleValue: T,
    sessionId = this.activeGuiSessionId(),
  ): Promise<T> {
    if (!this.guiSession.isActiveId(sessionId)) {
      return staleValue;
    }
    if (this.guiFatalError) {
      throw this.guiFatalError;
    }

    const run = async (): Promise<T> => {
      if (!this.guiSession.isActiveId(sessionId)) {
        return staleValue;
      }
      if (this.guiFatalError) {
        throw this.guiFatalError;
      }
      const wasm = await getStudioWasm();
      if (!this.guiSession.isActiveId(sessionId)) {
        return staleValue;
      }
      try {
        return await runWithWasm(wasm);
      } catch (error) {
        if (!this.guiSession.isActiveId(sessionId)) {
          return staleValue;
        }
        throw this.recordGuiFatalError(error, sessionId);
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
    if (!this.guiSession.isActiveId(sessionId)) {
      throw new Error('GUI backend session superseded');
    }
  }

  private async dispatchGuiEventSerialized(
    handlerId: number,
    payload: string,
    sessionId = this.activeGuiSessionId(),
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
    sessionId = this.activeGuiSessionId(),
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
    const normalized = checkedPublicVfsPath(path);
    throwPublicVfsError('write file', normalized, vfsWriteFile(normalized, textEncoder.encode(content), 0o644));
  }

  async mkdir(path: string): Promise<void> {
    const normalized = checkedPublicVfsPath(path);
    throwPublicVfsError('create directory', normalized, vfsMkdirAll(normalized, 0o755));
  }

  async removeEntry(path: string, recursive: boolean): Promise<void> {
    const normalized = checkedPublicVfsPath(path);
    const error = recursive ? vfsRemoveAll(normalized) : vfsRemove(normalized);
    throwPublicVfsError('remove entry', normalized, error);
  }

  async renameEntry(oldPath: string, newPath: string): Promise<void> {
    const oldNorm = checkedPublicVfsPath(oldPath);
    const newNorm = checkedPublicVfsPath(newPath);
    throwPublicVfsError('rename entry', `${oldNorm} -> ${newNorm}`, vfsRename(oldNorm, newNorm));
  }

  async copyEntry(src: string, dst: string): Promise<void> {
    const srcNorm = checkedPublicVfsPath(src);
    const dstNorm = checkedPublicVfsPath(dst);
    throwPublicVfsError('copy entry', `${srcNorm} -> ${dstNorm}`, copyVfsEntryAtomically(srcNorm, dstNorm));
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

  async checkVo(path: string): Promise<CheckResult> {
    const normalized = normalizePath(path);
    const result = await compileEntryForCommand(normalized, false);
    return { ok: result.ok, errors: result.errors };
  }

  async compileVo(path: string): Promise<CompileResult> {
    const normalized = normalizePath(path);
    const result = await compileEntryForCommand(normalized, true);
    if (!result.ok || !result.bytecode) {
      return { ok: false, errors: result.errors, outputPath: undefined };
    }
    const outputPath = defaultCompilerOutputPath(normalized);
    throwPublicVfsError('write compiler output', outputPath, vfsWriteFile(outputPath, result.bytecode, 0o644));
    return { ok: true, errors: [], outputPath };
  }

  async formatVo(_path: string): Promise<string> {
    throw new Error('vo format is not wired in WASM mode yet');
  }

  async buildVo(path: string, output?: string): Promise<BuildResult> {
    const normalized = normalizePath(path);
    const result = await compileEntryForCommand(normalized, true);
    if (!result.ok || !result.bytecode) {
      return { ok: false, errors: result.errors, outputPath: undefined };
    }
    const outputPath = output ? checkedPublicVfsPath(output) : defaultCompilerOutputPath(normalized);
    throwPublicVfsError('write build output', outputPath, vfsWriteFile(outputPath, result.bytecode, 0o644));
    return { ok: true, errors: [], outputPath };
  }

  async dumpVo(path: string): Promise<string> {
    const normalized = normalizePath(path);
    const workspaceDiscovery = workspaceDiscoveryForPath(normalized);
    const wasm = await getStudioWasm();
    await wasm.prepareEntry(normalized, workspaceDiscovery);
    return wasm.dumpEntry(normalized, workspaceDiscovery);
  }

  async dumpGuiBytecode(path: string): Promise<string> {
    const normalized = normalizePath(path);
    const workspaceDiscovery = workspaceDiscoveryForPath(normalized);
    const wasm = await getStudioWasm();
    await wasm.prepareEntry(normalized, workspaceDiscovery);
    return wasm.dumpGuiEntry(normalized, workspaceDiscovery);
  }

  runVo(path: string, _opts?: RunOpts): StreamHandle<RunEvent> {
    const normalized = normalizePath(path);
    const workspaceDiscovery = workspaceDiscoveryForPath(normalized);
    return makeStreamHandleFromProducer<RunEvent>((emit, onDone, onError) => {
      (async () => {
        const start = performance.now();
        const wasm = await getStudioWasm();
        await wasm.prepareEntry(normalized, workspaceDiscovery);
        const result = withRuntimeVfsRoot(
          findProjectRootForEntry(normalized) ?? dirname(normalized),
          () => wasm.compileRunEntry(normalized, workspaceDiscovery),
        );
        if (result.output) {
          for (const line of result.output.split('\n')) {
            emit({ kind: 'stdout', text: line });
          }
        }
        const durationMs = Math.round(performance.now() - start);
        emit({ kind: 'done', exitCode: result.exitCode, durationMs });
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

  async runGui(path: string, session: GuiSessionToken): Promise<GuiRunOutput> {
    const normalized = normalizePath(path);
    const workspaceDiscovery = workspaceDiscoveryForPath(normalized);
    const targetLabel = displayPath(normalized);
    const sessionId = session.id;
    this.guiSession.activate(session);
    setActiveVoplayPerfSessionId(sessionId);
    this.guiFatalError = null;
    this.clearGuiHostTimers();
    this.rejectGuiFirstRenderWaiter(new Error('GUI session superseded'));
    setStandaloneGuiEventDispatcher(null);
    resetGuiHostBridge();
    const totalStart = performance.now();
    try {
      setRuntimeVfsRoot(findProjectRootForEntry(normalized) ?? dirname(normalized));
      const startup = await this.serializeGuiOperation(async () => {
        consolePush('system', `Opening GUI ${targetLabel}`);
        const wasm = await getStudioWasm();
        this.assertGuiSessionCurrent(sessionId);
        consolePush('system', `Preparing dependencies for ${targetLabel}...`);
        const prepareStart = performance.now();
        await wasm.prepareEntry(normalized, workspaceDiscovery);
        const prepareDurationMs = performance.now() - prepareStart;
        consolePush('system', `Prepared dependencies for ${targetLabel} in ${formatDurationMs(prepareDurationMs)}`);
        this.assertGuiSessionCurrent(sessionId);
        this.installStandaloneGuiDispatcher(sessionId);
        consolePush('system', `Compiling and starting GUI ${targetLabel}...`);
        const compileStart = performance.now();
        const compileResult = wasm.compileGui(normalized, workspaceDiscovery);
        this.assertGuiSessionCurrent(sessionId);
        const compileDurationMs = performance.now() - compileStart;
        const wasmExtensionLabels = compileResult.wasmExtensions.map((ext) => `${ext.name}=>${ext.moduleKey ?? ext.name}`);
        const wasmExtensionSummary = wasmExtensionLabels.length > 0 ? wasmExtensionLabels.join(', ') : 'none';
        if (shouldEmitVoplayPerfConsoleDiagnostics()) {
          console.info(`[studio-gui] compileGui ${normalized} ${Math.round(compileDurationMs)}ms`);
          console.info(`[studio-gui] wasmExtensions ${normalized} count=${wasmExtensionLabels.length} names=${wasmExtensionSummary}`);
        }
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
      if (shouldEmitVoplayPerfConsoleDiagnostics()) {
        console.info(`[studio-gui] total open ${normalized} ${Math.round(totalDurationMs)}ms`);
      }
      return {
        ...startup.output,
        renderBytes,
      };
    } catch (error) {
      if (this.guiSession.isActive(session)) {
        setActiveVoplayPerfSessionId(null);
        const failure = error instanceof Error ? error : new Error(String(error));
        this.guiFatalError = failure;
        this.clearGuiHostTimers();
        this.rejectGuiFirstRenderWaiter(failure);
        setStandaloneGuiEventDispatcher(null);
        resetGuiHostBridge();
        await this.serializeGuiOperation(async () => {
          if (!this.guiSession.isActive(session)) return;
          const wasm = await getStudioWasm();
          wasm.stopGui();
        }).catch((cleanupError) => {
          console.error('[Vo Studio] failed GUI startup cleanup failed', cleanupError);
        });
        if (this.guiSession.isActive(session)) {
          clearRuntimeVfsRoot();
          this.guiSession.clear(session);
        }
      }
      throw error;
    }
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

  async pushAndPollIslandTransport(data: Uint8Array): Promise<Uint8Array[]> {
    return this.runGuiEventSerialized(
      (wasm) => {
        wasm.pushIslandData(data);
        this.drainPendingGuiHostEvents(wasm);
        const frames: Uint8Array[] = [];
        for (let i = 0; i < 32; i += 1) {
          const frame = wasm.pollIslandData();
          if (frame.length === 0) {
            break;
          }
          frames.push(new Uint8Array(frame));
        }
        return frames;
      },
      [],
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
    this.guiSession.clear();
    setActiveVoplayPerfSessionId(null);
    this.guiFatalError = null;
    this.clearGuiHostTimers();
    this.rejectGuiFirstRenderWaiter(new Error('GUI session superseded'));
    setStandaloneGuiEventDispatcher(null);
    resetGuiHostBridge();
    try {
      await this.serializeGuiOperation(async () => {
        const wasm = await getStudioWasm();
        wasm.stopGui();
      });
    } finally {
      if (!this.guiSession.active) clearRuntimeVfsRoot();
    }
  }

  async getRendererBridgeVfsSnapshot(path: string): Promise<RendererBridgeVfsSnapshot> {
    const wasm = await getStudioWasm();
    const normalized = normalizePath(path);
    const snapshot = wasm.getRenderIslandVfsSnapshot(normalized, workspaceDiscoveryForPath(normalized));
    setRuntimeVfsRoot(snapshot.rootPath);
    return snapshot;
  }

  async voInit(path: string, module: string, mainContent: string): Promise<string> {
    const normalized = checkedPublicVfsPath(path);
    const wasm = await getStudioWasm();
    const manifest = wasm.renderInitialModuleManifest(module);
    const manifestBytes = textEncoder.encode(manifest);
    const mainBytes = textEncoder.encode(mainContent);
    throwPublicVfsError('create module directory', normalized, vfsMkdir(normalized, 0o755));
    const manifestPath = `${normalized}/vo.mod`;
    const mainPath = `${normalized}/main.vo`;
    const manifestError = vfsWriteFile(manifestPath, manifestBytes, 0o644);
    const mainError = manifestError == null
      ? vfsWriteFile(mainPath, mainBytes, 0o644)
      : null;
    const error = manifestError == null
      ? (mainError == null ? null : `create module entry ${mainPath}: ${mainError}`)
      : `create module manifest ${manifestPath}: ${manifestError}`;
    if (error != null) {
      const rollbackError = vfsRemoveAll(normalized);
      throw new Error(
        rollbackError == null
          ? error
          : `${error}; rollback ${normalized} failed: ${rollbackError}`,
      );
    }
    return normalized;
  }

  async voVersion(): Promise<string> {
    const wasm = await getStudioWasm();
    return wasm.voVersion();
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
      const normalized = checkedPublicVfsPath(file.path);
      throwPublicVfsError('create project directory', dirname(normalized), vfsMkdirAll(dirname(normalized), 0o755));
      throwPublicVfsError('create project file', normalized, vfsWriteFile(normalized, textEncoder.encode(file.content), 0o644));
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

function checkedPublicVfsPath(path: string): string {
  const normalized = normalizePath(path);
  const [checked, error] = checkedRuntimeVfsPath(normalized);
  if (error || !checked || checked !== normalized) {
    throw new Error(`Invalid VFS path ${JSON.stringify(path)}: ${error ?? ERR_INVALID}`);
  }
  return checked;
}

function throwPublicVfsError(operation: string, path: string, error: string | null): void {
  if (error) throw new Error(`Could not ${operation} ${path}: ${error}`);
}

function copyVfsEntryAtomically(src: string, dst: string): string | null {
  if (src === ROOT || dst === ROOT) return ERR_PERMISSION;
  const sourceIsFile = vfsFiles.has(src);
  const sourceIsDirectory = directories.has(src);
  if (!sourceIsFile && !sourceIsDirectory) return ERR_NOT_EXIST;
  if (src === dst || (sourceIsDirectory && (vfsPathContains(src, dst) || vfsPathContains(dst, src)))) {
    return invalidVfsArgument('source and destination trees overlap');
  }
  if (overlapsReadOnlyStaticTree(dst)) return ERR_PERMISSION;

  type PlannedDirectory = { mode: number; modTime: number };
  type PlannedFile = { bytes: Uint8Array; mode: number; modTime: number };
  const directoryPlans = new Map<string, PlannedDirectory>();
  const filePlans = new Map<string, PlannedFile>();
  let plannedPathKeyBytes = 0;
  const reservePlannedPath = (path: string, exists: boolean): boolean => {
    if (exists) return true;
    plannedPathKeyBytes += vfsPathKeyBytes(path);
    return Number.isSafeInteger(plannedPathKeyBytes)
      && plannedPathKeyBytes <= MAX_VFS_PATH_KEY_BYTES;
  };

  if (sourceIsFile) {
    if (!reservePlannedPath(dst, filePlans.has(dst))) return ERR_OUT_OF_MEMORY;
    filePlans.set(dst, {
      bytes: vfsFiles.get(src)!,
      mode: vfsFileModes.get(src) ?? 0o644,
      modTime: vfsFileModTimes.get(src) ?? Date.now(),
    });
  } else {
    for (const sourcePath of [...directories].filter((path) => vfsPathContains(src, path))) {
      const targetPath = `${dst}${sourcePath.slice(src.length)}`;
      if (!reservePlannedPath(targetPath, directoryPlans.has(targetPath))) return ERR_OUT_OF_MEMORY;
      directoryPlans.set(targetPath, {
        mode: vfsDirModes.get(sourcePath) ?? 0o755,
        modTime: vfsDirModTimes.get(sourcePath) ?? Date.now(),
      });
    }
    for (const [sourcePath, bytes] of [...vfsFiles].filter(([path]) => vfsPathContains(src, path))) {
      const targetPath = `${dst}${sourcePath.slice(src.length)}`;
      if (!reservePlannedPath(targetPath, filePlans.has(targetPath))) return ERR_OUT_OF_MEMORY;
      filePlans.set(targetPath, {
        bytes,
        mode: vfsFileModes.get(sourcePath) ?? 0o644,
        modTime: vfsFileModTimes.get(sourcePath) ?? Date.now(),
      });
    }
  }

  const addMissingAncestors = (path: string): string | null => {
    let parent = dirname(path);
    while (parent !== ROOT) {
      if (vfsFiles.has(parent) || filePlans.has(parent)) return ERR_NOT_DIR;
      if (!directories.has(parent) && !directoryPlans.has(parent)) {
        if (!reservePlannedPath(parent, false)) return ERR_OUT_OF_MEMORY;
        directoryPlans.set(parent, { mode: 0o755, modTime: Date.now() });
      }
      parent = dirname(parent);
    }
    return null;
  };
  for (const path of [...directoryPlans.keys(), ...filePlans.keys()]) {
    const [checked, pathError] = checkedRuntimeVfsPath(path);
    if (pathError || checked !== path) return pathError ?? ERR_INVALID;
    const ancestorError = addMissingAncestors(path);
    if (ancestorError) return ancestorError;
  }

  const finalDirectories = new Set(directories);
  for (const path of directoryPlans.keys()) finalDirectories.add(path);
  const finalFiles = new Map(vfsFiles);
  for (const [path, plan] of filePlans) finalFiles.set(path, plan.bytes);
  for (const path of finalFiles.keys()) {
    if (finalDirectories.has(path)) return ERR_IS_DIR;
  }
  for (const path of directoryPlans.keys()) {
    if (finalFiles.has(path)) return ERR_NOT_DIR;
  }

  for (const path of directoryPlans.keys()) {
    if (directories.has(path)) continue;
    const parent = dirname(path);
    if (directories.has(parent) && !canMutateVfsDirectory(parent)) return ERR_PERMISSION;
  }
  for (const path of filePlans.keys()) {
    const parent = dirname(path);
    if (directories.has(parent) && !canMutateVfsDirectory(parent)) return ERR_PERMISSION;
    const existingMode = vfsFileModes.get(path);
    if (existingMode !== undefined && (existingMode & 0o222) === 0) return ERR_PERMISSION;
  }

  let finalBytes = liveVfsBytes;
  for (const [path, plan] of filePlans) {
    finalBytes += plan.bytes.byteLength - (vfsFiles.get(path)?.byteLength ?? 0);
  }
  if (
    !Number.isSafeInteger(finalBytes)
    || finalBytes + orphanVfsBytes > MAX_VFS_TOTAL_BYTES
    || finalDirectories.size + finalFiles.size + orphanVfsNodes > MAX_VFS_NODES
    || aggregateVfsPathKeyBytes(finalDirectories, finalFiles.keys())
      + orphanVfsPathBytes
      + openVfsPathBytes
      > MAX_VFS_PATH_KEY_BYTES
  ) {
    return ERR_OUT_OF_MEMORY;
  }
  const childCounts = new Map<string, number>();
  for (const path of finalDirectories) {
    if (path !== ROOT) childCounts.set(dirname(path), (childCounts.get(dirname(path)) ?? 0) + 1);
  }
  for (const path of finalFiles.keys()) {
    childCounts.set(dirname(path), (childCounts.get(dirname(path)) ?? 0) + 1);
  }
  if ([...childCounts.values()].some((count) => count > MAX_VFS_DIRECTORY_ENTRIES)) {
    return 'directory contains too many entries';
  }

  const snapshot = {
    directories: new Set(directories),
    files: new Map(files),
    vfsFiles: new Map(vfsFiles),
    vfsFileModes: new Map(vfsFileModes),
    vfsFileModTimes: new Map(vfsFileModTimes),
    vfsDirModes: new Map(vfsDirModes),
    vfsDirModTimes: new Map(vfsDirModTimes),
    liveVfsBytes,
    liveVfsPathBytes,
    textCacheBytes,
  };
  try {
    for (const [path, plan] of [...directoryPlans].sort(([left], [right]) => (
      left.length - right.length || compareUtf8(left, right)
    ))) {
      if (directories.has(path)) continue;
      addVfsDirectoryKey(path);
      vfsDirModes.set(path, normalizedVfsMode(plan.mode));
      vfsDirModTimes.set(path, plan.modTime);
    }
    for (const [path, plan] of filePlans) {
      setVfsFile(path, plan.bytes, plan.mode, false);
      vfsFileModTimes.set(path, plan.modTime);
    }
    if ([...directoryPlans.keys(), ...filePlans.keys()].some(isRuntimePersistPath)) {
      persistRuntimeVfsSnapshot();
    }
    return null;
  } catch (error) {
    restoreMap(directories, snapshot.directories);
    restoreMap(files, snapshot.files);
    restoreMap(vfsFiles, snapshot.vfsFiles);
    restoreMap(vfsFileModes, snapshot.vfsFileModes);
    restoreMap(vfsFileModTimes, snapshot.vfsFileModTimes);
    restoreMap(vfsDirModes, snapshot.vfsDirModes);
    restoreMap(vfsDirModTimes, snapshot.vfsDirModTimes);
    liveVfsBytes = snapshot.liveVfsBytes;
    liveVfsPathBytes = snapshot.liveVfsPathBytes;
    textCacheBytes = snapshot.textCacheBytes;
    return error instanceof Error ? error.message : ERR_OUT_OF_MEMORY;
  }
}

function setRuntimeVfsRoot(path: string): void {
  const [normalized, error] = checkedRuntimeVfsPath(normalizePath(path || ROOT));
  if (
    error
    || !normalized
    || normalized === ROOT
    || vfsPathContains(HOST_PRIVATE_VFS_ROOT, normalized)
    || vfsPathContains(normalized, HOST_PRIVATE_VFS_ROOT)
  ) {
    throw new Error(`Guest project root must be a canonical non-root VFS path: ${JSON.stringify(path)}`);
  }
  runtimeVfsRoot = normalized;
  runtimeVfsFloor = runtimeVfsRoot;
}

function clearRuntimeVfsRoot(): void {
  runtimeVfsRoot = ROOT;
  runtimeVfsFloor = null;
}

function withRuntimeVfsRoot<T>(path: string, operation: () => T): T {
  const previousRoot = runtimeVfsRoot;
  const previousFloor = runtimeVfsFloor;
  try {
    setRuntimeVfsRoot(path);
    return operation();
  } finally {
    runtimeVfsRoot = previousRoot;
    runtimeVfsFloor = previousFloor;
  }
}

function checkedGuestVfsPath(path: string): [string | null, string | null] {
  if (typeof path !== 'string' || path.includes('\0')) return [null, ERR_INVALID];
  if (path.length === 0) return [null, ERR_NOT_EXIST];
  if (textEncoder.encode(path).length > MAX_VFS_PATH_LENGTH) return [null, ERR_INVALID];
  const floor = runtimeVfsFloor;
  if (
    floor === null
    || !vfsPathContains(floor, runtimeVfsRoot)
    || vfsPathContains(HOST_PRIVATE_VFS_ROOT, floor)
    || vfsPathContains(floor, HOST_PRIVATE_VFS_ROOT)
  ) return [null, ERR_PERMISSION];

  const floorParts = floor.split('/').filter(Boolean);
  const parts = path.startsWith(ROOT)
    ? [...floorParts]
    : runtimeVfsRoot.split('/').filter(Boolean);
  for (const part of path.split('/')) {
    if (!part || part === '.') continue;
    if (part === '..') {
      if (parts.length <= floorParts.length) return [null, ERR_PERMISSION];
      parts.pop();
      continue;
    }
    if (textEncoder.encode(part).length > MAX_VFS_NAME_LENGTH) return [null, ERR_INVALID];
    parts.push(part);
    if (parts.length > MAX_VFS_PATH_DEPTH) return [null, ERR_INVALID];
  }
  const normalized = parts.length === 0 ? ROOT : `/${parts.join('/')}`;
  if (
    !vfsPathContains(floor, normalized)
    || textEncoder.encode(normalized).length > MAX_VFS_PATH_LENGTH
  ) {
    return [null, ERR_PERMISSION];
  }
  return [normalized, null];
}

function guestVfsGetwd(): [string, string | null] {
  const floor = runtimeVfsFloor;
  if (
    floor === null
    || !vfsPathContains(floor, runtimeVfsRoot)
    || vfsPathContains(HOST_PRIVATE_VFS_ROOT, floor)
    || vfsPathContains(floor, HOST_PRIVATE_VFS_ROOT)
  ) return ['', ERR_PERMISSION];
  const relative = runtimeVfsRoot.slice(floor.length);
  return [relative || ROOT, null];
}

function checkedRuntimeVfsPath(path: string): [string | null, string | null] {
  if (typeof path !== 'string' || path.includes('\0')) return [null, ERR_INVALID];
  if (path.length === 0) return [null, ERR_NOT_EXIST];
  if (textEncoder.encode(path).length > MAX_VFS_PATH_LENGTH) return [null, ERR_INVALID];
  const parts = path.startsWith(ROOT)
    ? []
    : runtimeVfsRoot.split('/').filter(Boolean);
  for (const part of path.split('/')) {
    if (!part || part === '.') continue;
    if (part === '..') {
      parts.pop();
      continue;
    }
    if (textEncoder.encode(part).length > MAX_VFS_NAME_LENGTH) return [null, ERR_INVALID];
    parts.push(part);
    if (parts.length > MAX_VFS_PATH_DEPTH) return [null, ERR_INVALID];
  }
  const normalized = parts.length === 0 ? ROOT : `/${parts.join('/')}`;
  return textEncoder.encode(normalized).length <= MAX_VFS_PATH_LENGTH
    ? [normalized, null]
    : [null, ERR_INVALID];
}

function vfsPathContains(path: string, candidate: string): boolean {
  return path === ROOT || candidate === path || candidate.startsWith(`${path}/`);
}

function isWithinReadOnlyStaticTree(path: string): boolean {
  for (const root of readOnlyStaticRoots) {
    if (vfsPathContains(root, path)) return true;
  }
  return false;
}

function overlapsReadOnlyStaticTree(path: string): boolean {
  for (const root of readOnlyStaticRoots) {
    if (vfsPathContains(root, path) || vfsPathContains(path, root)) return true;
  }
  return false;
}

function canMutateVfsDirectory(path: string): boolean {
  if (isWithinReadOnlyStaticTree(path)) return false;
  const mode = vfsDirModes.get(path) ?? 0o755;
  return (mode & 0o222) !== 0 && (mode & 0o111) !== 0;
}

function vfsParentLookupError(path: string): string | null {
  if (path === ROOT) return null;
  const parts = path.split('/').filter(Boolean);
  let current = ROOT;
  for (let index = 0; index < parts.length; index += 1) {
    if (!directories.has(current)) {
      return vfsFiles.has(current) ? ERR_NOT_DIR : ERR_NOT_EXIST;
    }
    if (((vfsDirModes.get(current) ?? 0o755) & 0o111) === 0) {
      return ERR_PERMISSION;
    }
    if (index + 1 < parts.length) {
      current = current === ROOT ? `/${parts[index]}` : `${current}/${parts[index]}`;
    }
  }
  return null;
}

function hasVfsTrailingSlash(path: string): boolean {
  return path.length > 1 && path.endsWith('/');
}

function sortEntries(entries: FsEntry[]): FsEntry[] {
  return [...entries].sort((a, b) => {
    if (a.isDir !== b.isDir) {
      return a.isDir ? -1 : 1;
    }
    return compareUtf8(a.name, b.name);
  });
}

function getStudioWasm(): Promise<StudioWasm> {
  ensureVfsBindings();
  if (!studioWasmPromise) {
    (globalThis as Record<string, unknown>).__voStudioLogRecord = (record: StudioLogRecord) => {
      if (handleVoplayPerfHostLog(record)) {
        return;
      }
      pushStudioLogRecord(record);
      if (shouldEmitStudioLogDebug()) {
        console.debug('[studio-log]', record);
      }
    };
    studioWasmPromise = loadStudioWasm();
  }
  return studioWasmPromise;
}

function shouldEmitStudioLogDebug(): boolean {
  try {
    const params = new URLSearchParams(window.location.search);
    return params.has('studioLogDebug')
      || params.has('debug')
      || window.localStorage.getItem('studio.logDebug') === '1';
  } catch {
    return false;
  }
}

async function compileEntryForCommand(
  path: string,
  includeBytecode: boolean,
): Promise<{ ok: boolean; errors: DiagnosticError[]; bytecode: Uint8Array | null }> {
  const normalized = normalizePath(path);
  const workspaceDiscovery = workspaceDiscoveryForPath(normalized);
  try {
    const wasm = await getStudioWasm();
    await wasm.prepareEntry(normalized, workspaceDiscovery);
    return includeBytecode
      ? wasm.compileEntry(normalized, workspaceDiscovery)
      : wasm.checkEntry(normalized, workspaceDiscovery);
  } catch (error) {
    return {
      ok: false,
      errors: [diagnosticFromError(normalized, 'compile', error)],
      bytecode: null,
    };
  }
}

function diagnosticFromError(path: string, category: string, error: unknown): DiagnosticError {
  return {
    file: normalizePath(path),
    line: 0,
    column: 0,
    message: error instanceof Error ? error.message : String(error),
    category,
    moduleStage: null,
    moduleKind: null,
    modulePath: null,
    moduleVersion: null,
  };
}

function defaultCompilerOutputPath(path: string): string {
  return withExtension(compilerOutputBasePath(path), 'vob');
}

function compilerOutputBasePath(path: string): string {
  const normalized = normalizePath(path);
  if (directories.has(normalized)) {
    return findProjectRoot(normalized) ?? normalized;
  }
  const parent = dirname(normalized);
  return findProjectRoot(parent) ?? normalized;
}

function withExtension(path: string, extension: string): string {
  const normalized = normalizePath(path);
  const slash = normalized.lastIndexOf('/');
  const dot = normalized.lastIndexOf('.');
  if (dot > slash) {
    return `${normalized.slice(0, dot + 1)}${extension}`;
  }
  return `${normalized}.${extension}`;
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
    renameNoreplace: vfsRenameNoreplace,
    stat: vfsStat,
    readDir: vfsReadDir,
    chmod: vfsChmod,
    truncate: vfsTruncate,
    getwd: vfsGetwd,
    chdir: vfsChdir,
    readFile: vfsReadFile,
    readFileLimited: vfsReadFileLimited,
    writeFile: vfsWriteFile,
    resolveGuestPath: checkedGuestVfsPath,
    guestGetwd: guestVfsGetwd,
  });
  vfsBindingsInstalled = true;
}

function readTextFile(path: string): string | null {
  const normalized = normalizePath(path);
  const cached = files.get(normalized);
  if (cached !== undefined) {
    // Map insertion order is the cache's deterministic LRU order.
    files.delete(normalized);
    files.set(normalized, cached);
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
  cacheTextFile(normalized, decoded);
  return decoded;
}

function textCacheEntryBytes(path: string, text: string): number {
  const stringBytes = text.length * 2;
  const total = 64 + vfsPathKeyBytes(path) + stringBytes;
  return Number.isSafeInteger(total) ? total : Number.MAX_SAFE_INTEGER;
}

function deleteCachedTextFile(path: string): boolean {
  const text = files.get(path);
  if (text === undefined || !files.delete(path)) return false;
  textCacheBytes -= textCacheEntryBytes(path, text);
  return true;
}

function cacheTextFile(path: string, text: string): boolean {
  deleteCachedTextFile(path);
  const cost = textCacheEntryBytes(path, text);
  if (cost > MAX_VFS_TEXT_CACHE_BYTES) return false;
  while (textCacheBytes + cost > MAX_VFS_TEXT_CACHE_BYTES) {
    const oldest = files.keys().next().value as string | undefined;
    if (oldest === undefined) break;
    deleteCachedTextFile(oldest);
  }
  if (textCacheBytes + cost > MAX_VFS_TEXT_CACHE_BYTES) return false;
  files.set(path, text);
  textCacheBytes += cost;
  return true;
}

function clearTextFileCache(): void {
  files.clear();
  textCacheBytes = 0;
}

function tryDecodeUtf8(bytes: Uint8Array): string | null {
  try {
    return utf8Decoder.decode(bytes);
  } catch {
    return null;
  }
}

function setVfsFile(path: string, bytes: Uint8Array, mode = 0o644, persist = true): void {
  const normalized = normalizePath(path);
  const parent = dirname(normalized);
  const previous = vfsFiles.get(normalized);
  const growth = bytes.length - (previous?.length ?? 0);
  const missingDirectories = missingVfsDirectories(parent);
  const addedNodes = missingDirectories.length + (previous === undefined ? 1 : 0);
  const addedPathBytes = vfsPathsByteLength(missingDirectories)
    + (previous === undefined ? vfsPathKeyBytes(normalized) : 0);
  if (!canAllocateVfs(addedNodes, Math.max(0, growth), addedPathBytes)) {
    throw new Error(ERR_OUT_OF_MEMORY);
  }
  const copy = new Uint8Array(bytes);
  ensureDir(parent);
  liveVfsBytes += growth;
  setVfsFileKey(normalized, copy);
  vfsFileModes.set(normalized, validVfsMode(mode) ? normalizedVfsMode(mode) : 0o644);
  vfsFileModTimes.set(normalized, Date.now());
  deleteCachedTextFile(normalized);
  if (persist && isRuntimePersistPath(normalized)) {
    persistRuntimeVfsSnapshot();
  }
}

function deleteFile(path: string, persist = true): void {
  const normalized = normalizePath(path);
  detachOpenVfsPath(normalized, false);
  liveVfsBytes -= vfsFiles.get(normalized)?.length ?? 0;
  deleteCachedTextFile(normalized);
  deleteVfsFileKey(normalized);
  vfsFileModes.delete(normalized);
  vfsFileModTimes.delete(normalized);
  if (persist && isRuntimePersistPath(normalized)) {
    persistRuntimeVfsSnapshot();
  }
}

function detachOpenVfsPath(path: string, isDir: boolean): void {
  let detached: DetachedOpenVfsNode | undefined;
  for (const file of openVfsFiles.values()) {
    if (file.detached || file.path !== path) continue;
    detached ??= {
      isDir,
      bytes: isDir ? new Uint8Array(0) : (vfsFiles.get(path) ?? new Uint8Array(0)),
      pathBytes: vfsPathKeyBytes(path),
      mode: isDir ? (vfsDirModes.get(path) ?? 0o755) : (vfsFileModes.get(path) ?? 0o644),
      modTime: isDir ? (vfsDirModTimes.get(path) ?? 0) : (vfsFileModTimes.get(path) ?? 0),
      openCount: 0,
    };
    if (detached.openCount === 0) {
      orphanVfsNodes += 1;
      orphanVfsBytes += detached.bytes.length;
      orphanVfsPathBytes += detached.pathBytes;
    }
    detached.openCount += 1;
    file.detached = detached;
  }
}

function openVfsIsDirectory(file: OpenVfsFile): boolean {
  return file.detached?.isDir ?? directories.has(file.path);
}

function openVfsBytes(file: OpenVfsFile): Uint8Array {
  return file.detached?.bytes ?? vfsFiles.get(file.path) ?? new Uint8Array(0);
}

function isRuntimePersistPath(path: string): boolean {
  const normalized = normalizePath(path);
  return normalized === RUNTIME_PERSIST_ROOT || normalized.startsWith(`${RUNTIME_PERSIST_ROOT}/`);
}

function runtimePersistStorage(): Storage | null {
  if (typeof window === 'undefined') {
    return null;
  }
  try {
    return window.localStorage ?? null;
  } catch {
    return null;
  }
}

function isJsonRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

function persistedMode(value: unknown, fallback: number): number | null {
  if (value === undefined) return fallback;
  return typeof value === 'number' && validVfsMode(value) ? normalizedVfsMode(value) : null;
}

function persistedModTime(value: unknown): number | null {
  if (value === undefined) return Date.now();
  return typeof value === 'number' && safeVfsInteger(value, 0, Number.MAX_SAFE_INTEGER)
    ? value
    : null;
}

function loadRuntimeVfsSnapshot(): void {
  const storage = runtimePersistStorage();
  if (!storage) {
    return;
  }
  let parsed: unknown;
  try {
    const raw = storage.getItem(RUNTIME_PERSIST_STORAGE_KEY);
    if (!raw) {
      return;
    }
    if (raw.length > RUNTIME_PERSIST_MAX_SNAPSHOT_CHARS) {
      throw new Error('snapshot exceeds its encoded size limit');
    }
    parsed = JSON.parse(raw) as unknown;
  } catch (error) {
    console.warn('Studio runtime VFS persistence could not be loaded:', error);
    return;
  }
  if (!isJsonRecord(parsed) || parsed.schemaVersion !== 1) {
    return;
  }

  if (!directories.has(RUNTIME_PERSIST_ROOT)) {
    if (!canAllocateVfs(1, 0, vfsPathKeyBytes(RUNTIME_PERSIST_ROOT))) return;
    addVfsDirectoryKey(RUNTIME_PERSIST_ROOT);
    vfsDirModes.set(RUNTIME_PERSIST_ROOT, 0o755);
    vfsDirModTimes.set(RUNTIME_PERSIST_ROOT, Date.now());
  }

  const childCounts = new Map<string, number>();
  for (const directory of directories) {
    if (directory !== ROOT) {
      const parent = dirname(directory);
      childCounts.set(parent, (childCounts.get(parent) ?? 0) + 1);
    }
  }
  for (const path of vfsFiles.keys()) {
    const parent = dirname(path);
    childCounts.set(parent, (childCounts.get(parent) ?? 0) + 1);
  }

  const rawDirs = isJsonRecord(parsed.dirs) ? parsed.dirs : {};
  const dirEntries = Object.entries(rawDirs).sort(([left], [right]) => {
    const depthDifference = left.split('/').length - right.split('/').length;
    if (depthDifference !== 0) return depthDifference;
    return compareUtf8(left, right);
  });
  for (const [path, rawDir] of dirEntries) {
    if (!isJsonRecord(rawDir)) continue;
    const [normalized, pathError] = checkedRuntimeVfsPath(path);
    if (pathError || !normalized || normalized !== path || !isRuntimePersistPath(normalized)) {
      continue;
    }
    const mode = persistedMode(rawDir.mode, 0o755);
    const modTime = persistedModTime(rawDir.modTime);
    if (mode === null || modTime === null || vfsFiles.has(normalized)) continue;
    if (!directories.has(normalized)) {
      const parent = dirname(normalized);
      if (!directories.has(parent) || (childCounts.get(parent) ?? 0) >= MAX_VFS_DIRECTORY_ENTRIES) {
        continue;
      }
      if (!canAllocateVfs(1, 0, vfsPathKeyBytes(normalized))) break;
      addVfsDirectoryKey(normalized);
      childCounts.set(parent, (childCounts.get(parent) ?? 0) + 1);
    }
    vfsDirModes.set(normalized, mode);
    vfsDirModTimes.set(normalized, modTime);
  }

  let totalBytes = 0;
  const rawFiles = isJsonRecord(parsed.files) ? parsed.files : {};
  for (const [path, rawFile] of Object.entries(rawFiles)) {
    if (!isJsonRecord(rawFile) || typeof rawFile.contentBase64 !== 'string') continue;
    const [normalized, pathError] = checkedRuntimeVfsPath(path);
    if (
      pathError
      || !normalized
      || normalized !== path
      || normalized === RUNTIME_PERSIST_ROOT
      || !isRuntimePersistPath(normalized)
      || directories.has(normalized)
      || vfsFiles.has(normalized)
    ) {
      continue;
    }
    const mode = persistedMode(rawFile.mode, 0o644);
    const modTime = persistedModTime(rawFile.modTime);
    const parent = dirname(normalized);
    if (
      mode === null
      || modTime === null
      || !directories.has(parent)
      || (childCounts.get(parent) ?? 0) >= MAX_VFS_DIRECTORY_ENTRIES
    ) {
      continue;
    }
    try {
      const bytes = decodeBase64Bytes(rawFile.contentBase64);
      if (
        bytes.byteLength > RUNTIME_PERSIST_MAX_FILE_BYTES
        || totalBytes + bytes.byteLength > RUNTIME_PERSIST_MAX_TOTAL_BYTES
        || !canAllocateVfs(1, bytes.byteLength, vfsPathKeyBytes(normalized))
      ) {
        continue;
      }
      totalBytes += bytes.byteLength;
      setVfsFile(normalized, bytes, mode, false);
      vfsFileModTimes.set(normalized, modTime);
      childCounts.set(parent, (childCounts.get(parent) ?? 0) + 1);
    } catch (error) {
      console.warn(`Studio runtime VFS persistence skipped ${normalized}:`, error);
    }
  }
}

function persistRuntimeVfsSnapshot(): void {
  const storage = runtimePersistStorage();
  if (!storage) {
    return;
  }
  const snapshot: RuntimePersistSnapshot = {
    schemaVersion: 1,
    files: {},
    dirs: {},
  };
  for (const dir of [...directories].sort(compareUtf8)) {
    if (!isRuntimePersistPath(dir)) {
      continue;
    }
    snapshot.dirs![dir] = {
      mode: vfsDirModes.get(dir) ?? 0o755,
      modTime: vfsDirModTimes.get(dir) ?? 0,
    };
  }
  let totalBytes = 0;
  for (const [path, bytes] of [...vfsFiles.entries()].sort(([a], [b]) => compareUtf8(a, b))) {
    if (!isRuntimePersistPath(path)) {
      continue;
    }
    if (bytes.byteLength > RUNTIME_PERSIST_MAX_FILE_BYTES || totalBytes + bytes.byteLength > RUNTIME_PERSIST_MAX_TOTAL_BYTES) {
      continue;
    }
    totalBytes += bytes.byteLength;
    snapshot.files![path] = {
      contentBase64: encodeBase64Bytes(bytes),
      mode: vfsFileModes.get(path) ?? 0o644,
      modTime: vfsFileModTimes.get(path) ?? Date.now(),
    };
  }
  try {
    const serialized = JSON.stringify(snapshot);
    if (serialized.length > RUNTIME_PERSIST_MAX_SNAPSHOT_CHARS) {
      throw new Error('snapshot exceeds its encoded size limit');
    }
    storage.setItem(RUNTIME_PERSIST_STORAGE_KEY, serialized);
  } catch (error) {
    console.warn('Studio runtime VFS persistence could not be saved:', error);
  }
}

function installRuntimeVfsPersistenceLifecycle(): void {
  if (runtimePersistLifecycleInstalled || typeof window === 'undefined') {
    return;
  }
  runtimePersistLifecycleInstalled = true;
  const flush = () => persistRuntimeVfsSnapshot();
  window.addEventListener('pagehide', flush);
  window.addEventListener('beforeunload', flush);
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
  if (!Number.isSafeInteger(flags) || flags < 0 || (flags & ~KNOWN_VFS_OPEN_FLAGS) !== 0) {
    return [-1, invalidVfsArgument('unsupported OpenFile flags')];
  }
  const access = flags & VFS_ACCESS_MASK;
  if (access > O_RDWR) return [-1, invalidVfsArgument('invalid OpenFile access mode')];
  if ((flags & O_EXCL) !== 0 && (flags & O_CREATE) === 0) {
    return [-1, invalidVfsArgument('O_EXCL requires O_CREATE')];
  }
  if ((flags & O_TRUNC) !== 0 && access === O_RDONLY) {
    return [-1, invalidVfsArgument('O_TRUNC requires a writable access mode')];
  }
  if (!validVfsMode(mode)) return [-1, invalidVfsArgument('invalid file mode')];
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return [-1, pathError];
  if (access !== O_RDONLY && isWithinReadOnlyStaticTree(normalized)) {
    return [-1, ERR_PERMISSION];
  }
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return [-1, parentError];
  const create = (flags & O_CREATE) !== 0;
  const excl = (flags & O_EXCL) !== 0;
  const trunc = (flags & O_TRUNC) !== 0;
  if (directories.has(normalized)) {
    if (create && excl) return [-1, ERR_EXIST];
    if (access !== O_RDONLY || trunc) return [-1, ERR_IS_DIR];
    const directoryMode = vfsDirModes.get(normalized) ?? 0o755;
    if ((directoryMode & 0o444) === 0 || (directoryMode & 0o111) === 0) {
      return [-1, ERR_PERMISSION];
    }
    const fd = allocateVfsFd();
    if (fd === null) return [-1, 'too many open files'];
    if (!canAllocateVfs(0, 0, vfsPathKeyBytes(normalized))) return [-1, ERR_OUT_OF_MEMORY];
    if (!tryTrackOpenVfsFile(fd, { path: normalized, flags, position: 0 })) {
      return [-1, ERR_OUT_OF_MEMORY];
    }
    return [fd, null];
  }
  const exists = vfsFiles.has(normalized);
  if (!exists) {
    if (!create) return [-1, ERR_NOT_EXIST];
    if (hasVfsTrailingSlash(path)) return [-1, ERR_NOT_EXIST];
    const parent = dirname(normalized);
    if (vfsFiles.has(parent)) return [-1, ERR_NOT_DIR];
    if (!directories.has(parent)) return [-1, ERR_NOT_EXIST];
    if (!canMutateVfsDirectory(parent)) return [-1, ERR_PERMISSION];
  } else {
    if (create && excl) return [-1, ERR_EXIST];
    if (hasVfsTrailingSlash(path)) return [-1, ERR_NOT_DIR];
    const fileMode = vfsFileModes.get(normalized) ?? 0o644;
    if ((access === O_RDONLY || access === O_RDWR) && (fileMode & 0o444) === 0) {
      return [-1, ERR_PERMISSION];
    }
    if ((access === O_WRONLY || access === O_RDWR) && (fileMode & 0o222) === 0) {
      return [-1, ERR_PERMISSION];
    }
  }
  const fd = allocateVfsFd();
  if (fd === null) return [-1, 'too many open files'];
  if (!exists) {
    if (!canAllocateVfs(1, 0, vfsPathKeyBytes(normalized) * 2)) {
      return [-1, ERR_OUT_OF_MEMORY];
    }
    setVfsFile(normalized, new Uint8Array(0), normalizedVfsMode(mode));
  } else if (trunc) {
    if (!canAllocateVfs(0, 0, vfsPathKeyBytes(normalized))) return [-1, ERR_OUT_OF_MEMORY];
    setVfsFile(normalized, new Uint8Array(0), vfsFileModes.get(normalized) ?? mode);
  } else if (!canAllocateVfs(0, 0, vfsPathKeyBytes(normalized))) {
    return [-1, ERR_OUT_OF_MEMORY];
  }
  if (!tryTrackOpenVfsFile(fd, {
    path: normalized,
    flags,
    position: 0,
  })) {
    if (!exists) deleteFile(normalized);
    return [-1, ERR_OUT_OF_MEMORY];
  }
  return [fd, null];
}

function vfsRead(fd: number, length: number): [Uint8Array | null, string | null] {
  if (!safeVfsInteger(length, 0, MAX_VFS_IO_BYTES)) {
    return [null, invalidVfsArgument('read length is out of range')];
  }
  const file = openVfsFiles.get(fd);
  if (!file) return [null, ERR_BAD_FD];
  if ((file.flags & VFS_ACCESS_MASK) === O_WRONLY) return [null, ERR_PERMISSION];
  if (openVfsIsDirectory(file)) return [null, ERR_IS_DIR];
  const bytes = openVfsBytes(file);
  const start = file.position;
  const end = Math.min(start + length, bytes.length);
  const chunk = bytes.slice(start, end);
  file.position = end;
  return [chunk, null];
}

function writeBytes(file: OpenVfsFile, offset: number, data: Uint8Array): [number, string | null] {
  const existing = openVfsBytes(file);
  const nextLength = Math.max(existing.length, offset + data.length);
  if (!Number.isSafeInteger(nextLength) || nextLength > MAX_VFS_FILE_BYTES) {
    return [0, ERR_FILE_TOO_LARGE];
  }
  const growth = Math.max(0, nextLength - existing.length);
  if (growth > 0 && !canAllocateVfs(0, growth)) return [0, ERR_OUT_OF_MEMORY];
  try {
    const next = new Uint8Array(nextLength);
    next.set(existing);
    next.set(data, offset);
    if (file.detached) {
      orphanVfsBytes += next.length - file.detached.bytes.length;
      file.detached.bytes = next;
      file.detached.modTime = Date.now();
    } else {
      setVfsFile(file.path, next, vfsFileModes.get(file.path) ?? 0o644);
    }
    return [data.length, null];
  } catch {
    return [0, ERR_OUT_OF_MEMORY];
  }
}

function vfsWrite(fd: number, data: Uint8Array): [number, string | null] {
  if (!(data instanceof Uint8Array) || data.length > MAX_VFS_IO_BYTES) {
    return [0, invalidVfsArgument('write length is out of range')];
  }
  const file = openVfsFiles.get(fd);
  if (!file) return [0, ERR_BAD_FD];
  if (openVfsIsDirectory(file)) return [0, ERR_IS_DIR];
  const access = file.flags & VFS_ACCESS_MASK;
  if (access === O_RDONLY) return [0, ERR_PERMISSION];
  if (data.length === 0) return [0, null];
  const position = (file.flags & O_APPEND) !== 0
    ? openVfsBytes(file).length
    : file.position;
  const [written, error] = writeBytes(file, position, data);
  if (error) return [written, error];
  file.position = position + written;
  return [written, null];
}

function vfsReadAt(fd: number, length: number, offset: number): [Uint8Array | null, string | null] {
  if (!safeVfsInteger(length, 0, MAX_VFS_IO_BYTES)) {
    return [null, invalidVfsArgument('read length is out of range')];
  }
  if (!safeVfsInteger(offset, 0, MAX_VFS_FILE_BYTES)) {
    return [null, invalidVfsArgument('file offset is out of range')];
  }
  const file = openVfsFiles.get(fd);
  if (!file) return [null, ERR_BAD_FD];
  if ((file.flags & VFS_ACCESS_MASK) === O_WRONLY) return [null, ERR_PERMISSION];
  if (openVfsIsDirectory(file)) return [null, ERR_IS_DIR];
  const bytes = openVfsBytes(file);
  if (offset >= bytes.length) return [new Uint8Array(0), null];
  return [bytes.slice(offset, Math.min(offset + length, bytes.length)), null];
}

function vfsWriteAt(fd: number, data: Uint8Array, offset: number): [number, string | null] {
  if (!(data instanceof Uint8Array) || data.length > MAX_VFS_IO_BYTES) {
    return [0, invalidVfsArgument('write length is out of range')];
  }
  if (!safeVfsInteger(offset, 0, MAX_VFS_FILE_BYTES)) {
    return [0, invalidVfsArgument('file offset is out of range')];
  }
  const file = openVfsFiles.get(fd);
  if (!file) return [0, ERR_BAD_FD];
  if ((file.flags & O_APPEND) !== 0) {
    return [0, invalidVfsArgument('invalid use of WriteAt on file opened with O_APPEND')];
  }
  if (openVfsIsDirectory(file)) return [0, ERR_IS_DIR];
  const access = file.flags & VFS_ACCESS_MASK;
  if (access === O_RDONLY) return [0, ERR_PERMISSION];
  if (data.length === 0) return [0, null];
  return writeBytes(file, offset, data);
}

function vfsSeek(fd: number, offset: number, whence: number): [number, string | null] {
  if (!Number.isSafeInteger(offset) || !Number.isSafeInteger(whence)) {
    return [-1, invalidVfsArgument('seek arguments are out of range')];
  }
  const file = openVfsFiles.get(fd);
  if (!file) return [-1, ERR_BAD_FD];
  if (openVfsIsDirectory(file)) return [-1, ERR_IS_DIR];
  const bytes = openVfsBytes(file);
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
  if (!safeVfsInteger(nextPosition, 0, MAX_VFS_FILE_BYTES)) {
    return [-1, invalidVfsArgument('resulting file offset is out of range')];
  }
  file.position = nextPosition;
  return [nextPosition, null];
}

function vfsClose(fd: number): string | null {
  const file = deleteTrackedOpenVfsFile(fd);
  if (!file) return ERR_BAD_FD;
  if (file.detached) {
    file.detached.openCount -= 1;
    if (file.detached.openCount === 0) {
      orphanVfsNodes -= 1;
      orphanVfsBytes -= file.detached.bytes.length;
      orphanVfsPathBytes -= file.detached.pathBytes;
    }
  }
  return null;
}

function vfsSync(fd: number): string | null {
  // Studio's browser backend is an authoritative in-memory filesystem, so
  // every successful write is already visible to all descriptors. Durable
  // project storage is owned by the surrounding Studio persistence layer.
  return openVfsFiles.has(fd) ? null : ERR_BAD_FD;
}

function vfsFstat(fd: number): [number, number, number, boolean, string | null] {
  const file = openVfsFiles.get(fd);
  if (!file) return [0, 0, 0, false, ERR_BAD_FD];
  const isDir = openVfsIsDirectory(file);
  const size = isDir ? 0 : openVfsBytes(file).length;
  return [
    size,
    file.detached?.mode
      ?? (isDir ? (vfsDirModes.get(file.path) ?? 0o755) : (vfsFileModes.get(file.path) ?? 0o644)),
    file.detached?.modTime
      ?? (isDir ? (vfsDirModTimes.get(file.path) ?? 0) : (vfsFileModTimes.get(file.path) ?? 0)),
    isDir,
    null,
  ];
}

function vfsFtruncate(fd: number, size: number): string | null {
  if (!safeVfsInteger(size, 0, MAX_VFS_FILE_BYTES)) {
    return size > MAX_VFS_FILE_BYTES ? ERR_FILE_TOO_LARGE : ERR_INVALID;
  }
  const file = openVfsFiles.get(fd);
  if (!file) return ERR_BAD_FD;
  if (openVfsIsDirectory(file)) return ERR_IS_DIR;
  if ((file.flags & VFS_ACCESS_MASK) === O_RDONLY) return ERR_PERMISSION;
  const existing = openVfsBytes(file);
  const growth = size - existing.length;
  if (growth > 0 && !canAllocateVfs(0, growth)) return ERR_OUT_OF_MEMORY;
  try {
    const next = new Uint8Array(size);
    next.set(existing.subarray(0, Math.min(existing.length, size)));
    if (file.detached) {
      orphanVfsBytes += next.length - file.detached.bytes.length;
      file.detached.bytes = next;
      file.detached.modTime = Date.now();
    } else {
      setVfsFile(file.path, next, vfsFileModes.get(file.path) ?? 0o644);
    }
    return null;
  } catch {
    return ERR_OUT_OF_MEMORY;
  }
}

function vfsMkdir(path: string, mode: number): string | null {
  if (!validVfsMode(mode)) return invalidVfsArgument('invalid directory mode');
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return pathError;
  if (normalized === ROOT) return ERR_EXIST;
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return parentError;
  const parent = dirname(normalized);
  if (vfsFiles.has(parent)) return ERR_NOT_DIR;
  if (!directories.has(parent)) return ERR_NOT_EXIST;
  if (!canMutateVfsDirectory(parent)) return ERR_PERMISSION;
  if (directories.has(normalized) || vfsFiles.has(normalized)) return ERR_EXIST;
  if (!canAllocateVfs(1, 0, vfsPathKeyBytes(normalized))) return ERR_OUT_OF_MEMORY;
  addVfsDirectoryKey(normalized);
  vfsDirModes.set(normalized, normalizedVfsMode(mode));
  vfsDirModTimes.set(normalized, Date.now());
  if (isRuntimePersistPath(normalized)) {
    persistRuntimeVfsSnapshot();
  }
  return null;
}

function vfsMkdirAll(path: string, mode: number): string | null {
  if (!validVfsMode(mode)) return invalidVfsArgument('invalid directory mode');
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return pathError;
  const parts = normalized.split('/').filter(Boolean);
  let current = ROOT;
  for (const part of parts) {
    if (((vfsDirModes.get(current) ?? 0o755) & 0o111) === 0) return ERR_PERMISSION;
    current = current === ROOT ? `/${part}` : `${current}/${part}`;
    if (vfsFiles.has(current)) return ERR_NOT_DIR;
    if (!directories.has(current)) {
      if (!canMutateVfsDirectory(dirname(current))) return ERR_PERMISSION;
      if (!canAllocateVfs(1, 0, vfsPathKeyBytes(current))) return ERR_OUT_OF_MEMORY;
      addVfsDirectoryKey(current);
      vfsDirModes.set(current, normalizedVfsMode(mode));
      vfsDirModTimes.set(current, Date.now());
    }
  }
  if (isRuntimePersistPath(normalized)) {
    persistRuntimeVfsSnapshot();
  }
  return null;
}

function vfsRemove(path: string): string | null {
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return pathError;
  if (normalized === ROOT) return ERR_PERMISSION;
  if (overlapsReadOnlyStaticTree(normalized)) return ERR_PERMISSION;
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return parentError;
  if (!canMutateVfsDirectory(dirname(normalized))) return ERR_PERMISSION;
  if (vfsFiles.has(normalized)) {
    if (hasVfsTrailingSlash(path)) return ERR_NOT_DIR;
    deleteFile(normalized);
    return null;
  }
  if (!directories.has(normalized)) return ERR_NOT_EXIST;
  if (listVfsEntries(normalized).length > 0) return 'directory not empty';
  if (vfsPathContains(normalized, runtimeVfsRoot)) return ERR_PERMISSION;
  detachOpenVfsPath(normalized, true);
  deleteVfsDirectoryKey(normalized);
  vfsDirModes.delete(normalized);
  vfsDirModTimes.delete(normalized);
  if (isRuntimePersistPath(normalized)) {
    persistRuntimeVfsSnapshot();
  }
  return null;
}

function vfsRemoveAll(path: string): string | null {
  if (path === '') return null;
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return pathError;
  if (normalized === ROOT) return ERR_PERMISSION;
  if (overlapsReadOnlyStaticTree(normalized)) return ERR_PERMISSION;
  const parentError = vfsParentLookupError(normalized);
  if (parentError === ERR_NOT_EXIST && !vfsFiles.has(normalized) && !directories.has(normalized)) {
    return null;
  }
  if (parentError) return parentError;
  if (!vfsFiles.has(normalized) && !directories.has(normalized)) return null;
  if (hasVfsTrailingSlash(path) && vfsFiles.has(normalized)) return ERR_NOT_DIR;
  if (!canMutateVfsDirectory(dirname(normalized))) return ERR_PERMISSION;
  if (vfsFiles.has(normalized)) {
    deleteFile(normalized);
    return null;
  }
  if (vfsPathContains(normalized, runtimeVfsRoot)) return ERR_PERMISSION;
  for (const filePath of [...vfsFiles.keys()]) {
    if (filePath === normalized || filePath.startsWith(`${normalized}/`)) {
      deleteFile(filePath, false);
    }
  }
  for (const dir of [...directories].sort((a, b) => b.length - a.length)) {
    if (dir === normalized || dir.startsWith(`${normalized}/`)) {
      detachOpenVfsPath(dir, true);
      deleteVfsDirectoryKey(dir);
      vfsDirModes.delete(dir);
      vfsDirModTimes.delete(dir);
    }
  }
  if (isRuntimePersistPath(normalized)) {
    persistRuntimeVfsSnapshot();
  }
  return null;
}

function vfsRenameNoreplace(oldPath: string, newPath: string): string | null {
  const [newNorm, pathError] = checkedRuntimeVfsPath(newPath);
  if (pathError || !newNorm) return pathError;
  if (directories.has(newNorm) || vfsFiles.has(newNorm)) return ERR_EXIST;
  return vfsRename(oldPath, newPath);
}

function vfsRename(oldPath: string, newPath: string): string | null {
  const [oldNorm, oldPathError] = checkedRuntimeVfsPath(oldPath);
  if (oldPathError || !oldNorm) return oldPathError;
  const [newNorm, newPathError] = checkedRuntimeVfsPath(newPath);
  if (newPathError || !newNorm) return newPathError;
  if (oldNorm === ROOT || newNorm === ROOT) return ERR_PERMISSION;
  if (overlapsReadOnlyStaticTree(oldNorm) || overlapsReadOnlyStaticTree(newNorm)) {
    return ERR_PERMISSION;
  }
  const oldParentError = vfsParentLookupError(oldNorm);
  if (oldParentError) return oldParentError;
  const newParentError = vfsParentLookupError(newNorm);
  if (newParentError) return newParentError;
  const oldParent = dirname(oldNorm);
  const newParent = dirname(newNorm);
  if (!canMutateVfsDirectory(oldParent) || !canMutateVfsDirectory(newParent)) {
    return ERR_PERMISSION;
  }
  const sourceIsFile = vfsFiles.has(oldNorm);
  const sourceIsDir = directories.has(oldNorm);
  if (!sourceIsFile && !sourceIsDir) return ERR_NOT_EXIST;
  if (oldPath.length > 1 && oldPath.endsWith('/') && sourceIsFile) return ERR_NOT_DIR;
  if (sourceIsDir && newNorm.startsWith(`${oldNorm}/`)) {
    return invalidVfsArgument('cannot move a directory into itself');
  }

  const targetIsFile = vfsFiles.has(newNorm);
  const targetIsDir = directories.has(newNorm);
  if (newPath.length > 1 && newPath.endsWith('/')) {
    if (!targetIsDir) return targetIsFile ? ERR_NOT_DIR : ERR_NOT_EXIST;
  }
  if (oldNorm === newNorm) return null;
  if (sourceIsDir && targetIsFile) return ERR_NOT_DIR;
  if (sourceIsFile && targetIsDir) return ERR_IS_DIR;
  if (targetIsDir && listVfsEntries(newNorm).length > 0) return 'directory not empty';
  if (targetIsDir && vfsPathContains(newNorm, runtimeVfsRoot)) return ERR_PERMISSION;

  const dirMoves = sourceIsDir
    ? [...directories]
      .filter((dir) => vfsPathContains(oldNorm, dir))
      .sort((left, right) => left.length - right.length)
      .map((dir) => [
        dir,
        `${newNorm}${dir.slice(oldNorm.length)}`,
        vfsDirModes.get(dir) ?? 0o755,
        vfsDirModTimes.get(dir) ?? Date.now(),
      ] as const)
    : [];
  const fileMoves = sourceIsFile
    ? [[
      oldNorm,
      newNorm,
      vfsFiles.get(oldNorm)!,
      vfsFileModes.get(oldNorm) ?? 0o644,
      vfsFileModTimes.get(oldNorm) ?? Date.now(),
    ] as const]
    : [...vfsFiles]
      .filter(([path]) => path.startsWith(`${oldNorm}/`))
      .map(([path, bytes]) => [
        path,
        `${newNorm}${path.slice(oldNorm.length)}`,
        bytes,
        vfsFileModes.get(path) ?? 0o644,
        vfsFileModTimes.get(path) ?? Date.now(),
      ] as const);

  for (const targetPath of [
    ...dirMoves.map(([, to]) => to),
    ...fileMoves.map(([, to]) => to),
  ]) {
    const [checkedTarget, targetPathError] = checkedRuntimeVfsPath(targetPath);
    if (targetPathError || checkedTarget !== targetPath) return targetPathError ?? ERR_INVALID;
  }

  const finalDirectories = new Set(directories);
  const finalFiles = new Set(vfsFiles.keys());
  if (targetIsDir) finalDirectories.delete(newNorm);
  if (targetIsFile) finalFiles.delete(newNorm);
  for (const [from, to] of dirMoves) {
    finalDirectories.delete(from);
    finalDirectories.add(to);
  }
  for (const [from, to] of fileMoves) {
    finalFiles.delete(from);
    finalFiles.add(to);
  }
  const targetWillDetach = (targetIsDir || targetIsFile)
    && [...openVfsFiles.values()].some((file) => !file.detached && file.path === newNorm);
  const finalOrphanNodes = orphanVfsNodes + (targetWillDetach ? 1 : 0);
  const finalOrphanPathBytes = orphanVfsPathBytes
    + (targetWillDetach ? vfsPathKeyBytes(newNorm) : 0);
  const finalOpenPathBytes = projectedOpenVfsPathBytesAfterRename(oldNorm, newNorm);
  if (
    finalDirectories.size + finalFiles.size + finalOrphanNodes > MAX_VFS_NODES
    || !Number.isSafeInteger(finalOpenPathBytes)
    || aggregateVfsPathKeyBytes(finalDirectories, finalFiles)
      + finalOrphanPathBytes
      + finalOpenPathBytes
      > MAX_VFS_PATH_KEY_BYTES
  ) {
    return ERR_OUT_OF_MEMORY;
  }

  if (targetIsFile) deleteFile(newNorm, false);
  if (targetIsDir) {
    detachOpenVfsPath(newNorm, true);
    deleteVfsDirectoryKey(newNorm);
    vfsDirModes.delete(newNorm);
    vfsDirModTimes.delete(newNorm);
  }
  for (const [from] of fileMoves) {
    deleteCachedTextFile(from);
    deleteVfsFileKey(from);
    vfsFileModes.delete(from);
    vfsFileModTimes.delete(from);
  }
  for (const [from] of [...dirMoves].reverse()) {
    deleteVfsDirectoryKey(from);
    vfsDirModes.delete(from);
    vfsDirModTimes.delete(from);
  }
  for (const [, to, mode, modTime] of dirMoves) {
    addVfsDirectoryKey(to);
    vfsDirModes.set(to, mode);
    vfsDirModTimes.set(to, modTime);
  }
  for (const [, to, bytes, mode, modTime] of fileMoves) {
    setVfsFileKey(to, bytes);
    vfsFileModes.set(to, mode);
    vfsFileModTimes.set(to, modTime);
    deleteCachedTextFile(to);
  }
  for (const file of openVfsFiles.values()) {
    if (!file.detached && (file.path === oldNorm || file.path.startsWith(`${oldNorm}/`))) {
      file.path = `${newNorm}${file.path.slice(oldNorm.length)}`;
    }
  }
  openVfsPathBytes = finalOpenPathBytes;
  if (runtimeVfsRoot === oldNorm || runtimeVfsRoot.startsWith(`${oldNorm}/`)) {
    runtimeVfsRoot = `${newNorm}${runtimeVfsRoot.slice(oldNorm.length)}`;
  }
  if (isRuntimePersistPath(oldNorm) || isRuntimePersistPath(newNorm)) {
    persistRuntimeVfsSnapshot();
  }
  return null;
}

function vfsStat(path: string): [string, number, number, number, boolean, string | null] {
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return ['', 0, 0, 0, false, pathError];
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return ['', 0, 0, 0, false, parentError];
  const name = normalized === ROOT ? ROOT : normalized.slice(normalized.lastIndexOf('/') + 1);
  if (directories.has(normalized)) {
    return [name, 0, vfsDirModes.get(normalized) ?? 0o755, vfsDirModTimes.get(normalized) ?? 0, true, null];
  }
  const bytes = vfsFiles.get(normalized);
  if (!bytes) return ['', 0, 0, 0, false, ERR_NOT_EXIST];
  if (hasVfsTrailingSlash(path)) return ['', 0, 0, 0, false, ERR_NOT_DIR];
  return [name, bytes.length, vfsFileModes.get(normalized) ?? 0o644, vfsFileModTimes.get(normalized) ?? 0, false, null];
}

function vfsReadDir(path: string): [Array<[string, boolean, number]>, string | null] {
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return [[], pathError];
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return [[], parentError];
  if (vfsFiles.has(normalized)) return [[], ERR_NOT_DIR];
  if (!directories.has(normalized)) return [[], ERR_NOT_EXIST];
  const entries = listVfsEntries(normalized);
  const mode = vfsDirModes.get(normalized) ?? 0o755;
  if ((mode & 0o444) === 0 || (mode & 0o111) === 0) return [[], ERR_PERMISSION];
  if (entries.length > MAX_VFS_DIRECTORY_ENTRIES) {
    return [[], 'directory contains too many entries'];
  }
  entries.sort((left, right) => compareUtf8(left[0], right[0]));
  return [entries, null];
}

function vfsChmod(path: string, mode: number): string | null {
  if (!validVfsMode(mode)) return invalidVfsArgument('invalid file mode');
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return pathError;
  if (isWithinReadOnlyStaticTree(normalized)) return ERR_PERMISSION;
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return parentError;
  if (directories.has(normalized)) {
    vfsDirModes.set(normalized, normalizedVfsMode(mode));
    if (isRuntimePersistPath(normalized)) {
      persistRuntimeVfsSnapshot();
    }
    return null;
  }
  if (!vfsFiles.has(normalized)) return ERR_NOT_EXIST;
  if (hasVfsTrailingSlash(path)) return ERR_NOT_DIR;
  vfsFileModes.set(normalized, normalizedVfsMode(mode));
  if (isRuntimePersistPath(normalized)) {
    persistRuntimeVfsSnapshot();
  }
  return null;
}

function vfsTruncate(path: string, size: number): string | null {
  if (!safeVfsInteger(size, 0, MAX_VFS_FILE_BYTES)) {
    return size > MAX_VFS_FILE_BYTES ? ERR_FILE_TOO_LARGE : ERR_INVALID;
  }
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return pathError;
  if (isWithinReadOnlyStaticTree(normalized)) return ERR_PERMISSION;
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return parentError;
  if (directories.has(normalized)) return ERR_IS_DIR;
  const existing = vfsFiles.get(normalized);
  if (!existing) return ERR_NOT_EXIST;
  if (hasVfsTrailingSlash(path)) return ERR_NOT_DIR;
  if (((vfsFileModes.get(normalized) ?? 0o644) & 0o222) === 0) return ERR_PERMISSION;
  const growth = size - existing.length;
  if (growth > 0 && !canAllocateVfs(0, growth)) return ERR_OUT_OF_MEMORY;
  let next: Uint8Array;
  try {
    if (size < existing.length) {
      next = existing.slice(0, size);
    } else if (size > existing.length) {
      next = new Uint8Array(size);
      next.set(existing);
    } else {
      next = existing;
    }
  } catch {
    return ERR_OUT_OF_MEMORY;
  }
  setVfsFile(normalized, next, vfsFileModes.get(normalized) ?? 0o644);
  return null;
}

function vfsReadFile(path: string): [Uint8Array | null, string | null] {
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return [null, pathError];
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return [null, parentError];
  if (directories.has(normalized)) return [null, ERR_IS_DIR];
  const bytes = vfsFiles.get(normalized);
  if (!bytes) return [null, ERR_NOT_EXIST];
  if (hasVfsTrailingSlash(path)) return [null, ERR_NOT_DIR];
  if ((vfsFileModes.get(normalized) ?? 0o644) & 0o444) {
    if (bytes.length > MAX_VFS_FILE_BYTES) return [null, ERR_FILE_TOO_LARGE];
  } else {
    return [null, ERR_PERMISSION];
  }
  return [new Uint8Array(bytes), null];
}

function vfsGetwd(): [string, string | null] {
  return [runtimeVfsRoot, null];
}

function vfsChdir(path: string): string | null {
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return pathError;
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return parentError;
  if (vfsFiles.has(normalized)) return ERR_NOT_DIR;
  if (!directories.has(normalized)) return ERR_NOT_EXIST;
  if (((vfsDirModes.get(normalized) ?? 0o755) & 0o111) === 0) return ERR_PERMISSION;
  runtimeVfsRoot = normalized;
  return null;
}

function vfsReadFileLimited(path: string, maxBytes: number): [Uint8Array | null, string | null] {
  if (!Number.isSafeInteger(maxBytes) || maxBytes < 0 || maxBytes > MAX_VFS_FILE_BYTES) {
    return [null, invalidVfsArgument('read limit is out of range')];
  }
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return [null, pathError];
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return [null, parentError];
  if (directories.has(normalized)) return [null, ERR_IS_DIR];
  const bytes = vfsFiles.get(normalized);
  if (!bytes) return [null, ERR_NOT_EXIST];
  if (hasVfsTrailingSlash(path)) return [null, ERR_NOT_DIR];
  if (((vfsFileModes.get(normalized) ?? 0o644) & 0o444) === 0) return [null, ERR_PERMISSION];
  if (bytes.length > maxBytes) return [null, 'file too large'];
  return [new Uint8Array(bytes), null];
}

function vfsWriteFile(path: string, data: Uint8Array, mode: number): string | null {
  if (!(data instanceof Uint8Array)) return invalidVfsArgument('data must be bytes');
  if (data.length > MAX_VFS_FILE_BYTES) return ERR_FILE_TOO_LARGE;
  if (!validVfsMode(mode)) return invalidVfsArgument('invalid file mode');
  const [normalized, pathError] = checkedRuntimeVfsPath(path);
  if (pathError || !normalized) return pathError;
  if (isWithinReadOnlyStaticTree(normalized)) return ERR_PERMISSION;
  const parentError = vfsParentLookupError(normalized);
  if (parentError) return parentError;
  if (normalized === ROOT) return ERR_IS_DIR;
  if (hasVfsTrailingSlash(path)) return ERR_NOT_DIR;
  if (directories.has(normalized)) return ERR_IS_DIR;
  const parent = dirname(normalized);
  if (vfsFiles.has(parent)) return ERR_NOT_DIR;
  if (!directories.has(parent)) return ERR_NOT_EXIST;
  const existingMode = vfsFileModes.get(normalized);
  if (existingMode !== undefined && (existingMode & 0o222) === 0) return ERR_PERMISSION;
  if (existingMode === undefined && !canMutateVfsDirectory(parent)) return ERR_PERMISSION;
  const previousSize = vfsFiles.get(normalized)?.length ?? 0;
  const growth = data.length - previousSize;
  if (!canAllocateVfs(
    existingMode === undefined ? 1 : 0,
    Math.max(0, growth),
    existingMode === undefined ? vfsPathKeyBytes(normalized) : 0,
  )) {
    return ERR_OUT_OF_MEMORY;
  }
  setVfsFile(normalized, data, existingMode ?? normalizedVfsMode(mode));
  return null;
}

function resetWorkspaceState(): void {
  clearTextFileCache();
  vfsFiles.clear();
  directories.clear();
  vfsFileModes.clear();
  vfsFileModTimes.clear();
  vfsDirModes.clear();
  vfsDirModTimes.clear();
  readOnlyStaticRoots.clear();
  openVfsFiles.clear();
  openVfsPathBytes = 0;
  nextVfsFd = FIRST_VFS_FD;
  runtimeVfsRoot = ROOT;
  runtimeVfsFloor = null;
  liveVfsBytes = 0;
  liveVfsPathBytes = 0;
  orphanVfsBytes = 0;
  orphanVfsPathBytes = 0;
  orphanVfsNodes = 0;
  addVfsDirectoryKey(ROOT);
  ensureDir(WORKSPACE_ROOT);
  ensureDir(URL_SESSION_ROOT);
  ensureDir('/tmp/cache');
  vfsDirModes.set('/tmp', 0o777);
  ensureDir('/home/config');
  for (const [path, content] of defaultWorkspaceFiles) {
    setFile(path, content);
  }
  loadRuntimeVfsSnapshot();
}

function missingVfsDirectories(path: string): string[] {
  const normalized = normalizePath(path);
  const parts = normalized.split('/').filter(Boolean);
  let current = ROOT;
  const missing: string[] = [];
  if (!directories.has(ROOT)) missing.push(ROOT);
  for (const part of parts) {
    current = current === ROOT ? `/${part}` : `${current}/${part}`;
    if (vfsFiles.has(current)) throw new Error(ERR_NOT_DIR);
    if (!directories.has(current)) missing.push(current);
  }
  return missing;
}

function vfsPathsByteLength(paths: readonly string[]): number {
  let total = 0;
  for (const path of paths) total += vfsPathKeyBytes(path);
  return total;
}

function ensureDir(path: string): void {
  const missing = missingVfsDirectories(path);
  if (!canAllocateVfs(missing.length, 0, vfsPathsByteLength(missing))) {
    throw new Error(ERR_OUT_OF_MEMORY);
  }
  addVfsDirectoryKey(ROOT);
  if (!vfsDirModes.has(ROOT)) {
    vfsDirModes.set(ROOT, 0o755);
    vfsDirModTimes.set(ROOT, Date.now());
  }
  for (const directory of missing) {
    if (directory === ROOT) continue;
    addVfsDirectoryKey(directory);
    vfsDirModes.set(directory, 0o755);
    vfsDirModTimes.set(directory, Date.now());
  }
}

function setFile(path: string, content: string): void {
  const normalized = normalizePath(path);
  const parent = dirname(normalized);
  const bytes = textEncoder.encode(content);
  const previous = vfsFiles.get(normalized);
  const growth = bytes.length - (previous?.length ?? 0);
  const missingDirectories = missingVfsDirectories(parent);
  const addedNodes = missingDirectories.length + (previous === undefined ? 1 : 0);
  const addedPathBytes = vfsPathsByteLength(missingDirectories)
    + (previous === undefined ? vfsPathKeyBytes(normalized) : 0);
  if (!canAllocateVfs(addedNodes, Math.max(0, growth), addedPathBytes)) {
    throw new Error(ERR_OUT_OF_MEMORY);
  }
  ensureDir(parent);
  cacheTextFile(normalized, content);
  liveVfsBytes += growth;
  setVfsFileKey(normalized, bytes);
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

function registerSessionWorkspaceDiscovery(root: string, workspaceDiscovery: WorkspaceDiscoveryMode): void {
  sessionWorkspaceDiscovery.set(normalizePath(root), workspaceDiscovery);
}

function workspaceDiscoveryForPath(path: string): WorkspaceDiscoveryMode {
  const normalized = normalizePath(path);
  let bestRoot = '';
  let bestMode: WorkspaceDiscoveryMode = 'auto';
  for (const [root, mode] of sessionWorkspaceDiscovery) {
    if (normalized === root || normalized.startsWith(`${root}/`)) {
      if (root.length > bestRoot.length) {
        bestRoot = root;
        bestMode = mode;
      }
    }
  }
  return bestMode;
}

function buildSessionInfo(
  root: string,
  origin: SessionInfo['origin'],
  source: SessionInfo['source'],
  workspaceDiscovery: WorkspaceDiscoveryMode,
): SessionInfo {
  const normalizedRoot = normalizePath(root);
  const entryPath = detectEntryPath(normalizedRoot);
  const share = buildShareInfo({ root: normalizedRoot, entryPath, source });
  registerSessionWorkspaceDiscovery(normalizedRoot, workspaceDiscovery);
  return {
    root: normalizedRoot,
    origin,
    projectMode: hasVfsFile(`${normalizedRoot}/vo.mod`) ? 'module' : 'single-file',
    entryPath,
    singleFileRun: false,
    workspaceDiscovery,
    source,
    share,
  };
}

function detectEntryPath(root: string): string | null {
  const main = `${root}/main.vo`;
  if (hasVfsFile(main)) return main;
  const first = [...vfsFiles.keys()]
    .filter((path) => path.startsWith(`${root}/`) && path.endsWith('.vo'))
    .sort(compareUtf8)[0];
  return first ?? null;
}

function openWorkspaceSession(): SessionInfo {
  if (!directories.has(WORKSPACE_ROOT)) {
    resetWorkspaceState();
  }
  return buildSessionInfo(WORKSPACE_ROOT, 'workspace', { kind: 'workspace' }, 'auto');
}

function openPathSession(path: string): SessionInfo {
  const normalized = normalizePath(path);
  if (normalized.endsWith('.vo') && hasVfsFile(normalized)) {
    const parent = normalized.substring(0, normalized.lastIndexOf('/')) || WORKSPACE_ROOT;
    const source: SessionInfo['source'] = { kind: 'path', path: normalized };
    registerSessionWorkspaceDiscovery(parent, 'auto');
    return {
      root: parent,
      origin: 'workspace',
      projectMode: 'single-file',
      entryPath: normalized,
      singleFileRun: false,
      workspaceDiscovery: 'auto',
      source,
      share: buildShareInfo({ root: parent, entryPath: normalized, source }),
    };
  }
  return buildSessionInfo(normalized, 'workspace', { kind: 'path', path: normalized }, 'auto');
}

async function openLocalProjectSession(path: string): Promise<SessionInfo> {
  const snapshot = await fetchLocalProjectSnapshot(path);
  const sessionRoot = `${LOCAL_SESSION_ROOT}/current`;
  replacePreparedStaticTreesAtomically([{ root: sessionRoot, files: snapshot.files }]);
  await yieldToBrowser();
  const projectRoot = normalizePath(`${sessionRoot}/${snapshot.projectRelativePath}`);
  if (!hasTreeAt(projectRoot)) {
    throw new Error(`Local project snapshot did not contain ${snapshot.projectRelativePath}`);
  }
  return buildSessionInfo(projectRoot, 'run-target', { kind: 'path', path: snapshot.projectPath }, 'auto');
}

async function fetchLocalProjectSnapshot(path: string): Promise<PreparedLocalProjectSnapshot> {
  const url = new URL(LOCAL_PROJECT_ENDPOINT, window.location.origin);
  url.searchParams.set('path', path);
  const response = await fetch(url.toString(), { cache: 'no-store' });
  if (!response.ok) {
    const detail = await response.text().catch(() => '');
    throw new Error(detail || `Local project bridge failed with HTTP ${response.status}`);
  }
  const bytes = await responseBytesLimited(response, MAX_LOCAL_PROJECT_SNAPSHOT_BYTES, url.toString());
  await yieldToBrowser();
  const value = parseJsonBytes(bytes, 'Local project bridge snapshot');
  return prepareLocalProjectSnapshot(value);
}

function prepareLocalProjectSnapshot(value: unknown): PreparedLocalProjectSnapshot {
  if (!isJsonRecord(value)) {
    throw new Error('Local project snapshot must be an object');
  }
  const { projectPath, projectRelativePath } = value;
  if (
    typeof projectPath !== 'string'
    || projectPath.length === 0
    || projectPath.length > MAX_VFS_PATH_LENGTH
    || /[\u0000-\u001f\u007f]/u.test(projectPath)
  ) {
    throw new Error('Local project snapshot has an invalid source path');
  }
  if (typeof projectRelativePath !== 'string') {
    throw new Error('Local project snapshot has an invalid project-relative path');
  }
  const normalizedProjectPath = normalizeStaticPackagePath(projectRelativePath);
  const preparedFiles = prepareStaticPackageFiles(value.files);
  if (!preparedFiles.some((file) => (
    file.relative === normalizedProjectPath
    || file.relative.startsWith(`${normalizedProjectPath}/`)
  ))) {
    throw new Error('Local project snapshot does not contain its declared project root');
  }
  return {
    projectPath,
    projectRelativePath: normalizedProjectPath,
    files: preparedFiles,
  };
}

function findProjectRootForEntry(entryPath: string): string | null {
  let dir = dirname(entryPath);
  while (dir && dir !== ROOT) {
    if (hasVfsFile(`${dir}/vo.mod`)) {
      return dir;
    }
    const next = dirname(dir);
    if (next === dir) break;
    dir = next;
  }
  return null;
}

function prepareStaticPackageFiles(value: unknown): PreparedStaticFile[] {
  if (!Array.isArray(value) || value.length > MAX_STATIC_PACKAGE_FILES) {
    throw new Error('Static package contains too many files');
  }
  const prepared: PreparedStaticFile[] = [];
  const seen = newPortablePathRegistry();
  let totalBytes = 0;
  for (const valueFile of value) {
    if (!isJsonRecord(valueFile) || typeof valueFile.path !== 'string') {
      throw new Error('Static package contains an invalid file entry');
    }
    const file = valueFile as unknown as StaticPackageFile;
    const relative = normalizeStaticPackagePath(file.path);
    insertPortablePath(seen, relative, false, 'static package path');
    const hasText = typeof file.content === 'string';
    const hasBase64 = typeof file.contentBase64 === 'string';
    if (hasText === hasBase64 || (file.content != null && !hasText) || (file.contentBase64 != null && !hasBase64)) {
      throw new Error(`Static package file must contain exactly one payload: ${file.path}`);
    }
    const bytes = hasBase64
      ? decodeCanonicalStaticBase64(file.contentBase64!, file.path)
      : textEncoder.encode(file.content!);
    if (
      (file.size !== undefined && (!Number.isSafeInteger(file.size) || file.size !== bytes.byteLength))
      || (file.digest !== undefined && !/^sha256:[0-9a-f]{64}$/.test(file.digest))
    ) {
      throw new Error(`Static package file integrity metadata is invalid: ${file.path}`);
    }
    if (bytes.byteLength > MAX_STATIC_PACKAGE_FILE_BYTES) {
      throw new Error(`Static package file exceeds the 256 MiB limit: ${file.path}`);
    }
    totalBytes += bytes.byteLength;
    if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_STATIC_SOURCE_PAYLOAD_BYTES) {
      throw new Error('Static package exceeds the 64 MiB payload limit');
    }
    prepared.push({
      relative,
      bytes,
      mode: normalizeStaticPackageMode(file.mode),
    });
  }
  return prepared;
}

function decodeCanonicalStaticBase64(value: string, path: string): Uint8Array {
  if (/\s/.test(value)) {
    throw new Error(`Static package file has non-canonical base64: ${path}`);
  }
  let bytes: Uint8Array;
  try {
    bytes = decodeBase64Bytes(value);
  } catch {
    throw new Error(`Static package file has invalid base64: ${path}`);
  }
  if (encodeBase64Bytes(bytes) !== value) {
    throw new Error(`Static package file has non-canonical base64: ${path}`);
  }
  return bytes;
}

function replacePreparedStaticTreesAtomically(
  trees: PreparedStaticTree[],
  commit: () => void = () => undefined,
): void {
  const normalizedTrees = trees.map((tree) => ({
    ...tree,
    root: normalizePath(tree.root),
  }));
  preflightPreparedStaticTrees(normalizedTrees);

  const snapshot = {
    directories: new Set(directories),
    files: new Map(files),
    vfsFiles: new Map(vfsFiles),
    vfsFileModes: new Map(vfsFileModes),
    vfsFileModTimes: new Map(vfsFileModTimes),
    vfsDirModes: new Map(vfsDirModes),
    vfsDirModTimes: new Map(vfsDirModTimes),
    readOnlyStaticRoots: new Set(readOnlyStaticRoots),
    liveVfsBytes,
    liveVfsPathBytes,
    textCacheBytes,
  };
  try {
    for (const root of [...readOnlyStaticRoots]) {
      if (normalizedTrees.some((tree) => vfsPathContains(tree.root, root))) {
        readOnlyStaticRoots.delete(root);
      }
    }
    for (const tree of normalizedTrees) {
      clearStaticPackageTree(tree.root);
    }
    for (const tree of normalizedTrees) {
      ensureDir(tree.root);
      for (const file of tree.files) {
        setPreparedVfsFile(`${tree.root}/${file.relative}`, file.bytes, file.mode);
      }
      if (tree.readOnly) {
        markStaticTreeReadOnly(tree.root);
      }
    }
    commit();
  } catch (error) {
    restoreMap(directories, snapshot.directories);
    restoreMap(files, snapshot.files);
    restoreMap(vfsFiles, snapshot.vfsFiles);
    restoreMap(vfsFileModes, snapshot.vfsFileModes);
    restoreMap(vfsFileModTimes, snapshot.vfsFileModTimes);
    restoreMap(vfsDirModes, snapshot.vfsDirModes);
    restoreMap(vfsDirModTimes, snapshot.vfsDirModTimes);
    restoreMap(readOnlyStaticRoots, snapshot.readOnlyStaticRoots);
    liveVfsBytes = snapshot.liveVfsBytes;
    liveVfsPathBytes = snapshot.liveVfsPathBytes;
    textCacheBytes = snapshot.textCacheBytes;
    throw error;
  }
}

function restoreMap<T>(target: Set<T>, snapshot: Set<T>): void;
function restoreMap<K, V>(target: Map<K, V>, snapshot: Map<K, V>): void;
function restoreMap<K, V>(target: Set<K> | Map<K, V>, snapshot: Set<K> | Map<K, V>): void {
  target.clear();
  if (target instanceof Set && snapshot instanceof Set) {
    for (const value of snapshot) target.add(value);
    return;
  }
  if (target instanceof Map && snapshot instanceof Map) {
    for (const [key, value] of snapshot) target.set(key, value);
  }
}

function preflightPreparedStaticTrees(trees: PreparedStaticTree[]): void {
  const sortedRoots = trees.map((tree) => tree.root).sort(compareUtf8);
  for (let index = 0; index < sortedRoots.length; index += 1) {
    const [checkedRoot, rootError] = checkedRuntimeVfsPath(sortedRoots[index]);
    if (
      rootError
      || checkedRoot !== sortedRoots[index]
      || sortedRoots[index] === ROOT
      || (index > 0 && sortedRoots[index] === sortedRoots[index - 1])
    ) {
      throw new Error(`Invalid static package destination root: ${sortedRoots[index]}`);
    }
  }
  const roots = new Set(sortedRoots);
  for (const root of sortedRoots) {
    let parent = dirname(root);
    while (parent !== ROOT) {
      if (roots.has(parent)) {
        throw new Error(`Static package destination roots overlap: ${parent} and ${root}`);
      }
      parent = dirname(parent);
    }
  }
  const plannedFiles = new Map<string, PreparedStaticFile>();
  const plannedDirectories = new Set<string>([ROOT]);
  let plannedPathKeyBytes = vfsPathKeyBytes(ROOT);
  let plannedBytes = 0;
  const addPlannedDirectory = (directory: string): void => {
    if (plannedDirectories.has(directory)) return;
    plannedPathKeyBytes += vfsPathKeyBytes(directory);
    if (
      !Number.isSafeInteger(plannedPathKeyBytes)
      || plannedPathKeyBytes > MAX_VFS_PATH_KEY_BYTES
    ) {
      throw new Error('Static package exceeds the 16 MiB planned VFS path-key limit');
    }
    plannedDirectories.add(directory);
  };

  for (const tree of trees) {
    let plannedDirectory = tree.root;
    while (true) {
      addPlannedDirectory(plannedDirectory);
      if (plannedDirectory === ROOT) break;
      plannedDirectory = dirname(plannedDirectory);
    }
    for (const openFile of openVfsFiles.values()) {
      if (!openFile.detached && vfsPathContains(tree.root, openFile.path)) {
        throw new Error(`Static package destination is busy: ${tree.root}`);
      }
    }
    for (const file of tree.files) {
      if (!(file.bytes instanceof Uint8Array) || file.bytes.byteLength > MAX_STATIC_PACKAGE_FILE_BYTES) {
        throw new Error(`Invalid static package file payload: ${file.relative}`);
      }
      const path = normalizePath(`${tree.root}/${file.relative}`);
      const [checkedPath, pathError] = checkedRuntimeVfsPath(path);
      if (pathError || checkedPath !== path) {
        throw new Error(`Invalid final static package path: ${path}`);
      }
      if (plannedFiles.has(path)) {
        throw new Error(`Duplicate static package destination: ${path}`);
      }
      plannedPathKeyBytes += vfsPathKeyBytes(path);
      if (
        !Number.isSafeInteger(plannedPathKeyBytes)
        || plannedPathKeyBytes > MAX_VFS_PATH_KEY_BYTES
      ) {
        throw new Error('Static package exceeds the 16 MiB planned VFS path-key limit');
      }
      plannedFiles.set(path, file);
      plannedBytes += file.bytes.byteLength;
      if (!Number.isSafeInteger(plannedBytes) || plannedBytes > MAX_STATIC_PACKAGE_TOTAL_BYTES) {
        throw new Error('Static package exceeds the 512 MiB aggregate payload limit');
      }
      let parent = dirname(path);
      while (true) {
        addPlannedDirectory(parent);
        if (parent === ROOT) break;
        parent = dirname(parent);
      }
    }
  }
  if (plannedFiles.size > MAX_STATIC_PACKAGE_FILES) {
    throw new Error(`Static package exceeds the ${MAX_STATIC_PACKAGE_FILES}-file aggregate limit`);
  }
  for (const path of plannedFiles.keys()) {
    if (plannedDirectories.has(path)) {
      throw new Error(`Static package path is both a file and a directory: ${path}`);
    }
  }

  const replaced = (path: string): boolean => {
    let candidate = path;
    while (candidate !== ROOT) {
      if (roots.has(candidate)) return true;
      candidate = dirname(candidate);
    }
    return false;
  };
  const retainedDirectories = new Set([...directories].filter((path) => !replaced(path)));
  const retainedFilePaths = new Set([...vfsFiles.keys()].filter((path) => !replaced(path)));
  let retainedBytes = 0;
  for (const [path, bytes] of vfsFiles) {
    if (!replaced(path)) retainedBytes += bytes.byteLength;
  }
  for (const directory of plannedDirectories) {
    if (retainedFilePaths.has(directory)) {
      throw new Error(`Static package directory conflicts with an existing file: ${directory}`);
    }
    retainedDirectories.add(directory);
  }
  for (const path of plannedFiles.keys()) {
    if (retainedDirectories.has(path) || retainedFilePaths.has(path)) {
      throw new Error(`Static package file conflicts with existing VFS state: ${path}`);
    }
  }
  const finalNodes = retainedDirectories.size + retainedFilePaths.size + plannedFiles.size + orphanVfsNodes;
  const finalBytes = retainedBytes + plannedBytes + orphanVfsBytes;
  const finalPathKeyBytes = aggregateVfsPathKeyBytes(
    retainedDirectories,
    [...retainedFilePaths, ...plannedFiles.keys()],
  ) + orphanVfsPathBytes + openVfsPathBytes;
  if (finalNodes > MAX_VFS_NODES) {
    throw new Error(`Static package would exceed the ${MAX_VFS_NODES}-node VFS limit`);
  }
  if (!Number.isSafeInteger(finalBytes) || finalBytes > MAX_VFS_TOTAL_BYTES) {
    throw new Error('Static package would exceed the 512 MiB VFS limit');
  }
  if (!Number.isSafeInteger(finalPathKeyBytes) || finalPathKeyBytes > MAX_VFS_PATH_KEY_BYTES) {
    throw new Error('Static package would exceed the 16 MiB VFS path-key limit');
  }
}

function setPreparedVfsFile(path: string, bytes: Uint8Array, mode: number): void {
  const normalized = normalizePath(path);
  const parent = dirname(normalized);
  const previous = vfsFiles.get(normalized);
  const growth = bytes.byteLength - (previous?.byteLength ?? 0);
  const missingDirectories = missingVfsDirectories(parent);
  const addedNodes = missingDirectories.length + (previous === undefined ? 1 : 0);
  const addedPathBytes = vfsPathsByteLength(missingDirectories)
    + (previous === undefined ? vfsPathKeyBytes(normalized) : 0);
  if (!canAllocateVfs(addedNodes, Math.max(0, growth), addedPathBytes)) {
    throw new Error(ERR_OUT_OF_MEMORY);
  }
  ensureDir(parent);
  liveVfsBytes += growth;
  setVfsFileKey(normalized, bytes);
  vfsFileModes.set(normalized, normalizedVfsMode(mode));
  vfsFileModTimes.set(normalized, Date.now());
  deleteCachedTextFile(normalized);
}

function markStaticTreeReadOnly(root: string): void {
  for (const path of directories) {
    if (vfsPathContains(root, path)) {
      vfsDirModes.set(path, (vfsDirModes.get(path) ?? 0o755) & ~0o222);
    }
  }
  for (const path of vfsFiles.keys()) {
    if (vfsPathContains(root, path)) {
      vfsFileModes.set(path, (vfsFileModes.get(path) ?? 0o644) & ~0o222);
    }
  }
  readOnlyStaticRoots.add(root);
}

function normalizeStaticPackageMode(mode: number | undefined): number {
  if (mode == null) return 0o644;
  if (!Number.isInteger(mode) || mode < 0 || mode > 0o7777) {
    throw new Error(`Invalid static package file mode: ${mode}`);
  }
  return mode;
}

function clearStaticPackageTree(root: string): void {
  const normalized = normalizePath(root);
  if (normalized === ROOT) {
    throw new Error('Refusing to replace the VFS root');
  }
  for (const filePath of [...vfsFiles.keys()]) {
    if (filePath === normalized || filePath.startsWith(`${normalized}/`)) {
      deleteFile(filePath, false);
    }
  }
  for (const dir of [...directories].sort((a, b) => b.length - a.length)) {
    if (dir === normalized || dir.startsWith(`${normalized}/`)) {
      detachOpenVfsPath(dir, true);
      deleteVfsDirectoryKey(dir);
      vfsDirModes.delete(dir);
      vfsDirModTimes.delete(dir);
    }
  }
}

function normalizeStaticPackagePath(path: string): string {
  if (
    typeof path !== 'string'
    || path.length === 0
    || textEncoder.encode(path).byteLength > 4 * 1024
    || hasUnicodeWhiteSpaceBoundary(path)
    || path.includes('\\')
  ) {
    throw new Error(`Invalid static package path: ${path}`);
  }
  const parts = path.split('/');
  if (
    parts.length > MAX_VFS_PATH_DEPTH
    || parts.some((part) => !isPortableStaticPackageComponent(part))
  ) {
    throw new Error(`Invalid static package path: ${path}`);
  }
  return path;
}

function isPortableStaticPackageComponent(value: string): boolean {
  if (
    value.length === 0
    || textEncoder.encode(value).byteLength > 255
    || value === '.'
    || value === '..'
    || hasUnicodeWhiteSpaceBoundary(value)
    || value.endsWith('.')
    || value.normalize('NFC') !== value
    || value.includes('/')
    || value.includes('\\')
    || /[\u0000-\u001f\u007f-\u009f<>:"|?*]/u.test(value)
  ) {
    return false;
  }
  const stem = portableCaseKey(value.split('.', 1)[0]);
  return !['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].includes(stem)
    && !/^(?:com|lpt)(?:[1-9]|[¹²³])$/.test(stem);
}

function hasUnicodeWhiteSpaceBoundary(value: string): boolean {
  return /^(?:\p{White_Space})|(?:\p{White_Space})$/u.test(value);
}

function newPortablePathRegistry(): PortablePathTrie {
  return new PortablePathTrie(MAX_VFS_NODES);
}

function insertPortablePath(
  registry: PortablePathTrie,
  value: string,
  isDirectory: boolean,
  label: string,
): void {
  registry.insert(normalizeStaticPackagePath(value), isDirectory, label);
}

function parseJsonBytes(bytes: Uint8Array, label: string): unknown {
  try {
    return JSON.parse(utf8Decoder.decode(bytes));
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    throw new Error(`Invalid JSON from ${label}: ${detail}`);
  }
}

function yieldToBrowser(): Promise<void> {
  return new Promise((resolve) => window.setTimeout(resolve, 0));
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
  const ref = decodeURIComponent(parts[3]);
  return {
    owner,
    repo,
    ref,
    commit: isGitCommitSha(ref) ? ref : null,
    subdir: parts.length > 4 ? parts.slice(4).map((part) => decodeURIComponent(part)).join('/') : null,
  };
}

async function openGitHubRepoSession(source: GitHubRepoInput): Promise<SessionInfo> {
  const resolved = await resolveGitHubSource(source);
  await populateGitHubSourceCache(resolved);
  materializeGitHubSession(resolved);
  return buildSessionInfo(
    resolved.projectRoot,
    'url',
    {
      kind: 'github_repo',
      owner: resolved.owner,
      repo: resolved.repo,
      requestedRef: resolved.requestedRef,
      resolvedCommit: resolved.resolvedCommit,
      subdir: resolved.subdir,
      htmlUrl: resolved.htmlUrl,
      sourceCacheRoot: resolved.sourceCacheRoot,
    },
    'disabled',
  );
}

async function resolveGitHubSource(source: GitHubRepoInput): Promise<ResolvedGitHubSource> {
  const requestedRef = source.ref?.trim() || 'main';
  const requestedFetchRef = source.commit?.trim() || requestedRef;
  if (!requestedFetchRef) {
    throw new Error(`Could not resolve a GitHub ref for ${source.owner}/${source.repo}`);
  }
  const resolvedCommit = isGitCommitSha(requestedFetchRef)
    ? requestedFetchRef
    : await resolveGitHubCommit(source.owner, source.repo, requestedFetchRef);
  const fetchRef = resolvedCommit;
  const normalizedSubdir = source.subdir ? normalizeRelativePath(source.subdir) : null;
  const cacheKey = encodeGitHubSourceCacheKey(resolvedCommit);
  const sourceCacheRoot = `${GITHUB_SOURCE_ROOT}/${source.owner}/${source.repo}/${cacheKey}`;
  const sessionRoot = `${GITHUB_SESSION_ROOT}/${source.owner}/${source.repo}/${cacheKey}/${sessionNonce()}`;
  const projectRoot = normalizedSubdir ? `${sessionRoot}/${normalizedSubdir}` : sessionRoot;
  return {
    owner: source.owner,
    repo: source.repo,
    requestedRef,
    fetchRef,
    resolvedCommit,
    subdir: normalizedSubdir,
    htmlUrl: buildGitHubHtmlUrl(source.owner, source.repo, resolvedCommit, normalizedSubdir),
    sourceCacheRoot,
    sessionRoot,
    projectRoot,
  };
}

async function resolveGitHubCommit(owner: string, repo: string, ref: string): Promise<string> {
  const commit = await fetchJsonFromUrl<GitHubCommitMetadata>(
    `${GITHUB_API_ROOT}/repos/${encodeURIComponent(owner)}/${encodeURIComponent(repo)}/commits/${encodeURIComponent(ref)}`,
  );
  if (!commit.sha || !isGitCommitSha(commit.sha)) {
    throw new Error(`Could not resolve GitHub commit: ${owner}/${repo}@${ref}`);
  }
  return commit.sha;
}

async function populateGitHubSourceCache(resolved: ResolvedGitHubSource): Promise<void> {
  if (completedGitHubSourceCaches.has(resolved.sourceCacheRoot) && hasTreeAt(resolved.sourceCacheRoot)) {
    return;
  }
  await clearImportedRoot(resolved.sourceCacheRoot);
  ensureDir(resolved.sourceCacheRoot);
  try {
    const files = await collectGitHubSourceFiles(
      resolved.owner,
      resolved.repo,
      resolved.fetchRef,
      resolved.subdir ?? '',
    );
    if (files.length === 0) {
      throw new Error(`GitHub repository contains no files: ${resolved.owner}/${resolved.repo}`);
    }
    await runWithConcurrency(
      files,
      GITHUB_FILE_FETCH_CONCURRENCY,
      async (file) => {
        const bytes = await fetchGitHubBlobBytes(resolved.owner, resolved.repo, file.sha);
        setVfsFile(`${resolved.sourceCacheRoot}/${file.path}`, bytes, file.mode);
      },
    );
    completedGitHubSourceCaches.add(resolved.sourceCacheRoot);
  } catch (error) {
    completedGitHubSourceCaches.delete(resolved.sourceCacheRoot);
    clearImportedRootSync(resolved.sourceCacheRoot);
    throw error;
  }
}

function materializeGitHubSession(resolved: ResolvedGitHubSource): void {
  clearImportedRootSync(resolved.sessionRoot);
  copyVfsTree(resolved.sourceCacheRoot, resolved.sessionRoot);
  if (!hasTreeAt(resolved.projectRoot)) {
    throw new Error(`GitHub project root not found: ${resolved.projectRoot}`);
  }
}

function isGitCommitSha(value: string): boolean {
  return /^[0-9a-f]{40}$/i.test(value);
}

function encodeGitHubSourceCacheKey(value: string): string {
  return encodeURIComponent(value);
}

function buildGitHubHtmlUrl(owner: string, repo: string, ref: string | null, subdir: string | null): string {
  const repoUrl = `https://github.com/${owner}/${repo}`;
  if (!ref) {
    return repoUrl;
  }
  const encodedSubdir = subdir
    ? `/${subdir.split('/').map((segment) => encodeURIComponent(segment)).join('/')}`
    : '';
  return `${repoUrl}/tree/${encodeURIComponent(ref)}${encodedSubdir}`;
}

async function collectGitHubSourceFiles(
  owner: string,
  repo: string,
  ref: string,
  subdir: string,
): Promise<GitHubSourceFile[]> {
  const normalizedSubdir = subdir ? normalizeRelativePath(subdir) : '';
  const subdirPrefix = normalizedSubdir ? `${normalizedSubdir}/` : '';
  const tree = await fetchJsonFromUrl<GitHubTreeMetadata>(
    `${GITHUB_API_ROOT}/repos/${encodeURIComponent(owner)}/${encodeURIComponent(repo)}/git/trees/${encodeURIComponent(ref)}?recursive=1`,
  );
  if (tree.truncated) {
    throw new Error(`GitHub tree is too large to import: ${owner}/${repo}@${ref}`);
  }
  const files: GitHubSourceFile[] = [];
  for (const item of tree.tree ?? []) {
    if (item.type !== 'blob' || !item.path || !item.sha) {
      continue;
    }
    const normalizedPath = normalizeRelativePath(item.path);
    if (subdirPrefix && !normalizedPath.startsWith(subdirPrefix)) {
      continue;
    }
    files.push({ path: normalizedPath, mode: parseGitFileMode(item.mode), sha: item.sha });
  }
  const selectedPaths = new Set(selectGitHubImportFilePaths(files.map((file) => file.path).sort(compareUtf8)));
  return files
    .filter((file) => selectedPaths.has(file.path))
    .sort((a, b) => compareUtf8(a.path, b.path));
}

function selectGitHubImportFilePaths(paths: string[]): string[] {
  const assetPackRoots = new Set(
    paths
      .filter((path) => portableCaseKey(path).endsWith('.vpak'))
      .map((path) => dirnameRelativePath(path)),
  );
  if (assetPackRoots.size === 0) {
    return paths;
  }
  return paths.filter((path) => shouldImportPackedGitHubPath(path, assetPackRoots));
}

function shouldImportPackedGitHubPath(path: string, assetPackRoots: Set<string>): boolean {
  const normalized = normalizeRelativePath(path);
  if (normalized === '.DS_Store' || normalized.endsWith('/.DS_Store')) {
    return false;
  }
  if (normalized === 'docs' || normalized.startsWith('docs/') || normalized === 'tools' || normalized.startsWith('tools/')) {
    return false;
  }
  for (const root of assetPackRoots) {
    if (normalized === root || !normalized.startsWith(`${root}/`)) {
      continue;
    }
    return portableCaseKey(normalized).endsWith('.vpak');
  }
  return true;
}

function dirnameRelativePath(path: string): string {
  const normalized = normalizeRelativePath(path);
  const index = normalized.lastIndexOf('/');
  return index < 0 ? '' : normalized.slice(0, index);
}

async function fetchGitHubBlobBytes(owner: string, repo: string, sha: string): Promise<Uint8Array> {
  const blob = await fetchJsonFromUrl<GitHubBlobMetadata>(
    `${GITHUB_API_ROOT}/repos/${encodeURIComponent(owner)}/${encodeURIComponent(repo)}/git/blobs/${encodeURIComponent(sha)}`,
  );
  if (blob.encoding !== 'base64' || !blob.content) {
    throw new Error(`GitHub blob is not base64 encoded: ${owner}/${repo}@${sha}`);
  }
  return decodeBase64Bytes(blob.content);
}

async function fetchJsonFromUrl<T>(url: string, headers?: Record<string, string>): Promise<T> {
  const response = await fetch(url, {
    headers: {
      Accept: 'application/vnd.github+json',
      ...headers,
    },
  });
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${url}`);
  }
  return response.json() as Promise<T>;
}

async function fetchBytesFromUrl(
  url: string,
  headers?: Record<string, string>,
  maxBytes = 256 * 1024 * 1024,
): Promise<Uint8Array> {
  const response = await fetch(url, { headers });
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}: ${url}`);
  }
  return responseBytesLimited(response, maxBytes, url);
}

async function responseBytesLimited(
  response: Response,
  maxBytes: number,
  label: string,
): Promise<Uint8Array> {
  const declared = response.headers.get('content-length');
  if (declared != null) {
    const size = Number(declared);
    if (!Number.isSafeInteger(size) || size < 0 || size > maxBytes) {
      throw new Error(`Response exceeds the ${maxBytes}-byte limit: ${label}`);
    }
  }
  if (!response.body) {
    const bytes = new Uint8Array(await response.arrayBuffer());
    if (bytes.byteLength > maxBytes) {
      throw new Error(`Response exceeds the ${maxBytes}-byte limit: ${label}`);
    }
    return bytes;
  }

  const reader = response.body.getReader();
  const chunks: Uint8Array[] = [];
  let total = 0;
  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      total += value.byteLength;
      if (total > maxBytes) {
        await reader.cancel();
        throw new Error(`Response exceeds the ${maxBytes}-byte limit: ${label}`);
      }
      chunks.push(value);
    }
  } finally {
    reader.releaseLock();
  }
  const bytes = new Uint8Array(total);
  let offset = 0;
  for (const chunk of chunks) {
    bytes.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return bytes;
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
      deleteVfsDirectoryKey(dir);
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
  const bytes = await fetchBytesFromUrl(url);
  if (looksLikeTarGz(url, bytes)) {
    const extracted = await extractTarGzFiles(bytes);
    if (extracted.length === 0) {
      throw new Error('archive contains no files');
    }
    const prefix = sharedPrefix(extracted.map((file) => file.path));
    const prepared: PreparedStaticFile[] = [];
    const seen = newPortablePathRegistry();
    for (const file of extracted) {
      const relative = prefix && file.path.startsWith(`${prefix}/`)
        ? file.path.slice(prefix.length + 1)
        : file.path;
      if (!relative) continue;
      const portable = normalizeStaticPackagePath(relative);
      insertPortablePath(seen, portable, false, 'archive path');
      prepared.push({ relative: portable, bytes: file.bytes, mode: 0o644 });
    }
    replacePreparedStaticTreesAtomically([{ root: sessionRoot, files: prepared }]);
  } else {
    const decoder = new TextDecoder('utf-8', { fatal: true });
    const content = decoder.decode(bytes);
    let fileName = fileNameFromUrl(url) ?? 'main.vo';
    if (!fileName.includes('.')) {
      fileName += '.vo';
    }
    const relative = normalizeStaticPackagePath(fileName);
    replacePreparedStaticTreesAtomically([{
      root: sessionRoot,
      files: [{ relative, bytes: textEncoder.encode(content), mode: 0o644 }],
    }]);
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
      deleteVfsDirectoryKey(dir);
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
  const tarBytes = await responseBytesLimited(
    new Response(decompressed),
    MAX_IMPORTED_TAR_BYTES,
    'decompressed tar archive',
  );
  return parseTarFiles(tarBytes);
}

function parseTarFiles(bytes: Uint8Array): Array<{ path: string; bytes: Uint8Array }> {
  const decoder = new TextDecoder('utf-8', { fatal: true });
  const filesOut: Array<{ path: string; bytes: Uint8Array }> = [];
  const paths = newPortablePathRegistry();
  let totalBytes = 0;
  let offset = 0;
  while (offset + 512 <= bytes.length) {
    const header = bytes.subarray(offset, offset + 512);
    if (header.every((value) => value === 0)) {
      if (!bytes.subarray(offset).every((value) => value === 0)) {
        throw new Error('Tar archive has non-zero data after its end marker');
      }
      return filesOut;
    }
    validateTarHeaderChecksum(header);
    const name = readTarString(decoder, header, 0, 100);
    const prefix = readTarString(decoder, header, 345, 155);
    const typeFlag = header[156];
    const size = parseTarOctal(header.subarray(124, 136), 'entry size');
    const rawPath = prefix ? `${prefix}/${name}` : name;
    const kind = typeFlag === 53 ? 'directory' : typeFlag === 0 || typeFlag === 48 ? 'file' : null;
    if (kind === null) {
      throw new Error(`Tar archive contains unsupported entry type ${typeFlag} at ${rawPath}`);
    }
    const path = sanitizeTarPath(kind === 'directory' ? rawPath.replace(/\/+$/g, '') : rawPath);
    if (!path) {
      throw new Error(`Tar archive contains an invalid path: ${rawPath}`);
    }
    const portable = normalizeStaticPackagePath(path);
    insertPortablePath(paths, portable, kind === 'directory', 'tar archive path');
    const bodyStart = offset + 512;
    const bodyEnd = bodyStart + size;
    const paddedEnd = bodyStart + Math.ceil(size / 512) * 512;
    if (
      !Number.isSafeInteger(bodyEnd)
      || !Number.isSafeInteger(paddedEnd)
      || bodyEnd > bytes.length
      || paddedEnd > bytes.length
    ) {
      throw new Error(`Tar archive entry is truncated: ${portable}`);
    }
    if (kind === 'directory') {
      if (size !== 0) {
        throw new Error(`Tar directory entry has a non-zero body: ${portable}`);
      }
    } else {
      if (filesOut.length >= MAX_STATIC_PACKAGE_FILES) {
        throw new Error(`Tar archive exceeds the ${MAX_STATIC_PACKAGE_FILES}-file limit`);
      }
      if (size > MAX_STATIC_PACKAGE_FILE_BYTES) {
        throw new Error(`Tar archive entry exceeds the ${MAX_STATIC_PACKAGE_FILE_BYTES}-byte limit: ${portable}`);
      }
      totalBytes += size;
      if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_STATIC_PACKAGE_TOTAL_BYTES) {
        throw new Error('Tar archive exceeds the aggregate payload limit');
      }
      filesOut.push({ path: portable, bytes: bytes.slice(bodyStart, bodyEnd) });
    }
    offset = paddedEnd;
  }
  if (offset !== bytes.length) {
    throw new Error('Tar archive ends with a truncated header');
  }
  return filesOut;
}

function readTarString(decoder: TextDecoder, header: Uint8Array, start: number, length: number): string {
  const slice = header.subarray(start, start + length);
  const zeroIndex = slice.indexOf(0);
  return decoder.decode(zeroIndex >= 0 ? slice.subarray(0, zeroIndex) : slice);
}

function parseTarOctal(value: Uint8Array, label: string): number {
  if ((value[0] & 0x80) !== 0) {
    throw new Error(`Tar ${label} uses unsupported base-256 encoding`);
  }
  const rendered = String.fromCharCode(...value);
  const trimmed = rendered.replace(/^ +|[ \0]+$/g, '');
  if (trimmed.length === 0) {
    return 0;
  }
  if (!/^[0-7]+$/.test(trimmed)) {
    throw new Error(`Tar ${label} is not canonical octal`);
  }
  const parsed = Number.parseInt(trimmed, 8);
  if (!Number.isSafeInteger(parsed) || parsed < 0) {
    throw new Error(`Tar ${label} exceeds the safe integer range`);
  }
  return parsed;
}

function validateTarHeaderChecksum(header: Uint8Array): void {
  const expected = parseTarOctal(header.subarray(148, 156), 'header checksum');
  let actual = 0;
  for (let index = 0; index < header.byteLength; index += 1) {
    actual += index >= 148 && index < 156 ? 0x20 : header[index];
  }
  if (actual !== expected) {
    throw new Error(`Tar header checksum mismatch: expected ${expected}, computed ${actual}`);
  }
}

function sanitizeTarPath(path: string): string | null {
  if (path.startsWith('/') || path.endsWith('/') || path.includes('\\')) {
    return null;
  }
  const parts = path.split('/');
  if (parts.length === 0 || parts.some((part) => !part || part === '.' || part === '..')) {
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

function encodeBase64Bytes(bytes: Uint8Array): string {
  const chunks: string[] = [];
  for (let offset = 0; offset < bytes.length; offset += 0x8000) {
    let binary = '';
    const chunk = bytes.subarray(offset, Math.min(offset + 0x8000, bytes.length));
    for (let i = 0; i < chunk.length; i += 1) {
      binary += String.fromCharCode(chunk[i]);
    }
    chunks.push(binary);
  }
  return btoa(chunks.join(''));
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
