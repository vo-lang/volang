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
  FrameworkContract,
  LaunchSpec,
  ProcEvent,
  PreparedSession,
  ReadManyResult,
  RendererBridgeVfsSnapshot,
  RunEvent,
  RunOpts,
  SessionInfo,
  StreamHandle,
} from '../types';

import { invoke as tauriInvoke, listen as tauriListen } from '../tauri';
import { consolePush } from '../../stores/console';
import { formatDurationMs, pushUiConsole, renderStudioLogRecord, type StudioLogRecord } from './gui_console';
import { makeTauriStreamHandle } from './stream_handle';
import { GuiSessionBinding, type GuiSessionToken } from '../gui_session';

type StudioLogEvent = { sessionId: number; record: StudioLogRecord };
type GuiGuestExitEvent = { sessionId: number; exitCode: number };

type NativeGuiRunResult = {
  renderBytes: number[];
  moduleBytes: number[];
  entryPath: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  hostWidgetHandlerId: number | null;
};

function displayPath(path: string): string {
  const normalized = path.trim().replace(/\\/g, '/');
  return normalized || path;
}

function waitForNextUiFrame(): Promise<void> {
  return new Promise((resolve) => {
    let settled = false;
    const finish = () => {
      if (settled) {
        return;
      }
      settled = true;
      resolve();
    };
    if (typeof window !== 'undefined' && typeof window.requestAnimationFrame === 'function') {
      window.requestAnimationFrame(() => finish());
      setTimeout(finish, 50);
      return;
    }
    setTimeout(finish, 0);
  });
}

function pushNativeStudioLog(record: StudioLogRecord): void {
  pushUiConsole(renderStudioLogRecord(record, displayPath));
}

export class NativeBackend implements Backend {
  readonly platform = 'native' as const;
  private readonly guiSession = new GuiSessionBinding();
  private guiLogListenerPromise: Promise<void> | null = null;
  private guiExitListenerPromise: Promise<void> | null = null;
  private guiGuestExitHandler: ((session: GuiSessionToken, exitCode: number) => void) | null = null;

  setGuiGuestExitHandler(handler: ((session: GuiSessionToken, exitCode: number) => void) | null): void {
    this.guiGuestExitHandler = handler;
  }

  async getBootstrapContext(): Promise<BootstrapContext> {
    return this.invoke<BootstrapContext>('cmd_get_bootstrap_context');
  }

  async openSession(spec: LaunchSpec): Promise<SessionInfo> {
    return this.activateSession(await this.prepareSession(spec));
  }

  async prepareSession(spec: LaunchSpec): Promise<PreparedSession> {
    return this.invoke<PreparedSession>('cmd_prepare_session', { spec });
  }

  async activateSession(candidate: PreparedSession): Promise<SessionInfo> {
    return this.invoke<SessionInfo>('cmd_activate_session', { candidate });
  }

  async restoreSession(previous: SessionInfo): Promise<SessionInfo> {
    return this.invoke<SessionInfo>('cmd_restore_session', { previous });
  }

  async discardPreparedSession(candidate: PreparedSession): Promise<void> {
    await this.invoke<void>('cmd_discard_prepared_session', { candidate });
  }

  async listPreparedSessionDir(candidate: PreparedSession, path: string): Promise<FsEntry[]> {
    return this.invoke<FsEntry[]>('cmd_list_prepared_session_dir', { candidate, path });
  }

  async readPreparedSessionFile(candidate: PreparedSession, path: string): Promise<string> {
    return this.invoke<string>('cmd_read_prepared_session_file', { candidate, path });
  }

  async discoverProjects(root: string): Promise<DiscoveredProject[]> {
    return this.invoke<DiscoveredProject[]>('cmd_discover_projects', { root });
  }

  async discoverWorkspaceProjects(): Promise<DiscoveredProject[]> {
    return this.invoke<DiscoveredProject[]>('cmd_discover_workspace_projects');
  }

  async listDir(path: string): Promise<FsEntry[]> {
    return this.invoke<FsEntry[]>('cmd_list_dir', { path });
  }

  async statPath(path: string): Promise<FsStat> {
    return this.invoke<FsStat>('cmd_stat_path', { path });
  }

  async readFile(path: string): Promise<string> {
    return this.invoke<string>('cmd_read_file', { path });
  }

  async readMany(paths: string[]): Promise<ReadManyResult[]> {
    return this.invoke<ReadManyResult[]>('cmd_read_many', { paths });
  }

  async writeFile(path: string, content: string): Promise<void> {
    await this.invoke<void>('cmd_write_file', { path, content });
  }

  async mkdir(path: string): Promise<void> {
    await this.invoke<void>('cmd_mkdir', { path });
  }

  async removeEntry(path: string, recursive: boolean): Promise<void> {
    await this.invoke<void>('cmd_remove_entry', { path, recursive });
  }

  async renameEntry(oldPath: string, newPath: string): Promise<void> {
    await this.invoke<void>('cmd_rename_entry', { oldPath, newPath });
  }

  async copyEntry(src: string, dst: string): Promise<void> {
    await this.invoke<void>('cmd_copy_entry', { src, dst });
  }

  async grep(path: string, pattern: string, opts?: GrepOpts): Promise<GrepMatch[]> {
    return this.invoke<GrepMatch[]>('cmd_grep', {
      path,
      pattern,
      caseSensitive: opts?.caseSensitive ?? false,
      maxResults: opts?.maxResults ?? 500,
    });
  }

  async checkVo(path: string): Promise<CheckResult> {
    return this.invoke<CheckResult>('cmd_check_vo', { path });
  }

  async compileVo(path: string): Promise<CompileResult> {
    return this.invoke<CompileResult>('cmd_compile_vo', { path });
  }

  async formatVo(path: string): Promise<string> {
    return this.invoke<string>('cmd_format_vo', { path });
  }

  async buildVo(path: string, output?: string): Promise<BuildResult> {
    return this.invoke<BuildResult>('cmd_build_vo', { path, output });
  }

  async dumpVo(path: string): Promise<string> {
    return this.invoke<string>('cmd_dump_vo', { path });
  }

  runVo(path: string, opts?: RunOpts): StreamHandle<RunEvent> {
    const runMode = opts?.mode ?? 'vm';
    return makeTauriStreamHandle<RunEvent>((channel) =>
      this.invoke<void>('cmd_run_vo_stream', { path, runMode, onEvent: channel }),
      (event) => event.kind === 'done' || event.kind === 'error' || event.kind === 'stopped',
    );
  }

  async stopVoRun(): Promise<void> {
    await this.invoke<void>('cmd_stop_vo_run');
  }

  async runGui(path: string, session: GuiSessionToken): Promise<GuiRunOutput> {
    const sessionId = session.id;
    this.guiSession.activate(session);
    const targetLabel = displayPath(path);
    try {
      consolePush('system', `Opening GUI ${targetLabel}`);
      consolePush('system', `Preparing dependencies and compiling GUI ${targetLabel}...`);
      await waitForNextUiFrame();
      await this.ensureGuiLogListener();
      await this.ensureGuiExitListener();
      const totalStart = performance.now();

      const result = await this.invoke<NativeGuiRunResult>('cmd_run_gui', { entryPath: path, sessionId });

      if (!this.guiSession.isActive(session)) {
        throw new Error('GUI session superseded');
      }

      consolePush('success', `Opened GUI ${targetLabel} in ${formatDurationMs(performance.now() - totalStart)}`);
      return {
        renderBytes: new Uint8Array(result.renderBytes),
        moduleBytes: new Uint8Array(result.moduleBytes),
        entryPath: result.entryPath,
        framework: result.framework,
        providerFrameworks: result.providerFrameworks,
        hostWidgetHandlerId: result.hostWidgetHandlerId,
      };
    } catch (error) {
      this.guiSession.clear(session);
      throw error;
    }
  }

  async sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array> {
    const raw = await this.invoke<ArrayBuffer>('cmd_send_gui_event', { handlerId, payload });
    return new Uint8Array(raw);
  }

  async sendGuiEventAsync(handlerId: number, payload: string): Promise<void> {
    await this.invoke<void>('cmd_send_gui_event_async', { handlerId, payload });
  }

  async pushIslandTransport(data: Uint8Array): Promise<void> {
    await this.invoke<void>('cmd_push_island_transport', { data: Array.from(data) });
  }

  async pushAndPollIslandTransport(data: Uint8Array): Promise<Uint8Array[]> {
    await this.pushIslandTransport(data);
    return [];
  }

  async pollIslandTransport(): Promise<Uint8Array> {
    return new Uint8Array(0);
  }

  async pollGuiRender(): Promise<Uint8Array> {
    const raw = await this.invoke<ArrayBuffer>('cmd_poll_gui_render');
    return new Uint8Array(raw);
  }

  async stopGui(): Promise<void> {
    this.guiSession.clear();
    await this.invoke<void>('cmd_stop_gui');
  }

  async getRendererBridgeVfsSnapshot(path: string): Promise<RendererBridgeVfsSnapshot> {
    const raw = await this.invoke<{ rootPath: string; files: Array<{ path: string; bytes: number[] }> }>(
      'cmd_get_renderer_bridge_vfs_snapshot',
      { entryPath: path },
    );
    return {
      rootPath: raw.rootPath,
      files: raw.files.map((file) => ({ path: file.path, bytes: new Uint8Array(file.bytes) })),
    };
  }

  async voInit(path: string, module: string, mainContent: string): Promise<string> {
    return this.invoke<string>('cmd_vo_init', { path, module, mainContent });
  }

  async voVersion(): Promise<string> {
    return this.invoke<string>('cmd_vo_version');
  }

  spawnProcess(
    program: string,
    args: string[],
    cwd?: string,
    env?: Record<string, string>,
  ): StreamHandle<ProcEvent> {
    return makeTauriStreamHandle<ProcEvent>((channel) =>
      this.invoke<void>('cmd_spawn_process', { program, args, cwd, env, onEvent: channel }),
      (event) => event.kind === 'done' || event.kind === 'error',
    );
  }

  async httpRequest(method: string, url: string, opts?: HttpOpts): Promise<HttpResult> {
    return this.invoke<HttpResult>('cmd_http_request', { method, url, opts });
  }

  async pickDirectory(defaultPath?: string): Promise<string | null> {
    return this.invoke<string | null>('cmd_pick_directory', { defaultPath: defaultPath ?? null });
  }

  async pickFile(defaultPath?: string, filters?: FileDialogFilter[]): Promise<string | null> {
    return this.invoke<string | null>('cmd_pick_file', {
      defaultPath: defaultPath ?? null,
      filters: filters ?? null,
    });
  }

  async createWorkspaceFiles(files: { path: string; content: string }[]): Promise<void> {
    await this.invoke<void>('cmd_create_workspace_files', { files });
  }

  async createProjectFiles(files: { path: string; content: string }[]): Promise<void> {
    await this.invoke<void>('cmd_create_project_files', { files });
  }

  async gitExec(op: GitOp): Promise<GitResult> {
    return this.invoke<GitResult>('cmd_git_exec', { op });
  }

  private async invoke<T>(command: string, args?: Record<string, unknown>): Promise<T> {
    return tauriInvoke<T>(command, args);
  }

  private async ensureGuiLogListener(): Promise<void> {
    if (!this.guiLogListenerPromise) {
      this.guiLogListenerPromise = tauriListen<StudioLogEvent>('studio_log', (event) => {
        if (!this.guiSession.isActiveId(event.payload.sessionId)) {
          return;
        }
        pushNativeStudioLog(event.payload.record);
      }).then(() => undefined);
    }
    await this.guiLogListenerPromise;
  }

  private async ensureGuiExitListener(): Promise<void> {
    if (!this.guiExitListenerPromise) {
      this.guiExitListenerPromise = tauriListen<GuiGuestExitEvent>('gui_guest_exit', (event) => {
        const session = this.guiSession.active;
        if (!session || event.payload.sessionId !== session.id) {
          return;
        }
        const exitCode = event.payload.exitCode;
        // Invalidate this frontend session immediately. The native worker has
        // already shut down and the next run clears its inert handle.
        this.guiSession.clear(session);
        this.guiGuestExitHandler?.(session, exitCode);
      }).then(() => undefined);
    }
    await this.guiExitListenerPromise;
  }
}
