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
import { makeTauriStreamHandle } from './stream_handle';

type Invoke = <T>(command: string, args?: Record<string, unknown>) => Promise<T>;

export class NativeBackend implements Backend {
  readonly platform = 'native' as const;
  private invokePromise: Promise<Invoke> | null = null;

  async getBootstrapContext(): Promise<BootstrapContext> {
    return this.invoke<BootstrapContext>('cmd_get_bootstrap_context');
  }

  async openWorkspaceSession(): Promise<SessionInfo> {
    return this.invoke<SessionInfo>('cmd_open_workspace_session');
  }

  async openRunSession(path: string): Promise<SessionInfo> {
    return this.invoke<SessionInfo>('cmd_open_run_session', { path });
  }

  async openUrlSession(url: string): Promise<SessionInfo> {
    return this.invoke<SessionInfo>('cmd_open_url_session', { url });
  }

  async discoverProjects(root: string): Promise<DiscoveredProject[]> {
    return this.invoke<DiscoveredProject[]>('cmd_discover_projects', { root });
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

  async runGui(path: string): Promise<GuiRunOutput> {
    const raw = await this.invoke<{
      renderBytes: number[];
      moduleBytes: number[];
      entryPath: string;
      framework: { name: string; entry: string; capabilities: string[]; rendererPath: string | null } | null;
      externalWidgetHandlerId: number | null;
    }>('cmd_run_gui', { entryPath: path });
    return {
      renderBytes: new Uint8Array(raw.renderBytes),
      moduleBytes: new Uint8Array(raw.moduleBytes),
      entryPath: raw.entryPath,
      framework: raw.framework,
      externalWidgetHandlerId: raw.externalWidgetHandlerId ?? null,
    };
  }

  async sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array> {
    const raw = await this.invoke<number[]>('cmd_send_gui_event', { handlerId, payload });
    return new Uint8Array(raw);
  }

  async sendGuiEventAsync(handlerId: number, payload: string): Promise<void> {
    await this.invoke<void>('cmd_send_gui_event_async', { handlerId, payload });
  }

  async pushIslandTransport(data: Uint8Array): Promise<void> {
    await this.invoke<void>('__island_transport_push', { data: Array.from(data) });
  }

  async pollGuiRender(): Promise<Uint8Array> {
    const raw = await this.invoke<number[]>('cmd_poll_gui_render');
    return new Uint8Array(raw);
  }

  async stopGui(): Promise<void> {
    await this.invoke<void>('cmd_stop_gui');
  }

  async getRenderIslandVfsSnapshot(path: string): Promise<RenderIslandVfsSnapshot> {
    const raw = await this.invoke<{ rootPath: string; files: Array<{ path: string; bytes: number[] }> }>(
      'cmd_get_render_island_vfs_snapshot',
      { entryPath: path },
    );
    return {
      rootPath: raw.rootPath,
      files: raw.files.map((file) => ({ path: file.path, bytes: new Uint8Array(file.bytes) })),
    };
  }

  voGet(spec: string): StreamHandle<InstallEvent> {
    return makeTauriStreamHandle<InstallEvent>((channel) =>
      this.invoke<void>('cmd_vo_get_stream', { spec, onEvent: channel }),
      (event) => event.kind === 'done' || event.kind === 'error',
    );
  }

  async voInit(path: string, name?: string): Promise<string> {
    return this.invoke<string>('cmd_vo_init', { path, name });
  }

  async voVersion(): Promise<string> {
    return this.invoke<string>('cmd_vo_version');
  }

  async listInstalledModules(): Promise<InstalledModule[]> {
    return this.invoke<InstalledModule[]>('cmd_list_installed_modules');
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

  async gitExec(op: GitOp): Promise<GitResult> {
    return this.invoke<GitResult>('cmd_git_exec', { op });
  }

  private async invoke<T>(command: string, args?: Record<string, unknown>): Promise<T> {
    const invoke = await this.getInvoke();
    return invoke<T>(command, args);
  }

  private async getInvoke(): Promise<Invoke> {
    if (!this.invokePromise) {
      this.invokePromise = import('@tauri-apps/api/core').then(({ invoke }) => invoke as Invoke);
    }
    return this.invokePromise;
  }
}
