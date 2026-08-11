import type {
  BootstrapContext,
  DiscoveredProject,
  DisplayPulseSubmission,
  DisplayTimingRequest,
  FsEntry,
  FrameworkLaneBinding,
  GitOp,
  GitResult,
  GuiRunOutput,
  HttpOpts,
  HttpResult,
  LaunchSpec,
  ProcEvent,
  PreparedSession,
  RunEvent,
  RunOpts,
  SessionInfo,
  StreamHandle,
} from '../types';
import type { GuiSessionToken } from '../gui_session';


export interface FileDialogFilter {
  name: string;
  extensions: string[];
}

export type RuntimeHandle = Readonly<{ index: number; generation: number }>;

export type ResolvedAppSurfaceRoute = Readonly<{
  session: RuntimeHandle;
  sessionEpoch: bigint;
  window: RuntimeHandle;
  view: RuntimeHandle;
  surface: RuntimeHandle;
  kind: 'game' | 'ui' | 'diagnostics';
  zOrder: number;
  inputPolicy: 'observe' | 'passthrough' | 'interactive' | 'exclusive';
}>;

export interface Backend {
  readonly platform: 'native' | 'wasm';

  // Bootstrap
  getBootstrapContext(): Promise<BootstrapContext>;

  // Session
  openSession(spec: LaunchSpec): Promise<SessionInfo>;
  prepareSession(spec: LaunchSpec): Promise<PreparedSession>;
  activateSession(candidate: PreparedSession): Promise<SessionInfo>;
  restoreSession(previous: SessionInfo): Promise<SessionInfo>;
  discardPreparedSession(candidate: PreparedSession): Promise<void>;
  listPreparedSessionDir(candidate: PreparedSession, path: string): Promise<FsEntry[]>;
  readPreparedSessionFile(candidate: PreparedSession, path: string): Promise<string>;

  // Filesystem
  discoverWorkspaceProjects(): Promise<DiscoveredProject[]>;
  listDir(path: string): Promise<FsEntry[]>;
  readFile(path: string): Promise<string>;
  writeFile(path: string, content: string): Promise<void>;
  mkdir(path: string): Promise<void>;
  removeEntry(path: string, recursive: boolean): Promise<void>;
  renameEntry(oldPath: string, newPath: string): Promise<void>;

  // Compiler
  dumpVo(path: string): Promise<string>;

  // Runtime
  runVo(path: string, opts?: RunOpts): StreamHandle<RunEvent>;
  stopVoRun(): Promise<void>;
  runGui(path: string, session: GuiSessionToken): Promise<GuiRunOutput>;
  selectGuiPreview(session: GuiSessionToken): Promise<void>;
  setGuiGuestExitHandler(handler: ((session: GuiSessionToken, exitCode: number) => void) | null): void;
  setGuiGuestErrorHandler(handler: ((session: GuiSessionToken, error: Error) => void) | null): void;
  sendGuiEvent(handlerId: number, payload: string, session?: GuiSessionToken): Promise<Uint8Array>;
  sendGuiEventAsync(handlerId: number, payload: string, session?: GuiSessionToken): Promise<void>;
  pushAndPollIslandTransport(data: Uint8Array, session?: GuiSessionToken): Promise<Uint8Array[]>;
  pollIslandTransport(session?: GuiSessionToken): Promise<Uint8Array>;
  pollGuiRender(session?: GuiSessionToken): Promise<Uint8Array>;
  pollGameRender(session?: GuiSessionToken): Promise<Uint8Array>;
  submitGameRenderResult?(result: Uint8Array, session?: GuiSessionToken): Promise<void>;
  resolveAppSurfaceRoute?(
    surface: RuntimeHandle,
    session?: GuiSessionToken,
  ): Promise<ResolvedAppSurfaceRoute>;
  registerAppSurfaceShortcuts?(
    surface: RuntimeHandle,
    registrations: readonly Readonly<{
      classMask: bigint;
      scope: 'view' | 'window' | 'session';
      priority: number;
    }>[],
    session?: GuiSessionToken,
  ): Promise<bigint>;
  restartComposedWebview?(session?: GuiSessionToken): Promise<bigint>;
  completeVoguiTargetCommit?(
    accepted: boolean,
    providerError: string,
    session?: GuiSessionToken,
  ): Promise<void>;
  loadFrameworkProvider?(moduleKey: string, session?: GuiSessionToken): Promise<void>;
  unloadFrameworkProvider?(moduleKey: string, session?: GuiSessionToken): Promise<void>;
  beginFrameworkProvider?(moduleKey: string, session?: GuiSessionToken): Promise<void>;
  readyFrameworkProvider?(moduleKey: string, session?: GuiSessionToken): Promise<void>;
  abortFrameworkProvider?(moduleKey: string, session?: GuiSessionToken): Promise<void>;
  closeFrameworkProvider?(moduleKey: string, session?: GuiSessionToken): Promise<void>;
  openFrameworkLane(owner: string, session?: GuiSessionToken): Promise<FrameworkLaneBinding>;
  pollFrameworkLane(
    binding: FrameworkLaneBinding,
    session?: GuiSessionToken,
  ): Promise<Uint8Array>;
  submitFrameworkLane(
    binding: FrameworkLaneBinding,
    packet: Uint8Array,
    session?: GuiSessionToken,
  ): Promise<void>;
  submitFrameworkLaneBatch(
    binding: FrameworkLaneBinding,
    packets: readonly Uint8Array[],
    session?: GuiSessionToken,
  ): Promise<void>;
  pollDisplayTimingRequest(session?: GuiSessionToken): Promise<DisplayTimingRequest | null>;
  submitDisplayPulse(
    request: DisplayTimingRequest,
    observedMicros: string,
    intervalMicros: string,
    session?: GuiSessionToken,
  ): Promise<DisplayPulseSubmission>;
  stopGui(session?: GuiSessionToken): Promise<void>;
  // Toolchain
  voInit(path: string, module: string, mainContent: string): Promise<string>;

  // Process (native only)
  spawnProcess(program: string, args: string[], cwd?: string, env?: Record<string, string>): StreamHandle<ProcEvent>;

  // HTTP
  httpRequest(method: string, url: string, opts?: HttpOpts): Promise<HttpResult>;

  // Dialog (native only — web returns null)
  pickDirectory(defaultPath?: string): Promise<string | null>;
  pickFile(defaultPath?: string, filters?: FileDialogFilter[]): Promise<string | null>;

  // Project creation
  createWorkspaceFiles(files: { path: string; content: string }[]): Promise<void>;
  // Creates one new .vo file in an existing directory without replacing anything.
  createProjectFile(path: string, content: string): Promise<void>;

  // Git
  gitExec(op: GitOp): Promise<GitResult>;
}
