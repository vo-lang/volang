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


export interface FileDialogFilter {
  name: string;
  extensions: string[];
}

export interface Backend {
  readonly platform: 'native' | 'wasm';

  // Bootstrap
  getBootstrapContext(): Promise<BootstrapContext>;

  // Session
  openSession(spec: LaunchSpec): Promise<SessionInfo>;

  // Filesystem
  discoverProjects(root: string): Promise<DiscoveredProject[]>;
  discoverWorkspaceProjects(): Promise<DiscoveredProject[]>;
  listDir(path: string): Promise<FsEntry[]>;
  statPath(path: string): Promise<FsStat>;
  readFile(path: string): Promise<string>;
  readMany(paths: string[]): Promise<ReadManyResult[]>;
  writeFile(path: string, content: string): Promise<void>;
  mkdir(path: string): Promise<void>;
  removeEntry(path: string, recursive: boolean): Promise<void>;
  renameEntry(oldPath: string, newPath: string): Promise<void>;
  copyEntry(src: string, dst: string): Promise<void>;
  grep(path: string, pattern: string, opts?: GrepOpts): Promise<GrepMatch[]>;

  // Compiler
  checkVo(path: string): Promise<CheckResult>;
  compileVo(path: string): Promise<CompileResult>;
  formatVo(path: string): Promise<string>;
  buildVo(path: string, output?: string): Promise<BuildResult>;
  dumpVo(path: string): Promise<string>;

  // Runtime
  runVo(path: string, opts?: RunOpts): StreamHandle<RunEvent>;
  stopVoRun(): Promise<void>;
  runGui(path: string): Promise<GuiRunOutput>;
  sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array>;
  sendGuiEventAsync(handlerId: number, payload: string): Promise<void>;
  pushIslandTransport(data: Uint8Array): Promise<void>;
  pollIslandTransport(): Promise<Uint8Array>;
  pollGuiRender(): Promise<Uint8Array>;
  stopGui(): Promise<void>;
  getRendererBridgeVfsSnapshot(path: string): Promise<RendererBridgeVfsSnapshot>;

  // Toolchain
  voGet(spec: string): StreamHandle<InstallEvent>;
  voInit(path: string, name?: string): Promise<string>;
  voVersion(): Promise<string>;
  listInstalledModules(): Promise<InstalledModule[]>;

  // Process (native only)
  spawnProcess(program: string, args: string[], cwd?: string, env?: Record<string, string>): StreamHandle<ProcEvent>;

  // HTTP
  httpRequest(method: string, url: string, opts?: HttpOpts): Promise<HttpResult>;

  // Dialog (native only — web returns null)
  pickDirectory(defaultPath?: string): Promise<string | null>;
  pickFile(defaultPath?: string, filters?: FileDialogFilter[]): Promise<string | null>;

  // Project creation (bypass session root restriction)
  createProjectFiles(files: { path: string; content: string }[]): Promise<void>;

  // Git
  gitExec(op: GitOp): Promise<GitResult>;
}
