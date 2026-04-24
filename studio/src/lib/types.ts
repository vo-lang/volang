export type StudioMode = 'dev' | 'runner';
export type SessionOrigin = 'workspace' | 'run-target' | 'url';
export type ProjectMode = 'single-file' | 'module';
export type BackendPlatform = 'native' | 'wasm';
export type WorkspaceDiscoveryMode = 'auto' | 'disabled';

export interface LaunchSpec {
  proj: string | null;
  mode: StudioMode;
}

export interface BootstrapContext {
  workspaceRoot: string;
  launch: LaunchSpec | null;
  mode: StudioMode;
  platform: BackendPlatform;
}

export type SessionSource =
  | { kind: 'workspace' }
  | { kind: 'path'; path: string }
  | {
      kind: 'github_repo';
      owner: string;
      repo: string;
      requestedRef: string | null;
      resolvedCommit: string | null;
      subdir: string | null;
      htmlUrl: string;
      sourceCacheRoot: string;
    };

export interface ShareInfo {
  canonicalUrl: string;
  shareable: boolean;
  reason?: string;
}

export interface SessionInfo {
  root: string;
  origin: SessionOrigin;
  projectMode: ProjectMode;
  entryPath: string | null;
  singleFileRun: boolean;
  workspaceDiscovery: WorkspaceDiscoveryMode;
  source: SessionSource | null;
  share: ShareInfo | null;
}

export interface DiscoveredProject {
  name: string;
  type: string;
  localPath: string;
  entryPath: string | null;
}

export interface FsEntry {
  name: string;
  path: string;
  isDir: boolean;
  size?: number;
  modifiedMs?: number;
}

export interface FsStat {
  path: string;
  isDir: boolean;
  isFile: boolean;
  size: number;
  modifiedMs: number;
}

export interface GrepOpts {
  caseSensitive?: boolean;
  maxResults?: number;
}

export interface GrepMatch {
  path: string;
  line: number;
  column: number;
  text: string;
}

export interface CheckResult {
  ok: boolean;
  errors: DiagnosticError[];
}

export interface CompileResult {
  ok: boolean;
  errors: DiagnosticError[];
  outputPath?: string;
}

export interface BuildResult {
  ok: boolean;
  errors: DiagnosticError[];
  outputPath?: string;
}

export interface DiagnosticError {
  file: string;
  line: number;
  column: number;
  message: string;
  category: string;
  moduleStage?: string | null;
  moduleKind?: string | null;
  modulePath?: string | null;
  moduleVersion?: string | null;
}

export type RunEvent =
  | { kind: 'stdout'; text: string }
  | { kind: 'stderr'; text: string }
  | { kind: 'stopped' }
  | { kind: 'done'; exitCode: number; durationMs: number }
  | { kind: 'error'; message: string };

export interface RunOpts {
  mode?: 'vm' | 'jit';
  args?: string[];
}

export type InstallEvent =
  | { kind: 'fetch'; message: string }
  | { kind: 'build'; line: string }
  | { kind: 'done'; module: string }
  | { kind: 'error'; message: string };

export interface InstalledModule {
  spec: string;
  path: string;
  hasNativeExt: boolean;
  hasWasmExt: boolean;
}

export interface FrameworkContract {
  name: string;
  entry: string | null;
  capabilities: string[];
  jsModules: Record<string, string>;
}

export function frameworkJsModulePath(framework: FrameworkContract, moduleName: string): string | null {
  return framework.jsModules[moduleName] ?? null;
}

export function frameworkContractKey(framework: FrameworkContract): string {
  const capabilities = [...framework.capabilities].sort();
  const jsModules = Object.entries(framework.jsModules)
    .sort(([left], [right]) => left.localeCompare(right))
    .flatMap(([name, path]) => [name, path]);
  return [
    framework.name,
    framework.entry ?? '',
    ...capabilities,
    ...jsModules,
  ].join('\0');
}

export interface GuiSession {
  initialRender: Uint8Array;
  moduleBytes: Uint8Array;
  entryPath: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  sendEvent(handlerId: number, payload: string): Promise<Uint8Array>;
  sendEventAsync(handlerId: number, payload: string): Promise<void>;
  pushIslandData(data: Uint8Array): Promise<void>;
  pollIslandData(): Promise<Uint8Array>;
  pollRender(): Promise<Uint8Array>;
  stop(): Promise<void>;
}

export interface GuiRunOutput {
  renderBytes: Uint8Array;
  moduleBytes: Uint8Array;
  entryPath: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  hostWidgetHandlerId: number | null;
}

export type ProcEvent =
  | { kind: 'stdout'; text: string }
  | { kind: 'stderr'; text: string }
  | { kind: 'done'; exitCode: number }
  | { kind: 'error'; message: string };

export interface HttpOpts {
  headers?: Record<string, string>;
  body?: string;
  timeoutMs?: number;
}

export interface HttpResult {
  status: number;
  headers: Record<string, string>;
  body: string;
}

export type GitOp =
  | { kind: 'git.status' }
  | { kind: 'git.add'; paths: string[] }
  | { kind: 'git.commit'; message: string }
  | { kind: 'git.push'; remote?: string; branch?: string }
  | { kind: 'git.pull'; remote?: string; branch?: string }
  | { kind: 'git.log'; limit?: number }
  | { kind: 'git.diff'; path?: string }
  | { kind: 'git.clone'; url: string; dest: string }
  | { kind: 'git.branch'; list?: boolean; create?: string; delete?: string };

export interface GitResult {
  ok: boolean;
  output: string;
}

export interface ReadManyResult {
  path: string;
  content: string | null;
  error: string | null;
}

export interface StreamHandle<T> {
  [Symbol.asyncIterator](): AsyncIterator<T>;
  cancel(): void;
}

export interface RendererBridgeVfsFile {
  path: string;
  bytes: Uint8Array;
}

export interface RendererBridgeVfsSnapshot {
  rootPath: string;
  files: RendererBridgeVfsFile[];
}
