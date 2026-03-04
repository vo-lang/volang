// =============================================================================
// Shell Protocol — shared type definitions (NO implementation)
// Consumed by: ShellClient (frontend), TauriTransport, WasmShellRouter (backend)
// =============================================================================

// ── Op types ──────────────────────────────────────────────────────────────────

export type VoRunMode    = 'vm' | 'jit';
export type VoTestTarget = 'both' | 'vm' | 'jit' | 'gc' | 'nostd' | 'wasm';

export type FsOp =
  | { kind: 'fs.list';     path: string }
  | { kind: 'fs.stat';     path: string }
  | { kind: 'fs.read';     path: string }
  | { kind: 'fs.readMany'; paths: string[] }
  | { kind: 'fs.grep';     path: string; pattern: string; recursive?: boolean; caseInsensitive?: boolean; fixedString?: boolean }
  | { kind: 'fs.write';    path: string; content: string }
  | { kind: 'fs.mkdir';    path: string; recursive?: boolean }
  | { kind: 'fs.remove';   path: string; recursive?: boolean }
  | { kind: 'fs.rename';   oldPath: string; newPath: string }
  | { kind: 'fs.copy';     src: string; dst: string };

export type VoOp =
  | { kind: 'vo.run';     path: string; mode?: VoRunMode; args?: string[]; stdin?: string }
  | { kind: 'vo.check';   path: string }
  | { kind: 'vo.build';   path: string; output?: string }
  | { kind: 'vo.test';    path?: string; target?: VoTestTarget; release?: boolean; verbose?: boolean; direct?: boolean }
  | { kind: 'vo.bench';   suite?: 'all' | 'vo' | 'score' }
  | { kind: 'vo.clean';   what?: 'all' | 'vo' | 'rust' }
  | { kind: 'vo.get';     module: string }
  | { kind: 'vo.dump';    path: string }
  | { kind: 'vo.compile'; path: string; output?: string }
  | { kind: 'vo.init'; name?: string }
  | { kind: 'vo.version' };

export type GitOp =
  | { kind: 'git.init' }
  | { kind: 'git.status' }
  | { kind: 'git.add';      paths: string[] }
  | { kind: 'git.commit';   message: string; amend?: boolean }
  | { kind: 'git.push';     remote?: string; branch?: string; force?: boolean }
  | { kind: 'git.pull';     remote?: string; branch?: string }
  | { kind: 'git.clone';    url: string; destPath: string }
  | { kind: 'git.log';      limit?: number }
  | { kind: 'git.diff';     staged?: boolean }
  | { kind: 'git.checkout'; branch: string; create?: boolean }
  | { kind: 'git.branch';   create?: string; delete?: string };

export type ZipOp =
  | { kind: 'zip.pack';   inputs: string[]; output: string }
  | { kind: 'zip.unpack'; archive: string; outputDir: string }
  | { kind: 'zip.list';   archive: string };

export type ProcOp =
  | { kind: 'proc.spawn'; program: string; args?: string[]; env?: Record<string, string> };

export type HttpOp =
  | { kind: 'http.get';    url: string; headers?: Record<string, string>; timeoutMs?: number }
  | { kind: 'http.head';   url: string; headers?: Record<string, string>; timeoutMs?: number }
  | { kind: 'http.post';   url: string; headers?: Record<string, string>; body?: string; timeoutMs?: number }
  | { kind: 'http.put';    url: string; headers?: Record<string, string>; body?: string; timeoutMs?: number }
  | { kind: 'http.patch';  url: string; headers?: Record<string, string>; body?: string; timeoutMs?: number }
  | { kind: 'http.delete'; url: string; headers?: Record<string, string>; body?: string; timeoutMs?: number };

export type ShellOp = FsOp | VoOp | GitOp | ZipOp | ProcOp | HttpOp;

// ── Request ───────────────────────────────────────────────────────────────────

export interface ShellRequest {
  id:   string;                        // UUID — correlation key
  cwd:  string;                        // caller-maintained; backend is stateless
  env?: Record<string, string>;
  op:   ShellOp;
}

// ── Response ──────────────────────────────────────────────────────────────────

export type ShellErrorCode =
  | 'ERR_NOT_SUPPORTED'   // op not implemented on this backend
  | 'ERR_NOT_FOUND'
  | 'ERR_ACCESS_DENIED'
  | 'ERR_ALREADY_EXISTS'
  | 'ERR_VO_COMPILE'
  | 'ERR_VO_RUNTIME'
  | 'ERR_TOOL_MISSING'    // binary / virtual program not registered
  | 'ERR_INTERNAL';

export type ShellResponse =
  | { id: string; kind: 'ok';     data: unknown }
  | { id: string; kind: 'stream'; jobId: string }
  | { id: string; kind: 'error';  code: ShellErrorCode; message: string };

// ── Streaming events ──────────────────────────────────────────────────────────

export type ShellEvent =
  | { jobId: string; kind: 'stdout';   line: string }
  | { jobId: string; kind: 'stderr';   line: string }
  | { jobId: string; kind: 'progress'; percent: number }
  | { jobId: string; kind: 'done';     exitCode: number }
  | { jobId: string; kind: 'fail';     code: ShellErrorCode; message: string };

// ── Capabilities ──────────────────────────────────────────────────────────────

export type Capability =
  | 'fs'
  | 'vo.run' | 'vo.check' | 'vo.build' | 'vo.test' | 'vo.bench'
  | 'vo.clean' | 'vo.get' | 'vo.dump' | 'vo.compile' | 'vo.init' | 'vo.version'
  | 'git' | 'zip' | 'proc.spawn' | 'http';

// ── Result types for specific ops ─────────────────────────────────────────────

// fs
export interface FsStatResult {
  name:        string;
  path:        string;
  isDir:       boolean;
  size?:       number;
  modifiedMs?: number;
}
export type FsListResult     = FsStatResult[];
export type FsReadManyEntry  = { path: string; content: string } | { path: string; error: string };
export type FsReadManyResult = FsReadManyEntry[];
export interface FsGrepMatch {
  path: string;
  line: number;    // 1-indexed
  text: string;    // full line content
}
export type FsGrepResult = FsGrepMatch[];

// vo
export interface VoCheckDiag {
  file:     string;    // workspace-relative path
  line:     number;    // 1-indexed
  col:      number;    // 1-indexed
  message:  string;
  severity: 'error' | 'warning';
}
export interface VoCheckResult { ok: boolean; diags: VoCheckDiag[] }
export interface VoRunResult   { stdout: string }
export interface VoBuildResult { output: string }
export interface VoDumpResult  { bytecode: string }
export interface VoInitResult  { path: string }

// git
export interface GitLogEntry    { id: string; summary: string; authorName: string; timeUnix: number }
export interface GitStatusEntry { path: string; status: string }
export interface GitBranchEntry { name: string; isHead: boolean }

// zip
export interface ZipListEntry {
  name:           string;
  size:           number;
  compressedSize: number;
  isDir:          boolean;
}

// http
export interface HttpResult {
  statusCode: number;
  status:     string;
  headers:    string[];
  body:       string;
}

// ── Op → result type map (used by typed exec() overloads) ────────────────────

export interface OpResultMap {
  'fs.list':     FsListResult;
  'fs.stat':     FsStatResult;
  'fs.read':     string;
  'fs.readMany': FsReadManyResult;
  'fs.grep':     FsGrepResult;
  'fs.write':    null;
  'fs.mkdir':    null;
  'fs.remove':   null;
  'fs.rename':   null;
  'fs.copy':     null;
  'vo.run':      VoRunResult;
  'vo.check':    VoCheckResult;
  'vo.build':    VoBuildResult;
  'vo.compile':  VoBuildResult;
  'vo.dump':     VoDumpResult;
  'vo.init':     VoInitResult;
  'vo.clean':    null;
  'vo.version':  string;
  'vo.get':      VoRunResult;
  'vo.test':     null;
  'vo.bench':    null;
  'git.status':  GitStatusEntry[];
  'git.log':     GitLogEntry[];
  'git.branch':  GitBranchEntry[] | string;
  'git.add':     string;
  'git.commit':  string;
  'git.diff':    string;
  'git.checkout': string;
  'git.init':    string;
  'git.push':    null;
  'git.pull':    null;
  'git.clone':   null;
  'zip.list':    ZipListEntry[];
  'zip.pack':    null;
  'zip.unpack':  null;
  'proc.spawn':  null;
  'http.get':    HttpResult;
  'http.head':   HttpResult;
  'http.post':   HttpResult;
  'http.put':    HttpResult;
  'http.patch':  HttpResult;
  'http.delete': HttpResult;
}

// ── Error class ───────────────────────────────────────────────────────────────

export class ShellError extends Error {
  constructor(
    public readonly code: ShellErrorCode,
    message: string,
  ) {
    super(message);
    this.name = 'ShellError';
  }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

export function capabilityForOp(op: ShellOp): Capability {
  const { kind } = op;
  if (kind.startsWith('fs.'))   return 'fs';
  if (kind.startsWith('git.'))  return 'git';
  if (kind.startsWith('zip.'))  return 'zip';
  if (kind.startsWith('http.')) return 'http';
  if (kind === 'proc.spawn')    return 'proc.spawn';
  return kind as Capability;
}
