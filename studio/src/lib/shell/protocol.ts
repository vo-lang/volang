// =============================================================================
// Shell Protocol — shared type definitions (NO implementation)
// Consumed by: ShellClient (frontend), TauriTransport, WasmShellRouter (backend)
// =============================================================================

// ── Op types ──────────────────────────────────────────────────────────────────

export type VoRunMode    = 'vm' | 'jit';
export type VoTestTarget = 'both' | 'vm' | 'jit' | 'gc' | 'nostd' | 'wasm';

export type FsOp =
  | { kind: 'fs.list';   path: string }
  | { kind: 'fs.stat';   path: string }
  | { kind: 'fs.read';   path: string }
  | { kind: 'fs.write';  path: string; content: string }
  | { kind: 'fs.mkdir';  path: string; recursive?: boolean }
  | { kind: 'fs.remove'; path: string; recursive?: boolean }
  | { kind: 'fs.rename'; oldPath: string; newPath: string }
  | { kind: 'fs.copy';   src: string; dst: string };

export type VoOp =
  | { kind: 'vo.run';     path: string; mode?: VoRunMode; args?: string[] }
  | { kind: 'vo.check';   path: string }
  | { kind: 'vo.build';   path: string; output?: string }
  | { kind: 'vo.test';    path?: string; target?: VoTestTarget; release?: boolean; verbose?: boolean; direct?: boolean }
  | { kind: 'vo.bench';   suite?: 'all' | 'vo' | 'score' }
  | { kind: 'vo.clean';   what?: 'all' | 'vo' | 'rust' }
  | { kind: 'vo.get';     module: string }
  | { kind: 'vo.dump';    path: string }
  | { kind: 'vo.compile'; path: string; output?: string }
  | { kind: 'vo.init' }
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

export interface FsStatResult {
  name:        string;
  path:        string;
  isDir:       boolean;
  size?:       number;
  modifiedMs?: number;
}

export type FsListResult = FsStatResult[];

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
