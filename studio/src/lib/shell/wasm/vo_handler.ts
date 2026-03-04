import { type VoOp, type VoCheckResult, type VoCheckDiag, ShellError } from '../protocol';
import type { VfsLike } from './fs_handler';

// =============================================================================
// WasmVoHandler — delegates to the Rust WASM module for Vo toolchain ops
// =============================================================================

// Minimal interface covering the wasm module exports we actually call.
export interface WasmModLike {
  compileRunEntry(entryPath: string): string | Error;
}

export class WasmVoHandler {
  constructor(
    private readonly wasmMod: WasmModLike,
    private readonly workspaceRoot: string,
    private readonly vfs: VfsLike,
  ) {}

  handle(op: VoOp, cwd: string): Promise<unknown> {
    switch (op.kind) {
      case 'vo.run':     return Promise.resolve(this.run(op.path, cwd, (op as any).mode));
      case 'vo.check':   return Promise.resolve(this.check(op.path, cwd));
      case 'vo.dump':    return Promise.resolve(this.dump(op.path, cwd));
      case 'vo.build':   return Promise.resolve(this.build(op.path, (op as any).output, cwd));
      case 'vo.compile': return Promise.resolve(this.build(op.path, (op as any).output, cwd));
      case 'vo.init':    return Promise.resolve(this.init(cwd));
      case 'vo.clean':   return Promise.resolve(this.clean(cwd));
      case 'vo.version': return Promise.resolve(this.version());
      default:
        throw new ShellError('ERR_NOT_SUPPORTED', `vo op '${op.kind}' is not yet implemented in WASM`);
    }
  }

  private run(path: string, cwd: string, _mode?: string): { stdout: string } {
    const abs = resolvePath(this.workspaceRoot, cwd, path);
    let stdout: string;
    try {
      const result = this.wasmMod.compileRunEntry(abs);
      // wasm-bindgen may return an Error object on some builds instead of throwing
      if (result instanceof Error) throw result;
      stdout = result as string;
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      if (msg.includes('vogui_') || msg.includes('emitRenderBinary')) {
        throw new ShellError('ERR_NOT_SUPPORTED', 'GUI program — use the ▶ Run button to launch it');
      }
      if (msg.includes('compile') || msg.includes('parse') || msg.includes('type')) {
        throw new ShellError('ERR_VO_COMPILE', msg);
      }
      throw new ShellError('ERR_VO_RUNTIME', msg);
    }
    return { stdout };
  }

  private check(path: string, cwd: string): VoCheckResult {
    const abs = resolvePath(this.workspaceRoot, cwd, path);
    try {
      const result = this.wasmMod.compileRunEntry(abs);
      if (result instanceof Error) throw result;
      return { ok: true, diags: [] };
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      return { ok: false, diags: parseWasmCheckError(msg, this.workspaceRoot) };
    }
  }

  private dump(path: string, cwd: string): string {
    throw new ShellError(
      'ERR_NOT_SUPPORTED',
      'vo.dump is not available in WASM (no bytecode formatter exposed to JS)',
    );
  }

  private build(path: string, output: string | undefined, cwd: string): { output: string } {
    throw new ShellError(
      'ERR_NOT_SUPPORTED',
      'vo.build is not available in WASM (no binary bytecode serializer exposed to JS)',
    );
  }

  private init(cwd: string): { path: string } {
    const dir      = cwd || this.workspaceRoot;
    const mainFile = dir.endsWith('/') ? dir + 'main.vo' : dir + '/main.vo';
    const [_, readErr] = this.vfs.readFile(mainFile);
    if (readErr !== null) {
      const src = 'package main\n\nimport "fmt"\n\nfunc main() {\n\tfmt.Println("Hello, Vo!")\n}\n';
      const writeErr = this.vfs.writeFile(mainFile, new TextEncoder().encode(src), 0o644);
      if (writeErr) throw new ShellError('ERR_INTERNAL', writeErr);
    }
    return { path: dir };
  }

  private clean(_cwd: string): null {
    return null;
  }

  private version(): string {
    return '0.1.0';
  }
}

// ── Helpers ───────────────────────────────────────────────────────────────────

// Parse compiler error string (may contain multiple newline-separated lines)
// into structured VoCheckDiag[]. Format: "<file>:<line>:<col>: <message>"
function parseWasmCheckError(errMsg: string, workspaceRoot: string): VoCheckDiag[] {
  const diags: VoCheckDiag[] = [];
  for (const raw of errMsg.split('\n')) {
    const line = raw.trim();
    if (!line) continue;
    diags.push(parseDiagLine(line, workspaceRoot));
  }
  return diags;
}

function parseDiagLine(line: string, workspaceRoot: string): VoCheckDiag {
  const base: VoCheckDiag = { file: '', line: 0, col: 0, message: line, severity: 'error' };

  const c1 = line.indexOf(':');
  if (c1 <= 0) return base;
  const file = line.slice(0, c1);
  const rest1 = line.slice(c1 + 1);

  const c2 = rest1.indexOf(':');
  if (c2 < 0) return { ...base, file: normalizeFilePath(file, workspaceRoot), message: rest1.trim() };

  const lineNum = parseInt(rest1.slice(0, c2), 10);
  if (isNaN(lineNum)) return { ...base, file: normalizeFilePath(file, workspaceRoot), message: rest1.trim() };
  const rest2 = rest1.slice(c2 + 1);

  const c3 = rest2.indexOf(':');
  if (c3 < 0) return { ...base, file: normalizeFilePath(file, workspaceRoot), line: lineNum, message: rest2.trim() };

  const colNum = parseInt(rest2.slice(0, c3), 10);
  if (isNaN(colNum)) return { ...base, file: normalizeFilePath(file, workspaceRoot), line: lineNum, message: rest2.trim() };
  let msg = rest2.slice(c3 + 1).trim();

  let severity: 'error' | 'warning' = 'error';
  if (msg.startsWith('warning:')) { severity = 'warning'; msg = msg.slice('warning:'.length).trim(); }
  else if (msg.startsWith('error:'))  { msg = msg.slice('error:'.length).trim(); }

  return { file: normalizeFilePath(file, workspaceRoot), line: lineNum, col: colNum, message: msg, severity };
}

function normalizeFilePath(file: string, workspaceRoot: string): string {
  const prefix = workspaceRoot.endsWith('/') ? workspaceRoot : workspaceRoot + '/';
  return file.startsWith(prefix) ? file.slice(prefix.length) : file;
}

function resolvePath(workspaceRoot: string, cwd: string, path: string): string {
  if (path.startsWith('/')) return path;
  const base = cwd.endsWith('/') ? cwd : cwd + '/';
  const joined = base + path;
  const parts = joined.split('/');
  const stack: string[] = [];
  for (const p of parts) {
    if (p === '' || p === '.') continue;
    if (p === '..') { stack.pop(); continue; }
    stack.push(p);
  }
  return '/' + stack.join('/');
}
