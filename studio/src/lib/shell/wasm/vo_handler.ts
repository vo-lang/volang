import { type VoOp, ShellError } from '../protocol';
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

  private check(path: string, cwd: string): { ok: boolean; errors: string[] } {
    const abs = resolvePath(this.workspaceRoot, cwd, path);
    try {
      const result = this.wasmMod.compileRunEntry(abs);
      if (result instanceof Error) throw result;
      return { ok: true, errors: [] };
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      return { ok: false, errors: [msg] };
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
