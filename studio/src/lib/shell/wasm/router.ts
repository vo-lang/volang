import {
  type ShellRequest,
  type ShellResponse,
  type ShellEvent,
  type Capability,
  type VoOp,
  ShellError,
} from '../protocol';
import type { VfsLike } from './fs_handler';
import { WasmVoHandler, type WasmModLike } from './vo_handler';
import { WasmVoRunner, type WasmShellModLike } from './vo_runner';

// =============================================================================
// WasmShellRouter — top-level dispatcher for all shell ops in WASM env.
//
// Architecture:
//   - vo.*  → WasmVoHandler: uses compileRunEntry / the studio WASM module
//             directly for Vo toolchain ops (compile, run, check from VFS).
//   - fs.*, git.*, zip.*, proc.* → WasmVoRunner: executes the same Vo handler
//             programs used on Tauri (studio/vo/shell/) via runShellHandler.
//             This gives a unified Vo layer across both platforms.
//             On WASM: fs ops use the JS VirtualFS, git reads use 3rdparty/git2
//             (graceful error if unavailable), zip uses 3rdparty/zip (zip.wasm),
//             exec-dependent ops (git write, proc) return ERR_NOT_SUPPORTED.
// =============================================================================

export class WasmShellRouter {
  private readonly vo:     WasmVoHandler;
  private readonly runner: WasmVoRunner;

  readonly workspaceRoot: string;

  constructor(
    _vfs: VfsLike,
    wasmMod: WasmModLike & WasmShellModLike,
    workspaceRoot: string,
  ) {
    this.workspaceRoot = workspaceRoot;
    this.vo            = new WasmVoHandler(wasmMod, workspaceRoot, _vfs);
    this.runner        = new WasmVoRunner(wasmMod, workspaceRoot);
  }

  capabilities(): Set<Capability> {
    return new Set<Capability>([
      'fs',
      'git',
      'zip',
      'proc.spawn',
      'vo.run',
      'vo.check',
      'vo.build',
      'vo.dump',
      'vo.compile',
      'vo.init',
      'vo.clean',
      'vo.version',
    ]);
  }

  async handle(req: ShellRequest, emit: (ev: ShellEvent) => void): Promise<ShellResponse> {
    try {
      if (req.op.kind.startsWith('vo.')) {
        const data = await this.vo.handle(req.op as VoOp, req.cwd);
        return { id: req.id, kind: 'ok', data };
      }
      return this.runner.handle(req, emit);
    } catch (e) {
      if (e instanceof ShellError) {
        return { id: req.id, kind: 'error', code: e.code, message: e.message };
      }
      return { id: req.id, kind: 'error', code: 'ERR_INTERNAL', message: String(e) };
    }
  }
}
