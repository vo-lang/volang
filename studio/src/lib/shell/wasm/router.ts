import {
  type ShellRequest,
  type ShellResponse,
  type ShellEvent,
  type Capability,
} from '../protocol';
import { WasmVoRunner, type WasmShellModLike } from './vo_runner';

// =============================================================================
// WasmShellRouter — top-level dispatcher for all shell ops in WASM env.
//
// Architecture:
//   - ALL ops (fs.*, git.*, vo.*, zip.*, proc.*, http.*) → WasmVoRunner:
//     executes the same Vo handler programs used on Tauri (studio/vo/shell/)
//     via runShellHandler. This gives a unified Vo layer across both platforms.
//     libs/vox FFI is exposed to the WASM runtime so vo_ops.vo runs natively.
//
// WASM limitations:
//   - fs.*      : fully supported (JS VirtualFS)
//   - git.*     : ERR_NOT_SUPPORTED — libgit2 is a native C library; all
//                 nativeXxx externs are registered as graceful stubs in
//                 studio/wasm/src/lib.rs. Git sync in WASM uses the GitHub
//                 REST API integration (Home screen), not terminal git.
//   - zip.*     : ERR_NOT_SUPPORTED (no zip.wasm loaded in shell context)
//   - proc.spawn: ERR_NOT_SUPPORTED (os/exec unavailable in browser)
//   - http.*    : fully supported
//   - vo.*      : fully supported via libs/vox WASM FFI (vox_wasm_ffi.rs)
// =============================================================================

export class WasmShellRouter {
  private readonly runner: WasmVoRunner;

  readonly workspaceRoot: string;

  constructor(
    wasmMod: WasmShellModLike,
    workspaceRoot: string,
  ) {
    this.workspaceRoot = workspaceRoot;
    this.runner        = new WasmVoRunner(wasmMod, workspaceRoot);
  }

  capabilities(): Set<Capability> {
    return new Set<Capability>([
      'fs',
      'vo.run',
      'vo.check',
      'vo.build',
      'vo.dump',
      'vo.compile',
      'vo.init',
      'vo.get',
      'vo.clean',
      'vo.version',
      'gui',
    ]);
  }

  handle(req: ShellRequest, emit: (ev: ShellEvent) => void): Promise<ShellResponse> {
    return this.runner.handle(req, emit);
  }
}
