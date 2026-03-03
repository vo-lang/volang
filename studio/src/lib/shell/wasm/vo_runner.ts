import type { ShellRequest, ShellResponse, ShellEvent, ShellErrorCode } from '../protocol';

// =============================================================================
// WasmVoRunner — executes shell ops by running the same Vo handler programs
// that the Tauri backend uses, but via the studio WASM module.
//
// The studio WASM module exposes runShellHandler(args: string[]) which:
//   1. Compiles studio/vo/shell/ from embedded sources (including 3rdparty/git2,
//      3rdparty/zip, libs/vox — all embedded at build time via build.rs).
//   2. Injects args as os.Args in the Vo program.
//   3. Returns stdout, which is a JSON-encoded shell response.
//
// On WASM:
//   - fs ops use the JS VirtualFS (same as before via os package)
//   - git read ops use 3rdparty/git2 (WASM: no native lib → graceful error)
//   - git write/streaming ops use os/exec (WASM stub → ERR_NOT_SUPPORTED)
//   - zip ops use 3rdparty/zip with zip.wasm (WASM-capable)
//   - vo toolchain ops use libs/vox (WASM: no native compiler → graceful error)
//   - proc ops use os/exec (WASM stub → ERR_NOT_SUPPORTED)
// =============================================================================

export interface WasmShellModLike {
  runShellHandler(args: string[]): string;
}

export class WasmVoRunner {
  constructor(
    private readonly wasmMod: WasmShellModLike,
    private readonly workspaceRoot: string,
  ) {}

  async handle(req: ShellRequest, emit: (ev: ShellEvent) => void): Promise<ShellResponse> {
    const reqJson = JSON.stringify({
      id:  req.id,
      cwd: req.cwd,
      op:  req.op,
    });

    let stdout: string;
    try {
      stdout = this.wasmMod.runShellHandler(['wasm', reqJson, this.workspaceRoot]);
    } catch (e) {
      return {
        id:      req.id,
        kind:    'error',
        code:    'ERR_INTERNAL' as ShellErrorCode,
        message: `runShellHandler threw: ${e}`,
      };
    }

    if (stdout.startsWith('error:')) {
      return {
        id:      req.id,
        kind:    'error',
        code:    'ERR_INTERNAL' as ShellErrorCode,
        message: stdout.slice('error:'.length),
      };
    }

    let resp: Record<string, unknown>;
    try {
      resp = JSON.parse(stdout.trim());
    } catch (_) {
      return {
        id:      req.id,
        kind:    'error',
        code:    'ERR_INTERNAL' as ShellErrorCode,
        message: `shell handler returned non-JSON: ${stdout.slice(0, 200)}`,
      };
    }

    const kind = resp['kind'] as string;

    if (kind === 'ok') {
      return { id: req.id, kind: 'ok', data: resp['data'] };
    }

    if (kind === 'stream') {
      // Process spawning is not available in WASM — streaming ops gracefully error.
      return {
        id:      req.id,
        kind:    'error',
        code:    'ERR_NOT_SUPPORTED' as ShellErrorCode,
        message: 'streaming shell ops are not supported in the WASM sandbox',
      };
    }

    if (kind === 'error') {
      return {
        id:      req.id,
        kind:    'error',
        code:    (resp['code'] as ShellErrorCode) ?? 'ERR_INTERNAL',
        message: (resp['message'] as string) ?? 'unknown error',
      };
    }

    return {
      id:      req.id,
      kind:    'error',
      code:    'ERR_INTERNAL' as ShellErrorCode,
      message: `unexpected handler response kind: ${kind}`,
    };
  }
}
