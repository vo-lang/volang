import { type ZipOp, ShellError } from '../protocol';
import type { VfsLike } from './fs_handler';

// =============================================================================
// WasmZipHandler — Phase 3 placeholder
//
// Full implementation requires a Vo archive library or `npm install fflate`.
// Zip ops are fully supported on the Tauri local backend via the Vo handler.
// =============================================================================

export class WasmZipHandler {
  // VfsLike kept for the future Phase 3 implementation.
  constructor(private readonly _vfs: VfsLike) {}

  handle(op: ZipOp, _cwd: string): Promise<unknown> {
    throw new ShellError(
      'ERR_NOT_SUPPORTED',
      `zip op '${op.kind}' is not available in the WASM sandbox (Phase 3)`,
    );
  }
}
