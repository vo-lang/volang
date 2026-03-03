import { type GitOp, type ShellEvent, ShellError } from '../protocol';

// =============================================================================
// WasmGitHandler — Phase 2 placeholder
// Real git in WASM requires isomorphic-git + a custom HTTP plugin.
// One-shot read ops (status/log/diff) could be implemented; write ops require
// an HTTP remote or a bundled git engine. Marked ERR_NOT_SUPPORTED for now.
// =============================================================================

export class WasmGitHandler {
  handle(
    op:   GitOp,
    _cwd: string,
    _emit: (ev: ShellEvent) => void,
  ): Promise<unknown> {
    throw new ShellError(
      'ERR_NOT_SUPPORTED',
      `git op '${op.kind}' is not available in the WASM sandbox (Phase 3: isogit)`,
    );
  }
}
