import { type ProcOp, type ShellEvent, ShellError } from '../protocol';

// =============================================================================
// WasmProcHandler — virtual process registry for WASM environment
// Real OS processes are unavailable; known tools are registered here.
// =============================================================================

export interface VirtualExecutable {
  run(
    args:  string[],
    cwd:   string,
    env:   Record<string, string>,
    emit:  (ev: ShellEvent) => void,
  ): Promise<number>;  // returns exit code
}

export class WasmProcHandler {
  private readonly registry = new Map<string, VirtualExecutable>();

  register(name: string, exe: VirtualExecutable): void {
    this.registry.set(name, exe);
  }

  unregister(name: string): void {
    this.registry.delete(name);
  }

  async handle(
    op:   ProcOp,
    cwd:  string,
    env:  Record<string, string>,
    emit: (ev: ShellEvent) => void,
  ): Promise<{ exitCode: number }> {
    const exe = this.registry.get(op.program);
    if (!exe) {
      throw new ShellError('ERR_TOOL_MISSING', `virtual program not registered: ${op.program}`);
    }
    const exitCode = await exe.run(op.args ?? [], cwd, { ...env, ...(op.env ?? {}) }, emit);
    return { exitCode };
  }
}
