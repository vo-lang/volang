import {
  type ShellOp,
  type ShellEvent,
  type Capability,
  ShellError,
  capabilityForOp,
} from './protocol';
import type { ShellTransport } from './transport';

// =============================================================================
// ShellClient — session context + typed exec interface
// Frontend code interacts exclusively through this class.
// =============================================================================

export class ShellClient {
  private _cwd:          string                         = '/';
  private _env:          Record<string, string>         = {};
  private _capabilities: Set<Capability>                = new Set();
  private _workspaceRoot: string                        = '/';

  constructor(private readonly transport: ShellTransport) {}

  async initialize(): Promise<void> {
    const info = await this.transport.initialize();
    this._workspaceRoot = info.workspaceRoot;
    this._capabilities  = info.capabilities;
    this._cwd           = info.workspaceRoot;
  }

  // ── Session state ────────────────────────────────────────────────────────

  get cwd(): string           { return this._cwd; }
  get workspaceRoot(): string { return this._workspaceRoot; }

  cd(path: string): void {
    this._cwd = path.startsWith('/') ? path : this._cwd + '/' + path;
  }

  setEnv(key: string, value: string): void {
    this._env = { ...this._env, [key]: value };
  }

  supports(cap: Capability): boolean {
    return this._capabilities.has(cap);
  }

  // ── One-shot execution ───────────────────────────────────────────────────

  async exec(op: ShellOp): Promise<unknown> {
    this.guardCapability(op);
    const resp = await this.transport.send(this.buildRequest(op));
    if (resp.kind === 'error') throw new ShellError(resp.code, resp.message);
    if (resp.kind === 'stream') throw new ShellError('ERR_INTERNAL', 'use shell.stream() for streaming ops');
    return resp.data;
  }

  // ── Streaming execution ──────────────────────────────────────────────────

  async *stream(op: ShellOp): AsyncGenerator<ShellEvent> {
    this.guardCapability(op);
    const resp = await this.transport.send(this.buildRequest(op));
    if (resp.kind === 'error') throw new ShellError(resp.code, resp.message);
    if (resp.kind === 'ok') return;

    const { jobId } = resp;

    type Waiter = (ev: ShellEvent) => void;
    const queue: ShellEvent[] = [];
    let waiter: Waiter | null = null;
    let closed = false;

    const unsub = this.transport.onEvent((ev) => {
      if (ev.jobId !== jobId) return;
      if (waiter) {
        const w = waiter;
        waiter = null;
        w(ev);
      } else {
        queue.push(ev);
      }
      if (ev.kind === 'done' || ev.kind === 'fail') closed = true;
    });

    try {
      while (true) {
        if (queue.length > 0) {
          const ev = queue.shift()!;
          yield ev;
          if (ev.kind === 'done' || ev.kind === 'fail') return;
          continue;
        }
        if (closed) return;
        const ev = await new Promise<ShellEvent>((resolve) => { waiter = resolve; });
        yield ev;
        if (ev.kind === 'done' || ev.kind === 'fail') return;
      }
    } finally {
      unsub();
    }
  }

  // ── Internals ────────────────────────────────────────────────────────────

  private guardCapability(op: ShellOp): void {
    const cap = capabilityForOp(op);
    if (!this._capabilities.has(cap)) {
      throw new ShellError('ERR_NOT_SUPPORTED', `op '${op.kind}' is not supported by this backend`);
    }
  }

  private buildRequest(op: ShellOp) {
    return {
      id:  crypto.randomUUID(),
      cwd: this._cwd,
      env: Object.keys(this._env).length > 0 ? this._env : undefined,
      op,
    };
  }
}
