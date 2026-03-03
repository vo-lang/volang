import type { ShellRequest, ShellResponse, ShellEvent, Capability } from './protocol';
import type { WasmShellRouter } from './wasm/router';

// =============================================================================
// Transport abstractions
// =============================================================================

export interface TransportInfo {
  workspaceRoot: string;
  capabilities:  Set<Capability>;
}

export interface ShellTransport {
  initialize(): Promise<TransportInfo>;
  send(req: ShellRequest): Promise<ShellResponse>;
  onEvent(handler: (event: ShellEvent) => void): () => void;
}

// =============================================================================
// TauriTransport — delegates to Rust via Tauri IPC
// =============================================================================

export class TauriTransport implements ShellTransport {
  private _invoke!: (cmd: string, args?: unknown) => Promise<unknown>;
  private _listen!: (event: string, handler: (ev: { payload: ShellEvent }) => void) => Promise<() => void>;

  async initialize(): Promise<TransportInfo> {
    const tauri = await import('@tauri-apps/api/core');
    const { listen } = await import('@tauri-apps/api/event');
    this._invoke = tauri.invoke as typeof this._invoke;
    this._listen = listen as typeof this._listen;
    return this._invoke('cmd_shell_init') as Promise<TransportInfo>;
  }

  send(req: ShellRequest): Promise<ShellResponse> {
    return this._invoke('cmd_shell_exec', { req }) as Promise<ShellResponse>;
  }

  onEvent(handler: (event: ShellEvent) => void): () => void {
    let unlisten: (() => void) | null = null;
    this._listen('shell-event', (ev) => handler(ev.payload)).then((fn) => {
      unlisten = fn;
    });
    return () => unlisten?.();
  }
}

// =============================================================================
// WasmTransport — delegates to TypeScript WasmShellRouter directly
// =============================================================================

export class WasmTransport implements ShellTransport {
  private eventHandlers = new Set<(ev: ShellEvent) => void>();

  constructor(private readonly router: WasmShellRouter) {}

  async initialize(): Promise<TransportInfo> {
    return {
      workspaceRoot: this.router.workspaceRoot,
      capabilities:  this.router.capabilities(),
    };
  }

  send(req: ShellRequest): Promise<ShellResponse> {
    return this.router.handle(req, (ev) => this.emit(ev));
  }

  onEvent(handler: (event: ShellEvent) => void): () => void {
    this.eventHandlers.add(handler);
    return () => this.eventHandlers.delete(handler);
  }

  private emit(ev: ShellEvent): void {
    this.eventHandlers.forEach((h) => h(ev));
  }
}
