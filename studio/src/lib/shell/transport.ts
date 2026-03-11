import type { ShellRequest, ShellResponse, ShellEvent, ShellErrorCode, Capability } from './protocol';
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
// WasmExecBackend — injected by bridge.ts so the transport can handle app.* and
// gui.* ops without owning WASM module references or timer state.
// =============================================================================

export interface WasmExecBackend {
  prepare(entryPath: string): Promise<void>;
  compileRun(entryPath: string): string;
  runGui(entryPath: string): Uint8Array;
  sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array>;
  stopGui(): void;
}

// =============================================================================
// Helpers — classify a raw error into a structured { code, message }
// =============================================================================

function studioErrorFromCatch(e: unknown): { code: ShellErrorCode; message: string } {
  // Tauri backend returns StudioError objects: { code, message }
  if (typeof e === 'object' && e !== null && 'code' in e && 'message' in e) {
    return { code: (e as any).code as ShellErrorCode, message: (e as any).message };
  }
  // WASM errors are plain strings — classify by content
  const msg = String(e);
  if (msg.includes('compile error'))  return { code: 'ERR_VO_COMPILE',  message: msg };
  if (msg.includes('No guest'))       return { code: 'ERR_VO_RUNTIME',  message: msg };
  if (msg.includes('not waiting'))    return { code: 'ERR_VO_RUNTIME',  message: msg };
  if (msg.includes('Runtime error'))  return { code: 'ERR_VO_RUNTIME',  message: msg };
  if (msg.includes('path escapes'))   return { code: 'ERR_ACCESS_DENIED', message: msg };
  return { code: 'ERR_INTERNAL', message: msg };
}

function errResp(id: string, e: unknown): ShellResponse {
  const { code, message } = studioErrorFromCatch(e);
  return { id, kind: 'error', code, message };
}

// =============================================================================
// TauriTransport — delegates to Rust via Tauri IPC
//
// app.* and gui.* ops are intercepted and routed to the dedicated Tauri commands
// (cmd_prepare_app, cmd_compile_run_app, cmd_run_gui, cmd_send_gui_event, cmd_stop_gui).
// All other ops go through cmd_shell_exec → Vo shell handler.
// =============================================================================

export class TauriTransport implements ShellTransport {
  private _invoke!: (cmd: string, args?: unknown) => Promise<unknown>;
  private _listen!: (event: string, handler: (ev: { payload: ShellEvent }) => void) => Promise<() => void>;

  async initialize(): Promise<TransportInfo> {
    const tauri = await import('@tauri-apps/api/core');
    const { listen } = await import('@tauri-apps/api/event');
    this._invoke = tauri.invoke as typeof this._invoke;
    this._listen = listen as typeof this._listen;
    const raw = await this._invoke('cmd_shell_init') as { workspaceRoot: string; capabilities: string[] };
    return {
      workspaceRoot: raw.workspaceRoot,
      capabilities:  new Set(raw.capabilities) as Set<Capability>,
    };
  }

  async send(req: ShellRequest): Promise<ShellResponse> {
    const kind = req.op.kind;

    // ── app.* ops → dedicated Tauri commands ────────────────────────────
    if (kind === 'app.prepare') {
      try {
        const op = req.op as { kind: string; path: string };
        await this._invoke('cmd_prepare_app', { entryPath: op.path });
        return { id: req.id, kind: 'ok', data: null };
      } catch (e) { return errResp(req.id, e); }
    }

    if (kind === 'app.compileRun') {
      try {
        const op = req.op as { kind: string; path: string };
        const stdout = await this._invoke('cmd_compile_run_app', { entryPath: op.path }) as string;
        return { id: req.id, kind: 'ok', data: { stdout } };
      } catch (e) { return errResp(req.id, e); }
    }

    // ── gui.* ops → dedicated Tauri commands ────────────────────────────

    if (kind === 'gui.run') {
      try {
        const op = req.op as { kind: string; path: string };
        const raw = await this._invoke('cmd_run_gui', { entryPath: op.path }) as number[];
        return { id: req.id, kind: 'ok', data: { renderBytes: new Uint8Array(raw) } };
      } catch (e) { return errResp(req.id, e); }
    }

    if (kind === 'gui.event') {
      try {
        const op = req.op as { kind: string; handlerId: number; payload: string };
        const raw = await this._invoke('cmd_send_gui_event', { handlerId: op.handlerId, payload: op.payload }) as number[];
        return { id: req.id, kind: 'ok', data: { renderBytes: new Uint8Array(raw) } };
      } catch (e) { return errResp(req.id, e); }
    }

    if (kind === 'gui.stop') {
      try {
        await this._invoke('cmd_stop_gui');
        return { id: req.id, kind: 'ok', data: null };
      } catch (e) { return errResp(req.id, e); }
    }

    // ── All other ops → Vo shell handler ────────────────────────────────
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
// WasmTransport — delegates to WasmShellRouter for shell ops, WasmExecBackend
// for app.* and gui.* ops.
// =============================================================================

export class WasmTransport implements ShellTransport {
  private eventHandlers = new Set<(ev: ShellEvent) => void>();

  constructor(
    private readonly router:     WasmShellRouter,
    private readonly execBackend: WasmExecBackend,
  ) {}

  async initialize(): Promise<TransportInfo> {
    return {
      workspaceRoot: this.router.workspaceRoot,
      capabilities:  this.router.capabilities(),
    };
  }

  async send(req: ShellRequest): Promise<ShellResponse> {
    const kind = req.op.kind;

    // ── app.* ops → WasmExecBackend ──────────────────────────────────────
    if (kind === 'app.prepare') {
      try {
        const op = req.op as { kind: string; path: string };
        await this.execBackend.prepare(op.path);
        return { id: req.id, kind: 'ok', data: null };
      } catch (e) { return errResp(req.id, e); }
    }

    if (kind === 'app.compileRun') {
      try {
        const op = req.op as { kind: string; path: string };
        const stdout = this.execBackend.compileRun(op.path);
        return { id: req.id, kind: 'ok', data: { stdout } };
      } catch (e) { return errResp(req.id, e); }
    }

    // ── gui.* ops → WasmExecBackend ──────────────────────────────────────

    if (kind === 'gui.run') {
      try {
        const op = req.op as { kind: string; path: string };
        const renderBytes = this.execBackend.runGui(op.path);
        return { id: req.id, kind: 'ok', data: { renderBytes } };
      } catch (e) { return errResp(req.id, e); }
    }

    if (kind === 'gui.event') {
      try {
        const op = req.op as { kind: string; handlerId: number; payload: string };
        const renderBytes = await this.execBackend.sendGuiEvent(op.handlerId, op.payload);
        return { id: req.id, kind: 'ok', data: { renderBytes } };
      } catch (e) { return errResp(req.id, e); }
    }

    if (kind === 'gui.stop') {
      try {
        this.execBackend.stopGui();
        return { id: req.id, kind: 'ok', data: null };
      } catch (e) { return errResp(req.id, e); }
    }

    // ── All other ops → Vo shell handler ────────────────────────────────
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
