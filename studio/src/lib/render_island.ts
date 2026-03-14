import { registerWidget } from '@vogui/index';
import type { WidgetFactory, WidgetInstance } from '@vogui/types';
import { initVFS, vfs } from '@vo-web/index';
import { get } from 'svelte/store';
import { consolePushLines, ide } from '../stores/ide';

interface StudioWasmRenderModule {
  default(input?: string | URL | Request | Response | BufferSource | WebAssembly.Module): Promise<void>;
  startRenderIsland(moduleBytes: Uint8Array): void;
  pushIslandData(data: Uint8Array): void;
  pollIslandData(): Uint8Array;
  pollPendingHostEvent(): unknown;
  wakeHostEvent(token: string): void;
  stopGui(): void;
}

type WidgetEventPayload = {
  type: string;
  width: number;
  height: number;
};

type RenderIslandVfsFile = {
  path: string;
  bytes: Uint8Array | number[];
};

type RenderIslandVfsSnapshot = {
  rootPath: string;
  files: RenderIslandVfsFile[];
};

let widgetRegistered = false;
let wasmModulePromise: Promise<StudioWasmRenderModule> | null = null;

function isTauriRuntime(): boolean {
  const w = window as any;
  return Boolean(w.__TAURI__ || w.__TAURI_INTERNALS__);
}

function toUint8Array(value: unknown): Uint8Array {
  if (value instanceof Uint8Array) {
    return value;
  }
  if (Array.isArray(value)) {
    return new Uint8Array(value);
  }
  if (value && typeof value === 'object' && 'length' in value) {
    return new Uint8Array(value as ArrayLike<number>);
  }
  return new Uint8Array();
}

function parentDir(path: string): string {
  const idx = path.lastIndexOf('/');
  if (idx <= 0) return '/';
  return path.slice(0, idx);
}

async function loadStudioWasmModule(): Promise<StudioWasmRenderModule> {
  if (!wasmModulePromise) {
    wasmModulePromise = (async () => {
      const wasmPkgUrl = new URL('../../wasm/pkg/vo_studio_wasm.js', import.meta.url).href;
      const wasmMod = await import(/* @vite-ignore */ wasmPkgUrl) as StudioWasmRenderModule;
      const wasmBinUrl = new URL('../../wasm/pkg/vo_studio_wasm_bg.wasm', import.meta.url).href;
      await wasmMod.default(wasmBinUrl);
      return wasmMod;
    })();
  }
  return wasmModulePromise;
}

class RenderIslandWidget implements WidgetInstance {
  private readonly container: HTMLElement;
  private readonly onEvent: (payload: string) => void;
  private readonly canvas: HTMLCanvasElement;
  private resizeObserver: ResizeObserver | null = null;
  private unlisten: (() => void) | null = null;
  private transportQueue: Promise<void> = Promise.resolve();
  private wasmMod: StudioWasmRenderModule | null = null;
  private invokeTransport: ((cmd: string, args?: Record<string, unknown>) => Promise<unknown>) | null = null;
  private started = false;
  private disposed = false;
  private failed = false;
  private props: Record<string, any>;
  private lastWidth = -1;
  private lastHeight = -1;
  private hostEventTimers = new Map<string, number>();

  constructor(container: HTMLElement, props: Record<string, any>, onEvent: (payload: string) => void) {
    this.container = container;
    this.props = props;
    this.onEvent = onEvent;
    this.canvas = document.createElement('canvas');
    this.canvas.style.width = '100%';
    this.canvas.style.height = '100%';
    this.canvas.style.display = 'block';
    this.canvas.style.touchAction = 'none';
    this.canvas.id = this.canvasId();
    this.container.replaceChildren(this.canvas);
    this.container.style.width = '100%';
    this.container.style.height = '100%';
    this.container.style.display = 'block';
    this.container.style.overflow = 'hidden';
    this.installResizeObserver();
    void this.start();
  }

  update(props: Record<string, any>): void {
    this.props = props;
    const nextCanvasId = this.canvasId();
    if (this.canvas.id !== nextCanvasId) {
      this.canvas.id = nextCanvasId;
    }
    const size = this.syncCanvasSize();
    if (this.started) {
      this.emitWidgetEvent({ type: 'resize', width: size.width, height: size.height });
    }
  }

  destroy(): void {
    if (this.disposed) {
      return;
    }
    this.disposed = true;
    this.resizeObserver?.disconnect();
    this.resizeObserver = null;
    this.unlisten?.();
    this.unlisten = null;
    for (const timerId of this.hostEventTimers.values()) {
      window.clearTimeout(timerId);
    }
    this.hostEventTimers.clear();
    try {
      this.wasmMod?.stopGui();
    } catch {
    }
    this.container.replaceChildren();
  }

  private canvasId(): string {
    return typeof this.props.canvasId === 'string' && this.props.canvasId.length > 0
      ? this.props.canvasId
      : 'canvas';
  }

  private currentModuleBytes(): Uint8Array {
    const bytes = get(ide).guestModuleBytes;
    if (!bytes || bytes.length === 0) {
      throw new Error('Studio render island module bytes are missing');
    }
    return bytes;
  }

  private currentEntryPath(): string {
    const entryPath = get(ide).guestEntryPath;
    if (!entryPath) {
      throw new Error('Studio render island entry path is missing');
    }
    return entryPath;
  }

  private async syncRenderIslandVfs(entryPath: string): Promise<void> {
    if (!this.invokeTransport) {
      throw new Error('Studio render island transport is not available');
    }
    await initVFS();
    const snapshot = await this.invokeTransport(
      'cmd_get_render_island_vfs_snapshot',
      { entryPath },
    ) as RenderIslandVfsSnapshot;
    const rootStat = vfs.stat(snapshot.rootPath);
    if (!rootStat[5]) {
      const removeErr = vfs.removeAll(snapshot.rootPath);
      if (removeErr) throw new Error(removeErr);
    }
    const rootErr = vfs.mkdirAll(snapshot.rootPath, 0o755);
    if (rootErr) throw new Error(rootErr);
    for (const file of snapshot.files) {
      const dirErr = vfs.mkdirAll(parentDir(file.path), 0o755);
      if (dirErr) throw new Error(dirErr);
      const writeErr = vfs.writeFile(file.path, toUint8Array(file.bytes), 0o644);
      if (writeErr) throw new Error(writeErr);
    }
  }

  private installResizeObserver(): void {
    this.resizeObserver?.disconnect();
    this.resizeObserver = new ResizeObserver(() => {
      const size = this.syncCanvasSize();
      if (this.started) {
        this.emitWidgetEvent({ type: 'resize', width: size.width, height: size.height });
      }
    });
    this.resizeObserver.observe(this.container);
    this.syncCanvasSize();
  }

  private syncCanvasSize(): { width: number; height: number } {
    const width = Math.max(1, Math.round(this.container.clientWidth));
    const height = Math.max(1, Math.round(this.container.clientHeight));
    const dpr = Math.max(1, window.devicePixelRatio || 1);
    const backingWidth = Math.max(1, Math.round(width * dpr));
    const backingHeight = Math.max(1, Math.round(height * dpr));

    if (this.canvas.width !== backingWidth) {
      this.canvas.width = backingWidth;
    }
    if (this.canvas.height !== backingHeight) {
      this.canvas.height = backingHeight;
    }

    return { width, height };
  }

  private emitWidgetEvent(payload: WidgetEventPayload): void {
    if (payload.width === this.lastWidth && payload.height === this.lastHeight && payload.type !== 'mount') {
      return;
    }
    this.lastWidth = payload.width;
    this.lastHeight = payload.height;
    this.onEvent(JSON.stringify(payload));
  }

  private enqueue(work: () => Promise<void> | void): void {
    this.transportQueue = this.transportQueue
      .then(async () => {
        if (this.disposed) {
          return;
        }
        await work();
      })
      .catch((error) => {
        this.reportError(error);
      });
  }

  private async start(): Promise<void> {
    try {
      if (!isTauriRuntime()) {
        return;
      }

      const moduleBytes = this.currentModuleBytes();
      const entryPath = this.currentEntryPath();
      const [{ listen }, { invoke }, wasmMod] = await Promise.all([
        import('@tauri-apps/api/event'),
        import('@tauri-apps/api/core'),
        loadStudioWasmModule(),
      ]);
      if (this.disposed) {
        return;
      }

      this.invokeTransport = invoke;
      this.wasmMod = wasmMod;
      await this.syncRenderIslandVfs(entryPath);
      const size = this.syncCanvasSize();

      const unlisten = await listen<Uint8Array | number[]>('island_data', (event) => {
        const frame = toUint8Array(event.payload);
        this.enqueue(async () => {
          if (!this.started || !this.wasmMod) {
            return;
          }
          this.wasmMod.pushIslandData(frame);
          await this.flushOutbound();
          this.scheduleHostEvents();
        });
      });
      if (this.disposed) {
        unlisten();
        return;
      }
      this.unlisten = unlisten;

      this.wasmMod.stopGui();
      this.wasmMod.startRenderIsland(moduleBytes.slice());
      this.started = true;
      await this.flushOutbound();
      this.scheduleHostEvents();
      this.emitWidgetEvent({ type: 'mount', width: size.width, height: size.height });
    } catch (error) {
      this.reportError(error);
    }
  }

  private async flushOutbound(): Promise<void> {
    if (!this.wasmMod || !this.invokeTransport) {
      return;
    }
    for (;;) {
      const frame = toUint8Array(this.wasmMod.pollIslandData());
      if (frame.length === 0) {
        return;
      }
      await this.invokeTransport('__island_transport_push', { data: Array.from(frame) });
    }
  }

  private scheduleHostEvents(): void {
    if (!this.wasmMod || this.disposed) {
      return;
    }
    for (;;) {
      const raw = this.wasmMod.pollPendingHostEvent() as { token?: unknown; delayMs?: unknown } | null;
      if (!raw || typeof raw !== 'object') {
        return;
      }
      const token = typeof raw.token === 'string' ? raw.token : '';
      const delayMs = Math.max(0, Number(raw.delayMs ?? 0));
      if (!token || this.hostEventTimers.has(token)) {
        continue;
      }
      const timerId = window.setTimeout(() => {
        this.hostEventTimers.delete(token);
        this.enqueue(async () => {
          if (!this.started || !this.wasmMod) {
            return;
          }
          this.wasmMod.wakeHostEvent(token);
          await this.flushOutbound();
          this.scheduleHostEvents();
        });
      }, delayMs);
      this.hostEventTimers.set(token, timerId);
    }
  }

  private reportError(error: unknown): void {
    if (this.failed) {
      return;
    }
    this.failed = true;
    const message = error instanceof Error ? error.message : String(error);
    consolePushLines('stderr', message);
    ide.update((state) => ({
      ...state,
      isGuiApp: false,
      guestRender: null,
      guestModuleBytes: null,
      guestEntryPath: null,
      isRunning: false,
      runStatus: 'error',
    }));
  }
}

const renderIslandWidgetFactory: WidgetFactory = {
  create(container: HTMLElement, props: Record<string, any>, onEvent: (payload: string) => void): WidgetInstance {
    return new RenderIslandWidget(container, props, onEvent);
  },
};

export function ensureRenderIslandWidgetRegistered(): void {
  if (widgetRegistered) {
    return;
  }
  widgetRegistered = true;
  registerWidget('voplay-render-island', renderIslandWidgetFactory);
}
