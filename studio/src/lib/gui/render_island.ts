// Render island management for Studio GUI runtime.
// Studio is framework-neutral: renderer modules are loaded dynamically
// from the VFS snapshot using a blob URL — no framework-specific imports.

import { invoke as tauriInvoke, listen as tauriListen } from '../tauri';
import type { Backend } from '../backend/backend';
import { isGuiSessionSupersededError, type RuntimeService } from '../services/runtime_service';
import type { FrameworkContract } from '../types';
import type { VoWebModule } from '../studio_wasm';
import { loadStudioWasm, makeVoWebModule } from '../studio_wasm';

async function logNativeDebug(message: string): Promise<void> {
  try {
    await tauriInvoke('cmd_debug_log', { message });
  } catch {}
}

// ---- StudioGuiHost ----
// Passed to renderer module's init(); renderer uses it to register widgets,
// access the canvas, and dispatch events back to the guest VM.

export interface WidgetFactory {
  create(
    container: HTMLElement,
    props: Record<string, unknown>,
    onEvent: (payload: string) => void,
  ): { update(props: Record<string, unknown>): void; destroy(): void };
}

export interface StudioIslandChannel {
  init(): Promise<void>;
  send(frame: Uint8Array): void;
  onReceive(handler: (frame: Uint8Array) => void): void;
  close(): void;
}

export interface StudioGuiHost {
  registerWidget(name: string, factory: WidgetFactory): void;
  getCanvas(): HTMLCanvasElement | null;
  sendEvent(handlerId: number, payload: string): Promise<Uint8Array>;
  createIslandChannel(): Promise<StudioIslandChannel>;
  debugLog(message: string): void;
  reportError(message: string): void;
  // Generic VM capability surface: create VMs + preload ext WASM modules.
  voWeb: VoWebModule;
  // Bytecode of the Vo module running on the logic island.
  moduleBytes: Uint8Array;
  // Look up a file by path from the VFS snapshot; returns null if not found.
  getVfsBytes(path: string): Uint8Array | null;
}

// ---- RendererModule contract ----
// Frameworks must export an object implementing this interface from their
// renderer JS entry (declared in vo.ext.toml [studio] renderer = "...").

export interface RendererModule {
  init(host: StudioGuiHost): Promise<void>;
  render(container: HTMLElement, bytes: Uint8Array): void;
  stop(): void;
}

export type RenderIslandHostContext = {
  moduleBytes: Uint8Array;
  entryPath: string;
  framework: FrameworkContract | null;
  onError?: (message: string) => void;
};

type ActiveRenderIsland = {
  renderer: RendererModule;
  blobUrls: string[];
  sessionId: number;
} | null;

type RendererBlobGraph = {
  entryUrl: string;
  urls: string[];
};
let activeRenderIsland: ActiveRenderIsland = null;
const widgetRegistry = new Map<string, WidgetFactory>();

// Whether the render island WASM VM is currently alive.
// Used by PreviewPanel to avoid re-launching during layout transitions.
export function isRenderIslandActive(sessionId?: number | null): boolean {
  if (sessionId == null) {
    return activeRenderIsland !== null;
  }
  return activeRenderIsland?.sessionId === sessionId;
}

function revokeBlobUrls(urls: string[]): void {
  for (const url of urls) {
    URL.revokeObjectURL(url);
  }
}

type VfsFile = { path: string; bytes: Uint8Array };

function normalizeVfsPath(path: string): string {
  const normalized = path.replace(/\\/g, '/');
  const absolute = normalized.startsWith('/');
  const out: string[] = [];
  for (const part of normalized.split('/')) {
    if (!part || part === '.') continue;
    if (part === '..') {
      if (out.length > 0 && out[out.length - 1] !== '..') {
        out.pop();
      } else if (!absolute) {
        out.push(part);
      }
      continue;
    }
    out.push(part);
  }
  if (!absolute) {
    return out.join('/');
  }
  return '/' + out.join('/');
}

function dirnameVfsPath(path: string): string {
  const normalized = normalizeVfsPath(path);
  const idx = normalized.lastIndexOf('/');
  if (idx < 0) return '.';
  if (idx === 0) return '/';
  return normalized.slice(0, idx);
}

function resolveModuleFile(
  importerPath: string,
  specifier: string,
  fileMap: Map<string, VfsFile>,
): VfsFile {
  const baseDir = dirnameVfsPath(importerPath);
  const rawPath = specifier.startsWith('/')
    ? specifier
    : `${baseDir}/${specifier}`;
  const candidates = [normalizeVfsPath(rawPath)];
  if (!/\.[a-z0-9]+$/i.test(candidates[0])) {
    candidates.push(`${candidates[0]}.js`);
    candidates.push(`${candidates[0]}/index.js`);
  }
  for (const candidate of candidates) {
    const file = fileMap.get(candidate);
    if (file) {
      return file;
    }
  }
  throw new Error(`Renderer module import not found: ${specifier} from ${importerPath}`);
}

function buildRendererBlobGraph(entryFile: VfsFile, files: VfsFile[]): RendererBlobGraph {
  const textDecoder = new TextDecoder();
  const fileMap = new Map<string, VfsFile>();
  for (const file of files) {
    fileMap.set(normalizeVfsPath(file.path), file);
  }

  const urls: string[] = [];
  const urlByPath = new Map<string, string>();
  const importFromRe = /(from\s*["'])(\.{1,2}\/[^"']+)(["'])/g;
  const importBareRe = /(\bimport\s*["'])(\.{1,2}\/[^"']+)(["'])/g;

  const materialize = (file: VfsFile): string => {
    const normalizedPath = normalizeVfsPath(file.path);
    const existing = urlByPath.get(normalizedPath);
    if (existing) {
      return existing;
    }
    let source = textDecoder.decode(file.bytes);
    const rewriteSpecifier = (_full: string, prefix: string, specifier: string, suffix: string) => {
      const depFile = resolveModuleFile(normalizedPath, specifier, fileMap);
      const depUrl = materialize(depFile);
      return `${prefix}${depUrl}${suffix}`;
    };
    source = source.replace(importFromRe, rewriteSpecifier);
    source = source.replace(importBareRe, rewriteSpecifier);
    const url = URL.createObjectURL(new Blob([source], { type: 'application/javascript' }));
    urlByPath.set(normalizedPath, url);
    urls.push(url);
    return url;
  };

  return {
    entryUrl: materialize(entryFile),
    urls,
  };
}

function makeStudioGuiHost(
  canvasId: string,
  backend: Backend,
  runtime: RuntimeService,
  voWeb: VoWebModule,
  moduleBytes: Uint8Array,
  vfsFiles: VfsFile[],
  onError?: (message: string) => void,
): StudioGuiHost {
  return {
    registerWidget(name: string, factory: WidgetFactory): void {
      widgetRegistry.set(name, factory);
    },
    getCanvas(): HTMLCanvasElement | null {
      return document.getElementById(canvasId) as HTMLCanvasElement | null;
    },
    async sendEvent(handlerId: number, payload: string): Promise<Uint8Array> {
      return runtime.sendGuiEvent(handlerId, payload);
    },
    async createIslandChannel(): Promise<StudioIslandChannel> {
      if (backend.platform !== 'native') {
        throw new Error('External island transport is only available on native backend');
      }
      let handler: ((frame: Uint8Array) => void) | null = null;
      let unlisten: (() => void) | null = null;
      return {
        async init(): Promise<void> {
          void logNativeDebug('render island channel init');
          unlisten = await tauriListen<number[]>('island_data', (event) => {
            const frame = new Uint8Array(event.payload);
            handler?.(frame);
          });
        },
        send(frame: Uint8Array): void {
          runtime.pushIslandTransport(frame).catch((error) => {
            if (isGuiSessionSupersededError(error)) {
              return;
            }
            console.error('[RenderIsland] island transport push failed:', error);
          });
        },
        onReceive(nextHandler: (frame: Uint8Array) => void): void {
          handler = nextHandler;
        },
        close(): void {
          void logNativeDebug('render island channel close');
          unlisten?.();
          unlisten = null;
          handler = null;
        },
      };
    },
    debugLog(message: string): void {
      void logNativeDebug(message);
    },
    reportError(message: string): void {
      console.error('[RenderIsland]', message);
      void logNativeDebug(`render island error: ${message}`);
      onError?.(message);
    },
    voWeb,
    moduleBytes,
    getVfsBytes(path: string): Uint8Array | null {
      const f = vfsFiles.find((x) => x.path === path || x.path.endsWith('/' + path));
      return f ? f.bytes : null;
    },
  };
}

// Load renderer module + full VFS snapshot (framework-neutral).
// Returns [renderer, all snapshot files] so the host can serve getVfsBytes.
async function loadRendererAndSnapshot(
  rendererPath: string,
  backend: Backend,
  entryPath: string,
): Promise<[RendererModule, VfsFile[], string[]]> {
  const snapshot = await backend.getRenderIslandVfsSnapshot(entryPath);
  const files: VfsFile[] = snapshot.files;

  const rendererFile = files.find((f) =>
    f.path.endsWith(rendererPath) || f.path === rendererPath,
  );
  if (!rendererFile) {
    throw new Error(`Renderer file not found in VFS snapshot: ${rendererPath}`);
  }
  const blobGraph = buildRendererBlobGraph(rendererFile, files);

  const mod = await import(/* @vite-ignore */ blobGraph.entryUrl) as {
    default?: RendererModule;
    init?: RendererModule['init'];
    render?: RendererModule['render'];
    stop?: RendererModule['stop'];
  };

  const renderer: RendererModule = mod.default ?? {
    init: mod.init ?? (() => Promise.resolve()),
    render: mod.render ?? (() => {}),
    stop: mod.stop ?? (() => {}),
  };

  return [renderer, files, blobGraph.urls];
}

export function getWidgetFactory(name: string): WidgetFactory | undefined {
  return widgetRegistry.get(name);
}

// Start render island with the framework's renderer loaded from VFS.
// Loads studio WASM, builds VoWebModule, and passes everything to renderer.init.
export async function startRenderIsland(
  canvasId: string,
  backend: Backend,
  runtime: RuntimeService,
  sessionId: number,
  context: RenderIslandHostContext,
): Promise<void> {
  stopRenderIsland();
  if (!context.framework) {
    throw new Error('No framework contract available');
  }

  const { rendererPath } = context.framework;
  if (!rendererPath) {
    throw new Error('Framework does not declare a renderer path');
  }

  // Load studio WASM to obtain the VoWebModule capability surface.
  (globalThis as Record<string, unknown>).__voStudioLogRecord = (record: {
    source?: string;
    code?: string;
    text?: string;
  }) => {
    const source = record.source ?? 'studio-wasm';
    const code = record.code ?? 'message';
    const suffix = record.text ? ` ${record.text}` : '';
    void logNativeDebug(`[${source}:${code}]${suffix}`);
  };
  const wasm = await loadStudioWasm();
  const voWeb = makeVoWebModule(wasm);

  const [renderer, vfsFiles, blobUrls] = await loadRendererAndSnapshot(rendererPath, backend, context.entryPath);
  const host = makeStudioGuiHost(canvasId, backend, runtime, voWeb, context.moduleBytes, vfsFiles, context.onError);
  try {
    await renderer.init(host);
    activeRenderIsland = { renderer, blobUrls, sessionId };
  } catch (error) {
    revokeBlobUrls(blobUrls);
    throw error;
  }
}

// Stop the active render island
export function stopRenderIsland(sessionId?: number | null): boolean {
  const active = activeRenderIsland;
  if (!active) return false;
  if (sessionId != null && active.sessionId !== sessionId) {
    return false;
  }
  activeRenderIsland = null;
  try {
    active.renderer.stop();
  } finally {
    revokeBlobUrls(active.blobUrls);
  }
  return true;
}

// Deliver render bytes to the active renderer
export function deliverRenderBytes(container: HTMLElement, bytes: Uint8Array): void {
  const renderer = activeRenderIsland?.renderer;
  if (!renderer || bytes.length === 0) return;
  renderer.render(container, bytes);
}
