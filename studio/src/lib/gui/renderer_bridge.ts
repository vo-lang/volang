// Renderer bridge for Studio GUI runtime.
// Studio is framework-neutral: renderer modules are loaded dynamically
// from the VFS snapshot using a blob URL — no framework-specific imports.

import { listen as tauriListen } from '../tauri';
import type { Backend } from '../backend/backend';
import { isGuiSessionSupersededError, type RuntimeService } from '../services/runtime_service';
import type { FrameworkContract } from '../types';
import type { VoWebModule } from '../studio_wasm';
import { loadStudioWasm, makeVoWebModule } from '../studio_wasm';

// ---- Protocol & HostBridge module contracts ----

export interface ProtocolModule {
  findExternalWidgetHandlerId(bytes: Uint8Array): number | null;
}

export interface HostBridgeContext {
  readString(ptr: number, len: number): string;
  alloc(size: number): number;
  writeBytes(destPtr: number, bytes: Uint8Array): void;
  writeU32(ptr: number, value: number): void;
}

export interface HostBridgeModule {
  buildImports(ctx: HostBridgeContext): Record<string, (...args: number[]) => number | void>;
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

// ---- RendererHost ----
// Lean interface passed to renderer module's init(). Core methods are always
// available; optional capabilities are lazy and only materialized on access.

export interface CanvasCapability {
  getCanvas(): HTMLCanvasElement | null;
}

export interface IslandTransportCapability {
  createChannel(): Promise<StudioIslandChannel>;
}

export interface VoWebCapability {
  getVoWeb(): Promise<VoWebModule>;
}

export interface VfsCapability {
  getBytes(path: string): Uint8Array | null;
}

export interface WidgetCapability {
  register(name: string, factory: WidgetFactory): void;
}

export interface CapabilityMap {
  canvas: CanvasCapability;
  island_transport: IslandTransportCapability;
  vo_web: VoWebCapability;
  vfs: VfsCapability;
  widget: WidgetCapability;
}

export interface RendererHost {
  sendEvent(handlerId: number, payload: string): Promise<Uint8Array>;
  log(message: string): void;
  reportError(message: string): void;
  getCapability<K extends keyof CapabilityMap>(name: K): CapabilityMap[K] | null;
}

// Legacy alias — existing renderer modules that typed against StudioGuiHost
// continue to work because RendererHost is a structural superset of what they
// actually use (sendEvent + debugLog).
export type StudioGuiHost = RendererHost & {
  registerWidget(name: string, factory: WidgetFactory): void;
  getCanvas(): HTMLCanvasElement | null;
  debugLog(message: string): void;
  voWeb: VoWebModule;
  moduleBytes: Uint8Array;
  getVfsBytes(path: string): Uint8Array | null;
  createIslandChannel(): Promise<StudioIslandChannel>;
};

// ---- RendererModule contract ----
// Frameworks must export an object implementing this interface from their
// renderer JS entry (declared in vo.ext.toml [studio] renderer = "...").

export interface RendererModule {
  init(host: RendererHost): Promise<void>;
  render(container: HTMLElement, bytes: Uint8Array): void;
  stop(): void;
}

export type RendererBridgeContext = {
  moduleBytes: Uint8Array;
  entryPath: string;
  framework: FrameworkContract | null;
  onError?: (message: string) => void;
};

type ActiveRendererBridge = {
  renderer: RendererModule;
  blobUrls: string[];
  sessionId: number;
} | null;

type RendererBlobGraph = {
  entryUrl: string;
  urls: string[];
};
let activeRendererBridge: ActiveRendererBridge = null;
const widgetRegistry = new Map<string, WidgetFactory>();

// Whether the renderer bridge is currently alive.
// Used by PreviewPanel to avoid re-launching during layout transitions.
export function isRendererBridgeActive(sessionId?: number | null): boolean {
  if (sessionId == null) {
    return activeRendererBridge !== null;
  }
  return activeRendererBridge?.sessionId === sessionId;
}

function revokeBlobUrls(urls: string[]): void {
  for (const url of urls) {
    URL.revokeObjectURL(url);
  }
}

export type VfsFile = { path: string; bytes: Uint8Array };

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

function makeRendererHost(
  canvasId: string,
  backend: Backend,
  runtime: RuntimeService,
  moduleBytes: Uint8Array,
  vfsFiles: VfsFile[],
  declaredCapabilities: string[],
  onError?: (message: string) => void,
): RendererHost {
  // Lazy VoWebModule — only loaded when a renderer actually requests it.
  let voWebPromise: Promise<VoWebModule> | null = null;
  function getVoWebLazy(): Promise<VoWebModule> {
    if (!voWebPromise) {
      voWebPromise = (async () => {
        (globalThis as Record<string, unknown>).__voStudioLogRecord = (_record: unknown) => {};
        const wasm = await loadStudioWasm();
        return makeVoWebModule(wasm);
      })();
    }
    return voWebPromise;
  }

  const capSet = new Set(declaredCapabilities);

  // Build capability map — only capabilities declared by the framework are available.
  const capabilities: Partial<CapabilityMap> = {};

  // Canvas is always available (cheap, no lazy loading needed).
  capabilities.canvas = {
    getCanvas(): HTMLCanvasElement | null {
      return document.getElementById(canvasId) as HTMLCanvasElement | null;
    },
  };

  if (capSet.has('island_transport')) {
    capabilities.island_transport = {
      async createChannel(): Promise<StudioIslandChannel> {
        if (backend.platform !== 'native') {
          throw new Error('External island transport is only available on native backend');
        }
        let handler: ((frame: Uint8Array) => void) | null = null;
        let unlisten: (() => void) | null = null;
        return {
          async init(): Promise<void> {
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
              console.error('[RendererBridge] island transport push failed:', error);
            });
          },
          onReceive(nextHandler: (frame: Uint8Array) => void): void {
            handler = nextHandler;
          },
          close(): void {
            unlisten?.();
            unlisten = null;
            handler = null;
          },
        };
      },
    };
  }

  if (capSet.has('vo_web')) {
    capabilities.vo_web = {
      getVoWeb: getVoWebLazy,
    };
  }

  capabilities.vfs = {
    getBytes(path: string): Uint8Array | null {
      const f = vfsFiles.find((x) => x.path === path || x.path.endsWith('/' + path));
      return f ? f.bytes : null;
    },
  };

  capabilities.widget = {
    register(name: string, factory: WidgetFactory): void {
      widgetRegistry.set(name, factory);
    },
  };

  return {
    async sendEvent(handlerId: number, payload: string): Promise<Uint8Array> {
      return runtime.sendGuiEvent(handlerId, payload);
    },
    log(_message: string): void {},
    reportError(message: string): void {
      console.error('[RendererBridge]', message);
      onError?.(message);
    },
    getCapability<K extends keyof CapabilityMap>(name: K): CapabilityMap[K] | null {
      return (capabilities[name] as CapabilityMap[K]) ?? null;
    },
  };
}

// Fetch VFS snapshot files once so multiple loaders can share the result.
export async function fetchVfsSnapshot(
  backend: Backend,
  entryPath: string,
): Promise<VfsFile[]> {
  const snapshot = await backend.getRendererBridgeVfsSnapshot(entryPath);
  return snapshot.files;
}

// Returns true if a VFS file path matches a given search path.
// Handles two cases:
//   - Web mode: f.path is an absolute VFS path, searchPath is a short relative suffix.
//   - Native mode: searchPath is an absolute filesystem path, f.path is a relative path.
function matchesVfsPath(filePath: string, searchPath: string): boolean {
  if (filePath === searchPath) return true;
  if (filePath.endsWith(searchPath)) return true;
  if (searchPath.endsWith('/' + filePath)) return true;
  return false;
}

// Load renderer module + full VFS snapshot (framework-neutral).
// Returns [renderer, all snapshot files] so the host can serve getVfsBytes.
async function loadRendererAndSnapshot(
  rendererPath: string,
  backend: Backend,
  entryPath: string,
  prefetchedFiles?: VfsFile[],
): Promise<[RendererModule, VfsFile[], string[]]> {
  const files: VfsFile[] = prefetchedFiles ?? await fetchVfsSnapshot(backend, entryPath);
  const { module: renderer, blobUrls } = await loadVfsModule<RendererModule>(rendererPath, files, (raw) =>
    raw.default as RendererModule ?? {
      init: (raw.init as RendererModule['init']) ?? (() => Promise.resolve()),
      render: (raw.render as RendererModule['render']) ?? (() => {}),
      stop: (raw.stop as RendererModule['stop']) ?? (() => {}),
    },
  );
  return [renderer, files, blobUrls];
}

export function getWidgetFactory(name: string): WidgetFactory | undefined {
  return widgetRegistry.get(name);
}

// Start renderer bridge with the framework's renderer loaded from VFS.
// No unconditional WASM loading — voWeb is a lazy capability inside RendererHost.
export async function startRendererBridge(
  canvasId: string,
  backend: Backend,
  runtime: RuntimeService,
  sessionId: number,
  context: RendererBridgeContext,
  vfsFiles?: VfsFile[],
): Promise<void> {
  stopRendererBridge();
  if (!context.framework) {
    throw new Error('No framework contract available');
  }

  const { rendererPath } = context.framework;
  if (!rendererPath) {
    throw new Error('Framework does not declare a renderer path');
  }

  const [renderer, resolvedVfsFiles, blobUrls] = await loadRendererAndSnapshot(rendererPath, backend, context.entryPath, vfsFiles);
  const host = makeRendererHost(
    canvasId, backend, runtime,
    context.moduleBytes, resolvedVfsFiles,
    context.framework.capabilities ?? [],
    context.onError,
  );
  try {
    await renderer.init(host);
    activeRendererBridge = { renderer, blobUrls, sessionId };
  } catch (error) {
    revokeBlobUrls(blobUrls);
    throw error;
  }
}

// Stop the active renderer bridge
export function stopRendererBridge(sessionId?: number | null): boolean {
  const active = activeRendererBridge;
  if (!active) return false;
  if (sessionId != null && active.sessionId !== sessionId) {
    return false;
  }
  activeRendererBridge = null;
  try {
    active.renderer.stop();
  } finally {
    revokeBlobUrls(active.blobUrls);
  }
  return true;
}

// Deliver render bytes to the active renderer
export function deliverRenderBytes(container: HTMLElement, bytes: Uint8Array): void {
  const renderer = activeRendererBridge?.renderer;
  if (!renderer || bytes.length === 0) return;
  renderer.render(container, bytes);
}

// ---- Generic VFS module loader ----

type VfsModuleResult<T> = { module: T; blobUrls: string[] };

async function loadVfsModule<T>(
  modulePath: string,
  files: VfsFile[],
  unwrap: (raw: Record<string, unknown>) => T,
): Promise<VfsModuleResult<T>> {
  const file = files.find((f) => matchesVfsPath(f.path, modulePath));
  if (!file) {
    throw new Error(`Module not found in VFS snapshot: ${modulePath}`);
  }
  const blobGraph = buildRendererBlobGraph(file, files);
  const raw = await import(/* @vite-ignore */ blobGraph.entryUrl);
  return { module: unwrap(raw), blobUrls: blobGraph.urls };
}

// ---- Cached module slot helpers ----

type CachedModule<T> = { key: string; module: T; blobUrls: string[] } | null;

function moduleCacheKey(entryPath: string, modulePath: string): string {
  return `${entryPath}\0${modulePath}`;
}

function evictCache<T>(slot: CachedModule<T>): void {
  if (slot) revokeBlobUrls(slot.blobUrls);
}

// ---- Protocol module loader ----

let activeProtocol: CachedModule<ProtocolModule> = null;

export async function loadProtocolModule(
  protocolPath: string,
  backend: Backend,
  entryPath: string,
  prefetchedFiles?: VfsFile[],
): Promise<ProtocolModule> {
  const key = moduleCacheKey(entryPath, protocolPath);
  if (activeProtocol?.key === key) return activeProtocol.module;
  evictCache(activeProtocol);

  const files: VfsFile[] = prefetchedFiles ?? await fetchVfsSnapshot(backend, entryPath);
  const { module, blobUrls } = await loadVfsModule<ProtocolModule>(protocolPath, files, (raw) =>
    raw.default as ProtocolModule ?? {
      findExternalWidgetHandlerId: (raw.findExternalWidgetHandlerId as ProtocolModule['findExternalWidgetHandlerId']) ?? (() => null),
    },
  );
  activeProtocol = { key, module, blobUrls };
  return module;
}

export function unloadProtocolModule(): void {
  if (!activeProtocol) return;
  revokeBlobUrls(activeProtocol.blobUrls);
  activeProtocol = null;
}

// ---- Host bridge module loader ----

let activeHostBridge: CachedModule<HostBridgeModule> = null;

export async function loadHostBridgeModule(
  hostBridgePath: string,
  backend: Backend,
  entryPath: string,
  prefetchedFiles?: VfsFile[],
): Promise<HostBridgeModule> {
  const key = moduleCacheKey(entryPath, hostBridgePath);
  if (activeHostBridge?.key === key) return activeHostBridge.module;
  evictCache(activeHostBridge);

  const files: VfsFile[] = prefetchedFiles ?? await fetchVfsSnapshot(backend, entryPath);
  const { module, blobUrls } = await loadVfsModule<HostBridgeModule>(hostBridgePath, files, (raw) =>
    raw.default as HostBridgeModule ?? {
      buildImports: (raw.buildImports as HostBridgeModule['buildImports']) ?? (() => ({})),
    },
  );
  activeHostBridge = { key, module, blobUrls };
  return module;
}

export function unloadHostBridgeModule(): void {
  if (!activeHostBridge) return;
  revokeBlobUrls(activeHostBridge.blobUrls);
  activeHostBridge = null;
}
