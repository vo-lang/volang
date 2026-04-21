// Renderer bridge for Studio GUI runtime.
// Studio is framework-neutral: renderer modules are loaded dynamically
// from the VFS snapshot using a blob URL — no framework-specific imports.

import { invoke as tauriInvoke, listen as tauriListen } from '../tauri';
import type { Backend } from '../backend/backend';
import { isGuiSessionSupersededError, type RuntimeService } from '../services/runtime_service';
import { frameworkContractKey, frameworkJsModulePath, type FrameworkContract } from '../types';
import type { VoWebModule } from '../studio_wasm';
import { loadStudioWasm, makeVoWebModule, resetStudioWasmInstance, setStudioWindowVfsBackendFactory } from '../studio_wasm';
import { createInMemoryWindowVfsBackend } from '../in_memory_window_vfs';

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
  moduleBytes: Uint8Array;
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
  registerWidget?(name: string, factory: WidgetFactory): void;
  destroyWidgets?(): void;
}

export type RendererBridgeContext = {
  moduleBytes: Uint8Array;
  entryPath: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  onError?: (message: string) => void;
};

type ActiveRendererBridge = {
  primaryRenderer: RendererModule | null;
  renderers: RendererModule[];
  blobUrls: string[];
  sessionId: number;
} | null;

export type VfsFile = { path: string; bytes: Uint8Array };

type RendererBlobGraph = {
  entryUrl: string;
  urls: string[];
};

let activeRendererBridge: ActiveRendererBridge = null;
const widgetRegistry = new Map<string, WidgetFactory>();

function revokeBlobUrls(urls: string[]): void {
  for (const url of urls) {
    URL.revokeObjectURL(url);
  }
}

function emitRendererBridgeDebug(backend: Backend, message: string): void {
  if (backend.platform === 'native') {
    void tauriInvoke('cmd_debug_log', { message: `[RendererBridge] ${message}` }).catch(() => {});
    return;
  }
  console.debug('[RendererBridge]', message);
}

function frameworkModuleKey(framework: FrameworkContract): string {
  return frameworkContractKey(framework);
}

function collectRendererFrameworks(context: RendererBridgeContext): FrameworkContract[] {
  const ordered = context.framework
    ? [context.framework, ...context.providerFrameworks]
    : [...context.providerFrameworks];
  const seen = new Set<string>();
  const frameworks: FrameworkContract[] = [];
  for (const framework of ordered) {
    const key = frameworkModuleKey(framework);
    if (seen.has(key)) {
      continue;
    }
    seen.add(key);
    frameworks.push(framework);
  }
  return frameworks;
}

// Whether the renderer bridge is currently alive.
// Used by PreviewPanel to avoid re-launching during layout transitions.
export function isRendererBridgeActive(sessionId?: number | null): boolean {
  if (sessionId == null) {
    return activeRendererBridge !== null;
  }
  return activeRendererBridge?.sessionId === sessionId;
}

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
  getVoWebLazy: () => Promise<VoWebModule>,
  declaredCapabilities: string[],
  onError?: (message: string) => void,
  registerWidgetWithRenderers?: (name: string, factory: WidgetFactory) => void,
): RendererHost {
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
        let handler: ((frame: Uint8Array) => void) | null = null;
        let unlisten: (() => void) | null = null;
        let closed = false;
        const waitForPollTick = (): Promise<void> => new Promise((resolve) => {
          if (typeof window !== 'undefined' && typeof window.requestAnimationFrame === 'function') {
            window.requestAnimationFrame(() => resolve());
            return;
          }
          setTimeout(resolve, 16);
        });
        const startWebPolling = (): void => {
          void (async () => {
            while (!closed) {
              try {
                const frame = await runtime.pollIslandTransport();
                if (closed) {
                  return;
                }
                if (frame.length > 0) {
                  handler?.(frame);
                  continue;
                }
              } catch (error) {
                if (!closed && !isGuiSessionSupersededError(error)) {
                  console.error('[RendererBridge] island transport poll failed:', error);
                }
                return;
              }
              await waitForPollTick();
            }
          })();
        };
        return {
          async init(): Promise<void> {
            if (backend.platform === 'native') {
              unlisten = await tauriListen<number[]>('island_data', (event) => {
                const frame = new Uint8Array(event.payload);
                handler?.(frame);
              });
              return;
            }
            startWebPolling();
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
            closed = true;
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

  if (capSet.has('vfs')) {
    capabilities.vfs = {
      getBytes(path: string): Uint8Array | null {
        const f = vfsFiles.find((x) => x.path === path || x.path.endsWith('/' + path));
        return f ? f.bytes : null;
      },
    };
  }

  if (capSet.has('widget')) {
    capabilities.widget = {
      register(name: string, factory: WidgetFactory): void {
        widgetRegistry.set(name, factory);
        registerWidgetWithRenderers?.(name, factory);
        emitRendererBridgeDebug(backend, `widget.register name=${name}`);
      },
    };
  }

  return {
    moduleBytes,
    async sendEvent(handlerId: number, payload: string): Promise<Uint8Array> {
      return runtime.sendGuiEvent(handlerId, payload);
    },
    log(message: string): void {
      emitRendererBridgeDebug(backend, message);
    },
    reportError(message: string): void {
      emitRendererBridgeDebug(backend, `error ${message}`);
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
async function loadRendererModule(
  rendererPath: string,
  files: VfsFile[],
): Promise<[RendererModule, string[]]> {
  const { module: renderer, blobUrls } = await loadVfsModule<RendererModule>(rendererPath, files, (raw) =>
    ({
      init: (raw.default as RendererModule | undefined)?.init ?? (raw.init as RendererModule['init']) ?? (() => Promise.resolve()),
      render: (raw.default as RendererModule | undefined)?.render ?? (raw.render as RendererModule['render']) ?? (() => {}),
      stop: (raw.default as RendererModule | undefined)?.stop ?? (raw.stop as RendererModule['stop']) ?? (() => {}),
      registerWidget: (raw.default as RendererModule | undefined)?.registerWidget ?? (raw.registerWidget as RendererModule['registerWidget']),
      destroyWidgets: (raw.default as RendererModule | undefined)?.destroyWidgets ?? (raw.destroyWidgets as RendererModule['destroyWidgets']),
    }),
  );
  return [renderer, blobUrls];
}

type LoadedRendererModule = {
  framework: FrameworkContract;
  renderer: RendererModule;
  blobUrls: string[];
};

async function loadRendererModules(
  frameworks: FrameworkContract[],
  files: VfsFile[],
): Promise<LoadedRendererModule[]> {
  const loaded: LoadedRendererModule[] = [];
  const seenRendererPaths = new Set<string>();
  try {
    for (const framework of frameworks) {
      const rendererPath = frameworkJsModulePath(framework, 'renderer');
      if (!rendererPath || seenRendererPaths.has(rendererPath)) {
        continue;
      }
      seenRendererPaths.add(rendererPath);
      const [renderer, blobUrls] = await loadRendererModule(rendererPath, files);
      loaded.push({ framework, renderer, blobUrls });
    }
    return loaded;
  } catch (error) {
    revokeBlobUrls(loaded.flatMap((entry) => entry.blobUrls));
    throw error;
  }
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
  const frameworks = collectRendererFrameworks(context);
  if (frameworks.length === 0) {
    throw new Error('No framework contract available');
  }

  const rendererFrameworks = frameworks.filter((framework) => frameworkJsModulePath(framework, 'renderer') != null);
  if (rendererFrameworks.length === 0) {
    throw new Error('No framework declares a renderer path');
  }

  widgetRegistry.clear();
  const resolvedVfsFiles: VfsFile[] = vfsFiles ?? await fetchVfsSnapshot(backend, context.entryPath);
  let sharedVoWebPromise: Promise<VoWebModule> | null = null;
  const getVoWebLazy = (): Promise<VoWebModule> => {
    if (!sharedVoWebPromise) {
      sharedVoWebPromise = (async () => {
        try {
          (globalThis as Record<string, unknown>).__voStudioLogRecord = (record: unknown) => {
            const source = typeof (record as { source?: unknown } | null)?.source === 'string'
              ? (record as { source: string }).source
              : 'studio-wasm';
            const code = typeof (record as { code?: unknown } | null)?.code === 'string'
              ? (record as { code: string }).code
              : 'log';
            const text = typeof (record as { text?: unknown } | null)?.text === 'string'
              ? (record as { text: string }).text
              : '';
            emitRendererBridgeDebug(backend, `[${source}:${code}]${text ? ` ${text}` : ''}`);
          };
          if (backend.platform === 'native') {
            emitRendererBridgeDebug(backend, `studio_wasm.host_vfs.install files=${resolvedVfsFiles.length}`);
            setStudioWindowVfsBackendFactory(() => createInMemoryWindowVfsBackend({
              files: resolvedVfsFiles.map((file) => ({ path: file.path, bytes: file.bytes })),
            }));
            resetStudioWasmInstance();
          }
          const wasm = await loadStudioWasm();
          return makeVoWebModule(wasm);
        } catch (error) {
          sharedVoWebPromise = null;
          throw error;
        }
      })();
    }
    return sharedVoWebPromise;
  };
  const loadedRenderers = await loadRendererModules(rendererFrameworks, resolvedVfsFiles);
  emitRendererBridgeDebug(
    backend,
    `start session=${sessionId} renderers=${loadedRenderers.map((entry) => `${entry.framework.name}:${frameworkJsModulePath(entry.framework, 'renderer')}`).join(',')}`,
  );
  const registerWidgetWithRenderers = (name: string, factory: WidgetFactory): void => {
    let bridged = 0;
    for (const entry of loadedRenderers) {
      if (entry.renderer.registerWidget) {
        entry.renderer.registerWidget(name, factory);
        bridged += 1;
      }
    }
    emitRendererBridgeDebug(backend, `widget.bridge name=${name} bridged=${bridged}`);
  };
  const primaryRendererPath = context.framework
    ? frameworkJsModulePath(context.framework, 'renderer')
    : loadedRenderers[0]
      ? frameworkJsModulePath(loadedRenderers[0].framework, 'renderer')
      : null;
  const primaryRenderer = primaryRendererPath
    ? loadedRenderers.find((entry) => frameworkJsModulePath(entry.framework, 'renderer') === primaryRendererPath)?.renderer ?? loadedRenderers[0]?.renderer ?? null
    : loadedRenderers[0]?.renderer ?? null;
  const blobUrls = loadedRenderers.flatMap((entry) => entry.blobUrls);
  const initializedRenderers: RendererModule[] = [];
  try {
    for (const entry of loadedRenderers) {
      const host = makeRendererHost(
        canvasId,
        backend,
        runtime,
        context.moduleBytes,
        resolvedVfsFiles,
        getVoWebLazy,
        entry.framework.capabilities ?? [],
        context.onError,
        registerWidgetWithRenderers,
      );
      await entry.renderer.init(host);
      emitRendererBridgeDebug(backend, `renderer.init name=${entry.framework.name}`);
      initializedRenderers.push(entry.renderer);
    }
    activeRendererBridge = { primaryRenderer, renderers: initializedRenderers, blobUrls, sessionId };
  } catch (error) {
    for (const renderer of initializedRenderers.reverse()) {
      try {
        renderer.stop();
      } catch (stopError) {
        console.error('[RendererBridge] renderer stop failed during init rollback:', stopError);
      }
    }
    widgetRegistry.clear();
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
    for (const renderer of [...active.renderers].reverse()) {
      try {
        renderer.destroyWidgets?.();
        renderer.stop();
      } catch (error) {
        console.error('[RendererBridge] renderer stop failed:', error);
      }
    }
  } finally {
    widgetRegistry.clear();
    revokeBlobUrls(active.blobUrls);
  }
  return true;
}

// Deliver render bytes to the active renderer
export function deliverRenderBytes(container: HTMLElement, bytes: Uint8Array): void {
  const renderer = activeRendererBridge?.primaryRenderer;
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

type CachedModule<T> = { module: T; blobUrls: string[] };
type CachedModuleMap<T> = Map<string, CachedModule<T>>;

function moduleCacheKey(entryPath: string, modulePath: string): string {
  return `${entryPath}\0${modulePath}`;
}

function clearCachedModules<T>(slots: CachedModuleMap<T>): void {
  for (const slot of slots.values()) {
    revokeBlobUrls(slot.blobUrls);
  }
  slots.clear();
}

// ---- Protocol module loader ----

let activeProtocols: CachedModuleMap<ProtocolModule> = new Map();

export async function loadProtocolModule(
  protocolPath: string,
  backend: Backend,
  entryPath: string,
  prefetchedFiles?: VfsFile[],
): Promise<ProtocolModule> {
  const key = moduleCacheKey(entryPath, protocolPath);
  const cached = activeProtocols.get(key);
  if (cached) return cached.module;

  const files: VfsFile[] = prefetchedFiles ?? await fetchVfsSnapshot(backend, entryPath);
  const { module, blobUrls } = await loadVfsModule<ProtocolModule>(protocolPath, files, (raw) =>
    raw.default as ProtocolModule ?? {
      findExternalWidgetHandlerId: (raw.findExternalWidgetHandlerId as ProtocolModule['findExternalWidgetHandlerId']) ?? (() => null),
    },
  );
  activeProtocols.set(key, { module, blobUrls });
  return module;
}

export function unloadProtocolModule(): void {
  clearCachedModules(activeProtocols);
}

// ---- Host bridge module loader ----

let activeHostBridges: CachedModuleMap<HostBridgeModule> = new Map();

export async function loadHostBridgeModule(
  hostBridgePath: string,
  backend: Backend,
  entryPath: string,
  prefetchedFiles?: VfsFile[],
): Promise<HostBridgeModule> {
  const key = moduleCacheKey(entryPath, hostBridgePath);
  const cached = activeHostBridges.get(key);
  if (cached) return cached.module;

  const files: VfsFile[] = prefetchedFiles ?? await fetchVfsSnapshot(backend, entryPath);
  const { module, blobUrls } = await loadVfsModule<HostBridgeModule>(hostBridgePath, files, (raw) =>
    raw.default as HostBridgeModule ?? {
      buildImports: (raw.buildImports as HostBridgeModule['buildImports']) ?? (() => ({})),
    },
  );
  activeHostBridges.set(key, { module, blobUrls });
  return module;
}

export function unloadHostBridgeModule(): void {
  clearCachedModules(activeHostBridges);
}
