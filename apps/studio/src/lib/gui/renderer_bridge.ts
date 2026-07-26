// Renderer bridge for Studio GUI runtime.
// Studio is framework-neutral: renderer modules are loaded dynamically
// from the VFS snapshot using a blob URL — no framework-specific imports.

import { parse as parseJavaScriptModule } from 'acorn';
import { listen as tauriListen } from '../tauri';
import type { Backend } from '../backend/backend';
import { handleVoplayPerfHostLog } from '../perf_report_bridge';
import { isGuiSessionSupersededError, type RuntimeService } from '../services/runtime_service';
import {
  frameworkContractKey,
  frameworkJsModulePath,
  type FrameworkContract,
  type FrameworkLaneBinding,
} from '../types';
import {
  EXACT_SCHEMA_FINGERPRINT as APP_EXACT_SCHEMA_FINGERPRINT,
  MessageKind as AppMessageKind,
  decodeAppEnvelope,
  encodeAppEnvelope,
} from '../../../../../lang/protocol/app-runtime/generated/app_protocol';
import type { VoWebModule } from '../studio_wasm';
import {
  clearStudioHostLogSinkForSession,
  clearStudioWindowVfsBackendFactoryForSession,
  loadStudioWasm,
  makeVoWebModule,
  setStudioHostLogSinkForSession,
  setStudioWindowVfsBackendFactoryForSession,
  withHostBridgeSession,
} from '../studio_wasm';
import { createInMemoryWindowVfsBackend } from '../in_memory_window_vfs';
import { ProviderInflightGate, type ProviderGateHandle } from './provider_inflight_gate';
import { ProviderInstanceSet } from './provider_instance_set';
import {
  AppCompositionHost,
  type AppAssetHandle,
  type AppSurfaceDescriptor,
  type AppSurfaceHitRegion,
  type AppSurfaceIdentity,
  type AppSurfaceInputEvent,
  type AppSurfaceInputSink,
  type AppSurfaceLease,
  type AppSystemShortcut,
} from './app_composition_host';

// ---- HostBridge module contract ----

export interface HostBridgeContext {
  readString(ptr: number, len: number): string;
  alloc(size: number): number;
  writeBytes(destPtr: number, bytes: Uint8Array): void;
  writeU32(ptr: number, value: number): void;
}

export interface HostBridgeModule {
  buildImports(ctx: HostBridgeContext): Record<string, (...args: number[]) => number | void>;
}

// ---- RendererHost ----
// Passed to renderer module init() for capabilities and event dispatch.

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

export interface StudioFrameworkLane {
  readonly binding: FrameworkLaneBinding;
  poll(): Promise<Uint8Array | null>;
  submit(payload: Uint8Array, requestId?: bigint): Promise<void>;
  submitBatch(
    entries: readonly Readonly<{ payload: Uint8Array; requestId?: bigint }>[],
  ): Promise<void>;
  close(): void;
}

export interface FrameworkLaneCapability {
  open(role?: string): Promise<StudioFrameworkLane>;
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

export interface AppSurfaceCapability {
  readonly sessionId: number;
  isInteractive(): boolean;
  resolve(surface: Readonly<{ index: number; generation: number }>): Promise<Readonly<{
    session: Readonly<{ index: number; generation: number }>;
    sessionEpoch: bigint;
    window: Readonly<{ index: number; generation: number }>;
    view: Readonly<{ index: number; generation: number }>;
    surface: Readonly<{ index: number; generation: number }>;
    kind: 'game' | 'ui' | 'diagnostics';
    zOrder: number;
    inputPolicy: 'observe' | 'passthrough' | 'interactive' | 'exclusive';
  }>>;
  attach(descriptor: AppSurfaceDescriptor): AppSurfaceLease;
  lookup(identity: AppSurfaceIdentity): AppSurfaceLease | null;
  subscribeInput(sink: AppSurfaceInputSink): () => void;
  capturePointer(pointerId: number, identity: AppSurfaceIdentity): void;
  releasePointer(pointerId: number): void;
  focus(identity: AppSurfaceIdentity): void;
  publishHitRegions(
    identity: AppSurfaceIdentity,
    revision: bigint,
    regions: readonly AppSurfaceHitRegion[],
  ): void;
  setInputSuspended(identity: AppSurfaceIdentity, suspended: boolean): void;
  setLowerInputSuspended(identity: AppSurfaceIdentity, suspended: boolean): void;
  reserveSystemShortcuts(
    identity: AppSurfaceIdentity,
    shortcuts: readonly AppSystemShortcut[],
  ): Promise<void>;
}

export interface AssetBufferCapability {
  bind(asset: AppAssetHandle, artifactId: Uint8Array): Promise<void>;
  read(asset: AppAssetHandle): Promise<ArrayBuffer>;
  release(asset: AppAssetHandle): void;
}

export type { AppSurfaceInputEvent };

export interface CapabilityMap {
  app_surface: AppSurfaceCapability;
  asset_buffer: AssetBufferCapability;
  canvas: CanvasCapability;
  framework_lane: FrameworkLaneCapability;
  island_transport: IslandTransportCapability;
  vo_web: VoWebCapability;
  vfs: VfsCapability;
  widget: WidgetCapability;
}

export interface RendererHost {
  readonly framework: Readonly<{
    name: string;
    roles: readonly string[];
    providerRoles: readonly string[];
  }>;
  moduleBytes: Uint8Array;
  sendEvent(handlerId: number, payload: string): Promise<Uint8Array>;
  log(message: string): void;
  reportError(message: string): void;
  getCapability<K extends keyof CapabilityMap>(name: K): CapabilityMap[K] | null;
}

// ---- Browser framework module contracts ----
// Every declared browser provider module owns its own lifecycle. Renderer
// modules additionally receive presentation bytes through render().

export interface BrowserFrameworkModule {
  init(host: RendererHost): Promise<void>;
  stop(): void;
  acceptHostRenderCommand?(bytes: Uint8Array): void | Uint8Array | null | Promise<void | Uint8Array | null>;
  quiesceForCapture?(): ({ stopped?: number } & Record<string, unknown>) | void;
  registerWidget?(name: string, factory: WidgetFactory): void;
  destroyWidgets?(): void;
}

export interface RendererModule extends BrowserFrameworkModule {
  render(container: HTMLElement, bytes: Uint8Array): void | Promise<void>;
}

export type RendererBridgeContext = {
  moduleBytes: Uint8Array;
  entryPath: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  onError?: (message: string) => void;
  onDetached?: (sessionId: number) => void;
  reuseActiveProviders?: boolean;
};

type ActiveRendererBridge = {
  primaryRenderer: RendererModule | null;
  ingressModules: RendererModule[];
  renderers: BrowserFrameworkModule[];
  blobUrls: string[];
  sessionId: number;
  widgetRegistry: Map<string, WidgetFactory>;
  callGate: ProviderGateHandle;
  compositionHost: AppCompositionHost;
  providerModuleKeys: string[];
  runtime: RuntimeService;
  lastRenderBytes: Uint8Array | null;
  deliveryChain: Promise<void>;
  reportError(message: string): void;
  releaseSurface(): void;
};

type StudioBrowserSmokeRendererDebugHook = {
  moduleBytesLength(): number;
  dumpModuleBytes(): Promise<string>;
  quiesceRenderLoop(): { renderers: unknown[]; stopped: number; sessionId: number | null };
  rendererState(): { active: boolean; renderers: unknown[]; sessionId: number | null };
};

export type VfsFile = { path: string; bytes: Uint8Array };
export type VfsSnapshot = { rootPath: string; files: VfsFile[] };

const MAX_RENDERER_VFS_FILES = 20_000;
const MAX_RENDERER_VFS_FILE_BYTES = 256 * 1024 * 1024;
const MAX_RENDERER_VFS_TOTAL_BYTES = 512 * 1024 * 1024;
const MAX_RENDERER_VFS_PATH_BYTES = 4096;
const MAX_RENDERER_VFS_NAME_BYTES = 255;
const MAX_RENDERER_VFS_DEPTH = 256;
const MAX_RENDERER_MODULE_GRAPH_DEPTH = 256;
const MAX_RENDERER_MODULE_GRAPH_FILES = 4096;
const MAX_RENDERER_MODULE_SOURCE_BYTES = 16 * 1024 * 1024;
const MAX_RENDERER_MODULE_GRAPH_BYTES = 64 * 1024 * 1024;
const MAX_RENDERER_MODULE_AST_NODES_PER_FILE = 250_000;
const MAX_RENDERER_MODULE_GRAPH_AST_NODES = 1_000_000;
const MAX_RENDERER_MODULE_TOKENS_PER_FILE = 250_000;
const MAX_RENDERER_MODULE_GRAPH_TOKENS = 1_000_000;
const MAX_RENDERER_MODULE_IMPORTS_PER_FILE = 100_000;
const MAX_RENDERER_MODULE_GRAPH_IMPORTS = 250_000;
const MAX_RENDERER_MODULE_REWRITTEN_BYTES = 32 * 1024 * 1024;
const MAX_RENDERER_MODULE_GRAPH_REWRITTEN_BYTES = 96 * 1024 * 1024;
const MAX_RENDERER_MODULE_SPECIFIER_BYTES = 8 * 1024;
const MAX_RENDERER_IMPORT_MAP_BYTES = 4 * 1024 * 1024;
const rendererPathEncoder = new TextEncoder();

function compareRendererPathsUtf8(left: string, right: string): number {
  const leftBytes = rendererPathEncoder.encode(left);
  const rightBytes = rendererPathEncoder.encode(right);
  const length = Math.min(leftBytes.byteLength, rightBytes.byteLength);
  for (let index = 0; index < length; index += 1) {
    if (leftBytes[index] !== rightBytes[index]) return leftBytes[index] - rightBytes[index];
  }
  return leftBytes.byteLength - rightBytes.byteLength;
}

type RendererBlobGraph = {
  entryUrl: string;
  urls: string[];
};

function requireFunction<T>(value: unknown, label: string): T {
  if (typeof value !== 'function') {
    throw new Error(`${label} must be a function`);
  }
  return value as T;
}

function optionalFunction<T>(value: unknown, label: string): T | undefined {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== 'function') {
    throw new Error(`${label} must be a function when declared`);
  }
  return value as T;
}

function studioBrowserSmokeDebugEnabled(): boolean {
  try {
    return new URL(window.location.href).searchParams.get('studioBrowserSmokeDebug') === '1';
  } catch {
    return false;
  }
}

function exposeStudioBrowserSmokeRendererDebug(moduleBytes: Uint8Array): void {
  if (!studioBrowserSmokeDebugEnabled()) {
    return;
  }
  const bytes = new Uint8Array(moduleBytes);
  (globalThis as typeof globalThis & {
    __voStudioBrowserSmokeRenderer?: StudioBrowserSmokeRendererDebugHook;
  }).__voStudioBrowserSmokeRenderer = {
    moduleBytesLength: () => bytes.length,
    dumpModuleBytes: async () => {
      const wasm = await loadStudioWasm();
      return wasm.dumpBytecode(bytes);
    },
    quiesceRenderLoop: () => quiesceRendererBridgeForSmoke(),
    rendererState: () => rendererBridgeSmokeState(),
  };
}

const activeRendererBridges = new ProviderInstanceSet<ActiveRendererBridge>();
const preparedFrameworkProviders = new ProviderInstanceSet<Set<string>>();
const rendererCallGates = new ProviderInflightGate();
const rendererBridgeTeardowns = new Map<number, Promise<boolean>>();
const rendererImportMapsByBlobUrl = new Map<string, HTMLScriptElement>();
let nextRendererImportMapIdentity = 1;

function revokeBlobUrls(urls: string[]): void {
  const uniqueUrls = new Set(urls);
  const importMaps = new Set<HTMLScriptElement>();
  for (const url of uniqueUrls) {
    const importMap = rendererImportMapsByBlobUrl.get(url);
    if (importMap) importMaps.add(importMap);
    rendererImportMapsByBlobUrl.delete(url);
  }
  for (const importMap of importMaps) {
    try {
      importMap.remove();
    } catch (error) {
      console.error('[RendererBridge] import map cleanup failed:', error);
    }
  }
  for (const url of uniqueUrls) {
    try {
      URL.revokeObjectURL(url);
    } catch (error) {
      // Revocation is best-effort per URL. Continue through the complete graph
      // so one host failure cannot strand every URL that follows it.
      console.error('[RendererBridge] blob URL revocation failed:', error);
    }
  }
}

function shouldEmitRendererBridgeDebug(): boolean {
  try {
    if (typeof window === 'undefined') {
      return false;
    }
    const params = new URLSearchParams(window.location.search);
    return params.has('rendererDebug')
      || params.has('debug')
      || window.localStorage.getItem('studio.rendererDebug') === '1';
  } catch {
    return false;
  }
}

function emitRendererBridgeDebug(backend: Backend, message: string): void {
  void backend;
  if (!shouldEmitRendererBridgeDebug()) {
    return;
  }
  console.debug(`[RendererBridge] ${message}`);
}

function islandFrameDebug(frame: Uint8Array): string {
  const head = Array.from(frame.slice(0, Math.min(frame.length, 96)))
    .map((byte) => byte.toString(16).padStart(2, '0'))
    .join('');
  const target = frame.length >= 4
    ? new DataView(frame.buffer, frame.byteOffset, frame.byteLength).getUint32(0, true)
    : null;
  const tag = frame.length >= 5 ? frame[4] : null;
  return `bytes=${frame.byteLength} target=${target ?? 'n/a'} tag=${tag ?? 'n/a'} head=${head}`;
}

function emitRendererBridgeFrameDebug(backend: Backend, direction: 'send' | 'recv', frame: Uint8Array): void {
  if (!shouldEmitRendererBridgeDebug()) {
    return;
  }
  emitRendererBridgeDebug(backend, `island transport ${direction} ${islandFrameDebug(frame)}`);
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
    return activeRendererBridges.size > 0;
  }
  return activeRendererBridges.has(sessionId);
}

export async function deliverGameRenderBytes(
  sessionId: number,
  bytes: Uint8Array,
): Promise<Uint8Array | null> {
  if (bytes.byteLength === 0) return null;
  const active = activeRendererBridges.get(sessionId);
  if (!active) throw new Error(`renderer bridge ${sessionId} is not active`);
  const consumers = active.renderers.filter(
    (renderer) => renderer.acceptHostRenderCommand !== undefined,
  );
  if (consumers.length !== 1) {
    throw new Error(`game host-render command matched ${consumers.length} renderer modules`);
  }
  let result: Uint8Array | null = null;
  const deliver = async (): Promise<void> => {
    const ticket = rendererCallGates.enter(active.callGate);
    if (!ticket) return;
    try {
      const returned = await consumers[0]!.acceptHostRenderCommand!(bytes);
      if (returned instanceof Uint8Array && returned.byteLength > 0) result = returned;
    } finally {
      ticket.release();
    }
  };
  const pending = active.deliveryChain.then(deliver, deliver);
  active.deliveryChain = pending;
  await pending;
  return result;
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

function validateRendererVfsPath(path: string, label: string): string {
  const rawParts = typeof path === 'string' ? path.replace(/\\/g, '/').split('/') : [];
  if (
    typeof path !== 'string'
    || path.length === 0
    || path.includes('\0')
    || rawParts.includes('..')
    || rendererPathEncoder.encode(path).byteLength > MAX_RENDERER_VFS_PATH_BYTES
  ) {
    throw new Error(`${label} is invalid`);
  }
  const normalized = normalizeVfsPath(path);
  const parts = normalized.split('/').filter(Boolean);
  if (
    parts.length === 0
    || parts.length > MAX_RENDERER_VFS_DEPTH
    || parts.some((part) => (
      part === '.'
      || part === '..'
      || rendererPathEncoder.encode(part).byteLength > MAX_RENDERER_VFS_NAME_BYTES
      || /[\u0000-\u001f\u007f]/u.test(part)
    ))
  ) {
    throw new Error(`${label} is invalid`);
  }
  return normalized;
}

function compareRendererVfsPaths(left: VfsFile, right: VfsFile): number {
  const a = rendererPathEncoder.encode(left.path);
  const b = rendererPathEncoder.encode(right.path);
  const length = Math.min(a.length, b.length);
  for (let index = 0; index < length; index += 1) {
    if (a[index] !== b[index]) return a[index] - b[index];
  }
  return a.length - b.length;
}

function validateRendererVfsRootPath(path: unknown): string {
  if (
    typeof path !== 'string'
    || !path.startsWith('/')
    || path.includes('\\')
    || path.length === 0
    || (path.length > 1 && path.endsWith('/'))
    || rendererPathEncoder.encode(path).byteLength > MAX_RENDERER_VFS_PATH_BYTES
  ) {
    throw new Error('Renderer VFS snapshot root path is invalid');
  }
  if (path === '/') return path;
  const parts = path.slice(1).split('/');
  if (
    parts.length > MAX_RENDERER_VFS_DEPTH
    || parts.some((part) => (
      part.length === 0
      || part === '.'
      || part === '..'
      || rendererPathEncoder.encode(part).byteLength > MAX_RENDERER_VFS_NAME_BYTES
      || /[\u0000-\u001f\u007f]/u.test(part)
    ))
  ) {
    throw new Error('Renderer VFS snapshot root path is invalid');
  }
  return path;
}

function validateVfsSnapshot(snapshot: VfsSnapshot): VfsSnapshot {
  if (
    !snapshot
    || !Array.isArray(snapshot.files)
    || snapshot.files.length > MAX_RENDERER_VFS_FILES
  ) {
    throw new Error('Renderer VFS snapshot metadata is invalid');
  }
  const rootPath = validateRendererVfsRootPath(snapshot.rootPath);
  const files: VfsFile[] = [];
  const paths = new Set<string>();
  let totalBytes = 0;
  for (const [index, file] of snapshot.files.entries()) {
    if (!file || !(file.bytes instanceof Uint8Array)) {
      throw new Error(`Renderer VFS snapshot file ${index} is invalid`);
    }
    const path = validateRendererVfsPath(file.path, `Renderer VFS snapshot file ${index} path`);
    if (paths.has(path)) {
      throw new Error(`Renderer VFS snapshot contains duplicate path ${path}`);
    }
    if (file.bytes.byteLength > MAX_RENDERER_VFS_FILE_BYTES) {
      throw new Error(`Renderer VFS snapshot file exceeds the 256 MiB limit: ${path}`);
    }
    totalBytes += file.bytes.byteLength;
    if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_RENDERER_VFS_TOTAL_BYTES) {
      throw new Error('Renderer VFS snapshot exceeds the 512 MiB aggregate limit');
    }
    paths.add(path);
    files.push({ path, bytes: file.bytes });
  }
  files.sort(compareRendererVfsPaths);
  return { rootPath, files };
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

type RendererModuleImport = {
  start: number;
  end: number;
  specifier: string;
  targetPath: string;
};

type ParsedRendererModule = {
  path: string;
  source: string;
  sourceBytes: number;
  imports: RendererModuleImport[];
};

type RendererModuleGraphBudget = {
  astNodes: number;
  tokens: number;
  imports: number;
};

type RendererModuleRewriteBudget = {
  bytes: number;
};

function rendererModuleSpecifiers(
  source: string,
  path: string,
  graphBudget: RendererModuleGraphBudget,
): Array<Omit<RendererModuleImport, 'targetPath'>> {
  let ast: unknown;
  let tokens = 0;
  try {
    ast = parseJavaScriptModule(source, {
      allowHashBang: true,
      ecmaVersion: 'latest',
      sourceType: 'module',
      onToken: () => {
        tokens += 1;
        graphBudget.tokens += 1;
        if (
          tokens > MAX_RENDERER_MODULE_TOKENS_PER_FILE
          || graphBudget.tokens > MAX_RENDERER_MODULE_GRAPH_TOKENS
        ) {
          throw new Error(`Renderer module ${path} exceeds the syntax token budget`);
        }
      },
    });
  } catch (error) {
    throw new Error(`Renderer module ${path} is invalid JavaScript: ${error instanceof Error ? error.message : String(error)}`);
  }

  const imports: Array<Omit<RendererModuleImport, 'targetPath'>> = [];
  const stack: unknown[] = [ast];
  const visited = new WeakSet<object>();
  let nodes = 0;
  const addLiteral = (value: unknown): void => {
    if (!value || typeof value !== 'object' || Array.isArray(value)) return;
    const literal = value as { start?: unknown; end?: unknown; value?: unknown };
    if (
      typeof literal.value !== 'string'
      || (!literal.value.startsWith('./') && !literal.value.startsWith('../'))
      || !Number.isSafeInteger(literal.start)
      || !Number.isSafeInteger(literal.end)
      || (literal.start as number) < 0
      || (literal.end as number) <= (literal.start as number)
      || (literal.end as number) > source.length
    ) {
      return;
    }
    imports.push({
      start: literal.start as number,
      end: literal.end as number,
      specifier: literal.value,
    });
  };

  while (stack.length > 0) {
    const value = stack.pop();
    if (!value || typeof value !== 'object') continue;
    if (visited.has(value)) continue;
    visited.add(value);
    nodes += 1;
    graphBudget.astNodes += 1;
    if (
      nodes > MAX_RENDERER_MODULE_AST_NODES_PER_FILE
      || graphBudget.astNodes > MAX_RENDERER_MODULE_GRAPH_AST_NODES
    ) {
      throw new Error(`Renderer module ${path} exceeds the syntax node budget`);
    }
    if (Array.isArray(value)) {
      for (let index = value.length - 1; index >= 0; index -= 1) stack.push(value[index]);
      continue;
    }
    const node = value as Record<string, unknown>;
    const type = node.type;
    if (
      type === 'ImportDeclaration'
      || type === 'ExportNamedDeclaration'
      || type === 'ExportAllDeclaration'
    ) {
      addLiteral(node.source);
    } else if (type === 'ImportExpression') {
      addLiteral(node.source);
    }
    for (const child of Object.values(node)) {
      if (child && typeof child === 'object') stack.push(child);
    }
  }

  imports.sort((left, right) => left.start - right.start);
  graphBudget.imports += imports.length;
  if (
    imports.length > MAX_RENDERER_MODULE_IMPORTS_PER_FILE
    || graphBudget.imports > MAX_RENDERER_MODULE_GRAPH_IMPORTS
  ) {
    throw new Error(`Renderer module ${path} exceeds the import edge budget`);
  }
  for (let index = 1; index < imports.length; index += 1) {
    if (imports[index].start < imports[index - 1].end) {
      throw new Error(`Renderer module ${path} has overlapping import specifiers`);
    }
  }
  return imports;
}

function rewriteRendererModule(
  module: ParsedRendererModule,
  replacement: (targetPath: string) => string,
  graphBudget: RendererModuleRewriteBudget,
): string {
  const rewrittenSpecifiers = module.imports.map((imported) => {
    const value = JSON.stringify(replacement(imported.targetPath));
    const bytes = rendererPathEncoder.encode(value).byteLength;
    if (bytes > MAX_RENDERER_MODULE_SPECIFIER_BYTES) {
      throw new Error(`Renderer module ${module.path} contains an oversized rewritten specifier`);
    }
    return { imported, value, bytes };
  });
  let rewrittenBytes = module.sourceBytes;
  for (const { imported, bytes } of rewrittenSpecifiers) {
    rewrittenBytes -= rendererPathEncoder.encode(module.source.slice(imported.start, imported.end)).byteLength;
    rewrittenBytes += bytes;
  }
  const nextGraphBytes = graphBudget.bytes + rewrittenBytes;
  if (
    !Number.isSafeInteger(rewrittenBytes)
    || rewrittenBytes < 0
    || rewrittenBytes > MAX_RENDERER_MODULE_REWRITTEN_BYTES
    || !Number.isSafeInteger(nextGraphBytes)
    || nextGraphBytes > MAX_RENDERER_MODULE_GRAPH_REWRITTEN_BYTES
  ) {
    throw new Error(`Renderer module ${module.path} exceeds the rewritten source budget`);
  }
  graphBudget.bytes = nextGraphBytes;

  const parts: string[] = [];
  let cursor = 0;
  for (const { imported, value } of rewrittenSpecifiers) {
    parts.push(module.source.slice(cursor, imported.start), value);
    cursor = imported.end;
  }
  parts.push(module.source.slice(cursor));
  return parts.join('');
}

function parseRendererModuleGraph(entryFile: VfsFile, files: VfsFile[]): Map<string, ParsedRendererModule> {
  const textDecoder = new TextDecoder('utf-8', { fatal: true });
  const fileMap = new Map<string, VfsFile>();
  for (const file of files) {
    const path = validateRendererVfsPath(file.path, 'Renderer module path');
    if (fileMap.has(path)) {
      throw new Error(`Renderer VFS snapshot contains duplicate module path: ${path}`);
    }
    fileMap.set(path, file);
  }

  const entryPath = normalizeVfsPath(entryFile.path);
  const queue: Array<{ file: VfsFile; depth: number }> = [{ file: entryFile, depth: 0 }];
  const queued = new Set<string>([entryPath]);
  const modules = new Map<string, ParsedRendererModule>();
  const graphBudget: RendererModuleGraphBudget = { astNodes: 0, tokens: 0, imports: 0 };
  let totalBytes = 0;
  for (let index = 0; index < queue.length; index += 1) {
    const { file, depth } = queue[index];
    const path = normalizeVfsPath(file.path);
    if (depth > MAX_RENDERER_MODULE_GRAPH_DEPTH) {
      throw new Error(`Renderer module graph exceeds the ${MAX_RENDERER_MODULE_GRAPH_DEPTH}-level depth limit`);
    }
    if (file.bytes.byteLength > MAX_RENDERER_MODULE_SOURCE_BYTES) {
      throw new Error(`Renderer module ${path} exceeds the ${MAX_RENDERER_MODULE_SOURCE_BYTES}-byte source limit`);
    }
    totalBytes += file.bytes.byteLength;
    if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_RENDERER_MODULE_GRAPH_BYTES) {
      throw new Error(`Renderer module graph exceeds the ${MAX_RENDERER_MODULE_GRAPH_BYTES}-byte source limit`);
    }
    if (modules.size >= MAX_RENDERER_MODULE_GRAPH_FILES) {
      throw new Error(`Renderer module graph exceeds the ${MAX_RENDERER_MODULE_GRAPH_FILES}-file limit`);
    }
    let source: string;
    try {
      source = textDecoder.decode(file.bytes);
    } catch (error) {
      throw new Error(`Renderer module ${path} is not valid UTF-8: ${error instanceof Error ? error.message : String(error)}`);
    }
    const imports = rendererModuleSpecifiers(source, path, graphBudget).map((imported) => {
      const target = resolveModuleFile(path, imported.specifier, fileMap);
      const targetPath = normalizeVfsPath(target.path);
      if (!queued.has(targetPath)) {
        queued.add(targetPath);
        queue.push({ file: target, depth: depth + 1 });
      }
      return { ...imported, targetPath };
    });
    modules.set(path, { path, source, sourceBytes: file.bytes.byteLength, imports });
  }
  return modules;
}

function rendererModuleGraphHasCycle(modules: Map<string, ParsedRendererModule>, entryPath: string): boolean {
  const visiting = new Set<string>();
  const visited = new Set<string>();
  const visit = (path: string, depth: number): boolean => {
    if (visiting.has(path)) return true;
    if (visited.has(path)) return false;
    if (depth > MAX_RENDERER_MODULE_GRAPH_DEPTH) {
      throw new Error(`Renderer module graph exceeds the ${MAX_RENDERER_MODULE_GRAPH_DEPTH}-level depth limit`);
    }
    visiting.add(path);
    const module = modules.get(path);
    if (!module) throw new Error(`Renderer module graph is missing ${path}`);
    for (const imported of module.imports) {
      if (visit(imported.targetPath, depth + 1)) return true;
    }
    visiting.delete(path);
    visited.add(path);
    return false;
  };
  return visit(entryPath, 0);
}

function nextRendererImportMapId(): number {
  if (!Number.isSafeInteger(nextRendererImportMapIdentity) || nextRendererImportMapIdentity > Number.MAX_SAFE_INTEGER) {
    throw new Error('Renderer import-map identity space is exhausted');
  }
  const identity = nextRendererImportMapIdentity;
  nextRendererImportMapIdentity += 1;
  return identity;
}

function materializeCyclicRendererGraph(
  modules: Map<string, ParsedRendererModule>,
  entryPath: string,
  urls: string[],
): RendererBlobGraph {
  if (
    typeof document === 'undefined'
    || (typeof HTMLScriptElement.supports === 'function' && !HTMLScriptElement.supports('importmap'))
  ) {
    throw new Error('Cyclic renderer modules require browser import-map support');
  }
  const graphId = nextRendererImportMapId();
  const paths = [...modules.keys()].sort(compareRendererPathsUtf8);
  const specifiers = new Map(paths.map((path, index) => [
    path,
    `vo-renderer-vfs-${graphId}/${index.toString(36)}`,
  ]));
  const imports: Record<string, string> = {};
  const rewriteBudget: RendererModuleRewriteBudget = { bytes: 0 };
  for (const path of paths) {
    const module = modules.get(path)!;
    const source = rewriteRendererModule(module, (targetPath) => specifiers.get(targetPath)!, rewriteBudget);
    const url = URL.createObjectURL(new Blob([source], { type: 'application/javascript' }));
    if (rendererPathEncoder.encode(url).byteLength > MAX_RENDERER_MODULE_SPECIFIER_BYTES) {
      URL.revokeObjectURL(url);
      throw new Error(`Renderer module ${path} produced an oversized blob URL`);
    }
    urls.push(url);
    imports[specifiers.get(path)!] = url;
  }
  const importMap = document.createElement('script');
  importMap.type = 'importmap';
  const importMapText = JSON.stringify({ imports });
  if (rendererPathEncoder.encode(importMapText).byteLength > MAX_RENDERER_IMPORT_MAP_BYTES) {
    throw new Error('Renderer import map exceeds its byte budget');
  }
  importMap.textContent = importMapText;
  const parent = document.head ?? document.documentElement;
  if (!parent) throw new Error('Renderer import map has no document host');
  for (const url of urls) rendererImportMapsByBlobUrl.set(url, importMap);
  parent.appendChild(importMap);
  return { entryUrl: specifiers.get(entryPath)!, urls };
}

function buildRendererBlobGraph(entryFile: VfsFile, files: VfsFile[]): RendererBlobGraph {
  const urls: string[] = [];
  try {
    const entryPath = normalizeVfsPath(entryFile.path);
    const modules = parseRendererModuleGraph(entryFile, files);
    if (rendererModuleGraphHasCycle(modules, entryPath)) {
      return materializeCyclicRendererGraph(modules, entryPath, urls);
    }
    const urlByPath = new Map<string, string>();
    const rewriteBudget: RendererModuleRewriteBudget = { bytes: 0 };
    const materialize = (path: string, depth: number): string => {
      const existing = urlByPath.get(path);
      if (existing) return existing;
      if (depth > MAX_RENDERER_MODULE_GRAPH_DEPTH) {
        throw new Error(`Renderer module graph exceeds the ${MAX_RENDERER_MODULE_GRAPH_DEPTH}-level depth limit`);
      }
      const module = modules.get(path);
      if (!module) throw new Error(`Renderer module graph is missing ${path}`);
      const source = rewriteRendererModule(
        module,
        (targetPath) => materialize(targetPath, depth + 1),
        rewriteBudget,
      );
      const url = URL.createObjectURL(new Blob([source], { type: 'application/javascript' }));
      urlByPath.set(path, url);
      urls.push(url);
      return url;
    };
    return { entryUrl: materialize(entryPath, 0), urls };
  } catch (error) {
    revokeBlobUrls(urls);
    throw error;
  }
}

function makeRendererHost(
  canvasId: string,
  backend: Backend,
  runtime: RuntimeService,
  sessionId: number,
  moduleBytes: Uint8Array,
  vfsFiles: VfsFile[],
  getVoWebLazy: () => Promise<VoWebModule>,
  framework: FrameworkContract,
  widgetRegistry: Map<string, WidgetFactory>,
  compositionHost: AppCompositionHost,
  onError?: (message: string) => void,
  registerWidgetWithRenderers?: (name: string, factory: WidgetFactory) => void,
): RendererHost {
  const capSet = new Set(framework.capabilities);
  exposeStudioBrowserSmokeRendererDebug(moduleBytes);

  // Build capability map — only capabilities declared by the framework are available.
  const capabilities: Partial<CapabilityMap> = {};

  if (capSet.has('asset_buffer')) {
    capabilities.asset_buffer = {
      async bind(asset: AppAssetHandle, artifactId: Uint8Array): Promise<void> {
        if (artifactId.byteLength !== 16) {
          throw new Error('asset artifact identity must contain 16 bytes');
        }
        const hex = Array.from(artifactId)
          .map((byte) => byte.toString(16).padStart(2, '0'))
          .join('');
        const candidates = [
          `assets/${hex}.bin`,
          `assets/${hex}`,
          `.voplay/cache/${hex}.bin`,
          `.voplay/cache/${hex}`,
        ];
        const file = candidates
          .map((path) => vfsFiles.find((entry) => normalizeVfsPath(entry.path) === path))
          .find((entry) => entry !== undefined);
        if (file === undefined) {
          throw new Error(`asset artifact ${hex} is absent from the module VFS`);
        }
        compositionHost.publishAssetBuffer(framework.name, asset, file.bytes);
      },
      async read(asset: AppAssetHandle): Promise<ArrayBuffer> {
        return compositionHost.readAssetBuffer(asset);
      },
      release(asset: AppAssetHandle): void {
        compositionHost.releaseAssetBuffer(framework.name, asset);
      },
    };
  }

  if (capSet.has('app_surface')) {
    capabilities.app_surface = {
      sessionId: compositionHost.sessionId,
      isInteractive() {
        return runtime.isGuiSessionSelected(sessionId);
      },
      resolve(surface) {
        return runtime.resolveAppSurfaceRoute(surface, sessionId);
      },
      attach(descriptor: AppSurfaceDescriptor): AppSurfaceLease {
        return compositionHost.attach(framework.name, descriptor);
      },
      lookup(identity: AppSurfaceIdentity): AppSurfaceLease | null {
        return compositionHost.lookup(identity);
      },
      subscribeInput(sink: AppSurfaceInputSink): () => void {
        return compositionHost.subscribeInput(framework.name, sink);
      },
      capturePointer(pointerId: number, identity: AppSurfaceIdentity): void {
        compositionHost.capturePointer(framework.name, pointerId, identity);
      },
      releasePointer(pointerId: number): void {
        compositionHost.releasePointer(framework.name, pointerId);
      },
      focus(identity: AppSurfaceIdentity): void {
        compositionHost.focus(framework.name, identity);
      },
      publishHitRegions(
        identity: AppSurfaceIdentity,
        revision: bigint,
        regions: readonly AppSurfaceHitRegion[],
      ): void {
        compositionHost.publishHitRegions(framework.name, identity, revision, regions);
      },
      setInputSuspended(identity: AppSurfaceIdentity, suspended: boolean): void {
        compositionHost.setInputSuspended(framework.name, identity, suspended);
      },
      setLowerInputSuspended(identity: AppSurfaceIdentity, suspended: boolean): void {
        compositionHost.setLowerInputSuspended(framework.name, identity, suspended);
      },
      async reserveSystemShortcuts(
        identity: AppSurfaceIdentity,
        shortcuts: readonly AppSystemShortcut[],
      ): Promise<void> {
        compositionHost.reserveSystemShortcuts(framework.name, identity, shortcuts);
        if (backend.platform !== 'native') return;
        try {
          await runtime.registerAppSurfaceShortcuts(
            identity.surface,
            shortcuts.map(({ classMask, scope, priority }) => ({
              classMask,
              scope,
              priority,
            })),
            sessionId,
          );
        } catch (error) {
          compositionHost.reserveSystemShortcuts(framework.name, identity, []);
          throw error;
        }
      },
    };
  }

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
        let transportQueue: Promise<void> = Promise.resolve();
        let draining = false;
        let drainAgain = false;
        const WEB_TRANSPORT_POLL_FALLBACK_MS = 16;
        const WEB_TRANSPORT_DRAIN_FRAME_BUDGET = 16;
        const WEB_TRANSPORT_DRAIN_TIME_BUDGET_MS = 4;
        const waitForPollTick = (): Promise<void> => new Promise((resolve) => {
          setTimeout(resolve, WEB_TRANSPORT_POLL_FALLBACK_MS);
        });
        const yieldToBrowserEventLoop = (): Promise<void> => new Promise((resolve) => {
          setTimeout(resolve, 0);
        });
        const runTransport = async <T>(operation: () => Promise<T>): Promise<T> => {
          const run = transportQueue.then(operation, operation);
          transportQueue = run.then(() => undefined, () => undefined);
          return run;
        };
        const deliverFrame = (frame: Uint8Array): void => {
          const ownedFrame = new Uint8Array(frame);
          emitRendererBridgeFrameDebug(backend, 'recv', ownedFrame);
          handler?.(ownedFrame);
        };
        const drainAvailableFrames = async (): Promise<void> => {
          if (draining) {
            drainAgain = true;
            return;
          }
          draining = true;
          try {
            do {
              drainAgain = false;
              let drainedFrames = 0;
              let drainStartMs = performance.now();
              while (!closed) {
                const frame = await runTransport(
                  () => runtime.pollIslandTransport(sessionId),
                );
                if (closed || frame.length === 0) {
                  break;
                }
                deliverFrame(frame);
                drainedFrames += 1;
                if (
                  drainedFrames >= WEB_TRANSPORT_DRAIN_FRAME_BUDGET
                  || performance.now() - drainStartMs >= WEB_TRANSPORT_DRAIN_TIME_BUDGET_MS
                ) {
                  drainedFrames = 0;
                  drainStartMs = performance.now();
                  await yieldToBrowserEventLoop();
                }
              }
            } while (!closed && drainAgain);
          } finally {
            draining = false;
          }
        };
        const pushFrame = async (frame: Uint8Array): Promise<void> => {
          const frames = await runTransport(
            () => runtime.pushAndPollIslandTransport(frame, sessionId),
          );
          let deliveredFrames = 0;
          let deliverStartMs = performance.now();
          for (const responseFrame of frames) {
            if (closed) {
              return;
            }
            deliverFrame(responseFrame);
            deliveredFrames += 1;
            if (
              deliveredFrames >= WEB_TRANSPORT_DRAIN_FRAME_BUDGET
              || performance.now() - deliverStartMs >= WEB_TRANSPORT_DRAIN_TIME_BUDGET_MS
            ) {
              deliveredFrames = 0;
              deliverStartMs = performance.now();
              await yieldToBrowserEventLoop();
            }
          }
          if (frames.length >= WEB_TRANSPORT_DRAIN_FRAME_BUDGET) {
            await drainAvailableFrames();
          }
        };
        const reportTransportError = (label: string, error: unknown): void => {
          if (isGuiSessionSupersededError(error)) {
            return;
          }
          console.error(`[RendererBridge] island transport ${label} failed:`, error);
          if (!closed) {
            closed = true;
            onError?.(`Island transport ${label} failed: ${error instanceof Error ? error.message : String(error)}`);
          }
        };
        const startWebPolling = (): void => {
          void (async () => {
            while (!closed) {
              try {
                await drainAvailableFrames();
              } catch (error) {
                reportTransportError('poll', error);
                return;
              }
              await waitForPollTick();
            }
          })();
        };
        return {
          async init(): Promise<void> {
            if (backend.platform === 'native') {
              unlisten = await tauriListen<{
                sessionId: number;
                bytes: number[];
              }>('island_data', (event) => {
                if (event.payload.sessionId !== sessionId) return;
                const frame = new Uint8Array(event.payload.bytes);
                handler?.(frame);
              });
              return;
            }
            startWebPolling();
          },
          send(frame: Uint8Array): void {
            const ownedFrame = new Uint8Array(frame);
            emitRendererBridgeFrameDebug(backend, 'send', ownedFrame);
            pushFrame(ownedFrame).catch((error) => reportTransportError('push', error));
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

  if (capSet.has('framework_lane') || framework.roles.includes('renderer')) {
    capabilities.framework_lane = {
      async open(role?: string): Promise<StudioFrameworkLane> {
        if (role !== undefined && !/^[a-z][a-z0-9-]{0,63}$/.test(role)) {
          throw new Error('framework lane role is invalid');
        }
        const owner = role === undefined ? framework.name : `${framework.name}/${role}`;
        const binding = await runtime.openFrameworkLane(owner, sessionId);
        if (
          binding.selectedExactFingerprint.length !== APP_EXACT_SCHEMA_FINGERPRINT.length
          || binding.selectedExactFingerprint.some(
            (byte, index) => byte !== APP_EXACT_SCHEMA_FINGERPRINT[index],
          )
        ) {
          throw new Error('framework lane selected an unexpected App protocol fingerprint');
        }
        let closed = false;
        let nextInboundSequence = 1n;
        let submitChain: Promise<void> = Promise.resolve();
        return {
          binding,
          async poll(): Promise<Uint8Array | null> {
            if (closed) return null;
            const packet = await runtime.pollFrameworkLane(binding, sessionId);
            if (closed || packet.length === 0) return null;
            const envelope = decodeAppEnvelope(packet);
            if (
              envelope.header.session.index !== binding.session.index
              || envelope.header.session.generation !== binding.session.generation
              || envelope.header.sessionEpoch !== BigInt(binding.sessionEpoch)
              || envelope.header.channel.index !== binding.channel.index
              || envelope.header.channel.generation !== binding.channel.generation
              || envelope.header.channelEpoch !== BigInt(binding.channelEpoch)
              || envelope.header.messageKind !== AppMessageKind.FrameworkPayload
            ) {
              throw new Error('framework lane received a misrouted App envelope');
            }
            return new Uint8Array(envelope.payload);
          },
          async submit(payload: Uint8Array, requestId = 0n): Promise<void> {
            if (closed) {
              throw new Error('framework lane is closed');
            }
            const ownedPayload = new Uint8Array(payload);
            if (ownedPayload.length === 0) {
              throw new Error('framework lane payload is empty');
            }
            const run = submitChain.then(async () => {
              if (closed) throw new Error('framework lane is closed');
              const sequence = nextInboundSequence;
              const packet = encodeAppEnvelope({
                session: binding.session,
                sessionEpoch: BigInt(binding.sessionEpoch),
                channel: binding.channel,
                channelEpoch: BigInt(binding.channelEpoch),
                messageKind: AppMessageKind.FrameworkPayload,
                flags: 0,
                sequence,
                requestId,
              }, ownedPayload);
              if (packet.length > binding.maxPacketBytes) {
                throw new Error('framework lane packet exceeds negotiated limit');
              }
              await runtime.submitFrameworkLane(binding, packet, sessionId);
              nextInboundSequence = sequence + 1n;
            });
            submitChain = run.then(() => undefined, () => undefined);
            await run;
          },
          async submitBatch(entries): Promise<void> {
            if (closed) {
              throw new Error('framework lane is closed');
            }
            if (entries.length === 0 || entries.length > 4096) {
              throw new Error('framework lane batch count is invalid');
            }
            const ownedEntries = entries.map(({ payload, requestId = 0n }) => {
              const ownedPayload = new Uint8Array(payload);
              if (ownedPayload.length === 0) {
                throw new Error('framework lane batch payload is empty');
              }
              return { payload: ownedPayload, requestId };
            });
            const run = submitChain.then(async () => {
              if (closed) throw new Error('framework lane is closed');
              let sequence = nextInboundSequence;
              const packets: Uint8Array[] = [];
              let batchBytes = 4;
              for (const entry of ownedEntries) {
                const packet = encodeAppEnvelope({
                  session: binding.session,
                  sessionEpoch: BigInt(binding.sessionEpoch),
                  channel: binding.channel,
                  channelEpoch: BigInt(binding.channelEpoch),
                  messageKind: AppMessageKind.FrameworkPayload,
                  flags: 0,
                  sequence,
                  requestId: entry.requestId,
                }, entry.payload);
                if (packet.length > binding.maxPacketBytes) {
                  throw new Error('framework lane packet exceeds negotiated limit');
                }
                batchBytes += 4 + packet.length;
                if (batchBytes > 0xffff_ffff) {
                  throw new Error('framework lane batch exceeds encoding limit');
                }
                packets.push(packet);
                sequence += 1n;
              }
              const batch = new Uint8Array(batchBytes);
              const view = new DataView(batch.buffer);
              view.setUint32(0, packets.length, true);
              let cursor = 4;
              for (const packet of packets) {
                view.setUint32(cursor, packet.length, true);
                cursor += 4;
                batch.set(packet, cursor);
                cursor += packet.length;
              }
              await runtime.submitFrameworkLaneBatch(binding, batch, sessionId);
              nextInboundSequence = sequence;
            });
            submitChain = run.then(() => undefined, () => undefined);
            await run;
          },
          close(): void {
            closed = true;
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
        const file = selectVfsFile(vfsFiles, path);
        return file ? new Uint8Array(file.bytes) : null;
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
    framework: Object.freeze({
      name: framework.name,
      roles: Object.freeze([...framework.roles]),
      providerRoles: Object.freeze([...framework.providerRoles]),
    }),
    moduleBytes: new Uint8Array(moduleBytes),
    async sendEvent(handlerId: number, payload: string): Promise<Uint8Array> {
      emitRendererBridgeDebug(backend, `sendEvent handler=${handlerId} payload=${payload.slice(0, 160)}`);
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

// Fetch VFS snapshot once so multiple loaders can share the same files/root.
export async function fetchVfsSnapshot(
  backend: Backend,
  entryPath: string,
  sessionId?: number,
): Promise<VfsSnapshot> {
  const snapshot = await backend.getRendererBridgeVfsSnapshot(entryPath, sessionId);
  return validateVfsSnapshot({ rootPath: snapshot.rootPath, files: snapshot.files });
}

// Match a framework-declared path across the web absolute-path and native
// relative-path snapshot shapes. Suffix matches require a component boundary,
// and ambiguity is rejected so directory iteration order never selects code.
function selectVfsFile(files: VfsFile[], searchPath: string): VfsFile | null {
  const normalizedSearch = validateRendererVfsPath(searchPath, 'Renderer module search path');
  const exact = files.filter((file) => normalizeVfsPath(file.path) === normalizedSearch);
  if (exact.length > 1) {
    throw new Error(`Renderer VFS snapshot contains duplicate path ${normalizedSearch}`);
  }
  if (exact.length === 1) return exact[0];
  const matches = files.filter((file) => {
    const normalizedFile = normalizeVfsPath(file.path);
    return normalizedFile.endsWith(`/${normalizedSearch}`)
      || normalizedSearch.endsWith(`/${normalizedFile}`);
  });
  if (matches.length > 1) {
    throw new Error(
      `Renderer module path is ambiguous: ${searchPath} matches ${matches.map((file) => file.path).join(', ')}`,
    );
  }
  return matches[0] ?? null;
}

// Load renderer module + full VFS snapshot (framework-neutral).
// Returns [renderer, all snapshot files] so the host can serve getVfsBytes.
async function loadBrowserFrameworkModule(
  modulePath: string,
  files: VfsFile[],
  requireRender: boolean,
): Promise<[BrowserFrameworkModule, string[]]> {
  const { module, blobUrls } = await loadVfsModule<BrowserFrameworkModule>(modulePath, files, (raw) => {
    const mod = raw.default as Record<string, unknown> | undefined;
    if (!mod || typeof mod !== 'object') {
      throw new Error(`Browser framework module ${modulePath} must export a default object`);
    }
    const browserModule: BrowserFrameworkModule & Partial<RendererModule> = {
      init: requireFunction<BrowserFrameworkModule['init']>(mod.init, `${modulePath}.default.init`),
      stop: requireFunction<BrowserFrameworkModule['stop']>(mod.stop, `${modulePath}.default.stop`),
      quiesceForCapture: optionalFunction<NonNullable<BrowserFrameworkModule['quiesceForCapture']>>(mod.quiesceForCapture, `${modulePath}.default.quiesceForCapture`),
      registerWidget: optionalFunction<NonNullable<BrowserFrameworkModule['registerWidget']>>(mod.registerWidget, `${modulePath}.default.registerWidget`),
      destroyWidgets: optionalFunction<NonNullable<BrowserFrameworkModule['destroyWidgets']>>(mod.destroyWidgets, `${modulePath}.default.destroyWidgets`),
    };
    const render = optionalFunction<RendererModule['render']>(mod.render, `${modulePath}.default.render`);
    if (requireRender && !render) {
      throw new Error(`Renderer module ${modulePath} must export default.render`);
    }
    if (render) browserModule.render = render;
    return browserModule;
  });
  return [module, blobUrls];
}

type LoadedBrowserFrameworkModule = {
  framework: FrameworkContract;
  moduleName: string;
  module: BrowserFrameworkModule;
  blobUrls: string[];
};

const BROWSER_PROVIDER_MODULE_NAMES = ['logic', 'asset', 'audio', 'renderer'] as const;

function browserProviderModuleEnabled(
  framework: FrameworkContract,
  moduleName: (typeof BROWSER_PROVIDER_MODULE_NAMES)[number],
): boolean {
  if (moduleName === 'audio') {
    return framework.providerRoles.includes('game-audio')
      && framework.capabilities.includes('audio');
  }
  if (moduleName === 'asset') {
    return framework.providerRoles.includes('game-asset');
  }
  if (moduleName === 'logic') {
    return framework.providerRoles.includes('ui-logic');
  }
  return true;
}

async function loadBrowserFrameworkModules(
  frameworks: FrameworkContract[],
  files: VfsFile[],
): Promise<LoadedBrowserFrameworkModule[]> {
  const loaded: LoadedBrowserFrameworkModule[] = [];
  const seenModulePaths = new Set<string>();
  try {
    for (const framework of frameworks) {
      for (const moduleName of BROWSER_PROVIDER_MODULE_NAMES) {
        if (!browserProviderModuleEnabled(framework, moduleName)) {
          continue;
        }
        const modulePath = frameworkJsModulePath(framework, moduleName);
        if (!modulePath || seenModulePaths.has(modulePath)) {
          continue;
        }
        seenModulePaths.add(modulePath);
        const [module, blobUrls] = await loadBrowserFrameworkModule(
          modulePath,
          files,
          moduleName === 'renderer' || moduleName === 'logic',
        );
        loaded.push({ framework, moduleName, module, blobUrls });
      }
    }
    return loaded;
  } catch (error) {
    revokeBlobUrls(loaded.flatMap((entry) => entry.blobUrls));
    throw error;
  }
}

export function getWidgetFactory(sessionId: number, name: string): WidgetFactory | undefined {
  return activeRendererBridges.get(sessionId)?.widgetRegistry.get(name);
}

export function installPreparedFrameworkProviders(
  sessionId: number,
  moduleKeys: Iterable<string>,
): void {
  const keys = new Set(moduleKeys);
  if (keys.size === 0) {
    preparedFrameworkProviders.invalidate(sessionId);
    return;
  }
  preparedFrameworkProviders.set(sessionId, keys);
}

export function takePreparedFrameworkProviders(sessionId: number): Set<string> {
  return preparedFrameworkProviders.invalidate(sessionId) ?? new Set<string>();
}

export function clearPreparedFrameworkProviders(sessionId: number): void {
  preparedFrameworkProviders.invalidate(sessionId);
}

// Start renderer bridge with the framework's renderer loaded from VFS.
// No unconditional WASM loading — voWeb is a lazy capability inside RendererHost.
export async function startRendererBridge(
  canvasId: string,
  surfaceHost: HTMLElement,
  backend: Backend,
  runtime: RuntimeService,
  sessionId: number,
  context: RendererBridgeContext,
  vfsSnapshot?: VfsSnapshot,
): Promise<void> {
  await detachRendererBridge(sessionId, false, false);
  const providerLease = activeRendererBridges.begin(sessionId);
  const frameworks = collectRendererFrameworks(context);
  if (frameworks.length === 0) {
    throw new Error('No framework contract available');
  }

  const rendererFrameworks = frameworks.filter((framework) => frameworkJsModulePath(framework, 'renderer') != null);
  if (rendererFrameworks.length === 0) {
    throw new Error('No framework declares a renderer path');
  }

  const widgetRegistry = new Map<string, WidgetFactory>();
  const resolvedVfsSnapshot = validateVfsSnapshot(
    vfsSnapshot ?? (await fetchVfsSnapshot(backend, context.entryPath, sessionId)),
  );
  const resolvedVfsFiles = resolvedVfsSnapshot.files;
  const resolvedVfsRootPath = resolvedVfsSnapshot.rootPath;
  emitRendererBridgeDebug(backend, `studio_wasm.host_vfs.install files=${resolvedVfsFiles.length}`);
  setStudioWindowVfsBackendFactoryForSession(sessionId, () => createInMemoryWindowVfsBackend({
    rootPath: resolvedVfsRootPath,
    files: resolvedVfsFiles.map((file) => ({ path: file.path, bytes: file.bytes })),
  }));
  setStudioHostLogSinkForSession(sessionId, (record) => {
    if (handleVoplayPerfHostLog(record, sessionId)) {
      return;
    }
    emitRendererBridgeDebug(
      backend,
      `[${record.source}:${record.code}]${record.text ? ` ${record.text}` : ''}`,
    );
  });
  let sharedVoWebPromise: Promise<VoWebModule> | null = null;
  const getVoWebLazy = (): Promise<VoWebModule> => {
    if (!sharedVoWebPromise) {
      sharedVoWebPromise = (async () => {
        try {
          const wasm = await withHostBridgeSession(sessionId, () => loadStudioWasm());
          return makeVoWebModule(wasm, (exitCode) => {
            runtime.finishGuiGuestExit(sessionId, exitCode);
          }, sessionId);
        } catch (error) {
          sharedVoWebPromise = null;
          throw error;
        }
      })();
    }
    return sharedVoWebPromise;
  };
  const providerModuleKeys = [...new Set(rendererFrameworks.map((framework) => framework.moduleKey))];
  let loadedModules: Awaited<ReturnType<typeof loadBrowserFrameworkModules>> = [];
  let blobUrls: string[] = [];
  const initializedModules: BrowserFrameworkModule[] = [];
  const compositionHost = new AppCompositionHost(sessionId, surfaceHost);
  const loadedProviderModuleKeys = new Set<string>();
  const pendingProviderModuleKeys = new Set<string>();
  const readyProviderModuleKeys = new Set<string>();
  const preparedProviderModuleKeys = takePreparedFrameworkProviders(sessionId);
  try {
    loadedModules = await loadBrowserFrameworkModules(frameworks, resolvedVfsFiles);
    blobUrls = loadedModules.flatMap((entry) => entry.blobUrls);
    for (const moduleKey of providerModuleKeys) {
      if (context.reuseActiveProviders) {
        continue;
      }
      if (preparedProviderModuleKeys.has(moduleKey)) {
        loadedProviderModuleKeys.add(moduleKey);
        readyProviderModuleKeys.add(moduleKey);
        continue;
      }
      await runtime.loadFrameworkProvider(moduleKey, sessionId);
      loadedProviderModuleKeys.add(moduleKey);
      await runtime.beginFrameworkProvider(moduleKey, sessionId);
      pendingProviderModuleKeys.add(moduleKey);
    }
    emitRendererBridgeDebug(
      backend,
      `start session=${sessionId} modules=${loadedModules.map((entry) => `${entry.framework.name}:${entry.moduleName}`).join(',')}`,
    );
    const registerWidgetWithRenderers = (name: string, factory: WidgetFactory): void => {
      let bridged = 0;
      for (const module of initializedModules) {
        if (module.registerWidget) {
          module.registerWidget(name, factory);
          bridged += 1;
        }
      }
      emitRendererBridgeDebug(backend, `widget.bridge name=${name} bridged=${bridged}`);
    };
    const primaryRendererPath = context.framework
      ? frameworkJsModulePath(context.framework, 'renderer')
      : loadedModules.find((entry) => entry.moduleName === 'renderer')
        ? frameworkJsModulePath(
          loadedModules.find((entry) => entry.moduleName === 'renderer')!.framework,
          'renderer',
        )
        : null;
    const primaryRenderer = primaryRendererPath
      ? loadedModules.find((entry) => (
        entry.moduleName === 'renderer'
        && frameworkJsModulePath(entry.framework, 'renderer') === primaryRendererPath
      ))?.module as RendererModule | undefined ?? null
      : null;
    if (!primaryRenderer) {
      throw new Error(primaryRendererPath ? `Primary renderer was not loaded: ${primaryRendererPath}` : 'No primary renderer available');
    }
    const ingressModules = backend.platform === 'wasm'
      ? loadedModules
        .filter((entry) => entry.moduleName === 'logic')
        .map((entry) => entry.module as RendererModule)
      : [];
    for (const entry of loadedModules) {
      const host = makeRendererHost(
        canvasId,
        backend,
        runtime,
        sessionId,
        context.moduleBytes,
        resolvedVfsFiles,
        getVoWebLazy,
        entry.framework,
        widgetRegistry,
        compositionHost,
        context.onError,
        registerWidgetWithRenderers,
      );
      // init may allocate timers, channels, or widgets before rejecting.
      // Register first so the failing renderer participates in rollback.
      initializedModules.push(entry.module);
      for (const [name, factory] of widgetRegistry) {
        entry.module.registerWidget?.(name, factory);
      }
      await entry.module.init(host);
      emitRendererBridgeDebug(backend, `framework-module.init name=${entry.framework.name}:${entry.moduleName}`);
    }
    for (const moduleKey of providerModuleKeys) {
      if (context.reuseActiveProviders) {
        continue;
      }
      if (preparedProviderModuleKeys.has(moduleKey)) {
        continue;
      }
      await runtime.readyFrameworkProvider(moduleKey, sessionId);
      pendingProviderModuleKeys.delete(moduleKey);
      readyProviderModuleKeys.add(moduleKey);
    }
    const callGate = rendererCallGates.open(sessionId);
    const active = {
      primaryRenderer,
      ingressModules,
      renderers: initializedModules,
      blobUrls,
      sessionId,
      widgetRegistry,
      callGate,
      compositionHost,
      providerModuleKeys,
      runtime,
      lastRenderBytes: null,
      deliveryChain: Promise.resolve(),
      reportError: context.onError ?? (() => undefined),
      releaseSurface: () => context.onDetached?.(sessionId),
    };
    if (!activeRendererBridges.install(providerLease, active)) {
      void rendererCallGates.beginDrain(callGate);
      throw new Error(`renderer provider startup superseded for session ${sessionId}`);
    }
    void drainInactiveGameRender(active);
  } catch (error) {
    for (const renderer of initializedModules.reverse()) {
      try {
        renderer.destroyWidgets?.();
      } catch (destroyError) {
        console.error('[RendererBridge] renderer widget cleanup failed during init rollback:', destroyError);
      }
      try {
        renderer.stop();
      } catch (stopError) {
        console.error('[RendererBridge] renderer stop failed during init rollback:', stopError);
      }
    }
    widgetRegistry.clear();
    compositionHost.close();
    revokeBlobUrls(blobUrls);
    clearStudioHostLogSinkForSession(sessionId);
    clearStudioWindowVfsBackendFactoryForSession(sessionId);
    for (const moduleKey of [...pendingProviderModuleKeys].reverse()) {
      try {
        await runtime.abortFrameworkProvider(moduleKey, sessionId);
      } catch (providerError) {
        console.error('[RendererBridge] provider rollback failed:', providerError);
      }
    }
    for (const moduleKey of [...readyProviderModuleKeys].reverse()) {
      try {
        await runtime.closeFrameworkProvider(moduleKey, sessionId);
      } catch (providerError) {
        console.error('[RendererBridge] provider close failed during init rollback:', providerError);
      }
    }
    for (const moduleKey of [...loadedProviderModuleKeys].reverse()) {
      try {
        await runtime.unloadFrameworkProvider(moduleKey, sessionId);
      } catch (providerError) {
        console.error('[RendererBridge] provider factory unload failed during init rollback:', providerError);
      }
    }
    throw error;
  }
}

// Stop the active renderer bridge
export function stopRendererBridge(sessionId?: number | null): boolean {
  if (sessionId == null) {
    const sessions = activeRendererBridges.trackedSessionIds();
    const hadActiveRenderer = activeRendererBridges.size > 0;
    for (const session of sessions) stopRendererBridge(session);
    return hadActiveRenderer;
  }
  const active = activeRendererBridges.get(sessionId);
  if (!active) return false;
  void detachRendererBridge(sessionId, false, true);
  return true;
}

async function detachRendererBridge(
  sessionId: number,
  retainProviders: boolean,
  releaseSurface: boolean,
): Promise<boolean> {
  const pending = rendererBridgeTeardowns.get(sessionId);
  if (pending) await pending;
  const active = activeRendererBridges.get(sessionId);
  activeRendererBridges.invalidate(sessionId);
  if (!active) return false;
  const teardown = (async (): Promise<boolean> => {
    await rendererCallGates.beginDrain(active.callGate);
    try {
      for (const renderer of [...active.renderers].reverse()) {
        try {
          renderer.destroyWidgets?.();
        } catch (error) {
          console.error('[RendererBridge] renderer widget cleanup failed:', error);
        }
        try {
          renderer.stop();
        } catch (error) {
          console.error('[RendererBridge] renderer stop failed:', error);
        }
      }
    } finally {
      active.widgetRegistry.clear();
      active.compositionHost.close();
      revokeBlobUrls(active.blobUrls);
      clearStudioHostLogSinkForSession(active.sessionId);
      clearStudioWindowVfsBackendFactoryForSession(active.sessionId);
      if (releaseSurface) {
        active.releaseSurface();
      }
      if (!retainProviders) {
        for (const moduleKey of [...active.providerModuleKeys].reverse()) {
          try {
            await active.runtime.closeFrameworkProvider(moduleKey, active.sessionId);
          } catch (error) {
            console.error('[RendererBridge] provider close failed:', error);
          }
        }
        for (const moduleKey of [...active.providerModuleKeys].reverse()) {
          try {
            await active.runtime.unloadFrameworkProvider(moduleKey, active.sessionId);
          } catch (error) {
            console.error('[RendererBridge] provider factory unload failed:', error);
          }
        }
      }
    }
    return true;
  })();
  rendererBridgeTeardowns.set(sessionId, teardown);
  try {
    return await teardown;
  } finally {
    if (rendererBridgeTeardowns.get(sessionId) === teardown) {
      rendererBridgeTeardowns.delete(sessionId);
    }
  }
}

export async function restartRendererBridge(
  canvasId: string,
  surfaceHost: HTMLElement,
  backend: Backend,
  runtime: RuntimeService,
  sessionId: number,
  context: RendererBridgeContext,
  vfsSnapshot?: VfsSnapshot,
): Promise<void> {
  const active = activeRendererBridges.get(sessionId);
  if (!active) {
    throw new Error(`renderer bridge ${sessionId} is not active`);
  }
  await detachRendererBridge(sessionId, true, false);
  await runtime.restartComposedWebview(sessionId);
  await startRendererBridge(
    canvasId,
    surfaceHost,
    backend,
    runtime,
    sessionId,
    { ...context, reuseActiveProviders: true },
    vfsSnapshot,
  );
}

async function drainInactiveGameRender(active: ActiveRendererBridge): Promise<void> {
  while (activeRendererBridges.get(active.sessionId) === active) {
    try {
      if (active.runtime.isGuiSessionSelected(active.sessionId)) {
        await rendererBridgeDelay(16);
        continue;
      }
      await active.runtime.serviceDisplayTimingForSession(active.sessionId, true);
      const bytes = await active.runtime.pollGameRenderForSession(active.sessionId, true);
      if (activeRendererBridges.get(active.sessionId) !== active) return;
      if (bytes.byteLength === 0) {
        await rendererBridgeDelay(8);
        continue;
      }
      const result = await deliverGameRenderBytes(active.sessionId, bytes);
      if (result !== null) {
        await active.runtime.submitGameRenderResult(result, active.sessionId);
      }
    } catch (error) {
      if (activeRendererBridges.get(active.sessionId) !== active) return;
      if (isGuiSessionSupersededError(error)) {
        await detachRendererBridge(active.sessionId, true, true);
        return;
      }
      active.reportError(
        `background game renderer failed: ${error instanceof Error ? error.message : String(error)}`,
      );
      return;
    }
  }
}

function rendererBridgeDelay(milliseconds: number): Promise<void> {
  return new Promise((resolve) => window.setTimeout(resolve, milliseconds));
}

export function rendererBridgeSmokeState(): { active: boolean; renderers: unknown[]; sessionId: number | null } {
  const active = latestRendererBridge();
  if (!active) {
    return { active: false, renderers: [], sessionId: null };
  }
  return {
    active: true,
    renderers: active.renderers.map((renderer) => ({
      destroyWidgets: typeof renderer.destroyWidgets === 'function',
      quiesceForCapture: typeof renderer.quiesceForCapture === 'function',
      registerWidget: typeof renderer.registerWidget === 'function',
      stop: typeof renderer.stop === 'function',
    })),
    sessionId: active.sessionId,
  };
}

export function quiesceRendererBridgeForSmoke(): { renderers: unknown[]; stopped: number; sessionId: number | null } {
  const active = latestRendererBridge();
  if (!active) {
    return { renderers: [], stopped: 0, sessionId: null };
  }
  let stopped = 0;
  const renderers: unknown[] = [];
  for (const renderer of active.renderers) {
    if (typeof renderer.quiesceForCapture === 'function') {
      const result = renderer.quiesceForCapture();
      const rendererStopped = result?.stopped === undefined ? 1 : Number(result.stopped);
      if (!Number.isSafeInteger(rendererStopped) || rendererStopped < 1) {
        throw new Error('renderer quiesceForCapture must stop at least one render loop');
      }
      if (!Number.isSafeInteger(stopped + rendererStopped)) {
        throw new Error('renderer quiesceForCapture stopped-count overflow');
      }
      stopped += rendererStopped;
      renderers.push({ quiesceForCapture: true, stopped: rendererStopped, result: result ?? null });
    } else {
      renderers.push({ quiesceForCapture: false, stopped: 0 });
    }
  }
  return { renderers, stopped, sessionId: active.sessionId };
}

// Deliver render bytes to the active renderer
export function deliverRenderBytes(sessionId: number, container: HTMLElement, bytes: Uint8Array): void {
  const active = activeRendererBridges.get(sessionId);
  if (
    !active?.primaryRenderer
    || bytes.length === 0
    || active.lastRenderBytes === bytes
  ) return;
  active.lastRenderBytes = bytes;
  active.deliveryChain = active.deliveryChain.then(async () => {
    const ticket = rendererCallGates.enter(active.callGate);
    if (!ticket) return;
    const requiresCommitAck = active.ingressModules.length > 0;
    try {
      if (!requiresCommitAck) {
        await active.primaryRenderer!.render(container, bytes);
      } else {
        await active.primaryRenderer!.render(container, new Uint8Array());
        for (const module of active.ingressModules) {
          await module.render(container, bytes);
        }
        await active.runtime.completeVoguiTargetCommit(true, '', sessionId);
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      if (requiresCommitAck) {
        try {
          await active.runtime.completeVoguiTargetCommit(false, message, sessionId);
        } catch (completionError) {
          const completionMessage = completionError instanceof Error
            ? completionError.message
            : String(completionError);
          active.reportError(
            `Vogui provider rejection could not be delivered: ${completionMessage}`,
          );
        }
      }
      active.reportError(`Framework render ingress failed: ${message}`);
    } finally {
      ticket.release();
    }
  });
}

function latestRendererBridge(): ActiveRendererBridge | null {
  let latest: ActiveRendererBridge | null = null;
  for (const bridge of activeRendererBridges.values()) latest = bridge;
  return latest;
}

// ---- Generic VFS module loader ----

type VfsModuleResult<T> = { module: T; blobUrls: string[] };

async function loadVfsModule<T>(
  modulePath: string,
  files: VfsFile[],
  unwrap: (raw: Record<string, unknown>) => T,
): Promise<VfsModuleResult<T>> {
  const file = selectVfsFile(files, modulePath);
  if (!file) {
    throw new Error(`Module not found in VFS snapshot: ${modulePath}`);
  }
  const blobGraph = buildRendererBlobGraph(file, files);
  try {
    const raw = await import(/* @vite-ignore */ blobGraph.entryUrl);
    return { module: unwrap(raw), blobUrls: blobGraph.urls };
  } catch (error) {
    revokeBlobUrls(blobGraph.urls);
    throw error;
  }
}

// ---- Cached module slot helpers ----

type CachedModule<T> = { module: T; blobUrls: string[] };
type CachedModuleMap<T> = Map<string, CachedModule<T>>;
const vfsSnapshotIdentities = new WeakMap<VfsFile[], number>();
let nextVfsSnapshotIdentity = 1;

function vfsSnapshotIdentity(files: VfsFile[]): number {
  const existing = vfsSnapshotIdentities.get(files);
  if (existing !== undefined) return existing;
  if (!Number.isSafeInteger(nextVfsSnapshotIdentity) || nextVfsSnapshotIdentity > Number.MAX_SAFE_INTEGER) {
    throw new Error('Renderer VFS snapshot identity space is exhausted');
  }
  const identity = nextVfsSnapshotIdentity;
  nextVfsSnapshotIdentity += 1;
  vfsSnapshotIdentities.set(files, identity);
  return identity;
}

function moduleCacheKey(entryPath: string, modulePath: string, files: VfsFile[]): string {
  return `${entryPath}\0${modulePath}\0${vfsSnapshotIdentity(files)}`;
}

function clearCachedModules<T>(slots: CachedModuleMap<T>): void {
  for (const slot of slots.values()) {
    revokeBlobUrls(slot.blobUrls);
  }
  slots.clear();
}

// ---- Host bridge module loader ----

const activeHostBridges = new ProviderInstanceSet<CachedModuleMap<HostBridgeModule>>();

export async function loadHostBridgeModule(
  sessionId: number,
  hostBridgePath: string,
  backend: Backend,
  entryPath: string,
  prefetchedFiles?: VfsFile[],
): Promise<HostBridgeModule> {
  let sessionModules = activeHostBridges.get(sessionId);
  if (!sessionModules) {
    sessionModules = new Map();
    activeHostBridges.set(sessionId, sessionModules);
  }
  const files: VfsFile[] = prefetchedFiles
    ?? (await fetchVfsSnapshot(backend, entryPath, sessionId)).files;
  const key = moduleCacheKey(entryPath, hostBridgePath, files);
  const cached = sessionModules.get(key);
  if (cached) return cached.module;

  const { module, blobUrls } = await loadVfsModule<HostBridgeModule>(hostBridgePath, files, (raw) => ({
    buildImports: requireFunction<HostBridgeModule['buildImports']>(
      raw.buildImports,
      `${hostBridgePath}.buildImports`,
    ),
  }));
  sessionModules.set(key, { module, blobUrls });
  return module;
}

export function unloadHostBridgeModule(sessionId?: number): void {
  if (sessionId == null) {
    for (const modules of activeHostBridges.values()) clearCachedModules(modules);
    activeHostBridges.clear();
    return;
  }
  const modules = activeHostBridges.get(sessionId);
  if (!modules) return;
  clearCachedModules(modules);
  activeHostBridges.delete(sessionId);
}
