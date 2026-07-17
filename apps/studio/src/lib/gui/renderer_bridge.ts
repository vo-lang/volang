// Renderer bridge for Studio GUI runtime.
// Studio is framework-neutral: renderer modules are loaded dynamically
// from the VFS snapshot using a blob URL — no framework-specific imports.

import { parse as parseJavaScriptModule } from 'acorn';
import { listen as tauriListen } from '../tauri';
import type { Backend } from '../backend/backend';
import { handleVoplayPerfHostLog } from '../perf_report_bridge';
import { isGuiSessionSupersededError, type RuntimeService } from '../services/runtime_service';
import { frameworkContractKey, frameworkJsModulePath, type FrameworkContract } from '../types';
import type { VoWebModule } from '../studio_wasm';
import { loadStudioWasm, makeVoWebModule, resetStudioWasmInstance, setStudioWindowVfsBackendFactory } from '../studio_wasm';
import { createInMemoryWindowVfsBackend } from '../in_memory_window_vfs';

// ---- Protocol & HostBridge module contracts ----

export interface ProtocolModule {
  findHostWidgetHandlerId(bytes: Uint8Array): number | null;
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

// ---- RendererModule contract ----
// Frameworks must export an object implementing this interface from their
// renderer JS entry (declared in vo.mod [extension.web.js] renderer = "...").

export interface RendererModule {
  init(host: RendererHost): Promise<void>;
  render(container: HTMLElement, bytes: Uint8Array): void;
  stop(): void;
  quiesceForCapture?(): ({ stopped?: number } & Record<string, unknown>) | void;
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

let activeRendererBridge: ActiveRendererBridge = null;
const widgetRegistry = new Map<string, WidgetFactory>();
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
  moduleBytes: Uint8Array,
  vfsFiles: VfsFile[],
  getVoWebLazy: () => Promise<VoWebModule>,
  declaredCapabilities: string[],
  onError?: (message: string) => void,
  registerWidgetWithRenderers?: (name: string, factory: WidgetFactory) => void,
): RendererHost {
  const capSet = new Set(declaredCapabilities);
  exposeStudioBrowserSmokeRendererDebug(moduleBytes);

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
                const frame = await runTransport(() => runtime.pollIslandTransport());
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
          const frames = await runTransport(() => runtime.pushAndPollIslandTransport(frame));
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
              unlisten = await tauriListen<number[]>('island_data', (event) => {
                const frame = new Uint8Array(event.payload);
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
): Promise<VfsSnapshot> {
  const snapshot = await backend.getRendererBridgeVfsSnapshot(entryPath);
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
async function loadRendererModule(
  rendererPath: string,
  files: VfsFile[],
): Promise<[RendererModule, string[]]> {
  const { module: renderer, blobUrls } = await loadVfsModule<RendererModule>(rendererPath, files, (raw) => {
    const mod = raw.default as Record<string, unknown> | undefined;
    if (!mod || typeof mod !== 'object') {
      throw new Error(`Renderer module ${rendererPath} must export a default renderer object`);
    }
    return {
      init: requireFunction<RendererModule['init']>(mod.init, `${rendererPath}.default.init`),
      render: requireFunction<RendererModule['render']>(mod.render, `${rendererPath}.default.render`),
      stop: requireFunction<RendererModule['stop']>(mod.stop, `${rendererPath}.default.stop`),
      quiesceForCapture: optionalFunction<NonNullable<RendererModule['quiesceForCapture']>>(mod.quiesceForCapture, `${rendererPath}.default.quiesceForCapture`),
      registerWidget: optionalFunction<NonNullable<RendererModule['registerWidget']>>(mod.registerWidget, `${rendererPath}.default.registerWidget`),
      destroyWidgets: optionalFunction<NonNullable<RendererModule['destroyWidgets']>>(mod.destroyWidgets, `${rendererPath}.default.destroyWidgets`),
    };
  });
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
  vfsSnapshot?: VfsSnapshot,
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
  const resolvedVfsSnapshot = validateVfsSnapshot(
    vfsSnapshot ?? (await fetchVfsSnapshot(backend, context.entryPath)),
  );
  const resolvedVfsFiles = resolvedVfsSnapshot.files;
  const resolvedVfsRootPath = resolvedVfsSnapshot.rootPath;
  let sharedVoWebPromise: Promise<VoWebModule> | null = null;
  const getVoWebLazy = (): Promise<VoWebModule> => {
    if (!sharedVoWebPromise) {
      sharedVoWebPromise = (async () => {
        try {
          (globalThis as Record<string, unknown>).__voStudioLogRecord = (record: unknown) => {
            if (handleVoplayPerfHostLog(
              record as { code?: unknown; text?: unknown },
              sessionId,
            )) {
              return;
            }
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
              rootPath: resolvedVfsRootPath,
              files: resolvedVfsFiles.map((file) => ({ path: file.path, bytes: file.bytes })),
            }));
            resetStudioWasmInstance();
          }
          const wasm = await loadStudioWasm();
          return makeVoWebModule(wasm, (exitCode) => {
            runtime.finishGuiGuestExit(sessionId, exitCode);
          });
        } catch (error) {
          sharedVoWebPromise = null;
          throw error;
        }
      })();
    }
    return sharedVoWebPromise;
  };
  const loadedRenderers = await loadRendererModules(rendererFrameworks, resolvedVfsFiles);
  const blobUrls = loadedRenderers.flatMap((entry) => entry.blobUrls);
  const initializedRenderers: RendererModule[] = [];
  try {
    emitRendererBridgeDebug(
      backend,
      `start session=${sessionId} renderers=${loadedRenderers.map((entry) => `${entry.framework.name}:${frameworkJsModulePath(entry.framework, 'renderer')}`).join(',')}`,
    );
    const registerWidgetWithRenderers = (name: string, factory: WidgetFactory): void => {
      let bridged = 0;
      for (const renderer of initializedRenderers) {
        if (renderer.registerWidget) {
          renderer.registerWidget(name, factory);
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
      ? loadedRenderers.find((entry) => frameworkJsModulePath(entry.framework, 'renderer') === primaryRendererPath)?.renderer ?? null
      : null;
    if (!primaryRenderer) {
      throw new Error(primaryRendererPath ? `Primary renderer was not loaded: ${primaryRendererPath}` : 'No primary renderer available');
    }
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
      // init may allocate timers, channels, or widgets before rejecting.
      // Register first so the failing renderer participates in rollback.
      initializedRenderers.push(entry.renderer);
      for (const [name, factory] of widgetRegistry) {
        entry.renderer.registerWidget?.(name, factory);
      }
      await entry.renderer.init(host);
      emitRendererBridgeDebug(backend, `renderer.init name=${entry.framework.name}`);
    }
    activeRendererBridge = { primaryRenderer, renderers: initializedRenderers, blobUrls, sessionId };
  } catch (error) {
    for (const renderer of initializedRenderers.reverse()) {
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
    widgetRegistry.clear();
    revokeBlobUrls(active.blobUrls);
  }
  return true;
}

export function rendererBridgeSmokeState(): { active: boolean; renderers: unknown[]; sessionId: number | null } {
  const active = activeRendererBridge;
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
  const active = activeRendererBridge;
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

// ---- Protocol module loader ----

let activeProtocols: CachedModuleMap<ProtocolModule> = new Map();

export async function loadProtocolModule(
  protocolPath: string,
  backend: Backend,
  entryPath: string,
  prefetchedFiles?: VfsFile[],
): Promise<ProtocolModule> {
  const files: VfsFile[] = prefetchedFiles ?? (await fetchVfsSnapshot(backend, entryPath)).files;
  const key = moduleCacheKey(entryPath, protocolPath, files);
  const cached = activeProtocols.get(key);
  if (cached) return cached.module;

  const { module, blobUrls } = await loadVfsModule<ProtocolModule>(protocolPath, files, (raw) => ({
    findHostWidgetHandlerId: requireFunction<ProtocolModule['findHostWidgetHandlerId']>(
      raw.findHostWidgetHandlerId,
      `${protocolPath}.findHostWidgetHandlerId`,
    ),
  }));
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
  const files: VfsFile[] = prefetchedFiles ?? (await fetchVfsSnapshot(backend, entryPath)).files;
  const key = moduleCacheKey(entryPath, hostBridgePath, files);
  const cached = activeHostBridges.get(key);
  if (cached) return cached.module;

  const { module, blobUrls } = await loadVfsModule<HostBridgeModule>(hostBridgePath, files, (raw) => ({
    buildImports: requireFunction<HostBridgeModule['buildImports']>(
      raw.buildImports,
      `${hostBridgePath}.buildImports`,
    ),
  }));
  activeHostBridges.set(key, { module, blobUrls });
  return module;
}

export function unloadHostBridgeModule(): void {
  clearCachedModules(activeHostBridges);
}
