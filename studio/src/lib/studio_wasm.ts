// Loader for the studio WASM module (vo-studio-wasm wasm-pack output).
// The wasm-pack output is expected at /wasm/ (served from studio/public/wasm/).
// Build: wasm-pack build studio/wasm --target web --out-dir ../public/wasm

// Framework-specific host bridge imports (focus, blur, scrollTo, measureText, etc.)
// are injected dynamically via setActiveHostBridge() before WASM instantiation.
// No static import of any framework package.
import type { HostBridgeModule } from './gui/renderer_bridge';
import { createInMemoryWindowVfsBackend } from './in_memory_window_vfs';
import { hasWindowVfsBindings, installWindowVfsBackend, type WindowVfsBackend } from './window_vfs_bindings';

// ── VoVm instance interface (matches VoVm wasm-bindgen class exports) ────────

export interface VoVmInstance {
  run(): string;
  runScheduled(): string;
  pushIslandCommand(frame: Uint8Array): void;
  takeOutboundCommands(): Uint8Array[];
  takePendingHostEvents(): Array<{ token: string; delayMs: number; replay: boolean }>;
  wakeHostEvent(token: string): void;
  takeOutput(): string;
}

// ── StudioWasm — full set of wasm-bindgen exports from vo-studio-wasm ────────

export interface StudioWasm {
  // Legacy singleton-based island API (still used for non-VoWebModule paths)
  runGuiFromBytecode(bytecode: Uint8Array): Uint8Array;
  startGuiFromBytecode(bytecode: Uint8Array): Uint8Array;
  runGui(entryPath: string): {
    renderBytes: Uint8Array;
    moduleBytes: Uint8Array;
    entryPath: string;
    framework: { name: string; entry: string; capabilities: string[]; rendererPath: string | null; protocolPath: string | null; hostBridgePath: string | null } | null;
    providerFrameworks: Array<{ name: string; entry: string; capabilities: string[]; rendererPath: string | null; protocolPath: string | null; hostBridgePath: string | null }>;
    externalWidgetHandlerId: number | null;
  };
  runGuiEntry(entryPath: string): Uint8Array;
  sendGuiEvent(handlerId: number, payload: string): Uint8Array;
  sendGuiEventAsync(handlerId: number, payload: string): void;
  startRenderIsland(bytecode: Uint8Array): void;
  pushIslandData(data: Uint8Array): void;
  pollGuiRender(): Uint8Array;
  getRenderIslandVfsSnapshot(entryPath: string): {
    rootPath: string;
    files: Array<{ path: string; bytes: Uint8Array }>;
  };
  pollIslandData(): Uint8Array;
  pollPendingHostEvent(): { token: string; delayMs: number } | null;
  wakeHostEvent(token: string): void;
  stopGui(): void;
  // Console run (compile + execute, returns stdout)
  compileRunEntry(entryPath: string): string;
  // Instance-based VM (VoWebModule interface)
  VoVm: { withExterns(bytecode: Uint8Array): VoVmInstance };
  preloadExtModule(path: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void>;
  prepareEntry(entryPath: string): Promise<void>;
  compileGui(entryPath: string): {
    bytecode: Uint8Array;
    entryPath: string;
    framework: { name: string; entry: string; capabilities: string[]; rendererPath: string | null; protocolPath: string | null; hostBridgePath: string | null } | null;
    providerFrameworks: Array<{ name: string; entry: string; capabilities: string[]; rendererPath: string | null; protocolPath: string | null; hostBridgePath: string | null }>;
    wasmExtensions: Array<{ name: string; moduleKey: string; wasmBytes: Uint8Array; jsGlueBytes: Uint8Array | null }>;
  };
  getBuildId(): string;
  initVFS(): Promise<void>;
}

type RawStudioWasmModule = Partial<StudioWasm> & {
  default: (wasmPath?: string) => Promise<void>;
  StudioVoVm?: { withExterns(bytecode: Uint8Array): VoVmInstance };
  VoVm?: { withExterns(bytecode: Uint8Array): VoVmInstance };
  VoVmIsland?: { withExterns(bytecode: Uint8Array): VoVmInstance };
};

// ── VoWebModule — framework-neutral VM capability surface ─────────────────────

export interface VoWebModule {
  initVFS(): Promise<void>;
  VoVm: { withExterns(bytecode: Uint8Array): VoVmInstance };
  preloadExtModule(path: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void>;
}

const bundledStudioBuildId = __STUDIO_BUILD_ID__;
let studioAssetBuildIdPromise: Promise<string> | null = null;

type StudioHostLogRecord = {
  source: string;
  code: string;
  level: string;
  text?: string;
};

function emitStudioHostLog(record: StudioHostLogRecord): void {
  const hook = (globalThis as Record<string, unknown>).__voStudioLogRecord;
  if (typeof hook === 'function') {
    (hook as (record: StudioHostLogRecord) => void)(record);
    return;
  }
  if (record.text) {
    console.debug(`[${record.source}:${record.code}] ${record.text}`);
    return;
  }
  console.debug(`[${record.source}:${record.code}]`);
}

function shouldTraceStandaloneExtern(externName: string): boolean {
  return externName.includes('HasHostCapability') || externName.endsWith('waitForEvent');
}

async function resolveJsGlueImportUrl(jsGlueUrl: string): Promise<{ importUrl: string; revoke(): void }> {
  if (!jsGlueUrl) {
    return { importUrl: '', revoke() {} };
  }
  if (jsGlueUrl.startsWith('blob:') || jsGlueUrl.startsWith('data:')) {
    return {
      importUrl: jsGlueUrl,
      revoke(): void {},
    };
  }
  const response = await fetch(jsGlueUrl, { cache: 'no-store' });
  if (!response.ok) {
    throw new Error(`Failed to fetch JS glue: HTTP ${response.status}`);
  }
  const jsText = await response.text();
  const blobUrl = URL.createObjectURL(new Blob([jsText], { type: 'application/javascript' }));
  return {
    importUrl: blobUrl,
    revoke(): void {
      URL.revokeObjectURL(blobUrl);
    },
  };
}

function withBuildId(path: string, buildId: string): string {
  const separator = path.includes('?') ? '&' : '?';
  return `${path}${separator}build=${encodeURIComponent(buildId)}`;
}

async function getStudioAssetBuildId(): Promise<string> {
  if (!import.meta.env.DEV) {
    return bundledStudioBuildId;
  }
  if (studioAssetBuildIdPromise) {
    return studioAssetBuildIdPromise;
  }
  studioAssetBuildIdPromise = (async () => {
    emitStudioHostLog({
      source: 'studio-wasm',
      code: 'asset_build_id_fetch_begin',
      level: 'system',
    });
    const response = await fetch(withBuildId('/wasm/vo_studio_wasm.build_id', Date.now().toString(36)), {
      cache: 'no-store',
    });
    if (!response.ok) {
      throw new Error(`Failed to load Studio WASM build id: ${response.status} ${response.statusText}`);
    }
    const buildId = (await response.text()).trim();
    if (!buildId) {
      throw new Error('Failed to load Studio WASM build id: empty response');
    }
    emitStudioHostLog({
      source: 'studio-wasm',
      code: 'asset_build_id_fetch_ready',
      level: 'system',
      text: `buildId=${buildId}`,
    });
    return buildId;
  })();
  return studioAssetBuildIdPromise;
}

function assertStudioBuildMatch(expectedBuildId: string, wasmBuildId: string, expectedSource: string): void {
  if (wasmBuildId !== expectedBuildId) {
    throw new Error(
      `Studio web asset mismatch: ${expectedSource} expects ${expectedBuildId}, wasm provides ${wasmBuildId}. Reload after the latest deploy. In local dev, rebuild studio/wasm and retry; a dev server restart should only be needed if the mismatch persists.`,
    );
  }
}

type StudioWindowVfsFactory = () => WindowVfsBackend;

let studioWindowVfsFactory: StudioWindowVfsFactory | null = null;
let studioWindowVfsRevision = 0;
let studioWindowVfsInstalledRevision = -1;

function ensureStudioWindowVfsBindings(): void {
  if (studioWindowVfsFactory) {
    if (studioWindowVfsInstalledRevision !== studioWindowVfsRevision) {
      installWindowVfsBackend(studioWindowVfsFactory());
      studioWindowVfsInstalledRevision = studioWindowVfsRevision;
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'host_vfs_provider_ready',
        level: 'system',
      });
    }
    return;
  }
  if (hasWindowVfsBindings()) {
    return;
  }
  installWindowVfsBackend(createInMemoryWindowVfsBackend());
  studioWindowVfsInstalledRevision = studioWindowVfsRevision;
  emitStudioHostLog({
    source: 'studio-wasm',
    code: 'host_vfs_fallback_ready',
    level: 'system',
  });
}

export function setStudioWindowVfsBackendFactory(factory: StudioWindowVfsFactory | null): void {
  studioWindowVfsFactory = factory;
  studioWindowVfsRevision += 1;
}

// ── Ext-bridge JS globals (mirrors playground/src/wasm/vo.ts) ─────────────────

type BindgenModule = Record<string, unknown> & {
  __voDispose?: () => void;
};

// Maps normalized module key → WebAssembly.Instance (standalone C-ABI modules).
const extInstances = new Map<string, WebAssembly.Instance>();
// Maps normalized module key → wasm-bindgen module (DOM/WebGPU access).
const extBindgenModules = new Map<string, BindgenModule>();

let extBridgeInstalled = false;

type StandaloneGuiEventDispatcher = (handlerId: number, payload: string) => Promise<void>;
type StandaloneGameLoopState = { rafId: number; lastTs: number };
let standaloneGuiEventDispatcher: StandaloneGuiEventDispatcher | null = null;
let standalonePopstateHandler: (() => void) | null = null;

function clearStandaloneHostState(): void {
  for (const handle of standaloneTimers.values()) {
    clearTimeout(handle);
  }
  standaloneTimers.clear();
  for (const handle of standaloneIntervals.values()) {
    clearInterval(handle);
  }
  standaloneIntervals.clear();
  standaloneRunningIntervalHandlers.clear();
  for (const handle of standaloneAnimFrames.values()) {
    cancelAnimationFrame(handle);
  }
  standaloneAnimFrames.clear();
  for (const loop of standaloneGameLoops.values()) {
    cancelAnimationFrame(loop.rafId);
  }
  standaloneGameLoops.clear();
}

function dispatchStandaloneGuiEventAsync(handlerId: number, payload: string): Promise<void> {
  const dispatcher = standaloneGuiEventDispatcher;
  if (!dispatcher) {
    return Promise.resolve();
  }
  return dispatcher(handlerId, payload);
}

function fireAndForgetStandaloneGuiEvent(handlerId: number, payload: string, label: string): void {
  void dispatchStandaloneGuiEventAsync(handlerId, payload).catch((error) => {
    console.error(`[vogui host] ${label} failed:`, error);
  });
}

export function setStandaloneGuiEventDispatcher(dispatcher: StandaloneGuiEventDispatcher | null): void {
  clearStandaloneHostState();
  if (standalonePopstateHandler) {
    window.removeEventListener('popstate', standalonePopstateHandler);
    standalonePopstateHandler = null;
  }
  standaloneGuiEventDispatcher = dispatcher;
  if (!dispatcher) {
    return;
  }
  standalonePopstateHandler = () => {
    fireAndForgetStandaloneGuiEvent(-3, JSON.stringify({ path: window.location.pathname }), 'navigation');
  };
  window.addEventListener('popstate', standalonePopstateHandler);
}

function removeStandaloneModuleEntries(instance: WebAssembly.Instance): void {
  for (const [key, value] of Array.from(extInstances.entries())) {
    if (value === instance) {
      extInstances.delete(key);
    }
  }
}

function removeBindgenModuleEntries(module: BindgenModule): void {
  for (const [key, value] of Array.from(extBindgenModules.entries())) {
    if (value === module) {
      extBindgenModules.delete(key);
    }
  }
}

function disposeBindgenModule(module: BindgenModule): void {
  try {
    module.__voDispose?.();
  } catch (error) {
    console.error('[voDisposeExtModule] bindgen dispose failed:', error);
  }
}

function unloadExtModule(key: string): void {
  const bindgenModule = extBindgenModules.get(key);
  if (bindgenModule) {
    disposeBindgenModule(bindgenModule);
    removeBindgenModuleEntries(bindgenModule);
  }
  const instance = extInstances.get(key);
  if (instance) {
    removeStandaloneModuleEntries(instance);
  }
}

// Late-binding reference for standalone WASM host imports.
// The imports capture this ref; after instantiation the caller sets ref.instance
// so that host functions can access the WASM linear memory.
interface StandaloneRef {
  instance: WebAssembly.Instance | null;
}

function readWasmString(ref: StandaloneRef, ptr: number, len: number): string {
  const mem = (ref.instance!.exports.memory as WebAssembly.Memory).buffer;
  return new TextDecoder().decode(new Uint8Array(mem, ptr, len));
}

function wasmAlloc(ref: StandaloneRef, size: number): number {
  return (ref.instance!.exports.vo_alloc as (size: number) => number)(size);
}

// Active timers/intervals/animation-frames/game-loops keyed by the Vo-level id.
const standaloneTimers = new Map<number, ReturnType<typeof setTimeout>>();
const standaloneIntervals = new Map<number, ReturnType<typeof setInterval>>();
const standaloneRunningIntervalHandlers = new Set<number>();
const standaloneAnimFrames = new Map<number, number>();
const standaloneGameLoops = new Map<number, StandaloneGameLoopState>();

// Host bridge module injected by the framework at render-island launch time.
// buildStandaloneImports() always includes lazy bridge wrappers so standalone WASM
// can always instantiate; wrappers forward to the active bridge once it is set.
let activeHostBridgeModule: HostBridgeModule | null = null;
let _measureTextFirstCallLogged = false;

export function setActiveHostBridge(mod: HostBridgeModule | null): void {
  activeHostBridgeModule = mod;
}

export function clearActiveHostBridge(): void {
  activeHostBridgeModule = null;
}

function buildStandaloneImports(): WebAssembly.Imports {
  const ref: StandaloneRef = { instance: null };

  // Per-instance bridge context and lazy-rebuild cache.
  // bridgeCtx captures ref so host functions share this instance's WASM memory.
  // getBridgeImports() rebuilds only when activeHostBridgeModule changes.
  const bridgeCtx = {
    readString: (ptr: number, len: number) => readWasmString(ref, ptr, len),
    alloc: (size: number) => wasmAlloc(ref, size),
    writeBytes: (destPtr: number, bytes: Uint8Array) => {
      new Uint8Array((ref.instance!.exports.memory as WebAssembly.Memory).buffer, destPtr, bytes.length).set(bytes);
    },
    writeU32: (ptr: number, value: number) => {
      new Uint32Array((ref.instance!.exports.memory as WebAssembly.Memory).buffer, ptr, 1)[0] = value;
    },
  };
  let cachedBridgeMod: HostBridgeModule | null = null;
  let cachedBridgeImports: Record<string, (...args: number[]) => number | void> | null = null;
  function getBridgeImports() {
    if (activeHostBridgeModule !== cachedBridgeMod) {
      cachedBridgeMod = activeHostBridgeModule;
      cachedBridgeImports = activeHostBridgeModule ? activeHostBridgeModule.buildImports(bridgeCtx) : null;
    }
    return cachedBridgeImports;
  }

  // Stash the ref so the caller can bind it after instantiation.
  // We use a convention: the returned object has a hidden __ref property.
  const imports: WebAssembly.Imports & { __ref?: StandaloneRef } = {
    env: {
      host_start_timeout(id: number, ms: number): void {
        if (!standaloneGuiEventDispatcher) {
          return;
        }
        const existing = standaloneTimers.get(id);
        if (existing !== undefined) {
          clearTimeout(existing);
        }
        const handle = setTimeout(() => {
          standaloneTimers.delete(id);
          fireAndForgetStandaloneGuiEvent(-1, JSON.stringify({ id }), `timeout:${id}`);
        }, ms);
        standaloneTimers.set(id, handle);
      },
      host_clear_timeout(id: number): void {
        const handle = standaloneTimers.get(id);
        if (handle !== undefined) { clearTimeout(handle); standaloneTimers.delete(id); }
      },
      host_start_interval(id: number, ms: number): void {
        if (!standaloneGuiEventDispatcher) {
          return;
        }
        const existing = standaloneIntervals.get(id);
        if (existing !== undefined) {
          clearInterval(existing);
        }
        standaloneRunningIntervalHandlers.delete(id);
        const handle = setInterval(() => {
          if (standaloneRunningIntervalHandlers.has(id)) {
            return;
          }
          standaloneRunningIntervalHandlers.add(id);
          void dispatchStandaloneGuiEventAsync(-1, JSON.stringify({ id }))
            .catch((error) => {
              console.error(`[vogui host] interval:${id} failed:`, error);
            })
            .finally(() => {
              standaloneRunningIntervalHandlers.delete(id);
            });
        }, ms);
        standaloneIntervals.set(id, handle);
      },
      host_clear_interval(id: number): void {
        const handle = standaloneIntervals.get(id);
        if (handle !== undefined) { clearInterval(handle); standaloneIntervals.delete(id); }
        standaloneRunningIntervalHandlers.delete(id);
      },
      host_has_host_capability(ptr: number, len: number): number {
        const name = readWasmString(ref, ptr, len);
        const result = name === 'external_island_host' && standaloneGuiEventDispatcher ? 1 : 0;
        if (name === 'external_island_host') {
          emitStudioHostLog({
            source: 'studio-extbridge',
            code: 'host_capability_query',
            level: result === 1 ? 'success' : 'error',
            text: `name=${name} dispatcher=${standaloneGuiEventDispatcher ? 'set' : 'null'} result=${result}`,
          });
        }
        return result;
      },
      host_navigate(ptr: number, len: number): void {
        const path = readWasmString(ref, ptr, len);
        if (window.location.pathname !== path) {
          window.history.pushState({}, '', path);
        }
      },
      host_get_current_path(outLenPtr: number): number {
        const path = window.location.pathname;
        const encoded = new TextEncoder().encode(path);
        const destPtr = wasmAlloc(ref, encoded.length);
        const mem = (ref.instance!.exports.memory as WebAssembly.Memory).buffer;
        new Uint8Array(mem, destPtr, encoded.length).set(encoded);
        new Uint32Array(mem, outLenPtr, 1)[0] = encoded.length;
        return destPtr;
      },
      host_set_title(ptr: number, len: number): void {
        document.title = readWasmString(ref, ptr, len);
      },
      host_set_meta(namePtr: number, nameLen: number, contentPtr: number, contentLen: number): void {
        const name = readWasmString(ref, namePtr, nameLen);
        const content = readWasmString(ref, contentPtr, contentLen);
        let el = document.querySelector(`meta[name="${name}"]`) as HTMLMetaElement | null;
        if (!el) { el = document.createElement('meta'); el.name = name; document.head.appendChild(el); }
        el.content = content;
      },
      host_toast(msgPtr: number, msgLen: number, _typPtr: number, _typLen: number, _durationMs: number): void {
        const msg = readWasmString(ref, msgPtr, msgLen);
        console.log('[vogui host] toast:', msg);
      },
      host_start_anim_frame(id: number): void {
        if (!standaloneGuiEventDispatcher) {
          return;
        }
        const existing = standaloneAnimFrames.get(id);
        if (existing !== undefined) {
          cancelAnimationFrame(existing);
        }
        const handle = requestAnimationFrame(() => {
          standaloneAnimFrames.delete(id);
          fireAndForgetStandaloneGuiEvent(-4, JSON.stringify({ Id: id }), `anim_frame:${id}`);
        });
        standaloneAnimFrames.set(id, handle);
      },
      host_cancel_anim_frame(id: number): void {
        const handle = standaloneAnimFrames.get(id);
        if (handle !== undefined) { cancelAnimationFrame(handle); standaloneAnimFrames.delete(id); }
      },
      host_start_game_loop(id: number): void {
        if (!standaloneGuiEventDispatcher) {
          return;
        }
        const existing = standaloneGameLoops.get(id);
        if (existing) {
          cancelAnimationFrame(existing.rafId);
        }
        const state: StandaloneGameLoopState = { rafId: 0, lastTs: 0 };
        standaloneGameLoops.set(id, state);
        const tick = (ts: number): void => {
          const loop = standaloneGameLoops.get(id);
          if (!loop) {
            return;
          }
          const dt = loop.lastTs === 0 ? 0 : ts - loop.lastTs;
          loop.lastTs = ts;
          dispatchStandaloneGuiEventAsync(-5, JSON.stringify({ Dt: dt })).then(
            () => {
              if (standaloneGameLoops.has(id)) {
                standaloneGameLoops.get(id)!.rafId = requestAnimationFrame(tick);
              }
            },
            (error) => {
              console.error(`[vogui host] game_loop:${id} failed:`, error);
              standaloneGameLoops.delete(id);
            },
          );
        };
        state.rafId = requestAnimationFrame(tick);
      },
      host_stop_game_loop(id: number): void {
        const loop = standaloneGameLoops.get(id);
        if (loop !== undefined) { cancelAnimationFrame(loop.rafId); standaloneGameLoops.delete(id); }
      },
      // Bridge functions — always present so standalone WASM can always instantiate.
      // Forward to activeHostBridgeModule lazily; safe zero-fallbacks when not yet set.
      host_focus(ptr: number, len: number): void {
        getBridgeImports()?.host_focus?.(ptr, len);
      },
      host_blur(ptr: number, len: number): void {
        getBridgeImports()?.host_blur?.(ptr, len);
      },
      host_scroll_to(ptr: number, len: number, top: number): void {
        getBridgeImports()?.host_scroll_to?.(ptr, len, top);
      },
      host_scroll_into_view(ptr: number, len: number): void {
        getBridgeImports()?.host_scroll_into_view?.(ptr, len);
      },
      host_select_text(ptr: number, len: number): void {
        getBridgeImports()?.host_select_text?.(ptr, len);
      },
      host_measure_text(
        textPtr: number, textLen: number,
        fontPtr: number, fontLen: number,
        maxWidth: number, lineHeight: number,
        whiteSpace: number,
        outLenPtr: number,
      ): number {
        const bridge = getBridgeImports();
        if (!_measureTextFirstCallLogged) {
          _measureTextFirstCallLogged = true;
          console.log('[host_measure_text] first call, bridge=', bridge ? 'loaded' : 'null', 'activeHostBridgeModule=', activeHostBridgeModule ? 'set' : 'null');
        }
        if (bridge?.host_measure_text) {
          return (bridge.host_measure_text as (...args: number[]) => number)(
            textPtr, textLen, fontPtr, fontLen, maxWidth, lineHeight, whiteSpace, outLenPtr,
          );
        }
        throw new Error('host_measure_text bridge missing');
      },
      host_measure_text_lines(
        textPtr: number, textLen: number,
        fontPtr: number, fontLen: number,
        maxWidth: number, lineHeight: number,
        whiteSpace: number,
        outLenPtr: number,
      ): number {
        const bridge = getBridgeImports();
        if (bridge?.host_measure_text_lines) {
          return (bridge.host_measure_text_lines as (...args: number[]) => number)(
            textPtr, textLen, fontPtr, fontLen, maxWidth, lineHeight, whiteSpace, outLenPtr,
          );
        }
        throw new Error('host_measure_text_lines bridge missing');
      },
    },
  };
  imports.__ref = ref;
  return imports;
}

function unloadAllExtModules(): void {
  const bindgenModules = Array.from(new Set(extBindgenModules.values()));
  for (const module of bindgenModules) {
    disposeBindgenModule(module);
  }
  extBindgenModules.clear();
  extInstances.clear();
}

function encodeStandaloneTagValueI64(value: number): Uint8Array {
  const bytes = new Uint8Array(9);
  bytes[0] = 0xE2;
  new DataView(bytes.buffer).setBigUint64(1, BigInt.asUintN(64, BigInt(value)), true);
  return bytes;
}

function encodeStandaloneTagBytes(bytes: Uint8Array): Uint8Array {
  const result = new Uint8Array(5 + bytes.length);
  result[0] = 0xE3;
  new DataView(result.buffer).setUint32(1, bytes.length, true);
  result.set(bytes, 5);
  return result;
}

function decodeWaitForEventReplayOutput(resumeData: Uint8Array): Uint8Array {
  if (resumeData.length < 4) {
    throw new Error(`invalid waitForEvent replay payload: expected at least 4 bytes, got ${resumeData.length}`);
  }
  const handlerId = new DataView(resumeData.buffer, resumeData.byteOffset, resumeData.byteLength).getInt32(0, true);
  const payloadBytes = resumeData.slice(4);
  const handlerOutput = encodeStandaloneTagValueI64(handlerId);
  const payloadOutput = encodeStandaloneTagBytes(payloadBytes);
  const output = new Uint8Array(handlerOutput.length + payloadOutput.length);
  output.set(handlerOutput, 0);
  output.set(payloadOutput, handlerOutput.length);
  return output;
}

function throwVoCallExtFailure(message: string, cause?: unknown): never {
  if (cause !== undefined) {
    console.error(message, cause);
    const detail = cause instanceof Error ? cause.message : String(cause);
    throw new Error(`${message}: ${detail}`);
  }
  console.error(message);
  throw new Error(message);
}

function installExtBridgeGlobals(): void {
  if (extBridgeInstalled) return;
  extBridgeInstalled = true;

  (window as unknown as Record<string, unknown>).voSetupExtModule = async (
    key: string,
    bytes: Uint8Array,
    jsGlueUrl?: string,
  ): Promise<void> => {
    unloadExtModule(key);
    emitStudioHostLog({
      source: 'studio-extbridge',
      code: 'ext_module_setup_begin',
      level: 'system',
      text: `key=${key} bindgen=${jsGlueUrl ? 'yes' : 'no'} bytes=${bytes.byteLength}`,
    });
    if (jsGlueUrl) {
      const { importUrl, revoke } = await resolveJsGlueImportUrl(jsGlueUrl);
      try {
        emitStudioHostLog({
          source: 'studio-extbridge',
          code: 'ext_module_glue_import_begin',
          level: 'system',
          text: `key=${key}`,
        });
        const glue = await import(/* @vite-ignore */ importUrl) as BindgenModule;
        emitStudioHostLog({
          source: 'studio-extbridge',
          code: 'ext_module_glue_import_ready',
          level: 'system',
          text: `key=${key}`,
        });
        emitStudioHostLog({
          source: 'studio-extbridge',
          code: 'ext_module_bindgen_instantiate_begin',
          level: 'system',
          text: `key=${key}`,
        });
        await (glue.default as (opts: { module_or_path: Uint8Array }) => Promise<void>)({
          module_or_path: bytes.slice(),
        });
        emitStudioHostLog({
          source: 'studio-extbridge',
          code: 'ext_module_bindgen_instantiate_ready',
          level: 'system',
          text: `key=${key}`,
        });
        if (typeof glue.__voInit === 'function') {
          emitStudioHostLog({
            source: 'studio-extbridge',
            code: 'ext_module_async_init_begin',
            level: 'system',
            text: `key=${key}`,
          });
          await (glue.__voInit as () => Promise<void>)();
          emitStudioHostLog({
            source: 'studio-extbridge',
            code: 'ext_module_async_init_ready',
            level: 'system',
            text: `key=${key}`,
          });
        }
        extBindgenModules.set(key, glue);
        emitStudioHostLog({
          source: 'studio-extbridge',
          code: 'ext_module_setup_ready',
          level: 'system',
          text: `key=${key} mode=bindgen`,
        });
      } finally {
        revoke();
      }
    } else {
      const imports = buildStandaloneImports();
      const { instance } = await WebAssembly.instantiate(bytes.slice(), imports);
      // Bind the late reference so host imports can access this instance's memory.
      const ref = (imports as { __ref?: { instance: WebAssembly.Instance | null } }).__ref;
      if (ref) ref.instance = instance;
      extInstances.set(key, instance);
      emitStudioHostLog({
        source: 'studio-extbridge',
        code: 'ext_module_setup_ready',
        level: 'system',
        text: `key=${key} mode=standalone`,
      });
    }
  };

  (window as unknown as Record<string, unknown>).voRegisterExtModuleAlias = (
    existingKey: string,
    aliasKey: string,
  ): void => {
    if (!aliasKey || aliasKey === existingKey) return;
    if (extBindgenModules.has(existingKey)) {
      extBindgenModules.set(aliasKey, extBindgenModules.get(existingKey)!);
    }
    if (extInstances.has(existingKey)) {
      extInstances.set(aliasKey, extInstances.get(existingKey)!);
    }
  };

  (window as unknown as Record<string, unknown>).voDisposeExtModule = (key: string): void => {
    unloadExtModule(key);
  };

  (window as unknown as Record<string, unknown>).voDisposeAllExtModules = (): void => {
    unloadAllExtModules();
  };

  (window as unknown as Record<string, unknown>).voCallExt = (
    externName: string,
    input: Uint8Array,
  ): Uint8Array => {
    const traceExtern = shouldTraceStandaloneExtern(externName);
    // Try wasm-bindgen modules first
    let bindgenModule: Record<string, unknown> | undefined;
    let bindgenKey = '';
    for (const [key, mod] of extBindgenModules) {
      if (externName.startsWith(key) && key.length > bindgenKey.length) {
        bindgenKey = key;
        bindgenModule = mod as Record<string, unknown>;
      }
    }
    if (bindgenModule) {
      const funcName = externName.substring(bindgenKey.length + 1);
      if (traceExtern) {
        emitStudioHostLog({
          source: 'studio-extbridge',
          code: 'ext_call_bindgen_match',
          level: 'system',
          text: `extern=${externName} key=${bindgenKey} func=${funcName}`,
        });
      }
      if (typeof bindgenModule[funcName] === 'function') {
        let result: unknown;
        try {
          result = (bindgenModule[funcName] as (i: Uint8Array) => unknown)(input);
        } catch (e) {
          throwVoCallExtFailure(`[voCallExt] Exception calling ${externName}`, e);
        }
        if (result instanceof Promise) {
          throwVoCallExtFailure(`[voCallExt] Async bindgen export is not supported for ${externName}`);
        }
        if (result instanceof Uint8Array) return result;
        if (typeof result === 'string') return new TextEncoder().encode(result);
        throwVoCallExtFailure(`[voCallExt] Unsupported bindgen return for ${externName}: ${typeof result}`);
      }
      throwVoCallExtFailure(`[voCallExt] Bindgen export not found: ${funcName} in module: ${bindgenKey}`);
    }

    // Fall back to standalone C-ABI modules
    let instance: WebAssembly.Instance | undefined;
    let matchedKey = '';
    for (const [key, inst] of extInstances) {
      if (externName.startsWith(key) && key.length > matchedKey.length) {
        matchedKey = key;
        instance = inst;
      }
    }
    if (!instance) {
      emitStudioHostLog({
        source: 'studio-extbridge',
        code: 'ext_call_no_module',
        level: 'error',
        text: `extern=${externName} standalone=[${Array.from(extInstances.keys()).join(',')}] bindgen=[${Array.from(extBindgenModules.keys()).join(',')}]`,
      });
      throwVoCallExtFailure(`[voCallExt] No loaded module for extern: ${externName}`);
    }
    const exp = instance.exports as Record<string, unknown>;
    const funcName = externName.substring(matchedKey.length + 1);
    if (traceExtern) {
      emitStudioHostLog({
        source: 'studio-extbridge',
        code: 'ext_call_standalone_match',
        level: 'system',
        text: `extern=${externName} key=${matchedKey} func=${funcName}`,
      });
    }
    let extFunc: ((ptr: number, len: number, outLen: number) => number) | undefined;
    if (typeof exp[funcName] === 'function') {
      extFunc = exp[funcName] as typeof extFunc;
    } else if (typeof exp[externName] === 'function') {
      extFunc = exp[externName] as typeof extFunc;
    }
    if (!extFunc) {
      throwVoCallExtFailure(`[voCallExt] Export not found: ${externName}`);
    }
    const allocFn = exp.vo_alloc as ((size: number) => number) | undefined;
    const deallocFn = exp.vo_dealloc as ((ptr: number, size: number) => void) | undefined;
    if (!allocFn || !deallocFn) {
      throwVoCallExtFailure(`[voCallExt] Alloc not found: ${matchedKey}`);
    }
    const mem = exp.memory as WebAssembly.Memory;
    const inputPtr = allocFn(input.length);
    new Uint8Array(mem.buffer).set(input, inputPtr);
    const outLenPtr = allocFn(4);
    const outPtr = extFunc(inputPtr, input.length, outLenPtr);
    deallocFn(inputPtr, input.length);
    if (outPtr === 0) {
      deallocFn(outLenPtr, 4);
      throwVoCallExtFailure(`[voCallExt] Export returned null output pointer: ${externName}`);
    }
    const outLen = new Uint32Array(mem.buffer, outLenPtr, 1)[0];
    const result = new Uint8Array(mem.buffer, outPtr, outLen).slice();
    deallocFn(outPtr, outLen);
    deallocFn(outLenPtr, 4);
    if (traceExtern) {
      emitStudioHostLog({
        source: 'studio-extbridge',
        code: 'ext_call_result',
        level: 'system',
        text: `extern=${externName} key=${matchedKey} func=${funcName} outLen=${result.length} firstTag=${result.length > 0 ? result[0] : -1}`,
      });
    }
    return result;
  };

  (window as unknown as Record<string, unknown>).voCallExtReplay = (
    externName: string,
    resumeData: Uint8Array,
  ): Uint8Array => {
    if (externName.endsWith('waitForEvent')) {
      return decodeWaitForEventReplayOutput(resumeData);
    }
    return ((window as unknown as Record<string, unknown>).voCallExt as (n: string, d: Uint8Array) => Uint8Array)(
      externName,
      resumeData,
    );
  };
}

 // ── Loader ────────────────────────────────────────────────────────────────────

 let instance: StudioWasm | null = null;
 let initPromise: Promise<StudioWasm> | null = null;
 let loadGeneration = 0;

 function requireStudioExport<T>(value: T | undefined, name: string): T {
   if (value === undefined) {
     throw new Error(`studio/wasm missing export: ${name}`);
  }
  return value;
}

function normalizeStudioWasmModule(mod: RawStudioWasmModule): StudioWasm {
  const vmExport = mod.StudioVoVm ?? mod.VoVm ?? mod.VoVmIsland;
  if (!vmExport) {
    throw new Error('studio/wasm missing VM export: StudioVoVm, VoVm, or VoVmIsland');
  }
  return {
    runGuiFromBytecode: requireStudioExport(mod.runGuiFromBytecode as StudioWasm['runGuiFromBytecode'], 'runGuiFromBytecode'),
    startGuiFromBytecode: requireStudioExport(mod.startGuiFromBytecode as StudioWasm['startGuiFromBytecode'], 'startGuiFromBytecode'),
    runGui: requireStudioExport(mod.runGui, 'runGui'),
    runGuiEntry: requireStudioExport(mod.runGuiEntry, 'runGuiEntry'),
    sendGuiEvent: requireStudioExport(mod.sendGuiEvent, 'sendGuiEvent'),
    sendGuiEventAsync: requireStudioExport(mod.sendGuiEventAsync, 'sendGuiEventAsync'),
    startRenderIsland: requireStudioExport(mod.startRenderIsland, 'startRenderIsland'),
    pushIslandData: requireStudioExport(mod.pushIslandData, 'pushIslandData'),
    pollGuiRender: requireStudioExport(mod.pollGuiRender, 'pollGuiRender'),
    getRenderIslandVfsSnapshot: requireStudioExport(mod.getRenderIslandVfsSnapshot, 'getRenderIslandVfsSnapshot'),
    pollIslandData: requireStudioExport(mod.pollIslandData, 'pollIslandData'),
    pollPendingHostEvent: requireStudioExport(mod.pollPendingHostEvent, 'pollPendingHostEvent'),
    wakeHostEvent: requireStudioExport(mod.wakeHostEvent, 'wakeHostEvent'),
    stopGui: requireStudioExport(mod.stopGui, 'stopGui'),
    preloadExtModule: requireStudioExport(mod.preloadExtModule, 'preloadExtModule'),
    compileRunEntry: requireStudioExport(mod.compileRunEntry as StudioWasm['compileRunEntry'], 'compileRunEntry'),
    prepareEntry: requireStudioExport(mod.prepareEntry, 'prepareEntry'),
    compileGui: requireStudioExport(mod.compileGui as StudioWasm['compileGui'], 'compileGui'),
    getBuildId: requireStudioExport(mod.getBuildId as StudioWasm['getBuildId'], 'getBuildId'),
    initVFS: requireStudioExport(mod.initVFS, 'initVFS'),
    VoVm: {
      withExterns: (bytecode) => vmExport.withExterns(bytecode),
    },
  };
}

export async function loadStudioWasm(): Promise<StudioWasm> {
  ensureStudioWindowVfsBindings();
  if (instance) return instance;
  if (initPromise) return initPromise;
  const generation = loadGeneration;
  initPromise = (async () => {
    try {
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'load_begin',
        level: 'system',
      });
      const assetBuildId = await getStudioAssetBuildId();
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'load_build_id_ready',
        level: 'system',
        text: `buildId=${assetBuildId}`,
      });
      const jsPath = withBuildId(['', 'wasm', 'vo_studio_wasm.js'].join('/'), assetBuildId);
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'js_import_begin',
        level: 'system',
        text: `path=${jsPath}`,
      });
      const mod = await (Function('p', 'return import(p)')(jsPath)) as RawStudioWasmModule;
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'js_import_ready',
        level: 'system',
      });
      const wasmPath = withBuildId('/wasm/vo_studio_wasm_bg.wasm', assetBuildId);
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'wasm_init_begin',
        level: 'system',
        text: `path=${wasmPath}`,
      });
      await mod.default(wasmPath);
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'wasm_init_ready',
        level: 'system',
      });
      const normalized = normalizeStudioWasmModule(mod);
      const expectedBuildId = import.meta.env.DEV ? assetBuildId : bundledStudioBuildId;
      const expectedSource = import.meta.env.DEV ? 'asset manifest' : 'frontend';
      assertStudioBuildMatch(expectedBuildId, normalized.getBuildId(), expectedSource);
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'init_vfs_begin',
        level: 'system',
      });
      await normalized.initVFS();
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'init_vfs_ready',
        level: 'system',
      });
      installExtBridgeGlobals();
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'ext_bridge_ready',
        level: 'system',
      });
      if (generation !== loadGeneration) {
        emitStudioHostLog({
          source: 'studio-wasm',
          code: 'load_superseded',
          level: 'system',
        });
        return initPromise ?? loadStudioWasm();
      }
      instance = normalized;
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'load_ready',
        level: 'system',
      });
      return instance;
    } catch (error) {
      if (generation !== loadGeneration) {
        return initPromise ?? loadStudioWasm();
      }
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'load_failed',
        level: 'error',
        text: error instanceof Error ? (error.stack ?? error.message) : String(error),
      });
      studioAssetBuildIdPromise = null;
      initPromise = null;
      instance = null;
      throw error;
    }
  })();
  return initPromise;
}

export function resetStudioWasmInstance(): void {
  loadGeneration += 1;
  unloadAllExtModules();
  studioAssetBuildIdPromise = null;
  instance = null;
  initPromise = null;
}

// ── VoWebModule factory ────────────────────────────────────────────────────────

export function makeVoWebModule(wasm: StudioWasm): VoWebModule {
  return {
    initVFS: () => wasm.initVFS(),
    preloadExtModule: (path, bytes, jsGlueUrl = '') =>
      wasm.preloadExtModule(path, bytes, jsGlueUrl),
    VoVm: {
      withExterns: (bytecode) => wasm.VoVm.withExterns(bytecode),
    },
  };
}
