// Loader for the studio WASM module (vo-studio-wasm wasm-pack output).
// The wasm-pack output is expected at /wasm/ (served from studio/public/wasm/).
// Build: wasm-pack build studio/wasm --target web --out-dir ../public/wasm

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
  startRenderIsland(bytecode: Uint8Array): void;
  pushIslandData(data: Uint8Array): void;
  pollIslandData(): Uint8Array;
  pollPendingHostEvent(): { token: string; delayMs: number } | null;
  wakeHostEvent(token: string): void;
  stopGui(): void;
  // Instance-based VM (VoWebModule interface)
  VoVm: { withExterns(bytecode: Uint8Array): VoVmInstance };
  preloadExtModule(path: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void>;
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

// ── Ext-bridge JS globals (mirrors playground/src/wasm/vo.ts) ─────────────────

type BindgenModule = Record<string, unknown> & {
  __voDispose?: () => void;
};

// Maps normalized module key → WebAssembly.Instance (standalone C-ABI modules).
const extInstances = new Map<string, WebAssembly.Instance>();
// Maps normalized module key → wasm-bindgen module (DOM/WebGPU access).
const extBindgenModules = new Map<string, BindgenModule>();

let extBridgeInstalled = false;

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

function unloadAllExtModules(): void {
  const bindgenModules = Array.from(new Set(extBindgenModules.values()));
  for (const module of bindgenModules) {
    disposeBindgenModule(module);
  }
  extBindgenModules.clear();
  extInstances.clear();
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
    if (jsGlueUrl) {
      const resp = await fetch(jsGlueUrl, { cache: 'no-store' });
      if (!resp.ok) throw new Error(`Failed to fetch JS glue: HTTP ${resp.status}`);
      const jsText = await resp.text();
      const blob = new Blob([jsText], { type: 'application/javascript' });
      const blobUrl = URL.createObjectURL(blob);
      try {
        const glue = await import(/* @vite-ignore */ blobUrl) as BindgenModule;
        await (glue.default as (opts: { module_or_path: Uint8Array }) => Promise<void>)({
          module_or_path: bytes.slice(),
        });
        if (typeof glue.__voInit === 'function') {
          await (glue.__voInit as () => Promise<void>)();
        }
        extBindgenModules.set(key, glue);
        console.log('[voSetupExtModule] bindgen module ready:', key);
      } finally {
        URL.revokeObjectURL(blobUrl);
      }
    } else {
      const { instance } = await WebAssembly.instantiate(bytes.slice());
      extInstances.set(key, instance);
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
      if (typeof bindgenModule[funcName] === 'function') {
        let result: unknown;
        try {
          result = (bindgenModule[funcName] as (i: Uint8Array) => unknown)(input);
        } catch (e) {
          console.error('[voCallExt] Exception calling:', externName, e);
          return new Uint8Array(0);
        }
        if (result instanceof Promise) { result.catch(() => {}); return new Uint8Array(0); }
        if (result instanceof Uint8Array) return result;
        if (typeof result === 'string') return new TextEncoder().encode(result);
        return new Uint8Array(0);
      }
      console.error('[voCallExt] Bindgen export not found:', funcName, 'in module:', bindgenKey);
      return new Uint8Array(0);
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
      console.error('[voCallExt] No loaded module for extern:', externName);
      return new Uint8Array(0);
    }
    const exp = instance.exports as Record<string, unknown>;
    const funcName = externName.substring(matchedKey.length + 1);
    let extFunc: ((ptr: number, len: number, outLen: number) => number) | undefined;
    if (typeof exp[funcName] === 'function') {
      extFunc = exp[funcName] as typeof extFunc;
    } else if (typeof exp[externName] === 'function') {
      extFunc = exp[externName] as typeof extFunc;
    }
    if (!extFunc) { console.error('[voCallExt] Export not found:', externName); return new Uint8Array(0); }
    const allocFn = exp.vo_alloc as ((size: number) => number) | undefined;
    const deallocFn = exp.vo_dealloc as ((ptr: number, size: number) => void) | undefined;
    if (!allocFn || !deallocFn) { console.error('[voCallExt] Alloc not found:', matchedKey); return new Uint8Array(0); }
    const mem = exp.memory as WebAssembly.Memory;
    const inputPtr = allocFn(input.length);
    new Uint8Array(mem.buffer).set(input, inputPtr);
    const outLenPtr = allocFn(4);
    const outPtr = extFunc(inputPtr, input.length, outLenPtr);
    deallocFn(inputPtr, input.length);
    if (outPtr === 0) { deallocFn(outLenPtr, 4); return new Uint8Array(0); }
    const outLen = new Uint32Array(mem.buffer, outLenPtr, 1)[0];
    const result = new Uint8Array(mem.buffer, outPtr, outLen).slice();
    deallocFn(outPtr, outLen);
    deallocFn(outLenPtr, 4);
    return result;
  };

  (window as unknown as Record<string, unknown>).voCallExtReplay = (
    externName: string,
    resumeData: Uint8Array,
  ): Uint8Array => {
    // Replay path: delegate to the same bindgen/standalone dispatch as voCallExt.
    // For bindgen modules that support TAG_SUSPEND, the function is called again
    // with resume data to produce the final result.
    return ((window as unknown as Record<string, unknown>).voCallExt as (n: string, d: Uint8Array) => Uint8Array)(
      externName,
      resumeData,
    );
  };
}

// ── Loader ────────────────────────────────────────────────────────────────────

let instance: StudioWasm | null = null;
let initPromise: Promise<StudioWasm> | null = null;

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
    startRenderIsland: requireStudioExport(mod.startRenderIsland, 'startRenderIsland'),
    pushIslandData: requireStudioExport(mod.pushIslandData, 'pushIslandData'),
    pollIslandData: requireStudioExport(mod.pollIslandData, 'pollIslandData'),
    pollPendingHostEvent: requireStudioExport(mod.pollPendingHostEvent, 'pollPendingHostEvent'),
    wakeHostEvent: requireStudioExport(mod.wakeHostEvent, 'wakeHostEvent'),
    stopGui: requireStudioExport(mod.stopGui, 'stopGui'),
    preloadExtModule: requireStudioExport(mod.preloadExtModule, 'preloadExtModule'),
    initVFS: requireStudioExport(mod.initVFS, 'initVFS'),
    VoVm: {
      withExterns: (bytecode) => vmExport.withExterns(bytecode),
    },
  };
}

export async function loadStudioWasm(): Promise<StudioWasm> {
  if (instance) return instance;
  if (initPromise) return initPromise;
  initPromise = (async () => {
    // Dynamic import of the wasm-bindgen JS glue file from public/wasm/.
    // Path is constructed at runtime to bypass Vite static analysis and TS resolution.
    const jsPath = ['', 'wasm', 'vo_studio_wasm.js'].join('/');
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const mod = await (Function('p', 'return import(p)')(jsPath)) as RawStudioWasmModule;
    // Initialize the WASM module (fetches vo_studio_wasm_bg.wasm)
    await mod.default('/wasm/vo_studio_wasm_bg.wasm');
    // Install window.voSetupExtModule / voCallExt / voCallExtReplay so that
    // preloadExtModule and VoVm.with_externs can dispatch ext module calls.
    installExtBridgeGlobals();
    instance = normalizeStudioWasmModule(mod);
    return instance;
  })();
  return initPromise;
}

export function resetStudioWasmInstance(): void {
  unloadAllExtModules();
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
