import { STUDIO_SYNC_EXAMPLES } from '../generated/examples.generated';
import { registerExtModuleGlobals } from './ext-modules';
import { isPathWithinRoot } from './path_utils';
import { ShellClient } from './shell/client';
import { DEFAULT_MAIN_VO } from './starter_templates';
import { TauriTransport, WasmTransport } from './shell/transport';
import { WasmShellRouter } from './shell/wasm/router';

// =============================================================================
// IndexedDB cache for shell handler bytecode
//
// Compilation inside WASM takes 2-5 seconds.  We cache the result keyed by a
// content hash of the embedded source files so reloads skip recompilation.
// =============================================================================

const IDB_NAME = 'vo-studio-cache';
const IDB_STORE = 'shell-handler';

function openCacheDB(): Promise<IDBDatabase> {
  return new Promise((resolve, reject) => {
    const req = indexedDB.open(IDB_NAME, 1);
    req.onupgradeneeded = () => {
      const db = req.result;
      if (!db.objectStoreNames.contains(IDB_STORE)) {
        db.createObjectStore(IDB_STORE);
      }
    };
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });
}

function idbGet(db: IDBDatabase, key: string): Promise<Uint8Array | undefined> {
  return new Promise((resolve, reject) => {
    const tx = db.transaction(IDB_STORE, 'readonly');
    const store = tx.objectStore(IDB_STORE);
    const req = store.get(key);
    req.onsuccess = () => resolve(req.result as Uint8Array | undefined);
    req.onerror = () => reject(req.error);
  });
}

function idbPut(db: IDBDatabase, key: string, value: Uint8Array): Promise<void> {
  return new Promise((resolve, reject) => {
    const tx = db.transaction(IDB_STORE, 'readwrite');
    const store = tx.objectStore(IDB_STORE);
    const req = store.put(value, key);
    req.onsuccess = () => resolve();
    req.onerror = () => reject(req.error);
  });
}

/** Try to load shell handler bytecode from IndexedDB.  Returns null on miss. */
async function tryLoadCachedShellHandler(wasmMod: any): Promise<boolean> {
  const hash: string = wasmMod.shellHandlerSourceHash();
  const db = await openCacheDB();
  const cached = await idbGet(db, hash);
  if (cached && cached.byteLength > 0 && wasmMod.loadCachedShellHandler(cached)) {
    return true;
  }
  return false;
}

/** Compile shell handler and store in IndexedDB for next load. */
async function compileAndCacheShellHandler(wasmMod: any): Promise<void> {
  try {
    const db = await openCacheDB();
    const hash: string = wasmMod.shellHandlerSourceHash();
    const bytecode: Uint8Array = wasmMod.buildShellHandler();
    await idbPut(db, hash, bytecode).catch(() => {});
  } catch {
    // IndexedDB unavailable — fall back to synchronous compile
    const err = wasmMod.initShellHandler();
    if (err) {
      throw new Error(`[studio] Shell handler compilation failed: ${err}`);
    }
  }
}

// =============================================================================
// FsEntry — returned by fsListDir
// =============================================================================

export interface FsEntry {
  name: string;
  path: string;
  isDir: boolean;
}

// =============================================================================
// Bridge — cross-platform abstraction (Tauri IPC / WASM VFS)
//
// All structured ops (fs, vo, git, gui, …) go through `shell.exec()`.
// The transport layer routes gui.* ops to the platform-specific GUI backend
// (Tauri IPC commands / WASM module calls) transparently.
// =============================================================================

export interface Bridge {
  // Filesystem convenience methods (delegate to shell.exec)
  fsListDir(dirPath: string): Promise<FsEntry[]>;
  fsStat(path: string): Promise<FsEntry & { size?: number; modifiedMs?: number }>;
  fsReadFile(path: string): Promise<string>;
  fsWriteFile(path: string, content: string): Promise<void | null>;
  fsMkdir(path: string): Promise<void | null>;
  fsRename(oldPath: string, newPath: string): Promise<void | null>;
  fsRemove(path: string, recursive: boolean): Promise<void | null>;
  materializeLocalLaunchTarget(path: string): Promise<string>;

  // GUI render callback — push-based, for JS timer/timeout driven re-renders.
  // On Tauri these are no-ops (timers handled natively); on WASM they pipe
  // timer-triggered render bytes to the PreviewPanel.
  setGuiRenderCallback(cb: (bytes: Uint8Array) => void): void;
  clearGuiRenderCallback(): void;

  // Workspace root (for display)
  workspaceRoot: string;

  // Unified shell API (filesystem + Vo toolchain + GUI + tools + processes)
  shell: ShellClient;
}

let _bridge: Bridge | null = null;

export function bridge(): Bridge {
  return _bridge!;
}

function isTauriRuntime(): boolean {
  const w = window as any;
  return Boolean(w.__TAURI__ || w.__TAURI_INTERNALS__);
}

function parentDir(path: string): string {
  const idx = path.lastIndexOf('/');
  if (idx <= 0) return '/';
  return path.slice(0, idx);
}

// =============================================================================
// Tauri bridge
// =============================================================================

async function initTauriBridge(onProgress: (step: string) => void): Promise<void> {
  onProgress('Connecting to backend…');
  const { invoke } = await import('@tauri-apps/api/core');

  const workspaceRoot: string = await invoke('cmd_get_workspace_root');

  const shellTransport = new TauriTransport();
  const shellClient    = new ShellClient(shellTransport);
  await shellClient.initialize();

  // Seed built-in examples into workspace (always overwrite so updates propagate)
  for (const ex of STUDIO_SYNC_EXAMPLES) {
    const fullPath = workspaceRoot + '/' + ex.path;
    await shellClient.exec({ kind: 'fs.mkdir', path: parentDir(fullPath), recursive: true });
    await shellClient.exec({ kind: 'fs.write', path: fullPath, content: ex.content });
  }

  _bridge = {
    workspaceRoot,
    shell: shellClient,

    async fsListDir(dirPath: string): Promise<FsEntry[]> {
      const entries = await shellClient.exec({ kind: 'fs.list', path: dirPath });
      return entries.sort((a, b) => {
        if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
        return a.name.localeCompare(b.name);
      });
    },
    fsStat:      (path: string)                     => shellClient.exec({ kind: 'fs.stat',   path }),
    fsReadFile:  (path: string)                     => shellClient.exec({ kind: 'fs.read',   path }),
    fsWriteFile: (path: string, content: string)    => shellClient.exec({ kind: 'fs.write',  path, content }),
    fsMkdir:     (path: string)                     => shellClient.exec({ kind: 'fs.mkdir',  path }),
    fsRename:    (oldPath: string, newPath: string)  => shellClient.exec({ kind: 'fs.rename', oldPath, newPath }),
    fsRemove:    (path: string, recursive: boolean)  => shellClient.exec({ kind: 'fs.remove', path, recursive }),
    materializeLocalLaunchTarget: (path: string)     => invoke<string>('cmd_materialize_local_launch_target', { targetPath: path }),

    // Poll for platform-driven renders (game loop, timers) via RAF loop.
    // Uses the `invoke` captured from initTauriBridge scope.
    setGuiRenderCallback(cb: (bytes: Uint8Array) => void) {
      let active = true;
      const poll = async () => {
        if (!active) return;
        try {
          const raw = await invoke('cmd_poll_gui_render') as number[];
          if (raw && raw.length > 0) {
            cb(new Uint8Array(raw));
          }
        } catch { /* guest stopped */ }
        if (active) requestAnimationFrame(poll);
      };
      requestAnimationFrame(poll);
      (this as any).__guiPollStop = () => { active = false; };
    },
    clearGuiRenderCallback() {
      const stop = (this as any).__guiPollStop;
      if (typeof stop === 'function') {
        stop();
        (this as any).__guiPollStop = null;
      }
    },
  };
}

// =============================================================================
// WASM bridge (JS VFS + Rust WASM for compilation)
// =============================================================================

const WASM_WORKSPACE = '/workspace';

async function initWasmBridge(onProgress: (step: string) => void): Promise<void> {
  // 1) Initialize JS VFS (OPFS + window._vfs* bindings) BEFORE loading WASM
  onProgress('Initializing file system…');
  const { initVFS, vfs } = await import('@vo-web/index');
  await initVFS();

  // 2) Seed built-in examples into VFS (always overwrite so updates propagate)
  const enc = new TextEncoder();
  for (const ex of STUDIO_SYNC_EXAMPLES) {
    const fullPath = WASM_WORKSPACE + '/' + ex.path;
    vfs.mkdirAll(parentDir(fullPath), 0o755);
    vfs.writeFile(fullPath, enc.encode(ex.content), 0o644);
  }

  // 3) Load studio WASM module
  onProgress('Loading compiler…');
  const wasmEntryUrl = new URL('../../wasm/pkg/vo_studio_wasm.js', import.meta.url).href;
  const wasmBinaryUrl = new URL('../../wasm/pkg/vo_studio_wasm_bg.wasm', import.meta.url).href;
  let wasmMod: any;
  try {
    wasmMod = await import(/* @vite-ignore */ wasmEntryUrl);
  } catch (e) {
    throw new Error(
      `Failed to load Studio WASM bundle (${wasmEntryUrl}). ` +
      `Build it first: cd studio/wasm && wasm-pack build --target web --out-dir pkg. ` +
      `Original error: ${e}`,
    );
  }
  await wasmMod.default(wasmBinaryUrl);

  // 3b) Register ext-module hooks that WASM calls when loading .wasm extension binaries
  //     (e.g. zip.wasm).  Must be on window before preloadShellDeps runs.
  registerExtModuleGlobals();

  // 4) Install shell handler deps + try loading cached bytecode in parallel.
  onProgress('Installing modules…');
  //    The IndexedDB cache check is fast; if it hits we skip the 2-5s compile.
  //    If it misses, we wait for deps then compile (deps are needed for compilation).
  const [depsResult, cacheHit] = await Promise.allSettled([
    wasmMod.preloadShellDeps(),
    tryLoadCachedShellHandler(wasmMod).catch(() => false),
  ]);

  if (depsResult.status === 'rejected') {
    throw new Error(`[studio] Failed to install shell deps: ${String(depsResult.reason)}`);
  }

  const shellCacheHit = cacheHit.status === 'fulfilled' && cacheHit.value === true;
  if (!shellCacheHit) {
    // Cache miss — compile (needs deps loaded) and store for next time
    onProgress('Compiling shell handler…');
    await compileAndCacheShellHandler(wasmMod);
  }

  // 5) Seed default workspace main project if it doesn't exist
  const mainStatErr = vfs.stat(WASM_WORKSPACE + '/main')[5];
  if (mainStatErr) {
    vfs.mkdirAll(WASM_WORKSPACE + '/main', 0o755);
    vfs.writeFile(
      WASM_WORKSPACE + '/main/main.vo',
      enc.encode(DEFAULT_MAIN_VO),
      0o644,
    );
  }

  // 5) Set up WasmPlatform globals — called by WASM at runtime (resolved at call time, not import time)
  let guiRenderCallback: ((bytes: Uint8Array) => void) | null = null;
  const activeTimers = new Map<number, number>();
  const activeTimeouts = new Map<number, number>();
  const activeAnimFrames = new Map<number, number>();
  const activeGameLoops = new Map<number, { rafId: number; lastTs: number }>();
  let guiEventInFlight = false;
  const guiEventQueue: Array<{
    handlerId: number;
    payload: string;
    emitRender: boolean;
    resolve: (bytes: Uint8Array) => void;
    reject: (err: unknown) => void;
  }> = [];

  const nativeSetInterval = window.setInterval.bind(window);
  const nativeClearInterval = window.clearInterval.bind(window);
  const nativeSetTimeout = window.setTimeout.bind(window);
  const nativeClearTimeout = window.clearTimeout.bind(window);

  function drainGuiEventQueue() {
    if (guiEventInFlight) return;
    const next = guiEventQueue.shift();
    if (!next) return;

    guiEventInFlight = true;
    try {
      const result = wasmMod.sendGuiEvent(next.handlerId, next.payload);
      if (result instanceof Error) throw result;
      const bytes = result as Uint8Array;
      if (next.emitRender && bytes && bytes.length > 0 && guiRenderCallback) {
        guiRenderCallback(bytes);
      }
      next.resolve(bytes);
    } catch (e) {
      next.reject(e);
    } finally {
      guiEventInFlight = false;
      if (guiEventQueue.length > 0) queueMicrotask(drainGuiEventQueue);
    }
  }

  function queueGuiEvent(handlerId: number, payload: string, emitRender: boolean): Promise<Uint8Array> {
    return new Promise((resolve, reject) => {
      guiEventQueue.push({ handlerId, payload, emitRender, resolve, reject });
      drainGuiEventQueue();
    });
  }

  function resetGuiEventQueue() {
    guiEventQueue.length = 0;
    guiEventInFlight = false;
  }

  function clearAllGuiTimers() {
    activeTimers.forEach((jsId) => nativeClearInterval(jsId));
    activeTimers.clear();
    activeTimeouts.forEach((jsId) => nativeClearTimeout(jsId));
    activeTimeouts.clear();
    activeAnimFrames.forEach((rafId) => cancelAnimationFrame(rafId));
    activeAnimFrames.clear();
    activeGameLoops.forEach((state) => cancelAnimationFrame(state.rafId));
    activeGameLoops.clear();
    resetGuiEventQueue();
  }

  (window as any).startInterval = (voId: number, ms: number) => {
    if (activeTimers.has(voId)) nativeClearInterval(activeTimers.get(voId)!);
    const jsId = nativeSetInterval(() => {
      queueGuiEvent(-1, JSON.stringify({ id: voId }), true).catch((e) => {
        console.error('[studio] Timer handler error:', e);
        const staleJsId = activeTimers.get(voId);
        if (staleJsId !== undefined) { nativeClearInterval(staleJsId); activeTimers.delete(voId); }
      });
    }, ms);
    activeTimers.set(voId, jsId);
  };

  (window as any).clearInterval = (voId: number) => {
    const jsId = activeTimers.get(voId);
    if (jsId !== undefined) { nativeClearInterval(jsId); activeTimers.delete(voId); }
  };

  (window as any).startTimeout = (voId: number, ms: number) => {
    if (activeTimeouts.has(voId)) nativeClearTimeout(activeTimeouts.get(voId)!);
    const jsId = nativeSetTimeout(() => {
      activeTimeouts.delete(voId);
      queueGuiEvent(-1, JSON.stringify({ id: voId }), true).catch((e) => {
        console.error('[studio] Timeout handler error:', e);
      });
    }, ms);
    activeTimeouts.set(voId, jsId);
  };

  (window as any).clearTimeout = (id: any) => {
    if (typeof id === 'number' && activeTimeouts.has(id)) {
      nativeClearTimeout(activeTimeouts.get(id)!);
      activeTimeouts.delete(id);
    } else {
      nativeClearTimeout(id);
    }
  };

  // Animation frame & game loop — eventIDAnimFrame=-4, eventIDGameLoop=-5
  (window as any).voguiStartAnimFrame = (id: number) => {
    if (activeAnimFrames.has(id)) cancelAnimationFrame(activeAnimFrames.get(id)!);
    const rafId = requestAnimationFrame(() => {
      activeAnimFrames.delete(id);
      queueGuiEvent(-4, JSON.stringify({ Id: id }), true)
        .catch((e) => { console.error('[studio] AnimFrame handler error:', e); });
    });
    activeAnimFrames.set(id, rafId);
  };

  (window as any).voguiCancelAnimFrame = (id: number) => {
    if (activeAnimFrames.has(id)) {
      cancelAnimationFrame(activeAnimFrames.get(id)!);
      activeAnimFrames.delete(id);
    }
  };

  (window as any).voguiStartGameLoop = (id: number) => {
    if (activeGameLoops.has(id)) cancelAnimationFrame(activeGameLoops.get(id)!.rafId);
    const state = { rafId: 0, lastTs: 0 };
    activeGameLoops.set(id, state);
    function tick(ts: number): void {
      if (!activeGameLoops.has(id)) return;
      const loop = activeGameLoops.get(id)!;
      const dt = loop.lastTs === 0 ? 0 : ts - loop.lastTs;
      loop.lastTs = ts;
      queueGuiEvent(-5, JSON.stringify({ Dt: dt }), true)
        .then(() => {
          if (activeGameLoops.has(id)) {
            activeGameLoops.get(id)!.rafId = requestAnimationFrame(tick);
          }
        })
        .catch((e) => {
          console.error('[studio] GameLoop handler error:', e);
          activeGameLoops.delete(id);
        });
    }
    state.rafId = requestAnimationFrame(tick);
  };

  (window as any).voguiStopGameLoop = (id: number) => {
    const loop = activeGameLoops.get(id);
    if (loop) { cancelAnimationFrame(loop.rafId); activeGameLoops.delete(id); }
  };

  // DOM platform hooks — no-ops for studio
  (window as any).navigate = (_path: string) => {};
  (window as any).getCurrentPath = () => '/';
  (window as any).voguiFocus = () => {};
  (window as any).voguiBlur = () => {};
  (window as any).voguiScrollTo = () => {};
  (window as any).voguiScrollIntoView = () => {};
  (window as any).voguiSelectText = () => {};
  (window as any).voguiSetTitle = () => {};
  (window as any).voguiSetMeta = () => {};
  (window as any).voguiToast = () => {};

  // Host bridge for vox standalone WASM module — compile/run delegate to studio WASM,
  // VFS ops delegate to the JS VFS layer directly.
  (window as any).voHostCompileFile = (path: string) => wasmMod.voHostCompileFile(path);
  (window as any).voHostCompileDir = (path: string) => wasmMod.voHostCompileDir(path);
  (window as any).voHostCompileString = (code: string) => wasmMod.voHostCompileString(code);
  (window as any).voHostCompileCheck = (code: string) => wasmMod.voHostCompileCheck(code);
  (window as any).voHostRunBytecode = (bytecode: Uint8Array) => wasmMod.voHostRunBytecode(bytecode);
  (window as any).voHostRunBytecodeCapture = (bytecode: Uint8Array) => wasmMod.voHostRunBytecodeCapture(bytecode);
  (window as any).voHostVfsRead = (path: string) => vfs.readFile(path);
  (window as any).voHostVfsWrite = (path: string, data: Uint8Array) => vfs.writeFile(path, data, 0o644);
  (window as any).voHostVfsMkdirAll = (path: string) => vfs.mkdirAll(path, 0o755);
  (window as any).voHostVfsExists = (path: string) => !vfs.stat(path)[5];

  // 6) Build WasmExecBackend — wraps WASM module calls + timer lifecycle.
  //    Passed to WasmTransport so app.* and gui.* ops are handled without the transport
  //    owning WASM module references or timer state.
  const execBackend: import('./shell/transport').WasmExecBackend = {
    async prepare(entryPath: string): Promise<void> {
      await wasmMod.prepareEntry(entryPath);
    },
    compileRun(entryPath: string): string {
      const result = wasmMod.compileRunEntry(entryPath);
      if (result instanceof Error) throw result;
      return result as string;
    },
    runGui(entryPath: string): Uint8Array {
      clearAllGuiTimers();
      const result = wasmMod.runGuiEntry(entryPath);
      if (result instanceof Error) throw result;
      return result as Uint8Array;
    },
    sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array> {
      return queueGuiEvent(handlerId, payload, false);
    },
    stopGui(): void {
      clearAllGuiTimers();
      wasmMod.stopGui();
    },
  };

  // 7) Build bridge — all ops route through ShellClient → WasmTransport
  const wasmRouter  = new WasmShellRouter(wasmMod, WASM_WORKSPACE);
  const shellClient = new ShellClient(new WasmTransport(wasmRouter, execBackend));
  await shellClient.initialize();

  _bridge = {
    workspaceRoot: WASM_WORKSPACE,
    shell: shellClient,

    async fsListDir(dirPath: string): Promise<FsEntry[]> {
      const entries = await shellClient.exec({ kind: 'fs.list', path: dirPath });
      return entries.sort((a, b) => {
        if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
        return a.name.localeCompare(b.name);
      });
    },
    fsStat:      (path: string)                     => shellClient.exec({ kind: 'fs.stat',   path }),
    fsReadFile:  (path: string)                    => shellClient.exec({ kind: 'fs.read',   path }),
    fsWriteFile: (path: string, content: string)   => shellClient.exec({ kind: 'fs.write',  path, content }),
    fsMkdir:     (path: string)                    => shellClient.exec({ kind: 'fs.mkdir',  path }),
    fsRename:    (oldPath: string, newPath: string) => shellClient.exec({ kind: 'fs.rename', oldPath, newPath }),
    fsRemove:    (path: string, recursive: boolean) => shellClient.exec({ kind: 'fs.remove', path, recursive }),
    async materializeLocalLaunchTarget(path: string): Promise<string> {
      if (isPathWithinRoot(path, WASM_WORKSPACE)) return path;
      throw new Error(`Local launch target is outside the browser workspace: ${path}`);
    },

    setGuiRenderCallback(cb: (bytes: Uint8Array) => void) {
      guiRenderCallback = cb;
    },

    clearGuiRenderCallback() {
      guiRenderCallback = null;
    },
  };
}

// =============================================================================
// Init
// =============================================================================

export async function initBridge(onProgress?: (step: string) => void): Promise<void> {
  const report = onProgress ?? (() => {});
  if (isTauriRuntime()) {
    await initTauriBridge(report);
  } else {
    await initWasmBridge(report);
  }
}
