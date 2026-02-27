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
// =============================================================================

export interface Bridge {
  // Filesystem
  fsListDir(dirPath: string): Promise<FsEntry[]>;
  fsReadFile(path: string): Promise<string>;
  fsWriteFile(path: string, content: string): Promise<void>;
  fsMkdir(path: string): Promise<void>;
  fsRename(oldPath: string, newPath: string): Promise<void>;
  fsRemove(path: string, recursive: boolean): Promise<void>;

  // Execution (entry path relative to workspace root)
  compileRun(entryPath: string): Promise<string>;
  runGui(entryPath: string): Promise<Uint8Array>;
  sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array>;
  stopGui(): Promise<void>;

  // GUI render callback — called by JS timer/timeout callbacks with fresh render bytes
  setGuiRenderCallback(cb: (bytes: Uint8Array) => void): void;
  clearGuiRenderCallback(): void;

  // Workspace root (for display)
  workspaceRoot: string;
}

let _bridge: Bridge | null = null;

export function bridge(): Bridge {
  return _bridge!;
}

function isTauriRuntime(): boolean {
  const w = window as any;
  return Boolean(w.__TAURI__ || w.__TAURI_INTERNALS__);
}

// =============================================================================
// Tauri bridge
// =============================================================================

async function initTauriBridge(): Promise<void> {
  const { invoke } = await import('@tauri-apps/api/core');

  const workspaceRoot: string = await invoke('cmd_get_workspace_root');

  _bridge = {
    workspaceRoot,

    fsListDir:    (dirPath)          => invoke('cmd_fs_list_dir', { dirPath }),
    fsReadFile:   (path)             => invoke('cmd_fs_read_file', { path }),
    fsWriteFile:  (path, content)    => invoke('cmd_fs_write_file', { path, content }),
    fsMkdir:      (path)             => invoke('cmd_fs_mkdir', { path }),
    fsRename:     (oldPath, newPath) => invoke('cmd_fs_rename', { oldPath, newPath }),
    fsRemove:     (path, recursive)  => invoke('cmd_fs_remove', { path, recursive }),

    compileRun:   (entryPath)            => invoke('cmd_compile_run', { entryPath }),
    runGui:       async (entryPath) => new Uint8Array(await invoke<number[]>('cmd_run_gui', { entryPath })),
    sendGuiEvent: async (handlerId, payload) => new Uint8Array(await invoke<number[]>('cmd_send_gui_event', { handlerId, payload })),
    stopGui:      ()                     => invoke('cmd_stop_gui'),

    // Tauri handles timers natively in its own process; no JS-side render callback needed
    setGuiRenderCallback(_cb: (bytes: Uint8Array) => void) {},
    clearGuiRenderCallback() {},
  };
}

// =============================================================================
// WASM bridge (JS VFS + Rust WASM for compilation)
// =============================================================================

const WASM_WORKSPACE = '/workspace';

const DEFAULT_MAIN_VO = `package main

import "fmt"

func main() {
\tfmt.Println("Hello, Vo!")
}
`;

async function initWasmBridge(): Promise<void> {
  // 1) Initialize JS VFS (OPFS + window._vfs* bindings) BEFORE loading WASM
  const { initVFS, vfs } = await import('@vo-web/index');
  await initVFS();

  // 2) Load studio WASM module
  const v = Date.now();
  const wasmEntryUrl = new URL(`../../wasm/pkg/vo_studio_wasm.js?v=${v}`, import.meta.url).href;
  const wasmBinaryUrl = new URL(`../../wasm/pkg/vo_studio_wasm_bg.wasm?v=${v}`, import.meta.url).href;
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

  // 3) Seed default workspace if empty
  const [rootEntries, rootErr] = vfs.readDir(WASM_WORKSPACE);
  if (rootErr || rootEntries.length === 0) {
    vfs.mkdirAll(WASM_WORKSPACE + '/main', 0o755);
    const enc = new TextEncoder();
    vfs.writeFile(
      WASM_WORKSPACE + '/main/main.vo',
      enc.encode(DEFAULT_MAIN_VO),
      0o644,
    );
  }

  // 4) Set up WasmPlatform globals — called by WASM at runtime (resolved at call time, not import time)
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

  // 5) Build bridge — FS ops go through JS VFS directly
  _bridge = {
    workspaceRoot: WASM_WORKSPACE,

    async fsListDir(dirPath: string): Promise<FsEntry[]> {
      const [entries, err] = vfs.readDir(dirPath);
      if (err) throw new Error(err);
      return entries
        .map(([name, isDir]) => ({
          name,
          path: dirPath === '/' ? '/' + name : dirPath + '/' + name,
          isDir,
        }))
        .sort((a, b) => {
          if (a.isDir !== b.isDir) return a.isDir ? -1 : 1;
          return a.name.localeCompare(b.name);
        });
    },

    async fsReadFile(path: string): Promise<string> {
      const [data, err] = vfs.readFile(path);
      if (err || !data) throw new Error(err ?? 'file not found');
      return new TextDecoder().decode(data);
    },

    async fsWriteFile(path: string, content: string): Promise<void> {
      const enc = new TextEncoder();
      const err = vfs.writeFile(path, enc.encode(content), 0o644);
      if (err) throw new Error(err);
    },

    async fsMkdir(path: string): Promise<void> {
      const err = vfs.mkdirAll(path, 0o755);
      if (err) throw new Error(err);
    },

    async fsRename(oldPath: string, newPath: string): Promise<void> {
      const err = vfs.rename(oldPath, newPath);
      if (err) throw new Error(err);
    },

    async fsRemove(path: string, recursive: boolean): Promise<void> {
      const err = recursive ? vfs.removeAll(path) : vfs.remove(path);
      if (err) throw new Error(err);
    },

    async compileRun(entryPath: string): Promise<string> {
      const result = wasmMod.compileRunEntry(entryPath);
      if (result instanceof Error) throw result;
      return result as string;
    },

    async runGui(entryPath: string): Promise<Uint8Array> {
      clearAllGuiTimers();  // clear any leftover timers from a previous run
      const result = wasmMod.runGuiEntry(entryPath);
      if (result instanceof Error) throw result;
      return result as Uint8Array;
    },

    async sendGuiEvent(handlerId: number, payload: string): Promise<Uint8Array> {
      return queueGuiEvent(handlerId, payload, false);
    },

    async stopGui(): Promise<void> {
      clearAllGuiTimers();
      wasmMod.stopGui();
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

export async function initBridge(): Promise<void> {
  if (isTauriRuntime()) {
    await initTauriBridge();
  } else {
    await initWasmBridge();
  }
}
