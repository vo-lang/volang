import { STUDIO_SYNC_EXAMPLES } from '../generated/examples.generated';
import { ShellClient } from './shell/client';
import { TauriTransport, WasmTransport } from './shell/transport';
import { WasmShellRouter } from './shell/wasm/router';

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
  fsWriteFile(path: string, content: string): Promise<void | null>;
  fsMkdir(path: string): Promise<void | null>;
  fsRename(oldPath: string, newPath: string): Promise<void | null>;
  fsRemove(path: string, recursive: boolean): Promise<void | null>;

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

  // Unified shell API (filesystem + Vo toolchain + tools + processes)
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

// =============================================================================
// Tauri bridge
// =============================================================================

async function initTauriBridge(): Promise<void> {
  const { invoke } = await import('@tauri-apps/api/core');

  const workspaceRoot: string = await invoke('cmd_get_workspace_root');

  const shellTransport = new TauriTransport();
  const shellClient    = new ShellClient(shellTransport);
  await shellClient.initialize();

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
    fsReadFile:  (path: string)                     => shellClient.exec({ kind: 'fs.read',   path }),
    fsWriteFile: (path: string, content: string)    => shellClient.exec({ kind: 'fs.write',  path, content }),
    fsMkdir:     (path: string)                     => shellClient.exec({ kind: 'fs.mkdir',  path }),
    fsRename:    (oldPath: string, newPath: string)  => shellClient.exec({ kind: 'fs.rename', oldPath, newPath }),
    fsRemove:    (path: string, recursive: boolean)  => shellClient.exec({ kind: 'fs.remove', path, recursive }),

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

  // 2) Seed examples into VFS as root-level single-file projects (skip if already exist)
  const enc = new TextEncoder();
  for (const ex of STUDIO_SYNC_EXAMPLES) {
    const fullPath = WASM_WORKSPACE + '/' + ex.path;
    const statErr = vfs.stat(fullPath)[5];
    if (statErr) {
      vfs.writeFile(fullPath, enc.encode(ex.content), 0o644);
    }
  }

  // 3) Load studio WASM module
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

  // 3b) Register ext-module hooks that WASM calls when loading .wasm extension binaries
  //     (e.g. zip.wasm).  Must be on window before preloadShellDeps runs.
  const extInstances = new Map<string, WebAssembly.Instance>();
  const extBindgenModules = new Map<string, any>();

  (window as any).voSetupExtModule = async (key: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void> => {
    if (jsGlueUrl) {
      const resp = await fetch(jsGlueUrl, { cache: 'no-store' });
      if (!resp.ok) throw new Error(`Failed to fetch JS glue: HTTP ${resp.status}`);
      const jsText = await resp.text();
      const blob = new Blob([jsText], { type: 'application/javascript' });
      const blobUrl = URL.createObjectURL(blob);
      try {
        const glue = await import(/* @vite-ignore */ blobUrl);
        await glue.default({ module_or_path: bytes.slice() });
        if (typeof glue.__voInit === 'function') await glue.__voInit();
        extBindgenModules.set(key, glue);
      } finally {
        URL.revokeObjectURL(blobUrl);
      }
    } else {
      // Standalone C-ABI module.  Provide host import functions under "env"
      // so modules that need host side effects (e.g. vogui timers, DOM, game loop)
      // can call them.  Pure-compute modules (e.g. zip) simply ignore unused imports.
      const w = window as any;
      const decoder = new TextDecoder();
      const encoder = new TextEncoder();

      // Helper: read a string from the module's linear memory.
      const memStr = (mem: WebAssembly.Memory, ptr: number, len: number): string =>
        decoder.decode(new Uint8Array(mem.buffer, ptr, len));

      // Lazy reference to instance memory — resolved after instantiation.
      let mem: WebAssembly.Memory;

      const importObject: WebAssembly.Imports = {
        env: {
          host_start_timeout:     (id: number, ms: number) => { w.startTimeout(id, ms); },
          host_clear_timeout:     (id: number) => { w.clearTimeout(id); },
          host_start_interval:    (id: number, ms: number) => { w.startInterval(id, ms); },
          host_clear_interval:    (id: number) => { w.clearInterval(id); },
          host_navigate:          (ptr: number, len: number) => { w.navigate(memStr(mem, ptr, len)); },
          host_get_current_path:  (outLenPtr: number): number => {
            const path = String(w.getCurrentPath?.() ?? '/');
            const enc = encoder.encode(path);
            const exp = instance!.exports as any;
            const dst: number = exp.vo_alloc(enc.length);
            new Uint8Array(mem.buffer).set(enc, dst);
            new DataView(mem.buffer).setUint32(outLenPtr, enc.length, true);
            return dst;
          },
          host_focus:             (ptr: number, len: number) => { w.voguiFocus(memStr(mem, ptr, len)); },
          host_blur:              (ptr: number, len: number) => { w.voguiBlur(memStr(mem, ptr, len)); },
          host_scroll_to:         (ptr: number, len: number, top: number) => { w.voguiScrollTo(memStr(mem, ptr, len), top); },
          host_scroll_into_view:  (ptr: number, len: number) => { w.voguiScrollIntoView(memStr(mem, ptr, len)); },
          host_select_text:       (ptr: number, len: number) => { w.voguiSelectText(memStr(mem, ptr, len)); },
          host_set_title:         (ptr: number, len: number) => { w.voguiSetTitle(memStr(mem, ptr, len)); },
          host_set_meta:          (np: number, nl: number, cp: number, cl: number) => { w.voguiSetMeta(memStr(mem, np, nl), memStr(mem, cp, cl)); },
          host_toast:             (mp: number, ml: number, tp: number, tl: number, dur: number) => { w.voguiToast(memStr(mem, mp, ml), memStr(mem, tp, tl), dur); },
          host_start_anim_frame:  (id: number) => { w.voguiStartAnimFrame(id); },
          host_cancel_anim_frame: (id: number) => { w.voguiCancelAnimFrame(id); },
          host_start_game_loop:   (id: number) => { w.voguiStartGameLoop(id); },
          host_stop_game_loop:    (id: number) => { w.voguiStopGameLoop(id); },

          // ── vox host imports ──────────────────────────────────────────
          // Write bytes into the module's memory and return pointer.
          // Sets *outLenPtr = data.length.

          // host_compile(path_ptr, path_len, ok_ptr, out_len_ptr) -> data_ptr
          host_compile: (pathPtr: number, pathLen: number, okPtr: number, outLenPtr: number): number => {
            const exp = instance!.exports as any;
            const dv = new DataView(mem.buffer);
            const path = memStr(mem, pathPtr, pathLen);
            try {
              const bytecode: Uint8Array = w.voHostCompileFile(path);
              const dst: number = exp.vo_alloc(bytecode.length);
              new Uint8Array(mem.buffer).set(bytecode, dst);
              dv.setUint32(okPtr, 1, true);
              dv.setUint32(outLenPtr, bytecode.length, true);
              return dst;
            } catch (e: any) {
              const msg = encoder.encode(String(e?.message ?? e));
              const dst: number = exp.vo_alloc(msg.length);
              new Uint8Array(mem.buffer).set(msg, dst);
              dv.setUint32(okPtr, 0, true);
              dv.setUint32(outLenPtr, msg.length, true);
              return dst;
            }
          },

          // host_compile_string(code_ptr, code_len, ok_ptr, out_len_ptr) -> data_ptr
          host_compile_string: (codePtr: number, codeLen: number, okPtr: number, outLenPtr: number): number => {
            const exp = instance!.exports as any;
            const dv = new DataView(mem.buffer);
            const code = memStr(mem, codePtr, codeLen);
            try {
              const bytecode: Uint8Array = w.voHostCompileString(code);
              const dst: number = exp.vo_alloc(bytecode.length);
              new Uint8Array(mem.buffer).set(bytecode, dst);
              dv.setUint32(okPtr, 1, true);
              dv.setUint32(outLenPtr, bytecode.length, true);
              return dst;
            } catch (e: any) {
              const msg = encoder.encode(String(e?.message ?? e));
              const dst: number = exp.vo_alloc(msg.length);
              new Uint8Array(mem.buffer).set(msg, dst);
              dv.setUint32(okPtr, 0, true);
              dv.setUint32(outLenPtr, msg.length, true);
              return dst;
            }
          },

          // host_compile_check(code_ptr, code_len, out_len_ptr) -> msg_ptr
          host_compile_check: (codePtr: number, codeLen: number, outLenPtr: number): number => {
            const exp = instance!.exports as any;
            const code = memStr(mem, codePtr, codeLen);
            const result: string = w.voHostCompileCheck(code);
            const msg = encoder.encode(result);
            const dst: number = exp.vo_alloc(msg.length);
            new Uint8Array(mem.buffer).set(msg, dst);
            new DataView(mem.buffer).setUint32(outLenPtr, msg.length, true);
            return dst;
          },

          // host_run_bytecode(bc_ptr, bc_len, err_len_ptr) -> err_ptr
          host_run_bytecode: (bcPtr: number, bcLen: number, errLenPtr: number): number => {
            const exp = instance!.exports as any;
            const bc = new Uint8Array(mem.buffer, bcPtr, bcLen).slice();
            try {
              w.voHostRunBytecode(bc);
              new DataView(mem.buffer).setUint32(errLenPtr, 0, true);
              return 0;
            } catch (e: any) {
              const msg = encoder.encode(String(e?.message ?? e));
              const dst: number = exp.vo_alloc(msg.length);
              new Uint8Array(mem.buffer).set(msg, dst);
              new DataView(mem.buffer).setUint32(errLenPtr, msg.length, true);
              return dst;
            }
          },

          // host_run_bytecode_capture(bc_ptr, bc_len, ok_ptr, out_len_ptr) -> data_ptr
          host_run_bytecode_capture: (bcPtr: number, bcLen: number, okPtr: number, outLenPtr: number): number => {
            const exp = instance!.exports as any;
            const dv = new DataView(mem.buffer);
            const bc = new Uint8Array(mem.buffer, bcPtr, bcLen).slice();
            try {
              const output: string = w.voHostRunBytecodeCapture(bc);
              const msg = encoder.encode(output);
              const dst: number = exp.vo_alloc(msg.length);
              new Uint8Array(mem.buffer).set(msg, dst);
              dv.setUint32(okPtr, 1, true);
              dv.setUint32(outLenPtr, msg.length, true);
              return dst;
            } catch (e: any) {
              const msg = encoder.encode(String(e?.message ?? e));
              const dst: number = exp.vo_alloc(msg.length);
              new Uint8Array(mem.buffer).set(msg, dst);
              dv.setUint32(okPtr, 0, true);
              dv.setUint32(outLenPtr, msg.length, true);
              return dst;
            }
          },

          // host_vfs_read(path_ptr, path_len, ok_ptr, out_len_ptr) -> data_ptr
          host_vfs_read: (pathPtr: number, pathLen: number, okPtr: number, outLenPtr: number): number => {
            const exp = instance!.exports as any;
            const dv = new DataView(mem.buffer);
            const path = memStr(mem, pathPtr, pathLen);
            try {
              const data: Uint8Array = w.voHostVfsRead(path);
              const dst: number = exp.vo_alloc(data.length);
              new Uint8Array(mem.buffer).set(data, dst);
              dv.setUint32(okPtr, 1, true);
              dv.setUint32(outLenPtr, data.length, true);
              return dst;
            } catch {
              dv.setUint32(okPtr, 0, true);
              dv.setUint32(outLenPtr, 0, true);
              return 0;
            }
          },

          // host_vfs_write(path_ptr, path_len, data_ptr, data_len) -> ok
          host_vfs_write: (pathPtr: number, pathLen: number, dataPtr: number, dataLen: number): number => {
            const path = memStr(mem, pathPtr, pathLen);
            const data = new Uint8Array(mem.buffer, dataPtr, dataLen).slice();
            try {
              w.voHostVfsWrite(path, data);
              return 1;
            } catch {
              return 0;
            }
          },

          // host_vfs_exists(path_ptr, path_len) -> 0/1
          host_vfs_exists: (pathPtr: number, pathLen: number): number => {
            const path = memStr(mem, pathPtr, pathLen);
            return w.voHostVfsExists(path) ? 1 : 0;
          },
        },
      };

      let instance: WebAssembly.Instance;
      const result = await WebAssembly.instantiate(bytes.slice(), importObject);
      instance = result.instance;
      mem = instance.exports.memory as WebAssembly.Memory;
      extInstances.set(key, instance);
    }
  };

  (window as any).voCallExt = (externName: string, input: Uint8Array): Uint8Array => {
    let bindgenModule: any;
    let bindgenKey = '';
    for (const [key, mod] of extBindgenModules) {
      if (externName.startsWith(key) && key.length > bindgenKey.length) { bindgenKey = key; bindgenModule = mod; }
    }
    if (bindgenModule) {
      const fn = bindgenModule[externName.substring(bindgenKey.length + 1)];
      if (typeof fn !== 'function') return new Uint8Array(0);
      try { const r = fn(input); if (r instanceof Uint8Array) return r; } catch { }
      return new Uint8Array(0);
    }
    let instance: WebAssembly.Instance | undefined;
    let matchedKey = '';
    for (const [key, inst] of extInstances) {
      if (externName.startsWith(key) && key.length > matchedKey.length) { matchedKey = key; instance = inst; }
    }
    if (!instance) return new Uint8Array(0);
    const exp = instance.exports as any;
    const funcName = externName.substring(matchedKey.length + 1);
    const extFunc: Function | undefined = exp[funcName] ?? exp[externName];
    if (!extFunc) return new Uint8Array(0);
    const shortMod = matchedKey.split('_').slice(-1)[0];
    const allocFn: Function = exp.vo_alloc ?? exp[`${shortMod}_alloc`];
    const deallocFn: Function = exp.vo_dealloc ?? exp[`${shortMod}_dealloc`];
    if (!allocFn || !deallocFn) throw new Error(`ext module '${matchedKey}' missing alloc/dealloc exports`);
    const inputPtr: number = allocFn(input.length);
    new Uint8Array(exp.memory.buffer).set(input, inputPtr);
    const outLenPtr: number = allocFn(4);
    const outPtr: number = extFunc(inputPtr, input.length, outLenPtr);
    deallocFn(inputPtr, input.length);
    if (outPtr === 0) { deallocFn(outLenPtr, 4); return new Uint8Array(0); }
    const outLen: number = new Uint32Array(exp.memory.buffer, outLenPtr, 1)[0];
    const result = new Uint8Array(exp.memory.buffer, outPtr, outLen).slice();
    deallocFn(outPtr, outLen);
    deallocFn(outLenPtr, 4);
    return result;
  };

  // Replay path for ext bridge HostEventWaitAndReplay.
  // The standalone .wasm module's waitForEvent export always returns TAG_SUSPEND.
  // On replay, the ext bridge calls voCallExtReplay with resume data.
  // We decode it here into tagged output format.
  (window as any).voCallExtReplay = (externName: string, resumeData: Uint8Array): Uint8Array => {
    // waitForEvent replay: resumeData = [i32 handler_id LE][UTF-8 payload]
    // Return: [TAG_VALUE(handler_id as u64)][TAG_BYTES(payload)]
    if (externName.endsWith('_waitForEvent')) {
      const handlerId = new DataView(resumeData.buffer, resumeData.byteOffset, 4).getInt32(0, true);
      const payloadEnc = resumeData.subarray(4);
      const result = new Uint8Array(9 + 5 + payloadEnc.length);
      const dv = new DataView(result.buffer);
      result[0] = 0xE2; // TAG_VALUE
      dv.setInt32(1, handlerId, true);
      dv.setInt32(5, handlerId < 0 ? -1 : 0, true);
      result[9] = 0xE3; // TAG_BYTES
      dv.setUint32(10, payloadEnc.length, true);
      result.set(payloadEnc, 14);
      return result;
    }
    return new Uint8Array(0);
  };

  // 4) Install shell handler dependencies declared in vo.mod into the JS VFS.
  //    Purge existing module dirs first — previous runs may have persisted corrupted data
  //    to OPFS because writeFile was storing a temporary WASM memory view rather than a copy.
  for (const depMod of wasmMod.getShellDepModules()) {
    vfs.removeAll('/' + depMod);
  }
  try {
    await wasmMod.preloadShellDeps();
  } catch (e) {
    throw new Error(`[studio] Failed to install shell deps: ${String(e)}`);
  }

  // Pre-warm shell handler compilation (so errors surface here, not on first shell op)
  const shellErr = wasmMod.initShellHandler();
  if (shellErr) {
    throw new Error(`[studio] Shell handler compilation failed: ${shellErr}`);
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

  // 5) Build bridge — all FS ops route through ShellClient → Vo shell handler
  const wasmRouter  = new WasmShellRouter(wasmMod, WASM_WORKSPACE);
  const shellClient = new ShellClient(new WasmTransport(wasmRouter));
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
    fsReadFile:  (path: string)                    => shellClient.exec({ kind: 'fs.read',   path }),
    fsWriteFile: (path: string, content: string)   => shellClient.exec({ kind: 'fs.write',  path, content }),
    fsMkdir:     (path: string)                    => shellClient.exec({ kind: 'fs.mkdir',  path }),
    fsRename:    (oldPath: string, newPath: string) => shellClient.exec({ kind: 'fs.rename', oldPath, newPath }),
    fsRemove:    (path: string, recursive: boolean) => shellClient.exec({ kind: 'fs.remove', path, recursive }),

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
