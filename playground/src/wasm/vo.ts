// Vo WASM runtime wrapper
import { vfs, initVFS } from '@vo-web';

export type RunStatus = 'idle' | 'running' | 'success' | 'error';

export interface RunResult {
  status: 'ok' | 'error' | 'compile_error';
  stdout: string;
  stderr: string;
}

export interface GuiResult {
  status: 'ok' | 'error' | 'compile_error';
  renderJson: string;
  error: string;
}

// Unified WASM module instance
let wasmModule: any = null;
let vfsInitialized = false;

async function loadWasm(): Promise<any> {
  if (wasmModule) return wasmModule;

  try {
    // Initialize VFS first (load from OPFS)
    if (!vfsInitialized) {
      await initVFS();
      vfsInitialized = true;
      console.log('VFS initialized');
    }

    const { default: init, compileAndRun, compileAndRunWithModules, version, initGuiApp, initGuiAppWithModules, handleGuiEvent } = await import('@vo-playground/vo_playground.js');
    await init();
    wasmModule = { compileAndRun, compileAndRunWithModules, version, initGuiApp, initGuiAppWithModules, handleGuiEvent };
    console.log('Vo Playground WASM loaded:', version());
    return wasmModule;
  } catch (e) {
    console.error('Failed to load Vo WASM:', e);
    throw new Error('Failed to load Vo runtime. Please refresh the page.');
  }
}

export async function runCode(source: string): Promise<RunResult> {
  const wasm = await loadWasm();

  const result = await wasm.compileAndRun(source, 'main.vo');

  return {
    status: result.status,
    stdout: result.stdout || '',
    stderr: result.stderr || '',
  };
}

export async function getVersion(): Promise<string> {
  const wasm = await loadWasm();
  return wasm.version();
}

// ============ GUI API ============

type RenderCallback = (json: string) => void;
let onRender: RenderCallback | null = null;

// Timer storage: Vo ID -> JS Interval ID
const activeTimers = new Map<number, number>();
const runningTimerHandlers = new Set<number>();
const activeTimeouts = new Map<number, number>();

// Save native timer functions before overriding
const nativeSetInterval = window.setInterval.bind(window);
const nativeClearInterval = window.clearInterval.bind(window);

// Serialize GUI events to prevent re-entrant VM access.
// Timers, timeouts, and UI events can fire while a previous event is still running.
let guiEventChain: Promise<unknown> = Promise.resolve();

// Synchronous busy guard: prevents re-entrant WASM calls.
// WASM is single-threaded; if a trap occurs during a call, subsequent calls
// must not attempt to re-enter the corrupted VM state.
let wasmBusy = false;
let wasmFatal = false;

function clearAllTimers() {
  activeTimers.forEach((jsId) => nativeClearInterval(jsId));
  activeTimers.clear();
  runningTimerHandlers.clear();
  activeTimeouts.forEach((jsId) => clearTimeout(jsId));
  activeTimeouts.clear();
}

export function setRenderCallback(callback: RenderCallback) {
  onRender = callback;
}

// Expose global functions for WASM to call
(window as any).startInterval = (id: number, ms: number) => {
	if (activeTimers.has(id)) {
		nativeClearInterval(activeTimers.get(id)!);
	}
	
	const intervalId = nativeSetInterval(async () => {
		if (runningTimerHandlers.has(id)) {
			return;
		}
		runningTimerHandlers.add(id);
		try {
			const result = await handleGuiEvent(-1, JSON.stringify({ id: id }));
			if (result.status === 'ok' && onRender) {
				onRender(result.renderJson);
			}
		} catch (e) {
			console.error('Timer handler failed:', e);
		} finally {
			runningTimerHandlers.delete(id);
		}
	}, ms);
	
	activeTimers.set(id, intervalId);
};

(window as any).clearInterval = (id: number) => {
  if (activeTimers.has(id)) {
    nativeClearInterval(activeTimers.get(id)!);
    activeTimers.delete(id);
  }
  runningTimerHandlers.delete(id);
};

(window as any).startTimeout = (id: number, ms: number) => {
  if (activeTimeouts.has(id)) {
    clearTimeout(activeTimeouts.get(id)!);
  }
  
  const timeoutId = setTimeout(async () => {
    activeTimeouts.delete(id);
    try {
      const result = await handleGuiEvent(-1, JSON.stringify({ id: id }));
      if (result.status === 'ok' && onRender) {
        onRender(result.renderJson);
      }
    } catch (e) {
      console.error('Timeout handler failed:', e);
    }
  }, ms);
  
  activeTimeouts.set(id, timeoutId);
};

(window as any).clearTimeout = (id: number) => {
  if (activeTimeouts.has(id)) {
    clearTimeout(activeTimeouts.get(id)!);
    activeTimeouts.delete(id);
  }
};

// Router
(window as any).navigate = (path: string) => {
  window.history.pushState({}, '', path);
  // Trigger re-render if needed? 
  // In our model, Navigate() in Vo calls this JS, then Vo updates itself.
  // But if User clicks Back button, we need to notify Vo.
  // For now, simple pushState. 
  // IMPORTANT: The Vo runtime needs to know when URL changes via Back/Forward.
  // We should listen to popstate and notify Vo?
  // But Vo doesn't have a callback for "onLocationChange" yet externally exposed easily.
  // Wait, VoGUI manages state. 
  // Ideally, `navigate` is just updating URL.
  // Handling popstate is separate.
};

(window as any).getCurrentPath = () => {
  return window.location.pathname;
};

// Listen for popstate (Back/Forward)
window.addEventListener('popstate', async () => {
  // We need to tell Vo application to re-render.
  // We can send a special event, but VoGUI currently pulls path in render().
  // So we just need to trigger an event loop cycle.
  // Let's send a "navigation" event or just a dummy event to wake it up?
  // HandlerID -3 for navigation event?
  if (wasmModule) {
      try {
        const result = await handleGuiEvent(-3, JSON.stringify({ path: window.location.pathname }));
        if (result.status === 'ok' && onRender) {
            onRender(result.renderJson);
        }
      } catch (e) {
          console.error("Failed to handle popstate:", e);
      }
  }
});

// ============ Canvas Registry ============

const voCanvasRegistry = new Map<string, HTMLCanvasElement>();

(window as any).voGetCanvas = (id: string): HTMLCanvasElement | null => {
  return voCanvasRegistry.get(id) ?? null;
};

(window as any).voRegisterCanvas = (id: string, canvas: HTMLCanvasElement) => {
  voCanvasRegistry.set(id, canvas);
};

(window as any).voUnregisterCanvas = (id: string) => {
  voCanvasRegistry.delete(id);
};

/** Clear all registered canvases (call on app reload). */
export function clearCanvasRegistry() {
  voCanvasRegistry.clear();
}

export async function initGuiApp(source: string): Promise<GuiResult> {
  // Reset fatal state so user can re-run after a crash
  wasmFatal = false;
  wasmBusy = false;

  // Clear timers and canvas registry on reload
  clearAllTimers();
  voCanvasRegistry.clear();

  const wasm = await loadWasm();
  const result = wasm.initGuiApp(source, 'main.vo');
  return {
    status: result.status,
    renderJson: result.renderJson || '',
    error: result.error || '',
  };
}

export async function handleGuiEvent(handlerId: number, payload: string): Promise<GuiResult> {
  if (wasmFatal) {
    return { status: 'error', renderJson: '', error: 'VM is in fatal state after a previous error' };
  }

  const wasm = await loadWasm();

  const run = async (): Promise<GuiResult> => {
    if (wasmBusy) {
      return { status: 'error', renderJson: '', error: 'Re-entrant WASM call blocked' };
    }
    if (wasmFatal) {
      return { status: 'error', renderJson: '', error: 'VM is in fatal state after a previous error' };
    }
    wasmBusy = true;
    try {
      const result = wasm.handleGuiEvent(handlerId, payload);
      return {
        status: result.status,
        renderJson: result.renderJson || '',
        error: result.error || '',
      };
    } catch (e) {
      // WASM trap (e.g. unreachable, memory access out of bounds).
      // Mark fatal and stop all timers to prevent cascading failures.
      wasmFatal = true;
      clearAllTimers();
      console.error('[Vo] Fatal WASM error, all timers stopped:', e);
      return { status: 'error', renderJson: '', error: String(e) };
    } finally {
      wasmBusy = false;
    }
  };

  const next = guiEventChain.then(run, run);
  guiEventChain = next.then(() => undefined, () => undefined);
  return next;
}

/// Run Vo source that contains `import "github.com/..."` statements.
/// Module fetching is done entirely in Rust/WASM via the browser Fetch API.
export async function runCodeWithModules(source: string): Promise<RunResult> {
  const wasm = await loadWasm();
  const result = await wasm.compileAndRunWithModules(source);
  return {
    status: result.status,
    stdout: result.stdout || '',
    stderr: result.stderr || '',
  };
}

/// Initialize a vogui app that also imports `import "github.com/..."` modules.
export async function initGuiAppWithModules(source: string): Promise<GuiResult> {
  // Reset fatal state so user can re-run after a crash
  wasmFatal = false;
  wasmBusy = false;

  // Clear timers and canvas registry on reload
  clearAllTimers();
  voCanvasRegistry.clear();

  const wasm = await loadWasm();
  const result = await wasm.initGuiAppWithModules(source);
  return {
    status: result.status,
    renderJson: result.renderJson || '',
    error: result.error || '',
  };
}

// ── Extension WASM dynamic loading ───────────────────────────────────────────
//
// Generic bridge: any Rust ext module that follows the standard Vo ext ABI
// (vo_alloc / vo_dealloc / <extern_name>) can be loaded and called without
// any per-module JS code.

// Maps normalized module key (e.g. "github_com_vo_lang_resvg") → WASM instance.
// Key matches normalize_module_key() in vo-web/runtime-wasm/src/ext_bridge.rs.
const extInstances = new Map<string, WebAssembly.Instance>();

// Maps normalized module key → wasm-bindgen module (has DOM access, can use canvas).
const extBindgenModules = new Map<string, any>();

/// Called from Rust via ext_bridge::load_wasm_ext_module.
/// `key` is the normalized module path (e.g. "github_com_vo_lang_resvg").
/// `jsGlueUrl` is optional: if provided, loads as wasm-bindgen module with DOM access.
(window as any).voSetupExtModule = async (key: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void> => {
  if (jsGlueUrl) {
    // wasm-bindgen module: full DOM access (canvas, WebGL, WebGPU, etc.)
    // Fetch JS glue text, then create a Blob URL with correct MIME type for import().
    // raw.githubusercontent.com serves text/plain which browsers reject for import().
    const resp = await fetch(jsGlueUrl);
    if (!resp.ok) throw new Error(`Failed to fetch JS glue: HTTP ${resp.status}`);
    const jsText = await resp.text();

    const blob = new Blob([jsText], { type: 'application/javascript' });
    const blobUrl = URL.createObjectURL(blob);
    try {
      const glue = await import(/* @vite-ignore */ blobUrl);
      // Instantiate the WASM module (synchronous internals only — no async start).
      await glue.default({ module_or_path: bytes.slice() });
      // Generic async init: any wasm-bindgen module that needs async setup
      // (GPU adapter, audio context, etc.) exports __voInit().
      // Await it so the hardware is ready before Vo code runs.
      if (typeof glue.__voInit === 'function') {
        await glue.__voInit();
      }
      extBindgenModules.set(key, glue);
    } finally {
      URL.revokeObjectURL(blobUrl);
    }
  } else {
    // Standalone module: current behavior (pure compute, no DOM access)
    // slice() copies the bytes out of Rust WASM memory before the async boundary
    const { instance } = await WebAssembly.instantiate(bytes.slice());
    extInstances.set(key, instance);
  }
};

/// Called from Rust's wasm_ext_bridge for any ext module function.
///
/// Dispatches to wasm-bindgen modules first, then falls back to standalone modules.
///
/// Standard Vo ext ABI (standalone):
///   vo_alloc(size) → ptr
///   vo_dealloc(ptr, size)
///   <extern_name>(input_ptr, input_len, out_len_ptr) → output_ptr
///
/// wasm-bindgen ABI:
///   Module exports named functions directly, called with Uint8Array input.
///
/// `externName` e.g. "github_com_vo_lang_resvg_Render"
/// `input` is raw bytes (UTF-8 string, JSON, or binary)
/// Returns result bytes, or empty Uint8Array on error.
(window as any).voCallExt = (externName: string, input: Uint8Array): Uint8Array => {
  // Try wasm-bindgen modules first (they have DOM/canvas access)
  let bindgenModule: any = undefined;
  let bindgenKey = '';
  for (const [key, mod] of extBindgenModules) {
    if (externName.startsWith(key) && key.length > bindgenKey.length) {
      bindgenKey = key;
      bindgenModule = mod;
    }
  }
  if (bindgenModule) {
    const funcName = externName.substring(bindgenKey.length + 1);
    if (typeof bindgenModule[funcName] === 'function') {
      let result: any;
      try {
        result = bindgenModule[funcName](input);
      } catch (e) {
        console.error('[voCallExt] Exception calling:', externName, e);
        return new Uint8Array(0);
      }
      // If the bindgen function is async, it returns a Promise.
      // Suppress the rejection so it doesn't become an unhandled rejection (SES_UNCAUGHT_EXCEPTION).
      if (result instanceof Promise) {
        result.catch(() => {});
        return new Uint8Array(0);
      }
      if (result instanceof Uint8Array) return result;
      if (typeof result === 'string') return new TextEncoder().encode(result);
      return new Uint8Array(0);
    }
    console.error('[voCallExt] Bindgen export not found:', funcName, 'in module:', bindgenKey);
    return new Uint8Array(0);
  }

  // Fall back to standalone modules
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

  const exp = instance.exports as any;

  // Extract function name from externName (e.g., "nativePack" from "github_com_vo_lang_zip_nativePack")
  const funcName = externName.substring(matchedKey.length + 1);

  // Resolve the extern function. Try in order:
  // 1. Short name (e.g., "nativePack") — preferred; decouples WASM exports from module path.
  // 2. Full extern name (e.g., "github_com_vo_lang_resvg_Render") — legacy full-name exports.
  // 3. Legacy: {shortModule}_{lowercase_func} (e.g., "resvg_render") — oldest convention.
  let extFunc: Function | undefined;
  if (typeof exp[funcName] === 'function') {
    extFunc = exp[funcName];
  } else if (typeof exp[externName] === 'function') {
    extFunc = exp[externName];
  } else {
    const parts = matchedKey.split('_');
    const shortName = parts[parts.length - 1];
    const legacyName = `${shortName}_${funcName.toLowerCase()}`;
    if (typeof exp[legacyName] === 'function') {
      extFunc = exp[legacyName];
    }
  }
  if (!extFunc) {
    console.error('[voCallExt] Export not found:', externName);
    return new Uint8Array(0);
  }

  // Resolve alloc/dealloc: standard (vo_alloc) or legacy ({shortModule}_alloc)
  let allocFn: Function | undefined;
  let deallocFn: Function | undefined;
  if (typeof exp.vo_alloc === 'function') {
    allocFn = exp.vo_alloc;
    deallocFn = exp.vo_dealloc;
  } else {
    const parts = matchedKey.split('_');
    const shortName = parts[parts.length - 1];
    allocFn = exp[`${shortName}_alloc`];
    deallocFn = exp[`${shortName}_dealloc`];
  }
  if (!allocFn || !deallocFn) {
    console.error('[voCallExt] Alloc/dealloc not found for module:', matchedKey);
    return new Uint8Array(0);
  }

  const inputPtr: number = (allocFn as any)(input.length);
  new Uint8Array(exp.memory.buffer).set(input, inputPtr);

  const outLenPtr: number = (allocFn as any)(4);
  const outPtr: number = (extFunc as any)(inputPtr, input.length, outLenPtr);
  (deallocFn as any)(inputPtr, input.length);

  if (outPtr === 0) {
    (deallocFn as any)(outLenPtr, 4);
    return new Uint8Array(0);
  }

  const outLen: number = new Uint32Array(exp.memory.buffer, outLenPtr, 1)[0];
  const result = new Uint8Array(exp.memory.buffer, outPtr, outLen).slice();
  (deallocFn as any)(outPtr, outLen);
  (deallocFn as any)(outLenPtr, 4);
  return result;
};
