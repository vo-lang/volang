// Vo WASM runtime wrapper
import { vfs, registerVFSBindings } from '@vo-web/vfs';

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
      await vfs.init();
      registerVFSBindings();
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

  const result = wasm.compileAndRun(source, 'main.vo');

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

// Save native timer functions before overriding
const nativeSetInterval = window.setInterval.bind(window);
const nativeClearInterval = window.clearInterval.bind(window);

export function setRenderCallback(callback: RenderCallback) {
  onRender = callback;
}

// Expose global functions for WASM to call
(window as any).startInterval = (id: number, ms: number) => {
  console.log('[VoGUI] startInterval called:', id, ms);
  if (activeTimers.has(id)) {
    nativeClearInterval(activeTimers.get(id)!);
  }
  
  const intervalId = nativeSetInterval(async () => {
    console.log('[VoGUI] Timer tick:', id);
    try {
      const result = await handleGuiEvent(-1, JSON.stringify({ id: id }));
      console.log('[VoGUI] Timer event result:', result.status, result.renderJson?.length || 0);
      if (result.status === 'ok' && onRender) {
        onRender(result.renderJson);
      }
    } catch (e) {
      console.error('Timer handler failed:', e);
    }
  }, ms);
  
  activeTimers.set(id, intervalId);
  console.log('[VoGUI] Timer registered, JS interval ID:', intervalId);
};

(window as any).clearInterval = (id: number) => {
  if (activeTimers.has(id)) {
    nativeClearInterval(activeTimers.get(id)!);
    activeTimers.delete(id);
  }
};

// Timeout support
const activeTimeouts = new Map<number, number>();

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

export async function initGuiApp(source: string): Promise<GuiResult> {
  // Clear timers on reload - use native function with JS interval IDs
  activeTimers.forEach((jsIntervalId) => nativeClearInterval(jsIntervalId));
  activeTimers.clear();

  const wasm = await loadWasm();
  const result = wasm.initGuiApp(source, 'main.vo');
  return {
    status: result.status,
    renderJson: result.renderJson || '',
    error: result.error || '',
  };
}

export async function handleGuiEvent(handlerId: number, payload: string): Promise<GuiResult> {
  const wasm = await loadWasm();
  const result = wasm.handleGuiEvent(handlerId, payload);
  return {
    status: result.status,
    renderJson: result.renderJson || '',
    error: result.error || '',
  };
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
  activeTimers.forEach((jsIntervalId) => nativeClearInterval(jsIntervalId));
  activeTimers.clear();

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

/// Called from Rust via ext_bridge::load_wasm_ext_module.
/// `key` is the normalized module path (e.g. "github_com_vo_lang_resvg").
(window as any).voSetupExtModule = async (key: string, bytes: Uint8Array): Promise<void> => {
  const { instance } = await WebAssembly.instantiate(bytes);
  extInstances.set(key, instance);
};

/// Called from Rust's wasm_ext_bridge for any ext module function.
///
/// Standard Vo ext ABI:
///   vo_alloc(size) → ptr
///   vo_dealloc(ptr, size)
///   <extern_name>(input_ptr, input_len, out_len_ptr) → output_ptr
///
/// `externName` e.g. "github_com_vo_lang_resvg_Render"
/// `input` is raw bytes (UTF-8 string, JSON, or binary)
/// Returns result bytes, or empty Uint8Array on error.
(window as any).voCallExt = (externName: string, input: Uint8Array): Uint8Array => {
  // Find the instance: the module key is the longest prefix of externName
  // that matches a loaded module key.
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
  if (typeof exp[externName] !== 'function') {
    console.error('[voCallExt] Export not found:', externName);
    return new Uint8Array(0);
  }

  const inputPtr: number = exp.vo_alloc(input.length);
  new Uint8Array(exp.memory.buffer).set(input, inputPtr);

  const outLenPtr: number = exp.vo_alloc(4);
  const outPtr: number = exp[externName](inputPtr, input.length, outLenPtr);
  exp.vo_dealloc(inputPtr, input.length);

  if (outPtr === 0) {
    exp.vo_dealloc(outLenPtr, 4);
    return new Uint8Array(0);
  }

  const outLen: number = new Uint32Array(exp.memory.buffer, outLenPtr, 1)[0];
  const result = new Uint8Array(exp.memory.buffer, outPtr, outLen).slice();
  exp.vo_dealloc(outPtr, outLen);
  exp.vo_dealloc(outLenPtr, 4);
  return result;
};
