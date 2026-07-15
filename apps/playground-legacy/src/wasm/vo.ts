// Vo WASM runtime wrapper
import { vfs, initVFS } from '@vo-web';

export type RunStatus = 'idle' | 'running' | 'success' | 'error';

export interface RunResult {
  status: 'ok' | 'exited' | 'error' | 'compile_error';
  stdout: string;
  stderr: string;
  exitCode: number | null;
}

export interface GuiResult {
  status: 'ok' | 'error' | 'compile_error';
  renderBytes: Uint8Array;
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

    const {
      default: init,
      compileAndRun,
      compileAndRunWithModules,
      version,
      initGuiApp,
      initGuiAppWithModules,
      handleGuiEvent,
      forgetWasmExtModuleOwner,
      clearWasmExtModuleOwners,
    } = await import('@vo-apps/playground-legacy/vo_playground.js');
    await init();
    wasmModule = {
      compileAndRun,
      compileAndRunWithModules,
      version,
      initGuiApp,
      initGuiAppWithModules,
      handleGuiEvent,
      forgetWasmExtModuleOwner,
      clearWasmExtModuleOwners,
    };
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
    exitCode: typeof result.exitCode === 'number' ? result.exitCode : null,
  };
}

export async function getVersion(): Promise<string> {
  const wasm = await loadWasm();
  return wasm.version();
}

// ============ GUI API ============

type RenderCallback = (bytes: Uint8Array) => void;
let onRender: RenderCallback | null = null;

// Timer storage: Vo ID -> JS Interval ID
const activeTimers = new Map<number, number>();
const runningTimerHandlers = new Set<number>();
const activeTimeouts = new Map<number, number>();

// Animation frame registry: Vo animframe ID -> rAF request ID
const activeAnimFrames = new Map<number, number>();

// Game loop registry: Vo loop ID -> { rafId, lastTs }
const activeGameLoops = new Map<number, { rafId: number; lastTs: number }>();

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
  activeAnimFrames.forEach((rafId) => cancelAnimationFrame(rafId));
  activeAnimFrames.clear();
  activeGameLoops.forEach((state) => cancelAnimationFrame(state.rafId));
  activeGameLoops.clear();
}

export function setRenderCallback(callback: RenderCallback) {
  onRender = callback;
}

const EMPTY_BYTES = new Uint8Array(0);

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
				onRender(result.renderBytes);
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
        onRender(result.renderBytes);
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
            onRender(result.renderBytes);
        }
      } catch (e) {
          console.error("Failed to handle popstate:", e);
      }
  }
});

// ============ Animation Frame & Game Loop ============

// eventIDAnimFrame = -4, eventIDGameLoop = -5 (must match canvas.vo constants)

(window as any).voguiStartAnimFrame = (id: number) => {
  if (activeAnimFrames.has(id)) {
    cancelAnimationFrame(activeAnimFrames.get(id)!);
  }
  const rafId = requestAnimationFrame(async () => {
    activeAnimFrames.delete(id);
    const result = await handleGuiEvent(-4, JSON.stringify({ Id: id }));
    if (result.status === 'ok' && onRender) {
      onRender(result.renderBytes);
    }
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
  if (activeGameLoops.has(id)) {
    cancelAnimationFrame(activeGameLoops.get(id)!.rafId);
  }
  const state = { rafId: 0, lastTs: 0 };
  activeGameLoops.set(id, state);

  function tick(ts: number): void {
    if (!activeGameLoops.has(id)) return;
    const loop = activeGameLoops.get(id)!;
    const dt = loop.lastTs === 0 ? 0 : ts - loop.lastTs;
    loop.lastTs = ts;
    // handleGuiEvent manages guiEventChain internally; call it directly
    // (same pattern as startInterval). Schedule next rAF after completion.
    handleGuiEvent(-5, JSON.stringify({ Dt: dt })).then(
      (result) => {
        if (result.status === 'ok' && onRender) {
          onRender(result.renderBytes);
        }
        if (result.status === 'error') {
          console.error('[playground] GameLoop error:', result.error);
          activeGameLoops.delete(id);
          return;
        }
        if (activeGameLoops.has(id)) {
          activeGameLoops.get(id)!.rafId = requestAnimationFrame(tick);
        }
      },
      (e) => {
        console.error('[playground] GameLoop handler error:', e);
        activeGameLoops.delete(id);
      }
    );
  }

  state.rafId = requestAnimationFrame(tick);
};

(window as any).voguiStopGameLoop = (id: number) => {
  const loop = activeGameLoops.get(id);
  if (loop) {
    cancelAnimationFrame(loop.rafId);
    activeGameLoops.delete(id);
  }
};

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
    renderBytes: result.renderBytes instanceof Uint8Array ? result.renderBytes : EMPTY_BYTES,
    error: result.error || '',
  };
}

export async function handleGuiEvent(handlerId: number, payload: string): Promise<GuiResult> {
  if (wasmFatal) {
    return { status: 'error', renderBytes: EMPTY_BYTES, error: 'VM is in fatal state after a previous error' };
  }

  const wasm = await loadWasm();

  const run = async (): Promise<GuiResult> => {
    if (wasmBusy) {
      return { status: 'error', renderBytes: EMPTY_BYTES, error: 'Re-entrant WASM call blocked' };
    }
    if (wasmFatal) {
      return { status: 'error', renderBytes: EMPTY_BYTES, error: 'VM is in fatal state after a previous error' };
    }
    wasmBusy = true;
    try {
      const result = wasm.handleGuiEvent(handlerId, payload);
      return {
        status: result.status,
        renderBytes: result.renderBytes instanceof Uint8Array ? result.renderBytes : EMPTY_BYTES,
        error: result.error || '',
      };
    } catch (e) {
      // WASM trap (e.g. unreachable, memory access out of bounds).
      // Mark fatal and stop all timers to prevent cascading failures.
      wasmFatal = true;
      clearAllTimers();
      console.error('[Vo] Fatal WASM error, all timers stopped:', e);
      return { status: 'error', renderBytes: EMPTY_BYTES, error: String(e) };
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
    exitCode: typeof result.exitCode === 'number' ? result.exitCode : null,
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
    renderBytes: result.renderBytes instanceof Uint8Array ? result.renderBytes : EMPTY_BYTES,
    error: result.error || '',
  };
}

// ── Extension WASM dynamic loading ───────────────────────────────────────────
//
// Generic bridge: any Rust ext module that follows the standard Vo ext ABI
// (vo_alloc / vo_dealloc / <exact_export_key>) can be loaded and called without
// any per-module JS code.

export const WASM_EXTENSION_PROTOCOL_VERSION = 3;
export const WASM_EXTENSION_EXPORT_PREFIX = '__vo_ext_';
const MAX_EXTERN_NAME_BYTES = 4 * 1024;
const MAX_CANONICAL_MODULE_OWNER_BYTES = 255;
const MAX_PORTABLE_PACKAGE_COMPONENT_BYTES = 255;
const EXTERN_NAME_PREFIX_BYTES = new Uint8Array([0x76, 0x6f, 0x31, 0x3a]);
const UTF8_ENCODER = new TextEncoder();
const FATAL_UTF8_DECODER = new TextDecoder('utf-8', { fatal: true, ignoreBOM: true });

export type DecodedVoExternName = Readonly<{
  packageName: string;
  functionName: string;
}>;

function assertWellFormedUnicode(value: string, label: string): void {
  for (let index = 0; index < value.length; index += 1) {
    const unit = value.charCodeAt(index);
    if (unit >= 0xd800 && unit <= 0xdbff) {
      const next = value.charCodeAt(index + 1);
      if (next < 0xdc00 || next > 0xdfff) {
        throw new Error(`${label} contains an unpaired UTF-16 surrogate`);
      }
      index += 1;
    } else if (unit >= 0xdc00 && unit <= 0xdfff) {
      throw new Error(`${label} contains an unpaired UTF-16 surrogate`);
    }
  }
}

function decodeExternUtf8Field(bytes: Uint8Array, start: number, end: number, field: string): string {
  try {
    return FATAL_UTF8_DECODER.decode(bytes.subarray(start, end));
  } catch {
    throw new Error(`extern ${field} is not valid UTF-8`);
  }
}

function parseExternByteLength(bytes: Uint8Array, cursorRef: { value: number }, field: string): number {
  const start = cursorRef.value;
  let value = 0;
  while (cursorRef.value < bytes.length && bytes[cursorRef.value] !== 0x3a) {
    const byte = bytes[cursorRef.value];
    if (byte < 0x30 || byte > 0x39) {
      throw new Error(`extern ${field} length contains a non-decimal byte`);
    }
    value = value * 10 + (byte - 0x30);
    if (!Number.isSafeInteger(value) || value > MAX_EXTERN_NAME_BYTES) {
      throw new Error(`extern ${field} length exceeds the ${MAX_EXTERN_NAME_BYTES}-byte wire limit`);
    }
    cursorRef.value += 1;
  }
  if (cursorRef.value === bytes.length || cursorRef.value === start) {
    throw new Error(`extern ${field} length is missing or unterminated`);
  }
  if (bytes[start] === 0x30 && cursorRef.value - start > 1) {
    throw new Error(`extern ${field} length has a leading zero`);
  }
  if (value === 0) {
    throw new Error(`extern ${field} length must be positive`);
  }
  cursorRef.value += 1;
  return value;
}

export function decodeVoExternName(encoded: string): DecodedVoExternName {
  assertWellFormedUnicode(encoded, 'extern name');
  const bytes = UTF8_ENCODER.encode(encoded);
  if (bytes.length > MAX_EXTERN_NAME_BYTES) {
    throw new Error(`encoded extern name exceeds the ${MAX_EXTERN_NAME_BYTES}-byte wire limit`);
  }
  for (let index = 0; index < EXTERN_NAME_PREFIX_BYTES.length; index += 1) {
    if (bytes[index] !== EXTERN_NAME_PREFIX_BYTES[index]) {
      throw new Error("extern name must begin with canonical prefix 'vo1:'");
    }
  }
  const cursor = { value: EXTERN_NAME_PREFIX_BYTES.length };
  const packageLength = parseExternByteLength(bytes, cursor, 'package');
  const packageEnd = cursor.value + packageLength;
  if (!Number.isSafeInteger(packageEnd) || packageEnd >= bytes.length) {
    throw new Error('extern package is shorter than its declared byte length');
  }
  const packageName = decodeExternUtf8Field(bytes, cursor.value, packageEnd, 'package');
  if (bytes[packageEnd] !== 0x3a) {
    throw new Error("extern package must be followed by ':'");
  }
  cursor.value = packageEnd + 1;
  const functionLength = parseExternByteLength(bytes, cursor, 'function');
  const functionEnd = cursor.value + functionLength;
  if (!Number.isSafeInteger(functionEnd) || functionEnd > bytes.length) {
    throw new Error('extern function is shorter than its declared byte length');
  }
  if (functionEnd !== bytes.length) {
    throw new Error('extern name has trailing bytes');
  }
  const functionName = decodeExternUtf8Field(bytes, cursor.value, functionEnd, 'function');
  return { packageName, functionName };
}

function wasmExtensionExportKeyFromCanonical(encoded: string): string {
  const bytes = UTF8_ENCODER.encode(encoded);
  let exportKey = WASM_EXTENSION_EXPORT_PREFIX;
  for (const byte of bytes) {
    exportKey += byte.toString(16).padStart(2, '0');
  }
  return exportKey;
}

/** Derive the exact protocol-v3 export key after strict canonical decoding. */
export function voExternExportKey(encoded: string): string {
  decodeVoExternName(encoded);
  return wasmExtensionExportKeyFromCanonical(encoded);
}

// Locked against vo-common-core by the Rust source-contract test.
export const WASM_EXTENSION_EXPORT_KEY_CONTRACT_VECTORS = [
  [
    'vo1:24:github.com/acme/graphics:4:Draw',
    '__vo_ext_766f313a32343a6769746875622e636f6d2f61636d652f67726170686963733a343a44726177',
  ],
  [
    'vo1:22:github.com/acme/图形:6:绘制',
    '__vo_ext_766f313a32323a6769746875622e636f6d2f61636d652fe59bbee5bda23a363ae7bb98e588b6',
  ],
  [
    'vo1:31:github.com/acme/graphics/render:4:Draw',
    '__vo_ext_766f313a33313a6769746875622e636f6d2f61636d652f67726170686963732f72656e6465723a343a44726177',
  ],
] as const;

// TextDecoder must preserve a field-leading UTF-8 BOM exactly like Rust str.
export const VO_EXTERN_BOM_CONTRACT_VECTORS = [
  ['vo1:27:\uFEFFgithub.com/acme/graphics:4:Draw', '\uFEFFgithub.com/acme/graphics', 'Draw'],
  ['vo1:24:github.com/acme/graphics:7:\uFEFFDraw', 'github.com/acme/graphics', '\uFEFFDraw'],
] as const;

// Descendant package components are accepted only in their canonical NFC spelling.
export const VO_PACKAGE_OWNER_NFC_CONTRACT_VECTORS = [
  ['github.com/acme/graphics/é', true],
  ['github.com/acme/graphics/e\u0301', false],
] as const;

function isReservedPortableModuleStem(stem: string): boolean {
  if (['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].includes(stem)) {
    return true;
  }
  return /^(com|lpt)[1-9]$/.test(stem);
}

function isCanonicalPortableModuleSegment(segment: string): boolean {
  return segment.length <= MAX_CANONICAL_MODULE_OWNER_BYTES
    && /^[a-z0-9][a-z0-9._-]*$/.test(segment)
    && !segment.endsWith('.')
    && !isReservedPortableModuleStem(segment.split('.')[0]);
}

function equalsAsciiIgnoreCase(value: string, expected: string): boolean {
  const characters = Array.from(value);
  if (characters.length !== expected.length) return false;
  return characters.every((character, index) => {
    const code = character.charCodeAt(0);
    const lower = code >= 0x41 && code <= 0x5a ? code + 0x20 : code;
    return lower === expected.charCodeAt(index);
  });
}

function isNumberedWindowsDevicePackageStem(stem: string, prefix: 'com' | 'lpt'): boolean {
  const characters = Array.from(stem);
  return characters.length === 4
    && equalsAsciiIgnoreCase(characters.slice(0, 3).join(''), prefix)
    && /^[1-9¹²³]$/.test(characters[3]);
}

function isPortablePackageSegment(segment: string): boolean {
  try {
    assertWellFormedUnicode(segment, 'extern package segment');
  } catch {
    return false;
  }
  if (
    !segment
    || UTF8_ENCODER.encode(segment).length > MAX_PORTABLE_PACKAGE_COMPONENT_BYTES
    || segment.normalize('NFC') !== segment
    || segment === '.'
    || segment === '..'
    || /^\p{White_Space}|\p{White_Space}$/u.test(segment)
    || segment.endsWith('.')
    || segment.includes('/')
    || segment.includes('\\')
    || segment.includes('@')
    || /[\p{Control}<>:"|?*]/u.test(segment)
  ) {
    return false;
  }
  const stem = segment.split('.')[0];
  if (['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].some((name) => equalsAsciiIgnoreCase(stem, name))) {
    return false;
  }
  return !isNumberedWindowsDevicePackageStem(stem, 'com')
    && !isNumberedWindowsDevicePackageStem(stem, 'lpt');
}

export function validateCanonicalModuleOwner(owner: string): void {
  assertWellFormedUnicode(owner, 'WASM extension module owner');
  const ownerBytes = UTF8_ENCODER.encode(owner);
  const segments = owner.split('/');
  if (
    !owner
    || owner.startsWith('/')
    || owner.endsWith('/')
    || ownerBytes.length > MAX_CANONICAL_MODULE_OWNER_BYTES
    || segments.length < 3
    || segments[0] !== 'github.com'
  ) {
    throw new Error(`invalid canonical WASM extension module owner '${owner}'`);
  }
  for (const segment of segments) {
    if (!isCanonicalPortableModuleSegment(segment)) {
      throw new Error(`invalid canonical WASM extension module owner '${owner}'`);
    }
  }
  const last = segments[segments.length - 1];
  if (/^v[0-9]+$/.test(last)) {
    const digits = last.slice(1);
    if (
      (digits.length > 1 && digits.startsWith('0'))
      || digits === '0'
      || digits === '1'
      || BigInt(digits) > 18_446_744_073_709_551_615n
    ) {
      throw new Error(`invalid canonical WASM extension module owner '${owner}'`);
    }
  }
}

export function isVoPackageOwnedByModule(packageName: string, owner: string): boolean {
  try {
    validateCanonicalModuleOwner(owner);
  } catch {
    return false;
  }
  if (typeof packageName !== 'string' || UTF8_ENCODER.encode(packageName).length > MAX_EXTERN_NAME_BYTES) {
    return false;
  }
  if (packageName === owner) {
    return true;
  }
  if (!packageName.startsWith(`${owner}/`)) {
    return false;
  }
  const suffix = packageName.slice(owner.length + 1);
  return suffix.length > 0 && suffix.split('/').every(isPortablePackageSegment);
}

export function selectVoExternModuleOwner(packageName: string, owners: Iterable<string>): string | null {
  let selected: string | null = null;
  for (const owner of owners) {
    if (isVoPackageOwnedByModule(packageName, owner) && (selected === null || owner.length > selected.length)) {
      selected = owner;
    }
  }
  return selected;
}

// Maps exact canonical module owner → WASM instance.
const extInstances = new Map<string, WebAssembly.Instance>();

// Maps exact canonical module owner → wasm-bindgen module (has DOM access, can use canvas).
const extBindgenModules = new Map<string, any>();
type LoadedExtensionArtifact = Readonly<{
  bytes: Uint8Array;
  jsGlueSource: string | null;
  artifactToken: string;
}>;
const extArtifacts = new Map<string, LoadedExtensionArtifact>();
type PreparedExtensionArtifact =
  | Readonly<{
      mode: 'bindgen';
      artifact: LoadedExtensionArtifact;
      bindgenModule: any;
    }>
  | Readonly<{
      mode: 'standalone';
      artifact: LoadedExtensionArtifact;
      instance: WebAssembly.Instance;
    }>;
type PendingExtensionLoad = {
  bytes: Uint8Array;
  hasJsGlue: boolean;
  jsGlueSourcePromise: Promise<string | null>;
  artifactToken: string;
  expectedResetGeneration: number;
  expectedOwnerGeneration: number;
  promise: Promise<void>;
  prepared: PreparedExtensionArtifact | null;
};
type ExtensionLoadHandle = Readonly<{
  artifactToken: string;
  leaseToken: string;
  ready: Promise<void>;
}>;
type ExtensionLoadLease = Readonly<{ owner: string; artifactToken: string }>;
type ExtensionLoadHandleLease = Readonly<ExtensionLoadLease & { leaseToken: string }>;
const extLoadOperations = new Map<string, PendingExtensionLoad>();
const extLoadLeases = new Map<string, ExtensionLoadLease>();
const extLoadHandleLeases = new WeakMap<ExtensionLoadHandle, ExtensionLoadHandleLease>();
const extOwnerLoadGenerations = new Map<string, number>();
const extExhaustedOwnerLoads = new Set<string>();
let extResetGeneration = 0;
let nextExtLoadLease = 0;

function nextExtensionGeneration(current: number, label: string): number {
  if (!Number.isSafeInteger(current) || current < 0 || current >= Number.MAX_SAFE_INTEGER) {
    throw new Error(`${label} lifecycle generation is exhausted`);
  }
  return current + 1;
}

function extensionLoadGenerationToken(resetGeneration: number, ownerGeneration: number): string {
  return `${resetGeneration}:${ownerGeneration}`;
}

function allocateExtensionLoadLease(owner: string, artifactToken: string): string {
  const nextLease = nextExtensionGeneration(nextExtLoadLease, 'WASM extension load lease');
  const leaseToken = String(nextLease);
  try {
    extLoadLeases.set(leaseToken, { owner, artifactToken });
  } catch (error) {
    extLoadLeases.delete(leaseToken);
    throw error;
  }
  nextExtLoadLease = nextLease;
  return leaseToken;
}

function extensionLoadHandle(
  owner: string,
  artifactToken: string,
  leaseToken: string,
  ready: Promise<void>,
): ExtensionLoadHandle {
  try {
    const handle = Object.freeze({
      artifactToken,
      leaseToken,
      ready,
    });
    extLoadHandleLeases.set(handle, { owner, artifactToken, leaseToken });
    return handle;
  } catch (error) {
    try {
      abortExtensionLoadLease(owner, artifactToken, leaseToken);
    } catch (cleanupError) {
      console.error('[voSetupExtModule] failed to release an unpublished setup handle:', cleanupError);
    }
    throw error;
  }
}

function requireExtensionProtocolV3(exports: Record<string, unknown>, owner: string, mode: string): void {
  const versionExport = exports.vo_ext_protocol_version;
  if (typeof versionExport !== 'function') {
    throw new Error(
      `WASM extension '${owner}' (${mode}) is missing vo_ext_protocol_version(); rebuild it for protocol v${WASM_EXTENSION_PROTOCOL_VERSION}`,
    );
  }
  let version: unknown;
  try {
    version = (versionExport as () => unknown)();
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    throw new Error(`WASM extension '${owner}' protocol version export failed: ${detail}`);
  }
  if (typeof version !== 'number' || !Number.isSafeInteger(version) || version !== WASM_EXTENSION_PROTOCOL_VERSION) {
    throw new Error(
      `WASM extension '${owner}' (${mode}) uses protocol ${String(version)}; expected v${WASM_EXTENSION_PROTOCOL_VERSION}`,
    );
  }
}

function bindgenProtocolExports(initialized: unknown, owner: string): Record<string, unknown> {
  if (initialized !== null && (typeof initialized === 'object' || typeof initialized === 'function')) {
    return initialized as Record<string, unknown>;
  }
  throw new Error(
    `WASM extension '${owner}' bindgen initializer did not return raw WebAssembly instance exports`,
  );
}

function disposePreparedExtensionArtifact(prepared: PreparedExtensionArtifact): void {
  if (prepared.mode === 'bindgen' && typeof prepared.bindgenModule.__voDispose === 'function') {
    try {
      prepared.bindgenModule.__voDispose();
    } catch (error) {
      console.error('[voAbortExtModuleLoad] bindgen dispose failed:', error);
    }
  }
}

function removeExtensionLoadLeases(owner: string, artifactToken?: string): void {
  for (const [leaseToken, lease] of extLoadLeases) {
    if (lease.owner === owner && (artifactToken === undefined || lease.artifactToken === artifactToken)) {
      extLoadLeases.delete(leaseToken);
    }
  }
}

function cancelPendingExtensionLoad(owner: string, artifactToken?: string): boolean {
  const operation = extLoadOperations.get(owner);
  if (!operation || (artifactToken !== undefined && operation.artifactToken !== artifactToken)) {
    return false;
  }
  extLoadOperations.delete(owner);
  removeExtensionLoadLeases(owner, operation.artifactToken);
  const prepared = operation.prepared;
  operation.prepared = null;
  if (prepared) disposePreparedExtensionArtifact(prepared);
  return true;
}

function abortExtensionLoadLease(
  key: string,
  artifactToken: string,
  leaseToken: string,
): void {
  validateCanonicalModuleOwner(key);
  if (typeof artifactToken !== 'string' || typeof leaseToken !== 'string') return;
  const lease = extLoadLeases.get(leaseToken);
  if (lease?.owner !== key || lease.artifactToken !== artifactToken) return;
  extLoadLeases.delete(leaseToken);
  if (extArtifacts.get(key)?.artifactToken === artifactToken) return;
  const operation = extLoadOperations.get(key);
  if (operation?.artifactToken !== artifactToken) return;
  const hasAnotherLease = Array.from(extLoadLeases.values()).some(
    (candidate) => candidate.owner === key && candidate.artifactToken === artifactToken,
  );
  if (hasAnotherLease) return;
  let nextOwnerGeneration: number;
  try {
    nextOwnerGeneration = nextExtensionGeneration(
      extOwnerLoadGenerations.get(key) ?? 0,
      `WASM extension '${key}'`,
    );
  } catch (error) {
    // The final lease still owns an in-flight artifact. Poison this owner and
    // cancel it explicitly so generation exhaustion cannot revive the same
    // token or leave a prepared instance unreachable.
    extExhaustedOwnerLoads.add(key);
    cancelPendingExtensionLoad(key, artifactToken);
    throw error;
  }
  extOwnerLoadGenerations.set(key, nextOwnerGeneration);
  cancelPendingExtensionLoad(key, artifactToken);
}

function unloadExtModule(owner: string): void {
  validateCanonicalModuleOwner(owner);
  const hasTrackedArtifact = extArtifacts.has(owner)
    || extBindgenModules.has(owner)
    || extInstances.has(owner);
  if (hasTrackedArtifact && typeof wasmModule?.forgetWasmExtModuleOwner !== 'function') {
    throw new Error(`WASM extension '${owner}' cannot be disposed without its Rust owner-state bridge`);
  }
  // Preflight generation capacity and Rust synchronization before changing
  // JavaScript state. A Rust-side epoch error leaves both routing layers live.
  const nextOwnerGeneration = nextExtensionGeneration(
    extOwnerLoadGenerations.get(owner) ?? 0,
    `WASM extension '${owner}'`,
  );
  const bindgen = extBindgenModules.get(owner);
  wasmModule?.forgetWasmExtModuleOwner(owner);
  extOwnerLoadGenerations.set(owner, nextOwnerGeneration);
  const pendingOperation = extLoadOperations.get(owner);
  const prepared = pendingOperation?.prepared ?? null;
  if (pendingOperation) {
    pendingOperation.prepared = null;
    extLoadOperations.delete(owner);
  }
  removeExtensionLoadLeases(owner);
  extBindgenModules.delete(owner);
  extInstances.delete(owner);
  extArtifacts.delete(owner);
  if (prepared) disposePreparedExtensionArtifact(prepared);
  if (bindgen && typeof bindgen.__voDispose === 'function') {
    try {
      bindgen.__voDispose();
    } catch (error) {
      console.error('[voDisposeExtModule] bindgen dispose failed:', error);
    }
  }
}

function bytesEqual(left: Uint8Array, right: Uint8Array): boolean {
  if (left.length !== right.length) return false;
  for (let index = 0; index < left.length; index += 1) {
    if (left[index] !== right[index]) return false;
  }
  return true;
}

async function readJsGlueSource(jsGlueUrl: string): Promise<string> {
  const response = await fetch(jsGlueUrl, { cache: 'no-store' });
  if (!response.ok) throw new Error(`Failed to fetch JS glue: HTTP ${response.status}`);
  return response.text();
}

function assertExtensionLoadActive(
  owner: string,
  expectedResetGeneration: number,
  expectedOwnerGeneration: number,
): void {
  if (
    extExhaustedOwnerLoads.has(owner)
    || extResetGeneration !== expectedResetGeneration
    || (extOwnerLoadGenerations.get(owner) ?? 0) !== expectedOwnerGeneration
  ) {
    throw new Error(`WASM extension '${owner}' load was cancelled by disposal or runtime reset`);
  }
}

function unloadAllExtModules(): void {
  const hasTrackedArtifacts = extArtifacts.size > 0
    || extBindgenModules.size > 0
    || extInstances.size > 0;
  if (hasTrackedArtifacts && typeof wasmModule?.clearWasmExtModuleOwners !== 'function') {
    throw new Error('WASM extensions cannot be reset without their Rust owner-state bridge');
  }
  // Preserve all JavaScript state until the Rust owner catalog has accepted
  // the reset, including when its lifecycle epoch is exhausted.
  const nextResetGeneration = nextExtensionGeneration(extResetGeneration, 'WASM extension reset');
  const preparedArtifacts = Array.from(extLoadOperations.values())
    .map((operation) => operation.prepared)
    .filter((prepared): prepared is PreparedExtensionArtifact => prepared !== null);
  const bindgenModules = Array.from(new Set(extBindgenModules.values()));
  wasmModule?.clearWasmExtModuleOwners();
  extResetGeneration = nextResetGeneration;
  extOwnerLoadGenerations.clear();
  extLoadOperations.clear();
  extLoadLeases.clear();
  extExhaustedOwnerLoads.clear();
  extBindgenModules.clear();
  extInstances.clear();
  extArtifacts.clear();
  for (const prepared of preparedArtifacts) disposePreparedExtensionArtifact(prepared);
  for (const module of bindgenModules) {
    if (module && typeof module.__voDispose === 'function') {
      try {
        module.__voDispose();
      } catch (error) {
        console.error('[voDisposeAllExtModules] bindgen dispose failed:', error);
      }
    }
  }
}

function commitExtModule(
  key: string,
  artifactToken: string,
  leaseToken: string,
): boolean {
  validateCanonicalModuleOwner(key);
  if (typeof artifactToken !== 'string' || typeof leaseToken !== 'string') return false;
  const lease = extLoadLeases.get(leaseToken);
  if (lease?.owner !== key || lease.artifactToken !== artifactToken) return false;
  if (artifactToken !== extensionLoadGenerationToken(
    extResetGeneration,
    extOwnerLoadGenerations.get(key) ?? 0,
  )) {
    return false;
  }

  const activeArtifact = extArtifacts.get(key);
  if (activeArtifact) {
    if (activeArtifact.artifactToken !== artifactToken) return false;
    const bindgenLoaded = extBindgenModules.has(key) && !extInstances.has(key);
    const standaloneLoaded = extInstances.has(key) && !extBindgenModules.has(key);
    if (bindgenLoaded === standaloneLoaded) return false;
    extLoadLeases.delete(leaseToken);
    return true;
  }

  const operation = extLoadOperations.get(key);
  if (operation?.artifactToken !== artifactToken || operation.prepared === null) return false;
  assertExtensionLoadActive(
    key,
    operation.expectedResetGeneration,
    operation.expectedOwnerGeneration,
  );
  if (extBindgenModules.has(key) || extInstances.has(key)) return false;
  const prepared = operation.prepared;
  try {
    operation.prepared = null;
    extLoadOperations.delete(key);
    if (prepared.mode === 'bindgen') {
      extBindgenModules.set(key, prepared.bindgenModule);
    } else {
      extInstances.set(key, prepared.instance);
    }
    extArtifacts.set(key, prepared.artifact);
    extLoadLeases.delete(leaseToken);
  } catch (error) {
    operation.prepared = null;
    extLoadOperations.delete(key);
    extBindgenModules.delete(key);
    extInstances.delete(key);
    extArtifacts.delete(key);
    extLoadLeases.delete(leaseToken);
    disposePreparedExtensionArtifact(prepared);
    throw error;
  }
  return true;
}

function wasmU32(value: unknown, label: string): number {
  if (
    typeof value !== 'number'
    || !Number.isInteger(value)
    || value < -0x8000_0000
    || value > 0xffff_ffff
  ) {
    throw new Error(`${label} is not a WebAssembly i32/u32 value: ${String(value)}`);
  }
  return value >>> 0;
}

function validateWasmRange(
  ptr: number,
  len: number,
  memoryBytes: number,
  label: string,
  allowNullEmpty = false,
): void {
  if (!Number.isInteger(len) || len < 0 || len > 0xffff_ffff) {
    throw new Error(`${label} has invalid u32 length ${len}`);
  }
  if (ptr === 0 && !(allowNullEmpty && len === 0)) {
    throw new Error(`${label} has a null pointer`);
  }
  const end = ptr + len;
  if (end > 0x1_0000_0000 || end > memoryBytes) {
    throw new Error(`${label} range ${ptr}..${end} exceeds module memory`);
  }
}

function wasmRangesOverlap(leftPtr: number, leftLen: number, rightPtr: number, rightLen: number): boolean {
  return leftLen > 0 && rightLen > 0 && leftPtr < rightPtr + rightLen && rightPtr < leftPtr + leftLen;
}

function bestEffortDealloc(
  dealloc: (ptr: number, size: number) => void,
  ptr: number,
  size: number,
  label: string,
  errors: string[],
): void {
  try {
    dealloc(ptr, size);
  } catch (error) {
    const detail = error instanceof Error ? error.message : String(error);
    errors.push(`${label}: ${detail}`);
  }
}

/// Called from Rust via ext_bridge::load_wasm_ext_module.
/// `key` is the exact canonical module owner (e.g. "github.com/vo-lang/resvg").
/// `jsGlueUrl` is optional: if provided, loads as wasm-bindgen module with DOM access.
(window as any).voSetupExtModule = (
  key: string,
  bytes: Uint8Array,
  jsGlueUrl?: string,
): ExtensionLoadHandle => {
  validateCanonicalModuleOwner(key);
  if (extExhaustedOwnerLoads.has(key)) {
    throw new Error(
      `WASM extension '${key}' lifecycle generation is exhausted; reset the extension runtime`,
    );
  }
  if (!(bytes instanceof Uint8Array)) {
    throw new Error(`WASM extension '${key}' bytes must be a Uint8Array`);
  }
  if (jsGlueUrl !== undefined && typeof jsGlueUrl !== 'string') {
    throw new Error(`WASM extension '${key}' JS glue URL must be a string`);
  }
  const moduleBytes = bytes.slice();
  const expectedResetGeneration = extResetGeneration;
  const expectedOwnerGeneration = extOwnerLoadGenerations.get(key) ?? 0;
  const generationToken = extArtifacts.get(key)?.artifactToken
    ?? extLoadOperations.get(key)?.artifactToken
    ?? extensionLoadGenerationToken(expectedResetGeneration, expectedOwnerGeneration);
  const leaseToken = allocateExtensionLoadLease(key, generationToken);
  const hasJsGlue = Boolean(jsGlueUrl);
  const activeArtifact = extArtifacts.get(key);
  if (activeArtifact) {
    const ready = (async (): Promise<void> => {
      const jsGlueSource = jsGlueUrl ? await readJsGlueSource(jsGlueUrl) : null;
      assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
      const currentArtifact = extArtifacts.get(key);
      const expectedLoaded = jsGlueSource !== null
        ? extBindgenModules.has(key) && !extInstances.has(key)
        : extInstances.has(key) && !extBindgenModules.has(key);
      if (
        currentArtifact?.artifactToken !== generationToken
        || !expectedLoaded
        || currentArtifact.jsGlueSource !== jsGlueSource
        || !bytesEqual(currentArtifact.bytes, moduleBytes)
      ) {
        throw new Error(`WASM extension owner '${key}' is already loaded with a different artifact`);
      }
    })();
    return extensionLoadHandle(key, generationToken, leaseToken, ready);
  }
  const ready = (async (): Promise<void> => {
    const pendingLoad = extLoadOperations.get(key);
    if (pendingLoad) {
      if (
        pendingLoad.artifactToken !== generationToken
        || pendingLoad.hasJsGlue !== hasJsGlue
        || !bytesEqual(pendingLoad.bytes, moduleBytes)
      ) {
        throw new Error(`WASM extension owner '${key}' is already loading a different artifact`);
      }
      const candidateSourcePromise = jsGlueUrl ? readJsGlueSource(jsGlueUrl) : Promise.resolve(null);
      const [pendingSource, candidateSource] = await Promise.all([
        pendingLoad.jsGlueSourcePromise,
        candidateSourcePromise,
      ]);
      assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
      if (pendingSource !== candidateSource) {
        throw new Error(`WASM extension owner '${key}' is already loading a different artifact`);
      }
      await pendingLoad.promise;
      return;
    }
    const jsGlueSourcePromise = jsGlueUrl ? readJsGlueSource(jsGlueUrl) : Promise.resolve(null);
    let operation: PendingExtensionLoad | undefined;
    const loadPromise = (async (): Promise<void> => {
      const jsGlueSource = await jsGlueSourcePromise;
      assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
      const existingArtifact = extArtifacts.get(key);
      if (existingArtifact) {
        const expectedLoaded = jsGlueSource !== null
          ? extBindgenModules.has(key) && !extInstances.has(key)
          : extInstances.has(key) && !extBindgenModules.has(key);
        if (!expectedLoaded) {
          throw new Error(`WASM extension '${key}' has inconsistent loaded state`);
        }
        if (
          existingArtifact.artifactToken === generationToken
          && existingArtifact.jsGlueSource === jsGlueSource
          && bytesEqual(existingArtifact.bytes, moduleBytes)
        ) {
          return;
        }
        throw new Error(
          `WASM extension owner '${key}' is already loaded with a different artifact; dispose it before loading a replacement`,
        );
      }
      if (extBindgenModules.has(key) || extInstances.has(key)) {
        throw new Error(`WASM extension '${key}' has untracked loaded state`);
      }
      const artifact = { bytes: moduleBytes, jsGlueSource, artifactToken: generationToken };
      if (jsGlueSource !== null) {
        // wasm-bindgen module: full DOM access (canvas, WebGL, WebGPU, etc.)
        // Always import a fresh Blob URL so explicit disposal cannot revive a
        // cached glue module that still closes over the old WASM instance.
        // Create a Blob URL with the correct MIME type for import().
        const blob = new Blob([jsGlueSource], { type: 'application/javascript' });
        const blobUrl = URL.createObjectURL(blob);
        let glue: any = null;
        try {
          glue = await import(/* @vite-ignore */ blobUrl);
          assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
          // Instantiate the WASM module (synchronous internals only — no async start).
          if (typeof glue.default !== 'function') {
            throw new Error(`WASM extension '${key}' bindgen glue is missing its default initializer`);
          }
          const initialized: unknown = await glue.default({ module_or_path: moduleBytes });
          assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
          requireExtensionProtocolV3(bindgenProtocolExports(initialized, key), key, 'bindgen');
          // Generic async init: any wasm-bindgen module that needs async setup
          // (GPU adapter, audio context, etc.) exports __voInit().
          // Await it so the hardware is ready before Vo code runs.
          if (typeof glue.__voInit === 'function') {
            await glue.__voInit();
            assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
          }
          console.log('[voSetupExtModule] bindgen module prepared:', key);
          const currentOperation = operation;
          if (!currentOperation || extLoadOperations.get(key) !== currentOperation) {
            throw new Error(`WASM extension '${key}' load transaction was not published`);
          }
          assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
          currentOperation.prepared = {
            mode: 'bindgen',
            artifact,
            bindgenModule: glue,
          };
          glue = null;
        } catch (error) {
          if (glue && typeof glue.__voDispose === 'function') {
            try {
              glue.__voDispose();
            } catch (disposeError) {
              console.error('[voSetupExtModule] failed bindgen cleanup:', disposeError);
            }
          }
          throw error;
        } finally {
          URL.revokeObjectURL(blobUrl);
        }
      } else {
        // slice() copies the bytes out of Rust WASM memory before the async boundary
        const { instance } = await WebAssembly.instantiate(moduleBytes);
        assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
        requireExtensionProtocolV3(instance.exports as Record<string, unknown>, key, 'standalone');
        const currentOperation = operation;
        if (!currentOperation || extLoadOperations.get(key) !== currentOperation) {
          throw new Error(`WASM extension '${key}' load transaction was not published`);
        }
        assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
        currentOperation.prepared = {
          mode: 'standalone',
          artifact,
          instance,
        };
      }
    })();
    operation = {
      bytes: moduleBytes,
      hasJsGlue,
      jsGlueSourcePromise,
      artifactToken: generationToken,
      expectedResetGeneration,
      expectedOwnerGeneration,
      promise: loadPromise,
      prepared: null,
    };
    extLoadOperations.set(key, operation);
    void loadPromise.catch(() => {});
    await loadPromise;
  })();
  return extensionLoadHandle(key, generationToken, leaseToken, ready);
};

(window as any).voIsExtModuleLoadCurrent = (key: string, generationToken: string): boolean => {
  validateCanonicalModuleOwner(key);
  if (typeof generationToken !== 'string') return false;
  if (generationToken !== extensionLoadGenerationToken(
    extResetGeneration,
    extOwnerLoadGenerations.get(key) ?? 0,
  )) {
    return false;
  }
  const activeArtifact = extArtifacts.get(key);
  if (activeArtifact?.artifactToken === generationToken) {
    const bindgenLoaded = extBindgenModules.has(key) && !extInstances.has(key);
    const standaloneLoaded = extInstances.has(key) && !extBindgenModules.has(key);
    return bindgenLoaded !== standaloneLoaded;
  }
  const pendingLoad = extLoadOperations.get(key);
  return pendingLoad?.artifactToken === generationToken && pendingLoad.prepared !== null;
};

(window as any).voCommitExtModule = commitExtModule;

(window as any).voAbortExtModuleLoad = (
  key: string,
  artifactToken: string,
  leaseToken: string,
): void => {
  abortExtensionLoadLease(key, artifactToken, leaseToken);
};

(window as any).voAbortExtModuleLoadHandle = (handle: unknown): void => {
  if ((typeof handle !== 'object' && typeof handle !== 'function') || handle === null) return;
  const lease = extLoadHandleLeases.get(handle as ExtensionLoadHandle);
  if (!lease) return;
  abortExtensionLoadLease(lease.owner, lease.artifactToken, lease.leaseToken);
};

(window as any).voDisposeExtModule = (owner: string): void => {
  unloadExtModule(owner);
};

(window as any).voDisposeAllExtModules = (): void => {
  unloadAllExtModules();
};

/// Called from Rust's wasm_ext_bridge for any ext module function.
///
/// Dispatches to the most-specific canonical module owner.
///
/// Standard Vo ext ABI (standalone):
///   vo_alloc(size) → ptr
///   vo_dealloc(ptr, size)
///   <exact_export_key>(input_ptr, input_len, out_len_ptr) → output_ptr
///
/// wasm-bindgen ABI:
///   Module exports the exact export key directly, called with Uint8Array input.
///
/// `externName` uses `vo1:<pkg_utf8_len>:<pkg>:<func_utf8_len>:<func>`.
/// `input` is the protocol-v3 positional binary parameter stream.
/// Returns a tagged protocol-v3 Uint8Array; malformed names, modules, and outputs throw.
(window as any).voCallExt = (externName: string, input: Uint8Array): Uint8Array => {
  if (!(input instanceof Uint8Array)) {
    throw new Error('[voCallExt] Input must be a Uint8Array');
  }
  const decoded = decodeVoExternName(externName);
  const exportKey = wasmExtensionExportKeyFromCanonical(externName);
  const owners = new Set<string>([
    ...extBindgenModules.keys(),
    ...extInstances.keys(),
  ]);
  const matchedOwner = selectVoExternModuleOwner(decoded.packageName, owners);
  if (matchedOwner === null) {
    throw new Error(`[voCallExt] No loaded module owns package '${decoded.packageName}'`);
  }

  const bindgenModule = extBindgenModules.get(matchedOwner);
  const standaloneInstance = extInstances.get(matchedOwner);
  if (bindgenModule && standaloneInstance) {
    throw new Error(`[voCallExt] Module owner '${matchedOwner}' is loaded in two modes`);
  }
  if (bindgenModule) {
    const exported = bindgenModule[exportKey];
    if (typeof exported !== 'function') {
      throw new Error(
        `[voCallExt] Bindgen export '${exportKey}' for '${externName}' is missing from module '${matchedOwner}'`,
      );
    }
    const result: unknown = exported(input);
    if (result instanceof Promise) {
      throw new Error(`[voCallExt] Async bindgen export is unsupported for '${externName}'`);
    }
    if (result instanceof Uint8Array) return result;
    throw new Error(`[voCallExt] Unsupported bindgen return for '${externName}': ${typeof result}`);
  }

  if (!standaloneInstance) {
    throw new Error(`[voCallExt] Module owner '${matchedOwner}' has no callable instance`);
  }
  const exp = standaloneInstance.exports as Record<string, unknown>;
  const extFunc = exp[exportKey];
  if (typeof extFunc !== 'function') {
    throw new Error(
      `[voCallExt] Standalone export '${exportKey}' for '${externName}' is missing from module '${matchedOwner}'`,
    );
  }
  const allocFn = exp.vo_alloc;
  const deallocFn = exp.vo_dealloc;
  const memory = exp.memory;
  if (typeof allocFn !== 'function' || typeof deallocFn !== 'function') {
    throw new Error(`[voCallExt] Alloc/dealloc exports are missing from module '${matchedOwner}'`);
  }
  if (!(memory instanceof WebAssembly.Memory)) {
    throw new Error(`[voCallExt] Memory export is missing from module '${matchedOwner}'`);
  }

  const alloc = allocFn as (size: number) => unknown;
  const dealloc = deallocFn as (ptr: number, size: number) => void;
  let inputPtr = 0;
  let outLenPtr = 0;
  let outPtr = 0;
  let outLen = 0;
  let inputAllocated = false;
  let outLenAllocated = false;
  let outputAllocated = false;
  let result: Uint8Array | undefined;
  let callFailed = false;
  let callError: unknown;
  const cleanupErrors: string[] = [];
  try {
    if (input.length > 0xffff_ffff) throw new Error(`input exceeds the v3 u32 length field`);
    inputPtr = wasmU32(alloc(input.length), 'input allocation');
    validateWasmRange(inputPtr, input.length, memory.buffer.byteLength, 'input allocation', true);
    inputAllocated = inputPtr !== 0;
    outLenPtr = wasmU32(alloc(4), 'output-length allocation');
    validateWasmRange(outLenPtr, 4, memory.buffer.byteLength, 'output-length allocation');
    if (
      outLenPtr === inputPtr
      || wasmRangesOverlap(inputPtr, input.length, outLenPtr, 4)
    ) {
      throw new Error('input and output-length allocations overlap');
    }
    outLenAllocated = true;
    new Uint8Array(memory.buffer, inputPtr, input.length).set(input);
    outPtr = wasmU32(
      (extFunc as (ptr: number, len: number, outLen: number) => unknown)(inputPtr, input.length, outLenPtr),
      'output pointer',
    );
    outLen = new DataView(memory.buffer, outLenPtr, 4).getUint32(0, true);
    if (outPtr === 0) {
      if (outLen !== 0) throw new Error(`null output pointer has non-zero length ${outLen}`);
      result = new Uint8Array(0);
    } else {
      validateWasmRange(outPtr, outLen, memory.buffer.byteLength, 'output');
      if (
        outPtr === inputPtr
        || outPtr === outLenPtr
        || wasmRangesOverlap(outPtr, outLen, inputPtr, input.length)
        || wasmRangesOverlap(outPtr, outLen, outLenPtr, 4)
      ) {
        throw new Error('output allocation overlaps bridge-owned input metadata');
      }
      outputAllocated = true;
      result = new Uint8Array(memory.buffer, outPtr, outLen).slice();
    }
  } catch (error) {
    callFailed = true;
    callError = error;
  } finally {
    if (outputAllocated) {
      bestEffortDealloc(dealloc, outPtr, outLen, 'output deallocation', cleanupErrors);
    }
    if (outLenAllocated) {
      bestEffortDealloc(dealloc, outLenPtr, 4, 'output-length deallocation', cleanupErrors);
    }
    if (inputAllocated) {
      bestEffortDealloc(dealloc, inputPtr, input.length, 'input deallocation', cleanupErrors);
    }
  }
  if (callFailed) {
    const detail = callError instanceof Error ? callError.message : String(callError);
    const cleanup = cleanupErrors.length > 0 ? `; cleanup failures: ${cleanupErrors.join('; ')}` : '';
    throw new Error(`[voCallExt] Standalone call ${externName} failed: ${detail}${cleanup}`);
  }
  if (cleanupErrors.length > 0) {
    throw new Error(`[voCallExt] Standalone call ${externName} cleanup failed: ${cleanupErrors.join('; ')}`);
  }
  if (result === undefined) {
    throw new Error(`[voCallExt] Standalone call ${externName} produced no result`);
  }
  return result;
};
