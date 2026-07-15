// Loader for the studio WASM module (vo-studio-wasm wasm-pack output).
// The wasm-pack output is expected at /wasm/ (served from apps/studio/public/wasm/).
// Build: wasm-pack build apps/studio/wasm --target web --out-dir ../public/wasm

// Framework-specific host bridge imports (focus, blur, scrollTo, measureText, etc.)
// are injected dynamically via setActiveHostBridge() before WASM instantiation.
// No static import of any framework package.
import type { HostBridgeModule } from './gui/renderer_bridge';
import type { DiagnosticError, FrameworkContract, WorkspaceDiscoveryMode } from './types';
import { hasWindowVfsBindings, installWindowVfsBackend, type WindowVfsBackend } from './window_vfs_bindings';
import { observeGuestExitVm } from './guest_exit';
import { portableCaseKey } from './portable_path_key';

export { guestExitCode } from './guest_exit';

// ── VoVm instance interface (matches VoVm wasm-bindgen class exports) ────────

export interface VoVmInstance {
  readonly exitCode: number | undefined;
  dumpBytecode(): string;
  run(): string;
  runInit(): string;
  runScheduled(): string;
  setGcStressEveryStep(enabled: boolean): void;
  pushIslandCommand(frame: Uint8Array): void;
  takeOutboundCommands(): Uint8Array[];
  takePendingHostEvents(): Array<{ key: string; source: string; token: string; delayMs: number; replay: boolean }>;
  wakeHostEvent(key: string): void;
  takeOutput(): string;
}

export interface WasmCompileResult {
  ok: boolean;
  errors: DiagnosticError[];
  bytecode: Uint8Array | null;
}

export interface WasmRunResult {
  readonly output: string;
  readonly exitCode: number;
}

// ── StudioWasm — full set of wasm-bindgen exports from vo-studio-wasm ────────

export interface StudioWasm {
  // Direct GUI VM API.
  runGuiFromBytecode(bytecode: Uint8Array): Uint8Array;
  startGuiFromBytecode(bytecode: Uint8Array, entryPath?: string): Uint8Array;
  runGui(entryPath: string): {
    renderBytes: Uint8Array;
    moduleBytes: Uint8Array;
    entryPath: string;
    framework: FrameworkContract | null;
    providerFrameworks: FrameworkContract[];
    hostWidgetHandlerId: number | null;
  };
  runGuiEntry(entryPath: string): Uint8Array;
  sendGuiEvent(handlerId: number, payload: string): Uint8Array;
  sendGuiEventAsync(handlerId: number, payload: string): void;
  setGcStressEveryStep(enabled: boolean): void;
  setGcStressHostStep(enabled: boolean): void;
  startRenderIsland(bytecode: Uint8Array): void;
  pushIslandData(data: Uint8Array): void;
  pollGuiRender(): Uint8Array;
  getRenderIslandVfsSnapshot(entryPath: string, workspaceDiscovery: WorkspaceDiscoveryMode): {
    rootPath: string;
    files: Array<{ path: string; bytes: Uint8Array }>;
  };
  pollIslandData(): Uint8Array;
  pollPendingHostEvent(): { key: string; source: string; token: string; delayMs: number; replay: boolean } | null;
  wakeHostEvent(key: string): void;
  stopGui(): void;
  checkEntry(entryPath: string, workspaceDiscovery: WorkspaceDiscoveryMode): WasmCompileResult;
  compileEntry(entryPath: string, workspaceDiscovery: WorkspaceDiscoveryMode): WasmCompileResult;
  dumpEntry(entryPath: string, workspaceDiscovery: WorkspaceDiscoveryMode): string;
  dumpGuiEntry(entryPath: string, workspaceDiscovery: WorkspaceDiscoveryMode): string;
  dumpBytecode(bytecode: Uint8Array): string;
  // Console run (compile + execute, returns stdout and process status)
  compileRunEntry(entryPath: string, workspaceDiscovery: WorkspaceDiscoveryMode): WasmRunResult;
  // Instance-based VM (VoWebModule interface)
  VoVm: { withExterns(bytecode: Uint8Array): VoVmInstance };
  preloadExtModule(path: string, bytes: Uint8Array, jsGlueUrl?: string): Promise<void>;
  forgetWasmExtModuleOwner(owner: string): void;
  clearWasmExtModuleOwners(): void;
  prepareEntry(entryPath: string, workspaceDiscovery: WorkspaceDiscoveryMode): Promise<void>;
  compileGui(entryPath: string, workspaceDiscovery: WorkspaceDiscoveryMode): {
    bytecode: Uint8Array;
    entryPath: string;
    framework: FrameworkContract | null;
    providerFrameworks: FrameworkContract[];
    wasmExtensions: Array<{ name: string; moduleKey: string; wasmBytes: Uint8Array; jsGlueBytes: Uint8Array | null }>;
  };
  getBuildId(): string;
  initVFS(): Promise<void>;
}

type RawStudioWasmModule = Partial<StudioWasm> & {
  default: (opts?: { module_or_path?: string }) => Promise<void>;
  StudioVoVm?: { withExterns(bytecode: Uint8Array): VoVmInstance };
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

export const WASM_EXTENSION_PROTOCOL_VERSION = 3;
export const WASM_EXTENSION_EXPORT_PREFIX = '__vo_ext_';
const MAX_EXTERN_NAME_BYTES = 4 * 1024;
const MAX_CANONICAL_MODULE_OWNER_BYTES = 255;
const MAX_PORTABLE_PACKAGE_COMPONENT_BYTES = 255;
const EXTERN_NAME_PREFIX_BYTES = new Uint8Array([0x76, 0x6f, 0x31, 0x3a]); // "vo1:"
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

/** Strictly decode `vo1:<pkg_utf8_len>:<pkg>:<func_utf8_len>:<func>`. */
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
  const stem = portableCaseKey(segment.split('.')[0]);
  if (['con', 'prn', 'aux', 'nul', 'conin$', 'conout$'].includes(stem)) {
    return false;
  }
  return !/^(?:com|lpt)(?:[1-9]|[¹²³])$/.test(stem);
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

/** Select the most-specific canonical loaded owner for a package. */
export function selectVoExternModuleOwner(packageName: string, owners: Iterable<string>): string | null {
  let selected: string | null = null;
  for (const owner of owners) {
    if (isVoPackageOwnedByModule(packageName, owner) && (selected === null || owner.length > selected.length)) {
      selected = owner;
    }
  }
  return selected;
}

function shouldTraceStandaloneExtern(externName: DecodedVoExternName): boolean {
  return externName.functionName === 'HasHostCapability' || externName.functionName === 'waitForEvent';
}

async function readJsGlueSource(jsGlueUrl: string): Promise<string> {
  const response = await fetch(jsGlueUrl, { cache: 'no-store' });
  if (!response.ok) {
    throw new Error(`Failed to fetch JS glue: HTTP ${response.status}`);
  }
  return response.text();
}

function createJsGlueImportUrl(jsGlueSource: string): { importUrl: string; revoke(): void } {
  // Always import a fresh Blob URL. ES module imports are cached by URL, so
  // reusing a caller-owned blob/data URL after explicit disposal would revive
  // the old wasm-bindgen module and its closed-over WebAssembly instance.
  const blobUrl = URL.createObjectURL(new Blob([jsGlueSource], { type: 'application/javascript' }));
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
      `Studio web asset mismatch: ${expectedSource} expects ${expectedBuildId}, wasm provides ${wasmBuildId}. Reload after the latest deploy. In local dev, rebuild apps/studio/wasm and retry; a dev server restart should only be needed if the mismatch persists.`,
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
  throw new Error('Studio Window VFS backend is not installed');
}

export function setStudioWindowVfsBackendFactory(factory: StudioWindowVfsFactory | null): void {
  studioWindowVfsFactory = factory;
  studioWindowVfsRevision += 1;
}

// ── Ext-bridge JS globals (mirrors apps/playground-legacy/src/wasm/vo.ts) ─────────────────

type BindgenModule = Record<string, unknown> & {
  default?: (opts: { module_or_path: Uint8Array }) => Promise<unknown>;
  __voInit?: () => Promise<void>;
  __voDispose?: () => void;
  vo_ext_protocol_version?: () => number;
};

// Maps exact canonical module owner → WebAssembly.Instance (standalone C-ABI modules).
const extInstances = new Map<string, WebAssembly.Instance>();
// Maps exact canonical module owner → wasm-bindgen module (DOM/WebGPU access).
const extBindgenModules = new Map<string, BindgenModule>();
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
      bindgenModule: BindgenModule;
    }>
  | Readonly<{
      mode: 'standalone';
      artifact: LoadedExtensionArtifact;
      instance: WebAssembly.Instance;
      standaloneRef: StandaloneRef;
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
let extOwnerStateBridge: Pick<StudioWasm, 'forgetWasmExtModuleOwner' | 'clearWasmExtModuleOwners'> | null = null;

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

let extBridgeInstalled = false;

type StandaloneGuiEventDispatcher = (handlerId: number, payload: string) => Promise<void>;
type StandaloneGameLoopState = { rafId: number; lastTs: number };
type StandaloneHostState = Readonly<{ clear(): void }>;
let standaloneGuiEventDispatcher: StandaloneGuiEventDispatcher | null = null;
let standalonePopstateHandler: (() => void) | null = null;
const standaloneHostStates = new Set<StandaloneHostState>();

function clearStandaloneHostState(): void {
  for (const state of standaloneHostStates) {
    state.clear();
  }
}

function requireStandaloneGuiEventDispatcher(label: string): StandaloneGuiEventDispatcher {
  const dispatcher = standaloneGuiEventDispatcher;
  if (!dispatcher) {
    throw new Error(`render island host event dispatcher is not installed for ${label}`);
  }
  return dispatcher;
}

function dispatchStandaloneGuiEventAsync(handlerId: number, payload: string): Promise<void> {
  return requireStandaloneGuiEventDispatcher(`handler:${handlerId}`)(handlerId, payload);
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

function disposeBindgenModule(module: BindgenModule): void {
  try {
    module.__voDispose?.();
  } catch (error) {
    console.error('[voDisposeExtModule] bindgen dispose failed:', error);
  }
}

function disposeStandaloneRef(ref: StandaloneRef, operation: string): void {
  try {
    ref.dispose();
  } catch (error) {
    console.error(`[${operation}] standalone cleanup failed:`, error);
  }
}

function disposePreparedExtensionArtifact(prepared: PreparedExtensionArtifact): void {
  if (prepared.mode === 'bindgen') {
    disposeBindgenModule(prepared.bindgenModule);
  } else {
    disposeStandaloneRef(prepared.standaloneRef, 'voAbortExtModuleLoad');
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

function unloadExtModule(key: string): void {
  validateCanonicalModuleOwner(key);
  const hasTrackedArtifact = extArtifacts.has(key)
    || extBindgenModules.has(key)
    || extInstances.has(key)
    || extStandaloneRefs.has(key);
  const ownerStateBridge = extOwnerStateBridge;
  if (hasTrackedArtifact && !ownerStateBridge) {
    throw new Error(`WASM extension '${key}' cannot be disposed without its Rust owner-state bridge`);
  }
  // Preflight every fallible lifecycle step before changing JavaScript state.
  // If the Rust epoch is exhausted, the active artifact and its generation
  // remain intact and both routing layers continue to agree.
  const nextOwnerGeneration = nextExtensionGeneration(
    extOwnerLoadGenerations.get(key) ?? 0,
    `WASM extension '${key}'`,
  );
  const bindgenModule = extBindgenModules.get(key);
  const standaloneRef = extStandaloneRefs.get(key);
  ownerStateBridge?.forgetWasmExtModuleOwner(key);
  extOwnerLoadGenerations.set(key, nextOwnerGeneration);
  const pendingOperation = extLoadOperations.get(key);
  const prepared = pendingOperation?.prepared ?? null;
  if (pendingOperation) {
    pendingOperation.prepared = null;
    extLoadOperations.delete(key);
  }
  removeExtensionLoadLeases(key);
  extBindgenModules.delete(key);
  extInstances.delete(key);
  extStandaloneRefs.delete(key);
  extArtifacts.delete(key);
  if (prepared) disposePreparedExtensionArtifact(prepared);
  if (standaloneRef) disposeStandaloneRef(standaloneRef, 'voDisposeExtModule');
  if (bindgenModule) disposeBindgenModule(bindgenModule);
}

function bytesEqual(left: Uint8Array, right: Uint8Array): boolean {
  if (left.length !== right.length) return false;
  for (let index = 0; index < left.length; index += 1) {
    if (left[index] !== right[index]) return false;
  }
  return true;
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

// Late-binding reference for standalone WASM host imports.
// The imports capture this ref; after instantiation the caller sets ref.instance
// so that host functions can access the WASM linear memory.
interface StandaloneRef {
  instance: WebAssembly.Instance | null;
  dispose(): void;
}

const extStandaloneRefs = new Map<string, StandaloneRef>();

function readWasmString(ref: StandaloneRef, ptr: number, len: number): string {
  const mem = (ref.instance!.exports.memory as WebAssembly.Memory).buffer;
  return new TextDecoder().decode(new Uint8Array(mem, ptr, len));
}

function wasmAlloc(ref: StandaloneRef, size: number): number {
  return (ref.instance!.exports.vo_alloc as (size: number) => number)(size);
}

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

function buildStandaloneImports(): { imports: WebAssembly.Imports; ref: StandaloneRef } {
  const standaloneTimers = new Map<number, ReturnType<typeof setTimeout>>();
  const standaloneIntervals = new Map<number, ReturnType<typeof setInterval>>();
  const standaloneRunningIntervalHandlers = new Set<number>();
  const standaloneAnimFrames = new Map<number, number>();
  const standaloneGameLoops = new Map<number, StandaloneGameLoopState>();
  let disposed = false;
  const hostState: StandaloneHostState = {
    clear(): void {
      for (const handle of standaloneTimers.values()) clearTimeout(handle);
      standaloneTimers.clear();
      for (const handle of standaloneIntervals.values()) clearInterval(handle);
      standaloneIntervals.clear();
      standaloneRunningIntervalHandlers.clear();
      for (const handle of standaloneAnimFrames.values()) cancelAnimationFrame(handle);
      standaloneAnimFrames.clear();
      for (const loop of standaloneGameLoops.values()) cancelAnimationFrame(loop.rafId);
      standaloneGameLoops.clear();
    },
  };
  standaloneHostStates.add(hostState);
  const ref: StandaloneRef = {
    instance: null,
    dispose(): void {
      if (disposed) return;
      disposed = true;
      hostState.clear();
      standaloneHostStates.delete(hostState);
      ref.instance = null;
    },
  };

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
  function getBridgeImports(): Record<string, (...args: number[]) => number | void> | null {
    if (activeHostBridgeModule !== cachedBridgeMod) {
      cachedBridgeMod = activeHostBridgeModule;
      cachedBridgeImports = activeHostBridgeModule ? activeHostBridgeModule.buildImports(bridgeCtx) : null;
    }
    return cachedBridgeImports;
  }

  function requireBridgeImport(name: string): (...args: number[]) => number | void {
    const handler = getBridgeImports()?.[name];
    if (typeof handler !== 'function') {
      throw new Error(`host bridge import is not installed: ${name}`);
    }
    return handler;
  }

  function audioBridgeFunction(name: string): ((...args: unknown[]) => unknown) | null {
    const handler = (window as unknown as Record<string, unknown>)[name];
    if (typeof handler !== 'function') {
      console.warn(`[vogui host] audio bridge function is not installed: ${name}`);
      return null;
    }
    return handler as (...args: unknown[]) => unknown;
  }

  function readWasmBytes(ptr: number, len: number): Uint8Array {
    const mem = (ref.instance!.exports.memory as WebAssembly.Memory).buffer;
    return new Uint8Array(mem, ptr, len).slice();
  }

  function callAudioVoid(name: string, ...args: unknown[]): void {
    const handler = audioBridgeFunction(name);
    if (!handler) return;
    handler(...args);
  }

  function callAudioNumber(name: string, ...args: unknown[]): number {
    const handler = audioBridgeFunction(name);
    if (!handler) return -1;
    const result = handler(...args);
    return typeof result === 'number' && Number.isFinite(result) ? result : -1;
  }

  const imports: WebAssembly.Imports = {
    env: {
      host_start_timeout(id: number, ms: number): void {
        requireStandaloneGuiEventDispatcher(`timeout:${id}`);
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
        requireStandaloneGuiEventDispatcher(`interval:${id}`);
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
        const result = name === 'render_island_host' ? 1 : 0;
        if (name === 'render_island_host') {
          requireStandaloneGuiEventDispatcher('render_island_host capability');
          emitStudioHostLog({
            source: 'studio-extbridge',
            code: 'host_capability_query',
            level: 'success',
            text: `name=${name} dispatcher=set result=${result}`,
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
        const encoded = UTF8_ENCODER.encode(path);
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
        requireStandaloneGuiEventDispatcher(`anim_frame:${id}`);
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
        requireStandaloneGuiEventDispatcher(`game_loop:${id}`);
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
      // Bridge functions are resolved through the active host bridge module.
      host_focus(ptr: number, len: number): void {
        requireBridgeImport('host_focus')(ptr, len);
      },
      host_blur(ptr: number, len: number): void {
        requireBridgeImport('host_blur')(ptr, len);
      },
      host_scroll_to(ptr: number, len: number, top: number): void {
        requireBridgeImport('host_scroll_to')(ptr, len, top);
      },
      host_scroll_into_view(ptr: number, len: number): void {
        requireBridgeImport('host_scroll_into_view')(ptr, len);
      },
      host_select_text(ptr: number, len: number): void {
        requireBridgeImport('host_select_text')(ptr, len);
      },
      host_measure_text(
        textPtr: number, textLen: number,
        fontPtr: number, fontLen: number,
        maxWidth: number, lineHeight: number,
        whiteSpace: number,
        outLenPtr: number,
      ): number {
        const bridge = requireBridgeImport('host_measure_text') as (...args: number[]) => number;
        if (!_measureTextFirstCallLogged) {
          _measureTextFirstCallLogged = true;
          console.log('[host_measure_text] first call, bridge=loaded');
        }
        return bridge(
          textPtr, textLen, fontPtr, fontLen, maxWidth, lineHeight, whiteSpace, outLenPtr,
        );
      },
      host_measure_text_lines(
        textPtr: number, textLen: number,
        fontPtr: number, fontLen: number,
        maxWidth: number, lineHeight: number,
        whiteSpace: number,
        outLenPtr: number,
      ): number {
        const bridge = requireBridgeImport('host_measure_text_lines') as (...args: number[]) => number;
        return bridge(
          textPtr, textLen, fontPtr, fontLen, maxWidth, lineHeight, whiteSpace, outLenPtr,
        );
      },
      host_audio_load_bytes(ptr: number, len: number): number {
        return callAudioNumber('voAudioLoad', readWasmBytes(ptr, len));
      },
      host_audio_free(clipId: number): void {
        callAudioVoid('voAudioFree', clipId);
      },
      host_audio_play_sound(clipId: number, volume: number, pitch: number): void {
        callAudioVoid('voAudioPlaySound', clipId, volume, pitch);
      },
      host_audio_set_listener(
        px: number, py: number, pz: number,
        fx: number, fy: number, fz: number,
        ux: number, uy: number, uz: number,
      ): void {
        callAudioVoid('voAudioSetListener', px, py, pz, fx, fy, fz, ux, uy, uz);
      },
      host_audio_play_sound_3d(
        clipId: number,
        px: number, py: number, pz: number,
        volume: number,
        refDistance: number,
        maxDistance: number,
      ): void {
        callAudioVoid('voAudioPlaySound3D', clipId, px, py, pz, volume, refDistance, maxDistance);
      },
      host_audio_create_source_3d(
        clipId: number,
        px: number, py: number, pz: number,
        volume: number,
        refDistance: number,
        maxDistance: number,
      ): number {
        return callAudioNumber('voAudioCreateSource3D', clipId, px, py, pz, volume, refDistance, maxDistance);
      },
      host_audio_update_spatial(): void {
        callAudioVoid('voAudioUpdateSpatial');
      },
      host_audio_set_source_3d_pos(sourceId: number, px: number, py: number, pz: number): void {
        callAudioVoid('voAudioSetSource3DPos', sourceId, px, py, pz);
      },
      host_audio_set_source_3d_params(sourceId: number, volume: number, pitch: number): void {
        callAudioVoid('voAudioSetSource3DParams', sourceId, volume, pitch);
      },
      host_audio_remove_source_3d(sourceId: number): void {
        callAudioVoid('voAudioRemoveSource3D', sourceId);
      },
      host_audio_play_music(clipId: number, volume: number): void {
        callAudioVoid('voAudioPlayMusic', clipId, volume);
      },
      host_audio_stop_music(): void {
        callAudioVoid('voAudioStopMusic');
      },
      host_audio_pause_music(): void {
        callAudioVoid('voAudioPauseMusic');
      },
      host_audio_resume_music(): void {
        callAudioVoid('voAudioResumeMusic');
      },
      host_audio_set_sfx_volume(volume: number): void {
        callAudioVoid('voAudioSetSFXVolume', volume);
      },
      host_audio_set_music_volume(volume: number): void {
        callAudioVoid('voAudioSetMusicVolume', volume);
      },
    },
  };
  return { imports, ref };
}

function unloadAllExtModules(): void {
  const hasTrackedArtifacts = extArtifacts.size > 0
    || extBindgenModules.size > 0
    || extInstances.size > 0
    || extStandaloneRefs.size > 0;
  const ownerStateBridge = extOwnerStateBridge;
  if (hasTrackedArtifacts && !ownerStateBridge) {
    throw new Error('WASM extensions cannot be reset without their Rust owner-state bridge');
  }
  // Keep the complete JavaScript lifecycle state untouched until Rust accepts
  // the matching catalog reset. This also makes reset-generation exhaustion
  // transactional.
  const nextResetGeneration = nextExtensionGeneration(extResetGeneration, 'WASM extension reset');
  const preparedArtifacts = Array.from(extLoadOperations.values())
    .map((operation) => operation.prepared)
    .filter((prepared): prepared is PreparedExtensionArtifact => prepared !== null);
  const bindgenModules = Array.from(new Set(extBindgenModules.values()));
  const standaloneRefs = Array.from(new Set(extStandaloneRefs.values()));
  ownerStateBridge?.clearWasmExtModuleOwners();
  extResetGeneration = nextResetGeneration;
  extOwnerLoadGenerations.clear();
  extLoadOperations.clear();
  extLoadLeases.clear();
  extExhaustedOwnerLoads.clear();
  extBindgenModules.clear();
  extInstances.clear();
  extStandaloneRefs.clear();
  extArtifacts.clear();
  for (const prepared of preparedArtifacts) disposePreparedExtensionArtifact(prepared);
  for (const ref of standaloneRefs) disposeStandaloneRef(ref, 'voDisposeAllExtModules');
  for (const module of bindgenModules) disposeBindgenModule(module);
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
    const standaloneLoaded = extInstances.has(key)
      && extStandaloneRefs.has(key)
      && !extBindgenModules.has(key);
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
  if (extBindgenModules.has(key) || extInstances.has(key) || extStandaloneRefs.has(key)) {
    return false;
  }
  const prepared = operation.prepared;
  try {
    operation.prepared = null;
    extLoadOperations.delete(key);
    if (prepared.mode === 'bindgen') {
      extBindgenModules.set(key, prepared.bindgenModule);
    } else {
      extInstances.set(key, prepared.instance);
      extStandaloneRefs.set(key, prepared.standaloneRef);
    }
    extArtifacts.set(key, prepared.artifact);
    extLoadLeases.delete(leaseToken);
  } catch (error) {
    operation.prepared = null;
    extLoadOperations.delete(key);
    extBindgenModules.delete(key);
    extInstances.delete(key);
    extStandaloneRefs.delete(key);
    extArtifacts.delete(key);
    extLoadLeases.delete(leaseToken);
    disposePreparedExtensionArtifact(prepared);
    throw error;
  }
  return true;
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

function installExtBridgeGlobals(
  ownerStateBridge: Pick<StudioWasm, 'forgetWasmExtModuleOwner' | 'clearWasmExtModuleOwners'>,
): void {
  extOwnerStateBridge = ownerStateBridge;
  if (extBridgeInstalled) return;
  extBridgeInstalled = true;

  (window as unknown as Record<string, unknown>).voSetupExtModule = (
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
          : extInstances.has(key) && extStandaloneRefs.has(key) && !extBindgenModules.has(key);
        if (
          currentArtifact?.artifactToken !== generationToken
          || !expectedLoaded
          || currentArtifact.jsGlueSource !== jsGlueSource
          || !bytesEqual(currentArtifact.bytes, moduleBytes)
        ) {
          throw new Error(
            `WASM extension owner '${key}' is already loaded with a different artifact`,
          );
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
          throw new Error(
            `WASM extension owner '${key}' is already loading a different artifact`,
          );
        }
        const candidateSourcePromise = jsGlueUrl ? readJsGlueSource(jsGlueUrl) : Promise.resolve(null);
        const [pendingSource, candidateSource] = await Promise.all([
          pendingLoad.jsGlueSourcePromise,
          candidateSourcePromise,
        ]);
        assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
        if (pendingSource !== candidateSource) {
          throw new Error(
            `WASM extension owner '${key}' is already loading a different artifact`,
          );
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
            : extInstances.has(key) && extStandaloneRefs.has(key) && !extBindgenModules.has(key);
          if (!expectedLoaded) {
            throw new Error(`WASM extension '${key}' has inconsistent loaded state`);
          }
          if (
            existingArtifact.artifactToken === generationToken
            && existingArtifact.jsGlueSource === jsGlueSource
            && bytesEqual(existingArtifact.bytes, moduleBytes)
          ) {
            emitStudioHostLog({
              source: 'studio-extbridge',
              code: 'ext_module_setup_idempotent',
              level: 'system',
              text: `key=${key}`,
            });
            return;
          }
          throw new Error(
            `WASM extension owner '${key}' is already loaded with a different artifact; dispose it before loading a replacement`,
          );
        }
        if (extBindgenModules.has(key) || extInstances.has(key) || extStandaloneRefs.has(key)) {
          throw new Error(`WASM extension '${key}' has untracked loaded state`);
        }
        const artifact = { bytes: moduleBytes, jsGlueSource, artifactToken: generationToken };
        emitStudioHostLog({
          source: 'studio-extbridge',
          code: 'ext_module_setup_begin',
          level: 'system',
          text: `key=${key} bindgen=${jsGlueSource !== null ? 'yes' : 'no'} bytes=${moduleBytes.byteLength}`,
        });
        if (jsGlueSource !== null) {
          const { importUrl, revoke } = createJsGlueImportUrl(jsGlueSource);
          let glue: BindgenModule | null = null;
          try {
            assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
            emitStudioHostLog({
              source: 'studio-extbridge',
              code: 'ext_module_glue_import_begin',
              level: 'system',
              text: `key=${key}`,
            });
            glue = await import(/* @vite-ignore */ importUrl) as BindgenModule;
            assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
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
            if (typeof glue.default !== 'function') {
              throw new Error(`WASM extension '${key}' bindgen glue is missing its default initializer`);
            }
            const initialized = await glue.default({
              module_or_path: moduleBytes,
            });
            assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
            requireExtensionProtocolV3(bindgenProtocolExports(initialized, key), key, 'bindgen');
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
              assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
              emitStudioHostLog({
                source: 'studio-extbridge',
                code: 'ext_module_async_init_ready',
                level: 'system',
                text: `key=${key}`,
              });
            }
            emitStudioHostLog({
              source: 'studio-extbridge',
              code: 'ext_module_setup_prepared',
              level: 'system',
              text: `key=${key} mode=bindgen`,
            });
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
            if (glue) {
              disposeBindgenModule(glue);
            }
            throw error;
          } finally {
            revoke();
          }
        } else {
          const { imports, ref } = buildStandaloneImports();
          try {
            const { instance } = await WebAssembly.instantiate(moduleBytes, imports);
            assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
            // Bind the late reference so host imports can access this instance's memory.
            ref.instance = instance;
            requireExtensionProtocolV3(instance.exports as Record<string, unknown>, key, 'standalone');
            emitStudioHostLog({
              source: 'studio-extbridge',
              code: 'ext_module_setup_prepared',
              level: 'system',
              text: `key=${key} mode=standalone`,
            });
            const currentOperation = operation;
            if (!currentOperation || extLoadOperations.get(key) !== currentOperation) {
              throw new Error(`WASM extension '${key}' load transaction was not published`);
            }
            assertExtensionLoadActive(key, expectedResetGeneration, expectedOwnerGeneration);
            currentOperation.prepared = {
              mode: 'standalone',
              artifact,
              instance,
              standaloneRef: ref,
            };
          } catch (error) {
            ref.dispose();
            throw error;
          }
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

  (window as unknown as Record<string, unknown>).voIsExtModuleLoadCurrent = (
    key: string,
    generationToken: string,
  ): boolean => {
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
      const standaloneLoaded = extInstances.has(key)
        && extStandaloneRefs.has(key)
        && !extBindgenModules.has(key);
      return bindgenLoaded !== standaloneLoaded;
    }
    const pendingLoad = extLoadOperations.get(key);
    return pendingLoad?.artifactToken === generationToken && pendingLoad.prepared !== null;
  };

  (window as unknown as Record<string, unknown>).voCommitExtModule = commitExtModule;

  (window as unknown as Record<string, unknown>).voAbortExtModuleLoad = (
    key: string,
    artifactToken: string,
    leaseToken: string,
  ): void => {
    abortExtensionLoadLease(key, artifactToken, leaseToken);
  };

  (window as unknown as Record<string, unknown>).voAbortExtModuleLoadHandle = (
    handle: unknown,
  ): void => {
    if ((typeof handle !== 'object' && typeof handle !== 'function') || handle === null) return;
    const lease = extLoadHandleLeases.get(handle as ExtensionLoadHandle);
    if (!lease) return;
    abortExtensionLoadLease(lease.owner, lease.artifactToken, lease.leaseToken);
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
    if (!(input instanceof Uint8Array)) {
      throwVoCallExtFailure('[voCallExt] Input must be a Uint8Array');
    }
    const decoded = decodeVoExternName(externName);
    const exportKey = wasmExtensionExportKeyFromCanonical(externName);
    const traceExtern = shouldTraceStandaloneExtern(decoded);
    const loadedOwners = new Set<string>([
      ...extBindgenModules.keys(),
      ...extInstances.keys(),
    ]);
    const matchedOwner = selectVoExternModuleOwner(decoded.packageName, loadedOwners);
    if (matchedOwner === null) {
      emitStudioHostLog({
        source: 'studio-extbridge',
        code: 'ext_call_no_module',
        level: 'error',
        text: `extern=${externName} package=${decoded.packageName} standalone=[${Array.from(extInstances.keys()).join(',')}] bindgen=[${Array.from(extBindgenModules.keys()).join(',')}]`,
      });
      throwVoCallExtFailure(`[voCallExt] No loaded module owns package '${decoded.packageName}'`);
    }

    const bindgenModule = extBindgenModules.get(matchedOwner);
    const standaloneInstance = extInstances.get(matchedOwner);
    if (bindgenModule && standaloneInstance) {
      throwVoCallExtFailure(`[voCallExt] Module owner '${matchedOwner}' is loaded in two modes`);
    }
    if (bindgenModule) {
      if (traceExtern) {
        emitStudioHostLog({
          source: 'studio-extbridge',
          code: 'ext_call_bindgen_match',
          level: 'system',
          text: `extern=${externName} owner=${matchedOwner} func=${decoded.functionName}`,
        });
      }
      if (typeof bindgenModule[exportKey] === 'function') {
        let result: unknown;
        try {
          result = (bindgenModule[exportKey] as (i: Uint8Array) => unknown)(input);
        } catch (e) {
          throwVoCallExtFailure(`[voCallExt] Exception calling ${externName}`, e);
        }
        if (result instanceof Promise) {
          throwVoCallExtFailure(`[voCallExt] Async bindgen export is not supported for ${externName}`);
        }
        if (result instanceof Uint8Array) return result;
        throwVoCallExtFailure(`[voCallExt] Unsupported bindgen return for ${externName}: ${typeof result}`);
      }
      throwVoCallExtFailure(
        `[voCallExt] Bindgen export '${exportKey}' for '${externName}' is missing from module '${matchedOwner}'`,
      );
    }

    if (!standaloneInstance) {
      throwVoCallExtFailure(`[voCallExt] Module owner '${matchedOwner}' has no callable instance`);
    }
    const exp = standaloneInstance.exports as Record<string, unknown>;
    if (traceExtern) {
      emitStudioHostLog({
        source: 'studio-extbridge',
        code: 'ext_call_standalone_match',
        level: 'system',
        text: `extern=${externName} owner=${matchedOwner} func=${decoded.functionName}`,
      });
    }
    const extFunc = exp[exportKey];
    if (typeof extFunc !== 'function') {
      throwVoCallExtFailure(
        `[voCallExt] Standalone export '${exportKey}' for '${externName}' is missing from module '${matchedOwner}'`,
      );
    }
    const allocFn = exp.vo_alloc;
    const deallocFn = exp.vo_dealloc;
    const memory = exp.memory;
    if (typeof allocFn !== 'function' || typeof deallocFn !== 'function') {
      throwVoCallExtFailure(`[voCallExt] Alloc/dealloc exports are missing from module '${matchedOwner}'`);
    }
    if (!(memory instanceof WebAssembly.Memory)) {
      throwVoCallExtFailure(`[voCallExt] Memory export is missing from module '${matchedOwner}'`);
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
      throwVoCallExtFailure(`[voCallExt] Standalone call ${externName} failed`, new Error(`${detail}${cleanup}`));
    }
    if (cleanupErrors.length > 0) {
      throwVoCallExtFailure(
        `[voCallExt] Standalone call ${externName} cleanup failed`,
        new Error(cleanupErrors.join('; ')),
      );
    }
    if (result === undefined) {
      throwVoCallExtFailure(`[voCallExt] Standalone call ${externName} produced no result`);
    }
    if (traceExtern) {
      emitStudioHostLog({
        source: 'studio-extbridge',
        code: 'ext_call_result',
        level: 'system',
        text: `extern=${externName} owner=${matchedOwner} func=${decoded.functionName} outLen=${result.length} firstTag=${result.length > 0 ? result[0] : -1}`,
      });
    }
    return result;
  };
}

// ── Loader ────────────────────────────────────────────────────────────────────

let instance: StudioWasm | null = null;
let initPromise: Promise<StudioWasm> | null = null;
let loadGeneration = 0;

function requireStudioExport<T>(value: T | undefined, name: string): T {
  if (value === undefined) {
    throw new Error(`apps/studio/wasm missing export: ${name}`);
  }
  return value;
}

function queryFlagEnabled(name: string): boolean {
  if (typeof window === 'undefined') {
    return false;
  }
  const value = new URLSearchParams(window.location.search).get(name);
  return value !== null && value !== '' && value !== '0' && value !== 'false';
}

function gcStressEveryStepEnabled(): boolean {
  return queryFlagEnabled('voGcStressEveryStep');
}

function gcStressHostStepEnabled(): boolean {
  return queryFlagEnabled('voplayGcStress') || queryFlagEnabled('voGcStressHostStep');
}

function studioBrowserSmokeDebugEnabled(): boolean {
  return queryFlagEnabled('studioBrowserSmokeDebug');
}

function normalizeStudioWasmModule(mod: RawStudioWasmModule): StudioWasm {
  const vmExport = mod.StudioVoVm;
  if (!vmExport) {
    throw new Error('apps/studio/wasm missing VM export: StudioVoVm');
  }
  const setGcStressEveryStep = requireStudioExport(
    mod.setGcStressEveryStep as StudioWasm['setGcStressEveryStep'],
    'setGcStressEveryStep',
  );
  const setGcStressHostStep = requireStudioExport(
    mod.setGcStressHostStep as StudioWasm['setGcStressHostStep'],
    'setGcStressHostStep',
  );
  const applyGcStressFlag = (): boolean => {
    const everyStepEnabled = gcStressEveryStepEnabled();
    const hostStepEnabled = gcStressHostStepEnabled();
    setGcStressEveryStep(everyStepEnabled);
    setGcStressHostStep(hostStepEnabled);
    return everyStepEnabled;
  };
  return {
    runGuiFromBytecode: (bytecode) => {
      applyGcStressFlag();
      return requireStudioExport(mod.runGuiFromBytecode as StudioWasm['runGuiFromBytecode'], 'runGuiFromBytecode')(bytecode);
    },
    startGuiFromBytecode: (bytecode, entryPath) => {
      applyGcStressFlag();
      return requireStudioExport(mod.startGuiFromBytecode as StudioWasm['startGuiFromBytecode'], 'startGuiFromBytecode')(bytecode, entryPath);
    },
    runGui: requireStudioExport(mod.runGui, 'runGui'),
    runGuiEntry: requireStudioExport(mod.runGuiEntry, 'runGuiEntry'),
    sendGuiEvent: requireStudioExport(mod.sendGuiEvent, 'sendGuiEvent'),
    sendGuiEventAsync: requireStudioExport(mod.sendGuiEventAsync, 'sendGuiEventAsync'),
    setGcStressEveryStep,
    setGcStressHostStep,
    startRenderIsland: (bytecode) => {
      applyGcStressFlag();
      return requireStudioExport(mod.startRenderIsland, 'startRenderIsland')(bytecode);
    },
    pushIslandData: requireStudioExport(mod.pushIslandData, 'pushIslandData'),
    pollGuiRender: requireStudioExport(mod.pollGuiRender, 'pollGuiRender'),
    getRenderIslandVfsSnapshot: requireStudioExport(mod.getRenderIslandVfsSnapshot, 'getRenderIslandVfsSnapshot'),
    pollIslandData: requireStudioExport(mod.pollIslandData, 'pollIslandData'),
    pollPendingHostEvent: requireStudioExport(mod.pollPendingHostEvent, 'pollPendingHostEvent'),
    wakeHostEvent: requireStudioExport(mod.wakeHostEvent, 'wakeHostEvent'),
    stopGui: requireStudioExport(mod.stopGui, 'stopGui'),
    preloadExtModule: requireStudioExport(mod.preloadExtModule, 'preloadExtModule'),
    forgetWasmExtModuleOwner: requireStudioExport(
      mod.forgetWasmExtModuleOwner,
      'forgetWasmExtModuleOwner',
    ),
    clearWasmExtModuleOwners: requireStudioExport(
      mod.clearWasmExtModuleOwners,
      'clearWasmExtModuleOwners',
    ),
    checkEntry: requireStudioExport(mod.checkEntry as StudioWasm['checkEntry'], 'checkEntry'),
    compileEntry: requireStudioExport(mod.compileEntry as StudioWasm['compileEntry'], 'compileEntry'),
    dumpEntry: requireStudioExport(mod.dumpEntry as StudioWasm['dumpEntry'], 'dumpEntry'),
    dumpGuiEntry: requireStudioExport(mod.dumpGuiEntry as StudioWasm['dumpGuiEntry'], 'dumpGuiEntry'),
    dumpBytecode: requireStudioExport(mod.dumpBytecode as StudioWasm['dumpBytecode'], 'dumpBytecode'),
    compileRunEntry: requireStudioExport(mod.compileRunEntry as StudioWasm['compileRunEntry'], 'compileRunEntry'),
    prepareEntry: requireStudioExport(mod.prepareEntry, 'prepareEntry'),
    compileGui: requireStudioExport(mod.compileGui as StudioWasm['compileGui'], 'compileGui'),
    getBuildId: requireStudioExport(mod.getBuildId as StudioWasm['getBuildId'], 'getBuildId'),
    initVFS: requireStudioExport(mod.initVFS, 'initVFS'),
    VoVm: {
      withExterns: (bytecode) => {
        const enabled = applyGcStressFlag();
        const vm = vmExport.withExterns(bytecode);
        vm.setGcStressEveryStep(enabled);
        if (studioBrowserSmokeDebugEnabled()) {
          const bytesLength = bytecode.length;
          (globalThis as typeof globalThis & {
            __voStudioBrowserSmokeRenderIsland?: {
              moduleBytesLength(): number;
              dumpModuleBytes(): Promise<string>;
            };
          }).__voStudioBrowserSmokeRenderIsland = {
            moduleBytesLength: () => bytesLength,
            dumpModuleBytes: async () => vm.dumpBytecode(),
          };
        }
        return vm;
      },
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
      await mod.default({ module_or_path: wasmPath });
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
      if (generation !== loadGeneration) {
        emitStudioHostLog({
          source: 'studio-wasm',
          code: 'load_superseded',
          level: 'system',
        });
        return initPromise ?? loadStudioWasm();
      }
      normalized.setGcStressEveryStep(gcStressEveryStepEnabled());
      normalized.setGcStressHostStep(gcStressHostStepEnabled());
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'init_vfs_ready',
        level: 'system',
      });
      installExtBridgeGlobals(normalized);
      emitStudioHostLog({
        source: 'studio-wasm',
        code: 'ext_bridge_ready',
        level: 'system',
      });
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
  loadGeneration = nextExtensionGeneration(loadGeneration, 'Studio WASM load');
  unloadAllExtModules();
  extOwnerStateBridge = null;
  studioAssetBuildIdPromise = null;
  instance = null;
  initPromise = null;
}

// ── VoWebModule factory ────────────────────────────────────────────────────────

export function makeVoWebModule(
  wasm: StudioWasm,
  onGuestExit?: (exitCode: number) => void,
): VoWebModule {
  return {
    initVFS: () => wasm.initVFS(),
    preloadExtModule: (path, bytes, jsGlueUrl = '') =>
      wasm.preloadExtModule(path, bytes, jsGlueUrl),
    VoVm: {
      withExterns: (bytecode) => observeGuestExitVm(
        wasm.VoVm.withExterns(bytecode),
        onGuestExit,
      ),
    },
  };
}
