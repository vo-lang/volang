// Web-backend GUI post-compile pipeline.
// The web backend (wasm.compileGui) produces a GuiCompileOutput and then
// funnels through executeGuiFromCompileOutput, which:
//   1. Preloads WASM extension modules (wasmExtensions list).
//   2. Loads the host bridge module before runGuiFromBytecode so that
//      host_measure_text returns real values during the first render.
//   3. Calls wasm.runGuiFromBytecode and returns GuiRunOutput.
// The native backend uses cmd_run_gui (native VM) and does NOT use this pipeline.

import type { Backend } from '../backend/backend';
import { frameworkJsModulePath, type FrameworkContract, type GuiRunOutput } from '../types';
import { setActiveHostBridge, type StudioWasm } from '../studio_wasm';
import { shouldEmitVoplayPerfConsoleDiagnostics } from '../perf_report_bridge';
import {
  fetchVfsSnapshot,
  loadHostBridgeModule,
  unloadHostBridgeModule,
  type HostBridgeModule,
} from './renderer_bridge';

export interface WasmExtCompileSpec {
  name: string;
  moduleKey: string;
  wasmBytes: Uint8Array;
  jsGlueBytes: Uint8Array | null;
}

export interface GuiCompileOutput {
  bytecode: Uint8Array;
  entryPath: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  wasmExtensions: WasmExtCompileSpec[];
}

const fingerprintEncoder = new TextEncoder();
const MAX_GUI_BYTECODE_BYTES = 128 * 1024 * 1024;
const MAX_GUI_EXTENSION_COUNT = 10_000;
const MAX_GUI_EXTENSION_FILE_BYTES = 256 * 1024 * 1024;
const MAX_GUI_EXTENSION_TOTAL_BYTES = 512 * 1024 * 1024;

function extensionAliasSegment(ext: WasmExtCompileSpec): string {
  if (ext.name.trim().length > 0) {
    return ext.name;
  }
  const moduleKey = ext.moduleKey || 'module';
  return moduleKey.split('/').filter(Boolean).pop() || 'module';
}

function packFingerprintBytes(parts: Uint8Array[]): Uint8Array {
  let total = 0;
  for (const part of parts) {
    if (part.byteLength > 0xffff_ffff) {
      throw new Error('Extension fingerprint component exceeds the u32 length limit');
    }
    total += 4 + part.byteLength;
    if (!Number.isSafeInteger(total)) {
      throw new Error('Extension fingerprint size overflow');
    }
  }
  const packed = new Uint8Array(total);
  const view = new DataView(packed.buffer);
  let offset = 0;
  for (const part of parts) {
    view.setUint32(offset, part.byteLength, true);
    offset += 4;
    packed.set(part, offset);
    offset += part.byteLength;
  }
  return packed;
}

function hexDigest(bytes: ArrayBuffer): string {
  return Array.from(new Uint8Array(bytes), (byte) => byte.toString(16).padStart(2, '0')).join('');
}

async function extensionArtifactFingerprint(ext: WasmExtCompileSpec): Promise<string> {
  const identityBytes = fingerprintEncoder.encode(`${ext.moduleKey || ''}\0${ext.name || ''}`);
  const subtle = globalThis.crypto?.subtle;
  if (!subtle) {
    throw new Error('Web Crypto SHA-256 is required to fingerprint WASM extensions');
  }
  const wasmDigest = new Uint8Array(await subtle.digest(
    'SHA-256',
    ext.wasmBytes.slice().buffer as ArrayBuffer,
  ));
  const glueDigest = new Uint8Array(await subtle.digest(
    'SHA-256',
    (ext.jsGlueBytes ?? new Uint8Array(0)).slice().buffer as ArrayBuffer,
  ));
  const packed = packFingerprintBytes([identityBytes, wasmDigest, glueDigest]);
  return `sha256_${hexDigest(await subtle.digest('SHA-256', packed.buffer as ArrayBuffer))}`;
}

async function extensionPreloadKey(ext: WasmExtCompileSpec): Promise<string> {
  const moduleKey = ext.moduleKey || ext.name || 'module';
  const alias = extensionAliasSegment(ext);
  const fingerprint = await extensionArtifactFingerprint(ext);
  return `${moduleKey}/artifact_${fingerprint}/${alias}`;
}

function combineHostBridgeModules(modules: HostBridgeModule[]): HostBridgeModule {
  return {
    buildImports(ctx) {
      const imports: Record<string, (...args: number[]) => number | void> = {};
      for (const module of modules) {
        const next = module.buildImports(ctx);
        for (const [name, handler] of Object.entries(next)) {
          if (name in imports) {
            throw new Error(`Multiple host bridge modules define import ${name}`);
          }
          imports[name] = handler;
        }
      }
      return imports;
    },
  };
}

function validateGuiCompileOutput(compiled: GuiCompileOutput): void {
  if (!(compiled.bytecode instanceof Uint8Array) || compiled.bytecode.byteLength > MAX_GUI_BYTECODE_BYTES) {
    throw new Error('GUI bytecode exceeds the 128 MiB limit');
  }
  if (!Array.isArray(compiled.wasmExtensions) || compiled.wasmExtensions.length > MAX_GUI_EXTENSION_COUNT) {
    throw new Error(`GUI compile output exceeds the ${MAX_GUI_EXTENSION_COUNT}-extension limit`);
  }
  const owners = new Set<string>();
  let totalBytes = 0;
  for (const [index, ext] of compiled.wasmExtensions.entries()) {
    if (
      !ext
      || typeof ext.name !== 'string'
      || typeof ext.moduleKey !== 'string'
      || !(ext.wasmBytes instanceof Uint8Array)
      || (ext.jsGlueBytes !== null && !(ext.jsGlueBytes instanceof Uint8Array))
    ) {
      throw new Error(`GUI extension ${index} has an invalid compile contract`);
    }
    if (ext.wasmBytes.byteLength > MAX_GUI_EXTENSION_FILE_BYTES) {
      throw new Error(`GUI extension ${ext.name || index} exceeds the 256 MiB WASM limit`);
    }
    if ((ext.jsGlueBytes?.byteLength ?? 0) > MAX_GUI_EXTENSION_FILE_BYTES) {
      throw new Error(`GUI extension ${ext.name || index} exceeds the 256 MiB JavaScript limit`);
    }
    totalBytes += ext.wasmBytes.byteLength + (ext.jsGlueBytes?.byteLength ?? 0);
    if (!Number.isSafeInteger(totalBytes) || totalBytes > MAX_GUI_EXTENSION_TOTAL_BYTES) {
      throw new Error('GUI extensions exceed the 512 MiB aggregate limit');
    }
    const owner = ext.moduleKey || ext.name;
    if (!owner || owners.has(owner)) {
      throw new Error(`GUI compile output contains a duplicate extension owner: ${owner || '<empty>'}`);
    }
    owners.add(owner);
  }
}

export function resetGuiHostBridge(): void {
  setActiveHostBridge(null);
  unloadHostBridgeModule();
}

export async function executeGuiFromCompileOutput(
  compiled: GuiCompileOutput,
  backend: Backend,
  wasm: StudioWasm,
  sessionId: number,
  assertSessionCurrent: (id: number) => void,
): Promise<GuiRunOutput> {
  validateGuiCompileOutput(compiled);
  // A new compile is a session boundary. Drop the previous bridge and its
  // module graph before any preload can fail and leave stale host imports live.
  resetGuiHostBridge();
  for (const ext of compiled.wasmExtensions) {
    let jsGlueUrl: string | undefined;
    if (ext.jsGlueBytes && ext.jsGlueBytes.length > 0) {
      const blob = new Blob([new Uint8Array(ext.jsGlueBytes)], { type: 'application/javascript' });
      jsGlueUrl = URL.createObjectURL(blob);
    }
    try {
      const preloadKey = await extensionPreloadKey(ext);
      if (shouldEmitVoplayPerfConsoleDiagnostics()) {
        console.info(
          `[studio-gui] preload wasm extension name=${ext.name} moduleKey=${preloadKey} wasmBytes=${ext.wasmBytes.length} jsGlueBytes=${ext.jsGlueBytes?.length ?? 0}`,
        );
      }
      await wasm.preloadExtModule(preloadKey, ext.wasmBytes, jsGlueUrl);
      if (shouldEmitVoplayPerfConsoleDiagnostics()) {
        console.info(`[studio-gui] preload wasm extension ready name=${ext.name} moduleKey=${preloadKey}`);
      }
    } finally {
      if (jsGlueUrl) URL.revokeObjectURL(jsGlueUrl);
    }
    assertSessionCurrent(sessionId);
  }

  // Load the host bridge BEFORE runGuiFromBytecode so that host_measure_text
  // returns real values during the initial render. Without this, VirtualScroll
  // computes zero item heights (maxScrollTop=0) and calls ScrollTo(ref,0) on
  // every Recompute, causing the chat-style scroll regression.
  try {
    const hostBridgeFrameworks = compiled.framework
      ? [compiled.framework, ...compiled.providerFrameworks]
      : [...compiled.providerFrameworks];
    const hostBridgeModules: HostBridgeModule[] = [];
    const hostBridgePaths = new Set<string>();
    for (const framework of hostBridgeFrameworks) {
      const hostBridgePath = frameworkJsModulePath(framework, 'host_bridge');
      if (hostBridgePath) hostBridgePaths.add(hostBridgePath);
    }
    const snapshot = hostBridgePaths.size > 0
      ? await fetchVfsSnapshot(backend, compiled.entryPath)
      : null;
    for (const hostBridgePath of hostBridgePaths) {
      hostBridgeModules.push(await loadHostBridgeModule(
        hostBridgePath,
        backend,
        compiled.entryPath,
        snapshot!.files,
      ));
      assertSessionCurrent(sessionId);
    }
    if (hostBridgeModules.length > 0) {
      setActiveHostBridge(combineHostBridgeModules(hostBridgeModules));
    }

    const renderBytes = wasm.startGuiFromBytecode(compiled.bytecode, compiled.entryPath);
    return {
      renderBytes,
      moduleBytes: compiled.bytecode,
      entryPath: compiled.entryPath,
      framework: compiled.framework,
      providerFrameworks: compiled.providerFrameworks,
      hostWidgetHandlerId: null,
    };
  } catch (error) {
    resetGuiHostBridge();
    throw error;
  }
}
