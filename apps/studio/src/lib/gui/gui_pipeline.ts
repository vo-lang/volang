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
import { loadHostBridgeModule, type HostBridgeModule } from './renderer_bridge';

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
    total += 4 + part.byteLength;
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

function fallbackFingerprint(bytes: Uint8Array): string {
  let hash = 2166136261;
  for (const byte of bytes) {
    hash ^= byte;
    hash = Math.imul(hash, 16777619) >>> 0;
  }
  return `fnv1a32_${hash.toString(16).padStart(8, '0')}`;
}

async function extensionArtifactFingerprint(ext: WasmExtCompileSpec): Promise<string> {
  const identityBytes = fingerprintEncoder.encode(`${ext.moduleKey || ''}\0${ext.name || ''}`);
  const packed = packFingerprintBytes([
    identityBytes,
    ext.wasmBytes,
    ext.jsGlueBytes ?? new Uint8Array(0),
  ]);
  const subtle = globalThis.crypto?.subtle;
  if (subtle) {
    return `sha256_${hexDigest(await subtle.digest('SHA-256', packed))}`;
  }
  return fallbackFingerprint(packed);
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
          if (!(name in imports)) {
            imports[name] = handler;
          }
        }
      }
      return imports;
    },
  };
}

export async function executeGuiFromCompileOutput(
  compiled: GuiCompileOutput,
  backend: Backend,
  wasm: StudioWasm,
  sessionId: number,
  assertSessionCurrent: (id: number) => void,
): Promise<GuiRunOutput> {
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

  setActiveHostBridge(null);

  // Load the host bridge BEFORE runGuiFromBytecode so that host_measure_text
  // returns real values during the initial render. Without this, VirtualScroll
  // computes zero item heights (maxScrollTop=0) and calls ScrollTo(ref,0) on
  // every Recompute, causing the chat-style scroll regression.
  const hostBridgeFrameworks = compiled.framework
    ? [compiled.framework, ...compiled.providerFrameworks]
    : [...compiled.providerFrameworks];
  const hostBridgeModules: HostBridgeModule[] = [];
  for (const framework of hostBridgeFrameworks) {
    const hostBridgePath = frameworkJsModulePath(framework, 'host_bridge');
    if (!hostBridgePath) {
      continue;
    }
    hostBridgeModules.push(await loadHostBridgeModule(
      hostBridgePath,
      backend,
      compiled.entryPath,
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
}
