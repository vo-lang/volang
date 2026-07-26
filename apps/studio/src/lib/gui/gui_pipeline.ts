// Web-backend GUI post-compile pipeline.
// The web backend (wasm.compileGui) produces a GuiCompileOutput and then
// funnels through executeGuiFromCompileOutput, which:
//   1. Preloads WASM extension modules (wasmExtensions list).
//   2. Prepares the resolved App Session, then loads host bridge providers.
//   3. Starts the prepared guest and returns GuiRunOutput.
// The native backend uses cmd_run_gui (native VM) and does NOT use this pipeline.

import type { Backend } from '../backend/backend';
import { frameworkJsModulePath, type FrameworkContract, type GuiRunOutput } from '../types';
import {
  resetLoadedWasmExtensions,
  clearHostBridgeForSession,
  setHostBridgeForSession,
  type StudioPreviewHandle,
  type StudioWasm,
} from '../studio_wasm';
import { shouldEmitVoplayPerfConsoleDiagnostics } from '../perf_report_bridge';
import {
  fetchVfsSnapshot,
  clearPreparedFrameworkProviders,
  installPreparedFrameworkProviders,
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
  launchToken: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  wasmExtensions: WasmExtCompileSpec[];
}

const MAX_GUI_BYTECODE_BYTES = 128 * 1024 * 1024;
const MAX_GUI_EXTENSION_COUNT = 10_000;
const MAX_GUI_EXTENSION_FILE_BYTES = 256 * 1024 * 1024;
const MAX_GUI_EXTENSION_TOTAL_BYTES = 512 * 1024 * 1024;

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
  if (typeof compiled.launchToken !== 'string' || !/^[1-9][0-9]*$/.test(compiled.launchToken)) {
    throw new Error('GUI compile output contains an invalid prepared launch token');
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
    const owner = ext.moduleKey;
    if (!owner || owners.has(owner)) {
      throw new Error(`GUI compile output contains a duplicate extension owner: ${owner || '<empty>'}`);
    }
    owners.add(owner);
  }
}

export function resetGuiHostBridge(sessionId: number): void {
  clearPreparedFrameworkProviders(sessionId);
  clearHostBridgeForSession(sessionId);
  unloadHostBridgeModule(sessionId);
}

export async function executeGuiFromCompileOutput(
  compiled: GuiCompileOutput,
  backend: Backend,
  wasm: StudioWasm,
  sessionId: number,
  assertSessionCurrent: (id: number) => void,
): Promise<GuiRunOutput & { previewHandle: StudioPreviewHandle }> {
  validateGuiCompileOutput(compiled);
  // A new compile is a session boundary. Drop the previous bridge and its
  // module graph before any preload can fail and leave stale host imports live.
  resetGuiHostBridge(sessionId);
  // Extension routing is keyed by the exact canonical module owner embedded in
  // bytecode. Clear the previous session before publishing the new artifact
  // set, then preserve every compiler-provided owner byte-for-byte.
  resetLoadedWasmExtensions();
  try {
    for (const ext of compiled.wasmExtensions) {
      let jsGlueUrl: string | undefined;
      if (ext.jsGlueBytes && ext.jsGlueBytes.length > 0) {
        const blob = new Blob([new Uint8Array(ext.jsGlueBytes)], { type: 'application/javascript' });
        jsGlueUrl = URL.createObjectURL(blob);
      }
      try {
        if (shouldEmitVoplayPerfConsoleDiagnostics()) {
          console.info(
            `[studio-gui] preload wasm extension name=${ext.name} moduleKey=${ext.moduleKey} wasmBytes=${ext.wasmBytes.length} jsGlueBytes=${ext.jsGlueBytes?.length ?? 0}`,
          );
        }
        await wasm.preloadExtModule(ext.moduleKey, ext.wasmBytes, jsGlueUrl);
        if (shouldEmitVoplayPerfConsoleDiagnostics()) {
          console.info(`[studio-gui] preload wasm extension ready name=${ext.name} moduleKey=${ext.moduleKey}`);
        }
      } finally {
        if (jsGlueUrl) URL.revokeObjectURL(jsGlueUrl);
      }
      assertSessionCurrent(sessionId);
    }
  } catch (error) {
    resetLoadedWasmExtensions();
    throw error;
  }

  // Create the planned App Session before loading host modules, then keep
  // renderer-capable providers pending until the renderer module initializes.
  // This preserves first-render host imports while keeping every evaluated
  // framework module inside the resolved provider factory lifecycle.
  let previewHandle: StudioPreviewHandle | null = null;
  const loadedProviderModuleKeys = new Set<string>();
  const pendingProviderModuleKeys = new Set<string>();
  try {
    previewHandle = wasm.prepareGuiFromBytecode(
      compiled.bytecode,
      compiled.entryPath,
      compiled.launchToken,
    );
    assertSessionCurrent(sessionId);
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
      ? await fetchVfsSnapshot(backend, compiled.entryPath, sessionId)
      : null;
    for (const hostBridgePath of hostBridgePaths) {
      hostBridgeModules.push(await loadHostBridgeModule(
        sessionId,
        hostBridgePath,
        backend,
        compiled.entryPath,
        snapshot!.files,
      ));
      assertSessionCurrent(sessionId);
    }
    if (hostBridgeModules.length > 0) {
      setHostBridgeForSession(sessionId, combineHostBridgeModules(hostBridgeModules));
    }

    const sessionProviders = new Map<string, FrameworkContract>();
    for (const framework of hostBridgeFrameworks) {
      sessionProviders.set(framework.moduleKey, framework);
    }
    for (const [moduleKey] of sessionProviders) {
      wasm.loadFrameworkProvider(previewHandle.index, previewHandle.generation, moduleKey);
      loadedProviderModuleKeys.add(moduleKey);
      wasm.beginFrameworkProvider(previewHandle.index, previewHandle.generation, moduleKey);
      pendingProviderModuleKeys.add(moduleKey);
    }
    installPreparedFrameworkProviders(sessionId, pendingProviderModuleKeys);
    // WebBackend owns the serialized host-bridge session around this entire
    // pipeline. Re-entering that queue here waits behind the current pipeline
    // and deadlocks startup after the VM has been prepared.
    const started = wasm.startPreparedGui(
      previewHandle.index,
      previewHandle.generation,
      compiled.entryPath,
    );
    return {
      previewHandle,
      renderBytes: started,
      moduleBytes: compiled.bytecode,
      entryPath: compiled.entryPath,
      framework: compiled.framework,
      providerFrameworks: compiled.providerFrameworks,
    };
  } catch (error) {
    clearPreparedFrameworkProviders(sessionId);
    if (previewHandle) {
      for (const moduleKey of [...pendingProviderModuleKeys].reverse()) {
        try {
          wasm.abortFrameworkProvider(previewHandle.index, previewHandle.generation, moduleKey);
        } catch {
          // Session shutdown below remains the final rollback authority.
        }
      }
      for (const moduleKey of [...loadedProviderModuleKeys].reverse()) {
        try {
          wasm.unloadFrameworkProvider(previewHandle.index, previewHandle.generation, moduleKey);
        } catch {
          // Session shutdown below remains the final rollback authority.
        }
      }
      try {
        wasm.stopGui(previewHandle.index, previewHandle.generation);
      } catch {
        // Preparation may already have rolled the Session back.
      }
    }
    try {
      wasm.discardPreparedGuiLaunch(compiled.launchToken);
    } catch {
      // Rust may already have consumed the token while the frontend session
      // was being superseded.
    }
    resetGuiHostBridge(sessionId);
    resetLoadedWasmExtensions();
    throw error;
  }
}
