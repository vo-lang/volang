// Web-backend GUI post-compile pipeline.
// The web backend (wasm.compileGui) produces a GuiCompileOutput and then
// funnels through executeGuiFromCompileOutput, which:
//   1. Prepares the resolved App Session and its token-bound extensions.
//   2. Loads host bridge providers.
//   3. Starts the prepared guest and returns GuiRunOutput.
// The native backend uses cmd_run_gui (native VM) and does NOT use this pipeline.

import { frameworkJsModulePath, type FrameworkContract, type GuiRunOutput, type RendererBridgeVfsSnapshot } from '../types';
import {
  resetLoadedWasmExtensions,
  clearHostBridgeForSession,
  setHostBridgeForSession,
  type StudioPreviewHandle,
  type StudioWasm,
} from '../studio_wasm';
import {
  clearPreparedFrameworkProviders,
  combineHostBridgeModules,
  installPreparedFrameworkProviders,
  loadHostBridgeModule,
  unloadHostBridgeModule,
  type HostBridgeModule,
} from './renderer_bridge';

export interface GuiCompileOutput {
  bytecode: Uint8Array;
  entryPath: string;
  launchToken: string;
  framework: FrameworkContract | null;
  providerFrameworks: FrameworkContract[];
  vfsSnapshot: RendererBridgeVfsSnapshot;
}

const MAX_GUI_BYTECODE_BYTES = 128 * 1024 * 1024;
const MAX_U64_DECIMAL = '18446744073709551615';

function validateGuiCompileOutput(compiled: GuiCompileOutput): void {
  if (!(compiled.bytecode instanceof Uint8Array) || compiled.bytecode.byteLength > MAX_GUI_BYTECODE_BYTES) {
    throw new Error('GUI bytecode exceeds the 128 MiB limit');
  }
  if (
    typeof compiled.launchToken !== 'string'
    || !/^[1-9][0-9]*$/.test(compiled.launchToken)
    || compiled.launchToken.length > MAX_U64_DECIMAL.length
    || (
      compiled.launchToken.length === MAX_U64_DECIMAL.length
      && compiled.launchToken > MAX_U64_DECIMAL
    )
  ) {
    throw new Error('GUI compile output contains an invalid prepared launch token');
  }
}

export function resetGuiHostBridge(sessionId: number): void {
  clearPreparedFrameworkProviders(sessionId);
  clearHostBridgeForSession(sessionId);
  unloadHostBridgeModule(sessionId);
}

export async function executeGuiFromCompileOutput(
  compiled: GuiCompileOutput,
  wasm: StudioWasm,
  sessionId: number,
  assertSessionCurrent: (id: number) => void,
  startupSignal: AbortSignal,
): Promise<GuiRunOutput & { previewHandle: StudioPreviewHandle }> {
  // Create the planned App Session before loading host modules, then keep
  // renderer-capable providers pending until the renderer module initializes.
  // This preserves first-render host imports while keeping every evaluated
  // framework module inside the resolved provider factory lifecycle.
  let previewHandle: StudioPreviewHandle | null = null;
  const loadedProviderModuleKeys = new Set<string>();
  const pendingProviderModuleKeys = new Set<string>();
  const readyProviderModuleKeys = new Set<string>();
  try {
    assertSessionCurrent(sessionId);
    validateGuiCompileOutput(compiled);
    // A new compile is a session boundary. Drop the previous bridge and its
    // module graph before any preload can fail and leave stale host imports live.
    resetGuiHostBridge(sessionId);
    // Extension routing is keyed by the exact canonical module owner embedded in
    // bytecode. The one-shot launch token owns the authenticated payload; clear
    // this session before Rust loads that payload and creates the VM.
    resetLoadedWasmExtensions();
    previewHandle = await wasm.prepareGuiFromBytecode(
      compiled.bytecode,
      compiled.entryPath,
      compiled.launchToken,
      `gui:${sessionId}`,
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
    for (const hostBridgePath of hostBridgePaths) {
      hostBridgeModules.push(await loadHostBridgeModule(
        sessionId,
        hostBridgePath,
        compiled.entryPath,
        compiled.vfsSnapshot.files,
        startupSignal,
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
      wasm.readyFrameworkProvider(previewHandle.index, previewHandle.generation, moduleKey);
      pendingProviderModuleKeys.delete(moduleKey);
      readyProviderModuleKeys.add(moduleKey);
    }
    installPreparedFrameworkProviders(sessionId, readyProviderModuleKeys);
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
      vfsSnapshot: compiled.vfsSnapshot,
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
      for (const moduleKey of [...readyProviderModuleKeys].reverse()) {
        try {
          wasm.closeFrameworkProvider(previewHandle.index, previewHandle.generation, moduleKey);
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
      // Preserve the original startup failure even if the token was malformed.
    }
    resetGuiHostBridge(sessionId);
    resetLoadedWasmExtensions();
    throw error;
  }
}
