// Web-backend GUI post-compile pipeline.
// The web backend (wasm.compileGui) produces a GuiCompileOutput and then
// funnels through executeGuiFromCompileOutput, which:
//   1. Preloads WASM extension modules (wasmExtensions list).
//   2. Loads the host bridge module before runGuiFromBytecode so that
//      host_measure_text returns real values during the first render.
//   3. Calls wasm.runGuiFromBytecode and returns GuiRunOutput.
// The native backend uses cmd_run_gui (native VM) and does NOT use this pipeline.

import type { Backend } from '../backend/backend';
import type { FrameworkContract, GuiRunOutput } from '../types';
import { setActiveHostBridge, type StudioWasm } from '../studio_wasm';
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
      const preloadKey = ext.moduleKey || ext.name;
      console.info(
        `[studio-gui] preload wasm extension name=${ext.name} moduleKey=${preloadKey} wasmBytes=${ext.wasmBytes.length} jsGlueBytes=${ext.jsGlueBytes?.length ?? 0}`,
      );
      await wasm.preloadExtModule(preloadKey, ext.wasmBytes, jsGlueUrl);
      console.info(`[studio-gui] preload wasm extension ready name=${ext.name} moduleKey=${preloadKey}`);
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
    if (!framework.hostBridgePath) {
      continue;
    }
    hostBridgeModules.push(await loadHostBridgeModule(
      framework.hostBridgePath,
      backend,
      compiled.entryPath,
    ));
    assertSessionCurrent(sessionId);
  }
  if (hostBridgeModules.length > 0) {
    setActiveHostBridge(combineHostBridgeModules(hostBridgeModules));
  }

  const renderBytes = wasm.startGuiFromBytecode(compiled.bytecode);
  return {
    renderBytes,
    moduleBytes: compiled.bytecode,
    entryPath: compiled.entryPath,
    framework: compiled.framework,
    providerFrameworks: compiled.providerFrameworks,
    externalWidgetHandlerId: null,
  };
}
