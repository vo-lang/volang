/**
 * WASM bridge for web (playground) mode.
 *
 * Loads the studio WASM module, compiles the IDE Vo source inside it,
 * and exposes a StudioBridge that communicates via WASM function calls
 * instead of Tauri IPC.
 */

import type { StudioBridge } from './main';

export async function initStudioWasm(): Promise<StudioBridge> {
  // Dynamically load the studio WASM module.
  // Use runtime URL + @vite-ignore so Vite doesn't require this file in Tauri mode.
  const v = Date.now();
  const wasmEntryUrl = new URL(`../wasm/pkg/vo_studio_wasm.js?v=${v}`, import.meta.url).href;
  const wasmBinaryUrl = new URL(`../wasm/pkg/vo_studio_wasm_bg.wasm?v=${v}`, import.meta.url).href;
  let wasmMod: any;
  try {
    wasmMod = await import(/* @vite-ignore */ wasmEntryUrl);
  } catch (e) {
    throw new Error(
      `Failed to load Studio WASM bundle (${wasmEntryUrl}). ` +
      `Build it first under studio/wasm/pkg. Original error: ${e}`,
    );
  }
  await wasmMod.default(wasmBinaryUrl);

  const result = wasmMod.studioInit();
  if (result.status !== 'ok') {
    throw new Error(result.error || 'WASM studio init failed');
  }

  return {
    async init() {
      return result.renderJson;
    },
    async sendEvent(handlerId: number, payload: string) {
      const r = wasmMod.studioSendEvent(handlerId, payload);
      if (r.status !== 'ok') {
        throw new Error(r.error);
      }
      return r.renderJson;
    },
  };
}
