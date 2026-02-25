/**
 * Vibe Studio WebView bootstrap.
 *
 * Wires the vogui renderer to the Tauri IPC backend (native) or the
 * WASM playground engine (web mode). Registers ExternalWidget plugins
 * for CodeMirror and the nested guest vogui renderer.
 */

import { render, injectStyles } from '@vogui/index';
import type { RenderMessage } from '@vogui/types';
import { registerCodeMirrorWidget } from './widgets/codemirror';
import { registerVoguiGuestWidget } from './widgets/vogui-guest';
import './widgets/file-explorer';

// =============================================================================
// Platform abstraction
// =============================================================================

export interface StudioBridge {
  /** Compile + launch the IDE Vo source, return initial render JSON. */
  init(): Promise<string>;
  /** Forward an IDE event to the host VM, return new render JSON. */
  sendEvent(handlerId: number, payload: string): Promise<string>;
}

// =============================================================================
// Tauri bridge (native Tauri app)
// =============================================================================

async function buildTauriBridge(): Promise<StudioBridge> {
  const { invoke } = await import('@tauri-apps/api/core');

  return {
    async init() {
      return invoke('init_ide') as Promise<string>;
    },
    async sendEvent(handlerId: number, payload: string) {
      return invoke('handle_ide_event', { handlerId, payload }) as Promise<string>;
    },
  };
}

// =============================================================================
// WASM bridge (web playground mode)
// =============================================================================

async function buildWasmBridge(): Promise<StudioBridge> {
  const { initStudioWasm } = await import('./wasm-bridge');
  return initStudioWasm();
}

// =============================================================================
// Main
// =============================================================================

let bridge: StudioBridge;

function isTauriRuntime(): boolean {
  const w = window as any;
  return Boolean(w.__TAURI__ || w.__TAURI_INTERNALS__);
}

async function main() {
  injectStyles();
  registerCodeMirrorWidget();
  registerVoguiGuestWidget();

  const container = document.getElementById('app')!;

  try {
    if (isTauriRuntime()) {
      bridge = await buildTauriBridge();

      // Forward native timer ticks from TauriPlatform to the host VM.
      // Rust emits "vo-timer" with payload id (number) on each tick.
      const { listen } = await import('@tauri-apps/api/event');
      await listen<number>('vo-timer', async (event) => {
        try {
          const newJson = await bridge.sendEvent(-1, JSON.stringify({ id: event.payload }));
          applyRender(container, newJson);
        } catch (e) {
          console.error('[Studio] timer event error:', e);
        }
      });
    } else {
      bridge = await buildWasmBridge();
    }

    const initialJson = await bridge.init();
    applyRender(container, initialJson);
  } catch (e) {
    container.innerHTML = `<pre style="color:red;padding:20px">Studio init failed:\n${e}</pre>`;
  }
}

function applyRender(container: HTMLElement, json: string) {
  if (!json) return;
  let msg: RenderMessage;
  try {
    msg = JSON.parse(json) as RenderMessage;
  } catch (e) {
    console.error('[Studio] failed to parse render JSON:', e);
    return;
  }
  if (msg.type !== 'render') return;

  render(container, msg, {
    onEvent: async (handlerId: number, payload: string) => {
      try {
        const newJson = await bridge.sendEvent(handlerId, payload);
        applyRender(container, newJson);
      } catch (e) {
        console.error('[Studio] event error:', e);
      }
    },
  });
}

main();
