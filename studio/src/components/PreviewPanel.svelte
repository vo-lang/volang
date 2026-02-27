<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { render, injectStyles, decodeBinaryRender } from '@vogui/index';
  import type { RendererConfig } from '@vogui/types';
  import { bridge } from '../lib/bridge';
  import { ide } from '../stores/ide';

  export let guestRender: Uint8Array | null = null;

  let root: HTMLDivElement;
  let stylesInjected = false;
  let cleanupKeyHandler: (() => void) | null = null;

  async function onEvent(handlerId: number, payload: string): Promise<void> {
    try {
      const newBytes = await bridge().sendGuiEvent(handlerId, payload);
      if (newBytes.length > 0) ide.update(s => ({ ...s, guestRender: newBytes }));
    } catch (e: any) {
      ide.update(s => ({
        ...s,
        isGuiApp: false,
        guestRender: null,
        isRunning: false,
        output: String(e),
      }));
    }
  }

  const config: RendererConfig = { onEvent };

  onMount(() => {
    bridge().setGuiRenderCallback((bytes) => {
      if (bytes.length > 0) ide.update(s => ({ ...s, guestRender: bytes }));
    });

    const gameKeys = new Set(['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight', ' ', 'PageUp', 'PageDown']);
    const keyHandler = (event: KeyboardEvent) => {
      const target = event.target as HTMLElement;
      const tag = target.tagName;
      if (tag === 'INPUT' || tag === 'TEXTAREA' || tag === 'SELECT') return;
      // Don't intercept events originating inside the code editor
      if (target.closest('.cm-editor, .cm-content')) return;
      if (!config.onEvent) return;
      if (gameKeys.has(event.key)) event.preventDefault();
      config.onEvent(-2, JSON.stringify({ key: event.key }));
    };
    document.addEventListener('keydown', keyHandler, { capture: true });
    cleanupKeyHandler = () => document.removeEventListener('keydown', keyHandler, { capture: true });
  });

  onDestroy(() => {
    bridge().clearGuiRenderCallback();
    cleanupKeyHandler?.();
    cleanupKeyHandler = null;
    if (root) root.innerHTML = '';
  });

  $: if (root && guestRender && guestRender.length > 0) {
    applyRender(guestRender);
  }

  function applyRender(bytes: Uint8Array) {
    if (!bytes || bytes.length === 0) return;
    if (!stylesInjected) {
      injectStyles();
      stylesInjected = true;
    }
    const msg = decodeBinaryRender(bytes);
    if (msg.type !== 'render') return;
    render(root, msg, config);
  }
</script>

<div class="preview-panel">
  <div class="panel-header">
    <span class="label">Preview</span>
    <span class="badge gui-badge">GUI</span>
  </div>
  <div bind:this={root} class="preview-root"></div>
</div>

<style>
  .preview-panel {
    flex: 0 0 400px;
    background: #ffffff;
    border-left: 1px solid #313244;
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }

  .panel-header {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 8px 12px;
    background: #181825;
    border-bottom: 1px solid #313244;
    flex-shrink: 0;
  }

  .label {
    color: #6c7086;
    font-size: 10px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.08em;
  }

  .badge {
    font-size: 10px;
    font-weight: 700;
    padding: 1px 6px;
    border-radius: 3px;
    text-transform: uppercase;
    letter-spacing: 0.05em;
  }

  .gui-badge { background: #1e3a5f80; color: #89b4fa; }

  .preview-root {
    flex: 1;
    overflow: auto;
    position: relative;
    color: #0f172a;
    font-family: system-ui, -apple-system, sans-serif;
    font-size: 14px;
  }
</style>
