<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { render, injectStyles, decodeBinaryRender } from '@vogui/index';
  import type { RendererConfig } from '@vogui/types';
  import { bridge } from '../lib/bridge';
  import { ide } from '../stores/ide';

  export let guestRender: Uint8Array | null = null;
  export let chromeless: boolean = false;

  let root: HTMLDivElement;
  let stylesInjected = false;
  let cleanupKeyHandler: (() => void) | null = null;

  // Resizable width (panel mode only)
  let panelWidth = 400;
  let isResizing = false;
  let resizeStartX = 0;
  let resizeStartWidth = 0;

  function onResizeStart(e: MouseEvent) {
    isResizing = true;
    resizeStartX = e.clientX;
    resizeStartWidth = panelWidth;
    e.preventDefault();
    window.addEventListener('mousemove', onResizeMove);
    window.addEventListener('mouseup', onResizeEnd);
  }

  function onResizeMove(e: MouseEvent) {
    if (!isResizing) return;
    const delta = resizeStartX - e.clientX;
    panelWidth = Math.max(220, Math.min(800, resizeStartWidth + delta));
  }

  function onResizeEnd() {
    isResizing = false;
    window.removeEventListener('mousemove', onResizeMove);
    window.removeEventListener('mouseup', onResizeEnd);
  }

  async function onEvent(handlerId: number, payload: string): Promise<void> {
    try {
      const result = await bridge().shell.exec({ kind: 'gui.event', handlerId, payload });
      const bytes = (result as { renderBytes: Uint8Array }).renderBytes;
      if (bytes && bytes.length > 0) ide.update(s => ({ ...s, guestRender: bytes }));
    } catch (e: any) {
      const { consolePushLines } = await import('../stores/ide');
      consolePushLines('stderr', String(e));
      ide.update(s => ({
        ...s,
        isGuiApp: false,
        guestRender: null,
        isRunning: false,
        runStatus: 'error',
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

<div
  class="preview-panel"
  class:chromeless
  class:resizing={isResizing}
  style={chromeless ? '' : `width: ${panelWidth}px; flex: 0 0 ${panelWidth}px;`}
>
  {#if !chromeless}
    <!-- svelte-ignore a11y-no-static-element-interactions -->
    <div class="resize-handle" on:mousedown={onResizeStart}>
      <div class="resize-bar"></div>
    </div>
    <div class="panel-header">
      <span class="label">Preview</span>
      <span class="badge gui-badge">GUI</span>
    </div>
  {/if}
  <div bind:this={root} class="preview-root"></div>
</div>

<style>
  .preview-panel {
    /* width / flex-basis set via inline style when not chromeless */
    flex-shrink: 0;
    background: #ffffff;
    border-left: 1px solid #1e1e2e;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    position: relative;
  }

  .preview-panel.chromeless {
    flex: 1;
    border-left: none;
  }

  .preview-panel.resizing {
    user-select: none;
  }

  /* Left-edge resize handle */
  .resize-handle {
    position: absolute;
    left: 0;
    top: 0;
    bottom: 0;
    width: 5px;
    cursor: ew-resize;
    z-index: 10;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .resize-handle:hover .resize-bar,
  .preview-panel.resizing .resize-bar {
    background: #3b3f5c;
  }

  .resize-bar {
    width: 2px;
    height: 40px;
    border-radius: 1px;
    background: #252535;
    transition: background 0.15s;
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
