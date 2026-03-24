<script lang="ts">
  import { onDestroy, onMount } from 'svelte';
  import { get } from 'svelte/store';
  import {
    decodeBinaryRender,
    injectStyles,
    render as renderGui,
    setupKeyHandler,
  } from '../../../../vogui/js/dist/index.js';
  import { isGuiSessionSupersededError, type RuntimeService } from '../lib/services/runtime_service';
  import { runtime } from '../stores/runtime';

  export let runtimeService: RuntimeService;

  let host: HTMLDivElement;
  let renderRevision = 0;
  let unsubscribe: (() => void) | null = null;
  let cleanupKeyHandler: (() => void) | null = null;

  onMount(() => {
    injectStyles();
    cleanupKeyHandler = setupKeyHandler({
      onEvent: (handlerId, payload) => {
        void dispatchGuiEvent(renderRevision, handlerId, payload);
      },
    });
    unsubscribe = runtime.subscribe((state) => {
      if (!host) return;
      const isGuiApp = state.kind === 'gui' && state.isRunning;
      if (!isGuiApp || !state.guiEntryPath || !state.guiModuleBytes || !state.guiRenderBytes || state.guiRenderBytes.length === 0) {
        renderRevision += 1;
        host.replaceChildren();
        return;
      }
      renderFrame(state.guiRenderBytes, ++renderRevision);
    });
  });

  onDestroy(() => {
    cleanupKeyHandler?.();
    cleanupKeyHandler = null;
    unsubscribe?.();
    unsubscribe = null;
    host?.replaceChildren();
  });

  function renderFrame(renderBytes: Uint8Array, revision: number): void {
    if (!host || revision !== renderRevision) return;
    const message = decodeBinaryRender(renderBytes);
    renderGui(host, message, {
      onEvent: (handlerId, payload) => {
        void dispatchGuiEvent(revision, handlerId, payload);
      },
    });
  }

  async function dispatchGuiEvent(revision: number, handlerId: number, payload: string): Promise<void> {
    const state = get(runtime);
    if (state.kind !== 'gui' || !state.isRunning) return;
    try {
      if (revision !== renderRevision) return;
      await runtimeService.sendGuiEvent(handlerId, payload);
    } catch (e) {
      if (isGuiSessionSupersededError(e)) return;
      console.error(e);
    }
  }
</script>

<div bind:this={host} class="gui-host" role="application"></div>

<style>
  .gui-host {
    width: 100%;
    flex: 1;
    overflow: hidden;
    background: #fff;
  }
</style>
