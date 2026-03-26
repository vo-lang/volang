<script lang="ts">
  import { runtime } from '../stores/runtime';
  import { console_ } from '../stores/console';
  import type { ServiceRegistry } from '../lib/services/service_registry';
  import PreviewPanel from './PreviewPanel.svelte';

  export let registry: ServiceRegistry;

  $: guiEntryPath = $runtime.guiEntryPath;
  $: isRunning = $runtime.status === 'running';
  $: isReady = $runtime.status === 'ready' && $runtime.kind === 'gui';
  $: hasError = $runtime.lastError != null;
  $: consoleLines = $console_.lines;
</script>

<div class="runner-surface">
  {#if isRunning}
    <div class="runner-loading">
      <div class="runner-loading-inner">
        <div class="spinner"></div>
        <span>Starting {guiEntryPath?.split('/').pop() ?? 'app'}…</span>
      </div>
    </div>
  {:else if hasError}
    <div class="runner-error">
      <div class="runner-error-inner">
        <div class="error-title">Failed to start</div>
        {#each consoleLines as line}
          {#if line.kind === 'stderr'}
            <pre class="error-line">{line.text}</pre>
          {/if}
        {/each}
        {#if $runtime.lastError}
          <pre class="error-line">{$runtime.lastError}</pre>
        {/if}
      </div>
    </div>
  {:else if isReady}
    <PreviewPanel {registry} chromeless={true} />
  {:else}
    <div class="runner-loading">
      <div class="runner-loading-inner">
        <div class="spinner"></div>
        <span>Initializing…</span>
      </div>
    </div>
  {/if}
</div>

<style>
  .runner-surface {
    display: flex;
    flex-direction: column;
    width: 100%;
    height: 100%;
    background: #0d0f16;
    overflow: hidden;
  }

  .runner-loading {
    display: grid;
    place-items: center;
    flex: 1;
    height: 100%;
  }

  .runner-loading-inner {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 16px;
    color: #585b70;
    font-size: 14px;
  }

  .spinner {
    width: 32px;
    height: 32px;
    border: 3px solid #313244;
    border-top-color: #89b4fa;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
  }

  @keyframes spin {
    to { transform: rotate(360deg); }
  }

  .runner-error {
    display: grid;
    place-items: center;
    flex: 1;
    height: 100%;
    padding: 24px;
  }

  .runner-error-inner {
    max-width: 600px;
    width: 100%;
  }

  .error-title {
    color: #f38ba8;
    font-size: 16px;
    font-weight: 600;
    margin-bottom: 12px;
  }

  .error-line {
    color: #fab387;
    font-family: 'JetBrains Mono', 'Fira Code', monospace;
    font-size: 12px;
    background: #181825;
    padding: 8px 12px;
    border-radius: 6px;
    border-left: 3px solid #f38ba8;
    margin: 4px 0;
    overflow-x: auto;
    white-space: pre-wrap;
  }
</style>
