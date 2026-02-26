<script lang="ts">
  import { onMount } from 'svelte';
  import Sidebar from './components/Sidebar.svelte';
  import Toolbar from './components/Toolbar.svelte';
  import FileTree from './components/FileTree.svelte';
  import Home from './components/Home.svelte';
  import Editor from './components/Editor.svelte';
  import OutputPanel from './components/OutputPanel.svelte';
  import PreviewPanel from './components/PreviewPanel.svelte';
  import ContextMenu from './components/ContextMenu.svelte';
  import { ide } from './stores/ide';
  import { editTargetLabel } from './stores/ide';
  import { explorer } from './stores/explorer';
  import { initBridge } from './lib/bridge';
  import { actions } from './lib/actions';

  let bridgeReady = false;
  let bridgeError = '';

  onMount(async () => {
    try {
      await initBridge();
      await actions.initWorkspace();
      bridgeReady = true;
    } catch (e: any) {
      bridgeError = String(e);
    }
  });

  $: isGuiApp = $ide.isGuiApp && $ide.guestRender !== '';
  $: appMode  = $explorer.appMode;
  $: fileName = $ide.activeFilePath ? $ide.activeFilePath.split('/').pop() ?? '' : '';
  $: ctxLabel = $ide.editTarget ? editTargetLabel($ide.editTarget) : '';
  $: ctxMode  = $ide.projectMode === 'multi' ? 'Multi File' : 'Single File';
</script>

{#if bridgeError}
  <div class="splash">
    <pre class="error-text">Failed to initialize Vibe Studio bridge:{'\n'}{bridgeError}</pre>
  </div>
{:else if !bridgeReady}
  <div class="splash">
    <span class="loading-text">Loading…</span>
  </div>
{:else}
  <div class="app-root">
    <!-- Activity bar -->
    <Sidebar />

    <!-- ── Home mode: Explorer + GitHub ── -->
    {#if appMode === 'manage'}
      <Home />

    <!-- ── Develop mode: IDE ── -->
    {:else if appMode === 'develop'}
      <div class="develop-panel">
        <!-- Edit context bar -->
        {#if ctxLabel}
          <div class="context-bar">
            <span class="ctx-source-icon">⊞</span>
            <span class="ctx-label">{ctxLabel}</span>
            <span class="ctx-sep">·</span>
            <span class="ctx-mode">{ctxMode}</span>
          </div>
        {/if}
        <Toolbar />
        <div class="ide-main">
          {#if $ide.projectMode === 'multi'}
            <FileTree />
          {/if}
          <Editor
            value={$ide.code}
            on:change={(e) => actions.onEditorChange(e.detail)}
          />
          {#if isGuiApp}
            <PreviewPanel guestRender={$ide.guestRender} />
          {:else}
            <OutputPanel output={$ide.output} compileError={$ide.compileError} />
          {/if}
        </div>
      </div>

    <!-- ── Run mode: full-screen output / preview ── -->
    {:else if appMode === 'run'}
      <div class="run-panel">
        <div class="run-header">
          <span class="run-title">{fileName || 'No file open'}</span>
          <div class="run-controls">
            <button
              class="run-btn"
              disabled={$ide.isRunning || !$ide.activeFilePath}
              on:click={() => actions.runCode()}
            >▶ Run</button>
            <button
              class="run-btn stop"
              disabled={!$ide.isRunning}
              on:click={() => actions.stopCode()}
            >⏹ Stop</button>
          </div>
          <button
            class="back-btn"
            on:click={() => explorer.update(e => ({ ...e, appMode: 'develop' }))}
          >← Editor</button>
        </div>
        <div class="run-content">
          {#if isGuiApp}
            <PreviewPanel guestRender={$ide.guestRender} />
          {:else}
            <OutputPanel output={$ide.output} compileError={$ide.compileError} />
          {/if}
        </div>
      </div>
    {/if}

    <!-- Singleton context menu (fixed position, always on top) -->
    <ContextMenu />
  </div>
{/if}

<style>
  :global(*) { box-sizing: border-box; margin: 0; padding: 0; }
  :global(html, body) { height: 100%; overflow: hidden; }
  :global(body) { font-family: -apple-system, 'Segoe UI', system-ui, sans-serif; }

  /* ── Shell ── */
  .app-root {
    display: flex;
    flex-direction: row;
    height: 100vh;
    background: #1e1e2e;
    color: #cdd6f4;
    overflow: hidden;
  }

  /* ── Splash screens ── */
  .splash {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100vh;
    background: #1e1e2e;
    padding: 20px;
  }

  .loading-text {
    color: #585b70;
    font-size: 14px;
  }

  .error-text {
    color: #f38ba8;
    font-size: 13px;
    white-space: pre-wrap;
  }

  /* ── Edit context bar ── */
  .context-bar {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 4px 14px;
    background: #11111b;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
    min-height: 26px;
  }
  .ctx-source-icon { font-size: 12px; color: #45475a; line-height: 1; }
  .ctx-label { font-size: 12px; color: #7f849c; font-weight: 500; }
  .ctx-sep { font-size: 11px; color: #313244; }
  .ctx-mode { font-size: 11px; color: #45475a; }

  /* ── Develop panel ── */
  .develop-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    min-width: 0;
  }

  .ide-main {
    display: flex;
    flex: 1;
    overflow: hidden;
    min-height: 0;
  }

  /* ── Run panel ── */
  .run-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    min-width: 0;
  }

  .run-header {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 8px 16px;
    background: #181825;
    border-bottom: 1px solid #313244;
    flex-shrink: 0;
  }

  .run-title {
    flex: 1;
    font-size: 13px;
    color: #a6adc8;
    font-weight: 500;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .run-controls {
    display: flex;
    gap: 6px;
    flex-shrink: 0;
  }

  .run-btn {
    border: 1px solid #313244;
    background: #252535;
    color: #a6adc8;
    font-size: 13px;
    padding: 4px 14px;
    border-radius: 5px;
    cursor: pointer;
    font-family: inherit;
  }
  .run-btn:hover:not(:disabled) { background: #313244; color: #cdd6f4; }
  .run-btn:disabled { opacity: 0.4; cursor: not-allowed; }
  .run-btn.stop { color: #f38ba8; }
  .run-btn.stop:hover:not(:disabled) { background: #3b1f2b; border-color: #5b2f3b; }

  .back-btn {
    border: none;
    background: none;
    color: #585b70;
    font-size: 12px;
    padding: 4px 8px;
    border-radius: 4px;
    cursor: pointer;
    font-family: inherit;
    flex-shrink: 0;
  }
  .back-btn:hover { background: #252535; color: #a6adc8; }

  .run-content {
    flex: 1;
    display: flex;
    overflow: hidden;
    min-height: 0;
  }
</style>
