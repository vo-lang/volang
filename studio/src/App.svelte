<script lang="ts">
  import { onMount } from 'svelte';
  import Sidebar from './components/Sidebar.svelte';
  import Toolbar from './components/Toolbar.svelte';
  import FileTree from './components/FileTree.svelte';
  import Home from './components/Home.svelte';
  import Editor from './components/Editor.svelte';
  import Console from './components/Console.svelte';
  import Terminal from './components/Terminal.svelte';
  import PreviewPanel from './components/PreviewPanel.svelte';
  import ContextMenu from './components/ContextMenu.svelte';
  import { ide } from './stores/ide';
  import { editTargetLabel } from './stores/ide';
  import { explorer } from './stores/explorer';
  import { initBridge, bridge } from './lib/bridge';
  import { actions } from './lib/actions';
  import { termInit } from './stores/terminal';

  let bridgeReady = false;
  let bridgeError = '';

  onMount(async () => {
    try {
      await initBridge();
      await actions.initWorkspace();
      termInit(bridge().workspaceRoot);
      bridgeReady = true;
    } catch (e: any) {
      bridgeError = String(e);
    }
  });

  $: isGuiApp = $ide.isGuiApp && $ide.guestRender !== null && $ide.guestRender.length > 0;
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
  <!-- ================================================================== -->
  <!-- HOME + DEVELOP: Normal IDE with sidebar                            -->
  <!-- ================================================================== -->
  <div class="app-root">
    <Sidebar />

    {#if appMode === 'run'}
      <!-- ============================================================== -->
      <!-- RUN MODE: shares sidebar, full content area                    -->
      <!-- ============================================================== -->
      <div class="run-panel">
        {#if isGuiApp}
          <PreviewPanel guestRender={$ide.guestRender} chromeless />
        {:else}
          <div class="run-terminal-wrapper">
            <div class="run-terminal-card">
              <div class="run-terminal-header">
                <div class="run-terminal-dots">
                  <span class="dot dot-red"></span>
                  <span class="dot dot-yellow"></span>
                  <span class="dot dot-green"></span>
                </div>
                <span class="run-terminal-title">{fileName || 'Untitled'}</span>
                <div class="run-terminal-actions">
                  <button
                    class="run-action-btn"
                    disabled={$ide.isRunning || !$ide.activeFilePath}
                    on:click={() => actions.runCode()}
                    title="Run"
                  >
                    <svg width="14" height="14" viewBox="0 0 24 24" fill="currentColor"><polygon points="5 3 19 12 5 21 5 3"/></svg>
                  </button>
                  <button
                    class="run-action-btn stop"
                    disabled={!$ide.isRunning}
                    on:click={() => actions.stopCode()}
                    title="Stop"
                  >
                    <svg width="14" height="14" viewBox="0 0 24 24" fill="currentColor"><rect x="4" y="4" width="16" height="16" rx="2"/></svg>
                  </button>
                </div>
              </div>
              <Console mode="fullscreen" />
            </div>
          </div>
        {/if}
      </div>

    {:else if appMode === 'manage'}
      <Home />

    {:else if appMode === 'terminal'}
      <div class="terminal-tab">
        <Terminal mode="full" />
      </div>

    {:else if appMode === 'develop'}
      <div class="develop-panel">
        {#if ctxLabel}
          <div class="context-bar">
            <span class="ctx-source-icon">⊞</span>
            <span class="ctx-label">{ctxLabel}</span>
            <span class="ctx-sep">·</span>
            <span class="ctx-mode">{ctxMode}</span>
          </div>
        {/if}
        <Toolbar />
        <div class="ide-body">
          {#if $ide.projectMode === 'multi'}
            <FileTree />
          {/if}
          <div class="editor-console">
            <Editor
              value={$ide.code}
              filePath={$ide.activeFilePath}
              on:change={(e) => actions.onEditorChange(e.detail)}
            />
            <Console mode="panel" />
          </div>
          {#if isGuiApp}
            <PreviewPanel guestRender={$ide.guestRender} />
          {/if}
        </div>
      </div>
    {/if}

    <ContextMenu />
  </div>
{/if}

<style>
  :global(*) { box-sizing: border-box; margin: 0; padding: 0; }
  :global(html, body) { height: 100%; overflow: hidden; }
  :global(body) { font-family: -apple-system, 'Segoe UI', system-ui, sans-serif; }

  /* ================================================================ */
  /* Shell                                                            */
  /* ================================================================ */
  .app-root {
    display: flex;
    flex-direction: row;
    height: 100vh;
    background: #1e1e2e;
    color: #cdd6f4;
    overflow: hidden;
  }

  /* Splash */
  .splash {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100vh;
    background: #1e1e2e;
    padding: 20px;
  }
  .loading-text { color: #585b70; font-size: 14px; }
  .error-text   { color: #f38ba8; font-size: 13px; white-space: pre-wrap; }

  /* Context bar */
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
  .ctx-label       { font-size: 12px; color: #7f849c; font-weight: 500; }
  .ctx-sep         { font-size: 11px; color: #313244; }
  .ctx-mode        { font-size: 11px; color: #45475a; }

  /* ================================================================ */
  /* Develop mode                                                     */
  /* ================================================================ */
  .develop-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    min-width: 0;
  }

  .ide-body {
    flex: 1;
    display: flex;
    flex-direction: row;
    overflow: hidden;
    min-height: 0;
  }

  .editor-console {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    min-width: 0;
  }

  /* ================================================================ */
  /* Terminal tab                                                     */
  /* ================================================================ */
  .terminal-tab {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    min-width: 0;
    min-height: 0;
  }

  /* ================================================================ */
  /* Run mode — lives inside app-root beside the sidebar             */
  /* ================================================================ */
  .run-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
    background: #08080e;
    overflow: hidden;
    min-width: 0;
    min-height: 0;
  }

  /* Console terminal card */
  .run-terminal-wrapper {
    width: 100%;
    height: 100%;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 32px;
  }

  .run-terminal-card {
    width: 100%;
    max-width: 860px;
    height: 100%;
    max-height: 580px;
    border-radius: 12px;
    overflow: hidden;
    display: flex;
    flex-direction: column;
    background: #0a0a0f;
    box-shadow:
      0 4px 24px rgba(0, 0, 0, 0.6),
      0 0 0 1px rgba(255, 255, 255, 0.04);
  }

  .run-terminal-header {
    display: flex;
    align-items: center;
    gap: 10px;
    padding: 10px 16px;
    background: #111118;
    border-bottom: 1px solid #1a1a28;
    flex-shrink: 0;
  }

  .run-terminal-dots {
    display: flex;
    gap: 6px;
  }

  .dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
  }
  .dot-red    { background: #ff5f57; }
  .dot-yellow { background: #febc2e; }
  .dot-green  { background: #28c840; }

  .run-terminal-title {
    flex: 1;
    text-align: center;
    font-size: 12px;
    font-weight: 600;
    color: #4a4a6a;
    letter-spacing: 0.02em;
  }

  .run-terminal-actions {
    display: flex;
    gap: 4px;
  }

  .run-action-btn {
    width: 28px;
    height: 28px;
    display: flex;
    align-items: center;
    justify-content: center;
    background: none;
    border: 1px solid #1e1e35;
    border-radius: 6px;
    color: #5a5a7a;
    cursor: pointer;
    transition: all 0.12s;
  }
  .run-action-btn:hover:not(:disabled) { background: #1a1a2e; color: #89b4fa; border-color: #2a2a4a; }
  .run-action-btn:disabled { opacity: 0.3; cursor: not-allowed; }
  .run-action-btn.stop:hover:not(:disabled) { color: #f87171; border-color: #3b1a1a; background: #1a0f0f; }

</style>
