<script lang="ts">
  import type { ComponentType } from 'svelte';
  import { onMount } from 'svelte';
  import Sidebar from './components/Sidebar.svelte';
  import Toolbar from './components/Toolbar.svelte';
  import FileTree from './components/FileTree.svelte';
  import Home from './components/Home.svelte';
  import Editor from './components/Editor.svelte';
  import Console from './components/Console.svelte';
  import Terminal from './components/Terminal.svelte';
  import ContextMenu from './components/ContextMenu.svelte';
  import { ide } from './stores/ide';
  import { explorer } from './stores/explorer';
  import { initBridge, bridge } from './lib/bridge';
  import { actions } from './lib/actions';
  import { termInit } from './stores/terminal';
  import { executeStudioLaunch } from './lib/launch';
  import { resolveInitialStudioLaunchUrl } from './lib/launch_protocol';

  let bridgeReady = false;
  let bridgeError = '';
  let bridgeErrorTitle = 'Failed to initialize Vibe Studio bridge';
  let loadingStep = 'Loading…';
  let PreviewPanelComponent: ComponentType | null = null;
  let previewPanelLoading: Promise<void> | null = null;

  function formatStartupError(err: unknown): string {
    if (typeof err === 'string') return err;
    if (err instanceof Error) return err.message;
    if (typeof err === 'object' && err !== null) {
      const message = (err as any).message;
      const code = (err as any).code;
      if (typeof message === 'string' && typeof code === 'string') {
        return `[${code}] ${message}`;
      }
      if (typeof message === 'string') return message;
      try {
        return JSON.stringify(err, null, 2);
      } catch {
      }
    }
    return String(err);
  }

  async function ensurePreviewPanelLoaded(): Promise<void> {
    if (PreviewPanelComponent) return;
    if (!previewPanelLoading) {
      previewPanelLoading = import('./components/PreviewPanel.svelte').then(mod => {
        PreviewPanelComponent = mod.default;
      });
    }
    await previewPanelLoading;
  }

  onMount(async () => {
    try {
      await initBridge((step) => { loadingStep = step; });
      loadingStep = 'Preparing workspace…';
      await actions.initWorkspace();
      termInit(bridge().workspaceRoot);
      bridgeReady = true;
      const launchUrl = await resolveInitialStudioLaunchUrl();
      if (launchUrl) {
        loadingStep = 'Opening launch target…';
        try {
          await executeStudioLaunch(launchUrl);
        } catch (e: unknown) {
          bridgeErrorTitle = 'Failed to open Studio launch target';
          bridgeError = formatStartupError(e);
        }
      }
    } catch (e: unknown) {
      bridgeErrorTitle = 'Failed to initialize Vibe Studio bridge';
      bridgeError = formatStartupError(e);
    }
  });

  $: isGuiApp = $ide.isGuiApp && $ide.guestRender !== null && $ide.guestRender.length > 0;
  $: appMode  = $explorer.appMode;
  $: expanded = $ide.outputExpanded;
  $: if (isGuiApp && !PreviewPanelComponent) {
    void ensurePreviewPanelLoaded();
  }

</script>

{#if bridgeError}
  <div class="splash">
    <pre class="error-text">{bridgeErrorTitle}:{'\n'}{bridgeError}</pre>
  </div>
{:else if !bridgeReady}
  <div class="splash">
    <span class="loading-text">{loadingStep}</span>
  </div>
{:else}
  <div class="app-root">
    <Sidebar />

    {#if appMode === 'manage'}
      <Home />

    {:else if appMode === 'terminal'}
      <div class="terminal-tab">
        <Terminal mode="full" />
      </div>

    {:else if appMode === 'develop'}
      <div class="develop-panel" class:fullscreen={expanded}>
        {#if expanded}
          <button
            class="fs-exit"
            title="Exit fullscreen"
            on:click={() => ide.update(s => ({ ...s, outputExpanded: false }))}
          >×</button>
          <div class="expanded-surface">
            {#if isGuiApp}
              {#if PreviewPanelComponent}
                <svelte:component this={PreviewPanelComponent} guestRender={$ide.guestRender} chromeless />
              {/if}
            {:else}
              <Console mode="fullscreen" />
            {/if}
          </div>
        {:else}
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
              {#if PreviewPanelComponent}
                <svelte:component
                  this={PreviewPanelComponent}
                  guestRender={$ide.guestRender}
                  showFullscreenAction
                  onFullscreenAction={() => ide.update(s => ({ ...s, outputExpanded: true }))}
                />
              {/if}
            {/if}
          </div>
        {/if}
      </div>
    {/if}

    <ContextMenu />
  </div>
{/if}

<style>
  :global(*) { box-sizing: border-box; margin: 0; padding: 0; }
  :global(html, body) { height: 100%; overflow: hidden; }
  :global(body) { font-family: -apple-system, 'Segoe UI', system-ui, sans-serif; }

  .app-root {
    display: flex;
    flex-direction: row;
    height: 100vh;
    background: #11111b;
    color: #cdd6f4;
    overflow: hidden;
  }

  .splash {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100vh;
    background: #11111b;
    padding: 20px;
  }
  .loading-text { color: #585b70; font-size: 14px; }
  .error-text   { color: #f38ba8; font-size: 13px; white-space: pre-wrap; }

  /* Fullscreen exit — floating × bottom-left */
  .fs-exit {
    position: fixed;
    bottom: 20px;
    left: 20px;
    z-index: 100;
    width: 36px;
    height: 36px;
    border-radius: 50%;
    border: 1px solid rgba(239, 68, 68, 0.30);
    background: rgba(239, 68, 68, 0.15);
    color: rgba(255, 180, 180, 0.80);
    font-size: 20px;
    line-height: 1;
    display: flex;
    align-items: center;
    justify-content: center;
    cursor: pointer;
    backdrop-filter: blur(8px);
    transition: background 0.15s, color 0.15s, border-color 0.15s, transform 0.12s;
    font-family: inherit;
  }
  .fs-exit:hover {
    background: rgba(239, 68, 68, 0.28);
    border-color: rgba(239, 68, 68, 0.50);
    color: rgba(255, 200, 200, 0.95);
    transform: scale(1.08);
  }

  /* Code — thin toolbar + IDE */
  .develop-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    min-width: 0;
    background: #11111b;
  }

  .develop-panel.fullscreen {
    background:
      radial-gradient(circle at top center, rgba(137, 180, 250, 0.05), transparent 24%),
      linear-gradient(180deg, #0d0f16 0%, #090b11 100%);
  }

  .expanded-surface {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    min-height: 0;
    background:
      radial-gradient(circle at center, rgba(255, 255, 255, 0.02), transparent 55%),
      linear-gradient(180deg, #090b11 0%, #06070b 100%);
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

  /* Terminal */
  .terminal-tab {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    min-width: 0;
    min-height: 0;
  }
</style>
