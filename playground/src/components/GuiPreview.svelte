<script lang="ts">
  import { onMount } from 'svelte';
  import { 
    type VoNode, 
    type EventCallback,
    render as voguiRender, 
    setupKeyHandler,
    injectStyles 
  } from '../../../libs/vogui/js/src/index';

  interface Props {
    nodeTree: VoNode | null;
    interactive?: boolean;
    onEvent?: EventCallback;
  }

  let { nodeTree, interactive = false, onEvent }: Props = $props();
  
  let renderEl: HTMLElement | undefined = $state();
  let contentEl: HTMLElement | undefined = $state();
  let cleanupKeyHandler: (() => void) | null = null;
  let scale = $state(1);
  
  
  $effect(() => {
    if (renderEl && nodeTree) {
      voguiRender(renderEl, nodeTree, { interactive, onEvent });
      // Calculate scale after render
      requestAnimationFrame(() => updateScale());
    } else if (renderEl) {
      renderEl.innerHTML = '';
      scale = 1;
    }
  });

  function updateScale() {
    if (!contentEl || !renderEl) return;
    
    // Temporarily reset scale to measure true content size
    const prevScale = scale;
    renderEl.style.transform = 'scale(1)';
    
    const container = contentEl.getBoundingClientRect();
    const content = renderEl.getBoundingClientRect();
    
    if (content.width === 0 || content.height === 0) {
      renderEl.style.transform = `scale(${prevScale})`;
      return;
    }
    
    const scaleX = container.width / content.width;
    const scaleY = container.height / content.height;
    scale = Math.min(scaleX, scaleY); // Scale to fit container
  }

  // Recalculate on resize
  onMount(() => {
    injectStyles();
    const resizeObserver = new ResizeObserver(() => {
      if (nodeTree) updateScale();
    });
    return () => {
      resizeObserver.disconnect();
      if (cleanupKeyHandler) cleanupKeyHandler();
    };
  });

  $effect(() => {
    if (contentEl) {
      const resizeObserver = new ResizeObserver(() => {
        if (nodeTree) updateScale();
      });
      resizeObserver.observe(contentEl);
      return () => resizeObserver.disconnect();
    }
  });
  
  $effect(() => {
    if (cleanupKeyHandler) {
      cleanupKeyHandler();
      cleanupKeyHandler = null;
    }
    if (interactive && onEvent) {
      cleanupKeyHandler = setupKeyHandler({ interactive, onEvent });
    }
  });
</script>

<div class="gui-preview">
  <div class="preview-header">
    <span class="preview-title">GUI Preview</span>
    <span class="preview-badge">{interactive ? 'Interactive' : 'Static'}</span>
  </div>
  <div class="preview-content" bind:this={contentEl}>
    {#if !nodeTree}
      <div class="placeholder">
        <div class="placeholder-icon">🖼️</div>
        <div class="placeholder-text">Run GUI code to see preview</div>
        <div class="placeholder-hint">Import "gui" and call gui.Run()</div>
      </div>
    {/if}
    <div 
      class="render-container" 
      class:hidden={!nodeTree} 
      bind:this={renderEl}
      style:transform="scale({scale})"
      style:transform-origin="center center"
    ></div>
  </div>
</div>

<style>
  .gui-preview {
    display: flex;
    flex-direction: column;
    height: 100%;
    background: var(--bg-primary);
  }

  .preview-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0 16px;
    height: 40px;
    background: var(--bg-secondary);
    border-bottom: 1px solid var(--border);
    flex-shrink: 0;
  }

  .preview-title {
    font-weight: 600;
    font-size: 13px;
    color: var(--text-secondary);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .preview-badge {
    font-size: 10px;
    padding: 2px 6px;
    background: var(--accent);
    color: white;
    border-radius: 4px;
    font-family: var(--font-mono);
  }

  .preview-content {
    flex: 1;
    padding: 8px;
    overflow: hidden;
    background: #ffffff;
    position: relative;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  :global(.dark) .preview-content {
    background: #1a1a2e;
  }

  .placeholder {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 100%;
    color: var(--text-tertiary);
    text-align: center;
    gap: 8px;
  }

  .placeholder-icon {
    font-size: 48px;
    opacity: 0.5;
  }

  .placeholder-text {
    font-size: 14px;
  }

  .placeholder-hint {
    font-size: 12px;
    font-family: var(--font-mono);
    opacity: 0.7;
  }

  .render-container.hidden {
    display: none;
  }
</style>
