<script lang="ts">
  interface Node {
    Type: string;
    Props?: Record<string, any>;
    Children?: Node[];
  }

  let { nodeTree }: { nodeTree: Node | null } = $props();
</script>

{#snippet renderNode(node: Node)}
  {#if node.Type === 'Column'}
    <div class="vo-column">
      {#if node.Children}
        {#each node.Children as child}
          {@render renderNode(child)}
        {/each}
      {/if}
    </div>
  {:else if node.Type === 'Row'}
    <div class="vo-row">
      {#if node.Children}
        {#each node.Children as child}
          {@render renderNode(child)}
        {/each}
      {/if}
    </div>
  {:else if node.Type === 'Text'}
    <span class="vo-text">{node.Props?.content ?? ''}</span>
  {:else if node.Type === 'Button'}
    <button class="vo-button" disabled>
      {node.Props?.text ?? 'Button'}
    </button>
  {:else if node.Type === 'Input'}
    <input class="vo-input" type="text" value={node.Props?.value ?? ''} disabled />
  {:else if node.Type === 'Checkbox'}
    <label class="vo-checkbox">
      <input type="checkbox" checked={node.Props?.checked ?? false} disabled />
    </label>
  {:else if node.Type === 'Spacer'}
    <div class="vo-spacer"></div>
  {:else if node.Type === 'Empty'}
    <!-- empty -->
  {:else}
    <div class="vo-unknown">[{node.Type}]</div>
  {/if}
{/snippet}

<div class="gui-preview">
  <div class="preview-header">
    <span class="preview-title">GUI Preview</span>
    <span class="preview-badge">Phase 1: Static</span>
  </div>
  <div class="preview-content">
    {#if nodeTree}
      {@render renderNode(nodeTree)}
    {:else}
      <div class="placeholder">
        <div class="placeholder-icon">🖼️</div>
        <div class="placeholder-text">Run GUI code to see preview</div>
        <div class="placeholder-hint">Import "gui" and call gui.Run()</div>
      </div>
    {/if}
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
    padding: 16px;
    overflow: auto;
    background: #ffffff;
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

  /* VoGUI Component Styles */
  .vo-column {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .vo-row {
    display: flex;
    flex-direction: row;
    gap: 8px;
    align-items: center;
  }

  .vo-text {
    font-size: 14px;
    color: #333;
  }

  :global(.dark) .vo-text {
    color: #eee;
  }

  .vo-button {
    padding: 8px 16px;
    font-size: 14px;
    border: none;
    border-radius: 6px;
    background: var(--accent);
    color: white;
    cursor: not-allowed;
    opacity: 0.8;
  }

  .vo-input {
    padding: 8px 12px;
    font-size: 14px;
    border: 1px solid var(--border);
    border-radius: 6px;
    background: var(--bg-primary);
    color: var(--text-primary);
  }

  .vo-checkbox {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .vo-spacer {
    flex: 1;
  }

  .vo-unknown {
    padding: 4px 8px;
    background: #fee;
    color: #c00;
    border-radius: 4px;
    font-family: var(--font-mono);
    font-size: 12px;
  }
</style>
