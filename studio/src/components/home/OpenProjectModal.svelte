<script lang="ts">
  import { createEventDispatcher } from 'svelte';

  export let busy = false;
  export let error = '';

  const dispatch = createEventDispatcher<{
    close: void;
    openFolder: void;
    openFile: void;
  }>();
</script>

<svelte:window on:keydown={(event) => event.key === 'Escape' && dispatch('close')} />

<div
  class="backdrop"
  role="button"
  tabindex="0"
  aria-label="Close dialog overlay"
  on:click|self={() => dispatch('close')}
  on:keydown={(event) => event.key === 'Escape' && dispatch('close')}
>
  <div class="modal" role="dialog" aria-modal="true" aria-label="Open project">
    <h3>Open Project</h3>
    <p>Open a module directory or a standalone <code>.vo</code> source file.</p>

    <div class="option-list">
      <button class="option-btn" on:click={() => dispatch('openFolder')} disabled={busy}>
        <span class="option-title">Open Folder</span>
        <span class="option-desc">Choose a project directory or module root.</span>
      </button>
      <button class="option-btn" on:click={() => dispatch('openFile')} disabled={busy}>
        <span class="option-title">Open .vo File</span>
        <span class="option-desc">Choose a standalone file or a file inside a module.</span>
      </button>
    </div>

    {#if error}
      <div class="error">{error}</div>
    {/if}

    <div class="actions">
      <button class="secondary" on:click={() => dispatch('close')} disabled={busy}>Cancel</button>
    </div>
  </div>
</div>

<style>
  .backdrop {
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.72);
    backdrop-filter: blur(6px);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 200;
  }
  .modal {
    width: min(100%, 430px);
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 16px;
    padding: 22px;
    display: flex;
    flex-direction: column;
    gap: 16px;
  }
  h3 {
    margin: 0;
    color: #cdd6f4;
    font-size: 18px;
  }
  p {
    margin: 0;
    color: #7f849c;
    font-size: 13px;
    line-height: 1.6;
  }
  code {
    color: #89b4fa;
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
  }
  .option-list {
    display: flex;
    flex-direction: column;
    gap: 10px;
  }
  .option-btn {
    display: flex;
    flex-direction: column;
    align-items: flex-start;
    gap: 4px;
    border: 1px solid #313244;
    background: #181825;
    color: #cdd6f4;
    border-radius: 12px;
    padding: 14px;
    cursor: pointer;
    font: inherit;
    text-align: left;
    transition: border-color 0.2s, background 0.2s;
  }
  .option-btn:hover:not(:disabled) {
    border-color: #89b4fa;
    background: rgba(137, 180, 250, 0.08);
  }
  .option-btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .option-title {
    font-size: 14px;
    font-weight: 700;
    color: #cdd6f4;
  }
  .option-desc {
    font-size: 12px;
    color: #7f849c;
    line-height: 1.5;
  }
  .actions {
    display: flex;
    justify-content: flex-end;
    gap: 10px;
  }
  .secondary {
    border: none;
    border-radius: 10px;
    padding: 10px 14px;
    font: inherit;
    cursor: pointer;
    background: #313244;
    color: #cdd6f4;
  }
  .secondary:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }
  .error {
    color: #f38ba8;
    font-size: 12px;
    line-height: 1.5;
  }
</style>
