<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import { github } from '../stores/github';
  import type { ProjectEntry, SyncState } from '../stores/projects';

  export let x: number;
  export let y: number;
  export let project: ProjectEntry;
  export let state: SyncState;

  const dispatch = createEventDispatcher<{
    close: void;
    open: ProjectEntry;
    rename: ProjectEntry;
    viewDiff: { project: ProjectEntry; state: SyncState };
    push: ProjectEntry;
    pull: ProjectEntry;
    viewOnGitHub: ProjectEntry;
    delete: ProjectEntry;
  }>();
</script>

<!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
<div class="ctx-backdrop" on:click={() => dispatch('close')}>
  <div class="ctx-menu" style="left:{x}px;top:{y}px" on:click|stopPropagation={() => {}}>
    <button class="ctx-item" on:click={() => { dispatch('close'); dispatch('open', project); }}>
      <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M5 3l14 9-14 9V3z"/></svg>
      Open
    </button>
    <button class="ctx-item" on:click={() => { dispatch('close'); dispatch('rename', project); }}>
      <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 20h9"/><path d="M16.5 3.5a2.121 2.121 0 113 3L7 19l-4 1 1-4 12.5-12.5z"/></svg>
      Rename
    </button>
    {#if project.localPath && project.remote}
      <button class="ctx-item" on:click={() => { dispatch('close'); dispatch('viewDiff', { project, state }); }}>
        <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 20V10M18 20V4M6 20v-4"/></svg>
        View Diff
      </button>
    {/if}
    {#if (state === 'local-only' && $github.token) || state === 'in-sync' || state === 'local-ahead' || state === 'diverged'}
      <button class="ctx-item" on:click={() => { dispatch('close'); dispatch('push', project); }}>
        <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="19" x2="12" y2="5"/><polyline points="5 12 12 5 19 12"/></svg>
        Push to GitHub
      </button>
    {/if}
    {#if state === 'remote-only' || state === 'in-sync' || state === 'remote-ahead' || state === 'diverged'}
      <button class="ctx-item" on:click={() => { dispatch('close'); dispatch('pull', project); }}>
        <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"/><polyline points="19 12 12 19 5 12"/></svg>
        Pull from GitHub
      </button>
    {/if}
    {#if project.remote}
      <button class="ctx-item" on:click={() => { dispatch('close'); dispatch('viewOnGitHub', project); }}>
        <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M18 13v6a2 2 0 01-2 2H5a2 2 0 01-2-2V8a2 2 0 012-2h6"/><polyline points="15 3 21 3 21 9"/><line x1="10" y1="14" x2="21" y2="3"/></svg>
        View on GitHub
      </button>
    {/if}
    <div class="ctx-sep"></div>
    <button class="ctx-item danger" on:click={() => { dispatch('close'); dispatch('delete', project); }}>
      <svg viewBox="0 0 24 24" width="14" height="14" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="3 6 5 6 21 6"/><path d="M19 6v14a2 2 0 01-2 2H7a2 2 0 01-2-2V6m3 0V4a2 2 0 012-2h4a2 2 0 012 2v2"/></svg>
      Delete
    </button>
  </div>
</div>

<style>
  .ctx-backdrop {
    position: fixed; inset: 0; z-index: 200;
  }

  .ctx-menu {
    position: fixed; z-index: 201;
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 8px;
    box-shadow: 0 8px 32px rgba(0,0,0,0.5);
    padding: 4px;
    min-width: 180px;
    animation: ctx-in 0.1s ease;
  }
  @keyframes ctx-in { from { opacity: 0; transform: scale(0.97); } to { opacity: 1; transform: none; } }

  .ctx-item {
    display: flex; align-items: center; gap: 8px;
    width: 100%; padding: 7px 10px;
    background: none; border: none; border-radius: 5px;
    color: #a6adc8; font-size: 12.5px; font-family: inherit;
    cursor: pointer; text-align: left;
    transition: background 0.1s, color 0.1s;
  }
  .ctx-item:hover { background: #313244; color: #cdd6f4; }
  .ctx-item.danger { color: #f38ba8; }
  .ctx-item.danger:hover { background: rgba(243,139,168,0.12); }

  .ctx-sep { height: 1px; background: #313244; margin: 4px 4px; }
</style>
