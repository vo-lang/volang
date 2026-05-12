<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import type { ProjectDiffResult } from '../../lib/project_catalog/types';
  import { projectStatusPresentation } from '../../lib/project_catalog/types';

  export let diff: ProjectDiffResult;
  export let busy = false;

  const dispatch = createEventDispatcher<{
    close: void;
    push: void;
    pull: void;
  }>();

  let selectedPath = '';
  $: selectedPath = diff?.entries.find((entry) => entry.status !== 'unchanged')?.path ?? diff?.entries[0]?.path ?? '';
  $: selected = diff?.entries.find((entry) => entry.path === selectedPath) ?? null;
  $: status = diff ? projectStatusPresentation(diff.project, { state: diff.syncState }) : null;

  function statusIcon(status: string): string {
    if (status === 'added') return '+';
    if (status === 'removed') return '−';
    if (status === 'modified') return '~';
    return '=';
  }
</script>

<div
  class="backdrop"
  role="button"
  tabindex="0"
  aria-label="Close dialog overlay"
  on:click|self={() => dispatch('close')}
  on:keydown={(event) => event.key === 'Escape' && dispatch('close')}
>
  <div class="modal" role="dialog" aria-modal="true" aria-label="Project diff">
    <div class="header">
      <div>
        <h3>{diff.project.name}</h3>
        <div class="state">{status?.label ?? ''}</div>
      </div>
      <button class="close" on:click={() => dispatch('close')} aria-label="Close">×</button>
    </div>

    <div class="summary">
      <span>{diff.entries.filter((entry) => entry.status !== 'unchanged').length} changed</span>
      <span>·</span>
      <span>{diff.entries.filter((entry) => entry.status === 'unchanged').length} unchanged</span>
    </div>

    <div class="body">
      <div class="files">
        {#each diff.entries as entry}
          <button class="file" class:active={entry.path === selectedPath} on:click={() => (selectedPath = entry.path)}>
            <span class={`status ${entry.status}`}>{statusIcon(entry.status)}</span>
            <span>{entry.path}</span>
          </button>
        {/each}
      </div>
      <div class="viewer">
        {#if selected}
          <div class="viewer-head">
            <span>{selected.path}</span>
            <span class={`badge ${selected.status}`}>{selected.status}</span>
          </div>
          <div class="panes">
            <div class="pane">
              <div class="pane-title">Local</div>
              <pre>{selected.localContent ?? '(not present)'}</pre>
            </div>
            <div class="pane">
              <div class="pane-title">Cloud</div>
              <pre>{selected.remoteContent ?? '(not present)'}</pre>
            </div>
          </div>
        {/if}
      </div>
    </div>

    <div class="actions">
      <button class="secondary" on:click={() => dispatch('close')} disabled={busy}>Close</button>
      {#if diff.project.localPath}
        <button class="push" on:click={() => dispatch('push')} disabled={busy}>Upload Local → Cloud</button>
      {/if}
      {#if diff.project.remote}
        <button class="pull" on:click={() => dispatch('pull')} disabled={busy}>Download Cloud → Local</button>
      {/if}
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
    padding: 24px;
    z-index: 200;
  }
  .modal {
    width: min(100%, 1080px);
    max-height: 88vh;
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 18px;
    display: flex;
    flex-direction: column;
    overflow: hidden;
  }
  .header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 18px 20px 14px;
    border-bottom: 1px solid #252535;
  }
  h3 {
    margin: 0;
    color: #cdd6f4;
    font-size: 17px;
  }
  .state {
    margin-top: 6px;
    color: #89b4fa;
    font-size: 12px;
    font-weight: 700;
  }
  .close {
    border: none;
    background: transparent;
    color: #7f849c;
    font-size: 24px;
    cursor: pointer;
  }
  .summary {
    padding: 10px 20px;
    border-bottom: 1px solid #252535;
    color: #7f849c;
    display: flex;
    gap: 8px;
    font-size: 12px;
  }
  .body {
    flex: 1;
    min-height: 0;
    display: grid;
    grid-template-columns: 280px minmax(0, 1fr);
  }
  .files {
    border-right: 1px solid #252535;
    overflow: auto;
    padding: 10px;
    display: flex;
    flex-direction: column;
    gap: 6px;
  }
  .file {
    border: none;
    background: transparent;
    color: #cdd6f4;
    font: inherit;
    text-align: left;
    padding: 9px 10px;
    border-radius: 10px;
    display: flex;
    gap: 10px;
    cursor: pointer;
  }
  .file:hover,
  .file.active {
    background: #313244;
  }
  .status {
    width: 16px;
    text-align: center;
    font-weight: 700;
  }
  .status.added { color: #a6e3a1; }
  .status.removed { color: #f38ba8; }
  .status.modified { color: #f9e2af; }
  .status.unchanged { color: #7f849c; }
  .viewer {
    min-width: 0;
    display: flex;
    flex-direction: column;
  }
  .viewer-head {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 12px 16px;
    border-bottom: 1px solid #252535;
    color: #cdd6f4;
    font-size: 13px;
  }
  .badge {
    padding: 3px 8px;
    border-radius: 999px;
    font-size: 11px;
    font-weight: 700;
    text-transform: capitalize;
  }
  .badge.added { background: rgba(166, 227, 161, 0.14); color: #a6e3a1; }
  .badge.removed { background: rgba(243, 139, 168, 0.14); color: #f38ba8; }
  .badge.modified { background: rgba(249, 226, 175, 0.14); color: #f9e2af; }
  .badge.unchanged { background: rgba(127, 132, 156, 0.14); color: #7f849c; }
  .panes {
    flex: 1;
    min-height: 0;
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
  .pane {
    min-width: 0;
    display: flex;
    flex-direction: column;
    border-right: 1px solid #252535;
  }
  .pane:last-child {
    border-right: none;
  }
  .pane-title {
    padding: 10px 14px;
    color: #89b4fa;
    font-size: 12px;
    font-weight: 700;
    border-bottom: 1px solid #252535;
  }
  pre {
    margin: 0;
    flex: 1;
    min-height: 0;
    overflow: auto;
    padding: 14px;
    color: #cdd6f4;
    background: #11111b;
    font-size: 12px;
    line-height: 1.6;
    font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
    white-space: pre-wrap;
    word-break: break-word;
  }
  .actions {
    padding: 14px 20px;
    border-top: 1px solid #252535;
    display: flex;
    justify-content: flex-end;
    gap: 10px;
  }
  button {
    font: inherit;
  }
  .actions button {
    border: none;
    border-radius: 10px;
    padding: 10px 14px;
    cursor: pointer;
  }
  .secondary {
    background: #313244;
    color: #cdd6f4;
  }
  .push {
    background: #89b4fa;
    color: #11111b;
    font-weight: 700;
  }
  .pull {
    background: #74c7ec;
    color: #11111b;
    font-weight: 700;
  }
</style>
