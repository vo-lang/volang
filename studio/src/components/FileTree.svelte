<script lang="ts">
  import { editor } from '../stores/editor';
  import type { FsEntry } from '../lib/types';

  export let entries: FsEntry[] = [];
  export let currentDir: string = '';
  export let sessionRoot: string = '';
  export let onOpenEntry: (entry: FsEntry) => void = () => {};
  export let onGoParent: () => void = () => {};

  $: selectedPath = $editor.activeFilePath;
  $: atRoot = currentDir === sessionRoot;

  function dirName(path: string): string {
    if (!path) return '';
    const parts = path.split('/');
    const name = parts[parts.length - 1] || parts[parts.length - 2] || path;
    return name;
  }
</script>

<div class="file-tree">
  <div class="tree-header">
    <span class="tree-title">Explorer</span>
    <div class="tree-path" title={currentDir}>{dirName(currentDir)}</div>
    <button class="up-btn" on:click={onGoParent} disabled={atRoot} title="Go up">↑</button>
  </div>
  <div class="tree-list">
    {#each entries as entry}
      <button
        class="entry"
        class:selected={selectedPath === entry.path}
        class:is-dir={entry.isDir}
        on:click={() => onOpenEntry(entry)}
      >
        <span class="entry-icon">{entry.isDir ? '📁' : '📄'}</span>
        <span class="entry-name">{entry.name}</span>
      </button>
    {/each}
    {#if entries.length === 0}
      <div class="empty">No files</div>
    {/if}
  </div>
</div>

<style>
  .file-tree {
    display: flex;
    flex-direction: column;
    height: 100%;
    background: #11111b;
    border-right: 1px solid #1e1e2e;
    min-width: 0;
    overflow: hidden;
  }
  .tree-header {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 8px 10px;
    border-bottom: 1px solid #1e1e2e;
    flex-shrink: 0;
  }
  .tree-title {
    font-size: 10px;
    font-weight: 700;
    letter-spacing: 0.08em;
    text-transform: uppercase;
    color: #585b70;
    flex: 1;
  }
  .tree-path {
    font-size: 11px;
    color: #45475a;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
    max-width: 100px;
  }
  .up-btn {
    border: none;
    background: none;
    color: #45475a;
    cursor: pointer;
    padding: 2px 6px;
    border-radius: 4px;
    font-size: 13px;
  }
  .up-btn:hover:not(:disabled) { background: #1e1e2e; color: #a6adc8; }
  .up-btn:disabled { opacity: 0.3; cursor: not-allowed; }
  .tree-list {
    flex: 1;
    overflow-y: auto;
    padding: 4px;
  }
  .entry {
    display: flex;
    align-items: center;
    gap: 6px;
    width: 100%;
    background: none;
    border: none;
    color: #a6adc8;
    padding: 5px 8px;
    border-radius: 5px;
    cursor: pointer;
    text-align: left;
    font-family: inherit;
    font-size: 13px;
    transition: background 0.1s;
  }
  .entry:hover { background: #1e1e2e; }
  .entry.selected { background: #313244; color: #cdd6f4; }
  .entry-icon { font-size: 12px; flex-shrink: 0; }
  .entry-name { overflow: hidden; text-overflow: ellipsis; white-space: nowrap; min-width: 0; }
  .empty { color: #45475a; font-size: 12px; padding: 12px 10px; }
</style>
