<script lang="ts">
  import { createEventDispatcher, onMount } from 'svelte';
  import type { ProjectEntry, SyncState } from '../stores/projects';
  import { syncStateLabel, fetchRemoteContent, fetchLocalContent } from '../stores/projects';

  export let project: ProjectEntry;
  export let state: SyncState;
  export let busy: boolean = false;

  const dispatch = createEventDispatcher<{
    close: void;
    push: void;
    pull: void;
  }>();

  interface FileDiff {
    path: string;
    localContent: string | null;
    remoteContent: string | null;
    status: 'added' | 'removed' | 'modified' | 'unchanged';
  }

  let isLoading = true;
  let loadError = '';
  let diffs: FileDiff[] = [];
  let selectedFile: FileDiff | null = null;
  let confirmAction: 'push' | 'pull' | null = null;

  onMount(async () => {
    try {
      const [localFiles, remoteFiles] = await Promise.all([
        fetchLocalContent(project),
        fetchRemoteContent(project),
      ]);

      const allPaths = new Set([...Object.keys(localFiles), ...Object.keys(remoteFiles)]);
      const result: FileDiff[] = [];

      for (const path of [...allPaths].sort()) {
        const local = localFiles[path] ?? null;
        const remote = remoteFiles[path] ?? null;
        let status: FileDiff['status'];
        if (local && !remote) status = 'added';
        else if (!local && remote) status = 'removed';
        else if (local !== remote) status = 'modified';
        else status = 'unchanged';
        result.push({ path, localContent: local, remoteContent: remote, status });
      }

      diffs = result;
      // Auto-select first changed file
      selectedFile = result.find(d => d.status !== 'unchanged') ?? result[0] ?? null;
    } catch (e: any) {
      loadError = String(e.message ?? e);
    } finally {
      isLoading = false;
    }
  });

  function formatTimestamp(value: string | null): string {
    if (!value) return '—';
    const d = new Date(value);
    if (Number.isNaN(d.getTime())) return '—';
    return d.toLocaleString(undefined, {
      month: 'short', day: 'numeric', hour: '2-digit', minute: '2-digit', hour12: false,
    });
  }

  function statusIcon(s: FileDiff['status']): string {
    switch (s) {
      case 'added': return '+';
      case 'removed': return '−';
      case 'modified': return '~';
      case 'unchanged': return '=';
    }
  }

  $: changedCount = diffs.filter(d => d.status !== 'unchanged').length;
  $: unchangedCount = diffs.filter(d => d.status === 'unchanged').length;

  function doAction(action: 'push' | 'pull') {
    confirmAction = null;
    dispatch(action);
  }
</script>

<!-- svelte-ignore a11y-click-events-have-key-events a11y-no-static-element-interactions -->
<div class="modal-backdrop" on:click={() => dispatch('close')}>
  <div class="modal" on:click|stopPropagation role="dialog" aria-modal="true">
    <!-- Header -->
    <div class="modal-header">
      <div class="header-info">
        <h3 class="modal-title">{project.name}</h3>
        <span class="state-badge" class:in-sync={state === 'in-sync'} class:local-ahead={state === 'local-ahead'} class:remote-ahead={state === 'remote-ahead'} class:diverged={state === 'diverged'}>
          {syncStateLabel(state)}
        </span>
      </div>
      <button class="close-btn" on:click={() => dispatch('close')}>
        <svg viewBox="0 0 24 24" width="16" height="16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="18" y1="6" x2="6" y2="18"/><line x1="6" y1="6" x2="18" y2="18"/></svg>
      </button>
    </div>

    <!-- Timestamps -->
    <div class="ts-row">
      <div class="ts-item">
        <svg viewBox="0 0 24 24" width="12" height="12" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="2" y="3" width="20" height="14" rx="2"/><line x1="8" y1="21" x2="16" y2="21"/><line x1="12" y1="17" x2="12" y2="21"/></svg>
        <span class="ts-label">Local sync:</span>
        <span class="ts-value">{formatTimestamp(project.pushedAt)}</span>
      </div>
      <div class="ts-item">
        <svg viewBox="0 0 24 24" width="12" height="12" fill="currentColor"><path d="M12 0C5.37 0 0 5.37 0 12c0 5.31 3.435 9.795 8.205 11.385.6.105.825-.255.825-.57 0-.285-.015-1.23-.015-2.235-3.015.555-3.795-.735-4.035-1.41-.135-.345-.72-1.41-1.23-1.695-.42-.225-1.02-.78-.015-.795.945-.015 1.62.87 1.845 1.23 1.08 1.815 2.805 1.305 3.495.99.105-.78.42-1.305.765-1.605-2.67-.3-5.46-1.335-5.46-5.925 0-1.305.465-2.385 1.23-3.225-.12-.3-.54-1.53.12-3.18 0 0 1.005-.315 3.3 1.23.96-.27 1.98-.405 3-.405s2.04.135 3 .405c2.295-1.56 3.3-1.23 3.3-1.23.66 1.65.24 2.88.12 3.18.765.84 1.23 1.905 1.23 3.225 0 4.605-2.805 5.625-5.475 5.925.435.375.81 1.095.81 2.22 0 1.605-.015 2.895-.015 3.3 0 .315.225.69.825.57A12.02 12.02 0 0024 12c0-6.63-5.37-12-12-12z"/></svg>
        <span class="ts-label">Remote updated:</span>
        <span class="ts-value">{formatTimestamp(project.remoteUpdatedAt)}</span>
      </div>
    </div>

    <!-- Content -->
    {#if isLoading}
      <div class="loading">
        <div class="spinner"></div>
        <span>Loading files…</span>
      </div>
    {:else if loadError}
      <div class="load-error">{loadError}</div>
    {:else}
      <!-- Summary -->
      <div class="diff-summary">
        {#if changedCount === 0}
          <span class="summary-ok">All files identical</span>
        {:else}
          <span class="summary-changed">{changedCount} file{changedCount > 1 ? 's' : ''} changed</span>
          {#if unchangedCount > 0}
            <span class="summary-sep">·</span>
            <span class="summary-unchanged">{unchangedCount} unchanged</span>
          {/if}
        {/if}
      </div>

      <div class="diff-body">
        <!-- File list sidebar -->
        <div class="file-list">
          {#each diffs.filter(d => d.status !== 'unchanged') as d}
            <button
              class="file-item"
              class:active={selectedFile === d}
              class:added={d.status === 'added'}
              class:removed={d.status === 'removed'}
              class:modified={d.status === 'modified'}
              on:click={() => (selectedFile = d)}
            >
              <span class="file-status">{statusIcon(d.status)}</span>
              <span class="file-name">{d.path}</span>
            </button>
          {/each}
          {#if unchangedCount > 0}
            <div class="unchanged-divider">
              <span>{unchangedCount} unchanged file{unchangedCount > 1 ? 's' : ''}</span>
            </div>
            {#each diffs.filter(d => d.status === 'unchanged') as d}
              <button
                class="file-item unchanged"
                class:active={selectedFile === d}
                on:click={() => (selectedFile = d)}
              >
                <span class="file-status">{statusIcon(d.status)}</span>
                <span class="file-name">{d.path}</span>
              </button>
            {/each}
          {/if}
        </div>

        <!-- Diff view -->
        <div class="diff-view">
          {#if selectedFile}
            <div class="diff-header">
              <span class="diff-filename">{selectedFile.path}</span>
              <span class="diff-status-tag" class:added={selectedFile.status === 'added'} class:removed={selectedFile.status === 'removed'} class:modified={selectedFile.status === 'modified'} class:unchanged={selectedFile.status === 'unchanged'}>
                {selectedFile.status}
              </span>
            </div>
            <div class="diff-panes">
              <div class="diff-pane local">
                <div class="pane-label">Local</div>
                <pre class="pane-content">{selectedFile.localContent ?? '(not present)'}</pre>
              </div>
              <div class="diff-pane remote">
                <div class="pane-label">Remote</div>
                <pre class="pane-content">{selectedFile.remoteContent ?? '(not present)'}</pre>
              </div>
            </div>
          {:else}
            <div class="diff-empty">Select a file to view differences</div>
          {/if}
        </div>
      </div>
    {/if}

    <!-- Actions -->
    <div class="modal-actions">
      {#if confirmAction}
        <div class="confirm-bar">
          <span class="confirm-msg">
            {#if confirmAction === 'push'}
              Overwrite remote with local content?
            {:else}
              Overwrite local with remote content?
            {/if}
          </span>
          <button class="confirm-yes" on:click={() => doAction(confirmAction)} disabled={busy}>
            {busy ? 'Working…' : 'Confirm'}
          </button>
          <button class="confirm-no" on:click={() => (confirmAction = null)} disabled={busy}>Cancel</button>
        </div>
      {:else}
        {#if project.localPath}
          <button class="action-btn push" on:click={() => (confirmAction = 'push')} disabled={busy || state === 'remote-ahead'} title={state === 'remote-ahead' ? 'Local has no changes to push' : 'Push local → remote'}>
            <svg viewBox="0 0 24 24" width="13" height="13" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="19" x2="12" y2="5"/><polyline points="5 12 12 5 19 12"/></svg>
            Push to Remote
          </button>
        {/if}
        {#if project.remote}
          <button class="action-btn pull" on:click={() => (confirmAction = 'pull')} disabled={busy || state === 'local-ahead'} title={state === 'local-ahead' ? 'Remote has no changes to pull' : 'Pull remote → local'}>
            <svg viewBox="0 0 24 24" width="13" height="13" fill="none" stroke="currentColor" stroke-width="2.2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="5" x2="12" y2="19"/><polyline points="19 12 12 19 5 12"/></svg>
            Pull to Local
          </button>
        {/if}
        <span class="action-spacer"></span>
        <button class="action-btn close-action" on:click={() => dispatch('close')}>Close</button>
      {/if}
    </div>
  </div>
</div>

<style>
  .modal-backdrop {
    position: fixed; inset: 0; z-index: 200;
    background: rgba(0, 0, 0, 0.7);
    backdrop-filter: blur(4px);
    display: flex; align-items: center; justify-content: center;
    padding: 24px;
    animation: fade-in 0.12s ease;
  }
  @keyframes fade-in { from { opacity: 0; } to { opacity: 1; } }

  .modal {
    background: #1e1e2e;
    border: 1px solid #313244;
    border-radius: 14px;
    box-shadow: 0 24px 64px rgba(0, 0, 0, 0.6);
    width: 100%; max-width: 820px; max-height: 85vh;
    display: flex; flex-direction: column;
    animation: slide-up 0.15s ease;
    overflow: hidden;
  }
  @keyframes slide-up { from { transform: translateY(12px); opacity: 0; } to { transform: none; opacity: 1; } }

  /* ── Header ── */
  .modal-header {
    display: flex; align-items: center; justify-content: space-between;
    padding: 16px 20px 12px;
    border-bottom: 1px solid #252535;
  }
  .header-info { display: flex; align-items: center; gap: 10px; }
  .modal-title { margin: 0; font-size: 15px; font-weight: 700; color: #cdd6f4; }
  .state-badge {
    font-size: 11px; font-weight: 700; padding: 3px 8px; border-radius: 4px;
    letter-spacing: 0.02em;
  }
  .state-badge.in-sync { background: rgba(166,227,161,0.12); color: #a6e3a1; border: 1px solid rgba(166,227,161,0.3); }
  .state-badge.local-ahead { background: rgba(249,226,175,0.12); color: #f9e2af; border: 1px solid rgba(249,226,175,0.3); }
  .state-badge.remote-ahead { background: rgba(137,180,250,0.12); color: #89b4fa; border: 1px solid rgba(137,180,250,0.3); }
  .state-badge.diverged { background: rgba(243,139,168,0.12); color: #f38ba8; border: 1px solid rgba(243,139,168,0.3); }
  .close-btn {
    border: none; background: none; color: #585b70; cursor: pointer;
    padding: 4px; border-radius: 4px; display: flex; transition: all 0.1s;
  }
  .close-btn:hover { color: #cdd6f4; background: #252535; }

  /* ── Timestamps ── */
  .ts-row {
    display: flex; gap: 24px; padding: 10px 20px;
    background: #181825; border-bottom: 1px solid #252535;
  }
  .ts-item {
    display: flex; align-items: center; gap: 6px;
    font-size: 11px; color: #7f849c;
  }
  .ts-item svg { flex-shrink: 0; }
  .ts-label { color: #585b70; }
  .ts-value { color: #a6adc8; font-weight: 500; }

  /* ── Loading / Error ── */
  .loading {
    display: flex; align-items: center; justify-content: center; gap: 10px;
    padding: 48px 20px; color: #585b70; font-size: 13px;
  }
  .spinner {
    width: 18px; height: 18px; border: 2px solid #252535;
    border-top-color: #89b4fa; border-radius: 50%;
    animation: spin 0.7s linear infinite;
  }
  @keyframes spin { to { transform: rotate(360deg); } }
  .load-error {
    padding: 20px; color: #f38ba8; font-size: 12px;
    background: rgba(243,139,168,0.06); margin: 12px 20px; border-radius: 8px;
    border: 1px solid rgba(243,139,168,0.15);
  }

  /* ── Diff summary ── */
  .diff-summary {
    display: flex; align-items: center; gap: 6px;
    padding: 8px 20px; font-size: 11px; color: #585b70;
  }
  .summary-changed { color: #f9e2af; font-weight: 600; }
  .summary-ok { color: #a6e3a1; font-weight: 600; }
  .summary-sep { color: #313244; }
  .summary-unchanged { color: #45475a; }

  /* ── Diff body ── */
  .diff-body {
    display: flex; flex: 1; min-height: 0; overflow: hidden;
    border-top: 1px solid #252535;
  }

  /* ── File list ── */
  .file-list {
    width: 200px; flex-shrink: 0; overflow-y: auto;
    border-right: 1px solid #252535;
    background: #181825;
  }
  .file-list::-webkit-scrollbar { width: 3px; }
  .file-list::-webkit-scrollbar-thumb { background: #252535; border-radius: 2px; }
  .file-item {
    display: flex; align-items: center; gap: 6px; width: 100%;
    padding: 6px 12px; border: none; background: none;
    font-size: 11px; font-family: inherit; cursor: pointer;
    text-align: left; color: #a6adc8; transition: background 0.08s;
  }
  .file-item:hover { background: #1e1e2e; }
  .file-item.active { background: #252535; color: #cdd6f4; }
  .file-item.unchanged { color: #45475a; }
  .file-status {
    width: 14px; height: 14px; font-size: 11px; font-weight: 800;
    display: flex; align-items: center; justify-content: center;
    border-radius: 3px; flex-shrink: 0;
  }
  .file-item.added .file-status { color: #a6e3a1; background: rgba(166,227,161,0.12); }
  .file-item.removed .file-status { color: #f38ba8; background: rgba(243,139,168,0.12); }
  .file-item.modified .file-status { color: #f9e2af; background: rgba(249,226,175,0.12); }
  .file-name {
    overflow: hidden; text-overflow: ellipsis; white-space: nowrap; min-width: 0;
  }
  .unchanged-divider {
    padding: 6px 12px; font-size: 10px; color: #313244;
    border-top: 1px solid #252535; margin-top: 4px;
  }

  /* ── Diff view ── */
  .diff-view { flex: 1; display: flex; flex-direction: column; min-width: 0; overflow: hidden; }
  .diff-header {
    display: flex; align-items: center; gap: 8px;
    padding: 8px 16px; background: #181825;
    border-bottom: 1px solid #252535;
  }
  .diff-filename {
    font-size: 12px; font-weight: 600; color: #cdd6f4;
    font-family: 'JetBrains Mono', monospace;
  }
  .diff-status-tag {
    font-size: 10px; font-weight: 700; padding: 2px 6px;
    border-radius: 3px; text-transform: uppercase; letter-spacing: 0.04em;
  }
  .diff-status-tag.added { background: rgba(166,227,161,0.12); color: #a6e3a1; }
  .diff-status-tag.removed { background: rgba(243,139,168,0.12); color: #f38ba8; }
  .diff-status-tag.modified { background: rgba(249,226,175,0.12); color: #f9e2af; }
  .diff-status-tag.unchanged { background: rgba(88,91,112,0.12); color: #585b70; }

  .diff-panes { display: flex; flex: 1; min-height: 0; overflow: hidden; }
  .diff-pane {
    flex: 1; display: flex; flex-direction: column; overflow: hidden;
    min-width: 0;
  }
  .diff-pane.local { border-right: 1px solid #252535; }
  .pane-label {
    font-size: 10px; font-weight: 700; color: #585b70;
    padding: 4px 12px; background: #181825;
    border-bottom: 1px solid #252535; text-transform: uppercase;
    letter-spacing: 0.06em;
  }
  .diff-pane.local .pane-label { color: #f9e2af; }
  .diff-pane.remote .pane-label { color: #89b4fa; }
  .pane-content {
    flex: 1; overflow: auto; margin: 0;
    padding: 10px 12px; font-size: 12px; line-height: 1.6;
    font-family: 'JetBrains Mono', monospace;
    color: #a6adc8; background: #11111b;
    white-space: pre-wrap; word-break: break-word;
    tab-size: 4;
  }
  .pane-content::-webkit-scrollbar { width: 4px; }
  .pane-content::-webkit-scrollbar-thumb { background: #252535; border-radius: 2px; }

  .diff-empty {
    flex: 1; display: flex; align-items: center; justify-content: center;
    color: #45475a; font-size: 13px;
  }

  /* ── Actions ── */
  .modal-actions {
    display: flex; align-items: center; gap: 8px;
    padding: 12px 20px;
    border-top: 1px solid #252535;
    background: #181825;
  }
  .action-spacer { flex: 1; }
  .action-btn {
    display: flex; align-items: center; gap: 6px;
    padding: 7px 14px; border-radius: 7px;
    font-size: 12px; font-weight: 600; font-family: inherit;
    cursor: pointer; border: 1px solid; transition: all 0.12s;
  }
  .action-btn:disabled { opacity: 0.35; cursor: not-allowed; }
  .action-btn.push {
    background: rgba(249,226,175,0.08); color: #f9e2af;
    border-color: rgba(249,226,175,0.3);
  }
  .action-btn.push:hover:not(:disabled) { background: rgba(249,226,175,0.18); border-color: #f9e2af; }
  .action-btn.pull {
    background: rgba(137,180,250,0.08); color: #89b4fa;
    border-color: rgba(137,180,250,0.3);
  }
  .action-btn.pull:hover:not(:disabled) { background: rgba(137,180,250,0.18); border-color: #89b4fa; }
  .action-btn.close-action {
    background: none; color: #585b70; border-color: #313244;
  }
  .action-btn.close-action:hover { color: #a6adc8; background: #252535; }

  /* ── Confirm bar ── */
  .confirm-bar {
    display: flex; align-items: center; gap: 10px; width: 100%;
  }
  .confirm-msg {
    font-size: 12px; color: #f9e2af; font-weight: 500; flex: 1;
  }
  .confirm-yes {
    padding: 7px 16px; border-radius: 7px; border: none;
    background: #f38ba8; color: #11111b;
    font-size: 12px; font-weight: 700; font-family: inherit;
    cursor: pointer; transition: opacity 0.1s;
  }
  .confirm-yes:hover:not(:disabled) { opacity: 0.88; }
  .confirm-yes:disabled { opacity: 0.4; cursor: not-allowed; }
  .confirm-no {
    padding: 7px 12px; border-radius: 7px;
    border: 1px solid #313244; background: none; color: #585b70;
    font-size: 12px; font-weight: 600; font-family: inherit;
    cursor: pointer; transition: all 0.1s;
  }
  .confirm-no:hover { color: #a6adc8; background: #252535; }
  .confirm-no:disabled { opacity: 0.4; cursor: not-allowed; }
</style>
