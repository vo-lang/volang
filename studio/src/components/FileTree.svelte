<script lang="ts">
  import { tick } from 'svelte';
  import { ide } from '../stores/ide';
  import { editTargetLabel } from '../stores/ide';

  import { explorer } from '../stores/explorer';
  import { actions } from '../lib/actions';
  import type { FsEntry } from '../lib/bridge';

  // ── Types ──────────────────────────────────────────────────────────────────
  type TreeRow =
    | { kind: 'entry'; entry: FsEntry; depth: number }
    | { kind: 'input'; depth: number; dirPath: string };

  // ── Inline input element ref ───────────────────────────────────────────────
  let inputRef: HTMLInputElement | undefined;

  $: if ($explorer.inlineInput) {
    tick().then(() => { inputRef?.focus(); inputRef?.select(); });
  }

  // ── Single-file: flat root entries (no dirs) ───────────────────────────────
  $: singleFiles = ($ide.dirCache[$ide.workspaceRoot] ?? []).filter(e => !e.isDir);

  // ── Multi-file: full tree ──────────────────────────────────────────────────
  $: flatRows = computeFlatList(
    $ide.dirCache,
    $ide.expandedDirs,
    $explorer.inlineInput,
    $ide.workspaceRoot,
    0,
  );

  function computeFlatList(
    cache: Record<string, FsEntry[]>,
    expandedDirs: string[],
    inlineInput: { mode: string; targetPath?: string; dirPath: string } | null,
    dirPath: string,
    depth: number,
  ): TreeRow[] {
    const rows: TreeRow[] = [];
    for (const entry of (cache[dirPath] ?? [])) {
      if (inlineInput?.mode === 'rename' && inlineInput.targetPath === entry.path) {
        rows.push({ kind: 'input', depth, dirPath: entry.path.substring(0, entry.path.lastIndexOf('/')) });
      } else {
        rows.push({ kind: 'entry', entry, depth });
        if (entry.isDir && expandedDirs.includes(entry.path)) {
          rows.push(...computeFlatList(cache, expandedDirs, inlineInput, entry.path, depth + 1));
        }
      }
    }
    if (inlineInput && inlineInput.mode !== 'rename' && inlineInput.dirPath === dirPath) {
      rows.push({ kind: 'input', depth, dirPath });
    }
    return rows;
  }

  // ── Event handlers ─────────────────────────────────────────────────────────
  function handleEntryClick(entry: FsEntry) {
    if (entry.isDir) actions.toggleDir(entry.path);
    else actions.openFile(entry.path);
  }

  function handleContextMenu(evt: MouseEvent, entry: FsEntry) {
    evt.preventDefault();
    explorer.update(e => ({
      ...e,
      contextMenu: { x: evt.clientX, y: evt.clientY, path: entry.path, name: entry.name, isDir: entry.isDir },
    }));
  }

  function handleInputKeydown(evt: KeyboardEvent) {
    if (evt.key === 'Enter') {
      actions.commitInlineInput((evt.target as HTMLInputElement).value);
    } else if (evt.key === 'Escape') {
      actions.cancelInlineInput();
    }
  }

  // ── File extension → icon class ────────────────────────────────────────────
  function fileIconClass(name: string): string {
    const dot = name.lastIndexOf('.');
    if (dot < 0) return 'ext-other';
    const ext = name.slice(dot + 1).toLowerCase();
    const known: Record<string, string> = {
      vo: 'ext-vo', md: 'ext-md', txt: 'ext-txt',
      json: 'ext-cfg', toml: 'ext-cfg', yaml: 'ext-cfg', yml: 'ext-cfg',
      sh: 'ext-sh', bash: 'ext-sh',
    };
    return known[ext] ?? 'ext-other';
  }

  // ── Inline input display values ────────────────────────────────────────────
  $: inputPlaceholder = $explorer.inlineInput?.mode === 'create-dir'
    ? 'folder name'
    : 'filename.vo';
  $: inputInitialValue = $explorer.inlineInput?.initialValue ?? '';
</script>

<div class="filetree">
  {#if $ide.projectMode === 'single'}
    <!-- ══════════════════════════════════════════════════════════════════
         SINGLE-FILE MODE  —  flat list of root files + convert button
         ══════════════════════════════════════════════════════════════════ -->
    <div class="tree-header">
      <span class="tree-label">{$ide.editTarget ? editTargetLabel($ide.editTarget) : 'Single File'}</span>
      <div class="toolbar">
        <button class="icon-btn" title="New File" on:click={() => actions.startCreate($ide.workspaceRoot, false)}>+F</button>
        <button class="icon-btn" title="Refresh" on:click={() => actions.loadDir($ide.workspaceRoot)}>↺</button>
      </div>
    </div>

    <div class="tree-scroll">
      {#each singleFiles as entry (entry.path)}
        {#if $explorer.inlineInput?.mode === 'rename' && $explorer.inlineInput.targetPath === entry.path}
          <div class="file-entry" style="padding-left:10px">
            <span class="file-icon input-dot"></span>
            <input
              bind:this={inputRef}
              class="inline-input"
              value={inputInitialValue}
              placeholder={entry.name}
              spellcheck="false"
              on:keydown={handleInputKeydown}
              on:blur={() => actions.cancelInlineInput()}
            />
          </div>
        {:else}
          <button
            class="file-entry"
            class:active={entry.path === $ide.activeFilePath}
            style="padding-left:10px"
            on:click={() => actions.openFile(entry.path)}
            on:contextmenu={(e) => handleContextMenu(e, entry)}
          >
            <span class="file-icon {fileIconClass(entry.name)}"></span>
            <span class="file-name">{entry.name}</span>
            {#if entry.path === $ide.activeFilePath && $ide.dirty}
              <span class="dirty-dot" title="Unsaved changes">●</span>
            {/if}
          </button>
        {/if}
      {/each}

      {#if $explorer.inlineInput && $explorer.inlineInput.mode !== 'rename' && $explorer.inlineInput.dirPath === $ide.workspaceRoot}
        <div class="file-entry" style="padding-left:10px">
          <span class="file-icon input-dot"></span>
          <input
            bind:this={inputRef}
            class="inline-input"
            value=""
            placeholder={inputPlaceholder}
            spellcheck="false"
            on:keydown={handleInputKeydown}
            on:blur={() => actions.cancelInlineInput()}
          />
        </div>
      {/if}
    </div>

    <!-- Convert to multi-file project -->
    <div class="convert-banner">
        <p class="convert-hint">Add <code>vo.mod</code> to enable multi-file projects</p>
        <button class="convert-btn" on:click={() => actions.convertToMultiProject()}>
          Convert to Project
        </button>
    </div>

  {:else}
    <!-- ══════════════════════════════════════════════════════════════════
         MULTI-FILE MODE  —  full project tree
         ══════════════════════════════════════════════════════════════════ -->
    <div class="tree-header">
      <span class="tree-label">{$ide.editTarget ? editTargetLabel($ide.editTarget) : ($ide.workspaceRoot.split('/').pop() ?? 'Project')}</span>
      <div class="toolbar">
        <button class="icon-btn" title="New File" on:click={() => actions.startCreate($ide.workspaceRoot, false)}>+F</button>
        <button class="icon-btn" title="New Folder" on:click={() => actions.startCreate($ide.workspaceRoot, true)}>+D</button>
        <button class="icon-btn" title="Refresh" on:click={() => actions.loadDir($ide.workspaceRoot)}>↺</button>
      </div>
    </div>

    <div class="tree-scroll">
      {#each flatRows as row (row.kind === 'entry' ? row.entry.path : 'inline-input')}
        {#if row.kind === 'input'}
          <div class="file-entry" style="padding-left: {10 + row.depth * 16}px">
            <span class="file-icon input-dot"></span>
            <input
              bind:this={inputRef}
              class="inline-input"
              value={inputInitialValue}
              placeholder={inputPlaceholder}
              spellcheck="false"
              on:keydown={handleInputKeydown}
              on:blur={() => actions.cancelInlineInput()}
            />
          </div>
        {:else}
          <button
            class="file-entry"
            class:active={row.entry.path === $ide.activeFilePath}
            class:is-dir={row.entry.isDir}
            style="padding-left: {10 + row.depth * 16}px"
            on:click={() => handleEntryClick(row.entry)}
            on:contextmenu={(e) => handleContextMenu(e, row.entry)}
          >
            {#if row.entry.isDir}
              <span class="file-icon dir-icon">
                {$ide.expandedDirs.includes(row.entry.path) ? '▾' : '▸'}
              </span>
            {:else}
              <span class="file-icon {fileIconClass(row.entry.name)}"></span>
            {/if}
            <span class="file-name">{row.entry.name}</span>
            {#if row.entry.path === $ide.activeFilePath && $ide.dirty}
              <span class="dirty-dot" title="Unsaved changes">●</span>
            {/if}
          </button>
        {/if}
      {/each}
    </div>
  {/if}
</div>

<style>
  .filetree {
    width: 240px;
    min-width: 180px;
    background: #181825;
    border-right: 1px solid #313244;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    flex-shrink: 0;
    user-select: none;
  }

  /* ── Header ── */
  .tree-header {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 5px 6px 5px 12px;
    border-bottom: 1px solid #252535;
    flex-shrink: 0;
  }

  .tree-label {
    font-size: 11px;
    font-weight: 700;
    color: #45475a;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .toolbar {
    display: flex;
    gap: 1px;
    flex-shrink: 0;
  }

  .icon-btn {
    border: none;
    background: none;
    color: #585b70;
    font-size: 13px;
    padding: 3px 5px;
    border-radius: 4px;
    cursor: pointer;
    line-height: 1;
    font-family: inherit;
  }

  .icon-btn:hover { background: #313244; color: #cdd6f4; }
  .icon-btn:disabled { opacity: 0.3; cursor: not-allowed; }

  /* ── Tree scroll ── */
  .tree-scroll {
    flex: 1;
    overflow-y: auto;
    overflow-x: hidden;
    padding-bottom: 8px;
  }

  .tree-scroll::-webkit-scrollbar { width: 4px; }
  .tree-scroll::-webkit-scrollbar-track { background: transparent; }
  .tree-scroll::-webkit-scrollbar-thumb { background: #313244; border-radius: 2px; }

  /* ── File entries ── */
  .file-entry {
    display: flex;
    align-items: center;
    width: 100%;
    min-height: 22px;
    padding-top: 1px;
    padding-bottom: 1px;
    padding-right: 8px;
    border: none;
    background: none;
    text-align: left;
    cursor: pointer;
    color: #a6adc8;
    font-size: 13px;
    gap: 5px;
    white-space: nowrap;
    overflow: hidden;
    font-family: inherit;
  }

  .file-entry:hover { background: #252535; color: #cdd6f4; }
  .file-entry.active { background: #2a2a3e; color: #cdd6f4; }
  .file-entry.is-dir { color: #89b4fa; font-weight: 500; }

  /* ── File icons ── */
  .file-icon {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 14px;
    flex-shrink: 0;
  }

  .dir-icon {
    font-size: 10px;
    color: #89b4fa;
  }

  .ext-vo::before, .ext-md::before, .ext-cfg::before, .ext-sh::before,
  .ext-txt::before, .ext-other::before, .input-dot::before {
    content: '';
    display: block;
    width: 7px;
    height: 7px;
    border-radius: 50%;
  }

  .ext-vo::before    { background: #89b4fa; }
  .ext-md::before    { background: #a6e3a1; }
  .ext-cfg::before   { background: #f9e2af; }
  .ext-sh::before    { background: #a6e3a1; }
  .ext-txt::before   { background: #6c7086; }
  .ext-other::before { background: #45475a; }
  .input-dot::before { background: #585b70; }

  .file-name {
    flex: 1;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .dirty-dot {
    color: #f9e2af;
    font-size: 9px;
    flex-shrink: 0;
    margin-left: 2px;
  }

  /* ── Inline input ── */
  .inline-input {
    flex: 1;
    background: #252535;
    border: 1px solid #89b4fa;
    border-radius: 3px;
    color: #cdd6f4;
    font-size: 13px;
    padding: 1px 5px;
    outline: none;
    min-width: 0;
    font-family: inherit;
  }

  /* ── Convert banner (single-file mode) ── */
  .convert-banner {
    border-top: 1px solid #252535;
    padding: 10px 12px;
    display: flex;
    flex-direction: column;
    gap: 7px;
    flex-shrink: 0;
  }

  .convert-hint {
    font-size: 11px;
    color: #45475a;
    line-height: 1.4;
  }

  .convert-hint code {
    color: #f9e2af;
    font-family: 'JetBrains Mono', 'Fira Code', monospace;
    font-size: 11px;
  }

  .convert-btn {
    border: 1px solid #313244;
    background: none;
    color: #89b4fa;
    font-size: 12px;
    padding: 4px 10px;
    border-radius: 5px;
    cursor: pointer;
    text-align: center;
    font-family: inherit;
  }

  .convert-btn:hover { background: #1e2a4a; border-color: #4a6aaa; }
</style>
