<script lang="ts">
  import { editor, editorSetCode } from '../stores/editor';
  import { createEventDispatcher } from 'svelte';
  import { session } from '../stores/session';

  const dispatch = createEventDispatcher<{ change: string }>();

  $: code = $editor.code;
  $: activeFile = $editor.activeFilePath;
  $: projectMode = $session.projectMode;
  $: entryName = $session.entryPath ? ($session.entryPath.split('/').pop() ?? '') : '';
  $: emptyTitle = projectMode === 'single-file'
    ? (entryName ? `Single-file session: ${entryName}` : 'Single-file session')
    : projectMode === 'module'
      ? 'Select a file to edit'
      : 'Open a session to start editing';
  $: emptyDescription = projectMode === 'single-file'
    ? 'This workspace is focused on one entry file. Run it directly from the toolbar.'
    : projectMode === 'module'
      ? 'Choose a file from Explorer to open it in the editor.'
      : 'Open a local path, remote URL, or project from Home.';

  function onInput(e: Event) {
    const val = (e.target as HTMLTextAreaElement).value;
    editorSetCode(val);
    dispatch('change', val);
  }

  function onKeyDown(e: KeyboardEvent) {
    if (e.key === 'Tab') {
      e.preventDefault();
      const ta = e.target as HTMLTextAreaElement;
      const start = ta.selectionStart;
      const end = ta.selectionEnd;
      const val = ta.value.substring(0, start) + '    ' + ta.value.substring(end);
      ta.value = val;
      ta.selectionStart = ta.selectionEnd = start + 4;
      editorSetCode(val);
    }
  }
</script>

<div class="editor">
  {#if !activeFile}
    <div class="empty-editor">
      <div class="empty-card">
        <div class="empty-title">{emptyTitle}</div>
        <div class="empty-description">{emptyDescription}</div>
      </div>
    </div>
  {:else}
    <textarea
      class="code-area"
      value={code}
      on:input={onInput}
      on:keydown={onKeyDown}
      spellcheck={false}
      autocorrect="off"
      autocapitalize="off"
    ></textarea>
  {/if}
</div>

<style>
  .editor {
    flex: 1;
    display: flex;
    flex-direction: column;
    min-height: 0;
    background: #1e1e2e;
    overflow: hidden;
  }
  .code-area {
    flex: 1;
    width: 100%;
    height: 100%;
    background: #1e1e2e;
    color: #cdd6f4;
    border: none;
    outline: none;
    resize: none;
    padding: 14px 16px;
    font-family: 'JetBrains Mono', 'Fira Mono', 'Cascadia Code', monospace;
    font-size: 13px;
    line-height: 1.6;
    tab-size: 4;
    white-space: pre;
  }
  .empty-editor {
    flex: 1;
    display: flex;
    align-items: center;
    justify-content: center;
    padding: 24px;
  }
  .empty-card {
    width: min(100%, 440px);
    padding: 20px 22px;
    border-radius: 16px;
    border: 1px solid #313244;
    background: rgba(24, 24, 37, 0.72);
  }
  .empty-title {
    color: #cdd6f4;
    font-size: 16px;
    font-weight: 700;
  }
  .empty-description {
    margin-top: 8px;
    color: #6c7086;
    font-size: 13px;
    line-height: 1.6;
  }
</style>
