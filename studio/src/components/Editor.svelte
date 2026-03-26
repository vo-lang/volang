<script lang="ts">
  import { createEventDispatcher, onDestroy, onMount, tick } from 'svelte';
  import type { IDisposable, editor as MonacoEditorApi } from 'monaco-editor';
  import { editor, editorSetCode } from '../stores/editor';
  import { session } from '../stores/session';
  import { loadMonaco, resolveMonacoLanguage } from '../lib/editor/monaco';

  const dispatch = createEventDispatcher<{ change: string }>();
  type Monaco = typeof import('monaco-editor');

  let editorHost: HTMLDivElement | undefined;
  let monaco: Monaco | null = null;
  let monacoEditor: MonacoEditorApi.IStandaloneCodeEditor | null = null;
  let model: MonacoEditorApi.ITextModel | null = null;
  let modelChangeSubscription: IDisposable | null = null;
  let resizeObserver: ResizeObserver | null = null;
  let suppressStoreSync = false;
  let boundPath = '';
  let editorInitGeneration = 0;

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

  function disposeModel(): void {
    modelChangeSubscription?.dispose();
    modelChangeSubscription = null;
    model?.dispose();
    model = null;
    boundPath = '';
  }

  function bindModel(monacoModule: Monaco, filePath: string, initialCode: string): void {
    if (!monacoEditor) {
      return;
    }
    disposeModel();
    boundPath = filePath;
    model = monacoModule.editor.createModel(initialCode, resolveMonacoLanguage(filePath), monacoModule.Uri.file(filePath));
    model.updateOptions({
      insertSpaces: true,
      tabSize: 4,
      trimAutoWhitespace: true,
    });
    monacoEditor.setModel(model);
    modelChangeSubscription = model.onDidChangeContent(() => {
      if (suppressStoreSync || !model) {
        return;
      }
      const next = model.getValue();
      editorSetCode(next);
      dispatch('change', next);
    });
  }

  function syncEditorState(monacoModule: Monaco): void {
    if (!monacoEditor) {
      return;
    }
    if (!activeFile) {
      if (model) {
        monacoEditor.setModel(null);
        disposeModel();
      }
      return;
    }
    if (!model || boundPath !== activeFile) {
      bindModel(monacoModule, activeFile, code);
      void tick().then(() => monacoEditor?.layout());
      return;
    }
    monacoModule.editor.setModelLanguage(model, resolveMonacoLanguage(activeFile));
    if (model.getValue() !== code) {
      suppressStoreSync = true;
      model.setValue(code);
      suppressStoreSync = false;
    }
  }

  async function ensureEditor(): Promise<void> {
    if (!editorHost || !activeFile || monacoEditor) {
      return;
    }
    const generation = ++editorInitGeneration;
    const monacoModule = await loadMonaco();
    if (generation !== editorInitGeneration || !editorHost) {
      return;
    }
    monaco = monacoModule;
    monacoModule.editor.setTheme('vo-studio');
    monacoEditor = monacoModule.editor.create(editorHost, {
      automaticLayout: false,
      fontFamily: `'JetBrains Mono', 'Fira Mono', 'Cascadia Code', monospace`,
      fontSize: 13,
      folding: true,
      glyphMargin: false,
      lineHeight: 22,
      minimap: { enabled: false },
      padding: { top: 14, bottom: 14 },
      renderWhitespace: 'selection',
      scrollBeyondLastLine: false,
      smoothScrolling: true,
      stickyScroll: { enabled: false },
      value: '',
      wordWrap: 'off',
    });
    resizeObserver = new ResizeObserver(() => {
      monacoEditor?.layout();
    });
    resizeObserver.observe(editorHost);
    syncEditorState(monacoModule);
  }

  onMount(() => {
    void ensureEditor();
  });

  onDestroy(() => {
    editorInitGeneration++;
    resizeObserver?.disconnect();
    resizeObserver = null;
    if (monacoEditor) {
      monacoEditor.setModel(null);
    }
    disposeModel();
    monacoEditor?.dispose();
    monacoEditor = null;
    monaco = null;
  });

  $: if (activeFile && editorHost && !monacoEditor) {
    void ensureEditor();
  }

  $: if (monacoEditor && monaco) {
    syncEditorState(monaco);
  }
</script>

<div class="editor">
  <div class="code-area" class:hidden={!activeFile} bind:this={editorHost}></div>
  {#if !activeFile}
    <div class="empty-editor">
      <div class="empty-card">
        <div class="empty-title">{emptyTitle}</div>
        <div class="empty-description">{emptyDescription}</div>
      </div>
    </div>
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
    min-height: 0;
    background: #1e1e2e;
  }
  .code-area.hidden {
    display: none;
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
