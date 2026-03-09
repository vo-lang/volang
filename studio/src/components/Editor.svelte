<script lang="ts">
  import { onMount, createEventDispatcher } from 'svelte';
  import { actions } from '../lib/actions';
  import { registerVoLanguage } from '../lib/monaco_vo';

  function langForFile(path: string): string {
    const ext = path.split('.').pop()?.toLowerCase() ?? '';
    if (ext === 'vo') return 'vo';
    if (ext === 'ts' || ext === 'tsx') return 'typescript';
    if (ext === 'js' || ext === 'jsx') return 'javascript';
    if (ext === 'json') return 'json';
    if (ext === 'rs') return 'rust';
    if (ext === 'toml') return 'ini';
    if (ext === 'md') return 'markdown';
    return 'go';
  }

  export let value: string = '';
  export let filePath: string = '';

  const dispatch = createEventDispatcher<{ change: string }>();

  let container: HTMLDivElement;
  let monaco: typeof import('monaco-editor') | null = null;
  let editor: import('monaco-editor').editor.IStandaloneCodeEditor | null = null;
  let suppressUpdate = false;

  onMount(() => {
    let disposed = false;

    void (async () => {
      const monacoModule = await import('monaco-editor');
      if (disposed) return;

      monaco = monacoModule;
      registerVoLanguage(monacoModule);

      const instance = monacoModule.editor.create(container, {
        value,
        language: langForFile(filePath),
        theme: 'vs-dark',
        fontSize: 13,
        fontFamily: "'JetBrains Mono', 'Fira Code', 'Cascadia Code', 'Consolas', monospace",
        fontLigatures: true,
        minimap: { enabled: false },
        scrollBeyondLastLine: false,
        renderWhitespace: 'selection',
        tabSize: 4,
        insertSpaces: false,
        automaticLayout: true,
        lineNumbers: 'on',
        folding: true,
        bracketPairColorization: { enabled: true },
        smoothScrolling: true,
        cursorBlinking: 'smooth',
        renderLineHighlight: 'line',
        scrollbar: {
          verticalScrollbarSize: 8,
          horizontalScrollbarSize: 8,
        },
      });
      if (disposed) {
        instance.dispose();
        return;
      }

      editor = instance;

      instance.onDidChangeModelContent(() => {
        if (!suppressUpdate) {
          dispatch('change', instance.getValue());
        }
      });

      instance.addCommand(monacoModule.KeyMod.CtrlCmd | monacoModule.KeyCode.KeyS, () => {
        actions.saveFile();
      });
    })();

    return () => {
      disposed = true;
      editor?.dispose();
      editor = null;
      monaco = null;
    };
  });

  $: if (editor) {
    const current = editor.getValue();
    if (current !== value) {
      suppressUpdate = true;
      editor.setValue(value);
      suppressUpdate = false;
    }
  }

  $: if (editor && monaco && filePath) {
    const model = editor.getModel();
    if (model) monaco.editor.setModelLanguage(model, langForFile(filePath));
  }
</script>

<div bind:this={container} class="editor-container"></div>

<style>
  .editor-container {
    flex: 1;
    overflow: hidden;
    min-width: 0;
  }
</style>
