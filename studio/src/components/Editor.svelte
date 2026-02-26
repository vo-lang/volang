<script lang="ts">
  import { onMount, onDestroy, createEventDispatcher } from 'svelte';
  import * as monaco from 'monaco-editor';
  import { actions } from '../lib/actions';

  export let value: string = '';

  const dispatch = createEventDispatcher<{ change: string }>();

  let container: HTMLDivElement;
  let editor: monaco.editor.IStandaloneCodeEditor;
  let suppressUpdate = false;

  onMount(() => {
    editor = monaco.editor.create(container, {
      value,
      language: 'go',
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

    editor.onDidChangeModelContent(() => {
      if (!suppressUpdate) {
        dispatch('change', editor.getValue());
      }
    });

    // Ctrl+S / Cmd+S to save
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyS, () => {
      actions.saveFile();
    });
  });

  $: if (editor) {
    const current = editor.getValue();
    if (current !== value) {
      suppressUpdate = true;
      editor.setValue(value);
      suppressUpdate = false;
    }
  }

  onDestroy(() => editor?.dispose());
</script>

<div bind:this={container} class="editor-container"></div>

<style>
  .editor-container {
    flex: 1;
    overflow: hidden;
    min-width: 0;
  }
</style>
