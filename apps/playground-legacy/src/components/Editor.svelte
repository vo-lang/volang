<script lang="ts">
  import { onMount } from 'svelte';
  import * as monaco from 'monaco-editor';
  import editorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
  import { theme } from '../lib/theme';

  let { value = $bindable('') }: { value: string } = $props();

  let container: HTMLDivElement;
  let editor: monaco.editor.IStandaloneCodeEditor;

  // Setup Monaco workers
  self.MonacoEnvironment = {
    getWorker: function () {
      return new editorWorker();
    },
  };

  // Register Vo language (Go-like syntax)
  function registerVoLanguage() {
    // ... (keep existing registration code)
    if (monaco.languages.getLanguages().some(l => l.id === 'vo')) return;
    
    monaco.languages.register({ id: 'vo' });

    monaco.languages.setMonarchTokensProvider('vo', {
      keywords: [
        'break', 'case', 'chan', 'const', 'continue', 'default', 'defer',
        'else', 'fallthrough', 'for', 'func', 'go', 'goto', 'if', 'import',
        'interface', 'map', 'package', 'range', 'return', 'select', 'struct',
        'switch', 'type', 'var', 'nil', 'true', 'false', 'iota',
      ],
      typeKeywords: [
        'bool', 'byte', 'complex64', 'complex128', 'error', 'float32', 'float64',
        'int', 'int8', 'int16', 'int32', 'int64', 'rune', 'string',
        'uint', 'uint8', 'uint16', 'uint32', 'uint64', 'uintptr', 'any',
      ],
      operators: [
        '+', '-', '*', '/', '%', '&', '|', '^', '<<', '>>', '&^',
        '+=', '-=', '*=', '/=', '%=', '&=', '|=', '^=', '<<=', '>>=', '&^=',
        '&&', '||', '<-', '++', '--', '==', '<', '>', '=', '!',
        '!=', '<=', '>=', ':=', '...', '~>',
      ],
      symbols: /[=><!~?:&|+\-*\/\^%]+/,
      escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

      tokenizer: {
        root: [
          [/[a-zA-Z_]\w*/, {
            cases: {
              '@keywords': 'keyword',
              '@typeKeywords': 'type',
              '@default': 'identifier',
            },
          }],
          { include: '@whitespace' },
          [/[{}()\[\]]/, '@brackets'],
          [/@symbols/, {
            cases: {
              '@operators': 'operator',
              '@default': '',
            },
          }],
          [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
          [/0[xX][0-9a-fA-F]+/, 'number.hex'],
          [/\d+/, 'number'],
          [/[;,.]/, 'delimiter'],
          [/"([^"\\]|\\.)*$/, 'string.invalid'],
          [/"/, 'string', '@string'],
          [/'[^\\']'/, 'string'],
          [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
          [/'/, 'string.invalid'],
        ],
        string: [
          [/[^\\"]+/, 'string'],
          [/@escapes/, 'string.escape'],
          [/\\./, 'string.escape.invalid'],
          [/"/, 'string', '@pop'],
        ],
        whitespace: [
          [/[ \t\r\n]+/, ''],
          [/\/\*/, 'comment', '@comment'],
          [/\/\/.*$/, 'comment'],
        ],
        comment: [
          [/[^\/*]+/, 'comment'],
          [/\*\//, 'comment', '@pop'],
          [/[\/*]/, 'comment'],
        ],
      },
    });

    monaco.languages.setLanguageConfiguration('vo', {
      comments: {
        lineComment: '//',
        blockComment: ['/*', '*/'],
      },
      brackets: [
        ['{', '}'],
        ['[', ']'],
        ['(', ')'],
      ],
      autoClosingPairs: [
        { open: '{', close: '}' },
        { open: '[', close: ']' },
        { open: '(', close: ')' },
        { open: '"', close: '"' },
        { open: "'", close: "'" },
        { open: '`', close: '`' },
      ],
      surroundingPairs: [
        { open: '{', close: '}' },
        { open: '[', close: ']' },
        { open: '(', close: ')' },
        { open: '"', close: '"' },
        { open: "'", close: "'" },
        { open: '`', close: '`' },
      ],
    });
  }

  onMount(() => {
    registerVoLanguage();

    editor = monaco.editor.create(container, {
      value,
      language: 'vo',
      theme: $theme === 'dark' ? 'vs-dark' : 'vs',
      fontSize: 14,
      fontFamily: "'JetBrains Mono', 'Fira Code', monospace",
      lineHeight: 24,
      minimap: { enabled: false },
      scrollBeyondLastLine: false,
      automaticLayout: true,
      tabSize: 4,
      insertSpaces: true,
      padding: { top: 16 },
      renderLineHighlight: 'all',
      smoothScrolling: true,
      cursorBlinking: 'smooth',
      cursorSmoothCaretAnimation: 'on',
    });

    editor.onDidChangeModelContent(() => {
      value = editor.getValue();
    });

    return () => {
      editor.dispose();
    };
  });

  $effect(() => {
    if (editor) {
      monaco.editor.setTheme($theme === 'dark' ? 'vs-dark' : 'vs');
    }
  });

  $effect(() => {
    if (editor && editor.getValue() !== value) {
      editor.setValue(value);
    }
  });
</script>

<div class="editor-container" bind:this={container}></div>

<style>
  .editor-container {
    width: 100%;
    height: 100%;
  }
</style>
