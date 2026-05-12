type Monaco = typeof import('monaco-editor/esm/vs/editor/editor.api.js');

declare global {
  interface Window {
    MonacoEnvironment?: {
      getWorker(moduleId: string, label: string): Worker;
    };
  }
}

let monacoModulePromise: Promise<Monaco> | null = null;
let voConfigured = false;
let themeConfigured = false;

const voKeywords = [
  'as',
  'break',
  'case',
  'chan',
  'const',
  'continue',
  'default',
  'defer',
  'else',
  'enum',
  'errdefer',
  'fail',
  'for',
  'func',
  'go',
  'if',
  'import',
  'in',
  'interface',
  'match',
  'mut',
  'pub',
  'return',
  'select',
  'struct',
  'switch',
  'type',
  'var',
];

const voTypeKeywords = [
  'any',
  'bool',
  'byte',
  'error',
  'f32',
  'f64',
  'float',
  'i16',
  'i32',
  'i64',
  'i8',
  'int',
  'rune',
  'string',
  'u16',
  'u32',
  'u64',
  'u8',
  'uint',
  'void',
];

async function importMonaco(): Promise<Monaco> {
  if (!monacoModulePromise) {
    monacoModulePromise = Promise.all([
      import('monaco-editor/esm/vs/editor/editor.api.js') as Promise<Monaco>,
      import('monaco-editor/esm/vs/editor/editor.worker?worker'),
    ]).then(([monaco, editorWorker]) => {
      window.MonacoEnvironment = {
        getWorker() {
          return new editorWorker.default();
        },
      };
      return monaco;
    });
  }

  return monacoModulePromise;
}

export async function loadMonaco(): Promise<Monaco> {
  const monaco = await importMonaco();

  if (!voConfigured) {
    monaco.languages.register({
      id: 'vo',
      aliases: ['Vo', 'vo'],
      extensions: ['.vo'],
    });

    monaco.languages.setLanguageConfiguration('vo', {
      comments: {
        lineComment: '//',
        blockComment: ['/*', '*/'],
      },
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
      brackets: [
        ['{', '}'],
        ['[', ']'],
        ['(', ')'],
      ],
    });

    monaco.languages.setMonarchTokensProvider('vo', {
      defaultToken: '',
      tokenPostfix: '.vo',
      keywords: voKeywords,
      typeKeywords: voTypeKeywords,
      operators: [
        '=', '>', '<', '!', '~', '?', ':', '==', '<=', '>=', '!=', '&&', '||', '++', '--', '+', '-', '*', '/', '&', '|', '^', '%', '<<', '>>', '+=', '-=', '*=', '/=', '%=', ':=', '=>', '<-',
      ],
      symbols: /[=><!~?:&|+\-*\/\^%]+/,
      escapes: /\\(?:[abfnrtv\\"'0-7]|x[0-9A-Fa-f]{2}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,
      tokenizer: {
        root: [
          [/[a-z_$][\w$]*/, {
            cases: {
              '@keywords': 'keyword',
              '@typeKeywords': 'type',
              '@default': 'identifier',
            },
          }],
          [/[A-Z][\w$]*/, 'type.identifier'],
          { include: '@whitespace' },
          [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
          [/0[xX][0-9a-fA-F_]+/, 'number.hex'],
          [/\d+/, 'number'],
          [/[{}()\[\]]/, '@brackets'],
          [/@symbols/, {
            cases: {
              '@operators': 'operator',
              '@default': '',
            },
          }],
          [/[;,.]/, 'delimiter'],
          [/"([^"\\]|\\.)*$/, 'string.invalid'],
          [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],
          [/`/, { token: 'string.quote', bracket: '@open', next: '@rawstring' }],
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
        string: [
          [/[^\\"]+/, 'string'],
          [/@escapes/, 'string.escape'],
          [/\\./, 'string.escape.invalid'],
          [/"/, { token: 'string.quote', bracket: '@close', next: '@pop' }],
        ],
        rawstring: [
          [/[^`]+/, 'string'],
          [/`/, { token: 'string.quote', bracket: '@close', next: '@pop' }],
        ],
      },
    });

    voConfigured = true;
  }

  if (!themeConfigured) {
    monaco.editor.defineTheme('vo-studio', {
      base: 'vs-dark',
      inherit: true,
      rules: [
        { token: 'keyword', foreground: 'CBA6F7' },
        { token: 'type', foreground: '89B4FA' },
        { token: 'type.identifier', foreground: '74C7EC' },
        { token: 'number', foreground: 'FAB387' },
        { token: 'string', foreground: 'A6E3A1' },
        { token: 'comment', foreground: '6C7086' },
        { token: 'operator', foreground: 'F5C2E7' },
      ],
      colors: {
        'editor.background': '#1E1E2E',
        'editor.foreground': '#CDD6F4',
        'editor.lineHighlightBackground': '#181825',
        'editorLineNumber.foreground': '#585B70',
        'editorLineNumber.activeForeground': '#CDD6F4',
        'editorCursor.foreground': '#89B4FA',
        'editor.selectionBackground': '#45475A80',
        'editor.inactiveSelectionBackground': '#31324466',
        'editorIndentGuide.background1': '#313244',
        'editorIndentGuide.activeBackground1': '#585B70',
      },
    });
    themeConfigured = true;
  }

  return monaco;
}

export function resolveMonacoLanguage(path: string): string {
  const normalized = path.toLowerCase();
  if (normalized.endsWith('.vo')) return 'vo';
  return 'plaintext';
}
