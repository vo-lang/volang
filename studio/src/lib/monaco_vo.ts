import * as monaco from 'monaco-editor';

// =============================================================================
// Vo language definition for Monaco editor.
// Based on Go syntax, extended for Vo-specific constructs:
//   ~>   dynamic access operator
//   ?    error propagation operator
//   fail built-in statement
//   errdefer  like defer but runs only on error return
// =============================================================================

const LANG_ID = 'vo';

export function registerVoLanguage(): void {
  // Guard: only register once
  const langs = monaco.languages.getLanguages();
  if (langs.some(l => l.id === LANG_ID)) return;

  monaco.languages.register({
    id: LANG_ID,
    extensions: ['.vo'],
    aliases: ['Vo', 'vo'],
    mimetypes: ['text/x-vo'],
  });

  monaco.languages.setMonarchTokensProvider(LANG_ID, {
    defaultToken: 'invalid',
    tokenPostfix: '.vo',

    keywords: [
      'break', 'case', 'chan', 'const', 'continue',
      'default', 'defer', 'errdefer', 'else', 'fallthrough',
      'fail', 'for', 'func', 'go', 'goto',
      'if', 'import', 'interface', 'map', 'package',
      'range', 'return', 'select', 'struct', 'switch',
      'type', 'var',
    ],

    builtins: [
      'append', 'cap', 'close', 'complex', 'copy',
      'delete', 'imag', 'len', 'make', 'new',
      'panic', 'print', 'println', 'real', 'recover',
    ],

    typeKeywords: [
      'bool', 'byte', 'complex64', 'complex128', 'error',
      'float32', 'float64', 'int', 'int8', 'int16', 'int32', 'int64',
      'rune', 'string', 'uint', 'uint8', 'uint16', 'uint32', 'uint64',
      'uintptr', 'any',
    ],

    operators: [
      '+', '-', '*', '/', '%',
      '&', '|', '^', '<<', '>>',
      '&&', '||', '!',
      '==', '!=', '<', '<=', '>', '>=',
      '=', ':=', '+=', '-=', '*=', '/=', '%=',
      '&=', '|=', '^=', '<<=', '>>=',
      '<-', '...', '~>',
    ],

    symbols: /[=><!~?:&|+\-*\/\^%]+/,

    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,

    tokenizer: {
      root: [
        // Error propagation operator (must come before identifiers)
        [/\?/, 'keyword.operator.vo'],

        // Dynamic access operator
        [/~>/, 'keyword.operator.dyn'],

        // Identifiers and keywords
        [/[a-zA-Z_]\w*/, {
          cases: {
            '@keywords':    'keyword',
            '@builtins':    'predefined',
            '@typeKeywords':'type',
            '@default':     'identifier',
          },
        }],

        // Whitespace
        { include: '@whitespace' },

        // Delimiters and operators
        [/[{}()\[\]]/, '@brackets'],
        [/[<>](?!@symbols)/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default':   '',
          },
        }],

        // Numbers
        [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
        [/0[xX][0-9a-fA-F]+/, 'number.hex'],
        [/\d+/, 'number'],

        // Delimiter: after number because of .\d floats
        [/[;,.]/, 'delimiter'],

        // Strings
        [/"([^"\\]|\\.)*$/, 'string.invalid'],
        [/"/, { token: 'string.quote', bracket: '@open', next: '@string' }],
        [/`/, { token: 'string.quote', bracket: '@open', next: '@rawstring' }],

        // Characters
        [/'[^\\']'/, 'string'],
        [/(')(@escapes)(')/, ['string', 'string.escape', 'string']],
        [/'/, 'string.invalid'],
      ],

      comment: [
        [/[^\/*]+/, 'comment'],
        [/\/\*/,    'comment', '@push'],
        [/\*\//,    'comment', '@pop'],
        [/[\/*]/,   'comment'],
      ],

      string: [
        [/[^\\"]+/,  'string'],
        [/@escapes/, 'string.escape'],
        [/\\./,      'string.escape.invalid'],
        [/"/,        { token: 'string.quote', bracket: '@close', next: '@pop' }],
      ],

      rawstring: [
        [/[^`]+/, 'string'],
        [/`/,     { token: 'string.quote', bracket: '@close', next: '@pop' }],
      ],

      whitespace: [
        [/[ \t\r\n]+/, 'white'],
        [/\/\*/,       'comment', '@comment'],
        [/\/\/.*$/,    'comment'],
      ],
    },
  } as monaco.languages.IMonarchLanguage);

  monaco.languages.setLanguageConfiguration(LANG_ID, {
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
      { open: '"', close: '"', notIn: ['string'] },
      { open: '`', close: '`', notIn: ['string'] },
    ],
    surroundingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
      { open: '`', close: '`' },
    ],
    indentationRules: {
      increaseIndentPattern: /^.*\{[^}"'`]*$/,
      decreaseIndentPattern: /^(.*\*\/)?\s*\}[;,]?\s*$/,
    },
  });
}
