// Lexical presentation only. Compiler diagnostics remain authoritative.
// The keyword contract test follows vo-common-core/src/identifier.rs.
export const editorKeywords = [
  'break', 'case', 'chan', 'const', 'continue', 'default', 'defer', 'else',
  'errdefer', 'fail', 'fallthrough', 'for', 'func', 'go', 'goto', 'if',
  'import', 'interface', 'island', 'map', 'package', 'port', 'range',
  'return', 'select', 'struct', 'switch', 'type', 'var',
];
const keywords = new Set(editorKeywords);
export const editorTypes = 'bool byte rune int int8 int16 int32 int64 uint uint8 uint16 uint32 uint64 float32 float64 string any error'.split(' ');
const types = new Set(editorTypes);
const number = /^(?:0[xX](?:[\da-fA-F_]+(?:\.[\da-fA-F_]*)?|\.[\da-fA-F_]+)(?:[pP][+-]?[\d_]+)?|0[bB][01_]+|0[oO][0-7_]+|(?:\d[\d_]*(?:\.(?!\.)[\d_]*)?|\.[\d_]+)(?:[eE][+-]?[\d_]+)?)/;
const identifier = /^[_\p{Alphabetic}][_\p{Alphabetic}\p{Decimal_Number}]*/u;
const rawQuote = '\u0060';

function blockComment(stream, state) {
  while (!stream.eol()) {
    if (stream.match('/*')) state.comment++;
    else if (stream.match('*/')) { if (--state.comment === 0) break; }
    else stream.next();
  }
  return 'comment';
}

function rawString(stream, state) {
  while (!stream.eol()) if (stream.next() === rawQuote) { state.raw = false; break; }
  return 'string';
}

export const voStreamParser = {
  name: 'Volang',
  startState: () => ({comment: 0, raw: false, depth: 0}),
  token(stream, state) {
    if (state.comment) return blockComment(stream, state);
    if (state.raw) return rawString(stream, state);
    if (stream.eatSpace()) return null;
    if (stream.match('//')) { stream.skipToEnd(); return 'comment'; }
    if (stream.match('/*')) { state.comment = 1; return blockComment(stream, state); }
    if (stream.match(number)) return 'number';
    const word = stream.match(identifier);
    if (word) {
      const name = word[0];
      if (keywords.has(name)) return 'keyword';
      if (name === 'true' || name === 'false') return 'bool';
      if (name === 'nil') return 'null';
      return types.has(name) ? 'typeName' : 'variableName';
    }
    const character = stream.next();
    if (character === rawQuote) { state.raw = true; return rawString(stream, state); }
    if (character === '"' || character === "'") {
      while (!stream.eol()) {
        const next = stream.next();
        if (next === '\\') stream.next();
        else if (next === character) break;
      }
      return 'string';
    }
    if ('{[('.includes(character)) state.depth++;
    else if ('}])'.includes(character)) state.depth = Math.max(0, state.depth - 1);
    return /[+\-*/%=<>!&|^:~?]/.test(character) ? 'operator' : 'punctuation';
  },
  indent(state, textAfter, context) {
    if (state.raw || state.comment) return null;
    return Math.max(0, state.depth - (/^\s*[}\])]/.test(textAfter) ? 1 : 0)) * context.unit;
  },
  languageData: {commentTokens: {line: '//', block: {open: '/*', close: '*/'}}, closeBrackets: {brackets: ['(', '[', '{', '"', "'", rawQuote]}},
};
