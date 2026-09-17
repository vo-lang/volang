import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import test from 'node:test';
import {StringStream} from '@codemirror/language';
import {editorKeywords, editorTypes, voStreamParser} from '../../lang/crates/vo-web/js/ui_next/editor-language.js';

function tokenize(source) {
  const state = voStreamParser.startState(), tokens = [];
  for (const line of source.split('\n')) {
    const stream = new StringStream(line, 4);
    while (!stream.eol()) {
      stream.start = stream.pos;
      const type = voStreamParser.token(stream, state);
      assert(stream.pos > stream.start, 'the editor lexer did not advance');
      if (type) tokens.push({value:stream.current(), type});
    }
  }
  return {state, tokens};
}

test('editor keywords match the compiler-owned language profile', async () => {
  const source = await readFile(new URL('../../lang/crates/vo-common-core/src/identifier.rs', import.meta.url), 'utf8');
  const definition = source.match(/VO_KEYWORDS:[^=]*=\s*&\[(.*?)\];/s);
  assert(definition);
  assert.deepEqual(editorKeywords, [...definition[1].matchAll(/"(\w+)"/g)].map(match => match[1]));
  assert(tokenize(editorKeywords.join(' ')).tokens.every(token => token.type === 'keyword'));
  assert.equal(tokenize('object').tokens[0].type, 'variableName');
  assert.equal(tokenize('uintptr').tokens[0].type, 'variableName');
});

test('VS Code and browser editors agree on Vo keywords and predeclared types', async () => {
  const grammar = JSON.parse(await readFile(new URL('../../ui/editors/vscode/syntaxes/volang.tmLanguage.json', import.meta.url)));
  for (const [name, words, token] of [['keywords',editorKeywords,'keyword'], ['types',editorTypes,'typeName']]) {
    const pattern = new RegExp(grammar.repository[name].patterns[0].match);
    for (const word of words) {
      assert.equal(pattern.exec(word)?.[0], word, name + ': ' + word);
      assert.equal(tokenize(word).tokens[0].type, token);
    }
    for (const word of ['object','uintptr','complex64','complex128','my_return_value','integer']) {
      assert.equal(pattern.test(word), false, name + ': ' + word);
      assert.equal(tokenize(word).tokens[0].type, 'variableName');
    }
  }
});

test('Vo nested comments and raw strings do not change indentation or keywords', () => {
  const quote = String.fromCharCode(96);
  const {state, tokens} = tokenize('func main() {\n/* outer { /* inner */\nreturn } */\nx := ' + quote + '/* not a comment\nfunc }' + quote + '\n}');
  assert.equal(state.comment, 0); assert.equal(state.raw, false); assert.equal(state.depth, 0);
  assert.equal(tokens.filter(token => token.type === 'keyword').length, 1);
  assert(tokens.some(token => token.type === 'comment' && token.value.includes('inner')));
  assert(tokens.some(token => token.type === 'string' && token.value.includes('func')));
});

test('numeric forms, escaped literals and Unicode identifiers retain token boundaries', () => {
  const {tokens} = tokenize('变量２ := 0x.8p+1 + 0b10_01 + 0o755 + .25e-2\ns := "a\\"b" // note\nr := \'{\'');
  assert.equal(tokens[0].value, '变量２');
  assert.deepEqual(tokens.filter(token => token.type === 'number').map(token => token.value), ['0x.8p+1','0b10_01','0o755','.25e-2']);
  assert(tokens.some(token => token.type === 'string' && token.value === '"a\\"b"'));
  assert.equal(tokenize('"unfinished\nfunc next() {}').tokens.at(1).type, 'keyword');
});
