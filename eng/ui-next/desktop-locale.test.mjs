import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {runInNewContext} from 'node:vm';
import {test} from 'node:test';

const script = await readFile(new URL('../../lang/crates/vo-ui-webview/src/window/locale.js', import.meta.url), 'utf8');
test('desktop locale repairs POSIX host values for Intl consumers', () => {
  for (const [language, languages, expected] of [
    ['C', ['C'], 'en-US'],
    ['C.UTF-8', [], 'en-US'],
    ['C', ['C', 'zh-CN', 'en-US'], 'zh-CN'],
    ['de-DE', ['de-DE', 'C'], 'de-DE'],
  ]) {
    const navigator = {language, languages};
    runInNewContext(script, {navigator});
    assert.equal(navigator.language, expected);
    assert.doesNotThrow(() => new Intl.NumberFormat(navigator.language).format(1234.5));
    assert.doesNotThrow(() => new Intl.DateTimeFormat(navigator.languages).format(0));
    assert.equal(Object.isFrozen(navigator.languages), true);
  }
});
test('desktop locale preserves valid browser language properties', () => {
  const languages = Object.freeze(['fr-CA', 'en-US']);
  const navigator = {language: 'fr-CA', languages};
  const descriptors = Object.getOwnPropertyDescriptors(navigator);
  runInNewContext(script, {navigator});
  assert.deepEqual(Object.getOwnPropertyDescriptors(navigator), descriptors);
});
