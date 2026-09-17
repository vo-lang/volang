import assert from 'node:assert/strict';
import test from 'node:test';
import {build} from './node_modules/esbuild/lib/main.js';
import {root} from './server.mjs';

const bundle = async (entry, names) => {
  const result = await build({stdin:{contents:`export {${names}} from './lang/crates/vo-web/js/ui_next/${entry}.ts';`, resolveDir:root},
    bundle:true, write:false, format:'esm', platform:'browser', target:'es2022', minify:true});
  return result.outputFiles[0].text;
};

test('ordinary application bundles omit the optional widget loader', async () => {
  const plain = await bundle('mount', 'mountUi');
  const enabled = await bundle('mount', 'mountUi, createLazyWidget');
  assert(!plain.includes('widget loading timed out'));
  assert(enabled.includes('widget loading timed out'));
});

test('development entries expose the same optional widget loader', async () => {
  assert((await bundle('development-mount', 'mountUi, createLazyWidget')).includes('widget loading timed out'));
});
