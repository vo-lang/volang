import assert from 'node:assert/strict';
import {mkdir, readFile, writeFile, copyFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {build} from 'esbuild';
import {root} from './repository-paths.mjs';

/** Adapter fixture assets; application styles stay with the shared example. */
export async function desktopAssets(directory, {check = false} = {}) {
  await mkdir(directory, {recursive: true});
  const input = await readFile(resolve(root, 'ui/next/examples/interaction/index.html'), 'utf8');
  const script = '<script type="module" src="/host/ui_next/lab.js"></script>';
  assert.equal(input.split(script).length, 2);
  const html = input.replace(/  <p id="status"[^\n]*\n/, '')
    .replace(script, '<!--volang-desktop-bootstrap-->' + (check ? '<script src="/check.js" defer></script>' : ''));
  await writeFile(resolve(directory, 'index.html'), html);
  await build({entryPoints:[resolve(root,'lang/crates/vo-web/js/ui_next/desktop.ts')],
    outfile:resolve(directory,'desktop.js'),bundle:true,format:'iife',platform:'browser',target:'es2020',minify:true});
  if (check) await copyFile(resolve(root,'eng/ui-next/desktop-check.js'),resolve(directory,'check.js'));
}
