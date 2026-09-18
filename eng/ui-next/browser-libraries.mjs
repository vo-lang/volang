import {build} from './node_modules/esbuild/lib/main.js';
import {resolve} from 'node:path';
import {root} from './server.mjs';

// Tool-owned optional dependencies never add npm manifests to Vo applications.
export async function buildBrowserLibraries() {
  for (const name of ['editor']) {
    await build({entryPoints:[resolve(root, `eng/ui-next/${name}-library.mjs`)],
      outfile:resolve(root, `target/ui-next/${name}-library.js`),
      bundle:true, format:'esm', platform:'browser', target:'es2022', minify:true});
  }
}
