import {fileURLToPath} from 'node:url';
import {join,resolve} from 'node:path';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';

// Checkout-only layout. Packaged tools omit this module and carry an explicit
// inventory instead, so a damaged installation cannot fall back to local builds.
const root = fileURLToPath(new URL('../../', import.meta.url));
export default {
  kind:'checkout', root, compiler:compilerPath(),
  desktop:resolve(root,process.env.VO_UI_DESKTOP_SDK || 'target/ui-next/desktop-sdk'),
  ui:join(root,'ui'), license:join(root,'LICENSE'),
  host:join(root,'lang/crates/vo-web/js/ui_next'),
  vm:join(root,'target/ui-next/wasm-runtime'),
  plot:join(root,'eng/ui-next/plot-library.mjs'),
  editor:join(root,'eng/ui-next/editor-library.mjs'),
  cli:join(root,'eng/ui-next/cli.mjs'),
  testing:join(root,'eng/ui-next/testing.mjs'),
  testModule:join(root,'eng/browser/node_modules/@playwright/test/index.mjs'),
  testCLI:join(root,'eng/browser/node_modules/@playwright/test/cli.js'),
  browserCache:join(root,'target/playwright-browsers'),
};
