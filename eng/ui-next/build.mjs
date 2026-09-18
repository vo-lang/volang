import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gzipSync } from 'node:zlib';
import { compilerPath } from '../../lang/crates/vo-web/test_compiler.mjs';
import { root } from './server.mjs';
import { buildPlaygroundSources } from './playground-sources.mjs';
import { verifyStudioDocuments } from './studio-documents.mjs';
import { checkStudioExamples } from './studio-examples.mjs';
import { buildBrowserLibraries } from './browser-libraries.mjs';
import {webArtifactInventory} from './web-artifacts.mjs';

await verifyStudioDocuments(root);
const webPackages=await webArtifactInventory(root);
await checkStudioExamples();
await mkdir(resolve(root, 'target/ui-next'), { recursive: true });
await buildBrowserLibraries();
function execute(args, workspace = false) {
  const result = spawnSync(compilerPath(), args, { cwd: root, env: { ...process.env, VOWORK: workspace ? resolve(root, 'vo.work') : 'off' },
    encoding: 'utf8', timeout: 120_000, maxBuffer: 32 * 1024 * 1024 });
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stderr);
  return result.stdout;
}
const outputs = [];
for (const [name, entry] of [['runtime', 'ui/next/tests/runtime'], ['keyed-order', 'ui/next/tests/keyed_order'], ['interaction', 'ui/next/examples/interaction'], ['workbench', 'ui/next/examples/workbench'], ['inspection', 'ui/next/examples/inspection'], ['styling', 'ui/next/examples/styling'], ['benchmark', 'ui/next/examples/benchmark'], ['studio', 'apps/studio/next']]) {
  const path = `target/ui-next/${name}.vob`;
  execute(['emit', 'bytecode', entry, '-o', path], name === 'studio');
  const bytes = await readFile(resolve(root, path));
  outputs.push({ path, sha256: createHash('sha256').update(bytes).digest('hex'), bytes: bytes.length, gzipBytes: gzipSync(bytes).length });
}
for (const name of ['interaction', 'workbench', 'styling', 'keyed-order']) {
  const entry = name === 'keyed-order' ? 'ui/next/tests/keyed_order' : `ui/next/examples/${name}`;
  const html = execute(['run', entry, '--', '--ssr']);
  assert(html.startsWith('<!--vo:r:1-->'), 'SSR must run the actual Vo application');
  await writeFile(resolve(root, `target/ui-next/${name}.ssr.html`), html);
}
const native = execute(['run', 'ui/next/tests/runtime']);
assert.equal(native, 'ui-next runtime contracts: ok\n');
assert.equal(execute(['run', 'apps/studio/next/tests/documents'], true), 'studio document contracts: 24 chapters and text search ok\n');
assert.equal(execute(['run', 'apps/studio/next/tests/pages'], true), 'studio page contracts: routes and 24 chapter identities ok\n');
const sources = await readFile(await buildPlaygroundSources());
outputs.push({ path: 'target/ui-next/playground-ui.json', sha256: createHash('sha256').update(sources).digest('hex'), bytes: sources.length, gzipBytes: gzipSync(sources).length });
assert.deepEqual(await webArtifactInventory(root),webPackages,'Web runtime packages changed during the application build');
await writeFile(resolve(root, 'target/ui-next/build-report.json'), JSON.stringify({
  schema: 'volang.ui-next-build.v1', compiler: execute(['version']).trim(), nativeContracts: true, outputs, webPackages,
}, null, 2) + '\n');
console.log('UI prototype: native contracts, bytecode and server HTML built');
