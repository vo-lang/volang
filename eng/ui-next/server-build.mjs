import { mkdir, writeFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { build as bundle } from './node_modules/esbuild/lib/main.js';
import {serverEntries} from './server-entries.mjs';

// Runtime paths are relative to the delivered bundle. Repository/compiler paths
// are consumed during the build and never embedded into the deployment entry.
export async function buildServerHost(directory, template, {fixedBase, assetDirectories, entries} = {}) {
  const declared = serverEntries(entries);
  await mkdir(directory, {recursive:true});
  await writeFile(join(directory, 'document.html'), template);
  await writeFile(join(directory, 'entries.json'), JSON.stringify({version:1, entries:declared}) + '\n');
  const adapter = fileURLToPath(new URL('./server-adapter.mjs', import.meta.url));
  await bundle({
    stdin: {contents:`
import {fileURLToPath} from 'node:url';
import {dirname, resolve} from 'node:path';
import {serveApplication} from ${JSON.stringify(adapter)};
export function start(options = {}) {
  const fixedBase = ${JSON.stringify(fixedBase) ?? 'undefined'};
  if (fixedBase !== undefined && (options.base ?? '/') !== fixedBase) throw new Error('This application requires deployment at ' + fixedBase);
  return serveApplication(resolve(dirname(fileURLToPath(import.meta.url)), '..'), {assetDirectories:${JSON.stringify(assetDirectories) ?? 'undefined'}, ...options});
}
if (process.argv[1] && resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const port = process.env.PORT === undefined ? 3000 : Number(process.env.PORT);
  if (!Number.isInteger(port) || port < 0 || port > 65535) throw new Error('PORT must be 0..65535.');
  const application = await start({hostname:process.env.HOST || '127.0.0.1', port, base:process.env.BASE_PATH || '/'});
  console.log(application.url);
  for (const signal of ['SIGINT','SIGTERM']) process.once(signal, () => { void application.close(); });
}
`, resolveDir:resolve('.'), sourcefile:'server-entry.mjs', loader:'js'},
    outfile:join(directory, 'entry.mjs'), bundle:true, platform:'node', target:'node24', format:'esm', minify:true,
  });
}
