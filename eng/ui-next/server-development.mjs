import {createServer} from 'node:http';
import {cp, readFile, rm} from 'node:fs/promises';
import {join} from 'node:path';
import {createApplication} from './server-adapter.mjs';
import {createAdmission} from './server-admission.mjs';
import {developmentEvents} from './development-events.mjs';
import {compilerPath,toolchain} from './toolchain.mjs';

const prefix = '/__ui-next/builds/';

/** A stable development origin with bounded, immutable build generations.
 * Old requests drain against their own build. Refresh requests select a complete
 * generation; ordinary module imports keep their original revision address. */
export async function serveDevelopmentApplication({fallbackDocument, retained = 8, concurrency = 4, queued = 16} = {}) {
  if (!Number.isInteger(retained) || retained < 2 || retained > 32) throw new Error('Retained development builds must be 2..32.');
  const events = developmentEvents(), generations = new Map();
  const admission = createAdmission({concurrency, queued});
  const developmentScript = await readFile(join(toolchain.host, 'development.js'));
  let current, closing;
  const server = createServer((request, response) => {
    try {
      const url = new URL(request.url, 'http://localhost');
      if (events.handle(url.pathname, request, response)) return;
      if (closing) {response.writeHead(503, {connection:'close'}).end(); return;}
      if (url.pathname.startsWith(prefix)) {
        const relative = url.pathname.slice(prefix.length), slash = relative.indexOf('/');
        const version = relative.slice(0, slash);
        // Reload queries may originate in an old boot module; exact published
        // revisions win, while manual/style refreshes select the current build.
        const refreshed = url.searchParams.get('ui-dev');
        const generation = refreshed === null ? generations.get(version) : generations.get(refreshed) ?? current;
        if (slash < 1 || !generation) {response.writeHead(410, {'cache-control':'no-store'}).end('This development version has expired. Reload the page.'); return;}
        generation.application.asset(request, response, decodeURIComponent(relative.slice(slash + 1)));
        return;
      }
      if (current) {current.application.handle(request, response); return;}
      if (url.pathname === '/assets/development.js' && ['GET', 'HEAD'].includes(request.method)) {
        response.writeHead(200, {'content-type':'text/javascript', 'cache-control':'no-store'});
        response.end(request.method === 'HEAD' ? undefined : developmentScript);
      } else if (['GET', 'HEAD'].includes(request.method) && request.headers['sec-fetch-dest'] === 'document') {
        response.writeHead(503, {'content-type':'text/html; charset=utf-8', 'cache-control':'no-store'});
        response.end(request.method === 'HEAD' ? undefined : fallbackDocument);
      } else response.writeHead(503, {'cache-control':'no-store', connection:'close'}).end('Waiting for a successful build.');
    } catch {response.writeHead(400, {connection:'close'}).end('Invalid development request.');}
  });
  await new Promise((resolve, reject) => {server.once('error', reject); server.listen(0, '127.0.0.1', resolve);});
  return {
    url:`http://127.0.0.1:${server.address().port}/`,
    broadcast:events.broadcast,
    // Ownership of a complete generated directory transfers on success.
    // The caller removes an unpublished directory if installation fails.
    async install(directory, revision) {
      if (closing) throw new Error('Development server is closing.');
      if (!/^[a-z0-9-]{1,80}$/.test(revision) || generations.has(revision)) throw new Error('Invalid or repeated development revision.');
      const application = await createApplication(directory, {executable:compilerPath(), assetBase:prefix + revision + '/', admission});
      try {
        if (generations.size >= retained) {
          const oldest = generations.values().next().value;
          await oldest.drained;
          await oldest.application.close();
          await rm(oldest.directory, {recursive:true, force:true});
          generations.delete(oldest.revision);
        }
        if (closing) throw new Error('Development server is closing.');
        const previous = current;
        current = {directory, revision, application};
        generations.set(revision, current);
        if (previous) previous.drained = previous.application.retire();
      } catch (error) {await application.close(); throw error;}
    },
    async updateStyles(source, relative) {
      if (!current || closing) throw new Error('No current development build.');
      const generation = current;
      await cp(source, join(generation.directory, 'public', relative));
      if (generation !== current || closing) throw new Error('Development build changed during stylesheet update.');
    },
    close() {
      return closing ??= (async () => {
        events.close();
        const stopped = new Promise(resolve => server.close(resolve));
        server.closeAllConnections();
        await Promise.all([...generations.values()].map(async generation => {
          await generation.application.close();
          await rm(generation.directory, {recursive:true, force:true});
        }));
        generations.clear(); current = undefined;
        await stopped;
      })();
    },
  };
}
