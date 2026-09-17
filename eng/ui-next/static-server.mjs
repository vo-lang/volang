import { createServer } from 'node:http';
import { developmentEvents } from './development-events.mjs';
import { realpath, stat } from 'node:fs/promises';
import { join, resolve, sep } from 'node:path';
import { sendStatic } from './static-response.mjs';

/** A local static preview with optional development events. No compiler or repository routes. */
export async function serveFiles(directory, { development = false, base = '/', fallbackDocument, notFoundDocument } = {}) {
  directory = resolve(directory);
  if (!base.startsWith('/') || !base.endsWith('/')) throw new Error('Static base must start and end with /.');
  const publicRoot = await realpath(directory);
  const missingPage = notFoundDocument === undefined ? undefined : await realpath(resolve(directory,notFoundDocument));
  if (missingPage && (!missingPage.startsWith(publicRoot+sep) || !(await stat(missingPage)).isFile())) throw new Error('The missing-page document must be a public file.');
  const events = developmentEvents();
  const pending = new Set();
  let closing = false, closed;
  const server = createServer((request,response)=>{
    const work = handle(request,response);
    pending.add(work);
    void work.then(()=>pending.delete(work),()=>{pending.delete(work); response.destroy();});
  });
  async function handle(request, response) {
    try {
      if (closing) {response.writeHead(503).end(); return;}
      if (!['GET', 'HEAD'].includes(request.method)) { response.writeHead(405, { allow: 'GET, HEAD' }).end(); return; }
      const url = new URL(request.url, 'http://localhost');
      let pathname;
      try { pathname = decodeURIComponent(url.pathname); } catch { response.writeHead(400).end(); return; }
      if (development && events.handle(pathname, request, response)) return;
      if (!pathname.startsWith(base)) { response.writeHead(404).end(); return; }
      const name = pathname.slice(base.length) || 'index.html';
      let path = resolve(directory, name);
      if (!path.startsWith(directory + sep)) { response.writeHead(404).end(); return; }
      if ((await stat(path)).isDirectory()) {
        if (!pathname.endsWith('/')) {
          response.writeHead(308, { location: url.pathname + '/' + url.search }).end();
          return;
        }
        path = join(path, 'index.html');
      }
      const baseDirectory = await realpath(directory);
      path = await realpath(path);
      if (!path.startsWith(baseDirectory+sep)) {response.writeHead(404).end(); return;}
      await sendStatic(request,response,path,{root:baseDirectory,cache:!development,precompressed:!development});
    } catch (error) {
      if (response.destroyed || response.writableEnded) return;
      if (response.headersSent) {response.destroy(); return;}
      const missing = ['ENOENT', 'ENOTDIR', 'EISDIR'].includes(error.code);
      // A direct document visit must be able to receive the current compiler
      // error and recover, even before its first successful build creates it.
      // Missing asset requests and healthy-site 404s retain their normal status.
      if (missing && development && events.hasError && fallbackDocument
          && request.headers['sec-fetch-dest'] === 'document') {
        response.writeHead(503, { 'content-type': 'text/html; charset=utf-8', 'cache-control': 'no-store' });
        response.end(request.method === 'HEAD' ? undefined : fallbackDocument);
        return;
      }
      if (missing && missingPage && (request.headers['sec-fetch-dest'] === 'document' || request.headers.accept?.includes('text/html'))) {
        await sendStatic(request,response,missingPage,{root:publicRoot,status:404,cache:false,precompressed:!development});
        return;
      }
      response.writeHead(missing ? 404 : 500, { 'content-type': 'text/plain' }).end(missing ? 'File not found.' : 'Could not read this preview file.');
    }
  }
  await new Promise((resolveListen, reject) => { server.once('error', reject); server.listen(0, '127.0.0.1', resolveListen); });
  return {
    url: `http://127.0.0.1:${server.address().port}${base}`,
    broadcast(event) {
      events.broadcast(event);
    },
    close() {
      return closed ??= (async () => {
        closing = true; events.close();
        const stopped = new Promise(resolveClose=>server.close(resolveClose));
        server.closeAllConnections();
        await Promise.allSettled([...pending]); await stopped;
      })();
    },
  };
}
