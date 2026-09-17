import { createServer } from 'node:http';
import { developmentEvents } from './development-events.mjs';
import { readFile } from 'node:fs/promises';
import { resolve, sep, extname } from 'node:path';
import { fileURLToPath } from 'node:url';
import {root} from './repository-paths.mjs';
export {root} from './repository-paths.mjs';
import { renderHtml } from './prerender.mjs';
import { studioDocument, studioDocumentHtml } from './studio-documents.mjs';

const routes = [
  ['/host/', resolve(root, 'lang/crates/vo-web/dist')],
  ['/wasm/', resolve(root, 'target/ui-next/wasm-runtime')],
  ['/artifacts/', resolve(root, 'target/ui-next')],
  ['/vendor/uplot/', resolve(root, 'eng/ui-next/node_modules/uplot/dist')],
  ['/studio-assets/', resolve(root, 'apps/studio/next')],
  ['/studio-docs/', resolve(root, 'apps/studio/next/documentation')],
  ['/style-lab/', resolve(root, 'ui/next/examples/styling')],
  ['/ui-kit/', resolve(root, 'ui/next/kit')],
  ['/compiler/', resolve(root, 'target/ui-next/wasm-compiler')],
];
const mime = { '.js': 'text/javascript', '.wasm': 'application/wasm', '.html': 'text/html', '.json': 'application/json', '.css': 'text/css' };
const searchResult = query => `Results for ${query}`;

async function serverHtml(entry, args, workspace = false, input) {
  const { compilerPath } = await import('../../lang/crates/vo-web/test_compiler.mjs');
  return renderHtml(compilerPath(), ['run', entry, '--', input === undefined ? '--ssr' : '--ssr-data', ...args], {
    cwd: root, env: { ...process.env, VOWORK: workspace ? resolve(root, 'vo.work') : 'off' },
    input,
  });
}

export async function serve({ development = false } = {}) {
  const events = developmentEvents();
  const developmentScript = development ? '<script type="module" src="/host/ui_next/development.js"></script>' : '';
  const server = createServer(async (request, response) => {
    try {
      const url = new URL(request.url, 'http://localhost');
      const pathname = decodeURIComponent(url.pathname);
      if (development && events.handle(pathname, request, response)) return;
      if (pathname === '/api/search') {
        const query = url.searchParams.get('q') ?? '';
        const delay = query.startsWith('slow') ? 800 : 30;
        const timer = setTimeout(() => {
          response.writeHead(query === 'error' ? 503 : 200, { 'content-type': 'text/plain' });
          response.end(searchResult(query));
        }, delay);
        response.once('close', () => clearTimeout(timer));
        return;
      }
      if (pathname === '/studio' || pathname.startsWith('/studio/')) {
        let html = await readFile(resolve(root, 'apps/studio/next/index.html'), 'utf8');
        const ssr = url.searchParams.has('ssr') && !development;
        const document = await studioDocument(root, url, { includeBody: ssr });
        const content = ssr ? await serverHtml('apps/studio/next', [], true,
          document.initial ?? JSON.stringify({ Version: 1, Location: pathname + url.search, Data: '' })) : undefined;
        html = studioDocumentHtml(html, document, content);
        response.writeHead(200, { 'content-type': 'text/html', 'cache-control': 'no-store' });
        if (development) html = html.replace('/studio-assets/boot.js', '/studio-assets/development-boot.js');
        response.end(html.replace('</body>', developmentScript + '</body>'));
        return;
      }
      const example = url.searchParams.get('example') ?? 'interaction';
      if (!['interaction', 'workbench', 'inspection', 'styling'].includes(example)) { response.writeHead(404).end(); return; }
      let path;
      if (pathname === '/') path = resolve(root, `ui/next/examples/${example}/index.html`);
      else {
        const route = routes.find(([prefix]) => pathname.startsWith(prefix));
        if (!route) { response.writeHead(404).end(); return; }
        path = resolve(route[1], pathname.slice(route[0].length));
        if (!path.startsWith(route[1] + sep)) { response.writeHead(404).end(); return; }
      }
      let bytes = await readFile(path);
      if (pathname === '/' && url.searchParams.has('ssr')) {
        let html, initial = '';
        if (example === 'workbench') {
          // The fixture's server and HTTP handler share one data source. The
          // exact snapshot travels with its HTML; the browser need not fetch it.
          initial = JSON.stringify({ Version: 1, Values: [{
            Request: { Service: 'web.fetch-text', Value: '/api/search?q=welcome', TimeoutMilliseconds: 300 },
            Value: searchResult('welcome'), UpdatedAtMilliseconds: Date.now() - (url.searchParams.has('stale') ? 60000 : 0),
          }] });
          html = await serverHtml('ui/next/examples/workbench', [initial]);
        } else html = await readFile(resolve(root, `target/ui-next/${example}.ssr.html`), 'utf8');
        bytes = Buffer.from(bytes.toString().replace('<div id="root"></div>', () => `<div id="root">${html}</div>`)
          .replace('<script id="ui-initial-data" type="application/json">""</script>',
            () => `<script id="ui-initial-data" type="application/json">${JSON.stringify(initial).replaceAll('<', '\\u003c')}</script>`));
      }
      response.writeHead(200, { 'content-type': mime[extname(path)] ?? 'application/octet-stream', 'cache-control': 'no-store' });
      response.end(bytes);
    } catch (error) {
      if (error?.code === 'ENOENT') { response.writeHead(404).end(); return; }
      console.error('UI fixture request failed:', error);
      response.writeHead(500, {'content-type':'text/plain'}).end(String(error?.message ?? error).slice(0, 4096));
    }
  });
  await new Promise((resolve, reject) => { server.once('error', reject); server.listen(0, '127.0.0.1', resolve); });
  return {
    url: `http://127.0.0.1:${server.address().port}`,
    broadcast: events.broadcast,
    close: () => { events.close(); return new Promise(resolve => server.close(resolve)); },
  };
}

if (process.argv[1] === fileURLToPath(import.meta.url)) {
  const server = await serve();
  console.log(server.url);
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, async () => { await server.close(); process.exit(); });
}
