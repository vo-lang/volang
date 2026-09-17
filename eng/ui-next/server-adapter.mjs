import { createServer } from 'node:http';
import { readFile, realpath, stat } from 'node:fs/promises';
import { join, resolve, sep } from 'node:path';
import { prepareHtml } from './prerender.mjs';
import { renderPage } from './server-response.mjs';
import { createAdmission, ServerBusy } from './server-admission.mjs';
import { requestBody, RequestError, serverMethods } from './server-request.mjs';
import { sendStatic } from './static-response.mjs';
import {readServerEntries} from './server-entries.mjs';

function deploymentBase(base) {
  if (typeof base !== 'string' || !base.isWellFormed() || !base.startsWith('/') || !base.endsWith('/') ||
      Buffer.byteLength(base) > 1024 || /[\\%?#\u0000-\u0020\u007f]/.test(base) ||
      base !== '/' && base.slice(1, -1).split('/').some(part => !part || part === '.' || part === '..')) {
    throw new Error('Server base must be a canonical path starting and ending with /.');
  }
  return base.split('/').map(encodeURIComponent).join('/');
}

/** One immutable build and its owned requests, independent of the listener. */
export async function createApplication(directory, { executable = process.env.VO_EXECUTABLE || 'vo',
  base = '/', assetBase = base, concurrency = 4, queued = 16,
  timeoutMilliseconds = 30000, admission, assetDirectories = ['assets'], onError = error => console.error(error), render = renderPage } = {}) {
  directory = await realpath(directory);
  if (!Number.isSafeInteger(timeoutMilliseconds) || timeoutMilliseconds < 1 || timeoutMilliseconds > 300000) throw new Error('Server timeout must be 1..300000 ms.');
  const basePath = deploymentBase(base), assets = deploymentBase(assetBase), publicDirectory = await realpath(join(directory, 'public'));
  if (!Array.isArray(assetDirectories) || assetDirectories.length > 32 || assetDirectories.some(name => typeof name !== 'string' || !/^[a-zA-Z0-9_-]{1,64}$/.test(name))) throw new Error('Static asset directories must be at most 32 top-level names.');
  const staticDirectories = new Set(['server', ...assetDirectories]);
  const template = await readFile(join(directory, 'server/document.html'), 'utf8');
  const composeHtml = prepareHtml(template, true);
  // Validate every required template outlet before admitting any requests.
  composeHtml('', {data:'', assets, title:'', description:''});
  const entries = new Set(await readServerEntries(directory));
  for (const entry of entries) composeHtml('', {data:'', assets, title:'', description:'', entry});
  const artifact = await realpath(join(directory, 'server/app.vob'));
  const admit = admission ?? createAdmission({concurrency, queued}), controllers = new Set(), pending = new Set();
  if (typeof admit !== 'function') throw new Error('Application admission must be a function.');
  let closing = false, retired = false;
  function dispatch(request, response, assetPath) {
    const work = handle(request, response, assetPath);
    pending.add(work);
    void work.then(() => pending.delete(work), error => {
      pending.delete(work);
      response.destroy();
      try { onError(error); } catch {}
    });
  }
  async function handle(request, response, assetPath) {
    let controller, timer, release, disconnected;
    const reply = (status, message, headers = {}) => {
      // Stop reading rejected/expired uploads. Closing after the response avoids
      // an unbounded drain while still allowing the client to receive the status.
      if (!request.readableEnded) { request.pause(); headers = {connection:'close', ...headers}; }
      if (!response.destroyed && !response.writableEnded) response.writeHead(status, {'content-type':'text/plain; charset=utf-8', 'cache-control':'no-store', ...headers}).end(request.method === 'HEAD' ? undefined : message);
    };
    try {
      if (closing || retired && assetPath === undefined) { reply(503, 'The application is stopping.'); return; }
      if (!serverMethods.includes(request.method)) { reply(405, 'Unsupported request method.', {allow:serverMethods.join(', ')}); return; }
      if (['GET', 'HEAD'].includes(request.method) && (Number(request.headers['content-length'] || 0) !== 0 || request.headers['transfer-encoding'])) {
        throw new RequestError(400, 'GET and HEAD do not accept a request body.');
      }
      const target = request.url;
      if (!target?.startsWith('/') || target.startsWith('//') || Buffer.byteLength(target) > 8192 || /[\\#\u0000-\u0020\u007f]/.test(target)) { reply(400, 'Invalid request URL.'); return; }
      let url, pathname;
      try { url = new URL(target, 'http://localhost'); pathname = decodeURIComponent(url.pathname); }
      catch { reply(400, 'Invalid request URL.'); return; }
      if (assetPath === undefined && !pathname.startsWith(base) || /[\\\u0000-\u001f\u007f]/.test(pathname)) { reply(404, 'Page not found.'); return; }
      controller = new AbortController();
      controllers.add(controller);
      disconnected = () => { if (!response.writableFinished) controller.abort(new Error('The request disconnected.')); };
      response.once('close', disconnected);
      timer = setTimeout(() => controller.abort(new Error('The request exceeded its deadline.')), timeoutMilliseconds);
      const name = assetPath ?? pathname.slice(base.length);
      const candidate = resolve(publicDirectory, name);
      if (candidate.startsWith(publicDirectory + sep)) {
        try {
          const path = await realpath(candidate);
          if (!path.startsWith(publicDirectory + sep)) { reply(404, 'File not found.'); return; }
          if ((await stat(path)).isFile()) {
            if (!['GET', 'HEAD'].includes(request.method)) { reply(405, 'Static files accept GET and HEAD.', {allow:'GET, HEAD'}); return; }
            await sendStatic(request,response,path,{root:publicDirectory,signal:controller.signal});
            return;
          }
        } catch (error) { if (!['ENOENT', 'ENOTDIR'].includes(error.code)) throw error; }
      }
      if (assetPath !== undefined || staticDirectories.has(name.split('/')[0])) { reply(404, 'File not found.'); return; }
      if (closing || response.destroyed) { reply(503, 'The application is stopping.'); return; }
      release = await admit(controller.signal);
      controller.signal.throwIfAborted();
      const body = await requestBody(request, controller.signal);
      const headers = Object.create(null);
      for (let index = 0; index < request.rawHeaders.length; index += 2) {
        const name = request.rawHeaders[index].toLowerCase();
        (headers[name] ??= []).push(request.rawHeaders[index + 1]);
      }
      const page = await render(executable, artifact, {method:request.method, url:url.pathname + url.search, basePath, headers, body}, {
        cwd:directory, env:{...process.env, VOWORK:'off'}, signal:controller.signal,
        timeoutMilliseconds,
      });
      controller.signal.throwIfAborted();
      if (page.html && !entries.has(page.entry)) throw new Error(`Server selected undeclared page entry ${JSON.stringify(page.entry)}.`);
      const output = page.kind === 'json' ? page.body : page.html ? composeHtml(page.html, {data:page.data, assets, title:page.title, description:page.description, entry:page.entry}) : '';
      const type = page.kind === 'json' ? 'application/json; charset=utf-8' : 'text/html; charset=utf-8';
      response.writeHead(page.status, {'content-type':type, 'cache-control':'no-store', ...page.headers});
      response.end(request.method === 'HEAD' ? undefined : output);
    } catch (error) {
      if (error instanceof RequestError) reply(error.status, error.message);
      else if (error instanceof ServerBusy) reply(503, 'The application is busy. Please retry.', {'retry-after':'1'});
      else if (controller?.signal.aborted && (error === controller.signal.reason || error.code === 'ABORT_ERR')) reply(504, 'This page took too long to render.');
      else { try { onError(error); } catch {} reply(500, 'This page could not be rendered.'); }
    } finally {
      clearTimeout(timer);
      if (disconnected) response.removeListener('close', disconnected);
      if (controller) controllers.delete(controller);
      release?.();
    }
  }
  let closed;
  return {
    handle: (request, response) => dispatch(request, response),
    // Explicit public-only dispatch; a missing asset never falls through to SSR.
    asset: (request, response, path) => dispatch(request, response, path),
    retire() { retired = true; return Promise.allSettled([...pending]); },
    close() {
      if (closed) return closed;
      closing = true;
      for (const controller of controllers) controller.abort(new Error('The application is stopping.'));
      return closed = Promise.allSettled([...pending]);
    },
  };
}

/** Deployable Node adapter. The public directory is the only static file root. */
export async function serveApplication(directory, {hostname = '127.0.0.1', port = 0, ...options} = {}) {
  const application = await createApplication(directory, options);
  const server = createServer(application.handle);
  try {
    await new Promise((resolveListen, reject) => {server.once('error', reject); server.listen(port, hostname, resolveListen);});
  } catch (error) {await application.close(); throw error;}
  let closed;
  return {
    url:`http://${hostname.includes(':') ? `[${hostname}]` : hostname}:${server.address().port}${deploymentBase(options.base ?? '/')}`,
    close() {
      return closed ??= (async () => {
        const stopped = new Promise(resolveClose => server.close(resolveClose));
        const disposed = application.close();
        server.closeAllConnections();
        await disposed; await stopped;
      })();
    },
  };
}
