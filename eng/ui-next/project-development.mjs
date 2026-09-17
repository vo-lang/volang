import { watch } from 'node:fs';
import { randomUUID } from 'node:crypto';
import { access, cp, mkdir, readFile, rm, writeFile } from 'node:fs/promises';
import { join, relative, resolve } from 'node:path';
import {buildProject} from './project.mjs';
import {loadProject,readProject} from './project-config.mjs';
import { serveFiles } from './static-server.mjs';
import {toolchain} from './toolchain.mjs';
import { previewBuild } from './project-preview.mjs';
import { serveDevelopmentApplication } from './server-development.mjs';
import {developmentPath} from './development-paths.mjs';

export async function previewProject(directory) {
  directory = await readProject(directory);
  const output = join(directory, 'target/ui-next/dist');
  return previewBuild(output);
}

export async function developProject(directory, { signal } = {}) {
  signal?.throwIfAborted();
  const project=await loadProject(directory);
  ({directory}=project);
  const output = join(directory, 'target/ui-next/dev');
  const configPath = join(directory, 'ui-next.json');
  const initialConfig=project.config;
  const serverMode = initialConfig.serverEntry !== undefined, session = randomUUID();
  let revisionNumber = 0, serverRoot;
  const updateServerRoot = config => {
    serverRoot = typeof config.serverEntry === 'string' ? relative(directory, resolve(directory, config.serverEntry)).replaceAll('\\', '/') : undefined;
  };
  updateServerRoot(initialConfig);
  const fallbackDocument = '<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Application preview</title><body><p role="status">Waiting for the first successful build…</p><script type="module" src="/assets/development.js"></script></body></html>';
  if (!serverMode) {
    await mkdir(output, { recursive: true });
    try { await access(join(output, 'index.html')); } catch (error) {
      if (error.code !== 'ENOENT') throw error;
      await mkdir(join(output, 'assets'), { recursive: true });
      await cp(join(toolchain.host, 'development.js'), join(output, 'assets/development.js'));
      await writeFile(join(output, 'index.html'), fallbackDocument);
    }
  }
  const server = serverMode ? await serveDevelopmentApplication({fallbackDocument})
    : await serveFiles(output, { development: true, fallbackDocument });
  const lifetime = new AbortController();
  const styles = new Set();
  let timer, building, dirty = true, closing, updateKind = 'reload';
  const rebuild = () => {
    dirty = true;
    if (building) return building;
    building = (async () => {
      while (dirty && !lifetime.signal.aborted) {
        clearTimeout(timer);
        dirty = false;
        const kind = updateKind;
        updateKind = 'guest';
        const started = performance.now();
        let unpublished;
        try {
          const config = JSON.parse(await readFile(configPath, 'utf8'));
          if ((config.serverEntry !== undefined) !== serverMode) throw new Error('Restart development after adding or removing serverEntry.');
          const version = serverMode ? `${session}-${++revisionNumber}` : String(Date.now());
          const built = await buildProject(directory, { development:true, developmentRevision:serverMode ? version : undefined, signal:lifetime.signal });
          if (serverMode) {
            unpublished = built;
            lifetime.signal.throwIfAborted();
            // Client and server compilation are separate operations. A source
            // edit during this build invalidates the whole unpublished revision.
            if (dirty) continue;
            await server.install(built, version);
            unpublished = undefined;
          }
          if (dirty) continue;
          updateServerRoot(config);
          console.log(`Application ready in ${Math.round(performance.now() - started)} ms`);
          server.broadcast({ type: kind, version });
        } catch (error) {
          if (lifetime.signal.aborted) return;
          if (dirty) continue;
          console.error(error.message);
          server.broadcast({ type: 'error', message: error.message });
        } finally {
          if (unpublished) await rm(unpublished, {recursive:true, force:true});
        }
      }
    })().finally(() => { building = undefined; });
    return building;
  };
  const watcher = watch(directory, { recursive: true }, (_event, filename) => {
    const path = developmentPath(filename);
    if (!path || lifetime.signal.aborted) return;
    if (path.startsWith('web/') && path.endsWith('.css') && !building) {
      // Only a stylesheet edit can skip the build. A file removal needs a full
      // output refresh, so a failed copy falls through to the regular rebuild.
      const style = (serverMode ? server.updateStyles(join(directory, path), relative('web', path))
        : cp(join(directory, path), join(output, relative('web', path)))).then(() => {
        if (!lifetime.signal.aborted) server.broadcast({ type: 'styles' });
      }).catch(() => {if (!lifetime.signal.aborted) {updateKind = 'reload'; return rebuild();}});
      styles.add(style);
      void style.then(() => styles.delete(style), () => styles.delete(style));
      return;
    }
    dirty = true;
    if (!path.endsWith('.vo') || serverMode && serverRoot !== undefined && (serverRoot === '' || path === serverRoot || path.startsWith(serverRoot + '/'))) updateKind = 'reload';
    clearTimeout(timer);
    timer = setTimeout(rebuild, 100);
  });
  const close = () => closing ??= (async () => {
    signal?.removeEventListener('abort', close);
    lifetime.abort();
    clearTimeout(timer);
    watcher.close();
    await building;
    await Promise.allSettled([...styles]);
    await server.close();
  })();
  signal?.addEventListener('abort', close, { once: true });
  if (signal?.aborted) { await close(); signal.throwIfAborted(); }
  watcher.on('error', error => { server.broadcast({ type: 'error', message: error.message }); void close(); });
  try { await rebuild(); } catch (error) { await close(); throw error; }
  return { ...server, url: server.url + '?backend=vm', close };
}
