import { spawn } from 'node:child_process';
import { watch } from 'node:fs';
import { mkdir, rename, rm, access } from 'node:fs/promises';
import { resolve, relative } from 'node:path';
import { compilerPath } from '../../lang/crates/vo-web/test_compiler.mjs';
import { serve, root } from './server.mjs';
import { buildPlaygroundSources } from './playground-sources.mjs';
import { buildBrowserLibraries } from './browser-libraries.mjs';
import { verifyStudioDocuments } from './studio-documents.mjs';
import {unitContracts} from './core-contracts.mjs';
import {developmentPath} from './development-paths.mjs';

export async function runStudioCommand(command) {
  await verifyStudioDocuments(root);
  const run = (executable, args, options = {}) => new Promise((resolve, reject) => {
    const child = spawn(executable, args, { cwd: root, env: process.env, stdio: 'inherit', ...options });
    child.once('error', reject);
    child.once('exit', code => code === 0 ? resolve() : reject(new Error(`${executable} exited with status ${code}`)));
  });
  const node = (...args) => run(process.execPath, args);
  const buildHost = () => run(process.platform === 'win32' ? 'npm.cmd' : 'npm', ['--prefix', 'lang/crates/vo-web', 'run', 'build:js']);

  if (command === 'build' || command === 'check') {
    await node('eng/ui-next/generate.mjs', '--check');
    await buildHost();
    await node('eng/ui-next/build.mjs');
    if (command === 'check') {
      await node('--test', ...await unitContracts());
      for (const engine of process.env.UI_NEXT_BROWSER ? [process.env.UI_NEXT_BROWSER] : ['chromium', 'firefox', 'webkit']) {
        await run(process.execPath, ['eng/ui-next/check.mjs'], { env: { ...process.env, UI_NEXT_BROWSER: engine } });
      }
      await node('eng/ui-next/development-contracts.mjs');
      await node('eng/ui-next/project-contracts.mjs');
      await node('eng/ui-next/project-editor-contracts.mjs');
      await node('eng/ui-next/portal-dom-contracts.mjs');
      await node('eng/ui-next/portal-project-contracts.mjs');
      await node('eng/ui-next/field-array-project-contracts.mjs');
      await node('eng/ui-next/file-project-contracts.mjs');
      await node('eng/ui-next/page-entries-contracts.mjs');
      await node('eng/ui-next/server-page-entries-contracts.mjs');
      await node('eng/ui-next/project-testing-contracts.mjs');
      await node('eng/ui-next/toolchain-contracts.mjs');
      await node('eng/ui-next/fieldnotes-contracts.mjs');
      await node('eng/ui-next/prerender-pages-contracts.mjs');
      await node('eng/ui-next/server-contracts.mjs');
      await node('eng/ui-next/server-generation-contracts.mjs');
      await node('eng/ui-next/server-development-contracts.mjs');
      await node('eng/ui-next/reload-contracts.mjs');
      await node('eng/ui-next/cli.mjs', 'build', '--studio');
      await node('eng/ui-next/studio-build-contracts.mjs');
      await node('eng/ui-next/studio-distribution-contracts.mjs');
      await node('eng/ui-next/studio-static-build-contracts.mjs');
      await node('eng/ui-next/studio-static-contracts.mjs');
      await node('eng/ui-next/studio-upgrade-contracts.mjs');
      await node('eng/ui-next/asset-delivery-contracts.mjs');
    }
  } else {
    await access(resolve(root, 'target/ui-next/wasm-runtime/vo_web_bg.wasm'));
    let active;
    let closing = false;
    let pending = false;
    let needsHost = false;
    let reloadPage = true;
    let timer;
    const watchers = [];
    const application = await serve({ development: command === 'dev' });
    const compile = async () => {
      await verifyStudioDocuments(root);
      await run(compilerPath(), ['check', 'apps/studio/next'], { env: { ...process.env, VOWORK: resolve(root, 'vo.work') } });
      return new Promise((resolveBuild, reject) => {
      const staged = resolve(root, `target/ui-next/studio-dev-${process.pid}.vob`);
      active = spawn(compilerPath(), ['emit', 'bytecode', 'apps/studio/next/development', '-o', staged], {
        cwd: root, env: { ...process.env, VOWORK: resolve(root, 'vo.work') }, stdio: ['ignore', 'pipe', 'pipe'],
      });
      let output = '';
      const append = bytes => { output = (output + bytes).slice(-32_768); };
      active.stdout.on('data', append);
      active.stderr.on('data', append);
      active.once('error', reject);
      active.once('exit', async code => {
        active = undefined;
        try {
          if (code !== 0 || closing) { await rm(staged, { force: true }); throw new Error(output || 'Compilation stopped.'); }
          await rename(staged, resolve(root, 'target/ui-next/studio-dev.vob'));
          resolveBuild();
        } catch (error) { reject(error); }
      });
      });
    };
    let building = false;
    const rebuild = async () => {
      pending = true;
      if (building) return;
      building = true;
      while (pending && !closing) {
        pending = false;
        const started = performance.now();
        const kind = reloadPage || needsHost ? 'reload' : 'guest';
        reloadPage = false;
        try {
          if (needsHost) { needsHost = false; await node('eng/ui-next/generate.mjs', '--check'); await buildHost(); await buildBrowserLibraries(); }
          if (closing) break;
          await compile();
          await buildPlaygroundSources();
          console.log(`Studio ready in ${Math.round(performance.now() - started)} ms`);
          application.broadcast({ type: kind, version: String(Date.now()) });
        } catch (error) {
          console.error(error.message);
          application.broadcast({ type: 'error', message: error.message });
        }
      }
      building = false;
    };
    const close = async () => {
      if (closing) return;
      closing = true;
      clearTimeout(timer);
      for (const watcher of watchers) watcher.close();
      active?.kill('SIGTERM');
      await application.close();
    };
    for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, close);
    if (command === 'dev') {
      await mkdir(resolve(root, 'target/ui-next'), { recursive: true });
      needsHost = true;
      for (const directory of ['ui/next', 'apps/studio/next', 'lang/crates/vo-web/js']) {
        watchers.push(watch(resolve(root, directory), { recursive: true }, (_event, filename) => {
          const changed = developmentPath(filename);
          if (!changed || closing) return;
          const path = relative(root, resolve(root, directory, changed)).replaceAll('\\', '/');
          if (/\.(css)$/.test(path)) { application.broadcast({ type: 'styles' }); return; }
          if (!/\.(vo|ts|js|html|json)$/.test(path) || path.includes('/tests/') || path.includes('/examples/')) return;
          needsHost ||= directory.startsWith('lang/') || path.endsWith('wire.schema.json');
          reloadPage ||= !path.endsWith('.vo');
          clearTimeout(timer);
          timer = setTimeout(rebuild, 100);
        }));
      }
      await rebuild();
    }
    console.log(`${application.url}/studio/gallery${command === 'dev' ? '?backend=vm' : ''}`);
  }
}
