import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {createHash} from 'node:crypto';
import {cp, mkdir, mkdtemp, readFile, readdir, rm, writeFile} from 'node:fs/promises';
import {createRequire} from 'node:module';
import {tmpdir} from 'node:os';
import {join, relative, resolve} from 'node:path';
import {build} from 'esbuild';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {root} from './repository-paths.mjs';
import {serveFiles} from './static-server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const output = join(root, 'target/ui-next/public-host-check');
const temporary = await mkdtemp(join(tmpdir(), 'volang-public-host-'));
const source = join(root, 'lang/crates/vo-web');
const fixture = 'ui/next/tests/public_host/main.vo';
const consumer = join(temporary, 'consumer');
const npm = process.platform === 'win32' ? 'npm.cmd' : 'npm';
const report = {schema: 'volang.ui-public-host.v1', passed: false, checks: {}, inputs: [], artifacts: [], cases: []};

function run(command, args, cwd = root) {
  const result = spawnSync(command, args, {cwd, encoding: 'utf8', timeout: 120_000,
    maxBuffer: 32 * 1024 * 1024,
    env: {...process.env, VOWORK: 'off', npm_config_cache: join(temporary, 'npm-cache')}});
  assert.ifError(result.error);
  assert.equal(result.status, 0, result.stdout + result.stderr);
  return result.stdout;
}

async function identity(path) {
  const bytes = await readFile(path);
  return {path: relative(root, path).split('\\').join('/'), bytes: bytes.length,
    sha256: createHash('sha256').update(bytes).digest('hex')};
}

await mkdir(output, {recursive: true});
await rm(join(output, 'report.json'), {force: true});
try {
  // npm decides archive membership. The consumer has no repository package links
  // and installs exactly that archive without fetching or executing package code.
  const staging = join(temporary, 'package');
  await mkdir(staging);
  const metadata = JSON.parse(await readFile(join(source, 'package.json')));
  const notices = (await readdir(source)).filter(name => /^(readme|licen[cs]e|copying)(\.|$)/i.test(name));
  for (const name of new Set(['package.json', 'prepare-package.mjs', ...notices, ...metadata.files])) {
    await cp(join(source, name), join(staging, name), {recursive: true});
  }
  // Reproduce fresh wasm-pack output. A previous local prepack must not hide a
  // missing preparation hook, and packaging must leave canonical outputs alone.
  for (const directory of ['pkg']) await rm(join(staging, directory, '.npmignore'), {force: true});
  const [packed] = JSON.parse(run(npm, ['pack', '--json', '--offline', '--pack-destination', temporary], staging));
  report.checks.freshPackageStaging = true;
  await mkdir(consumer);
  await writeFile(join(consumer, 'package.json'), JSON.stringify({private: true, type: 'module'}));
  run(npm, ['install', '--offline', '--ignore-scripts', '--no-audit', '--no-fund', '--package-lock=false', join(temporary, packed.filename)], consumer);
  const packageRoot = join(consumer, 'node_modules/vo-web');
  const manifest = JSON.parse(await readFile(join(packageRoot, 'package.json')));
  const entry = manifest.exports['./ui/next'];
  assert.equal(entry.import, './dist/ui_next/index.js');
  assert.equal(entry.types, './dist/ui_next/index.d.ts');
  for (const [name, path] of Object.entries({'.': 'dist/index', './vfs': 'dist/vfs', './wasm': 'pkg/vo_web'})) {
    assert.equal(manifest.exports[name]?.import, './' + path + '.js', name);
    assert.equal(manifest.exports[name]?.types, './' + path + '.d.ts', name);
  }
  for (const value of Object.values(manifest.exports)) {
    await readFile(join(packageRoot, value.import));
    await readFile(join(packageRoot, value.types));
  }
  assert.match(await readFile(join(packageRoot, 'ui-next.md'), 'utf8'), /preview/);
  for (const file of packed.files) {
    const installed = await readFile(join(packageRoot, file.path));
    assert.deepEqual(installed, await readFile(join(source, file.path)), file.path);
    report.inputs.push(await identity(join(source, file.path)));
  }
  const archive = join(output, packed.filename);
  await cp(join(temporary, packed.filename), archive);
  report.artifacts.push(await identity(archive));
  report.package = {name: manifest.name, version: manifest.version, files: packed.files.length, export: entry};
  report.checks.archiveInstalled = report.checks.existingExportsRetained = true;

  await cp(join(root, 'eng/ui-next/fixtures/public-host.ts'), join(consumer, 'consumer.ts'));
  await cp(join(root, 'target/ui-next/wasm-runtime'), join(consumer, 'runtime'), {recursive: true});
  run(process.execPath, [join(source, 'node_modules/typescript/bin/tsc'), '--strict', '--noEmit',
    '--module', 'NodeNext', '--moduleResolution', 'NodeNext', '--target', 'ES2022',
    '--lib', 'ES2022,ESNext.Disposable,DOM,DOM.Iterable', 'consumer.ts'], consumer);
  report.checks.strictConsumerTypes = true;
  // Type checking and bundling resolve the installed package, including dynamic
  // imports. Runtime asset delivery is exercised separately with native ESM.
  const bundle = await build({absWorkingDir: consumer, entryPoints: ['consumer.ts'],
    outdir: join(consumer, 'bundle'), bundle: true, format: 'esm', splitting: true,
    platform: 'browser', target: 'es2022', metafile: true, logLevel: 'silent'});
  const inputs = Object.keys(bundle.metafile.inputs);
  assert(inputs.some(path => path.endsWith('ui_next/mount.js')));
  assert(!inputs.some(path => /node_modules\/(?:codemirror|@codemirror|uplot)/.test(path)));
  report.checks.browserBundle = report.checks.heavyWidgetsExcluded = true;
  await writeFile(join(output, 'bundle.json'), JSON.stringify(bundle.metafile, null, 2) + '\n');
  report.artifacts.push(await identity(join(output, 'bundle.json')));

  for (const [suffix, args] of [
    ['vob', ['emit', 'bytecode', fixture]],
  ]) run(compilerPath(), [...args, '-o', join(output, 'app.' + suffix)]);
  const html = run(compilerPath(), ['run', fixture, '--', '--ssr']);
  assert(html.startsWith('<!--vo:r:1-->'));
  await writeFile(join(output, 'app.html'), html);
  for (const suffix of ['vob', 'html']) {
    const path = join(output, 'app.' + suffix);
    report.artifacts.push(await identity(path));
    await cp(path, join(consumer, 'app.' + suffix));
  }
  for (const path of [fixture, 'eng/ui-next/fixtures/public-host.ts', 'eng/ui-next/public-host-contracts.mjs',
    'lang/crates/vo-web/js/ui_next/index.ts', 'lang/crates/vo-web/prepare-package.mjs', compilerPath(),
    ...['vo_web.js', 'vo_web_bg.wasm', 'vo_web.d.ts'].map(name => 'target/ui-next/wasm-runtime/' + name)]) {
    report.inputs.push(await identity(resolve(root, path)));
  }
  // Keep the package's directory layout so wasm-bindgen
  // assets resolve as they will in a browser module host.
  await build({absWorkingDir: consumer, entryPoints: ['consumer.ts'], outfile: join(consumer, 'boot.js'),
    bundle: false, format: 'esm', target: 'es2022', logLevel: 'silent'});
  for (const hydrate of [false, true]) {
    const document = `<!doctype html><html><head><meta charset="utf-8"><title>Public host contract</title>
      <script type="importmap">${JSON.stringify({imports: {
        'vo-web/ui/next': './node_modules/vo-web/' + entry.import.slice(2),
        'vo-web/wasm': './node_modules/vo-web/' + manifest.exports['./wasm'].import.slice(2),
      }})}</script>
      </head><body><div id="first" data-root>${hydrate ? html : ''}</div><div id="second" data-root>${hydrate ? html : ''}</div>
      <script type="module">window.initialButtons=Array.from(document.querySelectorAll('[data-counter]'));</script>
      <script type="module" src="./boot.js"></script></body></html>`;
    await writeFile(join(consumer, hydrate ? 'hydrate.html' : 'index.html'), document);
  }
  const server = await serveFiles(consumer);
  const engines = createRequire(join(root, 'eng/browser/package.json'))('playwright');
  try {
    for (const engine of ['chromium', 'firefox', 'webkit']) {
      const browser = await engines[engine].launch({headless: true});
      try {
        for (const [backend, runtime] of [['vm', 'minimal'], ['vm', 'package']]) for (const hydrate of [false, true]) {
          const page = await browser.newPage();
          page.setDefaultTimeout(30_000);
          const errors = [];
          page.on('pageerror', error => errors.push(String(error)));
          try {
            await page.goto(server.url + (hydrate ? 'hydrate.html' : 'index.html') + '?backend=' + backend + (hydrate ? '&hydrate' : '') + (runtime === 'package' ? '&packaged' : ''));
            await page.waitForFunction(() => window.mounted || window.failures?.length);
            assert.deepEqual(await page.evaluate(() => window.failures), []);
            if (hydrate) assert(await page.evaluate(() => window.initialButtons.every((button, i) => button === document.querySelectorAll('[data-counter]')[i])), 'SSR nodes must be adopted');
            await page.waitForFunction(() => window.owners.length === 2 && window.owners.every(owner => owner.watches === 1 && owner.widgets === 1));
            const first = page.locator('#first'), second = page.locator('#second');
            await first.locator('[data-counter]').click();
            await page.waitForFunction(() => document.querySelector('#first [data-counter]')?.textContent === 'Count: 1');
            assert.equal(await second.locator('[data-counter]').textContent(), 'Count: 0');
            await first.locator('input').fill('中文 😀');
            await page.waitForFunction(() => document.querySelector('#first [data-greeting]')?.textContent === 'Hello, 中文 😀');
            assert.equal(await second.locator('input').inputValue(), '');
            assert.equal(await first.locator('[data-greeting]').getAttribute('data-match'), 'true');
            assert.equal(await second.locator('[data-greeting]').getAttribute('data-match'), 'false');
            await page.evaluate(() => { window.owners[0].emit('first-only'); window.owners[1].emit('second-only'); });
            await page.waitForFunction(() => document.querySelector('#first [data-message]')?.textContent === 'first-only' && document.querySelector('#second [data-message]')?.textContent === 'second-only');
            assert.equal(await first.locator('[data-badge]').textContent(), '1');
            await page.evaluate(async () => { window.applications[0].close(); window.applications[0].close(); await window.applications[0].done; });
            assert.equal(await first.evaluate(element => element.childNodes.length), 0);
            assert.deepEqual(await page.evaluate(() => window.owners.map(({watches, widgets, watchAborts, widgetDisposals}) => ({watches, widgets, watchAborts, widgetDisposals}))), [
              {watches: 0, widgets: 0, watchAborts: 1, widgetDisposals: 1}, {watches: 1, widgets: 1, watchAborts: 0, widgetDisposals: 0},
            ]);
            await second.locator('[data-counter]').click();
            await page.waitForFunction(() => document.querySelector('#second [data-counter]')?.textContent === 'Count: 1');
            assert.equal(await page.evaluate(() => window.remount()), true);
            await page.waitForFunction(() => window.owners.length === 3 && window.owners[2].widgets === 1);
            await page.evaluate(() => window.owners[0].emit('stale completion'));
            await first.locator('[data-counter]').click();
            await page.waitForFunction(() => document.querySelector('#first [data-counter]')?.textContent === 'Count: 1');
            assert.equal(await first.locator('[data-message]').textContent(), 'first');
            assert.equal(await first.locator('input').inputValue(), '');
            assert.equal(await second.locator('[data-counter]').textContent(), 'Count: 1');
            await page.evaluate(async () => {
              for (const application of window.applications) { application.close(); application.close(); }
              await Promise.all(window.applications.map(application => application.done));
            });
            assert(await page.evaluate(() => window.owners.every(owner => owner.watches === 0 && owner.widgets === 0 && owner.watchAborts === 1 && owner.widgetDisposals === 1)));
            assert(await page.locator('[data-root]').evaluateAll(roots => roots.every(element => element.childNodes.length === 0)));
            assert.deepEqual(await page.evaluate(() => window.failures), []);
            assert.deepEqual(errors, []);
            report.cases.push({engine, browserVersion: browser.version(), backend, runtime, hydrate, passed: true,
              independentState: true, nativeInput: true, regexp: true, scopedProviders: true, closeIsolation: true, remount: true, staleCompletion: true, cleanShutdown: true});
            console.log(`Public host: ${engine} ${backend} ${runtime} ${hydrate ? 'hydrate' : 'client'} passed`);
          } catch (error) {
            report.failure = {engine, backend, runtime, hydrate, errors,
              state: await page.evaluate(() => ({html: document.body.innerHTML, failures: window.failures, owners: window.owners})).catch(() => null)};
            throw error;
          } finally { await page.close(); }
        }
      } finally { await browser.close(); }
    }
  } finally { await server.close(); }
  report.passed = true;
} catch (error) {
  report.error = String(error.stack ?? error);
  throw error;
} finally {
  await writeFile(join(output, 'report.json'), JSON.stringify(report, null, 2) + '\n');
  await rm(temporary, {recursive: true, force: true});
}
console.log('Public host: actual npm archive, strict consumer, bundle and 18 browser cases passed.');
