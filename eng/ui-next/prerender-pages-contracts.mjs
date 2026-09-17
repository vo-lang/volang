import assert from 'node:assert/strict';
import { spawnSync } from 'node:child_process';
import { mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { createProject, buildProject } from './project.mjs';
import { developProject } from './project-development.mjs';
import { serveFiles } from './static-server.mjs';
import { compilerPath } from '../../lang/crates/vo-web/test_compiler.mjs';
import { root } from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const engines = await import('../browser/node_modules/playwright/index.mjs');
const temporary = await mkdtemp(resolve(root, 'target/ui-next/static-pages-contract-'));
const reports = [];
let browser, server;
try {
  const directory = await createProject(join(temporary, 'A small site 中文'));
  const configPath = join(directory, 'ui-next.json');
  const config = JSON.parse(await readFile(configPath));
  config.prerenderPages = [
    { path: '/', data: 'Ada' },
    { path: '/people/Grace', data: '</script><script>globalThis.pageDataInjected=true</script> 中文 $&',
      title: '</title> Grace & 中文', description: '\" onload=\"literal <description> & 中文' },
    { path: '/a small idea/中文/', data: '小林', title: '小林的小小想法', description: '' },
  ];
  await writeFile(configPath, JSON.stringify(config));
  const distribution = await buildProject(directory);
  const before = await readFile(join(distribution, 'build-report.json'), 'utf8');
  const build = JSON.parse(before);
  assert.equal(build.pages.length, 3);
  assert(build.prerender.artifact.sha256);
  assert.equal(build.artifacts.filter(artifact => artifact.path.endsWith('.vob')).length, 1, 'server bytecode leaked into the static distribution');
  server = await serveFiles(distribution, { base: '/ideas/' });
  const redirect = await fetch(server.url + 'people/Grace?backend=vm', { redirect: 'manual' });
  assert.equal(redirect.status, 308);
  assert.equal(redirect.headers.get('location'), '/ideas/people/Grace/?backend=vm');
  assert.equal((await fetch(server.url + 'a%20small%20idea/%E4%B8%AD%E6%96%87/', { method: 'HEAD' })).status, 200);
  assert.equal((await fetch(server.url + 'unknown/')).status, 404);
  for (const engine of ['chromium', 'firefox', 'webkit']) {
    browser = await engines[engine].launch({ headless: true });
    for (const [index, item] of config.prerenderPages.entries()) {
      const path = build.pages[index].path;
      const url = new URL(path.slice(1), server.url);
      const staticPage = await browser.newPage({ javaScriptEnabled: false });
      await staticPage.goto(url.href);
      assert.equal(await staticPage.title(), item.title ?? config.document.title);
      assert.equal(await staticPage.locator('meta[name=description]').getAttribute('content'), item.description ?? config.document.description);
      assert.equal(await staticPage.getByRole('heading').textContent(), `Make something good, ${item.data}.`);
      assert.equal(await staticPage.locator('#name').inputValue(), item.data);
      await staticPage.close();
      for (const backend of ['vm']) {
        const page = await browser.newPage();
        const errors = [], requests = [];
        page.on('pageerror', error => errors.push(error.message));
        page.on('request', request => requests.push(request.url()));
        let release;
        const gate = new Promise(resolveGate => { release = resolveGate; });
        await page.route('**/assets/app.*', async route => { await gate; await route.continue(); });
        url.searchParams.set('backend', backend);
        await page.goto(url.href, { waitUntil: 'commit' });
        await page.locator('#name').fill('Before startup ' + backend);
        await page.evaluate(() => { window.originalInput = document.getElementById('name'); });
        release();
        await page.getByRole('heading', { name: `Make something good, Before startup ${backend}.` }).waitFor();
        assert.equal(await page.title(), item.title ?? config.document.title);
        assert.equal(await page.evaluate(() => window.originalInput === document.getElementById('name')), true);
        assert.equal(await page.evaluate(() => window.pageDataInjected), undefined);
        await page.getByRole('button', { name: 'Make it happen' }).click();
        await page.waitForFunction(() => document.querySelector('output')?.textContent === '1 little steps');
        assert(requests.every(request => request.startsWith(server.url)), 'nested page fetched assets outside its deployment base');
        assert.deepEqual(errors, []);
        reports.push({ engine, backend, path, passed: true });
        await page.close();
      }
    }
    await browser.close(); browser = undefined;
  }
  await server.close(); server = undefined;

  const collision = join(directory, 'web/people/Grace');
  await mkdir(collision, { recursive: true });
  await writeFile(join(collision, 'index.html'), 'authored page');
  await assert.rejects(buildProject(directory), /collides with an authored web file/);
  assert.equal(await readFile(join(distribution, 'build-report.json'), 'utf8'), before);
  await rm(join(directory, 'web/people'), { recursive: true });
  const entryPath = join(directory, 'prerender/main.vo');
  const entry = await readFile(entryPath, 'utf8');
  await writeFile(entryPath, entry.replace('import (', 'import (\n\tui "github.com/vo-lang/ui/next"')
    .replace('prerender.Run(app.View)', `prerender.Run(func(initial string) ui.View {
      if initial == "broken" { panic("one page failed") }
      return app.View(initial)
    })`));
  config.prerenderPages[1].data = 'broken';
  await writeFile(configPath, JSON.stringify(config));
  await assert.rejects(buildProject(directory), /one page failed/);
  assert.equal(await readFile(join(distribution, 'build-report.json'), 'utf8'), before, 'one failed page replaced the working site');
  await writeFile(entryPath, entry);
  for (const input of [Buffer.from([255]), Buffer.alloc(1024 * 1024 + 1, 97)]) {
    const result = spawnSync(compilerPath(), ['run', join(directory, 'prerender')], {
      cwd: directory, env: { ...process.env, VOWORK: join(directory, 'vo.work') }, input, timeout: 30000,
    });
    assert.ifError(result.error);
    assert.notEqual(result.status, 0);
    assert.match(result.stderr.toString(), /valid UTF-8 within 1 MiB/);
    assert.equal(result.stdout.length, 0);
  }

  // Development emits the same paths/data with client rendering, so direct
  // navigation remains useful while pages are being edited.
  config.prerenderPages[1].data = 'Grace';
  await writeFile(configPath, JSON.stringify(config));
  const development = await buildProject(directory, { development: true });
  server = await serveFiles(development, { development: true, base: '/ideas/' });
  browser = await engines.chromium.launch({ headless: true });
  const page = await browser.newPage();
  await page.goto(server.url + 'people/Grace/?backend=vm');
  await page.getByRole('heading', { name: 'Make something good, Grace.' }).waitFor();
  assert.equal(await page.locator('meta[name="ui-next-render"]').getAttribute('content'), 'client');
  await page.getByRole('button', { name: 'Make it happen' }).click();
  await page.waitForFunction(() => document.querySelector('output')?.textContent === '1 little steps');
  await page.close(); await server.close(); server = undefined;

  // Start without a successful development output. The nested document must
  // show diagnostics and remain connected until a corrected build can load it.
  await rm(development, { recursive: true });
  const invalid = join(directory, 'app/invalid.vo');
  await writeFile(invalid, 'package app\nfunc unfinished(\n');
  server = await developProject(directory);
  const origin = new URL('/', server.url).href;
  const nested = await browser.newPage();
  const errors = [];
  nested.on('pageerror', error => errors.push(error.message));
  const response = await nested.goto(origin + 'people/Grace/?backend=vm');
  assert.equal(response.status(), 503);
  await nested.locator('#ui-development-error').waitFor();
  assert.equal((await fetch(origin + 'assets/missing.js')).status, 404);
  await rm(invalid);
  await nested.getByRole('heading', { name: 'Make something good, Grace.' }).waitFor();
  assert.equal(await nested.title(), config.prerenderPages[1].title);
  await nested.locator('#name').fill('Grace editing');
  await nested.getByRole('heading', { name: 'Make something good, Grace editing.' }).waitFor();
  await nested.getByRole('button', { name: 'Make it happen' }).click();
  await nested.waitForFunction(() => document.querySelector('output')?.textContent === '1 little steps');
  await nested.evaluate(() => { window.beforeCss = document.getElementById('name'); });
  const cssPath = join(directory, 'web/app.css');
  await writeFile(cssPath, (await readFile(cssPath, 'utf8')) + '\nbody { --nested-css-contract: ready; }\n');
  await nested.waitForFunction(() => getComputedStyle(document.body).getPropertyValue('--nested-css-contract').trim() === 'ready');
  assert.equal(await nested.evaluate(() => window.beforeCss === document.getElementById('name')), true);
  const appPath = join(directory, 'app/app.vo');
  await writeFile(appPath, (await readFile(appPath, 'utf8')).replace('Make it happen', 'Keep it going'));
  await nested.getByRole('button', { name: 'Keep it going' }).waitFor();
  assert.equal(await nested.locator('#name').inputValue(), 'Grace editing');
  assert.equal(await nested.locator('output').textContent(), '1 little steps');
  assert.equal((await nested.goto(origin + 'unknown/')).status(), 404);
  assert.deepEqual(errors, []);
  await writeFile(resolve(root, 'target/ui-next/prerender-pages-report.json'), JSON.stringify({
    passed: true, build, reports,
    contracts: ['one-compiled-prerender-image', 'isolated-page-processes', 'initial-data-stream', 'inert-page-data', 'page-title-and-description',
      'no-javascript-content', 'nested-subdirectory-assets', 'early-input-adoption', 'canonical-directory-redirect',
      'missing-page-404', 'authored-page-collision', 'failed-page-preserves-whole-site', 'native-data-bounds', 'development-nested-pages',
      'nested-first-build-diagnostic-recovery', 'nested-live-styles', 'nested-stateful-reload'],
  }, null, 2) + '\n');
  console.log('Static page contracts passed: three pages, three engines, VM and preserved early input');
} finally {
  await browser?.close(); await server?.close();
  await rm(temporary, { recursive: true, force: true });
}
