import assert from 'node:assert/strict';
import { cp, mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { createProject, checkProject, buildProject } from './project.mjs';
import { developProject } from './project-development.mjs';
import { serveFiles } from './static-server.mjs';
import { root } from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const engines = await import('../browser/node_modules/playwright/index.mjs');
const temporary = await mkdtemp(resolve(root, 'target/ui-next/project-contract-'));
let browser, server, development;
const reports = [];
try {
  const project = await createProject(join(temporary, 'A small idea 中文'));
  const configPath = join(project, 'ui-next.json');
  const initialConfig = JSON.parse(await readFile(configPath, 'utf8'));
  await writeFile(configPath, JSON.stringify({...initialConfig,defaultBackend:'vm'}));
  await writeFile(join(project, 'web/away.html'), '<!doctype html><title>Another document</title><h1>Another document</h1>');
  const manifest = await readFile(join(project, 'vo.mod'), 'utf8');
  const lock = await readFile(join(project, 'vo.lock'), 'utf8');
  await assert.rejects(createProject(project), { code: 'EEXIST' });
  assert.equal(await readFile(join(project, 'vo.mod'), 'utf8'), manifest);
  await checkProject(project);
  // Exercise portable language services from the deployed VM bundle.
  const sourcePath = join(project, 'main.vo');
  const source = (await readFile(sourcePath, 'utf8')).replace('import (', 'import (\n\t"regexp"')
    .replace('host.RunWithData(', 'if !regexp.MustCompile("^[A-Z]+$").MatchString("READY") { panic("regexp fixture failed") }\n\thost.RunWithData(');
  await writeFile(sourcePath, source);
  const output = await buildProject(project);
  assert.equal(await readFile(join(project, 'vo.lock'), 'utf8'), lock, 'build rewrote the selected dependency graph');
  const reportBefore = await readFile(join(output, 'build-report.json'), 'utf8');
  const production = JSON.parse(reportBefore);
  assert.equal(production.inspection, false);
  assert.equal(production.prerender.entry, 'prerender');
  assert(production.prerender.htmlBytes > 0);
  const serverHtml = await readFile(join(output, 'index.html'), 'utf8');
  assert(serverHtml.includes('A small idea, ready to grow.') && serverHtml.includes('data-vo-id='));
  assert(!serverHtml.includes('<!--ui-next:'));
  assert(serverHtml.includes('<meta name="ui-next-backend" content="vm">'));
  const productionGuest = await readFile(join(output, 'assets/app.vob'));
  for (const name of ['inspect', 'develop']) {
    assert.equal(productionGuest.includes(Buffer.from(`github.com/vo-lang/ui/next/${name}`)), false, `production bytecode linked ${name}`);
  }
  for (const artifact of production.artifacts.filter(item => item.path.endsWith('.js'))) {
    const source = await readFile(join(output, artifact.path), 'utf8');
    for (const marker of ['Inspect components', 'vo-ui-reload', 'ui.reload.request']) {
      assert.equal(source.includes(marker), false, `production JavaScript includes development marker ${marker}`);
    }
  }
  await writeFile(join(project, 'web/build-report.json'), 'authored file');
  await assert.rejects(buildProject(project), /reserved for generated output/);
  assert.equal(await readFile(join(output, 'build-report.json'), 'utf8'), reportBefore);
  await rm(join(project, 'web/build-report.json'));
  await writeFile(configPath, JSON.stringify({...initialConfig,defaultBackend:'jit'}));
  await assert.rejects(buildProject(project), /defaultBackend must be/);
  assert.equal(await readFile(join(output, 'build-report.json'), 'utf8'), reportBefore);
  await writeFile(configPath, JSON.stringify({...initialConfig,defaultBackend:'vm'}));
  const invalid = join(project, 'invalid.vo');
  await writeFile(invalid, 'package main\nfunc unfinished(\n');
  await assert.rejects(buildProject(project));
  assert.equal(await readFile(join(output, 'build-report.json'), 'utf8'), reportBefore, 'failed build replaced a working distribution');
  await rm(invalid);

  const invalidRender = join(project, 'prerender/invalid.vo');
  await writeFile(invalidRender, 'package main\nfunc unfinished(\n');
  await assert.rejects(buildProject(project), /unfinished|parse error|expected/);
  assert.equal(await readFile(join(output, 'build-report.json'), 'utf8'), reportBefore, 'failed prerender replaced the distribution');
  await rm(invalidRender);
  const htmlPath = join(project, 'web/index.html'), template = await readFile(htmlPath, 'utf8');
  await writeFile(htmlPath, template.replace('<!--ui-next:content-->', ''));
  await assert.rejects(buildProject(project), /exactly one/);
  assert.equal(await readFile(join(output, 'build-report.json'), 'utf8'), reportBefore);
  await writeFile(htmlPath, template);
  await writeFile(htmlPath, template.replace('<!--ui-next:backend-->', 'vm'));
  await assert.rejects(buildProject(project), /defaultBackend requires/);
  assert.equal(await readFile(join(output, 'build-report.json'), 'utf8'), reportBefore);
  await writeFile(htmlPath, template);

  const deployed = join(temporary, 'deployed');
  await cp(output, deployed, { recursive: true });
  server = await serveFiles(deployed, { base: '/ideas/' });
  assert.equal((await fetch(server.url + 'vo.mod')).status, 404);
  assert.equal((await fetch(server.url + 'index.html', { method: 'POST' })).status, 405);
  for (const engine of ['chromium', 'firefox', 'webkit']) {
    browser = await engines[engine].launch({ headless: true });
    const readable = await browser.newPage({ javaScriptEnabled: false });
    await readable.goto(server.url);
    await readable.getByRole('heading', { name: 'A small idea, ready to grow.' }).waitFor();
    assert.equal(await readable.locator('[data-count]').textContent(), '0 little steps');
    await readable.close();
    for (const selection of ['default', 'vm']) {
      const backend = selection === 'default' ? 'vm' : selection;
      const page = await browser.newPage({ viewport: { width: 390, height: 844 } });
      const errors = [], requests = [];
      page.on('pageerror', error => errors.push(String(error)));
      page.on('request', request => requests.push(request.url()));
      let release;
      const gate = new Promise(resolve => { release = resolve; });
      await page.route('**/assets/app.*', async route => { await gate; await route.continue(); });
      await page.goto(server.url + (selection === 'default' ? '' : `?backend=${backend}`), { waitUntil: 'commit' });
      await page.getByRole('heading', { name: 'A small idea, ready to grow.' }).waitFor();
      await page.locator('#name').fill('Before startup 中文');
      await page.evaluate(() => { window.serverInput = document.getElementById('name'); });
      release();
      await page.getByRole('heading', { name: 'Make something good, Before startup 中文.' }).waitFor();
      assert.equal(await page.evaluate(() => window.serverInput === document.getElementById('name')), true, 'static HTML input was replaced during activation');
      await page.getByRole('button', { name: 'Make it happen' }).click();
      await page.waitForFunction(() => document.querySelector('[data-count]').textContent.startsWith('1 '));
      await page.locator('#name').fill('小林');
      await page.getByRole('heading', { name: 'Make something good, 小林.' }).waitFor();
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      assert.deepEqual(errors, []);
      assert.equal(await page.locator('[data-ui-inspector]').count(), 0);
      assert(requests.every(url => url.startsWith(server.url)), 'static build requested a repository/absolute-root endpoint');
      assert(requests.some(url => url.endsWith(`/assets/app.vob`)));
      assert(!requests.some(url => url.endsWith(`/assets/app.wasm`)));
      if (engine === 'chromium' && backend === 'vm') await page.screenshot({ path: resolve(root, 'target/ui-next/starter-mobile.png'), fullPage: true });
      reports.push({ engine, browserVersion: browser.version(), backend, selection, passed: true,
        contracts: ['static-html-before-application', 'pre-boot-edit-retained', 'server-input-adopted'] });
      await page.close();
    }
    await browser.close(); browser = undefined;
  }
  await server.close(); server = undefined;

  const clientConfig = { ...initialConfig }; delete clientConfig.prerenderEntry;
  await writeFile(configPath, JSON.stringify(clientConfig));
  const clientOutput = await buildProject(project);
  assert.equal(JSON.parse(await readFile(join(clientOutput, 'build-report.json'), 'utf8')).prerender, null);
  const clientHtml = await readFile(join(clientOutput, 'index.html'), 'utf8');
  assert(clientHtml.includes('content="client"') && clientHtml.includes('<div id="root"></div>'));
  server = await serveFiles(clientOutput);
  browser = await engines.chromium.launch({ headless: true });
  const client = await browser.newPage();
  const clientRequests=[];client.on('request',request=>clientRequests.push(request.url()));
  await client.goto(server.url);
  await client.getByRole('button', { name: 'Make it happen' }).click();
  await client.waitForFunction(() => document.querySelector('[data-count]').textContent.startsWith('1 '));
  assert(clientRequests.some(url=>url.endsWith('/assets/app.vob')), 'an omitted default uses the production VM');
  await browser.close(); browser = undefined;
  await server.close(); server = undefined;
  await writeFile(configPath, JSON.stringify(initialConfig));

  await writeFile(invalid, 'package main\nfunc unfinished(\n');
  development = await developProject(project);
  browser = await engines.chromium.launch({ headless: true });
  const page = await browser.newPage();
  const events = page.waitForResponse(response => response.url().endsWith('/__ui-next/events'));
  await page.goto(development.url);
  await events;
  await page.locator('#ui-development-error').waitFor();
  assert.match(await page.locator('#ui-development-error').textContent(), /invalid\.vo:\d+:\d+: error\[E\d+\]:/);
  await rm(invalid);
  await page.getByRole('button', { name: 'Make it happen' }).click();
  await page.waitForFunction(() => document.querySelector('[data-count]').textContent.startsWith('1 '));
  assert.equal(await page.locator('meta[name="ui-next-backend"]').getAttribute('content'),'vm');
  const panel = page.locator('[data-ui-inspector]');
  await panel.getByText('Inspect components', { exact: true }).click();
  await panel.getByText('starter.App', { exact: true }).click();
  await page.waitForFunction(() => {
    const root = document.querySelector('[data-ui-inspector]')?.shadowRoot;
    const row = [...(root?.querySelectorAll('tbody tr') ?? [])].find(row => row.firstElementChild.textContent === 'count');
    return row?.lastElementChild.textContent === '1';
  });
  assert.equal(JSON.parse(await readFile(join(project, 'target/ui-next/dev/build-report.json'), 'utf8')).inspection, true);
  assert.equal(await panel.locator('[data-prop=initial] td').last().textContent(), '""');
  await panel.getByText('Inspect components', { exact: true }).click();
  await page.evaluate(() => { window.savedCounter = document.querySelector('[data-count]'); });
  const stylesheet = join(project, 'web/app.css');
  await writeFile(stylesheet, await readFile(stylesheet, 'utf8') + '\n.starter { --development-probe: ready; }\n');
  await page.waitForFunction(() => getComputedStyle(document.querySelector('.starter')).getPropertyValue('--development-probe').trim() === 'ready');
  assert.equal(await page.evaluate(() => window.savedCounter === document.querySelector('[data-count]')), true);
  await writeFile(invalid, 'package main\nfunc diagnosticProbe() { unused := 1; missing() }\n');
  await page.locator('#ui-development-error').waitFor();
  const diagnostic = await page.locator('#ui-development-error').textContent();
  assert.match(diagnostic, /type check failed: 1 error\(s\), \d+ warning\(s\)/);
  assert.match(diagnostic.split('\n').find(line => line.startsWith('  - ')), /invalid\.vo:2:39: error\[E\d+\]: .*missing/);
  await page.getByRole('button', { name: 'Make it happen' }).click();
  await page.waitForFunction(() => document.querySelector('[data-count]').textContent.startsWith('2 '));
  const fresh = await browser.newPage();
  await fresh.goto(development.url);
  await fresh.locator('#ui-development-error').waitFor();
  assert.equal(await fresh.locator('#ui-development-error').textContent(), diagnostic);
  await fresh.close();
  await rm(invalid);
  await page.waitForFunction(() => !document.querySelector('#ui-development-error') && document.querySelector('[data-count]')?.textContent.startsWith('2 '));
  await page.goto(new URL('/away.html', development.url).href);
  const reconnected = page.waitForResponse(response => response.url().endsWith('/__ui-next/events'));
  await page.goBack({waitUntil:'commit'});
  await reconnected;
  await page.getByRole('button', {name:'Make it happen', exact:true}).click();
  await page.waitForFunction(() => document.querySelector('[data-count]')?.textContent.startsWith('1 '));
  await writeFile(stylesheet, await readFile(stylesheet, 'utf8') + '\n.starter { --history-return: connected; }\n');
  await page.waitForFunction(() => getComputedStyle(document.querySelector('.starter')).getPropertyValue('--history-return').trim() === 'connected');
  assert.equal(await readFile(join(project, 'vo.lock'), 'utf8'), lock);
  await browser.close(); browser = undefined;
  await development.close(); development = undefined;
  const config = JSON.parse(await readFile(configPath, 'utf8'));
  delete config.developmentEntry;
  await writeFile(configPath, JSON.stringify(config));
  await writeFile(htmlPath, template.replace('<!--ui-next:backend-->', 'vm'));
  const legacy = await buildProject(project, { development: true });
  assert.equal(JSON.parse(await readFile(join(legacy, 'build-report.json'), 'utf8')).inspection, false);
  server=await serveFiles(legacy);browser=await engines.chromium.launch({headless:true});
  const legacyPage=await browser.newPage();await legacyPage.goto(server.url);
  await legacyPage.getByRole('button',{name:'Make it happen'}).click();
  await legacyPage.waitForFunction(()=>document.querySelector('[data-count]').textContent.startsWith('1 '));
  assert.equal(await legacyPage.locator('meta[name="ui-next-backend"]').first().getAttribute('content'),'vm');
  await browser.close();browser=undefined;await server.close();server=undefined;
  await mkdir(join(temporary, 'outside'));
  config.developmentEntry = '../outside';
  await writeFile(configPath, JSON.stringify(config));
  await assert.rejects(buildProject(project, { development: true }), /stay inside this project/);
  delete config.developmentEntry;
  config.prerenderEntry = '../outside';
  await writeFile(configPath, JSON.stringify(config));
  await assert.rejects(buildProject(project), /prerenderEntry must stay inside this project/);
  await writeFile(resolve(root, 'target/ui-next/project-report.json'), JSON.stringify({
    passed: true, browsers: reports,
    contracts: ['create-with-spaces-and-unicode', 'existing-directory-preserved', 'locked-framework-snapshot', 'source-check', 'failed-build-retains-distribution', 'standalone-static-subdirectory', 'vm-interaction', 'portable-regexp', 'mobile-layout', 'production-inspector-excluded', 'production-static-html', 'pre-boot-input-adoption', 'failed-prerender-retains-distribution', 'prerender-template-contract', 'optional-prerender-client-fallback', 'prerender-entry-contained', 'initial-compile-error-page', 'development-inspector-state', 'live-css-preserves-state', 'compile-error-retains-interaction', 'reconnected-error-overlay', 'source-fix-recovers', 'legacy-entry-preserved', 'development-entry-contained'],
  }, null, 2) + '\n');
  console.log('Application workflow passed: create, check, build, static deployment, development recovery and history return');
} finally {
  await browser?.close();
  await development?.close();
  await server?.close();
  await rm(temporary, { recursive: true, force: true });
}
