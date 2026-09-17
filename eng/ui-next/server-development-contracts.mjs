import assert from 'node:assert/strict';
import {access, mkdir, mkdtemp, readFile, readdir, rm, writeFile} from 'node:fs/promises';
import {watch} from 'node:fs';
import {join, resolve} from 'node:path';
import {createProject, execute} from './project.mjs';
import {developProject} from './project-development.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {root} from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const browsers = await import('../browser/node_modules/playwright/index.mjs');
const temporary = await mkdtemp(resolve(root, 'target/ui-next/server-development-'));
let development, browser;
const reports = [];
function stagedClient(project) {
  const output = join(project, 'target/ui-next');
  return new Promise((resolve, reject) => {
    let finished = false;
    const finish = error => {
      if (finished) return;
      finished = true; clearTimeout(timer); watcher.close();
      if (error) reject(error); else resolve();
    };
    const watcher = watch(output, {recursive:true}, async () => {
      try {
        for (const name of await readdir(output)) {
          if (!name.startsWith('staging-')) continue;
          try {await access(join(output, name, 'public/assets/app.vob')); finish(); return;} catch {}
        }
      } catch (error) {finish(error);}
    });
    watcher.on('error', finish);
    const timer = setTimeout(() => finish(new Error('The client compile stage did not appear.')), 20000);
  });
}
try {
  const project = await createProject(join(temporary, 'A server idea 中文'));
  const module = (await readFile(join(project, 'vo.mod'), 'utf8')).match(/^module = "([^"]+)"/m)[1];
  const appPath = join(project, 'app/app.vo'), serverPath = join(project, 'server/main.vo');
  await writeFile(appPath, await readFile(resolve(root, 'eng/ui-next/fixtures/server-app.vo.txt')));
  await mkdir(join(project, 'server'));
  await writeFile(serverPath, (await readFile(resolve(root, 'eng/ui-next/fixtures/server-entry.vo.txt'), 'utf8')).replace('{{module}}', module));
  await writeFile(join(project, 'web/away.html'), '<!doctype html><title>Another page</title><h1>Another page</h1>');
  const configPath = join(project, 'ui-next.json'), config = JSON.parse(await readFile(configPath));
  await writeFile(configPath, JSON.stringify({...config, serverEntry:'server'}, null, 2) + '\n');
  const env = {...process.env, VOWORK:join(project, 'vo.work')};
  await execute(compilerPath(), ['fmt', project], {cwd:project, env});
  const appSource = await readFile(appPath, 'utf8'), serverSource = await readFile(serverPath, 'utf8');
  const invalid = join(project, 'server/invalid.vo');
  await writeFile(invalid, 'package main\nfunc unfinished(\n');
  development = await developProject(project);
  browser = await browsers.chromium.launch({headless:true});
  const page = await browser.newPage(), errors = [];
  page.on('pageerror', error => {errors.push(error.message); console.error('Development page error:', error.message);});
  const initial = await page.goto(new URL('/profile?name=Dev', development.url).href);
  assert.equal(initial.status(), 503);
  await page.locator('#ui-development-error').waitFor();
  await rm(invalid);
  await page.getByRole('heading', {name:'Hello, Dev.', exact:true}).waitFor();
  await page.waitForFunction(() => document.getElementById('status')?.textContent === '' || document.getElementById('status')?.getAttribute('role') === 'alert');
  assert.equal(await page.evaluate(() => document.getElementById('status').textContent), '');
  const firstHtml = await (await fetch(new URL('/profile?name=SSR', development.url))).text();
  assert(firstHtml.includes('Hello, SSR.') && firstHtml.includes('ui-next-backend'));
  const firstAsset = new URL(firstHtml.match(/src="([^"]+\/assets\/app.js)"/)[1], development.url).href;
  const firstScript = await (await fetch(firstAsset)).text();
  assert(!firstScript.includes('/Users/'), 'development script exposed a build-machine import path');
  const early = await browser.newPage();
  let release;
  const gate = new Promise(resolve => {release = resolve;});
  await early.route('**/assets/app.vob', async route => {await gate; await route.continue();});
  try {
    await early.goto(new URL('/profile?name=Early', development.url).href, {waitUntil:'commit'});
    const field = early.getByRole('textbox', {name:'Your name'});
    await field.fill('Written before development starts');
    await field.evaluate(element => {window.earlyServerField = element;});
    release();
    await early.getByRole('heading', {name:'Hello, Written before development starts.', exact:true}).waitFor();
    assert.equal(await field.evaluate(element => element === window.earlyServerField), true);
  } finally {release(); await early.close();}
  await page.getByRole('textbox', {name:'Your name'}).fill('Kept across reload');
  await page.getByRole('heading', {name:'Hello, Kept across reload.', exact:true}).waitFor();
  await page.getByRole('button', {name:'One more', exact:true}).click();
  await page.waitForFunction(() => document.querySelector('output')?.textContent === '1');
  await page.evaluate(() => {window.savedOutput = document.querySelector('output');});
  const stylesheet = join(project, 'web/app.css');
  await writeFile(stylesheet, await readFile(stylesheet, 'utf8') + '\nmain { --dev-server-style: ready; }\n');
  await page.waitForFunction(() => getComputedStyle(document.querySelector('main')).getPropertyValue('--dev-server-style').trim() === 'ready');
  assert.equal(await page.locator('output').evaluate(element => element === window.savedOutput), true);
  await page.evaluate(() => {
    window.observedHeadings = [];
    const observer = new MutationObserver(() => {window.observedHeadings.push(document.querySelector('h1')?.textContent);});
    observer.observe(document.getElementById('root').parentNode, {subtree:true, childList:true, characterData:true});
  });
  const compiledClient = stagedClient(project);
  await writeFile(appPath, appSource.replace('"Hello, "', '"Superseded, "'));
  await compiledClient;
  await writeFile(appPath, appSource.replace('"Hello, "', '"Welcome, "'));
  await page.getByRole('heading', {name:'Welcome, Kept across reload.', exact:true}).waitFor();
  assert.equal(await page.evaluate(() => window.observedHeadings.some(text => text?.startsWith('Superseded,'))), false, 'a superseded client/server build was published');
  assert.equal(await page.locator('output').textContent(), '1');
  assert.equal(await (await fetch(firstAsset)).text(), firstScript, 'original revision changed in place');
  const currentHtml = await (await fetch(new URL('/profile?name=New', development.url))).text();
  assert(currentHtml.includes('Welcome, New.') && !currentHtml.includes(new URL(firstAsset).pathname));
  await writeFile(invalid, 'package main\nfunc unfinished(\n');
  await page.locator('#ui-development-error').waitFor();
  await page.getByRole('button', {name:'One more', exact:true}).click();
  await page.waitForFunction(() => document.querySelector('output')?.textContent === '2');
  assert((await (await fetch(new URL('/profile?name=Still%20served', development.url))).text()).includes('Welcome, Still served.'));
  const late = await browser.newPage();
  await late.goto(new URL('/profile?name=Late', development.url).href);
  await late.locator('#ui-development-error').waitFor();
  await late.close();
  await rm(invalid);
  await page.waitForFunction(() => !document.querySelector('#ui-development-error'));
  await page.waitForFunction(() => document.getElementById('status')?.textContent === '');
  // A server-only fix performs a document reload and receives fresh request data.
  await page.getByRole('heading', {name:'Welcome, Dev.', exact:true}).waitFor();
  await writeFile(serverPath, serverSource.replace('" · Request page"', '" · Updated server"'));
  await page.waitForFunction(() => document.title === 'Dev · Updated server');
  await page.getByRole('textbox', {name:'Your name'}).fill('Saved from dev');
  const saved = page.waitForURL(value => value.searchParams.get('name') === 'Saved from dev');
  await page.getByRole('button', {name:'Save name', exact:true}).click();
  await saved;
  await page.waitForFunction(() => document.getElementById('status')?.textContent === '');
  await page.getByRole('heading', {name:'Welcome, Saved from dev.', exact:true}).waitFor();
  assert.equal(await page.title(), 'Saved from dev · Updated server');
  await page.goto(new URL('/away.html', development.url).href);
  const connected = page.waitForResponse(response => response.url().endsWith('/__ui-next/events'));
  await page.goBack({waitUntil:'commit'}); await connected;
  await page.waitForFunction(() => document.getElementById('status')?.textContent === '');
  await page.getByRole('button', {name:'One more', exact:true}).click();
  await page.waitForFunction(() => document.querySelector('output')?.textContent === '1');
  assert.deepEqual(errors, []);
  reports.push({engine:'chromium', passed:true, contracts:['first-build-error-page', 'initial-native-html', 'default-vm-deep-url',
    'versioned-assets', 'superseded-build-not-published', 'early-input-node-adoption', 'css-keeps-dom', 'shared-code-keeps-state', 'server-code-reloads-data', 'failed-build-keeps-server',
    'late-error-stream', 'standard-post-in-development', 'history-reconnect']});
  await browser.close(); browser = undefined;
  for (const engine of ['firefox', 'webkit']) {
    browser = await browsers[engine].launch({headless:true});
    const page = await browser.newPage();
    await page.goto(new URL('/profile?name=Portable', development.url).href);
    await page.waitForFunction(() => document.getElementById('status')?.textContent === '');
    await page.getByRole('textbox', {name:'Your name'}).fill(engine);
    await page.getByRole('heading', {name:`Welcome, ${engine}.`, exact:true}).waitFor();
    await page.getByRole('button', {name:'One more', exact:true}).click();
    await page.waitForFunction(() => document.querySelector('output')?.textContent === '1');
    await page.evaluate(() => {window.previousRoot = document.getElementById('root'); window.dispatchEvent(new CustomEvent('vo-ui-reload', {cancelable:true, detail:{version:String(Date.now())}}));});
    await page.waitForFunction(() => window.previousRoot !== document.getElementById('root') && document.querySelector('output')?.textContent === '1');
    await page.getByRole('heading', {name:`Welcome, ${engine}.`, exact:true}).waitFor();
    reports.push({engine, passed:true, contracts:['server-html-activation', 'live-input', 'versioned-state-reload']});
    await browser.close(); browser = undefined;
  }
  const revisions = await readdir(join(project, 'target/ui-next/dev-revisions'));
  assert(revisions.length >= 3 && revisions.length <= 8);
  await development.close(); development = undefined;
  assert.deepEqual(await readdir(join(project, 'target/ui-next/dev-revisions')), []);
  await writeFile(resolve(root, 'target/ui-next/server-development-report.json'), JSON.stringify({passed:true, reports, sessionCleanup:true}, null, 2) + '\n');
  console.log('Server development contracts passed');
} finally {await browser?.close(); await development?.close(); await rm(temporary, {recursive:true, force:true});}
