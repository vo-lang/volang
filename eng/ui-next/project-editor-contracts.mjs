import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cp, mkdir, mkdtemp, readFile, rm, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {createProject, checkProject, buildProject, execute} from './project.mjs';
import {developProject} from './project-development.mjs';
import {serveFiles} from './static-server.mjs';
import {sourceEditor} from './editor-controls.mjs';
import {root} from './server.mjs';

// An ordinary generated application: its only editor host configuration is the
// feature declaration. App state and the native input contract remain in Vo.
const source = await readFile(resolve(root, 'eng/ui-next/fixtures/editor-app.vo.txt'),'utf8');

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const engines = await import('../browser/node_modules/playwright/index.mjs');
const evidence = resolve(root, 'target/ui-next/project-editor');
await mkdir(evidence, {recursive:true});
const temporary = await mkdtemp(resolve(root, 'target/ui-next/project-editor-contract-'));
const results = [];
let browser, server, development;
const libraryPattern = /\/editor-library(?:-[a-z0-9]+)?\.js(?:\?.*)?$/i;
const draft = 'package main\nfunc main() { println("A small idea 中文") }\n';
const ready = async page => {
  await page.waitForFunction(() => window.__projectApp !== undefined);
  assert.equal(await page.evaluate(() => window.__projectApp.ready), true);
};
const draftCommitted = (page, value = draft) => page.waitForFunction(value => document.querySelector('[data-source]')?.textContent === value, value);
const closeApp = page => page.evaluate(async () => {window.__projectApp.close(); await window.__projectApp.done;});

try {
  const project = await createProject(join(temporary, 'Scratchpad 中文'));
  const appPath = join(project, 'app/app.vo');
  await writeFile(appPath, source);
  await execute(compilerPath(), ['fmt', appPath], {cwd:project, env:{...process.env, VOWORK:join(project, 'vo.work')}});
  const formatted = await readFile(appPath, 'utf8');
  const configPath = join(project, 'ui-next.json');
  const config = {...JSON.parse(await readFile(configPath, 'utf8')), features:['editor'],
    prerenderPages:[{path:'/', data:'closed'}, {path:'/edit', data:'open'}]};
  await writeFile(configPath, JSON.stringify(config, null, 2) + '\n');
  const bootPath = join(project, 'web/boot.js');
  await writeFile(bootPath, await readFile(bootPath, 'utf8') + '\nwindow.__projectApp = application;\n');
  const cssPath = join(project, 'web/app.css');
  await writeFile(cssPath, await readFile(cssPath, 'utf8') + '\n.starter > * + * { margin-block-start: 20px; }\n.starter output { display: block; white-space: pre-wrap; overflow-wrap: anywhere; }\n');
  const manifest = await readFile(join(project, 'vo.mod'), 'utf8'), lock = await readFile(join(project, 'vo.lock'), 'utf8');
  await checkProject(project);
  console.log('Building the ordinary editor project');
  const output = await buildProject(project);
  const report = JSON.parse(await readFile(join(output, 'build-report.json'), 'utf8'));
  assert.deepEqual(report.features, ['editor']);
  assert.equal(report.thirdParty.length, 14);
  assert(report.thirdParty.every(item => item.license === 'MIT' && item.name !== 'fflate'));
  assert(report.artifacts.some(item => libraryPattern.test('/' + item.path)));
  const notice = await readFile(join(output, 'THIRD_PARTY_NOTICES.txt'), 'utf8');
  assert.match(notice, /@codemirror\/view 6\.43\.11/);
  assert.match(notice, /Permission is hereby granted/);
  assert(!notice.includes(root));
  for (const artifact of report.artifacts) {
    const bytes = await readFile(join(output, artifact.path));
    assert.equal(bytes.length, artifact.bytes);
    assert.equal(createHash('sha256').update(bytes).digest('hex'), artifact.sha256);
    if (artifact.path.endsWith('.js')) assert(!bytes.includes(Buffer.from(root)), artifact.path);
  }
  const deployed = join(evidence, 'distribution');
  await rm(deployed, {recursive:true, force:true});
  await cp(output, deployed, {recursive:true});
  server = await serveFiles(deployed, {base:'/scratch/'});
  for (const engine of ['chromium', 'firefox', 'webkit']) {
    browser = await engines[engine].launch({headless:true});
    const native = await browser.newPage({javaScriptEnabled:false});
    await native.goto(server.url + 'edit/');
    await native.getByRole('textbox', {name:'Source code'}).fill(draft);
    assert.equal(await native.locator('#project-source').inputValue(), draft);
    await native.close();
    for (const backend of ['vm']) for (const mode of ['client', 'hydrate', 'failure', 'close']) {
      console.log(`Project editor: ${engine} ${backend} ${mode}`);
      const page = await browser.newPage({viewport:{width:390, height:844}}), errors = [], requests = [];
      page.on('pageerror', error => errors.push(error.message));
      page.on('request', request => requests.push(request.url()));
      let releaseBoot = () => {}, releaseLibrary = () => {};
      if (mode === 'hydrate') {
        const gate = new Promise(resolve => {releaseBoot = resolve;});
        await page.route('**/assets/app.*', async route => {await gate; await route.continue();});
      }
      if (mode !== 'client') {
        const gate = new Promise(resolve => {releaseLibrary = resolve;});
        await page.route(libraryPattern, async route => {
          try {
            if (mode === 'failure') return await route.fulfill({status:503, body:'Editor temporarily unavailable'});
            await gate;
            const response = await route.fetch();
            await route.fulfill({response, body:(await response.text()) + '\n;globalThis.editorLibraryEvaluated = true;'});
          } catch (error) {if (!page.isClosed() && !route.request().failure()) throw error;}
        });
      }
      try {
        await page.goto(`${server.url}${mode === 'hydrate' ? 'edit/' : ''}?backend=${backend}`, {waitUntil:'commit'});
        const editor = sourceEditor(page, 'project-source');
        if (mode === 'hydrate') {
          await editor.fill(draft);
          await editor.input.evaluate(input => {window.serverInput = input; input.setSelectionRange(2, 8, 'backward');});
          releaseBoot(); await ready(page);
          await draftCommitted(page);
          assert.equal(await page.evaluate(() => window.serverInput === document.getElementById('project-source')), true);
          assert.equal(await page.locator('[data-initial]').textContent(), 'open');
          releaseLibrary(); await page.waitForFunction(() => window.editorLibraryEvaluated);
          assert.equal(await page.locator('.cm-editor').count(), 0, 'late enhancement interrupted the native input session');
          assert.deepEqual(await editor.input.evaluate(input => [input.selectionStart, input.selectionEnd, input.selectionDirection]), [2,8,'backward']);
          await page.getByRole('button', {name:'Hide editor', exact:true}).focus();
          await page.locator('.cm-content').waitFor();
          assert.equal(await editor.inputValue(), draft);
        } else {
          await ready(page);
          assert.equal(await page.locator('[data-initial]').textContent(), 'closed');
          assert(!requests.some(url => libraryPattern.test(url)), 'closed editor loaded its optional library');
          await page.getByRole('button', {name:'Show editor', exact:true}).click();
          if (mode === 'client') await page.locator('.cm-content').waitFor();
          if (mode === 'failure') await page.waitForFunction(() => document.querySelector('[data-enhancement-error]')?.textContent.length > 0);
          await editor.fill(draft); await draftCommitted(page);
          if (mode === 'close') {
            await closeApp(page); releaseLibrary();
            await page.waitForFunction(() => window.editorLibraryEvaluated);
            assert.equal(await page.locator('#root').textContent(), '');
            assert.equal(await page.locator('.cm-editor').count(), 0);
          } else if (mode === 'failure') {
            assert.equal(await page.locator('.cm-editor').count(), 0);
            assert.equal(await editor.input.getAttribute('aria-hidden'), null);
          } else {
            await page.getByRole('button', {name:'Hide editor', exact:true}).click();
            await page.locator('#project-source').waitFor({state:'detached'});
            await page.getByRole('button', {name:'Show editor', exact:true}).click();
            await page.locator('.cm-content').waitFor();
            assert.equal(await editor.inputValue(), draft);
            assert.equal(requests.filter(url => libraryPattern.test(url)).length, 1);
            if (engine === 'chromium' && backend === 'vm') await page.screenshot({path:join(evidence, 'editor-mobile.png'), fullPage:true});
          }
        }
        assert(requests.every(url => url.startsWith(server.url)), 'relocated build requested an absolute-root or repository URL');
        assert.deepEqual(errors, []);
        if (mode !== 'close') {
          assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
          await closeApp(page);
          assert.equal(await page.locator('.cm-editor').count(), 0);
        }
        results.push({engine, browserVersion:browser.version(), backend, mode, passed:true});
      } catch (error) {
        await page.screenshot({path:join(evidence, `failure-${engine}-${backend}-${mode}.png`), fullPage:true});
        await writeFile(join(evidence, `failure-${engine}-${backend}-${mode}.json`), JSON.stringify({error:String(error), errors, requests, completed:results}, null, 2));
        throw error;
      } finally {releaseBoot(); releaseLibrary(); await page.close();}
    }
    await browser.close(); browser = undefined;
  }
  await server.close(); server = undefined;

  console.log('Project editor: real compiler-driven development reload');
  development = await developProject(project);
  browser = await engines.chromium.launch({headless:true});
  const page = await browser.newPage(), errors = [];
  page.on('pageerror', error => errors.push(error.message));
  const events = page.waitForResponse(response => response.url().endsWith('/__ui-next/events'));
  await page.goto(development.url); await events; await ready(page);
  await page.getByRole('button', {name:'Show editor', exact:true}).click();
  await page.locator('.cm-content').waitFor();
  const editor = sourceEditor(page, 'project-source');
  await editor.fill(draft); await draftCommitted(page);
  await editor.press('ControlOrMeta+a');
  await page.keyboard.press('ArrowLeft');
  await page.keyboard.press('Shift+ArrowRight'); await page.keyboard.press('Shift+ArrowRight');
  assert.deepEqual(await editor.input.evaluate(input => [input.selectionStart, input.selectionEnd]), [0,2]);
  await page.evaluate(() => {window.previousRoot = document.getElementById('root');});
  await writeFile(appPath, formatted.replace('ui.Text("Scratchpad")', 'ui.Text("Scratchpad refreshed")'));
  await page.getByRole('heading', {name:'Scratchpad refreshed', exact:true}).waitFor();
  await page.waitForFunction(() => document.getElementById('root') !== window.previousRoot);
  await page.waitForFunction(() => document.activeElement.id === 'project-source' || document.activeElement.classList.contains('cm-content'));
  assert.equal(await editor.inputValue(), draft);
  assert.deepEqual(await editor.input.evaluate(input => [input.selectionStart, input.selectionEnd]), [0,2]);
  await draftCommitted(page);
  await page.getByRole('button', {name:'Hide editor', exact:true}).focus();
  await page.locator('.cm-content').waitFor();
  await editor.fill(draft + '// Still editable after reload\n');
  await draftCommitted(page, draft + '// Still editable after reload\n');
  assert.deepEqual(errors, []);
  const devReport = JSON.parse(await readFile(join(project, 'target/ui-next/dev/build-report.json'), 'utf8'));
  assert.equal(devReport.inspection, true); assert.deepEqual(devReport.features, ['editor']);
  assert.equal(devReport.thirdParty.length, 14);
  assert.equal(await readFile(join(project, 'vo.mod'), 'utf8'), manifest);
  assert.equal(await readFile(join(project, 'vo.lock'), 'utf8'), lock);
  await closeApp(page);
  await browser.close(); browser = undefined;
  await development.close(); development = undefined;
  await writeFile(join(evidence, 'report.json'), JSON.stringify({passed:true, results,
    development:{passed:true, realSourceReload:true, retainedDraft:true, retainedFocusAndSelection:true},
    build:report, contracts:['feature-only-host-configuration', 'independent-relocated-distribution',
      'unchanged-module-lock', 'native-without-javascript', 'lazy-library', 'early-input-adoption',
      'focused-native-handoff', 'failure-fallback', 'pending-root-close', 'component-reopen', 'owned-editor-disposal'],
  }, null, 2) + '\n');
  console.log('Ordinary editor project passed: 24 browser cases and real source reload');
} finally {
  await browser?.close(); await development?.close(); await server?.close();
  await rm(temporary, {recursive:true, force:true});
}
