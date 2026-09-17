import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {watch} from 'node:fs';
import {cp, mkdir, mkdtemp, readFile, readdir, rm, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {createProject, checkProject, buildProject} from './project.mjs';
import {developProject} from './project-development.mjs';
import {serveFiles} from './static-server.mjs';
import {root} from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const engines = await import('../browser/node_modules/playwright/index.mjs');
const temporary = await mkdtemp(resolve(root, 'target/ui-next/page-entries-'));
const evidence = resolve(root, 'target/ui-next/page-entries');
await mkdir(evidence, {recursive:true});
let browser, server, development;
const results = [];
const ready = async page => {
  await page.waitForFunction(() => window.__pagesApp !== undefined);
  assert.equal(await page.evaluate(() => window.__pagesApp.ready), true);
};
const images = requests => requests.map(value => new URL(value).pathname)
  .filter(path => /\/assets\/(?:entries\/[^/]+\/)?app\.vob$/.test(path));
const image = (base, id, backend) => new URL(`assets/${id === 'default' ? '' : `entries/${id}/`}app.vob`, base).pathname;
const sourceDigest = bytes => createHash('sha256').update(bytes).digest('hex');

try {
  const project = await createProject(join(temporary, 'Small pages 中文'), {template:'pages'});
  const bootPath = join(project, 'web/boot.js');
  await writeFile(bootPath, await readFile(bootPath, 'utf8') + '\nwindow.__pagesApp = application; window.__pagesDocument = Math.random();\n');
  const configPath = join(project, 'ui-next.json');
  const config = JSON.parse(await readFile(configPath, 'utf8'));
  const lock = await readFile(join(project, 'vo.lock'), 'utf8');
  await checkProject(project);
  console.log('Building two independent entries for three static pages');
  const built = await buildProject(project);
  const before = await readFile(join(built, 'build-report.json'), 'utf8');
  const build = JSON.parse(before);
  assert.deepEqual(build.entries.map(entry => entry.id), ['default', 'notes']);
  assert.deepEqual(build.pages.map(page => page.entry), ['default', 'notes', 'notes']);
  assert.equal(build.artifacts.filter(artifact => artifact.path.endsWith('.vob')).length, 2);
  assert.equal(build.artifacts.filter(artifact => /(?:^|\/)app\.js$/.test(artifact.path)).length, 1);
  for (const entry of build.entries) {
    assert(entry.prerender.artifact.sha256);
    assert(build.artifacts.some(artifact => artifact.path === entry.bytecode));
  }
  const homeImage = await readFile(join(built, 'assets/app.vob'));
  const notesImage = await readFile(join(built, 'assets/entries/notes/app.vob'));
  assert.equal(homeImage.includes(Buffer.from('pages.Notebook')), false);
  assert.equal(notesImage.includes(Buffer.from('pages.Notebook')), true);
  assert.equal(notesImage.includes(Buffer.from('pages.Home')), false);
  assert.equal(homeImage.includes(Buffer.from('pages.Home')), true);
  for (const bytes of [homeImage, notesImage]) for (const name of ['inspect', 'develop', 'prerender']) {
    assert.equal(bytes.includes(Buffer.from(`github.com/vo-lang/ui/next/${name}`)), false);
  }
  assert.equal(await readFile(join(project, 'vo.lock'), 'utf8'), lock);
  const output = join(evidence, 'distribution');
  await rm(output, {recursive:true, force:true});
  await cp(built, output, {recursive:true});
  server = await serveFiles(output, {base:'/small-pages/'});
  assert.equal((await fetch(server.url + 'unknown/')).status, 404);

  for (const engine of ['chromium', 'firefox', 'webkit']) {
    browser = await engines[engine].launch({headless:true});
    for (const item of build.pages) {
      const url = new URL(item.path.slice(1), server.url);
      const home = item.entry === 'default';
      const heading = home ? 'An idea begins here.' : 'A little room to write.';
      const native = await browser.newPage({javaScriptEnabled:false});
      await native.goto(url.href);
      await native.getByRole('heading', {name:heading, exact:true}).waitFor();
      assert.equal(await native.title(), item.title);
      if (!home) {
        const declared = config.prerenderPages.find(page => page.entry === item.entry && page.path.replace(/\/$/, '') === item.path.replace(/\/$/, ''));
        assert.equal(await native.locator('#note').inputValue(), JSON.parse(declared.data).note);
        await native.getByRole('link', {name:'Back to the beginning'}).click();
        await native.getByRole('heading', {name:'An idea begins here.', exact:true}).waitFor();
      }
      await native.close();

      for (const backend of ['vm']) {
        console.log(`Page entry: ${engine} ${backend} ${item.path}`);
        const page = await browser.newPage({viewport:{width:1100, height:850}});
        const errors = [], requests = [];
        page.on('pageerror', error => errors.push(error.message));
        page.on('request', request => requests.push(request.url()));
        let release = () => {};
        const gate = new Promise(resolveGate => { release = resolveGate; });
        await page.route(/\/assets\/(?:entries\/[^/]+\/)?app\.vob(?:\?|$)/, async route => { await gate; await route.continue(); });
        try {
          url.searchParams.set('backend', backend);
          await page.goto(url.href, {waitUntil:'commit'});
          await page.getByRole('heading', {name:heading, exact:true}).waitFor();
          const draft = 'Before startup 中文 — ' + backend;
          if (!home) {
            await page.locator('#note').fill(draft);
            await page.locator('#note').evaluate(input => {window.earlyInput = input; input.setSelectionRange(2, 8, 'backward');});
          }
          release(); await ready(page);
          assert.equal(await page.locator('meta[name="ui-next-entry"]').getAttribute('content'), item.entry);
          assert.deepEqual(images(requests), [image(server.url, item.entry, backend)]);
          assert(requests.every(request => request.startsWith(server.url)), 'assets escaped the deployment base');
          if (!home) {
            assert.deepEqual(await page.locator('#note').evaluate(input => [input === window.earlyInput, input.value, input === document.activeElement, input.selectionStart, input.selectionEnd, input.selectionDirection]),
              [true, draft, true, 2, 8, 'backward']);
            await page.getByRole('button', {name:'Keep this thought'}).click();
            await page.waitForFunction(draft => document.querySelector('[data-saved]')?.textContent === draft, draft);
          } else {
            await page.getByRole('button', {name:'Take a step'}).click();
            await page.waitForFunction(() => document.querySelector('[data-count]')?.textContent === '1 little steps');
          }
          if (engine === 'chromium' && backend === 'vm' && item.path !== '/notes/morning/') {
            const label = home ? 'home' : 'notebook';
            await page.screenshot({path:join(evidence, `${label}-desktop.png`), fullPage:true});
            await page.setViewportSize({width:390, height:844});
            assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
            await page.screenshot({path:join(evidence, `${label}-mobile.png`), fullPage:true});
          }
          const documentId = await page.evaluate(() => window.__pagesDocument);
          requests.length = 0;
          await page.getByRole('link', {name:home ? 'Open your notebook' : 'Back to the beginning'}).click();
          await ready(page);
          assert.notEqual(await page.evaluate(() => window.__pagesDocument), documentId, 'entry navigation did not open a new document');
          await page.getByRole('heading', {name:home ? 'A little room to write.' : 'An idea begins here.', exact:true}).waitFor();
          assert.deepEqual(images(requests), [image(server.url, home ? 'notes' : 'default', 'vm')]);
          requests.length = 0;
          await page.goBack({waitUntil:'commit'}); await ready(page);
          await page.getByRole('heading', {name:heading, exact:true}).waitFor();
          assert(images(requests).every(path => path === image(server.url, item.entry, backend)));
          // Both a restored document and a fresh history load must remain live.
          if (home) {
            const count = Number.parseInt(await page.locator('[data-count]').textContent());
            await page.getByRole('button', {name:'Take a step'}).click();
            await page.waitForFunction(count => document.querySelector('[data-count]')?.textContent === `${count + 1} little steps`, count);
          } else {
            await page.locator('#note').fill('Back and still writing.');
            await page.getByRole('button', {name:'Keep this thought'}).click();
            await page.waitForFunction(() => document.querySelector('[data-saved]')?.textContent === 'Back and still writing.');
          }
          await page.evaluate(async () => { window.__pagesApp.close(); await window.__pagesApp.done; });
          assert.equal(await page.locator('#root').textContent(), '');
          assert.deepEqual(errors, []);
          results.push({engine, browserVersion:browser.version(), backend, path:item.path, entry:item.entry, passed:true});
        } catch (error) {
          await page.screenshot({path:join(evidence, `failure-${engine}-${backend}-${item.entry}.png`), fullPage:true});
          throw error;
        } finally { release(); await page.close(); }
      }
    }
    await browser.close(); browser = undefined;
  }
  await server.close(); server = undefined;

  const preserved = async () => {
    assert.equal(await readFile(join(built, 'build-report.json'), 'utf8'), before);
    for (const artifact of build.artifacts) assert.equal(sourceDigest(await readFile(join(built, artifact.path))), artifact.sha256);
    assert.deepEqual((await readdir(join(project, 'target/ui-next'))).filter(name => name.startsWith('staging-')), []);
  };
  const htmlPath = join(project, 'web/index.html'), html = await readFile(htmlPath, 'utf8');
  await writeFile(htmlPath, html.replace('<!--ui-next:entry-->', 'default'));
  await assert.rejects(buildProject(project), /Named page entries require/); await preserved();
  await writeFile(htmlPath, html);
  const invalid = join(project, 'notes/invalid.vo');
  await writeFile(invalid, 'package main\nfunc unfinished(\n');
  await assert.rejects(buildProject(project), /unfinished|parse error|expected/); await preserved();
  await rm(invalid);
  const renderPath = join(project, 'notes/prerender/main.vo'), render = await readFile(renderPath, 'utf8');
  await writeFile(renderPath, render.replace('prerender.Run(app.View)', 'panic("named page render failed")\n\tprerender.Run(app.View)'));
  await assert.rejects(buildProject(project), /named page render failed/); await preserved();
  await writeFile(renderPath, render);

  // Cancel a real compilation after both prepared renderer files have started
  // being written. The builder must join that child before removing its stage.
  const lifetime = new AbortController(), reason = new Error('cancel named entry build');
  const watcher = watch(join(project, 'target/ui-next'), {recursive:true}, (_event, filename) => {
    if (String(filename).replaceAll('\\', '/').endsWith('.prerender/notes.vob')) lifetime.abort(reason);
  });
  const timeout = setTimeout(() => lifetime.abort(new Error('cancellation fixture did not reach the named renderer')), 30_000);
  try { await assert.rejects(buildProject(project, {signal:lifetime.signal}), error => error === reason); }
  finally { clearTimeout(timeout); watcher.close(); }
  await preserved();

  // An entry can own / while the unused default program is omitted entirely.
  const rootConfig = {...config, prerenderPages:[{path:'/', entry:'notes', data:JSON.stringify({note:'The notebook starts here.', home:'./'})}]};
  await writeFile(configPath, JSON.stringify(rootConfig));
  const rootBuild = await buildProject(project);
  const rootReport = JSON.parse(await readFile(join(rootBuild, 'build-report.json')));
  assert.deepEqual(rootReport.entries.map(entry => entry.id), ['notes']);
  assert.equal(rootReport.prerender.entry, 'notes/prerender');
  assert(!rootReport.artifacts.some(artifact => artifact.path === 'assets/app.vob' || artifact.path === 'assets/app.wasm'));
  const rootOutput = join(evidence, 'named-root-distribution');
  await rm(rootOutput, {recursive:true, force:true}); await cp(rootBuild, rootOutput, {recursive:true});
  server = await serveFiles(rootOutput, {base:'/notebook/'});
  browser = await engines.chromium.launch({headless:true});
  for (const backend of ['vm']) {
    const page = await browser.newPage(), requests = [];
    page.on('request', request => requests.push(request.url()));
    await page.goto(server.url + '?backend=' + backend); await ready(page);
    assert.equal(await page.locator('#note').inputValue(), 'The notebook starts here.');
    assert.deepEqual(images(requests), [image(server.url, 'notes', backend)]);
    await page.close();
  }
  await browser.close(); browser = undefined; await server.close(); server = undefined;
  await writeFile(configPath, JSON.stringify(config));

  console.log('Page entries: nested development image, failed build recovery and retained input');
  development = await developProject(project);
  browser = await engines.chromium.launch({headless:true});
  const page = await browser.newPage(), errors = [], requests = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('request', request => requests.push(request.url()));
  const connected = page.waitForResponse(response => response.url().endsWith('/__ui-next/events'));
  const origin = new URL('/', development.url).href;
  await page.goto(origin + 'notes/morning/?backend=vm'); await connected; await ready(page);
  assert.deepEqual(images(requests), [image(origin, 'notes', 'vm')]);
  assert.equal(await page.locator('meta[name="ui-next-render"]').getAttribute('content'), 'client');
  const draft = 'A morning draft that survives source edits.';
  await page.locator('#note').fill(draft);
  await page.locator('#note').evaluate(input => {input.setSelectionRange(1, 7, 'backward'); window.beforeReload = document.getElementById('root');});
  await writeFile(invalid, 'package main\nfunc unfinished(\n');
  await page.locator('#ui-development-error').waitFor();
  const devReport = JSON.parse(await readFile(join(project, 'target/ui-next/dev/build-report.json')));
  assert.equal(devReport.entries.length, 2);
  assert(devReport.entries.every(entry => entry.developmentEntry && entry.prerender === null));
  assert.equal(await page.locator('#note').inputValue(), draft);
  const sourcePath = join(project, 'notes/app/app.vo'), source = await readFile(sourcePath, 'utf8');
  await writeFile(sourcePath, source.replace('A little room to write.', 'A fresh page for your thoughts.'));
  await rm(invalid);
  await page.getByRole('heading', {name:'A fresh page for your thoughts.', exact:true}).waitFor();
  await page.waitForFunction(() => document.getElementById('root') !== window.beforeReload);
  assert.deepEqual(await page.locator('#note').evaluate(input => [input.value, input === document.activeElement, input.selectionStart, input.selectionEnd, input.selectionDirection]), [draft, true, 1, 7, 'backward']);
  assert(images(requests).every(path => path === image(origin, 'notes', 'vm')), 'nested reload fetched another entry');
  await page.getByRole('button', {name:'Keep this thought'}).click();
  await page.waitForFunction(draft => document.querySelector('[data-saved]')?.textContent === draft, draft);
  await page.getByRole('link', {name:'Back to the beginning'}).click(); await ready(page);
  await page.getByRole('heading', {name:'An idea begins here.', exact:true}).waitFor();
  assert.deepEqual(errors, []);
  await browser.close(); browser = undefined;
  await development.close(); development = undefined;

  await writeFile(join(evidence, 'report.json'), JSON.stringify({passed:true, build, namedRoot:rootReport, results,
    sourceIsolation:{homeOmitsNotebook:true, notebookOmitsHome:true},
    development:{passed:true, nestedImageOnly:true, retainedDraftFocusSelection:true, failedBuildRecovery:true},
    contracts:['two-images-three-pages', 'shared-host-runtime-styles', 'page-bound-initial-data', 'native-readable-html',
      'early-input-adoption', 'only-selected-image-downloads', 'native-document-navigation', 'live-history-return',
      'root-close', 'failed-entry-and-render-preserve-all-artifacts', 'cancel-during-named-render-compilation',
      'staging-cleanup', 'named-root-omits-unused-default-image', 'lock-unchanged']}, null, 2) + '\n');
  console.log('Page entries passed: 18 browser cases and nested source reload');
} finally { await browser?.close(); await development?.close(); await server?.close(); await rm(temporary, {recursive:true, force:true}); }
