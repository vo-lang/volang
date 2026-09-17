import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { readFile, writeFile, mkdtemp, rm } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { createProject, buildProject, execute } from './project.mjs';
import { compilerPath } from '../../lang/crates/vo-web/test_compiler.mjs';
import { serveFiles } from './static-server.mjs';
import { root } from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const browsers = await import('../browser/node_modules/playwright/index.mjs');
const directory = await mkdtemp(resolve(root, 'target/ui-next/reload-contract-'));
let server;
const reports = [], artifacts = [];
try {
  const project = await createProject(join(directory, 'app'));
  const fixture = await readFile(resolve(root, 'eng/ui-next/fixtures/reload.vo.txt'), 'utf8');
  const source = revision => fixture.replace('REVISION', String(revision))
    .replace('COUNT_DECLARATION', revision === 3 ? 'count := ui.String(scope, "count", "new")' : `count := ui.Int(scope, "count", ${revision === 2 ? 99 : 0})`)
    .replace('COUNT_UPDATE', revision === 3 ? 'count.Set(count.Get() + "!")' : `count.Set(count.Get() + ${revision === 2 ? 2 : 1})`);
  await writeFile(join(project, 'app/app.vo'), source(1));
  const boot = await readFile(join(project, 'web/boot.js'), 'utf8');
  await writeFile(join(project, 'web/boot.js'), boot.replace('  services: {', `  services: {
    watches: { 'lab.clock'(_value, signal, emit) {
      window.reloadLab.starts++; window.reloadLab.active++;
      window.reloadLab.emits.push(emit);
      signal.addEventListener('abort', () => { window.reloadLab.active--; window.reloadLab.stops++; }, {once:true});
    } },
    tasks: { 'lab.save'(_value, signal) {
      return new Promise(resolve => {
        window.reloadLab.saves.push(resolve);
        signal.addEventListener('abort', () => { window.reloadLab.aborts++; }, {once:true});
      });
    } },
  `).replace("const application =", "window.reloadLab = {starts:0, stops:0, active:0, aborts:0, saves:[], emits:[]};\nconst application =")
    + '\nwindow.reloadApplication = application;\n');
  const output = await buildProject(project, { development: true });
  const variants = new Map();
  for (const revision of [1, 2, 3, 4]) {
    await writeFile(join(project, 'app/app.vo'), source(revision));
    const path = join(directory, `revision-${revision}.vob`);
    const args = ['emit', 'bytecode', 'development', '-o', path];
    await execute(compilerPath(), args, {cwd:project,env:{...process.env,VOWORK:join(project,'vo.work')}});
    const compiled = await readFile(path);
    artifacts.push({revision,extension:'vob',sha256:createHash('sha256').update(compiled).digest('hex'),bytes:compiled.length});
    variants.set(revision, compiled);
  }
  server = await serveFiles(output, { development: true });
  for (const engine of ['chromium', 'firefox', 'webkit']) {
    const browser = await browsers[engine].launch({ headless: true });
    try {
      for (const backend of ['vm']) {
        const page = await browser.newPage();
        let revision = 1, artifactRequests = 0;
        const errors = [];
        page.on('pageerror', error => errors.push(String(error)));
        await page.route(/\/assets\/app\.vob(\?.*)?$/, route => {
          artifactRequests++;
          if (revision === 0) return route.fulfill({ status: 503, body: 'Cannot load replacement' });
          return route.fulfill({ contentType: 'application/octet-stream', body: variants.get(revision) });
        });
        await page.goto(server.url + '?backend=' + backend);
        await page.waitForFunction(() => window.reloadLab?.active === 1);
        await page.evaluate(() => { window.pageIdentity = {}; window.identity = window.pageIdentity; window.reloadErrors = 0; window.addEventListener('vo-ui-reload-status', event => { if(event.detail.type === 'error') window.reloadErrors++; }); });
        await page.locator('#reload-add').click();
        await page.locator('[data-item=a]').click();
        await page.locator('#reload-name').fill('Draft 中文');
        await page.locator('#reload-choices').selectOption(['a', 'c']);
        await page.locator('#reload-note').fill('Private draft 中文');
        await page.locator('#reload-native-check').uncheck();
        await page.locator('#reload-native-select').evaluate(select => { select.selectedIndex = -1; });
        await page.getByRole('button', { name: 'Save', exact: true }).click();
        await page.waitForFunction(() => document.querySelector('[data-reload-pending]').textContent === 'true');
        await page.locator('#reload-name').fill('Still editing');
        await page.locator('#reload-note').focus();
        await page.locator('#reload-note').evaluate(input => input.setSelectionRange(2, 7, 'backward'));
        revision = 2;
        await page.evaluate(() => window.dispatchEvent(new CustomEvent('vo-ui-reload', { cancelable: true, detail: {version:'2'} })));
        await page.getByRole('heading', { name: 'Revision 2' }).waitFor();
        await page.waitForFunction(() => window.reloadLab.active === 1 && window.reloadLab.starts === 2);
        assert.equal(await page.evaluate(() => window.pageIdentity === window.identity), true);
        assert.equal(await page.locator('[data-reload-count]').textContent(), '1');
        assert.equal(await page.locator('[data-reload-items] button').first().textContent(), 'b:0');
        assert.equal(await page.locator('[data-item=a]').textContent(), 'a:1');
        assert.equal(await page.locator('#reload-name').inputValue(), 'Still editing');
        assert.deepEqual(await page.locator('#reload-choices').evaluate(select => [...select.selectedOptions].map(option => option.value)), ['a','c']);
        assert.equal(await page.locator('#reload-note').inputValue(), 'Private draft 中文');
        assert.equal(await page.locator('#reload-native-check').isChecked(), false);
        assert.equal(await page.locator('#reload-native-select').evaluate(select => select.selectedIndex), -1);
        assert.deepEqual(await page.locator('#reload-note').evaluate(input => [document.activeElement === input, input.selectionStart, input.selectionEnd, input.selectionDirection]), [true,2,7,'backward']);
        assert.equal(await page.locator('[data-reload-pending]').textContent(), 'false');
        assert.equal(await page.evaluate(() => window.reloadLab.aborts), 1);
        await page.evaluate(() => { window.reloadLab.saves[0]('late old save'); window.reloadLab.emits[0]('late old clock'); });
        await page.locator('#reload-add').click();
        await page.waitForFunction(() => document.querySelector('[data-reload-count]').textContent === '3');
        assert.equal(await page.locator('[data-reload-message]').textContent(), '');
        assert.equal(await page.locator('[data-reload-ticks]').textContent(), '0');
        assert.equal(await page.locator('[data-ui-inspector]').count(), 1);
        for (const [index, broken] of [0, 4].entries()) {
          revision = broken;
          await page.evaluate(version => window.dispatchEvent(new CustomEvent('vo-ui-reload', {cancelable:true,detail:{version}})), String(broken));
          await page.waitForFunction(count => window.reloadErrors === count, index + 1);
          await page.locator('#ui-development-error').waitFor();
          assert.equal(await page.getByRole('heading').textContent(), 'Revision 2');
          assert.equal(await page.locator('#root').getAttribute('inert'), null);
          await page.locator('#reload-add').click();
          await page.waitForFunction(count => Number(document.querySelector('[data-reload-count]').textContent) === count, 5 + 2 * index);
        }
        revision = 3;
        const beforeRequests = artifactRequests;
        await page.locator('#reload-name').focus();
        await page.locator('#reload-name').evaluate(input => {
          input.dispatchEvent(new CompositionEvent('compositionstart', {bubbles:true}));
          input.value = 'Composition 中文';
          input.dispatchEvent(new InputEvent('input', {bubbles:true,isComposing:true}));
          window.dispatchEvent(new CustomEvent('vo-ui-reload', {cancelable:true,detail:{version:'3'}}));
        });
        await page.waitForTimeout(30);
        assert.equal(artifactRequests, beforeRequests, 'reload interrupted active composition');
        await page.locator('#reload-name').evaluate(input => input.dispatchEvent(new CompositionEvent('compositionend', {bubbles:true})));
        await page.getByRole('heading', {name:'Revision 3'}).waitFor();
        await page.waitForFunction(() => window.reloadLab.active === 1 && window.reloadLab.starts === 3);
        assert.equal(await page.locator('[data-reload-count]').textContent(), 'new');
        assert.equal(await page.locator('#reload-name').inputValue(), 'Composition 中文');
        await page.waitForFunction(() => document.getElementById('ui-development-reload')?.textContent.includes('1 reset'));
        await page.evaluate(async () => { window.reloadApplication.close(); await window.reloadApplication.done; });
        assert.equal(await page.evaluate(() => window.reloadLab.active), 0);
        assert.equal(await page.locator('[data-ui-inspector]').count(), 0);
        assert.deepEqual(errors, []);
        reports.push({engine,backend,passed:true,contracts:['same-page-replacement','typed-and-form-state','keyed-child-reorder','native-edits-and-selection','focus-and-caret','pending-save-cancel','late-result-isolation','one-owned-subscription','failed-load-and-render-retain-page','composition-deferral','incompatible-state-reset','development-status','close-cleanup']});
        console.log(engine, backend, 'reload contracts passed');
        await page.close();
      }
    } finally { await browser.close(); }
  }
  await writeFile(resolve(root,'target/ui-next/reload-report.json'),JSON.stringify({passed:true,reports,artifacts},null,2)+'\n');
} finally {
  await server?.close();
  await rm(directory,{recursive:true,force:true});
}
