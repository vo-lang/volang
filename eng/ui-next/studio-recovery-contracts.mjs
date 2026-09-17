import assert from 'node:assert/strict';
import {mkdir, mkdtemp, readFile, rm} from 'node:fs/promises';
import {join} from 'node:path';
import {tmpdir} from 'node:os';
import {unzipSync} from './node_modules/fflate/esm/browser.js';

async function recoveryPage(browser) {
  if (browser.browserType().name() !== 'webkit') {
    const page = await browser.newPage();
    return {page, close:() => page.close()};
  }
  // WebKit's ephemeral sessions reject OPFS getDirectory. A persistent test
  // profile enables it; the caller also supplies an isolated origin per case.
  const profile = await mkdtemp(join(tmpdir(), 'volang-recovery-webkit-'));
  let context;
  try {
    context = await browser.browserType().launchPersistentContext(profile, {headless:true});
    return {page:context.pages()[0] ?? await context.newPage(), close:async () => {
      try {
        const page = context.pages()[0];
        if (page && !page.isClosed()) await page.evaluate(async () => {
          const probe = window.recoveryProbe;
          if (!probe?.fixtureStarted) return;
          probe.protect = false;
          const root = await probe.original();
          try {await root.removeEntry('vo-web-vfs-v1', {recursive:true});}
          catch (error) {if (error.name !== 'NotFoundError') throw error;}
        });
      } finally {try {await context.close();} finally {await rm(profile, {recursive:true, force:true});}}
    }};
  } catch (error) {await context?.close(); await rm(profile, {recursive:true, force:true}); throw error;}
}

// Each caller supplies a fresh Playwright page/context. These fixtures never
// connect to a user's browser profile or instantiate the old writable VFS.
export async function instrumentRecovery(page) {
  await page.addInitScript(() => {
    const storage = navigator.storage, original = storage.getDirectory.bind(storage);
    const probe = window.recoveryProbe = {reads:0, mode:'normal', urls:[], revoked:[], protect:false, original};
    storage.getDirectory = () => {
      probe.reads++;
      if (probe.mode === 'missing') return Promise.reject(new DOMException('No workspace', 'NotFoundError'));
      if (probe.mode === 'failure') return Promise.reject(new Error('The saved project could not be read.'));
      if (probe.mode === 'pending') return new Promise(resolve => {probe.release = async () => resolve(await original());});
      return original();
    };
    for (const [prototype, name, writes] of [
      [FileSystemDirectoryHandle.prototype, 'getDirectoryHandle', args => args[1]?.create],
      [FileSystemDirectoryHandle.prototype, 'getFileHandle', args => args[1]?.create],
      [FileSystemDirectoryHandle.prototype, 'removeEntry', () => true],
      [FileSystemFileHandle.prototype, 'createWritable', () => true],
    ]) {
      const method = prototype[name];
      prototype[name] = function(...args) {
        if (probe.protect && writes(args)) throw new Error('Recovery attempted to mutate browser storage.');
        return method.apply(this, args);
      };
    }
    const create = URL.createObjectURL.bind(URL), revoke = URL.revokeObjectURL.bind(URL);
    URL.createObjectURL = blob => {const url = create(blob); probe.urls.push(url); return url;};
    URL.revokeObjectURL = url => {probe.revoked.push(url); revoke(url);};
  });
}

export async function seedRecovery(page) {
  await page.evaluate(async () => {
    let directory = await window.recoveryProbe.original();
    for await (const [name] of directory.entries()) throw new Error(`Recovery fixture ${location.origin} already contains ${name}.`);
    window.recoveryProbe.fixtureStarted = true;
    for (const name of ['vo-web-vfs-v1', 'data', 'workspace']) directory = await directory.getDirectoryHandle(name, {create:true});
    const write = async (parent, name, bytes) => {
      const file = await parent.getFileHandle(name, {create:true}), writer = await file.createWritable();
      await writer.write(bytes); await writer.close();
    };
    await write(directory, '.volang-studio-projects.json', '{invalid catalog');
    const project = await directory.getDirectoryHandle('小花园', {create:true});
    await write(project, 'main.vo', 'package main\nfunc main() { println("你好") }\n');
    await write(project, '.hidden', 'keep me');
    await write(project, 'pixels.bin', new Uint8Array([0, 255, 128, 1, 0]));
    await write(project, 'empty.txt', '');
    await project.getDirectoryHandle('空目录', {create:true});
    await directory.getDirectoryHandle('unfinished', {create:true});
    await directory.getDirectoryHandle('.studio-create-remaining', {create:true});
    window.recoveryProbe.protect = true;
  });
}

export async function snapshot(page) {
  return page.evaluate(async () => {
    const result = [];
    const visit = async (directory, prefix) => {
      for await (const [name, handle] of directory.entries()) {
        const path = prefix + name;
        if (handle.kind === 'directory') {result.push([path + '/', null]); await visit(handle, path + '/');}
        else {const file = await handle.getFile(); result.push([path, Array.from(new Uint8Array(await file.arrayBuffer())), file.lastModified]);}
      }
    };
    await visit(await window.recoveryProbe.original(), '');
    return result.sort((a,b) => a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0);
  });
}

const ready = async page => {
  await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
  assert.equal(await page.evaluate(() => window.__studioNext.error), null);
};
const scan = async page => {
  await page.getByRole('button', {name:'Find browser projects', exact:true}).click();
  await page.getByLabel('Browser project', {exact:true}).waitFor();
};
const download = page => page.locator('a[download]');
const libraryURL = /\/recovery-library(?:-[a-z0-9]+)?\.js(?:\?.*)?$/i;
const prepare = async page => {
  await page.getByRole('button', {name:'Prepare download', exact:true}).click();
  await download(page).waitFor();
  return download(page).getAttribute('href');
};
const assertRevoked = async (page, url) => {
  await page.waitForFunction(url => window.recoveryProbe.revoked.includes(url), url);
  assert.equal(await page.evaluate(async url => {try {await fetch(url); return false;} catch {return true;}}, url), true);
};

export async function checkStudioRecovery(browser, url, outputDirectory, {createOrigin} = {}) {
  const reports = [], origins = [];
  const open = async () => {
    const isolated = browser.browserType().name() === 'webkit';
    if (isolated && !createOrigin) throw new Error('WebKit OPFS tests require an isolated origin factory.');
    const origin = isolated ? await createOrigin() : null;
    // Keep each listener until the suite ends: closing one per case permits
    // the OS to reuse its port while WebKit is still retiring origin storage.
    // A persistent profile alone does not isolate OPFS on every WebKit build.
    if (origin) {
      assert(!origins.some(previous => previous.url === origin.url), 'recovery origins must be distinct');
      origins.push(origin);
    }
    const session = await recoveryPage(browser);
    return {...session, url:origin?.url.replace(/\/$/, '') ?? url};
  };
  try {
    if (outputDirectory) await mkdir(outputDirectory, {recursive:true});
    const response = await fetch(url + '/studio/recover?ssr=1');
    assert.equal(response.status, 200);
    const html = await response.text();
    assert.match(html, /Save all changes/);
    assert.match(html, /data-vo-id=/);
    for (const backend of ['vm']) {
      const {page, close, url:caseURL} = await open(), requests = [], errors = [];
      page.on('request', request => requests.push(request.url()));
      page.on('pageerror', error => errors.push(error.message));
      await instrumentRecovery(page);
      try {
        await page.goto(`${caseURL}/studio/gallery?backend=${backend}`); await ready(page);
        assert(!requests.some(url => /recovery-storage|recovery-library/.test(url)));
        await seedRecovery(page);
        const before = await snapshot(page);
        await page.getByRole('link', {name:'Recover browser projects', exact:true}).click();
        await page.getByRole('heading', {name:'Bring your projects with you.'}).waitFor();
        await page.waitForFunction(() => document.title === 'Recover browser projects · Volang Studio');
        assert.equal(await page.locator('.studio-nav-link[aria-current]').count(), 0);
        assert.equal(await page.evaluate(() => window.recoveryProbe.reads), 0);
        await scan(page);
        assert.deepEqual(await page.locator('#recovery-project option').allTextContents(), ['.studio-create-remaining','unfinished','小花园']);
        assert(!requests.some(url => /recovery-library/.test(url)), 'discovery loaded the optional ZIP library');
        await page.getByLabel('Browser project', {exact:true}).selectOption('小花园');
        const first = await prepare(page);
        assert.equal(await download(page).getAttribute('download'), 'volang-小花园.zip');
        const downloaded = page.waitForEvent('download');
        await download(page).click();
        const file = await downloaded;
        assert.equal(file.suggestedFilename(), 'volang-小花园.zip');
        const bytes = await readFile(await file.path());
        const contents = unzipSync(bytes);
        assert.deepEqual(Object.keys(contents).sort(), ['小花园/','小花园/.hidden','小花园/empty.txt','小花园/main.vo','小花园/pixels.bin','小花园/空目录/'].sort());
        assert.equal(new TextDecoder().decode(contents['小花园/main.vo']), 'package main\nfunc main() { println("你好") }\n');
        assert.deepEqual([...contents['小花园/pixels.bin']], [0,255,128,1,0]);
        assert.equal(new TextDecoder().decode(contents['小花园/.hidden']), 'keep me');
        assert.equal(contents['小花园/empty.txt'].length, 0);
        if (outputDirectory) {
          await file.saveAs(join(outputDirectory, `${backend}.zip`));
          if (backend === 'vm') {
            await page.evaluate(() => scrollTo(0, 0));
            await page.screenshot({path:join(outputDirectory, 'desktop.png'), fullPage:true});
            await page.setViewportSize({width:390, height:844});
            assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
            await page.screenshot({path:join(outputDirectory, 'mobile.png'), fullPage:true});
          }
        }
        await page.getByLabel('Browser project', {exact:true}).selectOption('unfinished');
        await assertRevoked(page, first);
        assert.equal(await download(page).count(), 0);
        const second = await prepare(page);
        await page.getByRole('button', {name:'Release download', exact:true}).click();
        await assertRevoked(page, second);
        const third = await prepare(page);
        await page.getByRole('link', {name:'Gallery', exact:true}).click();
        await page.getByRole('button', {name:'Make it happen', exact:true}).waitFor();
        await assertRevoked(page, third);
        assert.deepEqual(await snapshot(page), before, 'recovery changed the saved workspace');
        assert(!requests.some(url => /editor-library|\/compiler\//.test(url)), 'recovery loaded unrelated tools');
        assert.deepEqual(errors, []);
        assert.equal(await page.evaluate(() => window.__studioNext.error), null);
        reports.push({backend, mode:'read-only-download', passed:true});
        console.log(`${browser.browserType().name()} ${backend}: recovery read-only download passed`);
      } finally {await close();}

      const {page:unavailable, close:closeUnavailable, url:unavailableURL} = await open();
      await instrumentRecovery(unavailable);
      try {
        await unavailable.goto(`${unavailableURL}/studio/recover?backend=${backend}`); await ready(unavailable);
        await unavailable.getByRole('button', {name:'Find browser projects', exact:true}).click();
        await unavailable.getByRole('status').filter({hasText:'No projects found here.'}).waitFor();
        assert.deepEqual(await snapshot(unavailable), [], 'empty inspection initialized old storage');
        await seedRecovery(unavailable);
        await unavailable.evaluate(() => {window.recoveryProbe.mode = 'failure';});
        await unavailable.getByRole('button', {name:'Find browser projects', exact:true}).click();
        await unavailable.getByRole('alert').filter({hasText:'The saved project could not be read.'}).waitFor();
        assert.equal(await unavailable.evaluate(() => window.__studioNext.error), null);
        await unavailable.evaluate(() => {window.recoveryProbe.mode = 'normal';}); await scan(unavailable);
        await unavailable.evaluate(() => {window.recoveryProbe.mode = 'pending';});
        await unavailable.getByRole('button', {name:'Find browser projects', exact:true}).click();
        await unavailable.waitForFunction(() => window.recoveryProbe.release);
        await unavailable.getByRole('button', {name:'Cancel', exact:true}).click();
        await unavailable.waitForFunction(() => ![...document.querySelectorAll('button')].some(button => button.textContent === 'Cancel'));
        await unavailable.evaluate(async () => {await window.recoveryProbe.release(); window.recoveryProbe.mode = 'normal';});
        await scan(unavailable);
        await unavailable.evaluate(() => {navigator.storage.getDirectory = undefined;});
        await unavailable.getByRole('button', {name:'Find browser projects', exact:true}).click();
        await unavailable.getByRole('alert').filter({hasText:'Browser project storage is unavailable.'}).waitFor();
        assert.equal(await unavailable.evaluate(() => window.__studioNext.error), null);
        reports.push({backend, mode:'empty-errors-cancellation-retry', passed:true});
        console.log(`${browser.browserType().name()} ${backend}: recovery empty, errors and retry passed`);
      } catch (error) {
        throw new Error(`Recovery ${backend} empty/error state: ${await unavailable.locator('.studio-recovery').innerText()}`, {cause:error});
      } finally {await closeUnavailable();}

      for (const mode of ['cancel', 'leave']) {
        const {page:pending, close:closePending, url:pendingURL} = await open(); let release;
        const gate = new Promise(resolve => {release = resolve;});
        await instrumentRecovery(pending);
        await pending.route(libraryURL, async route => {
          try {
            await gate;
            const response = await route.fetch();
            await route.fulfill({response, body:(await response.text()) + '\n;globalThis.recoveryLibraryEvaluated = true;'});
          } catch (error) {if (!pending.isClosed() && !route.request().failure()) throw error;}
        });
        try {
          await pending.goto(`${pendingURL}/studio/recover?backend=${backend}`); await ready(pending);
          await seedRecovery(pending); await scan(pending);
          const requested = pending.waitForRequest(libraryURL);
          await pending.getByRole('button', {name:'Prepare download', exact:true}).click(); await requested;
          if (mode === 'cancel') {
            await pending.getByRole('button', {name:'Cancel', exact:true}).click();
            await pending.waitForFunction(() => ![...document.querySelectorAll('button')].some(button => button.textContent === 'Cancel'));
          } else {
            await pending.getByRole('link', {name:'Gallery', exact:true}).click();
            await pending.getByRole('button', {name:'Make it happen', exact:true}).waitFor();
          }
          release(); await pending.waitForFunction(() => window.recoveryLibraryEvaluated);
          assert.deepEqual(await pending.evaluate(() => window.recoveryProbe.urls), []);
          assert.equal(await download(pending).count(), 0);
          assert.equal(await pending.evaluate(() => window.__studioNext.error), null);
          reports.push({backend, mode:`late-library-${mode}`, passed:true});
          console.log(`${browser.browserType().name()} ${backend}: recovery late library ${mode} passed`);
        } finally {release(); await closePending();}
      }
    }
    return reports;
  } finally {await Promise.all(origins.map(origin => origin.close()));}
}
