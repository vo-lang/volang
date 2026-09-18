import {sourceEditor} from './editor-controls.mjs';
import assert from 'node:assert/strict';
import { resolve } from 'node:path';
import { root } from './repository-paths.mjs';
import { consoleOutputs, uiExampleIDs } from './studio-example-expectations.mjs';

export const exampleContracts = ['six-console-examples', 'three-ui-examples', 'explicit-open-keeps-current-draft',
  'replace-stops-running-work', 'restore-previous-draft', 'native-editor-focus', 'failed-read-does-not-save-default',
  'intentional-edit-can-recover-storage', 'example-hydration-keeps-early-selection-and-edit', 'examples-mobile-layout'];

async function ready(page) {
  await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
  assert.equal(await page.evaluate(() => window.__studioNext.error), null);
}
async function sourceIs(page, expected) {
  await page.waitForFunction(expected => document.querySelector('textarea')?.value === expected, expected);
}
async function idle(page) {
  await page.waitForFunction(() => window.__studioNext.workers.started === window.__studioNext.workers.stopped);
}

// Read examples through the public selector. Their native compilation is checked
// by the build gate; this journey verifies the delivered code and its observable
// output without requiring a compiler or application source on the test host.
async function openExample(page,id,sources,editorID="playground-source") {
  const editor=sourceEditor(page,editorID),draft=`package main\nfunc main() { println("Private draft: ${id}") }\n`;
  await editor.fill(draft);
  await page.getByLabel('Choose an example',{exact:true}).selectOption(id);
  assert.equal(await editor.inputValue(),draft,'selection replaced a draft without Open');
  await page.getByRole('button',{name:'Open example',exact:true}).click();
  await page.waitForFunction(draft=>document.querySelector('textarea')?.value!==draft,draft);
  const source=await editor.inputValue();
  assert(source.startsWith('package main')&&source.length<=100000,'example source is missing or oversized');
  if(sources.has(id))assert.equal(source,sources.get(id),'example differs across activation/backend: '+id);
  else sources.set(id,source);
}

export async function checkStudioExamplesBrowser(browser, url, outputDirectory = resolve(root, 'target/ui-next')) {
  const catalog = {console:Object.keys(consoleOutputs),ui:uiExampleIDs}, reports = [], sources = new Map();
  for (const backend of ['vm']) {
    const page = await browser.newPage({ viewport: { width: 1400, height: 1000 } });
    const errors = [], requests = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('request', request => requests.push(request.url()));
    try {
      await page.goto(`${url}/studio/playground?backend=${backend}`);
      await ready(page);
      const editor = sourceEditor(page);
      assert.deepEqual(await page.getByLabel('Choose an example',{exact:true}).locator('option').evaluateAll(options=>options.map(option=>option.value)),catalog.console);
      const draft = 'package main\nfunc main() { println("A private draft 中文") }\n';
      await editor.fill(draft);
      await page.getByLabel('Choose an example', { exact: true }).selectOption('channels');
      assert.equal(await editor.inputValue(), draft);
      await page.getByRole('button', { name: 'Open example', exact: true }).click();
      await page.waitForFunction(draft=>document.querySelector('textarea')?.value!==draft,draft);
      const channels=await editor.inputValue();
      if(sources.has('channels'))assert.equal(channels,sources.get('channels'));else sources.set('channels',channels);
      await page.waitForFunction(() => document.activeElement.id === 'playground-source' || document.activeElement.classList.contains('cm-content'));
      assert(!requests.some(url => url.includes('/compiler/')), 'opening an example loaded the compiler');
      await page.getByRole('button', { name: 'Restore previous draft', exact: true }).click();
      await sourceIs(page, draft);
      for (const id of catalog.console) {
        await openExample(page,id,sources);
        await page.getByRole('button', { name: 'Run code', exact: true }).click();
        await page.waitForFunction(expected => document.querySelector('[data-output]')?.textContent === expected,
          consoleOutputs[id], { timeout: 35000 });
        await idle(page);
      }
      await editor.fill('package main\nfunc main() { for {} }\n');
      await page.getByRole('button', { name: 'Run code', exact: true }).click();
      await page.getByLabel('Choose an example', { exact: true }).selectOption('hello-world');
      await page.getByRole('button', { name: 'Open example', exact: true }).click();
      await sourceIs(page, sources.get('hello-world'));
      await idle(page);
      await page.getByRole('link', { name: 'Try UI components →', exact: true }).click();
      await page.locator('#ui-example').waitFor();
      assert.deepEqual(await page.getByLabel('Choose an example',{exact:true}).locator('option').evaluateAll(options=>options.map(option=>option.value)),catalog.ui);
      for (const id of catalog.ui) {
        await openExample(page,id,sources,"ui-playground-source");
        assert.equal(await page.locator('iframe').count(), 0, 'replacing source retained the old preview');
        await page.getByRole('button', { name: 'Run preview', exact: true }).click();
        const frame = page.frameLocator('iframe');
        if (id === 'counter') {
          await frame.getByRole('button', { name: 'One more idea', exact: true }).click({ timeout: 35000 });
          await page.waitForFunction(() => document.querySelector('iframe')?.contentDocument.querySelector('output')?.textContent === '1 little ideas');
        } else if (id === 'filter') {
          await frame.getByRole('textbox', { name: 'Find a name', exact: true }).fill('Grace', { timeout: 35000 });
          await page.waitForFunction(() => document.querySelector('iframe')?.contentDocument.querySelectorAll('li').length === 1);
          assert.equal(await frame.getByRole('listitem').textContent(), 'Grace');
        } else {
          await frame.getByRole('checkbox', { name: 'Find an idea', exact: true }).check({ timeout: 35000 });
          assert.equal(await frame.getByRole('checkbox', { name: 'Make a small version', exact: true }).isChecked(), false);
        }
      }
      await page.screenshot({ path: resolve(outputDirectory, `studio-examples-${backend}.png`) });
      await page.setViewportSize({ width: 390, height: 844 });
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await page.screenshot({ path: resolve(outputDirectory, `studio-examples-mobile-${backend}.png`) });
      await page.getByRole('button', { name: 'Restore previous draft', exact: true }).click();
      await idle(page);
      assert.equal(await page.locator('iframe').count(), 0);
      assert.deepEqual(errors, []);
      reports.push({ backend, mode: 'client-examples', passed: true, contracts: exampleContracts });
    } finally { await page.close(); }

    const ssr = await browser.newPage();
    let release;
    const gate = new Promise(resolve => { release = resolve; });
    await ssr.route('**/artifacts/studio.*', async route => { await gate; await route.continue(); });
    try {
      await ssr.goto(`${url}/studio/playground?backend=${backend}&ssr`, { waitUntil: 'commit' });
      const draft = 'package main\nfunc main() { println("Before startup 中文") }\n';
      await sourceEditor(ssr).fill(draft);
      await ssr.getByLabel('Choose an example', { exact: true }).selectOption('channels');
      await ssr.evaluate(() => { window.beforeEditor = document.querySelector('textarea'); window.beforeChoice = document.querySelector('#console-example'); });
      release();
      await ready(ssr);
      await sourceIs(ssr, draft);
      assert.equal(await ssr.getByLabel('Choose an example', { exact: true }).inputValue(), 'channels');
      assert.equal(await ssr.evaluate(() => window.beforeEditor === document.querySelector('textarea') && window.beforeChoice === document.querySelector('#console-example')), true);
      await ssr.getByRole('button', { name: 'Open example', exact: true }).click();
      await sourceIs(ssr, sources.get('channels'));
      await ssr.getByRole('button', { name: 'Restore previous draft', exact: true }).click();
      await sourceIs(ssr, draft);
      reports.push({ backend, mode: 'hydrate-examples', passed: true, contracts: exampleContracts });
    } finally { release(); await ssr.close(); }

    const failedRead = await browser.newPage();
    try {
      await failedRead.addInitScript(() => {
        const read = IDBObjectStore.prototype.get, write = IDBObjectStore.prototype.put;
        window.draftWrites = [];
        IDBObjectStore.prototype.get = function(key) {
          if (key === 'volang.studio.next.draft.v1') throw new Error('Draft storage temporarily unavailable.');
          return read.call(this, key);
        };
        IDBObjectStore.prototype.put = function(value, key) {
          if (key === 'volang.studio.next.draft.v1') window.draftWrites.push(value);
          return write.call(this, value, key);
        };
      });
      await failedRead.goto(`${url}/studio/playground?backend=${backend}`);
      await ready(failedRead);
      await failedRead.getByText('Draft storage temporarily unavailable.', { exact: true }).waitFor();
      assert.deepEqual(await failedRead.evaluate(() => window.draftWrites), []);
      const intentional = 'package main\nfunc main() { println("A new draft") }\n';
      await sourceEditor(failedRead).fill(intentional);
      await failedRead.waitForFunction(value => window.draftWrites.includes(value), intentional);
      assert((await failedRead.evaluate(() => window.draftWrites)).every(value => value === intentional));
      reports.push({ backend, mode: 'draft-storage-recovery', passed: true });
    } finally { await failedRead.close(); }
  }
  return reports;
}
