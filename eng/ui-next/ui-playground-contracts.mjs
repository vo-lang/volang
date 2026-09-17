import {sourceEditor} from './editor-controls.mjs';
import assert from 'node:assert/strict';

export const uiPlaygroundContracts = ['ui-source-lazy-compile', 'ui-source-interactive-worker', 'ui-preview-rerun-reset',
  'ui-preview-theme-and-document-isolation', 'ui-preview-compile-error-recovery', 'ui-preview-stop-infinite-program',
  'ui-preview-route-disposal', 'ui-preview-independent-draft', 'ui-preview-empty-draft', 'ui-preview-mobile'];

export async function checkUiPlayground(page, screenshot) {
  await page.getByRole('link', { name: 'Playground', exact: true }).click();
  const consoleDraft = await page.evaluate(() => localStorage.getItem('volang.studio.next.draft.v1'))
    ?? await page.locator('#playground-source').inputValue();
  await page.waitForFunction(source => document.querySelector('#playground-source')?.value === source, consoleDraft);
  await page.getByRole('link', { name: 'Try UI components →', exact: true }).click();
  const editor = sourceEditor(page, 'ui-playground-source'), run = page.locator('[data-run-preview]');
  await editor.waitFor();
  await page.getByRole('button', { name: 'Reset example', exact: true }).click();
  const sample = await editor.inputValue();
  const status = page.locator('[data-preview-status]');
  const frame = page.frameLocator('iframe[title="Interactive UI preview"]');
  const ready = () => page.waitForFunction(() => document.querySelector('[data-preview-status]')?.textContent.startsWith('Your preview is ready'), null, { timeout: 35000 });
  const start = async () => {
    const before = await page.evaluate(() => window.__studioNext.workers.started);
    await run.click();
    await page.waitForFunction(count => window.__studioNext.workers.started > count, before);
    await ready();
  };
  await start();
  const button = frame.getByRole('button', { name: 'One more idea', exact: true });
  await button.click();
  await button.click();
  await page.waitForFunction(() => document.querySelector('iframe')?.contentDocument.querySelector('output')?.textContent === '2 little ideas');
  assert.equal(await page.locator('output').count(), 0, 'example content escaped its document');
  await page.locator('#studio-theme').check();
  await page.waitForFunction(() => document.querySelector('iframe').contentDocument.body.dataset.theme === 'dark');
  await page.locator('#studio-theme').uncheck();
  await start();
  assert.equal(await frame.locator('output').textContent(), '0 little ideas');
  if (screenshot) {
    await page.locator('.studio-output').screenshot({ path: screenshot });
  }
  const viewport = page.viewportSize();
  await page.setViewportSize({ width: 390, height: 844 });
  assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true, 'UI editor overflows a narrow screen');
  assert.equal(await frame.locator('body').evaluate(() => document.documentElement.scrollWidth <= innerWidth), true, 'UI example overflows its narrow frame');
  if (screenshot) await page.locator('.studio-output').screenshot({ path: screenshot.replace('.png', '-mobile.png') });
  await page.setViewportSize(viewport);
  const workers = await page.evaluate(() => ({ ...window.__studioNext.workers }));
  assert.equal(workers.started - workers.stopped, 1, 'rerun retained the previous preview Worker');
  await editor.fill('package main\nfunc main() { missingPreviewName() }');
  await run.click();
  await page.waitForFunction(() => document.querySelector('[data-preview-status]')?.textContent.includes('missingPreviewName'));
  assert.equal(await page.locator('iframe').count(), 0, 'compile failure left its frame mounted');
  await editor.fill(sample.replace('Make something good.', 'Hello from your edited UI.'));
  await start();
  await frame.getByRole('heading', { name: 'Hello from your edited UI.' }).waitFor();
  await editor.fill(sample.replace('count.Set(count.Get() + 1)', 'for {}'));
  await start();
  await frame.getByRole('button', { name: 'One more idea', exact: true }).click();
  // A running Vo event handler now spins without another host exchange.
  // The outer Stop button remains available on the document thread.
  await page.waitForTimeout(300);
  await page.locator('[data-stop-preview]').click();
  await page.waitForFunction(() => window.__studioNext.workers.started === window.__studioNext.workers.stopped);
  assert.match(await status.textContent(), /^Stopped/);
  assert.equal(await page.locator('iframe').count(), 0);
  await editor.fill(sample);
  await page.waitForFunction(source => localStorage.getItem('volang.studio.next.ui-draft.v1') === source, sample);
  await start();
  await page.getByRole('link', { name: 'Back to console examples →', exact: true }).click();
  await page.locator('#playground-source').waitFor();
  await page.waitForFunction(source => document.querySelector('#playground-source')?.value === source, consoleDraft);
  await page.waitForFunction(() => window.__studioNext.workers.started === window.__studioNext.workers.stopped);
  await page.getByRole('link', { name: 'Try UI components →', exact: true }).click();
  await page.waitForFunction(source => document.querySelector('#ui-playground-source')?.value === source, sample);
  await editor.fill('');
  await page.waitForFunction(() => localStorage.getItem('volang.studio.next.ui-draft.v1') === '');
  await page.getByRole('link', { name: 'Back to console examples →', exact: true }).click();
  await page.locator('#playground-source').waitFor();
  await page.getByRole('link', { name: 'Try UI components →', exact: true }).click();
  await page.waitForFunction(() => document.querySelector('#ui-playground-source')?.value === '');
  await editor.fill(sample);
  await page.waitForFunction(source => localStorage.getItem('volang.studio.next.ui-draft.v1') === source, sample);
  await page.getByRole('link', { name: 'Gallery', exact: true }).click();
}

export async function checkUiPlaygroundSsr(browser, url) {
  const reports = [];
  for (const backend of ['vm']) {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    let release;
    const gate = new Promise(resolve => { release = resolve; });
    try {
      await page.addInitScript(() => {
        if (window !== window.top) return;
        localStorage.setItem('volang.studio.next.ui-draft.v1', 'older saved draft');
        localStorage.setItem('volang.studio.next.draft.v1', 'independent console draft');
      });
      await page.route('**/artifacts/studio.*', async route => { await gate; await route.continue(); });
      const response = await page.goto(`${url}/studio/playground/ui?backend=${backend}&ssr`, { waitUntil: 'commit' });
      assert(response?.ok(), `Studio UI preview SSR failed: ${response?.status()} ${await response?.text()}`);
      const editor = sourceEditor(page, 'ui-playground-source');
      await editor.waitFor();
      const source = (await editor.inputValue()).replace('Make something good.', 'Written before boot 中文');
      await editor.fill(source);
      await editor.evaluate(element => { window.earlyUiEditor = element; });
      release();
      await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      await page.waitForFunction(source => localStorage.getItem('volang.studio.next.ui-draft.v1') === source, source);
      assert.equal(await editor.inputValue(), source, 'late draft restore replaced pre-boot editing');
      assert.equal(await page.evaluate(() => window.earlyUiEditor === document.querySelector('#ui-playground-source')), true);
      assert.equal(await page.evaluate(() => localStorage.getItem('volang.studio.next.draft.v1')), 'independent console draft');
      await page.locator('[data-run-preview]').click();
      await page.frameLocator('iframe').getByRole('heading', { name: 'Written before boot 中文', exact: true }).waitFor({ timeout: 35000 });
      await page.evaluate(async () => { window.__studioNext.close(); await window.__studioNext.done; });
      await page.waitForFunction(() => window.__studioNext.workers.started === window.__studioNext.workers.stopped);
      assert.deepEqual(errors, []);
      reports.push({ backend, mode: 'hydrate-ui-preview', passed: true, contracts: ['ui-preview-server-editor', 'ui-preview-early-edit-retained',
        'ui-preview-late-draft-does-not-overwrite', 'ui-preview-hydrated-compile', 'ui-preview-root-close-disposal'] });
    } finally { release(); await page.close(); }
  }
  return reports;
}
