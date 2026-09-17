import assert from 'node:assert/strict';
import { resolve } from 'node:path';
import { root } from './server.mjs';
import { checkCustomElement } from './custom-element-contracts.mjs';
import { checkMultipleForm } from './multiple-form-contracts.mjs';
import { checkMultiSelect, checkEarlyMultiSelect, editEarlyMultiSelect } from './multiselect-contracts.mjs';
import { checkSharedData, checkHydratedData } from './data-contracts.mjs';

export async function checkWorkbench(browser, url, outputDirectory = resolve(root, 'target/ui-next')) {
  const reports = [];
  for (const backend of ['vm']) {
    const page = await browser.newPage({ viewport: { width: 1100, height: 900 } });
    const errors = [], failed = [], requests = [];
    page.on('pageerror', error => errors.push(String(error)));
    page.on('requestfailed', request => failed.push(request.url()));
    page.on('request', request => requests.push(request.url()));
    await page.goto(`${url}/?example=workbench&backend=${backend}`);
    await page.waitForFunction(() => (window.__uiNext?.ready && document.querySelector('[data-chart] canvas')) || window.__uiNext?.error);
    assert.equal(await page.evaluate(() => window.__uiNext.error), null);
    assert.equal(await page.locator('[data-theme]').getAttribute('data-theme'), 'sage');
    assert.equal(await page.locator('#profile-frequency').inputValue(), 'weekly');
    await checkSharedData(page, requests);
    await checkCustomElement(page);
    await checkMultipleForm(page);
    await checkMultiSelect(page);
    await page.locator('#team-ada').fill('Keep this note');
    await page.evaluate(() => {
      window.retainedNote = document.querySelector('#team-ada');
      window.retainedNote.setSelectionRange(2, 6);
      document.querySelector('[data-reorder="team"]').click();
    });
    await page.waitForFunction(() => document.querySelector('[data-catalog="team"] li').dataset.row === 'linus');
    assert.equal(await page.evaluate(() => document.querySelector('#team-ada') === window.retainedNote && document.activeElement === window.retainedNote && window.retainedNote.selectionStart === 2), true);
    assert.equal(await page.locator('#team-ada').inputValue(), 'Keep this note');
    assert.equal(await page.locator('#guests-ada').inputValue(), '');
    await page.locator('#team-filter').fill('grace');
    await page.waitForFunction(() => document.querySelector('[data-catalog="team"] [data-count]').textContent === '1 person');
    assert.equal(await page.locator('[data-catalog="guests"] li').count(), 3);

    await page.locator('#profile-name').fill('');
    await page.keyboard.press('Enter');
    await page.waitForFunction(() => document.querySelector('#profile-error').textContent.includes('Enter your name'));
    await page.getByRole('button', { name: 'Save preferences', exact: true }).click();
    await page.waitForFunction(() => document.activeElement.id === 'profile-name');
    await page.locator('#profile-name').fill('Grace');
    await page.locator('#profile-email').check();
    await page.locator('#profile-frequency').selectOption('weekly');
    await page.evaluate(() => { const form = document.querySelector('form'); form.requestSubmit(); form.requestSubmit(); });
    await page.waitForFunction(() => document.querySelector('[data-saved]').textContent === 'Grace · weekly · email true');
    await page.locator('#profile-name').fill('Cancelled edit');
    await page.getByRole('button', { name: 'Save preferences', exact: true }).click();
    await page.locator('[data-cancel-save]').click();
    await page.locator('[data-reset-form]').click();
    await page.waitForFunction(() => document.querySelector('#profile-name').value === 'Ada'
      && document.querySelector('#profile-frequency').value === 'weekly'
      && !document.querySelector('#profile-email').checked && document.activeElement.id === 'profile-name');

    const slow = page.waitForRequest(request => request.url().includes('/api/search?q=slow-A'));
    await page.locator('#remote-search').fill('slow-A');
    await slow;
    await page.locator('#remote-search').fill('B');
    await page.waitForFunction(() => document.querySelector('[data-remote-result]').textContent === 'Results for B');
    assert(failed.some(url => url.includes('q=slow-A')), 'superseded fetch was not aborted');
    await page.locator('#remote-search').fill('slow-timeout');
    await page.waitForFunction(() => document.querySelector('[data-remote-result]').textContent.includes('timed out after 300 ms'));
    const retry = page.waitForRequest(request => request.url().includes('q=slow-timeout'));
    await page.locator('[data-retry]').click();
    await retry;
    await page.waitForFunction(() => document.querySelector('[data-remote-result]').textContent.includes('timed out after 300 ms'));
    await page.locator('#remote-search').fill('error');
    await page.waitForFunction(() => document.querySelector('[data-remote-result]').textContent.includes('HTTP 503'));
    await page.locator('#remote-search').fill('recovered');
    await page.waitForFunction(() => document.querySelector('[data-remote-result]').textContent === 'Results for recovered');

    await page.evaluate(() => { window.originalCanvas = document.querySelector('[data-chart] canvas'); });
    await page.locator('[data-chart-update]').click();
    await page.waitForFunction(() => window.__uiNext.widgets.updates > 0);
    assert.equal(await page.evaluate(() => document.querySelector('[data-chart] canvas') === window.originalCanvas), true);
    await page.locator('[data-chart] .u-over').scrollIntoViewIfNeeded();
    const box = await page.locator('[data-chart] .u-over').boundingBox();
    await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
    await page.waitForFunction(() => Number(document.querySelector('[data-cursor]').dataset.index) >= 0);
    await page.setViewportSize({ width: 600, height: 900 });
    await page.waitForFunction(() => window.__uiNext.widgets.resizes > 0);
    for (let i = 0; i < 3; i++) {
      await page.locator('[data-chart-toggle]').click();
      await page.waitForFunction(() => !document.querySelector('[data-chart] canvas'));
      await page.locator('[data-chart-toggle]').click();
      await page.waitForFunction(() => document.querySelector('[data-chart] canvas'));
    }
    assert.equal(await page.locator('[data-saved]').textContent(), 'Grace · weekly · email true');
    assert.equal(await page.locator('[data-chart-error]').textContent(), '');
    await page.screenshot({ path: resolve(outputDirectory, `workbench-${backend}.png`), fullPage: true });
    const closing = page.waitForRequest(request => request.url().includes('/api/search?q=slow-close'));
    await page.locator('#remote-search').fill('slow-close');
    await closing;
    await page.evaluate(async () => { window.__uiNext.close(); await window.__uiNext.done; });
    const stats = await page.evaluate(() => window.__uiNext.widgets);
    assert.equal(stats.mounts, stats.disposals);
    const custom = await page.evaluate(() => window.__uiNext.customElements);
    assert.equal(custom.connections, custom.disconnections);
    assert.equal(await page.locator('#root').textContent(), '');
    assert.deepEqual(errors, []);
    reports.push({ backend, passed: true, widgets: stats,
      contracts: ['cross-package-content', 'independent-lists', 'keyed-input-focus', 'derived-filter', 'context', 'structured-form', 'multi-value-checkbox-form', 'validation', 'save-cancel', 'native-form-reset', 'shared-fetch-deduplication', 'observer-cache-reuse', 'inactive-cache-reuse', 'shared-invalidation', 'latest-fetch-abort', 'timeout-abort-retry', 'local-load-error', 'custom-element-properties-events-and-disposal', 'widget-update-identity', 'widget-event', 'widget-resize', 'widget-disposal'] });
    console.log(`${backend}: workbench forms, asynchronous work and uPlot contracts passed`);
    await page.close();
  }
  for (const backend of ['vm']) {
    const page = await browser.newPage();
    let release;
    const gate = new Promise(resolve => { release = resolve; });
    await page.route('**/artifacts/workbench.*', async route => { await gate; await route.continue(); });
    const response = await page.goto(`${url}/?example=workbench&backend=${backend}&ssr`, { waitUntil: 'commit' });
    assert.equal(response.status(), 200, await response.text());
    assert.equal(await page.locator('#profile-frequency').inputValue(), 'weekly', 'SSR did not select the non-first controlled option');
    await page.locator('#profile-name').fill('Before startup');
    await page.locator('#profile-email').check();
    await page.locator('#profile-frequency').selectOption('daily');
    await page.locator('#workbench-interests-option-1').check();
    await page.locator('#workbench-interests-option-0').uncheck();
    await editEarlyMultiSelect(page);
    await page.evaluate(() => { window.earlyInterest = document.getElementById('workbench-interests-option-1'); });
    assert.equal(await page.locator('[data-chart] canvas').count(), 0);
    assert.equal(await page.locator('vo-example-counter').count(), 0);
    await page.evaluate(() => { window.customPlaceholder = document.querySelector('[data-custom-counter]'); });
    release();
    await page.waitForFunction(() => document.querySelector('[data-chart] canvas') || window.__uiNext?.error);
    assert.equal(await page.evaluate(() => window.__uiNext.error), null);
    assert(await page.evaluate(() => window.customPlaceholder === document.querySelector('[data-custom-counter]')));
    await page.waitForFunction(() => document.querySelector('[data-multiple-values]').textContent === 'design');
    assert(await page.evaluate(() => window.earlyInterest === document.getElementById('workbench-interests-option-1')));
    await checkEarlyMultiSelect(page);
    await checkCustomElement(page);
    await checkMultipleForm(page);
    await checkMultiSelect(page);
    await page.locator('#profile-name').focus();
    await page.keyboard.press('Enter');
    await page.waitForFunction(() => document.querySelector('[data-saved]').textContent === 'Before startup · daily · email true');
    await page.evaluate(async () => { window.__uiNext.close(); await window.__uiNext.done; });
    const widgets = await page.evaluate(() => window.__uiNext.widgets);
    assert.equal(widgets.mounts, 1);
    assert.equal(widgets.disposals, 1);
    const custom = await page.evaluate(() => window.__uiNext.customElements);
    assert.equal(custom.connections, custom.disconnections);
    reports.push({ backend, mode: 'hydrate', passed: true, contracts: ['early-text-checkbox-select', 'early-multiple-selection', 'widget-once-after-hydration', 'custom-element-placeholder-adoption', 'hydrated-form-submit'] });
    console.log(`${backend}: workbench SSR input and widget adoption passed`);
    await page.close();
  }
  reports.push(...await checkHydratedData(browser, url));
  return reports;
}
