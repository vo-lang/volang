import assert from 'node:assert/strict';

export async function checkSharedData(page, requests) {
  const result = page.locator('[data-remote-result]');
  const mirror = page.locator('[data-remote-mirror]');
  const count = () => requests.filter(url => url.includes('/api/search?q=shared-proof')).length;
  await page.locator('#remote-search').fill('shared-proof');
  await page.waitForFunction(() => document.querySelector('[data-remote-result]').textContent === 'Results for shared-proof'
    && document.querySelector('[data-remote-mirror]').textContent === 'Results for shared-proof');
  assert.equal(count(), 1, 'two observers sent duplicate HTTP requests');
  await page.evaluate(() => { window.sharedResult = document.querySelector('[data-remote-result]'); });
  await page.locator('[data-toggle-mirror]').click();
  await mirror.waitFor({ state: 'detached' });
  assert.equal(await page.evaluate(() => window.sharedResult === document.querySelector('[data-remote-result]')), true);
  await page.locator('[data-toggle-mirror]').click();
  await mirror.waitFor();
  assert.equal(await mirror.textContent(), 'Results for shared-proof');
  assert.equal(count(), 1, 'new observer ignored the fresh cache');
  await page.locator('[data-toggle-results]').click();
  await result.waitFor({ state: 'detached' });
  await page.locator('[data-toggle-results]').click();
  await result.waitFor();
  assert.equal(await result.textContent(), 'Results for shared-proof');
  assert.equal(count(), 1, 'inactive cache was lost when the last observer left');
  const refreshed = page.waitForResponse(response => response.url().includes('/api/search?q=shared-proof'));
  await page.locator('[data-retry]').click();
  await refreshed;
  assert.equal(count(), 2, 'shared invalidation did not produce exactly one refresh');
  assert.equal(await result.textContent(), 'Results for shared-proof');
  assert.equal(await mirror.textContent(), 'Results for shared-proof');
}

export async function checkHydratedData(browser, url) {
  const reports = [];
  for (const backend of ['vm']) for (const stale of [false, true]) {
    const page = await browser.newPage(), requests = [], errors = [];
    page.on('request', request => { if (request.url().includes('/api/search?q=welcome')) requests.push(request.url()); });
    page.on('pageerror', error => errors.push(String(error)));
    let release;
    const gate = new Promise(resolve => { release = resolve; });
    try {
      await page.addInitScript(() => {
        window.seedRefreshing = false;
        new MutationObserver(() => {
          const first = document.querySelector('[data-remote-result]'), second = document.querySelector('[data-remote-mirror]');
          if (first?.dataset.fetching === 'true' && second?.dataset.fetching === 'true'
            && first.textContent === 'Results for welcome' && second.textContent === first.textContent) window.seedRefreshing = true;
        }).observe(document, { childList: true, subtree: true, attributes: true, attributeFilter: ['data-fetching'] });
      });
      await page.route('**/artifacts/workbench.*', async route => { await gate; await route.continue(); });
      await page.goto(`${url}/?example=workbench&backend=${backend}&ssr${stale ? '&stale' : ''}`, { waitUntil: 'commit' });
      await page.locator('[data-remote-result]').waitFor();
      assert.equal(await page.locator('[data-remote-result]').textContent(), 'Results for welcome', 'server HTML omitted initial query data');
      assert.equal(await page.locator('[data-remote-mirror]').getAttribute('data-fetching'), 'false');
      assert.equal(requests.length, 0, 'a server snapshot started browser I/O before boot');
      await page.evaluate(() => { window.serverSeedResult = document.querySelector('[data-remote-result]'); });
      release();
      await page.waitForFunction(() => window.__uiNext?.ready || window.__uiNext?.error);
      assert.equal(await page.evaluate(() => window.__uiNext.error), null);
      if (stale) {
        await page.waitForFunction(() => window.seedRefreshing && document.querySelector('[data-remote-result]').dataset.fetching === 'false');
        assert.equal(requests.length, 1, 'stale observers did not share exactly one refresh');
      } else {
        assert.equal(requests.length, 0, 'fresh server data was fetched again');
        assert.equal(await page.evaluate(() => window.seedRefreshing), false);
      }
      assert.equal(await page.evaluate(() => window.serverSeedResult === document.querySelector('[data-remote-result]')), true);
      assert.equal(await page.locator('[data-remote-mirror]').textContent(), 'Results for welcome');
      const previous = requests.length;
      await page.evaluate(() => { window.seedRefreshing = false; });
      await page.locator('[data-retry]').click();
      await page.waitForFunction(() => window.seedRefreshing && document.querySelector('[data-remote-result]').dataset.fetching === 'false');
      assert.equal(requests.length, previous + 1, 'seeded cache invalidation did not issue one fresh read');
      await page.evaluate(async () => { window.__uiNext.close(); await window.__uiNext.done; });
      assert.deepEqual(errors, []);
      reports.push({ backend, mode: stale ? 'hydrate-stale-data' : 'hydrate-fresh-data', passed: true,
        contracts: ['server-data-before-wasm', 'seeded-dom-adoption', 'request-identity-transfer',
          stale ? 'post-commit-shared-revalidation' : 'no-duplicate-initial-request', 'retained-value-during-refresh', 'seeded-invalidation'] });
    } finally { release(); await page.close(); }
  }
  return reports;
}
