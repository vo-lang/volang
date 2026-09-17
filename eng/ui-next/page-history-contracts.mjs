import assert from 'node:assert/strict';

// Real full-document traversal. A cache hit is recorded explicitly; a reload
// must also activate successfully when the browser elects not to cache a page.
export async function checkPageHistory(browser, url, backend) {
  const page = await browser.newPage(), errors = [];
  page.on('pageerror', error => errors.push(error.message));
  await page.addInitScript(() => {
    window.persistedShows = 0;
    window.historyPageShown = false;
    window.addEventListener('pageshow', event => {
      window.historyPageShown = true;
      if (event.persisted) window.persistedShows++;
    });
  });
  try {
    await page.goto(`${url}profile?name=History&backend=${backend}`);
    await page.getByRole('textbox', {name:'Your name'}).fill('History draft');
    await page.getByRole('heading', {name:'Hello, History draft.', exact:true}).waitFor();
    await page.getByRole('button', {name:'One more', exact:true}).click();
    await page.waitForFunction(() => document.querySelector('output')?.textContent === '1');
    await page.evaluate(() => { window.historyOutput = document.querySelector('output'); window.historyPageShown = false; });
    await page.goto(url + 'away.html');
    await page.getByRole('heading', {name:'Another document', exact:true}).waitFor();
    await page.goBack({waitUntil:'commit'});
    await page.waitForFunction(() => window.historyPageShown === true);
    await page.getByRole('textbox', {name:'Your name'}).waitFor();
    const cached = await page.evaluate(() => window.persistedShows > 0);
    if (cached) {
      assert.equal(await page.locator('output').evaluate(element => element === window.historyOutput), true);
      assert.equal(await page.locator('output').textContent(), '1');
      assert.equal(await page.getByRole('textbox', {name:'Your name'}).inputValue(), 'History draft');
    }
    // Editing proves activation even after a non-cached traversal.
    await page.getByRole('textbox', {name:'Your name'}).fill('Back and ready');
    await page.getByRole('heading', {name:'Hello, Back and ready.', exact:true}).waitFor();
    const before = Number(await page.locator('output').textContent());
    await page.getByRole('button', {name:'One more', exact:true}).click();
    await page.waitForFunction(count => document.querySelector('output')?.textContent === String(count), before + 1);
    assert.deepEqual(errors, []);
    const cacheMiss = await page.evaluate(() => performance.getEntriesByType('navigation')[0]?.notRestoredReasons?.toJSON() ?? null);
    return {backend, passed:true, actualCacheRestore:cached, cacheMiss};
  } finally { await page.close(); }
}
