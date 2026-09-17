import assert from 'node:assert/strict';
import {join} from 'node:path';
import {expect} from '../browser/node_modules/@playwright/test/index.mjs';

const ready = page => page.waitForFunction(() => document.getElementById('status')?.textContent === '');
const field = page => page.getByRole('textbox', {name:'Collection name'});
const submit = page => page.locator('[data-save-preferences]');

async function finishRoute(route, action) {
  if (route.request().failure()) return;
  try { await action(); }
  catch (error) { if (!route.request().failure()) throw error; }
}

export async function checkFieldnotesForms(browser, base) {
  const reports = [];
  for (const mode of ['no-script', 'early-default', 'early-vm']) {
    const page = await browser.newPage({javaScriptEnabled:mode !== 'no-script'}), errors = [];
    page.on('pageerror', error => errors.push(error.message));
    let release;
    const gate = new Promise(resolve => {release = resolve;});
    if (mode !== 'no-script') await page.route('**/assets/app.*', async route => {await gate; await route.continue();});
    try {
      const suffix = mode === 'early-vm' ? '?backend=vm' : '';
      await page.goto(base + 'preferences' + suffix, {waitUntil:'commit'});
      const name = field(page);
      await name.fill('x');
      const invalid = page.waitForNavigation({waitUntil:'commit'});
      await submit(page).click();
      assert.equal((await invalid).status(), 422);
      await expect(name).toHaveValue('x');
      await expect(name).toHaveAttribute('aria-invalid', 'true');
      await expect(page.locator('[data-save-state]')).toContainText('unsaved');
      if (mode === 'no-script') {
        await name.fill('Saved without JavaScript 中文');
        const saved = page.waitForNavigation();
        await submit(page).click();
        assert.equal((await saved).status(), 200);
        assert.equal(new URL(page.url()).searchParams.get('saved'), '1');
        await page.reload();
        await expect(name).toHaveValue('Saved without JavaScript 中文');
        const cookie = (await page.context().cookies()).find(item => item.name === 'fieldnotes_preferences');
        assert.equal(cookie.path, new URL(base).pathname);
        assert.equal(cookie.httpOnly, true);
        assert.equal(cookie.sameSite, 'Lax');
      } else {
        await name.fill('Edited during startup 中文');
        await name.evaluate(element => {window.keptInput = element;});
        release(); await ready(page);
        await expect(name).toHaveValue('Edited during startup 中文');
        assert(await name.evaluate(element => element === window.keptInput));
        await expect(name).not.toHaveAttribute('aria-invalid', 'true');
        await page.getByRole('button', {name:'Discard changes', exact:true}).click();
        await expect(name).toHaveValue('My collection');
        await page.getByRole('link', {name:'Library', exact:true}).click();
        await page.getByRole('heading', {name:'Ideas worth keeping.'}).waitFor();
        await page.getByRole('link', {name:'Preferences', exact:true}).click();
        await expect(name).toHaveValue('My collection');
        await expect(name).not.toHaveAttribute('aria-invalid', 'true');
      }
      assert.deepEqual(errors, []);
      reports.push({mode, passed:true});
    } finally {release(); await page.close();}
  }
  return reports;
}

export async function checkFieldnotes(browser, base, backend, screenshotDirectory) {
  const page = await browser.newPage({viewport:{width:1360, height:920}}), errors = [], requests = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('request', request => requests.push(request));
  let releaseRead = () => {}, releaseWrite = () => {};
  try {
    await page.goto(base + 'library?backend=' + backend);
    await ready(page);
    await expect(page.locator('.note-card')).toHaveCount(6);
    assert.equal(requests.filter(request => request.url().includes('/api/')).length, 0, 'fresh SSR data was fetched again');
    await page.locator('[data-library-layout]').evaluate(element => {window.keptLayout = element;});
    const search = page.getByRole('searchbox', {name:'Search the library'});

    // Change independent URL fields before either guest navigation completes.
    await page.evaluate(() => {
      const search = document.getElementById('library-search'), sort = document.getElementById('library-sort');
      search.focus(); search.value = 'design'; search.dispatchEvent(new InputEvent('input', {bubbles:true}));
      sort.value = 'title'; sort.dispatchEvent(new Event('change', {bubbles:true}));
    });
    await expect(page).toHaveURL(url => url.searchParams.get('q') === 'design' && url.searchParams.get('sort') === 'title');
    await expect(page.getByText('6 notes to explore', {exact:true})).toBeVisible();
    await expect(search).toBeFocused();
    const titles = await page.locator('.note-card h2').allTextContents();
    assert.deepEqual(titles, [...titles].sort());

    // An obsolete response must not replace the result of a newer URL query.
    let received;
    const arrived = new Promise(resolve => {received = resolve;});
    const gate = new Promise(resolve => {releaseRead = resolve;});
    const slowQuery = async route => {
      if (new URL(route.request().url()).searchParams.get('q') !== 'engineering') return route.continue();
      const response = await route.fetch(); received(); await gate;
      await finishRoute(route, () => route.fulfill({response}));
    };
    await page.route(base + 'api/notes?*', slowQuery);
    await search.fill('engineering'); await arrived;
    await search.fill('writing');
    await expect(page.locator('.note-card .topic')).toHaveText(Array(6).fill('Writing'));
    releaseRead();
    await page.unroute(base + 'api/notes?*', slowQuery);
    await expect(search).toHaveValue('writing');
    await expect(page.locator('.note-card .topic')).toHaveText(Array(6).fill('Writing'));

    let attempts = 0;
    const failOnce = async route => {
      if (new URL(route.request().url()).searchParams.get('q') === 'research' && attempts++ === 0) {
        return route.fulfill({status:503, contentType:'text/plain', body:'Temporary test interruption'});
      }
      await route.continue();
    };
    await page.route(base + 'api/notes?*', failOnce);
    await search.fill('research');
    await page.getByRole('heading', {name:"We couldn't load this just yet."}).waitFor();
    await page.getByRole('button', {name:'Try again', exact:true}).click();
    await expect(page.locator('.note-card .topic')).toHaveText(Array(6).fill('Research'));
    await page.unroute(base + 'api/notes?*', failOnce);
    await search.fill('no matching note');
    await page.locator('[data-empty]').waitFor();
    await search.fill('');
    await expect(page.getByText('24 notes to explore', {exact:true})).toBeVisible();
    await page.getByRole('combobox', {name:'Sort by'}).selectOption('recent');
    await page.getByRole('navigation', {name:'Pagination'}).getByRole('button', {name:'Next', exact:true}).click();
    await expect(page).toHaveURL(url => url.searchParams.get('page') === '2');
    await page.getByRole('link', {name:'Keep a field journal', exact:true}).click();
    await page.getByRole('heading', {name:'Keep a field journal', exact:true}).waitFor();
    assert(await page.locator('[data-library-layout]').evaluate(element => element === window.keptLayout));
    await expect(page.locator('#fieldnotes-content')).toBeFocused();
    await page.goBack();
    await expect(page.locator('[data-results-page]')).toHaveAttribute('data-results-page', '2');
    await page.goForward();
    await page.getByRole('heading', {name:'Keep a field journal', exact:true}).waitFor();
    await page.goBack();
    let readArrived;
    const readPending = new Promise(resolve => {readArrived = resolve;});
    const leavingGate = new Promise(resolve => {releaseRead = resolve;});
    const leavingQuery = async route => {
      if (new URL(route.request().url()).searchParams.get('q') !== 'feedback') return route.continue();
      const response = await route.fetch(); readArrived(); await leavingGate;
      await finishRoute(route, () => route.fulfill({response}));
    };
    await page.route(base + 'api/notes?*', leavingQuery);
    await search.fill('feedback'); await readPending;
    const cancelledRead = page.waitForEvent('requestfailed', {
      predicate:request => new URL(request.url()).searchParams.get('q') === 'feedback',
    });
    await page.getByRole('link', {name:'Preferences', exact:true}).click();
    await page.getByRole('heading', {name:'Your preferences.'}).waitFor();
    await cancelledRead; releaseRead();
    await page.unroute(base + 'api/notes?*', leavingQuery);
    await expect(page.locator('.note-card')).toHaveCount(0);

    // A server-only error remains attached to its field while other fields change.
    const rejected = route => route.request().method() === 'PATCH'
      ? route.fulfill({status:422, contentType:'application/json', body:JSON.stringify({errors:{name:'Choose another collection name.'}})})
      : route.continue();
    await page.route(base + 'api/preferences', rejected);
    await field(page).fill('Reserved collection'); await submit(page).click();
    await expect(field(page)).toHaveAttribute('aria-invalid', 'true');
    await page.getByRole('combobox', {name:'Reading density'}).selectOption('compact');
    await expect(page.locator('#preference-name-error')).toHaveText('Choose another collection name.');
    await page.unroute(base + 'api/preferences', rejected);
    // Explicitly retry the same values after the server condition has changed.
    await submit(page).click();
    await expect(page.locator('[data-save-message]')).toHaveText('Your preferences are saved.');
    await expect(field(page)).not.toHaveAttribute('aria-invalid', 'true');

    let writeArrived;
    const written = new Promise(resolve => {writeArrived = resolve;});
    const writeGate = new Promise(resolve => {releaseWrite = resolve;});
    const delayedWrite = async route => {
      if (route.request().method() !== 'PATCH') return route.continue();
      const response = await route.fetch(); writeArrived(); await writeGate; await route.fulfill({response});
    };
    await page.route(base + 'api/preferences', delayedWrite);
    await field(page).fill('A saved corner');
    const before = requests.filter(request => request.method() === 'PATCH').length;
    await submit(page).evaluate(button => {button.click(); button.form.requestSubmit();});
    await written;
    assert.equal(requests.filter(request => request.method() === 'PATCH').length, before + 1);
    await field(page).fill('An edit made while saving');
    releaseWrite(); await expect(submit(page)).toBeEnabled();
    await expect(field(page)).toHaveValue('An edit made while saving');
    await expect(page.locator('[data-save-state]')).toContainText('unsaved');
    await page.unroute(base + 'api/preferences', delayedWrite);
    await page.getByRole('button', {name:'Discard changes', exact:true}).click();
    await expect(field(page)).toHaveValue('A saved corner');
    await expect(page.locator('[data-save-state]')).toHaveText('All changes saved');

    const cancelGate = new Promise(resolve => {releaseWrite = resolve;});
    let requested;
    const pending = new Promise(resolve => {requested = resolve;});
    const cancelledWrite = async route => {
      if (route.request().method() !== 'PATCH') return route.continue();
      requested(); await cancelGate; await finishRoute(route, () => route.abort());
    };
    await page.route(base + 'api/preferences', cancelledWrite);
    await field(page).fill('Cancelled edit'); await submit(page).click(); await pending;
    await page.getByRole('button', {name:'Cancel save', exact:true}).click();
    await expect(submit(page)).toBeEnabled();
    releaseWrite(); await page.unroute(base + 'api/preferences', cancelledWrite);
    await page.goto(base + 'preferences?backend=' + backend); await ready(page);
    await expect(field(page)).toHaveValue('A saved corner');
    await expect(page.locator('[data-collection-name]')).toHaveText('A saved corner');
    await expect(page.getByRole('combobox', {name:'Reading density'})).toHaveValue('compact');
    if (screenshotDirectory) {
      await page.screenshot({path:join(screenshotDirectory, 'preferences-desktop.png'), fullPage:true});
      assert(await page.getByRole('combobox', {name:'Reading density'}).evaluate(element => element.getBoundingClientRect().height < 65));
      await page.setViewportSize({width:390, height:844});
      await page.screenshot({path:join(screenshotDirectory, 'preferences-mobile.png'), fullPage:true});
      assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
      await page.getByRole('link', {name:'Library', exact:true}).click();
      await expect(page.locator('.note-card')).toHaveCount(6);
      await page.screenshot({path:join(screenshotDirectory, 'library-mobile.png'), fullPage:true});
      await page.setViewportSize({width:1360, height:920});
      await page.screenshot({path:join(screenshotDirectory, 'library-desktop.png'), fullPage:true});
    }
    assert(requests.every(request => request.url().startsWith(base)), 'a deployed app escaped its base path');
    assert.deepEqual(errors, []);
    return {backend, passed:true, contracts:['fresh-ssr-query-reuse', 'atomic-query-patches', 'search-focus',
      'obsolete-result-isolation', 'navigation-cancels-read', 'failed-query-retry', 'empty-query', 'nested-layout-identity', 'pagination-history',
      'route-focus', 'server-field-error-retention', 'unchanged-value-retry', 'frozen-submission', 'duplicate-write-suppression',
      'revert-to-saved-baseline', 'cancel', 'cookie-refresh', 'subpath-assets-and-requests']};
  } finally {releaseRead(); releaseWrite(); await page.close();}
}

export async function checkFieldnotesPrefetch(browser, base, backend) {
  const page = await browser.newPage(), errors = [], requests = [], failures = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('request', request => requests.push(request));
  page.on('requestfailed', request => failures.push({url:request.url(),error:request.failure()?.errorText}));
  let release = () => {};
  try {
    await page.goto(base + 'library?backend=' + backend); await ready(page);
    const firstURL = base + 'api/notes/quiet-design';
    const first = page.getByRole('link', {name:'The art of making room', exact:true});
    const fetched = page.waitForResponse(firstURL);
    await first.focus();
    await (await fetched).finished();
    await first.press('Enter');
    await page.getByRole('heading', {name:'The art of making room', exact:true}).waitFor();
    assert.equal(requests.filter(request => request.url() === firstURL).length, 1, 'focused-link prefetch was fetched again on navigation');
    await page.goBack();
    await page.getByRole('heading', {name:'Ideas worth keeping.'}).waitFor();

    const secondURL = base + 'api/notes/small-systems';
    let arrived;
    const received = new Promise(resolve => {arrived = resolve;});
    const gate = new Promise(resolve => {release = resolve;});
    await page.route(secondURL, async route => {
      const response = await route.fetch(); arrived(); await gate;
      await finishRoute(route, () => route.fulfill({response}));
    });
    const second = page.getByRole('link', {name:'Small systems, lasting habits', exact:true});
    await second.hover(); await received;
    await second.focus(); await second.click();
    await expect(page).toHaveURL(url => url.pathname.endsWith('/library/small-systems'));
    await expect(page.getByText('Opening your note…', {exact:true})).toBeVisible();
    release();
    await page.getByRole('heading', {name:'Small systems, lasting habits', exact:true}).waitFor();
    assert.equal(requests.filter(request => request.url() === secondURL).length, 1, 'route transition restarted its in-flight prefetch');
    assert(!failures.some(value=>value.url===secondURL), 'removing the old list cancelled an adopted prefetch: '+JSON.stringify(failures));
    assert.deepEqual(errors, []);
    return {backend, passed:true, contracts:['keyboard-focus-prefetch', 'pointer-intent-prefetch',
      'duplicate-intent-coalescing', 'completed-request-reuse', 'pending-route-adoption', 'one-detail-request']};
  } finally {release(); await page.close();}
}
