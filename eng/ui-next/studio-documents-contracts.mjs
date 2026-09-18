import assert from 'node:assert/strict';
import { resolve } from 'node:path';
import { root } from './repository-paths.mjs';

export const documentContracts = ['maintained-chapter-catalog', 'chapter-outline', 'heading-permalinks', 'copy-code', 'chapter-search', 'chapter-lazy-load',
  'generated-ui-guide', 'content-index-lazy-load', 'body-and-code-search', 'search-error-title-fallback-and-retry', 'content-index-cache-reuse',
  'chapter-cache-reuse', 'chapter-error-and-retry', 'invalid-document-local-error', 'chapter-route-cancellation',
  'delayed-fragment-restoration', 'server-document-content', 'server-document-metadata',
  'document-hydration-retains-dom-and-input', 'server-cache-handoff-no-fetch', 'mobile-document-overflow'];

async function ready(page) {
  await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
  assert.equal(await page.evaluate(() => window.__studioNext.error), null);
}
async function chapter(page, id) {
  await page.waitForFunction(id => {
    const element = document.querySelector(`[data-document="${id}"]`);
    return element?.getAttribute('aria-busy') === 'false' && element.querySelector('h2');
  }, id, { timeout: 60000 });
}

export async function checkStudioDocuments(browser, url, outputDirectory = resolve(root, 'target/ui-next')) {
  const reports = [];
  for (const backend of ['vm']) {
    const page = await browser.newPage({ viewport: { width: 1400, height: 1000 } });
    const errors = [], requests = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('request', request => requests.push(request.url()));
    try {
      await page.goto(`${url}/studio/gallery?backend=${backend}`);
      await ready(page);
      assert(!requests.some(url => url.includes('/studio-docs/')));
      await page.getByRole('link', { name: 'Documentation', exact: true }).click();
      const nav = page.getByRole('navigation', { name: 'Documentation chapters' });
      await chapter(page, 'first-steps');
      await page.locator('.studio-doc-contents summary').click();
      assert(await page.getByRole('navigation', {name:'On this page',exact:true}).getByRole('link').count() > 0);
      assert(await page.locator('.doc-heading-link').count() > 0);
      await page.evaluate(()=>Object.defineProperty(navigator,'clipboard',{configurable:true,value:{writeText:async value=>{window.copiedStudioCode=value;}}}));
      const firstCode=await page.locator('.doc-code-block pre').first().textContent();
      await page.getByRole('button',{name:'Copy code block',exact:true}).first().click();
      await page.waitForFunction(value=>window.copiedStudioCode===value,firstCode);

      assert(!requests.some(url => url.includes('/studio-docs/search.json')), 'opening a chapter eagerly loaded its search index');
      let searchAttempts = 0;
      await page.route('**/studio-docs/search.json?*', async route => {
        searchAttempts++;
        if (searchAttempts === 1) await route.fulfill({status:503,body:'Index unavailable'});
        else await route.continue();
      });
      await page.getByLabel('Find a chapter', { exact: true }).fill('InspectProp');
      await page.getByRole('button', {name:'Retry text search',exact:true}).waitFor();
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      await page.getByLabel('Find a chapter', { exact: true }).fill('First steps');
      await nav.getByRole('link', {name:'First steps',exact:true}).waitFor();
      await page.getByLabel('Find a chapter', { exact: true }).fill('InspectProp');
      await page.getByRole('button', {name:'Retry text search',exact:true}).click();
      await page.waitForFunction(() => document.querySelectorAll('.studio-doc-nav a').length === 2);
      assert.equal(searchAttempts,2);
      await nav.getByRole('link', {name:'State & identity',exact:true}).click();
      await chapter(page, 'state');
      assert.equal(await page.getByLabel('Find a chapter', { exact: true }).inputValue(), 'InspectProp');
      await page.getByLabel('Find a chapter', { exact: true }).fill('hello');
      await nav.getByRole('link', { name: 'Hello world', exact: true }).click();
      await chapter(page, 'hello-world');
      assert.equal(await page.title(), 'Hello world · Volang Studio');
      assert.equal(await page.getByLabel('Find a chapter', { exact: true }).inputValue(), 'hello');
      await page.getByLabel('Find a chapter', { exact: true }).fill('');
      await page.waitForFunction(() => document.querySelectorAll('.studio-doc-nav a').length === 24);
      assert.equal(await nav.getByRole('link').count(), 24);
      assert.equal(requests.filter(url => url.includes('/studio-docs/page-first-steps.json')).length, 1);
      assert.equal(requests.filter(url => url.includes('/studio-docs/page-hello-world.json')).length, 1);
      assert.equal(searchAttempts,2, 'query edits or chapter navigation refetched the immutable index');
      let moduleAttempts = 0;
      await page.route('**/studio-docs/page-modules.json?*', async route => {
        moduleAttempts++;
        if (moduleAttempts === 1) await route.fulfill({ status: 503, body: 'Chapter unavailable' });
        else await route.continue();
      });
      await nav.getByRole('link', { name: 'Modules and dependencies', exact: true }).click();
      await page.getByRole('button', { name: 'Try again', exact: true }).click();
      await chapter(page, 'modules');
      assert.equal(moduleAttempts, 2);
      await nav.getByRole('link', { name: 'Hello world', exact: true }).click();
      await chapter(page, 'hello-world');
      assert.equal(requests.filter(url => url.includes('/studio-docs/page-hello-world.json')).length, 1, 'cached chapter refetched');
      let invalid = true;
      await page.route('**/studio-docs/page-introduction.json?*', async route => {
        if (invalid) await route.fulfill({ contentType: 'application/json', body: '{"version":2,"nodes":[]}' });
        else await route.continue();
      });
      await nav.getByRole('link', { name: 'Introduction', exact: true }).click();
      await page.locator('.studio-doc-error [role=alert]').waitFor();
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      invalid = false;
      await page.getByRole('button', { name: 'Try again', exact: true }).click();
      await chapter(page, 'introduction');
      let release, intercepted;
      const gate = new Promise(resolve => { release = resolve; });
      const pending = new Promise(resolve => { intercepted = resolve; });
      await page.route('**/studio-docs/page-language-tour.json?*', async route => {
        intercepted(); await gate; await route.continue().catch(() => {});
      });
      await nav.getByRole('link', { name: 'Language tour', exact: true }).click();
      await pending;
      await page.getByRole('link', { name: 'Gallery', exact: true }).click();
      await page.getByRole('heading', { name: 'Explore the essentials' }).waitFor();
      release();
      assert.equal(await page.locator('[data-document]').count(), 0);
      // A fresh root starts before its requested heading exists. The owned
      // viewport waits for document content without polling or replaying history.
      await page.route('**/studio-docs/page-hello-world.json?*', async route => {
        await new Promise(resolve => setTimeout(resolve, 200)); await route.continue();
      });
      await page.goto(`${url}/studio/docs/hello-world?backend=${backend}#doc-add-a-test`);
      await ready(page);
      await chapter(page, 'hello-world');
      await page.waitForFunction(() => {
        const top = document.getElementById('doc-add-a-test')?.getBoundingClientRect().top;
        return top >= -1 && top <= 25 && scrollY > 0;
      });
      // Existing semantic document links are routed without downloading a new app.
      await page.getByLabel('Find a chapter', { exact: true }).fill('specification');
      await page.getByRole('navigation', { name: 'Documentation chapters' }).getByRole('link', { name: 'Language specification', exact: true }).click();
      await chapter(page, 'language-specification');
      const artifactRequests = requests.filter(url => /\/artifacts\/studio\.vob/.test(url)).length;
      await page.locator('.studio-prose a[href="/studio/docs/channel-specification"]').first().click();
      await chapter(page, 'channel-specification');
      assert.equal(requests.filter(url => /\/artifacts\/studio\.vob/.test(url)).length, artifactRequests);
      await page.goBack();
      await chapter(page, 'language-specification');
      assert(!requests.some(url => url.includes('/compiler/') || url.endsWith('/playground-ui.json')));
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      await page.getByLabel('Find a chapter', { exact: true }).fill('');
      await page.evaluate(() => scrollTo(0, 0));
      await page.screenshot({ path: resolve(outputDirectory, `studio-document-${backend}.png`) });
      await page.setViewportSize({ width: 390, height: 844 });
      assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
      await page.screenshot({ path: resolve(outputDirectory, `studio-document-mobile-${backend}.png`) });
      assert.deepEqual(errors, []);
      reports.push({ backend, mode: 'client-documents', passed: true, contracts: documentContracts });
    } finally { await page.close(); }

    const ssr = await browser.newPage({ viewport: { width: 1400, height: 1000 } });
    const ssrRequests = [], ssrErrors = [];
    ssr.on('request', request => ssrRequests.push(request.url()));
    ssr.on('pageerror', error => ssrErrors.push(error.message));
    let release;
    const gate = new Promise(resolve => { release = resolve; });
    await ssr.route('**/artifacts/studio.*', async route => { await gate; await route.continue(); });
    try {
      await ssr.goto(`${url}/studio/docs/language-specification?backend=${backend}&ssr`, { waitUntil: 'commit', timeout: 60000 });
      await ssr.getByRole('heading', { name: 'Language specification', exact: true }).waitFor();
      await ssr.getByRole('heading', { name: '1. Design Philosophy', exact: true }).waitFor();
      assert.equal(await ssr.title(), 'Language specification · Volang Studio');
      await ssr.evaluate(() => {
        window.beforeDocument = document.querySelector('[data-document]');
        window.beforeTable = document.querySelector('.studio-prose table');
        window.beforeSearch = document.querySelector('#docs-search');
      });
      await ssr.getByLabel('Find a chapter', { exact: true }).fill('channel');
      release();
      await ready(ssr);
      assert.equal(await ssr.getByLabel('Find a chapter', { exact: true }).inputValue(), 'channel');
      assert.equal(await ssr.evaluate(() => window.beforeDocument === document.querySelector('[data-document]') &&
        window.beforeTable !== null && window.beforeTable === document.querySelector('.studio-prose table') &&
        window.beforeSearch === document.querySelector('#docs-search')), true);
      assert(!ssrRequests.some(url => url.includes('/studio-docs/page-')), 'server chapter cache was not adopted');
      await ssr.getByRole('navigation', { name: 'Documentation chapters' }).getByRole('link', { name: 'Channel and island specification', exact: true }).click();
      await chapter(ssr, 'channel-specification');
      await ssr.goBack();
      await chapter(ssr, 'language-specification');
      assert(!ssrRequests.some(url => url.includes('/studio-docs/page-language-specification.json')));
      assert.deepEqual(ssrErrors, []);
      reports.push({ backend, mode: 'hydrate-documents', passed: true, contracts: documentContracts });
    } finally { release(); await ssr.close(); }
  }
  const staticPage = await browser.newPage({ javaScriptEnabled: false });
  try {
    await staticPage.goto(`${url}/studio/docs/hello-world?ssr`, { timeout: 60000 });
    await staticPage.getByRole('heading', { name: 'Run one file', exact: true }).waitFor();
    assert.equal(await staticPage.title(), 'Hello world · Volang Studio');
    assert.equal(await staticPage.locator('.studio-prose pre').count() > 5, true);
  } finally { await staticPage.close(); }
  return reports;
}
