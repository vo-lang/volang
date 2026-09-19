import assert from 'node:assert/strict';

// Hold the bindings response, rather than depending on a machine-speed budget:
// the Wasm request must already be in flight while those bindings are blocked.
export async function checkStudioStartup(browser, origin) {
  const context = await browser.newContext();
  let release;
  const gate = new Promise(resolve => {release = resolve;});
  let held;
  const bindingsHeld = new Promise(resolve => {held = resolve;});
  const requests = [];
  context.on('request', request => requests.push(new URL(request.url()).pathname));
  await context.route('**/wasm/vo_web.js', async route => {
    held();
    await gate;
    await route.continue().catch(() => {});
  });
  const page = await context.newPage();
  let timer;
  try {
    await page.goto(origin + '/studio/gallery/', {waitUntil:'domcontentloaded'});
    await Promise.race([bindingsHeld, new Promise((_, reject) => {
      timer = setTimeout(() => reject(new Error('Studio bindings request did not start')), 10000);
    })]);
    clearTimeout(timer);
    assert(requests.includes('/wasm/vo_web_bg.wasm'), 'Wasm download waited for the bindings');
    assert(requests.includes('/artifacts/studio.vob.gz'), 'Packaged Studio downloaded uncompressed bytecode');
    assert(!requests.includes('/artifacts/studio.vob'));
    release();
    await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
    assert.equal(await page.evaluate(() => window.__studioNext.error), null);
    await page.getByRole('button', {name:'Make it happen'}).click();
    await page.waitForFunction(() => document.querySelector('[data-demo-count]').textContent.startsWith('1 '));
    assert(!requests.some(path => path.startsWith('/studio-docs/')), 'Gallery eagerly loaded documentation');
    const chapter = '/studio-docs/page-first-steps.json';
    const fetched = context.waitForEvent('requestfinished', {
      predicate: request => new URL(request.url()).pathname === chapter,
    });
    await page.locator('[data-nav="docs"]').focus();
    await fetched;
    await page.locator('[data-nav="docs"]').click();
    await page.locator('[data-document="first-steps"][aria-busy="false"]').waitFor();
    await page.locator('[data-nav="gallery"]').click();
    await page.locator('[data-demo-count]').waitFor();
    await page.locator('[data-nav="docs"]').click();
    await page.locator('[data-document="first-steps"][aria-busy="false"]').waitFor();
    assert.equal(requests.filter(path => path === chapter).length, 1, 'Leaving Docs discarded its cache');
  } finally {
    clearTimeout(timer);
    release();
    await context.close();
  }
}
