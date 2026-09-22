import assert from 'node:assert/strict';

// Hold bytecode while confirming the runtime download starts independently.
export async function checkStudioStartup(browser, origin) {
  const context = await browser.newContext();
  let release;
  const gate = new Promise(resolve => {release = resolve;});
  let held;
  const bytecodeHeld = new Promise(resolve => {held = resolve;});
  const requests = [];
  context.on('request', request => requests.push(new URL(request.url()).pathname));
  await context.route('**/artifacts/studio.vob.gz', async route => {
    held();
    await gate;
    await route.continue().catch(() => {});
  });
  const page = await context.newPage();
  let timer;
  try {
    await page.goto(origin + '/studio/gallery/', {waitUntil:'domcontentloaded'});
    await Promise.race([bytecodeHeld, new Promise((_, reject) => {
      timer = setTimeout(() => reject(new Error('Studio bytecode request did not start')), 10000);
    })]);
    clearTimeout(timer);
    assert(requests.includes('/wasm/vo_web_bg.wasm'), 'Wasm download waited for bytecode');
    assert(requests.includes('/artifacts/studio.vob.gz'), 'Packaged Studio downloaded uncompressed bytecode');
    assert(!requests.includes('/artifacts/studio.vob'));
    release();
    await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
    assert.equal(await page.evaluate(() => window.__studioNext.error), null);
    await page.getByRole('button', {name:'Make it happen'}).click();
    await page.waitForFunction(() => document.querySelector('[data-demo-count]').textContent.startsWith('1 '));
    assert(!requests.some(path => path.startsWith('/studio-docs/')), 'Gallery eagerly loaded documentation');
    assert(!requests.includes('/wasm/vo_web.js'), 'Worker bindings must be bundled');
    await page.locator('[data-nav="docs"]').click();
    await page.waitForFunction(() => document.documentElement.hasAttribute('data-content-ready'));
    assert.equal(await page.evaluate(() => typeof window.__studioNext), 'undefined');
  } finally {
    clearTimeout(timer);
    release();
    await context.close();
  }
}
