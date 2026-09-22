import assert from 'node:assert/strict';
import {contentContracts} from './studio-content.mjs';

export async function checkStudioContent(browser, origin) {
  const context = await browser.newContext();
  const requests = [], errors = [];
  context.on('request', request => requests.push(new URL(request.url()).pathname));
  await context.route(/\/(wasm|compiler|artifacts)\//, route => route.abort());
  const page = await context.newPage();
  page.on('pageerror', error => errors.push(error.message));
  try {
    await page.goto(origin + '/studio/docs/');
    await page.waitForFunction(() => document.documentElement.hasAttribute('data-content-ready'));
    await page.getByRole('heading',{name:'A small idea, brought to life.'}).waitFor();
    assert(!requests.some(path => path.endsWith('search.json')), 'Search index downloaded without intent');
    await page.locator('#studio-theme').focus(); await page.keyboard.press('Space');
    assert.equal(await page.locator('.studio').getAttribute('data-theme'),'dark');
    await page.evaluate(() => {Object.defineProperty(navigator,'clipboard',{configurable:true,value:{writeText:async text => {window.copied = text;}}});});
    await page.getByRole('button',{name:'Copy code block'}).first().click();
    await page.waitForFunction(() => typeof window.copied === 'string' && window.copied.length > 0);
    let release;
    const gate = new Promise(resolve => {release=resolve;});
    await context.route('**/studio-docs/search.json?*', async route => {await gate; await route.continue().catch(()=>{});});
    await page.locator('#docs-search').fill('lifecycle');
    const link = page.getByRole('link',{name:'Lifecycle & requests',exact:true});
    await link.waitFor();
    await link.evaluate(node => {window.titleLink=node;});
    const before = await link.boundingBox();
    release();
    await page.getByText('Searching chapter text…',{exact:true}).waitFor({state:'hidden'});
    assert.deepEqual(await link.boundingBox(), before);
    assert(await link.evaluate(node => node===window.titleLink));
    await link.click();
    await page.getByRole('heading',{name:'A place for every effect.'}).waitFor();
    await page.goBack();
    await page.getByRole('heading',{name:'A small idea, brought to life.'}).waitFor();
    await context.unroute('**/studio-docs/search.json?*');
    await context.route('**/studio-docs/search.json?*', route => route.fulfill({status:503,body:'unavailable'}));
    await page.reload();
    await page.locator('#docs-search').fill('state');
    await page.getByRole('button',{name:'Retry text search'}).waitFor();
    await page.getByRole('link',{name:'State & identity',exact:true}).waitFor();
    await context.unroute('**/studio-docs/search.json?*');
    await page.getByRole('button',{name:'Retry text search'}).click();
    await page.getByRole('button',{name:'Retry text search'}).waitFor({state:'hidden'});
    await page.locator('#docs-search').fill('InspectProp');
    await page.getByRole('link',{name:'State & identity',exact:true}).waitFor();
    assert.equal(await page.locator('.studio-doc-nav a:visible').count(),2);
    await page.setViewportSize({width:390,height:844});
    assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
    assert(!requests.some(path => /^\/(wasm|compiler|artifacts)\//.test(path)), 'Content depends on the VM');
    assert.deepEqual(errors, []);
    return [{mode:'static-content',passed:true,contracts:contentContracts}];
  } finally {await context.close();}
}
