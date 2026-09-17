import assert from 'node:assert/strict';

export async function checkNestedRoutes(page) {
  await page.evaluate(() => {
    window.routeShell = document.querySelector('.studio-sidebar');
    window.routeSearch = document.querySelector('#docs-search');
    window.routeArticle = document.querySelector('.studio-prose');
  });
  const search = page.locator('#docs-search');
  await search.fill('identity');
  await page.getByRole('link', { name: 'State & identity', exact: true }).click();
  await page.getByRole('heading', { name: 'State that stays close.' }).waitFor();
  assert.equal(new URL(page.url()).pathname, '/studio/docs/state');
  assert.equal(await search.inputValue(), 'identity', 'nested navigation reset its layout state');
  assert.equal(await page.evaluate(() => window.routeSearch === document.querySelector('#docs-search')
    && window.routeArticle === document.querySelector('.studio-prose')), true, 'nested navigation replaced its layout DOM');
  await search.fill('');
  await page.evaluate(() => { window.routeDocument = document.querySelector('.studio-prose [data-document]'); });
  await page.getByRole('link', { name: 'Lifecycle & requests', exact: true }).click();
  await page.getByRole('heading', { name: 'A place for every effect.' }).waitFor();
  assert.equal(new URL(page.url()).pathname, '/studio/docs/lifecycle');
  assert.equal(await page.evaluate(() => window.routeDocument !== null && window.routeDocument === document.querySelector('.studio-prose [data-document]')), true,
    'dynamic parameter update remounted the leaf route');
  await page.goBack();
  await page.getByRole('heading', { name: 'State that stays close.' }).waitFor();
  await search.fill('retained until leaving docs');
  await page.getByRole('link', { name: 'Gallery', exact: true }).click();
  await page.getByRole('heading', { name: 'Explore the essentials' }).waitFor();
  await page.getByRole('link', { name: 'Documentation', exact: true }).click();
  await page.getByRole('heading', { name: 'A small idea, brought to life.' }).waitFor();
  assert.equal(await search.inputValue(), '', 'disposed documentation layout retained stale state');
  assert.equal(await page.evaluate(() => window.routeSearch !== document.querySelector('#docs-search')
    && window.routeShell === document.querySelector('.studio-sidebar')), true,
  'sibling navigation failed to preserve the shared Studio layout');
}

export async function checkStudioScroll(page) {
  await page.getByRole('link', { name: 'Gallery', exact: true }).click();
  await page.getByRole('heading', { name: 'Explore the essentials' }).waitFor();
  await page.waitForFunction(() => document.activeElement.id === 'studio-content' && scrollY === 0);
  await page.evaluate(() => {
    document.querySelector('#gallery-name').focus({ preventScroll: true });
    scrollTo(0, 400);
    window.routeScroll = scrollY;
    if (window.routeScroll !== 400) throw new Error('Gallery is too short for the history restoration probe.');
    document.querySelector('[data-nav="docs"]').click();
  });
  await page.getByRole('heading', { name: 'A small idea, brought to life.' }).waitFor();
  await page.waitForFunction(() => document.activeElement.id === 'studio-content' && scrollY === 0);
  await page.goBack();
  await page.getByRole('heading', { name: 'Explore the essentials' }).waitFor();
  await page.waitForFunction(() => document.activeElement.id === 'gallery-name' && Math.abs(scrollY - window.routeScroll) <= 1);
  await page.getByRole('link', { name: 'Documentation', exact: true }).click();
  await page.getByRole('heading', { name: 'A small idea, brought to life.' }).waitFor();
}
