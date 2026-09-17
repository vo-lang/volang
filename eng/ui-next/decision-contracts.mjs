import assert from 'node:assert/strict';

export const decisionContracts = ['alertdialog-safe-initial-focus', 'alertdialog-native-modal-scope',
  'alertdialog-cancel-return', 'alertdialog-confirm-burst', 'alertdialog-nested-escape'];

export async function checkDecisions(page, screenshot) {
  const outer = page.locator('#gallery-dialog');
  const reset = page.locator('#gallery-reset-dialog');
  await page.locator('[data-open-dialog]').click();
  await page.locator('#dialog-name').fill('Keep this thought 中文');
  await page.locator('[data-open-reset]').click();
  await page.waitForFunction(() => document.querySelector('#gallery-reset-dialog').matches(':modal') &&
    document.activeElement.classList.contains('vui-alertdialog-cancel'));
  assert.equal(await page.getByRole('alertdialog', { name: 'Make a fresh start?', exact: true }).count(), 1);
  assert.equal(await reset.getAttribute('aria-describedby'), 'gallery-reset-dialog-description');
  if (screenshot) await page.screenshot({ path: screenshot, animations: 'disabled' });
  await page.keyboard.press('Tab');
  assert.equal(await page.evaluate(() => document.activeElement.textContent), 'Reset the name');
  await page.keyboard.press('Tab');
  assert.equal(await page.evaluate(() => document.activeElement.textContent), 'Keep editing');
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => !document.querySelector('#gallery-reset-dialog').open && document.activeElement.matches('[data-open-reset]'));
  assert.equal(await page.locator('#dialog-name').inputValue(), 'Keep this thought 中文');
  assert.equal(await outer.evaluate(element => element.matches(':modal')), true);
  await page.locator('[data-open-reset]').click();
  await reset.waitFor({ state: 'visible' });
  await page.keyboard.press('Escape');
  await page.waitForFunction(() => !document.querySelector('#gallery-reset-dialog').open && document.activeElement.matches('[data-open-reset]'));
  assert.equal(await outer.evaluate(element => element.matches(':modal')), true);
  await page.locator('[data-open-reset]').click();
  await reset.waitFor({ state: 'visible' });
  await reset.locator('.vui-alertdialog-confirm').evaluate(button => { button.click(); button.click(); });
  await page.waitForFunction(() => !document.querySelector('#gallery-reset-dialog').open && document.activeElement.matches('[data-open-reset]'));
  assert.equal(await page.locator('#dialog-name').inputValue(), 'A new beginning');
  await page.keyboard.press('Escape');
  await page.waitForFunction(() => !document.querySelector('#gallery-dialog').open && document.activeElement.matches('[data-open-dialog]'));
}

export async function checkBreadcrumb(page) {
  const trail = page.getByRole('navigation', { name: 'Current chapter', exact: true });
  assert.equal(await trail.locator('ol > li').count(), 2);
  assert.equal(await trail.locator('[aria-current=page]').textContent(), 'First steps');
  await page.getByRole('link', { name: 'State & identity', exact: true }).click();
  await page.getByRole('heading', { name: 'State that stays close.' }).waitFor();
  assert.equal(await trail.locator('[aria-current=page]').textContent(), 'State & identity');
  await page.locator('#docs-search').fill('state');
  await page.evaluate(() => { window.breadcrumbSearch = document.querySelector('#docs-search'); });
  await trail.getByRole('link', { name: 'Guide', exact: true }).click();
  await page.getByRole('heading', { name: 'A small idea, brought to life.' }).waitFor();
  assert.equal(new URL(page.url()).pathname, '/studio/docs');
  assert.equal(await page.locator('#docs-search').inputValue(), 'state');
  assert.equal(await page.evaluate(() => document.querySelector('#docs-search') === window.breadcrumbSearch), true);
  // Observe the router before suppressing native new-tab/default actions. This
  // avoids depending on a headless browser's background-tab configuration.
  assert.deepEqual(await trail.getByRole('link', { name: 'Guide', exact: true }).evaluate(link => {
    const intercepted = [];
    const observe = event => { intercepted.push(event.defaultPrevented); event.preventDefault(); };
    document.addEventListener('click', observe);
    try {
      for (const key of ['ctrlKey', 'metaKey', 'shiftKey']) link.dispatchEvent(new MouseEvent('click', {
        bubbles: true, cancelable: true, [key]: true,
      }));
    } finally { document.removeEventListener('click', observe); }
    return intercepted;
  }), [false, false, false]);
  await page.locator('#docs-search').fill('');
}
