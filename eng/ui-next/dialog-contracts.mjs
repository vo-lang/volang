import assert from 'node:assert/strict';

export async function checkDialog(page) {
  for (let cycle = 0; cycle < 3; cycle++) {
    await page.locator('[data-open-dialog]').click();
    await page.waitForFunction(() => document.querySelector('#gallery-dialog').matches(':modal') && document.activeElement.id === 'dialog-name');
    assert.equal(await page.getByRole('dialog', { name: 'Make room for an idea', exact: true }).getAttribute('aria-modal'), 'true');
    if (cycle === 0) await page.locator('#dialog-name').fill('An idea · 中文');
    else assert.equal(await page.locator('#dialog-name').inputValue(), 'An idea · 中文');
    for (const key of ['Tab', 'Tab', 'Tab', 'Tab', 'Shift+Tab']) {
      await page.keyboard.press(key);
      assert.equal(await page.evaluate(() => document.querySelector('#gallery-dialog').contains(document.activeElement)), true, 'focus escaped the modal');
    }
    await page.locator('[data-open-inner]').click();
    await page.waitForFunction(() => document.querySelector('#gallery-inner-dialog').matches(':modal'));
    await page.keyboard.press('Escape');
    await page.waitForFunction(() => !document.querySelector('#gallery-inner-dialog').open && document.activeElement.matches('[data-open-inner]'));
    assert.equal(await page.evaluate(() => document.querySelector('#gallery-dialog').matches(':modal') && document.activeElement.matches('[data-open-inner]')), true);
    assert.equal(await page.evaluate(() => document.documentElement.style.overflow), 'hidden');
    await page.keyboard.press('Escape');
    await page.waitForFunction(() => !document.querySelector('#gallery-dialog').open && document.activeElement.matches('[data-open-dialog]'));
    assert.equal(await page.evaluate(() => document.activeElement.matches('[data-open-dialog]')), true, 'closing did not return focus to the trigger');
    assert.equal(await page.evaluate(() => document.documentElement.style.overflow), '');
  }
}
