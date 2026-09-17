import assert from 'node:assert/strict';

export async function checkAccordion(page) {
  const first = page.locator('#gallery-accordion-trigger-0');
  const second = page.locator('#gallery-accordion-trigger-1');
  const expanded = (index, value) => page.waitForFunction(({ index, value }) => {
    const trigger = document.getElementById(`gallery-accordion-trigger-${index}`);
    const panel = document.getElementById(trigger.getAttribute('aria-controls'));
    return trigger.getAttribute('aria-expanded') === String(value) && panel.hidden === !value;
  }, { index, value });
  assert.equal(await first.getAttribute('aria-expanded'), 'true');
  await second.focus();
  await page.keyboard.press('Enter');
  await expanded(1, true);
  assert.equal(await first.getAttribute('aria-expanded'), 'true', 'opening one section closed its sibling in multiple mode');
  await page.keyboard.press('Tab');
  assert.equal(await page.evaluate(() => document.activeElement.id), 'accordion-note');
  await page.locator('#accordion-note').fill('Keep this detail 中文');
  await page.evaluate(() => { window.accordionInput = document.getElementById('accordion-note'); });
  await second.click();
  await expanded(1, false);
  assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-accordion-trigger-1');
  await page.keyboard.press('Space');
  await expanded(1, true);
  assert.equal(await page.locator('#accordion-note').inputValue(), 'Keep this detail 中文');
  assert.equal(await page.evaluate(() => window.accordionInput === document.getElementById('accordion-note')), true);
  await page.evaluate(() => { const button = document.getElementById('gallery-accordion-trigger-1'); button.click(); button.click(); });
  // Drain the real input round before inspecting the same final expanded value.
  await page.keyboard.press('Tab');
  await page.locator('#accordion-note').fill('After two clicks');
  assert.equal(await second.getAttribute('aria-expanded'), 'true');
  assert.equal(await page.locator('#gallery-accordion-trigger-2').isDisabled(), true);
  assert.equal(await second.evaluate(button => button.parentElement.tagName), 'H3');
  assert.equal(await page.locator('#gallery-accordion-panel-1').getAttribute('aria-labelledby'), 'gallery-accordion-trigger-1');
}
