import assert from 'node:assert/strict';

export const tooltipContracts = ['tooltip-hover-keeps-focus', 'tooltip-keyboard-description',
  'tooltip-escape-suppression', 'tooltip-hoverable-gap', 'tooltip-touch-hover-ignored',
  'tooltip-retains-trigger-action', 'tooltip-inside-auto-popover'];

export async function checkTooltip(page, screenshot) {
  const trigger = page.getByRole('button', { name: 'Make it happen', exact: true });
  const tip = page.locator('#gallery-action-tip');
  const visible = () => tip.waitFor({ state: 'visible' });
  const hidden = () => tip.waitFor({ state: 'hidden' });
  await page.locator('#gallery-name').focus();
  await trigger.hover();
  await visible();
  assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-name');
  assert.equal(await tip.getAttribute('role'), 'tooltip');
  assert.equal(await tip.getAttribute('popover'), 'hint');
  assert.equal(await trigger.getAttribute('aria-describedby'), 'gallery-action-help gallery-action-tip');
  if (screenshot) await trigger.locator('xpath=ancestor::article').screenshot({ path: screenshot });
  await page.keyboard.press('Escape');
  await hidden();
  // Verify dismissal persists beyond the configured hover-open delay.
  await page.waitForTimeout(400);
  assert.equal(await tip.isVisible(), false);
  assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-name');
  // Record this browser's native Tab order with the hint dismissed. Some
  // platform defaults skip buttons; opening a tooltip must preserve that order.
  await trigger.focus();
  await page.keyboard.press('Tab');
  await page.evaluate(() => { window.tooltipNextFocus = document.activeElement; });
  await page.mouse.move(0, 0);
  await trigger.focus();
  await visible();
  await page.keyboard.press('Tab');
  assert.equal(await page.evaluate(() => document.activeElement === window.tooltipNextFocus), true);
  await hidden();
  await page.locator('#gallery-name').focus();
  await trigger.hover();
  await visible();
  const a = await trigger.boundingBox(), b = await tip.boundingBox();
  const side = await tip.getAttribute('data-ui-popover-side');
  const x = (Math.max(a.x, b.x) + Math.min(a.x + a.width, b.x + b.width)) / 2;
  const y = side === 'top' ? (b.y + b.height + a.y) / 2 : (a.y + a.height + b.y) / 2;
  await page.mouse.move(x, y);
  await page.waitForTimeout(220);
  assert.equal(await tip.isVisible(), true, 'pointer-transfer gap was not hoverable');
  await page.mouse.move(b.x + b.width / 2, b.y + b.height / 2);
  await page.waitForTimeout(220);
  assert.equal(await tip.isVisible(), true, 'tooltip disappeared while hovered');
  await page.mouse.move(0, 0);
  await hidden();
  await trigger.locator('..').evaluate(element => element.dispatchEvent(new PointerEvent('pointerenter', { pointerType: 'touch' })));
  await page.waitForTimeout(400);
  assert.equal(await tip.isVisible(), false, 'touch hover opened a tooltip');
  await trigger.click();
  await page.waitForFunction(() => document.querySelector('[data-demo-count]').textContent.startsWith('1 '));
  await hidden();
  await page.getByRole('button', { name: 'Start again', exact: true }).click();
  await page.waitForFunction(() => document.querySelector('[data-demo-count]').textContent.startsWith('0 '));

  await page.getByRole('button', { name: 'Quick settings', exact: true }).click();
  const popup = page.locator('#idea-popover'), nested = page.locator('#settings-reset-tip');
  await popup.waitFor({ state: 'visible' });
  await popup.getByRole('button', { name: 'Reset name', exact: true }).hover();
  await nested.waitFor({ state: 'visible' });
  assert.equal(await popup.evaluate(element => element.matches(':popover-open')), true);
  assert.equal(await page.evaluate(() => document.activeElement.id), 'popover-name');
  await page.keyboard.press('Escape');
  await nested.waitFor({ state: 'hidden' });
  assert.equal(await popup.evaluate(element => element.matches(':popover-open')), true, 'closing a hint closed its auto layer');
  await page.keyboard.press('Escape');
  await popup.waitFor({ state: 'hidden' });
  assert.equal(await page.evaluate(() => document.activeElement.textContent), 'Quick settings');
  await page.mouse.move(0, 0);
}
