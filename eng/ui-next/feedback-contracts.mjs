import assert from 'node:assert/strict';

export const feedbackContracts = ['native-slider-arrows-and-limits', 'native-slider-pointer', 'native-slider-rtl',
  'native-progress-determinate-and-indeterminate', 'feedback-loading-and-dismissal', 'feedback-reduced-motion'];

export async function checkFeedback(page, screenshot) {
  const slider = page.getByRole('slider', { name: 'Move at your own pace', exact: true });
  const progress = page.getByRole('progressbar', { name: 'Every little step counts', exact: true });
  const card = slider.locator('xpath=ancestor::article');
  const settled = value => page.waitForFunction(value => document.querySelector('#gallery-pace').value === String(value)
    && document.querySelector('#gallery-progress').value === value, value);
  await slider.focus();
  await page.keyboard.press('Home');
  await settled(0);
  await page.keyboard.press('ArrowRight');
  await settled(5);
  await page.keyboard.press('End');
  await settled(100);
  await page.keyboard.press('ArrowUp');
  assert.equal(await slider.inputValue(), '100');
  await page.keyboard.press('ArrowLeft');
  await settled(95);
  assert.equal(await slider.getAttribute('aria-valuetext'), '95% complete');
  assert.equal(await progress.evaluate(el => el.position), 0.95);
  // Engines differ in native RTL arrow policy. Compare to a plain range input
  // in the same browser rather than replacing the platform's keyboard behavior.
  await page.evaluate(() => {
    const native = document.createElement('input');
    native.type = 'range'; native.min = '0'; native.max = '100'; native.step = '5'; native.value = '95'; native.dir = 'rtl';
    native.dataset.rangeReference = '';
    document.body.append(native); native.focus();
  });
  await page.keyboard.press('ArrowRight');
  const expectedRTL = Number(await page.locator('[data-range-reference]').inputValue());
  assert([90, 100].includes(expectedRTL), 'native RTL range did not move by one step');
  await page.locator('[data-range-reference]').evaluate(el => el.remove());
  await card.evaluate(el => el.dir = 'rtl');
  await slider.focus();
  await page.keyboard.press('ArrowRight');
  await settled(expectedRTL);
  await card.evaluate(el => el.removeAttribute('dir'));
  const rect = await slider.boundingBox();
  await page.mouse.click(rect.x + rect.width * 0.3, rect.y + rect.height / 2);
  await page.waitForFunction(() => {
    const value = Number(document.querySelector('#gallery-pace').value);
    return value >= 20 && value <= 40 && document.querySelector('#gallery-progress').value === value;
  });
  await page.emulateMedia({ reducedMotion: 'reduce' });
  await page.locator('#gallery-prepare').click();
  await page.waitForFunction(() => document.querySelector('#gallery-progress').position === -1);
  assert.equal(await slider.isDisabled(), true);
  assert.equal(await page.locator('.vui-spinner [aria-hidden=true]').count(), 1);
  assert.equal(await page.locator('.vui-spinner-art').evaluate(el => getComputedStyle(el).animationName), 'none');
  await card.getByText('Ready when you are', { exact: true }).waitFor();
  assert.equal(await page.locator('.vui-spinner').count(), 0);
  assert.equal(await slider.isDisabled(), false);
  assert.equal(await card.locator('.vui-alert').getAttribute('role'), 'status');
  assert.equal(await progress.evaluate(el => el.position >= 0), true);
  if (screenshot) await card.screenshot({ path: screenshot });
  await card.getByRole('button', { name: 'Dismiss preview message' }).click();
  await page.waitForFunction(() => !document.querySelector('.vui-alert') && document.activeElement.id === 'gallery-prepare');
  await page.emulateMedia({ reducedMotion: 'no-preference' });
}
