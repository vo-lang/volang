import assert from 'node:assert/strict';
import { checkNativeModifiedKeys } from './kit-native-key-contracts.mjs';

export const toastContracts = ['toast-bounded-visible-queue', 'toast-no-arrival-focus', 'toast-scoped-focus-entry',
  'toast-action-and-escape', 'toast-dismiss-focus-return', 'toast-queued-advancement',
  'toast-programmatic-clear-focus', 'toast-mobile-rtl'];

export async function checkToasts(page, screenshot, timing = false) {
  await page.bringToFront();
  const region = page.getByRole('region', { name: 'Your notifications', exact: true });
  const notes = region.locator('.vui-toast');
  const send = page.getByRole('button', { name: 'Send a note', exact: true });
  const addAction = page.getByRole('button', { name: 'Try an action', exact: true });
  const clear = page.getByRole('button', { name: 'Dismiss all notes', exact: true });
  await addAction.focus();
  await page.keyboard.press('Enter');
  await notes.first().waitFor();
  assert.equal(await page.evaluate(() => document.activeElement.textContent), 'Try an action', 'notification arrival moved focus');
  assert.equal(await notes.first().getAttribute('role'), 'group');
  await page.waitForFunction(() => document.querySelector('#gallery-notifications [role=status]').textContent.startsWith('An idea'));
  // Six same-turn activations retain five entries, with only three visible.
  await addAction.evaluate(button => { for (let i = 0; i < 6; i++) button.click(); });
  await page.waitForFunction(() => document.querySelector('[data-toast-status]').textContent.includes('is full'));
  assert.equal(await notes.count(), 3);
  if (screenshot) await page.screenshot({ path: screenshot });
  await page.getByRole('button', { name: 'View notifications', exact: true }).click();
  await page.waitForFunction(() => document.activeElement.classList.contains('vui-toast-dismiss'));
  const first = await notes.first().getAttribute('data-toast-key');
  await checkNativeModifiedKeys(notes.first().getByRole('button', { name: 'Dismiss notification' }), 'Escape');
  await page.keyboard.press('Escape');
  await page.waitForFunction(key => !document.querySelector(`[data-toast-key="${key}"]`), first);
  assert.equal(await notes.count(), 3, 'pending toast did not advance into available space');
  await page.waitForFunction(() => document.activeElement.classList.contains('vui-toast-dismiss'));
  await page.keyboard.press('Shift+Tab');
  assert.equal(await page.evaluate(() => document.activeElement.textContent), 'Undo');
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.querySelector('[data-toast-status]').textContent.startsWith('Undone.'));
  await page.waitForFunction(() => document.activeElement.classList.contains('vui-toast-dismiss'));
  await clear.evaluate(button => button.click());
  await page.waitForFunction(() => !document.querySelector('.vui-toast') && document.activeElement.id === 'gallery-send-note');
  await addAction.click();
  await notes.first().waitFor();
  await notes.first().getByRole('button', { name: 'Dismiss notification' }).focus();
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => !document.querySelector('.vui-toast') && document.activeElement.id === 'gallery-send-note');

  if (timing) {
    await send.focus();
    await page.keyboard.press('Enter');
    await notes.first().waitFor();
    assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-send-note');
    await notes.first().hover();
    // Real timers: hovering keeps a timed note beyond its complete five seconds.
    await page.waitForTimeout(5200);
    assert.equal(await notes.count(), 1, 'hovered toast expired');
    await page.mouse.move(0, 0);
    await page.waitForFunction(() => !document.querySelector('.vui-toast'), null, { timeout: 7000 });
    assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-send-note', 'timer expiry moved focus');
    assert.match(await page.locator('[data-toast-status]').textContent(), /A little note for you/, 'timed information had no persistent alternative');
  }
  const viewport = page.viewportSize();
  await page.setViewportSize({ width: 390, height: 844 });
  await page.locator('.studio').evaluate(element => element.dir = 'rtl');
  await addAction.evaluate(button => { button.click(); button.click(); button.click(); });
  await page.waitForFunction(() => document.querySelectorAll('.vui-toast').length === 3);
  assert.equal(await region.evaluate(element => {
    const rect = element.getBoundingClientRect();
    return rect.left >= 0 && rect.right <= innerWidth && rect.bottom <= innerHeight;
  }), true, 'notification stack escaped the narrow viewport');
  assert.equal(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), true);
  if (screenshot) await page.screenshot({ path: screenshot.replace('.png', '-mobile.png') });
  await clear.evaluate(button => button.click());
  await page.waitForFunction(() => !document.querySelector('.vui-toast'));
  await page.locator('.studio').evaluate(element => element.removeAttribute('dir'));
  await page.setViewportSize(viewport);
}

export async function checkToastBackground(page) {
  // Playwright enables Chromium focus emulation even in headed mode. Disable
  // it for this native tab-activation probe and restore it afterward.
  const session = page.context().browser().browserType().name() === 'chromium'
    ? await page.context().newCDPSession(page) : null;
  await session?.send('Emulation.setFocusEmulationEnabled', { enabled: false });
  await page.bringToFront();
  await page.locator('#gallery-send-note').focus();
  await page.keyboard.press('Enter');
  await page.locator('.vui-toast').waitFor();
  const foreground = await page.context().newPage();
  try {
    await foreground.goto('about:blank');
    await foreground.bringToFront();
    await page.waitForFunction(() => !document.hasFocus() || document.visibilityState !== 'visible', null, { polling: 100 });
    await page.waitForTimeout(5200);
    assert.equal(await page.locator('.vui-toast').count(), 1, 'inactive document consumed the notification countdown');
    await page.bringToFront();
    await page.waitForFunction(() => document.hasFocus() && document.visibilityState === 'visible', null, { polling: 100 });
    await page.waitForFunction(() => !document.querySelector('.vui-toast'), null, { timeout: 7000 });
  } finally {
    await foreground.close();
    await session?.send('Emulation.setFocusEmulationEnabled', { enabled: true });
    await session?.detach();
  }
}
