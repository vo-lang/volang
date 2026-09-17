import assert from 'node:assert/strict';
import {checkPointerApplication} from './pointer-boundary.mjs';
import {checkNativeTextApplication} from './text-boundary.mjs';
import {checkIndeterminateApplication} from './indeterminate-boundary.mjs';

export async function checkNativeElements(page) {
  await checkPointerApplication(page);
  await checkNativeTextApplication(page);
  await checkIndeterminateApplication(page);
  const namespaces = await page.evaluate(() => [
    document.querySelector('[data-native-svg]').namespaceURI,
    document.querySelector('linearGradient').namespaceURI,
    document.querySelector('[data-circle]').namespaceURI,
    document.querySelector('[data-svg-html]').namespaceURI,
  ]);
  assert.deepEqual(namespaces, ['http://www.w3.org/2000/svg', 'http://www.w3.org/2000/svg',
    'http://www.w3.org/2000/svg', 'http://www.w3.org/1999/xhtml']);
  await page.evaluate(() => { window.retainedCircle = document.querySelector('[data-circle]'); });
  await page.locator('[data-grow]').click();
  await page.waitForFunction(() => document.querySelector('[data-circle]').getAttribute('r') === '28');
  assert.equal(await page.evaluate(() => window.retainedCircle === document.querySelector('[data-circle]')), true);
  assert.equal(await page.locator('[data-native-svg]').getAttribute('viewBox'), '0 0 330 80');
  await page.locator('#notes').focus();
  await page.evaluate(() => {
    const notes = document.querySelector('#notes');
    notes.value = '中文笔记\n<one> & two';
    notes.dispatchEvent(new InputEvent('input', { bubbles: true }));
    notes.closest('form').requestSubmit();
  });
  await page.waitForFunction(() => document.querySelector('[data-notes-saved]').textContent === '中文笔记\n<one> & two');
  assert.equal(await page.locator('#notes').inputValue(), '中文笔记\n<one> & two');
  assert.equal(await page.evaluate(() => document.activeElement.id), 'notes');

  await page.locator('[data-phase-click]').click();
  await page.waitForFunction(() => document.querySelector('[data-event-log]').textContent === 'capture target bubble');
  await page.locator('[data-phase-toggle]').click();
  await page.waitForFunction(() => document.querySelector('[data-phase-toggle]').dataset.stopped === 'true');
  await page.locator('[data-phase-click]').click();
  await page.waitForFunction(() => document.querySelector('[data-event-log]').textContent === 'capture target ');
  await page.locator('[data-shortcut]').focus();
  await page.keyboard.press('Control+Shift+K');
  await page.waitForFunction(() => document.querySelector('[data-shortcut-log]').textContent === 'ctrl=true shift=true composing=false');
  const before = page.url();
  await page.locator('[data-prevented-link]').click();
  await page.waitForFunction(() => document.querySelector('[data-event-log]').textContent === 'link handled');
  assert.equal(page.url(), before, 'default navigation ran before the guest could process the event');
  for (let cycle = 0; cycle < 3; cycle++) {
    await page.locator('[data-focus-open]').click();
    await page.waitForFunction(() => document.activeElement.id === 'quick-note');
    if (cycle === 0) await page.locator('#quick-note').fill('A retained draft');
    assert.equal(await page.locator('#quick-note').inputValue(), 'A retained draft');
    if (cycle % 2 === 0) await page.keyboard.press('Escape');
    else await page.locator('[data-focus-close]').click();
    await page.waitForFunction(() => !document.querySelector('#quick-note') && document.activeElement.hasAttribute('data-focus-open'));
  }
}
