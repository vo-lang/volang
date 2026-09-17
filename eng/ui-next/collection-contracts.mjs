import assert from 'node:assert/strict';
import {checkNativeModifiedKeys} from './kit-native-key-contracts.mjs';

export const collectionContracts = ['virtual-100000-items', 'bounded-live-options', 'retained-keyed-option',
  'active-descendant-pinned', 'listbox-batched-keyboard', 'listbox-single-and-multiple-selection',
  'collection-filter-repair', 'disabled-listbox', 'virtual-list-unmount'];

export async function checkCollections(page) {
  const list = page.locator('#virtual-library');
  const active = index => page.waitForFunction(index => document.querySelector('#virtual-library')?.getAttribute('aria-activedescendant') === `virtual-library-option-${index}`, index);
  const selected = text => page.waitForFunction(text => document.querySelector('[data-library-selected]')?.textContent === text, text);
  const bounded = async () => assert(await list.getByRole('option').count() <= 12, 'window retained too many live options');
  await list.focus();
  await active(0);
  // Three native events delivered before the next guest commit must skip the
  // disabled row and advance from the writer's current active key each time.
  await list.evaluate(element => { for (let i = 0; i < 3; i++) element.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowDown', bubbles: true, cancelable: true })); });
  await active(4);
  await selected('');
  await list.evaluate(element => {
    for (const key of [' ', 'ArrowDown', ' ']) element.dispatchEvent(new KeyboardEvent('keydown', { key, bubbles: true, cancelable: true }));
  });
  await selected('4,5');
  await page.waitForFunction(() => document.querySelector('#virtual-library-option-5')?.getAttribute('aria-selected') === 'true');
  await page.evaluate(() => { window.keptVirtualOption = document.querySelector('#virtual-library-option-5'); });
  await list.evaluate(element => element.scrollTop = 8000);
  await page.waitForFunction(() => document.querySelector('#virtual-library-option-200'));
  assert.equal(await page.evaluate(() => window.keptVirtualOption === document.querySelector('#virtual-library-option-5')), true);
  assert.equal(await page.evaluate(() => document.activeElement.id), 'virtual-library');
  assert.equal(await page.locator('#virtual-library-option-200').getAttribute('aria-posinset'), '201');
  await bounded();
  await page.keyboard.press('End');
  await active(999);
  await page.waitForFunction(() => {
    const row = document.querySelector('#virtual-library-option-999').getBoundingClientRect();
    const box = document.querySelector('#virtual-library').getBoundingClientRect();
    return row.top >= box.top - 1 && row.bottom <= box.bottom + 1;
  });
  await page.keyboard.press('Enter');
  await selected('4,5,999');
  await page.locator('[data-full-library]').click();
  await page.waitForFunction(() => document.querySelector('#virtual-library [role=option]')?.getAttribute('aria-setsize') === '100000', null, { timeout: 60_000 });
  await list.focus();
  await active(4);
  await page.keyboard.press('End');
  await active(99999);
  await page.waitForFunction(() => document.querySelector('#virtual-library').scrollTop > 3_999_000);
  await page.keyboard.press('Space');
  await selected('4,5,999,99999');
  await bounded();
  await page.evaluate(() => { window.keptLastOption = document.querySelector('#virtual-library-option-99999'); });
  await page.locator('[data-filter-library]').click();
  await page.waitForFunction(() => document.querySelector('#virtual-library [role=option]')?.getAttribute('aria-setsize') === '4');
  assert.equal(await list.getByRole('option').count(), 4);
  assert.equal(await page.evaluate(() => window.keptLastOption === document.querySelector('#virtual-library-option-3')), true);
  await active(3);
  await page.locator('[data-disable-library]').click();
  await page.waitForFunction(() => document.querySelector('#virtual-library')?.getAttribute('aria-disabled') === 'true');
  assert.equal(await list.getAttribute('tabindex'), '-1');
  assert.equal(await list.getAttribute('aria-activedescendant'), null);
  await list.evaluate(element => element.dispatchEvent(new KeyboardEvent('keydown', { key: ' ', bubbles: true, cancelable: true })));
  await selected('4,5,999,99999');
  await page.locator('[data-toggle-library]').click();
  await list.waitFor({ state: 'detached' });
  await page.locator('[data-disable-library]').click();
  await page.locator('[data-toggle-library]').click();
  await list.waitFor();
  await selected('4,5,999,99999');
  assert.equal(await list.getByRole('option', { selected: true }).count(), 4);
  await bounded();
}

export async function checkGalleryCollection(page) {
  const list = page.locator('#gallery-library');
  await list.focus();
  await checkNativeModifiedKeys(list,'ArrowDown');
  await page.keyboard.press('s');
  await page.waitForFunction(() => document.querySelector('#gallery-library').getAttribute('aria-activedescendant') === 'gallery-library-option-2');
  assert.equal(await list.getByRole('option', { selected: true }).getAttribute('id'), 'gallery-library-option-0');
  await page.keyboard.press('Space');
  await page.waitForFunction(() => document.querySelector('[data-gallery-library-choice]')?.textContent.includes('Something worth keeping · 3'));
  await page.keyboard.press('End');
  await page.waitForFunction(() => document.querySelector('#gallery-library').getAttribute('aria-activedescendant') === 'gallery-library-option-999');
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.querySelector('[data-gallery-library-choice]')?.textContent.includes('Room for a new thought · 1000'));
  assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-library');
  assert(await list.getByRole('option').count() <= 12);
}
