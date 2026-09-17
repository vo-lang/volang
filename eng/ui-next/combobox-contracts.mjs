import assert from 'node:assert/strict';
import {checkNativeModifiedKeys} from './kit-native-key-contracts.mjs';

export async function checkCombobox(page, screenshot) {
  const input = page.locator('#destination-place');
  const popup = page.locator('#destination-place-listbox');
  const dialog = page.locator('#destination-dialog');
  const status = page.locator('[data-destination-status]');
  const active = label => page.waitForFunction(label => {
    const input = document.querySelector('#destination-place');
    const option = document.getElementById(input.getAttribute('aria-activedescendant'));
    return document.activeElement === input && option?.getAttribute('aria-label') === label
      && option.getAttribute('aria-selected') === 'true';
  }, label);
  const closed = () => page.waitForFunction(() => document.querySelector('#destination-place').getAttribute('aria-expanded') === 'false'
    && !document.querySelector('#destination-place-listbox').matches(':popover-open'));
  await page.locator('[data-open-destination]').click();
  await page.waitForFunction(() => document.querySelector('#destination-dialog').matches(':modal')
    && document.activeElement.id === 'destination-place');
  await closed();
  await checkNativeModifiedKeys(input,'ArrowDown');
  await page.keyboard.press('ArrowDown');
  await active('Amsterdam');
  await checkNativeModifiedKeys(input,'Enter');
  assert.equal(await page.evaluate(() => Math.abs(document.querySelector('#destination-place-listbox').getBoundingClientRect().width
    - document.querySelector('#destination-place').getBoundingClientRect().width) < 1), true, 'suggestions did not follow input width');
  await page.keyboard.press('ArrowDown');
  await active('Copenhagen');
  await page.keyboard.press('ArrowUp');
  await active('Amsterdam');
  await page.keyboard.press('ArrowUp');
  await active('Amsterdam');
  assert.equal(await input.inputValue(), '', 'browsing suggestions changed the text');
  await page.keyboard.press('ArrowDown');
  await active('Copenhagen');
  await page.keyboard.press('Enter');
  await closed();
  assert.equal(await input.inputValue(), 'Copenhagen');
  assert.equal(await status.textContent(), '', 'accepting a suggestion also submitted the form');
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.querySelector('[data-destination-status]').textContent === 'Next stop: Copenhagen.');
  assert.equal(await input.evaluate(element => new FormData(element.form).get('place')), 'Copenhagen');
  await page.keyboard.press('ArrowUp');
  await active('東京');
  await page.waitForFunction(() => {
    const popup = document.querySelector('#destination-place-listbox');
    const rect = document.getElementById(document.querySelector('#destination-place').getAttribute('aria-activedescendant')).getBoundingClientRect();
    const bounds = popup.getBoundingClientRect();
    return popup.scrollTop > 0 && rect.top >= bounds.top && rect.bottom <= bounds.bottom;
  });
  if (screenshot) await page.screenshot({ path: screenshot });
  await page.keyboard.press('Escape');
  await closed();
  assert.equal(await dialog.evaluate(element => element.matches(':modal')), true, 'Escape also closed the parent dialog');
  assert.equal(await input.inputValue(), 'Copenhagen');
  await input.fill('po');
  await popup.waitFor({ state: 'visible' });
  assert.deepEqual(await popup.getByRole('option').allTextContents(), ['Porto']);
  await page.keyboard.press('ArrowDown');
  await active('Porto');
  await page.keyboard.press('Enter');
  await closed();
  assert.equal(await input.inputValue(), 'Porto');
  await input.fill('Someplace entirely new');
  await page.locator('#destination-place-empty').waitFor({ state: 'visible' });
  await closed();
  assert.equal(await input.getAttribute('aria-activedescendant'), null);
  assert.equal(await input.getAttribute('aria-describedby'), 'destination-place-empty destination-place-hint');
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.querySelector('[data-destination-status]').textContent === 'Next stop: Someplace entirely new.');
  await input.evaluate(element => {
    element.dispatchEvent(new CompositionEvent('compositionstart', { bubbles: true }));
    element.value = '京';
    element.dispatchEvent(new InputEvent('input', { bubbles: true, data: '京', isComposing: true }));
  });
  await closed();
  assert.equal(await input.evaluate(element => element.dispatchEvent(new KeyboardEvent('keydown', { key: 'Enter', bubbles: true, cancelable: true, isComposing: true }))), true, 'composition acceptance was intercepted');
  await input.evaluate(element => {
    element.value = '京都';
    element.dispatchEvent(new CompositionEvent('compositionend', { bubbles: true, data: '京都' }));
  });
  await popup.waitFor({ state: 'visible' });
  await page.keyboard.press('ArrowDown');
  await active('京都');
  await input.evaluate(element => element.setSelectionRange(1, 1));
  await page.keyboard.press('ArrowLeft');
  await page.waitForFunction(() => !document.querySelector('#destination-place').hasAttribute('aria-activedescendant'));
  assert.equal(await input.evaluate(element => element.selectionStart), 0, 'native text editing did not move the cursor');
  await page.keyboard.press('Escape');
  await closed();
  await input.click();
  await popup.waitFor({ state: 'visible' });
  await popup.getByRole('option', { name: 'Berlin', exact: true }).click({ force: true });
  assert.equal(await input.inputValue(), '京都');
  assert.equal(await input.getAttribute('aria-expanded'), 'true', 'disabled suggestion closed the popup');
  await popup.getByRole('option', { name: 'Paris', exact: true }).click();
  await closed();
  assert.equal(await input.inputValue(), 'Paris');
  assert.equal(await page.evaluate(() => document.activeElement.id), 'destination-place', 'pointer selection lost input focus');
  await page.keyboard.press('ArrowDown');
  await active('Amsterdam');
  await page.keyboard.press('Tab');
  await closed();
  assert.equal(await page.evaluate(() => document.activeElement.textContent), 'Save destination', 'Tab entered a suggestion instead of the next field');
  await dialog.getByRole('button', { name: 'Reset destination' }).click();
  await page.waitForFunction(() => document.querySelector('#destination-place').value === '');
  await dialog.getByRole('button', { name: 'Save destination' }).click();
  await page.waitForFunction(() => document.querySelector('#destination-place').getAttribute('aria-invalid') === 'true'
    && document.activeElement.id === 'destination-place');
  assert.equal(await input.getAttribute('aria-describedby'), 'destination-place-error destination-place-hint');
  await input.fill('Paris');
  await page.keyboard.press('Escape');
  await closed();
  await page.keyboard.press('Escape');
  await page.waitForFunction(() => !document.querySelector('#destination-dialog').open);
  await page.locator('[data-open-destination]').click();
  await input.waitFor({ state: 'visible' });
  assert.equal(await input.inputValue(), 'Paris', 'dialog closure discarded the field value');
  await dialog.getByRole('button', { name: 'Close dialog' }).click();
  await page.waitForFunction(() => !document.querySelector('#destination-dialog').open
    && document.activeElement.matches('[data-open-destination]'));
}
