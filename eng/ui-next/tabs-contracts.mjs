import assert from 'node:assert/strict';
import {checkNativeModifiedKeys} from './kit-native-key-contracts.mjs';

export async function checkTabs(page) {
  const selected = index => page.waitForFunction(index => {
    const tab = document.getElementById(`gallery-tabs-tab-${index}`);
    return tab.getAttribute('aria-selected') === 'true' && document.activeElement === tab;
  }, index);
  await page.locator('#gallery-tabs-tab-0').focus();
  await checkNativeModifiedKeys(page.locator('#gallery-tabs-tab-0'),'ArrowRight');
  await page.keyboard.press('ArrowRight');
  await selected(1);
  await page.keyboard.press('ArrowRight');
  await selected(3);
  await page.keyboard.press('ArrowRight');
  await selected(0);
  await page.keyboard.press('End');
  await selected(3);
  await page.keyboard.press('Home');
  await selected(0);
  await page.evaluate(() => {
    const tab = document.querySelector('#gallery-tabs-tab-0');
    for (let index = 0; index < 2; index++) tab.dispatchEvent(new KeyboardEvent('keydown', { key: 'ArrowRight', bubbles: true, cancelable: true }));
  });
  await selected(3);
  await page.keyboard.press('Home');
  await selected(0);
  await page.keyboard.press('ArrowRight');
  await selected(1);
  await page.keyboard.press('Tab');
  assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-tabs-panel-1');
  await page.keyboard.press('Tab');
  assert.equal(await page.evaluate(() => document.activeElement.id), 'tab-note');
  await page.locator('#tab-note').fill('Retained across tabs 中文');
  await page.evaluate(() => { window.retainedTabInput = document.querySelector('#tab-note'); });
  await page.getByRole('tab', { name: 'Details', exact: true }).click();
  await selected(3);
  assert.equal(await page.locator('#tab-note').isVisible(), false);
  await page.getByRole('tab', { name: 'Your notes', exact: true }).click();
  await selected(1);
  assert.equal(await page.locator('#tab-note').inputValue(), 'Retained across tabs 中文');
  assert.equal(await page.evaluate(() => document.querySelector('#tab-note') === window.retainedTabInput), true);
  assert.equal(await page.getByRole('tab', { name: 'Later', exact: true }).isDisabled(), true);
  assert.equal(await page.getByRole('tabpanel').count(), 1);
  assert.equal(await page.locator('[role=tab][tabindex="0"]').count(), 1);
  await page.keyboard.press('Shift+Tab');
  assert.equal(await page.evaluate(() => document.activeElement.closest('[role=tablist]') === null), true, 'backwards Tab stayed in the tab list');
}
