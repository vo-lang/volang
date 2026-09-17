import assert from 'node:assert/strict';

export const choiceContracts = ['native-choice-labels', 'native-radio-keyboard', 'native-choice-disabled',
  'native-choice-required', 'native-choice-form-data', 'native-choice-reset', 'native-choice-hydration'];

export async function editEarlyChoices(page) {
  await page.locator('#preferences-language').selectOption('zh');
  await page.locator('#preferences-density input[value=compact]').check();
  await page.locator('#preferences-tips').uncheck();
  await page.evaluate(() => { window.earlyChoiceSelect = document.querySelector('#preferences-language'); });
}

export async function checkEarlyChoices(page) {
  assert.equal(await page.locator('#preferences-language').inputValue(), 'zh');
  assert.equal(await page.locator('#preferences-density input[value=compact]').isChecked(), true);
  assert.equal(await page.locator('#preferences-tips').isChecked(), false);
  assert.equal(await page.evaluate(() => window.earlyChoiceSelect === document.querySelector('#preferences-language')), true);
}

export async function checkChoices(page) {
  const form = page.locator('#preferences-form');
  const language = page.getByRole('combobox', { name: 'A familiar language', exact: true });
  const group = page.getByRole('group', { name: 'Room to breathe', exact: true });
  const compact = group.getByRole('radio', { name: 'Compact', exact: true });
  const comfortable = group.getByRole('radio', { name: 'Comfortable', exact: true });
  const tips = page.getByRole('checkbox', { name: 'Show helpful tips', exact: true });
  await form.getByRole('button', { name: 'Reset preferences' }).click();
  await page.waitForFunction(() => document.querySelector('[data-preferences-result]').textContent === 'A fresh start, just for you.');
  assert.equal(await language.inputValue(), '');
  assert.equal(await language.evaluate(el => el.validity.valueMissing), true);
  await form.getByRole('button', { name: 'Save preferences' }).click();
  assert.equal(await page.locator('[data-preferences-result]').textContent(), 'A fresh start, just for you.', 'invalid native form submitted');
  assert.equal(await page.evaluate(() => document.activeElement.id), 'preferences-language');
  await language.selectOption('zh');
  assert.equal(await language.locator('option[value=fr]').isDisabled(), true);
  assert.equal(await group.getByRole('radio', { name: 'Spacious · coming soon', exact: true }).isDisabled(), true);
  await comfortable.focus();
  await page.keyboard.press('ArrowDown');
  assert.equal(await compact.isChecked(), true, 'native radio arrows failed to skip disabled choice');
  assert.equal(await compact.evaluate(el => document.activeElement === el), true);
  assert.equal(await compact.getAttribute('aria-describedby'), 'preferences-density-hint');
  await page.keyboard.press('ArrowUp');
  assert.equal(await comfortable.isChecked(), true);
  await group.locator('label').filter({ hasText: /^Compact$/ }).click();
  assert.equal(await compact.isChecked(), true);
  await tips.focus();
  await page.keyboard.press('Space');
  assert.equal(await tips.isChecked(), false);
  await page.locator('label[for=preferences-tips]').click();
  assert.equal(await tips.isChecked(), true);
  assert.deepEqual(await form.evaluate(el => [...new FormData(el)]), [['language', 'zh'], ['density', 'compact'], ['tips', 'yes']]);
  await form.getByRole('button', { name: 'Save preferences' }).click();
  await page.waitForFunction(() => document.querySelector('[data-preferences-result]').textContent === 'Your preferences are ready.');
  // An unrelated owner update keeps browser-owned selections and DOM identity.
  assert.equal(await language.inputValue(), 'zh');
  assert.equal(await compact.isChecked(), true);
  await form.getByRole('button', { name: 'Reset preferences' }).click();
  await page.waitForFunction(() => document.querySelector('[data-preferences-result]').textContent === 'A fresh start, just for you.');
  assert.equal(await language.inputValue(), '');
  assert.equal(await comfortable.isChecked(), true);
  assert.equal(await tips.isChecked(), true);
  assert.deepEqual(await form.evaluate(el => [...new FormData(el)]), [['density', 'comfortable'], ['tips', 'yes']]);
}

export async function checkControlledChoices(page) {
  const form = page.locator('#controlled-choice-form');
  const range = page.locator('#native-range');
  assert.equal(await range.inputValue(), '0.25');
  await range.focus();
  await page.keyboard.press('ArrowRight');
  await page.keyboard.press('ArrowRight');
  assert.equal(await range.inputValue(), '0.75');
  await page.evaluate(() => {
    const checkbox = document.querySelector('#controlled-check');
    checkbox.click(); checkbox.click(); checkbox.click();
    document.querySelector('#controlled-radio input[value=b]').click();
    const select = document.querySelector('#controlled-select');
    select.value = 'b'; select.dispatchEvent(new Event('change', { bubbles: true }));
    document.querySelector('#controlled-choice-form').requestSubmit();
  });
  await page.waitForFunction(() => document.querySelector('[data-choice-saved]').textContent === 'true/b/b');
  assert.equal(await page.locator('#controlled-check').isChecked(), true);
  assert.equal(await page.locator('#controlled-radio input[value=b]').isChecked(), true);
  assert.equal(await page.locator('#controlled-select').inputValue(), 'b');
  assert.deepEqual(await form.evaluate(el => [...new FormData(el)]), [['range', '0.75'], ['check', 'on'], ['plan', 'b'], ['theme', 'b'], ['external', 'on']]);
  await page.locator('[data-focus-choice]').click();
  await page.waitForFunction(() => document.activeElement === document.querySelector('#controlled-radio input[value=b]'));
  await form.getByRole('button', { name: 'Reset choices' }).click();
  await page.waitForFunction(() => document.querySelector('#controlled-check').checked
    && document.querySelector('#controlled-radio input[value=b]').checked
    && document.querySelector('#controlled-select').value === 'b');
  assert.equal(await range.inputValue(), '0.25', 'native slider reset did not restore its fractional default');
  await page.locator('[data-disable-choices]').click();
  await page.waitForFunction(() => document.querySelector('#controlled-radio').disabled);
  assert.equal(await page.locator('#controlled-check').isDisabled(), true);
  assert.equal(await page.locator('#controlled-radio input[value=b]').isDisabled(), true);
  assert.equal(await page.locator('#controlled-select').isDisabled(), true);
  assert.deepEqual(await form.evaluate(el => [...new FormData(el)]), [['range', '0.25'], ['external', 'on']]);
  await page.locator('[data-disable-choices]').click();
  await page.waitForFunction(() => !document.querySelector('#controlled-radio').disabled);
}
