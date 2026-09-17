import assert from 'node:assert/strict';

export async function editEarlyDefaults(page) {
  assert.equal(await page.locator('#defaults-input').inputValue(), 'A starting point');
  assert.equal(await page.locator('#defaults-notes').inputValue(), '\nA small note.\n中文');
  await page.locator('#defaults-input').fill('Before boot · 默认');
  await page.locator('#defaults-notes').fill('Before boot notes');
  await page.locator('#defaults-check').uncheck();
  await page.locator('#defaults-radio-b').check();
  await page.evaluate(() => { window.earlyDefaultInput = document.querySelector('#defaults-input'); });
}

export async function checkEarlyDefaults(page) {
  await page.waitForFunction(() => document.querySelector('[data-default-last-input]').textContent === 'Before boot · 默认'
    && document.querySelector('[data-default-last-notes]').textContent === 'Before boot notes'
    && document.querySelector('[data-default-last-check]').textContent === 'false'
    && document.querySelector('[data-default-last-radio]').textContent === 'b');
  assert.equal(await page.evaluate(() => window.earlyDefaultInput === document.querySelector('#defaults-input')), true);
  assert.equal(await page.locator('#defaults-input').inputValue(), 'Before boot · 默认');
  assert.equal(await page.locator('#defaults-notes').inputValue(), 'Before boot notes');
  assert.equal(await page.locator('#defaults-check').isChecked(), false);
  assert.equal(await page.locator('#defaults-radio-b').isChecked(), true);
}

export async function checkDefaults(page) {
  await page.locator('#defaults-fixed').fill('An unaccepted edit');
  await page.locator('#defaults-fixed-check').click();
  await page.waitForFunction(() => document.querySelector('#defaults-fixed').value === 'Component value'
    && !document.querySelector('#defaults-fixed-check').checked);
  const input = page.locator('#defaults-input'), notes = page.locator('#defaults-notes');
  const checkbox = page.locator('#defaults-check');
  await page.evaluate(() => { window.keptDefaultInput = document.querySelector('#defaults-input'); });
  await input.fill('An edited title');
  await notes.fill('An edited note · 中文');
  await checkbox.uncheck();
  await page.locator('#defaults-radio-b').check();
  for (const changed of [true, false, true]) {
    await page.locator('[data-change-defaults]').click();
    await page.waitForFunction(value => document.querySelector('[data-change-defaults]').dataset.defaultState === String(value), changed);
    assert.equal(await input.inputValue(), 'An edited title', 'default change replaced native text editing');
    assert.equal(await notes.inputValue(), 'An edited note · 中文');
    assert.equal(await checkbox.isChecked(), false, 'default change replaced edited checkedness');
    assert.equal(await checkbox.evaluate(element => element.defaultChecked), !changed);
    assert.equal(await input.evaluate(element => element.defaultValue), changed ? 'Updated baseline' : 'A starting point');
    assert.equal(await page.locator('#defaults-radio-b').isChecked(), true);
    assert.equal(await page.evaluate(() => window.keptDefaultInput === document.querySelector('#defaults-input')), true);
  }
  await page.locator('[data-reset-defaults]').click();
  await page.waitForFunction(() => document.querySelector('#defaults-input').value === 'Updated baseline'
    && document.querySelector('#defaults-notes').value === 'Updated note');
  assert.equal(await checkbox.isChecked(), false);
  assert.equal(await page.locator('#defaults-radio-a').isChecked(), true);
  assert.deepEqual(await page.locator('#native-default-form').evaluate(element => [...new FormData(element)]), [
    ['title', 'Updated baseline'], ['notes', 'Updated note'], ['plan', 'a'],
  ]);
  await page.locator('[data-change-defaults]').click();
  await page.waitForFunction(() => document.querySelector('#defaults-input').value === 'A starting point');
  assert.equal(await checkbox.isChecked(), true, 'pristine checkedness did not follow the changed default');
  assert.equal(await notes.inputValue(), '\nA small note.\n中文');
}
