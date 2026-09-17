import assert from 'node:assert/strict';

export async function editEarlyMultiSelect(page) {
  assert.deepEqual(await page.locator('#journey-destinations').evaluate(select => [...select.selectedOptions].map(option => option.value)), ['lisbon', 'taipei']);
  await page.locator('#journey-destinations').selectOption(['lisbon', 'kyoto']);
  await page.locator('#journey-days').selectOption(['fri', 'sun']);
  await page.evaluate(() => { window.earlyDestination = document.getElementById('journey-destinations'); });
}

export async function checkEarlyMultiSelect(page) {
  await page.waitForFunction(() => document.querySelector('[data-journey-values]').textContent === 'lisbon,kyoto'
    && document.querySelector('[data-journey-days]').textContent === 'fri,sun');
  assert(await page.evaluate(() => window.earlyDestination === document.getElementById('journey-destinations')));
}

export async function checkMultiSelect(page) {
  const select = page.getByRole('listbox', { name: 'Places to explore' });
  const reset = page.locator('[data-journey-reset]');
  const settle = values => page.waitForFunction(values => document.querySelector('[data-journey-values]').textContent === values.join(','), values);
  await reset.click();
  await settle(['lisbon', 'taipei']);
  assert.equal(await select.locator('option[value=later]').isDisabled(), true);
  await select.evaluate(select => {
    window.retainedDestination = select;
    for (const values of [['lisbon', 'kyoto'], ['taipei', 'kyoto']]) {
      for (const option of select.options) option.selected = values.includes(option.value);
      select.dispatchEvent(new Event('input', { bubbles: true }));
      select.dispatchEvent(new Event('change', { bubbles: true }));
    }
    const days = document.getElementById('journey-days');
    for (const option of days.options) option.selected = ['fri', 'sun'].includes(option.value);
    days.dispatchEvent(new Event('input', { bubbles: true }));
    days.dispatchEvent(new Event('change', { bubbles: true }));
    select.form.requestSubmit();
    select.form.requestSubmit();
  });
  await page.waitForFunction(() => document.querySelector('[data-journey-status]').textContent === 'Saved taipei,kyoto / fri,sun');
  await settle(['taipei', 'kyoto']);
  assert(await select.evaluate(select => select === window.retainedDestination));
  assert.deepEqual(await select.evaluate(select => new FormData(select.form).getAll('destinations')), ['taipei', 'kyoto']);
  assert.deepEqual(await select.evaluate(select => new FormData(select.form).getAll('days')), ['fri', 'sun']);
  await page.locator('[data-journey-disable]').click();
  await page.waitForFunction(() => document.getElementById('journey-destinations').disabled);
  assert.deepEqual(await select.evaluate(select => new FormData(select.form).getAll('destinations')), []);
  await page.locator('[data-journey-disable]').click();
  await page.waitForFunction(() => !document.getElementById('journey-destinations').disabled);

  // Compare real keyboard interaction to an unmanaged native control on the
  // same engine/platform; modifier behavior is owned by the browser.
  await select.selectOption(['lisbon']);
  await settle(['lisbon']);
  await select.evaluate(select => {
    const oracle = select.cloneNode(true);
    oracle.id = 'native-multiselect-oracle'; oracle.removeAttribute('name');
    oracle.removeAttribute('data-vo-id'); oracle.removeAttribute('data-vo-events');
    oracle.removeAttribute('data-vo-selected');
    for (const option of oracle.options) option.selected = option.value === 'lisbon';
    document.body.append(oracle);
  });
  try {
    const selections = [];
    for (const control of [page.locator('#native-multiselect-oracle'), select]) {
      await control.focus();
      await page.keyboard.press('Home');
      await page.keyboard.press('Shift+ArrowDown');
      await page.keyboard.press('Shift+ArrowDown');
      selections.push(await control.evaluate(select => [...select.selectedOptions].map(option => option.value)));
    }
    assert.deepEqual(selections[1], selections[0], 'controlled multiple selection changed native keyboard behavior');
    await settle(selections[0]);
  } finally { await page.locator('#native-multiselect-oracle').evaluate(element => element.remove()); }
  await select.selectOption([]);
  await page.locator('[data-journey-save]').click();
  await page.waitForFunction(() => document.getElementById('journey-destinations-error')?.textContent.includes('at least one')
    && document.activeElement?.id === 'journey-destinations');
  assert.equal(await select.getAttribute('aria-invalid'), 'true');
  await reset.click();
  await settle(['lisbon', 'taipei']);
  await page.waitForFunction(() => document.querySelector('[data-journey-days]').textContent === 'sat'
    && !document.getElementById('journey-destinations-error'));
  assert.equal(await page.locator('[data-journey-status]').textContent(), '');
}
