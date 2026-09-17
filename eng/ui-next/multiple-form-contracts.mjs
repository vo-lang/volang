import assert from 'node:assert/strict';

export async function checkMultipleForm(page) {
  const group = page.getByRole('group', { name: 'What makes you curious?' });
  const values = page.locator('[data-multiple-values]');
  const reset = page.locator('[data-multiple-reset]');
  await reset.click();
  await page.waitForFunction(() => document.querySelector('[data-multiple-values]').textContent === 'writing');
  assert.equal(await group.getByRole('checkbox', { name: 'Coming soon' }).isDisabled(), true);
  await group.evaluate(element => {
    window.multipleWriting = element.querySelector('input[value=writing]');
    element.querySelector('input[value=code]').click();
    element.querySelector('input[value=design]').click();
    element.closest('form').requestSubmit();
    element.closest('form').requestSubmit();
  });
  await page.waitForFunction(() => document.querySelector('[data-multiple-status]').textContent === 'Saved writing, design, code');
  assert.equal(await values.textContent(), 'writing,design,code', 'checkbox burst lost an earlier native edit');
  assert.equal(await group.evaluate(element => window.multipleWriting === element.querySelector('input[value=writing]')), true);
  assert.deepEqual(await group.evaluate(element => new FormData(element.closest('form')).getAll('interests')), ['writing', 'design', 'code']);
  assert.equal(await page.locator('[data-multiple-dirty]').textContent(), 'false');
  await page.locator('[data-multiple-disable]').click();
  await page.waitForFunction(() => document.getElementById('workbench-interests').disabled);
  assert.equal(await group.getByRole('checkbox', { name: 'Writing' }).isDisabled(), true);
  assert.deepEqual(await group.evaluate(element => new FormData(element.closest('form')).getAll('interests')), []);
  await page.locator('[data-multiple-disable]').click();
  await page.waitForFunction(() => !document.getElementById('workbench-interests').disabled);
  await group.getByRole('checkbox', { name: 'Code', exact: true }).focus();
  await page.keyboard.press('Space');
  await page.waitForFunction(() => document.querySelector('[data-multiple-values]').textContent === 'writing,design');
  await group.evaluate(element => {
    element.querySelector('input[value=writing]').click();
    element.querySelector('input[value=design]').click();
    element.closest('form').requestSubmit();
  });
  await page.waitForFunction(() => document.getElementById('workbench-interests-error')?.textContent.includes('at least one')
    && document.activeElement?.id === 'workbench-interests-option-0');
  assert.equal(await group.getAttribute('aria-invalid'), 'true');
  await reset.click();
  await page.waitForFunction(() => document.querySelector('[data-multiple-values]').textContent === 'writing'
    && !document.getElementById('workbench-interests-error'));
  assert.equal(await page.locator('[data-multiple-status]').textContent(), '');
  assert.equal(await page.locator('[data-multiple-dirty]').textContent(), 'false');
}
