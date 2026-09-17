import assert from 'node:assert/strict';

export const tableContracts = ['native-table-and-row-headers', 'table-sort-direction', 'table-batched-sort',
  'table-keyed-input-identity', 'bounded-pagination', 'pagination-batched-next', 'page-note-retention'];

export async function checkTable(page, screenshot) {
  const wrapper = page.locator('#gallery-table');
  const table = wrapper.locator('table');
  const pagination = page.getByRole('navigation', { name: 'Idea library pages' });
  const atPage = number => page.waitForFunction(number => document.querySelector('[data-gallery-table-page]')?.textContent.startsWith(`Page ${number} of`), number);
  const order = (column, direction) => page.waitForFunction(({ column, direction }) => {
    const heading = [...document.querySelectorAll('#gallery-table thead th')].find(node => node.textContent.startsWith(column));
    return heading?.getAttribute('aria-sort') === direction;
  }, { column, direction });
  await atPage(1);
  assert.equal(await table.getByRole('rowheader').count(), 3);
  const note = page.locator('#gallery-table-note-outdoors');
  await note.fill('Keep this little thought 中文');
  await page.evaluate(() => { window.keptTableNote = document.querySelector('#gallery-table-note-outdoors'); });
  // Numeric ascending retains outdoors in this page while moving its row.
  await table.getByRole('button', { name: 'Time', exact: true }).click();
  await order('Time', 'ascending');
  assert.equal(await table.locator('tbody tr').first().getByRole('rowheader').textContent(), 'A little sketch');
  assert.equal(await page.evaluate(() => window.keptTableNote === document.querySelector('#gallery-table-note-outdoors')), true);
  assert.equal(await note.inputValue(), 'Keep this little thought 中文');
  assert.equal(await table.locator('thead [aria-sort]').count(), 1);
  await table.getByRole('button', { name: 'Time', exact: true }).evaluate(button => { button.click(); button.click(); });
  // A distinct page commit acknowledges the preceding sort burst; the initial
  // ascending attribute alone would allow a premature assertion.
  await pagination.getByRole('button', { name: 'Next', exact: true }).click();
  await atPage(2);
  await order('Time', 'ascending');
  await pagination.getByRole('button', { name: 'Page 1', exact: true }).click();
  await atPage(1);
  assert.equal(await note.inputValue(), 'Keep this little thought 中文');
  // Reordering rows preserves the sort button and native keyboard activation.
  await table.getByRole('button', { name: 'Thought', exact: true }).focus();
  await page.keyboard.press('Enter');
  await order('Thought', 'ascending');
  assert.equal(await table.locator('tbody tr').first().getByRole('rowheader').textContent(), 'A day outdoors');
  await pagination.getByRole('button', { name: 'Next', exact: true }).evaluate(button => { button.click(); button.click(); });
  await atPage(3);
  assert.equal(await pagination.getByRole('button', { name: 'Next', exact: true }).isDisabled(), true);
  assert.equal(await table.locator('tbody tr').first().getByRole('rowheader').textContent(), 'Try a new recipe');
  await pagination.getByRole('button', { name: 'Page 1', exact: true }).click();
  await atPage(1);
  assert.equal(await note.inputValue(), 'Keep this little thought 中文');
  assert.equal(await pagination.locator('[aria-current=page]').textContent(), '1');
  assert.equal(await pagination.getByRole('button', { name: 'Previous', exact: true }).isDisabled(), true);
  assert.equal(await table.locator('tbody tr').count(), 3);
  await wrapper.evaluate(element => element.dir = 'rtl');
  await table.getByRole('button', { name: 'Thought', exact: true }).click();
  await order('Thought', 'descending');
  assert.equal(await table.locator('tbody tr').first().getByRole('rowheader').textContent(), 'Write a little story');
  await wrapper.evaluate(element => element.removeAttribute('dir'));
  if (screenshot) await wrapper.locator('xpath=ancestor::article').screenshot({ path: screenshot });
}
