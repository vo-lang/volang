import assert from 'node:assert/strict';

export async function checkForm(page) {
  const status = page.locator('[data-idea-status]');
  const source = page.locator('#idea-title');
  const submit = page.locator('[data-save-idea]');
  await submit.click();
  await page.waitForFunction(() => document.activeElement.id === 'idea-title' && document.getElementById('idea-title-error')?.textContent.includes('three characters'));
  assert.equal(await source.getAttribute('aria-invalid'), 'true');
  assert.match(await source.getAttribute('aria-describedby'), /idea-title-error/);
  await source.fill('hello');
  await page.keyboard.press('Tab');
  await page.waitForFunction(() => document.getElementById('idea-title-error')?.textContent.includes('descriptive'));
  await source.fill('A small garden');
  await page.evaluate(() => {
    document.querySelector('#idea-interests input[value=music]').click();
    document.querySelector('#idea-interests input[value=outside]').click();
    const form = document.getElementById('idea-title').form; form.requestSubmit(); form.requestSubmit();
  });
  await page.waitForFunction(() => document.querySelector('[data-idea-status]').textContent.startsWith('Saving'));
  await source.fill('A newer thought');
  await page.waitForFunction(() => document.querySelector('[data-idea-status]').textContent === 'Saved “A small garden”.');
  assert.equal(await source.inputValue(), 'A newer thought', 'save response overwrote a newer edit');
  await page.locator('#idea-with-note').check();
  await page.locator('#idea-note').fill('Keep this thought 中文');
  await page.locator('#idea-with-note').uncheck();
  await page.waitForFunction(() => !document.getElementById('idea-note'));
  await page.locator('#idea-with-note').check();
  assert.equal(await page.locator('#idea-note').inputValue(), 'Keep this thought 中文');
  await submit.click();
  await page.waitForFunction(() => document.querySelector('[data-idea-status]').textContent.startsWith('Saving'));
  await page.locator('[data-cancel-idea]').click();
  await page.waitForFunction(() => document.querySelector('[data-idea-status]').textContent === 'You have unsaved changes.');
  await page.locator('[data-reset-idea]').click();
  await page.waitForFunction(() => document.getElementById('idea-title').value === '' && document.getElementById('idea-note').value === ''
    && document.querySelector('[data-idea-status]').textContent === 'A small form, ready for your next idea.');
  assert.equal(await page.locator('#idea-title-error').count(), 0);
  assert.deepEqual(await page.locator('#idea-interests input:checked').evaluateAll(inputs => inputs.map(input => input.value)), ['paper']);
  assert.equal(await status.textContent(), 'A small form, ready for your next idea.');
  await page.locator('#idea-with-note').uncheck();
}
