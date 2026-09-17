import assert from 'node:assert/strict';

export const dialogMotionContracts = ['modal-exit-retains-presentation', 'modal-exit-blocks-input',
  'modal-exit-focus-return', 'modal-exit-reopen-preserves-content', 'modal-exit-reduced-motion',
  'modal-exit-route-disposal'];

export async function checkDialogMotion(page) {
  const style = await page.addStyleTag({ content: '#gallery-dialog.vui-dialog-motion, #gallery-dialog.vui-dialog-motion::backdrop { transition-duration: 60s !important; }' });
  const finish = () => page.locator('#gallery-dialog').evaluate(dialog => {
    for (const animation of dialog.getAnimations()) if (animation.effect?.getComputedTiming().endTime !== Infinity) animation.finish();
  });
  await page.locator('[data-open-dialog]').click();
  await page.waitForFunction(() => document.querySelector('#gallery-dialog').matches(':modal') && document.activeElement.id === 'dialog-name');
  await finish();
  await page.locator('#dialog-name').fill('An idea kept through motion 中文');
  await page.evaluate(() => {
    window.motionDialog = document.querySelector('#gallery-dialog');
    window.motionInput = document.querySelector('#dialog-name');
  });
  await page.locator('[data-close-dialog]').click();
  await page.waitForFunction(() => document.querySelector('#gallery-dialog').dataset.state === 'closed');
  const closing = await page.locator('#gallery-dialog').evaluate(dialog => {
    document.querySelector('[data-open-dialog]').focus();
    return { modal: dialog.matches(':modal'), inert: dialog.inert, hidden: dialog.getAttribute('aria-hidden'),
      locked: document.documentElement.style.overflow, backgroundFocused: document.activeElement.matches('[data-open-dialog]'),
      animations: dialog.getAnimations().filter(animation => animation.playState === 'running').length };
  });
  assert.equal(closing.modal, true);
  assert.equal(closing.inert, true);
  assert.equal(closing.hidden, 'true');
  assert.equal(closing.locked, 'hidden');
  assert.equal(closing.backgroundFocused, false);
  assert(closing.animations > 0, 'modal exit was not waiting on real CSS motion');

  // An application may reopen through a command while user input is blocked.
  // Invoke the same guest handler without pretending an inert control is usable.
  await page.locator('[data-open-dialog]').evaluate(button => button.click());
  await page.waitForFunction(() => {
    const dialog = document.querySelector('#gallery-dialog');
    return dialog.dataset.state === 'open' && !dialog.inert && dialog.matches(':modal') && dialog.contains(document.activeElement);
  });
  await finish();
  assert.equal(await page.evaluate(() => window.motionDialog === document.querySelector('#gallery-dialog')
    && window.motionInput === document.querySelector('#dialog-name')), true);
  assert.equal(await page.locator('#dialog-name').inputValue(), 'An idea kept through motion 中文');

  await page.locator('[data-close-dialog]').click();
  await page.waitForFunction(() => document.querySelector('#gallery-dialog').dataset.state === 'closed');
  await finish();
  await page.waitForFunction(() => !document.querySelector('#gallery-dialog').open && document.activeElement.matches('[data-open-dialog]'));
  assert.equal(await page.evaluate(() => document.documentElement.style.overflow), '');
  assert.equal(await page.evaluate(() => window.motionInput === document.querySelector('#dialog-name')), true);

  await style.evaluate(element => element.remove());
  await page.emulateMedia({ reducedMotion: 'reduce' });
  await page.locator('[data-open-dialog]').click();
  await page.waitForFunction(() => document.querySelector('#gallery-dialog').matches(':modal'));
  assert.equal(await page.locator('#gallery-dialog').evaluate(dialog => dialog.getAnimations().length), 0);
  await page.keyboard.press('Escape');
  await page.waitForFunction(() => !document.querySelector('#gallery-dialog').open && document.activeElement.matches('[data-open-dialog]'));
  await page.emulateMedia({ reducedMotion: 'no-preference' });

  const held = await page.addStyleTag({ content: '#gallery-dialog.vui-dialog-motion { transition-duration: 60s !important; }' });
  await page.locator('[data-open-dialog]').click();
  await page.waitForFunction(() => document.querySelector('#gallery-dialog').matches(':modal'));
  await finish();
  await page.locator('[data-close-dialog]').click();
  await page.waitForFunction(() => document.querySelector('#gallery-dialog').dataset.state === 'closed');
  await page.locator('[data-nav=docs]').evaluate(link => link.click());
  await page.getByRole('heading', { name: 'A small idea, brought to life.' }).waitFor();
  assert.equal(await page.evaluate(() => !window.motionDialog.isConnected && !document.querySelector('dialog:modal')
    && document.documentElement.style.overflow === ''), true);
  await held.evaluate(element => element.remove());
  await page.getByRole('link', { name: 'Gallery', exact: true }).click();
  await page.locator('[data-open-dialog]').waitFor();
}
