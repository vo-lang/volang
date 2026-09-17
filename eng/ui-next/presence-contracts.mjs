import assert from 'node:assert/strict';

export const presenceContracts = ['presence-interrupted-exit-identity', 'presence-exit-inert', 'presence-exit-focus-return',
  'presence-completed-exit-reset', 'presence-reduced-motion', 'presence-no-animation-cleanup'];

export async function checkPresence(page, screenshot) {
  const toggle = page.locator('#gallery-motion-toggle'), note = page.locator('#gallery-motion-note');
  const wrapper = page.locator('#gallery-motion');
  // Extend the real CSS transition so the guest can be deliberately
  // interrupted independent of their compilation/runtime speed on CI hosts.
  const styles = await page.addStyleTag({ content: '#gallery-motion { transition-duration: 1.5s; }' });
  await note.fill('Keep this thought 中文');
  await page.evaluate(() => { window.presenceNote = document.querySelector('#gallery-motion-note'); });
  await page.waitForFunction(() => !document.querySelector('#gallery-motion').getAnimations().some(a => a.playState === 'running'));
  // Programmatic activation preserves the input focus up to the actual exit.
  await toggle.evaluate(button => button.click());
  await page.waitForFunction(() => document.querySelector('#gallery-motion')?.dataset.state === 'closed');
  assert.equal(await wrapper.evaluate(element => element.inert && element.getAttribute('aria-hidden') === 'true'), true);
  await page.waitForFunction(() => document.activeElement.id === 'gallery-motion-toggle');
  assert.equal(await note.inputValue(), 'Keep this thought 中文');
  await toggle.evaluate(button => button.click());
  await page.waitForFunction(() => document.querySelector('#gallery-motion')?.dataset.state === 'open');
  assert.equal(await page.evaluate(() => window.presenceNote === document.querySelector('#gallery-motion-note')), true);
  assert.equal(await note.inputValue(), 'Keep this thought 中文');
  assert.equal(await wrapper.evaluate(element => element.inert), false);
  if (screenshot) await page.screenshot({ path: screenshot });
  // Finish entering, then allow the entire native exit and disposal to run.
  await page.waitForFunction(() => !document.querySelector('#gallery-motion').getAnimations().some(a => a.playState === 'running'));
  await note.focus();
  await page.getByRole('button', { name: 'Let it go', exact: true }).evaluate(button => button.click());
  await wrapper.waitFor({ state: 'detached' });
  assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-motion-toggle');
  await toggle.click();
  await note.waitFor();
  assert.equal(await note.inputValue(), 'A small thought for today.');
  assert.equal(await page.evaluate(() => window.presenceNote === document.querySelector('#gallery-motion-note')), false);
  await styles.evaluate(element => element.remove());

  await page.emulateMedia({ reducedMotion: 'reduce' });
  await note.fill('Quietly now');
  assert.equal(await wrapper.evaluate(element => getComputedStyle(element).transitionDuration), '0s');
  await toggle.click();
  await wrapper.waitFor({ state: 'detached' });
  await toggle.click();
  await note.waitFor();
  assert.equal(await wrapper.evaluate(element => element.getAnimations().length), 0);
  await page.emulateMedia({ reducedMotion: 'no-preference' });
  const none = await page.addStyleTag({ content: '#gallery-motion { transition: none; animation: none; }' });
  await note.fill('No animation');
  // Focus elsewhere before exit; closing must not steal it back to the trigger.
  await page.locator('#gallery-name').focus();
  await toggle.evaluate(button => button.click());
  await wrapper.waitFor({ state: 'detached' });
  assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-name');
  await toggle.click();
  await note.waitFor();
  await none.evaluate(element => element.remove());
}
