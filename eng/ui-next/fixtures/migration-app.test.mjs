import {test,expect} from './fixtures.mjs';

async function delayApplication(page, appURL) {
  let release, requested;
  const gate = new Promise(resolve => {release = resolve;});
  const waiting = new Promise(resolve => {requested = resolve;});
  await page.route(/\/assets\/app\.vob(\?|$)/, async route => {requested(); await gate; await route.continue();});
  await page.goto(appURL, {waitUntil: 'commit'});
  // Receiving the artifact request proves mount has begun owning the SSR root.
  await waiting;
  return release;
}

test('the migrated settings preserve queued input and save in order', async ({page, appURL}) => {
  const release = await delayApplication(page, appURL);
  try {
    const input = page.getByRole('textbox', {name: 'Display name'});
    const save = page.getByRole('button', {name: 'Save', exact: true});
    await input.fill('Early 中文 😀');
    await input.evaluate(element => {window.earlyInput = element;});
    await save.click();
    release();
    await expect(page.getByText('Opening your idea…', {exact: true})).toBeHidden();
    await expect(page.getByRole('status')).toHaveText('Settings saved');
    await expect(input).toHaveValue('Early 中文 😀');
    expect(await input.evaluate(element => element === window.earlyInput)).toBe(true);
    await expect(save).toBeDisabled();
    await input.fill('');
    await expect(save).toBeDisabled();
    await expect(input).toHaveAttribute('aria-invalid', 'true');
    await expect(page.getByRole('alert')).toHaveText('Display name is required');
    await input.fill('Ada');
    await expect(save).toBeEnabled();
    await save.click();
    await expect(save).toBeDisabled();
    const notifications = page.getByRole('switch', {name: 'Notifications'});
    await expect(notifications).toBeChecked();
    await notifications.click();
    await expect(notifications).not.toBeChecked();
    await expect(page.getByRole('status')).toHaveText('Changes have not been saved');
    await expect(save).toBeEnabled();
  } finally {release();}
});

test('the migrated save action validates the latest queued name', async ({page, appURL}) => {
  const release = await delayApplication(page, appURL);
  try {
    await page.getByRole('textbox', {name: 'Display name'}).fill('');
    await page.getByRole('button', {name: 'Save', exact: true}).click();
    release();
    await expect(page.getByText('Opening your idea…', {exact: true})).toBeHidden();
    await expect(page.getByRole('status')).toHaveText('Changes have not been saved');
    await expect(page.getByRole('alert')).toHaveText('Display name is required');
    await expect(page.getByRole('button', {name: 'Save', exact: true})).toBeDisabled();
  } finally {release();}
});
