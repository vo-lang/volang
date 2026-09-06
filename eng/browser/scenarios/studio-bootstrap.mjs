import { expect } from '@playwright/test';

// Delay one call at Studio's host-service boundary. This wrapper exists only
// in the test's intercepted response; deployed application bytes stay intact.
export async function prepareStudioBootstrap(page) {
  await page.route('**/studio-host.js', async route => {
    const response = await route.fetch();
    const source = await response.text();
    const entry = 'export async function createStudioHost(';
    expect(source.split(entry)).toHaveLength(2);
    const body = source.replace(entry, 'async function createStudioHostUnderTest(') + `
export async function createStudioHost(options) {
  const host = await createStudioHostUnderTest(options);
  let held = false;
  return async (...args) => {
    if (args[1] === 'projects.activate' && !held) {
      held = true;
      await new Promise(resolve => { globalThis.__volangBootstrapGate = resolve; });
    }
    return host(...args);
  };
}
`;
    await route.fulfill({ response, body });
  });
}

export async function runStudioBootstrapSmoke(contract) {
  const page = contract.page;
  const starter = page.getByRole('button', { name: 'Open Interactive counter example in Studio', exact: true });
  await expect.poll(() => page.evaluate(() => typeof globalThis.__volangBootstrapGate)).toBe('function');
  await expect(page.getByTestId('studio-initializing')).toBeVisible();
  await expect(starter).toHaveCount(0);
  await expect(page.getByRole('button', { name: 'New project', exact: true })).toHaveCount(0);
  await page.evaluate(() => { globalThis.__volangBootstrapGate(); delete globalThis.__volangBootstrapGate; });
  await expect(starter).toBeEnabled();
  await expect(page.getByTestId('studio-initializing')).toHaveCount(0);
  await starter.click();
  await expect(page.getByTestId('volang-code-editor')).toHaveValue(/UseIntState\(0\)/);
  return { complete: true, passed: true, checkpoints: {
    activationHeld: true, actionsUnavailable: true, activationReleased: true, firstClickOpened: true,
  } };
}
