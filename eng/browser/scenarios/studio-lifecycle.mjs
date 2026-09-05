import { expect } from '@playwright/test';
import { runStudioCanarySmoke } from './studio-canary.mjs';
import { waitForAotInteractive } from '../page-contract.mjs';

// A longer journey runs in one isolated context. Each cycle is new work, with
// no retry: the first failed contract stops the test and retains its trace.
export async function runStudioLifecycleSmoke(contract, timeoutMilliseconds) {
  const page = contract.page;
  const context = page.context();
  const checkpoints = {};
  const button = name => page.getByRole('button', { name, exact: true });
  const editor = page.getByTestId('volang-code-editor');
  const log = page.getByRole('log');
  const replaceSource = async (source, dirty = true) => {
    await editor.click();
    await editor.press('ControlOrMeta+A');
    await page.keyboard.insertText(source);
    await expect(editor).toHaveValue(source);
    if (dirty) await expect(button('Save File')).toBeEnabled();
    else await expect(button('Save File')).toBeDisabled();
  };
  const workers = { created: 0, closed: 0 };
  page.on('worker', worker => {
    workers.created++;
    worker.once('close', () => workers.closed++);
  });

  checkpoints.canary = (await runStudioCanarySmoke(contract, timeoutMilliseconds)).checkpoints;
  const original = await editor.inputValue();
  await page.getByRole('tab', { name: 'Console', exact: true }).click();
  // A single analysis worker stays available while the document is open.
  await expect.poll(() => page.workers().length).toBe(1);
  const idleWorkers = page.workers().length;
  checkpoints.cycles = [];
  for (let cycle = 0; cycle < 3; cycle++) {
    const marker = `lifecycle-${cycle}-${checkpoints.canary.project}`;
    const createdBefore = workers.created;
    const closedBefore = workers.closed;
    await replaceSource(`package main\n\nfunc main() { println("${marker}") }\n`);
    await expect(page.getByTestId('studio-topbar').getByRole('status')).toHaveText('0 problems');
    await button('Run VM').click();
    await expect(log).toContainText(marker);
    await expect(log).toContainText('process exited successfully');
    await expect.poll(() => page.workers().length).toBe(idleWorkers);
    expect(workers.created).toBe(createdBefore + 1);
    expect(workers.closed).toBe(closedBefore + 1);

    await replaceSource('package main\n\nfunc main() { for {} }\n');
    await expect(page.getByTestId('studio-topbar').getByRole('status')).toHaveText('0 problems');
    await button('Run VM').click();
    await expect.poll(() => page.workers().length).toBe(idleWorkers + 1);
    await expect(button('Stop VM')).toBeEnabled();
    await button('Stop VM').click();
    await expect(button('Run VM')).toBeEnabled();
    await expect(log).toContainText('Run cancelled');
    await expect.poll(() => page.workers().length).toBe(idleWorkers);
    expect(workers.created).toBe(createdBefore + 2);
    expect(workers.closed).toBe(closedBefore + 2);
    checkpoints.cycles.push({ marker, completedWorkerClosed: true, cancelledWorkerClosed: true });
  }
  checkpoints.workerCleanup = { ...workers, remaining: page.workers().length };

  await replaceSource('package main\n\nfunc main() { missingLifecycleSymbol() }\n');
  await expect(page.getByTestId('studio-topbar').getByRole('status')).toContainText('1 problem');
  await replaceSource(original, false);
  await expect(page.getByTestId('studio-topbar').getByRole('status')).toHaveText('0 problems');
  await expect(button('Save File')).toBeDisabled();
  checkpoints.diagnosticRecovery = true;

  // Warm the real user path after the service worker controls the page, so its
  // normal cache population includes the optional compiler and preview assets.
  await expect.poll(() => page.evaluate(() => navigator.serviceWorker.controller !== null)).toBe(true);
  await contract.reload();
  await waitForAotInteractive(contract, timeoutMilliseconds);
  await expect(editor).toHaveValue(original);
  await button('Open Preview').click();
  const preview = page.frameLocator('iframe[title="Volang application preview"]');
  await expect(preview.getByRole('button', { name: 'Count: 41', exact: true })).toBeVisible();
  await expect.poll(() => page.evaluate(async () =>
    Boolean(await caches.match('/runtime/pkg/vo_web_bg.wasm')))).toBe(true);
  checkpoints.offlineWarmup = true;

  await context.setOffline(true);
  try {
    await contract.reload();
    await waitForAotInteractive(contract, timeoutMilliseconds);
    await expect(editor).toHaveValue(original);
    const offlineSource = original.replace('UseIntState(41)', 'UseIntState(73)');
    expect(offlineSource).not.toBe(original);
    await replaceSource(offlineSource);
    await expect(page.getByTestId('studio-topbar').getByRole('status')).toHaveText('0 problems');
    await button('Save File').click();
    await expect(button('Save File')).toBeDisabled();
    await button('Open Preview').click();
    await expect(preview.getByRole('button', { name: 'Count: 73', exact: true })).toBeVisible();
    await preview.getByRole('button', { name: 'Count: 73', exact: true }).click();
    await expect(preview.getByRole('button', { name: 'Count: 74', exact: true })).toBeVisible();
    await contract.reload();
    await waitForAotInteractive(contract, timeoutMilliseconds);
    await expect(editor).toHaveValue(offlineSource);
    checkpoints.offlineReloadEditPreviewSave = true;
  } finally {
    await context.setOffline(false);
  }
  await contract.reload();
  await waitForAotInteractive(contract, timeoutMilliseconds);
  await expect(editor).toHaveValue(/UseIntState\(73\)/);
  await button('Open Preview').click();
  await expect(preview.getByRole('button', { name: 'Count: 73', exact: true })).toBeVisible();
  checkpoints.onlineRecovery = true;
  await expect(page.locator('#volang-diagnostic')).toHaveText('');
  return { complete: true, passed: true, checkpoints };
}
