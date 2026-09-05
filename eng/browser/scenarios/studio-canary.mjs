import { expect } from '@playwright/test';
import { randomUUID } from 'node:crypto';
import { waitForAotInteractive } from '../page-contract.mjs';

// Playwright supplies a fresh context with no user cookies or stored projects.
// Every mutation stays in that context's local Studio project store.
export async function runStudioCanarySmoke(contract, timeoutMilliseconds) {
  const page = contract.page;
  const projectName = `ci-canary-${randomUUID().slice(0, 12)}`;
  const button = name => page.getByRole('button', { name, exact: true });
  const editor = page.getByTestId('volang-code-editor');
  const checkpoints = {};
  const replaceSource = async source => {
    await editor.click();
    await editor.press('ControlOrMeta+A');
    await page.keyboard.insertText(source);
    await expect(editor).toHaveValue(source);
    await expect(button('Save File')).toBeEnabled();
    await expect(page.getByTestId('studio-topbar').getByRole('status')).toHaveText('0 problems');
  };

  await button('Open Interactive counter example in Studio').click();
  await expect(editor).toHaveValue(/UseIntState\(0\)/);
  const starter = await editor.inputValue();
  checkpoints.starter = true;

  await button('New project').click();
  await page.getByRole('textbox', { name: 'Project name', exact: true }).fill(projectName);
  await button('Create project').click();
  await expect(button(`Open project ${projectName}`)).toHaveAttribute('aria-selected', 'true');
  await expect(editor).toBeVisible();
  checkpoints.project = projectName;

  const runMarker = `run-${projectName}`;
  await replaceSource(`package main\n\nfunc main() { println("${runMarker}") }\n`);
  await button('Run VM').click();
  await expect(page.getByRole('log')).toContainText(runMarker);
  await expect(page.getByRole('log')).toContainText('process exited successfully');
  checkpoints.run = true;

  const edited = starter.replace('UseIntState(0)', 'UseIntState(41)')
    + `\n// saved by ${projectName}\n`;
  await replaceSource(edited);
  await button('Save File').click();
  await expect(button('Save File')).toBeDisabled();
  checkpoints.saved = true;

  await button('Open Preview').click();
  const preview = page.frameLocator('iframe[title="Volang application preview"]');
  await expect(preview.getByRole('button', { name: 'Count: 41', exact: true })).toBeVisible();
  await preview.getByRole('button', { name: 'Count: 41', exact: true }).click();
  await expect(preview.getByRole('button', { name: 'Count: 42', exact: true })).toBeVisible();
  checkpoints.previewInteraction = true;

  // Reload exercises persisted source, then navigate away and reopen through
  // the normal project selector. The preview's live counter must not overwrite
  // the saved source value.
  await contract.reload();
  await waitForAotInteractive(contract, timeoutMilliseconds);
  await expect(editor).toHaveValue(edited);
  await expect(button('Save File')).toBeDisabled();
  checkpoints.reload = true;
  await button('Home').click();
  await button(`Open project ${projectName}`).click();
  await expect(editor).toHaveValue(edited);
  await expect(page).toHaveURL(/\/workspace(?:[?#].*)?$/);
  await button('Open Preview').click();
  await expect(preview.getByRole('button', { name: 'Count: 41', exact: true })).toBeVisible();
  checkpoints.reopen = true;
  await contract.reload();
  await waitForAotInteractive(contract, timeoutMilliseconds);
  await expect(editor).toHaveValue(edited);
  checkpoints.reopenReload = true;
  await expect(page.locator('#volang-diagnostic')).toHaveText('');

  return { complete: true, passed: true, checkpoints };
}
