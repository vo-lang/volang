import {sourceEditor} from './editor-controls.mjs';
import {instrumentRecovery, seedRecovery} from './studio-recovery-contracts.mjs';
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { readFile, writeFile, rm } from 'node:fs/promises';
import { resolve } from 'node:path';
import { root } from './server.mjs';

// This contract temporarily changes Studio sources to exercise real compiler
// failures. Run it after browser/SSR contracts and builds have finished.
process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const { chromium } = await import('../browser/node_modules/playwright/index.mjs');
const child = spawn(process.execPath, ['eng/ui-next/cli.mjs', 'dev'], { cwd: root, env: process.env, stdio: ['ignore', 'pipe', 'pipe'] });
const stylesheet = resolve(root, 'apps/studio/next/studio.css');
const invalid = resolve(root, `apps/studio/next/development_probe_${process.pid}.vo`);
const marker = `\n/* development-contract-${process.pid} */\n.studio { --development-contract: verified; }\n`;
let browser, probeCreated = false, cssAdded = false;
let logs = '';
child.stderr.on('data', bytes => { logs += bytes; });
try {
  const url = await new Promise((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error(`Development server did not start:\n${logs}`)), 30000);
    child.once('error', error => { clearTimeout(timer); reject(error); });
    child.once('exit', code => { clearTimeout(timer); reject(new Error(`Development server exited ${code}:\n${logs}`)); });
    child.stdout.on('data', bytes => {
      logs += bytes;
      const match = logs.match(/http:\/\/127\.0\.0\.1:\d+\/studio\/gallery\?backend=vm/);
      if (match) { clearTimeout(timer); resolve(match[0]); }
    });
  });
  browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();
  await instrumentRecovery(page);
  const connected = page.waitForResponse(response => response.url().endsWith('/__ui-next/events'));
  await page.goto(url);
  await connected;
  await page.getByRole('button', { name: 'Make it happen' }).click();
  await page.waitForFunction(() => document.querySelector('[data-demo-count]').textContent.startsWith('1 '));
  await page.evaluate(() => { window.developmentCounter = document.querySelector('[data-demo-count]'); });
  await writeFile(stylesheet, await readFile(stylesheet, 'utf8') + marker);
  cssAdded = true;
  await page.waitForFunction(() => getComputedStyle(document.querySelector('.studio')).getPropertyValue('--development-contract').trim() === 'verified');
  assert.equal(await page.evaluate(() => window.developmentCounter === document.querySelector('[data-demo-count]')), true);
  assert.match(await page.locator('[data-demo-count]').textContent(), /^1 /);
  await writeFile(invalid, 'package main\nfunc incomplete(\n', { flag: 'wx' });
  probeCreated = true;
  await page.locator('#ui-development-error').waitFor();
  assert.equal(await page.evaluate(() => window.developmentCounter === document.querySelector('[data-demo-count]')), true);
  await page.getByRole('button', { name: 'Make it happen' }).click();
  await page.waitForFunction(() => document.querySelector('[data-demo-count]').textContent.startsWith('2 '));
  await rm(invalid);
  probeCreated = false;
  await page.waitForFunction(() => !document.querySelector('#ui-development-error') && document.querySelector('[data-demo-count]')?.textContent.startsWith('2 '));
  assert.equal(await page.evaluate(() => window.__studioNext.error), null);
  const snapshot = await (await fetch(new URL('/artifacts/playground-ui.json', url))).json();
  assert.equal(snapshot.format, 1);
  assert(snapshot.files.some(file => file.path === 'vendor/ui/next/kit/presence.vo'), 'dev startup did not package current UI sources');
  await page.getByRole('link', { name: 'Playground', exact: true }).click();
  const reload = async () => {
    await page.evaluate(() => {
      window.beforeReload = document.querySelector('.studio');
      window.dispatchEvent(new CustomEvent('vo-ui-reload', { cancelable: true, detail: { version: String(Date.now()) } }));
    });
    await page.waitForFunction(() => document.querySelector('.studio') !== window.beforeReload && !document.querySelector('#ui-development-error'));
  };
  const pendingSource = 'package main\nfunc main() { for {} }\n';
  await page.locator('.cm-content').waitFor();
  await sourceEditor(page).fill(pendingSource);
  await sourceEditor(page).press('ControlOrMeta+a');
  await page.keyboard.press('ArrowLeft');
  await page.keyboard.press('Shift+ArrowRight');
  await page.keyboard.press('Shift+ArrowRight');
  const selection = await sourceEditor(page).input.evaluate(input => [input.selectionStart, input.selectionEnd]);
  assert.deepEqual(selection, [0, 2]);
  await reload();
  await page.waitForFunction(() => document.activeElement.id === 'playground-source' || document.activeElement.classList.contains('cm-content'));
  assert.deepEqual(await sourceEditor(page).input.evaluate(input => [input.selectionStart, input.selectionEnd]), selection);
  assert.equal(await sourceEditor(page).inputValue(), pendingSource);
  await page.locator('[data-run]').click();
  await page.waitForFunction(() => document.querySelector('[data-stop]')?.disabled === false);
  await reload();
  await page.waitForFunction(() => document.querySelector('[data-run]')?.disabled === false && document.querySelector('[data-stop]')?.disabled === true, null, { timeout: 5000 });
  assert.equal(await page.locator('#playground-source').inputValue(), pendingSource);
  await page.waitForFunction(() => window.__studioNext.workers.started === window.__studioNext.workers.stopped);
  await page.getByLabel('Choose an example', { exact: true }).selectOption('channels');
  await page.getByRole('button', { name: 'Open example', exact: true }).click();
  await page.waitForFunction(() => document.querySelector('#playground-source')?.value.includes('Squares total:'));
  await reload();
  assert.equal(await page.getByLabel('Choose an example', { exact: true }).inputValue(), 'channels');
  await page.getByRole('button', { name: 'Restore previous draft', exact: true }).click();
  await page.waitForFunction(source => document.querySelector('#playground-source')?.value === source, pendingSource);
  await page.getByRole('link', { name: 'Try UI components →', exact: true }).click();
  await page.locator('[data-run-preview]').click();
  await page.frameLocator('iframe').getByRole('button', { name: 'One more idea', exact: true }).click({ timeout: 35000 });
  await page.waitForFunction(() => document.querySelector('iframe')?.contentDocument.querySelector('output')?.textContent === '1 little ideas');
  await reload();
  await page.waitForFunction(() => !document.querySelector('iframe') && document.querySelector('[data-stop-preview]')?.disabled === true);
  assert((await page.locator('#ui-playground-source').inputValue()).includes('Make something good.'));
  await page.locator('[data-run-preview]').click();
  await page.frameLocator('iframe').getByRole('button', { name: 'One more idea', exact: true }).waitFor({ timeout: 35000 });
  await page.locator('[data-stop-preview]').click();
  await page.waitForFunction(() => window.__studioNext.workers.started === window.__studioNext.workers.stopped);
  await page.getByRole('link', {name:'Recover browser projects', exact:true}).click();
  await page.getByRole('button', {name:'Find browser projects', exact:true}).waitFor();
  await seedRecovery(page);
  await page.getByRole('button', {name:'Find browser projects', exact:true}).click();
  await page.getByLabel('Browser project', {exact:true}).waitFor();
  await page.getByRole('button', {name:'Prepare download', exact:true}).click();
  await page.locator('a[download]').waitFor();
  const preparedURL = await page.locator('a[download]').getAttribute('href');
  await reload();
  await page.waitForFunction(url => window.recoveryProbe.revoked.includes(url), preparedURL);
  assert.equal(await page.locator('a[download]').count(), 0);
  assert.equal(await page.getByLabel('Browser project', {exact:true}).count(), 0);
  assert.equal(await page.getByRole('button', {name:'Find browser projects', exact:true}).isEnabled(), true);
  await writeFile(resolve(root, 'target/ui-next/development-report.json'), JSON.stringify({
    passed: true, contracts: ['initial-vm-build', 'live-css-preserves-dom-and-state', 'compile-error-retains-working-page', 'source-fix-rebuilds-and-recovers', 'development-ui-source-snapshot', 'development-ui-preview'],
    componentStateReload: true, runningWorkReload: true, exampleDraftRestoreReload: true, editorFocusSelectionReload:true, recoveryDownloadReload:true,
  }, null, 2) + '\n');
  console.log('Studio development contracts passed: live CSS, last good page, diagnostics and automatic recovery');
} finally {
  await browser?.close();
  const exited = new Promise(resolve => { if (child.exitCode !== null) resolve(); else child.once('exit', resolve); });
  child.kill('SIGTERM');
  await exited;
  if (probeCreated) await rm(invalid);
  if (cssAdded) {
    const current = await readFile(stylesheet, 'utf8');
    await writeFile(stylesheet, current.replace(marker, ''));
  }
}
