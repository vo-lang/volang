import assert from 'node:assert/strict';
import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { serve, root } from './server.mjs';
import { checkDialogMotion, dialogMotionContracts } from './dialog-motion-contracts.mjs';
import { checkDialog } from './dialog-contracts.mjs';
import { checkDecisions } from './decision-contracts.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const browsers = await import('../browser/node_modules/playwright/index.mjs');
const server = await serve(), reports = [];
let browser;
try {
  for (const engine of ['chromium', 'firefox', 'webkit']) {
    browser = await browsers[engine].launch({ headless: true });
    for (const backend of ['vm']) {
      const page = await browser.newPage();
      const errors = [];
      page.on('pageerror', error => errors.push(error.message));
      await page.goto(`${server.url}/studio/gallery?backend=${backend}`);
      await checkDialog(page);
      await checkDecisions(page);
      await checkDialogMotion(page);
      await page.evaluate(async () => { window.__studioNext.close(); await window.__studioNext.done; });
      assert.deepEqual(errors, []);
      reports.push({ engine, backend, browserVersion: browser.version(), passed: true });
      await page.close();
      console.log(`${engine}/${backend}: native modal motion and focus passed`);
    }
    await browser.close(); browser = undefined;
  }
  await writeFile(resolve(root, 'target/ui-next/dialog-motion-report.json'), JSON.stringify({
    passed: true, build: JSON.parse(await readFile(resolve(root, 'target/ui-next/build-report.json'), 'utf8')),
    reports, contracts: dialogMotionContracts,
  }, null, 2) + '\n');
} finally { await browser?.close(); await server.close(); }
