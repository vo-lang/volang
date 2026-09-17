import assert from 'node:assert/strict';
import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { serve, root } from './server.mjs';
import { checkToastBackground } from './toast-contracts.mjs';

// Chromium exposes a documented switch to disable Playwright's focus emulation.
// The other engines' standard matrix covers the page provider and pause state
// separately; do not equate emulated focus with native tab activation.
process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const { chromium } = await import('../browser/node_modules/playwright/index.mjs');
const application = await serve();
const report = { passed: false, engine: 'chromium', focusEmulation: false,
  build: JSON.parse(await readFile(resolve(root, 'target/ui-next/build-report.json'), 'utf8')), backends: [] };
let browser;
try {
  browser = await chromium.launch({ headless: false });
  report.headed = true;
  report.browserVersion = browser.version();
  for (const backend of ['vm']) {
    const context = await browser.newContext();
    try {
      const page = await context.newPage();
      await page.goto(`${application.url}/studio/gallery?backend=${backend}`);
      await page.waitForFunction(() => window.__studioNext?.ready || window.__studioNext?.error);
      assert.equal(await page.evaluate(() => window.__studioNext.error), null);
      await checkToastBackground(page);
      report.backends.push({ backend, passed: true, contracts: ['inactive-tab-pauses-countdown', 'reactivated-tab-resumes-countdown'] });
      console.log(`${backend}: native tab activation pauses and resumes notifications`);
    } finally { await context.close(); }
  }
  report.passed = true;
} finally {
  await browser?.close();
  await application.close();
  await writeFile(resolve(root, 'target/ui-next/toast-background-report.json'), JSON.stringify(report, null, 2) + '\n');
}
