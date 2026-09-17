import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { serve, root } from './server.mjs';
import { checkStudioDocuments, documentContracts } from './studio-documents-contracts.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const browsers = await import('../browser/node_modules/playwright/index.mjs');
const server = await serve(), reports = [];
let browser;
try {
  for (const engine of process.env.UI_NEXT_BROWSER ? [process.env.UI_NEXT_BROWSER] : ['chromium', 'firefox', 'webkit']) {
    browser = await browsers[engine].launch({ headless: true });
    for (const report of await checkStudioDocuments(browser, server.url)) reports.push({ engine, browserVersion: browser.version(), ...report });
    await browser.close(); browser = undefined;
    console.log(`${engine}: maintained documents, cache, SSR and hydration passed`);
  }
  await writeFile(resolve(root, 'target/ui-next/studio-documents-report.json'), JSON.stringify({
    passed: true, build: JSON.parse(await readFile(resolve(root, 'target/ui-next/build-report.json'), 'utf8')),
    reports, contracts: documentContracts,
  }, null, 2) + '\n');
} finally { await browser?.close(); await server.close(); }
