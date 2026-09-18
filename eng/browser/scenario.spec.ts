import { test as base, expect } from '@playwright/test';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { basename, dirname } from 'node:path';
import { prepareApplication } from './server.mjs';
import { deployedAssetRecords, verifyDeployedArtifact } from './deployed-artifact.mjs';
import { PageContract, pollEvaluation } from './page-contract.mjs';

const request = process.env.VO_BROWSER_REQUEST
  ? JSON.parse(await readFile(process.env.VO_BROWSER_REQUEST, 'utf8')) : null;
if (!request) throw new Error('use eng/run-browser-smoke.mjs to select a scenario and its built artifact');
const test = base.extend<{ applicationURL: string }>({
  applicationURL: async ({}, use) => {
    const application = await prepareApplication(request);
    try { await use(application.url); } finally { await application.close(); }
  },
});

test(request.scenario, async ({ page, context, browser, applicationURL }, testInfo) => {
  const diagnostics: object[] = [];
  page.on('console', msg => diagnostics.push({ type: 'console', level: msg.type(), text: msg.text() }));
  page.on('pageerror', error => diagnostics.push({ type: 'pageerror', error: error.stack }));
  page.on('requestfailed', req => diagnostics.push({ type: 'requestfailed', url: req.url(), error: req.failure() }));
  await context.grantPermissions(['clipboard-read', 'clipboard-write'], { origin: new URL(applicationURL).origin });
  const contract = new PageContract(page);
  let result: any;
  try {
    if (request.expectedArtifact) {
      const assets = await deployedAssetRecords(request.expectedArtifact);
      const verification = await verifyDeployedArtifact(applicationURL, assets);
      await testInfo.attach('deployed-artifact', { body: JSON.stringify({ ...verification, assets }, null, 2), contentType: 'application/json' });
    }
    await page.goto(applicationURL, {waitUntil:'load'});
    if (process.env.VO_BROWSER_INJECT_FAILURE === request.scenario) {
      expect(false, `controlled diagnostic failure: ${request.scenario}`).toBe(true);
    }
    if (request.button) await page.locator(`[id=${JSON.stringify(request.button)}]`).click();
    const report = await pollEvaluation(contract, `window[${JSON.stringify(request.global)}] ?? null`, value => value?.complete === true, request.timeout);
    expect(report.passed).toBe(true);
    result = { schema: 'volang.browser-result.v1', passed: true, project: basename(request.projectRoot), scenario: request.scenario, browser: browser.version(), report };
    await testInfo.attach('domain-result', { body: JSON.stringify(result, null, 2), contentType: 'application/json' });
  } catch (error) {
    result = { schema: 'volang.browser-result.v1', passed: false, scenario: request.scenario, browser: browser.version(), error: String(error), report: { complete: false, passed: false } };
    throw error;
  } finally {
    await testInfo.attach('browser-diagnostics', { body: JSON.stringify(diagnostics, null, 2), contentType: 'application/json' });
    if (result) {
      await mkdir(dirname(request.output), { recursive: true });
      await writeFile(request.output, JSON.stringify(result, null, 2) + '\n');
    }
  }
});
