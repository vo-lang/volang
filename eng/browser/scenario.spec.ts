import { test as base, expect } from '@playwright/test';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { basename, dirname } from 'node:path';
import { prepareApplication } from './server.mjs';
import { PageContract, pollEvaluation, waitForAotInteractive } from './page-contract.mjs';
import { runComponentStateSmoke } from './scenarios/component-state.mjs';
import { runUikitGallerySmoke } from './scenarios/uikit-gallery.mjs';
import { runDataApplicationSmoke } from './scenarios/data-application.mjs';
import { runContentSiteSmoke } from './scenarios/content-site.mjs';
import { runMediaApplicationSmoke } from './scenarios/media-application.mjs';
import { runStudioWorkbenchSmoke } from './scenarios/studio-workbench.mjs';
import { runStudioAotSmoke } from './scenarios/studio.mjs';
import { prepareStudioBootstrap, runStudioBootstrapSmoke } from './scenarios/studio-bootstrap.mjs';
import { runStudioLifecycleSmoke } from './scenarios/studio-lifecycle.mjs';
import { runStudioCanarySmoke } from './scenarios/studio-canary.mjs';

const request = process.env.VO_BROWSER_REQUEST
  ? JSON.parse(await readFile(process.env.VO_BROWSER_REQUEST, 'utf8')) : null;
if (!request) throw new Error('use eng/run-browser-smoke.mjs to select a scenario and its built artifact');
const coverage = JSON.parse(await readFile(new URL('./coverage.json', import.meta.url), 'utf8'))
  .scenarios.find((scenario: any) => scenario.flag === request.scenario);
if (!coverage) throw new Error(`unregistered browser scenario: ${request.scenario}`);

const test = base.extend<{ applicationURL: string }>({
  applicationURL: async ({}, use) => {
    const application = await prepareApplication(request);
    try { await use(application.url); } finally { await application.close(); }
  },
});

const scenarios = {
  componentStateSmoke: runComponentStateSmoke,
  uikitGallerySmoke: runUikitGallerySmoke,
  dataApplicationSmoke: runDataApplicationSmoke,
  contentSiteSmoke: runContentSiteSmoke,
  mediaApplicationSmoke: runMediaApplicationSmoke,
  studioWorkbenchSmoke: runStudioWorkbenchSmoke,
  studioAotSmoke: runStudioAotSmoke,
  studioCanarySmoke: runStudioCanarySmoke,
  studioBootstrapSmoke: runStudioBootstrapSmoke,
  studioLifecycleSmoke: runStudioLifecycleSmoke,
};

test(request.scenario, async ({ page, context, browser, applicationURL }, testInfo) => {
  const diagnostics: object[] = [];
  page.on('console', msg => diagnostics.push({ type: 'console', level: msg.type(), text: msg.text() }));
  page.on('pageerror', error => diagnostics.push({ type: 'pageerror', error: error.stack }));
  page.on('requestfailed', req => diagnostics.push({ type: 'requestfailed', url: req.url(), error: req.failure() }));
  await context.grantPermissions(['clipboard-read', 'clipboard-write'], { origin: new URL(applicationURL).origin });
  const contract = new PageContract(page);
  let result: any;
  try {
    if (request.studioBootstrapSmoke) await prepareStudioBootstrap(page);
    await page.goto(applicationURL, { waitUntil: 'load' });
    if (request.staticRoot !== null || request.baseURL) {
      const guard = await page.locator('#volang-root').evaluate(root => ({
        phase: (root as HTMLElement).dataset.volangActivation,
        inert: root.hasAttribute('inert'), busy: root.getAttribute('aria-busy'),
        bootHidden: (document.querySelector('#volang-boot') as HTMLElement)?.hidden,
      }));
      if (guard.phase !== 'ready') expect(guard).toMatchObject({ inert: true, busy: 'true', bootHidden: false });
      await waitForAotInteractive(contract, request.timeout);
    }
    if (process.env.VO_BROWSER_INJECT_FAILURE === request.scenario) {
      expect(false, `controlled diagnostic failure: ${request.scenario}`).toBe(true);
    }
    const scenario = Object.entries(scenarios).find(([flag]) => request[flag]);
    let report;
    if (scenario) report = await scenario[1](contract, request.timeout, request.projectRoot);
    else {
      if (request.button) await page.locator(`[id=${JSON.stringify(request.button)}]`).click();
      report = await pollEvaluation(contract, `window[${JSON.stringify(request.global)}] ?? null`, value => value?.complete === true, request.timeout);
    }
    expect(report.passed).toBe(true);
    if (coverage.checks) expect(report.checks).toEqual(coverage.checks);
    if (coverage.checkpoints) expect(Object.keys(report.checkpoints)).toEqual(expect.arrayContaining(coverage.checkpoints));
    if (coverage.added_checkpoints) expect(Object.keys(report.checkpoints)).toEqual(expect.arrayContaining(coverage.added_checkpoints));
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
