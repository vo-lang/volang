import assert from 'node:assert/strict';
import { cp, mkdir, mkdtemp, readFile, readdir, rm, writeFile } from 'node:fs/promises';
import { join, resolve } from 'node:path';
import { createProject } from './project.mjs';
import { testProject } from './project-testing.mjs';
import { root } from './server.mjs';

const temporary = await mkdtemp(resolve(root, 'target/ui-next/project-test-contract-'));
const evidence = resolve(root, 'target/ui-next/project-testing');
await mkdir(evidence, { recursive: true });
try {
  const directory = await createProject(join(temporary, 'An idea with tests 中文'));
  const passed = await testProject(directory, { browsers: ['chromium', 'firefox', 'webkit'] });
  const report = JSON.parse(await readFile(join(passed.output, 'report.json')));
  assert.equal(report.stats.expected, 6);
  assert.equal(report.stats.unexpected, 0);
  assert.equal(report.stats.skipped, 0);
  await cp(passed.output, join(evidence, 'passed'), { recursive: true });
  const pages = await createProject(join(temporary, 'Separate pages 中文'), {template:'pages'});
  const pageTests = await testProject(pages, {browsers:['chromium', 'firefox', 'webkit']});
  const pageReport = JSON.parse(await readFile(join(pageTests.output, 'report.json')));
  assert.equal(pageReport.stats.expected, 6);
  assert.equal(pageReport.stats.unexpected, 0);
  assert.equal(pageReport.stats.skipped, 0);
  await cp(pageTests.output, join(evidence, 'pages'), {recursive:true});
  const source = join(directory, 'tests/browser/app.test.mjs');
  await writeFile(source, `import { test, expect } from './fixtures.mjs';
test('assertion failure', async ({ page, appURL }) => {
  await page.goto(appURL);
  expect('actual value').toBe('intentional mismatch');
});
test('uncaught application error', async ({ page, appURL }) => {
  await page.goto(appURL);
  await Promise.all([
    page.waitForEvent('pageerror'),
    page.evaluate(() => { queueMicrotask(() => { throw new Error('intentional browser error'); }); }),
  ]);
});\n`);
  await assert.rejects(testProject(directory, { browsers: ['chromium'] }), error => {
    assert.match(error.message, /intentional mismatch/);
    assert.match(error.message, /intentional browser error/);
    assert.match(error.message, /Browser test artifacts:/);
    return true;
  });
  const runs = await readdir(join(directory, 'target/ui-next/browser-tests'));
  assert.equal(runs.length, 2, 'a failed run overwrote the successful run');
  const failed = runs.map(name => join(directory, 'target/ui-next/browser-tests', name)).find(path => path !== passed.output);
  const failedReport = JSON.parse(await readFile(join(failed, 'report.json')));
  assert.equal(failedReport.stats.unexpected, 4);
  const artifacts = await readdir(join(failed, 'results'), { recursive: true });
  assert(artifacts.some(path => path.endsWith('.png')), 'failure screenshot missing');
  assert(artifacts.some(path => path.endsWith('.zip')), 'failure trace missing');
  await cp(failed, join(evidence, 'intentional-failure'), { recursive: true });
  const stopped = new AbortController(); stopped.abort(new Error('test run cancelled'));
  await assert.rejects(testProject(directory, { signal: stopped.signal }), /test run cancelled/);
  await assert.rejects(testProject(directory, { browsers: ['unknown'] }), /chromium, firefox or webkit/);
  await rm(join(directory, 'tests'), { recursive: true });
  await assert.rejects(testProject(directory), /Add a tests\/browser/);
  await writeFile(join(evidence, 'report.json'), JSON.stringify({
    passed: true, browserBackendCases: 6, pageTemplateCases:6, expectedFailedCases: 4,
    contracts: ['standard-playwright-fixtures', 'production-build-identity', 'vm', 'three-browser-engines',
      'unicode-project-path', 'semantic-controls', 'assertion-fails-command', 'uncaught-browser-error-fails-command',
      'failure-screenshot-and-trace', 'independent-run-artifacts', 'cancellation', 'missing-tests-diagnostic'],
  }, null, 2) + '\n');
  console.log('Application testing contracts passed: twelve browser/backend runs and intentional failure diagnostics');
} finally { await rm(temporary, { recursive: true, force: true }); }
