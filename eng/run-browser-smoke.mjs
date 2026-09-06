#!/usr/bin/env node
// Compatibility entry point. Playwright owns browser lifecycle, waiting and reports.
import { spawn } from 'node:child_process';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { parseArguments } from './browser/arguments.mjs';
import { prepareApplication } from './browser/server.mjs';

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const options = parseArguments(process.argv.slice(2));
options.projectRoot = resolve(options.staticRoot ?? options.project);
options.bundleEntry = options.bundleEntry === null ? null : resolve(options.bundleEntry);
const scenario = Object.entries(options).find(([key, value]) => key.endsWith('Smoke') && value)?.[0] ?? 'ui-conformance';
options.scenario = scenario;
const attempt = join(root, 'target/ci/browser', scenario, `${Date.now()}-${process.pid}`);
await mkdir(attempt, { recursive: true });
options.output = options.output === null ? join(attempt, 'result.json') : resolve(options.output);
options.diagnostics = attempt;
if (options.serveOnly) {
  const application = await prepareApplication(options);
  process.stdout.write(JSON.stringify({ ready: true, url: application.url, project: options.projectRoot }) + '\n');
  await new Promise(done => { process.once('SIGINT', done); process.once('SIGTERM', done); });
  await application.close();
} else {
  await mkdir(dirname(options.output), { recursive: true });
  await writeFile(options.output, JSON.stringify({ schema: 'volang.browser-result.v1', passed: false, scenario, state: 'running' }) + '\n');
  const request = join(attempt, 'request.json');
  await writeFile(request, JSON.stringify(options, null, 2) + '\n');
  const child = spawn(process.execPath, [
    join(root, 'eng/browser/node_modules/@playwright/test/cli.js'),
    'test', '--config', join(root, 'eng/browser/playwright.config.ts'),
  ], {
    cwd: root,
    env: {
      ...process.env,
      VO_BROWSER_REQUEST: request,
      PLAYWRIGHT_BROWSERS_PATH: process.env.PLAYWRIGHT_BROWSERS_PATH ?? join(root, 'target/playwright-browsers'),
    },
    stdio: 'inherit',
  });
  for (const signal of ['SIGINT', 'SIGTERM']) process.once(signal, () => child.kill(signal));
  const status = await new Promise((done, reject) => {
    child.once('error', reject);
    child.once('exit', (code, signal) => done(signal === null ? code : 1));
  });
  let result = JSON.parse(await readFile(options.output, 'utf8'));
  if (result.state === 'running') {
    result = { schema: 'volang.browser-result.v1', passed: false, scenario, state: 'failed',
      error: `Playwright exited before producing a domain result (exit ${status})`,
      report: { complete: false, passed: false }, diagnostics: attempt };
    await writeFile(options.output, JSON.stringify(result, null, 2) + '\n');
  }
  process.stdout.write(JSON.stringify(result) + '\n');
  if (status !== 0 || result.passed !== true) process.exitCode = Number(status) || 1;
}
