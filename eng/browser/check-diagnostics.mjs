#!/usr/bin/env node
import { spawnSync } from 'node:child_process';
import { readFileSync, writeFileSync, mkdirSync, readdirSync, statSync } from 'node:fs';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import assert from 'node:assert/strict';
const root = resolve(dirname(fileURLToPath(import.meta.url)), '../..');
const inputs = JSON.parse(readFileSync(process.argv[2], 'utf8'));
const coverage = JSON.parse(readFileSync(new URL('./coverage.json', import.meta.url), 'utf8'));
const output = resolve(process.argv[3] ?? join(root, 'target/ci/browser-diagnostics-proof'));
mkdirSync(output, { recursive: true });
writeFileSync(join(output, 'result.json'), JSON.stringify({ schema: 'volang.browser-diagnostics.v1', passed: false, state: 'running' }) + '\n');
const results = [];
for (const scenario of coverage.scenarios) {
  const directory = join(root, 'target/ci/browser', scenario.flag);
  mkdirSync(directory, { recursive: true });
  const before = new Set(readdirSync(directory, { withFileTypes: true }).map(entry => entry.name));
  const report = join(output, `${scenario.flag}.json`);
  const source = inputs[scenario.flag];
  assert(source, `missing artifact for ${scenario.flag}`);
  const args = scenario.flag === 'ui-conformance'
    ? ['--project', source, '--global', '__volangUiBrowserSmoke']
    : ['--static-root', source, '--' + scenario.flag.replace(/Smoke$/, '').replace(/[A-Z]/g, char => '-' + char.toLowerCase()) + '-smoke'];
  const child = spawnSync(process.execPath, [join(root, 'eng/run-browser-smoke.mjs'), ...args, '--output', report], {
    cwd: root, env: { ...process.env, VO_BROWSER_INJECT_FAILURE: scenario.flag }, encoding: 'utf8', timeout: 120_000, maxBuffer: 4 * 1024 * 1024,
  });
  writeFileSync(join(output, `${scenario.flag}.log`), `${child.stdout ?? ''}\n${child.stderr ?? ''}`);
  assert.equal(child.status, 1, `${scenario.flag}: expected failed subprocess`);
  const domain = JSON.parse(readFileSync(report, 'utf8'));
  assert.equal(domain.passed, false);
  assert.match(domain.error, /controlled diagnostic failure/);
  const attempts = readdirSync(directory).filter(name => !before.has(name));
  assert.equal(attempts.length, 1, 'diagnostic probe must produce one isolated attempt');
  const attempt = join(directory, attempts[0]);
  const reportData = JSON.parse(readFileSync(join(attempt, 'playwright.json'), 'utf8'));
  assert.equal(reportData.stats.unexpected, 1);
  assert.equal(reportData.stats.flaky, 0);
  const files = [];
  function walk(path) {
    for (const name of readdirSync(path)) {
      const file = join(path, name); const info = statSync(file);
      if (info.isDirectory()) walk(file); else if (info.size) files.push(file);
    }
  }
  walk(join(attempt, 'artifacts'));
  assert(files.some(file => file.endsWith('/trace.zip')), 'failure trace missing');
  assert(files.some(file => file.endsWith('.png')), 'failure screenshot missing');
  results.push({ scenario: scenario.flag, passed: true, attempt });
  console.log(`Verified failure diagnostics: ${scenario.flag}`);
}
writeFileSync(join(output, 'result.json'), JSON.stringify({ schema: 'volang.browser-diagnostics.v1', passed: true, scenarios: results }, null, 2) + '\n');
