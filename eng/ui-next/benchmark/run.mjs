import assert from 'node:assert/strict';
import { readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { cpus, platform, release, arch } from 'node:os';
import { serve, root } from '../server.mjs';
import {distribution} from './statistics.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const { chromium } = await import('../../browser/node_modules/playwright/index.mjs');
const rounds = Number(process.env.UI_BENCH_ROUNDS ?? 3);
const samples = Number(process.env.UI_BENCH_SAMPLES ?? 20);
assert(Number.isSafeInteger(rounds) && rounds > 0 && Number.isSafeInteger(samples) && samples > 0);
const directory = resolve(root, 'target/ui-next/benchmark');
const build = JSON.parse(await readFile(`${directory}/build.json`, 'utf8'));
const server = await serve();
let browser;
const report = { schema: 'volang.ui-next-comparison.v1', passed: false, measuredAt: new Date().toISOString(),
  environment: { cpu: cpus()[0].model, cores: cpus().length, platform: platform(), release: release(), arch: arch(), node: process.version },
  method: { rows: 1000, rounds, samplesPerScenarioPerRound: samples, warmups: 3,
    latency: 'native event dispatch to matching DOM plus forced layout; excludes paint; MutationObserver records included in timing',
    load: 'navigation start to all 1,000 rows plus forced layout in a fresh browser context; localhost, no-store, uncompressed HTTP; process/OS/Wasm code caches may be warm',
    build: 'minified production bundles, browser target es2022; Vo compiled with default CLI options; gzip level 9 potential, not observed HTTP transfer',
    limits: ['single machine and Chromium engine', 'synthetic dispatch bypasses trusted-input overhead', 'p99 is descriptive for a small sample', 'no network throttling or mobile CPU simulation', 'memory domains differ; no VM collector counters', 'microbenchmark; does not establish overall framework maturity'] },
  build, runs: [] };

async function memory(page, cdp) {
  const raw = await cdp.send('Performance.getMetrics');
  const metrics = Object.fromEntries(raw.metrics.map(metric => [metric.name, metric.value]));
  return { jsHeapUsedBytes: metrics.JSHeapUsedSize, domNodes: metrics.Nodes, documents: metrics.Documents,
    guest: await page.evaluate(() => window.__benchmark.guestMemory?.() ?? null) };
}

// Execute all drivers through the same native controls. Every observation also
// checks retained DOM identity and exact values; timing cannot pass wrong output.
async function scenario(page, name, count) {
  return page.evaluate(async ({ name, count }) => {
    const results = [];
    for (let index = 0; index < count; index++) {
      await new Promise(requestAnimationFrame);
      const list = document.querySelector('ul');
      const retained = document.querySelector('[data-row="500"]');
      let action, matches;
      if (name === 'increment') {
        const button = retained.querySelector('button'), value = Number(button.textContent) + 1;
        action = () => button.click();
        matches = () => button.textContent === String(value);
      } else if (name === 'bulk100') {
        const targets = Array.from(list.children).filter(row => Number(row.dataset.row) % 10 === 0)
          .map(row => ({ button: row.querySelector('button'), value: Number(row.querySelector('button').textContent) + 1 }));
        const untouched = list.querySelector('[data-row="501"] button');
        const before = untouched.textContent;
        action = () => document.querySelector('#update').click();
        matches = () => untouched.textContent === before && targets.every(({ button, value }) => button.textContent === String(value));
      } else if (name === 'reverse1000') {
        const first = list.firstElementChild;
        const last = list.lastElementChild;
        action = () => document.querySelector('#reverse').click();
        matches = () => list.firstElementChild === last && list.lastElementChild === first;
      } else {
        const input = document.querySelector('#filter');
        const value = name === 'filter1000to19' ? '99' : '';
        action = () => { input.value = value; input.dispatchEvent(new Event('input', { bubbles: true })); };
        matches = () => list.children.length === (value ? 19 : 1000);
      }
      const metrics = { ...window.__benchmark.metrics };
      const result = await new Promise((resolve, reject) => {
        let changes = 0, additions = 0, removals = 0;
        const timer = setTimeout(() => { observer.disconnect(); reject(new Error(`timeout: ${name}; ${window.__benchmark.error}`)); }, 30000);
        const observer = new MutationObserver(records => {
          for (const record of records) { changes++; additions += record.addedNodes.length; removals += record.removedNodes.length; }
          if (!matches()) return;
          if (!name.startsWith('filter') && !name.startsWith('restore') && retained !== document.querySelector('[data-row="500"]')) {
            clearTimeout(timer); observer.disconnect(); reject(new Error('retained row was replaced')); return;
          }
          list.getBoundingClientRect();
          const ms = performance.now() - started;
          clearTimeout(timer); observer.disconnect();
          resolve({ ms, domRecords: changes, addedNodes: additions, removedNodes: removals,
            host: Object.fromEntries(Object.entries(metrics).map(([key, value]) => [key, window.__benchmark.metrics[key] - value])) });
        });
        observer.observe(document.getElementById('root'), { childList: true, characterData: true, attributes: true, subtree: true });
        const started = performance.now();
        action();
      });
      results.push(result);
    }
    return results;
  }, { name, count });
}

try {
  browser = await chromium.launch({ headless: true });
  report.environment.browser = browser.version();
  const drivers = ['vue', 'svelte', 'vm'];
  for (let round = 0; round < rounds; round++) {
    const order = [...drivers.slice(round % drivers.length), ...drivers.slice(0, round % drivers.length)];
    for (const driver of order) {
      const context = await browser.newContext({ viewport: { width: 1280, height: 800 } });
      try {
        const page = await context.newPage();
        const errors = [];
        page.on('pageerror', error => errors.push(String(error)));
        const cdp = await context.newCDPSession(page);
        await cdp.send('Performance.enable');
        await page.goto(`${server.url}/artifacts/benchmark/${driver}/index.html`);
        await page.waitForFunction(() => window.__benchmark?.loadedAt && window.__benchmark.close || window.__benchmark?.error, null, { timeout: 60000 });
        assert.equal(await page.evaluate(() => window.__benchmark.error), null);
        assert.equal(await page.locator('[data-row]').count(), 1000);
        const run = { driver, round, loadMs: await page.evaluate(() => window.__benchmark.loadedAt),
          initialMemory: await memory(page, cdp), scenarios: {} };
        for (const name of ['increment', 'bulk100', 'reverse1000']) {
          await scenario(page, name, 3);
          run.scenarios[name] = await scenario(page, name, samples);
        }
        for (let i = 0; i < samples + 3; i++) {
          for (const name of ['filter1000to19', 'restore19to1000']) {
            const result = await scenario(page, name, 1);
            if (i >= 3) (run.scenarios[name] ??= []).push(...result);
          }
        }
        run.afterUpdatesMemory = await memory(page, cdp);
        run.totalHost = await page.evaluate(() => window.__benchmark.metrics);
        await page.evaluate(async () => { await window.__benchmark.close(); });
        assert.equal(await page.locator('#root').textContent(), '');
        await cdp.send('HeapProfiler.collectGarbage');
        run.afterCloseAndGcMemory = await memory(page, cdp);
        assert.deepEqual(errors, []);
        report.runs.push(run);
        console.log(`${driver} round ${round + 1}: mount ${run.loadMs.toFixed(1)} ms; ` +
          Object.entries(run.scenarios).map(([name, results]) => `${name} ${distribution(results.map(result => result.ms)).p50.toFixed(2)} ms`).join(', '));
        await writeFile(`${directory}/report.partial.json`, JSON.stringify(report, null, 2) + '\n');
      } finally { await context.close(); }
    }
  }
  report.summary = {};
  for (const driver of drivers) {
    const runs = report.runs.filter(run => run.driver === driver);
    const scenarios = {};
    for (const name of Object.keys(runs[0].scenarios)) {
      const results = runs.flatMap(run => run.scenarios[name]);
      scenarios[name] = { ms: distribution(results.map(result => result.ms)), domRecords: distribution(results.map(result => result.domRecords)),
        guestBytes: distribution(results.map(result => result.host.guestBytes)), batches: distribution(results.map(result => result.host.batches)), hostMs: distribution(results.map(result => result.host.hostMs)) };
    }
    const files = build.outputs.filter(output => output.driver === driver);
    report.summary[driver] = { loadMs: distribution(runs.map(run => run.loadMs)), scenarios,
      bytes: files.reduce((sum, file) => sum + file.bytes, 0), gzipBytes: files.reduce((sum, file) => sum + file.gzipBytes, 0) };
  }
  report.passed = true;
} finally {
  await browser?.close();
  await server.close();
  await writeFile(`${directory}/report.json`, JSON.stringify(report, null, 2) + '\n');
}
