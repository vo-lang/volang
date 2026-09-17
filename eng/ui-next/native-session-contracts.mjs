import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {mkdir, readFile, rm, writeFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {resolve} from 'node:path';
import {serve, root} from './server.mjs';

const directory = resolve(root, 'target/ui-next/native-session');
await mkdir(directory, {recursive: true});
await rm(resolve(directory, 'report.json'), {force: true});
const {limits: {frameBytes: maxFrameBytes}} = JSON.parse(await readFile(resolve(root, 'ui/next/wire.schema.json'), 'utf8'));
process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const browsers = await import('../browser/node_modules/playwright/index.mjs');
const native = resolve(directory, process.platform === 'win32' ? 'exchange.exe' : 'exchange');
const artifact = resolve(root, 'target/ui-next/interaction.vob');
const drivers = [['vm', native, ['vm', artifact]], ['jit', native, ['jit', artifact]]];
if (process.env.UI_NEXT_NATIVE_AOT) drivers.push(['aot', resolve(process.env.UI_NEXT_NATIVE_AOT), []]);
const digest = async path => createHash('sha256').update(await readFile(path)).digest('hex');
const report = {schema: 'volang.ui-next-native-session.v1', passed: false,
  platform: process.platform, arch: process.arch, node: process.version, browserVersions: {},
  artifact: {path: artifact, sha256: await digest(artifact)}, drivers: [], cases: []};
for (const [backend, path] of drivers) report.drivers.push({backend, path, sha256: await digest(path)});
await writeFile(resolve(directory, 'report.json'), JSON.stringify(report, null, 2) + '\n');

function processTransport(path, args) {
  const child = spawn(path, args, {cwd: root, env: {...process.env, VOWORK: 'off'}, stdio: ['pipe', 'pipe', 'pipe']});
  let diagnostics = '';
  child.stderr.on('data', data => { diagnostics += data; });
  const closed = new Promise((accept, reject) => {
    child.once('error', reject);
    child.once('close', (code, signal) => code === 0 ? accept() : reject(new Error(`native session failed (${signal ?? code}): ${diagnostics}`)));
  });
  closed.catch(() => {});
  const iterator = child.stdout[Symbol.asyncIterator]();
  let buffered = Buffer.alloc(0);
  async function read(length, eof = false) {
    const parts = []; let remaining = length;
    while (remaining) {
      if (!buffered.length) {
        const next = await iterator.next();
        if (next.done) {
          await closed;
          if (eof && remaining === length) return null;
          throw new Error('truncated native frame');
        }
        buffered = next.value;
      }
      const count = Math.min(remaining, buffered.length);
      parts.push(buffered.subarray(0, count)); buffered = buffered.subarray(count); remaining -= count;
    }
    return Buffer.concat(parts, length);
  }
  return {
    async next() {
      const prefix = await read(4, true);
      if (!prefix) return null;
      const length = prefix.readUInt32LE();
      assert(length <= maxFrameBytes, 'native frame exceeds wire bounds');
      return [...await read(length)];
    },
    async reply(bytes) {
      assert(bytes.length <= maxFrameBytes);
      const prefix = Buffer.alloc(4); prefix.writeUInt32LE(bytes.length);
      await new Promise((accept, reject) => child.stdin.write(Buffer.concat([prefix, Buffer.from(bytes)]), error => error ? reject(error) : accept()));
    },
    async finished() { await closed; return diagnostics; },
    stop() { child.kill(); },
  };
}

const server = await serve();
try {
  for (const engine of ['chromium', 'firefox', 'webkit']) {
    const browser = await browsers[engine].launch({headless: true});
    report.browserVersions[engine] = browser.version();
    try {
      for (const [backend, path, args] of drivers) {
        console.log(`Native UI session: ${engine}/${backend}`);
        const process = processTransport(path, args);
        const page = await browser.newPage();
        try {
          await page.exposeFunction('nativeNext', () => process.next());
          await page.exposeFunction('nativeReply', bytes => process.reply(bytes));
          await page.route('**/native-session', route => route.fulfill({contentType: 'text/html', body: '<!doctype html><meta charset="utf-8"><title>Native session</title><div id="root"></div>'}));
          await page.goto(`${server.url}/native-session`);
          await page.evaluate(async () => {
            const {createUiTransport} = await import('/host/ui_next/host.js');
            const host = createUiTransport(document.getElementById('root'));
            const state = window.nativeSession = {ready: false, error: null, close: () => host.close()};
            host.ready.then(value => { state.ready = value; });
            state.done = (async () => {
              for (;;) {
                const bytes = await window.nativeNext();
                if (bytes === null) return;
                await window.nativeReply([...await host.exchange(Uint8Array.from(bytes))]);
              }
            })().catch(error => { state.error = String(error); });
          });
          await page.waitForFunction(() => window.nativeSession.ready || window.nativeSession.error);
          assert.equal(await page.evaluate(() => window.nativeSession.error), null);
          assert.equal(await page.locator('[data-lifecycle]').textContent(), 'Ready to explore');
          report.cases.push({engine, backend, name: 'initial-commit-and-effects', passed: true});

          await page.evaluate(() => { for (let i = 0; i < 12; i++) document.querySelector('[data-counter="Alpha"]').click(); });
          await page.waitForFunction(() => document.querySelector('[data-counter="Alpha"]').textContent === 'Alpha: 12');
          report.cases.push({engine, backend, name: 'continuous-native-input', passed: true});

          await page.evaluate(() => {
            const input = document.getElementById('name');
            input.value = '中文 🌱 <Volang>'; input.dispatchEvent(new InputEvent('input', {bubbles: true}));
            input.form.requestSubmit();
          });
          await page.waitForFunction(() => document.querySelector('[data-submitted]').textContent === '中文 🌱 <Volang>');
          report.cases.push({engine, backend, name: 'unicode-immediate-submit', passed: true});

          await page.evaluate(() => { window.retainedCounter = document.querySelector('[data-counter="Alpha"]'); });
          await page.locator('[data-reverse]').click();
          await page.waitForFunction(() => document.querySelector('[data-counters] button').dataset.counter === 'Beta');
          assert(await page.evaluate(() => window.retainedCounter === document.querySelector('[data-counter="Alpha"]')));
          assert.equal(await page.locator('[data-counter="Alpha"]').textContent(), 'Alpha: 12');
          report.cases.push({engine, backend, name: 'keyed-identity-and-state', passed: true});

          await page.locator('[data-counter="Beta"]').click();
          await page.waitForFunction(() => document.querySelector('[data-counter="Beta"]').textContent === 'Beta: 1');
          await page.locator('[data-toggle]').click();
          await page.waitForFunction(() => !document.querySelector('[data-counter="Beta"]'));
          await page.locator('[data-toggle]').click();
          await page.waitForFunction(() => document.querySelector('[data-counter="Beta"]')?.textContent === 'Beta: 0');
          report.cases.push({engine, backend, name: 'scope-disposal-and-remount', passed: true});

          await page.locator('[data-break]').click();
          await page.waitForFunction(() => document.querySelector('[data-recover]'));
          assert.equal(await page.locator('[data-counter="Alpha"]').textContent(), 'Alpha: 12');
          assert.equal(await page.locator('#name').inputValue(), '中文 🌱 <Volang>');
          await page.locator('[data-recover]').click();
          await page.waitForFunction(() => document.querySelector('[data-break]'));
          report.cases.push({engine, backend, name: 'render-failure-and-retry', passed: true});

          await page.evaluate(async () => { window.nativeSession.close(); await window.nativeSession.done; });
          assert.equal(await page.evaluate(() => window.nativeSession.error), null);
          assert.equal(await page.locator('#root').textContent(), '');
          const diagnostics = await process.finished();
          const stats = /native-ui-stats: entries=(\d+) continuations=(\d+) compilations=(\d+)/.exec(diagnostics);
          assert(stats, diagnostics);
          if (backend !== 'vm') assert(Number(stats[1]) > 0, 'native code was never entered');
          if (backend === 'aot') {
            assert(Number(stats[2]) > 0, 'AOT continuations were never entered');
            assert.equal(Number(stats[3]), 0, 'AOT fixture compiled JIT code');
          }
          report.cases.push({engine, backend, name: 'orderly-close', passed: true, entries: Number(stats[1]), continuations: Number(stats[2]), compilations: Number(stats[3])});
        } finally { process.stop(); await page.close(); }
      }
    } finally { await browser.close(); }
  }
  report.passed = true;
} finally {
  await server.close();
  await writeFile(resolve(directory, 'report.json'), JSON.stringify(report, null, 2) + '\n');
}
console.log(`Native UI session: ${report.cases.length} browser contracts passed`);
