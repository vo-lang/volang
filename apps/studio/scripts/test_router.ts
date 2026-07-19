import assert from 'node:assert/strict';

import { get } from 'svelte/store';

class MockBrowserWindow extends EventTarget {
  location = new URL('https://studio.test/#/develop');
  pushCalls: string[] = [];
  replaceCalls: string[] = [];

  history = {
    pushState: (_state: unknown, _unused: string, url?: string | URL | null) => {
      assert.ok(url);
      this.location = new URL(String(url), this.location.href);
      this.pushCalls.push(this.location.href);
    },
    replaceState: (_state: unknown, _unused: string, url?: string | URL | null) => {
      assert.ok(url);
      this.location = new URL(String(url), this.location.href);
      this.replaceCalls.push(this.location.href);
    },
  };
}

const browser = new MockBrowserWindow();
Object.defineProperty(globalThis, 'window', {
  configurable: true,
  value: browser,
});

const {
  onBrowserHistoryNavigation,
  resolveStartupLaunch,
  route,
  setDocsHash,
  setExampleHash,
  setModeHash,
} = await import('../src/lib/router.ts');

setExampleHash('channels');
assert.equal(browser.replaceCalls.length, 1, 'the first example replaces the bare develop route');
assert.equal(browser.pushCalls.length, 0);
assert.equal(browser.location.hash, '#/develop?example=channels');

setExampleHash('time');
assert.equal(browser.pushCalls.length, 1, 'a user-visible example switch adds history');
assert.equal(browser.location.hash, '#/develop?example=time');

setModeHash('manage');
setModeHash('docs');
const pushesBeforeFirstDoc = browser.pushCalls.length;
setDocsHash('getting-started/introduction.md');
assert.equal(browser.replaceCalls.length, 2, 'the first document replaces the bare docs route');
assert.equal(browser.pushCalls.length, pushesBeforeFirstDoc);
setDocsHash('advanced/modules.md');
assert.equal(browser.pushCalls.length, pushesBeforeFirstDoc + 1, 'document changes add history');

let historyNotifications = 0;
const unsubscribe = onBrowserHistoryNavigation(() => {
  historyNotifications++;
});
browser.location = new URL('https://studio.test/#/develop?example=channels');
browser.dispatchEvent(new Event('popstate'));
assert.equal(historyNotifications, 1);
assert.equal(get(route).exampleId, 'channels');
browser.dispatchEvent(new Event('hashchange'));
assert.equal(historyNotifications, 1, 'popstate/hashchange for one URL is deduplicated');
setExampleHash('time');
assert.equal(historyNotifications, 1, 'programmatic navigation is not reported as browser history');
unsubscribe();

const bootstrapLaunch = { proj: '/cli/project', mode: 'runner' } as const;
assert.deepEqual(resolveStartupLaunch({
  bootstrapLaunch,
  bootstrapMode: 'runner',
  hash: '',
  search: '',
  browserUrlWasSynchronized: false,
}), bootstrapLaunch);
assert.deepEqual(resolveStartupLaunch({
  bootstrapLaunch,
  bootstrapMode: 'runner',
  hash: '#/',
  search: '',
  browserUrlWasSynchronized: false,
}), { proj: null, mode: 'dev' });
assert.deepEqual(resolveStartupLaunch({
  bootstrapLaunch,
  bootstrapMode: 'runner',
  hash: '',
  search: '',
  browserUrlWasSynchronized: true,
}), { proj: null, mode: 'dev' });
assert.deepEqual(resolveStartupLaunch({
  bootstrapLaunch,
  bootstrapMode: 'dev',
  hash: '#/',
  search: '?mode=runner&proj=%2Fcurrent%2Fgame.vo',
  browserUrlWasSynchronized: true,
}), { proj: '/current/game.vo', mode: 'runner' });

console.log('studio router contracts: ok');
