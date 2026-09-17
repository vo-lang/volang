// Public browser fixtures for experimental UI applications. The runner owns the
// production server; applications use ordinary Playwright locators and actions.
import {toolchain} from './toolchain.mjs';
import {pathToFileURL} from 'node:url';
const {test:base,expect} = await import(pathToFileURL(toolchain.testModule).href);

export { expect };
export const test = base.extend({
  backend: ['vm', { option: true }],
  appURL: async ({ baseURL, backend }, use) => {
    if (!baseURL || !['vm'].includes(backend)) throw new Error('Run application tests through the UI test command.');
    const url = new URL(baseURL);
    url.searchParams.set('backend', backend);
    await use(url.href);
  },
  applicationErrors: [async ({ page, browser }, use, info) => {
    const errors = [];
    const record = error => errors.push(error.message);
    page.on('pageerror', record);
    info.annotations.push({ type: 'browser-version', description: browser.version() });
    try { await use(); }
    finally {
      page.off('pageerror', record);
      expect(errors, 'uncaught browser application errors').toEqual([]);
    }
  }, { auto: true }],
});
