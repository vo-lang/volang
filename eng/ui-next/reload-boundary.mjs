import assert from 'node:assert/strict';

export async function checkReloadBoundary(page, url) {
  await page.route('**/__reload_boundary', route => route.fulfill({contentType:'text/html',body:'<!doctype html><body></body>'}));
  await page.goto(url + '/__reload_boundary');
  const result = await page.evaluate(async () => {
    const { createUiTransport } = await import('/host/ui_next/host.js');
    const { encodeBootstrap, decodeBootstrap } = await import('/host/ui_next/generated/codec.js');
    const require = (condition, message) => { if (!condition) throw new Error(message); };
    const root = document.body.appendChild(document.createElement('div'));
    let signal;
    const host = createUiTransport(root, false, { initialData: async value => {
      signal = value; return await new Promise(() => {});
    } });
    const pending = host.exchange(encodeBootstrap({data:''}));
    await Promise.resolve();
    host.close();
    require(signal.aborted, 'closing did not cancel the initial data provider');
    require((await pending).length === 0 && await host.ready === false, 'a provider ignoring abort retained its guest');
    const ready = createUiTransport(root, false, {initialData:async () => '中文 initial data'});
    require(decodeBootstrap(await ready.exchange(encodeBootstrap({data:''}))).data === '中文 initial data', 'asynchronous bootstrap changed data');
    ready.close(); root.remove();
    await import('/host/ui_next/development.js');
    const first = {label:'First'}, second = {label:'Second'};
    const status = detail => window.dispatchEvent(new CustomEvent('vo-ui-reload-status', {detail}));
    status({source:first,type:'error',message:'first failed'});
    status({source:second,type:'ready'});
    require(document.getElementById('ui-development-error')?.textContent.includes('first failed'), 'another root hid a failed reload');
    status({source:second,type:'error',message:'second failed'});
    status({source:first,type:'closed'});
    require(document.getElementById('ui-development-error')?.textContent.includes('second failed'), 'closing one root cleared another diagnostic');
    status({source:second,type:'ready'});
    require(!document.getElementById('ui-development-error'), 'recovery left a stale reload diagnostic');
    return {passed:true,contracts:['asynchronous-bootstrap','close-releases-ignored-abort','readiness-cancellation','independent-root-diagnostics']};
  });
  assert(result.passed);
  return result;
}
