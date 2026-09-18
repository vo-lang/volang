import assert from 'node:assert/strict';
import {resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {build} from './node_modules/esbuild/lib/main.js';
import {root} from './repository-paths.mjs';
import {readStudioDraft,waitStudioDraft} from './studio-draft-contracts.mjs';

export async function checkPersistentStorage(browser) {
  const bundle=await build({entryPoints:[resolve(root,'lang/crates/vo-web/js/ui_next/storage.ts')],bundle:true,write:false,format:'iife',globalName:'storageAdapter',platform:'browser'});
  const page=await browser.newPage(),errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  try {
    await page.route('http://storage.volang.test/**',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Storage contract</title>'}));
    await page.goto('http://storage.volang.test/');
    const load=()=>page.addScriptTag({content:bundle.outputFiles[0].text});await load();
    const checks=await page.evaluate(async()=>{
      const {createPersistentStorage}=storageAdapter;
      const store=createPersistentStorage('storage-contract');
      const check=(condition,message)=>{if(!condition)throw Error(message);};
      check(await store.get('draft')===null,'new key must be absent');
      await store.set('draft','Saved 中文 🌿');await store.set('empty','');
      check(await store.get('empty')==='','empty value must remain distinct from missing');
      check(await createPersistentStorage('other-contract').get('draft')===null,'named stores must be isolated');
      await store.set('replaced','first');await store.set('replaced','second');
      check(await store.get('replaced')==='second','committed replacement missing');
      const before=new AbortController();before.abort();
      await store.set('never','bad',before.signal).then(()=>{throw Error('accepted cancelled write');},()=>{});
      const opening=new AbortController(),pending=store.set('never','bad',opening.signal);opening.abort();
      await pending.then(()=>{throw Error('accepted cancelled open');},()=>{});
      const writing=new AbortController(),put=IDBObjectStore.prototype.put;
      IDBObjectStore.prototype.put=function(value,key){const request=put.call(this,value,key);writing.abort();return request;};
      try {await store.set('draft','bad',writing.signal).then(()=>{throw Error('accepted aborted transaction');},()=>{});}
      finally {IDBObjectStore.prototype.put=put;}
      check(await store.get('draft')==='Saved 中文 🌿'&&await store.get('never')===null,'aborted write escaped its transaction');
      await store.remove('empty');check(await store.get('empty')===null,'delete did not commit');
      return ['committed-unicode-and-empty-values','isolated-names','committed-replacement','cancel-open-and-write','committed-delete'];
    });
    await page.reload();await load();
    assert.equal(await page.evaluate(()=>storageAdapter.createPersistentStorage('storage-contract').get('draft')),'Saved 中文 🌿');
    await page.evaluate(async()=>{
      const drafts=storageAdapter.createPersistentStorage('volang.studio.next.drafts.v1');
      await drafts.set('delayed-empty','previous draft');
      window.delayedDraftWrite=new Promise(resolve=>setTimeout(resolve,250)).then(()=>drafts.set('delayed-empty',''));
    });
    await waitStudioDraft(page,'','delayed-empty');
    assert.equal(await readStudioDraft(page,'delayed-empty'),'','draft wait returned before the empty write committed');
    await page.evaluate(()=>window.delayedDraftWrite);
    assert.deepEqual(errors,[]);
    return [...checks,'reopen-committed-draft','wait-for-delayed-empty-draft'].map(name=>({name,passed:true}));
  } finally {await page.close();}
}

if(process.argv[1]&&import.meta.url===pathToFileURL(resolve(process.argv[1])).href) {
  process.env.PLAYWRIGHT_BROWSERS_PATH??=resolve(root,'target/playwright-browsers');
  const browsers=await import('../browser/node_modules/playwright/index.mjs');
  for(const engine of ['chromium','firefox','webkit']) {
    const browser=await browsers[engine].launch({headless:true});
    try {console.log(engine,await checkPersistentStorage(browser));}finally{await browser.close();}
  }
}
