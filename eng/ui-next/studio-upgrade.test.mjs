import assert from 'node:assert/strict';
import test from 'node:test';
import {readFile} from 'node:fs/promises';
import {runInNewContext} from 'node:vm';
import {updateLegacyRegistration} from '../../apps/studio/next/legacy-registration.js';

async function workerFixture(fail=false) {
  const events=new Map(),actions=[];
  runInNewContext(await readFile(new URL('../../apps/studio/next/legacy-worker.js',import.meta.url),'utf8'),{
    self:{addEventListener:(name,handler)=>events.set(name,handler),skipWaiting:async()=>actions.push('skip'),
      clients:{claim:async()=>actions.push('claim'),matchAll:async()=>[{postMessage:message=>actions.push(message.type)}]},
      registration:{unregister:async()=>actions.push('unregister')}},
    caches:{keys:async()=>['volang-ui-d7559a3ca207bb8c-old','another-application'],delete:async key=>{
      actions.push('delete:'+key);if(fail) throw new Error('cache unavailable');return true;
    }},
  });
  return {actions,run:async (name,data)=>{let pending;events.get(name)({data,waitUntil:value=>{pending=value;}});await pending;}};
}

test('retirement removes only the previous Studio asset namespace and unregisters',async()=>{
  const fixture=await workerFixture();await fixture.run('install');await fixture.run('activate');
  assert.deepEqual(fixture.actions,['skip','claim','delete:volang-ui-d7559a3ca207bb8c-old','unregister','volang.studio.retired']);
});

test('failed cache cleanup still retires the registration',async()=>{
  const fixture=await workerFixture(true);await assert.rejects(fixture.run('activate'),/cache unavailable/);
  assert.equal(fixture.actions.at(-1),'unregister');
});

test('an installed retirement worker accepts a fresh activation request',async()=>{
  const fixture=await workerFixture();
  await fixture.run('message',{type:'unrelated'});assert.deepEqual(fixture.actions,[]);
  await fixture.run('message',{type:'volang.studio.retire'});assert.deepEqual(fixture.actions,['skip']);
});

test('Studio updates the known root registration without registering another worker',async()=>{
  let updates=0;
  const origin='https://studio.test',registration={scope:origin+'/',active:{scriptURL:origin+'/service-worker.js'},update:async()=>{updates++;}};
  const window={location:{origin},navigator:{serviceWorker:{getRegistration:async()=>registration}}};
  await updateLegacyRegistration(window);assert.equal(updates,1);
  registration.scope=origin+'/another/';await updateLegacyRegistration(window);
  registration.scope=origin+'/';registration.active.scriptURL=origin+'/other-worker.js';await updateLegacyRegistration(window);
  await updateLegacyRegistration({navigator:{},location:{origin}});
  assert.equal(updates,1);
});

test('retirement requests activation after installation and releases its listener',async()=>{
  for (const initial of ['installing','installed','activated','redundant']) {
    const messages=[],listeners=new Set(),origin='https://studio.test';
    const worker={scriptURL:origin+'/service-worker.js',state:initial,postMessage:value=>messages.push(value),
      addEventListener:(name,listener)=>listeners.add(listener),removeEventListener:(name,listener)=>listeners.delete(listener)};
    const registration={scope:origin+'/',active:worker,update:async()=>{},[initial==='installing'?'installing':'waiting']:worker};
    await updateLegacyRegistration({location:{origin},navigator:{serviceWorker:{getRegistration:async()=>registration}}});
    if (initial==='installing') {assert.equal(messages.length,0);worker.state='installed';for (const listener of listeners) listener();}
    assert.deepEqual(messages,['installing','installed'].includes(initial)?[{type:'volang.studio.retire'}]:[]);
    assert.equal(listeners.size,0);
  }
});
