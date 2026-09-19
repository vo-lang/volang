import test from 'node:test';
import assert from 'node:assert/strict';
import {gzipSync} from 'node:zlib';
import {loadStudioRuntime} from '../../apps/studio/next/startup.js';

test('Wasm and bytecode requests start before delayed bindings resolve', async () => {
  let finish;const bindings=new Promise(resolve=>{finish=resolve;});
  const urls=[];let initialized=false;
  const pending=loadStudioRuntime('https://studio.test/artifacts/studio.vob',{
    loadRuntime:()=>bindings,
    request:async url=>{urls.push(String(url));return new Response(new Uint8Array([1,2,3]));},
  });
  assert.deepEqual(urls,['/wasm/vo_web_bg.wasm','https://studio.test/artifacts/studio.vob']);
  finish({default:async ({module_or_path})=>{assert(module_or_path instanceof Response);initialized=true;}});
  assert.deepEqual((await pending).bytes,new Uint8Array([1,2,3]));assert(initialized);
});

for(const decoded of [false,true])test(`explicit gzip bytecode works with host decoding ${decoded}`,async()=>{
  const source=new Uint8Array([86,79,0,1,2,3]);const urls=[];
  const result=await loadStudioRuntime('https://studio.test/artifacts/studio.vob?v=1',{
    compressed:true,loadRuntime:async()=>({default:async()=>{}}),
    request:async url=>{urls.push(String(url));return new Response(String(url).includes('.vob') && !decoded?gzipSync(source):source);},
  });
  assert.equal(urls[1],'https://studio.test/artifacts/studio.vob.gz?v=1');assert.deepEqual(result.bytes,source);
});

test('failed startup cancels other outstanding downloads',async()=>{
  let signal;
  await assert.rejects(loadStudioRuntime('https://studio.test/studio.vob',{
    loadRuntime:async()=>({default:async()=>{}}),
    request:async(url,options)=>{signal=options.signal;return new Response('',{status:503});},
  }),/503/);
  assert(signal.aborted);
});
