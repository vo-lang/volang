import assert from 'node:assert/strict';
import test from 'node:test';
import {fetchHTTP, fetchText, readResponseText, MAX_HTTP_RESPONSE_BYTES} from '../../lang/crates/vo-web/dist/ui_next/http.js';
import {TaskHost} from '../../lang/crates/vo-web/dist/ui_next/tasks.js';

const signal = () => new AbortController().signal;
const request = delta => JSON.stringify({version:1,url:'https://example.test/data',method:'GET',body:'',credentials:'same-origin',headers:[],...delta});

test('HTTP keeps error status, exposed headers and JSON body; text convenience keeps status errors', async t => {
  let captured;
  t.mock.method(globalThis, 'fetch', async (url, options) => {
    captured = {url, options};
    return new Response('{"message":"名字已使用"}', {status:422,headers:{'content-type':'application/json','x-result':'invalid'}});
  });
  const response = JSON.parse(await fetchHTTP(request({method:'PATCH',body:'{"name":"中文"}',credentials:'omit',headers:[{name:'content-type',value:'application/json'}]}), signal()));
  assert.equal(response.status,422); assert.equal(response.headers['x-result'],'invalid');
  assert.deepEqual(JSON.parse(response.body),{message:'名字已使用'});
  assert.equal(captured.options.method,'PATCH'); assert.equal(captured.options.credentials,'omit');
  assert.equal(captured.options.headers.get('content-type'),'application/json');
  assert.equal(captured.options.body,'{"name":"中文"}');
  await assert.rejects(fetchText('https://example.test/data',signal()),/HTTP 422/);
});

test('HTTP text uses Fetch UTF-8 replacement and accepts empty bodies and exact byte limits', async () => {
  const bytes = Buffer.from('\ufeff中文🙂'), chunks = [...bytes,255].map(byte => Uint8Array.of(byte));
  const response = new Response(new ReadableStream({pull(controller) { if(chunks.length) controller.enqueue(chunks.shift()); else controller.close(); }}));
  assert.equal(await readResponseText(response,signal()),'中文🙂�');
  assert.equal(await readResponseText(new Response(null,{status:204}),signal()),'');
  assert.equal((await readResponseText(new Response(Buffer.alloc(MAX_HTTP_RESPONSE_BYTES,120)),signal())).length,MAX_HTTP_RESPONSE_BYTES);
});

test('HTTP oversized streams cancel their reader and produce one local task failure', async t => {
  let cancelled = 0, pulls = 0;
  t.mock.method(globalThis,'fetch',async () => new Response(new ReadableStream({
    pull(controller) {pulls++; controller.enqueue(new Uint8Array(MAX_HTTP_RESPONSE_BYTES / 2));},
    cancel() {cancelled++;},
  })));
  const results = [], host = new TaskHost((...args) => results.push(args));
  t.after(() => host.close());
  const commands = [{op:'start',id:1,name:'web.http',value:request(),timeoutMilliseconds:1000}];
  host.prepare(commands); host.apply(commands);
  await new Promise(setImmediate);
  assert.deepEqual(results,[[1,'','HTTP response exceeds 2 MiB']]);
  assert.equal(cancelled,1); assert(pulls <= 4);
});

test('HTTP cancellation interrupts a pending body read and releases its lock', async () => {
  let cancelled;
  const response = new Response(new ReadableStream({cancel(reason) {cancelled = reason;}}));
  const controller = new AbortController(), reason = new Error('owner closed');
  const reading = readResponseText(response,controller.signal);
  controller.abort(reason);
  await assert.rejects(reading,error => error === reason);
  assert.equal(cancelled,reason); assert.equal(response.body.locked,false);
  await assert.rejects(readResponseText(new Response('late'),controller.signal),error => error === reason);
});

test('malformed HTTP options fail before starting network work', async t => {
  let called = 0;
  t.mock.method(globalThis,'fetch',() => {called++; throw new Error('unexpected fetch');});
  for(const delta of [{version:2},{url:''},{url:'x'.repeat(8193)},{method:'TRACE'},
    {body:'unexpected'},{method:'HEAD',body:'unexpected'},{method:'POST',body:'界'.repeat(350000)},
    {credentials:'no-cors'},{headers:null},{headers:[{name:'X-Test',value:'x'}]},
    {headers:[{name:'x',value:'a'},{name:'x',value:'b'}]},{headers:[{name:'x',value:'bad\r\nline'}]},
    {headers:[{name:'x',value:'x'.repeat(65536)}]}]) {
    await assert.rejects(fetchHTTP(request(delta),signal()));
  }
  assert.equal(called,0);
});

test('response metadata overflow releases the unread body', async t => {
  let cancelled = false;
  t.mock.method(globalThis,'fetch',async () => new Response(new ReadableStream({cancel(){cancelled=true;}}),{headers:{'x-large':'x'.repeat(65536)}}));
  await assert.rejects(fetchHTTP(request(),signal()),/headers exceed/);
  assert(cancelled);
});
