import assert from 'node:assert/strict';
import test from 'node:test';
import {createAdmission, ServerBusy} from './server-admission.mjs';
import {decodePage} from './server-response.mjs';
import {serverProtocol, requestBody, maxBodyBytes, RequestError} from './server-request.mjs';
import {PassThrough} from 'node:stream';
import {EventEmitter} from 'node:events';
import {developmentEvents} from './development-events.mjs';

const page = {version:serverProtocol, kind:'html', body:'', html:'<main data-vo-id="1"></main>', entry:'default', data:'', title:'Title 中文', description:'A page', status:200, headers:null};

test('server responses bind HTML, data, metadata, status and repeated headers', () => {
  const original = {...page, data:'</script> & 中文', headers:{'set-cookie':['a=1; HttpOnly', 'b=2; SameSite=Lax']}};
  const parsed = decodePage(JSON.stringify(original));
  assert.equal(parsed.html, original.html);
  assert.equal(parsed.data, original.data);
  assert.deepEqual(parsed.headers['set-cookie'], original.headers['set-cookie']);
  for (const status of [204, 205, 304]) assert.equal(decodePage(JSON.stringify({...page, status, html:'', entry:''})).status, status);
  assert.equal(decodePage(JSON.stringify({...page, status:303, html:'', entry:'', headers:{location:['/next']}})).status, 303);
  assert.equal(decodePage(JSON.stringify({...page, entry:'notes'})).entry, 'notes');
});

test('invalid response contracts fail before HTTP output is written', () => {
  for (const delta of [{version:1}, {version:serverProtocol - 1}, {version:serverProtocol + 1}, {status:99}, {status:600}, {status:200.5}, {html:''},
    {entry:undefined}, {entry:null}, {entry:''}, {entry:'../notes'}, {entry:'con'}, {entry:'Notes'}, {entry:'a'.repeat(65)},
    {html:'extra<main data-vo-id="1"></main>'}, {html:'\ud800'}, {status:204}, {title:null},
    {title:'\u0000'}, {title:'x'.repeat(4097)}, {description:'x'.repeat(8193)},
    {data:'\ud800'}, {data:'x'.repeat(1024 * 1024 + 1)}, {headers:[]},
    {headers:{'content-type':['anything']}}, {headers:{'Content-Language':['en']}},
    {headers:{'bad header':['x']}}, {headers:{'x-value':['bad\r\nline']}},
    {headers:{'x-value':['中文']}}, {headers:{'x-value':[]}}, {headers:{'x-value':[1]}},
    {headers:{'x-value':Array(257).fill('value')}}, {headers:{'x-value':['x'.repeat(65537)]}},
    {status:302, html:'', headers:{location:['']}}, {unknown:true}]) {
    assert.throws(() => decodePage(JSON.stringify({...page, ...delta})), JSON.stringify(delta).slice(0, 180));
  }
  for (const value of ['null', '[]', '{}', JSON.stringify(page) + 'extra']) assert.throws(() => decodePage(value));
});

test('JSON responses validate their own body and cannot acquire page startup data', () => {
  const response = {...page,kind:'json',html:'',entry:'',data:'',title:'',description:'',status:422,body:'{"name":"名字已使用"}'};
  assert.equal(decodePage(JSON.stringify(response)).body,response.body);
  for(const delta of [{kind:'unknown'},{body:'not JSON'},{body:'"' + 'x'.repeat(2*1024*1024) + '"'},
    {status:204},{status:205},{status:304},{html:page.html},{entry:'notes'},{data:'{}'},{title:'Title'},{description:'Description'}]) {
    assert.throws(() => decodePage(JSON.stringify({...response,...delta})));
  }
  assert.throws(() => decodePage(JSON.stringify({...page,body:'{}'})));
});

test('admission bounds active requests, preserves FIFO and removes cancelled waiters', async () => {
  const admit = createAdmission({concurrency:1, queued:2});
  const first = await admit(new AbortController().signal);
  const cancelled = new AbortController(), reason = new Error('request left');
  const second = admit(cancelled.signal);
  const third = admit(new AbortController().signal);
  await assert.rejects(admit(new AbortController().signal), ServerBusy);
  cancelled.abort(reason);
  await assert.rejects(second, error => error === reason);
  const fourth = admit(new AbortController().signal);
  first(); first();
  const finishThird = await third;
  let fourthStarted = false;
  void fourth.then(() => {fourthStarted = true;});
  await Promise.resolve();
  assert.equal(fourthStarted, false, 'idempotent release created an extra execution slot');
  finishThird();
  (await fourth)();
  (await admit(new AbortController().signal))();
  await assert.rejects(admit(cancelled.signal), error => error === reason);
  for (const options of [{concurrency:0}, {concurrency:65}, {queued:-1}, {queued:1025}]) assert.throws(() => createAdmission(options));
});

function upload(headers = {}, method = 'POST') {
  return Object.assign(new PassThrough({autoDestroy:false}), {headers, method});
}
const signal = () => new AbortController().signal;

test('request bodies retain UTF-8 across chunks, BOM and exact byte limits', async () => {
  const request = upload(), body = requestBody(request, signal());
  const bytes = Buffer.from('\ufeffname=中文&empty=');
  for (const byte of bytes) request.write(Buffer.from([byte]));
  request.end();
  assert.equal(await body, bytes.toString());
  const large = upload({'content-length':String(maxBodyBytes)});
  const decoded = requestBody(large, signal());
  large.end(Buffer.alloc(maxBodyBytes, 120));
  assert.equal((await decoded).length, maxBodyBytes);
  const empty = upload();
  empty.end();
  assert.equal(await requestBody(empty, signal()), '');
  assert.equal(await requestBody(upload({}, 'HEAD'), signal()), '');
});

test('body validation rejects actual overflow, unsupported encoding and malformed UTF-8', async () => {
  for (const headers of [{'content-length':String(maxBodyBytes + 1)}, {'content-encoding':'gzip'}]) {
    const request = upload(headers);
    await assert.rejects(requestBody(request, signal()), RequestError);
    assert.equal(request.listenerCount('data'), 0);
    request.destroy();
  }
  for (const [bytes, status] of [[Buffer.alloc(maxBodyBytes + 1), 413], [Buffer.from([0xff]), 400]]) {
    const request = upload(), body = requestBody(request, signal());
    request.end(bytes);
    await assert.rejects(body, error => error.status === status);
    for (const event of ['data', 'end', 'close', 'error']) assert.equal(request.listenerCount(event), 0);
    assert.equal(request.destroyed, false, 'reader destroyed the socket before the HTTP error could be sent');
    request.destroy();
  }
  await assert.rejects(requestBody(upload({'content-length':'1'}, 'GET'), signal()), error => error.status === 400);
  await assert.rejects(requestBody(upload({'transfer-encoding':'chunked'}, 'HEAD'), signal()), error => error.status === 400);
});

test('aborted and disconnected uploads release their request listeners', async () => {
  const request = upload(), controller = new AbortController(), reason = new Error('deadline');
  const body = requestBody(request, controller.signal);
  request.write('partial');
  controller.abort(reason);
  await assert.rejects(body, error => error === reason);
  for (const event of ['data', 'end', 'close', 'error']) assert.equal(request.listenerCount(event), 0);
  assert.equal(request.destroyed, false);
  request.destroy();
  const disconnected = upload(), reading = requestBody(disconnected, signal());
  disconnected.destroy();
  await assert.rejects(reading, error => error.status === 400);
  await assert.rejects(requestBody(upload(), controller.signal), error => error === reason);
});

test('development events replay current errors and bound slow or disconnected subscribers', () => {
  const hub = developmentEvents();
  class Response extends EventEmitter {
    chunks = []; accepts = true;
    writeHead(status, headers) {this.status = status; this.headers = headers; return this;}
    write(text) {this.chunks.push(text); return this.accepts;}
    end() {this.ended = true; this.emit('close'); return this;}
  }
  const connect = (method = 'GET') => {
    const response = new Response();
    assert(hub.handle('/__ui-next/events', {method}, response));
    return response;
  };
  const unrelated = new Response();
  assert.equal(hub.handle('/assets/app.js', {method:'GET'}, unrelated), false);
  hub.broadcast({type:'error', message:'Broken source'});
  assert(hub.hasError);
  const first = connect();
  assert(first.chunks.some(text => text.includes('Broken source')));
  hub.broadcast({type:'reload', version:'fixed'});
  assert.equal(hub.hasError, false);
  const late = connect();
  assert.equal(late.chunks.length, 1, 'successful build replayed a stale compiler error');
  const invalid = connect('POST');
  assert.equal(invalid.status, 405); assert.equal(invalid.headers.allow, 'GET');
  const clients = [first, late];
  while (clients.length < 64) clients.push(connect());
  assert.equal(connect().status, 503);
  first.accepts = false;
  hub.broadcast({type:'styles'});
  assert(first.ended, 'slow subscriber kept an unbounded write queue');
  const replacement = connect(); assert.equal(replacement.status, 200);
  late.end(); assert.equal(connect().status, 200);
  hub.close();
  assert(clients.every(client => client.ended));
  assert(replacement.ended);
  assert.equal(connect().status, 503, 'a closed development stream accepted a new subscriber');
});
