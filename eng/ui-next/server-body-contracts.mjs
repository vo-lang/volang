import assert from 'node:assert/strict';
import {request as httpRequest} from 'node:http';
import {serveApplication} from './server-adapter.mjs';
import {maxBodyBytes} from './server-request.mjs';
import {createAdmission} from './server-admission.mjs';

// Real HTTP streams exercise backpressure, response delivery and cancellation;
// the render hook records whether invalid/unfinished uploads started guest work.
export async function checkServerBodies(directory) {
  const rendered = [], errors = [];
  const admit = createAdmission({concurrency:1, queued:0});
  let granted, released;
  const render = async (_executable, _artifact, request) => {
    rendered.push(request); return {status:204, html:'', headers:{}};
  };
  let server = await serveApplication(directory, {timeoutMilliseconds:5000, render,
    onError:error => errors.push(error.message), admission:async signal => {
      const release = await admit(signal);
      granted?.(); granted = undefined;
      return () => {release(); released?.(); released = undefined;};
    }});
  const sockets = new Set();
  function stream({method = 'POST', headers = {}, body, end = true} = {}) {
    let request;
    const response = new Promise((resolve, reject) => {
      request = httpRequest(server.url + 'profile', {method, headers, agent:false}, incoming => {
        incoming.once('error', reject);
        incoming.resume(); incoming.once('end', () => resolve(incoming));
      });
      sockets.add(request);
      request.once('close', () => sockets.delete(request));
      request.once('error', reject);
      request.flushHeaders();
      if (body !== undefined) request.write(body);
      if (end) request.end();
    });
    // Assertions may fail while another intentional partial upload is pending.
    // Keep its later teardown rejection from masking the original diagnostic.
    void response.catch(() => {});
    return {request, response};
  }
  try {
    for (const options of [
      {body:Buffer.alloc(maxBodyBytes + 1)},
      {headers:{'content-length':String(maxBodyBytes + 1)}, end:false},
    ]) assert.equal((await stream(options).response).statusCode, 413);
    assert.equal((await stream({body:Buffer.from([0xff])}).response).statusCode, 400);
    assert.equal((await stream({method:'GET', headers:{'content-length':'1'}, body:'x'}).response).statusCode, 400);
    assert.equal((await stream({headers:{'content-encoding':'gzip'}, body:'x'}).response).statusCode, 415);
    assert.equal(rendered.length, 0);
    const admitted = new Promise(resolve => {granted = resolve;});
    const disconnected = stream({headers:{'content-length':'100'}, body:'partial', end:false});
    await admitted;
    // A competing upload is rejected while the first owns the only slot.
    const overflow = await stream({body:'another'}).response;
    assert.equal(overflow.statusCode, 503);
    const ended = disconnected.response.catch(() => {});
    const freed = new Promise(resolve => {released = resolve;});
    disconnected.request.destroy();
    await ended; await freed;
    assert.equal(rendered.length, 0, 'disconnected body started a native process');
    const value = '\ufeffname=中文&empty=';
    assert.equal((await stream({body:value}).response).statusCode, 204);
    assert.equal(rendered.at(-1).body, value);
    assert.equal((await stream({headers:{'content-length':String(maxBodyBytes)}, body:Buffer.alloc(maxBodyBytes, 120)}).response).statusCode, 204);
    assert.equal(Buffer.byteLength(rendered.at(-1).body), maxBodyBytes);
    await server.close();
    server = await serveApplication(directory, {timeoutMilliseconds:150, render, onError:error => errors.push(error.message)});
    const before = rendered.length;
    const partial = stream({headers:{'content-length':'100'}, body:'partial', end:false});
    assert.equal((await partial.response).statusCode, 504);
    assert.equal(rendered.length, before, 'partial body started a native process');
    assert.deepEqual(errors, []);
    return {passed:true, contracts:['actual-body-limit', 'declared-body-limit', 'utf8-body', 'get-body-rejection',
      'encoding-rejection', 'upload-deadline', 'upload-admission', 'upload-disconnect', 'exact-byte-limit']};
  } finally {for (const request of sockets) request.destroy(); await server.close();}
}
