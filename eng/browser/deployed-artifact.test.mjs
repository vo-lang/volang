import { test } from 'node:test';
import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import { createHash } from 'node:crypto';
import { verifyDeployedArtifact } from './deployed-artifact.mjs';

async function serve(handler, run) {
  const server = createServer(handler);
  await new Promise(done => server.listen(0, '127.0.0.1', done));
  try { await run(`http://127.0.0.1:${server.address().port}/`); }
  finally { await new Promise(done => server.close(done)); }
}
const content = Buffer.from('verified deployment');
const records = [{path:'app.js',size:content.length,sha256:createHash('sha256').update(content).digest('hex')}];

test('checks every declared byte once without retries', async () => {
  let requests = 0;
  await serve((request, response) => { requests++; response.end(content); }, async url => {
    assert.deepEqual(await verifyDeployedArtifact(url, records), {complete:true,passed:true,files:1,bytes:content.length});
  });
  assert.equal(requests, 1);
});
for (const mode of ['same-size-stale', 'oversized', 'truncated', 'missing', 'redirect']) {
  test(`rejects ${mode} deployed assets`, async () => {
    let requests = 0;
    await serve((request, response) => {
      requests++;
      if (mode === 'missing') {response.writeHead(404);response.end();}
      else if (mode === 'redirect') {response.writeHead(302,{location:'/app.js'});response.end();}
      else response.end(mode === 'same-size-stale' ? Buffer.alloc(content.length) : mode === 'oversized' ? Buffer.alloc(content.length + 1) : '');
    }, async url => { await assert.rejects(verifyDeployedArtifact(url, records)); });
    assert.equal(requests, 1);
  });
}
test('rejects incomplete and unsafe records before fetch', async () => {
  await assert.rejects(verifyDeployedArtifact('http://127.0.0.1:1/', []), /invalid asset set/);
  await assert.rejects(verifyDeployedArtifact('http://user:secret@127.0.0.1/', records), /without credentials/);
  await assert.rejects(verifyDeployedArtifact('http://127.0.0.1:1/', [{...records[0],path:'../app.js'}]), /invalid asset record/);
});
