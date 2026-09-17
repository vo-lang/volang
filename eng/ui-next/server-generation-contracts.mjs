import assert from 'node:assert/strict';
import {request as httpRequest} from 'node:http';
import {cp, mkdtemp, readFile, readdir, rm, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {serveDevelopmentApplication} from './server-development.mjs';
import {root} from './server.mjs';

// Uses the independent server distribution produced by server-contracts.mjs.
const temporary = await mkdtemp(resolve(root, 'target/ui-next/generations-'));
let server;
try {
  for (const revision of ['one', 'two', 'three', 'invalid']) {
    const directory = join(temporary, revision);
    await cp(resolve(root, 'target/ui-next/server-distribution'), directory, {recursive:true});
    const path = join(directory, 'server/document.html'), template = await readFile(path, 'utf8');
    await writeFile(path, template.replace('</head>', `<meta name="generation" content="${revision}"></head>`));
    await writeFile(join(directory, 'public/probe.txt'), revision);
  }
  server = await serveDevelopmentApplication({retained:2, concurrency:1, queued:1, fallbackDocument:'<!doctype html><h1>Building</h1>'});
  await server.install(join(temporary, 'one'), 'one');
  function startSlow() {
    let accepted;
    const started = new Promise(resolve => {accepted = resolve;});
    const response = new Promise((resolve, reject) => {
      const request = httpRequest(server.url + 'slow', {headers:{expect:'100-continue'}, agent:false}, incoming => {
        let html = ''; incoming.setEncoding('utf8'); incoming.on('data', bytes => {html += bytes;});
        incoming.once('end', () => resolve({status:incoming.statusCode, html}));
        incoming.once('error', reject);
      });
      request.once('continue', accepted); request.once('error', reject); request.end();
    });
    return {started, response};
  }
  const first = startSlow(); await first.started;
  await server.install(join(temporary, 'two'), 'two');
  const waiting = [fetch(server.url + 'profile?name=First'), fetch(server.url + 'profile?name=Second')];
  assert.equal((await Promise.race(waiting)).status, 503, 'new build bypassed the shared admission budget');
  let thirdInstalled = false;
  const third = server.install(join(temporary, 'three'), 'three').then(() => {thirdInstalled = true;});
  assert.equal(thirdInstalled, false);
  const old = await first.response;
  assert.equal(old.status, 404); // /slow is intentionally outside the profile route.
  assert(old.html.includes('name="generation" content="one"'), 'in-flight SSR mixed build generations');
  const responses = await Promise.all(waiting);
  assert.deepEqual(responses.map(response => response.status).sort(), [200, 503]);
  assert((await responses.find(response => response.status === 200).text()).includes('name="generation" content="two"'), 'queued request lost its captured build');
  await third;
  assert.equal((await fetch(server.url + '__ui-next/builds/one/probe.txt')).status, 410);
  assert.equal(await (await fetch(server.url + '__ui-next/builds/two/probe.txt')).text(), 'two');
  assert.equal(await (await fetch(server.url + '__ui-next/builds/two/probe.txt?ui-dev=three')).text(), 'three');
  assert.equal(await (await fetch(server.url + '__ui-next/builds/two/probe.txt?ui-dev=manual')).text(), 'three');
  assert.equal((await fetch(server.url + '__ui-next/builds/three/missing.txt')).status, 404);
  assert.equal((await fetch(server.url + '__ui-next/builds/three/server/app.vob')).status, 404);
  assert.deepEqual((await readdir(temporary)).sort(), ['invalid', 'three', 'two']);
  await writeFile(join(temporary, 'invalid/server/document.html'), '<!doctype html><h1>Incomplete</h1>');
  await assert.rejects(server.install(join(temporary, 'invalid'), 'invalid'));
  assert((await (await fetch(server.url + 'profile')).text()).includes('name="generation" content="three"'));
  const interrupted = startSlow(), cancelled = interrupted.response.catch(error => error);
  await interrupted.started;
  const started = performance.now();
  await server.close(); server = undefined;
  await cancelled;
  assert(performance.now() - started < 4000, 'shutdown waited for the five-second guest instead of cancelling it');
  assert.deepEqual(await readdir(temporary), ['invalid']);
  await writeFile(resolve(root, 'target/ui-next/server-generation-report.json'), JSON.stringify({passed:true, retained:2,
    contracts:['in-flight-generation-isolation', 'cross-generation-admission', 'queued-generation-isolation', 'retirement-drains', 'bounded-retention', 'expired-assets-410',
      'exact-refresh-generation', 'manual-current-generation', 'asset-only-404', 'failed-install-retains-current', 'shutdown-cancels-and-cleans']}, null, 2) + '\n');
  console.log('Server generation contracts passed');
} finally {await server?.close(); await rm(temporary, {recursive:true, force:true});}
