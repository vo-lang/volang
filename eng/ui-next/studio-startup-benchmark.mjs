// Reproducible delivery model, not a claim about any user's Internet connection.
import assert from 'node:assert/strict';
import {createServer} from 'node:http';
import {readFile, writeFile} from 'node:fs/promises';
import {resolve, extname, sep} from 'node:path';
import {gzipSync} from 'node:zlib';
import {root} from './repository-paths.mjs';

const directory = resolve(root,'target/ui-next/studio-static');
let bytesPerSecond = 125000, delay = 200, requests = [];
const streams = new Set(); let previous = performance.now();
const pump = setInterval(() => {
  const now = performance.now(); let budget = Math.floor(bytesPerSecond * (now - previous) / 1000); previous = now;
  while (budget > 0 && streams.size) {
    const stream = streams.values().next().value;
    streams.delete(stream);
    if (stream.response.destroyed) continue;
    const length = Math.min(budget, 1024, stream.bytes.length - stream.offset);
    stream.response.write(stream.bytes.subarray(stream.offset, stream.offset + length));
    stream.offset += length; budget -= length;
    if (stream.offset === stream.bytes.length) stream.response.end();
    else streams.add(stream);

  }
}, 10);
const server = createServer(async (request,response) => {
  try {
    const pathname = new URL(request.url,'http://localhost').pathname;
    let file = resolve(directory,'.'+pathname);
    if (file !== directory && !file.startsWith(directory+sep)) throw new Error('Invalid path');
    if (pathname.endsWith('/')) file += '/index.html';
    const source = await readFile(file);
    const bytes = pathname.endsWith('.gz') ? source : gzipSync(source);
    const type = {'.html':'text/html','.js':'text/javascript','.css':'text/css','.wasm':'application/wasm','.json':'application/json'}[extname(file)] ?? 'application/octet-stream';
    requests.push({pathname,bytes:bytes.length,at:performance.now()});
    setTimeout(() => {
      if (response.destroyed) return;
      response.writeHead(200,{'content-type':type,'content-length':bytes.length,'cache-control':'max-age=600',
        ...(pathname.endsWith('.gz') ? {} : {'content-encoding':'gzip'})});
      streams.add({response,bytes,offset:0});
    }, delay);
  } catch {response.writeHead(404).end();}
});
await new Promise(resolve => server.listen(0,'127.0.0.1',resolve));
const origin = `http://127.0.0.1:${server.address().port}`;
process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
const {chromium} = await import('../browser/node_modules/playwright/index.mjs');
const browser = await chromium.launch();
const samples = [];
const percentile = (values,p) => values.toSorted((a,b)=>a-b)[Math.ceil(values.length*p)-1];
try {
  for (const kind of ['content','gallery','entry-navigation']) {
    bytesPerSecond = kind === 'gallery' ? 1250000 : 125000;
    delay = kind === 'gallery' ? 100 : 200;
    for (let sample=0; sample<10; sample++) {
      const context = await browser.newContext(), page = await context.newPage(); requests = [];
      try {
        const started = performance.now();
        await page.goto(origin+(kind === 'content'?'/studio/docs/':kind === 'gallery'?'/studio/gallery/':'/'),{waitUntil:kind === 'entry-navigation' ? 'commit' : 'domcontentloaded'});
        if (kind === 'entry-navigation') {
          await page.getByRole('link',{name:'State & identity',exact:true}).click();
        }
        await page.waitForFunction(kind => kind !== 'gallery' ? document.documentElement?.hasAttribute('data-content-ready') :
          window.__studioNext?.ready || window.__studioNext?.error,kind);
        const readyMs = performance.now()-started;
        const coldRequests = requests.map(item=>({...item,at:item.at-started}));
        let navigationMs, specificationMs;
        if (kind === 'content') {
          await page.locator('#studio-theme').check();
          assert.equal(await page.locator('.studio').getAttribute('data-theme'),'dark');
          let start = performance.now();
          await page.getByRole('link',{name:'State & identity',exact:true}).click();
          await page.waitForFunction(() => document.documentElement?.hasAttribute('data-content-ready'));
          navigationMs = performance.now()-start;
          start = performance.now();
          await page.getByRole('navigation',{name:'Documentation chapters'}).getByRole('link',{name:'Language specification',exact:true}).click();
          await page.waitForFunction(() => document.documentElement?.hasAttribute('data-content-ready'));
          specificationMs = performance.now()-start;
          assert(!requests.some(item=>/\/(?:wasm|artifacts|compiler)\//.test(item.pathname)));
        } else if (kind === 'entry-navigation') {
          assert(!requests.some(item=>/\/(?:wasm|artifacts|compiler)\//.test(item.pathname)));
        } else if (kind === 'gallery') {
          assert.equal(await page.evaluate(() => window.__studioNext.error),null);
          await page.getByRole('button',{name:'Make it happen'}).click();
          await page.waitForFunction(() => document.querySelector('[data-demo-count]').textContent.startsWith('1 '));
          for (const path of ['/wasm/vo_web_bg.wasm','/artifacts/studio.vob.gz']) {
            assert.equal(coldRequests.filter(item=>item.pathname===path).length,1,'Duplicate core download: '+path);
          }
        }
        const result={kind,sample,readyMs,navigationMs,specificationMs,bytes:coldRequests.reduce((n,item)=>n+item.bytes,0),requests:coldRequests};
        samples.push(result);
        console.log(JSON.stringify({...result,requests:undefined}));
      } finally {await context.close();}
    }
  }
} finally {
  clearInterval(pump); await browser.close(); server.closeAllConnections(); await new Promise(resolve=>server.close(resolve));
  const summary = ['content','gallery','entry-navigation'].map(kind => {
    const rows = samples.filter(sample=>sample.kind===kind);
    return {kind,count:rows.length,medianMs:percentile(rows.map(row=>row.readyMs),.5),p95Ms:percentile(rows.map(row=>row.readyMs),.95),
      navigationP95Ms:kind==='content'?percentile(rows.map(row=>row.navigationMs),.95):undefined,
      specificationP95Ms:kind==='content'?percentile(rows.map(row=>row.specificationMs),.95):undefined};
  });
  await writeFile(resolve(root,'target/ui-next/startup-benchmark.json'),JSON.stringify({
    method:'Fresh Chromium context per sample, local gzip HTTP server, max-age=600, round-robin shared response-byte bandwidth, fixed delay per request. Content and root-to-chapter navigation: 1 Mbps/200 ms; Gallery: 10 Mbps/100 ms. Includes functional readiness; excludes DNS/TLS and Internet variability.',
    summary,samples},null,2)+'\n');
  console.log(JSON.stringify(summary,null,2));
}
assert.equal(samples.length,30);
assert(percentile(samples.filter(row=>row.kind==='entry-navigation').map(row=>row.readyMs),.95)<=1500);
assert(percentile(samples.filter(row=>row.kind==='content').map(row=>row.readyMs),.95)<=1500);
assert(percentile(samples.filter(row=>row.kind==='content').map(row=>row.navigationMs),.95)<=1000);
assert(percentile(samples.filter(row=>row.kind==='content').map(row=>row.specificationMs),.95)<=1500);
assert(percentile(samples.filter(row=>row.kind==='gallery').map(row=>row.readyMs),.95)<=3000);
