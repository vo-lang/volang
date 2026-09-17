import assert from 'node:assert/strict';
import test from 'node:test';
import {request as httpRequest} from 'node:http';
import {chmod,mkdtemp,mkdir,open,readFile,rm,symlink,writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {brotliDecompressSync,gunzipSync} from 'node:zlib';
import {PassThrough} from 'node:stream';
import {selectEncoding,sendStatic} from './static-response.mjs';
import {precompressAssets} from './precompress.mjs';
import {serveFiles} from './static-server.mjs';
import {createApplication} from './server-adapter.mjs';
import {createServer} from 'node:http';
import {contentMarker,modeMarker,assetsMarker,dataMarker,titleMarker,descriptionMarker} from './prerender.mjs';

test('encoding negotiation preserves exclusions, weights, wildcard and identity fallback',()=>{
  const available = ['br','gzip','identity'];
  for (const [header,expected] of [[undefined,'identity'],['','identity'],['gzip, br','br'],
    ['gzip;q=1, br;q=0.5','gzip'],['BR;Q=0, gzip;q=0.7','gzip'],['br;q=0, *;q=1','gzip'],
    ['gzip;q=0','identity'],['identity;q=1, br;q=0.5','identity'],['gzip;q=0.5','gzip'],
    ['*;q=0',undefined],['identity;q=0, br;q=0, gzip;q=0',undefined],
    ['*;q=0, identity;q=1','identity'],['br;q=bad','identity'],['zstd','identity']]) {
    assert.equal(selectEncoding(header,available),expected,header);
  }
  assert.equal(selectEncoding('br',['identity']),'identity');
  assert.equal(selectEncoding('br, identity;q=0',['identity']),undefined);
});

function request(url,options={}) {
  return new Promise((resolve,reject)=>{
    const outgoing = httpRequest(url,options,incoming=>{
      const chunks=[];
      incoming.on('data',chunk=>chunks.push(chunk)); incoming.once('error',reject);
      incoming.once('end',()=>resolve({status:incoming.statusCode,headers:incoming.headers,bytes:Buffer.concat(chunks)}));
    });
    outgoing.once('error',reject); outgoing.end();
  });
}

async function serverFixture(prefix) {
  const directory=await mkdtemp(join(tmpdir(),prefix));
  await mkdir(join(directory,'public/assets'),{recursive:true}); await mkdir(join(directory,'server'));
  await writeFile(join(directory,'server/document.html'),`<title>${titleMarker}</title><meta content="${descriptionMarker}"><main data-mode="${modeMarker}">${contentMarker}</main><script>${dataMarker}</script><link href="${assetsMarker}theme.css">`);
  await writeFile(join(directory,'server/app.vob'),'unused static fixture');
  await writeFile(join(directory,'server/entries.json'),JSON.stringify({version:1,entries:['default']}));
  return directory;
}

test('public files negotiate compressed bytes, validators and HEAD without changing their MIME',async()=>{
  const directory=await mkdtemp(join(tmpdir(),'ui-static-response-'));
  let server;
  try {
    const bytes=Buffer.from('export const message = "中文";\n'.repeat(4000));
    await writeFile(join(directory,'app.js'),bytes);
    await writeFile(join(directory,'small.css'),'body {}');
    const outputs=await precompressAssets(directory);
    assert.equal(outputs.length,1); assert(outputs[0].br < bytes.length/4); assert(outputs[0].gzip < bytes.length/4);
    assert.deepEqual(brotliDecompressSync(await readFile(join(directory,'app.js.br'))),bytes);
    assert.deepEqual(gunzipSync(await readFile(join(directory,'app.js.gz'))),bytes);
    server=await serveFiles(directory);
    const plain=await request(server.url+'app.js');
    assert.deepEqual(plain.bytes,bytes); assert.equal(plain.headers['content-encoding'],undefined);
    const tags=[];
    for (const encoding of ['br','gzip']) {
      const response=await request(server.url+'app.js',{headers:{'accept-encoding':encoding}});
      assert.equal(response.status,200); assert.equal(response.headers['content-encoding'],encoding);
      assert.equal(response.headers['content-type'],'text/javascript'); assert.equal(response.headers.vary,'Accept-Encoding');
      assert.equal(Number(response.headers['content-length']),response.bytes.length);
      assert.deepEqual((encoding==='br'?brotliDecompressSync:gunzipSync)(response.bytes),bytes);
      tags.push(response.headers.etag);
      const cached=await request(server.url+'app.js',{headers:{'accept-encoding':encoding,'if-none-match':response.headers.etag}});
      assert.equal(cached.status,304); assert.equal(cached.bytes.length,0); assert.equal(cached.headers.etag,response.headers.etag);
      const head=await request(server.url+'app.js',{method:'HEAD',headers:{'accept-encoding':encoding}});
      assert.equal(head.status,200); assert.equal(head.bytes.length,0); assert.equal(head.headers['content-length'],response.headers['content-length']);
    }
    assert.notEqual(tags[0],tags[1]); assert.notEqual(tags[0],plain.headers.etag);
    assert.equal((await request(server.url+'app.js',{headers:{'if-modified-since':plain.headers['last-modified']}})).status,304);
    assert.equal((await request(server.url+'app.js',{headers:{'if-none-match':'"other"','if-modified-since':plain.headers['last-modified']}})).status,200);
    assert.equal((await request(server.url+'app.js',{headers:{'accept-encoding':'*;q=0'}})).status,406);
    assert.equal((await request(server.url+'missing.js')).status,404);
    assert.equal((await request(server.url+'small.css',{headers:{'accept-encoding':'br'}})).headers['content-encoding'],undefined);
    await server.close(); server=await serveFiles(directory,{development:true});
    const development=await request(server.url+'app.js',{headers:{'accept-encoding':'br','if-none-match':tags[0]}});
    assert.equal(development.status,200); assert.deepEqual(development.bytes,bytes);
    assert.equal(development.headers['content-encoding'],undefined); assert.equal(development.headers['cache-control'],'no-store');
  } finally {await server?.close(); await rm(directory,{recursive:true,force:true});}
});

test('build compression preserves authored collisions and joins cancellation',async()=>{
  const directory=await mkdtemp(join(tmpdir(),'ui-compression-build-'));
  try {
    await writeFile(join(directory,'app.js'),'a'.repeat(10000));
    await writeFile(join(directory,'app.js.gz'),'authored bytes');
    await assert.rejects(precompressAssets(directory),{code:'EEXIST'});
    assert.equal(await readFile(join(directory,'app.js.gz'),'utf8'),'authored bytes');
    const controller=new AbortController(),reason=new Error('build left'); controller.abort(reason);
    await assert.rejects(precompressAssets(directory,{signal:controller.signal}),error=>error===reason);
  } finally {await rm(directory,{recursive:true,force:true});}
});

test('static file ownership excludes escaping links and closes interrupted downloads',async()=>{
  const directory=await mkdtemp(join(tmpdir(),'ui-static-lifetime-'));
  let server;
  try {
    const publicDirectory=join(directory,'public'); await mkdir(publicDirectory);
    await writeFile(join(directory,'outside.js'),'outside');
    await symlink('../outside.js',join(publicDirectory,'link.js'));
    await writeFile(join(publicDirectory,'large.bin'),Buffer.alloc(8*1024*1024,42));
    server=await serveFiles(publicDirectory);
    assert.equal((await request(server.url+'link.js')).status,404);
    await new Promise((resolve,reject)=>{
      const outgoing=httpRequest(server.url+'large.bin',incoming=>{
        incoming.once('data',()=>{incoming.destroy(); resolve();}); incoming.once('error',reject);
      });
      outgoing.once('error',reject); outgoing.end();
    });
    await server.close(); await server.close();
  } finally {await server?.close(); await rm(directory,{recursive:true,force:true});}
});

test('a receiver closing after headers cancels the file without an application error',async()=>{
  const directory=await mkdtemp(join(tmpdir(),'ui-static-header-close-'));
  try {
    const path=join(directory,'app.js'); await writeFile(path,'console.log("ready");');
    const response=new PassThrough();
    response.writeHead=()=>{response.destroy(); return response;};
    await sendStatic({method:'GET',headers:{}},response,path,{root:directory});
    assert(response.destroyed);
  } finally {await rm(directory,{recursive:true,force:true});}
});

test('slow static downloads keep their deadline during build retirement',async()=>{
  const directory=await serverFixture('ui-static-retirement-');
  let application,listener,incoming;
  const errors=[];
  try {
    const file=await open(join(directory,'public/assets/large.bin'),'w');
    try {await file.truncate(64*1024*1024);} finally {await file.close();}
    application=await createApplication(directory,{timeoutMilliseconds:250,onError:error=>errors.push(error)});
    listener=createServer(application.handle); await new Promise(resolve=>listener.listen(0,'127.0.0.1',resolve));
    await new Promise((resolve,reject)=>{
      const outgoing=httpRequest(`http://127.0.0.1:${listener.address().port}/assets/large.bin`,response=>{
        incoming=response; incoming.pause(); resolve();
      }); outgoing.once('error',reject); outgoing.end();
    });
    let timer;
    try {
      await Promise.race([application.retire(),new Promise((_,reject)=>{timer=setTimeout(()=>reject(new Error('slow download blocked retirement')),3000);})]);
    } finally {clearTimeout(timer);}
    assert.deepEqual(errors,[]);
  } finally {
    incoming?.destroy();
    await application?.close();
    if(listener) {const stopped=new Promise(resolve=>listener.close(resolve)); listener.closeAllConnections(); await stopped;}
    await rm(directory,{recursive:true,force:true});
  }
});

test('file read failures keep their diagnostic when pipeline disposal also closes the connection',
  {skip:process.platform==='win32' || process.getuid?.()===0 ? 'requires enforced Unix file permissions' : false},async()=>{
  const directory=await serverFixture('ui-static-read-error-');
  const path=join(directory,'public/assets/unreadable.js');
  let application,listener; const errors=[];
  try {
    await writeFile(path,'console.log("unreadable");'); await chmod(path,0);
    application=await createApplication(directory,{onError:error=>errors.push(error)});
    listener=createServer(application.handle); await new Promise(resolve=>listener.listen(0,'127.0.0.1',resolve));
    await assert.rejects(request(`http://127.0.0.1:${listener.address().port}/assets/unreadable.js`));
    assert.equal(errors.length,1); assert.equal(errors[0].code,'EACCES');
  } finally {
    await chmod(path,0o600).catch(()=>{}); await application?.close();
    if(listener) {const stopped=new Promise(resolve=>listener.close(resolve)); listener.closeAllConnections(); await stopped;}
    await rm(directory,{recursive:true,force:true});
  }
});

test('native media streams byte ranges and preserves conditional, HEAD and encoded semantics',async()=>{
  const directory=await mkdtemp(join(tmpdir(),'ui-media-range-'));
  let server;
  try {
    const bytes=await readFile(new URL('../../ui/next/templates/listening/web/four-notes.wav',import.meta.url));
    await writeFile(join(directory,'recording.WAV'),bytes);
    await writeFile(join(directory,'empty.wav'),'');
    await writeFile(join(directory,'app.js'),'export const message="a small thought";\n'.repeat(1000));
    await precompressAssets(directory);
    server=await serveFiles(directory,{base:'/listening/'});
    const url=server.url+'recording.WAV',full=await request(url);
    assert.equal(full.headers['content-type'],'audio/wav');assert.equal(full.headers['accept-ranges'],'bytes');
    for(const [range,start,end] of [['bytes=0-43',0,43],['bytes=44-',44,bytes.length-1],
      ['bytes=-12',bytes.length-12,bytes.length-1],['BYTES= 2-5',2,5],
      ['bytes=0-900719925474099100000',0,bytes.length-1],['bytes=-900719925474099100000',0,bytes.length-1]]) {
      const response=await request(url,{headers:{range}});
      assert.equal(response.status,206,range);assert.equal(response.headers['content-range'],`bytes ${start}-${end}/${bytes.length}`);
      assert.equal(Number(response.headers['content-length']),end-start+1);assert.equal(response.headers.etag,full.headers.etag);
      assert.deepEqual(response.bytes,bytes.subarray(start,end+1));
    }
    for(const range of [`bytes=${bytes.length}-`,'bytes=900719925474099100000-','bytes=-0']) {
      const response=await request(url,{headers:{range}});
      assert.equal(response.status,416,range);assert.equal(response.headers['content-range'],`bytes */${bytes.length}`);assert.equal(response.bytes.length,0);
    }
    for(const range of ['bytes=8-2','bytes=-','bytes=0-0,2-3','items=0-1','bytes=1e3-','bytes='+'9'.repeat(1100)+'-']) {
      const response=await request(url,{headers:{range}});assert.equal(response.status,200,range);assert.deepEqual(response.bytes,bytes);
    }
    for(const condition of [full.headers.etag,full.headers.etag.slice(2),full.headers['last-modified'],'not a validator']) {
      const response=await request(url,{headers:{range:'bytes=0-1','if-range':condition}});
      assert.equal(response.status,200);assert.deepEqual(response.bytes,bytes);
    }
    const cached=await request(url,{headers:{range:'bytes=0-1','if-none-match':full.headers.etag}});
    assert.equal(cached.status,304);assert.equal(cached.headers['content-range'],undefined);
    const head=await request(url,{method:'HEAD',headers:{range:'bytes=0-1'}});
    assert.equal(head.status,200);assert.equal(head.headers['content-range'],undefined);
    assert.equal(Number(head.headers['content-length']),bytes.length);assert.equal(head.bytes.length,0);
    const empty=await request(server.url+'empty.wav',{headers:{range:'bytes=-1'}});
    assert.equal(empty.status,200);assert.equal(empty.bytes.length,0);
    const encoded=await readFile(join(directory,'app.js.gz'));
    const partial=await request(server.url+'app.js',{headers:{range:'bytes=0-9','accept-encoding':'gzip'}});
    assert.equal(partial.status,206);assert.equal(partial.headers['content-encoding'],'gzip');
    assert.equal(partial.headers['content-range'],`bytes 0-9/${encoded.length}`);assert.deepEqual(partial.bytes,encoded.subarray(0,10));
  } finally {await server?.close();await rm(directory,{recursive:true,force:true});}
});
