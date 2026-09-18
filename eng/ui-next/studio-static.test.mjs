import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdtemp,mkdir,readFile,writeFile,rm} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {studioStaticPages,studioStaticRedirects,exportStudio} from './studio-static.mjs';
import {serveFiles} from './static-server.mjs';

test('Studio export uses canonical bounded directory-index routes',()=>{
  assert.deepEqual(studioStaticPages(['/studio','/studio/docs/hello-world']).map(value=>value.file),['studio/index.html','studio/docs/hello-world/index.html']);
  for (const paths of [[],null,{},Array(257).fill('/studio'),['/'],['/assets'],['/studio/../file'],['/studio/A','/studio/a/'],['/studio/é','/studio/e\u0301']]) assert.throws(()=>studioStaticPages(paths));
});

test('redirects have one direct public target and cannot overwrite pages',()=>{
  const pages=studioStaticPages(['/studio/gallery','/studio/docs/hello-world']);
  const root={from:'/',to:'/studio/gallery'};
  assert.deepEqual(studioStaticRedirects([root,{from:'/docs/hello-world',to:'/studio/docs/hello-world'}],pages).map(value=>[value.file,value.to]),
    [['index.html','/studio/gallery/'],['docs/hello-world/index.html','/studio/docs/hello-world/']]);
  for(const values of [[],null,[root,root],[{from:'/',to:'/missing'}],[root,{from:'/STUDIO/GALLERY',to:'/studio/gallery'}],
    [root,{from:'/docs',to:'/studio/docs/hello-world',extra:true}]]) assert.throws(()=>studioStaticRedirects(values,pages));
});

test('invalid or cancelled static inputs preserve an existing delivery',async()=>{
  const directory=await mkdtemp(join(tmpdir(),'studio-static-input-'));
  const source=join(directory,'source'),destination=join(directory,'site');
  try {
    await mkdir(source);await mkdir(destination);
    await writeFile(join(destination,'index.html'),'previous site');
    await writeFile(join(source,'build-report.json'),JSON.stringify({schema:'volang.studio-next-distribution.v1',artifacts:[]}));
    await writeFile(join(source,'unexpected.txt'),'changed distribution');
    await assert.rejects(exportStudio({source,destination}),/distribution changed/);
    await assert.rejects(exportStudio({source,destination:join(source,'site')}),/separate/);
    const controller=new AbortController();controller.abort(new Error('cancelled static export'));
    await assert.rejects(exportStudio({source,destination,signal:controller.signal}),/cancelled static export/);
    assert.equal(await readFile(join(destination,'index.html'),'utf8'),'previous site');
  } finally {await rm(directory,{recursive:true,force:true});}
});

test('static document fallbacks retain 404 while missing assets stay plain responses',async()=>{
  const directory=await mkdtemp(join(tmpdir(),'studio-static-404-'));
  let server;
  try {
    await writeFile(join(directory,'404.html'),'<h1>Page not found</h1>');
    server=await serveFiles(directory,{notFoundDocument:'404.html'});
    for (const method of ['GET','HEAD']) {
      const response=await fetch(server.url+'missing',{method,headers:{accept:'text/html','if-none-match':'*'}});
      assert.equal(response.status,404);assert.match(response.headers.get('content-type'),/^text\/html/);
      assert.equal(await response.text(),method === 'HEAD'?'':'<h1>Page not found</h1>');
    }
    const asset=await fetch(server.url+'missing.js');assert.equal(asset.status,404);assert.equal(await asset.text(),'File not found.');
  } finally {await server?.close();await rm(directory,{recursive:true,force:true});}
});
