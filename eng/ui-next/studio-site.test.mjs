import assert from 'node:assert/strict';
import test from 'node:test';
import {mkdtemp,mkdir,readFile,writeFile,rm,symlink} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join} from 'node:path';
import {artifactInventory} from './artifact-inventory.mjs';
import {serveFiles} from './static-server.mjs';
import {stageStudioSite,verifyStudioSite,verifyStudioOrigin} from './studio-site.mjs';
import {siteArguments,checkStudioSite,siteFailureDetails} from './studio-site-cli.mjs';
import {studioSiteBudgets} from './studio-site-budgets.mjs';

async function fixture() {
  const directory=await mkdtemp(join(tmpdir(),'studio-site-')),source=join(directory,'static'),destination=join(directory,'site');
  await mkdir(source);await writeFile(join(source,'index.html'),'<h1>Studio</h1>');await writeFile(join(source,'app.js'),'export const ready=true;');
  await writeFile(join(source,'runtime.wasm'),Buffer.from([0,97,115,109,1,0,0,0]));
  await writeFile(join(source,'build-report.json'),JSON.stringify({schema:'volang.studio-next-static.v1',wireVersion:23,artifacts:await artifactInventory(source)})+'\n');
  return {directory,source,destination,domain:'volang.dev'};
}

test('site CLI rejects incomplete, duplicate and unknown flags without writing',()=>{
  assert.deepEqual(siteArguments(['check','--directory','site','--output','report']),{command:'check',options:{directory:'site',output:'report'}});
  assert.deepEqual(siteArguments(['stage','--help']),{help:true});
  for(const args of [[],['publish'],['stage','--source','source'],['verify','--directory','site','--directory','other'],['check','--directory','site'],['verify','--directory','site','--unknown','x'],['verify','directory','site']])assert.throws(()=>siteArguments(args),/Usage/);
});

test('site staging preserves its static bytes and repairs only owned outputs',async()=>{
  const value=await fixture();
  try {
    await stageStudioSite(value);const original=await verifyStudioSite(value.destination);
    assert.deepEqual(await readFile(join(value.source,'build-report.json')),await readFile(join(value.destination,'build-report.json')));
    assert.equal(await readFile(join(value.destination,'CNAME'),'utf8'),'volang.dev\n');
    await writeFile(join(value.destination,'index.html'),'damaged output');
    await stageStudioSite(value);assert.deepEqual(await verifyStudioSite(value.destination),original);
    await writeFile(join(value.source,'index.html'),'damaged source');
    await assert.rejects(stageStudioSite(value),/changed after verification/);
    assert.deepEqual(await verifyStudioSite(value.destination),original);
    await assert.rejects(stageStudioSite({...value,destination:value.source}),/separate/);
    const alias=join(value.directory,'alias');await symlink(value.source,alias,'dir');
    await assert.rejects(stageStudioSite({...value,destination:join(alias,'nested','output')}),/separate/);
    await assert.rejects(checkStudioSite({directory:value.source,output:join(alias,'reports')}),/separate/);
    await assert.rejects(stageStudioSite({...value,domain:'https://volang.dev'}),/hostname/);
    const controller=new AbortController();controller.abort(new Error('cancelled staging'));
    await assert.rejects(stageStudioSite({...value,signal:controller.signal}),/cancelled staging/);
  }finally{await rm(value.directory,{recursive:true,force:true});}
});

test('site verification binds every local file and preserves unrelated directories',async()=>{
  const value=await fixture();
  try {
    await mkdir(value.destination);await writeFile(join(value.destination,'notes.txt'),'retain');
    await assert.rejects(stageStudioSite(value));
    assert.equal(await readFile(join(value.destination,'notes.txt'),'utf8'),'retain');
    await rm(value.destination,{recursive:true});await stageStudioSite(value);
    await writeFile(join(value.destination,'extra.txt'),'unexpected');
    await assert.rejects(verifyStudioSite(value.destination),/changed after staging/);
    await rm(join(value.destination,'extra.txt'));
    const path=join(value.destination,'site-manifest.json'),site=JSON.parse(await readFile(path));
    await writeFile(path,JSON.stringify({...site,buildSha256:'0'.repeat(64)}));
    await assert.rejects(verifyStudioSite(value.destination),/different static build/);
  }finally{await rm(value.directory,{recursive:true,force:true});}
});

test('origin checks reject stale bytes and wrong MIME types and join cancelled work',async()=>{
  const value=await fixture();let server;
  try {
    await stageStudioSite(value);server=await serveFiles(value.destination);
    const result=await verifyStudioOrigin(value.destination,server.url);
    assert(result.passed);assert.deepEqual(result.hostingMetadata.map(entry=>entry.path),['CNAME']);
    await assert.rejects(verifyStudioOrigin(value.destination,server.url+'subdirectory/'),/HTTP origin/);
    const stale=async(url,options)=>url.pathname==='/app.js'?new Response('old',{headers:{'content-type':'text/javascript'}}):fetch(url,options);
    await assert.rejects(verifyStudioOrigin(value.destination,server.url,{fetch:stale}),/differs/);
    const wrong=async(url,options)=>url.pathname==='/runtime.wasm'?new Response('data',{headers:{'content-type':'text/plain'}}):fetch(url,options);
    await assert.rejects(verifyStudioOrigin(value.destination,server.url,{fetch:wrong}),/incorrect content type/);
    let pending=0,started=0;
    const interrupted=async(url,options)=>{
      if(url.pathname==='/app.js')return new Response('data',{headers:{'content-type':'text/plain'}});
      started++;pending++;
      return new Promise((resolve,reject)=>{
        const stop=()=>{pending--;reject(options.signal.reason);};
        if(options.signal.aborted)stop();else options.signal.addEventListener('abort',stop,{once:true});
      });
    };
    await assert.rejects(verifyStudioOrigin(value.destination,server.url,{fetch:interrupted}),/incorrect content type/);
    assert(started>0);assert.equal(pending,0,'failed verification left sibling requests running');
    const controller=new AbortController();controller.abort(new Error('cancelled origin'));
    await assert.rejects(verifyStudioOrigin(value.destination,server.url,{signal:controller.signal}),/cancelled origin/);
  }finally{await server?.close();await rm(value.directory,{recursive:true,force:true});}
});

test('deployment budgets measure delivered variants and reject missing or oversized artifacts',()=>{
  const files=['artifacts/studio.vob','wasm/vo_web_bg.wasm','compiler/vo_web_bg.wasm'].flatMap(path=>['','.gz','.br'].map(extension=>({path:path+extension,bytes:1,sha256:'a'.repeat(64)})));
  const candidate={buildSha256:'b'.repeat(64),files};
  const report=studioSiteBudgets(candidate);assert(report.passed);assert.equal(report.images.length,9);assert.equal(report.directory.bytes,9);
  assert.throws(()=>studioSiteBudgets({...candidate,files:files.slice(1)}),/missing/);
  for(const row of report.images){
    const atLimit=files.map(entry=>entry.path===row.path?{...entry,bytes:row.limit}:entry);
    assert(studioSiteBudgets({...candidate,files:atLimit}).passed);
    assert.throws(()=>studioSiteBudgets({...candidate,files:atLimit.map(entry=>entry.path===row.path?{...entry,bytes:entry.bytes+1}:entry)}),/exceeded/);
  }
  assert.throws(()=>studioSiteBudgets({...candidate,files:[...files,{path:'other.bin',bytes:67_000_000}]}),/directory budget exceeded/);
});


test('final-site browser journeys have no native compiler or UI build dependency',async()=>{
  const {build}=await import('./node_modules/esbuild/lib/main.js');
  const {fileURLToPath}=await import('node:url');
  const result=await build({entryPoints:[fileURLToPath(new URL('./studio-contracts.mjs',import.meta.url))],platform:'node',format:'esm',bundle:true,write:false,metafile:true,logLevel:'silent'});
  const inputs=Object.keys(result.metafile.inputs),imports=Object.values(result.metafile.outputs).flatMap(output=>output.imports);
  assert(inputs.length>0&&!inputs.some(path=>path.includes('node_modules/')),'browser verification loads UI build dependencies');
  assert(imports.every(entry=>entry.path.startsWith('node:')&&entry.path!=='node:child_process'),'browser verification loads native tooling');
});


test('network failures retain the requested URL and nested cause',async t=>{
  const value=await fixture();t.after(()=>rm(value.directory,{recursive:true,force:true}));
  await stageStudioSite(value);
  const cause=Object.assign(new Error('connection timed out'),{code:'UND_ERR_CONNECT_TIMEOUT'});
  const failure=new TypeError('fetch failed',{cause});
  await assert.rejects(verifyStudioOrigin(value.destination,'https://volang.dev/',{fetch:async()=>{throw failure;}}),error=>{
    assert.match(error.message,/https:\/\/volang\.dev\//);
    assert.equal(error.cause,failure);
    const details=siteFailureDetails(error,'origin-before');
    assert.equal(details.phase,'origin-before');
    assert.equal(details.causes.at(-1).code,'UND_ERR_CONNECT_TIMEOUT');
    return true;
  });
});

test('failure details stay bounded even for cyclic causes',()=>{
  const error=new Error('x'.repeat(20000));error.cause=error;
  const result=siteFailureDetails(error,'browser-journey');
  assert.equal(result.causes.length,1);
  assert.equal(result.causes[0].message.length,4096);
  assert(result.stack.length<=8192);
});
