import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cp,mkdir,mkdtemp,readFile,rename,rm,writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join,resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {root} from './server.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {serveFiles} from './static-server.mjs';
import {instrumentRecovery,seedRecovery,snapshot} from './studio-recovery-contracts.mjs';

const source=join(root,'target/ui-next/studio-static');
const build=JSON.parse(await readFile(join(source,'build-report.json')));
const nativePath=join(root,'target/ui-next/studio-distribution');
const nativeBytes=await readFile(join(nativePath,'build-report.json'));
assert.equal(build.sourceBuildSha256,createHash('sha256').update(nativeBytes).digest('hex'));
const worker=await readFile(new URL('./fixtures/studio-legacy-worker.js',import.meta.url),'utf8');
const fixture=JSON.parse(await readFile(new URL('./fixtures/studio-legacy-worker.json',import.meta.url)));
assert.equal(createHash('sha256').update(worker).digest('hex'),fixture.fixtureSha256);
assert.equal(build.redirects.length,build.documents + 14);
const {start}=await import(pathToFileURL(join(nativePath,'server/entry.mjs')).href);
const native=await start({executable:compilerPath()});
try {
  const retirement=await fetch(native.url+'service-worker.js');
  assert.equal(retirement.status,200);assert.match(retirement.headers.get('content-type'),/javascript/);
  assert.equal(await retirement.text(),await readFile(new URL('../../apps/studio/next/legacy-worker.js',import.meta.url),'utf8'));
  for(const redirect of build.redirects) {
    const response=await fetch(native.url+redirect.path.slice(1)+'?backend=vm',{redirect:'manual'});
    assert.equal(response.status,307,redirect.path);
    assert.equal(response.headers.get('location'),redirect.to.slice(0,-1)+'?backend=vm',redirect.path);
  }
  for(const method of ['POST','PUT','PATCH','DELETE','OPTIONS']) {
    const response=await fetch(native.url+'docs/hello-world',{method,body:''});
    assert.equal(response.status,405);assert.equal(response.headers.get('allow'),'GET, HEAD');
  }
  const head=await fetch(native.url+'docs/hello-world?backend=vm',{method:'HEAD',redirect:'manual'});
  assert.equal(head.status,307);assert.equal(await head.text(),'');
} finally {await native.close();}
console.log('Studio upgrade: native redirects and read-only methods passed');

const selected=process.env.UI_NEXT_BROWSER?[process.env.UI_NEXT_BROWSER]:['chromium','firefox','webkit'];
assert(selected.every(name=>['chromium','firefox','webkit'].includes(name)));
process.env.PLAYWRIGHT_BROWSERS_PATH??=resolve(root,'target/playwright-browsers');
const engines=await import('../browser/node_modules/playwright/index.mjs');
const output=join(root,'target/ui-next/studio-upgrade-check');await mkdir(output,{recursive:true});
const results=[];
async function ready(page) {
  await page.waitForFunction(()=>window.__studioNext?.ready || window.__studioNext?.error);
  assert.equal(await page.evaluate(()=>window.__studioNext.error),null);
}
for(const name of selected) {
  const temporary=await mkdtemp(join(tmpdir(),'studio-upgrade-'+name+'-'));
  const site=join(temporary,'site'),candidate=join(temporary,'candidate');
  let server,context;
  try {
    await mkdir(join(site,'offline'),{recursive:true});await mkdir(join(site,'other'));
    const old='<!doctype html><title>Previous Studio fixture</title><h1>Previous Studio</h1><textarea aria-label="Open draft"></textarea>';
    for(const file of ['index.html','404.html','offline/index.html']) await writeFile(join(site,file),old);
    await writeFile(join(site,'service-worker.js'),worker);
    await writeFile(join(site,'other/service-worker.js'),"self.addEventListener('install',event=>event.waitUntil(self.skipWaiting()));");
    await cp(source,candidate,{recursive:true});
    server=await serveFiles(site,{notFoundDocument:'404.html'});
    context=await engines[name].launchPersistentContext(join(temporary,'profile'),{headless:true,viewport:{width:1280,height:900}});
    const browserVersion=context.browser().version();
    const pendingRequests=new Set();
    context.on('request',request=>pendingRequests.add(request));
    context.on('requestfinished',request=>pendingRequests.delete(request));
    context.on('requestfailed',request=>pendingRequests.delete(request));
    const errors=[];context.on('page',page=>page.on('pageerror',error=>errors.push(error.message)));
    const oldPage=await context.newPage();await instrumentRecovery(oldPage);await oldPage.goto(server.url);
    await oldPage.evaluate(async()=>{
      await navigator.serviceWorker.register('/service-worker.js',{scope:'/'});await navigator.serviceWorker.ready;
      await navigator.serviceWorker.register('/other/service-worker.js',{scope:'/other/'});
    });
    await oldPage.waitForFunction(()=>navigator.serviceWorker.controller);
    await oldPage.getByLabel('Open draft').fill('Unsaved source 中文');
    await seedRecovery(oldPage);const before=await snapshot(oldPage);
    await oldPage.evaluate(async()=>{
      localStorage.setItem('legacy-draft','saved draft');
      const other=await caches.open('another-application-cache');await other.put('/other-value',new Response('keep me'));
      window.retired=false;
      navigator.serviceWorker.addEventListener('message',event=>{if(event.data?.type==='volang.studio.retired') window.retired=true;});
      window.workerTransitions=[];
      const registration=await navigator.serviceWorker.getRegistration('/');
      registration.addEventListener('updatefound',()=>{
        const installing=registration.installing;
        window.workerTransitions.push(installing.state);
        installing.addEventListener('statechange',()=>window.workerTransitions.push(installing.state));
      });
    });
    // Same origin, one atomic deployment change, with the previous tab alive.
    await rename(site,join(temporary,'previous'));await rename(candidate,site);
    const page=await context.newPage();
    await page.goto(server.url+'?studio-next&backend=vm');await ready(page);
    console.log(name+': new Studio opened beside the previous tab');
    try {
      await oldPage.waitForFunction(()=>window.retired,undefined,{polling:50});
    } catch(error) {
      console.error(JSON.stringify(await oldPage.evaluate(async()=>({retired:window.retired,transitions:window.workerTransitions,visibility:document.visibilityState,
        controller:navigator.serviceWorker.controller?.scriptURL,registrations:(await navigator.serviceWorker.getRegistrations()).map(value=>({scope:value.scope,active:value.active?.state,waiting:value.waiting?.state})),cacheKeys:await caches.keys()}))));
      console.error('pending requests',[...pendingRequests].map(request=>({url:request.url(),resourceType:request.resourceType()})));
      throw error;
    }
    assert.equal(await oldPage.getByLabel('Open draft').inputValue(),'Unsaved source 中文');
    assert.equal(await oldPage.locator('h1').textContent(),'Previous Studio');
    assert.deepEqual(await snapshot(oldPage),before);
    const retained=await page.evaluate(async()=>({
      root:!!(await navigator.serviceWorker.getRegistration('/')),
      other:!!(await navigator.serviceWorker.getRegistration('/other/')),
      keys:await caches.keys(),draft:localStorage.getItem('legacy-draft'),
      value:await (await (await caches.open('another-application-cache')).match('/other-value')).text(),
    }));
    assert.equal(retained.root,false);assert.equal(retained.other,true);
    assert.deepEqual(retained.keys,['another-application-cache']);
    assert.equal(retained.value,'keep me');assert.equal(retained.draft,'saved draft');
    for(const redirect of build.redirects) {
      const response=await fetch(server.url+redirect.path.slice(1));assert.equal(response.status,200,redirect.path);
      assert((await response.text()).includes(`href="${redirect.to}"`),redirect.path);
    }
    const plain=await context.browser().newContext({javaScriptEnabled:false});
    try {
      const noScript=await plain.newPage();await noScript.goto(server.url+'docs/hello-world');
      await noScript.waitForURL('**/studio/docs/hello-world/');
      await noScript.getByRole('heading',{name:'Hello world',exact:true}).waitFor();
      assert.equal(await noScript.evaluate(()=>window.__studioNext),undefined);
    } finally {await plain.close();}
    await page.goto(server.url+'docs/hello-world?backend=vm#doc-hello-world');await ready(page);
    assert.equal(new URL(page.url()).pathname,'/studio/docs/hello-world/');
    assert.equal(new URL(page.url()).search,'?backend=vm');assert.equal(new URL(page.url()).hash,'#doc-hello-world');
    await page.getByRole('heading',{name:'Hello world',exact:true}).waitFor();
    await page.goto(server.url+'workspace');await ready(page);
    await page.getByRole('button',{name:'Find browser projects',exact:true}).click();
    await page.getByLabel('Browser project',{exact:true}).selectOption('小花园');
    assert.deepEqual(await snapshot(oldPage),before);
    // Shared unknown-page HTML remains readable, including malformed old topics.
    await page.goto(server.url+'studio/docs?topic=..');
    await page.getByRole('heading',{name:'A little off the path.'}).waitFor();
    assert.equal(await page.locator('script').count(),0);
    await page.screenshot({path:join(output,name+'-404.png'),fullPage:true});
    await page.setViewportSize({width:390,height:844});
    assert(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth));
    await page.screenshot({path:join(output,name+'-404-mobile.png'),fullPage:true});
    await page.close();
    const retries=[];
    for(const backend of ['vm']) {
      const retry=await context.newPage();let attempts=0;
      await retry.route('**/artifacts/studio.*',route=>++attempts===1?route.fulfill({status:503,body:'temporary fixture failure'}):route.continue());
      await retry.goto(server.url+'studio/gallery?backend='+backend);
      await retry.getByRole('button',{name:'Reload Studio'}).waitFor();
      assert.equal(await retry.locator('#status details').getAttribute('open'),null);
      if(backend==='vm') await retry.screenshot({path:join(output,name+'-startup-error.png')});
      await Promise.all([retry.waitForNavigation(),retry.getByRole('button',{name:'Reload Studio'}).click()]);
      await ready(retry);assert.equal(attempts,2);assert.equal(await retry.locator('#status').textContent(),'');
      retries.push({backend,passed:true});await retry.close();
    }
    assert.deepEqual(errors,[]);
    results.push({engine:name,browserVersion,passed:true,retries,workerRetired:true,otherRegistrationRetained:true,
      assetCacheRetired:true,otherCacheRetained:true,openDraftRetained:true,projectFilesRetained:true,oldLinks:true,noScriptLegacy:true,unknownTopic:true});
    console.log(`${name}: actual legacy worker upgrade, ${build.redirects.length} redirects, preserved projects/open draft and VM boot retry passed`);
  } finally {await context?.close();await server?.close();await rm(temporary,{recursive:true,force:true});}
}
await writeFile(join(output,'report.json'),JSON.stringify({passed:true,build,fixtureSha256:fixture.fixtureSha256,
  nativeRedirects:true,readOnlyMethods:true,results},null,2)+'\n');
