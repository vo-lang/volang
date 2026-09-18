import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cp,mkdir,mkdtemp,readFile,rename,rm,writeFile} from 'node:fs/promises';
import {tmpdir} from 'node:os';
import {join,resolve} from 'node:path';
import {root} from './server.mjs';
import {artifactInventory} from './artifact-inventory.mjs';
import {exportStudio} from './studio-static.mjs';
import {serveFiles} from './static-server.mjs';
import {checkStudio} from './studio-contracts.mjs';
import {checkStudioEditor} from './studio-editor-contracts.mjs';
import {checkStudioLanguageService} from './studio-language-contracts.mjs';

const source=await exportStudio();
const build=JSON.parse(await readFile(join(source,'build-report.json')));
const native=await readFile(join(root,'target/ui-next/studio-distribution/build-report.json'));
assert.equal(build.sourceBuildSha256,createHash('sha256').update(native).digest('hex'));
const parent=await mkdtemp(join(tmpdir(),'studio-static-deployment-'));
const copied=join(parent,'copied'),deployed=join(parent,'deployed');
const output=join(root,'target/ui-next/studio-static-check');
const selected=process.env.UI_NEXT_BROWSER?[process.env.UI_NEXT_BROWSER]:['chromium','firefox','webkit'];
assert(selected.every(name=>['chromium','firefox','webkit'].includes(name)));
const browsers=[],results=[];
let application;
try {
  await cp(source,copied,{recursive:true});await rename(copied,deployed);
  assert.deepEqual((await artifactInventory(deployed)).filter(item=>item.path!=='build-report.json'),build.artifacts);
  assert(build.artifacts.every(item=>!item.path.startsWith('server/') && !item.path.includes('node_modules/')));
  await mkdir(output,{recursive:true});
  const createOrigin=()=>serveFiles(deployed,{notFoundDocument:'404.html'});
  application=await createOrigin();
  const url=application.url.replace(/\/$/,'');
  assert.equal(build.pages.filter(page=>page.status===200).length,build.documents + 5);
  for(const page of build.pages) {
    const response=await fetch(url+page.path,{headers:{accept:'text/html'}});
    assert.equal(response.status,page.status,page.path);
    const html=await response.text();
    assert.match(html,/data-vo-id=/,page.path);
    const title=page.title.replace(/[&<>"']/g,value=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[value]));
    assert(html.includes('<title>'+title+'</title>'),page.path);
    assert.equal(html,await readFile(join(deployed,page.file),'utf8'),page.path);
  }
  const index=JSON.parse(await readFile(join(root,'apps/studio/next/documentation/index.json')));
  const searchResponse=await fetch(url+'/studio-docs/'+index.search.Asset+'?v='+index.search.SHA256);
  assert.equal(searchResponse.status,200);
  assert.equal(createHash('sha256').update(Buffer.from(await searchResponse.arrayBuffer())).digest('hex'),index.search.SHA256);
  for(const page of index.pages) {
    const html=await readFile(join(deployed,'studio/docs',page.ID,'index.html'),'utf8');
    assert(html.includes(`id="${page.HeadingID}"`),page.ID);
    assert(html.includes(`data-document="${page.ID}"`),page.ID);
    assert(!html.includes('Loading this chapter'),page.ID);
  }
  for(const path of ['/server/app.vob','/studio-assets/missing.js','/compiler/missing.js']) {
    const response=await fetch(url+path);assert.equal(response.status,404);assert.match(response.headers.get('content-type'),/^text\/plain/);
  }
  process.env.PLAYWRIGHT_BROWSERS_PATH??=resolve(root,'target/playwright-browsers');
  const engines=await import('../browser/node_modules/playwright/index.mjs');
  for(const name of selected) {
    const browser=await engines[name].launch({headless:true});browsers.push(browser);
    const directory=join(output,name);await mkdir(directory,{recursive:true});
    const page=await browser.newPage();
    const errors=[];page.on('pageerror',error=>errors.push(error.message));
    await page.goto(url+'/?backend=vm#studio-content');
    await page.waitForFunction(()=>window.__studioNext?.ready || window.__studioNext?.error);
    assert.equal(await page.evaluate(()=>window.__studioNext.error),null);
    assert.equal(new URL(page.url()).pathname,'/studio/gallery/');
    assert.equal(new URL(page.url()).search,'?backend=vm');
    assert.equal(new URL(page.url()).hash,'#studio-content');
    await page.reload();
    await page.waitForFunction(()=>window.__studioNext?.ready || window.__studioNext?.error);
    assert.equal(await page.evaluate(()=>window.__studioNext.error),null);
    for (const path of ['/missing','/studio/missing','/studio/docs/no-such-chapter']) {
      const missing=await page.goto(url+path+'?backend=vm');assert.equal(missing.status(),404);
      await page.getByRole('heading',{name:'A little off the path.'}).waitFor();
      assert.equal(await page.getByRole('main').count(),1,'the application owns its missing-page landmark');
      assert.equal(await page.locator('script').count(),0,'shared 404 must not start a route-specific guest');
      assert.equal(await page.locator('#status').count(),0);
    }
    await page.getByRole('link',{name:'Back to the gallery'}).click();
    await page.locator('[data-demo-count]').waitFor();
    await page.waitForFunction(()=>window.__studioNext?.ready || window.__studioNext?.error);
    assert.equal(await page.evaluate(()=>window.__studioNext.error),null);
    assert.deepEqual(errors,[]);await page.close();
    const noScript=await browser.newPage({javaScriptEnabled:false});
    for(const path of ['/studio/gallery','/studio/docs/hello-world','/studio/playground']) {
      assert.equal((await noScript.goto(url+path)).status(),200);
      assert(await noScript.locator('h1').count());
      assert(await noScript.locator('#root').innerText());
    }
    await noScript.close();
    const studio=await checkStudio(browser,url,directory);
    const editor=await checkStudioEditor(browser,url);
    const language=await checkStudioLanguageService(browser,url);
    const result={engine:name,browserVersion:browser.version(),passed:true,studio,editor,language,
      rootRedirect:true,refresh:true,missingPage:true,noScript:true};
    results.push(result);
    await writeFile(join(directory,'report.json'),JSON.stringify(result,null,2)+'\n');
    await browser.close();
    console.log(`${name}: relocated static Studio, VM, ${build.documents + 5} pages, hydration, editor, workers passed`);
  }
  await writeFile(join(output,'report.json'),JSON.stringify({passed:true,build,relocated:true,documents:build.documents,results},null,2)+'\n');
} finally {
  await Promise.allSettled(browsers.map(browser=>browser.close()));
  await application?.close();await rm(parent,{recursive:true,force:true});
}
