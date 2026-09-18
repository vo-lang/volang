import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cp, mkdir, mkdtemp, readFile, rename, rm, writeFile} from 'node:fs/promises';
import {join, resolve} from 'node:path';
import {tmpdir} from 'node:os';
import {pathToFileURL} from 'node:url';
import {root} from './server.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {checkStudio} from './studio-contracts.mjs';
import {checkStudioEditor} from './studio-editor-contracts.mjs';

// Use the actual built distribution, then move it to a fresh directory whose
// parent has no application sources, module workspace or node_modules.
const source = resolve(root,'target/ui-next/studio-distribution');
const report = JSON.parse(await readFile(join(source,'build-report.json')));
const parent = await mkdtemp(join(tmpdir(),'volang-studio-distribution-'));
const copied = join(parent,'copied'), deployed = join(parent,'deployed');
const output = resolve(root,'target/ui-next/studio-distribution-check');
const digest = bytes => createHash('sha256').update(bytes).digest('hex');
let application;
const errors = [], browsers = [];
const results = [];
const selected = process.env.UI_NEXT_BROWSER ? [process.env.UI_NEXT_BROWSER] : ['chromium','firefox','webkit'];
assert(selected.every(name => ['chromium','firefox','webkit'].includes(name)), 'UI_NEXT_BROWSER must be chromium, firefox or webkit');
try {
  await cp(source,copied,{recursive:true});
  await rename(copied,deployed);
  await mkdir(output,{recursive:true});
  for (const artifact of report.artifacts) {
    const bytes = await readFile(join(deployed,artifact.path));
    assert.equal(bytes.length,artifact.bytes,artifact.path);
    assert.equal(digest(bytes),artifact.sha256,artifact.path);
    if (artifact.path.endsWith('.js') || artifact.path.endsWith('.mjs')) assert(!bytes.includes(Buffer.from(root)),`local path embedded in ${artifact.path}`);
  }
  assert(report.browserInputs.every(path => !/inspection|development/.test(path)), 'development implementation bundled');
  assert.equal(report.documents,24);
  const {start} = await import(pathToFileURL(join(deployed,'server/entry.mjs')).href);
  assert.throws(() => start({base:'/nested/'}), /requires deployment/);
  application = await start({executable:compilerPath(), onError:error=>errors.push(error.message)});
  const url = application.url.replace(/\/$/,'');
  const redirect = await fetch(url+'/?backend=vm',{redirect:'manual'});
  assert.equal(redirect.status,307);
  assert.equal(redirect.headers.get('location'),'/studio/gallery?backend=vm');
  for (const path of ['/studio/gallery','/studio/playground','/studio/playground/ui','/studio/docs/state']) {
    const response = await fetch(url+path);
    assert.equal(response.status,200,path);
    const html = await response.text();
    assert.match(html,/data-vo-id=/);
  }
  for (const path of ['/unknown','/studio/missing','/studio/docs/no-such-chapter']) {
    const response = await fetch(url+path);
    assert.equal(response.status,404,path);
    const html=await response.text();
    assert.match(html,/data-vo-id=/);assert.match(html,/<main\b/);
  }
  for (const path of ['/server/app.vob','/studio-assets/missing.js','/compiler/missing.js','/studio-docs/missing.json','/host/ui_dom.js']) {
    const response = await fetch(url+path);
    assert.equal(response.status,404,path);
    assert.match(response.headers.get('content-type'),/^text\/plain/);
  }
  const post = await fetch(url+'/studio/gallery',{method:'POST',body:''});
  assert.equal(post.status,405); assert.equal(post.headers.get('allow'),'GET, HEAD');
  assert.equal((await fetch(url+'/studio/gallery',{method:'HEAD'})).status,200);
  const index = JSON.parse(await readFile(resolve(root,'apps/studio/next/documentation/index.json')));
  const searchResponse=await fetch(url+'/studio-docs/'+index.search.Asset+'?v='+index.search.SHA256);
  assert.equal(searchResponse.status,200);
  assert.equal(digest(Buffer.from(await searchResponse.arrayBuffer())),index.search.SHA256);
  for (const page of index.pages) {
    const response = await fetch(url+'/studio/docs/'+page.ID);
    assert.equal(response.status,200,page.ID);
    const html = await response.text();
    assert(html.includes(`id="${page.HeadingID}"`),page.ID);
    assert(html.includes(`data-document="${page.ID}"`),page.ID);
    assert(!html.includes('Loading this chapter'),page.ID);
    const resource = await fetch(url+'/studio-docs/'+page.Asset+'?v='+page.SHA256);
    assert.equal(digest(Buffer.from(await resource.arrayBuffer())),page.SHA256,page.ID);
  }
  // A missing or malformed deployed chapter reports a request-local error and
  // keeps the server and unrelated pages usable. It never exposes a file path.
  const chapterPath = join(deployed,'public/studio-docs',index.pages[0].Asset);
  const original = await readFile(chapterPath);
  await writeFile(chapterPath,'{"version":999}');
  const failed = await fetch(url+'/studio/docs/'+index.pages[0].ID);
  assert.equal(failed.status,500);
  assert.equal(await failed.text(),'This page could not be rendered.');
  await writeFile(chapterPath,original);
  assert.equal((await fetch(url+'/studio/docs/'+index.pages[0].ID)).status,200);
  assert.equal(errors.length,1); errors.length=0;
  console.log('Relocated Studio: all 24 native chapters, search index, metadata, assets and request failures passed');
  process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
  const engines = await import('../browser/node_modules/playwright/index.mjs');
  for (const name of selected) {
    const browser = await engines[name].launch({headless:true});
    browsers.push(browser);
    const directory = join(output,name); await mkdir(directory,{recursive:true});
    const studio = await checkStudio(browser,url,directory);
    const editor = await checkStudioEditor(browser,url);
    results.push({engine:name,studio,editor});
    await writeFile(join(directory,'report.json'),JSON.stringify({passed:true,delivery:'request-time-ssr',build:report,studio,editor},null,2)+'\n');
    await browser.close();
    console.log(`${name}: deployed Studio VM, SSR, documents, drafts and workers passed`);
  }
  assert.deepEqual(errors,[]);
  await writeFile(join(output,'report.json'),JSON.stringify({passed:true,build:report,relocated:true,nativeChapters:24,searchIndex:true,
    publicAssetDigests:true,requestFailureIsolation:true,results},null,2)+'\n');
} finally {
  await Promise.allSettled(browsers.map(browser=>browser.close()));
  await application?.close();
  await rm(parent,{recursive:true,force:true});
}
