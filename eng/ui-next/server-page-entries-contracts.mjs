import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {cp,mkdir,mkdtemp,readFile,rm,writeFile} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {createProject,checkProject,buildProject,execute} from './project.mjs';
import {developProject} from './project-development.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {root} from './server.mjs';
import {decodePage} from './server-response.mjs';
import {renderOutput} from './prerender.mjs';
import {serverProtocol} from './server-request.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
const browsers=await import('../browser/node_modules/playwright/index.mjs');
const temporary=await mkdtemp(resolve(root,'target/ui-next/server-page-entries-'));
const evidence=resolve(root,'target/ui-next/server-page-entries');
await mkdir(evidence,{recursive:true});
let server,browser,development;
const results=[],errors=[];
const digest=bytes=>createHash('sha256').update(bytes).digest('hex');
const ready=async page=>{await page.waitForFunction(()=>window.__entryApp!==undefined);assert.equal(await page.evaluate(()=>window.__entryApp.ready),true);};
const imagePath=url=>new URL(url).pathname.match(/\/assets\/(?:entries\/[^/]+\/)?app\.vob$/)?.[0];
try {
  const project=await createProject(join(temporary,'Request pages'),{template:'pages'});
  const module=(await readFile(join(project,'vo.mod'),'utf8')).match(/^module = "([^"]+)"/m)[1];
  const configPath=join(project,'ui-next.json'),config=JSON.parse(await readFile(configPath));
  config.serverEntry='server';delete config.prerenderPages;delete config.prerenderEntry;delete config.pageEntries.notes.prerenderEntry;
  await writeFile(configPath,JSON.stringify(config,null,2)+'\n');
  const homePath=join(project,'app/app.vo'),home=await readFile(homePath,'utf8');
  await writeFile(homePath,home.replace('.Attr("href", "notes/")','.Attr("href", "notes/" + initial)'));
  const boot=join(project,'web/boot.js');await writeFile(boot,await readFile(boot,'utf8')+'\nwindow.__entryApp=application;\n');
  await mkdir(join(project,'server'));
  await writeFile(join(project,'server/main.vo'),`package main
import (
  "encoding/json"
  "net/url"
  "strings"
  home "${module}/app"
  notes "${module}/notes/app"
  "github.com/vo-lang/ui/next/server"
)
func main() {
  server.Run(func(request server.Request) (server.Page, error) {
    location, err := url.Parse(request.URL)
    if err != nil { return server.Page{}, err }
    path := strings.TrimPrefix(location.Path, request.BasePath)
    backend := location.Query().Get("backend")
    suffix := ""
    if backend == "vm" { suffix = "?backend=" + backend }
    if path == "api" { return server.JSON(200, "Request data") }
    if path == "" { return server.Page{Render: home.View, InitialData: suffix, Title: "A request at home"}, nil }
    text := location.Query().Get("text")
    if text == "" { text = "From this request: " + path }
    data, err := json.Marshal(struct { Note string \`json:"note"\`; Home string \`json:"home"\` }{Note: text, Home: request.BasePath + suffix})
    if err != nil { return server.Page{}, err }
    entry := "notes"
    if path == "unknown-entry" { entry = "missing" }
    if path == "invalid-entry" { entry = "../notes" }
    return server.Page{Render: notes.View, Entry: entry, InitialData: string(data), Title: "A request in the notebook"}, nil
  })
}
`);
  const env={...process.env,VOWORK:join(project,'vo.work')};
  await execute(compilerPath(),['fmt',project],{cwd:project,env});await checkProject(project);
  console.log('Building two request-selected client images and one native server');
  const built=await buildProject(project),output=join(evidence,'distribution');
  await rm(output,{recursive:true,force:true});await cp(built,output,{recursive:true});
  const build=JSON.parse(await readFile(join(output,'build-report.json')));
  assert.equal(build.server.requestProtocol,4);assert.deepEqual(build.entries.map(entry=>entry.id),['default','notes']);
  assert.deepEqual(JSON.parse(await readFile(join(output,'server/entries.json'))),{version:1,entries:['default','notes']});
  const homeImage=await readFile(join(output,'public/assets/app.vob')),notesImage=await readFile(join(output,'public/assets/entries/notes/app.vob'));
  assert(homeImage.includes(Buffer.from('pages.Home'))&&!homeImage.includes(Buffer.from('pages.Notebook')));
  assert(notesImage.includes(Buffer.from('pages.Notebook'))&&!notesImage.includes(Buffer.from('pages.Home')));
  const jit=decodePage(await renderOutput(compilerPath(),['run',join(output,'server/app.vob'),'--mode=jit'],{
    cwd:output,env:{...process.env,VOWORK:'off'},
    input:JSON.stringify({version:serverProtocol,request:{method:'GET',url:'/request-pages/notes/jit',basePath:'/request-pages/',headers:{},body:''}}),
  }));
  assert.equal(jit.entry,'notes');assert(jit.html.includes('From this request: notes/jit'));
  const {start}=await import(pathToFileURL(join(output,'server/entry.mjs')).href);
  server=await start({executable:compilerPath(),base:'/request-pages/',onError:error=>errors.push(error.message)});
  const json=await fetch(server.url+'api');assert.equal(await json.json(),'Request data');
  assert.equal((await fetch(server.url+'unknown-entry')).status,500);
  assert(errors.pop().includes('undeclared page entry'));
  assert.equal((await fetch(server.url+'invalid-entry')).status,500);
  assert(errors.pop().includes('invalid client page entry'));
  for(const engine of ['chromium','firefox','webkit']) {
    browser=await browsers[engine].launch({headless:true});
    const native=await browser.newPage({javaScriptEnabled:false});
    for(const path of ['','notes/morning']) {
      await native.goto(server.url+path);
      assert.equal(await native.locator('meta[name="ui-next-entry"]').getAttribute('content'),path?'notes':'default');
      await native.getByRole('heading',{name:path?'A little room to write.':'An idea begins here.',exact:true}).waitFor();
    }
    await native.close();
    for(const backend of ['vm'])for(const selected of ['default','notes']) {
      console.log(`Request pages: ${engine} ${backend} ${selected}`);
      const page=await browser.newPage(),pageErrors=[],requests=[];
      page.on('pageerror',error=>pageErrors.push(error.message));page.on('request',request=>{const path=imagePath(request.url());if(path)requests.push(path);});
      let release=()=>{};
      if(selected==='notes') {
        const gate=new Promise(resolve=>{release=resolve;});
        await page.route('**/assets/entries/notes/app.*',async route=>{await gate;await route.continue();});
      }
      try {
        await page.goto(`${server.url}${selected==='notes'?'notes/morning':''}?backend=${backend}&text=${encodeURIComponent('From the server 中文')}`,{waitUntil:'commit'});
        if(selected==='notes') {
          await page.locator('#note').fill('Written before activation 中文');
          await page.locator('#note').evaluate(input=>{window.earlyNote=input;input.setSelectionRange(2,7,'backward');});
          release();await ready(page);
          assert.deepEqual(await page.locator('#note').evaluate(input=>[input===window.earlyNote,input.value,input.selectionStart,input.selectionEnd,input.selectionDirection]),[true,'Written before activation 中文',2,7,'backward']);
        }else{await ready(page);await page.getByRole('button',{name:'Take a step',exact:true}).click();await page.waitForFunction(()=>document.querySelector('[data-count]').textContent==='1 little steps');}
        const image=`/assets/${selected==='notes'?'entries/notes/':''}app.vob`;
        assert.deepEqual(requests,[image]);
        await page.getByRole('link',{name:selected==='notes'?'Back to the beginning':'Open your notebook',exact:true}).click();
        await ready(page);
        const next=selected==='notes'?'default':'notes';
        assert.equal(await page.locator('meta[name="ui-next-entry"]').getAttribute('content'),next);
        if(next==='notes') {
          await page.locator('#note').fill('A request-selected notebook');
          await page.getByRole('button',{name:'Keep this thought',exact:true}).click();
          await page.waitForFunction(()=>document.querySelector('[data-saved]').textContent==='A request-selected notebook');
        }else{await page.getByRole('button',{name:'Take a step',exact:true}).click();await page.waitForFunction(()=>document.querySelector('[data-count]').textContent==='1 little steps');}
        assert(requests.every(path=>path.endsWith('.vob')));
        assert(requests.includes(`/assets/${next==='notes'?'entries/notes/':''}app.vob`));
        await page.goBack();await ready(page);
        assert.equal(await page.locator('meta[name="ui-next-entry"]').getAttribute('content'),selected);
        await page.evaluate(async()=>{window.__entryApp.close();await window.__entryApp.done;});
        assert.equal(await page.locator('#root').textContent(),'');assert.deepEqual(pageErrors,[]);
        results.push({engine,browserVersion:browser.version(),backend,entry:selected,passed:true,images:[...new Set(requests)]});
      }finally{release();await page.close();}
    }
    await browser.close();browser=undefined;
  }
  await server.close();server=undefined;assert.deepEqual(errors,[]);
  // Failed source/template builds leave every previous distributed byte intact.
  const preserve=async()=>{for(const artifact of build.artifacts)assert.equal(digest(await readFile(join(built,artifact.path))),artifact.sha256,artifact.path);};
  const notePath=join(project,'notes/app/app.vo'),noteSource=await readFile(notePath,'utf8');
  await writeFile(notePath,noteSource+'\nfunc broken(\n');
  await assert.rejects(buildProject(project),/parse error/);await preserve();await writeFile(notePath,noteSource);
  const htmlPath=join(project,'web/index.html'),html=await readFile(htmlPath,'utf8');
  await writeFile(htmlPath,html.replace('<!--ui-next:entry-->','default'));
  await assert.rejects(buildProject(project),/Named page entries require/);await preserve();await writeFile(htmlPath,html);
  console.log('Request pages: named-image source reload on an immutable server generation');
  development=await developProject(project);browser=await browsers.chromium.launch({headless:true});
  const page=await browser.newPage(),images=[];page.on('request',request=>{const path=imagePath(request.url());if(path)images.push(path);});
  const connected=page.waitForResponse(response=>response.url().endsWith('/__ui-next/events'));
  await page.goto(new URL('notes/morning?backend=vm',development.url).href);await connected;await ready(page);
  await page.locator('#note').fill('Keep this server-page draft 中文');
  await page.locator('#note').evaluate(input=>{input.setSelectionRange(1,6,'backward');window.beforeEntryReload=document.getElementById('root');});
  await writeFile(notePath,noteSource.replace('A little room to write.','A fresh request, a familiar draft.'));
  await page.getByRole('heading',{name:'A fresh request, a familiar draft.',exact:true}).waitFor();
  await page.waitForFunction(()=>window.beforeEntryReload!==document.getElementById('root'));
  assert.deepEqual(await page.locator('#note').evaluate(input=>[input.value,input===document.activeElement,input.selectionStart,input.selectionEnd,input.selectionDirection]),['Keep this server-page draft 中文',true,1,6,'backward']);
  assert(images.length>=2&&images.every(path=>path==='/assets/entries/notes/app.vob'));
  await browser.close();browser=undefined;await development.close();development=undefined;
  await writeFile(join(evidence,'report.json'),JSON.stringify({passed:true,build,results,serverProtocol:4,nativeJit:true,
    noJavaScriptPages:6,development:{passed:true,namedImageOnly:true,preservedDraftFocusSelection:true},
    contracts:['declared-client-image-manifest','independent-client-images','same-request-html-data-entry',
      'unknown-entry-fails-before-output','native-html','early-input-adoption','only-selected-image',
      'document-navigation-and-back','backend-consistent-links','close','atomic-failed-build','named-entry-source-reload']},null,2)+'\n');
  console.log('Request-selected pages passed: 12 browser combinations and named server reload');
}finally{await browser?.close();await development?.close();await server?.close();await rm(temporary,{recursive:true,force:true});}
