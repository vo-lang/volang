import assert from 'node:assert/strict';
import {cp,mkdir,mkdtemp,readFile,rm,writeFile} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {createProject,checkProject,buildProject} from './project.mjs';
import {developProject} from './project-development.mjs';
import {serveFiles} from './static-server.mjs';
import {root} from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
const engines=await import('../browser/node_modules/playwright/index.mjs');
const temporary=await mkdtemp(resolve(root,'target/ui-next/portal-project-'));
const evidence=resolve(root,'target/ui-next/portal-project');
await mkdir(evidence,{recursive:true});
let browser,server,development;
const results=[];
const draft='Keep this idea and its selection 中文';
const ready=async page=>{
  await page.waitForFunction(()=>window.__portalApp!==undefined);
  assert.equal(await page.evaluate(()=>window.__portalApp.ready),true);
};
try {
  const project=await createProject(join(temporary,'Room for an idea'));
  await cp(resolve(root,'ui/next/examples/portal/app/app.vo'),join(project,'app/app.vo'));
  await cp(resolve(root,'ui/next/examples/portal/styles.css'),join(project,'web/app.css'));
  const boot=join(project,'web/boot.js');
  await writeFile(boot,(await readFile(boot,'utf8')).replace('const application = mountUi(',`if (new URL(location.href).searchParams.has('client')) {
  document.getElementById('root').replaceChildren();
  document.querySelector('meta[name="ui-next-render"]').content = 'client';
}
window.__portalStartedWithChildren = document.getElementById('root').childNodes.length;
const application = mountUi(`)+'\nwindow.__portalApp = application;\n');
  const config=join(project,'ui-next.json');
  const settings=JSON.parse(await readFile(config,'utf8'));
  settings.document={title:'Give your idea some room · Volang UI',description:'A note moves between spaces while retaining its state.'};
  await writeFile(config,JSON.stringify(settings,null,2)+'\n');
  await checkProject(project);
  console.log('Building the ordinary Portal application');
  const built=await buildProject(project);
  const output=join(evidence,'distribution');
  await rm(output,{recursive:true,force:true});await cp(built,output,{recursive:true});
  const build=JSON.parse(await readFile(join(output,'build-report.json')));
  server=await serveFiles(output,{base:'/ideas/'});
  for(const engine of ['chromium','firefox','webkit']) {
    browser=await engines[engine].launch({headless:true});
    const native=await browser.newPage({javaScriptEnabled:false});
    await native.goto(server.url);
    await native.locator('#note-source #portal-note').fill(draft);
    assert.equal(await native.locator('#portal-note').inputValue(),draft);
    await native.close();
    for(const backend of ['vm']) for(const mode of ['client','hydrate']) {
      console.log(`Portal application: ${engine} ${backend} ${mode}`);
      const page=await browser.newPage({viewport:{width:1100,height:900}}),errors=[];
      page.on('pageerror',error=>errors.push(error.message));
      let release=()=>{};
      if(mode==='hydrate') {
        const gate=new Promise(resolve=>{release=resolve;});
        await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
      }
      try {
        await page.goto(`${server.url}?backend=${backend}${mode==='client'?'&client=1':''}`,{waitUntil:'commit'});
        const input=page.locator('#portal-note');
        if(mode==='hydrate') {
          await page.locator('#note-source #portal-note').fill(draft);
          await input.evaluate(input=>{window.earlyInput=input;input.setSelectionRange(2,8,'backward');});
          release();await ready(page);
          await page.waitForFunction(()=>document.querySelector('#inbox #portal-note')!==null);
          assert.equal(await page.evaluate(()=>window.earlyInput===document.getElementById('portal-note')),true);
          assert.deepEqual(await input.evaluate(input=>[input.value,input===document.activeElement,input.selectionStart,input.selectionEnd,input.selectionDirection]),[draft,true,2,8,'backward']);
        } else {
          await ready(page);
          assert.equal(await page.evaluate(()=>window.__portalStartedWithChildren),0);
          await page.locator('#inbox #portal-note').fill(draft);
        }
        await page.waitForFunction(draft=>document.querySelector('[data-draft]')?.textContent===draft,draft);
        await page.getByRole('button',{name:'Save note',exact:true}).click();
        await page.waitForFunction(()=>document.querySelector('[data-saved]')?.textContent==='1 saves');
        await input.evaluate(input=>{window.retainedInput=input;input.focus();input.setSelectionRange(2,8,'backward');});
        await input.press('Alt+ArrowRight');
        await page.locator('#pinboard #portal-note').waitFor();
        assert.deepEqual(await input.evaluate(input=>[input===window.retainedInput,input===document.activeElement,input.selectionStart,input.selectionEnd,input.selectionDirection]),[true,true,2,8,'backward']);
        assert.equal(await page.locator('[data-notebook]').textContent(),'My notebook');
        assert.equal(await page.locator('[data-saved]').textContent(),'1 saves');
        await input.press('Alt+ArrowLeft');await page.locator('#inbox #portal-note').waitFor();
        await page.getByRole('button',{name:'Toggle inbox',exact:true}).click();
        await page.locator('#note-source #portal-note').waitFor();
        await page.getByRole('button',{name:'Toggle inbox',exact:true}).click();
        await page.locator('#inbox #portal-note').waitFor();
        assert.equal(await input.inputValue(),draft);
        await page.getByRole('button',{name:'Open focus room',exact:true}).click();
        await page.waitForFunction(()=>document.querySelector('dialog')?.matches(':modal'));
        await page.locator('#focus-room #portal-note').fill(draft+' in the focus room');
        await input.press('Escape');
        await page.waitForFunction(()=>!document.querySelector('dialog')?.open && document.querySelector('#inbox #portal-note')!==null);
        assert.equal(await input.inputValue(),draft+' in the focus room');
        assert.equal(await page.locator('[data-notebook]').textContent(),'My notebook');
        if(engine==='chromium' && backend==='vm' && mode==='hydrate') {
          await page.screenshot({path:join(evidence,'desktop.png'),fullPage:true});
          await page.setViewportSize({width:390,height:844});
          assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true);
          await page.screenshot({path:join(evidence,'mobile.png'),fullPage:true});
        }
        await page.getByRole('button',{name:'Toggle note',exact:true}).click();
        await input.waitFor({state:'detached'});
        assert.equal(await page.evaluate(()=>window.retainedInput.isConnected),false);
        await page.getByRole('button',{name:'Toggle note',exact:true}).click();
        await page.locator('#inbox #portal-note').waitFor();
        assert.equal(await page.locator('[data-saved]').textContent(),'0 saves');
        await page.evaluate(async()=>{window.__portalApp.close();await window.__portalApp.done;});
        assert.equal(await page.locator('#root').textContent(),'');
        assert.deepEqual(errors,[]);
        results.push({engine,browserVersion:browser.version(),backend,mode,passed:true});
      }catch(error){await page.screenshot({path:join(evidence,`failure-${engine}-${backend}-${mode}.png`),fullPage:true});throw error;}
      finally{release();await page.close();}
    }
    await browser.close();browser=undefined;
  }
  await server.close();server=undefined;
  console.log('Portal application: real source reload with relocated input');
  development=await developProject(project);
  browser=await engines.chromium.launch({headless:true});
  const page=await browser.newPage(),errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  const connected=page.waitForResponse(response=>response.url().endsWith('/__ui-next/events'));
  await page.goto(development.url);await connected;await ready(page);
  await page.locator('#inbox #portal-note').fill(draft);
  await page.locator('#portal-note').press('Alt+ArrowRight');
  await page.locator('#pinboard #portal-note').waitFor();
  await page.locator('#portal-note').evaluate(input=>{input.setSelectionRange(1,5,'backward');window.beforeReload=document.getElementById('root');});
  const sourcePath=join(project,'app/app.vo'),source=await readFile(sourcePath,'utf8');
  await writeFile(sourcePath,source.replace('Give your idea some room.','A fresh space for your idea.'));
  await page.getByRole('heading',{name:'A fresh space for your idea.',exact:true}).waitFor();
  await page.waitForFunction(()=>document.getElementById('root')!==window.beforeReload);
  const input=page.locator('#pinboard #portal-note');
  await input.waitFor();
  assert.deepEqual(await input.evaluate(input=>[input.value,input===document.activeElement,input.selectionStart,input.selectionEnd,input.selectionDirection]),[draft,true,1,5,'backward']);
  await input.fill(draft+' after reload');
  await page.waitForFunction(draft=>document.querySelector('[data-draft]')?.textContent===draft,draft+' after reload');
  assert.equal(await page.locator('[data-notebook]').textContent(),'My notebook');
  assert.deepEqual(errors,[]);
  await browser.close();browser=undefined;
  await development.close();development=undefined;
  await writeFile(join(evidence,'report.json'),JSON.stringify({passed:true,build,results,
    clientMode:'test bootstrap clears the server root before invoking the ordinary client mount',
    development:{passed:true,retainedDestination:true,retainedDraftFocusSelection:true},
    contracts:['native-inline-ssr','early-input-adoption','retained-component-context-and-state',
      'keyboard-placement-and-selection','missing-target-fallback','native-modal-composition','owner-disposal','root-close']},null,2)+'\n');
  console.log('Portal application passed: 12 browser modes and real source reload');
}finally{await browser?.close();await development?.close();await server?.close();await rm(temporary,{recursive:true,force:true});}
