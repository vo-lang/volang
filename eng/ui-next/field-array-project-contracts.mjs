import assert from 'node:assert/strict';
import {cp,mkdir,mkdtemp,readFile,rm,writeFile} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {createProject,checkProject,buildProject} from './project.mjs';
import {developProject} from './project-development.mjs';
import {serveFiles} from './static-server.mjs';
import {root} from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
const engines=await import('../browser/node_modules/playwright/index.mjs');
const temporary=await mkdtemp(resolve(root,'target/ui-next/field-array-project-'));
const evidence=resolve(root,'target/ui-next/field-array-project');
await mkdir(evidence,{recursive:true});
let browser,server,development;
const results=[];
const draft='Ada keeps her place 中文';
const ready=async page=>{
  await page.waitForFunction(()=>window.__arrayApp!==undefined);
  assert.equal(await page.evaluate(()=>window.__arrayApp.ready),true);
};
const count=async(page,n)=>page.waitForFunction(n=>document.querySelectorAll('[data-person]').length===n,n);
try {
  const project=await createProject(join(temporary,'People'));
  await cp(resolve(root,'ui/next/examples/field-array/app/app.vo'),join(project,'app/app.vo'));
  await cp(resolve(root,'ui/next/examples/field-array/styles.css'),join(project,'web/app.css'));
  const boot=join(project,'web/boot.js');
  const source=await readFile(boot,'utf8');
  // Exercise both host startup paths with the same production guest. The normal
  // document is real SSR; this test-only switch empties it before a client mount.
  await writeFile(boot,source.replace('const application = mountUi(',`if (new URL(location.href).searchParams.has('client')) {
  document.getElementById('root').replaceChildren();
  document.querySelector('meta[name="ui-next-render"]').content = 'client';
}
window.__arrayStartedWithChildren = document.getElementById('root').childNodes.length;
const application = mountUi(`)+'\nwindow.__arrayApp = application;\n');
  const config=join(project,'ui-next.json'),settings=JSON.parse(await readFile(config,'utf8'));
  settings.document={title:'Keep your people close · Volang UI',description:'A small address book with room to grow.'};
  await writeFile(config,JSON.stringify(settings,null,2)+'\n');
  await checkProject(project);
  console.log('Building the ordinary nested form application');
  const built=await buildProject(project),output=join(evidence,'distribution');
  await rm(output,{recursive:true,force:true});await cp(built,output,{recursive:true});
  const build=JSON.parse(await readFile(join(output,'build-report.json')));
  server=await serveFiles(output,{base:'/people/'});
  for(const engine of ['chromium','firefox','webkit']) {
    browser=await engines[engine].launch({headless:true});
    const native=await browser.newPage({javaScriptEnabled:false});
    await native.goto(server.url);
    await native.locator('#name-initial-0').fill(draft);
    assert.deepEqual(await native.locator('#contacts-form').evaluate(form=>new FormData(form).getAll('people')),['','initial-0','initial-1']);
    await native.close();
    for(const backend of ['vm']) for(const mode of ['client','hydrate']) {
      console.log(`Nested form: ${engine} ${backend} ${mode}`);
      const page=await browser.newPage({viewport:{width:1060,height:1100}}),errors=[];
      page.on('pageerror',error=>errors.push(error.message));
      let release=()=>{};
      if(mode==='hydrate') {
        const gate=new Promise(resolve=>{release=resolve;});
        await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
      }
      try {
        await page.goto(`${server.url}?backend=${backend}${mode==='client'?'&client=1':''}`,{waitUntil:'commit'});
        const input=page.locator('#name-initial-0');
        if(mode==='hydrate') {
          await input.fill(draft);
          await input.evaluate(input=>{window.earlyInput=input;input.setSelectionRange(2,7,'backward');});
          release();await ready(page);
          assert.equal(await page.evaluate(()=>window.earlyInput===document.getElementById('name-initial-0')),true);
          assert.deepEqual(await input.evaluate(input=>[input.value,input===document.activeElement,input.selectionStart,input.selectionEnd,input.selectionDirection]),[draft,true,2,7,'backward']);
          assert(await page.evaluate(()=>window.__arrayStartedWithChildren)>0);
        } else {
          await ready(page);assert.equal(await page.evaluate(()=>window.__arrayStartedWithChildren),0);
          await input.fill(draft);
        }
        await page.waitForFunction(()=>document.querySelector('[data-dirty]')?.textContent==='Unsaved changes');
        const first=page.locator('[data-person="initial-0"]');
        await first.getByRole('button',{name:'Check contact',exact:true}).click();
        await page.waitForFunction(()=>document.querySelector('[data-person="initial-0"] [data-checks]')?.textContent==='1 checks');
        await input.evaluate(input=>{window.retainedInput=input;input.focus();input.setSelectionRange(2,7,'backward');});
        await input.press('Alt+ArrowDown');
        await page.waitForFunction(()=>document.querySelector('[data-person]:last-of-type')?.getAttribute('data-person')==='initial-0');
        assert.deepEqual(await input.evaluate(input=>[input===window.retainedInput,input===document.activeElement,input.selectionStart,input.selectionEnd,input.selectionDirection]),[true,true,2,7,'backward']);
        assert.equal(await first.locator('[data-checks]').textContent(),'1 checks');
        await input.fill('');await input.press('Tab');
        await first.getByText('Enter a name for this person.',{exact:true}).waitFor();
        await input.press('Alt+ArrowUp');
        await page.waitForFunction(()=>document.querySelector('[data-person]')?.getAttribute('data-person')==='initial-0');
        assert.equal(await first.getByText('Enter a name for this person.',{exact:true}).count(),1);
        assert.equal(await page.locator('[data-person="initial-1"]').getByText('Enter a name for this person.',{exact:true}).count(),0);
        await input.press('Alt+ArrowDown');
        await page.waitForFunction(()=>document.querySelector('[data-person]:last-of-type')?.getAttribute('data-person')==='initial-0');
        await input.fill(draft);
        await first.getByText('Enter a name for this person.',{exact:true}).waitFor({state:'detached'});
        assert.deepEqual(await page.locator('#contacts-form').evaluate(form=>new FormData(form).getAll('people')),['','initial-1','initial-0']);
        await input.fill('');await input.press('Tab');
        await first.getByText('Enter a name for this person.',{exact:true}).waitFor();
        const errorID=await input.getAttribute('aria-describedby');
        assert.equal(await input.getAttribute('aria-invalid'),'true');
        assert(errorID?.includes('name-initial-0-error'));
        await input.fill(draft);
        await first.getByRole('button',{name:'Add phone',exact:true}).click();
        await page.waitForFunction(()=>document.querySelectorAll('[data-person="initial-0"] [data-phone]').length===2);
        const addedPhone=first.locator('[data-phone]').last();
        await addedPhone.getByLabel('Phone number',{exact:true}).fill('+44 7000 123456');
        const phoneKey=await addedPhone.getAttribute('data-phone');
        await first.locator('[data-phone]').first().getByRole('button',{name:'Remove phone',exact:true}).click();
        await page.waitForFunction(()=>document.querySelectorAll('[data-person="initial-0"] [data-phone]').length===1);
        assert.equal(await first.getByLabel('Phone number',{exact:true}).inputValue(),'+44 7000 123456');
        assert.deepEqual(await page.locator('#contacts-form').evaluate(form=>new FormData(form).getAll('people[initial-0].phones')),['',phoneKey]);
        await page.getByRole('button',{name:'Save contacts',exact:true}).click();
        await page.getByText('Saving contacts…',{exact:true}).waitFor();
        await page.getByRole('button',{name:'Add person',exact:true}).click();await count(page,3);
        await page.waitForFunction(()=>document.getElementById('save-status').textContent==='');
        const addedPerson=page.locator('[data-person]').last(),addedKey=await addedPerson.getAttribute('data-person');
        await addedPerson.getByLabel('Full name',{exact:true}).fill('Katherine Johnson');
        await page.getByRole('button',{name:'Save contacts',exact:true}).click();
        await page.getByText('Saved 3 people.',{exact:true}).waitFor();
        await page.waitForFunction(()=>document.querySelector('[data-dirty]').textContent==='All changes saved');
        await addedPerson.getByRole('button',{name:'Remove person',exact:true}).evaluate(button=>{
          const input=button.closest('[data-person]').querySelector('input:not([type="hidden"])');
          button.click();
          input.value='A late edit to the removed row';
          input.dispatchEvent(new Event('input',{bubbles:true}));
          input.dispatchEvent(new FocusEvent('blur'));
        });
        await count(page,2);
        await page.getByRole('button',{name:'Discard edits',exact:true}).click();await count(page,3);
        assert.equal(await page.locator(`[data-person="${addedKey}"]`).getByLabel('Full name',{exact:true}).inputValue(),'Katherine Johnson');
        if(engine==='chromium' && backend==='vm' && mode==='hydrate') {
          await page.screenshot({path:join(evidence,'desktop.png'),fullPage:true});
          await page.setViewportSize({width:390,height:844});
          assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true);
          await page.screenshot({path:join(evidence,'mobile.png'),fullPage:true});
        }
        await page.getByRole('button',{name:'Reset original',exact:true}).click();await count(page,2);
        assert.equal(await page.locator('#name-initial-0').inputValue(),'Ada Lovelace');
        assert.equal(await first.getByLabel('Phone number',{exact:true}).inputValue(),'+44 20 7946 0000');
        await page.evaluate(async()=>{window.__arrayApp.close();await window.__arrayApp.done;});
        assert.equal(await page.locator('#root').textContent(),'');assert.deepEqual(errors,[]);
        results.push({engine,browserVersion:browser.version(),backend,mode,passed:true});
      } catch(error) {await page.screenshot({path:join(evidence,`failure-${engine}-${backend}-${mode}.png`),fullPage:true});throw error;}
      finally{release();await page.close();}
    }
    await browser.close();browser=undefined;
  }
  await server.close();server=undefined;
  console.log('Nested form: real source reload with an added row');
  development=await developProject(project);
  browser=await engines.chromium.launch({headless:true});
  const page=await browser.newPage(),errors=[];
  page.on('pageerror',error=>errors.push(error.message));
  const connected=page.waitForResponse(response=>response.url().endsWith('/__ui-next/events'));
  await page.goto(development.url);await connected;await ready(page);
  await page.getByRole('button',{name:'Add person',exact:true}).click();await count(page,3);
  const added=page.locator('[data-person]').last(),key=await added.getAttribute('data-person');
  await added.getByLabel('Full name',{exact:true}).fill(draft);
  await added.getByLabel('Full name',{exact:true}).evaluate(input=>{input.setSelectionRange(1,6,'backward');window.beforeReload=document.getElementById('root');});
  const sourcePath=join(project,'app/app.vo'),appSource=await readFile(sourcePath,'utf8');
  await writeFile(sourcePath,appSource.replace('Keep your people close.','A little more room for everyone.'));
  await page.getByRole('heading',{name:'A little more room for everyone.',exact:true}).waitFor();
  await page.waitForFunction(()=>document.getElementById('root')!==window.beforeReload);
  assert.deepEqual(await page.locator(`[data-person="${key}"]`).getByLabel('Full name',{exact:true}).evaluate(input=>[input.value,input===document.activeElement,input.selectionStart,input.selectionEnd,input.selectionDirection]),[draft,true,1,6,'backward']);
  await page.getByRole('button',{name:'Add person',exact:true}).click();await count(page,4);
  assert.notEqual(await page.locator('[data-person]').last().getAttribute('data-person'),key);
  assert.deepEqual(errors,[]);
  await browser.close();browser=undefined;await development.close();development=undefined;
  await writeFile(join(evidence,'report.json'),JSON.stringify({passed:true,build,results,
    clientMode:'test bootstrap clears the server root before invoking the ordinary client mount',
    development:{passed:true,retainedAddedRow:true,retainedDraftFocusSelection:true},
    contracts:['native-form-order-and-empty-marker','real-ssr-early-input','client-empty-root',
      'stable-row-component-and-dom-identity','focused-keyboard-move','field-error-association',
      'nested-append-remove','structural-cancellation','saved-shape-revert','original-reset','root-close']},null,2)+'\n');
  console.log('Nested form passed: 12 browser modes and real source reload');
}finally{await browser?.close();await development?.close();await server?.close();await rm(temporary,{recursive:true,force:true});}
