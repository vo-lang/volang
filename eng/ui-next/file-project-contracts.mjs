import assert from 'node:assert/strict';
import {cp,mkdir,mkdtemp,readFile,rm,writeFile} from 'node:fs/promises';
import {createServer,request as proxyRequest} from 'node:http';
import {join,resolve} from 'node:path';
import {createProject,checkProject,buildProject} from './project.mjs';
import {developProject} from './project-development.mjs';
import {serveFiles} from './static-server.mjs';
import {root} from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
const engines=await import('../browser/node_modules/playwright/index.mjs');
const temporary=await mkdtemp(resolve(root,'target/ui-next/file-project-'));
const evidence=resolve(root,'target/ui-next/file-project');
await mkdir(evidence,{recursive:true});
const received=[],results=[];
let browser,server,gateway,development;

// Test endpoint parses actual HTTP multipart bytes independently of the UI host.
// Static output remains the same immutable ordinary-project distribution.
async function uploadGateway(origin) {
  const listener=createServer(async(request,response)=>{
    if(request.url.split('?')[0]==='/attachments/api/upload') {
      try {
        const chunks=[];let size=0;
        for await(const chunk of request) {size+=chunk.length;if(size>16*1024*1024)throw new Error('fixture upload too large');chunks.push(chunk);}
        assert.match(request.headers['content-type'],/^multipart\/form-data; boundary=/);
        const form=await new Request('http://fixture/upload',{method:request.method,headers:request.headers,body:Buffer.concat(chunks)}).formData();
        const files=[];
        for(const file of form.getAll('attachments')) files.push({name:file.name,type:file.type,hex:Buffer.from(await file.arrayBuffer()).toString('hex')});
        const delivery={title:form.get('title'),files};received.push(delivery);
        const timer=setTimeout(()=>{if(!response.destroyed)response.writeHead(delivery.title==='Reject this' ? 422:200,{'content-type':'application/json'}).end(JSON.stringify({received:files.length}));},350);
        response.once('close',()=>clearTimeout(timer));
      }catch(error){response.writeHead(400,{'content-type':'text/plain'}).end(error.message);}
      return;
    }
    const target=new URL(request.url,origin);
    const upstream=proxyRequest(target,{method:request.method,headers:request.headers},remote=>{
      response.writeHead(remote.statusCode,remote.headers);remote.pipe(response);
    });
    upstream.on('error',()=>{if(!response.headersSent)response.writeHead(502);response.end();});
    response.on('close',()=>upstream.destroy());request.pipe(upstream);
  });
  await new Promise(resolve=>listener.listen(0,'127.0.0.1',resolve));
  return {url:`http://127.0.0.1:${listener.address().port}/attachments/`,close:()=>new Promise(resolve=>{listener.close(resolve);listener.closeAllConnections();})};
}

const ready=async page=>{
  await page.waitForFunction(()=>window.__fileApp!==undefined);
  assert.equal(await page.evaluate(()=>window.__fileApp.ready),true);
};
const textFile={name:'draft-中文.txt',mimeType:'text/plain',buffer:Buffer.from('A little context 中文\n')};
const binaryFile={name:'pixels.bin',mimeType:'application/octet-stream',buffer:Buffer.from([0,1,255,128])};
const pick=async(page,files)=>{
  await page.locator('#attachments').setInputFiles(files);
  await page.waitForFunction(count=>document.querySelectorAll('#chosen-files li').length===count,files.length);
};
try {
  const project=await createProject(join(temporary,'Attachments'));
  await cp(resolve(root,'ui/next/examples/files/app/app.vo'),join(project,'app/app.vo'));
  await cp(resolve(root,'ui/next/examples/files/styles.css'),join(project,'web/app.css'));
  const boot=join(project,'web/boot.js'),source=await readFile(boot,'utf8');
  await writeFile(boot,source.replace('const application = mountUi(',`if(new URL(location.href).searchParams.has('client')) {
  document.getElementById('root').replaceChildren();document.querySelector('meta[name="ui-next-render"]').content='client';
}
window.__fileInitialChildren=document.getElementById('root').childNodes.length;
const application = mountUi(`)+'\nwindow.__fileApp=application;\n');
  const config=join(project,'ui-next.json'),settings=JSON.parse(await readFile(config,'utf8'));
  settings.document={title:'A little context · Volang UI',description:'A small native file delivery.'};
  await writeFile(config,JSON.stringify(settings,null,2)+'\n');
  await checkProject(project);
  console.log('Building ordinary file application');
  const built=await buildProject(project),output=join(evidence,'distribution');
  await rm(output,{recursive:true,force:true});await cp(built,output,{recursive:true});
  const build=JSON.parse(await readFile(join(output,'build-report.json')));
  server=await serveFiles(output,{base:'/attachments/'});gateway=await uploadGateway(server.url);
  for(const engine of ['chromium','firefox','webkit']) {
    browser=await engines[engine].launch({headless:true});
    const native=await browser.newPage({javaScriptEnabled:false});
    await native.goto(gateway.url);await native.getByRole('heading',{name:'Send a little context.'}).waitFor();
    await native.locator('#attachments').setInputFiles(textFile);
    assert.equal(await native.locator('form').evaluate(form=>new FormData(form).get('attachments').name),textFile.name);
    await native.close();
    for(const backend of ['vm'])for(const mode of ['client','hydrate']) {
      console.log(`Files: ${engine} ${backend} ${mode}`);
      const page=await browser.newPage({viewport:{width:1060,height:1050}}),errors=[];
      page.on('pageerror',error=>errors.push(error.message));
      let release=()=>{};
      if(mode==='hydrate') {
        const gate=new Promise(resolve=>{release=resolve;});
        await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
      }
      try {
        await page.goto(`${gateway.url}?backend=${backend}${mode==='client'?'&client=1':''}`,{waitUntil:'commit'});
        if(mode==='hydrate') {
          await page.locator('#attachments').setInputFiles(textFile);
          await page.locator('#attachments').evaluate(input=>{window.earlyFileInput=input;});
          release();await ready(page);
          await page.getByText(textFile.name,{exact:true}).waitFor();
          assert.equal(await page.evaluate(()=>window.earlyFileInput===document.getElementById('attachments')),true);
          assert(await page.evaluate(()=>window.__fileInitialChildren)>0);
        }else{await ready(page);assert.equal(await page.evaluate(()=>window.__fileInitialChildren),0);await pick(page,[textFile]);}
        await page.getByRole('button',{name:'Preview text',exact:true}).click();
        await page.waitForFunction(()=>document.getElementById('file-preview').textContent==='A little context 中文\n');
        await pick(page,[textFile,binaryFile]);
        await page.getByLabel('Delivery title',{exact:true}).fill('Shared context 中文');
        const before=received.length;
        await page.getByRole('button',{name:'Send delivery',exact:true}).click();
        await page.getByText('Sending your files…',{exact:true}).waitFor();
        // Keep editing while the real server's response is pending.
        await pick(page,[{...textFile,name:'newer.txt'}]);
        await page.getByText('Delivery received. Thank you.',{exact:true}).waitFor();
        assert.deepEqual(received[before],{title:'Shared context 中文',files:[
          {name:textFile.name,type:textFile.mimeType,hex:textFile.buffer.toString('hex')},
          {name:binaryFile.name,type:binaryFile.mimeType,hex:binaryFile.buffer.toString('hex')},
        ]});
        assert.equal(await page.locator('#attachments').evaluate(input=>input.files[0].name),'newer.txt');
        await page.getByText('You have an unsent delivery',{exact:true}).waitFor();
        await page.getByRole('button',{name:'Send delivery',exact:true}).click();
        await page.getByText('Ready for a fresh delivery',{exact:true}).waitFor();
        assert.equal(await page.locator('#attachments').evaluate(input=>input.files.length),0);

        await page.getByRole('button',{name:'Send delivery',exact:true}).click();
        await page.getByText('Choose at least one file.',{exact:true}).waitFor();
        const count=received.length;
        await page.locator('#attachments').setInputFiles([0,1,2,3].map(i=>({...textFile,name:`over-${i}.txt`})));
        await page.getByText('Select at most 3 files.',{exact:true}).waitFor();
        await page.getByRole('button',{name:'Send delivery',exact:true}).click();
        assert.equal(received.length,count);
        assert.equal(await page.locator('#attachments').evaluate(input=>input.files.length),4);
        assert.match(await page.locator('#attachments').getAttribute('aria-describedby'),/attachments-error/);
        await page.getByRole('button',{name:'Clear files',exact:true}).click();
        await page.waitForFunction(()=>document.getElementById('attachments').files.length===0);

        await pick(page,[textFile]);await page.getByLabel('Delivery title',{exact:true}).fill('Reject this');
        await page.getByRole('button',{name:'Send delivery',exact:true}).click();
        await page.getByText('This delivery was declined. Choose another file and try again.',{exact:true}).waitFor();
        assert.equal(await page.locator('#attachments').evaluate(input=>input.files[0].name),textFile.name);
        await page.getByRole('button',{name:'Start over',exact:true}).click();
        await page.waitForFunction(()=>document.getElementById('attachments').files.length===0
          && document.getElementById('delivery-title').value==='A note for the team'
          && document.querySelectorAll('#chosen-files li').length===0);
        assert.equal(await page.getByLabel('Delivery title',{exact:true}).inputValue(),'A note for the team');
        await pick(page,[textFile]);await page.getByRole('button',{name:'Preview text',exact:true}).click();
        await page.waitForFunction(()=>document.getElementById('file-preview').textContent.startsWith('A little context'));
        if(engine==='chromium'&&backend==='vm'&&mode==='hydrate') {
          await page.screenshot({path:join(evidence,'desktop.png'),fullPage:true});
          await page.setViewportSize({width:390,height:844});
          assert.equal(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth),true);
          await page.screenshot({path:join(evidence,'mobile.png'),fullPage:true});
        }
        await page.getByRole('button',{name:'Send delivery',exact:true}).click();
        await page.getByText('Sending your files…',{exact:true}).waitFor();
        await page.evaluate(async()=>{window.__fileApp.close();await window.__fileApp.done;});
        assert.equal(await page.locator('#root').textContent(),'');assert.deepEqual(errors,[]);
        results.push({engine,backend,mode,browserVersion:browser.version(),passed:true});
      }catch(error){console.error({errors,status:await page.locator('#status').textContent()});await page.screenshot({path:join(evidence,`failure-${engine}-${backend}-${mode}.png`),fullPage:true});throw error;}
      finally{release();await page.close();}
    }
    await browser.close();browser=undefined;
  }
  await gateway.close();gateway=undefined;await server.close();server=undefined;
  console.log('Files: real source reload drops native selections and retains text');
  development=await developProject(project);
  browser=await engines.chromium.launch({headless:true});
  const page=await browser.newPage(),errors=[];page.on('pageerror',error=>errors.push(error.message));
  const connected=page.waitForResponse(response=>response.url().endsWith('/__ui-next/events'));
  await page.goto(development.url);await connected;await ready(page);
  await pick(page,[textFile]);await page.getByLabel('Delivery title',{exact:true}).fill('Keep this draft');
  await page.evaluate(()=>{window.beforeFileReload=document.getElementById('root');});
  const sourcePath=join(project,'app/app.vo'),appSource=await readFile(sourcePath,'utf8');
  await writeFile(sourcePath,appSource.replace('Send a little context.','Make room for a new thought.'));
  await page.getByRole('heading',{name:'Make room for a new thought.',exact:true}).waitFor();
  await page.waitForFunction(()=>window.beforeFileReload!==document.getElementById('root'));
  assert.equal(await page.getByLabel('Delivery title',{exact:true}).inputValue(),'Keep this draft');
  assert.equal(await page.locator('#attachments').evaluate(input=>input.files.length),0);
  assert.equal(await page.locator('#chosen-files li').count(),0);assert.deepEqual(errors,[]);
  await browser.close();browser=undefined;await development.close();development=undefined;
  await writeFile(join(evidence,'report.json'),JSON.stringify({passed:true,build,results,uploads:received,
    development:{passed:true,retainedText:true,clearedNativeFiles:true},
    contracts:['native-picker-and-label','preboot-file-adoption','true-client-mount','bounded-utf8-preview',
      'real-http-multipart-text-and-binary','submit-snapshot','newer-selection-preserved','successful-clear',
      'required-validation','file-limit-error','explicit-clear','422-file-errors','native-reset','close-during-upload','source-reload']},null,2)+'\n');
  console.log('File application passed: 12 browser modes, real multipart and source reload');
}finally{await browser?.close();await development?.close();await gateway?.close();await server?.close();await rm(temporary,{recursive:true,force:true});}
