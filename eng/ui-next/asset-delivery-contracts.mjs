import {sourceEditor} from './editor-controls.mjs';
import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {request as httpRequest} from 'node:http';
import {readFile,writeFile} from 'node:fs/promises';
import {join,resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {brotliDecompressSync,gunzipSync} from 'node:zlib';
import {root} from './server.mjs';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';

const directory=resolve(root,'target/ui-next/studio-distribution');
const build=JSON.parse(await readFile(join(directory,'build-report.json')));
const {start}=await import(pathToFileURL(join(directory,'server/entry.mjs')).href);
const application=await start({executable:compilerPath()});
const raw=(url,headers)=>new Promise((resolve,reject)=>{
  const outgoing=httpRequest(url,{headers},incoming=>{
    const chunks=[]; incoming.on('data',chunk=>chunks.push(chunk)); incoming.once('error',reject);
    incoming.once('end',()=>resolve({status:incoming.statusCode,headers:incoming.headers,bytes:Buffer.concat(chunks)}));
  }); outgoing.once('error',reject); outgoing.end();
});
const transfer=[], browsers=[];
const schemaProgram = `package main
import (
  ui "github.com/vo-lang/ui/next"
  "github.com/vo-lang/ui/next/forms"
  "github.com/vo-lang/ui/next/files"
  "github.com/vo-lang/ui/next/forms/schema"
  "github.com/vo-lang/ui/next/host"
)
var fileType = ui.Define("delivered.Files")
func main() {
  host.RunWithData(func(initial string) ui.View {
    return ui.Component(fileType, func(scope *ui.Scope) ui.View {
    selected := ui.State(scope, "files", func() any { return files.Selection{} })
    output := ui.String(scope, "file-result", "")
    selection := selected.Get().(files.Selection)
    values := forms.WithArray(forms.Values{"interests": []string{"", "design"}}, "people", []forms.Values{{"name": []string{""}}})
    errors := schema.From(forms.Data{Values: values}).Errors([]schema.Issue{
      {Path: schema.Field("people").Index(0).Field("name"), Message: "Name required."},
      {Path: schema.Field("interests").Value(1), Message: "Choose an interest."},
    })
    return ui.Element("section", ui.Element("p", ui.Text(errors["people[initial-0].name"] + " / " + errors["interests"])).Attr("data-schema", "ready"),
      files.Enhance(ui.Element("input").Attr("type", "file").Attr("id", "delivered-file").Attr("aria-label", "Delivered file"), selection, func(next files.Selection) {
        selected.Update(func(any) any { return next })
      }, files.Options{ID: "delivered-file"}),
      ui.Element("button", ui.Text("Read delivered file")).On("click", func(ui.Event) {
        current := selected.Get().(files.Selection)
        if current.Len() == 0 { return }
        ui.Start(scope, files.ReadText(current.Files()[0]), func(result ui.Result) { output.Set(result.Value) })
      }), ui.Element("output", ui.Text(output.Get())).Attr("id", "delivered-output"))
    })
  })
}
`;
try {
  for(const name of ['artifacts/studio.vob','compiler/vo_web_bg.wasm','wasm/vo_web_bg.wasm','artifacts/playground-ui.json','studio-docs/search.json']) {
    const artifact=build.artifacts.find(item=>item.path==='public/'+name);
    for(const encoding of ['identity','gzip','br']) {
      const response=await raw(application.url+name,{'accept-encoding':encoding});
      assert.equal(response.status,200);
      const bytes=encoding==='identity'?response.bytes:(encoding==='gzip'?gunzipSync:brotliDecompressSync)(response.bytes);
      assert.equal(createHash('sha256').update(bytes).digest('hex'),artifact.sha256,name);
      const cached=await raw(application.url+name,{'accept-encoding':encoding,'if-none-match':response.headers.etag});
      assert.equal(cached.status,304); assert.equal(cached.bytes.length,0);
      transfer.push({path:name,encoding,originalBytes:bytes.length,transferredBytes:response.bytes.length,revalidated:true});
    }
  }
  process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
  const engines=await import('../browser/node_modules/playwright/index.mjs');
  const combinations=[];
  for(const engine of ['chromium','firefox','webkit']) {
    const browser=await engines[engine].launch({headless:true}); browsers.push(browser);
    for(const backend of ['vm']) {
      const page=await browser.newPage(), errors=[], responses=[];
      page.on('pageerror',error=>errors.push(error.message));
      page.on('response',response=>responses.push(response));
      let release;
      const gate=new Promise(resolve=>{release=resolve;});
      await page.route('**/artifacts/studio.*',async route=>{await gate; await route.continue();});
      try {
        await page.goto(application.url+`studio/docs/language-specification?backend=${backend}`,{waitUntil:'commit'});
        const search=page.getByLabel('Find a chapter',{exact:true});
        await search.fill('channel');
        await search.evaluate(element=>{window.beforeAssetInput=element;});
        release();
        await page.waitForFunction(()=>window.__studioNext?.ready || window.__studioNext?.error);
        assert.equal(await page.evaluate(()=>window.__studioNext.error),null);
        assert.equal(await search.inputValue(),'channel');
        assert(await search.evaluate(element=>element===window.beforeAssetInput));
        assert(!responses.some(response=>/\/compiler\/|\/studio-docs\/page-|editor-library/.test(response.url())));
        await search.fill('InspectProp');
        await page.waitForFunction(()=>document.querySelectorAll('.studio-doc-nav a').length===2);
        await page.getByRole('navigation',{name:'Documentation chapters'}).getByRole('link',{name:'First steps',exact:true}).waitFor();
        assert.equal(responses.filter(response=>response.url().includes('/studio-docs/search.json')).length,1);
        const image=responses.find(response=>response.url().includes('/artifacts/studio.vob'));
        assert(new URL(image.url()).pathname.endsWith('/studio.vob.gz'),`${engine} did not request compressed bytecode`);
        const encoding='gzip';
        const delivered=await image.body();
        const decoded=delivered[0]===0x1f && delivered[1]===0x8b ? gunzipSync(delivered) : delivered;
        const artifact=build.artifacts.find(item=>item.path==='public/artifacts/studio.vob');
        assert.equal(createHash('sha256').update(decoded).digest('hex'),artifact.sha256);
        assert((await image.request().sizes()).responseBodySize < 700000,`${engine} exceeded the compressed bytecode budget`);
        await page.getByRole('link',{name:'Playground',exact:true}).click();
        await page.locator('.cm-content').waitFor();
        const editorLibrary = responses.find(response => response.url().includes('/editor-library-'));
        assert(editorLibrary, 'the delivered editor did not load its independent chunk');
        assert(['br','gzip'].includes((await editorLibrary.allHeaders())['content-encoding']));
        const code='package main\nfunc main() { println("Delivered 中文") }\n';
        await sourceEditor(page).fill(code);
        await page.locator('[data-run]').click();
        await page.waitForFunction(()=>document.querySelector('[data-output]')?.textContent==='Delivered 中文\n',null,{timeout:35000});
        await page.waitForFunction(()=>window.__studioNext.workers.started===window.__studioNext.workers.stopped);
        if (engine === 'chromium' && backend === 'vm') {
          await page.screenshot({path:resolve(root, 'target/ui-next/editor-studio-desktop.png'), fullPage:true});
          const viewport = page.viewportSize();
          await page.setViewportSize({width:390, height:844});
          assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth));
          await page.screenshot({path:resolve(root, 'target/ui-next/editor-studio-mobile.png'), fullPage:true});
          await page.setViewportSize(viewport);
        }
        await page.getByRole('link',{name:'Try UI components →',exact:true}).click();
        await page.locator('[data-run-preview]').click();
        const preview=page.frameLocator('iframe');
        await preview.getByRole('button',{name:'One more idea',exact:true}).click({timeout:35000});
        await page.waitForFunction(()=>document.querySelector('iframe')?.contentDocument.querySelector('output')?.textContent==='1 little ideas');
        await page.locator('[data-stop-preview]').click();
        await page.waitForFunction(()=>window.__studioNext.workers.started===window.__studioNext.workers.stopped);
        await sourceEditor(page,'ui-playground-source').fill(schemaProgram);
        await page.locator('[data-run-preview]').click();
        await preview.locator('[data-schema="ready"]').waitFor({timeout:35000});
        assert.equal(await preview.locator('[data-schema="ready"]').textContent(),'Name required. / Choose an interest.');
        await preview.getByLabel('Delivered file').setInputFiles({name:'delivered.txt',mimeType:'text/plain',buffer:Buffer.from('Files delivered 中文')});
        await preview.getByRole('button',{name:'Read delivered file',exact:true}).click();
        await page.waitForFunction(()=>document.querySelector('iframe')?.contentDocument.querySelector('#delivered-output')?.textContent==='Files delivered 中文');
        await page.locator('[data-stop-preview]').click();
        await page.waitForFunction(()=>window.__studioNext.workers.started===window.__studioNext.workers.stopped);
        const snapshot=responses.find(response=>response.url().endsWith('/artifacts/playground-ui.json'));
        assert(['br','gzip'].includes((await snapshot.allHeaders())['content-encoding']));
        assert.deepEqual(errors,[]);
        combinations.push({engine,backend,encoding,earlyInput:true,serverCache:true,compressedEditor:true,consoleWorker:true,uiWorker:true,uiSchemaWorker:true,uiFileWorker:true});
        console.log(`${engine} ${backend}: compressed image, SSR input and lazy workers passed`);
      } finally {release(); await page.close();}
    }
    await browser.close();
  }
  await writeFile(resolve(root,'target/ui-next/asset-delivery-report.json'),JSON.stringify({passed:true,build,transfer,combinations,formalPerformanceBenchmark:false},null,2)+'\n');
} finally {await Promise.allSettled(browsers.map(browser=>browser.close())); await application.close();}
