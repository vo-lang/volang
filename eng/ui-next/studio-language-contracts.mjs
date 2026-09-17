import assert from 'node:assert/strict';
import {sourceEditor} from './editor-controls.mjs';

export async function checkStudioLanguageService(browser,url) {
  const reports=[];
  for(const backend of ['vm'])for(const mode of ['console','ui']) {
    const page=await browser.newPage(),errors=[],requests=[];
    page.on('pageerror',error=>errors.push(error.message));page.on('request',request=>requests.push(request.url()));
    try {
      await page.goto(`${url}/studio/playground${mode==='ui'?'/ui':''}?backend=${backend}`);
      await page.waitForFunction(()=>window.__studioNext?.ready||window.__studioNext?.error);
      assert.equal(await page.evaluate(()=>window.__studioNext.error),null);
      await page.locator('.cm-content').waitFor();
      assert(!requests.some(url=>url.includes('/compiler/')),'an idle editor loaded its compiler');
      const id=mode==='ui'?'ui-playground-source':'playground-source',editor=sourceEditor(page,id);
      const libraryURL=requests.find(url=>/\/editor-library(?:-[a-z0-9]+)?\.js(?:\?.*)?$/i.test(url));
      assert(libraryURL,'optional editor library request missing');
      const place=async position=>page.evaluate(async({libraryURL,position})=>{
        const {EditorView}=await import(libraryURL),view=EditorView.findFromDOM(document.querySelector('.cm-editor'));
        view.dispatch({selection:{anchor:position}});view.focus();
      },{libraryURL,position});
      const source=mode==='ui'
        ?'package main\nimport ui "github.com/vo-lang/ui/next"\nimport "github.com/vo-lang/ui/next/host"\nfunc main() { host.Run(ui.Element("h1", ui.Te)) }\n'
        :'package main\nimport "fmt"\nfunc main() {\n    value := "Editor 中文🙂"\n    fmt.Pr\n}\n';
      await editor.fill(source);
      const prefix=mode==='ui'?'ui.Te':'fmt.Pr',label=mode==='ui'?'Text':'Println';
      await place(source.indexOf(prefix)+prefix.length);
      // A failed worker must leave editing usable and the next explicit query retryable.
      if(mode==='console') {
        await page.route('**/language-worker.js',route=>route.fulfill({status:503,body:'temporarily unavailable'}));
        await editor.press('Control+Space');
        await page.waitForFunction(()=>window.__studioNext.workers.started>0&&window.__studioNext.workers.started===window.__studioNext.workers.stopped);
        await page.unroute('**/language-worker.js');
        assert.equal(await editor.inputValue(),source);
      }
      await editor.press('Control+Space');
      const choice=page.getByRole('option').filter({has:page.locator('.cm-completionLabel',{hasText:new RegExp('^'+label+'$')})});
      await choice.waitFor({timeout:35000});await page.waitForTimeout(100);await choice.click();
      await page.waitForFunction(({id,label})=>document.getElementById(id).value.includes(label),{id,label});
      await editor.press('(');
      await page.keyboard.type(mode==='ui'?'"Live completion"':'value');
      await editor.press(')');
      const completed=await editor.inputValue();
      assert(completed.includes(label+'('));
      if(mode==='console') {
        await page.getByRole('button',{name:'Run code',exact:true}).click();
        await page.waitForFunction(()=>document.querySelector('[data-output]')?.textContent==='Editor 中文🙂\n',null,{timeout:35000});
        await place(completed.lastIndexOf('value'));await editor.press('F12');
        await page.waitForFunction(id=>{const input=document.getElementById(id);return input.selectionStart===input.value.indexOf('value :=')&&input.value.slice(input.selectionStart,input.selectionEnd)==='value';},id);
      }else {
        await page.getByRole('button',{name:'Run preview',exact:true}).click();
        await page.waitForFunction(()=>document.querySelector('[data-preview-status]')?.textContent.includes('Your preview is ready'),null,{timeout:35000});
        await page.getByRole('button',{name:'Stop preview',exact:true}).click();
      }
      await place(completed.indexOf(label));await editor.press('F12');
      await page.getByRole('region',{name:'Definition'}).waitFor({timeout:35000});
      assert.equal(await page.locator('.vui-code-definition mark').textContent(),label);
      assert.match(await page.locator('.vui-code-definition strong').textContent(),mode==='ui'?/vendor\/ui\/next\//:/fmt\//);
      assert.equal(await editor.inputValue(),completed,'definition view modified the source');
      await page.getByRole('button',{name:'Back to source'}).click();
      await page.waitForFunction(()=>document.activeElement?.classList.contains('cm-content'));
      await page.evaluate(async()=>{window.__studioNext.close();await window.__studioNext.done;});
      const workers=await page.evaluate(()=>window.__studioNext.workers);
      assert(workers.started>0);assert.equal(workers.started,workers.stopped,'an editor retained its compiler worker after closing');
      assert.deepEqual(errors,[]);
      reports.push({backend,mode,passed:true,completion:true,definition:true,lazy:true,workersReleased:true,retry:mode==='console'});
    }catch(error){
      error.stack+='\n'+JSON.stringify(await page.evaluate(()=>({workers:window.__studioNext?.workers,
        value:document.querySelector('textarea')?.value,options:[...document.querySelectorAll('[role="option"]')].map(option=>option.textContent),
        announcement:document.querySelector('.cm-announced')?.textContent,error:window.__studioNext?.error})))+'\n'+JSON.stringify(errors);
      await page.screenshot({path:`target/ui-next/semantic-stage/studio-language-${backend}-${mode}-failure.png`});
      throw error;
    }finally{await page.close();}
  }
  return reports;
}
