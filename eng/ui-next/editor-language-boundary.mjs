import assert from 'node:assert/strict';

export async function checkEditorLanguageBoundary(page,url) {
  await page.route('**/__editor-language',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Editor language service</title><textarea id="source"></textarea><div id="editor"></div><button id="outside">Outside</button>'}));
  await page.goto(url+'/__editor-language');
  await page.evaluate(async()=>{
    const library=await import('/artifacts/editor-library.js');
    const {createCodeEditorWidget}=await import('/host/ui_next/editor.js');
    const input=document.querySelector('#source'),element=document.querySelector('#editor');
    input.value='中文🙂\nval';const requests=[],failures=[],controller=new AbortController();
    const query=kind=>request=>new Promise((resolve,reject)=>requests.push({kind,...request,resolve,reject}));
    const provider=createCodeEditorWidget(library,()=>({complete:query('complete'),definition:query('definition')}));
    const widget=provider({element,value:JSON.stringify({version:1,inputID:'source',language:'vo'}),signal:controller.signal,fail:message=>failures.push(message)});
    const view=library.EditorView.findFromDOM(element.querySelector('.cm-editor'));
    window.editorLanguage={input,element,requests,failures,controller,widget,view,
      place(position=input.value.length){view.dispatch({selection:{anchor:position}});view.focus();},
      latest(){return requests.at(-1);},
    };
    editorLanguage.place();
  });
  const request=async key=>{
    const before=await page.evaluate(()=>editorLanguage.requests.length);
    await page.keyboard.press(key);
    await page.waitForFunction(count=>editorLanguage.requests.length>count,before);
  };
  try {
    assert.equal(await page.locator('#source').getAttribute('aria-keyshortcuts'),null);
    assert.equal(await page.locator('.cm-content').getAttribute('aria-keyshortcuts'),'Control+Space F12');
    await page.evaluate(()=>{editorLanguage.input.readOnly=true;editorLanguage.widget.afterCommit();});
    await page.waitForFunction(()=>document.querySelector('.cm-content')?.getAttribute('aria-keyshortcuts')==='F12');
    await page.evaluate(()=>{editorLanguage.input.readOnly=false;editorLanguage.widget.afterCommit();});
    await page.waitForFunction(()=>document.querySelector('.cm-content')?.getAttribute('aria-keyshortcuts')==='Control+Space F12');
    await page.evaluate(()=>{editorLanguage.widget.update(JSON.stringify({version:1,inputID:'source',language:'plain'}));editorLanguage.widget.afterCommit();});
    assert.equal(await page.locator('.cm-content').getAttribute('aria-keyshortcuts'),null);
    await page.evaluate(()=>{editorLanguage.widget.update(JSON.stringify({version:1,inputID:'source',language:'vo'}));editorLanguage.input.disabled=true;editorLanguage.widget.afterCommit();});
    assert.equal(await page.locator('.cm-content').getAttribute('aria-keyshortcuts'),null);
    await page.evaluate(()=>{editorLanguage.input.disabled=false;editorLanguage.widget.afterCommit();editorLanguage.place();});
    await request('Control+Space');
    await page.evaluate(()=>{const r=editorLanguage.latest();r.resolve({source:r.source,range:{start:5,end:8},items:[{label:'value',kind:'variable',detail:'var value int'}]});});
    await page.getByRole('option').filter({hasText:'value'}).waitFor();
    // CodeMirror deliberately ignores accept keys during its short initial
    // interaction delay, protecting text already in flight as the menu opens.
    await page.waitForTimeout(100);
    await page.keyboard.press('Enter');
    await page.waitForFunction(()=>editorLanguage.input.value==='中文🙂\nvalue');
    await request('F12');
    await page.evaluate(()=>{const r=editorLanguage.latest();r.resolve({source:r.source,target:{file:'widgets/component.vo',source:r.source,range:{start:5,end:10},local:true}});});
    await page.waitForFunction(()=>editorLanguage.input.selectionStart===5&&editorLanguage.input.selectionEnd===10);
    assert.equal(await page.evaluate(()=>getSelection().toString()),'value');

    await request('F12');
    await page.evaluate(()=>{const r=editorLanguage.latest();r.resolve({source:r.source,target:{file:'lib/main.vo',source:'package lib\nconst Value = 7\n',range:{start:18,end:23},local:false}});});
    await page.getByRole('region',{name:'Definition'}).waitFor();
    assert.equal(await page.locator('.vui-code-definition mark').textContent(),'Value');
    assert.match(await page.locator('.vui-code-definition strong').textContent(),/lib\/main\.vo/);
    await page.getByRole('button',{name:'Back to source'}).click();
    await page.waitForFunction(()=>document.activeElement===editorLanguage.view.contentDOM);
    assert.equal(await page.locator('.vui-code-definition').count(),0);

    await page.evaluate(()=>editorLanguage.place());
    await request('Control+Space');
    await page.keyboard.type('x');
    await page.waitForFunction(()=>editorLanguage.latest().signal.aborted);
    await page.evaluate(()=>{const r=editorLanguage.latest();r.resolve({source:r.source,range:{start:5,end:10},items:[{label:'stale'}]});});
    assert.equal(await page.getByRole('option').filter({hasText:'stale'}).count(),0);
    await request('F12');
    await page.evaluate(()=>{const {view}=editorLanguage;view.dispatch({selection:{anchor:0}});});
    await page.waitForFunction(()=>editorLanguage.latest().signal.aborted);
    await page.evaluate(()=>{const r=editorLanguage.latest();r.resolve({source:r.source,target:{file:'old.vo',source:r.source,range:{start:0,end:2},local:false}});});
    assert.equal(await page.locator('.vui-code-definition').count(),0);

    await request('F12');
    await page.locator('#outside').focus();
    await page.waitForFunction(()=>editorLanguage.latest().signal.aborted);
    await page.evaluate(()=>{const r=editorLanguage.latest();r.resolve({source:r.source,target:{file:'old.vo',source:r.source,range:{start:0,end:2},local:false}});});
    assert.equal(await page.evaluate(()=>document.activeElement.id),'outside');
    await page.evaluate(()=>editorLanguage.place());
    await request('Control+Space');
    await page.evaluate(()=>editorLanguage.latest().reject(new Error('The service failed.')));
    await page.waitForFunction(()=>document.querySelector('.cm-announced')?.textContent.includes('Code information is unavailable'));
    assert.equal(await page.locator('.cm-editor').count(),1);
    await request('Control+Space');
    await page.evaluate(()=>{const r=editorLanguage.latest();r.resolve({source:r.source,range:{start:3,end:4},items:[{label:'invalid'}]});});
    assert.equal(await page.getByRole('option').filter({hasText:'invalid'}).count(),0);

    await request('F12');
    await page.evaluate(()=>{editorLanguage.controller.abort();editorLanguage.widget.dispose();});
    assert.equal(await page.evaluate(()=>editorLanguage.latest().signal.aborted),true);
    await page.evaluate(()=>{const r=editorLanguage.latest();r.resolve({source:r.source,target:{file:'old.vo',source:r.source,range:{start:0,end:2},local:false}});});
    assert.equal(await page.locator('.cm-editor').count(),0);
    assert.deepEqual(await page.evaluate(()=>editorLanguage.failures),[]);
    return {passed:true,contracts:['explicit-semantic-completion','native-value-after-completion','local-definition-selection','external-definition-panel',
      'cancel-on-edit','cancel-on-selection','cancel-on-blur','cancel-on-disposal','invalid-result-rejected','query-failure-keeps-editor']};
  }catch(error){
    error.stack+='\n'+JSON.stringify(await page.evaluate(()=>({value:editorLanguage.input.value,
      requests:editorLanguage.requests.map(({kind,source,position,signal})=>({kind,source,position,aborted:signal.aborted})),
      failures:editorLanguage.failures,active:document.activeElement?.outerHTML.slice(0,200)})));
    throw error;
  }finally{await page.evaluate(()=>{editorLanguage.controller.abort();if(editorLanguage.element.childElementCount)editorLanguage.widget.dispose();});}
}
