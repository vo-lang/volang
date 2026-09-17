import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {root} from './server.mjs';

export async function checkKeyedOrder(browser,url) {
  const results=[];
  const ssr=await readFile(resolve(root,'target/ui-next/keyed-order.ssr.html'),'utf8');
  for(const backend of ['vm'])for(const hydrate of [false,true]) {
    const page=await browser.newPage(),errors=[];page.on('pageerror',error=>errors.push(error.message));
    try {
      await page.route('**/__keyed-order*',route=>route.fulfill({contentType:'text/html',body:`<!doctype html><title>Retained ranges</title><div id="root">${hydrate?ssr:''}</div>`}));
      await page.goto(url+'/__keyed-order');
      await page.evaluate(async({backend,hydrate})=>{
        const {mountUi}=await import('/host/ui_next/mount.js');
        const application=mountUi(document.getElementById('root'),{backend,hydrate,
          artifact:'/artifacts/keyed-order.vob',loadVm:()=>import('/wasm/vo_web.js')});
        window.orderApplication=application;
        if(!await application.ready)throw new Error('The retained-order application did not start.');
      },{backend,hydrate});
      const order=()=>page.locator('[data-row]').evaluateAll(nodes=>nodes.map(node=>node.dataset.row));
      assert.deepEqual(await order(),['a','b','c','d','e']);
      await page.getByRole('button',{name:'b: 0',exact:true}).click();
      await page.getByRole('button',{name:'b: 1',exact:true}).click();
      const input=page.getByRole('textbox',{name:'Item b',exact:true});
      await input.fill('中文🙂 retained');
      await page.waitForFunction(()=>document.querySelector('[data-value=b]').textContent==='中文🙂 retained');
      await input.evaluate(input=>{window.retainedInput=input;input.setSelectionRange(5,9,'backward');});
      await input.press('F8');
      await page.waitForFunction(()=>document.querySelector('[data-mode]').textContent==='rotate');
      assert.deepEqual(await order(),['b','c','d','e','a']);
      assert.deepEqual(await input.evaluate(input=>[input===window.retainedInput,document.activeElement===input,input.selectionStart,input.selectionEnd,input.selectionDirection]),[true,true,5,9,'backward']);
      for(const [mode,expected]of [['reverse',['e','d','c','b','a']],['mixed',['e','f','b','d']],['reset',['a','b','c','d','e']]]) {
        // A host-triggered action leaves focus on the input while ranges move.
        await page.locator('[data-action='+mode+']').evaluate(button=>button.click());
        await page.waitForFunction(mode=>document.querySelector('[data-mode]').textContent===mode,mode);
        assert.deepEqual(await order(),expected);
        assert.deepEqual(await input.evaluate(input=>[input===window.retainedInput,document.activeElement===input,input.value,input.selectionStart,input.selectionEnd,input.selectionDirection]),[true,true,'中文🙂 retained',5,9,'backward']);
        assert.equal(await page.locator('[data-count=b]').textContent(),'b: 2');
      }
      await page.locator('[data-action=empty]').evaluate(button=>button.click());
      await page.waitForFunction(()=>document.querySelector('[data-mode]').textContent==='empty');
      assert.deepEqual(await order(),[]);
      await page.locator('[data-action=reset]').click();
      await page.waitForFunction(()=>document.querySelector('[data-mode]').textContent==='reset');
      assert.equal(await input.evaluate(input=>input===window.retainedInput),false);
      assert.equal(await input.inputValue(),'');assert.equal(await page.locator('[data-count=b]').textContent(),'b: 0');
      await page.evaluate(async()=>{orderApplication.close();await orderApplication.done;});
      assert.equal(await page.locator('#root').textContent(),'');assert.deepEqual(errors,[]);
      results.push({backend,hydrate,passed:true,fragmentRanges:true,state:true,focus:true,selection:true,mixedMembership:true,disposal:true});
    } finally {await page.close();}
    console.log(browser.browserType().name()+' '+backend+' '+(hydrate?'hydrated':'client')+' retained range ordering passed');
  }
  return {passed:true,cases:results};
}
