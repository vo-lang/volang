import assert from 'node:assert/strict';
import {mkdir,writeFile} from 'node:fs/promises';
import {resolve} from 'node:path';
import {serveFiles} from './static-server.mjs';
import {root} from './server.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root,'target/playwright-browsers');
const engines=await import('../browser/node_modules/playwright/index.mjs');
const server=await serveFiles(resolve(root,'lang/crates/vo-web/dist'));
const results=[];
let browser;
try {
  for(const engine of ['chromium','firefox','webkit']) {
    browser=await engines[engine].launch({headless:true});
    const page=await browser.newPage();
    await page.goto(server.url+'blank.html');
    await page.setContent('<!doctype html><main id="root"></main>');
    const result=await page.evaluate(async base=>{
      const {DomRenderer}=await import(base+'ui_next/renderer.js');
      const {WIRE_VERSION}=await import(base+'ui_next/generated/protocol.js');
      const ensure=(ok,message)=>{if(!ok) throw new Error(message);};
      const events=[], lifecycle=[];
      const container=document.getElementById('root');
      const renderer=new DomRenderer(container,event=>events.push(event),false,{probe:({element,signal})=>{
        lifecycle.push('mount');
        element.append(document.createTextNode('Owned widget'));
        return {update(){},dispose(){ensure(signal.aborted,'widget disposed before cancellation');lifecycle.push('dispose');}};
      }});
      let revision=0;
      const m=(op,id,parent=0,name='',value='',before=0)=>({op,id,parent,name,value,before});
      const create=(id,parent=0,kind='div')=>[m('create',id,0,kind),m('insert',id,parent)];
      const apply=mutations=>renderer.applyBatch({version:WIRE_VERSION,revision:++revision,inputSequence:events.at(-1)?.sequence??0,mutations,commands:[]});
      try {
        apply([...create(1,0,'section'),m('attr',1,0,'id','owner'),m('listen',1,0,'click','0'),
          ...create(2,1),m('attr',2,0,'id','placed'),...create(3,2,'input'),
          m('attr',3,0,'id','field'),m('attr',3,0,'name','draft'),m('attr',3,0,'form','saved'),
          m('attr',3,0,'value','hello world'),m('listen',3,0,'input','0'),
          ...create(4,2,'#widget'),m('widget',4,0,'probe',''),
          m('portal',2,5),...create(5,0,'form'),m('attr',5,0,'id','saved'),m('listen',5,0,'click','0'),
          ...create(6,0,'aside'),m('attr',6,0,'id','other'),...create(7,1,'p')]);
        const field=document.getElementById('field'),placed=document.getElementById('placed');
        ensure(placed.parentNode.id==='saved','later portal target was not resolved');
        ensure(document.getElementById('owner').children.length===1,'source retained the physical element');
        field.click();
        ensure(events.some(event=>event.kind==='click' && event.target===5),'native event did not bubble through the destination');
        ensure(!events.some(event=>event.kind==='click' && event.target===1),'native event used logical source ancestry');
        field.focus();field.setSelectionRange(1,4,'backward');
        apply([m('portal',2,6)]);
        ensure(document.activeElement===field && field.selectionStart===1 && field.selectionEnd===4 && field.selectionDirection==='backward','target change lost native focus or selection');
        ensure(field===document.getElementById('field') && field.value==='hello world','target change replaced native input');
        ensure(new FormData(document.getElementById('saved')).get('draft')==='hello world','explicit native form association was lost');
        ensure(lifecycle.join()==='mount','target change recreated widget ownership');
        const observer=new MutationObserver(()=>{});observer.observe(container,{childList:true,subtree:true});
        apply([]);
        ensure(observer.takeRecords().length===0,'unchanged portal moved DOM nodes');observer.disconnect();
        apply([m('insert',7,1,'','',2)]);
        ensure(placed.parentNode.id==='other' && document.activeElement===field,'logical sibling reorder moved portal content');
        apply([m('remove',6)]);
        ensure(placed.parentNode.id==='owner' && placed.previousSibling.nodeName==='P','missing target did not restore logical position');
        ensure(document.activeElement===field && field.selectionStart===1,'target removal lost focus');
        apply([m('portal',2,9),...create(9,0,'aside')]);
        ensure(placed.parentNode===container.lastChild,'recreated target did not receive retained source');
        const html=container.innerHTML;
        try {renderer.applyBatch({version:WIRE_VERSION,revision:revision+1,inputSequence:events.at(-1)?.sequence??0,
          mutations:[m('attr',3,0,'value','must not commit'),m('portal',2,2)],commands:[]});throw new Error('cyclic portal was accepted');}
        catch(error){ensure(String(error).includes('own content'),'unexpected cycle error: '+error);}
        ensure(container.innerHTML===html && field.value==='hello world','invalid portal batch partially changed the DOM');
        apply([m('remove',1)]);
        ensure(!field.isConnected && !document.getElementById('placed'),'owner disposal left escaped portal DOM');
        ensure(lifecycle.join()==='mount,dispose','owner disposal did not release the widget exactly once');
      } finally {renderer.close();}
      ensure(container.childNodes.length===0,'root close retained portal DOM');

      // Two nested sources can use distinct destinations and return inline in
      // either order. Root close must remove every remote placement.
      const nested=new DomRenderer(container,()=>{});
      const batch=(revision,mutations)=>nested.applyBatch({version:WIRE_VERSION,revision,inputSequence:0,mutations,commands:[]});
      try {
        batch(1,[...create(1),...create(2,1),...create(3),...create(4),m('portal',1,3),m('portal',2,4)]);
        ensure(container.children.length===2 && container.children[0].children.length===1 && container.children[1].children.length===1,'nested sources did not escape independently');
        batch(2,[m('portal',1),m('portal',2)]);
        ensure(container.firstElementChild.firstElementChild!==null && container.children.length===3,'nested placements did not restore inline');
        batch(3,[m('portal',1,3),m('portal',2,4)]);
      } finally {nested.close();}
      ensure(container.childNodes.length===0,'nested root close retained nodes');
      return {passed:true,contracts:['deferred-target','physical-event-ancestry','native-form-association',
        'focus-selection-and-node-identity','unchanged-no-dom-moves','logical-sibling-reorder',
        'target-removal-inline-fallback','target-recreation','atomic-invalid-batch','widget-owner-disposal','nested-placement','root-close']};
    },server.url);
    assert(result.passed);
    results.push({engine,browserVersion:browser.version(),...result});
    console.log(`${engine}: portal DOM placement, native input and ownership passed`);
    await browser.close();browser=undefined;
  }
  await mkdir(resolve(root,'target/ui-next'),{recursive:true});
  await writeFile(resolve(root,'target/ui-next/portal-dom-report.json'),JSON.stringify({passed:true,results},null,2)+'\n');
}finally{await browser?.close();await server.close();}
