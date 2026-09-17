import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {execute} from './project.mjs';

let fixturePromise;
export async function checkCanvasBoundary(page,url) {
  const source=await (fixturePromise ??= execute(compilerPath(),['run','ui/next/tests/canvas'],{env:{...process.env,VOWORK:'off'}}));
  const fixture=JSON.parse(source);
  await page.route('**/canvas-contracts',route=>route.fulfill({contentType:'text/html; charset=utf-8',body:'<!doctype html><title>Canvas</title><main></main>'}));
  await page.goto(url+'/canvas-contracts');
  const result=await page.evaluate(async fixture=>{
   const {DomRenderer}=await import('/host/ui_next/renderer.js');
   const {decodeBatch}=await import('/host/ui_next/generated/codec.js');
   const first=decodeBatch(Uint8Array.from(atob(fixture.first),c=>c.charCodeAt(0))),last=decodeBatch(Uint8Array.from(atob(fixture.last),c=>c.charCodeAt(0)));
   const widget=first.mutations.find(value=>value.op==='widget'),firstPayload=widget.value,id=widget.id;
   const {canvasBitmap}=await import('/host/ui_next/canvas-bitmap.js');
   const require=(value,message)=>{if(!value)throw new Error(message);};
   const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
   const same=(a,b)=>JSON.stringify(a)===JSON.stringify(b);
   const pixel=canvas=>[...canvas.getContext('2d').getImageData(0,0,canvas.width,canvas.height).data];
   let draws=0;const draw=CanvasRenderingContext2D.prototype.putImageData;
   CanvasRenderingContext2D.prototype.putImageData=function(...args){draws++;return draw.apply(this,args);};
   for(const hydrate of [false,true]) {
    const root=document.querySelector('main').appendChild(document.createElement('div')),events=[];
    if(hydrate)root.innerHTML=fixture.html;
    const renderer=new DomRenderer(root,event=>events.push(event),hydrate,{'canvas-bitmap':canvasBitmap});let revision=0;
    const apply=mutations=>renderer.applyBatch({version:first.version,revision:++revision,inputSequence:events.at(-1)?.sequence??0,mutations,commands:null});
    try {
     const before=draws;
     apply(first.mutations);
     const owner=root.firstElementChild,canvas=owner.querySelector('canvas');
     require(canvas&&same(pixel(canvas),[255,0,0,255,0,255,0,255]),'actual Vo pixel frame was not drawn');
     require(canvas.getAttribute('aria-hidden')==='true'&&owner.getAttribute('aria-label')==='Red and green pixels','bitmap lost accessible description');
     apply([m('attr',id,{name:'title',value:'Unrelated'}),m('insert',id)]);
     require(draws===before+1&&canvas===owner.querySelector('canvas')&&same(pixel(canvas),[255,0,0,255,0,255,0,255]),'unrelated/keyed update redrew or replaced bitmap');
     owner.style.width='240px';require(canvas.getBoundingClientRect().width===240&&canvas.width===2,'CSS resizing changed logical bitmap');
     apply(last.mutations);
     require(draws===before+2&&canvas===owner.querySelector('canvas')&&same(pixel(canvas),[0,0,255,255,255,255,255,255]),'bitmap update lost native identity or pixels');
     canvas.dispatchEvent(new Event('contextlost'));
     require(events.length===1&&events[0].error.includes('context was lost')&&!canvas.isConnected&&canvas.width===0&&canvas.height===0,'context failure retained native backing or skipped local result');
     canvas.dispatchEvent(new Event('contextlost'));require(events.length===1,'disposed canvas retained its failure listener');
    }finally{renderer.close();root.remove();}
   }
   for(const value of ['invalid',JSON.stringify({version:2,width:1,height:1,pixels:'AAAAAA=='}),JSON.stringify({version:1,width:2049,height:1,pixels:''}),JSON.stringify({version:1,width:1,height:1,pixels:'!!!!!!!!!'}),JSON.stringify({version:1,width:1024,height:1024,pixels:''})]) {
    const root=document.querySelector('main').appendChild(document.createElement('div')),controller=new AbortController();let instance,error;
    try{instance=canvasBitmap({element:root,value,signal:controller.signal,emit(){},fail(){throw new Error('unexpected async failure');}});}catch(failure){error=failure;}finally{instance?.dispose();root.remove();}
    require(error&&!root.querySelector('canvas'),'malformed bitmap acquired native DOM');
   }
   const root=document.querySelector('main').appendChild(document.createElement('div')),controller=new AbortController();
   const value=JSON.stringify({version:1,width:1024,height:256,pixels:btoa('\x01\x02\x03\xff'.repeat(262144))});
   const instance=canvasBitmap({element:root,value,signal:controller.signal,emit(){},fail(){throw new Error('bitmap failed');}}),canvas=root.querySelector('canvas');
   require(canvas.width===1024&&canvas.height===256&&same([...canvas.getContext('2d').getImageData(1023,255,1,1).data],[1,2,3,255]),'maximum bitmap cannot be drawn');
   controller.abort();instance.update(firstPayload);instance.dispose();require(!canvas.isConnected&&canvas.width===0&&canvas.height===0,'abort retained backing or allowed a late update');root.remove();
   return {passed:true,cases:2,contracts:['actual-vo-rgba','client-and-adopted-widget','retained-canvas-update','no-unrelated-redraw','css-resizing','synthetic-context-loss','malformed-payload','maximum-pixels','abort-and-disposal']};
  },fixture);
  assert(result.passed);
  return {...result,fixtureSha256:createHash('sha256').update(source).digest('hex')};
}
