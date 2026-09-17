import assert from 'node:assert/strict';

export async function checkSizeBoundary(page,url) {
 await page.route('**/size-contracts',route=>route.fulfill({contentType:'text/html; charset=utf-8',body:'<!doctype html><title>Native sizes</title>'}));
 await page.goto(url+'/size-contracts');
 const result=await page.evaluate(async()=>{
  const {DomRenderer}=await import('/host/ui_next/renderer.js');
  const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
  const {encodeBatch}=await import('/host/ui_next/generated/codec.js');
  const require=(value,message)=>{if(!value)throw new Error(message);};
  const mutation=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
  const frame=()=>new Promise(resolve=>requestAnimationFrame(resolve));
  const settle=async()=>{await frame();await frame();await frame();};
  const cases=[];
  for(const hydrate of [false,true]){
   const container=document.body.appendChild(document.createElement('main')),events=[];
   const style='box-sizing:border-box;width:101.25px;height:53.5px;padding:3.25px;border:1.5px solid';
   if(hydrate)container.innerHTML=`<div data-vo-id="1" data-vo-events="size=0" style="${style}"></div>`;
   const early=container.firstElementChild,renderer=new DomRenderer(container,event=>events.push(event),hydrate);
   let revision=0;
   const apply=mutations=>{renderer.apply(encodeBatch({version:WIRE_VERSION,revision:revision+1,inputSequence:events.at(-1)?.sequence??0,mutations,commands:null}));revision++;};
   try{
    apply([mutation('create',1,{name:'div'}),mutation('insert',1),mutation('attr',1,{name:'style',value:style}),mutation('listen',1,{name:'size',value:'0'})]);
    await settle();const box=container.firstElementChild;
    require(!hydrate||box===early,'size observation replaced the SSR element');
    require(events.length===1&&events[0].kind==='size','initial size did not coalesce');
    const first=JSON.parse(events[0].value);
    require(Math.abs(first.inline-101.25)<.1&&Math.abs(first.block-53.5)<.1,'fractional border box was rounded or excluded padding/border');
    apply([mutation('attr',1,{name:'style',value:style+';transform:scale(2)'})]);await settle();
    require(events.length===1,'a transform changed the observed layout size');
    for(let height=60;height<=80;height++)box.style.height=height+'.25px';
    await settle();require(events.length===2&&Math.abs(JSON.parse(events.at(-1).value).block-80.25)<.1,'resize burst did not publish only the latest size');
    box.style.writingMode='vertical-rl';await settle();
    const vertical=JSON.parse(events.at(-1).value);
    require(events.length===3&&Math.abs(vertical.inline-80.25)<.1&&Math.abs(vertical.block-101.25)<.1,'size did not follow logical writing mode');
    box.style.display='none';await settle();
    require(events.length===4&&events.at(-1).value==='{"inline":0,"block":0}','hidden box did not report zero');
    box.style.display='block';await settle();require(events.length===5,'restored element did not report size');
    for(const [name,value] of [['size:capture','1'],['size','8']]){
     let rejected=false;try{apply([mutation('attr',1,{name:'title',value:'partial'}),mutation('listen',1,{name,value})]);}catch{rejected=true;}
     require(rejected&&!box.hasAttribute('title'),'invalid observation options partially committed');
    }
    let svgRejected=false;
    try{apply([mutation('create',2,{name:'svg'}),mutation('insert',2),mutation('listen',2,{name:'size',value:'0'})]);}catch{svgRejected=true;}
    require(svgRejected&&container.childElementCount===1,'SVG observation changed the tree');
    box.style.height='100px';apply([mutation('unlisten',1,{name:'size'})]);await settle();
    require(events.length===5,'unlisten delivered a pending size');
    apply([mutation('listen',1,{name:'size',value:'0'})]);await settle();require(events.length===6,'reinstalled observer did not emit its initial size');
    box.style.height='120px';renderer.close();await settle();
    require(events.length===6&&container.childNodes.length===0,'disposed size observer delivered or retained its node');
    cases.push({hydrate,passed:true});
   }finally{renderer.close();container.remove();}
  }
  return {passed:true,cases,contracts:['fractional-border-box','transforms-ignored','logical-writing-mode','hidden-restored','invalid-options-atomicity','SVG-rejected','unlisten-pending-size','disposed-pending-size']};
 });
 assert(result.passed);return result;
}
