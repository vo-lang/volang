import assert from 'node:assert/strict';
export async function checkMediaProperties(page,url) {
 await page.route('**/media-property-contracts',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Media properties</title>'}));
 await page.goto(url+'/media-property-contracts');
 const result=await page.evaluate(async()=>{
  const {DomRenderer}=await import('/host/ui_next/renderer.js');const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
  const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
  const require=(value,message)=>{if(!value)throw new Error(message);};let count=0;
  for(const kind of ['audio','video'])for(const declared of [false,true])for(const mode of ['client','hydrate','early-native-edit']) {
   const root=document.body.appendChild(document.createElement('div')),hydrate=mode!=='client';
   if(hydrate)root.innerHTML=`<${kind} data-vo-id="1"${declared?' muted="true"':''}></${kind}>`;
   const retained=root.firstElementChild;if(mode==='early-native-edit')retained.muted=!declared;
   const renderer=new DomRenderer(root,()=>{},hydrate);let revision=0;
   const apply=mutations=>renderer.applyBatch({version:WIRE_VERSION,revision:++revision,inputSequence:0,mutations,commands:null});
   const muted=value=>m('attr',1,{name:'muted',value:String(value)});
   try {
    apply([m('create',1,{name:kind}),m('insert',1),muted(declared)]);
    const element=root.firstElementChild,label=kind+'/'+declared+'/'+mode;
    const expected=mode==='early-native-edit'?!declared:declared;
    require(element.muted===expected && element.defaultMuted===declared,label+' initial mute disagrees with its view/native edit');
    require(!hydrate || element===retained,label+' replaced server media');
    apply([m('attr',1,{name:'title',value:'Retained player'})]);
    require(element.muted===expected,label+' changed mute on unrelated rendering');
    apply([muted(!declared)]);require(element.muted===!declared && element.defaultMuted===!declared,label+' ignored changed mute');
    apply([m('attr',1,{name:'MUTED',value:''})]);require(element.muted && element.defaultMuted,label+' cannot enable mute with a case alias/empty attribute');
    apply([m('removeAttr',1,{name:'muted'})]);require(!element.muted && !element.defaultMuted,label+' cannot remove mute');
    count++;
   } finally {renderer.close();root.remove();}
  }
  return {passed:true,cases:count,contracts:['client-parser-mute-parity','early-native-mute-preserved','declared-mute-update','mute-removal','unrelated-render-retains-native-state']};
 });assert.equal(result.passed,true);return result;
}
