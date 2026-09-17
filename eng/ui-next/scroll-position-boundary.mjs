import assert from 'node:assert/strict';

export async function checkScrollPositionBoundary(page,url) {
  await page.route('**/scroll-position-contracts',route=>route.fulfill({contentType:'text/html; charset=utf-8',body:'<!doctype html><title>Scroll position</title><button id="outside">Keep focus</button><main></main>'}));
  await page.goto(url+'/scroll-position-contracts');
  const result=await page.evaluate(async()=>{
   const {DomRenderer}=await import('/host/ui_next/renderer.js');
   const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
   const require=(value,message)=>{if(!value)throw new Error(message);};
   const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
   const root=document.querySelector('main'),events=[];
   const renderer=new DomRenderer(root,event=>events.push(event));let revision=0;
   const apply=mutations=>{renderer.applyBatch({version:WIRE_VERSION,revision:revision+1,inputSequence:events.at(-1)?.sequence??0,mutations,commands:null});revision++;};
   const scroll=(left,top)=>m('scrollPosition',1,{value:JSON.stringify([left,top])});
   const frames=()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)));
   try {
    document.querySelector('button').focus();
    apply([m('create',1,{name:'div'}),m('insert',1),m('attr',1,{name:'style',value:'width:200px;height:120px;overflow:auto;scroll-behavior:smooth;overflow-anchor:none'}),m('create',2,{name:'div'}),m('insert',2,{parent:1}),m('attr',2,{name:'style',value:'width:800px;height:1000px'}),scroll(50,400)]);
    const box=root.firstElementChild;
    require(box.scrollTop===400&&box.scrollLeft===50,'initial committed geometry was not used');
    require(document.activeElement.id==='outside','scroll changed focus');
    await frames();require(box.scrollTop===400,'scroll started a CSS smooth animation');
    apply([scroll(100,500),scroll(200,300)]);require(box.scrollTop===300&&box.scrollLeft===200,'ordered requests lost final coordinates');
    const guarded=(left,top,fromLeft,fromTop)=>m('scrollPosition',1,{value:JSON.stringify([left,top,fromLeft,fromTop])});
    apply([scroll(0,600)]);box.scrollTo({top:700,behavior:'instant'});
    apply([guarded(0,650,0,600)]);require(box.scrollTop===700,'stale correction overwrote newer native scroll');
    apply([guarded(0,750,0,700)]);require(box.scrollTop===750,'matching correction was omitted');
    apply([m('attr',2,{name:'style',value:'width:800px;height:180px'}),guarded(1e12,1e12,0,750)]);

    require(box.scrollTop===box.scrollHeight-box.clientHeight&&box.scrollLeft===box.scrollWidth-box.clientWidth,'native clamping ignored final extent');
    apply([m('attr',1,{name:'dir',value:'rtl'}),scroll(-100,0)]);require(box.scrollLeft===-100&&box.scrollTop===0,'negative RTL position was lost');
    const before=box.outerHTML;
    for(const value of ['{}','[0]','[0,1,2]','[null,0]','["1",0]','[1e999,0]','[NaN,0]',' '.repeat(129)]) {
     let error;try{apply([m('attr',1,{name:'title',value:'must not commit'}),m('scrollPosition',1,{value})]);}catch(failure){error=failure;}
     require(error&&box.outerHTML===before,'invalid coordinate payload changed live DOM');
    }
    let removed;try{apply([scroll(0,0),m('remove',1)]);}catch(error){removed=error;}
    require(removed&&box.isConnected,'removed imperative target was not atomically rejected');
    apply([scroll(0,0)]);require(box.scrollLeft===0,'failure poisoned the next valid commit');
    apply([m('create',3,{name:'div'}),m('insert',3),m('attr',3,{name:'style',value:'height:100px;width:100px;overflow:auto'}),m('create',4,{name:'div'}),m('insert',4,{parent:3}),m('attr',4,{name:'style',value:'height:1000px'}),m('scrollPosition',3,{value:'[0,80,0,0]'})]);
    require(root.lastElementChild.scrollTop===0,'new identity used an old position observation');
    renderer.close();require(!root.childNodes.length,'scroll target survived disposal');
    const {captureReloadInputs}=await import('/host/ui_next/reload-inputs.js');
    const previous=document.body.appendChild(document.createElement('div'));
    previous.innerHTML='<div id="reload-scroll" style="height:100px;width:100px;overflow:auto;scroll-behavior:smooth"><div style="height:1000px"></div></div><textarea id="reload-notes" style="height:40px;width:120px;scroll-behavior:smooth"></textarea>';
    const viewport=previous.firstElementChild,notes=previous.querySelector('textarea');
    notes.value='A long note\n'.repeat(100);notes.setSelectionRange(2,7);
    viewport.scrollTo({top:500,behavior:'instant'});notes.scrollTo({top:80,behavior:'instant'});
    const expectedNotes=notes.scrollTop,restore=captureReloadInputs(previous);
    const next=document.body.appendChild(previous.cloneNode(true));
    try {
      restore(next);
      require(next.firstElementChild.scrollTop===500&&next.querySelector('textarea').scrollTop===expectedNotes,'reload animated or lost native scroll');
      await frames();require(next.firstElementChild.scrollTop===500&&next.querySelector('textarea').scrollTop===expectedNotes,'reload left a pending scroll animation');
    } finally {previous.remove();next.remove();}
    return {passed:true,cases:1,contracts:['initial-geometry','CSS-smooth-overridden','focus-retained','ordered-requests','final-extent-clamping','RTL-negative-left','stale-scroll-preserved','guard-checked-before-layout','new-identity-guard-skipped','invalid-payload-atomicity','removed-target-atomicity','recovery','disposal','instant-reload-position']};
   }finally{renderer.close();}
  });
  assert(result.passed);
  return result;
}
