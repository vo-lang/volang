import assert from 'node:assert/strict';

export async function checkPointerApplication(page) {
  const handle=page.getByRole('separator',{name:'Preview width'});
  await handle.scrollIntoViewIfNeeded();
  const box=await handle.boundingBox();assert(box);
  const x=Math.round(box.x+box.width/2),y=Math.round(box.y+box.height/2);
  await page.mouse.move(x,y);await page.mouse.down();
  await page.mouse.move(x+35,y+20);await page.mouse.move(x+70,y+40);await page.mouse.up();
  await page.waitForFunction(()=>document.querySelector('[data-resize-width]')?.textContent==='310 px');
  await page.waitForFunction(()=>document.querySelector('[data-resize-handle]')?.dataset.resizing==='false');
  await handle.focus();await page.keyboard.press('ArrowLeft');
  await page.waitForFunction(()=>document.querySelector('[data-resize-width]')?.textContent==='300 px');
  await page.keyboard.press('Home');
  await page.waitForFunction(()=>document.querySelector('[data-resize-width]')?.textContent==='160 px');
  await page.keyboard.press('End');
  await page.waitForFunction(()=>document.querySelector('[data-resize-width]')?.textContent==='420 px');
  assert.equal(await handle.getAttribute('aria-valuenow'),'420');
}

export async function checkPointerBoundary(page,url) {
  await page.route('**/pointer-contracts',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Pointer contracts</title>'}));
  await page.goto(url+'/pointer-contracts');
  let cases=0;
  for(const hydrate of [false,true])for(const capture of [false,true]) {
    await page.evaluate(async({hydrate,capture})=>{
      const {DomRenderer}=await import('/host/ui_next/renderer.js');
      const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
      const root=document.body.appendChild(document.createElement('div'));
      const flags=16+(capture?1:0),down='pointerdown'+(capture?':capture':'');
      const style='position:fixed;left:20px;top:20px;width:100px;height:100px;touch-action:none;background:tan';
      const kinds=['pointermove','pointerup','pointercancel','gotpointercapture','lostpointercapture'];
      if(hydrate)root.innerHTML=`<div data-vo-id="1"><div data-vo-id="2" style="${style}" data-vo-events="${down}=${flags} ${kinds.map(kind=>kind+'=0').join(' ')}"></div></div><div data-vo-id="3"></div>`;
      const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
      const events=[],renderer=new DomRenderer(root,event=>events.push(event),hydrate);
      let revision=0;
      const apply=mutations=>{renderer.applyBatch({version:WIRE_VERSION,revision:revision+1,inputSequence:events.at(-1)?.sequence??0,mutations,commands:null});revision++;};
      const adopted=root.querySelector('[data-vo-id="2"]');
      apply([m('create',1,{name:'div'}),m('insert',1),m('create',2,{name:'div'}),m('insert',2,{parent:1}),
        m('attr',2,{name:'style',value:style}),m('listen',2,{name:down,value:String(flags)}),
        ...kinds.map(name=>m('listen',2,{name,value:'0'})),m('create',3,{name:'div'}),m('insert',3)]);
      const element=root.firstElementChild.firstElementChild;
      if(hydrate && element!==adopted)throw new Error('pointer adoption replaced the element');
      window.pointerTest={root,element,events,renderer,m,apply,down,flags,capture};
    },{hydrate,capture});
    try {
      await page.mouse.move(50,50);await page.mouse.down();await page.mouse.move(170,170);
      await page.evaluate(()=>{
        const {events,element,apply,m,down,flags,capture}=pointerTest;
        const downEvent=events.find(event=>event.kind==='pointerdown');
        if(!downEvent?.pointer || downEvent.pointer.buttons!==1 || !downEvent.pointer.isPrimary || downEvent.capture!==capture
          || downEvent.pointerType!=='mouse' || downEvent.pointer.clientX!==50 || downEvent.pointer.clientY!==50)throw new Error('native down payload');
        if(!element.hasPointerCapture(downEvent.pointer.id) || !events.some(event=>event.kind==='gotpointercapture'))throw new Error('native capture missing');
        // Rebinding the callback options retains this element's active capture.
        apply([m('listen',2,{name:down,value:String(flags|2)}),m('insert',2,{parent:3})]);
      });
      await page.mouse.move(230,210);await page.mouse.up();
      await page.evaluate(()=>{
        const {events,element}=pointerTest;
        const move=events.filter(event=>event.kind==='pointermove').at(-1),up=events.find(event=>event.kind==='pointerup');
        if(!move?.pointer || move.pointer.clientX!==230 || move.pointer.clientY!==210 || move.pointer.buttons!==1
          || !up?.pointer || up.pointer.buttons!==0 || up.target!==2)throw new Error('capture lost across movement or keyed placement');
        if(element.hasPointerCapture(up.pointer.id) || !events.some(event=>event.kind==='lostpointercapture'))throw new Error('native up did not release capture');
      });
      // Removing the capture option releases ownership while retaining the node.
      await page.mouse.move(50,50);await page.mouse.down();
      await page.evaluate(()=>{const {apply,m,down,capture}=pointerTest;apply([m('listen',2,{name:down,value:capture?'1':'0'})]);});
      await page.mouse.move(240,240);
      assert.equal(await page.evaluate(()=>{const {element,events}=pointerTest;return element.hasPointerCapture(events.filter(event=>event.kind==='pointerdown').at(-1).pointer.id);}),false);
      await page.mouse.up();
      // Fractional pen data is an event snapshot, independent of real capture.
      const payload=await page.evaluate(()=>{
        const {element,events,apply,m,down,flags}=pointerTest;
        element.dispatchEvent(new PointerEvent('pointercancel',{bubbles:true,pointerId:42,clientX:-12.75,clientY:19.125,buttons:0,pressure:0.625,pointerType:'pen',isPrimary:false}));
        const event=events.at(-1);
        apply([m('listen',2,{name:down,value:String(flags)})]);
        return event;
      });
      assert.deepEqual(payload.pointer,{id:42,clientX:-12.75,clientY:19.125,buttons:0,pressure:0.625,isPrimary:false});
      assert.equal(payload.pointerType,'pen');assert.equal(payload.kind,'pointercancel');
      await page.mouse.move(50,50);await page.mouse.down();
      await page.evaluate(()=>{const {renderer,element,events}=pointerTest;renderer.close(false);pointerTest.closedEvents=events.length;if(!element.isConnected)throw new Error('close(false) removed HTML');});
      await page.mouse.move(250,250);await page.mouse.up();
      assert.equal(await page.evaluate(()=>pointerTest.events.length===pointerTest.closedEvents && !pointerTest.element.hasPointerCapture(pointerTest.events.filter(event=>event.kind==='pointerdown').at(-1).pointer.id)),true);
      cases++;
    } finally {
      await page.mouse.up();
      await page.evaluate(()=>{pointerTest.renderer.close();pointerTest.root.remove();delete window.pointerTest;});
    }
  }
  await checkPointerOwnership(page);
  await checkLatestPointerDelivery(page);
  return {passed:true,cases,contracts:['native-pointer-payload','native-capture-outside-bounds','capture-and-bubble','ssr-pointer-adoption',
    'rebind-and-placement-retain-capture','native-up-releases-capture','capture-option-removal','fractional-pen-cancellation-payload','close-retained-html-releases-capture',
    'pending-capture-replacement','external-capture-retained','owner-removal-release','atomic-invalid-capture-options','explicit-latest-pointer-delivery']};
}

async function checkPointerOwnership(page) {
  await page.evaluate(async()=>{
    const {DomRenderer}=await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
    const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
    const root=document.body.appendChild(document.createElement('div')),events=[];
    const renderer=new DomRenderer(root,event=>events.push(event));let revision=0;
    const apply=mutations=>{renderer.applyBatch({version:WIRE_VERSION,revision:revision+1,inputSequence:events.at(-1)?.sequence??0,mutations,commands:null});revision++;};
    apply([m('create',1,{name:'div'}),m('insert',1),m('listen',1,{name:'pointerdown:capture',value:'17'}),
      m('create',2,{name:'div'}),m('insert',2,{parent:1}),m('attr',2,{name:'style',value:'position:fixed;left:20px;top:20px;width:100px;height:100px;touch-action:none'}),
      m('listen',2,{name:'pointerdown',value:'16'}),m('listen',2,{name:'pointermove',value:'0'}),
      m('create',3,{name:'div'}),m('insert',3)]);
    window.pointerOwnerTest={renderer,root,events,apply,m,parent:root.firstElementChild,element:root.firstElementChild.firstElementChild,other:root.lastElementChild};
  });
  try {
    await page.mouse.move(50,50);await page.mouse.down();
    assert.equal(await page.evaluate(()=>{
      const {events,element,parent,apply}=pointerOwnerTest,id=events.at(-1).pointer.id;
      if(!element.hasPointerCapture(id) || parent.hasPointerCapture(id))return false;
      apply([]);return element.hasPointerCapture(id) && !parent.hasPointerCapture(id);
    }),true,'a replaced pending capture reclaimed the pointer');
    await page.mouse.move(150,150);
    assert.equal(await page.evaluate(()=>{
      const {events,element,other,apply}=pointerOwnerTest,id=events.at(-1).pointer.id;
      other.setPointerCapture(id);apply([]);
      return other.hasPointerCapture(id) && !element.hasPointerCapture(id);
    }),true,'a later external capture was reclaimed');
    await page.mouse.up();
    await page.mouse.move(50,50);await page.mouse.down();await page.mouse.move(150,150);
    assert.equal(await page.evaluate(()=>{
      const {events,element,apply,m}=pointerOwnerTest,id=events.at(-1).pointer.id;
      apply([m('remove',2)]);pointerOwnerTest.removedEvents=events.length;
      return !element.hasPointerCapture(id) && !element.isConnected;
    }),true,'disposed owner retained native capture');
    await page.mouse.move(220,220);await page.mouse.up();
    assert.equal(await page.evaluate(()=>pointerOwnerTest.events.length===pointerOwnerTest.removedEvents),true,'removed owner retained a listener');
    await page.evaluate(()=>{
      const {apply,parent,m}=pointerOwnerTest;
      for(const name of ['click','pointermove','pointerup','keydown','viewport']) {
        let rejected=false;
        try {apply([m('attr',1,{name:'title',value:'partial'}),m('listen',1,{name,value:'16'})]);}catch(error) {rejected=/pointer capture requires/.test(String(error));}
        if(!rejected || parent.hasAttribute('title'))throw new Error('invalid capture options partially committed');
      }
    });
  } finally {
    await page.mouse.up();
    await page.evaluate(()=>{pointerOwnerTest.renderer.close();pointerOwnerTest.root.remove();delete window.pointerOwnerTest;});
  }
}

async function checkLatestPointerDelivery(page) {
  await page.evaluate(async()=>{
    const {DomRenderer,InputQueue}=await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
    const {decodeInputBatch}=await import('/host/ui_next/generated/codec.js');
    const root=document.body.appendChild(document.createElement('div')),queue=new InputQueue();
    const renderer=new DomRenderer(root,(event,latest)=>queue.push(event,latest));
    const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
    try {
      renderer.applyBatch({version:WIRE_VERSION,revision:1,inputSequence:0,commands:[],mutations:[
        m('create',1,{name:'div'}),m('insert',1),m('listen',1,{name:'pointermove',value:'32'}),m('listen',1,{name:'click',value:'0'}),
      ]});
      const point=x=>new PointerEvent('pointermove',{bubbles:true,pointerId:1,pointerType:'mouse',buttons:1,clientX:x});
      for(let i=0;i<1000;i++)root.firstChild.dispatchEvent(point(i));
      root.firstChild.click();
      root.firstChild.dispatchEvent(point(1000));root.firstChild.dispatchEvent(point(1001));
      const events=decodeInputBatch(await queue.next()).events;
      if(events.length!==3 || events[0].pointer.clientX!==999 || events[1].kind!=='click' || events[2].pointer.clientX!==1001)throw new Error('latest input lost a barrier or final position');
      if(events.some((event,index)=>index>0&&event.sequence<=events[index-1].sequence))throw new Error('latest input reordered sequences');
    } finally {renderer.close();queue.close();root.remove();}
  });
}
