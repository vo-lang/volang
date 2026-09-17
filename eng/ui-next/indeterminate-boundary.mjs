import assert from 'node:assert/strict';

export async function checkIndeterminateBoundary(page,url) {
  let cases=0;
  for(const mode of ['client','hydrate','early'])for(const checked of [false,true]) {
    const hydrate=mode!=='client';
    const html='<form data-vo-id="1"><input data-vo-id="2" id="mixed" aria-label="Mixed choice" type="checkbox" name="choice" value="selected" data-vo-checked="" data-vo-indeterminate="true" data-vo-events="change=0"'+(checked?' checked':'')+'></form>';
    await page.route('**/mixed-contracts',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Mixed checkbox</title><div id="root">'+(hydrate?html:'')+'</div>'}));
    await page.goto(url+'/mixed-contracts');
    if(mode==='early')await page.getByRole('checkbox',{name:'Mixed choice'}).click();
    await page.evaluate(async({mode,hydrate,checked})=>{
      const {DomRenderer}=await import('/host/ui_next/renderer.js');const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
      const root=document.getElementById('root'),events=[];
      const renderer=new DomRenderer(root,event=>events.push(event),hydrate);let revision=0,ack=0;
      const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
      const property=value=>m('property',2,{name:'indeterminate',value});
      const apply=(mutations=[],sequence=ack)=>{renderer.applyBatch({version:WIRE_VERSION,revision:revision+1,inputSequence:sequence,mutations,commands:null});revision++;ack=sequence;};
      const require=(value,message)=>{if(!value)throw new Error(message);};
      apply([m('create',1,{name:'form'}),m('insert',1),m('create',2,{name:'input'}),m('insert',2,{parent:1}),
        property('true'),m('attr',2,{name:'type',value:'checkbox'}),m('attr',2,{name:'id',value:'mixed'}),
        m('attr',2,{name:'aria-label',value:'Mixed choice'}),m('attr',2,{name:'name',value:'choice'}),m('attr',2,{name:'value',value:'selected'}),
        m('attr',2,{name:'checked',value:String(checked)}),m('listen',2,{name:'change',value:'0'})]);
      const input=root.querySelector('input');
      if(mode==='early') {
        require(input.checked===!checked&&!input.indeterminate&&events.some(value=>value.kind==='change'&&value.checked===!checked),'adoption overwrote or lost early native checkedness');
        apply([property('false'),m('attr',2,{name:'checked',value:String(input.checked)})],events.at(-1).sequence);
        apply([property('true')]);
      }else require(input.checked===checked&&input.indeterminate,'mixed client/adopted state was not initialized');
      require(!input.hasAttribute('indeterminate'),'live property became an ineffective attribute');
      window.mixedContract={root,renderer,input,events,apply,property,m,require};
    },{mode,hydrate,checked});
    const checkbox=page.getByRole('checkbox',{name:'Mixed choice'}),before=await checkbox.isChecked();
    await checkbox.click();
    await page.evaluate(before=>{
      const {input,events,apply,require,m}=window.mixedContract;
      require(input.checked===!before&&!input.indeterminate&&events.at(-1).checked===!before,'native click did not clear mixed state or report checkedness');
      apply([m('attr',1,{name:'title',value:'Unrelated'})]);
      require(input.checked===!before&&!input.indeterminate,'older acknowledgement overwrote the pending native choice');
      apply([],events.at(-1).sequence);
      require(input.indeterminate&&input.checked===before,'constant controlled mixed/checked state did not settle after acknowledgement');
    },before);
    await checkbox.focus();await page.keyboard.press('Space');
    await page.evaluate(()=>{
      const {input,events,apply,property,m,require}=window.mixedContract;
      const checked=input.checked;
      require(!input.indeterminate,'native Space did not clear mixed state');
      apply([property('false'),m('attr',2,{name:'checked',value:String(checked)})],events.at(-1).sequence);
      require(input.checked===checked&&!input.indeterminate,'accepted native choice did not settle');
      require(new FormData(input.form).get('choice')===(checked?'selected':null),'mixed property changed successful-control rules');
      apply([property('true')]);input.form.reset();apply([],events.at(-1).sequence);
      require(input.indeterminate&&input.checked===checked,'form reset lost controlled mixed state');
      for(const mutations of [
        [m('property',2,{name:'unknown',value:'true'})], [property('maybe')],
        [m('property',1,{name:'indeterminate',value:'true'})],
        [m('attr',2,{name:'type',value:'radio'})],
        [m('property',2,{name:'indeterminate',value:'false',parent:1})],
      ]) {
        let error;try{apply([m('attr',1,{name:'title',value:'must not commit'}),...mutations]);}catch(failure){error=failure;}
        require(error&&/property|checkbox/.test(error.message),'invalid property escaped preflight or failed for an unrelated reason');
        require(input.type==='checkbox'&&input.form.title==='Unrelated'&&input.indeterminate,'invalid property partially committed');
      }
      apply([property('')]);input.click();apply([],events.at(-1).sequence);
      require(!input.indeterminate,'released mixed property resumed control');
      apply([property('true')]);
      apply([property('false'),m('remove',2)]);
      const count=events.length;input.click();require(events.length===count&&!input.isConnected,'removed property owner retained listeners');
      window.mixedContract.renderer.close();
    });
    cases++;
  }
  await page.evaluate(async()=>{
    const {DomRenderer}=await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
    for(const delegated of [false,true]) {
      const root=document.body.appendChild(document.createElement('div')),events=[];
      const renderer=new DomRenderer(root,event=>events.push(event));let revision=0;
      const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
      const apply=(mutations,sequence=0)=>renderer.applyBatch({version:WIRE_VERSION,revision:++revision,inputSequence:sequence,mutations,commands:null});
      try {
        apply([m('create',1,{name:'form'}),m('insert',1),m('create',2,{name:'input'}),m('insert',2,{parent:1}),m('attr',2,{name:'type',value:'checkbox'}),m('property',2,{name:'indeterminate',value:'true'}),
          ...(delegated?[m('listen',1,{name:'change:capture',value:'1'})]:[])]);
        const input=root.querySelector('input');input.click();
        if(events.length<2||input.indeterminate||!input.checked)throw new Error('Unhandled mixed input did not produce bounded acknowledgements');
        if(delegated&&!events.some(value=>value.target===1&&value.kind==='change'&&value.checked))throw new Error('Delegated mixed input lost its originating checkbox');
        apply([],events[0].sequence);
        if(input.indeterminate||!input.checked)throw new Error('Partial acknowledgement replaced newer mixed input');
        apply([],events.at(-1).sequence);
        if(!input.indeterminate||!input.checked)throw new Error('Mixed-only binding changed browser-owned checkedness');
      }finally{renderer.close();root.remove();}
    }
    for(const html of ['<input data-vo-id="1" type="radio" data-vo-indeterminate="true">','<input data-vo-id="1" type="checkbox" data-vo-indeterminate="maybe">']) {
      const root=document.body.appendChild(document.createElement('div'));root.innerHTML=html;let renderer,error;
      try{renderer=new DomRenderer(root,()=>{},true);}catch(failure){error=failure;}finally{renderer?.close();root.remove();}
      if(!error)throw new Error('Invalid mixed hydration marker was accepted');
    }
  });
  return {passed:true,cases,contracts:['native-click-and-space','client-adoption-parity','early-choice-replay','pending-acknowledgement','unhandled-and-delegated-mixed-input','browser-owned-checkedness','native-form-data','controlled-reset','property-release','atomic-property-validation','disposed-property-and-listeners']};
}

export async function editEarlyIndeterminate(page) {
  await page.locator('#mixed-all').click();
  assert.equal(await page.locator('#mixed-all').isChecked(),true);
}

export async function checkEarlyIndeterminate(page) {
  await page.waitForFunction(()=>document.getElementById('mixed-all').checked&&!document.getElementById('mixed-all').indeterminate&&document.getElementById('mixed-reading').checked&&document.getElementById('mixed-updates').checked);
  // Restore the ordinary application starting selection for the shared check.
  await page.locator('#mixed-updates').click();
  await page.waitForFunction(()=>document.getElementById('mixed-all').indeterminate);
}

export async function checkIndeterminateApplication(page) {
  const master=page.locator('#mixed-all');
  await page.waitForFunction(()=>document.getElementById('mixed-all')?.indeterminate===true);
  await master.click();
  await page.waitForFunction(()=>document.getElementById('mixed-all').checked&&!document.getElementById('mixed-all').indeterminate&&document.getElementById('mixed-updates').checked);
  await page.locator('#mixed-updates').click();
  await page.waitForFunction(()=>document.getElementById('mixed-all').indeterminate&&!document.getElementById('mixed-all').checked);
  await master.focus();await page.keyboard.press('Space');
  await page.waitForFunction(()=>document.getElementById('mixed-all').checked&&!document.getElementById('mixed-all').indeterminate);
  await master.click();
  await page.waitForFunction(()=>!document.getElementById('mixed-all').checked&&!document.getElementById('mixed-all').indeterminate&&!document.getElementById('mixed-reading').checked);
  await page.evaluate(()=>{document.getElementById('mixed-reading').click();document.getElementById('mixed-form').requestSubmit();});
  await page.waitForFunction(()=>document.querySelector('[data-mixed-saved]')?.textContent==='true/false');
  assert.equal(await master.evaluate(element=>element.indeterminate),true);
  assert.deepEqual(await page.locator('#mixed-form').evaluate(element=>[...new FormData(element).entries()]),[['reading','on']]);
}
