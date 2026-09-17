import assert from 'node:assert/strict';

export async function checkDelegatedInputs(page,url) {
  await page.route('**/delegated-input-contracts',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Input composition</title>'}));
  await page.goto(url+'/delegated-input-contracts');
  const result=await page.evaluate(async()=>{
    const {DomRenderer}=await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
    const require=(ok,message)=>{if(!ok)throw new Error(message);};
    const same=(a,b)=>JSON.stringify(a)===JSON.stringify(b);
    const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
    const batch=(revision,mutations,inputSequence=0)=>({version:WIRE_VERSION,revision,mutations,inputSequence,commands:null});
    const attr=(name,value)=>m('attr',2,{name,value});
    const cases=[
      {name:'text',kind:'input',tag:'input',html:'<input data-vo-id="2" value="before">',initial:[attr('value','before')],
        edit:control=>{control.value='Edited 中文';},read:control=>control.value,expected:'Edited 中文',value:'Edited 中文',commit:[attr('value','Edited 中文')]},
      {name:'textarea',kind:'input',tag:'textarea',html:'<textarea data-vo-id="2" data-vo-controlled>before</textarea>',initial:[attr('value','before')],
        edit:control=>{control.value='A longer thought 中文';},read:control=>control.value,expected:'A longer thought 中文',value:'A longer thought 中文',commit:[attr('value','A longer thought 中文')]},
      {name:'checkbox',kind:'change',tag:'input',html:'<input data-vo-id="2" type="checkbox" data-vo-checked>',initial:[attr('type','checkbox'),attr('checked','false')],
        edit:control=>{control.checked=true;},read:control=>control.checked,expected:true,value:'on',checked:true,commit:[attr('checked','true')]},
      {name:'multiple',kind:'change',tag:'select',html:`<select data-vo-id="2" multiple data-vo-selected='["a"]'><option data-vo-id="3" value="a" selected></option><option data-vo-id="4" value="b"></option></select>`,
        initial:[attr('multiple','true'),m('selection',2,{value:'["a"]'}),...['a','b'].flatMap((value,index)=>[
          m('create',index+3,{name:'option'}),m('insert',index+3,{parent:2}),m('attr',index+3,{name:'value',value})])],
        edit:control=>{control.options[1].selected=true;},read:control=>[...control.selectedOptions].map(option=>option.value),expected:['a','b'],value:'a',selectedValues:['a','b'],commit:[m('selection',2,{value:'["a","b"]'})]},
    ];
    let count=0;
    for(const test of cases)for(const capture of [false,true])for(const hydrate of [false,true]) {
      const root=document.body.appendChild(document.createElement('div')),events=[];
      const binding=test.kind+(capture?':capture':''),flags=capture?'1':'0';
      if(hydrate){root.innerHTML=`<div data-vo-id="1" data-vo-events="${binding}=${flags}">${test.html}</div>`;test.edit(root.querySelector(test.tag));}
      const renderer=new DomRenderer(root,event=>events.push(event),hydrate);
      try {
        renderer.applyBatch(batch(1,[m('create',1,{name:'div'}),m('insert',1),m('listen',1,{name:binding,value:flags}),
          m('create',2,{name:test.tag}),m('insert',2,{parent:1}),...test.initial]));
        const control=root.querySelector(test.tag),label=test.name+'/'+capture+'/'+hydrate;
        if(!hydrate){test.edit(control);control.dispatchEvent(new Event(test.kind,{bubbles:true}));}
        const delivered=events.filter(event=>event.kind===test.kind);
        require(delivered.length===1,label+' lost or duplicated delegated input');
        const event=delivered[0];
        require(event.target===1 && event.capture===capture && event.value===test.value && event.checked===(test.checked??false)
          && same(event.selectedValues,test.selectedValues??null),label+' has the wrong native payload');
        require(same(test.read(control),test.expected),label+' overwrote the native edit');
        // The unhandled-control acknowledgement precedes the parent callback.
        // Its partial acknowledgement cannot settle a still-pending user edit.
        renderer.applyBatch(batch(2,[],event.sequence-1));
        require(same(test.read(control),test.expected),label+' settled before the parent handler was acknowledged');
        renderer.applyBatch(batch(3,test.commit,event.sequence));
        require(same(test.read(control),test.expected),label+' failed to commit the parent result');
        if(test.kind==='input') {
          control.dispatchEvent(new CompositionEvent('compositionstart',{bubbles:true}));
          control.value='In progress';control.dispatchEvent(new InputEvent('input',{bubbles:true,isComposing:true}));
          require(events.at(-1).isComposing,label+' lost composition state at the parent');
          renderer.applyBatch(batch(4,[attr('value','In progress')],events.at(-1).sequence));
          control.value='完成';control.dispatchEvent(new CompositionEvent('compositionend',{bubbles:true,data:'完成'}));
          require(events.at(-1).kind==='input' && events.at(-1).value==='完成' && !events.at(-1).isComposing,label+' lost the final composition value');
          renderer.applyBatch(batch(5,[attr('value','完成')],events.at(-1).sequence));
          require(control.value==='完成',label+' overwrote the final composed input');
        }
        renderer.close();const before=events.length;control.dispatchEvent(new Event(test.kind,{bubbles:true}));
        require(events.length===before,label+' retained a disposed listener');count++;
      } finally {renderer.close();root.remove();}
    }
    const root=document.body.appendChild(document.createElement('div')),events=[];
    root.innerHTML='<div data-vo-id="1" data-vo-events="input=0"><input data-vo-id="2" value="before" data-vo-events="change=0"></div>';
    root.querySelector('input').value='Early own change';
    const renderer=new DomRenderer(root,event=>events.push(event),true);
    try {
      require(events.length===1 && events[0].kind==='change' && events[0].target===2 && events[0].value==='Early own change','adoption changed an existing target handler preference');
    } finally {renderer.close();root.remove();}
    const buttonRoot=document.body.appendChild(document.createElement('div')),clicks=[];
    const buttons=new DomRenderer(buttonRoot,event=>clicks.push(event));
    try {
      buttons.applyBatch(batch(1,[m('create',1,{name:'button'}),m('insert',1),m('attr',1,{name:'value',value:'save'}),m('listen',1,{name:'click',value:'0'}),
        m('create',2,{name:'span'}),m('insert',2,{parent:1})]));
      buttonRoot.querySelector('span').click();require(clicks.length===1 && clicks[0].value==='save','non-input handler value changed');
    } finally {buttons.close();buttonRoot.remove();}
    return {passed:true,cases:count,contracts:['originating-control-values','delegated-checkbox-state','delegated-multiple-selection','capture-and-bubble','early-delegated-input','partial-ack-keeps-native-edit','parent-result-commits','disposed-listener-release','target-handler-preference','non-input-value-compatibility','delegated-composition-end']};
  });
  assert.equal(result.passed,true);return result;
}
