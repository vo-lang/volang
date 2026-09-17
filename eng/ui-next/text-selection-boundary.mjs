import assert from 'node:assert/strict';

export async function checkTextSelectionBoundary(page,url) {
  await page.route('**/text-selection-contracts',route=>route.fulfill({contentType:'text/html; charset=utf-8',
    body:'<!doctype html><title>Source selection</title><button id="outside">Another control</button><main></main>'}));
  await page.goto(url+'/text-selection-contracts');
  const result = await page.evaluate(async()=>{
    const {DomRenderer}=await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
    const require=(value,message)=>{if(!value)throw new Error(message);};
    const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
    const root=document.querySelector('main'),events=[],outside=document.querySelector('button');
    const renderer=new DomRenderer(root,event=>events.push(event));let revision=0;
    const apply=(mutations,inputSequence=events.at(-1)?.sequence??0)=>{
      renderer.applyBatch({version:WIRE_VERSION,revision:revision+1,inputSequence,mutations,commands:[]});revision++;
    };
    const source='中文🙂\nmissing()';
    const select=(text=source,start=5,end=12,direction='forward',id=1)=>m('textSelection',id,
      {value:JSON.stringify({source:text,selection:{start,end,direction}})});
    try {
      outside.focus();
      apply([m('create',1,{name:'textarea'}),m('insert',1),m('attr',1,{name:'value',value:source}),
        m('listen',1,{name:'input',value:'0'}),select()]);
      const input=root.firstElementChild;
      require(document.activeElement===input&&input.value.slice(input.selectionStart,input.selectionEnd)==='missing','initial selection lost UTF-16 range or focus');
      apply([select(source,0,2,'backward')]);
      require(input.selectionStart===0&&input.selectionEnd===2&&input.selectionDirection==='backward','selection direction lost');
      outside.focus();
      apply([select('old source',0,3)]);
      require(document.activeElement===outside&&input.selectionStart===0&&input.selectionEnd===2,'stale source changed selection or focus');
      input.value='new input';input.dispatchEvent(new InputEvent('input',{bubbles:true}));
      apply([select('new input',0,3)],0);
      require(document.activeElement===outside,'unacknowledged edit allowed a selection');
      apply([m('attr',1,{name:'value',value:source})]);
      input.dispatchEvent(new CompositionEvent('compositionstart',{bubbles:true}));
      apply([select()]);require(document.activeElement===outside,'selection interrupted composition');
      input.dispatchEvent(new CompositionEvent('compositionend',{bubbles:true}));
      apply([m('attr',1,{name:'disabled',value:'true'}),select()]);
      require(document.activeElement===outside,'disabled control stole focus');
      apply([m('attr',1,{name:'disabled',value:'false'}),m('attr',1,{name:'readonly',value:'true'}),select()]);
      require(document.activeElement===input&&input.selectionStart===5,'readonly source could not be selected');
      const before=root.innerHTML;
      for (const value of ['null','{}',JSON.stringify({source,selection:{start:3,end:4,direction:'forward'}}),
        JSON.stringify({source,selection:{start:0,end:100,direction:'forward'}}),
        JSON.stringify({source,selection:{start:5,end:4,direction:'forward'}})]) {
        let failed=false;try{apply([m('attr',1,{name:'title',value:'must not commit'}),m('textSelection',1,{value})]);}catch{failed=true;}
        require(failed&&root.innerHTML===before,'invalid range changed live DOM');
      }
      let failed=false;try{apply([select(),m('remove',1)]);}catch{failed=true;}
      require(failed&&input.isConnected,'removed target bypassed preflight');
      const singleLine='中文🙂 missing()';
      apply([m('create',2,{name:'input'}),m('insert',2),m('attr',2,{name:'type',value:'text'}),m('attr',2,{name:'value',value:singleLine}),select(singleLine,0,2,'forward',2)]);
      const textInput=root.lastElementChild;
      require(document.activeElement===textInput&&textInput.selectionEnd===2,'text input selection failed');
      failed=false;try{apply([select(singleLine,0,2,'forward',2),m('attr',2,{name:'type',value:'number'})]);}catch{failed=true;}
      require(failed&&textInput.type==='text','final input type was not checked atomically');
      apply([select()]);require(input.selectionStart===5,'invalid request poisoned subsequent selection');
      return {passed:true,contracts:['utf16-source-range','new-binding','backward-selection','stale-source-preserved',
        'unacknowledged-input-preserved','composition-preserved','disabled-preserved','readonly-selection',
        'invalid-range-atomicity','removed-target-atomicity','final-control-type','recovery']};
    } finally {renderer.close();}
  });
  assert(result.passed);return result;
}
