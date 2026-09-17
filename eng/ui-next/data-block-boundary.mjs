import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {join} from 'node:path';
import {toolchain,compilerPath} from './toolchain.mjs';
import {execute} from './project.mjs';

let fixturePromise;
export async function checkDataBlockBoundary(page,url,fixtureSource) {
  const source=fixtureSource ?? await (fixturePromise ??= execute(compilerPath(),['run',join(toolchain.ui,'next/tests/native-data')],{env:{...process.env,VOWORK:'off'}}));
  const fixture=JSON.parse(source),cases=[];
  const errors=[],requests=[];
  const failed=error=>errors.push(error.message),requested=request=>{if(request.url().endsWith('/must-not-load.js'))requests.push(request.url());};
  page.on('pageerror',failed);page.on('request',requested);
  try {
  for(const hydrate of [false,true]) {
    await page.route('**/native-data-contracts',route=>route.fulfill({contentType:'text/html; charset=utf-8',body:'<!doctype html><meta charset="utf-8"><div id="root">'+(hydrate?fixture.html:'')+'</div>'}));
    await page.goto(url+'/native-data-contracts');
    cases.push(await page.evaluate(async({fixture,hydrate})=>{
      const {DomRenderer}=await import('/host/ui_next/renderer.js');
      const {decodeBatch}=await import('/host/ui_next/generated/codec.js');
      const first=decodeBatch(Uint8Array.from(atob(fixture.batch),c=>c.charCodeAt(0)));
      const root=document.getElementById('root'),retained=root.querySelector('#linked-data');
      const require=(value,message)=>{if(!value)throw new Error(message);};
      const id=name=>first.mutations.find(value=>value.op==='attr'&&value.name==='id'&&value.value===name).id;
      const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
      const renderer=new DomRenderer(root,()=>{},hydrate);let revision=0;
      const apply=mutations=>{renderer.applyBatch({...first,revision:revision+1,mutations});revision++;};
      try {
        if(hydrate)require(JSON.parse(retained.textContent).count===1,'server JSON is unavailable before boot');
        apply(first.mutations);
        const script=root.querySelector('#linked-data'),text=script.firstChild;
        const scriptId=id('linked-data'),textId=first.mutations.find(value=>value.op==='insert'&&value.parent===scriptId).id;
        const expected='Data </script><!--<script> & 中文\r\n\0';
        require(!hydrate||script===retained,'adoption replaced the data block');
        require(JSON.parse(script.textContent).name===expected&&!script.textContent.includes('<'),'JSON data lost characters or parser escaping');
        require(script.childNodes.length===1&&text.nodeType===3,'data block contains identity comments');
        require(JSON.stringify(JSON.parse(root.querySelector('#json-data').textContent))==='[true,null,1.25,"plain"]','plain JSON values changed');
        require(root.querySelector('#after-data').textContent==='Following content remains visible','data consumed following HTML');
        const nextValue='{"name":"Updated \\u003c/script\\u003e","count":2}';
        apply([m('text',textId,{value:nextValue}),m('attr',scriptId,{name:'type',value:'APPLICATION/JSON'}),m('insert',scriptId,{parent:1})]);
        require(script===root.querySelector('#linked-data')&&script.firstChild===text&&JSON.parse(text.nodeValue).count===2,'data update or movement replaced its identity');
        const frame=root.querySelector('iframe'),frameId=first.mutations.find(value=>value.op==='create'&&value.name==='iframe').id;
        require(frame.childNodes.length===0&&frame.getAttribute('srcdoc').includes('<strong>document</strong>'),'iframe source document lost ownership');
        if(!frame.contentDocument?.getElementById('embedded')) await new Promise(resolve=>frame.addEventListener('load',resolve,{once:true}));
        require(frame.contentDocument.getElementById('embedded')?.textContent==='Native document & content','iframe source document did not render');
        let next=Math.max(...first.mutations.map(value=>value.id))+1;
        const invalid=[
          [m('text',textId,{value:'{'})], [m('text',textId,{value:'"<"'})],
          [m('text',textId,{value:'{}\r\n'})], [m('text',textId,{value:'"\0"'})],
          [m('attr',scriptId,{name:'type',value:'module'})], [m('removeAttr',scriptId,{name:'type'})],
          [m('attr',scriptId,{name:'TYPE',value:'text/javascript'}),m('attr',scriptId,{name:'type',value:'application/json'})],
          [m('attr',scriptId,{name:'src',value:'/must-not-load.js'}),m('removeAttr',scriptId,{name:'src'})],
          [m('remove',textId)],
          [m('create',next,{name:'#text',value:'null'}),m('insert',next,{parent:scriptId})],
          [m('create',next,{name:'#text',value:'fallback'}),m('insert',next,{parent:frameId})],
          [m('portal',id('after-data'),{parent:scriptId})], [m('portal',id('after-data'),{parent:frameId})],
          [m('create',next,{name:'script'}),m('insert',next,{parent:1}),m('create',next+1,{name:'#text',value:'{}'}),m('insert',next+1,{parent:next})],
          [m('create',next,{name:'svg'}),m('insert',next,{parent:1}),m('create',next+1,{name:'script'}),m('insert',next+1,{parent:next}),m('attr',next+1,{name:'type',value:'application/json'})],
        ];
        for(const kind of ['template','noscript','xmp','noembed','noframes','plaintext']) invalid.push([m('create',next,{name:kind}),m('insert',next,{parent:1})]);
        for(const mutations of invalid) {
          let error;try {apply([m('attr',id('after-data'),{name:'title',value:'must not commit'}),...mutations]);}catch(failure){error=failure;}
          require(error&&/data|Data|script|JSON|Text|text|NUL|iframe|content binding|portal/.test(error.message),'invalid native data batch passed or failed for an unrelated reason: '+error?.message);
          require(!root.querySelector('#after-data').hasAttribute('title')&&script.textContent===nextValue,'invalid native data batch partially committed');
        }
        // Raw clients may send insertion/text before the final valid type.
        apply([m('create',next,{name:'script'}),m('insert',next,{parent:1}),m('create',next+1,{name:'#text',value:'{"order":true}'}),m('insert',next+1,{parent:next}),m('attr',next,{name:'type',value:'application/ld+json'})]);
        require(root.querySelectorAll('script').length===3,'valid reordered data block did not mount');
        apply([m('remove',textId),m('create',next+2,{name:'#text',value:'null'}),m('insert',next+2,{parent:scriptId})]);
        require(script===root.querySelector('#linked-data')&&script.textContent==='null'&&script.firstChild!==text,'data text replacement changed its parent');
        renderer.close();require(root.childNodes.length===0&&!script.isConnected,'data block retained an owner after close');
        return {hydrate,passed:true};
      }finally{renderer.close();}
    },{fixture,hydrate}));
  }
  await page.evaluate(async()=>{
    const {DomRenderer}=await import('/host/ui_next/renderer.js');
    for(const html of [
      '<script type="application/not-json" data-vo-id="1" data-vo-text="2">{}</script>',
      '<script type="application/json" src="/must-not-load.js" data-vo-id="1" data-vo-text="2">{}</script>',
      '<script type="application/json" data-vo-id="1">{}</script>',
      '<script type="application/json" data-vo-id="1" data-vo-text="1">{}</script>',
      '<svg data-vo-id="1"><script type="application/json" data-vo-id="2"></script></svg>',
      '<template data-vo-id="1"></template>',
    ]) {
      const root=document.body.appendChild(document.createElement('div'));root.innerHTML=html;
      let renderer,error;
      try{renderer=new DomRenderer(root,()=>{},true);}catch(failure){error=failure;}finally{renderer?.close();root.remove();}
      if(!error)throw new Error('Malformed document data was adopted');
    }
  });
  await page.evaluate(()=>new Promise(resolve=>requestAnimationFrame(resolve)));
  assert.deepEqual(errors,[],'native document data executed as a script');
  assert.deepEqual(requests,[],'native document data fetched a script');
  assert(cases.every(value=>value.passed));
  return {passed:true,cases,fixtureSha256:createHash('sha256').update(source).digest('hex'),contracts:['actual-vo-html-and-wire','pre-boot-json','json-character-roundtrip','retained-data-identity','inert-mutation-order','no-script-execution-or-fetch','atomic-invalid-data','special-parser-diagnostics','iframe-document-ownership','hydration-data-validation','data-text-replacement-and-disposal']};
  }finally{page.off('pageerror',failed);page.off('request',requested);}
}
