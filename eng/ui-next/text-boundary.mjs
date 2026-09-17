import assert from 'node:assert/strict';
import {createHash} from 'node:crypto';
import {compilerPath} from '../../lang/crates/vo-web/test_compiler.mjs';
import {execute} from './project.mjs';

let fixturePromise;

export async function checkTextBoundary(page,url) {
  // Use actual Vo serialization and the generated binary frame together.
  const source=await (fixturePromise ??= execute(compilerPath(),['run','ui/next/tests/native-text'],{env:{...process.env,VOWORK:'off'}}));
  const fixture=JSON.parse(source),cases=[];
  for (const hydrate of [false,true]) {
    await page.route('**/native-text-contracts',route=>route.fulfill({contentType:'text/html; charset=utf-8',body:'<!doctype html><meta charset="utf-8"><title>Native text</title><div id="root">'+(hydrate?fixture.html:'')+'</div>'}));
    await page.goto(url+'/native-text-contracts');
    cases.push(await page.evaluate(async({fixture,hydrate})=>{
      const {DomRenderer}=await import('/host/ui_next/renderer.js');
      const {decodeBatch}=await import('/host/ui_next/generated/codec.js');
      const first=decodeBatch(Uint8Array.from(atob(fixture.batch),char=>char.charCodeAt(0)));
      const root=document.getElementById('root'),retained=root.querySelector('#native-style');
      const require=(value,message)=>{if(!value)throw new Error(message);};
      const id=name=>first.mutations.find(value=>value.op==='attr'&&value.name==='id'&&value.value===name).id;
      const textId=parent=>first.mutations.find(value=>value.op==='insert'&&value.parent===parent).id;
      const m=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
      const renderer=new DomRenderer(root,()=>{},hydrate);let revision=0;
      const apply=mutations=>{renderer.applyBatch({...first,revision:revision+1,mutations});revision++;};
      try {
        if(hydrate)require(getComputedStyle(root.querySelector('#native-sample')).color==='rgb(17, 34, 51)','server style did not apply before boot');
        if(hydrate)require(root.querySelector('#native-title').textContent===first.mutations.find(value=>value.op==='create'&&value.name==='#text').value,
          'title before adoption: '+JSON.stringify({actual:root.querySelector('#native-title').textContent,expected:first.mutations.find(value=>value.op==='create'&&value.name==='#text').value}));
        apply(first.mutations);
        const style=root.querySelector('#native-style'),text=style.firstChild,title=root.querySelector('#native-title');
        require(!hydrate||style===retained,'adoption replaced native style');
        require(style.childNodes.length===1&&text.nodeType===3&&!text.nodeValue.includes('<!--vo:'),'style contains identity comments');
        require(title.textContent==='Native <title> & "quote"\r\n中文','title escaping or CR changed');
        require(root.querySelector('#ordinary-text').textContent==='CR\r\nLF'&&root.querySelector('#ordinary-text').getAttribute('title')==='CR\r\nLF','ordinary text or attribute CR changed');
        require(root.querySelector('#empty-text').childNodes.length===1&&root.querySelector('#empty-element').childNodes.length===0,'empty Text lost identity');
        const replacement='#native-sample {color: rgb(91, 52, 122)} /* </stylesheet> */';
        apply([m('text',textId(id('native-style')),{value:replacement})]);
        require(style===root.querySelector('#native-style')&&style.firstChild===text&&getComputedStyle(root.querySelector('#native-sample')).color==='rgb(91, 52, 122)','style text update lost identity or native effect');
        apply([m('text',textId(id('native-title')),{value:'Updated </title> & text'})]);
        require(title.textContent==='Updated </title> & text','title update lost literal characters');
        apply([m('insert',id('native-style'),{parent:1})]);
        require(style===root.querySelector('#native-style')&&style.firstChild===text,'keyed movement replaced native text');
        let next=Math.max(...first.mutations.map(value=>value.id))+1;
        const invalid=[
          [m('text',textId(id('native-style')),{value:'</STYLE >'})],
          [m('text',textId(id('native-style')),{value:'a\r\nb'})],
          [m('text',textId(id('native-title')),{value:'a\0b'})],
          [m('create',next,{name:'#text',value:'extra'}),m('insert',next,{parent:id('native-style')})],
          [m('create',next,{name:'span'}),m('insert',next,{parent:id('empty-element')})],
          [m('portal',id('native-sample'),{parent:id('native-style')})],
        ];
        for(const mutations of invalid) {
          let error;
          try {apply([m('attr',id('native-sample'),{name:'title',value:'must not commit'}),...mutations]);}catch(failure){error=failure;}
          require(error&&/text|Text|style|Style|portal|NUL/.test(error.message),'invalid native text batch was accepted or rejected for an unrelated reason');
          require(!root.querySelector('#native-sample').hasAttribute('title')&&style.textContent===replacement,'invalid batch partially changed live DOM');
        }
        // A removed text identity can be replaced without replacing the style.
        apply([m('remove',textId(id('native-style')))]);
        require(style.childNodes.length===0,'text removal retained native content');
        apply([m('create',next,{name:'#text',value:replacement}),m('insert',next,{parent:id('native-style')})]);
        require(style.firstChild!==text&&getComputedStyle(root.querySelector('#native-sample')).color==='rgb(91, 52, 122)','replacement text did not bind its new identity');
        renderer.close();require(root.childNodes.length===0&&!style.isConnected,'close retained native style');
        return {hydrate,passed:true};
      } finally {renderer.close();}
    },{fixture,hydrate}));
  }
  await page.evaluate(async()=>{
    const {DomRenderer}=await import('/host/ui_next/renderer.js');
    for(const html of [
      '<style data-vo-id="1" data-vo-text="1">p{}</style>',
      '<style data-vo-id="1" data-vo-text="9007199254740992"></style>',
      '<style data-vo-id="1" data-vo-text="02"></style>',
      '<div data-vo-id="1" data-vo-text="2">text</div>',
      '<style data-vo-id="1">unmarked text</style>',
    ]) {
      const root=document.body.appendChild(document.createElement('div'));root.innerHTML=html;
      let renderer,error;
      try {renderer=new DomRenderer(root,()=>{},true);}catch(failure){error=failure;}finally{renderer?.close();root.remove();}
      if(!error)throw new Error('Malformed native text markers were accepted');
    }
  });
  assert(cases.every(value=>value.passed));
  return {passed:true,cases,fixtureSha256:createHash('sha256').update(source).digest('hex'),contracts:['actual-vo-html-and-wire','pre-boot-style','escaped-title','carriage-return-roundtrip','empty-text-identity','retained-style-update','keyed-movement','atomic-invalid-content','marker-validation','text-replacement-and-disposal']};
}

export async function checkNativeTextApplication(page) {
  await page.waitForFunction(()=>document.querySelector('[data-native-style]'));
  await page.evaluate(()=>{window.nativeStyleNode=document.querySelector('[data-native-style]');window.nativeStyleText=window.nativeStyleNode.firstChild;});
  for(const color of ['rgb(91, 52, 122)','rgb(37, 83, 65)']) {
    await page.locator('[data-native-style-toggle]').click();
    await page.waitForFunction(color=>getComputedStyle(document.getElementById('native-text-preview')).color===color,color);
    assert(await page.evaluate(()=>window.nativeStyleNode===document.querySelector('[data-native-style]')&&window.nativeStyleText===window.nativeStyleNode.firstChild));
  }
}
