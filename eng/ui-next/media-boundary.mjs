import assert from 'node:assert/strict';
import {readFile} from 'node:fs/promises';
import {checkMediaProperties} from './media-properties-boundary.mjs';

export async function checkMediaBoundary(page,url) {
  const properties=await checkMediaProperties(page,url);
  const recording=await readFile(new URL('../../ui/next/templates/listening/web/four-notes.wav',import.meta.url));
  await page.route('**/native-media.wav*',route=>route.fulfill({contentType:'audio/wav',body:recording}));
  await page.route('**/native-media-frame',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><label>A frame note<input id="note"></label>'}));
  await page.route('**/native-media-contracts',route=>route.fulfill({contentType:'text/html',body:'<!doctype html><title>Native media</title><div id="root"><audio data-vo-id="1" src="/native-media.wav" preload="none" controls></audio><iframe data-vo-id="2" src="/native-media-frame" title="A frame"></iframe><media-contract data-vo-id="3" data-stable="same" data-change="before"></media-contract></div>'}));
  await page.goto(url+'/native-media-contracts');
  const result=await page.evaluate(async()=>{
    const {DomRenderer}=await import('/host/ui_next/renderer.js');
    const {WIRE_VERSION}=await import('/host/ui_next/generated/protocol.js');
    const require=(ok,message)=>{if(!ok)throw new Error(message);};
    const until=async predicate=>{const deadline=performance.now()+5000;while(!predicate()){if(performance.now()>deadline)throw new Error('Timed out waiting for native media');await new Promise(resolve=>setTimeout(resolve,20));}};
    const changes=[];
    customElements.define('media-contract',class extends HTMLElement {
      static observedAttributes=['data-stable','data-change'];
      attributeChangedCallback(name,previous,value){changes.push({name,previous,value});}
    });
    changes.length=0;
    const container=document.getElementById('root'),audio=container.querySelector('audio'),frame=container.querySelector('iframe');
    const frameDocument=frame.contentDocument,field=frameDocument.getElementById('note');field.value='Before hydration';
    audio.muted=true;audio.loop=true;await audio.play();await until(()=>audio.currentTime>.1);
    let emptied=0;audio.addEventListener('emptied',()=>emptied++);
    const before=audio.currentTime;
    const mutation=(op,id,fields={})=>({op,id,parent:0,before:0,name:'',value:'',...fields});
    const batch=(revision,mutations)=>({version:WIRE_VERSION,revision,inputSequence:0,mutations,commands:null});
    const renderer=new DomRenderer(container,()=>{},true);
    try {
      renderer.applyBatch(batch(1,[
        mutation('create',1,{name:'audio'}),mutation('insert',1),
        mutation('attr',1,{name:'src',value:'/native-media.wav'}),mutation('attr',1,{name:'preload',value:'none'}),mutation('attr',1,{name:'controls',value:''}),
        mutation('create',2,{name:'iframe'}),mutation('insert',2),
        mutation('attr',2,{name:'src',value:'/native-media-frame'}),mutation('attr',2,{name:'title',value:'An updated frame'}),
        mutation('create',3,{name:'media-contract'}),mutation('insert',3),
        mutation('attr',3,{name:'data-stable',value:'same'}),mutation('attr',3,{name:'data-change',value:'after'}),
      ]));
      await new Promise(resolve=>setTimeout(resolve,100));
      require(container.querySelector('audio')===audio && !audio.paused && audio.currentTime>=before && emptied===0,'hydration restarted native audio');
      require(frame.contentDocument===frameDocument && field.isConnected && field.value==='Before hydration','hydration reloaded an unchanged iframe');
      require(frame.title==='An updated frame','hydration skipped a changed attribute');
      require(changes.length===1 && changes[0].name==='data-change' && changes[0].value==='after','hydration replayed unchanged custom element attributes');
      renderer.applyBatch(batch(2,[mutation('attr',1,{name:'src',value:'/native-media.wav?take=2'}),mutation('attr',1,{name:'preload',value:'auto'})]));
      await until(()=>emptied===1 && audio.readyState>=1);
      require(audio.paused && audio.currentTime===0 && audio.currentSrc.endsWith('?take=2'),'changed media source did not take effect');
      await audio.play();await until(()=>audio.currentTime>.1);
      renderer.applyBatch(batch(3,[mutation('insert',1)]));
      await new Promise(resolve=>setTimeout(resolve,50));
      require(!audio.paused && container.lastChild===audio,'synchronous keyed move interrupted native audio');
      audio.currentTime=6;
      renderer.applyBatch(batch(4,[mutation('remove',1)]));
      require(audio.paused && !audio.isConnected,'removal retained playback during a pending seek');
      renderer.applyBatch(batch(5,[mutation('create',4,{name:'video'}),mutation('insert',4),
        mutation('attr',4,{name:'src',value:'/native-media.wav'}),mutation('attr',4,{name:'preload',value:'auto'})]));
      const video=container.querySelector('video');video.muted=true;await video.play();
      await until(()=>video.currentTime>.1);video.currentTime=4;
      renderer.close();require(video.paused && !video.isConnected,'root close retained media playback during a seek');
      return {passed:true,contracts:['hydrated-audio-keeps-playback','hydrated-iframe-keeps-document','unchanged-custom-attributes-not-replayed','changed-attributes-applied','changed-media-source-applied','keyed-media-move-keeps-playback','removed-media-pauses','pending-seek-disposal','closed-video-pauses']};
    } finally {renderer.close();}
  });
  assert.equal(result.passed,true);return {...result,properties};
}
