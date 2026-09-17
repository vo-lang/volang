import assert from 'node:assert/strict';
import {mkdir, readFile, writeFile} from 'node:fs/promises';
import {createHash} from 'node:crypto';
import {resolve} from 'node:path';
import {pathToFileURL} from 'node:url';
import {build} from 'esbuild';

const root=resolve(import.meta.dirname,'../../..');
const directory=resolve(root,'target/ui-next/host-work');
await mkdir(directory,{recursive:true});
const sources=['topology.ts','nodes.ts','elements.ts','text.ts','events.ts','selection.ts','text-selection.ts','widgets.ts','popovers.ts','input.ts','generated/codec.ts','generated/protocol.ts'];
await build({stdin:{contents:`export {prepareBatch} from './topology.ts'; export {WidgetHost} from './widgets.ts'; export {InputQueue} from './input.ts'; export {WIRE_VERSION} from './generated/protocol.ts'; export {decodeInputBatch} from './generated/codec.ts';`,resolveDir:resolve(root,'lang/crates/vo-web/js/ui_next'),loader:'ts'},bundle:true,platform:'node',format:'esm',outfile:resolve(directory,'probe-runtime.mjs')});
const {prepareBatch,WidgetHost,InputQueue,WIRE_VERSION,decodeInputBatch}=await import(pathToFileURL(resolve(directory,'probe-runtime.mjs')));
const namespace='http://www.w3.org/1999/xhtml';
function fixture(width){
  const counts={clonedChildSlots:0,indexComparisons:0,shiftedChildSlots:0};
  function tracked(values){
    Object.defineProperties(values,{
      slice:{value:function(...args){const result=Array.prototype.slice.apply(this,args);counts.clonedChildSlots+=result.length;return tracked(result);}},
      indexOf:{value:function(value){const index=Array.prototype.indexOf.call(this,value);counts.indexComparisons+=index<0?this.length:index+1;return index;}},
      splice:{value:function(start,remove,...insert){if(remove!==insert.length) counts.shiftedChildSlots+=this.length-start-remove;return Array.prototype.splice.call(this,start,remove,...insert);}},
    });return values;
  }
  const entry=(kind,parent,children=[])=>({kind,parent,children:tracked(children),namespace,start:{nodeValue:'old'},listeners:new Map(),editedAt:0,composing:false});
  const nodes=new Map([[0,entry('#root',null,[1])],[1,entry('div',0,Array.from({length:width},(_,i)=>2+i))]]);
  for(let i=0;i<width;i++){nodes.set(2+i,entry('span',1,[width+2+i]));nodes.set(width+2+i,entry('#text',2+i));}
  const state={nodes,revision:1,inputSequence:0,sequence:0,highestId:width*2+1,hydrating:false};
  const batch=mutations=>({version:WIRE_VERSION,revision:2,inputSequence:0,mutations,commands:[]});
  return {counts,nodes,state,batch};
}
const textUpdates=[],reorders=[];
for(const width of [100,1000,10000]){
  const f=fixture(width);
  const prepared=prepareBatch(f.batch([{op:'text',id:width+2,parent:0,before:0,name:'',value:'new'}]),f.state,{supports:()=>false});
  assert.equal(prepared.shapes.get(width+2).text,'new');
  assert.equal(f.nodes.get(width+2).start.nodeValue,'old');
  assert.equal(f.nodes.get(1).children[0],2);
  textUpdates.push({siblings:width,mutations:1,preparedShapes:prepared.shapes.size,...f.counts});
  const g=fixture(width);
  // Move each successive sibling before the previous one, producing a reversal.
  const mutations=Array.from({length:width},(_,i)=>({op:'insert',id:2+i,parent:1,before:i===0?0:1+i,name:'',value:''}));
  const moved=prepareBatch(g.batch(mutations),g.state,{supports:()=>false});
  assert.deepEqual([...moved.shapes.get(1).children],Array.from({length:width},(_,i)=>width+1-i));
  assert.equal(g.nodes.get(1).children[0],2);
  reorders.push({siblings:width,mutations:width,...g.counts});
}
const widgetCommits=[];
for(const count of [1,100,1000]){
  let callbacks=0,disposed=0;
  const parent={parentNode:null},targets=Array.from({length:count},()=>({parentNode:parent}));
  const host=new WidgetHost(()=>{}, {probe:({element})=>({update(){},commitTargets:()=>[element],afterCommit(){callbacks++;},dispose(){disposed++;}})});
  for(let id=1;id<=count;id++)host.apply(id,targets[id-1],'probe','');
  host.afterCommit();assert.equal(callbacks,count);callbacks=0;
  host.afterCommit(new Set([{parentNode:parent}]));
  const unrelatedCallbacks=callbacks;assert.equal(callbacks,0);
  host.afterCommit(new Set([targets[0]]));
  const oneControlCallbacks=callbacks;assert.equal(callbacks,1);callbacks=0;
  host.afterCommit(new Set([parent]));
  assert.equal(callbacks,count);
  host.close();assert.equal(disposed,count);
  widgetCommits.push({widgets:count,unrelatedCallbacks,oneControlCallbacks,ancestorCallbacks:callbacks});
}
const inputBatches=[];
for(const count of [1,100,1000]){
  const queue=new InputQueue();
  const pending=queue.next();
  for(let sequence=1;sequence<=count;sequence++)queue.push({target:1,kind:'pointermove',value:'',key:'',checked:false,sequence,error:'',capture:false,altKey:false,ctrlKey:false,metaKey:false,shiftKey:false,repeat:false,isComposing:false,button:0,pointerType:'mouse',selectedValues:null,pointer:{id:1,clientX:sequence,clientY:0,buttons:1,pressure:0.5,isPrimary:true}});
  const first=await pending;
  let delivered=decodeInputBatch(first).events,bytes=first.length,batches=1;
  const firstBatch=delivered.length;
  while(delivered.length<count){const next=await queue.next();bytes+=next.length;batches++;delivered=delivered.concat(decodeInputBatch(next).events);}
  assert.equal(delivered.length,count);assert.equal(delivered.at(-1).sequence,count);
  queue.close();inputBatches.push({queuedEvents:count,deliveredInFirstBatch:firstBatch,batches,bytes});
}
const hashes=Object.fromEntries(await Promise.all(sources.map(async name=>[name,createHash('sha256').update(await readFile(resolve(root,'lang/crates/vo-web/js/ui_next',name))).digest('hex')])));
const report={schema:'volang.ui-scope-structural-audit.v1',date:new Date().toISOString(),node:process.version,passed:true,sourceHashes:hashes,method:'Execute current production prepareBatch, WidgetHost and InputQueue. Count copied child entries, dense-array search comparisons and logically shifted entries; no elapsed-time or browser paint claims. Fake node metadata and widget instances isolate host algorithms. Reordering uses a valid reverse-order mutation stream and asserts unchanged input tree plus exact output.',textUpdates,reorders,widgetCommits,inputBatches};
await writeFile(resolve(directory,'report.json'),JSON.stringify(report,null,2)+'\n');
console.log(JSON.stringify({passed:true,textUpdates,reorders,widgetCommits,inputBatches},null,2));
