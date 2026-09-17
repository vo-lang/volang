import assert from 'node:assert/strict';
import test from 'node:test';
import {PopoverHost} from '../../lang/crates/vo-web/dist/ui_next/popovers.js';

test('popover observation survives commits and follows shared or replaced anchors', () => {
  const previous = globalThis.ResizeObserver;
  const counts = {observe:0,unobserve:0,disconnect:0};
  const observed = new Set();
  globalThis.ResizeObserver = class {
    observe(element) {counts.observe++;observed.add(element);}
    unobserve(element) {counts.unobserve++;observed.delete(element);}
    disconnect() {counts.disconnect++;observed.clear();}
  };
  const element = () => {
    const styles = new Map(), attributes = new Map();
    return {
      isConnected:true,popover:'hint',open:false,
      style:{getPropertyValue:name=>styles.get(name)??'',getPropertyPriority:()=>'',setProperty:(name,value)=>styles.set(name,value),removeProperty:name=>styles.delete(name)},
      getAttribute:name=>attributes.get(name)??null,setAttribute:(name,value)=>attributes.set(name,value),removeAttribute:name=>attributes.delete(name),
      getClientRects:()=>[{}],getBoundingClientRect:()=>({left:20,right:60,top:20,bottom:40,width:40,height:20}),
      matches(){return this.open;},showPopover(){this.open=true;},hidePopover(){this.open=false;},contains(other){return this===other;},
    };
  };
  const document = {addEventListener(){},defaultView:{innerWidth:1000,innerHeight:800,addEventListener(){},getComputedStyle:()=>({visibility:'visible',direction:'ltr'})}};
  const host = new PopoverHost(document);
  const anchor=element(),replacement=element(),first=element(),second=element();
  try {
    host.begin();host.set(1,first,10,anchor,'bottom-start',true);host.set(2,second,10,anchor,'top-start',true);host.settle();
    assert.equal(first.open,true);assert.equal(second.open,true);
    assert.equal(counts.observe,3,'one observation for a shared anchor');
    for(let i=0;i<100;i++){host.begin();host.settle();}
    assert.deepEqual(counts,{observe:3,unobserve:0,disconnect:0},'unrelated commits restarted resize delivery');
    host.begin();host.set(1,first,11,replacement,'bottom-start',true);host.settle();
    assert.equal(observed.has(anchor),true,'second popup still owns the old anchor');
    assert.equal(observed.has(replacement),true);
    host.remove(new Set([2]));
    assert.deepEqual(observed,new Set([replacement,first]));
    assert.deepEqual(counts,{observe:4,unobserve:2,disconnect:0});
    host.close();
    assert.equal(observed.size,0);assert.equal(counts.disconnect,1);
    assert.equal(first.open,false);assert.equal(second.open,false);
    assert.equal(first.style.getPropertyValue('position'),'');
  } finally {host.close();globalThis.ResizeObserver=previous;}
});

test('popover layout batches siblings and resolves nested anchors after their parents', () => {
  const saved=globalThis.ResizeObserver, log=[];
  globalThis.ResizeObserver=class {observe(){}unobserve(){}disconnect(){}};
  const element=(name,parent=null)=>{
    const values=new Map();
    const node={
      parent,isConnected:true,popover:'hint',open:false,
      style:{getPropertyValue:key=>values.get(key)??'',getPropertyPriority:()=>'',setProperty(key,value){values.set(key,value);if(key==='left'||key==='top')log.push(`write:${name}`);},removeProperty:key=>values.delete(key)},
      getAttribute:()=>null,setAttribute(){},removeAttribute(){},
      getClientRects:()=>!parent||parent.open?[{}]:[],
      getBoundingClientRect(){log.push(`read:${name}`);const left=parent?Number.parseFloat(parent.style.getPropertyValue('left'))+10:20;return {left,right:left+40,top:20,bottom:40,width:40,height:20};},
      matches(){return this.open;},showPopover(){this.open=true;},hidePopover(){this.open=false;},
      contains(other){for(let current=other;current;current=current.parent)if(current===this)return true;return false;},
    };return node;
  };
  const document={addEventListener(){},defaultView:{innerWidth:1000,innerHeight:800,addEventListener(){},getComputedStyle:()=>({visibility:'visible',direction:'ltr'})}};
  const host=new PopoverHost(document),anchor=element('anchor'),first=element('first'),second=element('second'),nestedAnchor=element('nested-anchor',first),nested=element('nested');
  try {
    host.begin();
    host.set(3,nested,30,nestedAnchor,'bottom-start',true);
    host.set(1,first,10,anchor,'bottom-start',true);
    host.set(2,second,10,anchor,'top-start',true);
    host.settle();
    assert.equal(nested.open,true,'nested popup was removed before opening its parent');
    assert.equal(nested.style.getPropertyValue('left'),'30px','nested anchor used the previous parent position');
    const firstWrite=log.findIndex(value=>value.startsWith('write:'));
    assert.deepEqual(log.slice(0,firstWrite),['read:anchor','read:anchor','read:first','read:second']);
    assert(log.indexOf('read:nested-anchor')>log.indexOf('write:first'));
  } finally {host.close();globalThis.ResizeObserver=saved;}
});
