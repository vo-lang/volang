import assert from 'node:assert/strict';
import test from 'node:test';
import {LayoutObserverHost} from '../../lang/crates/vo-web/dist/ui_next/layout-observer.js';

test('one root shares size/viewport observers and batches reads before reports', () => {
  const saved = globalThis.ResizeObserver, observers = [], frames = new Map(), log = [];
  let next = 1;
  globalThis.ResizeObserver = class {
    targets = new Set();
    constructor(callback) { this.callback = callback; observers.push(this); }
    observe(element) { this.targets.add(element); }
    unobserve(element) { this.targets.delete(element); }
    disconnect() { this.targets.clear(); }
  };
  const window = {requestAnimationFrame(fn) { const id=next++;frames.set(id,fn);return id; },cancelAnimationFrame(id) { frames.delete(id); }};
  const host = new LayoutObserverHost(window), cleanups = [];
  const elements = Array.from({length:100}, (_,i) => ({
    isConnected:true,scrollTop:0,scrollLeft:0,clientHeight:20,
    get clientWidth(){log.push(`read:${i}`);return 100;},
    addEventListener(){},removeEventListener(){},
  }));
  const flush = () => { const pending=[...frames.values()];frames.clear();for(const fn of pending)fn(); };
  try {
    for (const element of elements) {
      cleanups.push(host.observe('size',element,value=>log.push(`size:${value}`)));
      cleanups.push(host.observe('viewport',element,value=>log.push(`viewport:${value}`)));
    }
    assert.equal(observers.length,2);
    assert.equal(frames.size,1);
    observers[0].callback(elements.map(target=>({target,borderBoxSize:[{inlineSize:100.5,blockSize:20.25}]})));
    assert.equal(frames.size,1);
    flush();
    const firstDelivery=log.findIndex(entry=>!entry.startsWith('read:'));
    assert.equal(firstDelivery,100);
    assert.equal(log.length,300);
    assert.equal(log.slice(firstDelivery).some(entry=>entry.startsWith('read:')),false);
    log.length=0;
    observers[0].callback(elements.map(target=>({target,borderBoxSize:[{inlineSize:100.5,blockSize:20.25}]})));
    flush();assert.deepEqual(log,[],'unchanged sizes must not publish');
    observers[0].callback([{target:elements[0],borderBoxSize:[{inlineSize:101,blockSize:20}]}]);
    cleanups[0]();flush();assert.deepEqual(log,[],'removed observation published pending data');
    host.close();host.close();
    assert.equal(frames.size,0);
    assert(observers.every(observer=>observer.targets.size===0));
    for(const cleanup of cleanups)cleanup();
    assert.throws(()=>host.observe('size',elements[0],()=>{}),/closed/);
  } finally {host.close();globalThis.ResizeObserver=saved;}
});

test('layout delivery honors reentrant disposal of later observations', () => {
  const saved=globalThis.ResizeObserver;
  globalThis.ResizeObserver=class {observe(){}unobserve(){}disconnect(){}};
  let frame;
  const host=new LayoutObserverHost({requestAnimationFrame(fn){frame=fn;return 1;},cancelAnimationFrame(){frame=undefined;}});
  const element=()=>({isConnected:true,clientWidth:1,clientHeight:1,scrollTop:0,scrollLeft:0,addEventListener(){},removeEventListener(){}});
  const delivered=[];
  try {
    host.observe('viewport',element(),()=>{delivered.push(1);host.close();});
    host.observe('viewport',element(),()=>delivered.push(2));
    frame();assert.deepEqual(delivered,[1]);
  } finally {host.close();globalThis.ResizeObserver=saved;}
});
