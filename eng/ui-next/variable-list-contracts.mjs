import assert from 'node:assert/strict';

async function stableLayout(page) {
 return page.evaluate(()=>new Promise((resolve,reject)=>{
  let previous,since=performance.now();const started=since;
  const sample=()=>{
   const list=document.querySelector('#variable-list'),origin=list.getBoundingClientRect().top+list.clientTop;
   const rows=[...list.querySelectorAll('[data-row]')].map(row=>({key:row.dataset.row,top:row.getBoundingClientRect().top-origin,height:row.getBoundingClientRect().height}));
   const anchor=rows.find(row=>row.top<=0&&row.top+row.height>0)??rows.find(row=>row.top>=0);
   const snapshot={top:list.scrollTop,width:list.clientWidth,anchor,rows},next=JSON.stringify(snapshot);
   if(next!==previous){previous=next;since=performance.now();}
   if(anchor&&performance.now()-since>350)return resolve(snapshot);
   if(performance.now()-started>10000)return reject(new Error('Variable list did not finish measurement'));
   requestAnimationFrame(sample);
  };sample();
 }));
}

// Exercise source reload through the same application/CLI supplied by the caller.
export async function checkVariableListReload(page,url,reload) {
 await page.goto(url);await page.locator('#variable-list').waitFor();await stableLayout(page);
 const input=page.getByRole('textbox',{name:'First note draft'});
 await input.fill('Keep the editing place');
 await input.evaluate(input=>input.setSelectionRange(5,12));
 for(const name of ['Expand notes','Change width','Jump ahead']){
  await page.getByRole('button',{name,exact:true}).evaluate(button=>button.click());await stableLayout(page);
 }
 await page.locator('#variable-list').evaluate(list=>{
  const origin=list.getBoundingClientRect().top+list.clientTop;
  const row=[...list.querySelectorAll('[data-row]')].find(row=>row.getBoundingClientRect().top<=origin&&row.getBoundingClientRect().bottom>origin);
  list.scrollTo({top:list.scrollTop+row.getBoundingClientRect().top-origin+row.getBoundingClientRect().height*.75,behavior:'instant'});
 });
 const before=await stableLayout(page);assert(-before.anchor.top>48,'reload probe did not enter a deep row inset');
 await reload();await page.getByRole('heading',{name:'Space for the next thought.'}).waitFor();
 const after=await stableLayout(page);
 assert.equal(after.anchor.key,before.anchor.key,'reload changed the visible item');
 assert(Math.abs(after.anchor.top-before.anchor.top)<1.1,'reload lost the visible inset');
 assert(after.width<before.width,'reload did not change native inline size');
 assert(await input.evaluate(input=>input===document.activeElement),'reload lost pinned editor focus');
 assert.deepEqual(await input.evaluate(input=>({value:input.value,start:input.selectionStart,end:input.selectionEnd})),{value:'Keep the editing place',start:5,end:12});
 await page.getByRole('button',{name:'Expand notes'}).evaluate(button=>button.click());
 const changed=await stableLayout(page);assert.equal(changed.anchor.key,before.anchor.key,'post-reload measurement lost the item');
 return {passed:true,sourceReload:true,keyRetained:true,insetRetained:true,widthChanged:true,focusRetained:true,newMeasurements:true,
  before:{top:before.top,anchor:before.anchor},after:{top:after.top,anchor:after.anchor}};
}
