import {test,expect} from './fixtures.mjs';
const snapshot=page=>page.locator('#variable-list').evaluate(list=>{
 const origin=list.getBoundingClientRect().top+list.clientTop;
 const rows=[...list.querySelectorAll('[data-row]')].map(row=>({key:row.dataset.row,top:row.getBoundingClientRect().top-origin,height:row.getBoundingClientRect().height,offset:parseFloat(row.style.top),node:row}));
 const anchor=rows.find(row=>row.top+row.height>0 && row.top<=0)??rows.find(row=>row.top>=0);
 return {top:list.scrollTop,max:list.scrollHeight-list.clientHeight,count:rows.length,anchor:anchor&&{key:anchor.key,top:anchor.top},rows:rows.map(({node,...row})=>row),gap:rows.slice(1).reduce((gap,row,index)=>Math.max(gap,Math.abs(row.top-rows[index].top-rows[index].height)),0)};
});
const settled=async page=>{
 let previous,stable=0;
 await expect.poll(async()=>{
  const value=await snapshot(page),signature=JSON.stringify(value);
  stable=signature===previous?stable+1:0;previous=signature;
  // Native layout at multi-million-pixel offsets may snap to one CSS pixel.
  return value.anchor!==undefined&&value.count>0&&value.count<40&&value.gap<1.1&&stable>=3;
 },{timeout:15000,intervals:[50,50,100]}).toBe(true);
};
for(const hydrate of [true,false])test(`variable rows ${hydrate?'SSR':'client'}`,async({page,appURL})=>{
 let release;const gate=new Promise(resolve=>{release=resolve;});
 await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
 if(!hydrate)await page.route(appURL,async route=>{const response=await route.fetch();await route.fulfill({response,body:(await response.text()).replace('content="server"','content="client"')});});
 try{
  await page.goto(appURL,{waitUntil:'commit'});
  if(hydrate)await page.locator('#variable-list').evaluate(list=>{window.earlyList=list;list.scrollTo({top:100,behavior:'instant'});});
  else await page.locator('#root').evaluate(root=>root.replaceChildren());
  release();await settled(page);
  if(hydrate)expect(await page.locator('#variable-list').evaluate(list=>list===window.earlyList)).toBe(true);
  await page.getByRole('button',{name:'Jump ahead'}).click();
  await expect.poll(async()=>(await snapshot(page)).top).toBeGreaterThan(4000);
  await settled(page);
  const before=await snapshot(page);
  await page.locator(`[data-row="${before.anchor.key}"]`).evaluate(row=>{window.retainedRow=row;});
  await page.getByRole('button',{name:'Expand notes'}).click();await settled(page);
  await expect.poll(async()=>(await snapshot(page)).anchor?.key).toBe(before.anchor.key);
  expect(Math.abs((await snapshot(page)).anchor.top-before.anchor.top)).toBeLessThan(1.1);
  expect(await page.locator(`[data-row="${before.anchor.key}"]`).evaluate(row=>row===window.retainedRow)).toBe(true);
  await page.getByRole('button',{name:'Change width'}).click();await settled(page);
  expect(await page.locator('#variable-list').evaluate(list=>list.getBoundingClientRect().width)).toBeCloseTo(260.25,2);
  await expect.poll(async()=>(await snapshot(page)).anchor?.key).toBe(before.anchor.key);
  const resized=await snapshot(page);
  await page.getByRole('button',{name:'Reverse notes'}).click();await settled(page);
  await expect.poll(async()=>(await snapshot(page)).anchor?.key).toBe(before.anchor.key);
  expect(Math.abs((await snapshot(page)).anchor.top-resized.anchor.top)).toBeLessThan(1.1);
  expect(await page.locator(`[data-row="${before.anchor.key}"]`).evaluate(row=>row===window.retainedRow)).toBe(true);
  await page.getByRole('button',{name:'100,000 notes'}).click();
  await expect(page.locator('[data-range]')).toContainText('of 100000');await settled(page);
  expect((await snapshot(page)).anchor.key).toBe(before.anchor.key);
  expect((await snapshot(page)).count).toBeLessThan(40);
  await page.getByRole('button',{name:'Five notes'}).click();
  await expect(page.locator('[data-range]')).toContainText('of 5');await settled(page);
  const small=await snapshot(page);expect(small.top).toBeLessThanOrEqual(small.max);expect(small.count).toBeLessThanOrEqual(5);
  await page.getByRole('button',{name:'Show or hide'}).click();await expect(page.locator('#variable-list')).toHaveCount(0);
  await page.getByRole('button',{name:'Show or hide'}).click();await settled(page);
 }finally{release();}
});

test('a pinned editor retains identity, focus and selection outside the viewport',async({page,appURL})=>{
 await page.goto(appURL);await settled(page);
 const input=page.getByRole('textbox',{name:'First note draft'});
 await input.fill('Keep this thought');
 await input.evaluate(input=>{window.retainedEditor=input;input.setSelectionRange(1,7);});
 await page.locator('#variable-list').evaluate(list=>list.scrollTo({top:6000,behavior:'instant'}));
 await expect.poll(()=>page.locator('[data-row]').evaluateAll(rows=>rows.some(row=>Number(row.dataset.row)>50))).toBe(true);
 await expect(input).toBeFocused();await expect(input).toHaveValue('Keep this thought');
 expect(await input.evaluate(input=>({same:input===window.retainedEditor,start:input.selectionStart,end:input.selectionEnd}))).toEqual({same:true,start:1,end:7});
 expect(await page.locator('[data-row]').count()).toBeLessThan(40);
 await page.getByRole('button',{name:'Reverse notes'}).evaluate(button=>button.click());
 await expect(page.locator('[data-row="0"]')).toHaveAttribute('aria-posinset','1000');
 await expect(input).toBeFocused();
 expect(await input.evaluate(input=>input===window.retainedEditor)).toBe(true);
 await expect(input).toHaveValue('Keep this thought');
 await page.getByRole('button',{name:'Show or hide'}).click();await expect(input).toHaveCount(0);
});
