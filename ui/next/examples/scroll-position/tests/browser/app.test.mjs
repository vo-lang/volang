import {test,expect} from './fixtures.mjs';
for(const hydrate of [true,false]) test(`scroll position through Vo (${hydrate?'SSR':'client'})`,async({page,appURL})=>{
 let release;const gate=new Promise(resolve=>{release=resolve;});
 await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
 if(!hydrate)await page.route(appURL,async route=>{const response=await route.fetch();await route.fulfill({response,body:(await response.text()).replace('content="server"','content="client"')});});
 try {
  await page.goto(appURL,{waitUntil:'commit'});
  if(hydrate)await page.getByRole('region',{name:'Scrollable landscape'}).evaluate(element=>{element.scrollTo({top:60,behavior:'instant'});window.earlyBox=element;});
  else await page.locator('#root').evaluate(element=>element.replaceChildren());
  release();
  const box=page.getByRole('region',{name:'Scrollable landscape'});
  await expect(page.locator('[data-position]')).toHaveText(hydrate?'0,60':'0,0');
  if(hydrate)expect(await box.evaluate(element=>element===window.earlyBox)).toBe(true);
  await page.getByRole('button',{name:'Jump',exact:true}).focus();
  await page.getByRole('button',{name:'Jump',exact:true}).press('Enter');
  await expect(page.locator('[data-position]')).toHaveText('40,480');
  await expect(page.getByRole('button',{name:'Jump',exact:true})).toBeFocused();
  await page.getByRole('button',{name:'Remember place'}).click();
  // Move natively and request the old correction in the same browser turn.
  await box.evaluate(element=>{element.scrollTo({left:40,top:600,behavior:'instant'});[...document.querySelectorAll('button')].find(button=>button.textContent==='Correct saved place').click();});
  await expect(page.locator('[data-position]')).toHaveText('40,600');
  expect(await box.evaluate(element=>element.scrollTop)).toBe(600);
  await page.getByRole('button',{name:'Remember place'}).click();
  await page.getByRole('button',{name:'Correct saved place'}).click();
  await expect(page.locator('[data-position]')).toHaveText('0,320');
  await page.getByRole('button',{name:'Extend and jump'}).click();
  await expect(page.locator('[data-position]')).toHaveText('0,2500');
  await page.getByRole('button',{name:'Hide',exact:true}).click();await expect(box).toHaveCount(0);
  await page.getByRole('button',{name:'Restore',exact:true}).click();await expect(box).toBeVisible();
  await expect(page.locator('[data-position]')).toHaveText('0,100');
  await page.getByRole('button',{name:'Right to left'}).click();
  await expect(page.locator('[data-position]')).toHaveText('-100,100');
  expect(await box.evaluate(element=>element.scrollLeft)).toBe(-100);
 }finally{release();}
});
