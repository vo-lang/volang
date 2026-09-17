import {test,expect} from './fixtures.mjs';

async function nativeModifiers(locator,key) {
  const prevented=await locator.evaluate((element,key)=>['altKey','ctrlKey','metaKey','shiftKey'].map(modifier=>{
    const event=new KeyboardEvent('keydown',{key,[modifier]:true,bubbles:true,cancelable:true});
    element.dispatchEvent(event);return event.defaultPrevented;
  }),key);
  expect(prevented).toEqual([false,false,false,false]);
}

for(const hydrate of [true,false])test(`kit native keys and retained accordion ${hydrate?'SSR':'client'}`,async({page,appURL})=>{
  let release;const gate=new Promise(resolve=>{release=resolve;});
  await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
  if(!hydrate)await page.route(appURL,async route=>{const response=await route.fetch();await route.fulfill({response,body:(await response.text()).replace('content="server"','content="client"')});});
  try {
    await page.goto(appURL,{waitUntil:'commit'});
    if(!hydrate)await page.locator('#root').evaluate(element=>element.replaceChildren());
    else await page.locator('#accordion-trigger-0').evaluate(element=>{window.initialTrigger=element;});
    release();await expect(page.locator('#status')).toHaveText('');
    if(hydrate)expect(await page.locator('#accordion-trigger-0').evaluate(element=>element===window.initialTrigger)).toBe(true);
    await expect(page.locator('.custom-tabs')).toHaveCount(1);
    await expect(page.getByRole('tab',{name:'Alpha',exact:true})).toHaveCount(1);
    await expect(page.getByRole('button',{name:'Actions',exact:true})).toHaveCount(1);
    const first=page.locator('#tabs-tab-0');await first.focus();await nativeModifiers(first,'ArrowRight');
    await expect(first).toHaveAttribute('aria-selected','true');await page.keyboard.press('ArrowRight');await expect(page.locator('#tabs-tab-1')).toHaveAttribute('aria-selected','true');
    const menu=page.locator('#menu-trigger');await menu.focus();await nativeModifiers(menu,'ArrowDown');await expect(menu).toHaveAttribute('aria-expanded','false');
    await page.keyboard.press('ArrowDown');await expect(page.locator('#menu-item-0')).toBeFocused();await nativeModifiers(page.locator('#menu-item-0'),'End');
    await page.keyboard.press('End');await expect(page.locator('#menu-item-1')).toBeFocused();await page.keyboard.press('Escape');
    const list=page.locator('#list');await list.focus();await nativeModifiers(list,'ArrowDown');await expect(list).toHaveAttribute('aria-activedescendant','list-option-0');
    await page.keyboard.press('ArrowDown');await expect(list).toHaveAttribute('aria-activedescendant','list-option-1');
    const combo=page.locator('#combo');await combo.focus();await nativeModifiers(combo,'ArrowDown');await expect(combo).toHaveAttribute('aria-expanded','false');
    await page.keyboard.press('ArrowDown');await expect(combo).toHaveAttribute('aria-activedescendant','combo-option-0');await nativeModifiers(combo,'Enter');
    await expect(combo).toHaveValue('');await page.keyboard.press('Enter');await expect(combo).toHaveValue('Alpha');
    const stable=page.getByRole('button',{name:'Stable',exact:true}),note=page.getByRole('textbox',{name:'Note',exact:true});
    await stable.click();await note.fill('Retain 中文');await note.evaluate(element=>{window.note=element;});
    await stable.click();await expect(stable).toBeFocused();await expect(note).toBeHidden();
    await stable.click();await expect(note).toHaveValue('Retain 中文');await note.focus();
    await page.getByRole('button',{name:'Reverse',exact:true}).evaluate(element=>element.click());
    await expect(stable).toHaveAttribute('id','accordion-trigger-7');await expect(note).toBeFocused();
    expect(await note.evaluate(element=>element===window.note)).toBe(true);
    await page.getByRole('button',{name:'Replace other items',exact:true}).click();await expect(stable).toHaveAttribute('aria-expanded','true');await expect(note).toHaveValue('Retain 中文');
    await page.getByRole('button',{name:'Toggle stable item',exact:true}).click();await expect(stable).toHaveCount(0);
    await page.getByRole('button',{name:'Toggle stable item',exact:true}).click();await stable.click();await expect(note).toHaveValue('');
    expect(await note.evaluate(element=>element===window.note)).toBe(false);
  }finally{release();}
});
