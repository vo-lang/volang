import {test,expect} from './fixtures.mjs';

test('a wrapper owns input updates across activation',async({page,appURL})=>{
  let release;const gate=new Promise(resolve=>{release=resolve;});
  await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
  try {
    await page.goto(appURL,{waitUntil:'commit'});
    const input=page.getByRole('textbox',{name:'What should we call you?'});
    await input.fill('Early 中文');await input.evaluate(element=>{window.earlyInput=element;});
    release();
    await expect(page.getByRole('heading',{name:'Make something good, Early 中文.'})).toBeVisible();
    expect(await input.evaluate(element=>element===window.earlyInput)).toBe(true);
    await input.fill('Ada');
    await expect(page.getByRole('heading',{name:'Make something good, Ada.'})).toBeVisible();
    await page.getByRole('button',{name:'Make it happen'}).click();
    await expect(page.getByRole('status')).toHaveText('1 little steps');
  } finally {release();}
});
