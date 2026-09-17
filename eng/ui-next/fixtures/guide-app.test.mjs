import {test,expect} from './fixtures.mjs';

test('the complete first-steps guide renders and updates',async({page,appURL})=>{
  await page.goto(appURL);
  await expect(page.getByRole('heading',{name:'A little progress.',exact:true})).toBeVisible();
  await expect(page.getByRole('status',{name:'Progress',exact:true})).toHaveText('Steps taken: 0');
  await page.getByRole('button',{name:'Take a step',exact:true}).click();
  await expect(page.getByRole('status',{name:'Progress',exact:true})).toHaveText('Steps taken: 1');
});
