import {test, expect} from './fixtures.mjs';

test('home and notebook remain useful as separate pages', async ({page, appURL}) => {
  let release;const gate=new Promise(resolve=>{release=resolve;});
  await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
  try {
    await page.goto(appURL,{waitUntil:'commit'});
    await expect(page.locator('#page-data')).toHaveAttribute('type','application/ld+json');
    expect(await page.locator('#page-data').evaluate(element=>{window.initialPageData=element;return JSON.parse(element.textContent);})).toEqual({'@context':'https://schema.org','@type':'WebPage',name:'An idea begins here.'});
  }finally{release();}
  await expect(page.locator('#status')).toHaveText('');
  await page.getByRole('button', {name:'Take a step'}).click();
  await expect(page.locator('[data-count]')).toHaveText('1 little steps');
  expect(await page.locator('#page-data').evaluate(element=>element===window.initialPageData)).toBe(true);
  await page.getByRole('link', {name:'Open your notebook'}).click();
  await expect(page.locator('#status')).toHaveText('');
  const thought='A thought </script><!--<script> & 中文\nfor tomorrow.';
  await page.locator('#page-data').evaluate(element=>{window.notebookData=element;window.notebookText=element.firstChild;});
  await page.getByRole('textbox', {name:'Your note'}).fill(thought);
  await page.getByRole('button', {name:'Keep this thought'}).click();
  await expect(page.locator('[data-saved]')).toHaveText(thought);
  expect(await page.locator('#page-data').evaluate(element=>JSON.parse(element.textContent))).toEqual({name:'A little room to write.',saved:thought});
  expect(await page.locator('#page-data').evaluate(element=>element===window.notebookData&&element.firstChild===window.notebookText&&!element.textContent.includes('<'))).toBe(true);
});
