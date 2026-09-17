import {test,expect} from './fixtures.mjs';

for(const hydrate of [false,true])test(`raw JSON whitespace ${hydrate?'SSR':'client'}`,async({page,appURL})=>{
  let release;const gate=new Promise(resolve=>{release=resolve;});
  await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
  if(!hydrate)await page.route(appURL,async route=>{const response=await route.fetch();await route.fulfill({response,body:(await response.text()).replace('content="server"','content="client"')});});
  const expected={text:'A\rB </script> <!-- <script> & 中文',count:0};
  try {
    await page.goto(appURL,{waitUntil:'commit'});
    if(hydrate)expect(await page.locator('#raw-data').evaluate(element=>{window.earlyData=element;return JSON.parse(element.textContent);})).toEqual(expected);
    else await page.locator('#root').evaluate(root=>root.replaceChildren());
    release();await expect(page.locator('#status')).toHaveText('');
    expect(await page.locator('#raw-data').evaluate(element=>{window.dataNode=element;window.dataText=element.firstChild;return JSON.parse(element.textContent);})).toEqual(expected);
    if(hydrate)expect(await page.locator('#raw-data').evaluate(element=>element===window.earlyData)).toBe(true);
    await page.getByRole('button',{name:'Update data'}).click();
    await expect.poll(()=>page.locator('#raw-data').evaluate(element=>JSON.parse(element.textContent).count)).toBe(1);
    expect(await page.locator('#raw-data').evaluate(element=>JSON.parse(element.textContent))).toEqual({...expected,count:1});
    expect(await page.locator('#raw-data').evaluate(element=>element===window.dataNode&&element.firstChild===window.dataText&&!element.textContent.includes('\r')&&!element.textContent.includes('<'))).toBe(true);
  }finally{release();}
});
