import {test,expect} from './fixtures.mjs';

for (const hydrate of [true,false]) test(`Optional chart owns its data, styles and lifetime (${hydrate ? 'SSR' : 'client'})`, async ({page,appURL}) => {
  const errors=[],imports=[];
  page.on('pageerror',error=>errors.push(String(error)));
  page.on('request',request=>{if (/\/plot-library-[^/]+\.js$/.test(new URL(request.url()).pathname)) imports.push(request.url());});
  let release;const gate=new Promise(resolve=>{release=resolve;});
  await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
  if (!hydrate) await page.route(appURL,async route=>{
    const response=await route.fetch();
    await route.fulfill({response,body:(await response.text()).replace('content="server"','content="client"')});
  });
  try {
    await page.goto(appURL,{waitUntil:'commit'});
    if (hydrate) {
      await expect(page.getByRole('table')).toBeVisible();
      await expect(page.getByRole('cell',{name:'15 cm',exact:true})).toBeVisible();
      await expect(page.locator('canvas')).toHaveCount(0);
    } else await page.locator('#root').evaluate(element=>element.replaceChildren());
    release();
    // The public starter clears its loading status after activation.
    await expect(page.locator('#status')).toHaveText('');
    await page.getByRole('button',{name:'Change week'}).click();
    await expect(page.getByRole('cell',{name:'20 cm',exact:true})).toBeVisible();
    expect(imports).toEqual([]);
    await expect(page.locator('style[data-ui-plot]')).toHaveCount(0);
    await page.getByRole('button',{name:'Show chart'}).click();
    const canvas=page.locator('canvas');await expect(canvas).toBeVisible();
    await expect(page.getByRole('img',{name:'Basil height over seven days'})).toBeVisible();
    expect(imports).toHaveLength(1);
    await expect(page.locator('style[data-ui-plot]')).toHaveCount(1);
    await canvas.evaluate(element=>{window.plotCanvas=element;window.plotPixels=element.toDataURL();});
    await page.getByRole('button',{name:'Change week'}).click();
    await expect(page.getByRole('cell',{name:'15 cm',exact:true})).toBeVisible();
    await expect.poll(()=>canvas.evaluate(element=>element===window.plotCanvas && element.toDataURL()!==window.plotPixels)).toBe(true);
    const overlay=page.locator('.u-over');await overlay.hover({position:{x:80,y:40}});
    await expect(page.getByRole('status',{name:'Selected measurement'})).toHaveText(/Day [1-7]: \d+ cm/);
    await page.locator('.growth-chart').evaluate(element=>element.style.width='180px');
    await expect.poll(()=>page.locator('.uplot').evaluate(element=>element.getBoundingClientRect().width)).toBe(180);
    expect(await page.locator('.growth-chart').evaluate(element=>element.scrollWidth<=element.clientWidth)).toBe(true);
    await page.getByRole('button',{name:'Hide chart'}).click();
    await expect(canvas).toHaveCount(0);await expect(page.locator('style[data-ui-plot]')).toHaveCount(0);
    expect(await page.evaluate(()=>window.plotCanvas.isConnected)).toBe(false);
    await page.getByRole('button',{name:'Show chart'}).click();await expect(canvas).toBeVisible();
    expect(await canvas.evaluate(element=>element!==window.plotCanvas)).toBe(true);
    expect(imports).toHaveLength(1);
    await page.getByRole('button',{name:'Hide chart'}).click();
    await expect(page.locator('style[data-ui-plot]')).toHaveCount(0);
    expect(errors).toEqual([]);
  } finally {release();}
});
