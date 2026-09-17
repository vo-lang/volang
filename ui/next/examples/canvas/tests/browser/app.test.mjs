import {test,expect} from './fixtures.mjs';

for (const hydrate of [true, false]) test(`Vo bitmap updates and lifetime (${hydrate ? 'SSR' : 'client'})`, async ({page,appURL}) => {
  let release; const gate = new Promise(resolve => { release = resolve; });
  await page.route('**/assets/app.*',async route=>{await gate; await route.continue();});
  if (!hydrate) await page.route(appURL, async route => {
    const response = await route.fetch();
    await route.fulfill({response,body:(await response.text()).replace('content="server"','content="client"')});
  });
  try {
    await page.goto(appURL,{waitUntil:'commit'});
    if (hydrate) {
      await expect(page.getByRole('img',{name:/blue Mandelbrot/})).toBeVisible();
      await expect(page.locator('canvas')).toHaveCount(0);
      await expect(page.getByText('96 × 64 pixels')).toBeVisible();
    } else await page.locator('#root').evaluate(element => element.replaceChildren());
    release();
    const canvas = page.locator('canvas'); await expect(canvas).toBeVisible();
    const original = await canvas.evaluate(element => {
      window.originalCanvas = element;
      window.originalPixels = element.toDataURL();
      window.draws = 0;
      const original = CanvasRenderingContext2D.prototype.putImageData;
      CanvasRenderingContext2D.prototype.putImageData = function(...args) {window.draws++; return original.apply(this,args);};
      return {width:element.width,height:element.height,pixel:[...element.getContext('2d').getImageData(0,0,1,1).data]};
    });
    expect(original).toEqual({width:96,height:64,pixel:[49,103,143,255]});
    await page.getByRole('button',{name:'Count a moment'}).click();
    await expect(page.locator('[data-count]')).toHaveText('1 moments');
    expect(await page.evaluate(()=>window.draws)).toBe(0);
    await page.getByRole('button',{name:'Change palette'}).click();
    await expect(page.getByRole('img',{name:/copper Mandelbrot/})).toBeVisible();
    expect(await canvas.evaluate(element=>element === window.originalCanvas && element.toDataURL() !== window.originalPixels)).toBe(true);
    expect(await page.evaluate(()=>window.draws)).toBe(1);
    await page.getByRole('button',{name:'Show or hide'}).click();
    await expect(canvas).toHaveCount(0);
    expect(await page.evaluate(()=>[window.originalCanvas.width,window.originalCanvas.height])).toEqual([0,0]);
    await page.getByRole('button',{name:'Show or hide'}).click(); await expect(canvas).toBeVisible();
    expect(await canvas.evaluate(element=>element !== window.originalCanvas)).toBe(true);
    await canvas.evaluate(element=>{window.lostCanvas=element;element.dispatchEvent(new Event('contextlost'));});
    await expect(page.getByRole('alert')).toHaveText('This landscape could not be displayed.');
    expect(await page.evaluate(()=>[window.lostCanvas.width,window.lostCanvas.height])).toEqual([0,0]);
    await page.getByRole('button',{name:'Try again'}).click(); await expect(canvas).toBeVisible();
    await expect(page.getByRole('img',{name:/copper Mandelbrot/})).toBeVisible();
  } finally {release();}
});
