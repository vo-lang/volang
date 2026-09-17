import assert from 'node:assert/strict';

export async function checkPlotReload(page,url,reload) {
  const errors=[],documents=[];
  page.on('pageerror',error=>errors.push(String(error)));
  page.on('framenavigated',frame=>{if(frame===page.mainFrame())documents.push(frame.url());});
  await page.goto(url);
  await page.waitForFunction(()=>document.getElementById('status')?.textContent==='');
  await page.getByRole('button',{name:'Show chart'}).click();await page.locator('canvas').waitFor();
  await page.getByRole('button',{name:'Change week'}).click();await page.getByRole('cell',{name:'20 cm',exact:true}).waitFor();
  await page.locator('canvas').evaluate(element=>{window.previousPlotCanvas=element;window.previousPlotStyle=document.querySelector('style[data-ui-plot]');});
  const count=documents.length;
  await reload();
  await page.getByRole('heading',{name:'A little room to grow.'}).waitFor();
  await page.getByRole('cell',{name:'20 cm',exact:true}).waitFor();await page.locator('canvas').waitFor();
  assert(await page.locator('canvas').evaluate(element=>element!==window.previousPlotCanvas&&!window.previousPlotCanvas.isConnected));
  assert.equal(await page.locator('style[data-ui-plot]').count(),1);
  await page.getByRole('button',{name:'Hide chart'}).click();
  await page.locator('canvas').waitFor({state:'detached'});
  assert.equal(await page.locator('style[data-ui-plot]').count(),0);
  assert(await page.evaluate(()=>!window.previousPlotStyle.isConnected));
  assert.equal(documents.length,count,'chart reload discarded the page');
  assert.deepEqual(errors,[]);
  return {passed:true,sourceReload:true,stateRetained:true,oldCanvasReleased:true,stylesReleased:true};
}
