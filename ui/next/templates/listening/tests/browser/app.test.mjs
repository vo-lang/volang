import {test,expect} from './fixtures.mjs';

test('native playback survives activation and updates; removal stops playback',async({page,appURL})=>{
  const requests=[];
  page.on('request',request=>requests.push(request.url()));
  let release;
  const gate=new Promise(resolve=>{release=resolve;});
  await page.route('**/assets/app.*',async route=>{await gate;await route.continue();});
  try {
    await page.goto(appURL,{waitUntil:'commit'});
    const audio=page.locator('audio'),note=page.getByRole('textbox',{name:'A thought to keep'});
    await expect(audio).toBeVisible();
    expect(await audio.getAttribute('preload')).toBe('none');
    expect(requests.some(url=>url.endsWith('four-notes.wav'))).toBe(false);
    const recording=page.waitForResponse(response=>response.url().endsWith('four-notes.wav'));
    await audio.evaluate(async element=>{
      window.beforeMedia=element;window.mediaReloads=0;
      element.muted=true;element.loop=true;
      await element.play();
      element.addEventListener('emptied',()=>window.mediaReloads++);
    });
    expect((await recording).headers()['content-type']).toBe('audio/wav');
    await expect.poll(()=>audio.evaluate(element=>element.currentTime)).toBeGreaterThan(.1);
    await note.fill('A thought before the page wakes 中文');
    release();
    await expect(page.locator('#status')).toBeEmpty();
    expect(await audio.evaluate(element=>element===window.beforeMedia && !element.paused && element.currentTime>0)).toBe(true);
    expect(await page.evaluate(()=>window.mediaReloads)).toBe(0);
    await expect(note).toHaveValue('A thought before the page wakes 中文');
    await page.getByRole('button',{name:'Change the light'}).click();
    await expect(page.locator('main')).toHaveAttribute('data-theme','dark');
    expect(await audio.evaluate(element=>element===window.beforeMedia && !element.paused)).toBe(true);
    await audio.evaluate(element=>{
      window.seekTime=null;
      element.addEventListener('seeked',()=>{window.seekTime=element.currentTime;},{once:true});
      element.currentTime=5;
    });
    await expect.poll(()=>page.evaluate(()=>window.seekTime)).toBeGreaterThanOrEqual(5);
    expect(await page.evaluate(()=>window.seekTime)).toBeLessThan(6);
    await page.getByRole('button',{name:'Put the player away'}).click();
    await expect(audio).toHaveCount(0);
    await expect.poll(()=>page.evaluate(()=>window.beforeMedia.paused)).toBe(true);
    await page.getByRole('button',{name:'Bring the player back'}).click();
    await expect(audio).toBeVisible();
    expect(await audio.evaluate(element=>element!==window.beforeMedia && element.paused && element.currentTime===0)).toBe(true);
    await expect(note).toHaveValue('A thought before the page wakes 中文');
    await page.setViewportSize({width:390,height:844});
    expect(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth)).toBe(true);
    await expect(page.getByRole('link',{name:'Download the recording'})).toHaveAttribute('download','four-notes.wav');
    const native=await page.context().browser().newContext({javaScriptEnabled:false});
    try {
      const html=await native.newPage();await html.goto(appURL);
      await expect(html.getByRole('heading',{name:'A moment to listen.'})).toBeVisible();
      await expect(html.locator('audio')).toBeVisible();
      await expect(html.getByRole('link',{name:'Download the recording'})).toHaveAttribute('href','./four-notes.wav');
      await html.getByRole('textbox',{name:'A thought to keep'}).fill('Without scripting');
    } finally {await native.close();}
  } finally {release();}
});
