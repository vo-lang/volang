import assert from 'node:assert/strict';

/** Exercise browser-owned POST navigation before and after guest activation. */
export async function checkServerForms(browser, url) {
  const reports = [];
  for (const mode of ['no-script', 'early-vm', 'active-vm']) {
    const backend = 'vm';
    const page = await browser.newPage({javaScriptEnabled:mode !== 'no-script'}), errors = [], posts = [];
    page.on('pageerror', error => errors.push(error.message));
    page.on('request', request => {if (request.method() === 'POST') posts.push(request.postData());});
    let release;
    const gate = new Promise(resolve => {release = resolve;});
    if (mode.startsWith('early')) await page.route('**/assets/app.*', async route => {await gate; await route.continue();});
    else release();
    try {
      await page.goto(`${url}profile?name=Original&backend=${backend}`, {waitUntil:'commit'});
      const input = page.getByRole('textbox', {name:'Your name'});
      await input.fill('Saved 中文 & +');
      if (mode.startsWith('active')) await page.getByRole('heading', {name:'Hello, Saved 中文 & +.', exact:true}).waitFor();
      // Submit immediately with a newer native value in the same browser task.
      // The guest's async input turn cannot replace the form's current value.
      const submitted = 'Latest 中文 & +';
      const navigation = page.waitForURL(value => value.searchParams.get('name') === submitted, {waitUntil:'commit'});
      await input.evaluate((element, value) => {
        element.value = value;
        element.dispatchEvent(new InputEvent('input', {bubbles:true, data:value}));
        element.form.requestSubmit();
      }, submitted);
      await navigation;
      release();
      await page.getByRole('heading', {name:`Hello, ${submitted}.`, exact:true}).waitFor();
      assert.equal(new URLSearchParams(posts[0]).get('name'), submitted);
      assert.equal(new URL(page.url()).searchParams.get('backend'), backend);
      assert.equal(await page.title(), submitted + ' · Request page');
      assert.equal(await input.inputValue(), submitted);
      await page.reload();
      assert.equal(posts.length, 1, 'refresh repeated the POST after a successful redirect');
      if (mode !== 'no-script') {
        await page.waitForFunction(() => document.getElementById('status')?.textContent === '');
        await page.getByRole('button', {name:'One more', exact:true}).click();
        await page.waitForFunction(() => document.querySelector('output')?.textContent === '1');
      }
      // A validation response retains the submitted value and remains resubmittable.
      await input.fill('');
      const invalid = page.waitForResponse(response => response.request().isNavigationRequest() && response.request().method() === 'POST');
      await page.getByRole('button', {name:'Save name', exact:true}).click();
      assert.equal((await invalid).status(), 422);
      await page.getByRole('alert').filter({hasText:'Enter your name.'}).waitFor();
      assert.equal(await input.inputValue(), '');
      await input.fill('Recovered');
      const recovery = page.waitForURL(value => value.searchParams.get('name') === 'Recovered');
      await page.getByRole('button', {name:'Save name', exact:true}).click();
      await recovery;
      await page.getByRole('heading', {name:'Hello, Recovered.', exact:true}).waitFor();
      assert.deepEqual(errors, []);
      reports.push({mode, passed:true});
    } finally {release(); await page.close();}
  }
  return reports;
}
