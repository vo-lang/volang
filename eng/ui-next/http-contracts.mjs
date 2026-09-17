import assert from 'node:assert/strict';

/** Real Vo client, native JSON endpoint, and browser Fetch on the deployed site. */
export async function checkHTTP(browser, base, backend) {
  const page = await browser.newPage(), errors = [], submissions = [];
  page.on('pageerror', error => errors.push(error.message));
  page.on('request', request => {if(request.url() === base + 'api/http' && request.method() === 'PATCH') submissions.push(request);});
  let releaseBoot;
  const boot = new Promise(resolve => {releaseBoot = resolve;});
  await page.route('**/assets/app.*', async route => {await boot; await route.continue();});
  try {
    await page.goto(base + 'http?backend=' + backend, {waitUntil:'commit'});
    const name = page.getByRole('textbox',{name:'Display name'}), submit = page.locator('[data-http-submit]');
    await name.fill('Before startup 中文');
    await name.evaluate(element => {window.httpInput = element;});
    releaseBoot();
    await page.waitForFunction(() => document.getElementById('status')?.textContent === '');
    assert.equal(await name.evaluate(element => element === window.httpInput),true);
    await submit.click();
    await page.waitForFunction(() => document.querySelector('[data-http-message]')?.textContent === 'Saved Before startup 中文');
    assert.equal(await page.locator('[data-http-status]').textContent(),'201');
    assert.equal(await page.locator('[data-http-dirty]').textContent(),'false');
    assert.equal(submissions[0].headers()['x-request'],'typed-vo');
    assert.equal(submissions[0].headers()['content-type'],'application/json');
    assert.deepEqual(submissions[0].postDataJSON(),{name:'Before startup 中文'});

    await name.fill('taken'); await submit.click();
    await page.waitForFunction(() => document.getElementById('http-name-error')?.textContent === 'Name is already used.');
    assert.equal(await name.getAttribute('aria-invalid'),'true');
    assert.equal(await page.locator('[data-http-status]').textContent(),'422');

    await name.fill('timeout'); await submit.click();
    await page.waitForFunction(() => document.querySelector('[data-http-error]')?.textContent.includes('timed out'));
    await name.fill('Retry succeeded'); await submit.click();
    await page.waitForFunction(() => document.querySelector('[data-http-message]')?.textContent === 'Saved Retry succeeded');

    const cancelling = page.waitForRequest(request => request.url() === base + 'api/http' && request.method() === 'PATCH');
    await name.fill('cancel'); await submit.click(); await cancelling;
    await page.getByRole('button',{name:'Cancel save'}).click();
    await page.waitForFunction(() => !document.querySelector('[data-http-submit]')?.disabled);
    assert.equal(await page.locator('[data-http-message]').textContent(),'');

    // Hold complete native responses to verify edits made during submission.
    for(const [sent, edited, expected] of [['Frozen snapshot','Newer edit','Saved Frozen snapshot'],['taken','Valid newer name','']]) {
      let arrived, release;
      const received = new Promise(resolve => {arrived = resolve;});
      const gate = new Promise(resolve => {release = resolve;});
      const handler = async route => { const response = await route.fetch(); arrived(); await gate; await route.fulfill({response}); };
      await page.route(base + 'api/http',handler);
      try {
        await name.fill(sent);
        const before = submissions.length;
        await submit.evaluate(button => {button.click(); button.form.requestSubmit();});
        await received;
        assert.equal(submissions.length,before+1,'a pending form started duplicate HTTP writes');
        await name.fill(edited); release();
        await page.waitForFunction(() => !document.querySelector('[data-http-submit]')?.disabled);
        assert.equal(await name.inputValue(),edited);
        assert.equal(await page.locator('[data-http-message]').textContent(),expected);
        assert.equal(await page.locator('#http-name-error').textContent(),'');
        assert.equal(await page.locator('[data-http-dirty]').textContent(),'true');
      } finally {release(); await page.unroute(base + 'api/http',handler);}
    }
    await page.getByRole('button',{name:'Load profile'}).click();
    await page.waitForFunction(() => document.querySelector('[data-http-loaded]')?.textContent === 'Loaded from Vo');
    assert.deepEqual(errors,[]);
    return {backend,passed:true,submissions:submissions.length,
      contracts:['early-input','typed-patch-json','status-422-fields','timeout-retry','cancel','frozen-submit-baseline','stale-error-isolation','one-pending-write','typed-get']};
  } finally {releaseBoot(); await page.close();}
}
