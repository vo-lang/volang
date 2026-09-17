import assert from 'node:assert/strict';

export async function checkPopover(page) {
  const trigger = page.locator('#idea-popover-trigger');
  const popup = page.locator('#idea-popover');
  const input = page.locator('#popover-name');
  // A closing native CSS transition can still have a visible layout box.
  const opened = () => page.waitForFunction(() => document.querySelector('#idea-popover').matches(':popover-open')
    && document.querySelector('#idea-popover-trigger').getAttribute('aria-expanded') === 'true'
    && document.activeElement.id === 'popover-name');
  await trigger.focus();
  await page.keyboard.press('Enter');
  await page.waitForFunction(() => document.querySelector('#idea-popover').matches(':popover-open')
    && document.activeElement.id === 'popover-name');
  await input.fill('A thoughtful 中文 idea');
  await page.keyboard.press('Escape');
  await page.waitForFunction(() => !document.querySelector('#idea-popover').matches(':popover-open')
    && document.activeElement.id === 'idea-popover-trigger'
    && document.querySelector('#idea-popover-trigger').getAttribute('aria-expanded') === 'false');
  assert.equal(await trigger.getAttribute('aria-expanded'), 'false');
  await trigger.click();
  await opened();
  assert.equal(await input.inputValue(), 'A thoughtful 中文 idea');
  await trigger.click();
  await page.waitForFunction(() => !document.querySelector('#idea-popover').matches(':popover-open')
    && document.querySelector('#idea-popover-trigger').getAttribute('aria-expanded') === 'false');
  assert.equal(await trigger.getAttribute('aria-expanded'), 'false', 'trigger pointer close reopened the popup');
  await trigger.click();
  await opened();
  await page.locator('#gallery-search').click();
  await page.waitForFunction(() => !document.querySelector('#idea-popover').matches(':popover-open')
    && document.querySelector('#idea-popover-trigger').getAttribute('aria-expanded') === 'false');
  assert.equal(await page.evaluate(() => document.activeElement.id), 'gallery-search', 'outside dismissal stole the new focus target');
  await trigger.evaluate(element => {
    const top = element.getBoundingClientRect().top + scrollY;
    scrollTo(0, top - innerHeight + 65);
  });
  await trigger.click();
  await opened();
  assert.equal(await popup.getAttribute('data-ui-popover-side'), 'top', 'popup did not flip above its viewport-edge trigger');
  await page.getByRole('button', { name: 'Close popover', exact: true }).click();
  await page.waitForFunction(() => document.activeElement.id === 'idea-popover-trigger'
    && !document.querySelector('#idea-popover').matches(':popover-open'));
  const viewport = page.viewportSize();
  await page.setViewportSize({ width: 390, height: 844 });
  await trigger.click();
  await opened();
  await page.waitForFunction(() => {
    const rect = document.querySelector('#idea-popover').getBoundingClientRect();
    return rect.left >= 7 && rect.right <= innerWidth - 7 && rect.top >= 7 && rect.bottom <= innerHeight - 7;
  });
  await page.keyboard.press('Escape');
  await page.waitForFunction(() => !document.querySelector('#idea-popover').matches(':popover-open'));
  await page.setViewportSize(viewport);
  await checkPopoverMotionKit(page);
}

async function checkPopoverMotionKit(page) {
  const trigger = page.locator('#idea-popover-trigger'), popup = page.locator('#idea-popover');
  const style = await page.addStyleTag({content:'#idea-popover.vui-popover-motion { transition-duration: 60s !important; }'});
  const finish = () => popup.evaluate(element => {
    for (const animation of element.getAnimations()) animation.finish();
  });
  const opened = () => page.waitForFunction(() => {
    const popup = document.querySelector('#idea-popover');
    return popup.matches(':popover-open') && !popup.inert && document.activeElement.id === 'popover-name';
  });
  const closed = () => page.waitForFunction(() => {
    const popup = document.querySelector('#idea-popover');
    return !popup.matches(':popover-open') && popup.inert && popup.getAttribute('aria-hidden') === 'true';
  });
  try {
    await trigger.click(); await opened(); await finish();
    await page.evaluate(() => {
      window.motionPopup = document.querySelector('#idea-popover');
      window.motionPopupInput = document.querySelector('#popover-name');
    });
    const position = await popup.evaluate(element => element.style.left);
    await popup.getByRole('button', {name:'Close popover', exact:true}).click(); await closed();
    const exit = await popup.evaluate(element => ({left:element.style.left, animations:element.getAnimations().length,
      active:document.activeElement.id, overlay:CSS.supports('overlay', 'auto')}));
    assert.equal(exit.active, 'idea-popover-trigger');
    if (exit.overlay) {
      assert(exit.animations > 0, 'kit popover did not use native exit motion');
      assert.equal(exit.left, position, 'kit popover moved during its exit');
    }
    await trigger.click(); await opened(); await finish();
    assert.equal(await page.evaluate(() => window.motionPopup === document.querySelector('#idea-popover')
      && window.motionPopupInput === document.querySelector('#popover-name')), true);
    assert.equal(await page.locator('#popover-name').inputValue(), 'A thoughtful 中文 idea');
    await page.keyboard.press('Escape'); await closed(); await finish();
    await page.waitForFunction(() => document.querySelector('#idea-popover').style.left === '');
  } finally {await style.evaluate(element => element.remove());}
  await page.emulateMedia({reducedMotion:'reduce'});
  try {
    await trigger.click(); await opened();
    assert.equal(await popup.evaluate(element => element.getAnimations().length), 0);
    await page.keyboard.press('Escape'); await closed();
    assert.equal(await popup.evaluate(element => element.style.left), '');
  } finally {await page.emulateMedia({reducedMotion:'no-preference'});}
}
