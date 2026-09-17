import assert from 'node:assert/strict';
import { resolve } from 'node:path';

export async function checkStyles(browser, url, outputDirectory) {
  const reports = [];
  for (const backend of ['vm']) for (const hydrate of [false, true]) {
    const page = await browser.newPage({ viewport: { width: 1180, height: 1000 } });
    const errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    let release;
    try {
      if (hydrate) {
        const gate = new Promise(resolve => { release = resolve; });
        await page.route('**/artifacts/styling.*', async route => { await gate; await route.continue(); });
      }
      await page.goto(`${url}/?example=styling&backend=${backend}${hydrate ? '&ssr' : ''}`, { waitUntil: 'commit' });
      await page.waitForFunction(() => document.querySelector('#night > .title') && getComputedStyle(document.querySelector('#night > .title')).color === 'rgb(121, 92, 155)');
      const styles = () => page.evaluate(() => {
        const value = (selector, property) => getComputedStyle(document.querySelector(selector))[property];
        return {
          day: value('#day > .title', 'color'), night: value('#night > .title', 'color'),
          guestSize: value('#guest > .title', 'fontSize'), guestBorder: value('#guest', 'borderInlineStartWidth'),
          guestDetail: value('#guest > .detail', 'borderInlineStartWidth'),
          outside: value('#outside > .detail', 'borderInlineStartWidth'),
          dayBorder: value('#day > p.detail', 'borderInlineStartWidth'),
          radius: value('#day .action', 'borderRadius'),
          inherited: value('#guest .token', 'color'),
        };
      });
      const initial = await styles();
      assert.deepEqual(initial, { day: 'rgb(60, 101, 73)', night: 'rgb(121, 92, 155)', guestSize: '15px',
        guestBorder: '0px', guestDetail: '0px', outside: '0px', dayBorder: '3px', radius: '99px', inherited: 'rgb(60, 101, 73)' });
      const checkContrast = async () => {
        const samples = await page.evaluate(() => {
          const luminance = color => {
            const channels = color.match(/[\d.]+/g).slice(0, 3).map(value => Number(value) / 255)
              .map(value => value <= .04045 ? value / 12.92 : ((value + .055) / 1.055) ** 2.4);
            return channels.reduce((sum, value, index) => sum + value * [.2126, .7152, .0722][index], 0);
          };
          return [['#day > .title', '#day'], ['#night > .title', '#night'], ['#guest .token', '#guest'],
            ['#day .action', '#day .action'], ['#night .action', '#night .action']].map(([text, background]) => {
            const a = luminance(getComputedStyle(document.querySelector(text)).color);
            const b = luminance(getComputedStyle(document.querySelector(background)).backgroundColor);
            return { text, ratio: (Math.max(a, b) + .05) / (Math.min(a, b) + .05) };
          });
        });
        for (const sample of samples) assert(sample.ratio >= 4.5, `${sample.text} contrast ${sample.ratio.toFixed(2)} is too low`);
      };
      await checkContrast();
      await page.getByRole('textbox', { name: 'day note', exact: true }).fill('A note before the next change.');
      await page.evaluate(() => { window.styleInput = document.querySelector('#day input'); window.styleCard = document.querySelector('#day'); });
      release?.();
      await page.waitForFunction(() => window.__uiNext?.ready || window.__uiNext?.error);
      assert.equal(await page.evaluate(() => window.__uiNext.error), null);
      assert.equal(await page.evaluate(() => window.styleInput === document.querySelector('#day input') && window.styleCard === document.querySelector('#day')), true);
      assert.equal(await page.getByRole('textbox', { name: 'day note', exact: true }).inputValue(), 'A note before the next change.');
      assert.deepEqual(await styles(), initial, 'activation changed the server style boundary');
      await page.getByRole('button', { name: 'Save day', exact: true }).click();
      await page.waitForFunction(() => document.querySelector('#day output').textContent === '1');
      await page.getByRole('button', { name: 'Change theme', exact: true }).click();
      await page.waitForFunction(() => getComputedStyle(document.querySelector('#day > .title')).color === 'rgb(141, 170, 122)');
      assert.equal((await styles()).inherited, 'rgb(141, 170, 122)');
      assert.equal((await styles()).night, 'rgb(179, 155, 208)');
      await checkContrast();
      await page.getByRole('button', { name: 'Swap cards', exact: true }).click();
      await page.waitForFunction(() => document.querySelector('.cards').firstElementChild.id === 'night');
      assert.equal(await page.evaluate(() => window.styleInput === document.querySelector('#day input') && window.styleCard === document.querySelector('#day')), true);
      assert.equal(await page.locator('#day output').textContent(), '1');
      assert.equal(await page.getByRole('textbox', { name: 'day note', exact: true }).inputValue(), 'A note before the next change.');

      // Layer order is declared once; loading library CSS later keeps overrides.
      await page.evaluate(async () => {
        const link = document.createElement('link'); link.rel = 'stylesheet'; link.href = '/ui-kit/theme.css?late';
        await new Promise((resolve, reject) => { link.onload = resolve; link.onerror = reject; document.head.append(link); });
      });
      assert.equal((await styles()).radius, '99px');
      const override = await page.addStyleTag({ content: 'button { border-radius: 4px; }' });
      assert.equal((await styles()).radius, '4px', 'ordinary application CSS cannot override a component default');
      await override.evaluate(element => element.remove());
      assert.equal((await styles()).radius, '99px');

      await page.locator('#day .action').hover();
      assert.equal(await page.locator('#day .action').evaluate(element => getComputedStyle(element).filter), 'brightness(0.88)');
      // WebKit's default keyboard preference skips native buttons with Tab.
      // Pick the full navigation chord against an ordinary browser control.
      await page.evaluate(() => {
        const reference = document.createElement('div'); reference.id = 'native-style-tab';
        reference.innerHTML = '<input aria-label="Native tab reference"><button type="button">Native tab target</button>';
        document.body.append(reference);
      });
      await page.locator('#native-style-tab input').focus();
      await page.keyboard.press('Tab');
      let tabKey = 'Tab';
      if (!await page.locator('#native-style-tab button').evaluate(element => element === document.activeElement)) {
        tabKey = 'Alt+Tab';
        await page.locator('#native-style-tab input').focus();
        await page.keyboard.press(tabKey);
      }
      assert(await page.locator('#native-style-tab button').evaluate(element => element === document.activeElement));
      await page.locator('#native-style-tab').evaluate(element => element.remove());
      await page.getByRole('textbox', { name: 'day note', exact: true }).focus();
      await page.keyboard.press(tabKey);
      assert.deepEqual(await page.locator('#day .action').evaluate(element => ({ active: element === document.activeElement,
        offset: getComputedStyle(element).outlineOffset, focusVisible: element.matches(':focus-visible') })),
      { active: true, offset: '4px', focusVisible: true });
      await page.evaluate(() => { document.querySelector('#night').style.maxWidth = '260px'; });
      await page.waitForFunction(() => getComputedStyle(document.querySelector('#night > .title')).fontSize === '20px');
      assert.equal(await page.locator('#day > .title').evaluate(element => getComputedStyle(element).fontSize), '24px');
      await page.evaluate(() => { document.querySelector('#night').style.maxWidth = ''; });
      await page.getByRole('button', { name: 'Change direction', exact: true }).click();
      await page.waitForFunction(() => getComputedStyle(document.querySelector('#day > p.detail')).borderRightWidth === '3px');
      assert.equal(await page.locator('#day > p.detail').evaluate(element => getComputedStyle(element).borderLeftWidth), '0px');
      await page.setViewportSize({ width: 390, height: 844 });
      await page.waitForFunction(() => getComputedStyle(document.querySelector('#day')).padding === '20px');
      assert.equal(await page.locator('#day > .title').evaluate(element => getComputedStyle(element).fontSize), '20px');
      assert(await page.evaluate(() => document.documentElement.scrollWidth <= innerWidth), 'scoped layout overflows at 390px');
      if (backend === 'vm' && !hydrate) await page.screenshot({ path: resolve(outputDirectory, 'styling-mobile.png'), fullPage: true });
      await page.evaluate(async () => { window.__uiNext.close(); await window.__uiNext.done; });
      assert.deepEqual(errors, []);
      reports.push({ backend, hydrate, tabKey, passed: true, contracts: ['sibling-style-isolation', 'nested-root-and-descendant-exclusion',
        'shared-inherited-tokens', 'theme-update', 'layered-and-unlayered-overrides', 'late-stylesheet-order',
        'light-and-dark-accent-text-contrast',
        'native-hover-and-keyboard-focus', 'container-and-media-queries', 'logical-rtl-border',
        'keyed-dom-state-retention', ...(hydrate ? ['preboot-css', 'server-dom-and-input-adoption'] : []), 'narrow-layout', 'close'] });
    } finally { release?.(); await page.close(); }
  }
  return reports;
}
