import { expect } from '@playwright/test';

export class PageContract {
  constructor(page) { this.page = page; this.initialViewport = page.viewportSize(); }
  evaluate(expression) { return this.page.evaluate(expression); }
  navigate({ url }) { return this.page.goto(url, { waitUntil: 'domcontentloaded' }); }
  reload() { return this.page.reload({ waitUntil: 'domcontentloaded' }); }
  viewport({ width, height }) { return this.page.setViewportSize({ width, height }); }
  resetViewport() { return this.page.setViewportSize(this.initialViewport); }
  async mouseEvent({ type, x, y, button = 'left', clickCount = 1 }) {
    await this.page.mouse.move(x, y);
    if (type === 'mousePressed') await this.page.mouse.down({ button, clickCount });
    else if (type === 'mouseReleased') await this.page.mouse.up({ button, clickCount });
    else if (type !== 'mouseMoved') throw new Error(`unknown pointer operation ${type}`);
  }
  async keyEvent({ type, key }) {
    if (type === 'keyDown') await this.page.keyboard.down(key);
    else if (type === 'keyUp') await this.page.keyboard.up(key);
    else throw new Error(`unknown keyboard operation ${type}`);
  }
  clickButton(name) { return this.page.getByRole('button', { name, exact: true }).click(); }
  async activate(selector, name) {
    const candidates = this.page.locator(selector);
    const labelled = candidates.and(this.page.getByLabel(name, { exact: true }));
    const named = candidates.and(this.page.getByRole('button', { name, exact: true }));
    const text = candidates.filter({ hasText: new RegExp(`^${name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}$`) });
    await labelled.or(named).or(text).first().click();
  }
  fill(selector, value) { return this.page.locator(selector).fill(value); }
  async settleInput() {
    await this.page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
  }
}

export async function pollEvaluation(contract, expression, predicate, timeout) {
  let value = null;
  await expect.poll(async () => {
    value = await contract.evaluate(expression);
    return predicate(value);
  }, { timeout, intervals: [25, 50, 100], message: `page contract: ${expression.slice(0, 180)}` }).toBe(true);
  return value;
}

export async function waitForAotInteractive(contract, timeout) {
  await pollEvaluation(contract, `({
    interactive: performance.getEntriesByName('volang-aot-interactive', 'mark').length > 0,
    diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    phase: document.getElementById('volang-root')?.dataset.volangActivation ?? '',
    inert: document.getElementById('volang-root')?.hasAttribute('inert') === true,
    busy: document.getElementById('volang-root')?.getAttribute('aria-busy') ?? '',
    bootHidden: document.getElementById('volang-boot')?.hidden === true,
  })`, value => value?.interactive === true && value?.diagnostic === ''
    && value?.phase === 'ready' && value?.inert === false
    && value?.busy === 'false' && value?.bootHidden === true, timeout);
}
