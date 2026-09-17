import assert from 'node:assert/strict';

export async function checkMeasurementBoundary(page, url) {
  await page.route('**/__measurement', route => route.fulfill({ contentType: 'text/html', body: '<!doctype html><body></body>' }));
  await page.goto(`${url}/__measurement`);
  return page.evaluate(async () => {
    const { DomRenderer } = await import('/host/ui_next/renderer.js');
    const { WIRE_VERSION } = await import('/host/ui_next/generated/protocol.js');
    const root = document.body.appendChild(document.createElement('div'));
    const otherRoot = document.body.appendChild(document.createElement('div'));
    const host = new DomRenderer(root, () => {});
    const other = new DomRenderer(otherRoot, () => {});
    const mutation = (op, id, name = '', value = '', parent = 0) => ({ op, id, name, value, parent, before: 0 });
    const batch = (revision, mutations) => ({ version: WIRE_VERSION, revision, inputSequence: 0, mutations, commands: [] });
    const require = (value, message) => { if (!value) throw new Error(message); };
    const geometry = (renderer, id) => JSON.parse(renderer.measure(String(id)));
    const absent = { found: false, x: 0, y: 0, width: 0, height: 0 };
    const equal = (left, right) => JSON.stringify(left) === JSON.stringify(right);
    const native = element => { const { x, y, width, height } = element.getBoundingClientRect(); return { found: true, x, y, width, height }; };
    try {
      host.applyBatch(batch(1, [mutation('create', 1, 'div'), mutation('insert', 1),
        mutation('attr', 1, 'style', 'width:120.75px;height:60.25px;box-sizing:border-box;border:2px solid;transform:translate(-12.5px,3.25px) scale(1.5)'),
        mutation('create', 2, 'svg'), mutation('insert', 2), mutation('attr', 2, 'width', '100'), mutation('attr', 2, 'height', '60')]));
      other.applyBatch(batch(1, [mutation('create', 1, 'div'), mutation('insert', 1), mutation('attr', 1, 'style', 'width:33px;height:44px')]));
      require(equal(geometry(host, 1), native(root.firstElementChild)), 'transformed border box differs from the native browser');
      require(equal(geometry(host, 2), native(root.lastElementChild)), 'SVG bounding rectangle changed');
      require(geometry(other, 1).width === 33 && geometry(host, 1).width !== 33, 'roots shared measurement identities');
      require(equal(geometry(host, 0), absent) && equal(geometry(host, 999), absent), 'missing target exposed another element');
      const hidden = root.firstElementChild;
      hidden.style.display = 'none';
      require(equal(geometry(host, 1), { ...absent, found: true }), 'hidden element was treated as missing');
      hidden.style.display = '';
      const originalRead = hidden.getBoundingClientRect;
      hidden.getBoundingClientRect = () => ({ x: Infinity, y: 0, width: 1, height: 1 });
      let invalid = false;
      try { host.measure('1'); } catch { invalid = true; }
      require(invalid, 'nonfinite native geometry was serialized');
      hidden.getBoundingClientRect = originalRead;
      for (const value of ['', '-1', '1.5', '01', '9007199254740992', '1'.repeat(100)]) {
        invalid = false;
        try { host.measure(value); } catch { invalid = true; }
        require(invalid, 'malformed node identity was accepted');
      }
      host.applyBatch(batch(2, [mutation('remove', 1)]));
      require(equal(geometry(host, 1), absent), 'removed identity remained measurable');
      host.close();
      require(equal(geometry(host, 2), absent), 'closed renderer exposed old geometry');
      require(geometry(other, 1).found, 'closing one root affected another');
      otherRoot.remove();
      require(equal(geometry(other, 1), absent), 'detached document content reported live geometry');
      return { passed: true, contracts: ['native-transformed-border-box', 'svg', 'per-root-identities', 'missing-hidden-and-detached',
        'finite-result-validation', 'target-validation', 'removed-and-closed'] };
    } finally { host.close(); other.close(); root.remove(); otherRoot.remove(); }
  });
}

export async function checkMeasurement(page) {
  await page.locator('[data-measure-show]').click();
  await page.waitForFunction(() => document.querySelector('[data-measurement]').textContent === '160.50 × 80.25');
  await page.evaluate(() => { window.measuredBox = document.querySelector('[data-measured-box]'); });
  await page.locator('[data-measure-resize]').click();
  await page.waitForFunction(() => document.querySelector('[data-measurement]').textContent === '240.75 × 80.25');
  assert(await page.evaluate(() => document.querySelector('[data-measured-box]') === window.measuredBox));
  await page.locator('[data-measure-hide]').click();
  await page.waitForFunction(() => document.querySelector('[data-measurement]').textContent === 'No space is shown.');
  assert.equal(await page.locator('[data-measured-box]').count(), 0);
}
