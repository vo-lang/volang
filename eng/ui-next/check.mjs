import assert from 'node:assert/strict';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { performance } from 'node:perf_hooks';
import { serve, root } from './server.mjs';
import { checkDomContracts } from './dom-contracts.mjs';
import {checkKeyedOrder} from './keyed-order-contracts.mjs';
import { checkWorkbench } from './workbench-contracts.mjs';
import { checkStudio } from './studio-contracts.mjs';
import {checkPersistentStorage} from './storage-contracts.mjs';
import {checkStudioEditor} from './studio-editor-contracts.mjs';
import {checkStudioLanguageService} from './studio-language-contracts.mjs';
import {checkEditorLanguageBoundary} from './editor-language-boundary.mjs';
import {checkStudioRecovery} from './studio-recovery-contracts.mjs';
import { checkNavigationContracts } from './navigation-contracts.mjs';
import { checkModalBoundary } from './modal-boundary.mjs';
import { checkMotionBoundary } from './motion-boundary.mjs';
import { checkWorkerUi } from './worker-contracts.mjs';
import { checkKeyboardBoundary } from './keyboard-contracts.mjs';
import { checkNativeElements } from './native-contracts.mjs';
import {checkMediaBoundary} from './media-boundary.mjs';
import {checkDelegatedInputs} from './delegated-input-boundary.mjs';
import {checkPointerBoundary} from './pointer-boundary.mjs';
import {checkTextBoundary} from './text-boundary.mjs';
import {checkDataBlockBoundary} from './data-block-boundary.mjs';
import {checkIndeterminateBoundary,editEarlyIndeterminate,checkEarlyIndeterminate} from './indeterminate-boundary.mjs';
import {checkPlotBoundary} from './plot-boundary.mjs';
import {checkCanvasBoundary} from './canvas-boundary.mjs';
import {checkScrollPositionBoundary} from './scroll-position-boundary.mjs';
import {checkTextSelectionBoundary} from './text-selection-boundary.mjs';
import {checkSizeBoundary} from './size-boundary.mjs';
import { checkMount } from './mount-contracts.mjs';
import { checkScrollNavigation } from './scroll-contracts.mjs';
import { checkPopoverBoundary } from './popover-boundary.mjs';
import { checkPopoverMotion } from './popover-motion-boundary.mjs';
import {checkCodeEditorBoundary} from './editor-boundary.mjs';
import {checkWidgetCommitBoundary} from './widget-commit-boundary.mjs';
import {checkFileBoundary} from './file-boundary.mjs';
import { checkLazyWidgetBoundary } from './lazy-widget-boundary.mjs';
import { checkLazyWidgetApplications } from './lazy-widget-contracts.mjs';
import { editEarlyDefaults, checkEarlyDefaults, checkDefaults } from './defaults-contracts.mjs';
import { checkDefaultsBoundary } from './defaults-boundary.mjs';
import { checkControlledChoices } from './choices-contracts.mjs';
import { checkViewportBoundary } from './viewport-contracts.mjs';
import { checkCollections, collectionContracts } from './collection-contracts.mjs';
import { checkInspection } from './inspection-contracts.mjs';
import { checkInspectionPanel } from './inspection-panel-contracts.mjs';
import { checkLocalApplicationCompiler } from './compiler-contracts.mjs';
import {checkEditorCompiler} from './editor-compiler-contracts.mjs';
import { checkMeasurementBoundary, checkMeasurement } from './measurement-contracts.mjs';
import { checkCustomElementBoundary } from './custom-element-contracts.mjs';
import { checkShadowFocusBoundary } from './shadow-focus-contracts.mjs';
import { checkSelectionBoundary } from './selection-boundary.mjs';
import { checkReloadBoundary } from './reload-boundary.mjs';
import { checkStyles } from './style-contracts.mjs';
import {webArtifactInventory} from './web-artifacts.mjs';

process.env.PLAYWRIGHT_BROWSERS_PATH ??= resolve(root, 'target/playwright-browsers');
const engine = process.env.UI_NEXT_BROWSER ?? 'chromium';
if (!['chromium', 'firefox', 'webkit'].includes(engine)) throw new Error('UI_NEXT_BROWSER must be chromium, firefox or webkit');
const browsers = await import('../browser/node_modules/playwright/index.mjs');
const outputDirectory = resolve(root, 'target/ui-next', engine === 'chromium' ? '' : engine);
await mkdir(outputDirectory, { recursive: true });
const build = JSON.parse(await readFile(resolve(root, 'target/ui-next/build-report.json'), 'utf8'));
assert.deepEqual(await webArtifactInventory(root),build.webPackages,'Web runtime packages changed after the application build');
const application = await serve();
let browser;
const report = { schema: 'volang.ui-next-prototype.v1', passed: false, engine, build, backends: [] };
try {
  browser = await browsers[engine].launch({ headless: true });
  report.browserVersion = browser.version();
  const boundary = await browser.newPage();
  for (const [name, check] of [
    ['domBoundary', checkDomContracts], ['nativeMedia', checkMediaBoundary], ['nativeDefaults', checkDefaultsBoundary], ['multipleSelection', checkSelectionBoundary],
    ['delegatedInputs', checkDelegatedInputs],
    ['pointerBoundary', checkPointerBoundary],
    ['nativeText', checkTextBoundary],
    ['dataBlocks', checkDataBlockBoundary],
    ['indeterminate', checkIndeterminateBoundary],
    ['canvas', checkCanvasBoundary],
    ['plot', checkPlotBoundary],
    ['scrollPosition', checkScrollPositionBoundary],
    ['textSelection', checkTextSelectionBoundary],
    ['size', checkSizeBoundary],
    ['reloadBoundary', checkReloadBoundary],
    ['fileBoundary', checkFileBoundary],
    ['customElements', checkCustomElementBoundary], ['lazyWidgets', checkLazyWidgetBoundary], ['widgetCommit', checkWidgetCommitBoundary], ['codeEditor', checkCodeEditorBoundary], ['editorLanguage',checkEditorLanguageBoundary], ['shadowFocus', checkShadowFocusBoundary], ['measurement', checkMeasurementBoundary], ['viewport', checkViewportBoundary], ['navigation', checkNavigationContracts],
    ['scrollNavigation', checkScrollNavigation], ['modalBoundary', checkModalBoundary],
    ['motionBoundary', checkMotionBoundary], ['workerUi', checkWorkerUi],
    ['inspectionPanel', checkInspectionPanel], ['localApplicationCompiler', checkLocalApplicationCompiler], ['editorCompiler',checkEditorCompiler],
    ['popoverBoundary', checkPopoverBoundary], ['popoverMotion', checkPopoverMotion], ['keyboardBoundary', checkKeyboardBoundary], ['mount', checkMount],
  ]) {
    let timer;
    try {
      report[name] = await Promise.race([check(boundary, application.url), new Promise((_, reject) => {
        timer = setTimeout(() => reject(new Error(`${name} browser contract exceeded 45 seconds`)), 45000);
      })]);
      console.log(`${engine}: ${name} passed`);
    } finally { clearTimeout(timer); }
  }
  await boundary.close();
  for (const backend of ['vm']) {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    const started = performance.now();
    await page.goto(`${application.url}/?backend=${backend}`);
    await page.waitForFunction(() => document.querySelector('[data-counter]') || window.__uiNext?.error);
    assert.equal(await page.evaluate(() => window.__uiNext.error), null);
    await page.waitForFunction(() => document.querySelector('[data-lifecycle]')?.textContent === 'Ready to explore');
    const mountedMs = performance.now() - started;
    await page.locator('[data-counter="Alpha"]').click();
    await page.waitForFunction(() => document.querySelector('[data-counter="Alpha"]').textContent === 'Alpha: 1');
    // Preserve the actual DOM object, not just its text or selector identity.
    await page.evaluate(() => { window.retainedCounter = document.querySelector('[data-counter="Alpha"]'); });
    await page.locator('[data-reverse]').click();
    await page.waitForFunction(() => document.querySelector('[data-counters]').firstElementChild.textContent === 'Beta: 0');
    assert.equal(await page.evaluate(() => window.retainedCounter === document.querySelector('[data-counter="Alpha"]')), true);
    assert.equal(await page.locator('[data-counter="Alpha"]').textContent(), 'Alpha: 1');

    // Deliver a burst in one browser turn, without waiting for guest render-idle.
    await page.evaluate(() => {
      for (let i = 0; i < 25; i++) document.querySelector('[data-counter="Alpha"]').click();
    });
    await page.waitForFunction(() => document.querySelector('[data-counter="Alpha"]').textContent === 'Alpha: 26');
    await page.locator('#name').focus();
    await page.keyboard.type('Volang', { delay: 0 });
    await page.keyboard.press('Enter');
    await page.waitForFunction(() => document.querySelector('[data-submitted]').textContent === 'Volang');
    assert.equal(await page.locator('#name').inputValue(), 'Volang');
    assert.equal(await page.evaluate(() => document.activeElement.id), 'name');

    await page.evaluate(() => {
      const input = document.querySelector('#name');
      input.dispatchEvent(new CompositionEvent('compositionstart', { bubbles: true }));
      input.value = '中文输入';
      input.dispatchEvent(new InputEvent('input', { bubbles: true, data: '中文输入', isComposing: true }));
      input.dispatchEvent(new CompositionEvent('compositionend', { bubbles: true, data: '中文输入' }));
      document.querySelector('form').requestSubmit();
    });
    await page.waitForFunction(() => document.querySelector('[data-submitted]').textContent === '中文输入');
    assert.equal(await page.locator('#name').inputValue(), '中文输入');
    for (let cycle = 0; cycle < 3; cycle++) {
      await page.locator('[data-break]').click();
      await page.locator('[data-recover]').waitFor();
      assert.equal(await page.evaluate(() => window.retainedCounter === document.querySelector('[data-counter="Alpha"]')), true);
      await page.locator('[data-counter="Alpha"]').click();
      await page.waitForFunction(value => document.querySelector('[data-counter="Alpha"]').textContent === `Alpha: ${value}`, 27 + cycle);
      assert.equal(await page.locator('#name').inputValue(), '中文输入');
      await page.locator('[data-recover]').click();
      await page.locator('[data-break]').waitFor();
    }
    await page.locator('[data-toggle]').click();
    await page.waitForFunction(() => !document.querySelector('[data-counter="Beta"]'));
    await page.locator('[data-toggle]').click();
    await page.waitForFunction(() => document.querySelector('[data-counter="Beta"]')?.textContent === 'Beta: 0');
    assert.equal(await page.locator('#notes').inputValue(), '\nA little space for your ideas.\n中文 & <thoughts>');
    await checkNativeElements(page);
    await checkMeasurement(page);
    await checkDefaults(page);
    await checkControlledChoices(page);
    await checkCollections(page);
    await page.screenshot({ path: resolve(outputDirectory, `${backend}.png`), fullPage: true });
    await page.evaluate(async () => { window.__uiNext.close(); await window.__uiNext.done; });
    assert.equal(await page.locator('#root').textContent(), '');
    assert.deepEqual(errors, []);
    report.backends.push({ backend, mountedMs, passed: true,
      contracts: ['post-commit-effect', 'live-dom-identity', 'keyed-state', '25-event-burst', 'input-enter', 'composition-event-order', 'local-error-retry', 'unmount-remount', 'svg-namespace-update', 'textarea-immediate-submit', 'capture-bubble-order', 'dynamic-stop-propagation', 'keyboard-modifiers', 'synchronous-default-prevention', 'close'] });
    console.log(`${backend}: browser contracts passed (${mountedMs.toFixed(1)} ms to mount, local warm environment)`);
    await page.close();
  }
  for (const backend of ['vm']) {
    const page = await browser.newPage();
    const errors = [];
    page.on('pageerror', error => errors.push(String(error)));
    let release;
    const gated = new Promise(resolve => { release = resolve; });
    await page.route('**/artifacts/interaction.*', async route => { await gated; await route.continue(); });
    await page.goto(`${application.url}/?backend=${backend}&ssr`, { waitUntil: 'commit' });
    assert.equal(await page.locator('#notes').inputValue(), '\nA little space for your ideas.\n中文 & <thoughts>');
    await page.locator('#notes').fill('Before boot\n早期笔记');
    await editEarlyDefaults(page);
    await editEarlyIndeterminate(page);
    await page.locator('#name').fill('Before Wasm starts');
    await page.evaluate(() => {
      window.serverLibrary = document.querySelector('#virtual-library');
      window.serverLibrary.scrollTop = 8000;
    });
    assert.equal(await page.locator('[data-lifecycle]').textContent(), 'Starting…');
    await page.evaluate(() => { window.serverInput = document.querySelector('#name'); window.serverNotes = document.querySelector('#notes'); window.serverCircle = document.querySelector('[data-circle]'); });
    release();
    await page.waitForFunction(() => document.querySelector('[data-greeting]')?.textContent === 'Hello, Before Wasm starts' || window.__uiNext?.error);
    assert.equal(await page.evaluate(() => window.__uiNext.error), null);
    await page.waitForFunction(() => document.querySelector('[data-lifecycle]')?.textContent === 'Ready to explore');
    assert.equal(await page.evaluate(() => window.serverInput === document.querySelector('#name')), true);
    assert.equal(await page.locator('#name').inputValue(), 'Before Wasm starts');
    assert.equal(await page.evaluate(() => document.activeElement === window.serverInput), true);
    await page.waitForFunction(() => document.querySelector('#virtual-library-option-200'));
    assert.equal(await page.evaluate(() => window.serverLibrary === document.querySelector('#virtual-library') && window.serverLibrary.scrollTop === 8000), true);
    await page.keyboard.press('Enter');
    await page.waitForFunction(() => document.querySelector('[data-submitted]')?.textContent === 'Before Wasm starts');
    assert.equal(await page.evaluate(() => window.serverNotes === document.querySelector('#notes') && window.serverCircle === document.querySelector('[data-circle]')), true);
    assert.equal(await page.locator('#notes').inputValue(), 'Before boot\n早期笔记');
    await page.evaluate(() => document.querySelector('#notes').closest('form').requestSubmit());
    await page.waitForFunction(() => document.querySelector('[data-notes-saved]').textContent === 'Before boot\n早期笔记');
    await checkEarlyIndeterminate(page);
    await checkNativeElements(page);
    await checkMeasurement(page);
    await checkEarlyDefaults(page);
    await checkDefaults(page);
    await checkControlledChoices(page);
    await checkCollections(page);
    await page.evaluate(async () => { window.__uiNext.close(); await window.__uiNext.done; });
    assert.deepEqual(errors, []);
    report.backends.push({ backend, mode: 'hydrate', passed: true,
      contracts: ['html-before-wasm', 'early-input-retained', 'early-mixed-choice', 'server-dom-adopted', 'focus-retained', 'input-enter', 'svg-adoption', 'early-textarea-retained'] });
    console.log(`${backend}: SSR adoption and pre-boot input passed`);
    await page.close();
  }
  report.keyedOrder = await checkKeyedOrder(browser,application.url);
  report.inspection = await checkInspection(browser, application.url);
  report.styles = await checkStyles(browser, application.url, outputDirectory);
  report.workbench = await checkWorkbench(browser, application.url, outputDirectory);
  report.lazyWidgetApplications = await checkLazyWidgetApplications(browser, application.url);
  report.storage = await checkPersistentStorage(browser);
  report.studioEditor = await checkStudioEditor(browser, application.url);
  report.studioLanguage = await checkStudioLanguageService(browser,application.url);
  report.studioRecovery = await checkStudioRecovery(browser, application.url, resolve(outputDirectory, 'recovery'), {createOrigin:serve});
  report.collectionContracts = collectionContracts;
  report.studio = await checkStudio(browser, application.url, outputDirectory);
  assert.deepEqual(await webArtifactInventory(root),build.webPackages,'Web runtime packages changed during browser tests');
  report.passed = true;
} catch (error) {
  report.error = String(error?.stack ?? error);
  throw error;
} finally {
  await browser?.close();
  await application.close();
  await mkdir(resolve(root, 'target/ui-next'), { recursive: true });
  await writeFile(resolve(outputDirectory, 'browser-report.json'), JSON.stringify(report, null, 2) + '\n');
}
