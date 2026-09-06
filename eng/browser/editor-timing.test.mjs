import { after, before, test } from 'node:test';
import assert from 'node:assert/strict';
import { chromium } from '@playwright/test';
import { beginEditorOpenTiming, editorOpenDuration, disposeEditorOpenTiming } from './editor-timing.mjs';

let browser;
before(async () => { browser = await chromium.launch(); });
after(async () => { await browser?.close(); });

test('editor timing excludes driver delay after the ready frame', async () => {
  const page = await browser.newPage();
  await page.setContent('<button aria-label="Open example">Open</button><textarea data-testid="volang-code-editor"></textarea>');
  await page.getByRole('button').evaluate(button => button.addEventListener('click', () => {
    document.querySelector('textarea').value = 'select {';
  }));
  const timing = await beginEditorOpenTiming(page, 'Open example', 'select {');
  try {
    await page.getByRole('button').click();
    await page.waitForFunction(() => document.querySelector('textarea').value === 'select {');
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
    const duration = await editorOpenDuration(timing);
    // Deliberately delay the driver after completion to model trace/report work.
    await new Promise(resolve => setTimeout(resolve, 600));
    assert.equal(await editorOpenDuration(timing), duration);
    assert.ok(duration >= 0);
  } finally {
    await disposeEditorOpenTiming(timing);
    await page.close();
  }
});

test('editor timing requires a real click and includes delayed readiness', async () => {
  const page = await browser.newPage();
  await page.setContent('<button aria-label="Open example">Open</button><textarea data-testid="volang-code-editor"></textarea>');
  const timing = await beginEditorOpenTiming(page, 'Open example', 'select {');
  try {
    await page.getByRole('button').evaluate(button => button.click());
    await assert.rejects(editorOpenDuration(timing), /trusted click/);
    await page.getByRole('button').click();
    await assert.rejects(editorOpenDuration(timing), /ready frame/);
    await page.evaluate(() => {
      setTimeout(() => { document.querySelector('textarea').value = 'select {'; }, 350);
    });
    await page.waitForFunction(() => document.querySelector('textarea').value === 'select {');
    await page.evaluate(() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve))));
    assert.ok(await editorOpenDuration(timing) >= 300, 'product delay must remain in the measurement');
  } finally {
    await disposeEditorOpenTiming(timing);
    await page.close();
  }
});
