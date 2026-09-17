// Native Studio acceptance, delivered only with --check builds.
(async () => {
  let host;
  const wait = async (predicate, label) => {
    const deadline = performance.now() + 45_000;
    while (!predicate()) {
      if (window.__studioNext?.error) throw new Error(window.__studioNext.error);
      if (performance.now() >= deadline) throw new Error(`Studio desktop timed out: ${label}; ${['#status','[data-output]','[data-preview-status]'].map(selector=>document.querySelector(selector)?.textContent??'').join('; ')}`);
      await new Promise(resolve => setTimeout(resolve, 25));
    }
  };
  const click = selector => {
    const element = document.querySelector(selector);
    if (!element) throw new Error(`Missing Studio control: ${selector}`);
    element.click();
  };
  const source = value => {
    const input = document.querySelector('#playground-source');
    input.value = value; input.dispatchEvent(new InputEvent('input', {bubbles:true}));
  };
  try {
    await wait(() => host = window.__volangDesktop, 'native transport');
    await wait(() => window.__studioNext?.ready, 'initial view');
    click('[aria-describedby~="gallery-action-help"]');
    await wait(() => document.querySelector('[data-demo-count]')?.textContent.startsWith('1 '), 'Gallery state');
    click('[data-nav="docs"]');
    await wait(() => document.querySelector('[data-document="first-steps"]')?.getAttribute('aria-busy') === 'false' && document.querySelector('[data-document] h2'), 'offline documentation');
    if (!document.querySelector('[data-document]').textContent.includes('Create your first application')) throw new Error('Missing documentation body');
    click('[data-nav="playground"]');
    await wait(() => document.querySelector('#playground-source'), 'Playground editor');
    await wait(() => document.querySelector('.cm-content'), 'optional source editor');
    source('package main\nimport "fmt"\nfunc main() { fmt.Println("Desktop 中文") }\n');
    click('[data-run]');
    await wait(() => document.querySelector('[data-output]')?.textContent === 'Desktop 中文\n', 'compiler Worker output');
    await wait(() => localStorage.getItem('volang.studio.next.draft.v1')?.includes('Desktop 中文'), 'saved draft');
    source('package main\nfunc main() { missingDesktopName() }\n');
    click('[data-run]');
    await wait(() => document.querySelector('[data-output]')?.textContent.includes('missingDesktopName'), 'compile failure');
    source('package main\nfunc main() { for {} }\n');click('[data-run]');
    await wait(() => document.querySelector('[data-stop]')?.disabled === false, 'running state');click('[data-stop]');
    await wait(() => document.querySelector('[data-output]')?.textContent.startsWith('Stopped.'), 'Worker cancellation');
    click('a[href="/studio/playground/ui"]');
    await wait(() => document.querySelector('[data-run-preview]'), 'UI Playground');
    click('[data-run-preview]');
    await wait(() => document.querySelector('[data-preview-status]')?.textContent.startsWith('Your preview is ready'), 'UI preview Worker');
    const frame = document.querySelector('iframe');
    const button = [...frame.contentDocument.querySelectorAll('button')].find(value => value.textContent === 'One more idea');
    if (!button) throw new Error('Missing UI preview action');
    button.click();button.click();
    await wait(() => frame.contentDocument.querySelector('output')?.textContent === '2 little ideas', 'UI preview interaction');
    click('[data-stop-preview]');
    await wait(() => window.__studioNext.workers.started === window.__studioNext.workers.stopped, 'Worker disposal');
    click('[data-nav="gallery"]');
    await wait(() => document.querySelector('[data-demo-count]'), 'return to Gallery');
    if (document.querySelector('.cm-content') || document.querySelector('iframe')) throw new Error('Studio retained a disposed editor or preview');
    host.close();
  } catch (error) {
    if (host) host.fail(String(error)); else throw error;
  }
})();
