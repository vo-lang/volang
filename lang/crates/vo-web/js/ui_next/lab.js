/** Loader for the experimental examples; production packaging is a later stage. */
import { mountUi, createLazyWidget } from './mount.js';

const status = document.getElementById('status');
const root = document.getElementById('root');
const query = new URL(location.href).searchParams;
const backend = query.get('backend') ?? 'vm';
const example = query.get('example') ?? 'interaction';
const hydrate = query.has('ssr');
window.__uiNext = { ready: false, error: null, close: null, done: null, widgets: { mounts: 0, updates: 0, resizes: 0, disposals: 0 } };

try {
  if (!['vm'].includes(backend) || !['interaction', 'workbench', 'styling'].includes(example)) throw new Error('unknown lab example or backend');
  const services = { initialData: JSON.parse(document.getElementById('ui-initial-data')?.textContent ?? '""') };
  if (example === 'workbench') {
    window.__uiNext.customElements = { properties: 0, attributes: 0, connections: 0, disconnections: 0 };
    services.widgets = {
      uplot: createLazyWidget(async (_document, signal) => {
        const [{ default: Plot }, { createUPlotWidget }] = await Promise.all([
          import('/vendor/uplot/uPlot.esm.js'), import('./uplot.js'),
        ]);
        signal.throwIfAborted();
        return createUPlotWidget(Plot, window.__uiNext.widgets);
      }),
      'example-counter': createLazyWidget(async (document, signal) => {
        const [{ createCustomElementWidget }, { defineCounter }] = await Promise.all([import('./custom-element.js'), import('./custom-element-lab.js')]);
        signal.throwIfAborted();
        defineCounter(document, window.__uiNext.customElements);
        return createCustomElementWidget({ tag: 'vo-example-counter', properties: ['model'], events: ['count-change'] });
      }),
    };
  }
  const application = mountUi(root, {
    backend, artifact: `/artifacts/${example}.${'vob'}`,
    hydrate, services, loadVm: () => import('/wasm/vo_web.js'),
  });
  window.__uiNext.close = () => application.close();
  window.__uiNext.done = application.done;
  void application.done.catch(error => {
    window.__uiNext.error = String(error?.stack ?? error);
    status.dataset.error = '';
    status.textContent = window.__uiNext.error;
  });
  if (await application.ready) {
    status.textContent = `${'Wasm VM'} · UI rewrite lab`;
    window.__uiNext.ready = true;
  }
} catch (error) {
  window.__uiNext.error = String(error?.stack ?? error);
  status.dataset.error = '';
  status.textContent = window.__uiNext.error;
}
