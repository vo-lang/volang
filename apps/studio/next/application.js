import { createNavigationServices } from '/host/ui_next/navigation.js';
import {createCodeEditorProvider} from '/host/ui_next/editor-provider.js';
import {studioLanguageService} from './language-service.js';
import { previewWidget } from './preview-widget.js';
import {createRecoveryWatch} from './recovery-watch.js';
import {updateLegacyRegistration} from './legacy-registration.js';
import {createPersistentStorage} from '/host/ui_next/storage.js';

export async function startStudio(mountUi, { artifact = 'studio', defaultBackend = 'vm', reload = () => location.reload() } = {}) {
  const container = document.getElementById('root');
  const status = document.getElementById('status');
  const query = new URL(location.href).searchParams;
  const backend = query.get('backend') ?? defaultBackend;
  const state = window.__studioNext = { error: null, ready: false, close: null, done: null, workers: { started: 0, stopped: 0 } };
  const drafts = createPersistentStorage('volang.studio.next.drafts.v1');

  function showError(error) {
    const wasReady = state.ready;
    state.ready = false;
    state.error = String(error?.stack ?? error);
    status.dataset.error = '';
    status.setAttribute('role', 'alert');
    const message = document.createElement('p');
    message.textContent = wasReady ? 'Studio stopped unexpectedly.' : 'Studio could not finish opening.';
    const retry = document.createElement(reload ? 'button' : 'p');
    if (reload) {
      retry.type = 'button'; retry.className = 'vui-button'; retry.textContent = 'Reload Studio';
      retry.addEventListener('click', reload);
    } else retry.textContent = 'Close this window and reopen Studio to try again.';
    const details = document.createElement('details'), summary = document.createElement('summary'), diagnostic = document.createElement('pre');
    summary.textContent = 'Details'; diagnostic.textContent = state.error;
    details.append(summary, diagnostic); status.replaceChildren(message, retry, details);
  }

  function draftTasks(service, key) {
    return {
      async [`${service}-read`](_value, signal) {
        const source = await drafts.get(key, signal, () => localStorage.getItem(key));
        return JSON.stringify({ found: source !== null, source: source ?? '' });
      },
      async [`${service}-write`](value, signal) { await drafts.set(key, value, signal); return ''; },
    };
  }

  function runSource(source, signal) {
    if (source.length > 100_000) return Promise.reject(new Error('Please keep this example under 100,000 characters.'));
    return new Promise((resolve, reject) => {
      const worker = new Worker('/studio-assets/runner.js', { type: 'module', name: 'volang-playground' });
      state.workers.started++;
      let finished = false;
      const finish = (error, output) => {
        if (finished) return;
        finished = true;
        signal.removeEventListener('abort', abort);
        worker.terminate();
        state.workers.stopped++;
        if (error) reject(error); else resolve(output);
      };
      const abort = () => finish(new Error('Run stopped.'));
      signal.addEventListener('abort', abort, { once: true });
      worker.onmessage = event => finish(event.data.error ? new Error(event.data.error) : null, JSON.stringify(event.data));
      worker.onerror = event => { event.preventDefault(); finish(new Error(event.message || 'The runner could not start.')); };
      worker.onmessageerror = () => finish(new Error('The runner returned an unreadable result.'));
      if (signal.aborted) abort(); else worker.postMessage({ source });
    });
  }

  try {
    if (!['vm'].includes(backend)) throw new Error('Unknown Studio backend.');
    const initialData = JSON.parse(document.getElementById('studio-initial-data').textContent);
    if (typeof initialData !== 'string') throw new Error('Studio initial data must be a string.');
    const services = container => {
      const navigation = createNavigationServices(container);
      return {
      ...navigation,
      initialData: async () => JSON.stringify({ Version: 1,
        Location: location.pathname + location.search + location.hash, Data: initialData }),
      widgets: {
        'studio.preview': previewWidget(state.workers),
        'code-editor': createCodeEditorProvider(() => import('/artifacts/editor-library.js'),studioLanguageService(state.workers)),
      },
      watches: {...navigation.watches, 'studio.recovery':createRecoveryWatch(container.ownerDocument.defaultView)},
      tasks: {
        ...navigation.tasks,
        ...draftTasks('studio.draft', 'volang.studio.next.draft.v1'),
        ...draftTasks('studio.ui-draft', 'volang.studio.next.ui-draft.v1'),
        'studio.run': runSource,
      },
    };
    };
    const application = mountUi(container, {
      backend, artifact: `/artifacts/${artifact}.${'vob'}`,
      loadVm: () => import('/wasm/vo_web.js'), services,
    });
    state.close = () => application.close();
    state.done = application.done.catch(showError);
    if (await application.ready) { state.ready = true; status.textContent = ''; }
  } catch (error) {
    showError(error);
  } finally {
    // Let startup finish its requests before replacing the worker serving them.
    void updateLegacyRegistration(window).catch(() => {});
  }
}
