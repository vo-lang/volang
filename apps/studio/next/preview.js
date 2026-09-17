import { createWorkerUi } from '/host/ui_next/worker-host.js';

// This frame owns a separate document, keeping example styles, IDs and modal
// focus local. The containing widget directly owns and closes this session.
window.startUiPreview = (source, receive, workers) => {
  const worker = new Worker('./ui-runner.js', { type: 'module', name: 'volang-ui-preview' });
  workers.started++;
  let app, diagnostics;
  const captureDiagnostics = event => {
    if (event.data && Object.hasOwn(event.data, 'diagnostics')) diagnostics = event.data.diagnostics;
  };
  worker.addEventListener('message', captureDiagnostics);
  try { app = createWorkerUi(document.getElementById('preview-root'), worker); }
  catch (error) { worker.removeEventListener('message',captureDiagnostics); worker.terminate(); workers.stopped++; throw error; }
  let ready = false;
  void app.ready.then(value => { ready = value; if (value) receive({ state: 'ready', diagnostics }); });
  void app.done.then(() => receive({ state: 'closed', message: ready ? 'Your preview has finished.' : 'This program finished without opening a UI.', diagnostics }),
    error => receive({ state: 'error', message: String(error?.message ?? error), diagnostics }))
    .finally(() => { worker.removeEventListener('message',captureDiagnostics); workers.stopped++; });
  worker.postMessage({ source });
  return app;
};
