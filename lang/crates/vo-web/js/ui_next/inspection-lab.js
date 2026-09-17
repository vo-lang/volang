import { renderInspection } from './inspection-view.js';
import { mountUi } from './mount.js';
import { createInspectionServices } from './inspection.js';

const inspector = createInspectionServices();
const backend = new URL(location.href).searchParams.get('backend') ?? 'vm';
const state = window.__inspection = { ready: false, error: null, snapshot: null, close: null, capture: null };
const button = document.getElementById('capture-inspection');
const status = document.getElementById('inspection-status');
const content = document.getElementById('inspection-content');
let application;
const capture = async () => {
  button.disabled = true;
  try { state.snapshot = await inspector.snapshot(); renderInspection(content, status, state.snapshot); }
  catch (error) { status.textContent = String(error.message ?? error); }
  finally { button.disabled = !state.ready; }
  return state.snapshot;
};
state.capture = capture;
button.addEventListener('click', capture);
try {
  application = mountUi(document.getElementById('root'), {
    backend, artifact: `/artifacts/inspection.${'vob'}`,
    services: inspector.services, loadVm: () => import('/wasm/vo_web.js'),
  });
  state.close = () => { state.ready = false; button.disabled = true; inspector.close(); application.close(); };
  state.done = application.done;
  void application.done.then(state.close, error => { state.error = String(error); state.close(); status.textContent = state.error; });
  if (await application.ready) { state.ready = true; await capture(); }
} catch (error) { state.error = String(error); status.textContent = state.error; inspector.close(); }
