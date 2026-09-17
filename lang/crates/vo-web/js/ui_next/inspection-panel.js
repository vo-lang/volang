import { renderInspection } from './inspection-view.js';

// Development documents own their registry. Removing the last application
// removes all controls/listeners; frames and independent documents stay isolated.
const documents = new WeakMap();

function createPanel(document) {
  const host = document.createElement('div');
  host.dataset.uiInspector = '';
  const shadow = host.attachShadow({ mode: 'open' });
  const style = document.createElement('style');
  style.textContent = `
    :host { all: initial; position: fixed; z-index: 2147483646; right: 16px; bottom: 16px; max-width: calc(100vw - 32px); font: 13px/1.55 system-ui, sans-serif; color: #243a30; }
    * { box-sizing: border-box; }
    #panel { background: #fff; border: 1px solid #cbd6c7; border-radius: 12px; box-shadow: 0 8px 32px #20362826; }
    #panel > summary { cursor: pointer; padding: 10px 16px; font-weight: 650; }
    #body { width: 580px; max-width: calc(100vw - 32px); max-height: 65vh; overflow: auto; padding: 0 16px 12px; }
    #toolbar { display: flex; flex-wrap: wrap; align-items: center; gap: 8px; }
    select { max-width: 55%; }
    select, button { font: inherit; color: inherit; border: 1px solid #bccdb5; background: #f4f7f0; border-radius: 6px; padding: 5px 8px; }
    button { cursor: pointer; } button:disabled { cursor: default; opacity: .6; }
    :focus-visible { outline: 3px solid #97b77a; outline-offset: 2px; }
    #status, .muted { color: #647367; font-size: 12px; }
    #content details { border-top: 1px solid #e0e7d9; padding: 10px 0; }
    #content summary { cursor: pointer; font-weight: 650; overflow-wrap: anywhere; }
    table { width: 100%; border-collapse: collapse; margin: 8px 0; font-size: 12px; }
    th, td { text-align: start; vertical-align: top; padding: 5px; border-bottom: 1px solid #e8eddf; overflow-wrap: anywhere; }
    .source, .cause { font: 11px/1.55 ui-monospace, monospace; overflow-wrap: anywhere; }
    p { margin: 6px 0; }
  `;
  const panel = document.createElement('details'); panel.id = 'panel';
  const title = document.createElement('summary'); title.textContent = 'Inspect components';
  const body = document.createElement('div'); body.id = 'body';
  const toolbar = document.createElement('div'); toolbar.id = 'toolbar';
  const label = document.createElement('label'); label.htmlFor = 'roots'; label.textContent = 'Application';
  const select = document.createElement('select'); select.id = 'roots';
  const button = document.createElement('button'); button.type = 'button'; button.textContent = 'Capture snapshot';
  const status = document.createElement('p'); status.id = 'status'; status.setAttribute('role', 'status'); status.textContent = 'Choose an application and capture its state.';
  const content = document.createElement('div'); content.id = 'content';
  toolbar.append(label, select, button); body.append(toolbar, status, content); panel.append(title, body); shadow.append(style, panel);
  document.body.append(host);
  const roots = new Map();
  let nextId = 0, generation = 0, busy = false;
  const invalidate = () => { generation++; busy = false; button.disabled = roots.size === 0; content.replaceChildren(); };
  const capture = async () => {
    if (busy) return;
    const current = roots.get(select.value);
    if (!current) return;
    const turn = ++generation;
    busy = true; button.disabled = true;
    status.textContent = 'Capturing…';
    try {
      const snapshot = await current.inspector.snapshot();
      if (turn === generation) renderInspection(content, status, snapshot);
    } catch (error) { if (turn === generation) status.textContent = String(error.message ?? error); }
    finally { if (turn === generation) { busy = false; button.disabled = roots.size === 0; } }
  };
  button.addEventListener('click', capture);
  select.addEventListener('change', () => { invalidate(); void capture(); });
  panel.addEventListener('toggle', () => { if (panel.open && content.childElementCount === 0) void capture(); });
  return {
    add(container, inspector) {
      const id = String(++nextId);
      const option = document.createElement('option'); option.value = id; option.textContent = container.id || `Application ${id}`;
      roots.set(id, { inspector }); select.append(option);
      return () => {
        if (!roots.delete(id)) return;
        const selected = select.value === id;
        option.remove();
        if (selected) { invalidate(); status.textContent = 'Choose an application and capture its state.'; if (panel.open && roots.size) void capture(); }
        if (!roots.size) { generation++; host.remove(); documents.delete(document); }
      };
    },
  };
}

export function attachInspectionPanel(container, inspector) {
  const document = container.ownerDocument;
  let panel = documents.get(document);
  if (!panel) { panel = createPanel(document); documents.set(document, panel); }
  return panel.add(container, inspector);
}
