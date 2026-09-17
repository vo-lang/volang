// Included only by the development server, outside the application component tree.
const events = new EventSource('/__ui-next/events');
const failures = new Map();
let diagnostic, reloadNotice, compileError;
const renderErrors = () => {
  const messages = [compileError, ...failures.values()].filter(Boolean);
  if (!messages.length) { diagnostic?.remove(); diagnostic = undefined; return; }
  reloadNotice?.remove(); reloadNotice = undefined;
  diagnostic ??= document.body.appendChild(document.createElement('pre'));
  diagnostic.id = 'ui-development-error'; diagnostic.setAttribute('role', 'alert');
  diagnostic.style.cssText = 'position:fixed;z-index:1000;bottom:16px;left:16px;right:16px;max-height:35vh;overflow:auto;white-space:pre-wrap;padding:20px;color:#ffe5e0;background:#442b2b;border-radius:12px;font:13px/1.6 monospace';
  diagnostic.textContent = messages.join('\n\n');
};
const onReloadStatus = event => {
  const { source, type, message, report } = event.detail;
  if (type === 'error') failures.set(source, `${source?.label ?? 'Application'}: ${message}`);
  else failures.delete(source);
  renderErrors();
  if (type === 'closed') { reloadNotice?.remove(); reloadNotice = undefined; }
  if (report && !diagnostic) {
    reloadNotice ??= document.body.appendChild(document.createElement('p'));
    reloadNotice.id = 'ui-development-reload'; reloadNotice.setAttribute('role', 'status');
    reloadNotice.style.cssText = 'position:fixed;z-index:999;bottom:12px;right:16px;margin:0;padding:9px 13px;border-radius:8px;background:#20372d;color:#f2f7ef;font:12px/1.5 system-ui;box-shadow:0 2px 10px #0002';
    reloadNotice.textContent = `UI updated · ${report.restored} states kept, ${report.reset} reset`;
    reloadNotice.title = (report.messages ?? []).join('\n');
  }
};
window.addEventListener('vo-ui-reload-status', onReloadStatus);
events.onmessage = event => {
  const message = JSON.parse(event.data);
  if (message.type === 'styles') {
    for (const link of document.querySelectorAll('link[rel=stylesheet]')) {
      const url = new URL(link.href);
      url.searchParams.set('ui-dev', String(Date.now()));
      link.href = url.href;
    }
  } else if (message.type === 'guest') {
    compileError = undefined; renderErrors();
    if (window.dispatchEvent(new CustomEvent('vo-ui-reload', { cancelable: true, detail: { version: message.version } }))) location.reload();
  } else if (message.type === 'reload') {
    location.reload();
  } else if (message.type === 'error') {
    compileError = message.message; renderErrors();
  }
};
window.addEventListener('pagehide', () => {
  events.close(); window.removeEventListener('vo-ui-reload-status', onReloadStatus); failures.clear();
}, { once: true });
// A restored development document reconnects to the current compiled version.
// Production mounts keep their state in the browser's back/forward cache.
window.addEventListener('pageshow', event => { if (event.persisted) location.reload(); });
