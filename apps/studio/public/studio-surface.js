import { connectUiVmToDom, createVmIsland, init } from '/runtime/dist/index.js';

// Neutral asset naming avoids privacy filters that block generic "preview-host" URLs.
const PROTOCOL = 'volang.studio.preview.v1';
const MAX_ARTIFACT_BYTES = 128 * 1024 * 1024;
const root = document.querySelector('#preview-root');
const diagnostic = document.querySelector('#preview-error');
// `srcdoc` documents can expose an empty referrer and a serialized `null`
// location origin even though the module itself is served by the embedding
// application. Derive the authority from this same-origin module URL so the
// postMessage boundary remains strict in every browser and deployment host.
const embeddingOrigin = new URL(import.meta.url).origin;
let session;
let runtimeReady;

function showError(cause) {
  const error = cause instanceof Error ? cause : new Error(String(cause));
  diagnostic.textContent = error.stack ?? error.message;
  diagnostic.style.display = 'block';
}

window.addEventListener('message', (event) => {
  if (event.source !== window.parent || event.origin !== embeddingOrigin) return;
  const message = event.data;
  // Studio and the embedded application share one same-origin parent/child
  // channel. Messages for other Studio services are unrelated to this surface
  // and must remain invisible to the preview.
  if (message?.protocol !== PROTOCOL) return;
  if (!(message.artifact instanceof ArrayBuffer) || message.artifact.byteLength === 0
    || message.artifact.byteLength > MAX_ARTIFACT_BYTES) {
    showError(new Error('Studio preview payload is invalid'));
    return;
  }
  void (async () => {
    diagnostic.textContent = '';
    diagnostic.style.display = 'none';
    session?.dispose();
    root.replaceChildren();
    runtimeReady ??= init(new URL('/runtime/pkg/vo_web_bg.wasm', embeddingOrigin));
    await runtimeReady;
    session = connectUiVmToDom(createVmIsland(new Uint8Array(message.artifact)), root, {
      onError: showError,
      initialLocation: '/',
    });
    session.start();
  })().catch(showError);
});
