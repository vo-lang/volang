import {recoveryRead} from './recovery-cancel.js';

// The watch owns the download URL after preparation finishes. Leaving the page,
// replacing the request, cancelling or reloading releases every owned resource.
export function createRecoveryWatch(window, {
  loadLibrary = () => import('/artifacts/recovery-library.js'),
  timeoutMilliseconds = 30000,
} = {}) {
  if (!Number.isInteger(timeoutMilliseconds) || timeoutMilliseconds < 1 || timeoutMilliseconds > 60000) {
    throw new Error('Recovery preparation deadline is invalid.');
  }
  return (value, signal, emit) => {
    if (signal.aborted) return;
    if (value.length > 4096) throw new Error('Recovery request is too large.');
    const request = JSON.parse(value);
    if (!request || request.version !== 1 || !['inspect', 'export'].includes(request.operation) ||
      typeof request.project !== 'string') throw new Error('Recovery request is invalid.');
    let stopped = false, objectURL, timer;
    const operation = new AbortController();
    const dispose = () => {
      if (stopped) return;
      stopped = true; clearTimeout(timer); operation.abort(new Error('Recovery cancelled.'));
      signal.removeEventListener('abort', dispose);
      if (objectURL) window.URL.revokeObjectURL(objectURL);
      objectURL = undefined;
    };
    const failed = error => {
      if (stopped) return;
      dispose(); emit('', String(error?.message ?? error));
    };
    signal.addEventListener('abort', dispose, {once:true});
    timer = setTimeout(() => failed(new Error('Preparing this download took too long. Please retry.')), timeoutMilliseconds);
    void (async () => {
      const {inspectBrowserProjects, archiveBrowserProject} = await recoveryRead(() => import('./recovery-storage.js'), operation.signal);
      operation.signal.throwIfAborted();
      if (request.operation === 'inspect') {
        const projects = await inspectBrowserProjects(window.navigator.storage, operation.signal);
        operation.signal.throwIfAborted(); clearTimeout(timer);
        emit(JSON.stringify({version:1, kind:'projects', projects}));
        return;
      }
      const library = await recoveryRead(loadLibrary, operation.signal);
      operation.signal.throwIfAborted();
      const archive = await archiveBrowserProject(window.navigator.storage, request.project, library, operation.signal);
      operation.signal.throwIfAborted(); clearTimeout(timer);
      objectURL = window.URL.createObjectURL(archive.blob);
      emit(JSON.stringify({version:1, kind:'archive', url:objectURL,
        filename:'volang-' + request.project + '.zip', bytes:archive.bytes, sourceBytes:archive.sourceBytes, files:archive.files}));
    })().catch(failed);
  };
}
