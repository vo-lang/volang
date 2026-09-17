export const serverProtocol = 4;
export const serverMethods = ['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS'];
export const maxBodyBytes = 1024 * 1024;
// JSON escaping can expand each body byte sixfold; metadata has its own bounds.
export const maxRequestBytes = 8 * 1024 * 1024;

export class RequestError extends Error {
  constructor(status, message) { super(message); this.status = status; }
}

/** Read only after admission. Cancellation releases listeners without destroying
 * the socket, so the adapter can still send a deadline or size-limit response. */
export async function requestBody(request, signal) {
  signal.throwIfAborted();
  const length = request.headers['content-length'];
  if (length !== undefined && (!/^\d+$/.test(length) || Number(length) > maxBodyBytes)) {
    throw new RequestError(413, 'Request body exceeds 1 MiB.');
  }
  if (request.method === 'GET' || request.method === 'HEAD') {
    if (Number(length || 0) !== 0 || request.headers['transfer-encoding']) throw new RequestError(400, 'GET and HEAD do not accept a request body.');
    return '';
  }
  if (request.headers['content-encoding'] && request.headers['content-encoding'].toLowerCase() !== 'identity') {
    throw new RequestError(415, 'This entry accepts uncompressed UTF-8 request bodies.');
  }
  return new Promise((resolve, reject) => {
    const chunks = [];
    let bytes = 0;
    const finish = (error, value) => {
      request.pause();
      request.removeListener('data', data);
      request.removeListener('end', end);
      request.removeListener('error', failure);
      request.removeListener('close', closed);
      signal.removeEventListener('abort', abort);
      if (error) reject(error); else resolve(value);
    };
    const failure = error => finish(error);
    const closed = () => finish(new RequestError(400, 'Incomplete request body.'));
    const abort = () => finish(signal.reason);
    const data = chunk => {
      bytes += chunk.length;
      if (bytes > maxBodyBytes) finish(new RequestError(413, 'Request body exceeds 1 MiB.'));
      else chunks.push(chunk);
    };
    const end = () => {
      try { finish(null, new TextDecoder('utf-8', {fatal:true, ignoreBOM:true}).decode(Buffer.concat(chunks, bytes))); }
      catch { finish(new RequestError(400, 'Request body must be valid UTF-8.')); }
    };
    request.on('data', data);
    request.once('end', end);
    request.once('error', failure);
    request.once('close', closed);
    signal.addEventListener('abort', abort, {once:true});
    if (signal.aborted) abort();
    else if (request.readableEnded) end();
    else if (request.destroyed) closed();
  });
}
