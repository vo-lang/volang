/** Browser transport only. Application decoding, retries and form outcomes stay
 * in Vo; the task owner supplies cancellation and deadlines. */
export const MAX_HTTP_REQUEST_BYTES = 1024 * 1024;
export const MAX_HTTP_RESPONSE_BYTES = 2 * 1024 * 1024;
const encoder = new TextEncoder();
const methods = new Set(['GET', 'HEAD', 'POST', 'PUT', 'PATCH', 'DELETE', 'OPTIONS']);

export async function readResponseText(response: Response, signal: AbortSignal): Promise<string> {
  signal.throwIfAborted();
  if (!response.body) return '';
  const reader = response.body.getReader();
  let data = new Uint8Array(0);
  let bytes = 0;
  const abort = (): void => { void reader.cancel(signal.reason).catch(() => {}); };
  signal.addEventListener('abort', abort, { once: true });
  try {
    for (;;) {
      const chunk = await reader.read();
      signal.throwIfAborted();
      if (chunk.done) break;
      const size = bytes + chunk.value.byteLength;
      if (size > MAX_HTTP_RESPONSE_BYTES) throw new Error('HTTP response exceeds 2 MiB');
      if (size > data.length) {
        const grown = new Uint8Array(Math.min(MAX_HTTP_RESPONSE_BYTES, Math.max(size, data.length * 2, 1024)));
        grown.set(data.subarray(0, bytes)); data = grown;
      }
      data.set(chunk.value, bytes); bytes = size;
    }
    return new TextDecoder().decode(data.subarray(0, bytes));
  } catch (error) {
    // Cancellation must not wait for a remote peer to finish its body.
    void reader.cancel(error).catch(() => {});
    throw error;
  } finally {
    signal.removeEventListener('abort', abort);
    reader.releaseLock();
  }
}

export async function fetchText(url: string, signal: AbortSignal): Promise<string> {
  const response = await fetch(url, { signal });
  if (!response.ok) {
    void response.body?.cancel().catch(() => {});
    throw new Error(`HTTP ${response.status}`);
  }
  return readResponseText(response, signal);
}

export interface HTTPRequest {
  url:string; method:string; headers:Headers; body:string; credentials:RequestCredentials;
}

export function decodeHTTPRequest(value:string): HTTPRequest {
  const request = JSON.parse(value);
  if (!request || request.version !== 1 || typeof request.url !== 'string' || !request.url ||
      encoder.encode(request.url).length > 8192 || !methods.has(request.method) ||
      typeof request.body !== 'string' || encoder.encode(request.body).length > MAX_HTTP_REQUEST_BYTES ||
      (request.method === 'GET' || request.method === 'HEAD') && request.body !== '' ||
      !['same-origin', 'omit', 'include'].includes(request.credentials) ||
      !Array.isArray(request.headers) || request.headers.length > 128) throw new Error('Invalid HTTP request');
  const headers = new Headers();
  let headerBytes = 0, previous = '';
  for (const header of request.headers) {
    if (!header || typeof header.name !== 'string' || typeof header.value !== 'string' ||
        header.name !== header.name.toLowerCase() || header.name <= previous) throw new Error('Invalid HTTP request headers');
    headerBytes += encoder.encode(header.name).length + encoder.encode(header.value).length;
    if (headerBytes > 64 * 1024) throw new Error('HTTP request headers exceed 64 KiB');
    headers.set(header.name, header.value);
    previous = header.name;
  }
  return {url:request.url, method:request.method, headers, body:request.body, credentials:request.credentials};
}

/** Text and native multipart use the same bounded response and HTTP policy. */
export async function fetchHTTPBody(request:HTTPRequest, requestBody:BodyInit | undefined, signal:AbortSignal): Promise<string> {
  signal.throwIfAborted();
  const response = await fetch(request.url, {
    method: request.method, headers:request.headers, body:requestBody,
    credentials: request.credentials, signal,
  });
  const exposed: Record<string, string> = Object.create(null);
  let bytes = 0, count = 0;
  try {
    for (const [name, text] of response.headers) {
      bytes += encoder.encode(name).length + encoder.encode(text).length;
      if (++count > 128 || bytes > 64 * 1024) throw new Error('HTTP response headers exceed their budget');
      exposed[name] = text;
    }
    if (response.status < 200 || response.status > 599 || encoder.encode(response.url).length > 8192) throw new Error('Invalid HTTP response');
  } catch (error) { void response.body?.cancel(error).catch(() => {}); throw error; }
  const body = await readResponseText(response, signal);
  // The body limit leaves room for worst-case JSON escaping inside a UI frame.
  return JSON.stringify({ version: 1, status: response.status, url: response.url,
    redirected: response.redirected, headers: exposed, body });
}

export async function fetchHTTP(value:string, signal:AbortSignal): Promise<string> {
  const request = decodeHTTPRequest(value);
  return fetchHTTPBody(request, request.body === '' ? undefined : request.body, signal);
}
