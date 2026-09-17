import { validateHeaderName, validateHeaderValue } from 'node:http';
import { renderOutput, validateRootHtml } from './prerender.mjs';
import { documentMetadata, initialPageData } from './prerender-pages.mjs';
import { serverProtocol, maxRequestBytes } from './server-request.mjs';
import {validEntryId} from './project-entries.mjs';

const framingHeaders = new Set(['connection', 'content-length', 'content-type', 'content-encoding',
  'transfer-encoding', 'trailer', 'upgrade', 'keep-alive']);

export function responseHeaders(value) {
  const result = Object.create(null);
  if (value == null) return result;
  if (typeof value !== 'object' || Array.isArray(value) || Object.keys(value).length > 128) throw new Error('Invalid server response headers.');
  let bytes = 0, count = 0;
  for (const [name, values] of Object.entries(value)) {
    validateHeaderName(name);
    if (name !== name.toLowerCase() || framingHeaders.has(name) || !Array.isArray(values) || !values.length) throw new Error('Invalid server response header contract.');
    for (const text of values) {
      if (typeof text !== 'string' || !text.isWellFormed()) throw new Error('Invalid server response header value.');
      validateHeaderValue(name, text);
      bytes += Buffer.byteLength(name) + Buffer.byteLength(text);
      if (++count > 256 || bytes > 64 * 1024) throw new Error('Server response headers exceed their budget.');
    }
    result[name] = [...values];
  }
  return result;
}

export function decodePage(encoded) {
  if (Buffer.byteLength(encoded) > 32 * 1024 * 1024) throw new Error('Server response exceeds 32 MiB.');
  const value = JSON.parse(encoded);
  if (!value || typeof value !== 'object' || Array.isArray(value) || value.version !== serverProtocol ||
      Object.keys(value).some(key => !['version', 'kind', 'body', 'html', 'data', 'title', 'description', 'status', 'headers', 'entry'].includes(key)) ||
      !['html', 'json'].includes(value.kind) || typeof value.body !== 'string' || !value.body.isWellFormed() || Buffer.byteLength(value.body) > 2 * 1024 * 1024 ||
      !Number.isInteger(value.status) || value.status < 200 || value.status > 599 ||
      typeof value.html !== 'string' || !value.html.isWellFormed() || Buffer.byteLength(value.html) > 16 * 1024 * 1024 ||
      typeof value.title !== 'string' || typeof value.description !== 'string' || typeof value.entry !== 'string') throw new Error('Invalid server page envelope.');
  initialPageData(value.data);
  documentMetadata({title:value.title, description:value.description});
  const headers = responseHeaders(value.headers);
  const noContent = [204, 205, 304].includes(value.status);
  const redirect = value.status >= 300 && value.status < 400 && value.status !== 304 && headers.location?.length === 1 && headers.location[0] !== '';
  if (value.html ? !validEntryId(value.entry) : value.entry !== '') throw new Error('Page entry requires rendered HTML and a valid declared identity.');
  if (value.kind === 'json') {
    if (noContent || value.html || value.data || value.title || value.description) throw new Error('JSON response includes an incompatible status or HTML data.');
    JSON.parse(value.body);
  } else {
    if (value.body || (noContent ? value.html !== '' : value.html === '' && !redirect)) throw new Error('Page status and HTML do not agree.');
    if (value.html) validateRootHtml(value.html);
  }
  return {...value, headers};
}

export async function renderPage(executable, artifact, request, options = {}) {
  const input = JSON.stringify({version:serverProtocol, request});
  return decodePage(await renderOutput(executable, ['run', artifact], {...options, input, maxInputBytes:maxRequestBytes, maxBytes:32 * 1024 * 1024}));
}
