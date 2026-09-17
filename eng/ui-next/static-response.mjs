import {createReadStream} from 'node:fs';
import {realpath, stat} from 'node:fs/promises';
import {extname, sep} from 'node:path';
import {pipeline} from 'node:stream/promises';

const types = {'.html':'text/html; charset=utf-8','.css':'text/css; charset=utf-8','.js':'text/javascript',
  '.mjs':'text/javascript','.json':'application/json','.wasm':'application/wasm','.svg':'image/svg+xml',
  '.png':'image/png','.jpg':'image/jpeg','.jpeg':'image/jpeg','.webp':'image/webp','.ico':'image/x-icon','.woff2':'font/woff2',
  '.wav':'audio/wav','.mp3':'audio/mpeg','.m4a':'audio/mp4','.ogg':'audio/ogg','.flac':'audio/flac',
  '.mp4':'video/mp4','.webm':'video/webm','.ogv':'video/ogg','.vtt':'text/vtt; charset=utf-8'};

// RFC 9110 §12.5.3. Unspecified identity remains a fallback; an explicit
// identity preference participates in ranking. No header uses identity for
// compatibility with clients that do not advertise their decoding support.
export function selectEncoding(header, available) {
  if (header === undefined || header.trim() === '') return available.includes('identity') ? 'identity' : undefined;
  const weights = new Map();
  for (const entry of header.toLowerCase().split(',')) {
    const [name, ...parameters] = entry.trim().split(';').map(value=>value.trim());
    if (!['br','gzip','identity','*'].includes(name)) continue;
    let weight = 1;
    if (parameters.length) {
      if (parameters.length !== 1) continue;
      const match = /^q\s*=\s*(0(?:\.\d{0,3})?|1(?:\.0{0,3})?)$/.exec(parameters[0]);
      if (!match) continue;
      weight = Number(match[1]);
    }
    weights.set(name,Math.min(weights.get(name) ?? 1,weight));
  }
  let selected, best = 0;
  for (const encoding of available) {
    const fallback = encoding === 'identity' ? (weights.get('*') === 0 ? 0 : 0.0001) : (weights.get('*') ?? 0);
    const weight = weights.get(encoding) ?? fallback;
    if (weight > best) {best = weight; selected = encoding;}
  }
  return selected;
}

function unchanged(request, tag, modified) {
  const condition = request.headers['if-none-match'];
  if (condition !== undefined) {
    return condition.split(',').some(value=>value.trim() === '*' || value.trim().replace(/^W\//,'') === tag.slice(2));
  }
  const since = Date.parse(request.headers['if-modified-since']);
  return Number.isFinite(since) && Math.floor(modified / 1000) <= Math.floor(since / 1000);
}

// RFC 9110 §§13.1.5/14: one byte range over the selected representation.
// Metadata validators here are weak, including modification times whose
// same-second history is unknown. If-Range therefore falls back to a full body.
// Unsupported/malformed/multiple ranges are ignored; null means unsatisfiable.
function requestedRange(request, size) {
  const header = request.headers.range;
  if (request.method !== 'GET' || request.headers['if-range'] !== undefined || typeof header !== 'string'
    || header.length > 1024 || size === 0n || size > BigInt(Number.MAX_SAFE_INTEGER)) return;
  const match = /^bytes=\s*(\d*)-(\d*)\s*$/i.exec(header.trim());
  if (!match || (!match[1] && !match[2])) return;
  let start, end = size - 1n;
  if (!match[1]) {
    const suffix = BigInt(match[2]);
    if (suffix === 0n) return null;
    start = suffix >= size ? 0n : size - suffix;
  } else {
    start = BigInt(match[1]);
    if (match[2]) {
      const last = BigInt(match[2]);
      if (last < start) return;
      if (last < end) end = last;
    }
    if (start >= size) return null;
  }
  return {start:Number(start),end:Number(end)};
}

/** Stream an already-resolved public file. No full-file buffer or compression
 * work is allocated per visitor; HEAD and 304 only read filesystem metadata. */
export async function sendStatic(request, response, path, {root, cache = true, precompressed = true, signal, status = 200} = {}) {
  if (response.destroyed || response.writableEnded) return;
  signal?.throwIfAborted();
  const original = await stat(path,{bigint:true});
  if (!original.isFile()) throw Object.assign(new Error('A public file is required.'),{code:'EISDIR'});
  const variants = new Map([['identity',{path,info:original}]]);
  if (precompressed) for (const [encoding,extension] of [['br','.br'],['gzip','.gz']]) {
    try {
      const variant = await realpath(path+extension);
      if (!variant.startsWith(root+sep)) continue;
      const info = await stat(variant,{bigint:true});
      if (info.isFile()) variants.set(encoding,{path:variant,info});
    } catch(error) {if (!['ENOENT','ENOTDIR'].includes(error.code)) throw error;}
  }
  const encoding = selectEncoding(request.headers['accept-encoding'], ['br','gzip','identity'].filter(name=>variants.has(name)));
  if (response.destroyed || response.writableEnded) return;
  signal?.throwIfAborted();
  if (!encoding) {response.writeHead(406,{'vary':'Accept-Encoding','cache-control':'no-store','content-length':0}).end(); return;}
  const selected = variants.get(encoding), info = selected.info;
  const tag = `W/"${info.size.toString(16)}-${info.mtimeNs.toString(16)}-${encoding}"`;
  const modified = Number(info.mtimeMs);
  const headers = {'content-type':types[extname(path).toLowerCase()] ?? 'application/octet-stream',
    'cache-control':cache ? 'public, max-age=0, must-revalidate' : 'no-store', 'vary':'Accept-Encoding'};
  if (encoding !== 'identity') headers['content-encoding'] = encoding;
  if (cache) {headers.etag = tag; headers['last-modified'] = new Date(modified).toUTCString();}
  if (status === 200 && cache && unchanged(request,tag,modified)) {response.writeHead(304,headers).end(); return;}
  if (status === 200) headers['accept-ranges'] = 'bytes';
  const range = status === 200 ? requestedRange(request,info.size) : undefined;
  if (range === null) {
    delete headers['content-encoding'];
    response.writeHead(416,{...headers,'content-range':`bytes */${info.size}`,'content-length':'0'}).end();
    return;
  }
  if (range) headers['content-range'] = `bytes ${range.start}-${range.end}/${info.size}`;
  response.writeHead(range ? 206 : status,{...headers,'content-length':range ? String(range.end-range.start+1) : String(info.size)});
  if (request.method === 'HEAD') {response.end(); return;}
  try {await pipeline(createReadStream(selected.path,range),response,{signal});}
  catch(error) {
    if (!response.destroyed || !['ERR_STREAM_PREMATURE_CLOSE','ERR_STREAM_UNABLE_TO_PIPE','ECONNRESET'].includes(error.code)) throw error;
  }
}
