import { spawn } from 'node:child_process';
import { initialPageData } from './prerender-pages.mjs';
import {validEntryId} from './project-entries.mjs';

export const contentMarker = '<!--ui-next:content-->';
export const modeMarker = '<!--ui-next:mode-->';
export const dataMarker = '<!--ui-next:data-->';
export const assetsMarker = '<!--ui-next:assets-->';
export const titleMarker = '<!--ui-next:title-->';
export const descriptionMarker = '<!--ui-next:description-->';
export const entryMarker = '<!--ui-next:entry-->';

const htmlText = value => value.replace(/[&<>"']/g, character => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[character]));

export function prepareHtml(template, enabled) {
  for (const marker of [contentMarker, modeMarker]) {
    const count = template.split(marker).length - 1;
    if (count > 1 || (enabled && count !== 1)) throw new Error(`Prerendering requires exactly one ${marker} in web/index.html.`);
  }
  for (const marker of [dataMarker, titleMarker, descriptionMarker, entryMarker]) {
    if (template.split(marker).length > 2) throw new Error(`Use at most one ${marker} in web/index.html.`);
  }
  return (html, { data = '', assets = './', title, description, entry = 'default' } = {}) => {
    initialPageData(data);
    if (!validEntryId(entry)) throw new Error('Invalid page entry identity.');
    if (entry !== 'default' && !template.includes(entryMarker)) throw new Error(`Named page entries require ${entryMarker} in web/index.html and an entry-aware web/boot.js.`);
    if (data && !template.includes(dataMarker)) throw new Error(`Initial data requires ${dataMarker} in web/index.html.`);
    if (assets !== './' && !template.includes(assetsMarker)) throw new Error(`Nested static pages require ${assetsMarker} in web/index.html.`);
    for (const [value, marker] of [[title, titleMarker], [description, descriptionMarker]]) {
      if (value !== undefined && !template.includes(marker)) throw new Error(`Document metadata requires ${marker} in web/index.html.`);
    }
    const serialized = JSON.stringify(data).replace(/[<>&\u2028\u2029]/g, character => `\\u${character.charCodeAt(0).toString(16).padStart(4, '0')}`);
    return template.replaceAll(assetsMarker, () => assets)
      .replace(entryMarker, () => entry)
      .replace(titleMarker, () => htmlText(title ?? '')).replace(descriptionMarker, () => htmlText(description ?? ''))
      .replace(dataMarker, () => serialized)
      .replace(modeMarker, html === undefined ? 'client' : 'server').replace(contentMarker, () => html ?? '');
  };
}

// Native server rendering gets a fresh process, isolated stdout and a bounded
// lifetime. Failures settle only after that process exits, before stage cleanup.
export async function renderOutput(executable, args, { cwd, env, signal, input = '', maxInputBytes = 1024 * 1024, timeoutMilliseconds = 30_000, maxBytes = 16 * 1024 * 1024 } = {}) {
  signal?.throwIfAborted();
  if (typeof input !== 'string' || !input.isWellFormed() || Buffer.byteLength(input) > maxInputBytes) throw new Error('Native render input exceeds its UTF-8 byte budget.');
  return new Promise((resolve, reject) => {
    const child = spawn(executable, args, { cwd, env, stdio: ['pipe', 'pipe', 'pipe'] });
    const chunks = [];
    let bytes = 0, diagnostic = '', failure;
    const stop = error => { failure ??= error; child.kill('SIGKILL'); };
    const abort = () => stop(signal.reason ?? new Error('Prerendering was cancelled.'));
    const timeout = setTimeout(() => stop(new Error(`Prerendering exceeded ${timeoutMilliseconds} ms.`)), timeoutMilliseconds);
    signal?.addEventListener('abort', abort, { once: true });
    if (signal?.aborted) abort();
    child.stdout.on('data', chunk => {
      if (failure) return;
      bytes += chunk.length;
      if (bytes > maxBytes) stop(new Error(`Prerendered HTML exceeds ${maxBytes} bytes.`));
      else chunks.push(chunk);
    });
    child.stderr.on('data', chunk => { diagnostic = (diagnostic + chunk.toString('utf8')).slice(-65_536); });
    child.once('error', error => { failure ??= error; });
    child.stdin.on('error', error => stop(error));
    child.stdin.end(input);
    child.once('close', (code, exitSignal) => {
      clearTimeout(timeout);
      signal?.removeEventListener('abort', abort);
      if (failure) { reject(failure); return; }
      if (code !== 0) { reject(new Error(diagnostic.trim() || `Prerendering exited with ${exitSignal ?? code}.`)); return; }
      try {
        resolve(new TextDecoder('utf-8', { fatal: true }).decode(Buffer.concat(chunks, bytes)));
      } catch (error) { reject(error); }
    });
  });
}

export function validateRootHtml(html) {
  const range = /^<!--vo:([rt]):1-->/.exec(html);
  const element = /^<([a-z][a-z0-9-]*) data-vo-id="1"[^>]*>/.exec(html);
  const wrapped = range ? html.endsWith(`<!--vo:/${range[1]}:1-->`)
    : element && (html === element[0] || html.endsWith(`</${element[1]}>`));
  if (!wrapped) throw new Error('The prerender entry must print only host.HTML(view), without a document wrapper or extra output.');
  return html;
}

export async function renderHtml(executable, args, options) {
  initialPageData(options?.input ?? '');
  return validateRootHtml(await renderOutput(executable, args, options));
}
