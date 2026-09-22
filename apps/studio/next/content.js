import {codeCopy} from './code-copy.js';

// This entry owns only exported content pages; no VM attaches to these roots.
const alias = document.documentElement.dataset.studioAlias;
if (alias) history.replaceState(history.state, '', alias + location.search + location.hash);
const lifetime = new AbortController();
const {signal} = lifetime;
const catalog = JSON.parse(document.getElementById('studio-content-data').textContent);
const theme = document.getElementById('studio-theme');
function applyTheme() { document.querySelector('.studio').dataset.theme = theme.checked ? 'dark' : 'light'; }
theme.addEventListener('change', applyTheme, {signal});
applyTheme();
for (const block of document.querySelectorAll('.doc-code-block')) {
  const element = block.querySelector('.studio-code-tools'), code = block.querySelector('pre');
  if (element && code) codeCopy({element, value:code.textContent, signal});
}
const input = document.getElementById('docs-search');
const nav = document.querySelector('.studio-doc-nav');
const titles = nav.querySelector('.studio-doc-groups');
const links = [...titles.querySelectorAll('a')].map(link => {
  const path = new URL(link.href).pathname.replace(/\/$/, '');
  const id = path === '/studio/docs' ? 'first-steps' : path.split('/').at(-1);
  return {link, page:catalog.pages.find(page => page.ID === id)};
});
const status = document.createElement('div');
status.className = 'studio-doc-search-status';
status.setAttribute('role', 'status');
const content = document.createElement('div');
content.className = 'studio-doc-groups';
nav.append(status, content);
let index, loading = false, failure = false;
const terms = () => input.value.trim().toLowerCase().split(/\s+/).filter(Boolean);
const matches = (text, query) => query.every(term => text.toLowerCase().includes(term));
function render() {
  const query = terms(), titleIDs = new Set();
  for (const {link, page} of links) {
    const visible = matches(`${page.Title} ${page.Summary} ${page.Section}`, query);
    link.parentElement.hidden = !visible;
    if (visible) titleIDs.add(page.ID);
  }
  for (const section of titles.children) section.hidden = ![...section.querySelectorAll('li')].some(li => !li.hidden);
  content.replaceChildren();
  if (query.length && index) {
    for (const page of catalog.pages) {
      if (titleIDs.has(page.ID) || !matches(index.get(page.ID), query)) continue;
      const link = document.createElement('a'), item = document.createElement('p');
      link.href = page.ID === 'first-steps' ? '/studio/docs/' : `/studio/docs/${page.ID}/`;
      link.textContent = page.Title; item.append(link); content.append(item);
    }
  }
  status.replaceChildren();
  if (!query.length) return;
  if (failure) {
    const message = document.createElement('p'), retry = document.createElement('button');
    message.textContent = 'Chapter text search is unavailable. You can still search titles.';
    retry.type = 'button'; retry.textContent = 'Retry text search';
    retry.onclick = () => {failure = false; void loadIndex();};
    status.append(message, retry);
  } else if (loading) status.textContent = 'Searching chapter text…';
  else if (!titleIDs.size && !content.children.length) status.textContent = 'No matching chapters.';
}
async function loadIndex() {
  if (loading || index || failure || !terms().length) return;
  loading = true; render();
  try {
    const response = await fetch(`/studio-docs/${catalog.search.Asset}?v=${catalog.search.SHA256}`,
      {signal:AbortSignal.any([signal, AbortSignal.timeout(10000)])});
    if (!response.ok) throw new Error('Search request failed');
    const reader = response.body.getReader(), chunks = []; let size = 0;
    try {
      for (;;) {
        const {done, value} = await reader.read(); if (done) break;
        size += value.length; if (size > 2 * 1024 * 1024) throw new Error('Search index too large');
        chunks.push(value);
      }
    } finally { await reader.cancel(); }
    const bytes = new Uint8Array(size); let offset = 0;
    for (const chunk of chunks) {bytes.set(chunk, offset); offset += chunk.length;}
    const data = JSON.parse(new TextDecoder().decode(bytes));
    const known = new Set(catalog.pages.map(page => page.ID));
    if (data.version !== 1 || !Array.isArray(data.pages) || data.pages.length !== known.size) throw new Error('Invalid search index');
    const parsed = new Map();
    for (const page of data.pages) {
      if (!known.delete(page.ID) || typeof page.Text !== 'string') throw new Error('Invalid search chapter');
      parsed.set(page.ID, page.Text);
    }
    index = parsed;
  } catch { if (!signal.aborted) failure = true; }
  finally {loading = false; if (!signal.aborted) render();}
}
input.addEventListener('input', () => {render(); void loadIndex();}, {signal});
render(); void loadIndex();
window.addEventListener('pagehide', event => {if (!event.persisted) lifetime.abort();}, {signal});
document.documentElement.dataset.contentReady = '';
