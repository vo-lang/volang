import {projectEntries} from './project-entries.mjs';

export const maxPageDataBytes = 1024 * 1024;
export const maxPages = 256;
export const maxPagesHtmlBytes = 64 * 1024 * 1024;

export function documentMetadata(value = {}) {
  if (!value || typeof value !== 'object' || Array.isArray(value)
      || Object.keys(value).some(key => !['title', 'description'].includes(key))) {
    throw new Error('Document metadata accepts title and description.');
  }
  const result = {};
  for (const [key, limit] of [['title', 4096], ['description', 8192]]) {
    if (value[key] === undefined) continue;
    if (typeof value[key] !== 'string' || !value[key].isWellFormed()
        || /[\u0000-\u0008\u000b\u000c\u000e-\u001f\u007f]/.test(value[key]) || Buffer.byteLength(value[key]) > limit) {
      throw new Error(`Document ${key} must be a valid Unicode string within ${limit} bytes.`);
    }
    result[key] = value[key];
  }
  return result;
}

export function initialPageData(value) {
  if (typeof value !== 'string' || !value.isWellFormed() || Buffer.byteLength(value) > maxPageDataBytes) {
    throw new Error('Page initial data must be a valid Unicode string within 1 MiB.');
  }
  return value;
}

export function staticPagePath(value) {
  if (typeof value !== 'string') throw new Error('Static page paths must be strings.');
  if (!value.isWellFormed() || Buffer.byteLength(value) > 1024) throw new Error('Static page paths must be valid Unicode within 1024 bytes.');
  let path = value.normalize('NFC');
  if (!path.startsWith('/') || /[\\%?#<>:"|*\u0000-\u001f\u007f]/.test(path) || path.includes('//')) {
    throw new Error(`Invalid static page path: ${value}`);
  }
  if (!path.endsWith('/')) path += '/';
  const parts = path.slice(1, -1).split('/').filter(Boolean);
  if (parts.some(part => ['.', '..'].includes(part) || /[. ]$/.test(part) || Buffer.byteLength(part) > 255 ||
    /^(con|prn|aux|nul|com[1-9]|lpt[1-9])(?:\.|$)/i.test(part)) ||
    ['assets', 'theme.css', 'build-report.json', 'index.html', 'third_party_notices.txt'].includes(parts[0]?.toLowerCase())) {
    throw new Error(`Static page path conflicts with generated output: ${value}`);
  }
  return {path, file:[...parts, 'index.html'].join('/'), assets:parts.length ? '../'.repeat(parts.length) : './'};
}

// URL paths map to portable directory-index files. Canonical keys prevent two
// declarations from overwriting one page on a case-insensitive filesystem.
export function prerenderPages(config) {
  const entries = projectEntries(config);
  const defaults = documentMetadata(config.document);
  const declared = config.prerenderPages;
  const pages = declared === undefined ? [{ path: '/' }] : declared;
  if (!Array.isArray(pages) || !pages.length || pages.length > maxPages) {
    throw new Error(`prerenderPages requires 1..${maxPages} pages.`);
  }
  const seen = new Set();
  const result = pages.map(page => {
    if (!page || typeof page !== 'object' || Array.isArray(page) || typeof page.path !== 'string' ||
      Object.keys(page).some(key => !['path', 'data', 'title', 'description', 'entry'].includes(key))) throw new Error('Each prerender page requires path, optional string data, entry and document metadata.');
    const entry = entries.get(page.entry === undefined ? 'default' : page.entry);
    if (!entry) throw new Error(`Unknown page entry: ${String(page.entry)}.`);
    if (declared !== undefined && !entry.prerenderEntry) throw new Error(`prerenderPages requires prerenderEntry for entry ${entry.id}.`);
    const location = staticPagePath(page.path), {path} = location;
    const key = path.toLowerCase();
    if (seen.has(key)) throw new Error(`Duplicate static page path: ${page.path}`);
    seen.add(key);
    return { ...location,
      ...(page.entry === undefined ? {} : {entry:entry.id}),
      data: initialPageData(page.data === undefined ? '' : page.data), ...defaults,
      ...documentMetadata({ title: page.title, description: page.description }) };
  });
  if (!seen.has('/')) throw new Error('prerenderPages must include the root page /.');
  return result;
}
