export const contentContracts = ['content-without-vm','content-native-history','content-lazy-search','content-search-retry','content-stable-title-links','content-copy','content-theme','content-mobile'];

// Static documents have one browser owner and never hydrate a guest root.
export function canonicalStudioLinks(html) {
  return html.replace(/href="(\/studio\/[^"?#]*?)([?#][^"]*)?"/g, (_, path, suffix = '') =>
    `href="${path.replace(/\/$/, '')}/${suffix}"`);
}

export function contentDocument(html, catalog) {
  const metadata = JSON.stringify({pages:catalog.pages.map(({ID,Title,Summary,Section}) => ({ID,Title,Summary,Section})),
    search:catalog.search}).replaceAll('<', '\\u003c');
  return canonicalStudioLinks(html)
    .replace('<html ', '<html data-studio-content ')
    .replace(/\s*<script\b[^>]*>[\s\S]*?<\/script>/g, '')
    .replace(/\s*<(p|div) id="status"[^>]*>[\s\S]*?<\/\1>/, '')
    .replace('</body>', `<script id="studio-content-data" type="application/json">${metadata}</script>\n<script type="module" src="/studio-assets/content.js"></script>\n</body>`);
}
