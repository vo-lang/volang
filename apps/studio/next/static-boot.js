// Directory-index hosts cannot select server HTML by query string. Preserve
// older chapter links by visiting the canonical, prerendered chapter first.
const url = new URL(location.href);
const topic = url.searchParams.get('topic');
if (/^\/studio\/docs\/?$/.test(url.pathname) && topic) {
  url.pathname = /^[a-z0-9-]+$/.test(topic) ? '/studio/docs/' + topic + '/' : '/studio/unknown-chapter/';
  url.searchParams.delete('topic');
  location.replace(url.href);
} else {
  await import('./boot.js');
}
