// Redirect documents share this external entry and retain a usable native link.
// Backends and fragments survive legacy bookmarks and the root landing page.
const link = document.getElementById('studio-redirect');
const target = new URL(link.href);
target.search = location.search;
target.hash = location.hash;
location.replace(target.href);
