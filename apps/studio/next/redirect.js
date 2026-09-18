// Redirect documents share this external entry and retain a usable native link.
// Preserve query parameters and fragments at the root landing page.
const link = document.getElementById('studio-redirect');
const target = new URL(link.href);
target.search = location.search;
target.hash = location.hash;
location.replace(target.href);
