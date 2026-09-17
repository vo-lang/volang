import { mountUi, createNavigationServices } from '@volang/ui-next';

const status = document.getElementById('status');
const backend = new URL(location.href).searchParams.get('backend')
  ?? document.querySelector('meta[name="ui-next-backend"]')?.content ?? 'vm';
const application = mountUi(document.getElementById('root'), {
  backend,
  hydrate: document.querySelector('meta[name="ui-next-render"]')?.content === 'server',
  artifact: new URL(`app.${'vob'}`, import.meta.url),
  loadVm: () => import(new URL('vm/vo_web.js', import.meta.url).href),
  services: container => ({ ...createNavigationServices(container), initialData: async () => {
    const data = document.getElementById('ui-next-data');
    const value = data ? JSON.parse(data.textContent) : '';
    if (typeof value !== 'string') throw new Error('Page initial data must be a string.');
    return value;
  } }),
});
application.ready.then(ready => { if (ready) status.textContent = ''; });
application.done.catch(error => {
  status.setAttribute('role', 'alert');
  status.textContent = `This page could not start. ${error.message}`;
});
