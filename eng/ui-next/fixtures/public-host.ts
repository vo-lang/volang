import {
  mountUi, createLazyWidget, createNavigationServices,
  type UiApplication, type MountOptions, type UiServices, type UiVmRuntime,
  type TaskProvider, type TaskProviders, type WatchProvider, type WatchProviders,
  type WidgetContext, type WidgetInstance, type WidgetProvider, type WidgetProviders,
  type WidgetLoader, type LazyWidgetOptions,
} from 'vo-web/ui/next';

interface Owner {
  watches: number;
  watchAborts: number;
  widgets: number;
  widgetDisposals: number;
  emit?: (value: string) => void;
}

declare global {
  interface Window {
    applications: UiApplication[];
    owners: Owner[];
    mounted: boolean;
    failures: string[];
    remount(): Promise<boolean>;
  }
}

const query = new URLSearchParams(location.search);
const backend = 'vm';
const hydrate = query.has('hydrate');
window.owners = [];
window.failures = [];

function services(container: HTMLElement): UiServices {
  const owner: Owner = {watches: 0, watchAborts: 0, widgets: 0, widgetDisposals: 0};
  window.owners.push(owner);
  const watch: WatchProvider = (_value, signal, emit) => {
    owner.watches++;
    owner.emit = emit;
    signal.addEventListener('abort', () => { owner.watches--; owner.watchAborts++; }, {once: true});
    emit(container.id);
  };
  const widget: WidgetProvider = ({element, value, signal}: WidgetContext): WidgetInstance => {
    owner.widgets++;
    element.dataset.badge = '';
    element.textContent = value;
    return {
      update(next) { element.textContent = next; },
      dispose() {
        if (!signal.aborted) throw new Error('Widget disposal must follow cancellation.');
        owner.widgets--;
        owner.widgetDisposals++;
        element.replaceChildren();
      },
    };
  };
  const echo: TaskProvider = async (value, signal) => { signal.throwIfAborted(); return value; };
  const tasks: TaskProviders = {'example.echo': echo};
  const watches: WatchProviders = {'example.messages': watch};
  const loader: WidgetLoader = async () => widget;
  const lazyOptions: LazyWidgetOptions = {};
  const widgets: WidgetProviders = {'example.badge': createLazyWidget(loader, lazyOptions)};
  return {...createNavigationServices(container), tasks, watches, widgets};
}

function mount(container: HTMLElement, adopt: boolean): UiApplication {
  const options: MountOptions = {
    backend, artifact: './app.vob', hydrate: adopt,
    loadVm: async (): Promise<UiVmRuntime> => query.has('packaged') ? import('vo-web/wasm') : import('./runtime/vo_web.js'), services,
  };
  const application = mountUi(container, options);
  application.done.catch(error => window.failures.push(String(error)));
  return application;
}

const containers = Array.from(document.querySelectorAll<HTMLElement>('[data-root]'));
window.applications = containers.map(container => mount(container, hydrate));
window.mounted = (await Promise.all(window.applications.map(application => application.ready))).every(Boolean);
window.remount = async () => {
  window.applications[0] = mount(containers[0], false);
  return window.applications[0].ready;
};
