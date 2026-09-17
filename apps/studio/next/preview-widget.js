export function previewWidget(workers) {
  return context => {
    const frame = document.createElement('iframe');
    frame.title = 'Interactive UI preview';
    frame.className = 'studio-preview-frame';
    let app, disposed = false, loaded = false;
    const theme = context.element.closest('[data-theme]');
    const syncTheme = () => {
      if (frame.contentDocument?.body) frame.contentDocument.body.dataset.theme = theme?.dataset.theme ?? 'light';
    };
    const observer = new MutationObserver(syncTheme);
    if (theme) observer.observe(theme, { attributes: true, attributeFilter: ['data-theme'] });
    const dispose = () => {
      if (disposed) return;
      disposed = true;
      clearTimeout(loading);
      context.signal.removeEventListener('abort', dispose);
      observer.disconnect();
      app?.close();
      frame.remove();
    };
    const error = message => { context.emit(JSON.stringify({ state: 'error', message })); dispose(); };
    const loading = setTimeout(() => error('The preview took too long to load. Please try running it again.'), 30000);
    frame.onload = () => {
      if (disposed) return;
      if (loaded) { error('This preview navigated away. Run it again for a fresh start.'); return; }
      loaded = true;
      clearTimeout(loading);
      try {
        if (typeof frame.contentWindow?.startUiPreview !== 'function') throw new Error('The preview could not start. Please rebuild Studio and reload.');
        syncTheme();
        app = frame.contentWindow.startUiPreview(context.value, value => { if (!disposed) context.emit(JSON.stringify(value)); }, workers);
      } catch (cause) { error(String(cause?.message ?? cause)); }
    };
    frame.onerror = () => error('The preview could not be loaded.');
    context.signal.addEventListener('abort', dispose, { once: true });
    if (context.signal.aborted) dispose();
    else { frame.src = '/studio-assets/preview.html'; context.element.append(frame); }
    return {
      update(value) { if (value !== context.value) throw new Error('A new preview run needs a fresh widget key'); },
      dispose,
    };
  };
}
