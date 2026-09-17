/** Optional uPlot adapter: the application loader supplies its pinned constructor.
 * @param {any} Plot
 * @param {{mounts: number, updates: number, resizes: number, disposals: number}} [stats]
 * @returns {import('./widgets.js').WidgetProvider}
 */
export function createUPlotWidget(Plot, stats) {
  return ({ element, value, emit, signal, fail }) => {
    signal.throwIfAborted();
    const parse = value => {
      const data = JSON.parse(value);
      // JSON represents a zero-value Vo slice as null. It is an empty series;
      // absent fields and non-array values remain malformed payloads.
      const x = data?.x === null ? [] : data?.x;
      const y = data?.y === null ? [] : data?.y;
      if (!Array.isArray(x) || !Array.isArray(y) || x.length !== y.length
        || x.length > 100_000 || x.some(value => !Number.isFinite(value)) || y.some(value => !Number.isFinite(value))
        || x.some((value, index) => index > 0 && value <= x[index - 1])) throw new Error('invalid chart data');
      return [x, y];
    };
    const data = parse(value);
    const width = () => {
      if (!element.getClientRects().length) return 1;
      const style = element.ownerDocument.defaultView.getComputedStyle(element);
      let size = parseFloat(style.width);
      if (style.boxSizing === 'border-box') {
        size -= parseFloat(style.paddingLeft) + parseFloat(style.paddingRight)
          + parseFloat(style.borderLeftWidth) + parseFloat(style.borderRightWidth);
      }
      return Math.max(1, Math.floor(Number.isFinite(size) ? size : 0));
    };
    let disposed = false;
    const plot = new Plot({
      width: width(), height: 210, legend: { show: false },
      scales: { x: { time: false } },
      series: [{}, { stroke: '#35674c', width: 2, fill: '#35674c15' }],
      hooks: { setCursor: [plot => { if (plot.cursor.idx != null) emit(String(plot.cursor.idx)); }] },
    }, data, element);
    if (stats) stats.mounts++;
    let observer;
    const dispose = () => {
      if (disposed) return;
      disposed = true;
      observer?.disconnect();
      plot.destroy();
      signal.removeEventListener('abort', dispose);
      if (stats) stats.disposals++;
    };
    try {
      observer = new element.ownerDocument.defaultView.ResizeObserver(entries => {
        if (disposed) return;
        try {
          const next = Math.max(1, Math.floor(entries[entries.length - 1].contentRect.width));
          if (next !== plot.width) {
            plot.setSize({ width: next, height: 210 });
            if (stats) stats.resizes++;
          }
        } catch (error) { fail(String(error?.message ?? error)); }
      });
      observer.observe(element);
      signal.throwIfAborted();
      signal.addEventListener('abort', dispose, { once: true });
    } catch (error) { dispose(); throw error; }
    return {
      update(value) { if (disposed) return; plot.setData(parse(value)); if (stats) stats.updates++; },
      dispose,
    };
  };
}
