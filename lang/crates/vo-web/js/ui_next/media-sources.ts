/** Packaged system WebViews may need a native URL for bundled media. */
export type MediaSources = Readonly<Record<string, string>>;

export function mediaSource(value: string, kind: string, attribute: string, base: string, sources?: MediaSources): string {
  if (!sources || attribute !== 'src' || !['audio', 'video', 'source'].includes(kind)) return value;
  let url: URL, document: URL;
  try { url = new URL(value, base); document = new URL(base); }
  catch { return value; }
  // Custom protocols have an opaque URL.origin; compare their actual parts.
  if (url.protocol !== document.protocol || url.host !== document.host) return value;
  const replacement = Object.prototype.hasOwnProperty.call(sources, url.pathname) ? sources[url.pathname] : undefined;
  return replacement ? replacement + url.hash : value;
}
