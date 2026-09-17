/** These elements parse text rather than our normal child identity comments. */
export const textElements = new Set(['title', 'style', 'script']);
const unsupportedHtmlContent = new Set(['template', 'noscript', 'xmp', 'noembed', 'noframes', 'plaintext']);

export function dataBlockType(value: string | null | undefined): boolean {
  const type = value?.toLowerCase();
  return type === 'application/json' || type === 'application/ld+json';
}

export function validateHtmlContent(kind: string, children: number): void {
  if (unsupportedHtmlContent.has(kind)) throw new Error(kind + ' requires a dedicated HTML content binding');
  if (kind === 'iframe' && children !== 0) throw new Error('iframe cannot have managed children; use src or srcdoc');
  if (kind === 'script' && children !== 1) throw new Error('JSON data blocks require one direct Text child');
}

export function validateNativeText(kind: string, value: string): void {
  if (value.includes('\0')) throw new Error('Native text elements cannot contain NUL');
  if (kind === 'script') {
    if (value.includes('<') || value.includes('\r')) throw new Error('Data block text requires HTML escapes and LF newlines; use DataBlock');
    try {JSON.parse(value);} catch {throw new Error('Data block text requires valid JSON');}
  }
  if (kind === 'style') {
    if (value.includes('\r')) throw new Error('Style text requires LF newlines');
    if (/<\/style(?=[\t\n\f />])/i.test(value)) throw new Error('Style text cannot contain an HTML closing style tag');
  }
}
