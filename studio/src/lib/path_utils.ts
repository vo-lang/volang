export function normalizePath(path: string): string {
  const collapsed = path.replace(/\/+/g, '/');
  const isAbsolute = collapsed.startsWith('/');
  const parts: string[] = [];
  for (const segment of collapsed.split('/')) {
    if (!segment || segment === '.') continue;
    if (segment === '..') {
      if (parts.length > 0 && parts[parts.length - 1] !== '..') {
        parts.pop();
      } else if (!isAbsolute) {
        parts.push('..');
      }
      continue;
    }
    parts.push(segment);
  }
  const normalized = `${isAbsolute ? '/' : ''}${parts.join('/')}`;
  if (isAbsolute) return normalized || '/';
  return normalized || '.';
}

export function dirname(path: string): string {
  const normalized = normalizePath(path);
  if (normalized === '/') return '/';
  const idx = normalized.lastIndexOf('/');
  if (idx < 0) return '.';
  if (idx === 0) return '/';
  return normalized.slice(0, idx);
}

export function joinPath(base: string, rel: string): string {
  if (!rel) return normalizePath(base);
  if (rel.startsWith('/')) return normalizePath(rel);
  return normalizePath(`${base}/${rel}`);
}

export function isPathWithinRoot(path: string, root: string): boolean {
  const normalizedPath = normalizePath(path);
  const normalizedRoot = normalizePath(root);
  if (!normalizedPath.startsWith('/')) return false;
  if (!normalizedRoot.startsWith('/')) return false;
  return normalizedPath === normalizedRoot || normalizedPath.startsWith(normalizedRoot + '/');
}

export function sanitizePathSegment(value: string): string {
  return value.replace(/[^A-Za-z0-9._-]+/g, '_');
}
