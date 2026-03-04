import type { FsGrepMatch } from './shell/protocol';

// =============================================================================
// terminal_fmt.ts — pure ANSI formatting helpers for terminal output.
// No side effects; all functions return string[].
// =============================================================================

// ── ANSI colour palette ───────────────────────────────────────────────────────

export const C = {
  reset:   '\x1b[0m',
  bold:    '\x1b[1m',
  dim:     '\x1b[2m',
  dir:     '\x1b[1;34m',   // bold blue
  src:     '\x1b[32m',     // green (.vo, .ts, .js, config)
  doc:     '\x1b[37m',     // white (default)
  img:     '\x1b[35m',     // magenta
  hidden:  '\x1b[2;37m',   // dim
  size:    '\x1b[36m',     // cyan
  match:   '\x1b[1;33m',   // bold yellow (grep highlight)
  lnum:    '\x1b[2;36m',   // dim cyan (line numbers)
  ok:      '\x1b[32m',     // green
  err:     '\x1b[31m',     // red
  warn:    '\x1b[33m',     // yellow
} as const;

// ── Helpers ───────────────────────────────────────────────────────────────────

export function humanSize(bytes: number): string {
  if (bytes === 0)             return '      0';
  if (bytes < 1024)            return `${bytes.toString().padStart(6)}B`;
  if (bytes < 1024 * 1024)     return `${(bytes / 1024).toFixed(1).padStart(6)}K`;
  return                              `${(bytes / (1024 * 1024)).toFixed(1).padStart(6)}M`;
}

export function fileColor(name: string, isDir: boolean): string {
  if (isDir) return C.dir;
  if (name.startsWith('.')) return C.hidden;
  const ext = name.split('.').pop()?.toLowerCase() ?? '';
  if (['vo','ts','js','jsx','tsx','mjs','cjs','rs','go','py','rb','c','h','cpp'].includes(ext)) return C.src;
  if (['toml','json','yaml','yml','env','lock','ini','cfg'].includes(ext)) return C.src;
  if (['png','jpg','jpeg','gif','svg','ico','webp','bmp'].includes(ext)) return C.img;
  return C.doc;
}

// ── Result formatters ─────────────────────────────────────────────────────────

export function formatFsListResult(data: unknown): string[] {
  if (!Array.isArray(data)) return [String(data)];
  const entries = (data as Array<{ name: string; isDir: boolean; size?: number }>)
    .sort((a, b) => Number(b.isDir) - Number(a.isDir) || a.name.localeCompare(b.name));
  if (entries.length === 0) return [`${C.dim}(empty directory)${C.reset}`];
  return entries.map(e => {
    const col = fileColor(e.name, e.isDir);
    const label = e.isDir ? e.name + '/' : e.name;
    const sz = (!e.isDir && e.size != null)
      ? `${C.dim}${C.size}${humanSize(e.size)}${C.reset}  `
      : '         ';
    return `  ${sz}${col}${label}${C.reset}`;
  });
}

export function formatStatResult(data: unknown): string[] {
  const s = data as Record<string, unknown>;
  const lines: string[] = [];
  lines.push(`  ${C.dim}name:    ${C.reset} ${C.bold}${s.name}${C.reset}`);
  lines.push(`  ${C.dim}type:    ${C.reset} ${s.isDir ? `${C.dir}directory${C.reset}` : 'file'}`);
  if (!s.isDir && s.size != null) lines.push(`  ${C.dim}size:    ${C.reset} ${C.size}${humanSize(s.size as number)} (${s.size} bytes)${C.reset}`);
  if (s.modifiedMs != null) lines.push(`  ${C.dim}modified:${C.reset} ${new Date(s.modifiedMs as number).toLocaleString()}`);
  return lines;
}

export function formatGitStatus(data: unknown): string[] {
  const items = data as Array<{ path: string; status: string }>;
  if (!items || items.length === 0) return [`${C.ok}nothing to commit, working tree clean${C.reset}`];
  return items.map(i => {
    const col = i.status.includes('?') ? C.dim : i.status.includes('D') ? C.err : C.warn;
    return `  ${col}${i.status.padEnd(2)}${C.reset} ${i.path}`;
  });
}

export function formatGitLog(data: unknown): string[] {
  const commits = data as Array<{
    id: string; summary: string; authorName: string; timeUnix: number;
  }>;
  if (!commits || commits.length === 0) return [`${C.dim}(no commits)${C.reset}`];
  const out: string[] = [];
  for (const c of commits) {
    out.push(`${C.warn}commit ${c.id.slice(0, 12)}${C.reset}`);
    out.push(`${C.dim}  Author: ${C.reset}${c.authorName}`);
    out.push(`${C.dim}  Date:   ${C.reset}${new Date(c.timeUnix * 1000).toLocaleString()}`);
    out.push(`  ${c.summary}`);
    out.push('');
  }
  if (out[out.length - 1] === '') out.pop();
  return out;
}

export function formatGitBranch(data: unknown): string[] {
  const branches = data as Array<{ name: string; isHead: boolean }>;
  if (!branches || branches.length === 0) return [`${C.dim}(no branches)${C.reset}`];
  return branches.map(b =>
    b.isHead
      ? `${C.ok}* ${b.name}${C.reset}`
      : `${C.dim}  ${b.name}${C.reset}`,
  );
}

export function formatZipList(data: unknown): string[] {
  const entries = data as Array<{ name: string; size: number; compressedSize: number; isDir: boolean }>;
  if (!entries || entries.length === 0) return [`${C.dim}(empty archive)${C.reset}`];
  return entries.map(e =>
    e.isDir
      ? `  ${C.dir}${e.name}/${C.reset}`
      : `  ${C.size}${humanSize(e.size)}${C.reset}  ${e.name}`,
  );
}

export function formatGrepMatches(matches: FsGrepMatch[], showFile: boolean): string[] {
  const out: string[] = [];
  for (const m of matches) {
    const prefix = showFile
      ? `${C.src}${m.path}${C.reset}${C.dim}:${C.reset}${C.lnum}${m.line}${C.reset}${C.dim}:${C.reset} `
      : `${C.lnum}${String(m.line).padStart(4)}${C.reset}  `;
    out.push(prefix + m.text);
  }
  return out;
}
