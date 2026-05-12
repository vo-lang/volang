import { consolePush } from '../../stores/console';

export type UiConsoleLine = {
  kind: 'stdout' | 'stderr' | 'system' | 'success';
  text: string;
};

export type StudioLogRecord = {
  source: string;
  code: string;
  level: UiConsoleLine['kind'];
  text?: string;
  path?: string;
  module?: string;
  version?: string;
  durationMs?: number;
  assetKind?: string;
  assetName?: string;
  cached?: boolean;
  names?: string[];
};

type GuiLogRule<TContext> = {
  pattern: RegExp;
  format: (context: TContext, match: RegExpMatchArray) => UiConsoleLine;
};

const commonGuiLogRules: readonly GuiLogRule<(path: string) => string>[] = [
  {
    pattern: /^compile cache hit (.+)$/,
    format: (displayPath, match) => {
      const [, path] = match;
      return { kind: 'success', text: `Using cached GUI build for ${displayPath(path)}` };
    },
  },
  {
    pattern: /^compile cache store (.+)$/,
    format: (displayPath, match) => {
      const [, path] = match;
      return { kind: 'system', text: `Updated GUI build cache for ${displayPath(path)}` };
    },
  },
  {
    pattern: /^runGui compile (.+) (\d+)ms$/,
    format: (displayPath, match) => {
      const [, path, durationMs] = match;
      return { kind: 'system', text: `Compiled GUI ${displayPath(path)} in ${formatDurationMs(Number(durationMs))}` };
    },
  },
  {
    pattern: /^runGui load vm (.+) (\d+)ms$/,
    format: (displayPath, match) => {
      const [, path, durationMs] = match;
      return { kind: 'system', text: `Loaded GUI runtime for ${displayPath(path)} in ${formatDurationMs(Number(durationMs))}` };
    },
  },
  {
    pattern: /^runGui start app (.+) (\d+)ms$/,
    format: (displayPath, match) => {
      const [, path, durationMs] = match;
      return { kind: 'system', text: `Started GUI app ${displayPath(path)} in ${formatDurationMs(Number(durationMs))}` };
    },
  },
];

const voWebGuiLogRules: readonly GuiLogRule<void>[] = [
  {
    pattern: /^using cached library version (.+) -> (.+)$/,
    format: (_context, match) => {
      const [, moduleName, version] = match;
      return { kind: 'success', text: `Using cached dependency ${moduleName}@${version}` };
    },
  },
  {
    pattern: /^fetching library version for (.+)$/,
    format: (_context, match) => {
      const [, moduleName] = match;
      return { kind: 'system', text: `Resolving dependency version for ${moduleName}...` };
    },
  },
  {
    pattern: /^fetching library (.+)@([^@]+)$/,
    format: (_context, match) => {
      const [, moduleName, version] = match;
      return { kind: 'system', text: `Downloading dependency ${moduleName}@${version}...` };
    },
  },
  {
    pattern: /^fetched library (.+) in (\d+)ms$/,
    format: (_context, match) => {
      const [, moduleName, durationMs] = match;
      return { kind: 'success', text: `Downloaded dependency ${moduleName} in ${formatDurationMs(Number(durationMs))}` };
    },
  },
  {
    pattern: /^using cached library ext (.+)@([^@]+)$/,
    format: (_context, match) => {
      const [, moduleName, version] = match;
      return { kind: 'success', text: `Using cached extension for ${moduleName}@${version}` };
    },
  },
  {
    pattern: /^fetching (cached )?library ext wasm (.+)@([^@]+) \((.+)\)$/,
    format: (_context, match) => {
      const [, , moduleName, version] = match;
      return { kind: 'system', text: `Loading extension WASM for ${moduleName}@${version}...` };
    },
  },
  {
    pattern: /^fetching (cached )?library ext JS glue (.+)@([^@]+) \((.+)\)$/,
    format: (_context, match) => {
      const [, , moduleName, version] = match;
      return { kind: 'system', text: `Loading extension JS glue for ${moduleName}@${version}...` };
    },
  },
];

export function formatDurationMs(durationMs: number): string {
  return durationMs < 1000 ? `${Math.round(durationMs)}ms` : `${(durationMs / 1000).toFixed(durationMs < 10_000 ? 2 : 1)}s`;
}

function matchGuiLogRules<TContext>(
  message: string,
  rules: readonly GuiLogRule<TContext>[],
  context: TContext,
): UiConsoleLine | null {
  for (const rule of rules) {
    const match = message.match(rule.pattern);
    if (match) {
      return rule.format(context, match);
    }
  }
  return null;
}

export function pushUiConsole(line: UiConsoleLine | null): void {
  if (!line || !line.text) {
    return;
  }
  consolePush(line.kind, line.text);
}

export function renderStudioLogRecord(
  record: StudioLogRecord,
  displayPath: (path: string) => string,
): UiConsoleLine | null {
  const text = record.text?.trim();
  switch (record.code) {
    case 'stdout':
      if (!text) {
        return null;
      }
      return record.source === 'render-island'
        ? { kind: 'stdout', text: `[render-island] ${text}` }
        : { kind: 'stdout', text };
    case 'compile_cache_hit':
      return record.path ? { kind: 'success', text: `Using cached GUI build for ${displayPath(record.path)}` } : null;
    case 'compile_cache_store':
      return record.path ? { kind: 'system', text: `Updated GUI build cache for ${displayPath(record.path)}` } : null;
    case 'gui_compile_done':
      return record.path != null && record.durationMs != null
        ? { kind: 'system', text: `Compiled GUI ${displayPath(record.path)} in ${formatDurationMs(record.durationMs)}` }
        : null;
    case 'gui_load_vm_done':
      return record.path != null && record.durationMs != null
        ? { kind: 'system', text: `Loaded GUI runtime for ${displayPath(record.path)} in ${formatDurationMs(record.durationMs)}` }
        : null;
    case 'gui_start_done':
      return record.path != null && record.durationMs != null
        ? { kind: 'system', text: `Started GUI app ${displayPath(record.path)} in ${formatDurationMs(record.durationMs)}` }
        : null;
    case 'dependency_version_cached':
    case 'dependency_cached':
      return record.module != null && record.version != null
        ? { kind: 'success', text: `Using cached dependency ${record.module}@${record.version}` }
        : null;
    case 'dependency_version_resolve_start':
      return record.module ? { kind: 'system', text: `Resolving dependency version for ${record.module}...` } : null;
    case 'dependency_fetch_start':
      return record.module != null && record.version != null
        ? { kind: 'system', text: `Downloading dependency ${record.module}@${record.version}...` }
        : null;
    case 'dependency_fetch_done':
      if (record.module == null) {
        return null;
      }
      if (record.durationMs != null) {
        return { kind: 'success', text: `Downloaded dependency ${record.module} in ${formatDurationMs(record.durationMs)}` };
      }
      if (record.version != null) {
        return { kind: 'success', text: `Downloaded dependency ${record.module}@${record.version}` };
      }
      return { kind: 'success', text: `Downloaded dependency ${record.module}` };
    case 'extension_cached':
      return record.module != null && record.version != null
        ? { kind: 'success', text: `Using cached extension for ${record.module}@${record.version}` }
        : null;
    case 'prepare_gui_extensions':
      return record.names == null || record.names.length === 0
        ? { kind: 'system', text: 'Preparing GUI runtime...' }
        : { kind: 'system', text: `Preparing GUI extensions: ${record.names.join(', ')}` };
    case 'native_extension_cached':
      return record.path ? { kind: 'success', text: `Using cached native extension ${displayPath(record.path)}` } : null;
    case 'native_extension_build_start':
      return record.path ? { kind: 'system', text: `Building native extension ${displayPath(record.path)}...` } : null;
    case 'native_extension_build_done':
      return record.path ? { kind: 'success', text: `Built native extension ${displayPath(record.path)}` } : null;
    case 'extension_asset_load_start':
      if (record.module == null || record.version == null || record.assetKind == null) {
        return null;
      }
      if (record.assetKind === 'wasm') {
        return { kind: 'system', text: `Loading extension WASM for ${record.module}@${record.version}...` };
      }
      if (record.assetKind === 'js_glue') {
        return { kind: 'system', text: `Loading extension JS glue for ${record.module}@${record.version}...` };
      }
      return null;
    case 'prepare_entry_read_package_done':
    case 'prepare_entry_ensure_deps_done':
    case 'prepare_entry_load_single_file_done':
    case 'prepare_entry_resolve_install_done':
    case 'prepare_entry_done':
    case 'gui_total_done':
    case 'dependency_version_resolved':
    case 'extension_asset_load_done':
      return null;
    default:
      return text ? { kind: record.level, text } : { kind: 'system', text: `[${record.source}] ${record.code}` };
  }
}

export function formatCommonGuiLogLine(
  source: string,
  message: string,
  displayPath: (path: string) => string,
): UiConsoleLine | null {
  const trimmed = message.trim();
  if (!trimmed) {
    return null;
  }
  if (source === 'guest') {
    return { kind: 'stdout', text: trimmed };
  }
  return matchGuiLogRules(trimmed, commonGuiLogRules, displayPath);
}

export function formatVoWebGuiLogLine(message: string): UiConsoleLine | null {
  const trimmed = message.trim();
  if (!trimmed) {
    return null;
  }
  return matchGuiLogRules(trimmed, voWebGuiLogRules, undefined);
}
