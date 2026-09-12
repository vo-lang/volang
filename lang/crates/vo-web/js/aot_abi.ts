/** Versioned generated-image and JavaScript-host contract. */
export const AOT_MANIFEST_SECTION = 'volang.aot.v8';
export const AOT_EXTERN_SECTION = 'volang.externs.v3';
export const AOT_RUNTIME_METADATA_SECTION = 'volang.runtime.v1';
export const AOT_DEBUG_METADATA_SECTION = 'volang.debug.v3';
export const AOT_INLINE_SOURCE_SECTION = 'volang.inline-sources.v1';
export const AOT_RUNTIME_MODULE = 'volang:runtime/v3';
export const AOT_RUNTIME_FUNCTION = 'call-extern';
export const AOT_MEMORY_EXPORT = 'memory';
export const AOT_ENTRY_EXPORT = 'vo_start';
export const AOT_ALLOC_EXPORT = 'vo_alloc';
export const AOT_SEQUENCE_ALLOC_EXPORT = 'vo_alloc_sequence';
export const AOT_TYPED_ALLOC_EXPORT = 'vo_alloc_typed';
export const AOT_MAP_LOOKUP_EXPORT = 'vo_map_lookup';
export const AOT_PANIC_MESSAGE_EXPORT = 'vo_panic_message';
export const AOT_PANIC_TYPE_EXPORT = 'vo_panic_type';
export const AOT_PANIC_DATA_EXPORT = 'vo_panic_data';
export const AOT_RAISE_HOST_PANIC_EXPORT = 'vo_raise_host_panic';
export const AOT_FUEL_EXPORT = 'vo_fuel';
export const AOT_ABI_VERSION = 9;
export const AOT_CORE_MODULE_KIND = 1;
export const MAX_AOT_IMAGE_BYTES = 128 * 1024 * 1024;
export const MAX_AOT_ARGUMENT_BYTES = 16 * 1024 * 1024;
export const MAX_AOT_STDIN_BYTES = 64 * 1024 * 1024;
export const DEFAULT_AOT_MEMORY_LIMIT_PAGES = 4096;
export const MAX_EXTERN_COUNT = 1_000_000;


// Names are source-level identities. Cache their UTF-8 encoding once; dispatch
// compares canonical strings without rebuilding them on every guest call.
const externNames = new Map<string, Map<string, string>>();
export function canonicalExternName(packageName: string, functionName: string): string {
  let names = externNames.get(packageName);
  if (!names) { names = new Map(); externNames.set(packageName, names); }
  let name = names.get(functionName);
  if (name === undefined) {
    const encoder = new TextEncoder();
    name = `vo1:${encoder.encode(packageName).byteLength}:${packageName}`
      + `:${encoder.encode(functionName).byteLength}:${functionName}`;
    names.set(functionName, name);
  }
  return name;
}
