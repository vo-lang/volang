import type {TaskProviders} from './tasks.js';
import type {WidgetContext, WidgetInstance, WidgetProviders} from './widgets.js';
import {decodeHTTPRequest, fetchHTTPBody} from './http.js';

const maxFiles = 32, maxFileBytes = 128 * 1024 * 1024;
const maxTextBytes = 1024 * 1024, maxReadBytes = 256 * 1024;
const maxRetainedFiles = 256, maxRetainedBytes = 512 * 1024 * 1024;
interface Settings {
  version:1; inputID:string; owner:string; revision:number; tokens:string[]; error:string;
  maxFiles:number; maxFileBytes:number;
}
interface HeldFile {file:File; owner:Owner; revision:number}
interface Owner {
  key:string; revision:number; acknowledged:number; keep:Set<string>; closed:boolean;
  retire?:ReturnType<typeof setTimeout>;
}
const encoder = new TextEncoder();
const validString = (value:unknown, limit:number): value is string => typeof value === 'string'
  && !/[\uD800-\uDBFF](?![\uDC00-\uDFFF])|(?<![\uD800-\uDBFF])[\uDC00-\uDFFF]/u.test(value)
  && encoder.encode(value).length <= limit;

function settings(value:string): Settings {
  if (value.length > 16 * 1024) throw new Error('File input configuration is too large.');
  const result = JSON.parse(value);
  if (!result || result.version !== 1 || !validString(result.inputID, 1024) || !result.inputID || /\s/.test(result.inputID)
      || !validString(result.owner, 128) || !Number.isInteger(result.revision) || result.revision < 0 || result.revision > 2_147_483_647
      || !Array.isArray(result.tokens) || result.tokens.length > maxFiles || result.tokens.some((token:unknown) => !validString(token, 128) || !token)
      || new Set(result.tokens).size !== result.tokens.length
      || !validString(result.error, 4096) || result.error && result.tokens.length
      || !Number.isInteger(result.maxFiles) || result.maxFiles < 1 || result.maxFiles > maxFiles
      || !Number.isInteger(result.maxFileBytes) || result.maxFileBytes < 1 || result.maxFileBytes > maxFileBytes) {
    throw new Error('Invalid native file input configuration.');
  }
  return result;
}

function nativeInput(element:HTMLElement, id:string): HTMLInputElement {
  const field = element.previousElementSibling;
  const matches = field ? [field, ...field.querySelectorAll('input')].filter(node => node.id === id && node.localName === 'input') : [];
  if (matches.length !== 1 || (matches[0] as HTMLInputElement).type !== 'file') throw new Error('A file enhancement requires one matching input[type=file] in the preceding field.');
  const input = matches[0] as HTMLInputElement;
  if (input.hasAttribute('value')) throw new Error('File inputs cannot bind a string value.');
  return input;
}

function readBlob(blob:Blob, signal:AbortSignal): Promise<Uint8Array> {
  signal.throwIfAborted();
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    let settled = false;
    const finish = (error?:unknown) => {
      if (settled) return;
      settled = true;
      signal.removeEventListener('abort', abort);
      reader.onload = reader.onerror = reader.onabort = null;
      if (error) reject(error); else resolve(new Uint8Array(reader.result as ArrayBuffer));
    };
    const abort = () => { reader.abort(); finish(signal.reason ?? new Error('File reading was cancelled.')); };
    reader.onload = () => finish();
    reader.onerror = () => finish(reader.error ?? new Error('The file could not be read.'));
    reader.onabort = () => finish(signal.reason ?? new Error('File reading was cancelled.'));
    signal.addEventListener('abort', abort, {once:true});
    try { reader.readAsArrayBuffer(blob); } catch (error) { finish(error); }
  });
}

function encodeBytes(bytes:Uint8Array): string {
  const parts:string[] = [];
  for (let offset = 0; offset < bytes.length; offset += 8192) parts.push(String.fromCharCode(...bytes.subarray(offset, offset + 8192)));
  return btoa(parts.join(''));
}

/** Native File objects stay below the UI ABI, owned by one root. */
export class FileHost {
  private prefix = '';
  private readonly held = new Map<string, HeldFile>();
  private readonly owners = new Set<Owner>();
  private sequence = 0;
  private retainedBytes = 0;
  private closed = false;

  readonly widgets:WidgetProviders = {'native-files':context => this.mount(context)};
  readonly tasks:TaskProviders = {
    'ui.files.text':(value, signal) => this.read(value, signal, true),
    'ui.files.bytes':(value, signal) => this.read(value, signal, false),
    'web.multipart':(value, signal) => this.multipart(value, signal),
  };

  private identity(): string {
    if (this.closed || this.sequence === Number.MAX_SAFE_INTEGER) throw new Error('This file root is closed or exhausted.');
    // getRandomValues also works on ordinary HTTP origins. Generate a root
    // prefix only when its first file input mounts; unrelated roots pay no cost.
    this.prefix ||= Array.from(crypto.getRandomValues(new Uint32Array(4)), value => value.toString(16).padStart(8, '0')).join('');
    return `${this.prefix}:${++this.sequence}`;
  }

  private forget(token:string): void {
    const record = this.held.get(token);
    if (record) { this.retainedBytes -= record.file.size; this.held.delete(token); }
  }

  private retire(owner:Owner): void {
    if (owner.retire !== undefined || this.closed) return;
    // TaskHost schedules providers as microtasks after the DOM commit. Defer
    // retirement to the end of this turn so those requests capture their Blob,
    // including a read started in the same commit that removes its input.
    owner.retire = setTimeout(() => {
      owner.retire = undefined;
      for (const [token, file] of this.held) {
        if (file.owner === owner && (owner.closed || file.revision <= owner.acknowledged && !owner.keep.has(token))) this.forget(token);
      }
      if (owner.closed) this.owners.delete(owner);
    }, 0);
  }

  private async read(value:string, signal:AbortSignal, text:boolean): Promise<string> {
    signal.throwIfAborted();
    if (this.closed || value.length > 1024) throw new Error('The file request is unavailable.');
    const request = JSON.parse(value);
    if (!request || request.version !== 1 || !validString(request.token, 128) || !request.token
        || !Number.isSafeInteger(request.offset) || request.offset < 0 || !Number.isSafeInteger(request.length) || request.length < 0
        || (text ? request.offset !== 0 || request.length !== 0 : request.length > maxReadBytes)) throw new Error('Invalid bounded file read.');
    const held = this.held.get(request.token);
    if (!held) throw new Error('This file selection is no longer available. Select the file again.');
    const file = held.file; // Captured before the first asynchronous operation.
    if (text && file.size > maxTextBytes) throw new Error('Text reading is limited to 1 MiB; read byte slices for larger files.');
    const start = Math.min(request.offset, file.size);
    const bytes = await readBlob(text ? file : file.slice(start, start + request.length), signal);
    signal.throwIfAborted();
    return text ? new TextDecoder('utf-8', {fatal:true, ignoreBOM:true}).decode(bytes) : encodeBytes(bytes);
  }

  private async multipart(value:string, signal:AbortSignal): Promise<string> {
    signal.throwIfAborted();
    if (this.closed || value.length > 8 * 1024 * 1024) throw new Error('The multipart request is unavailable or too large.');
    const payload = JSON.parse(value);
    if (!payload || payload.version !== 1 || typeof payload.request !== 'string' || payload.request.length > 512 * 1024
        || !Array.isArray(payload.fields) || payload.fields.length > 4096) throw new Error('Invalid multipart request.');
    const request = decodeHTTPRequest(payload.request);
    if (!['POST', 'PUT', 'PATCH'].includes(request.method) || request.body !== '' || request.headers.has('content-type')) {
      throw new Error('Multipart requires POST, PUT or PATCH, an empty text body, and a browser-generated content type.');
    }
    const form = new FormData();
    let textBytes = 0, fileBytes = 0, count = 0;
    for (const field of payload.fields) {
      if (!field || !validString(field.name, 1024) || !field.name || /[\r\n\0]/.test(field.name)
          || typeof field.value !== 'string' || typeof field.token !== 'string' || field.token && field.value) throw new Error('Invalid multipart field.');
      textBytes += encoder.encode(field.name).length;
      if (field.token) {
        const held = this.held.get(field.token);
        if (!held) throw new Error('This file selection is no longer available. Select the file again.');
        fileBytes += held.file.size;
        if (++count > maxFiles || fileBytes > maxFileBytes) throw new Error('An upload is limited to 32 files and 128 MiB in total.');
        // FormData captures the native Blob before any await. No file contents
        // are copied into the guest model, UI wire or JSON request envelope.
        form.append(field.name, held.file);
      } else {
        if (!validString(field.value, 1024 * 1024)) throw new Error('Invalid multipart text value.');
        textBytes += encoder.encode(field.value).length;
        form.append(field.name, field.value);
      }
      if (textBytes > 1024 * 1024) throw new Error('Multipart text fields exceed 1 MiB.');
    }
    return fetchHTTPBody(request, form, signal);
  }

  private mount(context:WidgetContext): WidgetInstance {
    let options = settings(context.value);
    const input = nativeInput(context.element, options.inputID);
    const owner:Owner = {key:this.identity(), revision:0, acknowledged:0, keep:new Set(), closed:false};
    this.owners.add(owner);
    let previous:File[] = [], tokens:string[] = [], currentError = '', validity = '', replacedOwner = '';
    let selectionLimit = input.multiple ? options.maxFiles : 1;
    let limitsChanged = false;
    const publish = (files:File[], error = '') => {
      if (owner.revision === 2_147_483_647) throw new Error('File input revisions are exhausted; replace this field.');
      owner.revision++;
      previous = files;
      if (!error) {
        const bytes = files.reduce((sum, file) => sum + file.size, 0);
        if (files.length > selectionLimit) error = `Select at most ${selectionLimit} files.`;
        else if (files.some(file => !Number.isSafeInteger(file.size) || file.size < 0 || file.size > options.maxFileBytes)) error = `Each file must be within ${options.maxFileBytes} bytes.`;
        else if (files.some(file => !validString(file.name, 4096) || !validString(file.type, 1024) || !Number.isSafeInteger(file.lastModified))) error = 'A selected file has unsupported metadata.';
        else if (this.held.size + files.length > maxRetainedFiles || this.retainedBytes + bytes > maxRetainedBytes) error = 'Too many file references are still in use. Clear a selection before adding more files.';
      }
      tokens = [];
      if (!error) for (const file of files) {
        const token = this.identity();
        this.held.set(token, {file, owner, revision:owner.revision}); this.retainedBytes += file.size;
        tokens.push(token);
      }
      currentError = error;
      if (error || input.validationMessage === validity) input.setCustomValidity(error);
      validity = error;
      const snapshot = {version:1, owner:owner.key, revision:owner.revision, error,
        files:tokens.map((token, index) => ({token, name:files[index].name, size:files[index].size,
          mediaType:files[index].type, lastModified:files[index].lastModified}))};
      const encoded = JSON.stringify(snapshot);
      if (encoder.encode(encoded).length > 128 * 1024) {
        for (const token of tokens) this.forget(token);
        tokens = []; currentError = 'The selected file metadata is too large.';
        input.setCustomValidity(currentError); validity = currentError;
        context.emit(JSON.stringify({...snapshot, files:[], error:currentError}));
      } else context.emit(encoded);
    };
    const capture = () => {
      if (owner.closed || context.signal.aborted) return;
      try {
        const files = [...input.files ?? []];
        if (!limitsChanged && files.length === previous.length && files.every((file, index) => file === previous[index])) return;
        limitsChanged = false;
        publish(files);
      } catch (error) { context.fail(String((error as Error)?.message ?? error)); }
    };
    const reset = (event:Event) => {
      if (event.target !== input.form) return;
      queueMicrotask(() => { if (!event.defaultPrevented) capture(); });
    };
    // Capture native reset/autofill/programmatic FileList changes before the
    // managed form's submit listener queues its snapshot request.
    const submit = (event:Event) => { if (event.target === input.form) capture(); };
    input.addEventListener('change', capture);
    input.ownerDocument.addEventListener('reset', reset, true);
    input.ownerDocument.addEventListener('submit', submit, true);
    const commit = () => {
      if (owner.closed) return;
      if (nativeInput(context.element, options.inputID) !== input) throw new Error('Replace the file enhancement key together with its native input.');
      const nextLimit = input.multiple ? options.maxFiles : 1;
      limitsChanged ||= selectionLimit !== nextLimit;
      selectionLimit = nextLimit;
      if (options.owner && options.owner !== owner.key && options.tokens.length && options.owner !== replacedOwner) {
        replacedOwner = options.owner;
        input.value = '';
        publish([], 'Choose files again after this input was replaced.'); return;
      }
      capture();
      if (options.owner !== owner.key) return;
      if (options.revision > owner.revision) throw new Error('A file selection cannot acknowledge a future revision.');
      if (options.revision >= owner.acknowledged) {
        owner.acknowledged = options.revision;
        owner.keep = new Set(options.tokens);
      }
      if (options.revision === owner.revision) {
        if (!options.tokens.length && !options.error && (tokens.length || currentError)) {
          input.value = ''; publish([]);
        } else if (options.error !== currentError || tokens.length !== options.tokens.length || tokens.some((token, index) => token !== options.tokens[index])) {
          throw new Error('Native file selections can be retained or cleared, not assigned from another selection.');
        }
      }
      this.retire(owner);
    };
    return {
      update:value => {
        const next = settings(value);
        if (next.inputID !== options.inputID) throw new Error('Replace the file enhancement key when its input ID changes.');
        limitsChanged ||= next.maxFiles !== options.maxFiles || next.maxFileBytes !== options.maxFileBytes;
        options = next;
      },
      afterCommit:commit,
      // Another input in the same field can acquire this input's ID. Observe
      // those attributes too, preserving the unique native-control contract.
      commitTargets:() => [input, context.element, ...(context.element.previousElementSibling?.querySelectorAll('input') ?? [])],
      dispose:() => {
        if (owner.closed) return;
        owner.closed = true;
        input.removeEventListener('change', capture);
        input.ownerDocument.removeEventListener('reset', reset, true);
        input.ownerDocument.removeEventListener('submit', submit, true);
        if (validity && input.validationMessage === validity) input.setCustomValidity('');
        this.retire(owner);
      },
    };
  }

  close(): void {
    if (this.closed) return;
    this.closed = true;
    for (const owner of this.owners) { clearTimeout(owner.retire); owner.closed = true; }
    this.owners.clear(); this.held.clear(); this.retainedBytes = 0;
  }
}
