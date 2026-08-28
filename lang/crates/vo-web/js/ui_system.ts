const VUS_MAGIC = Object.freeze([0x56, 0x55, 0x53, 0x31]);
const VUS_REQUEST = 1;
const VUS_RESPONSE = 2;
const MAX_TEXT_BYTES = 16 * 1024 * 1024;
const MAX_IMAGE_PIXELS = 16_777_216;
const MAX_PATHS = 1024;
const MAX_PATH_BYTES = 32 * 1024;
const MAX_FILTERS = 64;
const MAX_EXTENSIONS = 64;
const MAX_MENU_ITEMS = 4096;
const MAX_MENU_DEPTH = 16;

export type UiClipboardContent =
  | { readonly type: 'text'; readonly text: string }
  | { readonly type: 'html'; readonly html: string; readonly plainText: string }
  | { readonly type: 'rgba8'; readonly width: number; readonly height: number; readonly pixels: Uint8Array };

export interface UiFileDialogFilter {
  readonly name: string;
  readonly extensions: readonly string[];
}

export interface UiFileDialogRequest {
  readonly kind: 1 | 2 | 3 | 4 | 5;
  readonly title: string;
  readonly initialDirectory?: string;
  readonly initialFileName?: string;
  readonly canCreateDirectories: boolean;
  readonly filters: readonly UiFileDialogFilter[];
}

export interface UiMenuNode {
  readonly kind: 1 | 2 | 3 | 4;
  readonly index: number;
  readonly generation: number;
  readonly label?: string;
  readonly enabled?: boolean;
  readonly checked?: boolean;
  readonly shortcut?: string;
  readonly children?: readonly UiMenuNode[];
}

export type UiSystemRequest =
  | { readonly requestId: bigint; readonly type: 'read-clipboard'; readonly format: 1 | 2 | 3 }
  | { readonly requestId: bigint; readonly type: 'write-clipboard'; readonly content: UiClipboardContent }
  | { readonly requestId: bigint; readonly type: 'file-dialog'; readonly request: UiFileDialogRequest }
  | { readonly requestId: bigint; readonly type: 'message-dialog'; readonly level: 1 | 2 | 3; readonly buttons: 1 | 2 | 3 | 4; readonly title: string; readonly description: string }
  | { readonly requestId: bigint; readonly type: 'install-menu'; readonly revision: bigint; readonly roots: readonly UiMenuNode[] }
  | { readonly requestId: bigint; readonly type: 'wait-event' }
  | { readonly requestId: bigint; readonly type: 'begin-file-drag'; readonly mode: 1 | 2; readonly paths: readonly string[]; readonly preview?: string }
  | { readonly requestId: bigint; readonly type: 'invoke-host'; readonly service: string; readonly operation: string; readonly payload: Uint8Array };

export type UiSystemEvent =
  | { readonly type: 'menu'; readonly sequence: bigint; readonly index: number; readonly generation: number }
  | { readonly type: 'drag-drop'; readonly sequence: bigint; readonly phase: 1 | 2 | 3 | 4; readonly x: number; readonly y: number; readonly paths: readonly string[] };

export type UiSystemFailureKind = 1 | 2 | 3 | 4;

export type UiSystemResponse =
  | { readonly type: 'complete' }
  | { readonly type: 'clipboard'; readonly content?: UiClipboardContent }
  | { readonly type: 'file-dialog'; readonly paths: readonly string[] }
  | { readonly type: 'message-dialog'; readonly result: 1 | 2 | 3 | 4 }
  | { readonly type: 'menu-installed'; readonly revision: bigint }
  | { readonly type: 'event'; readonly event: UiSystemEvent }
  | { readonly type: 'host-payload'; readonly payload: Uint8Array }
  | { readonly type: 'failure'; readonly kind: UiSystemFailureKind; readonly message: string };

export interface UiSystemHost {
  execute(frame: Uint8Array): Promise<Uint8Array>;
  reset?(): void;
  dispose?(): void;
}

class Reader {
  private readonly view: DataView;
  private position = 0;

  constructor(private readonly bytes: Uint8Array) {
    this.view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  }

  get remaining(): number { return this.bytes.byteLength - this.position; }

  finish(): void {
    if (this.remaining !== 0) throw new Error('VUS1 frame has trailing bytes');
  }

  u8(): number {
    this.require(1);
    return this.view.getUint8(this.position++);
  }

  bool(): boolean {
    const value = this.u8();
    if (value > 1) throw new Error('VUS1 boolean tag is invalid');
    return value === 1;
  }

  u16(): number {
    this.require(2);
    const value = this.view.getUint16(this.position, true);
    this.position += 2;
    return value;
  }

  u32(): number {
    this.require(4);
    const value = this.view.getUint32(this.position, true);
    this.position += 4;
    return value;
  }

  u64(): bigint {
    this.require(8);
    const value = this.view.getBigUint64(this.position, true);
    this.position += 8;
    return value;
  }

  f64(): number {
    this.require(8);
    const value = this.view.getFloat64(this.position, true);
    this.position += 8;
    if (!Number.isFinite(value)) throw new Error('VUS1 coordinate must be finite');
    return value;
  }

  data(max: number): Uint8Array {
    const length = this.length(max);
    this.require(length);
    const value = this.bytes.slice(this.position, this.position + length);
    this.position += length;
    return value;
  }

  text(max = MAX_TEXT_BYTES): string {
    return new TextDecoder('utf-8', { fatal: true }).decode(this.data(max));
  }

  optionalText(max: number): string | undefined {
    const present = this.u8();
    if (present === 0) return undefined;
    if (present !== 1) throw new Error('VUS1 optional string tag is invalid');
    return this.text(max);
  }

  texts(maxCount: number, maxBytes: number): string[] {
    const count = this.length(maxCount);
    return Array.from({ length: count }, () => this.text(maxBytes));
  }

  length(max: number): number {
    const value = this.u32();
    if (value > max) throw new Error('VUS1 bounded length exceeds its contract');
    return value;
  }

  private require(length: number): void {
    if (!Number.isSafeInteger(length) || length < 0 || this.position + length > this.bytes.byteLength) {
      throw new Error('VUS1 frame is truncated');
    }
  }
}

class Writer {
  private readonly bytes: number[] = [];
  private readonly encoder = new TextEncoder();

  constructor(frame: number, tag: number, requestId: bigint) {
    this.bytes.push(...VUS_MAGIC, frame, tag, 0, 0);
    this.u64(requestId);
  }

  finish(): Uint8Array { return Uint8Array.from(this.bytes); }
  u8(value: number): void { this.bytes.push(value & 0xff); }
  u32(value: number): void {
    if (!Number.isSafeInteger(value) || value < 0 || value > 0xffff_ffff) {
      throw new Error('VUS1 u32 value is invalid');
    }
    for (let shift = 0; shift < 32; shift += 8) this.bytes.push((value >>> shift) & 0xff);
  }
  u64(value: bigint): void {
    if (value < 0n || value > 0xffff_ffff_ffff_ffffn) throw new Error('VUS1 u64 value is invalid');
    for (let shift = 0n; shift < 64n; shift += 8n) this.bytes.push(Number((value >> shift) & 0xffn));
  }
  f64(value: number): void {
    if (!Number.isFinite(value)) throw new Error('VUS1 coordinate must be finite');
    const bytes = new Uint8Array(8);
    new DataView(bytes.buffer).setFloat64(0, value, true);
    this.bytes.push(...bytes);
  }
  data(value: Uint8Array): void { this.u32(value.byteLength); this.bytes.push(...value); }
  text(value: string): void { this.data(this.encoder.encode(value)); }
  texts(values: readonly string[]): void { this.u32(values.length); for (const value of values) this.text(value); }
}

function requestHeader(frame: Uint8Array): { reader: Reader; tag: number; requestId: bigint } {
  const reader = new Reader(frame);
  for (const expected of VUS_MAGIC) {
    if (reader.u8() !== expected) throw new Error('VUS1 request magic is invalid');
  }
  if (reader.u8() !== VUS_REQUEST) throw new Error('VUS1 frame is not a request');
  const tag = reader.u8();
  if (reader.u16() !== 0) throw new Error('VUS1 reserved request bits are non-zero');
  const requestId = reader.u64();
  if (requestId === 0n) throw new Error('VUS1 request identity is invalid');
  return { reader, tag, requestId };
}

function clipboard(reader: Reader): UiClipboardContent {
  switch (reader.u8()) {
    case 1: return { type: 'text', text: reader.text() };
    case 2: return { type: 'html', html: reader.text(), plainText: reader.text() };
    case 3: {
      const width = reader.u32();
      const height = reader.u32();
      if (width === 0 || height === 0 || width * height > MAX_IMAGE_PIXELS) {
        throw new Error('VUS1 clipboard image dimensions are invalid');
      }
      const pixels = reader.data(MAX_IMAGE_PIXELS * 4);
      if (pixels.byteLength !== width * height * 4) throw new Error('VUS1 clipboard image storage is invalid');
      return { type: 'rgba8', width, height, pixels };
    }
    default: throw new Error('VUS1 clipboard content tag is invalid');
  }
}

function fileDialog(reader: Reader): UiFileDialogRequest {
  const kind = reader.u8();
  if (kind < 1 || kind > 5) throw new Error('VUS1 file dialog kind is invalid');
  const title = reader.text();
  const initialDirectory = reader.optionalText(MAX_PATH_BYTES);
  const initialFileName = reader.optionalText(MAX_PATH_BYTES);
  const canCreateDirectories = reader.bool();
  const count = reader.length(MAX_FILTERS);
  const filters: UiFileDialogFilter[] = [];
  for (let index = 0; index < count; index += 1) {
    const name = reader.text();
    const extensions = reader.texts(MAX_EXTENSIONS, MAX_PATH_BYTES);
    if (name.length === 0 || extensions.length === 0) throw new Error('VUS1 file filter is empty');
    filters.push({ name, extensions });
  }
  return { kind: kind as UiFileDialogRequest['kind'], title, initialDirectory, initialFileName, canCreateDirectories, filters };
}

function menuNode(reader: Reader, depth: number, count: { value: number }): UiMenuNode {
  if (depth > MAX_MENU_DEPTH || ++count.value > MAX_MENU_ITEMS) throw new Error('VUS1 menu limits exceeded');
  const kind = reader.u8();
  if (kind < 1 || kind > 4) throw new Error('VUS1 menu item kind is invalid');
  const typedKind = kind as UiMenuNode['kind'];
  const index = reader.u32();
  const generation = reader.u32();
  if (generation === 0) throw new Error('VUS1 menu item identity is invalid');
  if (typedKind === 4) return { kind: typedKind, index, generation };
  const label = reader.text();
  const enabled = reader.bool();
  if (label.length === 0) throw new Error('VUS1 menu label is empty');
  if (typedKind === 3) {
    const childCount = reader.length(MAX_MENU_ITEMS - count.value);
    const children = Array.from({ length: childCount }, () => menuNode(reader, depth + 1, count));
    return { kind: typedKind, index, generation, label, enabled, children };
  }
  const checked = typedKind === 2 ? reader.bool() : undefined;
  const shortcut = reader.optionalText(MAX_TEXT_BYTES);
  return { kind: typedKind, index, generation, label, enabled, checked, shortcut };
}

export function decodeUiSystemRequest(frame: Uint8Array): UiSystemRequest {
  const { reader, tag, requestId } = requestHeader(frame);
  let request: UiSystemRequest;
  switch (tag) {
    case 1: {
      const format = reader.u8();
      if (format < 1 || format > 3) throw new Error('VUS1 clipboard format is invalid');
      request = { requestId, type: 'read-clipboard', format: format as 1 | 2 | 3 };
      break;
    }
    case 2: request = { requestId, type: 'write-clipboard', content: clipboard(reader) }; break;
    case 3: request = { requestId, type: 'file-dialog', request: fileDialog(reader) }; break;
    case 4: {
      const level = reader.u8();
      const buttons = reader.u8();
      if (level < 1 || level > 3 || buttons < 1 || buttons > 4) {
        throw new Error('VUS1 message dialog option is invalid');
      }
      request = {
        requestId,
        type: 'message-dialog',
        level: level as 1 | 2 | 3,
        buttons: buttons as 1 | 2 | 3 | 4,
        title: reader.text(),
        description: reader.text(),
      };
      break;
    }
    case 5: {
      const revision = reader.u64();
      if (revision === 0n) throw new Error('VUS1 menu revision is invalid');
      const roots = reader.length(MAX_MENU_ITEMS);
      const count = { value: 0 };
      request = {
        requestId,
        type: 'install-menu',
        revision,
        roots: Array.from({ length: roots }, () => menuNode(reader, 1, count)),
      };
      break;
    }
    case 6: request = { requestId, type: 'wait-event' }; break;
    case 7: {
      const mode = reader.u8();
      const paths = reader.texts(MAX_PATHS, MAX_PATH_BYTES);
      const preview = reader.optionalText(MAX_PATH_BYTES);
      if ((mode !== 1 && mode !== 2) || paths.length === 0) {
        throw new Error('VUS1 file drag request is invalid');
      }
      request = { requestId, type: 'begin-file-drag', mode, paths, preview };
      break;
    }
    case 8: request = {
      requestId, type: 'invoke-host', service: reader.text(255), operation: reader.text(255),
      payload: reader.data(MAX_TEXT_BYTES),
    }; break;
    default: throw new Error('VUS1 request tag is invalid');
  }
  reader.finish();
  return request;
}

function writeClipboard(writer: Writer, content: UiClipboardContent): void {
  switch (content.type) {
    case 'text': writer.u8(1); writer.text(content.text); break;
    case 'html': writer.u8(2); writer.text(content.html); writer.text(content.plainText); break;
    case 'rgba8':
      if (content.width * content.height * 4 !== content.pixels.byteLength) {
        throw new Error('VUS1 clipboard response image storage is invalid');
      }
      writer.u8(3); writer.u32(content.width); writer.u32(content.height); writer.data(content.pixels); break;
  }
}

function writeOptionalText(writer: Writer, value: string | undefined): void {
  writer.u8(value === undefined ? 0 : 1);
  if (value !== undefined) writer.text(value);
}

function writeMenuNode(writer: Writer, node: UiMenuNode): void {
  writer.u8(node.kind);
  writer.u32(node.index);
  writer.u32(node.generation);
  if (node.kind === 4) return;
  writer.text(node.label ?? '');
  writer.u8(node.enabled === true ? 1 : 0);
  if (node.kind === 3) {
    const children = node.children ?? [];
    writer.u32(children.length);
    for (const child of children) writeMenuNode(writer, child);
    return;
  }
  if (node.kind === 2) writer.u8(node.checked === true ? 1 : 0);
  writeOptionalText(writer, node.shortcut);
}

export function encodeUiSystemRequest(request: UiSystemRequest): Uint8Array {
  let tag: number;
  switch (request.type) {
    case 'read-clipboard': tag = 1; break;
    case 'write-clipboard': tag = 2; break;
    case 'file-dialog': tag = 3; break;
    case 'message-dialog': tag = 4; break;
    case 'install-menu': tag = 5; break;
    case 'wait-event': tag = 6; break;
    case 'begin-file-drag': tag = 7; break;
    case 'invoke-host': tag = 8; break;
  }
  const writer = new Writer(VUS_REQUEST, tag, request.requestId);
  switch (request.type) {
    case 'read-clipboard': writer.u8(request.format); break;
    case 'write-clipboard': writeClipboard(writer, request.content); break;
    case 'file-dialog':
      writer.u8(request.request.kind);
      writer.text(request.request.title);
      writeOptionalText(writer, request.request.initialDirectory);
      writeOptionalText(writer, request.request.initialFileName);
      writer.u8(request.request.canCreateDirectories ? 1 : 0);
      writer.u32(request.request.filters.length);
      for (const filter of request.request.filters) {
        writer.text(filter.name);
        writer.texts(filter.extensions);
      }
      break;
    case 'message-dialog':
      writer.u8(request.level); writer.u8(request.buttons);
      writer.text(request.title); writer.text(request.description);
      break;
    case 'install-menu':
      writer.u64(request.revision); writer.u32(request.roots.length);
      for (const root of request.roots) writeMenuNode(writer, root);
      break;
    case 'wait-event': break;
    case 'begin-file-drag':
      if (request.paths.length === 0 || request.paths.length > MAX_PATHS) {
        throw new Error('VUS1 file drag path count is invalid');
      }
      writer.u8(request.mode);
      writer.texts(request.paths);
      writeOptionalText(writer, request.preview);
      break;
    case 'invoke-host':
      writer.text(request.service); writer.text(request.operation); writer.data(request.payload);
      break;
  }
  return writer.finish();
}

export function encodeUiSystemResponse(requestId: bigint, response: UiSystemResponse): Uint8Array {
  let tag: number;
  switch (response.type) {
    case 'complete': tag = 1; break;
    case 'clipboard': tag = 2; break;
    case 'file-dialog': tag = 3; break;
    case 'message-dialog': tag = 4; break;
    case 'menu-installed': tag = 5; break;
    case 'event': tag = 6; break;
    case 'host-payload': tag = 7; break;
    case 'failure': tag = 0x80; break;
  }
  const writer = new Writer(VUS_RESPONSE, tag, requestId);
  switch (response.type) {
    case 'complete': break;
    case 'clipboard':
      writer.u8(response.content === undefined ? 0 : 1);
      if (response.content !== undefined) writeClipboard(writer, response.content);
      break;
    case 'file-dialog': writer.texts(response.paths); break;
    case 'message-dialog': writer.u8(response.result); break;
    case 'menu-installed': writer.u64(response.revision); break;
    case 'event':
      if (response.event.type === 'menu') {
        writer.u8(1); writer.u64(response.event.sequence);
        writer.u32(response.event.index); writer.u32(response.event.generation);
      } else {
        writer.u8(2); writer.u64(response.event.sequence); writer.u8(response.event.phase);
        writer.f64(response.event.x); writer.f64(response.event.y); writer.texts(response.event.paths);
      }
      break;
    case 'host-payload': writer.data(response.payload); break;
    case 'failure': writer.u8(response.kind); writer.text(response.message); break;
  }
  return writer.finish();
}

export function decodeUiSystemResponse(frame: Uint8Array): { requestId: bigint; response: UiSystemResponse } {
  const reader = new Reader(frame);
  for (const expected of VUS_MAGIC) {
    if (reader.u8() !== expected) throw new Error('VUS1 response magic is invalid');
  }
  if (reader.u8() !== VUS_RESPONSE) throw new Error('VUS1 frame is not a response');
  const tag = reader.u8();
  if (reader.u16() !== 0) throw new Error('VUS1 reserved response bits are non-zero');
  const requestId = reader.u64();
  if (requestId === 0n) throw new Error('VUS1 response identity is invalid');
  let response: UiSystemResponse;
  switch (tag) {
    case 1: response = { type: 'complete' }; break;
    case 2: {
      const present = reader.u8();
      if (present > 1) throw new Error('VUS1 clipboard presence tag is invalid');
      response = { type: 'clipboard', content: present === 1 ? clipboard(reader) : undefined };
      break;
    }
    case 3: response = { type: 'file-dialog', paths: reader.texts(MAX_PATHS, MAX_PATH_BYTES) }; break;
    case 4: {
      const result = reader.u8();
      if (result < 1 || result > 4) throw new Error('VUS1 message result is invalid');
      response = { type: 'message-dialog', result: result as 1 | 2 | 3 | 4 };
      break;
    }
    case 5: {
      const revision = reader.u64();
      if (revision === 0n) throw new Error('VUS1 installed menu revision is invalid');
      response = { type: 'menu-installed', revision };
      break;
    }
    case 6: {
      const eventTag = reader.u8();
      if (eventTag === 1) {
        const sequence = reader.u64();
        const index = reader.u32();
        const generation = reader.u32();
        if (sequence === 0n || generation === 0) throw new Error('VUS1 menu event identity is invalid');
        response = { type: 'event', event: { type: 'menu', sequence, index, generation } };
      } else if (eventTag === 2) {
        const sequence = reader.u64();
        const phase = reader.u8();
        if (sequence === 0n || phase < 1 || phase > 4) throw new Error('VUS1 drag event identity is invalid');
        response = {
          type: 'event',
          event: {
            type: 'drag-drop', sequence, phase: phase as 1 | 2 | 3 | 4,
            x: reader.f64(), y: reader.f64(), paths: reader.texts(MAX_PATHS, MAX_PATH_BYTES),
          },
        };
      } else throw new Error('VUS1 event tag is invalid');
      break;
    }
    case 7: response = { type: 'host-payload', payload: reader.data(MAX_TEXT_BYTES) }; break;
    case 0x80: {
      const kind = reader.u8();
      if (kind < 1 || kind > 4) throw new Error('VUS1 failure kind is invalid');
      response = { type: 'failure', kind: kind as UiSystemFailureKind, message: reader.text() };
      break;
    }
    default: throw new Error('VUS1 response tag is invalid');
  }
  reader.finish();
  return { requestId, response };
}

interface BrowserFileHandle { readonly kind?: 'file'; readonly name: string }
interface BrowserDirectoryHandle {
  readonly kind?: 'directory';
  readonly name: string;
  entries?: () => AsyncIterableIterator<[string, BrowserFileHandle | BrowserDirectoryHandle]>;
}
interface FilePickerType { readonly description?: string; readonly accept: Record<string, readonly string[]> }
interface PickerWindow extends Window {
  showOpenFilePicker?: (options?: { multiple?: boolean; types?: readonly FilePickerType[] }) => Promise<BrowserFileHandle[]>;
  showSaveFilePicker?: (options?: { suggestedName?: string; types?: readonly FilePickerType[] }) => Promise<BrowserFileHandle>;
  showDirectoryPicker?: () => Promise<BrowserDirectoryHandle>;
}

const MAX_BROWSER_FILE_HANDLES = 256;
const browserFileHandles = new Map<string, BrowserFileHandle | BrowserDirectoryHandle>();
let nextBrowserFileHandle = 1;

function retainBrowserFileHandle(handle: BrowserFileHandle | BrowserDirectoryHandle): string {
  const random = new Uint32Array(2);
  globalThis.crypto?.getRandomValues(random);
  const token = `volang-browser-handle://${nextBrowserFileHandle++}-${random[0].toString(16)}${random[1].toString(16)}`;
  browserFileHandles.set(token, handle);
  while (browserFileHandles.size > MAX_BROWSER_FILE_HANDLES) {
    const oldest = browserFileHandles.keys().next().value as string | undefined;
    if (oldest === undefined) break;
    browserFileHandles.delete(oldest);
  }
  return token;
}

/** Resolve a browser picker capability inside the same page module graph. */
export function resolveBrowserFileHandle(token: string): BrowserFileHandle | BrowserDirectoryHandle | undefined {
  return browserFileHandles.get(token);
}

/** Release a picker capability after an application host has consumed it. */
export function releaseBrowserFileHandle(token: string): void {
  browserFileHandles.delete(token);
}

export interface UiBrowserSystemHostOptions {
  readonly maxPendingEvents?: number;
  readonly invokeHost?: (service: string, operation: string, payload: Uint8Array) => Promise<Uint8Array>;
}

export class UiBrowserSystemHost implements UiSystemHost {
  private readonly pendingEvents: UiSystemEvent[] = [];
  private readonly eventWaiters: Array<{
    readonly resolve: (event: UiSystemEvent) => void;
    readonly reject: (cause: Error) => void;
  }> = [];
  private readonly listeners: Array<readonly [string, EventListener]> = [];
  private readonly maxPendingEvents: number;
  private sequence = 1n;
  private disposed = false;

  constructor(private readonly root: HTMLElement, private readonly options: UiBrowserSystemHostOptions = {}) {
    this.maxPendingEvents = options.maxPendingEvents ?? 4096;
    if (!Number.isSafeInteger(this.maxPendingEvents) || this.maxPendingEvents <= 0) {
      throw new Error('browser UI system event limit is invalid');
    }
    this.listen('dragenter', (event) => this.drag(event as DragEvent, 1));
    this.listen('dragover', (event) => this.drag(event as DragEvent, 2));
    this.listen('dragleave', (event) => this.drag(event as DragEvent, 3));
    this.listen('drop', (event) => this.drag(event as DragEvent, 4));
  }

  async execute(frame: Uint8Array): Promise<Uint8Array> {
    const request = decodeUiSystemRequest(frame);
    try {
      const response = await this.executeRequest(request);
      return encodeUiSystemResponse(request.requestId, response);
    } catch (cause) {
      const failure = browserFailure(cause);
      return encodeUiSystemResponse(request.requestId, { type: 'failure', ...failure });
    }
  }

  dispose(): void {
    if (this.disposed) return;
    this.disposed = true;
    for (const [type, listener] of this.listeners) this.root.removeEventListener(type, listener);
    this.listeners.length = 0;
    this.reset();
  }

  reset(): void {
    this.pendingEvents.length = 0;
    const error = new Error('browser UI system session was reset');
    for (const waiter of this.eventWaiters.splice(0)) waiter.reject(error);
  }

  async executeRequest(request: UiSystemRequest): Promise<UiSystemResponse> {
    if (this.disposed) throw new Error('browser UI system host has been disposed');
    switch (request.type) {
      case 'read-clipboard': return { type: 'clipboard', content: await this.readClipboard(request.format) };
      case 'write-clipboard': await this.writeClipboard(request.content); return { type: 'complete' };
      case 'file-dialog': return { type: 'file-dialog', paths: await this.fileDialog(request.request) };
      case 'message-dialog': return { type: 'message-dialog', result: this.messageDialog(request) };
      case 'install-menu': throw new UiSystemUnsupportedError('native application menus are unavailable in browsers');
      case 'wait-event': return { type: 'event', event: await this.nextEvent() };
      case 'begin-file-drag': throw new UiSystemUnsupportedError('native file drag sources are unavailable in browsers');
      case 'invoke-host': {
        if (this.options.invokeHost === undefined) throw new UiSystemUnsupportedError('application host invocation is unavailable');
        return { type: 'host-payload', payload: await this.options.invokeHost(request.service, request.operation, request.payload) };
      }
    }
  }

  private async readClipboard(format: 1 | 2 | 3): Promise<UiClipboardContent | undefined> {
    const clipboard = this.root.ownerDocument.defaultView?.navigator.clipboard;
    if (clipboard === undefined) throw new UiSystemUnsupportedError('Clipboard API is unavailable');
    if (format === 1) {
      const text = await clipboard.readText();
      return { type: 'text', text };
    }
    if (clipboard.read === undefined) throw new UiSystemUnsupportedError('rich Clipboard API is unavailable');
    for (const item of await clipboard.read()) {
      if (format === 2 && item.types.includes('text/html')) {
        const html = await (await item.getType('text/html')).text();
        const plainText = item.types.includes('text/plain')
          ? await (await item.getType('text/plain')).text() : '';
        return { type: 'html', html, plainText };
      }
      if (format === 3) {
        const type = item.types.find((candidate) => candidate.startsWith('image/'));
        if (type !== undefined) return imageBlobToRgba(await item.getType(type));
      }
    }
    return undefined;
  }

  private async writeClipboard(content: UiClipboardContent): Promise<void> {
    const window = this.root.ownerDocument.defaultView;
    const clipboard = window?.navigator.clipboard;
    if (clipboard === undefined) throw new UiSystemUnsupportedError('Clipboard API is unavailable');
    if (content.type === 'text') { await clipboard.writeText(content.text); return; }
    if (clipboard.write === undefined || window?.ClipboardItem === undefined) {
      throw new UiSystemUnsupportedError('rich Clipboard API is unavailable');
    }
    if (content.type === 'html') {
      await clipboard.write([new window.ClipboardItem({
        'text/html': new Blob([content.html], { type: 'text/html' }),
        'text/plain': new Blob([content.plainText], { type: 'text/plain' }),
      })]);
      return;
    }
    const blob = await rgbaToPng(this.root.ownerDocument, content);
    await clipboard.write([new window.ClipboardItem({ 'image/png': blob })]);
  }

  private async fileDialog(request: UiFileDialogRequest): Promise<string[]> {
    const window = this.root.ownerDocument.defaultView as PickerWindow | null;
    if (window === null) throw new UiSystemUnsupportedError('browser window is unavailable');
    const types = pickerTypes(request.filters);
    try {
      switch (request.kind) {
        case 1:
        case 2:
          if (window.showOpenFilePicker !== undefined) {
            return (await window.showOpenFilePicker({ multiple: request.kind === 2, types })).map(retainBrowserFileHandle);
          }
          return fallbackFileInput(this.root.ownerDocument, request.kind === 2, false, request.filters);
        case 3:
          if (window.showDirectoryPicker !== undefined) return [retainBrowserFileHandle(await window.showDirectoryPicker())];
          return fallbackFileInput(this.root.ownerDocument, false, true, request.filters);
        case 4: throw new UiSystemUnsupportedError('multiple directory selection is unavailable in this browser');
        case 5:
          if (window.showSaveFilePicker === undefined) throw new UiSystemUnsupportedError('save file picker is unavailable');
          return [retainBrowserFileHandle(await window.showSaveFilePicker({ suggestedName: request.initialFileName, types }))];
      }
    } catch (cause) {
      if (cause instanceof DOMException && cause.name === 'AbortError') return [];
      throw cause;
    }
  }

  private messageDialog(request: Extract<UiSystemRequest, { type: 'message-dialog' }>): 1 | 2 | 3 | 4 {
    const window = this.root.ownerDocument.defaultView;
    if (window === null) throw new UiSystemUnsupportedError('browser window is unavailable');
    const message = request.title.length === 0 ? request.description : `${request.title}\n\n${request.description}`;
    if (request.buttons === 1) { window.alert(message); return 1; }
    const accepted = window.confirm(message);
    if (request.buttons === 2) return accepted ? 1 : 2;
    return accepted ? 3 : 4;
  }

  private nextEvent(): Promise<UiSystemEvent> {
    const event = this.pendingEvents.shift();
    if (event !== undefined) return Promise.resolve(event);
    return new Promise((resolve, reject) => this.eventWaiters.push({ resolve, reject }));
  }

  private publish(event: Omit<Extract<UiSystemEvent, { type: 'drag-drop' }>, 'sequence'>): void {
    const sequenced: UiSystemEvent = { ...event, sequence: this.sequence++ };
    const waiter = this.eventWaiters.shift();
    if (waiter !== undefined) { waiter.resolve(sequenced); return; }
    if (this.pendingEvents.length >= this.maxPendingEvents) throw new Error('browser UI system event queue is full');
    this.pendingEvents.push(sequenced);
  }

  private drag(event: DragEvent, phase: 1 | 2 | 3 | 4): void {
    event.preventDefault();
    const rect = this.root.getBoundingClientRect();
    const paths = phase === 1 || phase === 4
      ? Array.from(event.dataTransfer?.files ?? []).map((file) => file.webkitRelativePath || file.name)
      : [];
    if ((phase === 1 || phase === 4) && paths.length === 0) return;
    this.publish({ type: 'drag-drop', phase, x: event.clientX - rect.left, y: event.clientY - rect.top, paths });
  }

  private listen(type: string, listener: EventListener): void {
    this.root.addEventListener(type, listener);
    this.listeners.push([type, listener]);
  }
}

class UiSystemUnsupportedError extends Error {}

function browserFailure(cause: unknown): { kind: UiSystemFailureKind; message: string } {
  if (cause instanceof UiSystemUnsupportedError) return { kind: 2, message: cause.message };
  if (cause instanceof DOMException) {
    if (cause.name === 'NotAllowedError' || cause.name === 'SecurityError') return { kind: 1, message: cause.message };
    if (cause.name === 'AbortError') return { kind: 3, message: cause.message };
  }
  return { kind: 4, message: cause instanceof Error ? cause.message : String(cause) };
}

function pickerTypes(filters: readonly UiFileDialogFilter[]): FilePickerType[] {
  return filters.map((filter) => ({
    description: filter.name,
    accept: { 'application/octet-stream': filter.extensions.map((extension) => `.${extension}`) },
  }));
}

function fallbackFileInput(
  document: Document,
  multiple: boolean,
  directory: boolean,
  filters: readonly UiFileDialogFilter[],
): Promise<string[]> {
  return new Promise((resolve) => {
    const input = document.createElement('input');
    input.type = 'file';
    input.multiple = multiple;
    input.accept = filters.flatMap((filter) => filter.extensions.map((extension) => `.${extension}`)).join(',');
    if (directory) input.setAttribute('webkitdirectory', '');
    input.style.display = 'none';
    let settled = false;
    const window = document.defaultView;
    const finish = (paths: string[]): void => {
      if (settled) return;
      settled = true;
      window?.removeEventListener('focus', onFocus);
      input.remove();
      resolve(paths);
    };
    const onFocus = (): void => {
      window?.setTimeout(() => {
        if ((input.files?.length ?? 0) === 0) finish([]);
      }, 0);
    };
    document.body.append(input);
    input.addEventListener('change', () => {
      const paths = Array.from(input.files ?? []).map((file) => file.webkitRelativePath || file.name);
      finish(paths);
    }, { once: true });
    input.addEventListener('cancel', () => finish([]), { once: true });
    window?.addEventListener('focus', onFocus, { once: true });
    input.click();
  });
}

async function imageBlobToRgba(blob: Blob): Promise<UiClipboardContent> {
  const bitmap = await createImageBitmap(blob);
  try {
    const canvas = new OffscreenCanvas(bitmap.width, bitmap.height);
    const context = canvas.getContext('2d');
    if (context === null) throw new Error('browser 2D canvas is unavailable');
    context.drawImage(bitmap, 0, 0);
    const image = context.getImageData(0, 0, bitmap.width, bitmap.height);
    return { type: 'rgba8', width: bitmap.width, height: bitmap.height, pixels: new Uint8Array(image.data) };
  } finally {
    bitmap.close();
  }
}

async function rgbaToPng(document: Document, image: Extract<UiClipboardContent, { type: 'rgba8' }>): Promise<Blob> {
  const canvas = document.createElement('canvas');
  canvas.width = image.width;
  canvas.height = image.height;
  const context = canvas.getContext('2d');
  if (context === null) throw new Error('browser 2D canvas is unavailable');
  context.putImageData(new ImageData(new Uint8ClampedArray(image.pixels), image.width, image.height), 0, 0);
  return new Promise((resolve, reject) => canvas.toBlob(
    (blob) => blob === null ? reject(new Error('browser failed to encode clipboard image')) : resolve(blob),
    'image/png',
  ));
}
