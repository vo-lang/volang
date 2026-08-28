import type { AotExternCall, AotExternProvider, AotRunOptions } from './index.js';
import {
  decodeUiSystemResponse,
  encodeUiSystemRequest,
  type UiClipboardContent,
  type UiFileDialogFilter,
  type UiMenuNode,
  type UiSystemHost,
  type UiSystemRequest,
  type UiSystemResponse,
} from './ui_system.js';

const UI_SYSTEM_PACKAGE = 'github.com/vo-lang/ui/system';
const HOST_REPLAY_EFFECT = 1n << 4n;
const MAX_MENU_ITEMS = 4096;
const MAX_MENU_DEPTH = 16;

type UiSystemRequestWithoutId<T = UiSystemRequest> = T extends UiSystemRequest
  ? Omit<T, 'requestId'> : never;

function canonicalExternName(functionName: string): string {
  const encoder = new TextEncoder();
  return `vo1:${encoder.encode(UI_SYSTEM_PACKAGE).byteLength}:${UI_SYSTEM_PACKAGE}`
    + `:${encoder.encode(functionName).byteLength}:${functionName}`;
}

function argument(call: AotExternCall, offset: number): bigint {
  return call.readSlot(call.argumentsStart + offset);
}

function signed(value: bigint): bigint { return BigInt.asIntN(64, value); }

function safeNumber(value: bigint, field: string): number {
  const decoded = Number(value);
  if (!Number.isSafeInteger(decoded) || BigInt(decoded) !== value) {
    throw new Error(`Volang UI system ${field} exceeds safe host indices`);
  }
  return decoded;
}

function readIntSlice(call: AotExternCall, reference: bigint): bigint[] {
  if (reference === 0n) return [];
  const header = safeNumber(reference, 'slice reference');
  if (header + 32 > call.memory.buffer.byteLength) throw new Error('UI system slice header is out of bounds');
  const view = new DataView(call.memory.buffer);
  const data = safeNumber(view.getBigUint64(header, true), 'slice data');
  const length = safeNumber(view.getBigUint64(header + 8, true), 'slice length');
  const stride = safeNumber(view.getBigUint64(header + 24, true), 'slice stride');
  if (stride !== 8 || length > 100_000 || data + length * 8 > call.memory.buffer.byteLength) {
    throw new Error('UI system integer slice violates its bounded ABI');
  }
  return Array.from({ length }, (_, index) => view.getBigUint64(data + index * 8, true));
}

function failure(response: UiSystemResponse): string | undefined {
  if (response.type !== 'failure') return undefined;
  const kind = ['', 'denied', 'unsupported', 'cancelled', 'failed'][response.kind];
  return response.message.length === 0
    ? `UI system request ${kind}` : `UI system request ${kind}: ${response.message}`;
}

function clearReadClipboard(call: AotExternCall): void {
  call.writeSlot(call.destination, 0n);
  call.writeSlot(call.destination + 1, call.allocateString(''));
  call.writeSlot(call.destination + 2, call.allocateString(''));
  call.writeSlot(call.destination + 3, 0n);
  call.writeSlot(call.destination + 4, 0n);
  call.writeSlot(call.destination + 5, 0n);
  call.writeSlot(call.destination + 6, 0n);
}

function parseClipboardWrite(call: AotExternCall): UiClipboardContent {
  switch (safeNumber(signed(argument(call, 0)), 'clipboard kind')) {
    case 1: return { type: 'text', text: call.readString(argument(call, 1)) };
    case 2: return {
      type: 'html',
      html: call.readString(argument(call, 1)),
      plainText: call.readString(argument(call, 2)),
    };
    case 3: {
      const width = safeNumber(signed(argument(call, 3)), 'clipboard width');
      const height = safeNumber(signed(argument(call, 4)), 'clipboard height');
      return { type: 'rgba8', width, height, pixels: call.readByteSlice(argument(call, 5)).slice() };
    }
    default: throw new Error('UI clipboard content kind is invalid');
  }
}

function parseFileDialog(call: AotExternCall): Extract<UiSystemRequest, { type: 'file-dialog' }>['request'] {
  const kind = safeNumber(signed(argument(call, 0)), 'file dialog kind');
  if (kind < 1 || kind > 5) throw new Error('UI file dialog kind is invalid');
  const names = call.readStringSlice(argument(call, 5));
  const extensions = call.readStringSlice(argument(call, 6));
  const counts = readIntSlice(call, argument(call, 7)).map((value) => safeNumber(signed(value), 'filter count'));
  if (names.length !== counts.length) throw new Error('UI file dialog filter arrays disagree');
  let cursor = 0;
  const filters: UiFileDialogFilter[] = [];
  for (let index = 0; index < names.length; index += 1) {
    const end = cursor + counts[index];
    if (counts[index] < 0 || end > extensions.length) throw new Error('UI file dialog extension arrays disagree');
    filters.push({ name: names[index], extensions: extensions.slice(cursor, end) });
    cursor = end;
  }
  if (cursor !== extensions.length) throw new Error('UI file dialog extension arrays disagree');
  const initialDirectory = call.readString(argument(call, 2));
  const initialFileName = call.readString(argument(call, 3));
  return {
    kind: kind as 1 | 2 | 3 | 4 | 5,
    title: call.readString(argument(call, 1)),
    initialDirectory: initialDirectory.length === 0 ? undefined : initialDirectory,
    initialFileName: initialFileName.length === 0 ? undefined : initialFileName,
    canCreateDirectories: argument(call, 4) !== 0n,
    filters,
  };
}

function parseMenu(call: AotExternCall): { revision: bigint; roots: UiMenuNode[] } {
  const revision = signed(argument(call, 0));
  if (revision <= 0n) throw new Error('UI menu revision is invalid');
  const kinds = readIntSlice(call, argument(call, 1)).map((value) => safeNumber(signed(value), 'menu kind'));
  const ids = readIntSlice(call, argument(call, 2));
  const parents = readIntSlice(call, argument(call, 3)).map((value) => safeNumber(signed(value), 'menu parent'));
  const labels = call.readStringSlice(argument(call, 4));
  const flags = readIntSlice(call, argument(call, 5)).map((value) => safeNumber(signed(value), 'menu flags'));
  const shortcuts = call.readStringSlice(argument(call, 6));
  const count = kinds.length;
  if (count > MAX_MENU_ITEMS
    || [ids.length, parents.length, labels.length, flags.length, shortcuts.length].some((length) => length !== count)) {
    throw new Error('UI menu arrays disagree');
  }
  const flat: Array<{ parent: number; node: UiMenuNode }> = [];
  const depths: number[] = [];
  for (let index = 0; index < count; index += 1) {
    const parent = parents[index];
    if (parent >= index || parent < -1 || (parent >= 0 && kinds[parent] !== 3)) {
      throw new Error('UI menu parent is invalid');
    }
    const depth = parent < 0 ? 1 : depths[parent] + 1;
    if (depth > MAX_MENU_DEPTH) throw new Error('UI menu depth exceeds its contract');
    depths.push(depth);
    const id = ids[index];
    const generation = Number((id >> 32n) & 0xffff_ffffn);
    const itemIndex = Number(id & 0xffff_ffffn);
    if (generation === 0 || flags[index] < 0 || (flags[index] & ~3) !== 0) {
      throw new Error('UI menu identity or flags are invalid');
    }
    const kind = kinds[index];
    if (kind < 1 || kind > 4) throw new Error('UI menu item kind is invalid');
    const node: UiMenuNode = kind === 4
      ? { kind, index: itemIndex, generation }
      : kind === 3
        ? { kind, index: itemIndex, generation, label: labels[index], enabled: (flags[index] & 1) !== 0, children: [] }
        : {
          kind: kind as 1 | 2,
          index: itemIndex,
          generation,
          label: labels[index],
          enabled: (flags[index] & 1) !== 0,
          checked: kind === 2 ? (flags[index] & 2) !== 0 : undefined,
          shortcut: shortcuts[index].length === 0 ? undefined : shortcuts[index],
        };
    flat.push({ parent, node });
  }
  const children = Array.from({ length: count }, () => [] as UiMenuNode[]);
  const roots: UiMenuNode[] = [];
  for (let index = count - 1; index >= 0; index -= 1) {
    let node = flat[index].node;
    if (node.kind === 3) node = { ...node, children: children[index].reverse() };
    else if (children[index].length !== 0) throw new Error('UI menu leaf contains children');
    if (flat[index].parent < 0) roots.push(node);
    else children[flat[index].parent].push(node);
  }
  roots.reverse();
  return { revision, roots };
}

/** Core-Wasm AOT providers for the official UI system package. */
export class AotUiSystemHost {
  private nextRequestId = 1n;

  constructor(private readonly host: UiSystemHost) {}

  externs(): NonNullable<AotRunOptions['externs']> {
    const providers: Record<string, AotExternProvider> = {};
    const bind = (name: string, handler: (call: AotExternCall) => Promise<void>) => {
      providers[canonicalExternName(name)] = { handler, supportedEffects: HOST_REPLAY_EFFECT };
    };
    bind('runtimeReadClipboard', (call) => this.readClipboard(call));
    bind('runtimeWriteClipboard', (call) => this.writeClipboard(call));
    bind('runtimeFileDialog', (call) => this.fileDialog(call));
    bind('runtimeMessageDialog', (call) => this.messageDialog(call));
    bind('runtimeInstallMenu', (call) => this.installMenu(call));
    bind('runtimeBeginFileDrag', (call) => this.beginFileDrag(call));
    bind('runtimeWaitEvent', (call) => this.waitEvent(call));
    bind('runtimeInvokeHost', (call) => this.invokeHost(call));
    return providers;
  }

  private async roundTrip(request: UiSystemRequestWithoutId): Promise<UiSystemResponse> {
    const requestId = this.nextRequestId++;
    const frame = encodeUiSystemRequest({ ...request, requestId } as UiSystemRequest);
    const decoded = decodeUiSystemResponse(await this.host.execute(frame));
    if (decoded.requestId !== requestId) throw new Error('UI system AOT response identity mismatch');
    return decoded.response;
  }

  private async readClipboard(call: AotExternCall): Promise<void> {
    clearReadClipboard(call);
    const format = safeNumber(signed(argument(call, 0)), 'clipboard format');
    if (format < 1 || format > 3) throw new Error('UI clipboard format is invalid');
    const response = await this.roundTrip({ type: 'read-clipboard', format: format as 1 | 2 | 3 });
    const message = failure(response);
    if (message !== undefined) { call.writeError(call.destination + 7, message); return; }
    if (response.type !== 'clipboard') throw new Error('UI clipboard read response kind is invalid');
    if (response.content === undefined) { call.clearError(call.destination + 7); return; }
    call.writeSlot(call.destination + 6, 1n);
    switch (response.content.type) {
      case 'text':
        call.writeSlot(call.destination, 1n);
        call.writeSlot(call.destination + 1, call.allocateString(response.content.text));
        break;
      case 'html':
        call.writeSlot(call.destination, 2n);
        call.writeSlot(call.destination + 1, call.allocateString(response.content.html));
        call.writeSlot(call.destination + 2, call.allocateString(response.content.plainText));
        break;
      case 'rgba8':
        call.writeSlot(call.destination, 3n);
        call.writeSlot(call.destination + 3, BigInt(response.content.width));
        call.writeSlot(call.destination + 4, BigInt(response.content.height));
        call.writeSlot(call.destination + 5, call.allocateByteSlice(response.content.pixels));
        break;
    }
    call.clearError(call.destination + 7);
  }

  private async writeClipboard(call: AotExternCall): Promise<void> {
    const response = await this.roundTrip({ type: 'write-clipboard', content: parseClipboardWrite(call) });
    const message = failure(response);
    if (message !== undefined) call.writeError(call.destination, message);
    else if (response.type === 'complete') call.clearError(call.destination);
    else throw new Error('UI clipboard write response kind is invalid');
  }

  private async fileDialog(call: AotExternCall): Promise<void> {
    const response = await this.roundTrip({ type: 'file-dialog', request: parseFileDialog(call) });
    const message = failure(response);
    if (message !== undefined) {
      call.writeSlot(call.destination, 0n);
      call.writeError(call.destination + 1, message);
    } else if (response.type === 'file-dialog') {
      call.writeSlot(call.destination, call.allocateStringSlice(response.paths));
      call.clearError(call.destination + 1);
    } else throw new Error('UI file dialog response kind is invalid');
  }

  private async messageDialog(call: AotExternCall): Promise<void> {
    const level = safeNumber(signed(argument(call, 0)), 'message level');
    const buttons = safeNumber(signed(argument(call, 1)), 'message buttons');
    if (level < 1 || level > 3 || buttons < 1 || buttons > 4) throw new Error('UI message options are invalid');
    const response = await this.roundTrip({
      type: 'message-dialog', level: level as 1 | 2 | 3, buttons: buttons as 1 | 2 | 3 | 4,
      title: call.readString(argument(call, 2)), description: call.readString(argument(call, 3)),
    });
    const message = failure(response);
    if (message !== undefined) {
      call.writeSlot(call.destination, 0n);
      call.writeError(call.destination + 1, message);
    } else if (response.type === 'message-dialog') {
      call.writeSlot(call.destination, BigInt(response.result));
      call.clearError(call.destination + 1);
    } else throw new Error('UI message response kind is invalid');
  }

  private async installMenu(call: AotExternCall): Promise<void> {
    const menu = parseMenu(call);
    const response = await this.roundTrip({ type: 'install-menu', ...menu });
    const message = failure(response);
    if (message !== undefined) call.writeError(call.destination, message);
    else if (response.type === 'menu-installed' && response.revision === menu.revision) {
      call.clearError(call.destination);
    } else throw new Error('UI menu response kind is invalid');
  }

  private async beginFileDrag(call: AotExternCall): Promise<void> {
    const mode = safeNumber(signed(argument(call, 2)), 'file drag mode');
    if (mode !== 1 && mode !== 2) throw new Error('UI file drag mode is invalid');
    const preview = call.readString(argument(call, 1));
    const response = await this.roundTrip({
      type: 'begin-file-drag',
      mode,
      paths: call.readStringSlice(argument(call, 0)),
      preview: preview.length === 0 ? undefined : preview,
    });
    const message = failure(response);
    if (message !== undefined) call.writeError(call.destination, message);
    else if (response.type === 'complete') call.clearError(call.destination);
    else throw new Error('UI file drag response kind is invalid');
  }

  private async waitEvent(call: AotExternCall): Promise<void> {
    for (let offset = 0; offset <= 6; offset += 1) call.writeSlot(call.destination + offset, 0n);
    const response = await this.roundTrip({ type: 'wait-event' });
    const message = failure(response);
    if (message !== undefined) { call.writeError(call.destination + 7, message); return; }
    if (response.type !== 'event') throw new Error('UI event response kind is invalid');
    call.writeSlot(call.destination + 1, response.event.sequence);
    if (response.event.type === 'menu') {
      call.writeSlot(call.destination, 1n);
      call.writeSlot(
        call.destination + 2,
        (BigInt(response.event.generation) << 32n) | BigInt(response.event.index),
      );
    } else {
      call.writeSlot(call.destination, 2n);
      call.writeSlot(call.destination + 3, BigInt(response.event.phase));
      call.writeFloat64(call.destination + 4, response.event.x);
      call.writeFloat64(call.destination + 5, response.event.y);
      call.writeSlot(call.destination + 6, call.allocateStringSlice(response.event.paths));
    }
    call.clearError(call.destination + 7);
  }

  private async invokeHost(call: AotExternCall): Promise<void> {
    const response = await this.roundTrip({
      type: 'invoke-host', service: call.readString(argument(call, 0)),
      operation: call.readString(argument(call, 1)), payload: call.readByteSlice(argument(call, 2)).slice(),
    });
    const message = failure(response);
    if (message !== undefined) {
      call.writeSlot(call.destination, 0n); call.writeError(call.destination + 1, message); return;
    }
    if (response.type !== 'host-payload') throw new Error('UI host invocation response kind is invalid');
    call.writeSlot(call.destination, call.allocateByteSlice(response.payload));
    call.clearError(call.destination + 1);
  }
}
