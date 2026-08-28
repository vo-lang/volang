import type { AotExternCall, AotExternProvider, AotRunOptions } from './index.js';
import { UiDomAdapter, type UiDomAdapterOptions } from './ui_dom.js';
import {
  decodeUiEvent,
  type UiEventEnvelope,
  type UiIdentity,
  type UiListener,
  type UiMutation,
  type UiMutationBatch,
  type UiValue,
  uiIdentityKey,
} from './ui_protocol.js';
import { UiBrowserSystemHost, type UiSystemHost } from './ui_system.js';
import { AotUiSystemHost } from './ui_system_aot.js';

const UI_PACKAGE = 'github.com/vo-lang/ui';
const MAX_VIEWS = 100_000;
const MAX_HANDLERS = 65_536;
const MAX_KEY_BYTES = 4 * 1024;
const MAX_COMPONENT_DEPTH = 256;
const SCOPED_STATE_HANDLE_TAG = 1n << 63n;
const ROOT_ID: UiIdentity = Object.freeze({ index: 0, generation: 1 });

type StateCell = string | boolean | bigint | number;

interface AotViewListener {
  readonly handler: number;
  readonly capture: boolean;
}

interface AotView {
  readonly key?: string;
  readonly kind: 'element' | 'text';
  readonly primitive?: number;
  readonly text?: string;
  readonly properties: ReadonlyMap<number, UiValue>;
  readonly listeners: ReadonlyMap<number, AotViewListener>;
  readonly children: readonly AotView[];
}

interface RenderedNode {
  readonly id: UiIdentity;
  readonly view: AotView;
  readonly parent: UiIdentity;
  readonly handlerGeneration: number;
  readonly children: readonly RenderedNode[];
}

interface PendingEvent {
  readonly call: AotExternCall;
  readonly resolve: (status: number) => void;
  readonly reject: (cause: unknown) => void;
}

interface AotComponentFrame {
  readonly path: string;
  stateCursor: number;
}

interface ComponentStateCheckpoint {
  readonly cells: readonly (StateCell | undefined)[];
  readonly generations: readonly number[];
  readonly free: readonly number[];
  readonly handles: ReadonlyMap<string, number>;
}

function canonicalExternName(functionName: string): string {
  const encoder = new TextEncoder();
  return `vo1:${encoder.encode(UI_PACKAGE).byteLength}:${UI_PACKAGE}`
    + `:${encoder.encode(functionName).byteLength}:${functionName}`;
}

function argument(call: AotExternCall, offset: number): bigint {
  return call.readSlot(call.argumentsStart + offset);
}

function signed(value: bigint): bigint {
  return BigInt.asIntN(64, value);
}

function safeNumber(value: bigint, field: string): number {
  const decoded = Number(value);
  if (!Number.isSafeInteger(decoded) || BigInt(decoded) !== value) {
    throw new Error(`Volang UI ${field} exceeds safe host indices`);
  }
  return decoded;
}

function identity(index: number, generation: number): UiIdentity {
  if (!Number.isSafeInteger(index) || index <= 0 || index > 0xffff_ffff
    || !Number.isSafeInteger(generation) || generation <= 0 || generation > 0xffff_ffff) {
    throw new Error('Volang UI identity space is exhausted');
  }
  return { index, generation };
}

function emptyElement(primitive: number, children: readonly AotView[] = []): AotView {
  return {
    kind: 'element',
    primitive,
    properties: new Map(),
    listeners: new Map(),
    children,
  };
}

function textNode(text: string): AotView {
  return {
    kind: 'text',
    text,
    properties: new Map(),
    listeners: new Map(),
    children: [],
  };
}

function cloneWithProperty(view: AotView, property: number, value: UiValue): AotView {
  if (view.kind !== 'element') throw new Error('Volang UI modifier target is a text node');
  const properties = new Map(view.properties);
  properties.set(property, value);
  return { ...view, properties };
}

function cloneWithListener(
  view: AotView,
  event: number,
  handler: number,
  capture = false,
): AotView {
  if (view.kind !== 'element') throw new Error('Volang UI listener target is a text node');
  const listeners = new Map(view.listeners);
  listeners.set(event, { handler, capture });
  return { ...view, listeners };
}

function cloneWithKey(view: AotView, key: string): AotView {
  return { ...view, key };
}

function floatValue(call: AotExternCall, offset: number, field: string): number {
  const value = call.readFloat64(call.argumentsStart + offset);
  if (!Number.isFinite(value)) throw new Error(`Volang UI ${field} requires a finite value`);
  return value;
}

function sameValue(left: UiValue | undefined, right: UiValue | undefined): boolean {
  if (left === undefined || right === undefined || left.type !== right.type) return left === right;
  switch (left.type) {
    case 'bool':
    case 'i64':
    case 'f64':
    case 'text':
    case 'color':
      return left.value === right.value;
    case 'length': {
      if (right.type !== 'length' || left.value.unit !== right.value.unit) return false;
      if (left.value.unit === 'auto' || right.value.unit === 'auto') return true;
      return left.value.value === right.value.value;
    }
    case 'bytes':
      return right.type === 'bytes'
        && left.value.byteLength === right.value.byteLength
        && left.value.every((value, index) => value === right.value[index]);
  }
}

function readViewSlice(call: AotExternCall, reference: bigint): bigint[] {
  if (reference === 0n) return [];
  const header = safeNumber(reference, 'view slice reference');
  if (header + 32 > call.memory.buffer.byteLength) {
    throw new Error('Volang UI view slice header is outside guest memory');
  }
  const view = new DataView(call.memory.buffer);
  const data = safeNumber(view.getBigUint64(header, true), 'view slice data');
  const length = safeNumber(view.getBigUint64(header + 8, true), 'view slice length');
  const stride = safeNumber(view.getBigUint64(header + 24, true), 'view slice stride');
  if (stride !== 8 || length > MAX_VIEWS || data + length * stride > call.memory.buffer.byteLength) {
    throw new Error('Volang UI view slice violates its bounded ABI');
  }
  const handles: bigint[] = [];
  for (let index = 0; index < length; index += 1) {
    handles.push(view.getBigUint64(data + index * stride, true));
  }
  return handles;
}

/** Core-Wasm AOT provider for the official renderer-neutral UI ABI. */
export class AotUiHost {
  readonly adapter: UiDomAdapter;
  private readonly views = new Map<bigint, AotView>();
  private readonly state: (StateCell | undefined)[] = [];
  private stateCursor = 0;
  private componentState: (StateCell | undefined)[] = [];
  private componentStateGenerations: number[] = [];
  private componentStateFree: number[] = [];
  private componentStateHandles = new Map<string, number>();
  private readonly componentStateLive = new Set<string>();
  private readonly componentStack: AotComponentFrame[] = [];
  private readonly componentOccurrences = new Map<string, number>();
  private componentStateCheckpoint?: ComponentStateCheckpoint;
  private nextView = 1n;
  private nextNodeIdentity = 1;
  private revision = 0n;
  private generation = 0;
  private deliveredEventSequence = 0n;
  private rendered: readonly RenderedNode[] = [];
  private liveNodes = new Map<string, RenderedNode>();
  private pending?: PendingEvent;
  private invalidationPending = false;
  private readonly browserWindow?: Window;
  private location = '/';
  private readonly system: AotUiSystemHost;
  private readonly onCommit?: (revision: bigint, mutationCount: number) => void;

  constructor(root: HTMLElement, options: AotUiHostOptions = {}) {
    const { systemHost, onCommit, ...adapterOptions } = options;
    this.browserWindow = root.ownerDocument.defaultView ?? undefined;
    this.system = new AotUiSystemHost(systemHost ?? new UiBrowserSystemHost(root));
    this.onCommit = onCommit;
    if (this.browserWindow !== undefined) {
      this.location = browserLocation(this.browserWindow);
      this.browserWindow.addEventListener('popstate', () => {
        this.location = browserLocation(this.browserWindow as Window);
        this.invalidate();
      });
      this.browserWindow.addEventListener('resize', () => this.invalidate());
    }
    this.adapter = new UiDomAdapter(root, {
      ...adapterOptions,
      rootId: adapterOptions.rootId ?? ROOT_ID,
      onEvent: () => this.deliverEvent(),
    });
  }

  externs(): NonNullable<AotRunOptions['externs']> {
    const providers: Record<string, AotExternProvider> = {};
    const bind = (name: string, handler: (call: AotExternCall) => number | void | Promise<number>) => {
      providers[canonicalExternName(name)] = { handler };
    };
    providers[canonicalExternName('runtimeBegin')] = {
      handler: (call) => this.begin(call),
      supportedEffects: 1n << 5n,
    };
    bind('runtimeEnterComponent', (call) => this.enterComponent(call));
    bind('runtimeExitComponent', () => this.exitComponent());
    providers[canonicalExternName('runtimeCommitAndWait')] = {
      handler: (call) => this.commitAndWait(call),
      supportedEffects: 1n << 4n,
    };
    bind('Invalidate', () => this.invalidate());
    bind('LocationPath', (call) => {
      call.writeSlot(call.destination, call.allocateString(this.location));
    });
    bind('Navigate', (call) => this.navigate(call, false));
    bind('ReplaceLocation', (call) => this.navigate(call, true));
    bind('NavigateBack', () => this.browserWindow?.history.back());
    bind('NavigateForward', () => this.browserWindow?.history.forward());
    bind('runtimeViewportMetrics', (call) => {
      call.writeFloat64(call.destination, this.browserWindow?.innerWidth ?? 1024);
      call.writeFloat64(call.destination + 1, this.browserWindow?.innerHeight ?? 768);
      call.writeFloat64(call.destination + 2, this.browserWindow?.devicePixelRatio ?? 1);
    });
    bind('UseStringState', (call) => this.useState(call, call.readString(argument(call, 0))));
    bind('StringStateValue', (call) => this.readStringState(call));
    bind('SetStringState', (call) => this.setState(call, call.readString(argument(call, 1)), 'string'));
    bind('UseBoolState', (call) => this.useState(call, argument(call, 0) !== 0n));
    bind('BoolStateValue', (call) => this.readScalarState(call, 'boolean'));
    bind('SetBoolState', (call) => this.setState(call, argument(call, 1) !== 0n, 'boolean'));
    bind('UseIntState', (call) => this.useState(call, signed(argument(call, 0))));
    bind('IntStateValue', (call) => this.readScalarState(call, 'bigint'));
    bind('SetIntState', (call) => this.setState(call, signed(argument(call, 1)), 'bigint'));
    bind('IntStateAlive', (call) => this.stateAlive(call, 'bigint'));
    bind('IntStateCommitted', (call) => this.stateCommitted(call, 'bigint'));
    bind('UseFloatState', (call) => this.useState(call, floatValue(call, 0, 'float state')));
    bind('FloatStateValue', (call) => this.readFloatState(call));
    bind('SetFloatState', (call) => this.setState(call, floatValue(call, 1, 'float state'), 'number'));

    const containers: Readonly<Record<string, number>> = {
      Fragment: 1, Box: 2, Row: 3, Column: 4, Stack: 5, Grid: 6, Scroll: 7,
    };
    for (const [name, primitive] of Object.entries(containers)) {
      bind(name, (call) => this.container(call, primitive));
    }
    bind('Text', (call) => this.text(call));
    bind('runtimeImage', (call) => this.image(call));
    bind('runtimeCanvas', (call) => this.canvas(call));
    bind('runtimePlatformView', (call) => this.platformView(call));
    bind('runtimeButton', (call) => this.button(call));
    bind('runtimeTextInput', (call) => this.textInput(call));
    bind('runtimeTextArea', (call) => this.textArea(call));
    bind('runtimeToggle', (call) => this.toggle(call));
    bind('runtimeSlider', (call) => this.slider(call));

    const lengths: Readonly<Record<string, number>> = {
      Width: 1, Height: 2, MinWidth: 3, MinHeight: 4, MaxWidth: 5, MaxHeight: 6,
      Gap: 8, Padding: 9, FontSize: 12, Radius: 24, BorderWidth: 56,
    };
    for (const [name, property] of Object.entries(lengths)) {
      bind(name, (call) => this.modify(call, property, {
        type: 'length', value: { unit: 'px', value: floatValue(call, 1, name) },
      }));
    }
    const floats: Readonly<Record<string, number>> = { Flex: 7, ScrollX: 26, ScrollY: 27, Opacity: 50 };
    for (const [name, property] of Object.entries(floats)) {
      bind(name, (call) => this.modify(call, property, {
        type: 'f64', value: floatValue(call, 1, name),
      }));
    }
    const texts: Readonly<Record<string, number>> = {
      Align: 14, Justify: 15, Role: 19, AccessibleName: 20, TestID: 21,
      GridColumns: 22, Overflow: 23, AccessibleDescription: 30,
      GridTemplateAreas: 33, GridArea: 34,
      PointerEvents: 37,
      AccessibleValue: 16,
      Current: 45,
      Source: 47, ContentType: 48, Fit: 49, Transform: 51,
      GraphicsProgram: 52, MediaState: 53, Poster: 54,
    };
    for (const [name, property] of Object.entries(texts)) {
      bind(name, (call) => this.modify(call, property, {
        type: 'text', value: call.readString(argument(call, 1)),
      }));
    }
    const booleans: Readonly<Record<string, number>> = {
      Disabled: 18, Checked: 25, Required: 28, Invalid: 29, Modal: 35, AutoFocus: 36,
      CapturePointer: 38, Selected: 42, Expanded: 43, Pressed: 44, Hidden: 46,
      AccessibilityHidden: 60,
      Focusable: 61,
    };
    for (const [name, property] of Object.entries(booleans)) {
      bind(name, (call) => this.modify(call, property, {
        type: 'bool', value: argument(call, 1) !== 0n,
      }));
    }
    bind('Background', (call) => this.color(call, 10));
    bind('Foreground', (call) => this.color(call, 11));
    bind('BorderColor', (call) => this.color(call, 55));
    bind('FontWeight', (call) => this.modify(call, 13, {
      type: 'i64', value: signed(argument(call, 1)),
    }));
    bind('FlowDirection', (call) => {
      const direction = signed(argument(call, 1));
      if (direction !== 0n && direction !== 1n) throw new Error('Volang UI flow direction is invalid');
      this.modify(call, 39, { type: 'i64', value: direction });
    });
    bind('Portal', (call) => {
      const layer = signed(argument(call, 1));
      if (layer < -1_000_000n || layer > 1_000_000n) {
        throw new Error('Volang UI portal layer exceeds the portable range');
      }
      this.modify(call, 40, { type: 'i64', value: layer });
    });
    bind('FocusRequest', (call) => {
      const token = signed(argument(call, 1));
      if (token < 0n) throw new Error('Volang UI focus request token cannot be negative');
      this.modify(call, 41, { type: 'i64', value: token });
    });
    bind('SelectionStartUTF16', (call) => this.selection(call, 31, 'selection start'));
    bind('SelectionLengthUTF16', (call) => this.selection(call, 32, 'selection length'));
    bind('Key', (call) => this.key(call));

    const listeners: Readonly<Record<string, number>> = {
      runtimeOnClick: 1,
      runtimeOnSubmit: 4,
      runtimeOnFocus: 5,
      runtimeOnBlur: 6,
      runtimeOnKeyDown: 7,
      runtimeOnKeyUp: 8,
      runtimeOnPointerDown: 9,
      runtimeOnPointerMove: 10,
      runtimeOnPointerUp: 11,
      runtimeOnPointerCancel: 18,
      runtimeOnScroll: 12,
      runtimeOnCompositionStart: 13,
      runtimeOnCompositionUpdate: 14,
      runtimeOnCompositionEnd: 15,
      runtimeOnSelectionChange: 20,
      runtimeOnWheel: 16,
      runtimeOnLayout: 19,
    };
    for (const [name, event] of Object.entries(listeners)) {
      bind(name, (call) => this.listen(call, event));
    }
    bind('runtimeOnKeyDownCapture', (call) => this.listen(call, 7, true));
    Object.assign(providers, this.system.externs());
    return providers;
  }

  private begin(call: AotExternCall): void {
    const initial = argument(call, 0) !== 0n;
    if (initial !== (this.revision === 0n)) {
      throw new Error('Volang UI render transaction has an invalid initial flag');
    }
    if (this.pending !== undefined) throw new Error('Volang UI began rendering while awaiting an event');
    if (this.componentStateCheckpoint !== undefined) {
      this.componentState = [...this.componentStateCheckpoint.cells];
      this.componentStateGenerations = [...this.componentStateCheckpoint.generations];
      this.componentStateFree = [...this.componentStateCheckpoint.free];
      this.componentStateHandles = new Map(this.componentStateCheckpoint.handles);
    }
    this.componentStateCheckpoint = {
      cells: [...this.componentState],
      generations: [...this.componentStateGenerations],
      free: [...this.componentStateFree],
      handles: new Map(this.componentStateHandles),
    };
    this.views.clear();
    this.stateCursor = 0;
    this.componentStateLive.clear();
    this.componentStack.length = 0;
    this.componentOccurrences.clear();
    this.nextView = 1n;
    // Core-Wasm AOT keeps its guest-owned render loop until the component
    // evaluator table is wired into this host. VM/JIT direct updates use the
    // same private ABI and return true when the root call can be skipped.
    call.writeSlot(call.destination, 0n);
  }

  private useState(call: AotExternCall, initial: StateCell): void {
    const component = this.componentStack[this.componentStack.length - 1];
    if (component !== undefined) {
      const stateKey = `${component.path}/s:${component.stateCursor}`;
      component.stateCursor += 1;
      this.componentStateLive.add(stateKey);
      let index = this.componentStateHandles.get(stateKey);
      if (index === undefined) {
        index = this.componentStateFree.pop();
        if (index === undefined) {
          index = this.componentState.length;
          this.componentState.push(initial);
          this.componentStateGenerations.push(1);
        } else {
          const generation = (this.componentStateGenerations[index] ?? 0) + 1;
          if (generation > 0x7fff_ffff) {
            throw new Error('Volang UI component state generation space is exhausted');
          }
          this.componentStateGenerations[index] = generation;
          this.componentState[index] = initial;
        }
        this.componentStateHandles.set(stateKey, index);
      } else {
        const current = this.componentState[index];
        if (current === undefined || typeof current !== typeof initial) {
          throw new Error('Volang UI component state declaration changed its value type');
        }
      }
      if (this.state.length + this.componentStateHandles.size > MAX_HANDLERS) {
        throw new Error('Volang UI state declaration limit exceeded');
      }
      const generation = this.componentStateGenerations[index];
      call.writeSlot(
        call.destination,
        SCOPED_STATE_HANDLE_TAG | (BigInt(generation) << 32n) | BigInt(index + 1),
      );
      return;
    }
    const index = this.stateCursor;
    this.stateCursor += 1;
    if (index >= MAX_HANDLERS) throw new Error('Volang UI state declaration limit exceeded');
    const current = this.state[index];
    if (current === undefined) this.state.push(initial);
    else if (typeof current !== typeof initial) {
      throw new Error('Volang UI state declaration order changed its value type');
    }
    call.writeSlot(call.destination, BigInt(index + 1));
  }

  private stateSlot(call: AotExternCall): {
    readonly cells: (StateCell | undefined)[];
    readonly index: number;
  } {
    const handle = argument(call, 0);
    if (handle === 0n) throw new Error('Volang UI state handle is zero');
    const scoped = (handle & SCOPED_STATE_HANDLE_TAG) !== 0n;
    const raw = scoped ? handle & ~SCOPED_STATE_HANDLE_TAG : handle;
    const encodedIndex = scoped ? raw & 0xffff_ffffn : raw;
    const index = safeNumber(encodedIndex - 1n, 'state handle');
    const cells = scoped ? this.componentState : this.state;
    const generation = scoped ? safeNumber(raw >> 32n, 'state generation') : 0;
    if (index >= cells.length || cells[index] === undefined
      || (scoped && (generation === 0 || this.componentStateGenerations[index] !== generation))) {
      throw new Error('Volang UI state handle is stale');
    }
    return { cells, index };
  }

  private readStringState(call: AotExternCall): void {
    const slot = this.stateSlot(call);
    const value = slot.cells[slot.index];
    if (typeof value !== 'string') throw new Error('Volang UI state handle does not contain a string');
    call.writeSlot(call.destination, call.allocateString(value));
  }

  private readScalarState(call: AotExternCall, expected: 'boolean' | 'bigint'): void {
    const slot = this.stateSlot(call);
    const value = slot.cells[slot.index];
    if (typeof value !== expected) throw new Error(`Volang UI state handle does not contain a ${expected}`);
    call.writeSlot(call.destination, typeof value === 'boolean' ? BigInt(value) : value as bigint);
  }

  private stateAlive(call: AotExternCall, expected: 'boolean' | 'bigint' | 'number' | 'string'): void {
    try {
      const slot = this.stateSlot(call);
      call.writeSlot(call.destination, BigInt(typeof slot.cells[slot.index] === expected));
    } catch {
      call.writeSlot(call.destination, 0n);
    }
  }

  private stateCommitted(
    call: AotExternCall,
    expected: 'boolean' | 'bigint' | 'number' | 'string',
  ): void {
    if (this.revision === 0n || this.componentStateCheckpoint !== undefined) {
      call.writeSlot(call.destination, 0n);
      return;
    }
    this.stateAlive(call, expected);
  }

  private readFloatState(call: AotExternCall): void {
    const slot = this.stateSlot(call);
    const value = slot.cells[slot.index];
    if (typeof value !== 'number') throw new Error('Volang UI state handle does not contain a float');
    call.writeFloat64(call.destination, value);
  }

  private setState(call: AotExternCall, value: StateCell, expected: string): void {
    const slot = this.stateSlot(call);
    if (typeof slot.cells[slot.index] !== expected) {
      throw new Error(`Volang UI state handle does not contain a ${expected}`);
    }
    slot.cells[slot.index] = value;
  }

  private enterComponent(call: AotExternCall): void {
    if (this.componentStack.length >= MAX_COMPONENT_DEPTH) {
      throw new Error('Volang UI component scope nesting limit exceeded');
    }
    const identityValue = call.readString(argument(call, 0));
    const identityBytes = new TextEncoder().encode(identityValue).byteLength;
    if (identityBytes === 0 || identityBytes > MAX_KEY_BYTES) {
      throw new Error('Volang UI component identity must contain 1..=4096 UTF-8 bytes');
    }
    const callSite = argument(call, 1);
    const keyed = argument(call, 2) !== 0n;
    const parent = this.componentStack[this.componentStack.length - 1]?.path ?? '';
    let instance: string;
    if (keyed) {
      const key = call.readString(argument(call, 3));
      const keyBytes = new TextEncoder().encode(key).byteLength;
      if (keyBytes === 0 || keyBytes > MAX_KEY_BYTES) {
        throw new Error('Volang UI component key must contain 1..=4096 UTF-8 bytes');
      }
      instance = `k:${keyBytes}:${key}`;
    } else {
      const occurrenceKey = `${parent}|${identityBytes}:${identityValue}|${callSite}`;
      const occurrence = this.componentOccurrences.get(occurrenceKey) ?? 0;
      this.componentOccurrences.set(occurrenceKey, occurrence + 1);
      instance = `c:${callSite}:${occurrence}`;
    }
    const path = `${parent}/${identityBytes}:${identityValue}/${instance}`;
    this.componentStack.push({ path, stateCursor: 0 });
  }

  private exitComponent(): void {
    if (this.componentStack.pop() === undefined) {
      throw new Error('Volang UI component scope stack is empty');
    }
  }

  private invalidate(): void {
    // A mutation made while the guest owns a UI turn is included in the
    // render that follows that turn. Retaining another invalidation here
    // would immediately advance every listener generation after commit and
    // could discard the next browser event that was already queued.
    if (this.pending === undefined) return;
    this.invalidationPending = true;
    this.deliverEvent();
  }

  private navigate(call: AotExternCall, replace: boolean): void {
    const path = call.readString(argument(call, 0));
    const window = this.browserWindow;
    if (window === undefined) {
      validateNavigationPath(path);
      this.location = path;
      return;
    }
    const url = checkedNavigationUrl(window, path);
    if (replace) window.history.replaceState(null, '', url);
    else window.history.pushState(null, '', url);
    this.location = browserLocation(window);
  }

  private insert(call: AotExternCall, view: AotView): void {
    if (this.views.size >= MAX_VIEWS) throw new Error('Volang UI construction exceeded its View limit');
    const handle = this.nextView;
    this.nextView += 1n;
    this.views.set(handle, view);
    call.writeSlot(call.destination, handle);
  }

  private requireView(handle: bigint): AotView {
    const view = this.views.get(handle);
    if (view === undefined) throw new Error('Volang UI View handle is stale');
    return view;
  }

  private container(call: AotExternCall, primitive: number): void {
    const children = readViewSlice(call, argument(call, 0)).map((handle) => this.requireView(handle));
    this.insert(call, emptyElement(primitive, children));
  }

  private text(call: AotExternCall): void {
    this.insert(call, emptyElement(15, [textNode(call.readString(argument(call, 0)))]));
  }

  private image(call: AotExternCall): void {
    this.insert(call, {
      ...emptyElement(8),
      properties: new Map<number, UiValue>([
        [47, { type: 'text', value: call.readString(argument(call, 0)) }],
        [20, { type: 'text', value: call.readString(argument(call, 1)) }],
      ]),
    });
  }

  private canvas(call: AotExternCall): void {
    this.insert(call, {
      ...emptyElement(13),
      properties: new Map<number, UiValue>([
        [19, { type: 'text', value: 'img' }],
        [52, { type: 'text', value: call.readString(argument(call, 0)) }],
        [20, { type: 'text', value: call.readString(argument(call, 1)) }],
      ]),
    });
  }

  private platformView(call: AotExternCall): void {
    this.insert(call, {
      ...emptyElement(14),
      properties: new Map<number, UiValue>([
        [19, { type: 'text', value: 'group' }],
        [48, { type: 'text', value: call.readString(argument(call, 0)) }],
        [53, { type: 'text', value: call.readString(argument(call, 1)) }],
        [20, { type: 'text', value: call.readString(argument(call, 2)) }],
      ]),
    });
  }

  private guestHandler(call: AotExternCall, offset: number): number {
    const handler = safeNumber(argument(call, offset), 'handler identity');
    if (handler >= MAX_HANDLERS) throw new Error('Volang UI handler limit exceeded');
    return handler;
  }

  private button(call: AotExternCall): void {
    const label = call.readString(argument(call, 0));
    const handler = this.guestHandler(call, 1);
    const properties = new Map<number, UiValue>([
      [19, { type: 'text', value: 'button' }],
      [20, { type: 'text', value: label }],
    ]);
    const listeners = new Map([[1, { handler, capture: false }]]);
    this.insert(call, { ...emptyElement(9, [textNode(label)]), properties, listeners });
  }

  private textInput(call: AotExternCall): void {
    const value = call.readString(argument(call, 0));
    const placeholder = call.readString(argument(call, 1));
    const handler = this.guestHandler(call, 2);
    const properties = new Map<number, UiValue>([
      [19, { type: 'text', value: 'textbox' }],
      [16, { type: 'text', value }],
      [17, { type: 'text', value: placeholder }],
    ]);
    this.insert(call, {
      ...emptyElement(10), properties, listeners: new Map([[2, { handler, capture: false }]]),
    });
  }

  private textArea(call: AotExternCall): void {
    const value = call.readString(argument(call, 0));
    const placeholder = call.readString(argument(call, 1));
    const handler = this.guestHandler(call, 2);
    const properties = new Map<number, UiValue>([
      [19, { type: 'text', value: 'textbox' }],
      [16, { type: 'text', value }],
      [17, { type: 'text', value: placeholder }],
    ]);
    this.insert(call, {
      ...emptyElement(16), properties, listeners: new Map([[2, { handler, capture: false }]]),
    });
  }

  private toggle(call: AotExternCall): void {
    const checked = argument(call, 0) !== 0n;
    const label = call.readString(argument(call, 1));
    const handler = this.guestHandler(call, 2);
    const properties = new Map<number, UiValue>([
      [19, { type: 'text', value: 'switch' }],
      [25, { type: 'bool', value: checked }],
      [20, { type: 'text', value: label }],
    ]);
    this.insert(call, {
      ...emptyElement(11), properties,
      listeners: new Map([[3, { handler, capture: false }]]),
    });
  }

  private slider(call: AotExternCall): void {
    const value = floatValue(call, 0, 'slider value');
    const minimum = floatValue(call, 1, 'slider minimum');
    const maximum = floatValue(call, 2, 'slider maximum');
    const step = floatValue(call, 3, 'slider step');
    const label = call.readString(argument(call, 4));
    if (maximum <= minimum || step <= 0 || value < minimum || value > maximum || label.length === 0) {
      throw new Error('Volang UI slider contract is invalid');
    }
    const handler = this.guestHandler(call, 5);
    const properties = new Map<number, UiValue>([
      [19, { type: 'text', value: 'slider' }],
      [20, { type: 'text', value: label }],
      [16, { type: 'f64', value }],
      [57, { type: 'f64', value: minimum }],
      [58, { type: 'f64', value: maximum }],
      [59, { type: 'f64', value: step }],
    ]);
    this.insert(call, {
      ...emptyElement(12), properties, listeners: new Map([[2, { handler, capture: false }]]),
    });
  }

  private modify(call: AotExternCall, property: number, value: UiValue): void {
    this.insert(call, cloneWithProperty(this.requireView(argument(call, 0)), property, value));
  }

  private color(call: AotExternCall, property: number): void {
    const raw = argument(call, 1);
    if (raw > 0xffff_ffffn) throw new Error('Volang UI color exceeds u32');
    this.modify(call, property, { type: 'color', value: Number(raw) });
  }

  private selection(call: AotExternCall, property: number, field: string): void {
    const value = signed(argument(call, 1));
    if (value < 0n) throw new Error(`Volang UI ${field} cannot be negative`);
    this.modify(call, property, { type: 'i64', value });
  }

  private listen(call: AotExternCall, event: number, capture = false): void {
    const handler = this.guestHandler(call, 1);
    this.insert(call, cloneWithListener(
      this.requireView(argument(call, 0)), event, handler, capture,
    ));
  }

  private key(call: AotExternCall): void {
    const key = call.readString(argument(call, 1));
    if (key.length === 0 || new TextEncoder().encode(key).byteLength > MAX_KEY_BYTES) {
      throw new Error('Volang UI key must contain 1..=4096 UTF-8 bytes');
    }
    this.insert(call, cloneWithKey(this.requireView(argument(call, 0)), key));
  }

  private flatten(root: AotView, handlerGeneration: number): {
    readonly rendered: readonly RenderedNode[];
    readonly nextNodeIdentity: number;
  } {
    let nextNodeIdentity = this.nextNodeIdentity;
    const allocateNodeIdentity = (): UiIdentity => {
      const allocated = identity(nextNodeIdentity, 1);
      nextNodeIdentity += 1;
      return allocated;
    };
    const compatible = (previous: RenderedNode | undefined, view: AotView): boolean => previous !== undefined
      && previous.view.kind === view.kind
      && previous.view.primitive === view.primitive
      && previous.view.key === view.key;
    const visit = (
      view: AotView,
      parent: UiIdentity,
      previous: RenderedNode | undefined,
    ): RenderedNode => {
      const reuse = compatible(previous, view);
      const id = reuse ? (previous as RenderedNode).id : allocateNodeIdentity();
      const priorChildren = reuse ? (previous as RenderedNode).children : [];
      const seen = new Set<string>();
      const used = new Set<RenderedNode>();
      const children = view.children.map((child, index) => {
        let candidate: RenderedNode | undefined;
        if (child.key !== undefined) {
          if (seen.has(child.key)) throw new Error(`Volang UI duplicate sibling key: ${child.key}`);
          seen.add(child.key);
          candidate = priorChildren.find((node) => node.view.key === child.key && !used.has(node));
        } else {
          const positional = priorChildren[index];
          if (positional?.view.key === undefined && !used.has(positional)) candidate = positional;
        }
        if (candidate !== undefined) used.add(candidate);
        return visit(child, id, candidate);
      });
      return { id, view, parent, handlerGeneration, children };
    };
    return {
      rendered: [visit(root, ROOT_ID, this.rendered[0])],
      nextNodeIdentity,
    };
  }

  private nodes(nodes: readonly RenderedNode[]): RenderedNode[] {
    const flattened: RenderedNode[] = [];
    const visit = (node: RenderedNode) => {
      flattened.push(node);
      for (const child of node.children) visit(child);
    };
    for (const node of nodes) visit(node);
    return flattened;
  }

  private createNode(node: RenderedNode, mutations: UiMutation[]): void {
    if (node.view.kind === 'text') {
      mutations.push({ type: 'create-text', id: node.id });
      mutations.push({ type: 'set-text', id: node.id, text: node.view.text ?? '' });
    } else {
      mutations.push({ type: 'create-element', id: node.id, primitive: node.view.primitive ?? 2 });
      for (const [property, value] of node.view.properties) {
        mutations.push({ type: 'set-property', id: node.id, property, value });
      }
      for (const [event, viewListener] of node.view.listeners) {
        const listener: UiListener = {
          event,
          handler: { index: viewListener.handler, generation: node.handlerGeneration },
          capture: viewListener.capture,
          passive: false,
          once: false,
        };
        mutations.push({ type: 'listen', id: node.id, listener });
      }
    }
  }

  private updateNode(previous: RenderedNode, next: RenderedNode, mutations: UiMutation[]): void {
    if (next.view.kind === 'text') {
      if (previous.view.text !== next.view.text) {
        mutations.push({ type: 'set-text', id: next.id, text: next.view.text ?? '' });
      }
      return;
    }
    for (const [property] of previous.view.properties) {
      if (!next.view.properties.has(property)) {
        mutations.push({ type: 'remove-property', id: next.id, property });
      }
    }
    for (const [property, value] of next.view.properties) {
      if (!sameValue(previous.view.properties.get(property), value)) {
        mutations.push({ type: 'set-property', id: next.id, property, value });
      }
    }
    for (const [event, viewListener] of previous.view.listeners) {
      const nextListener = next.view.listeners.get(event);
      if (nextListener?.handler !== viewListener.handler
        || nextListener?.capture !== viewListener.capture
        || previous.handlerGeneration !== next.handlerGeneration) {
        mutations.push({
          type: 'unlisten', id: next.id, event,
          handler: { index: viewListener.handler, generation: previous.handlerGeneration },
        });
      }
    }
    for (const [event, viewListener] of next.view.listeners) {
      const previousListener = previous.view.listeners.get(event);
      if (previousListener?.handler !== viewListener.handler
        || previousListener?.capture !== viewListener.capture
        || previous.handlerGeneration !== next.handlerGeneration) {
        mutations.push({
          type: 'listen',
          id: next.id,
          listener: {
            event,
            handler: { index: viewListener.handler, generation: next.handlerGeneration },
            capture: viewListener.capture,
            passive: false,
            once: false,
          },
        });
      }
    }
  }

  private relations(nodes: readonly RenderedNode[]): Map<string, readonly [UiIdentity, UiIdentity]> {
    const relations = new Map<string, readonly [UiIdentity, UiIdentity]>();
    const visit = (node: RenderedNode) => {
      const key = `${uiIdentityKey(node.parent)}>${uiIdentityKey(node.id)}`;
      relations.set(key, [node.parent, node.id]);
      for (const child of node.children) visit(child);
    };
    for (const node of nodes) visit(node);
    return relations;
  }

  private childOrders(nodes: readonly RenderedNode[]): Map<string, {
    readonly parent: UiIdentity;
    readonly children: readonly UiIdentity[];
  }> {
    const orders = new Map<string, { parent: UiIdentity; children: UiIdentity[] }>();
    const append = (parent: UiIdentity, child: UiIdentity) => {
      const key = uiIdentityKey(parent);
      const current = orders.get(key);
      if (current === undefined) orders.set(key, { parent, children: [child] });
      else current.children.push(child);
    };
    const visit = (node: RenderedNode) => {
      append(node.parent, node.id);
      for (const child of node.children) visit(child);
    };
    for (const node of nodes) visit(node);
    return orders;
  }

  private commitAndWait(call: AotExternCall): Promise<number> {
    if (this.pending !== undefined) throw new Error('Volang UI already has a pending event wait');
    const initial = argument(call, 1) !== 0n;
    if (initial !== (this.revision === 0n)) {
      throw new Error('Volang UI commit has an invalid initial flag');
    }
    if (this.componentStack.length !== 0) {
      throw new Error('Volang UI component scope was not exited before root commit');
    }
    const root = this.requireView(argument(call, 0));
    const nextRevision = this.revision + 1n;
    if (nextRevision > 0xffff_ffffn) throw new Error('Volang UI revision space is exhausted');
    const nextGeneration = Number(nextRevision);
    const flattened = this.flatten(root, nextGeneration);
    const next = flattened.rendered;
    const mutations: UiMutation[] = [];
    const previousNodes = this.nodes(this.rendered);
    const nextNodes = this.nodes(next);
    const previousByKey = new Map(previousNodes.map((node) => [uiIdentityKey(node.id), node]));
    const nextByKey = new Map(nextNodes.map((node) => [uiIdentityKey(node.id), node]));
    const previousRelations = this.relations(this.rendered);
    const nextRelations = this.relations(next);
    const previousOrders = this.childOrders(this.rendered);
    const nextOrders = this.childOrders(next);
    for (const [key, [parent, child]] of previousRelations) {
      if (!nextRelations.has(key)) mutations.push({ type: 'remove', parent, child });
    }
    for (const node of [...previousNodes].reverse()) {
      if (!nextByKey.has(uiIdentityKey(node.id))) mutations.push({ type: 'delete', id: node.id });
    }
    for (const node of nextNodes) {
      const previous = previousByKey.get(uiIdentityKey(node.id));
      if (previous === undefined) this.createNode(node, mutations);
      else this.updateNode(previous, node, mutations);
    }
    for (const [parentKey, order] of nextOrders) {
      const desired = order.children;
      const desiredKeys = new Set(desired.map(uiIdentityKey));
      const current = (previousOrders.get(parentKey)?.children ?? [])
        .filter((child) => desiredKeys.has(uiIdentityKey(child)));
      for (let index = 0; index < desired.length; index += 1) {
        const child = desired[index];
        if (current[index] !== undefined
          && uiIdentityKey(current[index]) === uiIdentityKey(child)) continue;
        const before = current[index];
        mutations.push(before === undefined
          ? { type: 'insert-before', parent: order.parent, child }
          : { type: 'insert-before', parent: order.parent, child, before });
        const previousIndex = current.findIndex((candidate) => uiIdentityKey(candidate) === uiIdentityKey(child));
        if (previousIndex >= 0) current.splice(previousIndex, 1);
        current.splice(index, 0, child);
      }
    }
    const batch: UiMutationBatch = {
      sessionEpoch: 1n,
      revision: nextRevision,
      mutations,
    };
    this.adapter.applyBatch(batch);
    this.state.length = this.stateCursor;
    for (const [key, index] of this.componentStateHandles) {
      if (!this.componentStateLive.has(key)) {
        this.componentStateHandles.delete(key);
        this.componentState[index] = undefined;
        this.componentStateFree.push(index);
      }
    }
    this.componentStateLive.clear();
    this.componentOccurrences.clear();
    this.componentStateCheckpoint = undefined;
    this.revision = nextRevision;
    this.generation = nextGeneration;
    this.nextNodeIdentity = flattened.nextNodeIdentity;
    this.rendered = next;
    this.liveNodes = new Map();
    const remember = (node: RenderedNode) => {
      this.liveNodes.set(uiIdentityKey(node.id), node);
      for (const child of node.children) remember(child);
    };
    for (const node of next) remember(node);
    this.onCommit?.(nextRevision, mutations.length);
    return new Promise<number>((resolve, reject) => {
      this.pending = { call, resolve, reject };
      this.deliverEvent();
    });
  }

  private deliverEvent(): void {
    const pending = this.pending;
    if (pending === undefined) return;
    try {
      if (this.invalidationPending) {
        this.invalidationPending = false;
        this.deliveredEventSequence += 1n;
        this.writeEvent(pending.call, {
          sessionEpoch: 1n,
          handler: { index: 0xffff_ffff, generation: this.generation },
          event: 17,
          target: ROOT_ID,
          sequence: this.deliveredEventSequence,
          payload: { type: 'none' },
        });
        this.pending = undefined;
        pending.resolve(0);
        return;
      }
      while (true) {
        const frame = this.adapter.shiftEventFrame();
        if (frame === undefined) return;
        const event = decodeUiEvent(frame);
        const node = this.liveNodes.get(uiIdentityKey(event.target));
        const handler = node?.view.listeners.get(event.event)?.handler;
        if (event.sessionEpoch !== 1n
          || event.handler.generation !== this.generation
          || handler === undefined
          || handler !== event.handler.index) continue;
        this.deliveredEventSequence += 1n;
        this.writeEvent(pending.call, { ...event, sequence: this.deliveredEventSequence });
        this.pending = undefined;
        pending.resolve(0);
        return;
      }
    } catch (cause) {
      this.pending = undefined;
      pending.reject(cause);
    }
  }

  private writeEvent(call: AotExternCall, event: UiEventEnvelope): void {
    const values: Array<bigint | number | string> = [
      BigInt(event.event), event.sequence, '', 0n, '', 0, 0, '', 0n, 0n, 0n,
      0n, 0n, 0n, 0n, 0, 0, 0n, 0n, 0n,
    ];
    const modifiers = (value: { shift: boolean; control: boolean; alt: boolean; meta: boolean }) => (
      Number(value.shift) | (Number(value.control) << 1) | (Number(value.alt) << 2)
      | (Number(value.meta) << 3)
    );
    switch (event.payload.type) {
      case 'text': values[2] = event.payload.value; break;
      case 'bytes': values[2] = new TextDecoder().decode(event.payload.value); break;
      case 'toggle': values[3] = BigInt(event.payload.value); break;
      case 'scalar': values[6] = Number(event.payload.value); break;
      case 'key':
        values[4] = event.payload.key;
        values[7] = event.payload.code;
        values[8] = BigInt(modifiers(event.payload.modifiers));
        values[9] = BigInt(event.payload.repeat);
        values[10] = BigInt(event.payload.composing);
        break;
      case 'pointer':
        values[5] = event.payload.x;
        values[6] = event.payload.y;
        values[8] = BigInt(modifiers(event.payload.modifiers));
        values[11] = BigInt(event.payload.button);
        values[12] = BigInt(event.payload.buttons);
        values[13] = event.payload.pointerId;
        values[14] = BigInt({ unknown: 0, mouse: 1, pen: 2, touch: 3 }[event.payload.kind]);
        break;
      case 'scroll':
        values[5] = event.payload.x;
        values[6] = event.payload.y;
        values[8] = BigInt(modifiers(event.payload.modifiers));
        values[15] = event.payload.deltaX;
        values[16] = event.payload.deltaY;
        values[17] = BigInt({ pixel: 0, line: 1, page: 2 }[event.payload.unit]);
        break;
      case 'composition':
      case 'text-input':
        values[2] = event.payload.value;
        values[18] = BigInt(event.payload.selectionStartUtf16);
        values[19] = BigInt(event.payload.selectionLengthUtf16);
        break;
      case 'none': break;
    }
    call.writeSlot(call.destination, BigInt(event.handler.index));
    for (let index = 0; index < values.length; index += 1) {
      const slot = call.destination + index + 1;
      const value = values[index];
      if (typeof value === 'string') call.writeSlot(slot, call.allocateString(value));
      else if (typeof value === 'number') call.writeFloat64(slot, value);
      else call.writeSlot(slot, value);
    }
  }
}

function browserLocation(window: Window): string {
  return `${window.location.pathname}${window.location.search}${window.location.hash}`;
}

function validateNavigationPath(path: string): void {
  if (!path.startsWith('/') || path.startsWith('//') || path.length > 16 * 1024
    || [...path].some((character) => character === '\\' || character.charCodeAt(0) < 0x20)) {
    throw new Error('Volang UI navigation path is invalid');
  }
}

function checkedNavigationUrl(window: Window, path: string): URL {
  validateNavigationPath(path);
  const url = new URL(path, window.location.href);
  if (url.origin !== window.location.origin) {
    throw new Error('Volang UI navigation must stay on the current origin');
  }
  return url;
}

/** Construct the official AOT UI host and its authenticated extern table. */
export function connectAotUiToDom(
  root: HTMLElement,
  options: AotUiHostOptions = {},
): { readonly host: AotUiHost; readonly externs: NonNullable<AotRunOptions['externs']> } {
  const host = new AotUiHost(root, options);
  return { host, externs: host.externs() };
}

export interface AotUiHostOptions extends UiDomAdapterOptions {
  readonly systemHost?: UiSystemHost;
  readonly onCommit?: (revision: bigint, mutationCount: number) => void;
}
