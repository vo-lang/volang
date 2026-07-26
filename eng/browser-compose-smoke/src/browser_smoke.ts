import {
  AppCompositionHost,
  type AppSurfaceDescriptor,
  type AppSurfaceIdentity,
  type AppSurfaceInputEvent,
} from "../../../apps/studio/src/lib/gui/app_composition_host";
import {
  DomRenderer,
  type DomPatchBatch,
  type DomRendererIdentity,
} from "../../../../vogui/js/src/dom_renderer";
import { Canvas2dGpuAdapter } from "../../../../voplay/web/src/canvas2d_adapter";
import {
  BrowserSurfaceHost,
  type BrowserFrameSubmission,
  type BrowserSurfaceId,
} from "../../../../voplay/web/src/platform_surface";

interface SmokeReport {
  readonly complete: boolean;
  readonly passed: boolean;
  readonly cases: readonly string[];
  readonly detail: string;
}

declare global {
  interface Window {
    __voplayVoguiBrowserComposeSmoke?: SmokeReport;
  }
}

const root = requireElement("composition", HTMLDivElement);
const output = requireElement("results", HTMLPreElement);
const cases: string[] = [];

try {
  const composition = new AppCompositionHost(77, root, 4);
  const gameIdentity = surfaceIdentity(4);
  const uiIdentity = surfaceIdentity(5);
  const game = composition.attach("voplay", descriptor(
    gameIdentity,
    "canvas",
    0,
    "opaque",
    "Voplay game",
  ));
  const ui = composition.attach("vogui", descriptor(
    uiIdentity,
    "dom",
    1,
    "transparent",
    "Vogui overlay",
  ));
  assert(game.element instanceof HTMLCanvasElement, "game canvas lease");
  assert(ui.element instanceof HTMLDivElement, "Vogui DOM lease");
  assert(
    composition.layers().map((layer) => layer.descriptor.label).join(",")
      === "Voplay game,Vogui overlay",
    "shared View layer order",
  );

  const adapter = new Canvas2dGpuAdapter({
    deviceGeneration: 1n,
    maxCommands: 8,
    maxCommandBytes: 1024,
  });
  const surfaceHost = new BrowserSurfaceHost(adapter, {
    maxSurfaces: 1,
    maxCommandBytes: 1024,
  });
  const gameSurface = browserSurface();
  surfaceHost.attach(gameSurface, game.element, {
    width: 640,
    height: 360,
    scaleNumerator: 1,
    scaleDenominator: 1,
  });
  const submitted = surfaceHost.submit(frame(
    gameSurface,
    portableFrame(
      [1, 14, 28, 48, 255],
      rectangleCommand(32, 32, 160, 96, [34, 197, 94, 255]),
    ),
  ));
  assert(
    surfaceHost.present(gameSurface, 1n, submitted.fenceValue, 90n, 100n)
      === "presented",
    "Voplay Canvas2D present",
  );
  const context = game.element.getContext("2d");
  assert(context !== null, "Canvas2D context");
  assertPixel(
    context.getImageData(48, 48, 1, 1).data,
    [34, 197, 94, 255],
    "Voplay layer pixel",
  );
  cases.push("Voplay Canvas2D present in shared App View");

  const rendererIdentity: DomRendererIdentity = {
    session: { index: 1, generation: 1 },
    root: { index: 20, generation: 1 },
    uiRootEpoch: 1,
    appCodeEpoch: 1n,
    rendererGeneration: { index: 21, generation: 1 },
  };
  const renderer = new DomRenderer(
    ui.element,
    "shadow",
    rendererIdentity,
    {
      maxNodes: 8,
      maxMutations: 32,
      maxPropertiesPerNode: 16,
      maxStringBytes: 4096,
    },
    () => {},
  );
  renderer.apply(patch(rendererIdentity, [
    {
      kind: "create",
      node: rendererIdentity.root,
      index: 0,
    },
    {
      kind: "setKind",
      node: rendererIdentity.root,
      nodeKind: { kind: "element", tag: "button" },
    },
    {
      kind: "setProperties",
      node: rendererIdentity.root,
      properties: [
        { field: "text", value: "Pause" },
        { field: "role", value: "button" },
        { field: "ariaLabel", value: "Pause game" },
        { field: "tabIndex", value: 0 },
        { field: "styleToken", name: "display", value: "block" },
        { field: "styleToken", name: "width", value: "120px" },
        { field: "styleToken", name: "height", value: "64px" },
        { field: "styleToken", name: "background-color", value: "#f4a340ff" },
      ],
    },
    {
      kind: "bindEvent",
      node: rendererIdentity.root,
      eventType: "pointerDown",
      token: { index: 22, generation: 1 },
      policy: { preventDefault: true, stopPropagation: false },
    },
  ]));
  await animationFrame();
  const button = ui.element.shadowRoot?.querySelector<HTMLElement>(
    "[data-vogui-node]",
  );
  assert(button !== null && button !== undefined, "Vogui retained button");
  assert(button.getAttribute("aria-label") === "Pause game", "Vogui semantics");
  const regions = renderer.interactiveHitRegions();
  assert(regions.length === 1, "Vogui interactive hit region");
  composition.publishHitRegions("vogui", uiIdentity, 1n, regions);
  cases.push("Vogui retained DOM and semantic overlay in shared App View");

  const gameEvents: AppSurfaceInputEvent[] = [];
  const uiEvents: AppSurfaceInputEvent[] = [];
  const unsubscribeGame = composition.subscribeInput(
    "voplay",
    (event) => gameEvents.push(event),
  );
  const unsubscribeUi = composition.subscribeInput(
    "vogui",
    (event) => uiEvents.push(event),
  );
  const rootBounds = root.getBoundingClientRect();
  const buttonBounds = button.getBoundingClientRect();
  root.dispatchEvent(new PointerEvent("pointerdown", {
    bubbles: true,
    pointerId: 7,
    pointerType: "mouse",
    clientX: buttonBounds.left + 8,
    clientY: buttonBounds.top + 8,
    button: 0,
    buttons: 1,
  }));
  assert(
    uiEvents.some((event) => event.type === "pointerDown"),
    "Vogui receives its opaque hit",
  );
  assert(
    !gameEvents.some((event) => event.type === "pointerDown"),
    "opaque UI hit blocks game",
  );

  root.dispatchEvent(new PointerEvent("pointerdown", {
    bubbles: true,
    pointerId: 8,
    pointerType: "mouse",
    clientX: rootBounds.right - 16,
    clientY: rootBounds.bottom - 16,
    button: 0,
    buttons: 1,
  }));
  assert(
    gameEvents.some((event) => (
      event.type === "pointerDown" && event.pointerId === 8
    )),
    "transparent UI region routes to game",
  );
  cases.push("UI/game pointer arbitration by typed hit regions");

  composition.setLowerInputSuspended("vogui", uiIdentity, true);
  assert(
    game.element.dataset.appSurfaceSuspended === "true",
    "modal overlay suspends lower game input",
  );
  composition.setLowerInputSuspended("vogui", uiIdentity, false);
  assert(
    game.element.dataset.appSurfaceSuspended === undefined,
    "modal close restores lower game input",
  );
  cases.push("modal focus and lower-layer suspension recovery");

  unsubscribeUi();
  unsubscribeGame();
  renderer.close();
  surfaceHost.detach(gameSurface);
  assert(surfaceHost.close() === 0, "Voplay surface close");
  composition.closeOwner("vogui");
  composition.closeOwner("voplay");
  assert(composition.layers().length === 0, "composition owners released");
  composition.close();
  cases.push("reverse owner shutdown leaves no shared surfaces");

  finish({ complete: true, passed: true, cases, detail: "ok" });
} catch (error) {
  finish({
    complete: true,
    passed: false,
    cases,
    detail: error instanceof Error
      ? `${error.name}: ${error.message}`
      : String(error),
  });
}

function finish(report: SmokeReport): void {
  window.__voplayVoguiBrowserComposeSmoke = report;
  document.documentElement.dataset.smoke = report.passed ? "passed" : "failed";
  output.textContent = JSON.stringify(report, null, 2);
}

function surfaceIdentity(surfaceIndex: number): AppSurfaceIdentity {
  return {
    sessionId: 77,
    session: { index: 1, generation: 1 },
    sessionEpoch: 1n,
    window: { index: 2, generation: 1 },
    view: { index: 3, generation: 1 },
    surface: { index: surfaceIndex, generation: 1 },
  };
}

function descriptor(
  identity: AppSurfaceIdentity,
  kind: "canvas" | "dom",
  layer: number,
  input: "opaque" | "transparent",
  label: string,
): AppSurfaceDescriptor {
  return { identity, kind, layer, input, label };
}

function browserSurface(): BrowserSurfaceId {
  return {
    engine: {
      session: { index: 1, generation: 1 },
      engine: { index: 2, generation: 1 },
    },
    surface: { index: 3, generation: 1 },
    domain: { index: 4, generation: 1 },
  };
}

function frame(
  surface: BrowserSurfaceId,
  commands: Uint8Array,
): BrowserFrameSubmission {
  return {
    surface,
    pulseId: 1n,
    frameId: 1n,
    renderEndpoint: { index: 5, generation: 1 },
    deviceGeneration: 1n,
    requiredRenderRevision: 1n,
    requiredControlRevision: 1n,
    graphSignature: 1n,
    commands,
  };
}

function portableFrame(...commands: readonly number[][]): Uint8Array {
  const size = 8 + commands.reduce(
    (total, command) => total + command.length,
    0,
  );
  const bytes = new Uint8Array(size);
  bytes.set([0x56, 0x46, 0x43, 0x31]);
  new DataView(bytes.buffer).setUint32(4, commands.length, true);
  let offset = 8;
  for (const command of commands) {
    bytes.set(command, offset);
    offset += command.length;
  }
  return bytes;
}

function rectangleCommand(
  x: number,
  y: number,
  width: number,
  height: number,
  color: readonly [number, number, number, number],
): number[] {
  const bytes = new Uint8Array(21);
  const view = new DataView(bytes.buffer);
  view.setUint8(0, 2);
  view.setUint32(1, x, true);
  view.setUint32(5, y, true);
  view.setUint32(9, width, true);
  view.setUint32(13, height, true);
  bytes.set(color, 17);
  return [...bytes];
}

function patch(
  identity: DomRendererIdentity,
  mutations: DomPatchBatch["mutations"],
): DomPatchBatch {
  return {
    identity,
    baseRevision: 0n,
    newRevision: 1n,
    replacement: true,
    mutations,
  };
}

function assertPixel(
  actual: Uint8ClampedArray,
  expected: readonly number[],
  message: string,
): void {
  assert(
    expected.every((value, index) => actual[index] === value),
    message,
  );
}

function animationFrame(): Promise<void> {
  return new Promise((resolve) => requestAnimationFrame(() => resolve()));
}

function assert(condition: boolean, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function requireElement<T extends Element>(
  id: string,
  constructor: { new (): T },
): T {
  const element = document.getElementById(id);
  if (!(element instanceof constructor)) throw new Error(`missing #${id}`);
  return element;
}
