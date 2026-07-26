import {
  MessageKind,
  decodeFrameworkPacket,
} from "../../../../voplay/protocol/generated/voplay_protocol.js";
import {
  BROWSER_GAMEPAD_DEVICE_BASE,
  encodePlatformInput,
  type BrowserPlatformInputEvent,
  type BrowserSurfaceControl,
} from "../../../../voplay/web/src/framework_lane.js";
import {
  BrowserGamepadInputSource,
  type BrowserGamepadEvent,
} from "../../../../voplay/web/src/gamepad_input.js";

interface SmokeReport {
  readonly complete: boolean;
  readonly passed: boolean;
  readonly cases: readonly string[];
  readonly detail: string;
}

declare global {
  interface Window {
    __voplayBrowserInputSmoke?: SmokeReport;
  }
}

const output = document.getElementById("results");
const cases: string[] = [];

try {
  await smokeTypedInputPackets();
  cases.push("typed pointer/key/text/IME/focus/gamepad packets");
  await smokeGamepadPolling();
  cases.push("RAF gamepad generation and synthesized releases");
  finish({ complete: true, passed: true, cases, detail: "ok" });
} catch (error) {
  finish({
    complete: true,
    passed: false,
    cases,
    detail: error instanceof Error ? `${error.name}: ${error.message}` : String(error),
  });
}

function finish(report: SmokeReport): void {
  window.__voplayBrowserInputSmoke = report;
  document.documentElement.dataset.smoke = report.passed ? "passed" : "failed";
  if (output !== null) output.textContent = JSON.stringify(report, null, 2);
}

async function smokeTypedInputPackets(): Promise<void> {
  const control = surfaceControl();
  const surface = {
    sessionId: 9,
    session: control.session,
    sessionEpoch: 101n,
    window: control.window,
    view: control.view,
    surface: control.surface,
  };
  const variants: Array<Partial<BrowserPlatformInputEvent> & Pick<BrowserPlatformInputEvent, "type">> = [
    {
      type: "pointerDown",
      pointerId: 7,
      pointerType: "pen",
      xMilli: 11_000,
      yMilli: 12_000,
      localXMilli: 1_000,
      localYMilli: 2_000,
      button: 0,
      buttons: 1,
      pressureQ16: 32_768,
      tiltX: -12,
      tiltY: 18,
    },
    { type: "pointerMove", pointerId: 7, pointerType: "pen", buttons: 1 },
    { type: "pointerUp", pointerId: 7, pointerType: "pen", button: 0 },
    { type: "pointerCancel", pointerId: 7, pointerType: "pen", synthesized: true },
    { type: "wheel", deltaXMilli: 125, deltaYMilli: -250, deltaMode: 0 },
    { type: "keyDown", physical: "KeyA", logical: "a", shift: true },
    { type: "keyUp", physical: "KeyA", logical: "a" },
    { type: "text", text: "火", inputType: "insertText" },
    { type: "compositionStart", text: "", inputType: "insertCompositionText", composing: true },
    { type: "compositionUpdate", text: "火", inputType: "insertCompositionText", composing: true },
    { type: "compositionEnd", text: "火", inputType: "insertCompositionText" },
    { type: "focus", focused: true },
    {
      type: "gamepadConnect",
      gamepadIndex: 2,
      gamepadGeneration: 3,
      gamepadId: "Voplay smoke pad",
      gamepadMapping: "standard",
    },
    {
      type: "gamepadButton",
      gamepadIndex: 2,
      gamepadGeneration: 3,
      gamepadControl: 1,
      gamepadValueQ16: 65_535,
    },
    {
      type: "gamepadAxis",
      gamepadIndex: 2,
      gamepadGeneration: 3,
      gamepadControl: 0,
      gamepadValueQ16: -16_384,
    },
    { type: "gamepadDisconnect", gamepadIndex: 2, gamepadGeneration: 3, synthesized: true },
  ];

  for (let index = 0; index < variants.length; index += 1) {
    const event: BrowserPlatformInputEvent = {
      ...variants[index],
      type: variants[index]!.type,
      sequence: BigInt(index + 1),
      timestampMicros: BigInt(10_000 + index),
      surface,
    };
    const encoded = encodePlatformInput(event, control, BigInt(index + 1));
    const packet = decodeFrameworkPacket(encoded);
    assert(packet.header.kind === MessageKind.PlatformInput, `${event.type} packet kind`);
    assert(packet.header.sequence === BigInt(index + 1), `${event.type} packet sequence`);
    assert(packet.payload.byteLength >= 80, `${event.type} packet payload`);
    const view = new DataView(
      packet.payload.buffer,
      packet.payload.byteOffset,
      packet.payload.byteLength,
    );
    assert(
      view.getUint32(76, true) === packet.payload.byteLength - 80,
      `${event.type} detail length`,
    );
    if (event.type.startsWith("gamepad")) {
      assert(
        view.getUint32(60, true) === BROWSER_GAMEPAD_DEVICE_BASE + 2,
        `${event.type} device namespace`,
      );
      assert(view.getUint32(64, true) === 3, `${event.type} device generation`);
    }
  }

  expectThrow(() => encodePlatformInput({
    type: "keyDown",
    sequence: 99n,
    timestampMicros: 99n,
    surface: { ...surface, view: { index: 999, generation: 1 } },
  }, control, 99n), "Surface route mismatch");
}

async function smokeGamepadPolling(): Promise<void> {
  const original = Object.getOwnPropertyDescriptor(Navigator.prototype, "getGamepads");
  const events: BrowserGamepadEvent[] = [];
  let pads: Array<Gamepad | null> = [];
  let enabled = true;
  Object.defineProperty(Navigator.prototype, "getGamepads", {
    configurable: true,
    value: () => pads,
  });
  const source = new BrowserGamepadInputSource(
    (event) => events.push(event),
    () => enabled,
  );
  try {
    pads = [gamepad("pad-a", 0, [0.5], [0.25])];
    source.start();
    await animationFrames(2);
    assert(events.some((event) => event.type === "gamepadConnect"), "gamepad connect");
    assert(events.some((event) => event.type === "gamepadButton"), "gamepad button");
    assert(events.some((event) => event.type === "gamepadAxis"), "gamepad axis");
    assert(source.generation(0) === 1, "initial gamepad generation");

    events.length = 0;
    pads = [gamepad("pad-b", 0, [1], [-0.75])];
    await animationFrames(2);
    assert(
      events.some((event) =>
        event.type === "gamepadDisconnect"
        && event.gamepadGeneration === 1
        && event.synthesized === true),
      "replacement disconnects old generation",
    );
    assert(
      events.some((event) =>
        event.type === "gamepadConnect" && event.gamepadGeneration === 2),
      "replacement creates new generation",
    );

    events.length = 0;
    enabled = false;
    await animationFrames(2);
    assert(
      events.some((event) =>
        event.type === "gamepadButton"
        && event.gamepadValueQ16 === 0
        && event.synthesized === true),
      "disabled source synthesizes button release",
    );
    assert(
      events.some((event) =>
        event.type === "gamepadAxis"
        && event.gamepadValueQ16 === 0
        && event.synthesized === true),
      "disabled source synthesizes axis release",
    );
    assert(
      events.some((event) =>
        event.type === "gamepadDisconnect" && event.synthesized === true),
      "disabled source synthesizes disconnect",
    );

    source.close();
    const snapshot = source.ownerSnapshot();
    assert(snapshot.closed && !snapshot.polling && snapshot.connected === 0, "gamepad owner close");
  } finally {
    source.close(false);
    if (original === undefined) {
      delete (Navigator.prototype as { getGamepads?: unknown }).getGamepads;
    } else {
      Object.defineProperty(Navigator.prototype, "getGamepads", original);
    }
  }
}

function surfaceControl(): BrowserSurfaceControl {
  return {
    action: "attach",
    session: { index: 1, generation: 1 },
    window: { index: 2, generation: 1 },
    view: { index: 3, generation: 1 },
    surface: { index: 4, generation: 1 },
    engine: { index: 5, generation: 1 },
    domain: { index: 6, generation: 1 },
    renderEndpoint: { index: 7, generation: 1 },
    deviceGeneration: 1n,
    controlRevision: 1n,
    timingRevision: 1n,
    width: 640,
    height: 360,
    scaleNumerator: 2,
    scaleDenominator: 1,
    inputPolicy: 3,
    channelEpoch: 11n,
    sequence: 1n,
  };
}

function gamepad(
  id: string,
  index: number,
  buttons: readonly number[],
  axes: readonly number[],
): Gamepad {
  return {
    id,
    index,
    connected: true,
    mapping: "standard",
    timestamp: performance.now(),
    buttons: buttons.map((value) => ({
      pressed: value > 0.5,
      touched: value > 0,
      value,
    })),
    axes: [...axes],
    vibrationActuator: null,
  } as unknown as Gamepad;
}

async function animationFrames(count: number): Promise<void> {
  for (let index = 0; index < count; index += 1) {
    await new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
  }
}

function expectThrow(operation: () => void, message: string): void {
  let thrown = false;
  try {
    operation();
  } catch (error) {
    thrown = error instanceof Error && error.message.includes(message);
  }
  assert(thrown, `expected ${message}`);
}

function assert(condition: boolean, message: string): asserts condition {
  if (!condition) throw new Error(message);
}
