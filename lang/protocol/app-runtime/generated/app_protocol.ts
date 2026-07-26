// @generated from lang/protocol/app-runtime/app.schema.toml
export const SCHEMA_ID = "vo.app.runtime" as const;
export const APP_PROTOCOL_MAJOR = 1;
export const APP_PROTOCOL_MINOR = 0;
export const APP_PROTOCOL_MAGIC = 0x50414f56;
export const HEADER_BYTES = 64;
export const MAX_PACKET_BYTES = 8388608;
export const MAX_PAYLOAD_BYTES = 8388544;
export const MAX_SUPPORTED_MINORS = 16;
export const MAX_OPTIONAL_SECTIONS = 64;
export const OPTIONAL_SECTION_KIND_BITS = 16;
export const OPTIONAL_SECTION_LENGTH_BITS = 32;
export const OPTIONAL_SECTION_HEADER_BYTES = 6;
export const OPTIONAL_SECTION_GOLDEN = [1, 0, 7, 0, 0, 0, 107, 110, 111, 119, 110, 45, 97, 99, 0, 6, 0, 0, 0, 102, 117, 116, 117, 114, 101, 2, 0, 7, 0, 0, 0, 107, 110, 111, 119, 110, 45, 98] as const;
export const MAJOR_COMPAT_FINGERPRINT = [236, 192, 250, 210, 171, 8, 143, 107, 250, 190, 6, 39, 21, 101, 49, 5, 2, 204, 18, 210, 137, 153, 29, 12, 0, 50, 45, 45, 108, 93, 65, 228] as const;
export const EXACT_SCHEMA_FINGERPRINT = [174, 162, 36, 120, 214, 168, 40, 91, 246, 86, 245, 237, 106, 163, 185, 233, 213, 25, 208, 54, 41, 120, 127, 160, 157, 222, 55, 190, 247, 225, 124, 99] as const;
export const SCHEMA_IDENTITY = [19, 126, 214, 76, 9, 216, 94, 81, 252, 150, 2, 215, 82, 95, 207, 105] as const;
export type U64 = bigint;
export interface GenerationalHandle { readonly index: number; readonly generation: number; }
export interface OptionalSection { readonly kind: number; readonly payload: Uint8Array; }
export function encodeOptionalSection(kind: number, payload: Uint8Array): Uint8Array {
if (!Number.isInteger(kind) || kind <= 0 || kind > 0xffff) throw new RangeError("invalid optional section kind");
if (payload.byteLength > MAX_PAYLOAD_BYTES) throw new RangeError("optional section payload too large");
const output = new Uint8Array(OPTIONAL_SECTION_HEADER_BYTES + payload.byteLength);
const view = new DataView(output.buffer);
view.setUint16(0, kind, true);
view.setUint32(2, payload.byteLength, true);
output.set(payload, OPTIONAL_SECTION_HEADER_BYTES);
return output;
}
export function decodeOptionalSections(input: Uint8Array): readonly OptionalSection[] {
if (input.byteLength > MAX_PAYLOAD_BYTES) throw new RangeError("optional section payload too large");
const sections: OptionalSection[] = [];
let offset = 0;
while (offset < input.byteLength) {
if (sections.length === MAX_OPTIONAL_SECTIONS) throw new RangeError("too many optional sections");
if (input.byteLength - offset < OPTIONAL_SECTION_HEADER_BYTES) throw new RangeError("truncated optional section header");
const view = new DataView(input.buffer, input.byteOffset + offset, OPTIONAL_SECTION_HEADER_BYTES);
const kind = view.getUint16(0, true);
if (kind === 0) throw new RangeError("invalid optional section kind");
const length = view.getUint32(2, true);
const payloadStart = offset + OPTIONAL_SECTION_HEADER_BYTES;
const payloadEnd = payloadStart + length;
if (!Number.isSafeInteger(payloadEnd) || payloadEnd > input.byteLength) throw new RangeError("truncated optional section payload");
sections.push({ kind, payload: input.subarray(payloadStart, payloadEnd) });
offset = payloadEnd;
}
return sections;
}
export const CAPABILITY_APP_TIMER_ONCE = "app.timer.once" as const;
export const enum MessageKind {
  ChannelOpen = 1,
  ChannelAccept = 2,
  ChannelReject = 3,
  RequestCancel = 4,
  SessionClose = 5,
  SessionCloseAck = 6,
  FrameworkPayload = 32,
  PlatformInput = 33,
  PlatformRequest = 34,
  PlatformCompletion = 35,
  Diagnostics = 63,
}
export interface AppEnvelopeHeader {
readonly session: GenerationalHandle;
readonly sessionEpoch: U64;
readonly channel: GenerationalHandle;
readonly channelEpoch: U64;
readonly messageKind: MessageKind;
readonly flags: number;
readonly sequence: U64;
readonly requestId: U64;
readonly payloadLength: number;
}
export interface AppEnvelope { readonly header: AppEnvelopeHeader; readonly payload: Uint8Array; }
export function messageKindFromWire(value: number): MessageKind | null {
switch (value) {
    case 1: return MessageKind.ChannelOpen;
    case 2: return MessageKind.ChannelAccept;
    case 3: return MessageKind.ChannelReject;
    case 4: return MessageKind.RequestCancel;
    case 5: return MessageKind.SessionClose;
    case 6: return MessageKind.SessionCloseAck;
    case 32: return MessageKind.FrameworkPayload;
    case 33: return MessageKind.PlatformInput;
    case 34: return MessageKind.PlatformRequest;
    case 35: return MessageKind.PlatformCompletion;
    case 63: return MessageKind.Diagnostics;
    default: return null;
}
}
export function decodeAppEnvelope(input: Uint8Array): AppEnvelope {
if (!(input instanceof Uint8Array)) throw new TypeError("App envelope must be Uint8Array");
if (input.byteLength < HEADER_BYTES) throw new RangeError("truncated App envelope header");
if (input.byteLength > MAX_PACKET_BYTES) throw new RangeError("App envelope exceeds packet limit");
const view = new DataView(input.buffer, input.byteOffset, HEADER_BYTES);
if (view.getUint32(0, true) !== APP_PROTOCOL_MAGIC) throw new RangeError("invalid App envelope magic");
if (view.getUint16(4, true) !== APP_PROTOCOL_MAJOR) throw new RangeError("unsupported App protocol major");
const minor = view.getUint16(6, true);
if (minor > APP_PROTOCOL_MINOR || minor >= MAX_SUPPORTED_MINORS) throw new RangeError("unsupported App protocol minor");
const session = readHandle(view, 8);
const sessionEpoch = view.getBigUint64(16, true);
const channel = readHandle(view, 24);
const channelEpoch = view.getBigUint64(32, true);
if (sessionEpoch === 0n || channelEpoch === 0n) throw new RangeError("invalid App envelope epoch");
const messageKind = messageKindFromWire(view.getUint16(40, true));
if (messageKind === null) throw new RangeError("unknown App message kind");
const flags = view.getUint16(42, true);
const sequence = view.getBigUint64(44, true);
const requestId = view.getBigUint64(52, true);
const payloadLength = view.getUint32(60, true);
if (payloadLength > MAX_PAYLOAD_BYTES || input.byteLength !== HEADER_BYTES + payloadLength) {
throw new RangeError("App envelope payload length mismatch");
}
return {
header: { session, sessionEpoch, channel, channelEpoch, messageKind, flags, sequence, requestId, payloadLength },
payload: input.subarray(HEADER_BYTES),
};
}
export function encodeAppEnvelope(
header: Omit<AppEnvelopeHeader, "payloadLength">,
payload: Uint8Array,
): Uint8Array {
if (!(payload instanceof Uint8Array)) throw new TypeError("App envelope payload must be Uint8Array");
if (payload.byteLength > MAX_PAYLOAD_BYTES) throw new RangeError("App envelope payload exceeds limit");
validateHandle(header.session);
validateHandle(header.channel);
validateU64(header.sessionEpoch, "session epoch", true);
validateU64(header.channelEpoch, "channel epoch", true);
validateU64(header.sequence, "sequence", false);
validateU64(header.requestId, "request ID", false);
if (messageKindFromWire(header.messageKind) === null) throw new RangeError("unknown App message kind");
if (!Number.isInteger(header.flags) || header.flags < 0 || header.flags > 0xffff) throw new RangeError("invalid App envelope flags");
const output = new Uint8Array(HEADER_BYTES + payload.byteLength);
const view = new DataView(output.buffer);
view.setUint32(0, APP_PROTOCOL_MAGIC, true);
view.setUint16(4, APP_PROTOCOL_MAJOR, true);
view.setUint16(6, APP_PROTOCOL_MINOR, true);
writeHandle(view, 8, header.session);
view.setBigUint64(16, header.sessionEpoch, true);
writeHandle(view, 24, header.channel);
view.setBigUint64(32, header.channelEpoch, true);
view.setUint16(40, header.messageKind, true);
view.setUint16(42, header.flags, true);
view.setBigUint64(44, header.sequence, true);
view.setBigUint64(52, header.requestId, true);
view.setUint32(60, payload.byteLength, true);
output.set(payload, HEADER_BYTES);
return output;
}
function readHandle(view: DataView, offset: number): GenerationalHandle {
const handle = { index: view.getUint32(offset, true), generation: view.getUint32(offset + 4, true) };
validateHandle(handle);
return handle;
}
function writeHandle(view: DataView, offset: number, handle: GenerationalHandle): void {
view.setUint32(offset, handle.index, true);
view.setUint32(offset + 4, handle.generation, true);
}
function validateHandle(handle: GenerationalHandle): void {
if (!Number.isInteger(handle.index) || handle.index < 0 || handle.index >= 0xffffffff
|| !Number.isInteger(handle.generation) || handle.generation < 1 || handle.generation > 0xffffffff) {
throw new RangeError("invalid App envelope handle");
}
}
function validateU64(value: bigint, label: string, nonzero: boolean): void {
if (typeof value !== "bigint" || value < (nonzero ? 1n : 0n) || value > 0xffffffffffffffffn) {
throw new RangeError(`invalid App envelope ${label}`);
}
}
