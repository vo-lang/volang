import assert from "node:assert/strict";
import { readFileSync } from "node:fs";

import {
  MAX_OPTIONAL_SECTIONS,
  OPTIONAL_SECTION_GOLDEN,
  decodeOptionalSections,
  encodeOptionalSection,
} from "../../../lang/protocol/app-runtime/generated/app_protocol.ts";

const encoder = new TextEncoder();
const encoded = [
  encodeOptionalSection(1, encoder.encode("known-a")),
  encodeOptionalSection(99, encoder.encode("future")),
  encodeOptionalSection(2, encoder.encode("known-b")),
];
const combined = new Uint8Array(encoded.reduce((sum, item) => sum + item.byteLength, 0));
let offset = 0;
for (const item of encoded) {
  combined.set(item, offset);
  offset += item.byteLength;
}

const golden = new Uint8Array(
  readFileSync(
    new URL(
      "../../../lang/protocol/app-runtime/generated/golden-optional-sections.bin",
      import.meta.url,
    ),
  ),
);
assert.deepEqual([...combined], [...golden]);
assert.deepEqual([...combined], [...OPTIONAL_SECTION_GOLDEN]);

const decoded = decodeOptionalSections(golden);
assert.deepEqual(
  decoded.map((section) => section.kind),
  [1, 99, 2],
);
assert.equal(new TextDecoder().decode(decoded[0].payload), "known-a");
assert.equal(new TextDecoder().decode(decoded[2].payload), "known-b");

const forgedLength = golden.slice();
forgedLength.set([0xff, 0xff, 0xff, 0xff], 2);
assert.throws(() => decodeOptionalSections(forgedLength), /truncated optional section payload/);

const tooMany = new Uint8Array((MAX_OPTIONAL_SECTIONS + 1) * 6);
for (let index = 0; index <= MAX_OPTIONAL_SECTIONS; index += 1) {
  tooMany.set(encodeOptionalSection(index + 1, new Uint8Array()), index * 6);
}
assert.throws(() => decodeOptionalSections(tooMany), /too many optional sections/);

console.log("app protocol optional-section TypeScript golden: ok");
