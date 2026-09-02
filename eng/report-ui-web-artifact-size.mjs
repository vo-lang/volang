#!/usr/bin/env node

import { createHash } from "node:crypto";
import { readFileSync, writeFileSync } from "node:fs";
import { brotliCompressSync, constants, gzipSync } from "node:zlib";

const options = new Map();
for (let index = 2; index < process.argv.length; index += 2) {
  const name = process.argv[index];
  const value = process.argv[index + 1];
  if (!name?.startsWith("--") || value === undefined) {
    throw new Error("arguments must be --name value pairs");
  }
  options.set(name.slice(2), value);
}

function required(name) {
  const value = options.get(name);
  if (!value) throw new Error(`missing --${name}`);
  return value;
}

function byteLimit(name) {
  const value = Number(required(name));
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`--${name} must be a positive byte count`);
  }
  return value;
}

const artifactPath = required("artifact");
const artifact = readFileSync(artifactPath);
const gzipBytes = gzipSync(artifact, { level: 9, mtime: 0 }).byteLength;
const brotliBytes = brotliCompressSync(artifact, {
  params: { [constants.BROTLI_PARAM_QUALITY]: 11 },
}).byteLength;
const brotliLimit = byteLimit("brotli-limit");
const gzipLimit = options.has("gzip-limit") ? byteLimit("gzip-limit") : null;
const report = {
  schema: "volang.ui.web-artifact-size.v1",
  label: required("label"),
  artifact: artifactPath,
  raw_bytes: artifact.byteLength,
  gzip_bytes: gzipBytes,
  gzip_limit_bytes: gzipLimit,
  brotli_bytes: brotliBytes,
  brotli_limit_bytes: brotliLimit,
  remaining_bytes: brotliLimit - brotliBytes,
  artifact_sha256: createHash("sha256").update(artifact).digest("hex"),
};
const encoded = `${JSON.stringify(report)}\n`;
const output = options.get("output");
if (output) writeFileSync(output, encoded);
process.stdout.write(encoded);
if (brotliBytes > brotliLimit) {
  throw new Error(`${report.label} Brotli image ${brotliBytes} exceeds ${brotliLimit}`);
}
if (gzipLimit !== null && gzipBytes > gzipLimit) {
  throw new Error(`${report.label} gzip image ${gzipBytes} exceeds ${gzipLimit}`);
}
