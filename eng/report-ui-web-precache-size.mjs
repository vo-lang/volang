#!/usr/bin/env node

import { readFileSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { gzipSync } from "node:zlib";

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

function positiveInteger(name) {
  const value = Number(required(name));
  if (!Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`--${name} must be a positive byte count`);
  }
  return value;
}

function assetPath(root, url) {
  if (url === "/") return join(root, "index.html");
  if (url.endsWith("/")) return join(root, url.slice(1), "index.html");
  return join(root, url.slice(1));
}

const root = required("root");
const worker = readFileSync(join(root, "service-worker.js"), "utf8");
const match = worker.match(/^const PRECACHE = (\[[^\n]+\]);$/mu);
if (!match) throw new Error("service worker does not expose a bounded PRECACHE manifest");
const assets = JSON.parse(match[1]);
if (!Array.isArray(assets) || assets.length === 0 || assets.length > 25_000) {
  throw new Error("service worker PRECACHE manifest has an invalid asset count");
}
const unique = [...new Set(assets)];
const invalidAsset = (asset) => typeof asset !== "string" || !asset.startsWith("/")
  || asset.startsWith("//") || asset.includes("\\") || asset.includes("?") || asset.includes("#")
  || asset.split("/").some((part) => part === "." || part === "..");
if (unique.length !== assets.length || unique.some(invalidAsset)) {
  throw new Error("service worker PRECACHE manifest contains duplicate or invalid assets");
}

let rawBytes = 0;
let gzipBytes = 0;
const entries = unique.map((asset) => {
  const bytes = readFileSync(assetPath(root, asset));
  const compressed = gzipSync(bytes, { level: 9, mtime: 0 }).byteLength;
  rawBytes += bytes.byteLength;
  gzipBytes += compressed;
  return { asset, raw_bytes: bytes.byteLength, gzip_bytes: compressed };
});
const gzipLimit = positiveInteger("gzip-limit");
const report = {
  schema: "volang.ui.web-precache-size.v1",
  label: required("label"),
  root,
  asset_count: entries.length,
  raw_bytes: rawBytes,
  gzip_bytes: gzipBytes,
  gzip_limit_bytes: gzipLimit,
  remaining_bytes: gzipLimit - gzipBytes,
  assets: entries,
};
const encoded = `${JSON.stringify(report)}\n`;
const output = options.get("output");
if (output) writeFileSync(output, encoded);
process.stdout.write(encoded);
if (gzipBytes > gzipLimit) {
  throw new Error(`${report.label} precache gzip transfer ${gzipBytes} exceeds ${gzipLimit}`);
}
