import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { brotliCompressSync, constants, gzipSync } from "node:zlib";

const options = new Map();
for (let index = 2; index < process.argv.length; index += 2) {
  const name = process.argv[index];
  const value = process.argv[index + 1];
  if (!name?.startsWith("--") || value === undefined) {
    throw new Error("profile artifact report arguments must be --name value pairs");
  }
  options.set(name.slice(2), value);
}

function required(name) {
  const value = options.get(name);
  if (!value) {
    throw new Error(`missing --${name}`);
  }
  return value;
}

const artifact = readFileSync(required("artifact"));
const dependencyTree = readFileSync(required("dependency-tree"));
const gzipLimit = Number(required("gzip-limit"));
if (!Number.isSafeInteger(gzipLimit) || gzipLimit < 0) {
  throw new Error("--gzip-limit must be a non-negative safe integer");
}

const gzipBytes = gzipSync(artifact, { level: 9, mtime: 0 }).byteLength;
const report = {
  framework: required("framework"),
  profile: required("profile"),
  target: required("target"),
  raw_bytes: artifact.byteLength,
  gzip_bytes: gzipBytes,
  brotli_bytes: brotliCompressSync(artifact, {
    params: {
      [constants.BROTLI_PARAM_QUALITY]: 11,
    },
  }).byteLength,
  gzip_limit: gzipLimit,
  artifact_sha256: createHash("sha256").update(artifact).digest("hex"),
  dependency_lines: dependencyTree.toString("utf8").split(/\r?\n/u).filter(Boolean).length,
  dependency_tree_sha256: createHash("sha256").update(dependencyTree).digest("hex"),
};

console.log(JSON.stringify(report));
if (gzipLimit > 0 && gzipBytes > gzipLimit) {
  throw new Error(
    `${report.framework} ${report.profile} gzip artifact ${gzipBytes} exceeds ${gzipLimit}`,
  );
}
