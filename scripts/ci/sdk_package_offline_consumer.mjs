#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  rmSync,
  statSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { gunzipSync } from "node:zlib";

const repoRoot = realpathSync(
  fileURLToPath(new URL("../..", import.meta.url)),
);
const rootReadme = readFileSync(path.join(repoRoot, "README.md"));
const rustToolchain = readFileSync(
  path.join(repoRoot, "rust-toolchain.toml"),
  "utf8",
);
const toolchainChannel = rustToolchain.match(/^channel\s*=\s*"([^"]+)"$/m)?.[1];
if (!toolchainChannel) {
  throw new Error("rust-toolchain.toml has no pinned channel");
}
const expectedRustVersion = toolchainChannel;
const expectedRepository = "https://github.com/vo-lang/volang";
const expectedHomepage = "https://volang.dev";
const maxCrateArchiveSize = 64 * 1024 * 1024;
const maxCrateUnpackedSize = 256 * 1024 * 1024;
const maxCrateArchiveEntries = 100_000;

const packages = [
  {
    name: "vo-common-core",
    source: "lang/crates/vo-common-core",
    internalDependencies: [],
  },
  {
    name: "vo-stdlib-source",
    source: "lang/stdlib",
    internalDependencies: [],
  },
  {
    name: "vo-common",
    source: "lang/crates/vo-common",
    internalDependencies: ["vo-common-core"],
  },
  {
    name: "vo-syntax",
    source: "lang/crates/vo-syntax",
    internalDependencies: ["vo-common"],
  },
  {
    name: "vo-module",
    source: "lang/crates/vo-module",
    internalDependencies: ["vo-common", "vo-syntax"],
  },
  {
    name: "vo-analysis",
    source: "lang/crates/vo-analysis",
    internalDependencies: [
      "vo-common",
      "vo-common-core",
      "vo-module",
      "vo-syntax",
    ],
  },
  {
    name: "vo-ffi-macro",
    source: "lang/crates/vo-ffi-macro",
    internalDependencies: [
      "vo-analysis",
      "vo-common",
      "vo-module",
      "vo-stdlib-source",
      "vo-syntax",
    ],
  },
  {
    name: "vo-runtime",
    source: "lang/crates/vo-runtime",
    internalDependencies: ["vo-common-core", "vo-ffi-macro"],
  },
  {
    name: "vo-ext",
    source: "lang/crates/vo-ext",
    internalDependencies: ["vo-common-core", "vo-ffi-macro", "vo-runtime"],
  },
  {
    name: "vo-stdlib",
    source: "lang/crates/vo-stdlib",
    internalDependencies: [
      "vo-common",
      "vo-common-core",
      "vo-ffi-macro",
      "vo-module",
      "vo-runtime",
      "vo-stdlib-source",
    ],
  },
].map((entry) => {
  const workspacePath = entry.source;
  const source = realpathSync(path.join(repoRoot, workspacePath));
  const localReadme = path.join(source, "README.md");
  return {
    ...entry,
    source,
    workspacePath,
    expectedReadme: existsSync(localReadme)
      ? readFileSync(localReadme)
      : rootReadme,
  };
});

const packageByName = new Map(packages.map((entry) => [entry.name, entry]));

function shellQuote(value) {
  return /^[A-Za-z0-9_./:=+-]+$/.test(value)
    ? value
    : JSON.stringify(value);
}

function invoke(command, args, options = {}) {
  const capture = options.capture ?? false;
  process.stdout.write(
    `+ ${[command, ...args].map(shellQuote).join(" ")}\n`,
  );
  const result = spawnSync(command, args, {
    cwd: options.cwd ?? repoRoot,
    encoding: "utf8",
    env: {
      ...process.env,
      CARGO_NET_OFFLINE: "true",
      CARGO_TERM_COLOR: "never",
      ...options.env,
    },
    maxBuffer: 16 * 1024 * 1024,
    stdio: capture ? ["ignore", "pipe", "pipe"] : "inherit",
  });
  if (result.error) {
    throw result.error;
  }
  if (result.status !== 0) {
    if (capture) {
      process.stderr.write(result.stderr ?? "");
      process.stderr.write(result.stdout ?? "");
    }
    throw new Error(`${command} exited with status ${result.status}`);
  }
  return {
    stdout: result.stdout ?? "",
    stderr: result.stderr ?? "",
  };
}

function requireCondition(condition, message) {
  if (!condition) {
    throw new Error(message);
  }
}

function tomlString(value) {
  return JSON.stringify(value);
}

function transitiveInternalDependencies(packageName) {
  const found = new Set();
  const visit = (name) => {
    for (const dependency of packageByName.get(name).internalDependencies) {
      if (!found.has(dependency)) {
        found.add(dependency);
        visit(dependency);
      }
    }
  };
  visit(packageName);
  return packages.filter((entry) => found.has(entry.name));
}

function cargoPatchArguments(entries) {
  return entries.flatMap((entry) => [
    "--config",
    `patch.crates-io.${entry.name}.path=${tomlString(entry.source)}`,
  ]);
}

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

function packageBlock(manifest) {
  const match = manifest.match(/\[package\]\n([\s\S]*?)(?=\n\[|$)/);
  requireCondition(match, "normalized manifest has no [package] table");
  return match[1];
}

function dependencyBlocks(manifest, dependency) {
  const escaped = escapeRegExp(dependency);
  const matches = manifest.matchAll(
    new RegExp(
      `\\[(?:dependencies|build-dependencies|target\\.[^\\]\\n]+\\.dependencies)\\.${escaped}\\]\\n([\\s\\S]*?)(?=\\n\\[|$)`,
      "g",
    ),
  );
  return [...matches].map((match) => match[1]);
}

const tarBlockSize = 512;
const utf8 = new TextDecoder("utf-8", { fatal: true });

function tarString(field) {
  const nul = field.indexOf(0);
  return utf8.decode(nul === -1 ? field : field.subarray(0, nul));
}

function tarNumber(field, label) {
  if ((field[0] & 0x80) !== 0) {
    requireCondition(
      (field[0] & 0x40) === 0,
      `${label} uses a negative base-256 tar number`,
    );
    let value = BigInt(field[0] & 0x3f);
    for (const byte of field.subarray(1)) {
      value = (value << 8n) | BigInt(byte);
    }
    requireCondition(
      value <= BigInt(Number.MAX_SAFE_INTEGER),
      `${label} exceeds JavaScript's safe integer range`,
    );
    return Number(value);
  }

  const text = tarString(field).trim();
  if (text === "") {
    return 0;
  }
  requireCondition(/^[0-7]+$/.test(text), `${label} is not an octal tar number`);
  const value = Number.parseInt(text, 8);
  requireCondition(Number.isSafeInteger(value), `${label} is too large`);
  return value;
}

function verifyTarChecksum(header, archive, offset) {
  const expected = tarNumber(
    header.subarray(148, 156),
    `${archive} header checksum at ${offset}`,
  );
  let actual = 0;
  for (let index = 0; index < tarBlockSize; index += 1) {
    actual += index >= 148 && index < 156 ? 0x20 : header[index];
  }
  requireCondition(
    actual === expected,
    `${archive} has an invalid tar checksum at byte ${offset}`,
  );
}

function parsePaxRecords(bytes, archive) {
  const records = new Map();
  let offset = 0;
  while (offset < bytes.length) {
    const separator = bytes.indexOf(0x20, offset);
    requireCondition(separator !== -1, `${archive} has a malformed PAX record`);
    const lengthText = utf8.decode(bytes.subarray(offset, separator));
    requireCondition(/^\d+$/.test(lengthText), `${archive} has an invalid PAX length`);
    const recordLength = Number.parseInt(lengthText, 10);
    const end = offset + recordLength;
    requireCondition(
      Number.isSafeInteger(recordLength) &&
        recordLength > separator - offset + 2 &&
        end <= bytes.length &&
        bytes[end - 1] === 0x0a,
      `${archive} has an out-of-bounds PAX record`,
    );
    const equals = bytes.indexOf(0x3d, separator + 1);
    requireCondition(
      equals !== -1 && equals < end - 1,
      `${archive} has a malformed PAX key/value`,
    );
    const key = utf8.decode(bytes.subarray(separator + 1, equals));
    const value = utf8.decode(bytes.subarray(equals + 1, end - 1));
    requireCondition(
      !records.has(key),
      `${archive} has a duplicate PAX key ${JSON.stringify(key)}`,
    );
    records.set(key, value);
    offset = end;
  }
  return records;
}

function checkedArchivePath(entryName, entryType, archive, prefix) {
  const withoutTrailingSlash =
    entryType === "5" ? entryName.replace(/\/$/, "") : entryName;
  requireCondition(
    withoutTrailingSlash !== "" &&
      !withoutTrailingSlash.includes("\\") &&
      !path.posix.isAbsolute(withoutTrailingSlash) &&
      path.posix.normalize(withoutTrailingSlash) === withoutTrailingSlash &&
      withoutTrailingSlash !== ".." &&
      !withoutTrailingSlash.startsWith("../") &&
      (withoutTrailingSlash === prefix ||
        withoutTrailingSlash.startsWith(`${prefix}/`)),
    `${archive} contains an unsafe entry: ${entryName}`,
  );
  return withoutTrailingSlash;
}

function extractCrateArchive(archive, destination, packageName, version) {
  const archiveSize = statSync(archive).size;
  requireCondition(
    archiveSize <= maxCrateArchiveSize,
    `${archive} exceeds the ${maxCrateArchiveSize}-byte compressed archive limit`,
  );
  const tar = gunzipSync(readFileSync(archive), {
    maxOutputLength: maxCrateUnpackedSize,
  });
  const prefix = `${packageName}-${version}`;
  const explicitEntries = new Set();
  let offset = 0;
  let pendingLongName = null;
  let pendingPax = new Map();
  let reachedEnd = false;
  let entryCount = 0;

  while (offset + tarBlockSize <= tar.length) {
    const header = tar.subarray(offset, offset + tarBlockSize);
    if (header.every((byte) => byte === 0)) {
      reachedEnd = true;
      for (const byte of tar.subarray(offset)) {
        requireCondition(byte === 0, `${archive} has data after its tar terminator`);
      }
      break;
    }

    entryCount += 1;
    requireCondition(
      entryCount <= maxCrateArchiveEntries,
      `${archive} exceeds the ${maxCrateArchiveEntries}-entry limit`,
    );

    verifyTarChecksum(header, archive, offset);
    const headerSize = tarNumber(
      header.subarray(124, 136),
      `${archive} entry size at ${offset}`,
    );
    const typeByte = header[156];
    const type = typeByte === 0 ? "0" : String.fromCharCode(typeByte);
    const dataOffset = offset + tarBlockSize;
    const dataEnd = dataOffset + headerSize;
    requireCondition(dataEnd <= tar.length, `${archive} has a truncated tar entry`);
    const data = tar.subarray(dataOffset, dataEnd);
    offset = dataOffset + Math.ceil(headerSize / tarBlockSize) * tarBlockSize;

    if (type === "x") {
      pendingPax = parsePaxRecords(data, archive);
      continue;
    }
    if (type === "g") {
      const globalPax = parsePaxRecords(data, archive);
      for (const forbidden of ["path", "linkpath", "size"]) {
        requireCondition(
          !globalPax.has(forbidden),
          `${archive} uses unsupported global PAX key ${forbidden}`,
        );
      }
      continue;
    }
    if (type === "L") {
      pendingLongName = tarString(data);
      continue;
    }

    const name = tarString(header.subarray(0, 100));
    const headerPrefix = tarString(header.subarray(345, 500));
    const headerPath = headerPrefix ? `${headerPrefix}/${name}` : name;
    const entryName = pendingPax.get("path") ?? pendingLongName ?? headerPath;
    const paxSize = pendingPax.get("size");
    requireCondition(
      paxSize === undefined || /^\d+$/.test(paxSize),
      `${archive} has an invalid PAX size override`,
    );
    const effectiveSize =
      paxSize === undefined ? headerSize : Number.parseInt(paxSize, 10);
    requireCondition(
      Number.isSafeInteger(effectiveSize) &&
        effectiveSize >= 0 &&
        effectiveSize === headerSize,
      `${archive} has an unsupported PAX size override`,
    );
    pendingPax = new Map();
    pendingLongName = null;

    requireCondition(
      type === "0" || type === "5",
      `${archive} contains unsupported tar entry type ${JSON.stringify(type)} for ${entryName}`,
    );
    const safePath = checkedArchivePath(entryName, type, archive, prefix);
    requireCondition(
      !explicitEntries.has(safePath),
      `${archive} contains duplicate entry ${safePath}`,
    );
    explicitEntries.add(safePath);
    const outputPath = path.join(destination, ...safePath.split("/"));
    if (type === "5") {
      mkdirSync(outputPath, { recursive: true });
      continue;
    }
    mkdirSync(path.dirname(outputPath), { recursive: true });
    writeFileSync(outputPath, data, { flag: "wx", mode: 0o644 });
  }

  requireCondition(reachedEnd, `${archive} has no tar terminator`);
  requireCondition(
    pendingLongName === null && pendingPax.size === 0,
    `${archive} ends with dangling per-entry tar metadata`,
  );
  requireCondition(explicitEntries.size > 0, `${archive} is empty`);
}

function validateNormalizedPackage(entry, packageRoot, version) {
  const manifestPath = path.join(packageRoot, "Cargo.toml");
  requireCondition(
    existsSync(manifestPath),
    `${entry.name} archive has no normalized Cargo.toml`,
  );
  const manifest = readFileSync(manifestPath, "utf8");
  const metadata = packageBlock(manifest);
  const requiredMetadata = [
    `name = ${tomlString(entry.name)}`,
    `version = ${tomlString(version)}`,
    `rust-version = ${tomlString(expectedRustVersion)}`,
    `homepage = ${tomlString(expectedHomepage)}`,
    'readme = "README.md"',
    'license = "MIT"',
    'publish = ["crates-io"]',
    `repository = ${tomlString(expectedRepository)}`,
  ];
  for (const required of requiredMetadata) {
    requireCondition(
      metadata.includes(required),
      `${entry.name} package metadata is missing ${required}`,
    );
  }
  requireCondition(
    /^description = ".+"$/m.test(metadata),
    `${entry.name} package metadata has no description`,
  );

  const packagedReadme = path.join(packageRoot, "README.md");
  requireCondition(
    existsSync(packagedReadme) &&
      readFileSync(packagedReadme).equals(entry.expectedReadme),
    `${entry.name} did not package its declared README.md`,
  );

  for (const dependency of entry.internalDependencies) {
    const blocks = dependencyBlocks(manifest, dependency);
    requireCondition(
      blocks.length > 0,
      `${entry.name} normalized manifest lost ${dependency}`,
    );
    for (const block of blocks) {
      requireCondition(
        block.includes(`version = ${tomlString(`=${version}`)}`),
        `${entry.name} does not require ${dependency} at =${version}`,
      );
      requireCondition(
        !/^path\s*=/m.test(block),
        `${entry.name} published dependency ${dependency} still has a path`,
      );
    }
  }

  if (entry.name === "vo-runtime") {
    requireCondition(
      !manifest.includes("vo-source-contract"),
      "cargo package retained the maintainer-only vo-source-contract dev dependency",
    );
  }
}

function validateWorkspaceInternalDependencies(metadata, version) {
  const topologicalIndex = new Map(
    packages.map((entry, index) => [entry.name, index]),
  );
  for (const entry of packages) {
    const packageMetadata = metadata.packages.find(
      (candidate) => candidate.name === entry.name,
    );
    requireCondition(packageMetadata, `workspace has no ${entry.name} package`);
    const internalDependencies = packageMetadata.dependencies.filter(
      (dependency) =>
        dependency.kind !== "dev" && packageByName.has(dependency.name),
    );
    const actualNames = [
      ...new Set(internalDependencies.map((dependency) => dependency.name)),
    ].sort();
    const expectedNames = [...entry.internalDependencies].sort();
    requireCondition(
      actualNames.join("\n") === expectedNames.join("\n"),
      `${entry.name} internal dependency model drifted; metadata=[${actualNames.join(", ")}], gate=[${expectedNames.join(", ")}]`,
    );

    for (const dependency of internalDependencies) {
      const dependencyEntry = packageByName.get(dependency.name);
      requireCondition(
        dependency.req === `=${version}`,
        `${entry.name} workspace dependency ${dependency.name} uses ${dependency.req}, expected =${version}`,
      );
      requireCondition(
        dependency.path !== null &&
          realpathSync(dependency.path) === dependencyEntry.source,
        `${entry.name} workspace dependency ${dependency.name} does not resolve to its canonical path`,
      );
      requireCondition(
        topologicalIndex.get(dependency.name) < topologicalIndex.get(entry.name),
        `${entry.name} appears before dependency ${dependency.name} in the publish order`,
      );
    }
  }
}

function validateWorkspacePublishBoundary(metadata, version) {
  const publicPackages = new Set(packages.map((entry) => entry.name));
  const workspaceMembers = new Set(metadata.workspace_members);
  const members = metadata.packages.filter((entry) =>
    workspaceMembers.has(entry.id),
  );
  requireCondition(
    members.length === workspaceMembers.size,
    "cargo metadata omitted one or more workspace members",
  );
  for (const entry of members) {
    requireCondition(
      entry.version === version,
      `${entry.name} has workspace version ${entry.version}, expected ${version}`,
    );
    requireCondition(
      entry.rust_version === expectedRustVersion &&
        entry.license === "MIT" &&
        entry.repository === expectedRepository &&
        entry.homepage === expectedHomepage &&
        typeof entry.description === "string" &&
        entry.description.length > 0,
      `${entry.name} has incomplete canonical workspace package metadata`,
    );
    if (publicPackages.has(entry.name)) {
      requireCondition(
        JSON.stringify(entry.publish) === JSON.stringify(["crates-io"]),
        `${entry.name} must publish only to crates-io`,
      );
    } else {
      requireCondition(
        Array.isArray(entry.publish) && entry.publish.length === 0,
        `${entry.name} must explicitly set publish = false`,
      );
    }
  }
}

function validateWorkspaceDependencyTable(version) {
  const rootManifest = readFileSync(path.join(repoRoot, "Cargo.toml"), "utf8");
  const table = rootManifest.match(
    /\[workspace\.dependencies\]\n([\s\S]*?)(?=\n\[|$)/,
  )?.[1];
  requireCondition(table, "root manifest has no [workspace.dependencies] table");
  const defaultFeaturesDisabled = new Set([
    "vo-common-core",
    "vo-common",
    "vo-syntax",
    "vo-module",
    "vo-runtime",
  ]);
  for (const entry of packages) {
    const line = table.match(
      new RegExp(`^${escapeRegExp(entry.name)}\\s*=\\s*\\{([^\\n]+)\\}\\s*$`, "m"),
    )?.[1];
    requireCondition(line, `workspace dependencies omit ${entry.name}`);
    requireCondition(
      line.includes(`path = ${tomlString(entry.workspacePath)}`) &&
        line.includes(`version = ${tomlString(`=${version}`)}`),
      `workspace dependency ${entry.name} must use ${entry.workspacePath} at =${version}`,
    );
    if (defaultFeaturesDisabled.has(entry.name)) {
      requireCondition(
        line.includes("default-features = false"),
        `workspace dependency ${entry.name} must disable default features`,
      );
    }
  }
}

function patchTable(entries, extractedRoots) {
  return entries
    .map(
      (entry) =>
        `${entry.name} = { path = ${tomlString(extractedRoots.get(entry.name))} }`,
    )
    .join("\n");
}

function writeFullConsumer(consumerRoot, version, extractedRoots) {
  mkdirSync(path.join(consumerRoot, "src"), { recursive: true });
  mkdirSync(path.join(consumerRoot, "math"), { recursive: true });
  writeFileSync(
    path.join(consumerRoot, "Cargo.toml"),
    `[package]
name = "vo-sdk-package-consumer"
version = "0.0.0"
edition = "2021"
publish = false

[lib]
crate-type = ["rlib"]

[dependencies]
vo-ext = { version = "=${version}" }
vo-runtime = { version = "=${version}" }
vo-stdlib = { version = "=${version}", features = ["std"] }

[package.metadata.vo]
vomod = "vo.mod"

[patch.crates-io]
${patchTable(packages, extractedRoots)}
`,
  );
  writeFileSync(
    path.join(consumerRoot, "vo.mod"),
    'module = "github.com/vo-lang/sdkgate"\nvo = "^0.1.0"\n',
  );
  writeFileSync(
    path.join(consumerRoot, "math", "math.vo"),
    "package math\n\nfunc Add(left, right int) int\n",
  );
  writeFileSync(
    path.join(consumerRoot, "src", "lib.rs"),
    `use vo_ext::prelude::*;

#[vo_fn("sdkgate/math", "Add")]
fn add(left: i64, right: i64) -> i64 {
    left + right
}

vo_ext::export_extensions!();

pub fn stdlib_source_fingerprint() -> String {
    vo_stdlib::EmbeddedStdlib::new().source_fingerprint()
}
`,
  );
}

function writeStdlibMacroConsumer(consumerRoot, version, extractedRoots) {
  mkdirSync(path.join(consumerRoot, "src"), { recursive: true });
  const closureNames = new Set([
    "vo-runtime",
    ...transitiveInternalDependencies("vo-runtime").map((entry) => entry.name),
  ]);
  const closure = packages.filter((entry) => closureNames.has(entry.name));
  writeFileSync(
    path.join(consumerRoot, "Cargo.toml"),
    `[package]
name = "vo-stdlib-macro-package-consumer"
version = "0.0.0"
edition = "2021"
publish = false

[dependencies]
vo-ffi-macro = { version = "=${version}" }
vo-runtime = { version = "=${version}" }

[patch.crates-io]
${patchTable(closure, extractedRoots)}
`,
  );
  writeFileSync(
    path.join(consumerRoot, "src", "lib.rs"),
    `#[vo_ffi_macro::vostd_fn("math", "Sqrt")]
fn sdk_stdlib_sqrt(value: f64) -> f64 {
    value.sqrt()
}
`,
  );
}

function writeNoStdConsumer(consumerRoot, version, extractedRoots) {
  mkdirSync(path.join(consumerRoot, "src"), { recursive: true });
  const closure = [
    "vo-common-core",
    "vo-stdlib-source",
    "vo-common",
    "vo-syntax",
    "vo-module",
    "vo-analysis",
    "vo-ffi-macro",
    "vo-runtime",
  ].map((name) => packageByName.get(name));
  writeFileSync(
    path.join(consumerRoot, "Cargo.toml"),
    `[package]
name = "vo-runtime-no-std-package-consumer"
version = "0.0.0"
edition = "2021"
publish = false

[dependencies]
vo-runtime = { version = "=${version}", default-features = false }

[patch.crates-io]
${patchTable(closure, extractedRoots)}
`,
  );
  writeFileSync(
    path.join(consumerRoot, "src", "lib.rs"),
    "#![no_std]\n\npub use vo_runtime as runtime;\n",
  );
}

function writeCoreNoStdConsumer(consumerRoot, version, extractedRoots) {
  mkdirSync(path.join(consumerRoot, "src"), { recursive: true });
  writeFileSync(
    path.join(consumerRoot, "Cargo.toml"),
    `[package]
name = "vo-common-core-no-std-package-consumer"
version = "0.0.0"
edition = "2021"
publish = false

[dependencies]
vo-common-core = { version = "=${version}", default-features = false }

[patch.crates-io]
${patchTable([packageByName.get("vo-common-core")], extractedRoots)}
`,
  );
  writeFileSync(
    path.join(consumerRoot, "src", "lib.rs"),
    "#![no_std]\n\npub use vo_common_core as common_core;\n",
  );
}

function writeWasmExtensionConsumer(consumerRoot, version, extractedRoots) {
  mkdirSync(path.join(consumerRoot, "src"), { recursive: true });
  const closureNames = new Set([
    "vo-ext",
    ...transitiveInternalDependencies("vo-ext").map((entry) => entry.name),
  ]);
  const closure = packages.filter((entry) => closureNames.has(entry.name));
  writeFileSync(
    path.join(consumerRoot, "Cargo.toml"),
    `[package]
name = "vo-extension-wasm-package-consumer"
version = "0.0.0"
edition = "2021"
publish = false

[dependencies]
vo-ext = { version = "=${version}", default-features = false, features = ["wasm"] }
vo-runtime = { version = "=${version}", default-features = false }

[patch.crates-io]
${patchTable(closure, extractedRoots)}
`,
  );
  writeFileSync(
    path.join(consumerRoot, "src", "lib.rs"),
    "vo_ext::export_wasm_extension_protocol!();\n",
  );
}

function checkConsumer(consumerRoot, cargoTargetDir, target = null) {
  const manifestPath = path.join(consumerRoot, "Cargo.toml");
  const environment = {
    CARGO_INCREMENTAL: "0",
    CARGO_PROFILE_DEV_DEBUG: "0",
    CARGO_PROFILE_DEV_SPLIT_DEBUGINFO: "off",
    CARGO_TARGET_DIR: cargoTargetDir,
    VO_FFI_SOURCE_FINGERPRINT: "sdk-package-offline-consumer-v1",
  };
  invoke(
    "cargo",
    ["generate-lockfile", "--offline", "--manifest-path", manifestPath],
    { env: environment },
  );
  const targetArguments = target === null ? [] : ["--target", target];
  invoke(
    "cargo",
    [
      "check",
      "--offline",
      "--locked",
      "--manifest-path",
      manifestPath,
      ...targetArguments,
    ],
    { env: environment },
  );
  const filterArguments =
    target === null ? [] : ["--filter-platform", target];
  return JSON.parse(
    invoke(
      "cargo",
      [
        "metadata",
        "--offline",
        "--locked",
        "--format-version",
        "1",
        "--manifest-path",
        manifestPath,
        ...filterArguments,
      ],
      { capture: true, env: environment },
    ).stdout,
  );
}

function validateFullConsumerMetadata(metadata, extractedRoots, version) {
  for (const entry of packages) {
    const resolved = metadata.packages.find(
      (candidate) => candidate.name === entry.name && candidate.version === version,
    );
    requireCondition(
      resolved,
      `offline consumer did not resolve ${entry.name} ${version}`,
    );
    requireCondition(
      realpathSync(path.dirname(resolved.manifest_path)) ===
        realpathSync(extractedRoots.get(entry.name)),
      `offline consumer resolved ${entry.name} outside the extracted package set`,
    );
    requireCondition(
      resolved.source === null,
      `offline consumer did not use the extracted ${entry.name} source`,
    );
  }
  const stdlib = metadata.packages.find(
    (candidate) => candidate.name === "vo-stdlib" && candidate.version === version,
  );
  const stdlibNode = metadata.resolve.nodes.find(
    (candidate) => candidate.id === stdlib.id,
  );
  requireCondition(
    stdlibNode?.features.includes("std"),
    "full consumer did not compile the published vo-stdlib/std surface",
  );
}

function validateStdlibMacroMetadata(metadata, extractedRoots, version) {
  for (const name of ["vo-ffi-macro", "vo-stdlib-source", "vo-runtime"]) {
    const resolved = metadata.packages.find(
      (candidate) => candidate.name === name && candidate.version === version,
    );
    requireCondition(resolved, `stdlib macro consumer did not resolve ${name}`);
    requireCondition(
      resolved.source === null &&
        realpathSync(path.dirname(resolved.manifest_path)) ===
          realpathSync(extractedRoots.get(name)),
      `stdlib macro consumer resolved ${name} outside the extracted package set`,
    );
  }
}

function validateNoStdMetadata(metadata, coreMetadata, version) {
  const runtime = metadata.packages.find(
    (candidate) => candidate.name === "vo-runtime" && candidate.version === version,
  );
  requireCondition(runtime, "no_std consumer did not resolve vo-runtime");
  const node = metadata.resolve.nodes.find((candidate) => candidate.id === runtime.id);
  requireCondition(node, "no_std consumer has no vo-runtime resolve node");
  requireCondition(
    !node.features.includes("std"),
    "vo-runtime/std leaked into the default-features=false consumer",
  );
  const commonCoreDependency = runtime.dependencies.find(
    (dependency) => dependency.name === "vo-common-core",
  );
  requireCondition(
    commonCoreDependency &&
      commonCoreDependency.uses_default_features === false &&
      commonCoreDependency.features.length === 0,
    "vo-runtime package enables vo-common-core features outside vo-runtime/std",
  );
  requireCondition(
    runtime.features.std.includes("vo-common-core/std"),
    "vo-runtime/std no longer enables vo-common-core/std",
  );

  const commonCore = coreMetadata.packages.find(
    (candidate) =>
      candidate.name === "vo-common-core" && candidate.version === version,
  );
  requireCondition(commonCore, "no_std core consumer did not resolve vo-common-core");
  const commonCoreNode = coreMetadata.resolve.nodes.find(
    (candidate) => candidate.id === commonCore.id,
  );
  requireCondition(
    commonCoreNode && !commonCoreNode.features.includes("std"),
    "vo-common-core/std leaked into its default-features=false consumer",
  );
}

function validateWasmExtensionMetadata(metadata, version) {
  const extension = metadata.packages.find(
    (candidate) => candidate.name === "vo-ext" && candidate.version === version,
  );
  const runtime = metadata.packages.find(
    (candidate) => candidate.name === "vo-runtime" && candidate.version === version,
  );
  requireCondition(extension && runtime, "WASM consumer did not resolve the SDK closure");
  const extensionNode = metadata.resolve.nodes.find(
    (candidate) => candidate.id === extension.id,
  );
  const runtimeNode = metadata.resolve.nodes.find(
    (candidate) => candidate.id === runtime.id,
  );
  requireCondition(
    extensionNode &&
      extensionNode.features.includes("wasm") &&
      !extensionNode.features.includes("native"),
    "vo-ext WASM consumer selected an invalid SDK feature set",
  );
  requireCondition(
    runtimeNode && !runtimeNode.features.includes("std"),
    "vo-ext/wasm leaked vo-runtime/std into the published WASM closure",
  );
}

function validateStdlibAssets(extractedRoots) {
  const sourceRoot = extractedRoots.get("vo-stdlib-source");
  for (const relative of [
    "stdlib.toml",
    "encoding/json/json.vo",
    "math/math.vo",
    "src/lib.rs",
  ]) {
    requireCondition(
      existsSync(path.join(sourceRoot, relative)),
      `vo-stdlib-source package is missing ${relative}`,
    );
  }
}

const privateTemporaryRoot = existsSync("/private/tmp") ? "/private/tmp" : tmpdir();
const temporaryRoot = mkdtempSync(
  path.join(privateTemporaryRoot, "volang-sdk-package-gate-"),
);

try {
  const packageTarget = path.join(temporaryRoot, "package-target");
  const metadataResult = invoke(
    "cargo",
    ["metadata", "--offline", "--locked", "--format-version", "1", "--no-deps"],
    { capture: true, env: { CARGO_TARGET_DIR: packageTarget } },
  );
  requireCondition(
    metadataResult.stderr.trim() === "",
    `cargo metadata emitted warnings:\n${metadataResult.stderr}`,
  );
  const workspaceMetadata = JSON.parse(metadataResult.stdout);
  const versions = new Set(
    packages.map((entry) => {
      const packageMetadata = workspaceMetadata.packages.find(
        (candidate) => candidate.name === entry.name,
      );
      requireCondition(packageMetadata, `workspace has no ${entry.name} package`);
      return packageMetadata.version;
    }),
  );
  requireCondition(
    versions.size === 1,
    `SDK packages do not share one version: ${[...versions].join(", ")}`,
  );
  const [version] = versions;
  validateWorkspacePublishBoundary(workspaceMetadata, version);
  validateWorkspaceDependencyTable(version);
  validateWorkspaceInternalDependencies(workspaceMetadata, version);

  const archiveRoot = path.join(packageTarget, "package");
  const extractedRoot = path.join(temporaryRoot, "extracted");
  mkdirSync(extractedRoot, { recursive: true });
  const extractedRoots = new Map();

  for (const entry of packages) {
    invoke(
      "cargo",
      [
        "package",
        "--offline",
        "--locked",
        "--allow-dirty",
        "--no-verify",
        "--package",
        entry.name,
        ...cargoPatchArguments(transitiveInternalDependencies(entry.name)),
      ],
      { env: { CARGO_TARGET_DIR: packageTarget } },
    );
    const archive = path.join(archiveRoot, `${entry.name}-${version}.crate`);
    requireCondition(existsSync(archive), `${entry.name} produced no package archive`);
    extractCrateArchive(archive, extractedRoot, entry.name, version);
    const packageRoot = realpathSync(
      path.join(extractedRoot, `${entry.name}-${version}`),
    );
    extractedRoots.set(entry.name, packageRoot);
    validateNormalizedPackage(entry, packageRoot, version);
  }

  validateStdlibAssets(extractedRoots);

  const consumerTarget = path.join(temporaryRoot, "consumer-target");
  const fullConsumer = path.join(temporaryRoot, "full-consumer");
  writeFullConsumer(fullConsumer, version, extractedRoots);
  const fullMetadata = checkConsumer(fullConsumer, consumerTarget);
  validateFullConsumerMetadata(fullMetadata, extractedRoots, version);

  const stdlibMacroConsumer = path.join(
    temporaryRoot,
    "stdlib-macro-consumer",
  );
  writeStdlibMacroConsumer(stdlibMacroConsumer, version, extractedRoots);
  const stdlibMacroMetadata = checkConsumer(
    stdlibMacroConsumer,
    consumerTarget,
  );
  validateStdlibMacroMetadata(
    stdlibMacroMetadata,
    extractedRoots,
    version,
  );

  const noStdTarget = "wasm32-unknown-unknown";
  const noStdConsumer = path.join(temporaryRoot, "no-std-consumer");
  writeNoStdConsumer(noStdConsumer, version, extractedRoots);
  const noStdMetadata = checkConsumer(
    noStdConsumer,
    consumerTarget,
    noStdTarget,
  );
  const coreNoStdConsumer = path.join(temporaryRoot, "core-no-std-consumer");
  writeCoreNoStdConsumer(coreNoStdConsumer, version, extractedRoots);
  const coreNoStdMetadata = checkConsumer(
    coreNoStdConsumer,
    consumerTarget,
    noStdTarget,
  );
  validateNoStdMetadata(noStdMetadata, coreNoStdMetadata, version);

  const wasmExtensionConsumer = path.join(
    temporaryRoot,
    "wasm-extension-consumer",
  );
  writeWasmExtensionConsumer(
    wasmExtensionConsumer,
    version,
    extractedRoots,
  );
  const wasmExtensionMetadata = checkConsumer(
    wasmExtensionConsumer,
    consumerTarget,
    noStdTarget,
  );
  validateWasmExtensionMetadata(wasmExtensionMetadata, version);

  process.stdout.write(
    `SDK package gate passed for ${packages.length} crates at ${version}; archives and consumers stayed offline.\n`,
  );
} finally {
  if (process.env.VO_KEEP_SDK_PACKAGE_GATE_TEMP === "1") {
    process.stdout.write(`kept SDK package gate temp directory: ${temporaryRoot}\n`);
  } else {
    rmSync(temporaryRoot, { recursive: true, force: true });
  }
}
