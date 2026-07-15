#!/usr/bin/env node

import { spawnSync } from "node:child_process";
import { realpathSync } from "node:fs";
import { fileURLToPath } from "node:url";

const repoRoot = realpathSync(
  fileURLToPath(new URL("../..", import.meta.url)),
);
const target = "wasm32-unknown-unknown";

function invoke(args, capture = false) {
  process.stdout.write(`+ cargo ${args.join(" ")}\n`);
  const result = spawnSync("cargo", args, {
    cwd: repoRoot,
    encoding: "utf8",
    env: {
      ...process.env,
      CARGO_NET_OFFLINE: "true",
      CARGO_TERM_COLOR: "never",
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
    throw new Error(`cargo exited with status ${result.status}`);
  }
  return result.stdout ?? "";
}

function inspectTargetClosure(packageName, enabledFeatures = []) {
  const args = [
    "tree",
    "--locked",
    "--offline",
    "-p",
    packageName,
    "--target",
    target,
    "--no-default-features",
    "--edges",
    "normal,no-proc-macro",
    "--format",
    "{p}|{f}",
  ];
  if (enabledFeatures.length > 0) {
    args.push("--features", enabledFeatures.join(","));
  }

  const output = invoke(args, true);
  const nodes = output
    .trim()
    .split("\n")
    .map((line) => {
      const separator = line.lastIndexOf("|");
      const description = line.slice(0, separator);
      const name = description.match(/([A-Za-z0-9_-]+) v[0-9]/)?.[1];
      const features = new Set(
        line
          .slice(separator + 1)
          .split(",")
          .map((feature) => feature.trim())
          .filter(Boolean),
      );
      return { line, name, features };
    });

  const forbiddenPackages = new Set([
    "libloading",
    "rust-embed",
    "termcolor",
    "ureq",
    "vo-analysis",
    "vo-common",
    "vo-module",
    "vo-stdlib-source",
    "vo-syntax",
    "zip",
  ]);
  for (const node of nodes) {
    if (node.features.has("std")) {
      throw new Error(
        `${packageName} target closure enables std: ${node.line}`,
      );
    }
    if (forbiddenPackages.has(node.name)) {
      throw new Error(
        `${packageName} target closure contains host/compiler package: ${node.line}`,
      );
    }
  }

  const root = nodes[0];
  const expectedRootFeatures = new Set(enabledFeatures);
  if (
    root.name !== packageName ||
    root.features.size !== expectedRootFeatures.size ||
    [...root.features].some((feature) => !expectedRootFeatures.has(feature))
  ) {
    throw new Error(
      `${packageName} root feature mismatch: expected ${[
        ...expectedRootFeatures,
      ].join(",") || "<none>"}, found ${[...root.features].join(",") || "<none>"}`,
    );
  }
}

inspectTargetClosure("vo-stdlib");
inspectTargetClosure("vo-vm");
inspectTargetClosure("vo-ext", ["wasm"]);

invoke([
  "check",
  "--locked",
  "--offline",
  "-p",
  "vo-stdlib",
  "-p",
  "vo-vm",
  "--no-default-features",
]);
invoke([
  "check",
  "--locked",
  "--offline",
  "-p",
  "vo-vm",
  "--target",
  target,
  "--no-default-features",
]);
invoke([
  "check",
  "--locked",
  "--offline",
  "-p",
  "vo-ext",
  "--target",
  target,
  "--no-default-features",
  "--features",
  "wasm",
  "--tests",
]);

process.stdout.write("no_std target dependency closures are clean\n");
