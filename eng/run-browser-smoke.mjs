#!/usr/bin/env node

import { spawn, spawnSync } from "node:child_process";
import { mkdirSync } from "node:fs";
import { createServer } from "node:http";
import { mkdtemp, readFile, readdir, rm, stat, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { basename, extname, join, relative, resolve, sep } from "node:path";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

const scriptRoot = dirname(fileURLToPath(import.meta.url));

const options = parseArguments(process.argv.slice(2));
const projectRoot = resolve(options.project);
const htmlPath = join(projectRoot, options.html);
const temporaryRoot = await mkdtemp(join(tmpdir(), "vo-browser-smoke-"));
const outputRoot = join(temporaryRoot, "compiled");
const profileRoot = join(temporaryRoot, "chrome-profile");
let server;
let chrome;

try {
  compileProject(projectRoot, outputRoot, options.bundleEntry);
  const smokeModule = await findUniqueFile(outputRoot, "browser_smoke.js");
  const html = (await readFile(htmlPath, "utf8")).replace(
    /\/src\/browser_smoke\.ts/g,
    `/${toUrlPath(relative(outputRoot, smokeModule))}`,
  );
  server = await startServer(outputRoot, html);
  const address = server.address();
  if (address === null || typeof address === "string") {
    throw new Error("browser smoke server has no TCP address");
  }
  const url = `http://127.0.0.1:${address.port}/`;
  if (options.serveOnly) {
    process.stdout.write(`${JSON.stringify({ ready: true, url, project: projectRoot })}\n`);
    await waitForSignal();
  } else {
    const chromeBinary = await resolveChromeBinary();
    const launched = await launchChrome(chromeBinary, profileRoot);
    chrome = launched;
    const report = await runSmoke(launched.webSocketUrl, url, options);
    if (report === null || typeof report !== "object") {
      throw new Error("browser smoke returned no structured report");
    }
    const result = {
      passed: report.passed === true,
      project: basename(projectRoot),
      report,
    };
    if (options.output !== null) {
      const outputPath = resolve(options.output);
      mkdirSync(dirname(outputPath), { recursive: true });
      await writeFile(outputPath, `${JSON.stringify(result, null, 2)}\n`, "utf8");
    }
    process.stdout.write(`${JSON.stringify(result)}\n`);
    if (report.passed !== true) process.exitCode = 1;
  }
} finally {
  if (chrome !== undefined) {
    await chrome.close();
  }
  if (server !== undefined) {
    await new Promise((done) => server.close(done));
  }
  await rm(temporaryRoot, { recursive: true, force: true });
}

function parseArguments(arguments_) {
  const parsed = {
    project: "",
    html: "browser-smoke.html",
    global: "",
    button: null,
    timeout: 30_000,
    serveOnly: false,
    bundleEntry: null,
    output: null,
  };
  for (let index = 0; index < arguments_.length; index += 1) {
    const argument = arguments_[index];
    const value = arguments_[index + 1];
    if (argument === "--project" && value !== undefined) {
      parsed.project = value;
      index += 1;
    } else if (argument === "--html" && value !== undefined) {
      parsed.html = value;
      index += 1;
    } else if (argument === "--global" && value !== undefined) {
      parsed.global = value;
      index += 1;
    } else if (argument === "--button" && value !== undefined) {
      parsed.button = value;
      index += 1;
    } else if (argument === "--timeout-ms" && value !== undefined) {
      parsed.timeout = Number(value);
      index += 1;
    } else if (argument === "--serve-only") {
      parsed.serveOnly = true;
    } else if (argument === "--bundle-entry" && value !== undefined) {
      parsed.bundleEntry = value;
      index += 1;
    } else if (argument === "--output" && value !== undefined) {
      parsed.output = value;
      index += 1;
    } else {
      throw new Error(`unknown or incomplete argument: ${argument}`);
    }
  }
  if (!parsed.project || !parsed.global) {
    throw new Error("usage: run-browser-smoke.mjs --project <dir> --global <window-key> [--button <id>]");
  }
  if (!Number.isSafeInteger(parsed.timeout) || parsed.timeout < 1_000 || parsed.timeout > 120_000) {
    throw new Error("browser smoke timeout must be an integer between 1000 and 120000 ms");
  }
  if (!/^[A-Za-z_$][A-Za-z0-9_$]*$/.test(parsed.global)) {
    throw new Error("browser smoke global key is invalid");
  }
  return parsed;
}

function compileProject(project, output, bundleEntry) {
  if (bundleEntry !== null) {
    mkdirSync(output, { recursive: true });
    const executableName = process.platform === "win32" ? "rolldown.exe" : "rolldown";
    const executable = join(
      scriptRoot,
      "..",
      "apps",
      "studio",
      "node_modules",
      ".bin",
      executableName,
    );
    const result = spawnSync(executable, [
      resolve(bundleEntry),
      "--format",
      "esm",
      "--platform",
      "browser",
      "--file",
      join(output, "browser_smoke.js"),
      "--transform.target",
      "es2022",
    ], {
      cwd: project,
      encoding: "utf8",
      env: process.env,
    });
    if (result.error !== undefined) throw result.error;
    if (result.status !== 0) {
      throw new Error(`browser smoke bundling failed:\n${result.stdout}${result.stderr}`);
    }
    return;
  }
  const executableName = process.platform === "win32" ? "tsc.cmd" : "tsc";
  const executable = [
    join(project, "node_modules", ".bin", executableName),
    join(scriptRoot, "..", "apps", "studio", "node_modules", ".bin", executableName),
  ].find((candidate) => {
    const result = spawnSync(candidate, ["--version"], {
      cwd: project,
      encoding: "utf8",
      env: process.env,
    });
    return result.status === 0;
  });
  if (executable === undefined) {
    throw new Error("TypeScript compiler was not found in the smoke project or Studio");
  }
  const result = spawnSync(executable, [
    "--project",
    join(project, "tsconfig.json"),
    "--noEmit",
    "false",
    "--declaration",
    "false",
    "--declarationMap",
    "false",
    "--sourceMap",
    "false",
    "--outDir",
    output,
  ], {
    cwd: project,
    encoding: "utf8",
    env: process.env,
  });
  if (result.error !== undefined) throw result.error;
  if (result.status !== 0) {
    throw new Error(`TypeScript smoke compilation failed:\n${result.stdout}${result.stderr}`);
  }
}

async function findUniqueFile(root, name) {
  const matches = [];
  async function walk(directory) {
    for (const entry of await readdir(directory, { withFileTypes: true })) {
      const path = join(directory, entry.name);
      if (entry.isDirectory()) await walk(path);
      else if (entry.isFile() && entry.name === name) matches.push(path);
    }
  }
  await walk(root);
  if (matches.length !== 1) {
    throw new Error(`expected one ${name} in compiled output, found ${matches.length}`);
  }
  return matches[0];
}

async function startServer(root, indexHtml) {
  const httpServer = createServer(async (request, response) => {
    try {
      const requestUrl = new URL(request.url ?? "/", "http://127.0.0.1");
      if (requestUrl.pathname === "/" || requestUrl.pathname === "/browser-smoke.html") {
        response.writeHead(200, {
          "content-type": "text/html; charset=utf-8",
          "cache-control": "no-store",
          "cross-origin-opener-policy": "same-origin",
          "cross-origin-embedder-policy": "require-corp",
        });
        response.end(indexHtml);
        return;
      }
      const candidate = resolve(root, `.${decodeURIComponent(requestUrl.pathname)}`);
      if (candidate !== root && !candidate.startsWith(`${root}${sep}`)) {
        response.writeHead(403);
        response.end("forbidden");
        return;
      }
      const metadata = await stat(candidate);
      if (!metadata.isFile()) throw new Error("not a file");
      response.writeHead(200, {
        "content-type": mimeType(candidate),
        "cache-control": "no-store",
        "cross-origin-resource-policy": "same-origin",
      });
      response.end(await readFile(candidate));
    } catch {
      response.writeHead(404);
      response.end("not found");
    }
  });
  await new Promise((done, reject) => {
    httpServer.once("error", reject);
    httpServer.listen(0, "127.0.0.1", done);
  });
  return httpServer;
}

function mimeType(path) {
  switch (extname(path)) {
    case ".js":
      return "text/javascript; charset=utf-8";
    case ".json":
      return "application/json; charset=utf-8";
    case ".wasm":
      return "application/wasm";
    default:
      return "application/octet-stream";
  }
}

async function resolveChromeBinary() {
  const candidates = [
    process.env.CHROME_BIN,
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/usr/bin/google-chrome",
    "/usr/bin/google-chrome-stable",
    "/usr/bin/chromium",
    "/usr/bin/chromium-browser",
  ].filter((candidate) => typeof candidate === "string" && candidate.length > 0);
  for (const candidate of candidates) {
    try {
      const metadata = await stat(candidate);
      if (metadata.isFile()) return candidate;
    } catch {
      // Continue through the fixed candidate list.
    }
  }
  throw new Error("Chrome/Chromium was not found; set CHROME_BIN to an exact executable path");
}

async function launchChrome(binary, userDataDirectory) {
  if (process.platform === "darwin" && binary.includes(".app/Contents/MacOS/")) {
    return launchMacChrome(binary, userDataDirectory);
  }
  const child = spawn(binary, [
    "--headless=new",
    "--remote-debugging-port=0",
    `--user-data-dir=${userDataDirectory}`,
    "--no-first-run",
    "--no-default-browser-check",
    "--disable-background-networking",
    "--disable-component-update",
    "--disable-sync",
    "--metrics-recording-only",
    "--enable-unsafe-webgpu",
    "--use-angle=metal",
    "--disable-gpu-sandbox",
    "about:blank",
  ], {
    stdio: ["ignore", "ignore", "pipe"],
  });
  let diagnostics = "";
  const webSocketUrl = await new Promise((resolvePromise, reject) => {
    const timeout = setTimeout(() => {
      reject(new Error(`Chrome DevTools endpoint timeout:\n${diagnostics}`));
    }, 15_000);
    child.once("exit", (code, signal) => {
      clearTimeout(timeout);
      reject(new Error(`Chrome exited before DevTools was ready (${code ?? signal}):\n${diagnostics}`));
    });
    child.stderr.setEncoding("utf8");
    child.stderr.on("data", (chunk) => {
      diagnostics = `${diagnostics}${chunk}`.slice(-16_384);
      const match = diagnostics.match(/DevTools listening on (ws:\/\/[^\s]+)/);
      if (match !== null) {
        clearTimeout(timeout);
        resolvePromise(match[1]);
      }
    });
  });
  return {
    webSocketUrl,
    close: async () => {
      child.kill("SIGTERM");
      await waitForExit(child, 2_000).catch(() => child.kill("SIGKILL"));
    },
  };
}

async function launchMacChrome(binary, userDataDirectory) {
  let lastError;
  for (let attempt = 1; attempt <= 2; attempt += 1) {
    const profile = attempt === 1 ? userDataDirectory : `${userDataDirectory}-retry-${attempt}`;
    try {
      return await launchMacChromeAttempt(binary, profile);
    } catch (error) {
      lastError = error;
    }
  }
  throw lastError;
}

async function launchMacChromeAttempt(binary, userDataDirectory) {
  const appSuffix = ".app/Contents/MacOS/";
  const suffixIndex = binary.indexOf(appSuffix);
  const appBundle = binary.slice(0, suffixIndex + 4);
  const launcher = spawn("/usr/bin/open", [
    "-na",
    appBundle,
    "--args",
    "--headless=new",
    "--remote-debugging-port=0",
    "--remote-debugging-address=127.0.0.1",
    `--user-data-dir=${userDataDirectory}`,
    "--no-first-run",
    "--no-default-browser-check",
    "--disable-background-networking",
    "--disable-component-update",
    "--disable-sync",
    "--metrics-recording-only",
    "--enable-unsafe-webgpu",
    "--use-angle=metal",
    "--disable-gpu-sandbox",
    "about:blank",
  ], {
    stdio: ["ignore", "pipe", "pipe"],
  });
  let diagnostics = "";
  launcher.stdout.setEncoding("utf8");
  launcher.stderr.setEncoding("utf8");
  launcher.stdout.on("data", (chunk) => {
    diagnostics = `${diagnostics}${chunk}`.slice(-16_384);
  });
  launcher.stderr.on("data", (chunk) => {
    diagnostics = `${diagnostics}${chunk}`.slice(-16_384);
  });
  await new Promise((resolvePromise, reject) => {
    const timeout = setTimeout(
      () => reject(new Error(`macOS open command timed out:\n${diagnostics}`)),
      15_000,
    );
    launcher.once("error", (error) => {
      clearTimeout(timeout);
      reject(error);
    });
    launcher.once("exit", (code) => {
      clearTimeout(timeout);
      if (code === 0) resolvePromise();
      else reject(new Error(`macOS open command failed (${code}):\n${diagnostics}`));
    });
  });
  const pid = await pollChromePid(userDataDirectory);
  if (pid === null) {
    throw new Error("macOS Chrome started without an identifiable profile owner");
  }
  let webSocketUrl;
  try {
    webSocketUrl = await pollDevtoolsActivePort(userDataDirectory, 20_000);
  } catch (error) {
    await terminatePid(pid);
    throw error;
  }
  return {
    webSocketUrl,
    close: () => terminatePid(pid),
  };
}

async function pollDevtoolsActivePort(userDataDirectory, timeoutMilliseconds) {
  const path = join(userDataDirectory, "DevToolsActivePort");
  const deadline = Date.now() + timeoutMilliseconds;
  let lastError = "";
  while (Date.now() < deadline) {
    try {
      const lines = (await readFile(path, "utf8")).trim().split(/\r?\n/);
      const port = Number(lines[0]);
      const socketPath = lines[1];
      if (
        Number.isSafeInteger(port)
        && port >= 1
        && port <= 65_535
        && typeof socketPath === "string"
        && socketPath.startsWith("/devtools/browser/")
      ) {
        return `ws://127.0.0.1:${port}${socketPath}`;
      }
      lastError = "DevToolsActivePort has invalid content";
    } catch (error) {
      lastError = error instanceof Error ? error.message : String(error);
    }
    await delay(50);
  }
  throw new Error(`Chrome DevToolsActivePort timeout: ${lastError}`);
}

async function terminatePid(pid) {
  try {
    process.kill(pid, "SIGTERM");
  } catch (error) {
    if (error?.code === "ESRCH") return;
    throw error;
  }
  const deadline = Date.now() + 2_000;
  while (Date.now() < deadline) {
    try {
      process.kill(pid, 0);
    } catch (error) {
      if (error?.code === "ESRCH") return;
      throw error;
    }
    await delay(50);
  }
  process.kill(pid, "SIGKILL");
}

function chromePidForProfile(userDataDirectory) {
  const result = spawnSync("/bin/ps", ["-axo", "pid=,command="], {
    encoding: "utf8",
  });
  if (result.status !== 0) return null;
  const marker = `--user-data-dir=${userDataDirectory}`;
  for (const line of result.stdout.split("\n")) {
    if (!line.includes(marker) || line.includes("--type=")) continue;
    const match = line.trim().match(/^(\d+)\s/);
    if (match !== null) return Number(match[1]);
  }
  return null;
}

async function pollChromePid(userDataDirectory) {
  const deadline = Date.now() + 15_000;
  while (Date.now() < deadline) {
    const pid = chromePidForProfile(userDataDirectory);
    if (pid !== null) return pid;
    await delay(50);
  }
  return null;
}

async function runSmoke(browserWebSocketUrl, url, smokeOptions) {
  const browser = new CdpConnection(browserWebSocketUrl);
  await browser.open();
  try {
    const target = await browser.call("Target.createTarget", { url: "about:blank" });
    const attached = await browser.call("Target.attachToTarget", {
      targetId: target.targetId,
      flatten: true,
    });
    const sessionId = attached.sessionId;
    await browser.call("Page.enable", {}, sessionId);
    await browser.call("Runtime.enable", {}, sessionId);
    await browser.call("Page.navigate", { url }, sessionId);

    if (smokeOptions.button !== null) {
      const expression = `(() => {
        const element = document.getElementById(${JSON.stringify(smokeOptions.button)});
        if (!(element instanceof HTMLButtonElement) || element.disabled) return null;
        const rect = element.getBoundingClientRect();
        return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
      })()`;
      const point = await pollEvaluation(
        browser,
        sessionId,
        expression,
        (value) => value !== null && Number.isFinite(value.x) && Number.isFinite(value.y),
        smokeOptions.timeout,
      );
      await browser.call("Input.dispatchMouseEvent", {
        type: "mousePressed",
        x: point.x,
        y: point.y,
        button: "left",
        clickCount: 1,
      }, sessionId);
      await browser.call("Input.dispatchMouseEvent", {
        type: "mouseReleased",
        x: point.x,
        y: point.y,
        button: "left",
        clickCount: 1,
      }, sessionId);
    }

    return await pollEvaluation(
      browser,
      sessionId,
      `window[${JSON.stringify(smokeOptions.global)}] ?? null`,
      (value) => value !== null && value.complete === true,
      smokeOptions.timeout,
    );
  } finally {
    browser.close();
  }
}

async function pollEvaluation(connection, sessionId, expression, predicate, timeoutMilliseconds) {
  const deadline = Date.now() + timeoutMilliseconds;
  let lastValue = null;
  while (Date.now() < deadline) {
    const evaluated = await connection.call("Runtime.evaluate", {
      expression,
      awaitPromise: true,
      returnByValue: true,
    }, sessionId);
    if (evaluated.exceptionDetails !== undefined) {
      throw new Error(`browser evaluation failed: ${evaluated.exceptionDetails.text}`);
    }
    lastValue = evaluated.result?.value ?? null;
    if (predicate(lastValue)) return lastValue;
    await delay(50);
  }
  throw new Error(`browser smoke timed out; last value: ${JSON.stringify(lastValue)}`);
}

function CdpConnection(url) {
  this.url = url;
  this.socket = null;
  this.nextId = 1;
  this.pending = new Map();
  this.open = async () => {
    this.socket = new WebSocket(this.url);
    this.socket.addEventListener("message", (event) => {
      const message = JSON.parse(String(event.data));
      if (message.id === undefined) return;
      const pending = this.pending.get(message.id);
      if (pending === undefined) return;
      this.pending.delete(message.id);
      if (message.error !== undefined) {
        pending.reject(new Error(`CDP ${pending.method}: ${message.error.message}`));
      } else {
        pending.resolve(message.result ?? {});
      }
    });
    this.socket.addEventListener("close", () => {
      for (const pending of this.pending.values()) {
        pending.reject(new Error(`CDP connection closed during ${pending.method}`));
      }
      this.pending.clear();
    });
    await new Promise((resolvePromise, reject) => {
      this.socket.addEventListener("open", resolvePromise, { once: true });
      this.socket.addEventListener("error", reject, { once: true });
    });
  };
  this.call = (method, parameters = {}, sessionId = undefined) => {
    if (this.socket === null || this.socket.readyState !== WebSocket.OPEN) {
      return Promise.reject(new Error("CDP connection is not open"));
    }
    const id = this.nextId;
    this.nextId += 1;
    return new Promise((resolvePromise, reject) => {
      this.pending.set(id, { resolve: resolvePromise, reject, method });
      this.socket.send(JSON.stringify({
        id,
        method,
        params: parameters,
        ...(sessionId === undefined ? {} : { sessionId }),
      }));
    });
  };
  this.close = () => {
    this.socket?.close();
  };
}

function toUrlPath(path) {
  return path.split(sep).join("/");
}

function delay(milliseconds) {
  return new Promise((done) => setTimeout(done, milliseconds));
}

function waitForExit(child, timeoutMilliseconds) {
  if (child.exitCode !== null || child.signalCode !== null) return Promise.resolve();
  return new Promise((resolvePromise, reject) => {
    const timeout = setTimeout(() => reject(new Error("process exit timeout")), timeoutMilliseconds);
    child.once("exit", () => {
      clearTimeout(timeout);
      resolvePromise();
    });
  });
}

function waitForSignal() {
  return new Promise((done) => {
    process.once("SIGINT", done);
    process.once("SIGTERM", done);
  });
}
