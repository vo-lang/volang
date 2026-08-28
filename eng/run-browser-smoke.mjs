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
const projectRoot = resolve(options.staticRoot ?? options.project);
const htmlPath = join(projectRoot, options.html);
const temporaryRoot = await mkdtemp(join(tmpdir(), "vo-browser-smoke-"));
const outputRoot = options.staticRoot === null
  ? join(temporaryRoot, "compiled")
  : projectRoot;
const profileRoot = join(temporaryRoot, "chrome-profile");
let server;
let chrome;

try {
  let html = await readFile(htmlPath, "utf8");
  if (options.staticRoot === null) {
    compileProject(projectRoot, outputRoot, options.bundleEntry);
    const smokeModule = await findUniqueFile(outputRoot, "browser_smoke.js");
    html = html.replace(
      /\/src\/browser_smoke\.ts/g,
      `/${toUrlPath(relative(outputRoot, smokeModule))}`,
    );
  }
  server = await startServer(outputRoot, html, options.staticRoot === null);
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
} catch (error) {
  const diagnostics = chrome?.diagnostics?.() ?? "";
  if (diagnostics.length > 0) {
    throw new Error(`${error instanceof Error ? error.message : String(error)}\nChrome diagnostics:\n${diagnostics}`);
  }
  throw error;
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
    staticRoot: null,
    componentStateSmoke: false,
    uikitGallerySmoke: false,
    dataApplicationSmoke: false,
    contentSiteSmoke: false,
    mediaApplicationSmoke: false,
    studioWorkbenchSmoke: false,
    studioAotSmoke: false,
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
    } else if (argument === "--static-root" && value !== undefined) {
      parsed.staticRoot = value;
      if (parsed.html === "browser-smoke.html") parsed.html = "index.html";
      index += 1;
    } else if (argument === "--component-state-smoke") {
      parsed.componentStateSmoke = true;
    } else if (argument === "--uikit-gallery-smoke") {
      parsed.uikitGallerySmoke = true;
    } else if (argument === "--data-application-smoke") {
      parsed.dataApplicationSmoke = true;
    } else if (argument === "--content-site-smoke") {
      parsed.contentSiteSmoke = true;
    } else if (argument === "--media-application-smoke") {
      parsed.mediaApplicationSmoke = true;
    } else if (argument === "--studio-workbench-smoke") {
      parsed.studioWorkbenchSmoke = true;
    } else if (argument === "--studio-aot-smoke") {
      parsed.studioAotSmoke = true;
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
  const compiledSmoke = parsed.project.length > 0 && parsed.global.length > 0;
  const staticSmoke = parsed.staticRoot !== null
    && (parsed.componentStateSmoke || parsed.uikitGallerySmoke
      || parsed.dataApplicationSmoke || parsed.contentSiteSmoke
      || parsed.mediaApplicationSmoke || parsed.studioWorkbenchSmoke
      || parsed.studioAotSmoke);
  const staticScenarios = [
    parsed.componentStateSmoke,
    parsed.uikitGallerySmoke,
    parsed.dataApplicationSmoke,
    parsed.contentSiteSmoke,
    parsed.mediaApplicationSmoke,
    parsed.studioWorkbenchSmoke,
    parsed.studioAotSmoke,
  ].filter(Boolean).length;
  if (staticScenarios > 1) {
    throw new Error("choose exactly one static browser smoke scenario");
  }
  if (!compiledSmoke && !staticSmoke) {
    throw new Error(
      "usage: run-browser-smoke.mjs (--project <dir> --global <window-key> | --static-root <dir> (--component-state-smoke | --uikit-gallery-smoke | --data-application-smoke | --content-site-smoke | --media-application-smoke | --studio-workbench-smoke | --studio-aot-smoke))",
    );
  }
  if (!Number.isSafeInteger(parsed.timeout) || parsed.timeout < 1_000 || parsed.timeout > 120_000) {
    throw new Error("browser smoke timeout must be an integer between 1000 and 120000 ms");
  }
  if (parsed.global.length > 0 && !/^[A-Za-z_$][A-Za-z0-9_$]*$/.test(parsed.global)) {
    throw new Error("browser smoke global key is invalid");
  }
  return parsed;
}

function compileProject(project, output, bundleEntry) {
  if (bundleEntry !== null) {
    mkdirSync(output, { recursive: true });
    const executableName = process.platform === "win32" ? "rolldown.exe" : "rolldown";
    const executable = join(project, "node_modules", ".bin", executableName);
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
  const executable = [join(project, "node_modules", ".bin", executableName)].find((candidate) => {
    const result = spawnSync(candidate, ["--version"], {
      cwd: project,
      encoding: "utf8",
      env: process.env,
    });
    return result.status === 0;
  });
  if (executable === undefined) {
    throw new Error("TypeScript compiler was not found in the smoke project");
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

async function startServer(root, indexHtml, crossOriginIsolated) {
  const httpServer = createServer(async (request, response) => {
    try {
      const requestUrl = new URL(request.url ?? "/", "http://127.0.0.1");
      if (requestUrl.pathname === "/" || requestUrl.pathname === "/browser-smoke.html") {
        response.writeHead(200, {
          "content-type": "text/html; charset=utf-8",
          "cache-control": "no-store",
          ...(crossOriginIsolated ? {
            "cross-origin-opener-policy": "same-origin",
            "cross-origin-embedder-policy": "require-corp",
          } : {}),
        });
        response.end(indexHtml);
        return;
      }
      let candidate = resolve(root, `.${decodeURIComponent(requestUrl.pathname)}`);
      if (candidate !== root && !candidate.startsWith(`${root}${sep}`)) {
        response.writeHead(403);
        response.end("forbidden");
        return;
      }
      let metadata = await stat(candidate);
      if (metadata.isDirectory()) {
        candidate = join(candidate, "index.html");
        metadata = await stat(candidate);
      }
      if (!metadata.isFile()) throw new Error("not a file");
      response.writeHead(200, {
        "content-type": mimeType(candidate),
        "cache-control": "no-store",
        ...(crossOriginIsolated ? { "cross-origin-resource-policy": "same-origin" } : {}),
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
    case ".html":
      return "text/html; charset=utf-8";
    case ".css":
      return "text/css; charset=utf-8";
    case ".svg":
      return "image/svg+xml";
    case ".webmanifest":
      return "application/manifest+json; charset=utf-8";
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
  if (
    process.platform === "darwin"
    && binary.includes(".app/Contents/MacOS/")
    && process.env.VO_BROWSER_DIRECT_CHROME !== "1"
  ) {
    return launchMacChrome(binary, userDataDirectory);
  }
  const graphicsArguments = process.env.VO_BROWSER_DISABLE_GPU === "1"
    ? ["--disable-gpu"]
    : ["--enable-unsafe-webgpu", "--use-angle=metal", "--disable-gpu-sandbox"];
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
    ...graphicsArguments,
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
    diagnostics: () => diagnostics,
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
  const graphicsArguments = process.env.VO_BROWSER_DISABLE_GPU === "1"
    ? ["--disable-gpu"]
    : ["--enable-unsafe-webgpu", "--use-angle=metal", "--disable-gpu-sandbox"];
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
    ...graphicsArguments,
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
    diagnostics: () => diagnostics,
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
    await browser.call("Page.bringToFront", {}, sessionId);
    await pollEvaluation(
      browser,
      sessionId,
      `document.readyState === "complete" && window.location.href === ${JSON.stringify(url)}`,
      (value) => value === true,
      smokeOptions.timeout,
    );

    if (smokeOptions.componentStateSmoke) {
      // Keep the CDP connection alive until every component interaction has
      // completed; the surrounding finally block owns browser shutdown.
      return await runComponentStateSmoke(browser, sessionId, smokeOptions.timeout);
    }
    if (smokeOptions.uikitGallerySmoke) {
      return await runUikitGallerySmoke(browser, sessionId, smokeOptions.timeout);
    }
    if (smokeOptions.dataApplicationSmoke) {
      return await runDataApplicationSmoke(browser, sessionId, smokeOptions.timeout);
    }
    if (smokeOptions.contentSiteSmoke) {
      return await runContentSiteSmoke(browser, sessionId, smokeOptions.timeout);
    }
    if (smokeOptions.mediaApplicationSmoke) {
      return await runMediaApplicationSmoke(browser, sessionId, smokeOptions.timeout);
    }
    if (smokeOptions.studioWorkbenchSmoke) {
      return await runStudioWorkbenchSmoke(browser, sessionId, smokeOptions.timeout);
    }
    if (smokeOptions.studioAotSmoke) {
      return await runStudioAotSmoke(browser, sessionId, smokeOptions.timeout);
    }

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

async function runDataApplicationSmoke(browser, sessionId, timeoutMilliseconds) {
  const evaluate = async (expression) => {
    const evaluated = await browser.call("Runtime.evaluate", {
      expression,
      awaitPromise: true,
      returnByValue: true,
    }, sessionId);
    if (evaluated.exceptionDetails !== undefined) {
      throw new Error(`data application evaluation failed: ${evaluated.exceptionDetails.text}`);
    }
    return evaluated.result?.value ?? null;
  };
  const activate = async (selector, name) => {
    const activated = await evaluate(`(() => {
      const element = Array.from(document.querySelectorAll(${JSON.stringify(selector)})).find(
        (candidate) => candidate.getAttribute("aria-label") === ${JSON.stringify(name)}
          || (candidate.textContent ?? "").trim() === ${JSON.stringify(name)},
      );
      if (!(element instanceof HTMLElement) || element.getAttribute("aria-disabled") === "true"
        || (element instanceof HTMLButtonElement && element.disabled)) return false;
      element.scrollIntoView({ block: "center", inline: "center" });
      element.click();
      return true;
    })()`);
    if (activated !== true) throw new Error(`data application could not activate ${name}`);
  };
  const setInput = async (selector, value) => {
    const changed = await evaluate(`(() => {
      const input = document.querySelector(${JSON.stringify(selector)});
      if (!(input instanceof HTMLInputElement)) return false;
      input.focus();
      input.value = ${JSON.stringify(value)};
      input.setSelectionRange(input.value.length, input.value.length);
      input.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`);
    if (changed !== true) {
      const inputs = await evaluate(`Array.from(document.querySelectorAll("input"), (input) => ({
        type: input.type, role: input.getAttribute("role"), name: input.getAttribute("aria-label"),
        placeholder: input.placeholder, value: input.value,
      }))`);
      throw new Error(`data application could not edit ${selector}; inputs=${JSON.stringify(inputs)}`);
    }
  };
  const checkpoints = {};
  checkpoints.initial = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const pagination = document.querySelector('[role="navigation"][aria-label="Member pages"]');
      const treeRoot = document.querySelector('[role="treeitem"][aria-label="All members"]');
      const memberHeader = document.querySelector('[role="columnheader"][aria-label="Member"]');
      const chart = document.querySelector('[role="img"][aria-label="Weekly activity"]');
      return {
        heading: document.body.textContent?.includes("Operations dashboard") === true,
        total: document.body.textContent?.includes("100000") === true,
        synchronized: document.body.textContent?.includes("synchronized") === true,
        firstRow: document.querySelector('[role="rowheader"][aria-label="Member 0"]') !== null,
        materializedRows: document.querySelectorAll('[role="grid"][aria-label="Members"] [role="row"]').length,
        pages: pagination === null ? [] : Array.from(pagination.querySelectorAll("button"),
          (button) => (button.textContent ?? "").trim()),
        treeRoot: (treeRoot?.textContent ?? "").trim(),
        header: (memberHeader?.textContent ?? "").trim(),
        chart: chart?.getAttribute("aria-description") ?? "",
        diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
      };
    })()`,
    (value) => value?.heading === true && value?.total === true && value?.synchronized === true
      && value?.firstRow === true && value?.materializedRows > 1 && value?.materializedRows <= 32
      && JSON.stringify(value?.pages) === JSON.stringify(["1", "2", "3", "100"])
      && value?.treeRoot === "− All members"
      && value?.header === "Member ascending"
      && value?.chart === "Mon 62, Tue 84, Wed 47, Thu 91, Fri 76"
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );

  await activate('button[role="rowheader"]', "Member 0");
  checkpoints.selection = await pollEvaluation(
    browser,
    sessionId,
    `({
      selected: document.querySelector('[role="row"][aria-label="member-0"]')
        ?.getAttribute("aria-selected") ?? "",
      text: document.body.textContent ?? "",
    })`,
    (value) => value?.selected === "true" && value?.text.includes("member-0"),
    timeoutMilliseconds,
  );
  await activate("button", "Archive selected");
  checkpoints.optimistic = await pollEvaluation(
    browser,
    sessionId,
    `document.body.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("Archiving member-0"),
    timeoutMilliseconds,
  );
  await activate("button", "Commit optimistic change");
  checkpoints.committed = await pollEvaluation(
    browser,
    sessionId,
    `document.body.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("Archived member-0"),
    timeoutMilliseconds,
  );

  await activate('[role="navigation"][aria-label="Member pages"] button', "2");
  checkpoints.page = await pollEvaluation(
    browser,
    sessionId,
    `({
      current: document.querySelector('[role="navigation"][aria-label="Member pages"] [aria-current="page"]')
        ?.textContent ?? "",
      firstRow: document.querySelector('[role="rowheader"][aria-label="Member 1000"]') !== null,
    })`,
    (value) => value?.current.trim() === "2" && value?.firstRow === true,
    timeoutMilliseconds,
  );

  await setInput('input[aria-label="Filter members"]', "active");
  checkpoints.filter = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const pagination = document.querySelector('[role="navigation"][aria-label="Member pages"]');
      return {
        total: document.body.textContent?.includes("TOTAL MEMBERS250") === true,
        firstRow: document.querySelector('[role="rowheader"][aria-label="Member 0"]') !== null,
        pages: pagination?.querySelectorAll("button").length ?? -1,
      };
    })()`,
    (value) => value?.total === true && value?.firstRow === true && value?.pages === 1,
    timeoutMilliseconds,
  );
  await activate('[role="columnheader"]', "Member");
  checkpoints.sort = await pollEvaluation(
    browser,
    sessionId,
    `({
      description: document.querySelector('[role="columnheader"][aria-label="Member"]')
        ?.getAttribute("aria-description") ?? "",
      header: document.querySelector('[role="columnheader"][aria-label="Member"]')
        ?.textContent?.trim() ?? "",
      firstRow: document.querySelector('[role="rowheader"][aria-label="Member 249"]') !== null,
    })`,
    (value) => value?.description === "Sorted descending"
      && value?.header === "Member descending" && value?.firstRow === true,
    timeoutMilliseconds,
  );

  const offlineChanged = await evaluate(`(() => {
    const toggle = document.querySelector('[role="switch"][aria-label="Work offline"]');
    if (!(toggle instanceof HTMLInputElement)) return false;
    toggle.click();
    return true;
  })()`);
  if (offlineChanged !== true) throw new Error("data application could not enable offline mode");
  checkpoints.offline = await pollEvaluation(
    browser,
    sessionId,
    `document.body.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("offline cache"),
    timeoutMilliseconds,
  );

  await activate("button", "Open commands");
  await setInput('input[aria-label="Command palette query"]', "commit");
  checkpoints.commands = await pollEvaluation(
    browser,
    sessionId,
    `Array.from(document.querySelectorAll(
      '[role="listbox"][aria-label="Command palette results"] [role="option"]',
    ), (option) => (option.textContent ?? "").trim())`,
    (value) => Array.isArray(value)
      && value.length === 1 && value[0] === "Commit optimistic change",
    timeoutMilliseconds,
  );
  await activate(
    '[role="listbox"][aria-label="Command palette results"] [role="option"]',
    "Commit optimistic change",
  );
  checkpoints.commandActivated = await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[role="dialog"][aria-label="Command palette"]') === null`,
    (value) => value === true,
    timeoutMilliseconds,
  );

  await activate('[role="link"]', "Settings");
  checkpoints.settings = await pollEvaluation(
    browser,
    sessionId,
    `({
      path: location.pathname,
      workspace: document.querySelector('input[aria-label="Workspace name"]') !== null,
      owner: document.querySelector('input[aria-label="Owner"]') !== null,
      save: document.querySelector('button[aria-label="Save settings"]') !== null,
    })`,
    (value) => value?.path === "/settings" && value?.workspace === true
      && value?.owner === true && value?.save === true,
    timeoutMilliseconds,
  );
  await setInput('input[aria-label="Workspace name"]', "");
  await activate("button", "Save settings");
  checkpoints.validation = await pollEvaluation(
    browser,
    sessionId,
    `({
      alert: document.querySelector('[role="alert"]')?.textContent ?? "",
      invalid: document.querySelector('input[aria-label="Workspace name"]')
        ?.getAttribute("aria-invalid") ?? "",
    })`,
    (value) => value?.alert.includes("Workspace name is required") && value?.invalid === "true",
    timeoutMilliseconds,
  );
  await setInput('input[aria-label="Workspace name"]', "Operations");
  await activate("button", "Save settings");
  checkpoints.submission = await pollEvaluation(
    browser,
    sessionId,
    `({
      value: document.querySelector('input[aria-label="Workspace name"]')?.value ?? "",
      invalid: document.querySelector('input[aria-label="Workspace name"]')
        ?.getAttribute("aria-invalid") ?? "",
      disabled: document.querySelector('button[aria-label="Save settings"]')?.disabled ?? true,
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.value === "Operations" && value?.invalid !== "true"
      && value?.disabled === false && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  await activate('[role="link"]', "Back to dashboard");
  checkpoints.restored = await pollEvaluation(
    browser,
    sessionId,
    `({ path: location.pathname, text: document.body.textContent ?? "" })`,
    (value) => value?.path === "/" && value?.text.includes("Operations dashboard"),
    timeoutMilliseconds,
  );

  return { complete: true, passed: true, checkpoints };
}

async function runContentSiteSmoke(browser, sessionId, timeoutMilliseconds) {
  const routeFiles = new Map([
    ["/", "index.html"],
    ["/articles/wasm-aot", "articles/wasm-aot/index.html"],
    ["/guides/concurrency", "guides/concurrency/index.html"],
    ["/search", "search/index.html"],
    ["/subscribe", "subscribe/index.html"],
    ["/offline", "offline/index.html"],
  ]);
  const routeContent = new Map([
    ["/", "A content site with zero JavaScript application code"],
    ["/articles/wasm-aot", "Wasm AOT without a JavaScript framework"],
    ["/guides/concurrency", "Goroutines in interactive applications"],
    ["/search", "Search Field Notes"],
    ["/subscribe", "Subscribe to Field Notes"],
    ["/offline", "Field Notes is available offline"],
  ]);
  const ssrRoutes = {};
  for (const [route, file] of routeFiles) {
    const html = await readFile(join(projectRoot, file), "utf8");
    const activationMatch = html.match(
      /<script type="application\/json" id="volang-activation">([^<]*)<\/script>/,
    );
    const activation = activationMatch === null ? null : JSON.parse(activationMatch[1]);
    const expected = routeContent.get(route);
    if (expected === undefined || !html.includes(expected)
      || !html.includes('<div id="volang-root" data-volang-revision="1">')
      || !Array.isArray(activation)) {
      throw new Error(`content site SSR contract failed for ${route}`);
    }
    ssrRoutes[route] = {
      bytes: Buffer.byteLength(html),
      nodes: (html.match(/data-volang-node=/g) ?? []).length,
      activationEntries: activation.length,
      usefulContent: true,
    };
  }
  const manifest = JSON.parse(await readFile(join(projectRoot, "manifest.webmanifest"), "utf8"));
  const deployment = JSON.parse(await readFile(join(projectRoot, "deployment.json"), "utf8"));
  const headers = await readFile(join(projectRoot, "_headers"), "utf8");
  const serviceWorker = await readFile(join(projectRoot, "service-worker.js"), "utf8");
  if (manifest.name !== "Volang Field Notes" || manifest.display !== "standalone"
    || deployment.rendering !== "static-ssr-with-client-activation"
    || deployment.server_authority !== "native-aot-only"
    || deployment.routes.length !== routeFiles.size
    || !headers.includes("Content-Security-Policy:")
    || !headers.includes("Strict-Transport-Security:")
    || !serviceWorker.includes('const OFFLINE = "/offline/";')) {
    throw new Error("content site production artifact contract failed");
  }

  const evaluate = async (expression) => {
    const evaluated = await browser.call("Runtime.evaluate", {
      expression,
      awaitPromise: true,
      returnByValue: true,
    }, sessionId);
    if (evaluated.exceptionDetails !== undefined) {
      throw new Error(`content site evaluation failed: ${evaluated.exceptionDetails.text}`);
    }
    return evaluated.result?.value ?? null;
  };
  const navigateLink = async (name, path) => pollEvaluation(
    browser,
    sessionId,
    `(() => {
      if (location.pathname === ${JSON.stringify(path)}) return location.pathname;
      const link = Array.from(document.querySelectorAll('[role="link"]')).find(
        (candidate) => candidate.getAttribute("aria-label") === ${JSON.stringify(name)},
      );
      if (link instanceof HTMLElement) link.click();
      return location.pathname;
    })()`,
    (value) => value === path,
    timeoutMilliseconds,
  );
  const activate = async (selector, name) => {
    const activated = await evaluate(`(() => {
      const element = Array.from(document.querySelectorAll(${JSON.stringify(selector)})).find(
        (candidate) => candidate.getAttribute("aria-label") === ${JSON.stringify(name)}
          || (candidate.textContent ?? "").trim() === ${JSON.stringify(name)},
      );
      if (!(element instanceof HTMLElement) || element.getAttribute("aria-disabled") === "true"
        || (element instanceof HTMLButtonElement && element.disabled)) return false;
      element.click();
      return true;
    })()`);
    if (activated !== true) throw new Error(`content site could not activate ${name}`);
  };
  const setInput = async (name, value) => {
    const focused = await evaluate(`(() => {
      const input = document.querySelector('input[aria-label=${JSON.stringify(name)}]');
      if (!(input instanceof HTMLInputElement)) return false;
      input.focus();
      return document.activeElement === input;
    })()`);
    if (focused !== true) throw new Error(`content site could not focus ${name}`);
    await evaluate(`new Promise((resolve) => requestAnimationFrame(() => resolve(true)))`);
    const changed = await evaluate(`(() => {
      const input = document.querySelector('input[aria-label=${JSON.stringify(name)}]');
      if (!(input instanceof HTMLInputElement) || document.activeElement !== input) return false;
      input.value = ${JSON.stringify(value)};
      input.setSelectionRange(input.value.length, input.value.length);
      input.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`);
    if (changed !== true) throw new Error(`content site could not edit ${name}`);
  };

  const checkpoints = {
    ssr: ssrRoutes,
    artifacts: {
      manifest: manifest.name,
      rendering: deployment.rendering,
      routes: deployment.routes.length,
      securityHeaders: true,
      offlineFallback: true,
    },
  };
  checkpoints.initial = await pollEvaluation(
    browser,
    sessionId,
    `({
      title: document.title,
      language: document.documentElement.lang,
      direction: document.documentElement.dir,
      canonical: document.querySelector('link[rel="canonical"]')?.href ?? "",
      manifest: document.querySelector('link[rel="manifest"]')?.getAttribute("href") ?? "",
      home: document.querySelector('[role="main"]')?.textContent ?? "",
      activation: JSON.parse(document.getElementById("volang-activation")?.textContent ?? "[]").length,
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.title === "Volang Field Notes" && value?.language === "en"
      && value?.direction === "ltr" && value?.canonical.endsWith("/")
      && value?.manifest === "/manifest.webmanifest"
      && value?.home.includes("zero JavaScript application code")
      && value?.activation > 0 && value?.diagnostic === "",
    timeoutMilliseconds,
  );

  await navigateLink("Wasm AOT", "/articles/wasm-aot");
  checkpoints.article = await pollEvaluation(
    browser,
    sessionId,
    `({ path: location.pathname, main: document.querySelector('[role="main"]')?.textContent ?? "" })`,
    (value) => value?.path === "/articles/wasm-aot"
      && value?.main.includes("Wasm AOT without a JavaScript framework")
      && value?.main.includes("Server node identities")
      && !value?.main.includes("zero JavaScript application code"),
    timeoutMilliseconds,
  );

  await navigateLink("Search", "/search");
  await setInput("Search articles", "goroutines");
  checkpoints.search = await pollEvaluation(
    browser,
    sessionId,
    `({
      path: location.pathname,
      query: document.querySelector('input[aria-label="Search articles"]')?.value ?? "",
      main: document.querySelector('[role="main"]')?.textContent ?? "",
      results: Array.from(document.querySelectorAll('[role="search"] [role="heading"]'),
        (node) => (node.textContent ?? "").trim()),
    })`,
    (value) => value?.path === "/search" && value?.query === "goroutines"
      && JSON.stringify(value?.results) === JSON.stringify(["Goroutines in interactive applications"])
      && value?.main.includes("Scoped lifetime") && !value?.main.includes("Compilation"),
    timeoutMilliseconds,
  );

  await navigateLink("Subscribe", "/subscribe");
  await activate('[role="main"] button', "Join Field Notes");
  checkpoints.validation = await pollEvaluation(
    browser,
    sessionId,
    `({
      alert: document.querySelector('[role="alert"]')?.textContent ?? "",
      email: document.querySelector('input[aria-label="Email address"]')?.getAttribute("aria-invalid") ?? "",
      name: document.querySelector('input[aria-label="Display name"]')?.getAttribute("aria-invalid") ?? "",
    })`,
    (value) => value?.alert.includes("Email address is required")
      && value?.alert.includes("Display name is required")
      && value?.email === "true" && value?.name === "true",
    timeoutMilliseconds,
  );
  await setInput("Email address", "ada");
  checkpoints.invalidEmail = await pollEvaluation(
    browser,
    sessionId,
    `({
      alert: document.querySelector('[role="alert"]')?.textContent ?? "",
      invalid: document.querySelector('input[aria-label="Email address"]')
        ?.getAttribute("aria-invalid") ?? "",
    })`,
    (value) => value?.alert.includes("Enter a valid email address") && value?.invalid === "true",
    timeoutMilliseconds,
  );
  await setInput("Email address", "ada@example.test");
  checkpoints.validEmail = await pollEvaluation(
    browser,
    sessionId,
    `({
      value: document.querySelector('input[aria-label="Email address"]')?.value ?? "",
      invalid: document.querySelector('input[aria-label="Email address"]')
        ?.getAttribute("aria-invalid") ?? "",
    })`,
    (value) => value?.value === "ada@example.test" && value?.invalid !== "true",
    timeoutMilliseconds,
  );
  await setInput("Display name", "Ada");
  checkpoints.validFields = await pollEvaluation(
    browser,
    sessionId,
    `Array.from(document.querySelectorAll('[role="main"] input'), (input) => ({
      value: input.value,
      invalid: input.getAttribute("aria-invalid") ?? "",
    }))`,
    (value) => Array.isArray(value)
      && JSON.stringify(value.map((field) => field.value))
        === JSON.stringify(["ada@example.test", "Ada"])
      && value.every((field) => field.invalid !== "true"),
    timeoutMilliseconds,
  );
  await activate('[role="main"] button', "Join Field Notes");
  checkpoints.submission = await pollEvaluation(
    browser,
    sessionId,
    `({
      email: document.querySelector('input[aria-label="Email address"]')?.value ?? "",
      name: document.querySelector('input[aria-label="Display name"]')?.value ?? "",
      invalid: Array.from(document.querySelectorAll('[role="main"] input'),
        (input) => input.getAttribute("aria-invalid") ?? ""),
      status: document.querySelector('[role="status"]')?.textContent ?? "",
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.email === "ada@example.test" && value?.name === "Ada"
      && Array.isArray(value?.invalid) && value.invalid.every((item) => item !== "true")
      && value?.status.includes("Subscription confirmed")
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );

  const origin = await evaluate("location.origin");
  await browser.call("Page.navigate", { url: `${origin}/offline/` }, sessionId);
  checkpoints.offline = await pollEvaluation(
    browser,
    sessionId,
    `({
      ready: document.readyState,
      path: location.pathname,
      main: document.querySelector('[role="main"]')?.textContent ?? "",
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.ready === "complete" && value?.path === "/offline/"
      && value?.main.includes("Field Notes is available offline") && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  checkpoints.productionFetch = await pollEvaluation(
    browser,
    sessionId,
    `(async () => {
      const [manifestResponse, deploymentResponse, workerResponse] = await Promise.all([
        fetch('/manifest.webmanifest'), fetch('/deployment.json'), fetch('/service-worker.js'),
      ]);
      const registration = await navigator.serviceWorker.getRegistration('/');
      return {
        statuses: [manifestResponse.status, deploymentResponse.status, workerResponse.status],
        registration: registration?.scope ?? "",
      };
    })()`,
    (value) => JSON.stringify(value?.statuses) === JSON.stringify([200, 200, 200])
      && typeof value?.registration === "string" && value.registration.endsWith("/"),
    timeoutMilliseconds,
  );

  return { complete: true, passed: true, checkpoints };
}

async function runMediaApplicationSmoke(browser, sessionId, timeoutMilliseconds) {
  const routeFiles = new Map([
    ["/", "index.html"],
    ["/capture", "capture/index.html"],
    ["/offline", "offline/index.html"],
  ]);
  const routeContent = new Map([
    ["/", "Portable playback with a native media host"],
    ["/capture", "Camera capture with explicit recovery"],
    ["/offline", "Media controls remain available offline"],
  ]);
  const ssrRoutes = {};
  for (const [route, file] of routeFiles) {
    const html = await readFile(join(projectRoot, file), "utf8");
    const activationMatch = html.match(
      /<script type="application\/json" id="volang-activation">([^<]*)<\/script>/,
    );
    const activation = activationMatch === null ? null : JSON.parse(activationMatch[1]);
    if (!html.includes(routeContent.get(route))
      || !html.includes('<div id="volang-root" data-volang-revision="1">')
      || !Array.isArray(activation)) {
      throw new Error(`media application SSR contract failed for ${route}`);
    }
    ssrRoutes[route] = {
      bytes: Buffer.byteLength(html),
      nodes: (html.match(/data-volang-node=/g) ?? []).length,
      activationEntries: activation.length,
    };
  }
  const manifest = JSON.parse(await readFile(join(projectRoot, "manifest.webmanifest"), "utf8"));
  const deployment = JSON.parse(await readFile(join(projectRoot, "deployment.json"), "utf8"));
  const headers = await readFile(join(projectRoot, "_headers"), "utf8");
  const worker = await readFile(join(projectRoot, "service-worker.js"), "utf8");
  if (manifest.name !== "Volang Media Laboratory" || deployment.routes.length !== routeFiles.size
    || !headers.includes("Permissions-Policy: camera=(self), microphone=(self), display-capture=(self)")
    || !worker.includes('const OFFLINE = "/offline/";')) {
    throw new Error("media application production artifact contract failed");
  }

  const evaluate = async (expression) => {
    const evaluated = await browser.call("Runtime.evaluate", {
      expression, awaitPromise: true, returnByValue: true,
    }, sessionId);
    if (evaluated.exceptionDetails !== undefined) {
      throw new Error(`media application evaluation failed: ${evaluated.exceptionDetails.text}`);
    }
    return evaluated.result?.value ?? null;
  };
  const activate = async (name) => {
    await pollEvaluation(
      browser,
      sessionId,
      `Array.from(document.querySelectorAll('button, input'), (candidate) => ({
        label: candidate.getAttribute('aria-label') ?? '',
        text: (candidate.textContent ?? '').trim(),
        disabled: candidate.disabled,
      }))`,
      (value) => Array.isArray(value) && value.some((candidate) =>
        (candidate.label === name || candidate.text === name) && candidate.disabled === false),
      timeoutMilliseconds,
    );
    const activated = await evaluate(`(() => {
      const element = Array.from(document.querySelectorAll('button, input')).find(
        (candidate) => candidate.getAttribute('aria-label') === ${JSON.stringify(name)}
          || (candidate.textContent ?? '').trim() === ${JSON.stringify(name)},
      );
      if (!(element instanceof HTMLElement) || element.getAttribute('aria-disabled') === 'true') {
        return false;
      }
      element.scrollIntoView({ block: 'center', inline: 'center' });
      element.click();
      return true;
    })()`);
    if (activated !== true) throw new Error(`media application could not activate ${name}`);
  };
  const navigate = async (name, path) => pollEvaluation(
    browser,
    sessionId,
    `(() => {
      if (location.pathname === ${JSON.stringify(path)}) return location.pathname;
      const link = Array.from(document.querySelectorAll('[role="link"]')).find(
        (candidate) => candidate.getAttribute('aria-label') === ${JSON.stringify(name)},
      );
      if (link instanceof HTMLElement) link.click();
      return location.pathname;
    })()`,
    (value) => value === path,
    timeoutMilliseconds,
  );
  const statusIncludes = async (expected) => pollEvaluation(
    browser,
    sessionId,
    `Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? '')`,
    (value) => Array.isArray(value) && value.some((item) => item.includes(expected)),
    timeoutMilliseconds,
  );

  const checkpoints = {
    ssr: ssrRoutes,
    artifacts: { manifest: manifest.name, routes: deployment.routes.length, permissions: true },
  };
  checkpoints.initial = await pollEvaluation(
    browser,
    sessionId,
    `({
      main: document.querySelector('[role="main"]')?.textContent ?? document.body.textContent ?? '',
      graphics: document.querySelectorAll('[data-volang-graphics]').length,
      artwork: document.querySelector('img[aria-label="Volang Media Laboratory artwork"]')?.getAttribute('src') ?? '',
      platformMedia: document.querySelectorAll('[data-volang-platform-view]').length,
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    })`,
    (value) => value?.main.includes("Portable playback with a native media host")
      && value?.graphics >= 3 && value?.artwork.endsWith("/poster.svg")
      && value?.platformMedia >= 1 && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  await activate("Play media");
  checkpoints.play = await statusIncludes("Playback is running");
  await activate("Seek to 30 seconds");
  checkpoints.seek = await statusIncludes("Playback position is 30 seconds");
  await activate("Set volume to 50 percent");
  checkpoints.volume = await statusIncludes("Playback volume is 50 percent");
  await activate("Set speed to 1.5 times");
  checkpoints.rate = await statusIncludes("Playback speed is 1.5 times");
  await activate("Show visualizations");
  checkpoints.visualizations = await pollEvaluation(
    browser,
    sessionId,
    `({
      checked: document.querySelector('[role="switch"][aria-label="Show visualizations"]')?.checked ?? true,
      hidden: document.querySelector('[aria-label="Audio spectrum visualization"]')?.closest('[hidden]') !== null
        || document.querySelector('[aria-label="Audio spectrum visualization"]')?.getClientRects().length === 0,
    })`,
    (value) => value?.checked === false && value?.hidden === true,
    timeoutMilliseconds,
  );

  await navigate("Capture", "/capture");
  await activate("Request camera permission");
  checkpoints.permission = await statusIncludes("Camera permission granted");
  await activate("Start camera capture");
  checkpoints.capturing = await statusIncludes("Camera is capturing showcase-camera");
  await activate("Stop camera capture");
  checkpoints.stopped = await statusIncludes("Camera capture stopped");
  await activate("Test denied permission");
  checkpoints.denied = await statusIncludes("Camera failed: media capture permission denied");
  await activate("Recover camera session");
  checkpoints.recovered = await statusIncludes("Camera session recovered; permission is ready to request");

  const origin = await evaluate("location.origin");
  await browser.call("Page.navigate", { url: `${origin}/offline/` }, sessionId);
  checkpoints.offline = await pollEvaluation(
    browser,
    sessionId,
    `({
      ready: document.readyState,
      main: document.querySelector('[role="main"]')?.textContent ?? document.body.textContent ?? '',
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    })`,
    (value) => value?.ready === "complete"
      && value?.main.includes("Media controls remain available offline")
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  checkpoints.productionFetch = await pollEvaluation(
    browser,
    sessionId,
    `(async () => ({
      statuses: await Promise.all(['/manifest.webmanifest', '/deployment.json', '/service-worker.js']
        .map(async (path) => (await fetch(path)).status)),
      registration: (await navigator.serviceWorker.getRegistration('/'))?.scope ?? '',
    }))()`,
    (value) => JSON.stringify(value?.statuses) === JSON.stringify([200, 200, 200])
      && value?.registration.endsWith("/"),
    timeoutMilliseconds,
  );
  return { complete: true, passed: true, checkpoints };
}

async function runUikitGallerySmoke(browser, sessionId, timeoutMilliseconds) {
  const evaluate = async (expression) => {
    const evaluated = await browser.call("Runtime.evaluate", {
      expression,
      awaitPromise: true,
      returnByValue: true,
    }, sessionId);
    if (evaluated.exceptionDetails !== undefined) {
      throw new Error(`UIKit gallery evaluation failed: ${evaluated.exceptionDetails.text}`);
    }
    return evaluated.result?.value ?? null;
  };
  const activateButton = async (name) => {
    const activated = await evaluate(`(() => {
      const button = Array.from(document.querySelectorAll("button")).find(
        (candidate) => candidate.getAttribute("aria-label") === ${JSON.stringify(name)}
          || (candidate.textContent ?? "").trim() === ${JSON.stringify(name)},
      );
      if (!(button instanceof HTMLButtonElement) || button.disabled) return false;
      button.scrollIntoView({ block: "center", inline: "center" });
      button.click();
      return true;
    })()`);
    if (activated !== true) throw new Error(`UIKit gallery could not activate ${name}`);
  };
  const focusElement = async (selector) => {
    const focused = await evaluate(`(() => {
      const element = document.querySelector(${JSON.stringify(selector)});
      if (!(element instanceof HTMLElement)) return false;
      element.scrollIntoView({ block: "center", inline: "center" });
      element.focus();
      return document.activeElement === element;
    })()`);
    if (focused !== true) throw new Error(`UIKit gallery could not focus ${selector}`);
  };
  const pressKey = async (key, code, windowsVirtualKeyCode) => {
    await browser.call("Input.dispatchKeyEvent", {
      type: "keyDown", key, code, windowsVirtualKeyCode,
    }, sessionId);
    await browser.call("Input.dispatchKeyEvent", {
      type: "keyUp", key, code, windowsVirtualKeyCode,
    }, sessionId);
  };
  const checkpoints = {};
  checkpoints.initial = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const checkbox = document.querySelector('[role="checkbox"][aria-label="Include source maps"]');
      const textarea = document.querySelector('textarea[aria-label="Release notes"]');
      const slider = document.querySelector('[role="slider"][aria-label="Optimization level"]');
      const runtime = Array.from(document.querySelectorAll("button")).find(
        (button) => (button.textContent ?? "").includes("Development runtime"),
      );
      const targets = Array.from(document.querySelectorAll("button")).find(
        (button) => (button.textContent ?? "").includes("Release targets"),
      );
      return {
        title: document.body.textContent?.includes("Volang UI component gallery") === true,
        checkbox: checkbox instanceof HTMLInputElement ? checkbox.checked : null,
        textarea: textarea instanceof HTMLTextAreaElement
          && textarea.required && textarea.getAttribute("aria-invalid") === "false",
        slider: slider instanceof HTMLInputElement
          ? [slider.value, slider.min, slider.max, slider.step] : [],
        accordion: [runtime?.getAttribute("aria-expanded"), targets?.getAttribute("aria-expanded")],
        accordionText: [(runtime?.textContent ?? "").trim(), (targets?.textContent ?? "").trim()],
        diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
      };
    })()`,
    (value) => value?.title === true
      && value?.checkbox === true
      && value?.textarea === true
      && JSON.stringify(value?.slider) === JSON.stringify(["72", "0", "100", "1"])
      && JSON.stringify(value?.accordion) === JSON.stringify(["true", "false"])
      && JSON.stringify(value?.accordionText) === JSON.stringify(["−  Development runtime", "+  Release targets"])
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );

  await focusElement('[role="checkbox"][aria-label="Include source maps"]');
  await pressKey(" ", "Space", 32);
  checkpoints.checkbox = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const checkbox = document.querySelector('[role="checkbox"][aria-label="Include source maps"]');
      return checkbox instanceof HTMLInputElement ? checkbox.checked : null;
    })()`,
    (value) => value === false,
    timeoutMilliseconds,
  );

  const notes = "AOT gallery input with IME-ready multiline text.";
  const textareaChanged = await evaluate(`(() => {
    const textarea = document.querySelector('textarea[aria-label="Release notes"]');
    if (!(textarea instanceof HTMLTextAreaElement)) return false;
    textarea.focus();
    textarea.value = ${JSON.stringify(notes)};
    textarea.setSelectionRange(textarea.value.length, textarea.value.length);
    textarea.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
    return true;
  })()`);
  if (textareaChanged !== true) throw new Error("UIKit gallery could not edit the text area");
  checkpoints.textArea = await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('textarea[aria-label="Release notes"]')?.value ?? null`,
    (value) => value === notes,
    timeoutMilliseconds,
  );

  await focusElement('[role="slider"][aria-label="Optimization level"]');
  await pressKey("ArrowRight", "ArrowRight", 39);
  checkpoints.slider = await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[role="slider"][aria-label="Optimization level"]')?.value ?? null`,
    (value) => value === "73",
    timeoutMilliseconds,
  );

  await activateButton("+  Release targets");
  checkpoints.accordion = await pollEvaluation(
    browser,
    sessionId,
    `Array.from(document.querySelectorAll("button"), (button) => ({
      text: (button.textContent ?? "").trim(), expanded: button.getAttribute("aria-expanded"),
    })).filter((item) => item.text.includes("runtime") || item.text.includes("Release targets"))`,
    (value) => Array.isArray(value)
      && value.some((item) => item.text === "+  Development runtime" && item.expanded === "false")
      && value.some((item) => item.text === "−  Release targets" && item.expanded === "true"),
    timeoutMilliseconds,
  );

  await activateButton("New");
  checkpoints.menu = await pollEvaluation(
    browser,
    sessionId,
    `Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
      .some((text) => text.includes("Created a document"))`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  const autoSaveChanged = await evaluate(`(() => {
    const control = document.querySelector('[role="menuitemcheckbox"][aria-label="Auto save"]');
    if (!(control instanceof HTMLInputElement) || control.disabled) return false;
    control.click();
    return true;
  })()`);
  if (autoSaveChanged !== true) throw new Error("UIKit gallery could not toggle Auto save");
  checkpoints.menuToggle = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const control = document.querySelector('[role="menuitemcheckbox"][aria-label="Auto save"]');
      return control instanceof HTMLInputElement ? control.checked : null;
    })()`,
    (value) => value === false,
    timeoutMilliseconds,
  );

  await activateButton("Package ascending");
  checkpoints.dataSort = await pollEvaluation(
    browser,
    sessionId,
    `({
      description: document.querySelector('[role="columnheader"][aria-label="Package"]')
        ?.getAttribute("aria-description") ?? "",
      text: document.querySelector('[role="columnheader"][aria-label="Package"]')
        ?.textContent?.trim() ?? "",
    })`,
    (value) => value?.description === "Sorted descending" && value?.text === "Package descending",
    timeoutMilliseconds,
  );
  await activateButton("Renderers");
  checkpoints.dataSelection = await pollEvaluation(
    browser,
    sessionId,
    `({
      selected: document.querySelector('[role="row"][aria-label="renderer"]')
        ?.getAttribute("aria-selected") ?? "",
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .find((text) => text.includes("Selected data row")) ?? "",
    })`,
    (value) => value?.selected === "true" && value?.status.includes("renderer"),
    timeoutMilliseconds,
  );
  await activateButton("− UIKit");
  checkpoints.treeCollapsed = await pollEvaluation(
    browser,
    sessionId,
    `({
      expanded: document.querySelector('[role="treeitem"][aria-label="UIKit"]')
        ?.getAttribute("aria-expanded") ?? "",
      components: document.querySelector('[role="treeitem"][aria-label="Components"]') !== null,
    })`,
    (value) => value?.expanded === "false" && value?.components === false,
    timeoutMilliseconds,
  );
  await activateButton("+ UIKit");
  checkpoints.treeExpanded = await pollEvaluation(
    browser,
    sessionId,
    `({
      expanded: document.querySelector('[role="treeitem"][aria-label="UIKit"]')
        ?.getAttribute("aria-expanded") ?? "",
      components: document.querySelector('[role="treeitem"][aria-label="Components"]') !== null,
      chart: document.querySelector('[role="img"][aria-label="Runtime throughput"]')
        ?.getAttribute("aria-description") ?? "",
    })`,
    (value) => value?.expanded === "true" && value?.components === true
      && value?.chart === "VM 58, JIT 86, AOT 96",
    timeoutMilliseconds,
  );

  await activateButton("Show toast");
  checkpoints.toastVisible = await pollEvaluation(
    browser,
    sessionId,
    `({
      message: document.querySelector('[role="status"][aria-label="Release ready"]')?.textContent ?? "",
      region: document.querySelector('[role="region"][aria-label="Notifications"]') !== null,
    })`,
    (value) => value?.message.includes("artifacts passed verification") && value?.region === true,
    timeoutMilliseconds,
  );
  await activateButton("Dismiss");
  checkpoints.toastDismissed = await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[role="status"][aria-label="Release ready"]') === null`,
    (value) => value === true,
    timeoutMilliseconds,
  );

  await activateButton("Open drawer");
  await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[role="dialog"][aria-label="Release inspector"]') !== null`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await pressKey("Escape", "Escape", 27);
  checkpoints.drawer = await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[role="dialog"][aria-label="Release inspector"]') === null`,
    (value) => value === true,
    timeoutMilliseconds,
  );

  for (const name of ["Dark theme", "High contrast", "Right-to-left", "Compact density"]) {
    const changed = await evaluate(`(() => {
      const control = document.querySelector('[role="switch"][aria-label=${JSON.stringify(name)}]');
      if (!(control instanceof HTMLInputElement) || control.disabled) return false;
      control.click();
      return true;
    })()`);
    if (changed !== true) throw new Error(`UIKit gallery could not toggle ${name}`);
  }
  checkpoints.environment = await pollEvaluation(
    browser,
    sessionId,
    `({
      checked: Array.from(document.querySelectorAll('[role="switch"]'), (control) =>
        control instanceof HTMLInputElement ? control.checked : null),
      direction: document.querySelector('[dir="rtl"]')?.getAttribute("dir") ?? "",
      caption: document.body.textContent ?? "",
    })`,
    (value) => Array.isArray(value?.checked)
      && value.checked.slice(0, 4).every((item) => item === true)
      && value?.direction === "rtl"
      && value?.caption.includes("direction RTL"),
    timeoutMilliseconds,
  );

  const rtlRestored = await evaluate(`(() => {
    const control = document.querySelector('[role="switch"][aria-label="Right-to-left"]');
    if (!(control instanceof HTMLInputElement) || control.disabled) return false;
    control.click();
    return true;
  })()`);
  if (rtlRestored !== true) throw new Error("UIKit gallery could not restore left-to-right mode");
  checkpoints.environmentRestored = await pollEvaluation(
    browser,
    sessionId,
    `({
      direction: document.querySelector('[dir="ltr"]')?.getAttribute("dir") ?? "",
      caption: document.body.textContent ?? "",
    })`,
    (value) => value?.direction === "ltr" && value?.caption.includes("direction LTR")
      && !value?.caption.includes("direction RTL"),
    timeoutMilliseconds,
  );

  await activateButton("Open commands");
  await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[role="combobox"][aria-label="Command palette query"]') instanceof HTMLInputElement`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  const queryChanged = await evaluate(`(() => {
    const input = document.querySelector('[role="combobox"][aria-label="Command palette query"]');
    if (!(input instanceof HTMLInputElement)) return false;
    input.value = "publish";
    input.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
    return true;
  })()`);
  if (queryChanged !== true) throw new Error("UIKit gallery could not filter commands");
  checkpoints.commandFiltered = await pollEvaluation(
    browser,
    sessionId,
    `Array.from(document.querySelectorAll(
      '[role="listbox"][aria-label="Command palette results"] [role="option"]',
    ), (option) => (option.textContent ?? "").trim())`,
    (value) => Array.isArray(value) && value.length === 1 && value[0].includes("Publish release"),
    timeoutMilliseconds,
  );
  await focusElement('[role="combobox"][aria-label="Command palette query"]');
  await pressKey("Enter", "Enter", 13);
  checkpoints.command = await pollEvaluation(
    browser,
    sessionId,
    `({
      closed: document.querySelector('[role="dialog"][aria-label="Command palette"]') === null,
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .find((text) => text.includes("Command executed")) ?? "",
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.closed === true
      && value?.status.includes("release.publish")
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );

  return { complete: true, passed: true, checkpoints };
}

async function runComponentStateSmoke(browser, sessionId, timeoutMilliseconds) {
  await pollEvaluation(
    browser,
    sessionId,
    `({
      interactive: performance.getEntriesByName("volang-aot-interactive", "mark").length > 0,
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.interactive === true && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  const buttonTexts = `Array.from(document.querySelectorAll("button"), (button) =>
    (button.textContent ?? "").trim())`;
  const expectButtons = async (expected) => {
    const observed = await pollEvaluation(
      browser,
      sessionId,
      `({
        buttons: ${buttonTexts},
        diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
      })`,
      (value) => value !== null
        && Array.isArray(value.buttons)
        && value.buttons.length === expected.length
        && value.buttons.every((item, index) => item === expected[index]),
      timeoutMilliseconds,
    );
    return observed.buttons;
  };
  const clickButton = async (text) => {
    const point = await pollEvaluation(
      browser,
      sessionId,
      `(() => {
        const button = Array.from(document.querySelectorAll("button")).find(
          (candidate) => (candidate.textContent ?? "").trim() === ${JSON.stringify(text)},
        );
        if (!(button instanceof HTMLButtonElement) || button.disabled) return null;
        const rect = button.getBoundingClientRect();
        return { x: rect.left + rect.width / 2, y: rect.top + rect.height / 2 };
      })()`,
      (value) => value !== null && Number.isFinite(value.x) && Number.isFinite(value.y),
      timeoutMilliseconds,
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
  };
  const checkpoints = {};
  checkpoints.initial = await expectButtons(["Reorder", "Alpha 0", "Beta 0"]);
  await clickButton("Alpha 0");
  checkpoints.afterCounterClick = await expectButtons(["Reorder", "Alpha 1", "Beta 0"]);
  await clickButton("Reorder");
  checkpoints.afterMovement = await expectButtons(["Remove Beta", "Beta 0", "Alpha 1"]);
  await clickButton("Remove Beta");
  checkpoints.afterRemoval = await expectButtons(["Insert Beta", "Alpha 1"]);
  await clickButton("Insert Beta");
  checkpoints.afterInsertion = await expectButtons(["Replace Alpha", "Alpha 1", "Beta 0"]);
  await clickButton("Replace Alpha");
  checkpoints.afterReplacement = await expectButtons(["Complete", "Alpha 0", "Beta 0"]);
  return {
    complete: true,
    passed: true,
    checkpoints,
  };
}

async function runStudioWorkbenchSmoke(browser, sessionId, timeoutMilliseconds) {
  const routeFiles = new Map([
    ["/", "index.html"],
    ["/offline", "offline/index.html"],
  ]);
  const routeContent = new Map([
    ["/", "Volang Studio"],
    ["/offline", "Your local workspace remains available"],
  ]);
  const ssrRoutes = {};
  for (const [route, file] of routeFiles) {
    const html = await readFile(join(projectRoot, file), "utf8");
    const activationMatch = html.match(
      /<script type="application\/json" id="volang-activation">([^<]*)<\/script>/,
    );
    const activation = activationMatch === null ? null : JSON.parse(activationMatch[1]);
    if (!html.includes(routeContent.get(route))
      || !html.includes('<div id="volang-root" data-volang-revision="1">')
      || !Array.isArray(activation)) {
      throw new Error(`Studio Workbench SSR contract failed for ${route}`);
    }
    ssrRoutes[route] = {
      bytes: Buffer.byteLength(html),
      nodes: (html.match(/data-volang-node=/g) ?? []).length,
      activationEntries: activation.length,
    };
  }
  const manifest = JSON.parse(await readFile(join(projectRoot, "manifest.webmanifest"), "utf8"));
  const deployment = JSON.parse(await readFile(join(projectRoot, "deployment.json"), "utf8"));
  const headers = await readFile(join(projectRoot, "_headers"), "utf8");
  const worker = await readFile(join(projectRoot, "service-worker.js"), "utf8");
  if (manifest.name !== "Volang Studio" || deployment.routes.length !== routeFiles.size
    || !headers.includes("Permissions-Policy: camera=(), microphone=(), geolocation=()")
    || !worker.includes('const OFFLINE = "/offline/";')) {
    throw new Error("Studio Workbench production artifact contract failed");
  }

  const evaluate = async (expression) => {
    const evaluated = await browser.call("Runtime.evaluate", {
      expression, awaitPromise: true, returnByValue: true,
    }, sessionId);
    if (evaluated.exceptionDetails !== undefined) {
      throw new Error(`Studio Workbench evaluation failed: ${evaluated.exceptionDetails.text}`);
    }
    return evaluated.result?.value ?? null;
  };
  const activate = async (name) => {
    await pollEvaluation(
      browser,
      sessionId,
      `Array.from(document.querySelectorAll('button'), (candidate) => ({
        label: candidate.getAttribute('aria-label') ?? '',
        text: (candidate.textContent ?? '').trim(),
        disabled: candidate.disabled,
      }))`,
      (value) => Array.isArray(value) && value.some((candidate) =>
        (candidate.label === name || candidate.text === name) && candidate.disabled === false),
      timeoutMilliseconds,
    );
    const activated = await evaluate(`(() => {
      const element = Array.from(document.querySelectorAll('button')).find(
        (candidate) => candidate.getAttribute('aria-label') === ${JSON.stringify(name)}
          || (candidate.textContent ?? '').trim() === ${JSON.stringify(name)},
      );
      if (!(element instanceof HTMLButtonElement) || element.disabled) return false;
      element.scrollIntoView({ block: 'center', inline: 'center' });
      element.click();
      return true;
    })()`);
    if (activated !== true) throw new Error(`Studio Workbench could not activate ${name}`);
  };
  const edit = async (accessibleName, value) => {
    const focused = await evaluate(`(() => {
      const editor = document.querySelector('[aria-label=${JSON.stringify(accessibleName)}]');
      if (!(editor instanceof HTMLTextAreaElement) && !(editor instanceof HTMLInputElement)) return false;
      editor.focus();
      return document.activeElement === editor;
    })()`);
    if (focused !== true) throw new Error(`Studio Workbench could not focus ${accessibleName}`);
    await evaluate("new Promise((resolve) => requestAnimationFrame(() => resolve(true)))");
    const changed = await evaluate(`(() => {
      const editor = document.querySelector('[aria-label=${JSON.stringify(accessibleName)}]');
      if ((!(editor instanceof HTMLTextAreaElement) && !(editor instanceof HTMLInputElement))
        || document.activeElement !== editor) return false;
      editor.value = ${JSON.stringify(value)};
      editor.setSelectionRange(editor.value.length, editor.value.length);
      editor.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertText' }));
      return true;
    })()`);
    if (changed !== true) throw new Error(`Studio Workbench could not edit ${accessibleName}`);
  };
  const statusIncludes = async (expected) => pollEvaluation(
    browser,
    sessionId,
    `Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? '')`,
    (value) => Array.isArray(value) && value.some((item) => item.includes(expected)),
    timeoutMilliseconds,
  );

  const checkpoints = {
    ssr: ssrRoutes,
    artifacts: { manifest: manifest.name, routes: deployment.routes.length, security: true },
  };
  checkpoints.initial = await pollEvaluation(
    browser,
    sessionId,
    `({
      title: document.title,
      workspace: document.querySelector('[data-testid="volang-workspace"]') !== null,
      editor: document.querySelector('[data-testid="volang-code-editor"]')?.getAttribute('aria-label') ?? '',
      tabs: Array.from(document.querySelectorAll('[role="tab"]'), (tab) => tab.textContent ?? ''),
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    })`,
    (value) => value?.title === "Volang Studio" && value?.workspace === true
      && value?.editor === "main.vo code editor"
      && value?.tabs.includes("Editor") && value?.tabs.includes("Docs")
      && value?.tabs.includes("Console") && value?.tabs.includes("Preview")
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );

  const source = `package main

import "github.com/vo-lang/ui"
import "github.com/vo-lang/ui/kit"

func App() ui.View {
\treturn kit.Page(kit.Title("Browser edited Studio"))
}

func main() {
\tif err := ui.Mount(App); err != nil { panic(err.Error()) }
}
`;
  await edit("main.vo code editor", source);
  checkpoints.edited = await pollEvaluation(
    browser,
    sessionId,
    `({
      value: document.querySelector('textarea[aria-label="main.vo code editor"]')?.value ?? '',
      body: document.body.textContent ?? '',
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    })`,
    (value) => value?.value === source && value?.body.includes("UNSAVED")
      && value?.body.includes("Unsaved changes in main.vo at version 1")
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  await activate("Save active file");
  checkpoints.saved = await statusIncludes("Saved main.vo at version 1");

  await activate("vo.mod");
  checkpoints.fileSwitch = await pollEvaluation(
    browser,
    sessionId,
    `({
      label: document.querySelector('[data-testid="volang-code-editor"]')?.getAttribute('aria-label') ?? '',
      value: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? '',
    })`,
    (value) => value?.label === "vo.mod code editor"
      && value?.value.includes("module example.com/studio-workbench"),
    timeoutMilliseconds,
  );
  await activate("main.vo");
  await activate("Wasm AOT release");
  await statusIncludes("Target changed to Wasm AOT release");
  await activate("Run project");
  checkpoints.run = await pollEvaluation(
    browser,
    sessionId,
    `({
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? ''),
      canvas: document.querySelector('[aria-label="Embedded application preview"]') !== null,
      graphics: document.querySelectorAll('[data-volang-graphics]').length,
    })`,
    (value) => Array.isArray(value?.status)
      && value.status.some((item) => item.includes("Run 1 completed with Wasm AOT release"))
      && value.status.some((item) => item.includes("Preview synchronized after run 1"))
      && value?.canvas === true && value?.graphics >= 1,
    timeoutMilliseconds,
  );
  await activate("Show Console");
  checkpoints.console = await pollEvaluation(
    browser,
    sessionId,
    `document.body.textContent ?? ''`,
    (value) => typeof value === "string"
      && value.includes("Run 1 completed with Wasm AOT release")
      && value.includes("0 PROBLEMS"),
    timeoutMilliseconds,
  );

  await activate("Open command palette");
  const commandQueryChanged = await evaluate(`(() => {
    const input = document.querySelector('[role="combobox"][aria-label="Command palette query"]');
    if (!(input instanceof HTMLInputElement)) return false;
    input.value = 'native';
    input.setSelectionRange(input.value.length, input.value.length);
    input.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertText' }));
    return true;
  })()`);
  if (commandQueryChanged !== true) {
    throw new Error("Studio Workbench could not filter its command palette");
  }
  checkpoints.commandFiltered = await pollEvaluation(
    browser,
    sessionId,
    `({
      open: document.querySelector('[role="dialog"][aria-label="Command palette"]') !== null,
      query: document.querySelector('[role="combobox"][aria-label="Command palette query"]')?.value ?? '',
      options: Array.from(document.querySelectorAll(
        '[role="listbox"][aria-label="Command palette results"] [role="option"]',
      ), (option) => (option.textContent ?? '').trim()),
      body: (document.body.textContent ?? '').slice(-500),
    })`,
    (value) => value?.open === true && value?.query === "native"
      && Array.isArray(value?.options)
      && value.options.some((option) => option.includes("Use Native AOT Release")),
    timeoutMilliseconds,
  );
  const nativeCommandActivated = await evaluate(`(() => {
    const option = Array.from(document.querySelectorAll(
      '[role="listbox"][aria-label="Command palette results"] [role="option"]',
    )).find((candidate) => (candidate.textContent ?? '').includes('Use Native AOT Release'));
    if (!(option instanceof HTMLElement)) return false;
    option.click();
    return true;
  })()`);
  if (nativeCommandActivated !== true) {
    throw new Error("Studio Workbench could not activate its filtered Native AOT command");
  }
  checkpoints.palette = await statusIncludes("Target changed to Native AOT release");
  await activate("Show Documentation");
  checkpoints.documentation = await pollEvaluation(
    browser,
    sessionId,
    `document.body.textContent ?? ''`,
    (value) => typeof value === "string" && value.includes("Build once, run everywhere")
      && value.includes("Structured concurrency"),
    timeoutMilliseconds,
  );
  await activate("Reset Workspace Layout");
  checkpoints.restored = await statusIncludes("Workspace layout restored");

  const origin = await evaluate("location.origin");
  await browser.call("Page.navigate", { url: `${origin}/offline/` }, sessionId);
  checkpoints.offline = await pollEvaluation(
    browser,
    sessionId,
    `({
      ready: document.readyState,
      main: document.body.textContent ?? '',
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    })`,
    (value) => value?.ready === "complete"
      && value?.main.includes("Your local workspace remains available")
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  checkpoints.productionFetch = await pollEvaluation(
    browser,
    sessionId,
    `(async () => ({
      statuses: await Promise.all(['/manifest.webmanifest', '/deployment.json', '/service-worker.js']
        .map(async (path) => (await fetch(path)).status)),
      registration: (await navigator.serviceWorker.getRegistration('/'))?.scope ?? '',
    }))()`,
    (value) => JSON.stringify(value?.statuses) === JSON.stringify([200, 200, 200])
      && value?.registration.endsWith("/"),
    timeoutMilliseconds,
  );
  return { complete: true, passed: true, checkpoints };
}

async function runStudioAotSmoke(browser, sessionId, timeoutMilliseconds) {
  const clickPoint = async (expression) => {
    const point = await pollEvaluation(
      browser,
      sessionId,
      expression,
      (value) => value !== null && Number.isFinite(value.x) && Number.isFinite(value.y),
      timeoutMilliseconds,
    );
    await browser.call("Input.dispatchMouseEvent", {
      type: "mousePressed", x: point.x, y: point.y, button: "left", clickCount: 1,
    }, sessionId);
    await browser.call("Input.dispatchMouseEvent", {
      type: "mouseReleased", x: point.x, y: point.y, button: "left", clickCount: 1,
    }, sessionId);
  };
  const activateButton = async (accessibleName) => {
    await pollEvaluation(
      browser,
      sessionId,
      `Array.from(document.querySelectorAll("button")).some(
        (button) => button.getAttribute("aria-label") === ${JSON.stringify(accessibleName)}
          && !button.disabled,
      )`,
      (value) => value === true,
      timeoutMilliseconds,
    );
    const activated = await browser.call("Runtime.evaluate", {
      expression: `(() => {
        const button = Array.from(document.querySelectorAll("button")).find(
          (candidate) => candidate.getAttribute("aria-label") === ${JSON.stringify(accessibleName)},
        );
        if (!(button instanceof HTMLButtonElement) || button.disabled) return false;
        button.click();
        return true;
      })()`,
      returnByValue: true,
    }, sessionId);
    if (activated.result?.value !== true) {
      throw new Error(`Studio AOT smoke could not activate ${accessibleName}`);
    }
  };
  const checkpoints = {};
  checkpoints.home = await pollEvaluation(
    browser,
    sessionId,
    `({
      starter: Array.from(document.querySelectorAll("button")).some(
        (button) => button.getAttribute("aria-label") === "Open Interactive counter example in Studio",
      ),
      project: Array.from(document.querySelectorAll("button")).some(
        (button) => button.getAttribute("aria-label") === "Open project hello-studio",
      ),
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.starter === true && value?.project === true && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  checkpoints.performance = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const startup = performance.getEntriesByName('volang-aot-startup', 'measure').at(-1);
      const host = performance.getEntriesByName('volang-aot-host-startup', 'measure').at(-1);
      const image = performance.getEntriesByName('volang-aot-image-fetch', 'measure').at(-1);
      const resource = performance.getEntriesByType('resource').find(
        (entry) => new URL(entry.name).pathname === '/app.wasm',
      );
      return {
        startupMs: startup?.duration ?? -1,
        hostMs: host?.duration ?? -1,
        imageFetchMs: image?.duration ?? -1,
        imageTransferBytes: resource?.transferSize ?? 0,
        imageEncodedBytes: resource?.encodedBodySize ?? 0,
        imageDecodedBytes: resource?.decodedBodySize ?? 0,
      };
    })()`,
    (value) => Number.isFinite(value?.startupMs) && value.startupMs >= 0
      && value.startupMs <= 5_000
      && Number.isFinite(value?.hostMs) && value.hostMs >= 0
      && Number.isFinite(value?.imageFetchMs) && value.imageFetchMs >= 0
      && value.imageDecodedBytes > 0,
    timeoutMilliseconds,
  );

  await activateButton("Open project hello-studio");
  await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[data-testid="volang-code-editor"]') instanceof HTMLTextAreaElement`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  const source = "package main\n\nfunc main() {\n\tprintln(\"studio AOT smoke\")\n}\n";
  const edited = await browser.call("Runtime.evaluate", {
    expression: `(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      if (!(editor instanceof HTMLTextAreaElement)) return false;
      editor.focus();
      editor.value = ${JSON.stringify(source)};
      editor.setSelectionRange(editor.value.length, editor.value.length);
      editor.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`,
    returnByValue: true,
  }, sessionId);
  if (edited.result?.value !== true) throw new Error("Studio AOT smoke could not edit main.vo");
  checkpoints.edited = await pollEvaluation(
    browser,
    sessionId,
    `({
      source: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? "",
      status: document.querySelector('[role="status"]')?.textContent ?? "",
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.source === source && value?.status.includes("6 lines")
      && value?.status.includes("0 problems") && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  await activateButton("Save File");
  await activateButton("Run VM");
  checkpoints.run = await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[role="log"]')?.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("studio AOT smoke")
      && value.includes("process exited successfully"),
    timeoutMilliseconds,
  );

  await activateButton("Commands");
  await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('[role="combobox"][aria-label="Command palette query"]') instanceof HTMLInputElement`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await browser.call("Runtime.evaluate", {
    expression: `(() => {
      const query = document.querySelector('[role="combobox"][aria-label="Command palette query"]');
      if (!(query instanceof HTMLInputElement)) return false;
      query.value = "documentation";
      query.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`,
    returnByValue: true,
  }, sessionId);
  const commandActivated = await browser.call("Runtime.evaluate", {
    expression: `(() => {
    const option = Array.from(document.querySelectorAll('[role="option"]')).find(
      (candidate) => (candidate.textContent ?? "").includes("Show Documentation"),
    );
    if (!(option instanceof HTMLElement)) return false;
    option.click();
    return true;
  })()`,
    returnByValue: true,
  }, sessionId);
  if (commandActivated.result?.value !== true) {
    throw new Error("Studio AOT smoke could not activate the filtered command");
  }
  checkpoints.command = await pollEvaluation(
    browser,
    sessionId,
    `({
      text: document.body.textContent ?? "",
      search: document.querySelector('input[aria-label="Search documentation"]') instanceof HTMLInputElement,
    })`,
    (value) => value?.search === true && value.text.includes("Build your first application")
      && value.text.includes("Run with VM for the shortest startup path"),
    timeoutMilliseconds,
  );

  await browser.call("Runtime.evaluate", {
    expression: `(() => {
      const input = document.querySelector('input[aria-label="Search documentation"]');
      if (!(input instanceof HTMLInputElement)) return false;
      input.value = "goroutine";
      input.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`,
    returnByValue: true,
  }, sessionId);
  await activateButton("Open documentation Goroutines in UI");
  checkpoints.documentation = await pollEvaluation(
    browser,
    sessionId,
    `({ path: location.pathname, text: document.body.textContent ?? "" })`,
    (value) => value?.path === "/docs/goroutines-ui"
      && value.text.includes("Goroutines in UI")
      && value.text.includes("Latest-wins work")
      && !value.text.includes("Components and state"),
    timeoutMilliseconds,
  );
  await clickPoint(`(() => {
    const button = Array.from(document.querySelectorAll('button')).find(
      (candidate) => candidate.getAttribute('aria-label') === 'Home',
    );
    if (!(button instanceof HTMLButtonElement) || button.disabled) return null;
    button.scrollIntoView({ block: 'center', inline: 'center' });
    const bounds = button.getBoundingClientRect();
    return { x: bounds.left + bounds.width / 2, y: bounds.top + bounds.height / 2 };
  })()`);
  await pollEvaluation(
    browser,
    sessionId,
    `location.pathname === '/' && document.body.textContent?.includes('QUICK START') === true`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await clickPoint(`(() => {
    const button = Array.from(document.querySelectorAll('button')).find(
      (candidate) => candidate.getAttribute('aria-label') === 'Open Interactive counter example in Studio',
    );
    if (!(button instanceof HTMLButtonElement) || button.disabled) return null;
    button.scrollIntoView({ block: 'center', inline: 'center' });
    const bounds = button.getBoundingClientRect();
    return { x: bounds.left + bounds.width / 2, y: bounds.top + bounds.height / 2 };
  })()`);
  await pollEvaluation(
    browser,
    sessionId,
    `({
      source: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? "",
      title: document.querySelector("header")?.textContent ?? document.body.textContent?.slice(0, 240) ?? "",
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.source.includes("UseIntState(0)"),
    timeoutMilliseconds,
  );
  await activateButton("Open Preview");
  checkpoints.preview = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const frame = document.querySelector('iframe[title="Volang application preview"]');
      const child = frame?.contentDocument;
      return {
        text: child?.body?.innerText ?? "",
        error: child?.querySelector("#preview-error")?.textContent ?? "",
      };
    })()`,
    (value) => value?.text.includes("Welcome to Volang UI")
      && value?.text.includes("Count: 0") && value?.error === "",
    timeoutMilliseconds,
  );
  await clickPoint(`(() => {
    const frame = document.querySelector('iframe[title="Volang application preview"]');
    const button = Array.from(frame?.contentDocument?.querySelectorAll("button") ?? []).find(
      (candidate) => (candidate.textContent ?? "").trim() === "Count: 0",
    );
    if (!(frame instanceof HTMLIFrameElement) || button?.tagName !== "BUTTON") return null;
    frame.scrollIntoView({ block: "center", inline: "center" });
    const frameRect = frame.getBoundingClientRect();
    const buttonRect = button.getBoundingClientRect();
    return {
      x: frameRect.left + buttonRect.left + buttonRect.width / 2,
      y: frameRect.top + buttonRect.top + buttonRect.height / 2,
    };
  })()`);
  checkpoints.interaction = await pollEvaluation(
    browser,
    sessionId,
    `document.querySelector('iframe[title="Volang application preview"]')
      ?.contentDocument?.body?.innerText ?? ""`,
    (value) => typeof value === "string" && value.includes("Count: 1"),
    timeoutMilliseconds,
  );
  await activateButton("Source Control");
  await activateButton("GitHub account");
  checkpoints.account = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const dialog = document.querySelector('[role="dialog"][aria-label="GitHub account"]');
      return {
        text: dialog?.textContent ?? "",
        connect: Array.from(dialog?.querySelectorAll('button') ?? []).some(
          (button) => button.getAttribute('aria-label') === 'Connect GitHub account',
        ),
      };
    })()`,
    (value) => value?.text.includes("browser host")
      && value?.text.includes("page session") && value?.connect === true,
    timeoutMilliseconds,
  );
  await activateButton("Close dialog");
  await activateButton("Share current project");
  checkpoints.share = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const dialog = document.querySelector('[role="dialog"][aria-label="Share project"]');
      return {
        text: dialog?.textContent ?? "",
        openRunner: Array.from(dialog?.querySelectorAll('button') ?? []).some(
          (button) => button.getAttribute('aria-label') === 'Open shared project Runner',
        ),
      };
    })()`,
    (value) => value?.text.includes("Studio link")
      && value.text.includes("Runner link")
      && value.text.includes("portable-snapshot")
      && value.openRunner === true,
    timeoutMilliseconds,
  );
  await activateButton("Open shared project Runner");
  checkpoints.runner = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const frame = document.querySelector('iframe[title="Volang application preview"]');
      return {
        path: location.pathname,
        hash: location.hash.startsWith('#share='),
        chrome: document.body.textContent?.includes('Portable Runner') === true,
        editor: document.querySelector('[data-testid="volang-code-editor"]') !== null,
        preview: frame?.contentDocument?.body?.innerText ?? "",
        error: frame?.contentDocument?.querySelector('#preview-error')?.textContent ?? "",
      };
    })()`,
    (value) => value?.path === "/runner" && value?.hash === true && value?.chrome === true
      && value?.editor === false && value?.preview.includes("Welcome to Volang UI")
      && value?.preview.includes("Count: 0") && value?.error === "",
    timeoutMilliseconds,
  );
  await browser.call("Page.reload", { ignoreCache: true }, sessionId);
  checkpoints.runnerColdStart = await pollEvaluation(
    browser,
    sessionId,
    `(() => {
      const frame = document.querySelector('iframe[title="Volang application preview"]');
      return {
        path: location.pathname,
        shared: location.hash.startsWith('#share='),
        project: document.body.textContent?.includes('example-counter') === true,
        preview: frame?.contentDocument?.body?.innerText ?? "",
        diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? "",
      };
    })()`,
    (value) => value?.path === "/runner" && value?.shared === true && value?.project === true
      && value?.preview.includes("Welcome to Volang UI")
      && value?.preview.includes("Count: 0") && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  await clickPoint(`(() => {
    const button = Array.from(document.querySelectorAll('button')).find(
      (candidate) => candidate.getAttribute('aria-label') === 'Return to Studio workspace',
    );
    if (!(button instanceof HTMLButtonElement) || button.disabled) return null;
    const bounds = button.getBoundingClientRect();
    return { x: bounds.left + bounds.width / 2, y: bounds.top + bounds.height / 2 };
  })()`);
  checkpoints.runnerReturn = await pollEvaluation(
    browser,
    sessionId,
    `({
      path: location.pathname,
      editor: document.querySelector('[data-testid="volang-code-editor"]') instanceof HTMLTextAreaElement,
      project: document.body.textContent?.includes('example-counter') === true,
    })`,
    (value) => value?.path === "/" && value?.editor === true && value?.project === true,
    timeoutMilliseconds,
  );
  const origin = await browser.call("Runtime.evaluate", {
    expression: "location.origin", returnByValue: true,
  }, sessionId);
  await browser.call("Page.navigate", {
    url: `${origin.result?.value}/docs/goroutines-ui`,
  }, sessionId);
  checkpoints.documentationColdStart = await pollEvaluation(
    browser,
    sessionId,
    `({
      path: location.pathname,
      text: document.body.textContent ?? "",
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? "",
    })`,
    (value) => value?.path === "/docs/goroutines-ui"
      && value?.text.includes("Goroutines in UI")
      && value?.text.includes("Latest-wins work")
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  return { complete: true, passed: true, checkpoints };
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
