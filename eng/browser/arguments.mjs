export function parseArguments(arguments_) {
  const parsed = {
    project: "",
    html: "browser-smoke.html",
    global: "",
    staticRoot: null,
    baseURL: null,
    expectedArtifact: null,
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
    } else if (argument === "--base-url" && value !== undefined) {
      const url = new URL(value);
      if (!['http:', 'https:'].includes(url.protocol) || url.username || url.password) throw new Error('base URL must be an HTTP(S) origin without credentials');
      parsed.baseURL = url.href;
      index += 1;
    } else if (argument === "--expected-artifact" && value !== undefined) {
      parsed.expectedArtifact = value;
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
  if (!parsed.global || !(parsed.project || parsed.staticRoot || parsed.baseURL)) {
    throw new Error("usage: run-browser-smoke.mjs (--project <dir> | --static-root <dir> | --base-url <url>) --global <window-key>");
  }
  if (parsed.expectedArtifact && !parsed.baseURL) throw new Error("expected artifact requires a deployed base URL");
  if (!Number.isSafeInteger(parsed.timeout) || parsed.timeout < 1_000 || parsed.timeout > 120_000) {
    throw new Error("browser smoke timeout must be an integer between 1000 and 120000 ms");
  }
  if (parsed.global.length > 0 && !/^[A-Za-z_$][A-Za-z0-9_$]*$/.test(parsed.global)) {
    throw new Error("browser smoke global key is invalid");
  }
  return parsed;
}
