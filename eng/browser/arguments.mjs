export function parseArguments(arguments_) {
  const parsed = {
    project: "",
    html: "browser-smoke.html",
    global: "",
    staticRoot: null,
    baseURL: null,
    componentStateSmoke: false,
    uikitGallerySmoke: false,
    dataApplicationSmoke: false,
    contentSiteSmoke: false,
    mediaApplicationSmoke: false,
    studioWorkbenchSmoke: false,
    studioAotSmoke: false,
    studioCanarySmoke: false,
    studioBootstrapSmoke: false,
    studioLifecycleSmoke: false,
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
    } else if (argument === "--studio-canary-smoke") {
      parsed.studioCanarySmoke = true;
    } else if (argument === "--studio-bootstrap-smoke") {
      parsed.studioBootstrapSmoke = true;
    } else if (argument === "--studio-lifecycle-smoke") {
      parsed.studioLifecycleSmoke = true;
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
  const staticSmoke = (parsed.staticRoot !== null || parsed.baseURL !== null)
    && (parsed.componentStateSmoke || parsed.uikitGallerySmoke
      || parsed.dataApplicationSmoke || parsed.contentSiteSmoke
      || parsed.mediaApplicationSmoke || parsed.studioWorkbenchSmoke
      || parsed.studioAotSmoke || parsed.studioCanarySmoke || parsed.studioBootstrapSmoke || parsed.studioLifecycleSmoke);
  const staticScenarios = [
    parsed.componentStateSmoke,
    parsed.uikitGallerySmoke,
    parsed.dataApplicationSmoke,
    parsed.contentSiteSmoke,
    parsed.mediaApplicationSmoke,
    parsed.studioWorkbenchSmoke,
    parsed.studioAotSmoke,
    parsed.studioCanarySmoke,
    parsed.studioBootstrapSmoke,
    parsed.studioLifecycleSmoke,
  ].filter(Boolean).length;
  if (parsed.baseURL && !parsed.studioAotSmoke && !parsed.studioCanarySmoke) throw new Error("base URL requires a Studio journey");
  if (staticScenarios > 1) {
    throw new Error("choose exactly one static browser smoke scenario");
  }
  if (!compiledSmoke && !staticSmoke) {
    throw new Error(
      "usage: run-browser-smoke.mjs (--project <dir> --global <window-key> | --static-root <dir> (--component-state-smoke | --uikit-gallery-smoke | --data-application-smoke | --content-site-smoke | --media-application-smoke | --studio-workbench-smoke | --studio-aot-smoke | --studio-canary-smoke | --studio-bootstrap-smoke | --studio-lifecycle-smoke))",
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
