import { pollEvaluation } from '../page-contract.mjs';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';

export async function runContentSiteSmoke(contract, timeoutMilliseconds, projectRoot) {
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
      || !html.includes('<div id="volang-root" inert aria-busy="true" data-volang-activation="pending" data-volang-revision="1">')
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
    const evaluated = await contract.evaluate(expression);

    return evaluated ?? null;
  };
  const navigateLink = async (name, path) => pollEvaluation(contract,
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
  const activate = async (selector, name) => contract.activate(selector, name);
  const setInput = async (name, value) => contract.page.getByLabel(name, { exact: true }).fill(value);

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
  checkpoints.initial = await pollEvaluation(contract,
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
  checkpoints.article = await pollEvaluation(contract,
    `({ path: location.pathname, main: document.querySelector('[role="main"]')?.textContent ?? "" })`,
    (value) => value?.path === "/articles/wasm-aot"
      && value?.main.includes("Wasm AOT without a JavaScript framework")
      && value?.main.includes("Server node identities")
      && !value?.main.includes("zero JavaScript application code"),
    timeoutMilliseconds,
  );

  await navigateLink("Search", "/search");
  await setInput("Search articles", "goroutines");
  checkpoints.search = await pollEvaluation(contract,
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
  checkpoints.validation = await pollEvaluation(contract,
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
  checkpoints.invalidEmail = await pollEvaluation(contract,
    `({
      alert: document.querySelector('[role="alert"]')?.textContent ?? "",
      invalid: document.querySelector('input[aria-label="Email address"]')
        ?.getAttribute("aria-invalid") ?? "",
    })`,
    (value) => value?.alert.includes("Enter a valid email address") && value?.invalid === "true",
    timeoutMilliseconds,
  );
  await setInput("Email address", "ada@example.test");
  checkpoints.validEmail = await pollEvaluation(contract,
    `({
      value: document.querySelector('input[aria-label="Email address"]')?.value ?? "",
      invalid: document.querySelector('input[aria-label="Email address"]')
        ?.getAttribute("aria-invalid") ?? "",
    })`,
    (value) => value?.value === "ada@example.test" && value?.invalid !== "true",
    timeoutMilliseconds,
  );
  await setInput("Display name", "Ada");
  checkpoints.validFields = await pollEvaluation(contract,
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
  checkpoints.submission = await pollEvaluation(contract,
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
  await contract.navigate({ url: `${origin}/offline/` });
  checkpoints.offline = await pollEvaluation(contract,
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
  checkpoints.productionFetch = await pollEvaluation(contract,
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
