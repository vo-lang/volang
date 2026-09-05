import { pollEvaluation } from '../page-contract.mjs';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';

export async function runMediaApplicationSmoke(contract, timeoutMilliseconds, projectRoot) {
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
      || !html.includes('<div id="volang-root" inert aria-busy="true" data-volang-activation="pending" data-volang-revision="1">')
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
    const evaluated = await contract.evaluate(expression);

    return evaluated ?? null;
  };
  const activate = async name => contract.activate('button, input', name);
  const navigate = async (name, path) => pollEvaluation(contract,
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
  const statusIncludes = async (expected) => pollEvaluation(contract,
    `Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? '')`,
    (value) => Array.isArray(value) && value.some((item) => item.includes(expected)),
    timeoutMilliseconds,
  );

  const checkpoints = {
    ssr: ssrRoutes,
    artifacts: { manifest: manifest.name, routes: deployment.routes.length, permissions: true },
  };
  checkpoints.initial = await pollEvaluation(contract,
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
  checkpoints.visualizations = await pollEvaluation(contract,
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
  await contract.navigate({ url: `${origin}/offline/` });
  checkpoints.offline = await pollEvaluation(contract,
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
  checkpoints.productionFetch = await pollEvaluation(contract,
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
