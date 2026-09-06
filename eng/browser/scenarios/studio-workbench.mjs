import { pollEvaluation } from '../page-contract.mjs';
import { readFile } from 'node:fs/promises';
import { join } from 'node:path';

export async function runStudioWorkbenchSmoke(contract, timeoutMilliseconds, projectRoot) {
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
      || !html.includes('<div id="volang-root" inert aria-busy="true" data-volang-activation="pending" data-volang-revision="1">')
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
    const evaluated = await contract.evaluate(expression);

    return evaluated ?? null;
  };
  const activate = async name => contract.activate('button, [role="button"], [role="treeitem"], [role="tab"], [role="option"]', name);
  const edit = async (name, value) => contract.page.getByLabel(name, { exact: true }).fill(value);
  const statusIncludes = async (expected) => pollEvaluation(contract,
    `Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? '')`,
    (value) => Array.isArray(value) && value.some((item) => item.includes(expected)),
    timeoutMilliseconds,
  );

  const checkpoints = {
    ssr: ssrRoutes,
    artifacts: { manifest: manifest.name, routes: deployment.routes.length, security: true },
  };
  checkpoints.initial = await pollEvaluation(contract,
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
  checkpoints.edited = await pollEvaluation(contract,
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
  checkpoints.fileSwitch = await pollEvaluation(contract,
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
  checkpoints.run = await pollEvaluation(contract,
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
  checkpoints.console = await pollEvaluation(contract,
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
  checkpoints.commandFiltered = await pollEvaluation(contract,
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
  checkpoints.documentation = await pollEvaluation(contract,
    `document.body.textContent ?? ''`,
    (value) => typeof value === "string" && value.includes("Build once, run everywhere")
      && value.includes("Structured concurrency"),
    timeoutMilliseconds,
  );
  await activate("Reset Workspace Layout");
  checkpoints.restored = await statusIncludes("Workspace layout restored");

  const origin = await evaluate("location.origin");
  await contract.navigate({ url: `${origin}/offline/` });
  checkpoints.offline = await pollEvaluation(contract,
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
