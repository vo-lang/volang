import { pollEvaluation, waitForAotInteractive } from '../page-contract.mjs';

export async function runStudioAotSmoke(contract, timeoutMilliseconds, projectRoot) {
  const languageExampleOpenBudgetMilliseconds = 5_000;
  const clickPoint = async (expression) => {
    const point = await pollEvaluation(contract,
      expression,
      (value) => value !== null && Number.isFinite(value.x) && Number.isFinite(value.y),
      timeoutMilliseconds,
    );
    await contract.mouseEvent({
      type: "mousePressed", x: point.x, y: point.y, button: "left", clickCount: 1,
    });
    await contract.mouseEvent({
      type: "mouseReleased", x: point.x, y: point.y, button: "left", clickCount: 1,
    });
    return point;
  };
  const activateButton = async name => contract.activate('button, [role="button"], [role="treeitem"]', name);
  const checkpoints = {};
  checkpoints.home = await pollEvaluation(contract,
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
  checkpoints.performance = await pollEvaluation(contract,
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

  await contract.viewport({
    width: 390,
    height: 844,
    deviceScaleFactor: 1,
    mobile: true,
    screenWidth: 390,
    screenHeight: 844,
  });
  checkpoints.compactLayout = await pollEvaluation(contract,
    `(() => {
      const root = document.querySelector('#volang-root');
      const navigation = document.querySelector('[role="navigation"][aria-label="Studio activities"]');
      const bounds = navigation?.getBoundingClientRect();
      const toolbarButtons = Array.from(document.querySelectorAll('[role="toolbar"] button'))
        .map((button) => button.getAttribute('aria-label') ?? (button.textContent ?? '').trim());
      return {
        innerWidth,
        bodyClientWidth: document.body.clientWidth,
        bodyScrollWidth: document.body.scrollWidth,
        rootClientWidth: root?.clientWidth ?? -1,
        rootScrollWidth: root?.scrollWidth ?? -1,
        navigationBottom: bounds?.bottom ?? -1,
        navigationWidth: bounds?.width ?? -1,
        toolbarButtons,
      };
    })()`,
    (value) => value?.innerWidth === 390
      && value?.bodyClientWidth === 390 && value?.bodyScrollWidth <= 390
      && value?.rootClientWidth === 390 && value?.rootScrollWidth <= 390
      && value?.navigationWidth >= 380 && value?.navigationBottom >= 830
      && JSON.stringify(value?.toolbarButtons) === JSON.stringify(["New project"]),
    timeoutMilliseconds,
  );
  await contract.viewport({
    width: 1280,
    height: 720,
    deviceScaleFactor: 1,
    mobile: false,
    screenWidth: 1280,
    screenHeight: 720,
  });
  await pollEvaluation(contract,
    `innerWidth >= 840`,
    (value) => value === true,
    timeoutMilliseconds,
  );

  await contract.page.getByTestId('studio-topbar').getByRole('button', { name: 'New project', exact: true }).click();
  checkpoints.projectValidation = await pollEvaluation(contract,
    `(() => {
      const input = document.querySelector('input[aria-label="Project name"]');
      const alert = Array.from(document.querySelectorAll('[role="alert"]')).find(
        (candidate) => (candidate.textContent ?? '').includes('Enter a project name'),
      );
      return {
        invalid: input?.getAttribute('aria-invalid') ?? '',
        required: input?.required === true,
        description: input?.getAttribute('aria-description') ?? '',
        focused: document.activeElement === input,
        alert: alert?.textContent ?? '',
        createDisabled: Array.from(document.querySelectorAll('button')).some(
          (button) => (button.textContent ?? '').trim() === 'Create project' && button.disabled,
        ),
      };
    })()`,
    (value) => value?.invalid === "true" && value?.required === true
      && value?.description === "Enter a project name." && value?.focused === true
      && value?.alert.includes("Enter a project name") && value?.createDisabled === true,
    timeoutMilliseconds,
  );
  await activateButton("Home");
  await pollEvaluation(contract,
    `document.body.textContent?.includes('QUICK START') === true`,
    (value) => value === true,
    timeoutMilliseconds,
  );

  await activateButton("Open project hello-studio");
  await pollEvaluation(contract,
    `document.querySelector('[data-testid="volang-code-editor"]') instanceof HTMLTextAreaElement`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  const source = "package main\n\nfunc main() {\n\tprintln(\"studio AOT smoke\") // "
    + "volang".repeat(100) + "\n}\n";
  const edited = await contract.evaluate(`(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      if (!(editor instanceof HTMLTextAreaElement)) return false;
      editor.focus();
      editor.value = ${JSON.stringify(source)};
      editor.setSelectionRange(editor.value.length, editor.value.length);
      editor.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`);
  if (edited !== true) throw new Error("Studio AOT smoke could not edit main.vo");
  checkpoints.edited = await pollEvaluation(contract,
    `({
      source: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? "",
      statuses: Array.from(document.querySelectorAll('[role="status"]'),
        (status) => status.textContent ?? ''),
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.source === source && Array.isArray(value?.statuses)
      && value.statuses.some((status) => status.includes("6 lines") && status.includes("0 problems"))
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  const scrolled = await contract.evaluate(`(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      if (!(editor instanceof HTMLTextAreaElement) || editor.scrollWidth <= editor.clientWidth) return false;
      editor.scrollLeft = 420;
      editor.dispatchEvent(new Event('scroll'));
      return editor.scrollLeft === 420;
    })()`);
  if (scrolled !== true) {
    throw new Error("Studio AOT smoke could not scroll a long editor line");
  }
  checkpoints.editorScroll = await pollEvaluation(contract,
    `(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      const mirror = document.querySelector('[data-testid="volang-code-editor-highlight"]');
      return {
        editorLeft: editor?.scrollLeft ?? -1,
        mirrorLeft: mirror?.scrollLeft ?? -1,
        editorWidth: editor?.scrollWidth ?? -1,
        mirrorWidth: mirror?.scrollWidth ?? -1,
      };
    })()`,
    (value) => value?.editorLeft === 420 && value?.mirrorLeft === 420
      && value?.editorWidth > 4_000 && value?.mirrorWidth > 4_000,
    timeoutMilliseconds,
  );
  await activateButton("Search");
  const searchedUnsaved = await contract.evaluate(`(() => {
      const input = document.querySelector('input[aria-label="Search workspace"]');
      if (!(input instanceof HTMLInputElement)) return false;
      input.value = "studio AOT smoke";
      input.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`);
  if (searchedUnsaved !== true) {
    throw new Error("Studio AOT smoke could not search the unsaved editor snapshot");
  }
  checkpoints.unsavedSearch = await pollEvaluation(contract,
    `document.body.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("main.vo:4")
      && value.includes("studio AOT smoke"),
    timeoutMilliseconds,
  );
  await activateButton("Explorer");
  await activateButton("Save File");
  await activateButton("New file");
  const duplicatePathEntered = await contract.evaluate(`(() => {
      const input = document.querySelector('input[aria-label="New file path"]');
      if (!(input instanceof HTMLInputElement)) return false;
      input.value = 'main.vo';
      input.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertText' }));
      return true;
    })()`);
  if (duplicatePathEntered !== true) {
    throw new Error("Studio AOT smoke could not enter an existing file path");
  }
  const duplicateCreateActivated = await contract.evaluate(`(() => {
      const button = Array.from(document.querySelectorAll('button')).find(
        (candidate) => (candidate.textContent ?? '').trim() === 'Create file',
      );
      if (!(button instanceof HTMLButtonElement) || button.disabled) return false;
      button.click();
      return true;
    })()`);
  if (duplicateCreateActivated !== true) {
    throw new Error("Studio AOT smoke could not submit an existing file path");
  }
  checkpoints.noOverwriteCreate = await pollEvaluation(contract,
    `({
      source: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? '',
      body: document.body.textContent ?? '',
    })`,
    (value) => value?.source === source && value?.body.includes('File creation failed'),
    timeoutMilliseconds,
  );
  const duplicateDialogClosed = await contract.evaluate(`(() => {
      const button = Array.from(document.querySelectorAll('button')).find(
        (candidate) => (candidate.textContent ?? '').trim() === 'Close dialog',
      );
      if (!(button instanceof HTMLButtonElement)) return false;
      button.click();
      return true;
    })()`);
  if (duplicateDialogClosed !== true) {
    throw new Error("Studio AOT smoke could not close the duplicate-file dialog");
  }
  await activateButton("New file");
  const nestedPathEntered = await contract.evaluate(`(() => {
      const input = document.querySelector('input[aria-label="New file path"]');
      if (!(input instanceof HTMLInputElement)) return false;
      input.value = 'feature/tools.vo';
      input.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertText' }));
      return true;
    })()`);
  if (nestedPathEntered !== true) {
    throw new Error("Studio AOT smoke could not enter a nested source path");
  }
  const nestedCreateActivated = await contract.evaluate(`(() => {
      const button = Array.from(document.querySelectorAll('button')).find(
        (candidate) => (candidate.textContent ?? '').trim() === 'Create file',
      );
      if (!(button instanceof HTMLButtonElement) || button.disabled) return false;
      button.click();
      return true;
    })()`);
  if (nestedCreateActivated !== true) {
    throw new Error("Studio AOT smoke could not create a nested source file");
  }
  checkpoints.nestedSourcePackage = await pollEvaluation(contract,
    `({
      source: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? '',
      label: document.querySelector('[data-testid="volang-code-editor"]')?.getAttribute('aria-label') ?? '',
      statuses: Array.from(document.querySelectorAll('[role="status"]'),
        (status) => status.textContent ?? ''),
      folder: document.querySelector('[role="treeitem"][aria-label="Collapse folder feature"]') !== null,
      file: document.querySelector('[role="treeitem"][aria-label="Open feature/tools.vo"]') !== null,
    })`,
    (value) => value?.source === "package feature\n"
      && value?.label === "Editor for feature/tools.vo"
      && Array.isArray(value?.statuses)
      && value.statuses.some((status) => status.includes("0 problems"))
      && value?.folder === true && value?.file === true,
    timeoutMilliseconds,
  );
  await activateButton("Collapse folder feature");
  checkpoints.directoryCollapse = await pollEvaluation(contract,
    `({
      collapsed: document.querySelector('[role="treeitem"][aria-label="Expand folder feature"]') !== null,
      childVisible: document.querySelector('[role="treeitem"][aria-label="Open feature/tools.vo"]') !== null,
    })`,
    (value) => value?.collapsed === true && value?.childVisible === false,
    timeoutMilliseconds,
  );
  await activateButton("Expand folder feature");
  await pollEvaluation(contract,
    `document.querySelector('[role="treeitem"][aria-label="Open feature/tools.vo"]') !== null`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await activateButton("Delete");
  checkpoints.deleteConfirmation = await pollEvaluation(contract,
    `(() => {
      const dialog = document.querySelector('[role="dialog"][aria-label="Delete feature/tools.vo"]');
      return {
        visible: dialog !== null,
        destructive: Array.from(dialog?.querySelectorAll('button') ?? []).some(
          (button) => (button.textContent ?? '').trim() === 'Delete file permanently',
        ),
        fileVisible: document.querySelector('[role="treeitem"][aria-label="Open feature/tools.vo"]') !== null,
      };
    })()`,
    (value) => value?.visible === true && value?.destructive === true && value?.fileVisible === true,
    timeoutMilliseconds,
  );
  await activateButton("Close dialog");
  await activateButton("Open main.vo");
  await pollEvaluation(contract,
    `document.querySelector('[data-testid="volang-code-editor"]')?.value ?? ''`,
    (value) => value === source,
    timeoutMilliseconds,
  );
  await activateButton("Run VM");
  checkpoints.run = await pollEvaluation(contract,
    `document.querySelector('[role="log"]')?.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("studio AOT smoke")
      && value.includes("process exited successfully")
      && /Duration [0-9.]+(?:ns|µs|ms|s|m|h)/.test(value),
    timeoutMilliseconds,
  );
  const endlessSource = "package main\n\nfunc main() {\n\tfor {}\n}\n";
  const endlessEdited = await contract.evaluate(`(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      if (!(editor instanceof HTMLTextAreaElement)) return false;
      editor.focus();
      editor.value = ${JSON.stringify(endlessSource)};
      editor.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertText' }));
      return true;
    })()`);
  if (endlessEdited !== true) {
    throw new Error("Studio AOT smoke could not prepare a cancellable run");
  }
  await activateButton("Run VM");
  await pollEvaluation(contract,
    `document.querySelector('[role="log"]')?.textContent ?? ''`,
    (value) => typeof value === "string"
      && (value.match(/Web VM session started/gu) ?? []).length >= 2,
    timeoutMilliseconds,
  );
  await activateButton("Stop VM");
  checkpoints.runCancellation = await pollEvaluation(contract,
    `({
      stopped: (document.body.textContent ?? '').includes('Stopped'),
      runnable: Array.from(document.querySelectorAll('button')).some(
        (button) => button.getAttribute('aria-label') === 'Run VM' && !button.disabled,
      ),
    })`,
    (value) => value?.stopped === true && value?.runnable === true,
    timeoutMilliseconds,
  );
  const sourceRestoredAfterCancellation = await contract.evaluate(`(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      if (!(editor instanceof HTMLTextAreaElement)) return false;
      editor.value = ${JSON.stringify(source)};
      editor.setSelectionRange(editor.value.length, editor.value.length);
      editor.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertText' }));
      return true;
    })()`);
  if (sourceRestoredAfterCancellation !== true) {
    throw new Error("Studio AOT smoke could not restore source after run cancellation");
  }

  await activateButton("Commands");
  checkpoints.commandOverlay = await pollEvaluation(contract,
    `(() => {
      const portal = document.querySelector('[data-volang-portal="200"]');
      const backdrop = portal?.firstElementChild;
      const bounds = backdrop?.getBoundingClientRect();
      return {
        portal: portal?.getBoundingClientRect().toJSON() ?? null,
        backdrop: bounds?.toJSON() ?? null,
        background: backdrop ? getComputedStyle(backdrop).backgroundColor : "",
        viewport: { width: innerWidth, height: innerHeight },
      };
    })()`,
    (value) => value?.portal?.left === 0 && value.portal.top === 0
      && value.portal.right === value.viewport?.width && value.portal.bottom === value.viewport?.height
      && value?.backdrop?.left === 0 && value.backdrop.top === 0
      && value.backdrop.right === value.viewport?.width && value.backdrop.bottom === value.viewport?.height
      && value.background !== "" && value.background !== "rgba(0, 0, 0, 0)",
    timeoutMilliseconds,
  );
  await pollEvaluation(contract,
    `document.querySelector('[role="combobox"][aria-label="Command palette query"]') instanceof HTMLInputElement`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await contract.evaluate(`(() => {
      const query = document.querySelector('[role="combobox"][aria-label="Command palette query"]');
      if (!(query instanceof HTMLInputElement)) return false;
      query.value = "documentation";
      query.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`);
  const commandActivated = await contract.evaluate(`(() => {
    const option = Array.from(document.querySelectorAll('[role="option"]')).find(
      (candidate) => (candidate.textContent ?? "").includes("Show Documentation"),
    );
    if (!(option instanceof HTMLElement)) return false;
    option.click();
    return true;
  })()`);
  if (commandActivated !== true) {
    throw new Error("Studio AOT smoke could not activate the filtered command");
  }
  checkpoints.command = await pollEvaluation(contract,
    `({
      text: document.body.textContent ?? "",
      search: document.querySelector('input[aria-label="Search documentation"]') instanceof HTMLInputElement,
    })`,
    (value) => value?.search === true && value.text.includes("The working model")
      && value.text.includes("Development and release")
      && value.text.includes("Canonical source · lang/docs/guides/introduction.md"),
    timeoutMilliseconds,
  );

  await contract.evaluate(`(() => {
      const input = document.querySelector('input[aria-label="Search documentation"]');
      if (!(input instanceof HTMLInputElement)) return false;
      input.value = "goroutine";
      input.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`);
  await activateButton("Goroutines, channels, and islands");
  checkpoints.documentation = await pollEvaluation(contract,
    `({ path: location.pathname, text: document.body.textContent ?? "" })`,
    (value) => value?.path === "/docs/concurrency"
      && value.text.includes("Goroutines, channels, and islands")
      && value.text.includes("Islands and ports")
      && value.text.includes("Canonical source · lang/docs/guides/concurrency.md"),
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
  await pollEvaluation(contract,
    `location.pathname === '/' && document.body.textContent?.includes('QUICK START') === true`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  const languageExampleStarted = Date.now();
  await activateButton("Open Select example in Studio");
  checkpoints.languageExample = await pollEvaluation(contract,
    `({
      source: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? '',
      lock: document.querySelector('[role="treeitem"][aria-label="Open vo.lock"]') !== null,
      work: document.querySelector('[role="treeitem"][aria-label="Open vo.work"]') !== null,
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    })`,
    (value) => value?.source.includes('select {') && value?.lock === false
      && value?.work === false && value?.diagnostic === '',
    timeoutMilliseconds,
  );
  checkpoints.languageExampleOpenMs = Date.now() - languageExampleStarted;
  if (checkpoints.languageExampleOpenMs > languageExampleOpenBudgetMilliseconds) {
    throw new Error(
      `Studio language example took ${checkpoints.languageExampleOpenMs}ms to open; `
        + `budget is ${languageExampleOpenBudgetMilliseconds}ms`,
    );
  }
  await activateButton("Home");
  await pollEvaluation(contract,
    `location.pathname === '/' && document.body.textContent?.includes('QUICK START') === true`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  const uiExampleStarted = Date.now();
  await activateButton("Open Interactive counter example in Studio");
  await pollEvaluation(contract,
    `({
      source: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? "",
      title: document.querySelector("header")?.textContent ?? document.body.textContent?.slice(0, 240) ?? "",
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
      statuses: Array.from(document.querySelectorAll('[role="status"]')).map(
        (value) => (value.textContent ?? '').trim(),
      ),
      exampleDisabled: Array.from(document.querySelectorAll('button')).find(
        (candidate) => candidate.getAttribute('aria-label') === 'Open Interactive counter example in Studio',
      )?.disabled ?? null,
    })`,
    (value) => value?.source.includes("UseIntState(0)"),
    timeoutMilliseconds,
  );
  checkpoints.uiExampleOpenMs = Date.now() - uiExampleStarted;
  if (checkpoints.uiExampleOpenMs > 10_000) {
    throw new Error(`Studio UI example took ${checkpoints.uiExampleOpenMs}ms to open`);
  }
  const starterSource = await contract.evaluate(`document.querySelector('[data-testid="volang-code-editor"]')?.value ?? ""`);
  const invalidSource = "package main\n\nfunc main(";
  const invalidated = await contract.evaluate(`(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      if (!(editor instanceof HTMLTextAreaElement)) return false;
      editor.focus();
      editor.value = ${JSON.stringify(invalidSource)};
      editor.setSelectionRange(0, 0);
      editor.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`);
  if (invalidated !== true) {
    throw new Error("Studio AOT smoke could not create an editor diagnostic");
  }
  await pollEvaluation(contract,
    `Array.from(document.querySelectorAll('[role="status"]')).some(
      (status) => (status.textContent ?? '').includes('1 problems'),
    )`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  const problemsActivated = await contract.evaluate(`(() => {
      const button = document.querySelector('[data-testid="workspace-tab-problems"]');
      if (!(button instanceof HTMLButtonElement) || button.disabled) return false;
      button.click();
      return true;
    })()`);
  if (problemsActivated !== true) {
    throw new Error("Studio AOT smoke could not open the Problems panel");
  }
  await pollEvaluation(contract,
    `(() => {
      const panel = document.querySelector('[data-testid="studio-problems-panel"]');
      const button = panel?.querySelector('button');
      return {
        text: panel?.textContent ?? "",
        actionable: button instanceof HTMLButtonElement,
      };
    })()`,
    (value) => value?.actionable === true && value.text.includes("main.vo:3:11")
      && value.text.includes("expected type, found EOF"),
    timeoutMilliseconds,
  );
  const diagnosticActivated = await contract.evaluate(`(() => {
      const button = document.querySelector('[data-testid="studio-problems-panel"] button');
      if (!(button instanceof HTMLButtonElement)) return false;
      button.click();
      return true;
    })()`);
  if (diagnosticActivated !== true) {
    throw new Error("Studio AOT smoke could not activate the editor diagnostic");
  }
  checkpoints.diagnosticNavigation = await pollEvaluation(contract,
    `(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      return {
        focused: document.activeElement === editor,
        selectionStart: editor?.selectionStart ?? -1,
        selectionEnd: editor?.selectionEnd ?? -1,
        focusRequest: editor?.getAttribute('data-volang-focus-request') ?? '',
      };
    })()`,
    (value) => value?.focused === true
      && value?.selectionStart === invalidSource.length
      && value?.selectionEnd === invalidSource.length
      && value?.focusRequest !== "",
    timeoutMilliseconds,
  );
  const restoredStarter = await contract.evaluate(`(() => {
      const editor = document.querySelector('[data-testid="volang-code-editor"]');
      if (!(editor instanceof HTMLTextAreaElement)) return false;
      editor.value = ${JSON.stringify(starterSource ?? "")};
      editor.setSelectionRange(editor.value.length, editor.value.length);
      editor.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
      return true;
    })()`);
  if (restoredStarter !== true) {
    throw new Error("Studio AOT smoke could not restore the starter source");
  }
  await pollEvaluation(contract,
    `Array.from(document.querySelectorAll('[role="status"]'), (status) => status.textContent ?? '')`,
    (value) => Array.isArray(value) && value.some((status) => status.includes("0 problems")),
    timeoutMilliseconds,
  );
  checkpoints.dirtyRevert = await pollEvaluation(contract,
    `(() => {
      const tab = Array.from(document.querySelectorAll('[role="tab"]')).find(
        (candidate) => (candidate.textContent ?? '').includes('main.vo'),
      );
      return tab?.textContent ?? '';
    })()`,
    (value) => typeof value === "string" && value.includes("main.vo") && !value.includes("●"),
    timeoutMilliseconds,
  );
  checkpoints.cleanSaveDisabled = await pollEvaluation(contract,
    `(() => {
      const tab = Array.from(document.querySelectorAll('[role="tab"]')).find(
        (candidate) => (candidate.textContent ?? '').includes('main.vo'),
      );
      const save = Array.from(document.querySelectorAll('button')).find(
        (candidate) => candidate.getAttribute('aria-label') === 'Save File',
      );
      return { tab: tab?.textContent ?? '', saveDisabled: save?.disabled ?? false };
    })()`,
    (value) => value?.tab.includes("main.vo") && !value.tab.includes("●")
      && value?.saveDisabled === true,
    timeoutMilliseconds,
  );
  await activateButton("Open Preview");
  checkpoints.preview = await pollEvaluation(contract,
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
  checkpoints.interaction = await pollEvaluation(contract,
    `document.querySelector('iframe[title="Volang application preview"]')
      ?.contentDocument?.body?.innerText ?? ""`,
    (value) => typeof value === "string" && value.includes("Count: 1"),
    timeoutMilliseconds,
  );
  const unrelatedMessagePosted = await contract.evaluate(`(() => {
      const frame = document.querySelector('iframe[title="Volang application preview"]');
      if (!(frame instanceof HTMLIFrameElement) || frame.contentWindow === null) return false;
      frame.contentWindow.postMessage({
        protocol: 'volang.studio.host.v1',
        kind: 'workspace-status',
      }, location.origin);
      return true;
    })()`);
  if (unrelatedMessagePosted !== true) {
    throw new Error("Studio AOT smoke could not exercise preview message isolation");
  }
  checkpoints.previewMessageIsolation = await pollEvaluation(contract,
    `(() => {
      const frame = document.querySelector('iframe[title="Volang application preview"]');
      return {
        text: frame?.contentDocument?.body?.innerText ?? "",
        error: frame?.contentDocument?.querySelector('#preview-error')?.textContent ?? "",
      };
    })()`,
    (value) => value?.text.includes("Count: 1") && value?.error === "",
    timeoutMilliseconds,
  );
  await activateButton("Source Control");
  await activateButton("GitHub account");
  checkpoints.account = await pollEvaluation(contract,
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
      && value?.text.includes("never persists") && value?.connect === true,
    timeoutMilliseconds,
  );
  await activateButton("Connect GitHub account");
  checkpoints.accountCredential = await pollEvaluation(contract,
    `(() => {
      const dialog = document.querySelector('dialog[aria-label="Connect GitHub account"]');
      const input = dialog?.querySelector('input[aria-label="GitHub token"]');
      return {
        open: dialog?.open === true,
        type: input?.type ?? '',
        autocomplete: input?.autocomplete ?? '',
        focused: document.activeElement === input,
      };
    })()`,
    (value) => value?.open === true && value?.type === "password"
      && value?.autocomplete === "off" && value?.focused === true,
    timeoutMilliseconds,
  );
  await activateButton("Cancel GitHub connection");
  checkpoints.accountCancellation = await pollEvaluation(contract,
    `({
      credentialDialog: document.querySelector('dialog[aria-label="Connect GitHub account"]') !== null,
      accountDialog: document.querySelector('[role="dialog"][aria-label="GitHub account"]') !== null,
      status: Array.from(document.querySelectorAll('[role="status"]'),
        (candidate) => candidate.textContent ?? ''),
    })`,
    (value) => value?.credentialDialog === false && value?.accountDialog === true
      && Array.isArray(value?.status)
      && value.status.some((status) => status.includes("GitHub connection cancelled")),
    timeoutMilliseconds,
  );
  await activateButton("Close dialog");
  await activateButton("Share current project");
  checkpoints.share = await pollEvaluation(contract,
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
  checkpoints.runner = await pollEvaluation(contract,
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
  const warmReloadStarted = Date.now();
  await contract.reload({ ignoreCache: true });
  await waitForAotInteractive(contract, timeoutMilliseconds);
  checkpoints.warmReloadMs = Date.now() - warmReloadStarted;
  if (checkpoints.warmReloadMs > 5_000) {
    throw new Error(`Studio warm reload took ${checkpoints.warmReloadMs}ms`);
  }
  checkpoints.runnerColdStart = await pollEvaluation(contract,
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
  checkpoints.runnerReturn = await pollEvaluation(contract,
    `({
      path: location.pathname,
      editor: document.querySelector('[data-testid="volang-code-editor"]') instanceof HTMLTextAreaElement,
      project: document.body.textContent?.includes('example-counter') === true,
    })`,
    (value) => value?.path === "/workspace" && value?.editor === true && value?.project === true,
    timeoutMilliseconds,
  );
  await contract.reload({ ignoreCache: true });
  await waitForAotInteractive(contract, timeoutMilliseconds);
  checkpoints.sharedProjectPersistence = await pollEvaluation(contract,
    `({
      path: location.pathname,
      project: Array.from(document.querySelectorAll('button')).some(
        (button) => button.getAttribute('aria-label') === 'Open project example-counter',
      ),
      active: document.querySelector(
        'button[aria-label="Open project example-counter"][aria-selected="true"]',
      ) !== null,
      editor: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? '',
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    })`,
    (value) => value?.path === "/workspace" && value?.project === true && value?.active === true
      && value?.editor.includes('UseIntState(0)') && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  await activateButton("Open project example-counter");
  checkpoints.sharedProjectReopen = await pollEvaluation(contract,
    `({
      source: document.querySelector('[data-testid="volang-code-editor"]')?.value ?? '',
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? '',
    })`,
    (value) => value?.source.includes('UseIntState(0)') && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  const origin = await contract.evaluate("location.origin");
  await contract.navigate({
    url: `${origin}/docs/concurrency`,
  });
  checkpoints.documentationColdStart = await pollEvaluation(contract,
    `({
      path: location.pathname,
      text: document.body.textContent ?? "",
      diagnostic: document.getElementById('volang-diagnostic')?.textContent ?? "",
    })`,
    (value) => value?.path === "/docs/concurrency"
      && value?.text.includes("Goroutines, channels, and islands")
      && value?.text.includes("Islands and ports")
      && value?.text.includes("Canonical source · lang/docs/guides/concurrency.md")
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );
  return { complete: true, passed: true, checkpoints };
}
