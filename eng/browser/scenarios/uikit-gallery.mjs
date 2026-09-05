import { pollEvaluation } from '../page-contract.mjs';

export async function runUikitGallerySmoke(contract, timeoutMilliseconds, projectRoot) {
  const evaluate = async (expression) => {
    const evaluated = await contract.evaluate(expression);

    return evaluated ?? null;
  };
  const activateButton = async name => contract.activate('button, [role="button"], [role="menuitem"], [role="menuitemcheckbox"]', name);
  const focusElement = async selector => contract.page.locator(selector).focus();
  const setNamedInput = async (name, value) => contract.page.getByLabel(name, { exact: true }).fill(value);
  const pressKey = async (key, code, windowsVirtualKeyCode) => {
    await contract.keyEvent({
      type: "keyDown", key, code, windowsVirtualKeyCode,
    });
    await contract.keyEvent({
      type: "keyUp", key, code, windowsVirtualKeyCode,
    });
  };
  const checkpoints = {};
  checkpoints.initial = await pollEvaluation(contract,
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
      const search = document.querySelector('[role="searchbox"][aria-label="Component search"]');
      const number = document.querySelector('[role="spinbutton"][aria-label="Build replicas"]');
      const loadingButton = document.querySelector('[role="button"][aria-label="Publishing, in progress"]');
      const rating = document.querySelector('[role="radiogroup"][aria-label="Framework quality"]');
      const carousel = document.querySelector('[role="region"][aria-label="Runtime pipeline"]');
      const underline = document.querySelector('[role="button"][aria-label="Underline"]');
      const inlineCode = document.querySelector('[role="group"][aria-label="Build command"]')
        ?.querySelector('span[style*="font-family"]');
      const blockCode = document.querySelector('[role="region"][aria-label="Volang entry point"]')
        ?.querySelector('span[style*="font-family"]');
      const resizeHandle = document.querySelector('[role="separator"][aria-label="Inspector size"]');
      const adaAvatar = document.querySelector('[role="img"][aria-label="Ada Lovelace"]');
      const graceAvatar = document.querySelector('[role="img"][aria-label="Grace Hopper"]');
      const adaBounds = adaAvatar?.getBoundingClientRect();
      const graceBounds = graceAvatar?.getBoundingClientRect();
      const packageGroup = document.querySelector('[role="group"][aria-label="Package address"]');
      const invalidInputGroup = document.querySelector('[role="group"][aria-label="Registry token"]');
      const productNavigation = document.querySelector('[role="navigation"][aria-label="Product navigation"]');
      const packageCombobox = document.querySelector('input[role="combobox"][aria-label="Framework package"]');
      const platformMultiSelect = document.querySelector('input[role="combobox"][aria-label="Release platforms"]');
      const releaseDate = document.querySelector('input[aria-label="Release date"]');
      const releaseWindow = document.querySelector('[role="grid"][aria-label="Release window dates"]');
      const uploader = document.querySelector('[role="group"][aria-label="Release artifacts uploader"]');
      const contextTarget = document.querySelector('[role="region"][aria-label="Project canvas"]');
      const darkSwitch = document.querySelector('[role="switch"][aria-label="Dark theme"]');
      const selectedRadio = document.querySelector('[role="radio"][aria-label="Fast processing"]');
      const unselectedRadio = document.querySelector('[role="radio"][aria-label="Precise processing"]');
      const canvas = document.querySelector('[data-volang-node="1:1"]');
      const previewPanel = document.querySelector('[data-volang-node="7:1"]');
      const switchStyle = darkSwitch instanceof HTMLElement ? getComputedStyle(darkSwitch) : null;
      const selectedRadioStyle = selectedRadio instanceof HTMLElement ? getComputedStyle(selectedRadio) : null;
      const unselectedRadioStyle = unselectedRadio instanceof HTMLElement ? getComputedStyle(unselectedRadio) : null;
      return {
        title: document.body.textContent?.includes("Volang UI component gallery") === true,
        checkbox: checkbox instanceof HTMLInputElement ? checkbox.checked : null,
        textarea: textarea instanceof HTMLTextAreaElement
          && textarea.required && textarea.getAttribute("aria-invalid") === "false",
        slider: slider instanceof HTMLInputElement
          ? [slider.value, slider.min, slider.max, slider.step] : [],
        accordion: [runtime?.getAttribute("aria-expanded"), targets?.getAttribute("aria-expanded")],
        accordionText: [(runtime?.textContent ?? "").trim(), (targets?.textContent ?? "").trim()],
        essentials: {
          avatar: document.querySelector('[role="img"][aria-label="Ada Lovelace"]') !== null,
          iconButton: document.querySelector('[role="button"][aria-label="Open settings"]') !== null,
          loadingButton: loadingButton?.getAttribute("aria-disabled") === "true"
            || (loadingButton instanceof HTMLButtonElement && loadingButton.disabled),
          search: search instanceof HTMLInputElement ? search.value : null,
          number: number instanceof HTMLInputElement ? {
            value: number.value,
            now: number.getAttribute("aria-valuenow"),
            minimum: number.getAttribute("aria-valuemin"),
            maximum: number.getAttribute("aria-valuemax"),
            step: number.getAttribute("data-volang-step"),
          } : null,
          target: document.querySelector('[role="radio"][aria-label="Web"]')?.getAttribute("aria-checked") ?? "",
          list: document.querySelector('[role="option"][aria-label="Runtime"]')?.getAttribute("aria-selected") ?? "",
          step: document.querySelector('[role="listitem"][aria-label="Verify"]')?.getAttribute("aria-current") ?? "",
          form: document.querySelector('[role="form"][aria-label="Workspace profile"]') !== null,
        },
        extended: {
          avatarGroup: document.querySelector('[role="group"][aria-label="Core maintainers"]') !== null,
          overflowAvatar: document.querySelector('[role="img"][aria-label="+1 more people"]') !== null,
          avatarOverlap: adaBounds !== undefined && graceBounds !== undefined
            && graceBounds.left < adaBounds.right && graceBounds.right > adaBounds.left,
          inputGroup: document.querySelector('[role="group"][aria-label="Package address"]') !== null,
          inputGroupFields: packageGroup?.querySelectorAll('input').length ?? -1,
          inputGroupBorder: packageGroup?.querySelector('input')?.style.borderWidth ?? "missing",
          invalidInputGroup: invalidInputGroup === null ? null : {
            invalid: invalidInputGroup.getAttribute("aria-invalid"),
            description: invalidInputGroup.getAttribute("aria-description"),
            fields: invalidInputGroup.querySelectorAll('input').length,
            fieldRequired: invalidInputGroup.querySelector('input')?.required ?? false,
          },
          bold: document.querySelector('[role="button"][aria-label="Bold"]')?.getAttribute("aria-pressed") ?? "",
          underlineDisabled: underline?.getAttribute("aria-disabled") === "true"
            || (underline instanceof HTMLButtonElement && underline.disabled),
          rating: rating?.getAttribute("aria-valuetext") ?? "",
          ratingChecked: document.querySelector('[role="radio"][aria-label="Framework quality, 4 of 5 stars"]')
            ?.getAttribute("aria-checked") ?? "",
          ratingReadOnly: document.querySelector('[role="img"][aria-label="Read-only framework quality"]')
            ?.getAttribute("aria-valuetext") ?? "",
          ratingDisabled: document.querySelector('[role="radiogroup"][aria-label="Unavailable framework quality"]')
            ?.getAttribute("aria-disabled") ?? "",
          codeInline: inlineCode instanceof HTMLElement
            ? [inlineCode.style.fontFamily, inlineCode.style.whiteSpace] : [],
          codeBlock: blockCode instanceof HTMLElement
            ? [blockCode.style.fontFamily, blockCode.style.whiteSpace] : [],
          resizable: resizeHandle instanceof HTMLElement ? {
            value: resizeHandle.getAttribute("aria-valuetext"),
            minimum: resizeHandle.getAttribute("aria-valuemin"),
            maximum: resizeHandle.getAttribute("aria-valuemax"),
            step: resizeHandle.getAttribute("data-volang-step"),
            tabIndex: resizeHandle.tabIndex,
          } : null,
          navigation: productNavigation !== null,
          navigationGroup: productNavigation
            ?.querySelector('[role="group"][aria-label="Workspace"]') !== null,
          navigationOverflow: productNavigation
            ?.querySelector('[role="button"][aria-label="More Product navigation"]')
            ?.getAttribute("aria-expanded") ?? "",
          currentPage: document.querySelector('[role="link"][aria-label="Home"]')
            ?.getAttribute("aria-current") ?? "",
          scrollArea: document.querySelector('[role="region"][aria-label="Portable scroll area"]') !== null,
          carousel: carousel?.getAttribute("aria-description") ?? "",
          combobox: packageCombobox instanceof HTMLInputElement ? {
            value: packageCombobox.value,
            id: packageCombobox.id,
            controls: packageCombobox.getAttribute("aria-controls"),
            autocomplete: packageCombobox.getAttribute("aria-autocomplete"),
            expanded: packageCombobox.getAttribute("aria-expanded"),
          } : null,
          multiSelect: platformMultiSelect instanceof HTMLInputElement ? {
            controls: platformMultiSelect.getAttribute("aria-controls"),
            expanded: platformMultiSelect.getAttribute("aria-expanded"),
            multiple: document.getElementById(platformMultiSelect.getAttribute("aria-controls") ?? "")
              ?.getAttribute("aria-multiselectable") ?? "",
          } : null,
          datePicker: releaseDate instanceof HTMLInputElement ? {
            value: releaseDate.value,
            controls: releaseDate.getAttribute("aria-controls"),
            expanded: releaseDate.getAttribute("aria-expanded"),
            invalid: releaseDate.getAttribute("aria-invalid"),
          } : null,
          calendar: releaseWindow === null ? null : {
            cells: releaseWindow.querySelectorAll('[role="gridcell"]').length,
            selected: releaseWindow.querySelectorAll('[role="gridcell"][aria-selected="true"]').length,
            disabled: releaseWindow.querySelector('[role="gridcell"][aria-label="Release window 2026-08-20"]')
              ?.getAttribute("aria-disabled") === "true"
              || releaseWindow.querySelector('[role="gridcell"][aria-label="Release window 2026-08-20"]')?.disabled === true,
            focusable: releaseWindow.querySelector('[role="gridcell"][aria-label="Release window 2026-08-18"]')
              ?.getAttribute("tabindex") ?? "",
          },
          uploader: uploader === null ? null : {
            files: uploader.querySelectorAll('[role="listitem"]').length,
            dropzone: uploader.querySelector('[role="button"][aria-label="Release artifacts"]')
              ?.getAttribute("tabindex") ?? "",
            progress: uploader.querySelector('[role="progressbar"][aria-label="app.wasm upload progress"]')
              ?.getAttribute("aria-valuetext") ?? "",
            rejected: uploader.querySelector('[role="listitem"][aria-label^="archive.iso, Rejected"]') !== null,
          },
          contextMenu: contextTarget instanceof HTMLElement ? {
            focusable: contextTarget.getAttribute("tabindex"),
            closed: document.querySelector('[role="menu"][aria-label="Project canvas menu"]')?.closest('[hidden]') !== null,
          } : null,
        },
        visual: {
          switchAppearance: switchStyle?.appearance ?? "",
          switchRadius: switchStyle?.borderRadius ?? "",
          selectedRadio: selectedRadioStyle === null ? []
            : [selectedRadioStyle.backgroundColor, selectedRadioStyle.color],
          unselectedRadio: unselectedRadioStyle === null ? []
            : [unselectedRadioStyle.backgroundColor, unselectedRadioStyle.color],
          hierarchy: canvas instanceof HTMLElement && previewPanel instanceof HTMLElement
            ? [getComputedStyle(canvas).backgroundColor, getComputedStyle(previewPanel).backgroundColor] : [],
          hierarchyShadow: previewPanel instanceof HTMLElement ? getComputedStyle(previewPanel).boxShadow : "",
        },
        diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
      };
    })()`,
    (value) => value?.title === true
      && value?.checkbox === true
      && value?.textarea === true
      && JSON.stringify(value?.slider) === JSON.stringify(["72", "0", "100", "1"])
      && JSON.stringify(value?.accordion) === JSON.stringify(["true", "false"])
      && JSON.stringify(value?.accordionText) === JSON.stringify(["−  Development runtime", "+  Release targets"])
      && value?.essentials?.avatar === true && value?.essentials?.iconButton === true
      && value?.essentials?.loadingButton === true && value?.essentials?.search === "Volang"
      && JSON.stringify(value?.essentials?.number)
        === JSON.stringify({ value: "3", now: "3", minimum: "1", maximum: "10", step: "1" })
      && value?.essentials?.target === "true" && value?.essentials?.list === "true"
      && value?.essentials?.step === "step" && value?.essentials?.form === true
      && value?.extended?.avatarGroup === true && value?.extended?.overflowAvatar === true
      && value?.extended?.avatarOverlap === true && value?.extended?.inputGroup === true
      && value?.extended?.inputGroupFields === 1 && value?.extended?.inputGroupBorder === ""
      && JSON.stringify(value?.extended?.invalidInputGroup) === JSON.stringify({
        invalid: "true", description: "Token is required", fields: 1, fieldRequired: true,
      })
      && value?.extended?.bold === "true"
      && value?.extended?.underlineDisabled === true
      && value?.extended?.rating === "4 of 5 stars" && value?.extended?.ratingChecked === "true"
      && value?.extended?.ratingReadOnly === "3 of 5 stars" && value?.extended?.ratingDisabled === "true"
      && JSON.stringify(value?.extended?.codeInline) === JSON.stringify(["monospace", "nowrap"])
      && JSON.stringify(value?.extended?.codeBlock) === JSON.stringify(["monospace", "pre"])
      && JSON.stringify(value?.extended?.resizable)
        === JSON.stringify({ value: "280", minimum: "0", maximum: "600", step: "16", tabIndex: 0 })
      && value?.extended?.navigation === true && value?.extended?.navigationGroup === true
      && value?.extended?.navigationOverflow === "false" && value?.extended?.currentPage === "page"
      && value?.extended?.scrollArea === true && value?.extended?.carousel === "1 of 3"
      && JSON.stringify(value?.extended?.combobox) === JSON.stringify({
        value: "Renderer package", id: "gallery-package-combobox-input",
        controls: "gallery-package-combobox-listbox", autocomplete: "list", expanded: "false",
      })
      && JSON.stringify(value?.extended?.multiSelect) === JSON.stringify({
        controls: "gallery-target-multiselect-listbox", expanded: "false", multiple: "true",
      })
      && JSON.stringify(value?.extended?.datePicker) === JSON.stringify({
        value: "29/08/2026", controls: "gallery-release-date-calendar", expanded: "false", invalid: "false",
      })
      && JSON.stringify(value?.extended?.calendar) === JSON.stringify({
        cells: 42, selected: 7, disabled: true, focusable: "0",
      })
      && JSON.stringify(value?.extended?.uploader) === JSON.stringify({
        files: 3, dropzone: "0", progress: "64%", rejected: true,
      })
      && JSON.stringify(value?.extended?.contextMenu) === JSON.stringify({
        focusable: "0", closed: true,
      })
      && value?.visual?.switchAppearance === "none" && value?.visual?.switchRadius !== "0px"
      && Array.isArray(value?.visual?.selectedRadio) && Array.isArray(value?.visual?.unselectedRadio)
      && value.visual.selectedRadio.length === 2 && value.visual.unselectedRadio.length === 2
      && value.visual.selectedRadio[0] !== value.visual.unselectedRadio[0]
      && value.visual.selectedRadio[1] !== value.visual.unselectedRadio[1]
      && Array.isArray(value?.visual?.hierarchy) && value.visual.hierarchy.length === 2
      && value.visual.hierarchy[0] !== value.visual.hierarchy[1]
      && typeof value?.visual?.hierarchyShadow === "string"
      && value.visual.hierarchyShadow !== "" && value.visual.hierarchyShadow !== "none"
      && value?.diagnostic === "",
    timeoutMilliseconds,
  );

  const primaryHoverPoint = await pollEvaluation(contract,
    `(() => {
      const button = Array.from(document.querySelectorAll("button")).find(
        (candidate) => (candidate.textContent ?? "").trim() === "Primary",
      );
      if (!(button instanceof HTMLButtonElement)) return null;
      button.scrollIntoView({ block: "center", inline: "center" });
      const rect = button.getBoundingClientRect();
      const style = getComputedStyle(button);
      return {
        x: rect.left + rect.width / 2,
        y: rect.top + rect.height / 2,
        background: style.backgroundColor,
        hoverToken: style.getPropertyValue("--volang-hover-background").trim(),
        hoverContract: button.hasAttribute("data-volang-hover-background"),
        pressedContract: button.hasAttribute("data-volang-pressed-background"),
        focusContract: button.hasAttribute("data-volang-focus-ring"),
      };
    })()`,
    (value) => value !== null && value.hoverContract === true && value.pressedContract === true
      && value.focusContract === true && value.hoverToken !== "",
    timeoutMilliseconds,
  );
  await contract.mouseEvent({
    type: "mouseMoved",
    x: primaryHoverPoint.x,
    y: primaryHoverPoint.y,
  });
  checkpoints.interactionVisuals = await pollEvaluation(contract,
    `(() => {
      const button = Array.from(document.querySelectorAll("button")).find(
        (candidate) => (candidate.textContent ?? "").trim() === "Primary",
      );
      return button instanceof HTMLButtonElement ? getComputedStyle(button).backgroundColor : "";
    })()`,
    (value) => typeof value === "string" && value !== "" && value !== primaryHoverPoint.background,
    timeoutMilliseconds,
  );
  await contract.mouseEvent({ type: "mouseMoved", x: 0, y: 0 });

  const searchChanged = await evaluate(`(() => {
    const input = document.querySelector('[role="searchbox"][aria-label="Component search"]');
    if (!(input instanceof HTMLInputElement)) return false;
    input.scrollIntoView({ block: "center", inline: "center" });
    input.focus();
    input.value = "runtime";
    input.setSelectionRange(input.value.length, input.value.length);
    input.dispatchEvent(new InputEvent("input", { bubbles: true, inputType: "insertText" }));
    return true;
  })()`);
  if (searchChanged !== true) throw new Error("UIKit gallery could not edit component search");
  await focusElement('[role="searchbox"][aria-label="Component search"]');
  await pressKey("Enter", "Enter", 13);
  checkpoints.search = await pollEvaluation(contract,
    `Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
      .some((text) => text.includes("Search submitted: runtime"))`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await activateButton("Clear Component search");
  checkpoints.searchCleared = await pollEvaluation(contract,
    `(() => {
      const input = document.querySelector('[role="searchbox"][aria-label="Component search"]');
      return {
        value: input instanceof HTMLInputElement ? input.value : null,
        focused: document.activeElement === input,
      };
    })()`,
    (value) => value?.value === "" && value?.focused === true,
    timeoutMilliseconds,
  );

  await activateButton("Increase Build replicas");
  checkpoints.numberInput = await pollEvaluation(contract,
    `(() => {
      const input = document.querySelector('[role="spinbutton"][aria-label="Build replicas"]');
      return {
        value: input instanceof HTMLInputElement ? input.value : null,
        now: input?.getAttribute("aria-valuenow") ?? "",
        status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
          .some((text) => text.includes("Build replica count changed")),
      };
    })()`,
    (value) => value?.value === "4" && value?.now === "4" && value?.status === true,
    timeoutMilliseconds,
  );

  await activateButton("Desktop");
  checkpoints.segmented = await pollEvaluation(contract,
    `({
      checked: document.querySelector('[role="radio"][aria-label="Desktop"]')?.getAttribute("aria-checked") ?? "",
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Preview target: desktop")),
    })`,
    (value) => value?.checked === "true" && value?.status === true,
    timeoutMilliseconds,
  );

  const listActivated = await evaluate(`(() => {
    const item = document.querySelector('[role="option"][aria-label="Renderer"]');
    if (!(item instanceof HTMLElement)) return false;
    item.scrollIntoView({ block: "center", inline: "center" });
    item.click();
    return true;
  })()`);
  if (listActivated !== true) throw new Error("UIKit gallery could not activate Renderer list item");
  checkpoints.list = await pollEvaluation(contract,
    `document.querySelector('[role="option"][aria-label="Renderer"]')?.getAttribute("aria-selected") ?? ""`,
    (value) => value === "true",
    timeoutMilliseconds,
  );
  await activateButton("Remove Stable");
  checkpoints.tag = await pollEvaluation(contract,
    `({
      removed: document.querySelector('[role="button"][aria-label="Remove Stable"]') === null,
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Stable tag removed")),
    })`,
    (value) => value?.removed === true && value?.status === true,
    timeoutMilliseconds,
  );

  await activateButton("Resolve package");
  checkpoints.inputGroup = await pollEvaluation(contract,
    `Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
      .some((text) => text.includes("Package resolved"))`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await activateButton("Copy Build command");
  checkpoints.codeSnippet = await pollEvaluation(contract,
    `({
      copied: Array.from(document.querySelectorAll('button')).some(
        (button) => button.getAttribute("aria-label") === "Copied Build command",
      ),
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Code copied")),
    })`,
    (value) => value?.copied === true && value?.status === true,
    timeoutMilliseconds,
  );
  await setNamedInput("Framework package", "run");
  checkpoints.comboboxFiltered = await pollEvaluation(contract,
    `(() => {
      const input = document.querySelector('input[aria-label="Framework package"]');
      const active = input?.getAttribute("aria-activedescendant") ?? "";
      return {
        expanded: input?.getAttribute("aria-expanded") ?? "",
        active,
        activeText: (document.getElementById(active)?.textContent ?? "").trim(),
      };
    })()`,
    (value) => value?.expanded === "true" && value?.active !== ""
      && value?.activeText === "Runtime package",
    timeoutMilliseconds,
  );
  await pressKey("Enter", "Enter", 13);
  checkpoints.comboboxSelected = await pollEvaluation(contract,
    `(() => {
      const input = document.querySelector('input[aria-label="Framework package"]');
      return {
        value: input instanceof HTMLInputElement ? input.value : "",
        expanded: input?.getAttribute("aria-expanded") ?? "",
        status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
          .some((text) => text.includes("Framework package: runtime")),
      };
    })()`,
    (value) => value?.value === "Runtime package" && value?.expanded === "false"
      && value?.status === true,
    timeoutMilliseconds,
  );
  await setNamedInput("Release platforms", "desk");
  checkpoints.multiSelectFiltered = await pollEvaluation(contract,
    `(() => {
      const input = document.querySelector('input[aria-label="Release platforms"]');
      const active = input?.getAttribute("aria-activedescendant") ?? "";
      return {
        expanded: input?.getAttribute("aria-expanded") ?? "",
        activeText: (document.getElementById(active)?.textContent ?? "").trim(),
      };
    })()`,
    (value) => value?.expanded === "true" && value?.activeText === "Desktop target",
    timeoutMilliseconds,
  );
  await pressKey("Enter", "Enter", 13);
  checkpoints.multiSelectSelected = await pollEvaluation(contract,
    `({
      selected: document.querySelector('[role="group"][aria-label="Desktop target"]') !== null,
      multiple: document.getElementById("gallery-target-multiselect-listbox")
        ?.getAttribute("aria-multiselectable") ?? "",
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Release platform changed: desktop")),
    })`,
    (value) => value?.selected === true && value?.multiple === "true" && value?.status === true,
    timeoutMilliseconds,
  );
  await pressKey("Escape", "Escape", 27);
  await activateButton("Remove Desktop target");
  checkpoints.multiSelectRemoved = await pollEvaluation(contract,
    `document.querySelector('[role="group"][aria-label="Desktop target"]') === null`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await setNamedInput("Release date", "30/08/2026");
  await pressKey("Enter", "Enter", 13);
  checkpoints.datePickerParsed = await pollEvaluation(contract,
    `(() => {
      const input = document.querySelector('input[aria-label="Release date"]');
      return {
        value: input instanceof HTMLInputElement ? input.value : "",
        invalid: input?.getAttribute("aria-invalid") ?? "",
        status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
          .some((text) => text.includes("Release date: 2026-08-30")),
      };
    })()`,
    (value) => value?.value === "30/08/2026" && value?.invalid === "false"
      && value?.status === true,
    timeoutMilliseconds,
  );
  await activateButton("Release window 2026-08-21");
  checkpoints.calendarRangeRestarted = await pollEvaluation(contract,
    `(() => {
      const grid = document.querySelector('[role="grid"][aria-label="Release window dates"]');
      return {
        selected: grid?.querySelectorAll('[role="gridcell"][aria-selected="true"]').length ?? -1,
        current: grid?.querySelector('[role="gridcell"][aria-label="Release window 2026-08-21"]')
          ?.getAttribute("aria-selected") ?? "",
        status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
          .some((text) => text.includes("Release window date: 2026-08-21")),
      };
    })()`,
    (value) => value?.selected === 1 && value?.current === "true" && value?.status === true,
    timeoutMilliseconds,
  );
  await pressKey("ArrowRight", "ArrowRight", 39);
  checkpoints.calendarKeyboard = await pollEvaluation(contract,
    `(() => {
      const next = document.querySelector('[role="gridcell"][aria-label="Release window 2026-08-22"]');
      return {
        active: document.activeElement?.getAttribute("aria-label") ?? "",
        tabIndex: next?.getAttribute("tabindex") ?? "",
      };
    })()`,
    (value) => value?.active === "Release window 2026-08-22" && value?.tabIndex === "0",
    timeoutMilliseconds,
  );
  await activateButton("Cancel app.wasm");
  checkpoints.uploadCancelled = await pollEvaluation(contract,
    `({
      cancelled: document.querySelector('[role="listitem"][aria-label^="app.wasm, Cancelled"]') !== null,
      retry: document.querySelector('[role="button"][aria-label="Retry app.wasm"]') !== null,
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Upload cancelled: bundle")),
    })`,
    (value) => value?.cancelled === true && value?.retry === true && value?.status === true,
    timeoutMilliseconds,
  );
  await activateButton("Retry app.wasm");
  checkpoints.uploadRetried = await pollEvaluation(contract,
    `({
      running: document.querySelector('[role="listitem"][aria-label^="app.wasm, Uploading"]') !== null,
      cancel: document.querySelector('[role="button"][aria-label="Cancel app.wasm"]') !== null,
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Upload retried: bundle")),
    })`,
    (value) => value?.running === true && value?.cancel === true && value?.status === true,
    timeoutMilliseconds,
  );
  const droppedFile = await evaluate(`(() => {
    const dropzone = document.querySelector('[role="button"][aria-label="Release artifacts"]');
    if (!(dropzone instanceof HTMLElement) || typeof DataTransfer !== "function") return false;
    const transfer = new DataTransfer();
    transfer.items.add(new File(["package main"], "dropped.vo", { type: "text/plain" }));
    dropzone.dispatchEvent(new DragEvent("dragover", { bubbles: true, cancelable: true, dataTransfer: transfer }));
    dropzone.dispatchEvent(new DragEvent("drop", { bubbles: true, cancelable: true, dataTransfer: transfer }));
    return true;
  })()`);
  if (droppedFile !== true) throw new Error("UIKit gallery could not synthesize a file drop");
  checkpoints.uploadDropped = await pollEvaluation(contract,
    `({
      file: document.querySelector('[role="listitem"][aria-label^="dropped.vo, Queued"]') !== null,
      count: document.querySelector('[role="list"][aria-label="Release artifacts files"]')
        ?.querySelectorAll('[role="listitem"]').length ?? -1,
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Upload files accepted")),
    })`,
    (value) => value?.file === true && value?.count === 4 && value?.status === true,
    timeoutMilliseconds,
  );
  const rejectedFileType = await evaluate(`(() => {
    const dropzone = document.querySelector('[role="button"][aria-label="Release artifacts"]');
    if (!(dropzone instanceof HTMLElement) || typeof DataTransfer !== "function") return false;
    const transfer = new DataTransfer();
    transfer.items.add(new File(["binary"], "payload.exe", { type: "application/octet-stream" }));
    dropzone.dispatchEvent(new DragEvent("drop", { bubbles: true, cancelable: true, dataTransfer: transfer }));
    return true;
  })()`);
  if (rejectedFileType !== true) throw new Error("UIKit gallery could not synthesize a rejected file drop");
  checkpoints.uploadTypeRejected = await pollEvaluation(contract,
    `({
      rejected: document.querySelector('[role="listitem"][aria-label^="rejected batch, Rejected"]') !== null,
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Upload rejected: File type is not accepted")),
    })`,
    (value) => value?.rejected === true && value?.status === true,
    timeoutMilliseconds,
  );
  const contextInvoked = await evaluate(`(() => {
    const target = document.querySelector('[role="region"][aria-label="Project canvas"]');
    if (!(target instanceof HTMLElement)) return false;
    target.scrollIntoView({ block: "center", inline: "center" });
    target.focus();
    return !target.dispatchEvent(new MouseEvent("contextmenu", {
      bubbles: true, cancelable: true, button: 2,
      clientX: Math.max(0, innerWidth - 2), clientY: Math.max(0, innerHeight - 2),
    }));
  })()`);
  if (contextInvoked !== true) throw new Error("UIKit gallery context menu did not prevent the host menu");
  checkpoints.contextMenuOpened = await pollEvaluation(contract,
    `(() => {
      const menu = document.querySelector('[role="menu"][aria-label="Project canvas menu"]');
      const bounds = menu?.getBoundingClientRect();
      return {
        open: menu !== null && menu.closest('[hidden]') === null,
        active: document.activeElement?.getAttribute("aria-label") ?? "",
        collision: bounds !== undefined && bounds.right <= innerWidth + 0.5 && bounds.bottom <= innerHeight + 0.5,
      };
    })()`,
    (value) => value?.open === true && value?.active === "Open project" && value?.collision === true,
    timeoutMilliseconds,
  );
  await pressKey("p", "KeyP", 80);
  checkpoints.contextMenuTypeahead = await pollEvaluation(contract,
    `document.activeElement?.getAttribute("aria-label") ?? ""`,
    (value) => value === "Project details",
    timeoutMilliseconds,
  );
  await pressKey("v", "KeyV", 86);
  await pollEvaluation(contract,
    `document.activeElement?.getAttribute("aria-label") ?? ""`,
    (value) => value === "View options",
    timeoutMilliseconds,
  );
  await pressKey("ArrowRight", "ArrowRight", 39);
  checkpoints.contextSubmenu = await pollEvaluation(contract,
    `({
      menus: document.querySelectorAll('[role="menu"]').length,
      expanded: document.querySelector('[role="menuitem"][aria-label="View options"]')
        ?.getAttribute("aria-expanded") ?? "",
      active: document.activeElement?.getAttribute("aria-label") ?? "",
      phase: document.getElementById("volang-root")?.dataset.volangActivation ?? "",
      diagnostic: document.getElementById("volang-diagnostic")?.textContent ?? "",
    })`,
    (value) => value?.menus >= 2 && value?.expanded === "true" && value?.active === "Compact view",
    timeoutMilliseconds,
  );
  await pressKey("Enter", "Enter", 13);
  checkpoints.contextMenuActivated = await pollEvaluation(contract,
    `({
      closed: document.querySelector('[role="menu"][aria-label="Project canvas menu"]')?.closest('[hidden]') !== null,
      focus: document.activeElement?.getAttribute("aria-label") ?? "",
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Context action: compact")),
    })`,
    (value) => value?.closed === true && value?.focus === "Project canvas" && value?.status === true,
    timeoutMilliseconds,
  );
  await pressKey("ContextMenu", "ContextMenu", 93);
  await pollEvaluation(contract,
    `document.querySelector('[role="menu"][aria-label="Project canvas menu"]')?.closest('[hidden]') === null`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await pressKey("Escape", "Escape", 27);
  checkpoints.contextMenuDismissed = await pollEvaluation(contract,
    `({
      closed: document.querySelector('[role="menu"][aria-label="Project canvas menu"]')?.closest('[hidden]') !== null,
      focus: document.activeElement?.getAttribute("aria-label") ?? "",
    })`,
    (value) => value?.closed === true && value?.focus === "Project canvas",
    timeoutMilliseconds,
  );
  const contextHoverOpened = await evaluate(`(() => {
    const target = document.querySelector('[role="region"][aria-label="Project canvas"]');
    if (!(target instanceof HTMLElement)) return false;
    target.focus();
    return !target.dispatchEvent(new MouseEvent("contextmenu", {
      bubbles: true, cancelable: true, button: 2, clientX: 80, clientY: 80,
    }));
  })()`);
  if (contextHoverOpened !== true) throw new Error("UIKit gallery could not reopen the context menu for pointer testing");
  await pollEvaluation(contract,
    `document.querySelector('[role="menu"][aria-label="Project canvas menu"]')?.closest('[hidden]') === null`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  const contextHovered = await evaluate(`(() => {
    const item = document.querySelector('[role="menuitem"][aria-label="View options"]');
    if (!(item instanceof HTMLElement)) return false;
    item.dispatchEvent(new PointerEvent("pointermove", {
      bubbles: true, cancelable: true, pointerId: 7, pointerType: "mouse",
      clientX: 90, clientY: 90,
    }));
    return true;
  })()`);
  if (contextHovered !== true) throw new Error("UIKit gallery could not hover the context submenu trigger");
  checkpoints.contextMenuHover = await pollEvaluation(contract,
    `({
      menus: document.querySelectorAll('[role="menu"]').length,
      expanded: document.querySelector('[role="menuitem"][aria-label="View options"]')
        ?.getAttribute("aria-expanded") ?? "",
      shortcut: Array.from(document.querySelectorAll('[role="menuitem"]'), (item) => item.textContent ?? "")
        .some((text) => text.includes("Enter")),
      check: document.querySelector('[role="menuitemcheckbox"][aria-label="Compact view"]')
        ?.getAttribute("aria-checked") ?? "",
    })`,
    (value) => value?.menus >= 2 && value?.expanded === "true"
      && value?.shortcut === true && value?.check === "true",
    timeoutMilliseconds,
  );
  await activateButton("Comfortable view");
  checkpoints.contextMenuPointerActivated = await pollEvaluation(contract,
    `({
      closed: document.querySelector('[role="menu"][aria-label="Project canvas menu"]')?.closest('[hidden]') !== null,
      focus: document.activeElement?.getAttribute("aria-label") ?? "",
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Context action: comfortable")),
    })`,
    (value) => value?.closed === true && value?.focus === "Project canvas" && value?.status === true,
    timeoutMilliseconds,
  );
  await activateButton("More Product navigation");
  checkpoints.navigationOverflow = await pollEvaluation(contract,
    `(() => {
      const trigger = document.querySelector('[role="button"][aria-label="More Product navigation"]');
      const metrics = document.querySelector('[role="link"][aria-label="Metrics"]');
      return {
        expanded: trigger?.getAttribute("aria-expanded") ?? "",
        overflow: document.querySelector('[role="group"][aria-label="Product navigation overflow"]') !== null,
        metricsVisible: metrics !== null && metrics.closest('[hidden]') === null,
      };
    })()`,
    (value) => value?.expanded === "true" && value?.overflow === true
      && value?.metricsVisible === true,
    timeoutMilliseconds,
  );
  await activateButton("Compact");
  checkpoints.navigationCompact = await pollEvaluation(contract,
    `(() => {
      const navigation = document.querySelector('[role="navigation"][aria-label="Product navigation"]');
      const home = navigation?.querySelector('[role="link"][aria-label="Home"]');
      return {
        checked: document.querySelector('[role="radio"][aria-label="Compact"]')
          ?.getAttribute("aria-checked") ?? "",
        homeText: (home?.textContent ?? "").trim(),
        overflowAbsent: navigation?.querySelector('[aria-label="More Product navigation"]') === null,
        status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
          .some((text) => text.includes("Navigation layout: compact")),
      };
    })()`,
    (value) => value?.checked === "true" && value?.homeText === "H"
      && value?.overflowAbsent === true && value?.status === true,
    timeoutMilliseconds,
  );
  await activateButton("Side");
  checkpoints.navigationSide = await pollEvaluation(contract,
    `(() => {
      const navigation = document.querySelector('[role="navigation"][aria-label="Product navigation"]');
      const home = navigation?.querySelector('[role="link"][aria-label="Home"]');
      const components = navigation?.querySelector('[role="link"][aria-label="Components"]');
      const homeBounds = home?.getBoundingClientRect();
      const componentsBounds = components?.getBoundingClientRect();
      return {
        checked: document.querySelector('[role="radio"][aria-label="Side"]')
          ?.getAttribute("aria-checked") ?? "",
        homeText: (home?.textContent ?? "").trim(),
        vertical: homeBounds !== undefined && componentsBounds !== undefined
          && componentsBounds.top > homeBounds.top && componentsBounds.left === homeBounds.left,
        status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
          .some((text) => text.includes("Navigation layout: side")),
      };
    })()`,
    (value) => value?.checked === "true" && value?.homeText === "Home"
      && value?.vertical === true && value?.status === true,
    timeoutMilliseconds,
  );
  await focusElement('[role="button"][aria-label="Bold"]');
  await pressKey("ArrowRight", "ArrowRight", 39);
  checkpoints.toggleRovingFocus = await pollEvaluation(contract,
    `(() => {
      const bold = document.querySelector('[role="button"][aria-label="Bold"]');
      const italic = document.querySelector('[role="button"][aria-label="Italic"]');
      return {
        active: document.activeElement?.getAttribute("aria-label") ?? "",
        boldTabIndex: bold instanceof HTMLElement ? bold.tabIndex : null,
        italicTabIndex: italic instanceof HTMLElement ? italic.tabIndex : null,
        boldPressed: bold?.getAttribute("aria-pressed") ?? "",
        italicPressed: italic?.getAttribute("aria-pressed") ?? "",
      };
    })()`,
    (value) => value?.active === "Italic" && value?.boldTabIndex === -1
      && value?.italicTabIndex === 0 && value?.boldPressed === "true"
      && value?.italicPressed === "false",
    timeoutMilliseconds,
  );
  await pressKey(" ", "Space", 32);
  checkpoints.toggleGroup = await pollEvaluation(contract,
    `({
      pressed: document.querySelector('[role="button"][aria-label="Italic"]')
        ?.getAttribute("aria-pressed") ?? "",
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Text style changed: italic")),
    })`,
    (value) => value?.pressed === "true" && value?.status === true,
    timeoutMilliseconds,
  );
  const ratingChanged = await evaluate(`(() => {
    const control = document.querySelector('[role="radio"][aria-label="Framework quality, 5 of 5 stars"]');
    if (!(control instanceof HTMLElement)) return false;
    control.scrollIntoView({ block: "center", inline: "center" });
    control.click();
    return true;
  })()`);
  if (ratingChanged !== true) throw new Error("UIKit gallery could not change rating");
  checkpoints.rating = await pollEvaluation(contract,
    `({
      checked: document.querySelector('[role="radio"][aria-label="Framework quality, 5 of 5 stars"]')
        ?.getAttribute("aria-checked") ?? "",
      value: document.querySelector('[role="radiogroup"][aria-label="Framework quality"]')
        ?.getAttribute("aria-valuetext") ?? "",
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Framework quality changed")),
    })`,
    (value) => value?.checked === "true" && value?.value === "5 of 5 stars"
      && value?.status === true,
    timeoutMilliseconds,
  );
  await focusElement('[role="separator"][aria-label="Inspector size"]');
  await pressKey("ArrowRight", "ArrowRight", 39);
  checkpoints.resizableKeyboard = await pollEvaluation(contract,
    `({
      value: document.querySelector('[role="separator"][aria-label="Inspector size"]')
        ?.getAttribute("aria-valuetext") ?? "",
      width: document.querySelector('[role="region"][aria-label="Resizable primary pane"]')
        ?.style.width ?? "",
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Inspector size changed")),
    })`,
    (value) => value?.value === "296" && value?.width === "296px" && value?.status === true,
    timeoutMilliseconds,
  );
  const dispatchResizePointer = async (type, x, buttons) => evaluate(`(() => {
    const handle = document.querySelector('[role="separator"][aria-label="Inspector size"]');
    if (!(handle instanceof HTMLElement)) return false;
    return handle.dispatchEvent(new PointerEvent(${JSON.stringify(type)}, {
      bubbles: true, pointerId: 7, pointerType: "mouse", button: 0,
      buttons: ${buttons}, clientX: ${x}, clientY: 10,
    }));
  })()`);
  for (const [type, x, buttons] of [
    ["pointerdown", 100, 1], ["pointermove", 132, 1], ["pointerup", 132, 0],
  ]) {
    if (await dispatchResizePointer(type, x, buttons) !== true) {
      throw new Error(`UIKit gallery could not dispatch resize ${type}`);
    }
    await contract.settleInput();
  }
  checkpoints.resizablePointer = await pollEvaluation(contract,
    `({
      value: document.querySelector('[role="separator"][aria-label="Inspector size"]')
        ?.getAttribute("aria-valuetext") ?? "",
      width: document.querySelector('[role="region"][aria-label="Resizable primary pane"]')
        ?.style.width ?? "",
    })`,
    (value) => value?.value === "328" && value?.width === "328px",
    timeoutMilliseconds,
  );
  await focusElement('[role="separator"][aria-label="Inspector size"]');
  await pressKey("Home", "Home", 36);
  checkpoints.resizableCollapsed = await pollEvaluation(contract,
    `({
      value: document.querySelector('[role="separator"][aria-label="Inspector size"]')
        ?.getAttribute("aria-valuetext") ?? "",
      width: document.querySelector('[role="region"][aria-label="Resizable primary pane"]')
        ?.style.width ?? "",
    })`,
    (value) => value?.value === "0" && value?.width === "0px",
    timeoutMilliseconds,
  );
  await activateButton("Next, Compiler");
  checkpoints.carousel = await pollEvaluation(contract,
    `({
      position: document.querySelector('[role="region"][aria-label="Runtime pipeline"]')
        ?.getAttribute("aria-description") ?? "",
      slide: document.querySelector('[role="group"][aria-label="Runtime"]') !== null,
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Carousel item: runtime")),
    })`,
    (value) => value?.position === "2 of 3" && value?.slide === true && value?.status === true,
    timeoutMilliseconds,
  );
  const dispatchCarouselPointer = async (type, x, buttons) => evaluate(`(() => {
    const slide = document.querySelector('[role="group"][aria-label="Runtime"]');
    if (!(slide instanceof HTMLElement)) return false;
    return slide.dispatchEvent(new PointerEvent(${JSON.stringify(type)}, {
      bubbles: true, pointerId: 11, pointerType: "touch", button: 0,
      buttons: ${buttons}, clientX: ${x}, clientY: 20,
    }));
  })()`);
  for (const [type, x, buttons] of [
    ["pointerdown", 180, 1], ["pointermove", 110, 1], ["pointerup", 110, 0],
  ]) {
    if (await dispatchCarouselPointer(type, x, buttons) !== true) {
      throw new Error(`UIKit gallery could not dispatch carousel ${type}`);
    }
    await contract.settleInput();
  }
  checkpoints.carouselPointer = await pollEvaluation(contract,
    `({
      position: document.querySelector('[role="region"][aria-label="Runtime pipeline"]')
        ?.getAttribute("aria-description") ?? "",
      slide: (() => {
        const slide = document.querySelector('[role="group"][aria-label="Release"]');
        return slide !== null && slide.closest('[hidden]') === null;
      })(),
      status: Array.from(document.querySelectorAll('[role="status"]'), (node) => node.textContent ?? "")
        .some((text) => text.includes("Carousel item: release")),
    })`,
    (value) => value?.position === "3 of 3" && value?.slide === true && value?.status === true,
    timeoutMilliseconds,
  );
  const scrollChanged = await evaluate(`(() => {
    const area = document.querySelector('[role="region"][aria-label="Portable scroll area"]');
    if (!(area instanceof HTMLElement)) return false;
    area.scrollIntoView({ block: "center", inline: "center" });
    area.scrollLeft = 120;
    area.scrollTop = 48;
    area.dispatchEvent(new Event("scroll", { bubbles: true }));
    return true;
  })()`);
  if (scrollChanged !== true) throw new Error("UIKit gallery could not scroll controlled area");
  checkpoints.scrollArea = await pollEvaluation(contract,
    `(() => {
      const area = document.querySelector('[role="region"][aria-label="Portable scroll area"]');
      return area instanceof HTMLElement ? {
        x: area.scrollLeft,
        y: area.scrollTop,
        state: document.querySelector('[role="status"][aria-label="Scroll position"]')?.textContent ?? "",
      } : null;
    })()`,
    (value) => value?.x === 120 && value?.y === 48
      && value?.state.includes("Scroll position: 120,48"),
    timeoutMilliseconds,
  );

  await focusElement('[role="checkbox"][aria-label="Include source maps"]');
  await pressKey(" ", "Space", 32);
  checkpoints.checkbox = await pollEvaluation(contract,
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
  checkpoints.textArea = await pollEvaluation(contract,
    `document.querySelector('textarea[aria-label="Release notes"]')?.value ?? null`,
    (value) => value === notes,
    timeoutMilliseconds,
  );

  await focusElement('[role="slider"][aria-label="Optimization level"]');
  await pressKey("ArrowRight", "ArrowRight", 39);
  checkpoints.slider = await pollEvaluation(contract,
    `document.querySelector('[role="slider"][aria-label="Optimization level"]')?.value ?? null`,
    (value) => value === "73",
    timeoutMilliseconds,
  );

  await activateButton("+  Release targets");
  checkpoints.accordion = await pollEvaluation(contract,
    `Array.from(document.querySelectorAll("button"), (button) => ({
      text: (button.textContent ?? "").trim(), expanded: button.getAttribute("aria-expanded"),
    })).filter((item) => item.text.includes("runtime") || item.text.includes("Release targets"))`,
    (value) => Array.isArray(value)
      && value.some((item) => item.text === "+  Development runtime" && item.expanded === "false")
      && value.some((item) => item.text === "−  Release targets" && item.expanded === "true"),
    timeoutMilliseconds,
  );

  await activateButton("New");
  checkpoints.menu = await pollEvaluation(contract,
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
  checkpoints.menuToggle = await pollEvaluation(contract,
    `(() => {
      const control = document.querySelector('[role="menuitemcheckbox"][aria-label="Auto save"]');
      return control instanceof HTMLInputElement ? control.checked : null;
    })()`,
    (value) => value === false,
    timeoutMilliseconds,
  );

  await activateButton("Package ascending");
  checkpoints.dataSort = await pollEvaluation(contract,
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
  checkpoints.dataSelection = await pollEvaluation(contract,
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
  checkpoints.treeCollapsed = await pollEvaluation(contract,
    `({
      expanded: document.querySelector('[role="treeitem"][aria-label="UIKit"]')
        ?.getAttribute("aria-expanded") ?? "",
      components: document.querySelector('[role="treeitem"][aria-label="Components"]') !== null,
    })`,
    (value) => value?.expanded === "false" && value?.components === false,
    timeoutMilliseconds,
  );
  await activateButton("+ UIKit");
  checkpoints.treeExpanded = await pollEvaluation(contract,
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
  checkpoints.toastVisible = await pollEvaluation(contract,
    `({
      message: document.querySelector('[role="status"][aria-label="Release ready"]')?.textContent ?? "",
      region: document.querySelector('[role="region"][aria-label="Notifications"]') !== null,
    })`,
    (value) => value?.message.includes("artifacts passed verification") && value?.region === true,
    timeoutMilliseconds,
  );
  await activateButton("Dismiss");
  checkpoints.toastDismissed = await pollEvaluation(contract,
    `document.querySelector('[role="status"][aria-label="Release ready"]') === null`,
    (value) => value === true,
    timeoutMilliseconds,
  );

  await activateButton("Open drawer");
  await pollEvaluation(contract,
    `document.querySelector('[role="dialog"][aria-label="Release inspector"]') !== null`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await pressKey("Escape", "Escape", 27);
  checkpoints.drawer = await pollEvaluation(contract,
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
  checkpoints.environment = await pollEvaluation(contract,
    `({
      checked: Array.from(document.querySelectorAll('[role="switch"]'), (control) =>
        control instanceof HTMLInputElement ? control.checked : null),
      direction: document.querySelector('[dir="rtl"]')?.getAttribute("dir") ?? "",
      avatarOrder: (() => {
        const ada = document.querySelector('[role="img"][aria-label="Ada Lovelace"]')?.getBoundingClientRect();
        const grace = document.querySelector('[role="img"][aria-label="Grace Hopper"]')?.getBoundingClientRect();
        return ada !== undefined && grace !== undefined && ada.left > grace.left;
      })(),
      visual: (() => {
        const input = document.querySelector('input[aria-label="Name"]');
        const row = document.querySelector('[role="row"][aria-label="compiler"]');
        const cell = row?.querySelector('[role="gridcell"]');
        const rtlSwitch = document.querySelector('[role="switch"][aria-label="Right-to-left"]');
        if (!(input instanceof HTMLElement) || !(row instanceof HTMLElement)
          || !(cell instanceof HTMLElement) || !(rtlSwitch instanceof HTMLElement)) return null;
        const inputStyle = getComputedStyle(input);
        const rowStyle = getComputedStyle(row);
        const cellStyle = getComputedStyle(cell);
        return {
          input: [inputStyle.color, inputStyle.backgroundColor],
          row: [cellStyle.color, rowStyle.backgroundColor],
          switchTransform: getComputedStyle(rtlSwitch, "::after").transform,
        };
      })(),
      caption: document.body.textContent ?? "",
    })`,
    (value) => Array.isArray(value?.checked)
      && value.checked.slice(0, 4).every((item) => item === true)
      && value?.direction === "rtl"
      && value?.avatarOrder === true
      && Array.isArray(value?.visual?.input) && value.visual.input[0] !== value.visual.input[1]
      && Array.isArray(value?.visual?.row) && value.visual.row[0] !== value.visual.row[1]
      && typeof value?.visual?.switchTransform === "string"
      && value.visual.switchTransform !== "none"
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
  checkpoints.environmentRestored = await pollEvaluation(contract,
    `({
      direction: document.querySelector('[dir="ltr"]')?.getAttribute("dir") ?? "",
      caption: document.body.textContent ?? "",
    })`,
    (value) => value?.direction === "ltr" && value?.caption.includes("direction LTR")
      && !value?.caption.includes("direction RTL"),
    timeoutMilliseconds,
  );

  await contract.viewport({
    width: 390, height: 844, deviceScaleFactor: 1, mobile: false,
  });
  checkpoints.responsive = await pollEvaluation(contract,
    `(() => {
      const actions = document.querySelector('[role="group"][aria-label="Overlay actions"]');
      const bounds = actions === null ? []
        : Array.from(actions.querySelectorAll('button'), (button) => button.getBoundingClientRect());
      const grid = document.querySelector('[role="grid"][aria-label="Framework packages"]');
      const scroller = grid?.parentElement;
      return {
        viewport: innerWidth,
        documentWidth: document.documentElement.scrollWidth,
        actionColumns: new Set(bounds.map((rect) => Math.round(rect.x))).size,
        fullWidthActions: bounds.length > 0
          && bounds.every((rect) => rect.width >= 300 && rect.right <= innerWidth),
        grid: scroller instanceof HTMLElement ? {
          client: scroller.clientWidth,
          scroll: scroller.scrollWidth,
          overflow: getComputedStyle(scroller).overflow,
        } : null,
      };
    })()`,
    (value) => value?.viewport === 390
      && value?.documentWidth > 0 && value.documentWidth <= value.viewport
      && value?.actionColumns === 1 && value?.fullWidthActions === true
      && value?.grid?.client > 0 && value?.grid?.scroll > value.grid.client
      && value?.grid?.overflow === "auto",
    timeoutMilliseconds,
  );
  await contract.resetViewport({});
  await pollEvaluation(contract,
    `innerWidth > 390`,
    (value) => value === true,
    timeoutMilliseconds,
  );

  await activateButton("Open commands");
  await pollEvaluation(contract,
    `document.querySelector('[role="combobox"][aria-label="Command palette query"]') instanceof HTMLInputElement`,
    (value) => value === true,
    timeoutMilliseconds,
  );
  await contract.page.getByRole('combobox', { name: 'Command palette query', exact: true }).fill('publish');
  checkpoints.commandFiltered = await pollEvaluation(contract,
    `Array.from(document.querySelectorAll(
      '[role="listbox"][aria-label="Command palette results"] [role="option"]',
    ), (option) => (option.textContent ?? "").trim())`,
    (value) => Array.isArray(value) && value.length === 1 && value[0].includes("Publish release"),
    timeoutMilliseconds,
  );
  await focusElement('[role="combobox"][aria-label="Command palette query"]');
  await pressKey("Enter", "Enter", 13);
  checkpoints.command = await pollEvaluation(contract,
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

  await activateButton('Open commands');
  await contract.page.getByRole('option', { name: 'Run project', exact: true }).click();
  checkpoints.commandPointer = await pollEvaluation(contract,
    `({ closed: !document.querySelector('[role="dialog"][aria-label="Command palette"]'),
      status: Array.from(document.querySelectorAll('[role="status"]'), node => node.textContent ?? '').find(text => text.includes('Command executed')) ?? '' })`,
    value => value?.closed === true && value.status.includes('run.start'), timeoutMilliseconds);

  await activateButton('Open commands');
  await contract.page.getByRole('option', { name: 'Open file', exact: true }).focus();
  await pressKey('Escape', 'Escape', 27);
  checkpoints.commandRowEscape = await pollEvaluation(contract,
    `document.querySelector('[role="dialog"][aria-label="Command palette"]') === null`,
    value => value === true, timeoutMilliseconds);

  await activateButton('Open commands');
  await contract.page.getByRole('combobox', { name: 'Command palette query', exact: true }).press('Escape');
  checkpoints.commandInputEscape = await pollEvaluation(contract,
    `document.querySelector('[role="dialog"][aria-label="Command palette"]') === null`,
    value => value === true, timeoutMilliseconds);

  return { complete: true, passed: true, checkpoints };
}
