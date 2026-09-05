import { pollEvaluation } from '../page-contract.mjs';

export async function runDataApplicationSmoke(contract, timeoutMilliseconds, projectRoot) {
  const evaluate = async (expression) => {
    const evaluated = await contract.evaluate(expression);

    return evaluated ?? null;
  };
  const activate = async (selector, name) => contract.activate(selector, name);
  const setInput = async (selector, value) => contract.fill(selector, value);
  const checkpoints = {};
  checkpoints.initial = await pollEvaluation(contract,
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
  checkpoints.selection = await pollEvaluation(contract,
    `({
      selected: document.querySelector('[role="row"][aria-label="member-0"]')
        ?.getAttribute("aria-selected") ?? "",
      text: document.body.textContent ?? "",
    })`,
    (value) => value?.selected === "true" && value?.text.includes("member-0"),
    timeoutMilliseconds,
  );
  await activate("button", "Archive selected");
  checkpoints.optimistic = await pollEvaluation(contract,
    `document.body.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("Archiving member-0"),
    timeoutMilliseconds,
  );
  await activate("button", "Commit optimistic change");
  checkpoints.committed = await pollEvaluation(contract,
    `document.body.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("Archived member-0"),
    timeoutMilliseconds,
  );

  await activate('[role="navigation"][aria-label="Member pages"] button', "2");
  checkpoints.page = await pollEvaluation(contract,
    `({
      current: document.querySelector('[role="navigation"][aria-label="Member pages"] [aria-current="page"]')
        ?.textContent ?? "",
      firstRow: document.querySelector('[role="rowheader"][aria-label="Member 1000"]') !== null,
    })`,
    (value) => value?.current.trim() === "2" && value?.firstRow === true,
    timeoutMilliseconds,
  );

  await setInput('input[aria-label="Filter members"]', "active");
  checkpoints.filter = await pollEvaluation(contract,
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
  checkpoints.sort = await pollEvaluation(contract,
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
  checkpoints.offline = await pollEvaluation(contract,
    `document.body.textContent ?? ""`,
    (value) => typeof value === "string" && value.includes("offline cache"),
    timeoutMilliseconds,
  );

  await activate("button", "Open commands");
  await setInput('input[aria-label="Command palette query"]', "commit");
  checkpoints.commands = await pollEvaluation(contract,
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
  checkpoints.commandActivated = await pollEvaluation(contract,
    `document.querySelector('[role="dialog"][aria-label="Command palette"]') === null`,
    (value) => value === true,
    timeoutMilliseconds,
  );

  await activate('[role="link"]', "Settings");
  checkpoints.settings = await pollEvaluation(contract,
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
  checkpoints.validation = await pollEvaluation(contract,
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
  checkpoints.submission = await pollEvaluation(contract,
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
  checkpoints.restored = await pollEvaluation(contract,
    `({ path: location.pathname, text: document.body.textContent ?? "" })`,
    (value) => value?.path === "/" && value?.text.includes("Operations dashboard"),
    timeoutMilliseconds,
  );

  return { complete: true, passed: true, checkpoints };
}
