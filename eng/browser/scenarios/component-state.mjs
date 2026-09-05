import { pollEvaluation } from '../page-contract.mjs';

export async function runComponentStateSmoke(contract, timeoutMilliseconds, projectRoot) {
  const buttonTexts = `Array.from(document.querySelectorAll("#volang-root button"), (button) =>
    (button.textContent ?? "").trim())`;
  const expectButtons = async (expected) => {
    const observed = await pollEvaluation(contract,
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
  const clickButton = async text => contract.clickButton(text);
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
