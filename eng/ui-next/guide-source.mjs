// The complete application in the maintained guide is the public test fixture.
// Keep extraction shared with evidence validation so a second copy cannot drift.
export function guideApplication(markdown) {
  const programs = [...markdown.matchAll(/^```vo\r?\n([\s\S]*?)^```\s*$/gm)];
  if (programs.length !== 1) throw new Error('The guide must contain one complete Vo application.');
  return programs[0][1];
}
