import { isAbsolute, relative, sep } from "node:path";

/**
 * Return the canonical repository-relative virtual filename passed into the
 * WASM compiler. Compiler source roots never contain absolute paths, `.` or
 * `..` components, regardless of the test runner's current working directory.
 */
export function repositorySourcePath(repositoryRoot, fullPath) {
  const candidate = relative(repositoryRoot, fullPath);
  if (
    candidate.length === 0 ||
    candidate === ".." ||
    candidate.startsWith(`..${sep}`) ||
    isAbsolute(candidate)
  ) {
    throw new Error(`test source is outside the repository: ${fullPath}`);
  }

  const components = candidate.split(sep);
  if (
    components.some(
      (component) =>
        component.length === 0 || component === "." || component === "..",
    )
  ) {
    throw new Error(`test source path is not normalized: ${fullPath}`);
  }
  return components.join("/");
}
