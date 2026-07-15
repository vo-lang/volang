export const STUDIO_ROOT: string;
export const QUICKPLAY_PACKAGE_ROOT: string;
export const QUICKPLAY_PACKAGE_BASE_FILES: readonly string[];

export function quickplayPackageFiles(options?: { studioRoot?: string }): string[];

export function readQuickplayPackageBuildId(
  options?: { studioRoot?: string },
): string | null;

export function resolveStudioBuildId(
  env?: Record<string, string | undefined>,
  options?: { studioRoot?: string },
): string;

export function validateStudioWasmBuildId(
  value: string,
  env?: Record<string, string | undefined>,
  options?: { studioRoot?: string },
): string;
