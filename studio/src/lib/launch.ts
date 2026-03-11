import { openWorkspaceTarget } from './actions/workspace';
import { runCode } from './actions/exec';
import { bridge } from './bridge';
import { importStudioLaunchTarget } from './launch_import';
import { parseStudioLaunchUrl } from './launch_protocol';

export async function executeStudioLaunch(launchUrl: string): Promise<boolean> {
  const request = parseStudioLaunchUrl(launchUrl, bridge().appWorkspaceRoot);
  if (!request) return false;

  const localTarget = await importStudioLaunchTarget(request.target);
  await openWorkspaceTarget(localTarget.targetPath, localTarget.entryPath);

  if (request.action === 'run') {
    await runCode();
  }
  return true;
}
