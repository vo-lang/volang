import type { Backend } from '../backend/backend';
import type { BootstrapContext, LaunchSpec, PreparedSession, SessionInfo } from '../types';
import type { WorkspaceService } from './workspace_service';

const BUILTIN_EXAMPLE_ID_PATTERN = /^[a-z0-9]+(?:[-_][a-z0-9]+)*$/;
const BUILTIN_EXAMPLE_ENTRY_PATTERN = /^[a-z0-9]+(?:[-_][a-z0-9]+)*\.vo$/;
const WINDOWS_RESERVED_PATH_STEM_PATTERN = /^(?:con|prn|aux|nul|com[1-9]|lpt[1-9])$/;
const MAX_BUILTIN_EXAMPLE_ID_LENGTH = 64;
const MAX_BUILTIN_EXAMPLE_ENTRY_LENGTH = 128;

export interface BuiltinExampleSessionSpec {
  readonly id: string;
  readonly entryName: string;
  readonly content: string;
}

function assertBuiltinExampleSegment(
  value: unknown,
  label: string,
  maxLength: number,
  pattern: RegExp,
): string {
  if (typeof value !== 'string') {
    throw new Error(`Invalid built-in example ${label}`);
  }
  const stem = value.endsWith('.vo') ? value.slice(0, -3) : value;
  if (
    value.length === 0
    || value.length > maxLength
    || !pattern.test(value)
    || WINDOWS_RESERVED_PATH_STEM_PATTERN.test(stem)
  ) {
    throw new Error(`Invalid built-in example ${label}`);
  }
  return value;
}

function appendPath(root: string, suffix: string): string {
  const normalizedRoot = root.replace(/[\\/]+$/u, '');
  return normalizedRoot ? `${normalizedRoot}/${suffix}` : `/${suffix}`;
}

function normalizeContractPath(path: string): string {
  let normalized = path.replace(/\\/gu, '/');
  if (normalized.startsWith('//?/UNC/')) {
    normalized = `//${normalized.slice('//?/UNC/'.length)}`;
  } else if (normalized.startsWith('//?/')) {
    normalized = normalized.slice('//?/'.length);
  }
  normalized = normalized.replace(/\/+$/u, '');
  if (/^[a-z]:\//iu.test(normalized)) {
    normalized = normalized.toLowerCase();
  }
  return normalized || '/';
}

function assertBuiltinExampleSession(
  session: SessionInfo,
  expectedRoot: string,
  expectedEntryPath: string,
): void {
  const normalizedRoot = normalizeContractPath(session.root);
  const normalizedEntry = session.entryPath === null
    ? null
    : normalizeContractPath(session.entryPath);
  const normalizedExpectedRoot = normalizeContractPath(expectedRoot);
  const normalizedExpectedEntry = normalizeContractPath(expectedEntryPath);
  const expectedSuffixOffset = normalizedExpectedRoot.lastIndexOf('/.volang/apps/studio/sessions/examples/');
  const expectedSuffix = expectedSuffixOffset >= 0
    ? normalizedExpectedRoot.slice(expectedSuffixOffset)
    : normalizedExpectedRoot;
  const entryName = normalizedExpectedEntry.slice(normalizedExpectedRoot.length + 1);
  const rootMatches = normalizedRoot === normalizedExpectedRoot
    || normalizedRoot.endsWith(expectedSuffix);
  if (
    !rootMatches
    || normalizedEntry !== `${normalizedRoot}/${entryName}`
    || session.origin !== 'run-target'
    || session.projectMode !== 'single-file'
    || !session.singleFileRun
    || session.workspaceDiscovery !== 'disabled'
  ) {
    throw new Error('Backend returned an invalid built-in example session');
  }
}

function sameSessionInfo(actual: SessionInfo, expected: SessionInfo): boolean {
  return normalizeContractPath(actual.root) === normalizeContractPath(expected.root)
    && (actual.entryPath === null
      ? expected.entryPath === null
      : expected.entryPath !== null
        && normalizeContractPath(actual.entryPath) === normalizeContractPath(expected.entryPath))
    && actual.origin === expected.origin
    && actual.projectMode === expected.projectMode
    && actual.singleFileRun === expected.singleFileRun
    && actual.workspaceDiscovery === expected.workspaceDiscovery
    && sameSessionSource(actual.source, expected.source)
    && actual.share?.canonicalUrl === expected.share?.canonicalUrl
    && actual.share?.shareable === expected.share?.shareable
    && actual.share?.reason === expected.share?.reason;
}

function sameSessionSource(actual: SessionInfo['source'], expected: SessionInfo['source']): boolean {
  if (actual?.kind !== expected?.kind) return false;
  if (!actual || !expected) return actual === expected;
  if (actual.kind === 'workspace' && expected.kind === 'workspace') return true;
  if (actual.kind === 'path' && expected.kind === 'path') {
    return normalizeContractPath(actual.path) === normalizeContractPath(expected.path);
  }
  if (actual.kind === 'github_repo' && expected.kind === 'github_repo') {
    return actual.owner === expected.owner
      && actual.repo === expected.repo
      && actual.requestedRef === expected.requestedRef
      && actual.resolvedCommit === expected.resolvedCommit
      && actual.subdir === expected.subdir
      && actual.htmlUrl === expected.htmlUrl
      && normalizeContractPath(actual.sourceCacheRoot) === normalizeContractPath(expected.sourceCacheRoot);
  }
  return false;
}

export class ProjectService {
  private bootstrap: BootstrapContext | null = null;
  private session: SessionInfo | null = null;

  constructor(
    private readonly backend: Backend,
    private readonly workspace: WorkspaceService,
  ) {}

  get bootstrapContext(): BootstrapContext {
    if (!this.bootstrap) {
      throw new Error('Bootstrap context has not been loaded');
    }
    return this.bootstrap;
  }

  get sessionInfo(): SessionInfo | null {
    return this.session;
  }

  async initialize(): Promise<BootstrapContext> {
    this.bootstrap = await this.backend.getBootstrapContext();
    return this.bootstrap;
  }

  async openSession(spec: LaunchSpec): Promise<SessionInfo> {
    const candidate = await this.prepareSession(spec);
    try {
      return await this.activatePreparedSession(candidate);
    } catch (error) {
      await this.backend.discardPreparedSession(candidate).catch(() => undefined);
      throw error;
    }
  }

  async openBuiltinExampleSession(spec: BuiltinExampleSessionSpec): Promise<SessionInfo> {
    const candidate = await this.prepareBuiltinExampleSession(spec);
    try {
      return await this.activatePreparedSession(candidate);
    } catch (error) {
      await this.backend.discardPreparedSession(candidate).catch(() => undefined);
      throw error;
    }
  }

  async prepareSession(spec: LaunchSpec): Promise<PreparedSession> {
    return this.backend.prepareSession(spec);
  }

  async prepareBuiltinExampleSession(spec: BuiltinExampleSessionSpec): Promise<PreparedSession> {
    if (!spec || typeof spec !== 'object') {
      throw new Error('Invalid built-in example session specification');
    }
    const id = assertBuiltinExampleSegment(
      spec.id,
      'id',
      MAX_BUILTIN_EXAMPLE_ID_LENGTH,
      BUILTIN_EXAMPLE_ID_PATTERN,
    );
    const entryName = assertBuiltinExampleSegment(
      spec.entryName,
      'entry name',
      MAX_BUILTIN_EXAMPLE_ENTRY_LENGTH,
      BUILTIN_EXAMPLE_ENTRY_PATTERN,
    );
    if (typeof spec.content !== 'string') {
      throw new Error('Invalid built-in example content');
    }

    const examplesRoot = appendPath(
      this.bootstrapContext.workspaceRoot,
      '.volang/apps/studio/sessions/examples',
    );
    const exampleRoot = `${examplesRoot}/${id}`;
    const entryPath = `${exampleRoot}/${entryName}`;
    await this.backend.createWorkspaceFiles([{ path: entryPath, content: spec.content }]);
    const candidate = await this.backend.prepareSession({
      proj: entryPath,
      mode: 'dev',
      isolation: 'single-file',
    });
    try {
      assertBuiltinExampleSession(candidate.session, exampleRoot, entryPath);
      return candidate;
    } catch (error) {
      await this.backend.discardPreparedSession(candidate).catch(() => undefined);
      throw error;
    }
  }

  async activatePreparedSession(candidate: PreparedSession): Promise<SessionInfo> {
    const previous = this.session;
    const session = await this.backend.activateSession(candidate);
    if (!sameSessionInfo(session, candidate.session)) {
      if (previous) {
        const restored = await this.backend.restoreSession(previous);
        this.bindSession(restored);
      } else {
        this.bindSession(session);
      }
      throw new Error('Backend activated a different session than it prepared');
    }
    this.bindSession(session);
    return session;
  }

  async restoreSession(previous: SessionInfo): Promise<SessionInfo> {
    const restored = await this.backend.restoreSession(previous);
    if (!sameSessionInfo(restored, previous)) {
      this.bindSession(restored);
      throw new Error('Backend restored a different session than requested');
    }
    this.bindSession(restored);
    return restored;
  }

  async discardPreparedSession(candidate: PreparedSession): Promise<void> {
    await this.backend.discardPreparedSession(candidate);
  }

  async listPreparedSessionDir(candidate: PreparedSession, path: string) {
    return this.backend.listPreparedSessionDir(candidate, path);
  }

  async readPreparedSessionFile(candidate: PreparedSession, path: string): Promise<string> {
    return this.backend.readPreparedSessionFile(candidate, path);
  }

  private bindSession(session: SessionInfo): void {
    this.session = session;
    this.workspace.bindSession(session);
  }
}
