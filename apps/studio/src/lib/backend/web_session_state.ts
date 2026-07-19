import type { PreparedSession, SessionInfo, WorkspaceDiscoveryMode } from '../types';

interface SessionLease {
  session: SessionInfo;
  ownedRoot: string | null;
}

interface PendingSession extends SessionLease {
  candidate: PreparedSession;
}

export interface PreparedWebSession {
  candidate: PreparedSession;
  retiredRoots: string[];
}

export interface WebSessionTransition {
  session: SessionInfo;
  retiredRoots: string[];
}

export class WebSessionState {
  private active: SessionLease | null = null;
  private pending: PendingSession | null = null;
  private rollback: SessionLease | null = null;
  private nextToken = 0;

  prepare(session: SessionInfo, ownedRoot: string | null = null): PreparedWebSession {
    const retiredRoots = uniqueRoots([
      this.pending?.ownedRoot ?? null,
      this.rollback?.ownedRoot ?? null,
    ]);
    const candidate: PreparedSession = {
      token: `web-session-${++this.nextToken}`,
      session,
    };
    this.pending = { candidate, session, ownedRoot };
    this.rollback = null;
    return { candidate, retiredRoots };
  }

  activate(candidate: PreparedSession): WebSessionTransition {
    const pending = this.requirePending(candidate);
    this.pending = null;
    this.rollback = this.active;
    this.active = { session: pending.session, ownedRoot: pending.ownedRoot };
    return { session: pending.session, retiredRoots: [] };
  }

  restore(previous: SessionInfo): WebSessionTransition {
    if (!this.rollback || !sameValue(this.rollback.session, previous)) {
      throw new Error('Session does not match the rollback candidate');
    }
    const retiredRoots = uniqueRoots([this.active?.ownedRoot ?? null]);
    this.active = this.rollback;
    this.rollback = null;
    this.pending = null;
    return { session: this.active.session, retiredRoots };
  }

  discard(candidate: PreparedSession): string[] {
    const pending = this.requirePending(candidate);
    this.pending = null;
    return uniqueRoots([pending.ownedRoot]);
  }

  pendingRoot(candidate: PreparedSession): string {
    return this.requirePending(candidate).session.root;
  }

  workspaceDiscoveryForPath(path: string): WorkspaceDiscoveryMode {
    if (!this.active) {
      return 'auto';
    }
    const root = this.active.session.root;
    return path === root || path.startsWith(`${root}/`)
      ? this.active.session.workspaceDiscovery
      : 'auto';
  }

  private requirePending(candidate: PreparedSession): PendingSession {
    if (!this.pending) {
      throw new Error('Prepared session is no longer available');
    }
    if (!sameValue(this.pending.candidate, candidate)) {
      throw new Error('Prepared session does not match the pending candidate');
    }
    return this.pending;
  }
}

function uniqueRoots(roots: Array<string | null>): string[] {
  return [...new Set(roots.filter((root): root is string => root != null))];
}

function sameValue(left: unknown, right: unknown): boolean {
  return stableJson(left) === stableJson(right);
}

function stableJson(value: unknown): string {
  if (value === null || typeof value !== 'object') {
    return JSON.stringify(value) ?? 'undefined';
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(',')}]`;
  }
  const record = value as Record<string, unknown>;
  return `{${Object.keys(record)
    .sort()
    .map((key) => `${JSON.stringify(key)}:${stableJson(record[key])}`)
    .join(',')}}`;
}
