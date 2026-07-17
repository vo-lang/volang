declare const guiSessionTokenBrand: unique symbol;

/**
 * Identity allocated by RuntimeService for one GUI lifetime.
 *
 * The object identity is intentional: backend-local lifecycle state can reject
 * an asynchronous completion from an older run even when it carries otherwise
 * well-formed data.
 */
export type GuiSessionToken = Readonly<{
  id: number;
  [guiSessionTokenBrand]: true;
}>;

function assertGuiSessionId(id: number): void {
  if (!Number.isSafeInteger(id) || id < 1) {
    throw new Error('GUI session ID must be a positive safe integer');
  }
}

function makeGuiSessionToken(id: number): GuiSessionToken {
  assertGuiSessionId(id);
  return Object.freeze({ id }) as GuiSessionToken;
}

/** RuntimeService-owned source of GUI identities. */
export class GuiSessionAuthority {
  private nextSessionId = 0;
  private activeToken: GuiSessionToken | null = null;

  get active(): GuiSessionToken | null {
    return this.activeToken;
  }

  begin(): GuiSessionToken {
    const id = this.nextSessionId + 1;
    assertGuiSessionId(id);
    this.nextSessionId = id;
    const token = makeGuiSessionToken(id);
    this.activeToken = token;
    return token;
  }

  invalidate(expected?: GuiSessionToken): GuiSessionToken | null {
    if (expected && this.activeToken !== expected) {
      return null;
    }
    const invalidated = this.activeToken;
    this.activeToken = null;
    return invalidated;
  }

  isActive(token: GuiSessionToken): boolean {
    return this.activeToken === token;
  }
}

/** Backend-side binding for the RuntimeService-owned token. */
export class GuiSessionBinding {
  private activeToken: GuiSessionToken | null = null;

  get active(): GuiSessionToken | null {
    return this.activeToken;
  }

  activate(token: GuiSessionToken): void {
    assertGuiSessionId(token.id);
    this.activeToken = token;
  }

  clear(expected?: GuiSessionToken): GuiSessionToken | null {
    if (expected && this.activeToken !== expected) {
      return null;
    }
    const cleared = this.activeToken;
    this.activeToken = null;
    return cleared;
  }

  isActive(token: GuiSessionToken): boolean {
    return this.activeToken === token;
  }

  isActiveId(sessionId: number): boolean {
    return this.activeToken?.id === sessionId;
  }
}
