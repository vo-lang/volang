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
  private readonly tokens = new Map<number, GuiSessionToken>();
  private selectedToken: GuiSessionToken | null = null;

  get active(): GuiSessionToken | null {
    return this.selectedToken;
  }

  get size(): number {
    return this.tokens.size;
  }

  sessions(): readonly GuiSessionToken[] {
    return [...this.tokens.values()];
  }

  begin(): GuiSessionToken {
    const id = this.nextSessionId + 1;
    assertGuiSessionId(id);
    this.nextSessionId = id;
    const token = makeGuiSessionToken(id);
    this.tokens.set(id, token);
    this.selectedToken = token;
    return token;
  }

  invalidate(expected?: GuiSessionToken): GuiSessionToken | null {
    const target = expected ?? this.selectedToken;
    if (!target || this.tokens.get(target.id) !== target) {
      return null;
    }
    this.tokens.delete(target.id);
    if (this.selectedToken === target) {
      this.selectedToken = this.tokens.values().next().value ?? null;
    }
    return target;
  }

  isActive(token: GuiSessionToken): boolean {
    return this.tokens.get(token.id) === token;
  }

  select(token: GuiSessionToken): void {
    if (!this.isActive(token)) {
      throw new Error('GUI session token is not live');
    }
    this.selectedToken = token;
  }
}

/** Backend-side binding for the RuntimeService-owned token. */
export class GuiSessionBinding {
  private readonly tokens = new Map<number, GuiSessionToken>();
  private selectedToken: GuiSessionToken | null = null;

  get active(): GuiSessionToken | null {
    return this.selectedToken;
  }

  get size(): number {
    return this.tokens.size;
  }

  activate(token: GuiSessionToken): void {
    assertGuiSessionId(token.id);
    this.tokens.set(token.id, token);
    this.selectedToken = token;
  }

  clear(expected?: GuiSessionToken): GuiSessionToken | null {
    const target = expected ?? this.selectedToken;
    if (!target || this.tokens.get(target.id) !== target) {
      return null;
    }
    this.tokens.delete(target.id);
    if (this.selectedToken === target) {
      this.selectedToken = this.tokens.values().next().value ?? null;
    }
    return target;
  }

  isActive(token: GuiSessionToken): boolean {
    return this.tokens.get(token.id) === token;
  }

  isActiveId(sessionId: number): boolean {
    return this.tokens.has(sessionId);
  }

  get(sessionId: number): GuiSessionToken | null {
    return this.tokens.get(sessionId) ?? null;
  }

  select(token: GuiSessionToken): void {
    if (!this.isActive(token)) {
      throw new Error('GUI backend session token is not live');
    }
    this.selectedToken = token;
  }
}
