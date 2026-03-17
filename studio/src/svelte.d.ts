declare module '*.svelte' {
  import type { ComponentConstructorOptions, SvelteComponentTyped } from 'svelte';
  export default class Component extends SvelteComponentTyped<Record<string, unknown>, Record<string, unknown>, Record<string, unknown>> {
    constructor(options: ComponentConstructorOptions<Record<string, unknown>>);
  }
}
