// Ref registry: maps Vo ref names to DOM elements.

export const refRegistry = new Map<string, HTMLElement>();

/** Get a DOM element by its Vo ref name. */
export function getRef(name: string): HTMLElement | undefined {
    return refRegistry.get(name);
}

/** Create a Preact ref callback that registers/unregisters in the refRegistry. */
export function refCallback(name: string): (el: HTMLElement | null) => void {
    return (el) => {
        if (el) {
            refRegistry.set(name, el);
        } else {
            refRegistry.delete(name);
        }
    };
}
