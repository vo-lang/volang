# Nested component list probe

This permanent probe covers the first executable slice of Component Model V2:

- keyed instances of one imported component across dynamic branches;
- independent component-local scalar state and handler closures;
- insertion, removal, movement, replacement, and fresh-state semantics;
- incremental text/property updates after each event;
- canonical VUB1 component identities across VM, JIT, Native AOT, and Core
  Wasm AOT compilation.

The phase control reorders, removes, reinserts, and replaces instances. Movement
preserves Alpha's state, reinsertion creates fresh Beta state, and replacement
creates fresh Alpha state.

Manual development checks:

```sh
vo ui test ui/tests/nested-component-list --mode=vm --click="Alpha 0" --click=Reorder --click="Remove Beta" --click="Insert Beta" --click="Replace Alpha"
vo ui test ui/tests/nested-component-list --mode=jit --click="Alpha 0" --click=Reorder --click="Remove Beta" --click="Insert Beta" --click="Replace Alpha"
```
