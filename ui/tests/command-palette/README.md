# Command palette contract probe

Exercises the official controlled command palette through focus, filtering,
keyboard selection, activation, empty results, disabled commands, and modal
dismissal. The same source is expected to pass VM, JIT, Web, and native hosts.

```sh
vo ui test ui/tests/command-palette --mode=vm --input="Command palette query=run" --wait-text="first=Run project" --key="Command palette query=Enter" --wait-text="executed=run.start"
vo ui test ui/tests/command-palette --mode=jit --input="Command palette query=run" --wait-text="first=Run project" --key="Command palette query=Enter" --wait-text="executed=run.start"
```
