# Testing and troubleshooting

## Testing layers

Start with the smallest layer that proves the behavior:

1. pure `.vo` model and package tests;
2. `vo ui test` semantic interactions on the headless renderer;
3. VM/JIT/AOT differential tests;
4. DOM, accessibility, paint and protocol conformance;
5. real browser and native-window tests;
6. packaged Web and desktop startup.

`ui/testing` provides a deterministic clock, bounded event recorder,
cancellation-aware eventual assertions and named fault countdowns.
`ui/observability` provides structured bounded records, spans, metrics,
sensitive-value redaction and callback recovery. Always assert drop counts so
telemetry pressure remains visible.

## Fast diagnosis

```sh
vo ui doctor . --format=json
vo ui inspect . --format=json --target=web
vo ui test . --mode=jit --profile
vo mod verify
```

| Symptom | Check |
| --- | --- |
| package or lock mismatch | `vo mod verify`, then `vo ui doctor` |
| browser page reports an authority error | inspect `authority_packages` for the Web target |
| stale result updates the screen | verify task context and component/input generation |
| visual update is missing | inspect changed state writes and submitted direct slots |
| renderer rejects a frame | run protocol and renderer conformance for that target |
| packaged app does not start | verify target, runtime receipt, signing policy and offline startup |
| memory or jank regression | run the optimized benchmark and compare p95/p99 budgets |

The development server keeps the last valid generation after a failed compile
and displays diagnostics without replacing the mounted application. A host
commit is atomic; DOM/native projection failure restores the previous revision.

## Reporting a defect

Include `vo --version`, target triple, execution mode, `vo ui doctor --format=json`,
the smallest `.vo` reproduction and whether the failure appears in VM, JIT,
Wasm AOT or Native AOT. Remove secrets from application logs even though the
official observability API redacts fields marked sensitive.
