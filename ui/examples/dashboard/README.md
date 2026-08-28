# Production dashboard example

The dashboard demonstrates a conventional production shell with a banner,
navigation region, responsive named grid areas, accessible landmarks, cards,
and renderer-neutral design tokens. The same `main.vo` compiles for every
execution backend.

Development commands from the repository root:

```sh
vo ui run ui/examples/dashboard --mode=vm
vo ui run ui/examples/dashboard --mode=jit
vo ui dev ui/examples/dashboard
```

Web release:

```sh
vo ui build ui/examples/dashboard -o dist/dashboard-web
```

Native release archives include `libvo_ui_aot_runtime_native.a`. Link the
application with the runtime matching the release target:

```sh
vo build ui/examples/dashboard \
  --runtime=/path/to/libvo_ui_aot_runtime_native.a \
  -o dashboard
```

The generated Web tree contains the Core Wasm AOT image and its version-matched
browser runtime. The native executable contains AOT application code and enters
the retained WGPU shell directly.
