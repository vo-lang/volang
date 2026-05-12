# Web, WASM, Studio, Playground, And App Runtime

## Contents

- [vo-web](#vo-web)
- [WASM VFS, Registry, And Runtime Plans](#wasm-vfs-registry-and-runtime-plans)
- [vo-app-runtime](#vo-app-runtime)
- [Studio Web And Native](#studio-web-and-native)
- [Playground](#playground)
- [Verification](#verification)
- [Caveats](#caveats)

## vo-web

`lang/crates/vo-web` exposes browser/WASM compile and run bindings plus generic VM management.

Important files:

Paths in this list are relative to `lang/crates/vo-web`.

- `src/lib.rs`: public exports and module structure.
- `src/compile.rs`: browser compile pipeline.
- `src/vm.rs`: VM creation, extern registration, sync run APIs.
- `src/async_runner.rs`: async VM run loop.
- `src/js_types.rs`: `CompileResult`, `RunResult`.
- `src/browser_registry.rs`: browser dependency registry.
- `src/wasm_vfs.rs`: Rust `FileSystem` over JS VFS.
- `src/browser_runtime.rs`: browser runtime graph/plan/snapshot/artifact models.
- `src/browser_runtime_dev.rs`: native dev planning/materialization.
- `runtime-wasm/src/*`: browser host APIs used by stdlib and ext bridge.
- `js/vfs.ts`: npm/browser VirtualFS helper.

Compile flow:

1. Build stdlib memory FS from embedded stdlib.
2. Prepare single-file or entry project input.
3. Enforce web policy, including limits on ad hoc external imports.
4. Load project context and ready module information.
5. Analyze with `vo-analysis`.
6. Compile with `vo-codegen`.
7. Return bytecode and diagnostics through wasm-bindgen types.

Run flow:

1. `create_vm` registers `vo_stdlib` and `vo_web_runtime_wasm` externs.
2. `create_loaded_vm` deserializes/loads bytecode.
3. Sync run APIs call VM directly.
4. Async run APIs handle suspended host events, fetch promises, timers, and wakeups.

## WASM VFS, Registry, And Runtime Plans

There are multiple VFS layers:

- `lang/crates/vo-web/js/vfs.ts`: JS `VirtualFS`, memory plus OPFS, installs `window._vfs*` bindings.
- `lang/crates/vo-web/runtime-wasm/src/vfs.rs`: wasm-bindgen imports for those globals.
- `lang/crates/vo-web/src/wasm_vfs.rs`: Rust `WasmVfs` implementing `FileSystem` and install surfaces.
- `apps/studio/src/lib/window_vfs_bindings.ts`: Studio-specific binding installer.

Browser dependency registry:

- `BrowserRegistry` implements async registry behavior.
- It prefers packaged VFS `vo.web.json` when available.
- It can fetch GitHub raw/API data when explicitly preparing dependencies.
- It validates module/version/commit, size, and sha256.
- `apps/studio/public/quickplay/blockkart/provenance.json` records the checked-in quickplay generator command, declared inputs, BlockKart commit, dependency versions, and output digests. `scripts/ci/quickplay_validate.mjs` rejects missing or stale provenance.

Browser runtime planning:

- `BrowserRuntimeContract`
- `BrowserRuntimeModule`
- `BrowserRuntimeGraph`
- `BrowserRuntimePlan`
- `BrowserWasmExtensionSpec`
- `BrowserSnapshotPlan`
- `BrowserArtifactIntent`

These map extension manifests and ready modules to renderer/protocol/host_bridge modules, WASM extension assets, snapshot mounts, and artifact requirements.

Separate preparation from compilation in docs. Studio Web prepares dependencies/runtime assets before compiling; do not imply every compile silently networks.

## vo-app-runtime

`lang/crates/vo-app-runtime` is shared app/session orchestration for Vo GUI hosts.

Important files:

Paths in this list are relative to `lang/crates/vo-app-runtime`.

- `src/lib.rs`: public exports.
- `src/app_session.rs`: `AppSession`, core VM session wrapper.
- `src/guest_runtime_session.rs`: `GuestRuntime`.
- `src/gui_session.rs`: `GuiAppSession`, initial render requirements.
- `src/render_island_session.rs`: render island VM session.
- `src/effects.rs`: `StepResult`.
- `src/mailbox.rs`: replay wait token, host events, outbound island frames.
- `src/session.rs`: low-level event resume and island frame helpers.
- `src/protocol.rs`: special host handler IDs.
- `src/render_buffer.rs`: keep-latest render output.
- `src/native_event_loop.rs`: native GUI VM thread/event loop.

`AppSession` owns:

- `Vm`
- `SessionMailbox`
- pending host events
- outbound island frames
- stdout source

Every `run*` method clears prior output, runs the VM, advances session state, collects effects, and returns `StepResult { outcome, render_output, stdout }`.

`RenderBuffer` keeps latest output only. It is not a replay queue.

Protocol constants include:

- `TIMER = -1`
- `ANIM_FRAME = -4`
- `GAME_LOOP = -5`

## Studio Web And Native

Studio is under `apps/studio`.

Frontend:

- `apps/studio/src/App.svelte`
- `apps/studio/src/components/*`
- `apps/studio/src/lib/backend/backend.ts`: backend interface.
- `apps/studio/src/lib/backend/web_backend.ts`: browser backend, in-memory workspace, VFS, host events.
- `apps/studio/src/lib/backend/native_backend.ts`: Tauri invoke backend.
- `apps/studio/src/lib/services/service_registry.ts`: chooses native or web backend.
- `apps/studio/src/lib/services/runtime_service.ts`: serializes GUI runtime operations and updates stores.
- `apps/studio/src/lib/gui/gui_pipeline.ts`: GUI preparation/run sequence.
- `apps/studio/src/lib/gui/renderer_bridge.ts`: loads renderer/protocol/host_bridge JS from VFS snapshot.
- `apps/studio/src/lib/studio_wasm.ts`: loader and JS-side WASM/extension bridge.

Studio WASM:

- `apps/studio/wasm/src/lib.rs`: exports `prepareEntry`, `compileRunEntry`, `compileGui`, `startGuiFromBytecode`, `sendGuiEvent`, `pollPendingHostEvent`, `wakeHostEvent`, `pollGuiRender`, etc.
- `apps/studio/scripts/build_wasm.mjs`: builds wasm-pack output.
- `apps/studio/public/wasm`: generated/public WASM assets.

Tauri:

- `apps/studio/src-tauri/src/lib.rs`: command registration.
- `apps/studio/src-tauri/src/commands/gui.rs`: native GUI compile/runtime commands.
- `apps/studio/src-tauri/src/gui_runtime.rs`: native runtime state.

Studio Web GUI path:

1. `WebBackend.runGui`
2. `wasm.prepareEntry`
3. `wasm.compileGui`
4. `executeGuiFromCompileOutput`
5. `wasm.startGuiFromBytecode`
6. frontend renderer bridge consumes render bytes and host bridge effects

Host bridge must be ready before initial render. First-frame measurement/scroll/focus behavior depends on this ordering.

## Playground

Playground is separate from Studio.

Important files:

- `apps/playground-legacy/src/pages/*`
- `apps/playground-legacy/src/components/*`
- `apps/playground-legacy/src/wasm/vo.ts`
- `apps/playground-legacy/rust/src/lib.rs`
- `apps/playground-legacy/src/assets/examples`
- `apps/playground-legacy/src/assets/docs`

Do not collapse Playground and Studio into one runtime. Playground GUI still has its own `GuiAppState` / event wait token path rather than fully using `vo-app-runtime`.

## Verification

Choose based on touched code:

- `vo-web` Rust compile path: `cargo check -p vo-web --target wasm32-unknown-unknown`
- Web runtime tests: `./d.py test wasm`
- Studio WASM build: `./d.py ci task studio-wasm-build` or `./d.py studio --build-only`
- Focused Studio frontend build: `./d.py ci task studio-build`
- Studio WASM-only helper: `./d.py studio --build-only`
- Full Studio/site task graph: `./d.py ci site` or `./d.py ci task studio-build`
- Direct `cd apps/studio && npm run build` is acceptable for local Vite/Svelte debugging, but final verification should use the task graph.
- Studio web interactive run: `./d.py studio`
- Studio native interactive run: `./d.py studio-native`
- Tauri Rust changes: check `apps/studio/src-tauri` plus workspace if command surfaces changed.

Studio local Vite port is commonly `5174` via repo scripts.

## Caveats

- `vo-web`, `apps/studio/wasm`, and `apps/playground-legacy/rust` are not identical runtime surfaces.
- Browser registry may network during explicit preparation, but frozen compile/run semantics still matter.
- Web single-file external imports are limited. Studio `prepareEntry` is the main browser dependency preparation path.
- Renderer bridge snapshot paths differ in Web and Native; use helper code rather than hard-coded path rules.
- `DISPLAY_PULSE_DELAY_MS = u32::MAX` is a special rAF pulse sentinel, not a normal long timer.
- Event payload casing has historical differences across host bridges. Cite the concrete bridge before documenting payload schema.
