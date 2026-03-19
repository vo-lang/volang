# Vo-Makepad Host Design

**Status:** Proposal  
**Date:** 2026-03-18  
**Author:** Cascade  

## 1. Goal

Embed the Vo VM as a scripting/application layer on top of the Makepad UI framework.
Users write Vo code; the host translates Vo-side declarative UI descriptions and event
handlers into Makepad widget operations. Makepad owns rendering, layout, GPU shaders,
and platform integration. Vo owns state, logic, and dynamic UI structure.

The deliverable is a new Rust crate **`vo-makepad-host`** (living under a new repo
`vomakepad/`) that:

1. Embeds a Vo `Vm` and drives its scheduler.
2. Implements a stable bridge protocol between Vo and Makepad widgets.
3. Exposes itself as a standard Makepad `AppMain` so the result is a normal Makepad
   binary — no separate process, no IPC.
4. Can optionally be integrated into Studio as an additional GUI backend.

---

## 2. Makepad Architecture Summary (from source audit)

### 2.1 App lifecycle

`app_main!` macro generates platform entry points. Each creates a `Cx` (the central
runtime context) with a boxed event handler closure:

```
Event::Startup  → App::new_main(cx)
Event::LiveEdit → app.update_main(cx)   // hot-reload DSL
*               → app.handle_event(cx, event)
```

`Cx::event_loop(cx)` runs the platform-specific event pump (Metal on macOS, DX11 on
Windows, WebGL on WASM, etc.). There is **no** way to run a Makepad app without owning
the event loop — `Cx` is `!Send`.

### 2.2 Widget trait

Every visual element implements `Widget`:

```rust
trait Widget: WidgetNode {
    fn handle_event(&mut self, cx: &mut Cx, event: &Event, scope: &mut Scope);
    fn draw_walk(&mut self, cx: &mut Cx2d, scope: &mut Scope, walk: Walk) -> DrawStep;
    fn text(&self) -> String;
    fn set_text(&mut self, cx: &mut Cx, v: &str);
    fn widget(&self, path: &[LiveId]) -> WidgetRef;
    fn data_to_widget(&mut self, cx: &mut Cx, nodes: &[LiveNode], path: &[LiveId]);
    fn widget_to_data(&self, cx: &mut Cx, actions: &Actions, nodes: &mut LiveNodeVec, path: &[LiveId]) -> bool;
    // ...
}
```

Key observations:
- Widgets are **retained** — created once, mutated in place.
- `WidgetRef` is `Rc<RefCell<Option<WidgetRefInner>>>` — single-threaded, not `Send`.
- The `Scope` carries `&mut dyn Any` data and `&dyn Any` props via type-erased downcasting.
- Actions (widget events) are collected in `Cx.new_actions` and dispatched via
  `MatchEvent::handle_actions`.

### 2.3 Live design system

The `live_design!{}` macro defines widget trees in a Makepad-specific DSL. Each node
maps to a `LiveNode` array. Widgets implement `LiveApply` / `LiveHook`:

- `before_apply` / `after_apply` — lifecycle hooks around property application.
- `ApplyFrom::NewFromDoc` — initial creation from DSL.
- `ApplyFrom::UpdateFromDoc` — hot-reload / live edit.
- `ApplyFrom::Over` — programmatic `apply_over()`.

`apply_over()` is the programmatic entry point: you can mutate any widget's properties
at runtime by constructing `LiveNode` arrays and calling `widget.apply_over(cx, nodes)`.
This is the key mechanism the bridge will use.

### 2.4 Event model

`Event` is a flat enum with ~54 variants covering:
- Lifecycle: `Startup`, `Shutdown`, `Foreground`, `Background`, `Resume`, `Pause`
- Input: `MouseDown/Move/Up`, `TouchUpdate`, `Scroll`, `KeyDown/Up`, `TextInput`
- Rendering: `Draw`, `NextFrame`
- System: `Timer`, `Signal`, `NetworkResponses`, `LiveEdit`
- Studio: `DesignerPick`

Widgets don't receive raw events directly — they call `event.hits(area)` to get
hit-tested `Hit` results. Actions bubble up as `ActionsBuf`.

### 2.5 Studio communication

Makepad apps can connect to Makepad Studio via WebSocket. The protocol is defined in
`platform/src/studio.rs`:

- `StudioToApp`: `LiveChange { file_name, content }`, `DesignerSelectFile`, `Screenshot`
- `AppToStudio`: `LogItem`, `EventSample`, `GPUSample`, `PatchFile`, `EditFile`

This is Makepad's own Studio protocol. Our Vo Studio would use a different integration
path (see §7).

### 2.6 Stitch (embedded WASM VM)

Makepad ships `libs/stitch`, a fast WASM interpreter. It exists but the main framework
does not currently use it for scripting. It is excluded from the workspace. This
confirms that Makepad has no production-ready scripting VM — there is a clear gap that
Vo can fill.

### 2.7 Key constraints

| Constraint | Impact |
|---|---|
| `Cx` is `!Send`, single-threaded event loop | Vo VM must run on a **separate thread**; bridge via channels |
| Widgets are `Rc<RefCell<...>>` | No concurrent mutation; all widget ops must happen on the Makepad thread |
| `live_design!` is a proc macro evaluated at compile time | Cannot generate DSL at runtime; must use `apply_over()` or `WidgetFactory` |
| `WidgetRegistry` requires `LiveType` | Custom dynamic widgets need Rust-side registration |
| No stable public API guarantee | Bridge must minimize surface area touching Makepad internals |

---

## 3. Vo VM Embedding Surface (from source audit)

### 3.1 vo-engine API

```rust
// Compile
vo_engine::compile(path) -> Result<CompileOutput, CompileError>
vo_engine::compile_string(code) -> Result<CompileOutput, CompileError>
vo_engine::compile_from_memory(fs, root) -> Result<CompileOutput, CompileError>

// Run
vo_engine::run(compiled, mode, args) -> Result<(), RunError>
vo_engine::run_with_output(compiled, mode, args, sink) -> Result<(), RunError>
vo_engine::run_with_output_interruptible(compiled, mode, args, sink, interrupt) -> Result<(), RunError>
```

`CompileOutput` contains `Module`, `source_root`, `extensions`, `locked_modules`.

### 3.2 Vm direct API

```rust
let mut vm = Vm::new();
vm.enable_external_island_transport();
vm.load_with_extensions(module, ext_loader);
vm.set_program_args(args);
vm.set_interrupt_flag(flag);
vm.state.output = sink;

// Run to first yield
let outcome = vm.run()?;  // -> SchedulingOutcome

// Event dispatch
let pending = vm.scheduler.take_pending_host_events();
vm.wake_host_event_with_data(token, data);
let outcome = vm.run_scheduled()?;

// Island transport
vm.push_island_command(cmd);
let outbound = vm.take_outbound_commands();

// Output
let bytes = vm.take_host_output();
```

### 3.3 Extern registration

Three mechanisms:
1. **stdlib_register!** — compile-time stdlib externs
2. **linkme distributed_slice** — static registration from linked crates
3. **ExtensionLoader** — runtime dylib loading from `vo.ext.toml` manifests

For `vo-makepad-host`, we use mechanism #2 (linkme) for the bridge externs, so they
are automatically available when the host crate is linked.

### 3.4 Existing GUI host pattern (Studio gui_runtime.rs)

Studio already runs a Vo GUI VM on a background thread:

```
┌─────────────┐   mpsc channels   ┌──────────────────┐
│ Tauri UI     │ ←───────────────→ │ Vo VM thread     │
│ (main thread)│                   │ (gui_runtime.rs) │
└─────────────┘                   └──────────────────┘
```

- `GuestEvent::Ide { handler_id, payload }` — sync event, waits for render bytes
- `GuestEvent::IdeAsync { handler_id, payload }` — fire-and-forget
- `GuestEvent::IslandData { data }` — island transport
- `GuestEvent::Shutdown`

Vo VM yields `SuspendedForHostEvents`; host wakes it with `wake_host_event_with_data`.
VM produces output bytes (render commands) collected via `take_host_output`.

This pattern transfers directly to Makepad hosting with one key difference: the Makepad
host thread is not a Tauri webview but a Makepad `Cx::event_loop`.

---

## 4. Architecture

### 4.1 Threading model

```
┌──────────────────────────────────┐
│ Makepad thread (main)            │
│                                  │
│  Cx::event_loop                  │
│    ↓                             │
│  VoMakepadApp::handle_event()    │
│    ├─ forward UI events → VM     │
│    ├─ recv patches from VM       │
│    └─ apply patches to widgets   │
│                                  │
│  Widget tree (WidgetRef, Rc)     │
└──────────┬───────────────────────┘
           │ mpsc channels
┌──────────▼───────────────────────┐
│ Vo VM thread                     │
│                                  │
│  Vm::run() / run_scheduled()     │
│    ├─ Vo app logic               │
│    ├─ State updates              │
│    └─ Produce UiPatch commands   │
│                                  │
│  Output: serialized patch bytes  │
└──────────────────────────────────┘
```

The Makepad thread owns `Cx` and all `WidgetRef`s.  
The Vo VM thread owns the `Vm` and all Vo state.  
They communicate via typed `mpsc` channels.

### 4.2 Channel protocol

```rust
// Makepad → Vo VM
enum HostToVm {
    UiEvent { event_id: u32, widget_id: u64, payload: Vec<u8> },
    Timer { timer_id: u32, dt_ms: f64 },
    NextFrame { dt_ms: f64 },
    Resize { width: f64, height: f64 },
    Lifecycle(LifecycleEvent),
    Shutdown,
}

// Vo VM → Makepad
enum VmToHost {
    InitialTree(UiTree),
    Patch(Vec<UiOp>),
    RequestTimer { timer_id: u32, interval_ms: u32, repeat: bool },
    CancelTimer { timer_id: u32 },
    RequestNextFrame,
    Log(String),
    HostCall { call_id: u32, method: String, args: Vec<u8> },
}
```

### 4.3 UiOp — the patch instruction set

```rust
enum UiOp {
    // Tree mutations
    CreateWidget { id: u64, kind: WidgetKind, parent: u64, index: u32 },
    RemoveWidget { id: u64 },
    MoveWidget { id: u64, new_parent: u64, new_index: u32 },

    // Property mutations
    SetText { id: u64, text: String },
    SetVisible { id: u64, visible: bool },
    SetDisabled { id: u64, disabled: bool },
    ApplyProps { id: u64, props: Vec<LiveNode> },

    // Focus / scroll
    SetKeyFocus { id: u64 },
    ScrollTo { id: u64, x: f64, y: f64 },

    // Redraw
    Redraw { id: u64 },
    RedrawAll,
}
```

`WidgetKind` maps to Makepad widget types:

```rust
enum WidgetKind {
    View,
    Button,
    Label,
    TextInput,
    Image,
    ScrollBars,
    FlatList,
    PortalList,
    Slider,
    CheckBox,
    RadioButton,
    DropDown,
    Dock,
    Splitter,
    Window,
    Custom(String),  // registered native widget
}
```

### 4.4 UiTree — initial tree description

```rust
struct UiTree {
    nodes: Vec<UiNode>,
}

struct UiNode {
    id: u64,
    kind: WidgetKind,
    props: Vec<LiveNode>,
    children: Vec<UiNode>,
    event_mask: EventMask,  // which events this node wants
}
```

The host builds the Makepad widget tree from `UiTree` on the first render, then
applies `UiOp` patches incrementally.

### 4.5 Widget ID mapping

Vo assigns `u64` widget IDs. The host maintains a `HashMap<u64, WidgetRef>` mapping.
On `CreateWidget`, the host instantiates a Makepad widget via `WidgetFactory` or
`live_design!` templates and inserts it into the map.

On `RemoveWidget`, the host removes the widget from its parent's child list and drops
the mapping entry.

---

## 5. Vo-side API Design

### 5.1 Core types (vo-makepad module)

```vo
package makepad

// Widget handle — opaque, backed by host-assigned u64 ID
type Widget struct {
    id u64
}

// Widget constructors
func View(children ...Widget) Widget
func Button(text string) Widget
func Label(text string) Widget
func TextInput(placeholder string) Widget
func Image(src string) Widget
func Slider(min f64, max f64) Widget
func CheckBox(text string) Widget

// Widget methods
func (w Widget) SetText(text string)
func (w Widget) SetVisible(visible bool)
func (w Widget) SetDisabled(disabled bool)
func (w Widget) SetProp(key string, value any)
func (w Widget) Redraw()
func (w Widget) Focus()
func (w Widget) Child(name string) Widget

// Layout helpers
func (w Widget) Flow(dir FlowDir) Widget
func (w Widget) Spacing(px f64) Widget
func (w Widget) Padding(p Padding) Widget
func (w Widget) Align(x f64, y f64) Widget
func (w Widget) Width(s Size) Widget
func (w Widget) Height(s Size) Widget
```

### 5.2 Event handling

```vo
package makepad

type Event struct {
    Kind     EventKind
    WidgetID u64
    payload  []byte
}

type EventKind int
const (
    Clicked EventKind = iota
    TextChanged
    ValueChanged
    FocusGained
    FocusLost
    KeyDown
    KeyUp
)

// Host event loop integration
func WaitEvent() Event            // blocks VM fiber, yields to host
func OnClick(w Widget, fn func())
func OnTextChange(w Widget, fn func(string))
func OnValueChange(w Widget, fn func(f64))
```

### 5.3 App structure

A Vo Makepad app looks like:

```vo
package main

import "makepad"

func main() {
    counter := 0
    label := makepad.Label("Count: 0")
    btn := makepad.Button("Click me")

    makepad.Window(
        makepad.View(label, btn).Flow(makepad.Down).Spacing(20).Align(0.5, 0.5),
    )

    makepad.OnClick(btn, func() {
        counter++
        label.SetText(fmt.Sprintf("Count: %d", counter))
    })

    makepad.Run()  // enters event loop, yields to host
}
```

### 5.4 Extern implementation

Each Vo-side function is backed by a Rust extern in `vo-makepad-host`:

```rust
#[vo_extern(pkg = "makepad", name = "View")]
fn makepad_view(vm: &mut VmContext, args: &[Value]) -> Value {
    // Serialize CreateWidget + child attachment ops → host output
}

#[vo_extern(pkg = "makepad", name = "OnClick")]
fn makepad_on_click(vm: &mut VmContext, args: &[Value]) -> Value {
    // Register event subscription for widget ID
}

#[vo_extern(pkg = "makepad", name = "Run")]
fn makepad_run(vm: &mut VmContext, args: &[Value]) -> Value {
    // Yield to host with SuspendedForHostEvents
}
```

---

## 6. Host Implementation (vo-makepad-host crate)

### 6.1 Crate structure

```
vo-makepad-host/
├── Cargo.toml           # depends on makepad-widgets, vo-vm, vo-engine, vo-runtime
├── src/
│   ├── lib.rs           # public API: run_vo_app(path)
│   ├── app.rs           # VoMakepadApp: AppMain impl
│   ├── bridge.rs        # Widget ID map, UiOp application, event forwarding
│   ├── protocol.rs      # HostToVm, VmToHost, UiOp, UiTree definitions
│   ├── vm_thread.rs     # Vo VM thread runner
│   ├── externs.rs       # Vo extern function implementations
│   └── widgets.rs       # WidgetKind → Makepad widget factory mapping
```

### 6.2 VoMakepadApp

```rust
#[derive(Live, LiveHook)]
pub struct VoMakepadApp {
    #[live] ui: WidgetRef,
    #[rust] bridge: Bridge,
    #[rust] vm_handle: Option<VmHandle>,
}

impl AppMain for VoMakepadApp {
    fn handle_event(&mut self, cx: &mut Cx, event: &Event) {
        // 1. Poll VM output channel for patches
        self.bridge.apply_pending_patches(cx);

        // 2. Forward relevant events to VM
        match event {
            Event::Startup => self.bridge.start_vm(cx),
            Event::Shutdown => self.bridge.shutdown_vm(),
            Event::Draw(_) => self.bridge.draw_all(cx),
            Event::NextFrame(_) => self.bridge.forward_next_frame(),
            Event::Timer(e) => self.bridge.forward_timer(e),
            _ => {}
        }

        // 3. Let widgets handle events and collect actions
        let actions = cx.capture_actions(|cx| {
            self.ui.handle_event(cx, event, &mut Scope::empty());
        });

        // 4. Forward widget actions to VM
        self.bridge.forward_actions(&actions);
    }
}
```

### 6.3 Bridge

```rust
struct Bridge {
    // Channels
    to_vm: mpsc::Sender<HostToVm>,
    from_vm: mpsc::Receiver<VmToHost>,

    // Widget ID map
    widgets: HashMap<u64, WidgetRef>,
    next_id: u64,

    // Event subscriptions
    click_handlers: HashMap<u64, VoCallbackId>,
    text_change_handlers: HashMap<u64, VoCallbackId>,
    value_change_handlers: HashMap<u64, VoCallbackId>,

    // Timer map
    timers: HashMap<u32, Timer>,
}

impl Bridge {
    fn apply_pending_patches(&mut self, cx: &mut Cx) {
        while let Ok(msg) = self.from_vm.try_recv() {
            match msg {
                VmToHost::InitialTree(tree) => self.build_tree(cx, tree),
                VmToHost::Patch(ops) => {
                    for op in ops {
                        self.apply_op(cx, op);
                    }
                }
                VmToHost::RequestTimer { timer_id, interval_ms, repeat } => {
                    let timer = cx.start_interval(interval_ms as f64 / 1000.0);
                    self.timers.insert(timer_id, timer);
                }
                VmToHost::CancelTimer { timer_id } => {
                    if let Some(timer) = self.timers.remove(&timer_id) {
                        cx.stop_timer(timer);
                    }
                }
                VmToHost::RequestNextFrame => {
                    cx.request_next_frame();
                }
                VmToHost::Log(msg) => {
                    log!("{}", msg);
                }
                _ => {}
            }
        }
    }

    fn apply_op(&mut self, cx: &mut Cx, op: UiOp) {
        match op {
            UiOp::SetText { id, text } => {
                if let Some(w) = self.widgets.get(&id) {
                    w.set_text(cx, &text);
                }
            }
            UiOp::SetVisible { id, visible } => {
                if let Some(w) = self.widgets.get(&id) {
                    w.set_visible(cx, visible);
                }
            }
            UiOp::ApplyProps { id, props } => {
                if let Some(w) = self.widgets.get(&id) {
                    w.apply_over(cx, &props);
                }
            }
            UiOp::Redraw { id } => {
                if let Some(w) = self.widgets.get(&id) {
                    w.redraw(cx);
                }
            }
            // CreateWidget, RemoveWidget, MoveWidget handled by tree builder...
            _ => {}
        }
    }
}
```

### 6.4 Widget instantiation

Makepad widgets are normally created from `live_design!` templates. For dynamic
creation from Vo, we use `WidgetRef::new_from_module()` with pre-registered
templates:

```rust
fn create_widget(&mut self, cx: &mut Cx, kind: WidgetKind) -> WidgetRef {
    match kind {
        WidgetKind::Button => {
            // Use a pre-defined live_design template
            WidgetRef::new_from_module(cx, module_id, live_id!(VoButton))
        }
        WidgetKind::Label => {
            WidgetRef::new_from_module(cx, module_id, live_id!(VoLabel))
        }
        // ...
    }
}
```

The host crate registers a `live_design!` block with default templates for each
supported widget kind:

```rust
live_design! {
    VoButton = <Button> { text: "" }
    VoLabel = <Label> { text: "" }
    VoTextInput = <TextInput> { text: "" }
    VoView = <View> { }
    VoImage = <Image> { }
    VoSlider = <Slider> { }
    // ...
}
```

---

## 7. Studio Integration

### 7.1 Approach

Studio already supports framework-neutral GUI backends via its render island
architecture. Makepad can be added as another backend alongside vogui.

The integration path:

1. `vo-makepad-host` compiles to a **dylib** with a C ABI entry point.
2. Studio's `gui_runtime.rs` gains a `MakepadBackend` variant that:
   - Loads the dylib
   - Spawns the Makepad process or thread
   - Communicates via the same channel protocol

However, since Makepad's `Cx` is `!Send` and requires owning the event loop, the
cleanest Studio integration is:

- **Native preview**: spawn a separate Makepad window process, communicate via pipe/socket.
- **Web preview**: Makepad compiles to WASM; Studio loads it in a webview.

### 7.2 Framework contract

```typescript
interface MakepadFrameworkContract {
    kind: "makepad";
    nativeHostPath: string;    // path to vo-makepad-host binary
    rendererPath: string;      // path to WASM renderer (for web preview)
    supportsCanvas: false;     // Makepad owns its own window
    supportsIpc: true;
}
```

### 7.3 Phased Studio support

- **Phase 1**: No Studio integration. Users build standalone Makepad binaries.
- **Phase 2**: Studio can `Run` → spawn native Makepad window.
- **Phase 3**: Studio can `Preview` → embedded webview with Makepad WASM.

---

## 8. Comparison: Why Not Alternative Approaches

### 8.1 Approach: Vo generates Makepad DSL strings

**Rejected.** Makepad DSL is a compile-time proc macro language. There is no
`cx.eval(dsl_string)` in the production codebase. The `apply_over` API takes
`LiveNode` arrays, not strings. Generating DSL text would require re-parsing it
through the live compiler — fragile, slow, and untyped.

### 8.2 Approach: 1:1 Makepad API wrapper

**Rejected.** Makepad has no stable public API. Widgets are `Rc<RefCell<...>>`
on the main thread. Exposing every method as a Vo extern would create:
- Massive surface area with no stability guarantee
- Cross-thread safety issues (WidgetRef is !Send)
- Poor user experience (Vo users would need to learn Makepad internals)

### 8.3 Approach: Vo replaces Makepad's live system

**Rejected.** The live system is deeply integrated into Makepad's proc macros,
compiler, and widget infrastructure. Replacing it would require forking Makepad.

### 8.4 Approach: This design (stable bridge protocol)

**Chosen.** Benefits:
- Minimal coupling to Makepad internals (only `Widget` trait + `apply_over`)
- Thread-safe by design (channel-based)
- Vo users see a clean, Vo-native API
- Makepad version upgrades require updating only the bridge crate
- Same pattern already proven in Studio's gui_runtime.rs

---

## 9. Risks and Mitigations

| Risk | Severity | Mitigation |
|---|---|---|
| Makepad API breaking changes | High | Bridge touches only Widget trait + apply_over; minimize surface |
| Dynamic widget creation limitations | Medium | Pre-register templates via live_design!; use WidgetFactory for custom types |
| Performance of channel-based patches | Low | Batch patches per frame; use binary encoding; Makepad's own frame rate is the bottleneck |
| Widget parity gap (missing widget types) | Medium | Start with core set; add Custom(String) escape hatch for native registration |
| Makepad WASM target maturity | Medium | Phase 1 is native-only; WASM preview is Phase 3 |
| Concurrent mutation of widget state | High | All widget ops happen on Makepad thread; Vo thread never touches WidgetRef |

---

## 10. Phased Rollout

### Phase 1: Minimal Viable Host (weeks 1-3)

**Goal:** Run a Vo script that creates a window with buttons/labels and handles clicks.

- [ ] Create `vo-makepad-host` crate with Cargo.toml
- [ ] Implement `VoMakepadApp` as `AppMain`
- [ ] Implement VM thread spawning (reuse gui_runtime pattern)
- [ ] Define channel protocol types
- [ ] Implement bridge for: View, Button, Label, SetText, Redraw
- [ ] Implement Vo externs: View, Button, Label, SetText, OnClick, Run, WaitEvent
- [ ] Create Vo-side `makepad` package with core types
- [ ] Demo app: counter (button + label)

### Phase 2: Extended Widget Set (weeks 4-6)

**Goal:** Support enough widgets for real apps.

- [ ] TextInput, Slider, CheckBox, RadioButton, DropDown
- [ ] Image (with resource loading)
- [ ] ScrollBars, FlatList (with data binding)
- [ ] Layout properties: Flow, Spacing, Padding, Align, Width, Height
- [ ] Timer and NextFrame support
- [ ] Proper error reporting from VM to host

### Phase 3: Studio Integration (weeks 7-9)

**Goal:** Studio can run Makepad apps.

- [ ] Framework contract definition for Makepad
- [ ] Studio Run command spawns native Makepad process
- [ ] IPC between Studio and Makepad process
- [ ] Console log forwarding
- [ ] Hot reload: recompile Vo → send new module → VM restart

### Phase 4: Advanced Features (weeks 10+)

- [ ] Custom widget registration from Vo
- [ ] Canvas / custom draw support
- [ ] Animation helpers
- [ ] WASM target for web preview
- [ ] Dock / Splitter / multi-window support
- [ ] Designer integration (visual editing)

---

## 11. Open Questions

1. **Widget child management**: Makepad's `View` children are defined in `live_design!`.
   Dynamic child insertion may need `create_child()` or a container widget that
   reads children from data. Need to verify the current API for runtime child
   manipulation.

2. **LiveNode construction**: Building `LiveNode` arrays programmatically (for
   `apply_over`) requires understanding the node format. This is doable but
   undocumented — needs experimentation.

3. **Action routing**: Makepad actions use `widget.clicked(&actions)` pattern.
   The bridge needs to intercept actions before they reach Makepad's default
   handling and route them to Vo callbacks.

4. **Resource loading**: Images, fonts, and other assets need a Vo-accessible
   loading path. Makepad uses `CxDependency` for this — need to expose it
   through the bridge.

5. **Makepad Studio coexistence**: If the app connects to Makepad Studio for
   live design, should Vo Studio also be active? Probably not — pick one IDE
   per session.
