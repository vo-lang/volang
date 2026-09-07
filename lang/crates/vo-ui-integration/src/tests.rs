use super::*;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use vo_engine::{load_extensions, RunMode};
use vo_vm::vm::{SchedulingOutcome, Vm};

static UI_VM_TEST_COUNTER: AtomicU64 = AtomicU64::new(1);

struct UiTestWorkspace(std::path::PathBuf);

impl UiTestWorkspace {
    fn create() -> Self {
        Self::create_with_main(
            r#"
package main
import "github.com/vo-lang/ui"
var label = "before"
func App() ui.View {
	return ui.Padding(ui.Column(
		ui.Text(label),
		ui.TextInput(label, "Type", func(event ui.Event) { label = event.Text }),
	), 12)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
        )
    }

    fn create_with_main(source: &str) -> Self {
        let sequence = UI_VM_TEST_COUNTER.fetch_add(1, Ordering::Relaxed);
        let root = std::env::temp_dir().join(format!(
            "volang-ui-vm-test-{}-{sequence}",
            std::process::id()
        ));
        let app = root.join("app");
        let ui = root.join("ui");
        std::fs::create_dir_all(&app).unwrap();
        std::fs::create_dir_all(ui.join("animation")).unwrap();
        std::fs::create_dir_all(ui.join("assets")).unwrap();
        std::fs::create_dir_all(ui.join("chart")).unwrap();
        std::fs::create_dir_all(ui.join("commands")).unwrap();
        std::fs::create_dir_all(ui.join("desktop")).unwrap();
        std::fs::create_dir_all(ui.join("document")).unwrap();
        std::fs::create_dir_all(ui.join("editor")).unwrap();
        std::fs::create_dir_all(ui.join("kit")).unwrap();
        std::fs::create_dir_all(ui.join("kit/components")).unwrap();
        std::fs::create_dir_all(ui.join("kit/data")).unwrap();
        std::fs::create_dir_all(ui.join("kit/headless")).unwrap();
        std::fs::create_dir_all(ui.join("kit/icons")).unwrap();
        std::fs::create_dir_all(ui.join("kit/tokens")).unwrap();
        std::fs::create_dir_all(ui.join("forms")).unwrap();
        std::fs::create_dir_all(ui.join("gesture")).unwrap();
        std::fs::create_dir_all(ui.join("graphics")).unwrap();
        std::fs::create_dir_all(ui.join("i18n/core")).unwrap();
        std::fs::create_dir_all(ui.join("language")).unwrap();
        std::fs::create_dir_all(ui.join("media")).unwrap();
        std::fs::create_dir_all(ui.join("motion")).unwrap();
        std::fs::create_dir_all(ui.join("navigation")).unwrap();
        std::fs::create_dir_all(ui.join("observability")).unwrap();
        std::fs::create_dir_all(ui.join("persistence")).unwrap();
        std::fs::create_dir_all(ui.join("platform")).unwrap();
        std::fs::create_dir_all(ui.join("resource")).unwrap();
        std::fs::create_dir_all(ui.join("system")).unwrap();
        std::fs::create_dir_all(ui.join("task")).unwrap();
        std::fs::create_dir_all(ui.join("testing")).unwrap();
        std::fs::create_dir_all(ui.join("web/server")).unwrap();
        std::fs::create_dir_all(ui.join("workspace")).unwrap();
        std::fs::write(
            root.join("vo.work"),
            "format = 1\nmembers = [\"app\", \"ui\"]\n",
        )
        .unwrap();
        let app_mod = concat!(
            "format = 1\n",
            "module = \"github.com/acme/ui-test\"\n",
            "version = \"0.1.4\"\n",
            "vo = \"0.1.4\"\n",
            "[dependencies]\n",
            "\"github.com/vo-lang/ui\" = \"^0.1.4\"\n",
        );
        let ui_mod = include_str!("../../../../ui/vo.mod");
        std::fs::write(app.join("vo.mod"), app_mod).unwrap();
        std::fs::write(ui.join("vo.mod"), ui_mod).unwrap();
        std::fs::write(ui.join("ui.vo"), include_str!("../../../../ui/ui.vo")).unwrap();
        std::fs::write(
            ui.join("animation/animation.vo"),
            include_str!("../../../../ui/animation/animation.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("assets/assets.vo"),
            include_str!("../../../../ui/assets/assets.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("chart/chart.vo"),
            include_str!("../../../../ui/chart/chart.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("commands/commands.vo"),
            include_str!("../../../../ui/commands/commands.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("desktop/desktop.vo"),
            include_str!("../../../../ui/desktop/desktop.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("document/document.vo"),
            include_str!("../../../../ui/document/document.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("editor/editor.vo"),
            include_str!("../../../../ui/editor/editor.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("kit/kit.vo"),
            include_str!("../../../../ui/kit/kit.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("kit/components/components.vo"),
            include_str!("../../../../ui/kit/components/components.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("kit/data/data.vo"),
            include_str!("../../../../ui/kit/data/data.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("kit/headless/headless.vo"),
            include_str!("../../../../ui/kit/headless/headless.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("kit/icons/icons.vo"),
            include_str!("../../../../ui/kit/icons/icons.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("kit/tokens/tokens.vo"),
            include_str!("../../../../ui/kit/tokens/tokens.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("forms/forms.vo"),
            include_str!("../../../../ui/forms/forms.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("gesture/gesture.vo"),
            include_str!("../../../../ui/gesture/gesture.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("graphics/graphics.vo"),
            include_str!("../../../../ui/graphics/graphics.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("i18n/core/core.vo"),
            include_str!("../../../../ui/i18n/core/core.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("i18n/i18n.vo"),
            include_str!("../../../../ui/i18n/i18n.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("language/language.vo"),
            include_str!("../../../../ui/language/language.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("media/media.vo"),
            include_str!("../../../../ui/media/media.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("motion/motion.vo"),
            include_str!("../../../../ui/motion/motion.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("navigation/navigation.vo"),
            include_str!("../../../../ui/navigation/navigation.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("observability/observability.vo"),
            include_str!("../../../../ui/observability/observability.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("persistence/persistence.vo"),
            include_str!("../../../../ui/persistence/persistence.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("platform/platform.vo"),
            include_str!("../../../../ui/platform/platform.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("resource/resource.vo"),
            include_str!("../../../../ui/resource/resource.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("system/system.vo"),
            include_str!("../../../../ui/system/system.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("task/task.vo"),
            include_str!("../../../../ui/task/task.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("testing/testing.vo"),
            include_str!("../../../../ui/testing/testing.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("web/web.vo"),
            include_str!("../../../../ui/web/web.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("web/server/server.vo"),
            include_str!("../../../../ui/web/server/server.vo"),
        )
        .unwrap();
        std::fs::write(
            ui.join("workspace/workspace.vo"),
            include_str!("../../../../ui/workspace/workspace.vo"),
        )
        .unwrap();
        let root_manifest = vo_module::schema::modfile::ModFile::parse(app_mod).unwrap();
        let ui_manifest = vo_module::schema::modfile::ModFile::parse(ui_mod).unwrap();
        let lock = vo_module::schema::lockfile::LockFile {
            format: vo_module::schema::lockfile::LOCK_FILE_VERSION,
            root: vo_module::lock::module_intent_digest(&root_manifest).unwrap(),
            modules: vec![vo_module::schema::lockfile::LockedModule {
                path: vo_module::identity::ModulePath::parse("github.com/vo-lang/ui").unwrap(),
                version: vo_module::version::ExactVersion::parse("0.1.4").unwrap(),
                origin: vo_module::schema::lockfile::LockOrigin::Workspace,
                release: None,
                intent: Some(vo_module::lock::module_intent_digest(&ui_manifest).unwrap()),
                selection: None,
            }],
        };
        std::fs::write(app.join("vo.lock"), lock.render().unwrap()).unwrap();
        std::fs::write(app.join("main.vo"), source).unwrap();
        Self(root)
    }

    fn create_with_imported_component(app_source: &str, library_source: &str) -> Self {
        let workspace = Self::create_with_main(app_source);
        let root = &workspace.0;
        let app = root.join("app");
        let widgets = root.join("widgets");
        std::fs::create_dir_all(&widgets).unwrap();
        std::fs::write(
            root.join("vo.work"),
            "format = 1\nmembers = [\"app\", \"ui\", \"widgets\"]\n",
        )
        .unwrap();
        let app_mod = concat!(
            "format = 1\n",
            "module = \"github.com/acme/ui-test\"\n",
            "version = \"0.1.4\"\n",
            "vo = \"0.1.4\"\n",
            "[dependencies]\n",
            "\"github.com/acme/widgets\" = \"^0.1.4\"\n",
            "\"github.com/vo-lang/ui\" = \"^0.1.4\"\n",
        );
        let widgets_mod = concat!(
            "format = 1\n",
            "module = \"github.com/acme/widgets\"\n",
            "version = \"0.1.4\"\n",
            "vo = \"0.1.4\"\n",
            "[dependencies]\n",
            "\"github.com/vo-lang/ui\" = \"^0.1.4\"\n",
        );
        let ui_mod = include_str!("../../../../ui/vo.mod");
        std::fs::write(app.join("vo.mod"), app_mod).unwrap();
        std::fs::write(widgets.join("vo.mod"), widgets_mod).unwrap();
        std::fs::write(widgets.join("widgets.vo"), library_source).unwrap();
        let root_manifest = vo_module::schema::modfile::ModFile::parse(app_mod).unwrap();
        let widgets_manifest = vo_module::schema::modfile::ModFile::parse(widgets_mod).unwrap();
        let ui_manifest = vo_module::schema::modfile::ModFile::parse(ui_mod).unwrap();
        let lock = vo_module::schema::lockfile::LockFile {
            format: vo_module::schema::lockfile::LOCK_FILE_VERSION,
            root: vo_module::lock::module_intent_digest(&root_manifest).unwrap(),
            modules: vec![
                vo_module::schema::lockfile::LockedModule {
                    path: vo_module::identity::ModulePath::parse("github.com/acme/widgets")
                        .unwrap(),
                    version: vo_module::version::ExactVersion::parse("0.1.4").unwrap(),
                    origin: vo_module::schema::lockfile::LockOrigin::Workspace,
                    release: None,
                    intent: Some(vo_module::lock::module_intent_digest(&widgets_manifest).unwrap()),
                    selection: None,
                },
                vo_module::schema::lockfile::LockedModule {
                    path: vo_module::identity::ModulePath::parse("github.com/vo-lang/ui").unwrap(),
                    version: vo_module::version::ExactVersion::parse("0.1.4").unwrap(),
                    origin: vo_module::schema::lockfile::LockOrigin::Workspace,
                    release: None,
                    intent: Some(vo_module::lock::module_intent_digest(&ui_manifest).unwrap()),
                    selection: None,
                },
            ],
        };
        std::fs::write(app.join("vo.lock"), lock.render().unwrap()).unwrap();
        workspace
    }

    fn app(&self) -> std::path::PathBuf {
        self.0.join("app")
    }

    fn compile(&self) -> vo_engine::CompileOutput {
        let workfile = self.0.join("vo.work");
        #[cfg(not(windows))]
        let workfile = workfile.canonicalize().unwrap();
        let options = vo_module::project::ProjectContextOptions::new(
            vo_module::workspace::WorkspaceDiscovery::Explicit(workfile),
        );
        engine()
            .compile_with_options(self.app().to_string_lossy().as_ref(), &options)
            .unwrap()
    }
}

impl Drop for UiTestWorkspace {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn ui_batches_for(
    module: Arc<vo_runtime::bytecode::LoadedModule>,
    mode: RunMode,
) -> (vo_ui_protocol::MutationBatch, vo_ui_protocol::MutationBatch) {
    let (initial, mut updates) = ui_input_batches_for(module, mode, &["after"]);
    (initial, updates.remove(0))
}

fn ui_initial_batch_for(
    module: Arc<vo_runtime::bytecode::LoadedModule>,
    mode: RunMode,
) -> vo_ui_protocol::MutationBatch {
    let mut vm = match mode {
        RunMode::Vm => Vm::new(),
        RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        })
        .expect("JIT should initialize"),
    };
    register_externs(&mut vm, &module).unwrap();
    vm.load_verified(module).unwrap();
    assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
    vo_ui_protocol::decode_batch(
        &vm.take_host_output()
            .expect("UI Mount should publish its initial mutation batch"),
        vo_ui_protocol::ProtocolLimits::default(),
    )
    .unwrap()
}

fn ui_input_batches_for(
    module: Arc<vo_runtime::bytecode::LoadedModule>,
    mode: RunMode,
    values: &[&str],
) -> (
    vo_ui_protocol::MutationBatch,
    Vec<vo_ui_protocol::MutationBatch>,
) {
    let mut vm = match mode {
        RunMode::Vm => Vm::new(),
        RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        })
        .expect("JIT should initialize"),
    };
    register_externs(&mut vm, &module).unwrap();
    vm.load_verified(module).unwrap();
    assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
    let limits = vo_ui_protocol::ProtocolLimits::default();
    let initial = vo_ui_protocol::decode_batch(
        &vm.take_host_output()
            .expect("UI Mount should publish its initial mutation batch"),
        limits,
    )
    .unwrap();
    let (target, handler) = initial
        .mutations
        .iter()
        .find_map(|mutation| match mutation {
            vo_ui_protocol::Mutation::Listen { id, listener }
                if listener.event == vo_ui_core::EventType::INPUT =>
            {
                Some((*id, listener.handler))
            }
            _ => None,
        })
        .expect("fixture should publish a live input listener");
    let mut updates = Vec::with_capacity(values.len());
    for (index, value) in values.iter().enumerate() {
        let event = vo_ui_protocol::EventEnvelope::new(
            initial.session_epoch,
            vo_ui_core::UiEvent {
                handler,
                event: vo_ui_core::EventType::INPUT,
                target,
                sequence: index as u64 + 1,
                payload: vo_ui_core::EventPayload::Text((*value).to_string()),
            },
        );
        let event_bytes = vo_ui_protocol::encode_event(&event, limits).unwrap();
        let pending = vm.take_pending_host_events();
        assert_eq!(pending.len(), 1);
        assert!(pending[0].source.is_gui_event_replay());
        assert!(vm.wake_host_event_with_data(pending[0].key, event_bytes));
        assert_eq!(
            vm.run_scheduled().unwrap(),
            SchedulingOutcome::SuspendedForHostEvents
        );
        updates.push(
            vo_ui_protocol::decode_batch(
                &vm.take_host_output()
                    .expect("UI handler should publish one update mutation batch"),
                limits,
            )
            .unwrap(),
        );
    }
    (initial, updates)
}

fn ui_single_event_batches_for(
    module: Arc<vo_runtime::bytecode::LoadedModule>,
    mode: RunMode,
    event_type: vo_ui_core::EventType,
    payload: vo_ui_core::EventPayload,
) -> (vo_ui_protocol::MutationBatch, vo_ui_protocol::MutationBatch) {
    ui_single_event_batches_for_named(module, mode, event_type, payload, None)
}

fn ui_single_event_batches_for_named(
    module: Arc<vo_runtime::bytecode::LoadedModule>,
    mode: RunMode,
    event_type: vo_ui_core::EventType,
    payload: vo_ui_core::EventPayload,
    accessible_name: Option<&str>,
) -> (vo_ui_protocol::MutationBatch, vo_ui_protocol::MutationBatch) {
    let mut vm = match mode {
        RunMode::Vm => Vm::new(),
        RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        })
        .expect("JIT should initialize"),
    };
    register_externs(&mut vm, &module).unwrap();
    vm.load_verified(module).unwrap();
    assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
    let limits = vo_ui_protocol::ProtocolLimits::default();
    let initial = vo_ui_protocol::decode_batch(
        &vm.take_host_output()
            .expect("UI Mount should publish its initial mutation batch"),
        limits,
    )
    .unwrap();
    vm.gc_collect()
        .expect("mounted UI handler leases should survive a complete GC cycle");
    let named_target = accessible_name.and_then(|expected| {
        initial
            .mutations
            .iter()
            .find_map(|mutation| match mutation {
                vo_ui_protocol::Mutation::SetProperty { id, property }
                    if property.id == vo_ui_core::PropertyId::ACCESSIBLE_NAME
                        && property.value == vo_ui_core::Value::Text(expected.to_string()) =>
                {
                    Some(*id)
                }
                _ => None,
            })
    });
    let (target, handler) = initial
        .mutations
        .iter()
        .find_map(|mutation| match mutation {
            vo_ui_protocol::Mutation::Listen { id, listener }
                if listener.event == event_type
                    && named_target.is_none_or(|target| target == *id) =>
            {
                Some((*id, listener.handler))
            }
            _ => None,
        })
        .expect("fixture should publish the requested listener");
    let event = vo_ui_protocol::EventEnvelope::new(
        initial.session_epoch,
        vo_ui_core::UiEvent {
            handler,
            event: event_type,
            target,
            sequence: 1,
            payload,
        },
    );
    let pending = vm.take_pending_host_events();
    assert_eq!(pending.len(), 1);
    assert!(pending[0].source.is_gui_event_replay());
    assert!(vm.wake_host_event_with_data(
        pending[0].key,
        vo_ui_protocol::encode_event(&event, limits).unwrap()
    ));
    assert_eq!(
        vm.run_scheduled().unwrap(),
        SchedulingOutcome::SuspendedForHostEvents
    );
    let update = vo_ui_protocol::decode_batch(
        &vm.take_host_output()
            .expect("UI handler should publish one update mutation batch"),
        limits,
    )
    .unwrap();
    (initial, update)
}

fn click_first_native_button(
    session: &mut crate::NativeUiVmSession,
    window: vo_app_protocol::WindowHandle,
    view: vo_app_protocol::ViewHandle,
    now: std::time::Instant,
) -> crate::NativeUiSessionReport {
    let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
    let layout = session
        .renderer_mut()
        .host_mut()
        .compute_and_set_layout(
            vo_ui_layout::Size::new(640.0, 480.0),
            vo_ui_layout::LayoutLimits::default(),
            &mut measurer,
        )
        .unwrap();
    let button = layout
        .iter()
        .find(|layout| {
            session
                .renderer()
                .host()
                .tree()
                .node(layout.node)
                .is_some_and(|node| node.listeners.contains_key(&vo_ui_core::EventType::CLICK))
        })
        .unwrap();
    let x_milli = ((button.rect.x + button.rect.width / 2.0) * 1_000.0) as i32;
    let y_milli = ((button.rect.y + button.rect.height / 2.0) * 1_000.0) as i32;
    let input = |sequence, pressed| vo_app_host_native::NativeInputEvent {
        sequence,
        timestamp_micros: sequence,
        window,
        view,
        kind: vo_app_host_native::NativeInputKind::PointerButton {
            device: 1,
            button: vo_app_host_native::NativePointerButton::Primary,
            pressed,
            click_count: 1,
            x_milli,
            y_milli,
        },
    };
    assert!(!session.route_input(&input(1, true)).unwrap());
    assert!(session.route_input(&input(2, false)).unwrap());
    session.pump(now).unwrap()
}

fn click_named_native_button(
    session: &mut crate::NativeUiVmSession,
    window: vo_app_protocol::WindowHandle,
    view: vo_app_protocol::ViewHandle,
    name: &str,
    interaction: u64,
    now: std::time::Instant,
) -> crate::NativeUiSessionReport {
    let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
    let layout = session
        .renderer_mut()
        .host_mut()
        .compute_and_set_layout(
            vo_ui_layout::Size::new(640.0, 480.0),
            vo_ui_layout::LayoutLimits::default(),
            &mut measurer,
        )
        .unwrap();
    let button = layout
        .iter()
        .find(|layout| {
            session
                .renderer()
                .host()
                .tree()
                .node(layout.node)
                .is_some_and(|node| {
                    node.listeners.contains_key(&vo_ui_core::EventType::CLICK)
                        && node
                            .properties
                            .get(&vo_ui_core::PropertyId::ACCESSIBLE_NAME)
                            == Some(&vo_ui_core::Value::Text(name.to_string()))
                })
        })
        .unwrap_or_else(|| panic!("button {name:?} is missing"));
    let x_milli = ((button.rect.x + button.rect.width / 2.0) * 1_000.0) as i32;
    let y_milli = ((button.rect.y + button.rect.height / 2.0) * 1_000.0) as i32;
    let first_sequence = interaction.checked_mul(2).unwrap().saturating_sub(1);
    let input = |sequence, pressed| vo_app_host_native::NativeInputEvent {
        sequence,
        timestamp_micros: sequence,
        window,
        view,
        kind: vo_app_host_native::NativeInputKind::PointerButton {
            device: 1,
            button: vo_app_host_native::NativePointerButton::Primary,
            pressed,
            click_count: 1,
            x_milli,
            y_milli,
        },
    };
    assert!(!session.route_input(&input(first_sequence, true)).unwrap());
    assert!(session
        .route_input(&input(first_sequence + 1, false))
        .unwrap());
    session.pump(now).unwrap()
}

#[test]
fn component_bundle_mounts_nested_instances_in_vm_and_jit() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
func Label(label string) ui.View { return ui.Text(label) }
func App() ui.View {
	label := "linked"
	return ui.Column(Label(label), ui.Key(Label("fixed"), "fixed-key"))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .module()
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .is_none());
    assert!(compiled
        .module
        .module()
        .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
        .is_some());
    for mode in [RunMode::Vm, RunMode::Jit] {
        let batch = ui_initial_batch_for(compiled.module.clone(), mode);
        for expected in ["linked", "fixed"] {
            assert!(batch.mutations.iter().any(|mutation| {
                matches!(
                    mutation,
                    vo_ui_protocol::Mutation::SetText { text, .. } if text == expected
                )
            }));
        }
    }
}

#[test]
fn component_bundle_keeps_nested_state_and_handlers_instance_local() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	})
}
func App() ui.View {
	return ui.Column(Counter("A"), Counter("B"))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .module()
        .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
        .is_some());
    for mode in [RunMode::Vm, RunMode::Jit] {
        let (initial, update) = ui_single_event_batches_for_named(
            compiled.module.clone(),
            mode,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
            Some("A 0"),
        );
        for expected in ["A 0", "B 0"] {
            assert!(initial.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetText { text, .. } if text == expected
            )));
        }
        assert_eq!(update.revision, 2);
        assert!(
            update.mutations.iter().any(|mutation| matches!(
                mutation,
                vo_ui_protocol::Mutation::SetText { text, .. } if text == "A 1"
            )),
            "unexpected component update: {:?}",
            update.mutations
        );
        assert!(!update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "B 1"
        )));
        assert!(update.mutations.iter().all(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { .. } | vo_ui_protocol::Mutation::SetProperty { .. }
        )));
    }
}

#[test]
fn imported_component_bundle_executes_state_and_handlers_in_vm_and_jit() {
    let workspace = UiTestWorkspace::create_with_imported_component(
        r#"
package main
import (
	"github.com/acme/widgets"
	"github.com/vo-lang/ui"
)
func App() ui.View {
	return ui.Column(widgets.Counter("Imported"), widgets.Counter("Second"))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        r#"
package widgets
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	})
}
"#,
    );
    let compiled = workspace.compile();
    let artifact = compiled
        .module
        .module()
        .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
        .expect("imported component bundle");
    let bundle = vo_ui_artifact::decode_component_bundle(
        &artifact.payload,
        vo_ui_artifact::BundleLimits::default(),
        vo_ui_plan::PlanLimits::default(),
    )
    .unwrap();
    assert!(bundle.definitions.iter().any(|definition| {
        definition.type_id.module() == "github.com/acme/widgets"
            && definition.type_id.object() == "Counter"
    }));
    for mode in [RunMode::Vm, RunMode::Jit] {
        let (_, update) = ui_single_event_batches_for_named(
            compiled.module.clone(),
            mode,
            vo_ui_core::EventType::CLICK,
            vo_ui_core::EventPayload::None,
            Some("Imported 0"),
        );
        assert!(update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "Imported 1"
        )));
        assert!(!update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetText { text, .. } if text == "Second 1"
        )));
    }
}

#[test]
fn component_scopes_preserve_dynamic_keyed_state_in_vm_and_jit() {
    let workspace = UiTestWorkspace::create_with_imported_component(
        r#"
package main
import (
	"github.com/acme/widgets"
	"github.com/vo-lang/ui"
)
func App() ui.View {
	phase := int64(0)
	advance := func(event ui.Event) { phase++ }
	if phase == 0 { return ui.Column(ui.Button("Reorder", advance), ui.Key(widgets.Counter("Alpha"), "alpha"), ui.Key(widgets.Counter("Beta"), "beta")) }
	if phase == 1 { return ui.Column(ui.Button("Remove Beta", advance), ui.Key(widgets.Counter("Beta"), "beta"), ui.Key(widgets.Counter("Alpha"), "alpha")) }
	if phase == 2 { return ui.Column(ui.Button("Insert Beta", advance), ui.Key(widgets.Counter("Alpha"), "alpha")) }
	if phase == 3 { return ui.Column(ui.Button("Replace Alpha", advance), ui.Key(widgets.Counter("Alpha"), "alpha"), ui.Key(widgets.Counter("Beta"), "beta")) }
	return ui.Column(ui.Button("Complete", func(event ui.Event) {}), ui.Key(widgets.Counter("Alpha"), "alpha-v2"), ui.Key(widgets.Counter("Beta"), "beta"))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
        r#"
package widgets
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) { count++ })
}
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .module()
        .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
        .is_none());
    for mode in [RunMode::Vm, RunMode::Jit] {
        let vm = build_native_gui_vm_for_mode(compiled.clone(), mode).unwrap();
        let window = vo_app_protocol::GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let view = vo_app_protocol::GenerationalHandle {
            index: 2,
            generation: 1,
        };
        let now = std::time::Instant::now();
        let (mut session, _) = crate::NativeUiVmSession::start(
            vm,
            window,
            view,
            crate::NativeUiSessionConfig::default(),
            now,
        )
        .unwrap();
        let has_text = |session: &crate::NativeUiVmSession, expected: &str| {
            session
                .renderer()
                .host()
                .tree()
                .nodes()
                .any(|node| node.text == expected)
        };
        assert!(has_text(&session, "Alpha 0") && has_text(&session, "Beta 0"));

        click_named_native_button(&mut session, window, view, "Alpha 0", 1, now);
        assert!(has_text(&session, "Alpha 1") && has_text(&session, "Beta 0"));

        click_named_native_button(&mut session, window, view, "Reorder", 2, now);
        assert!(has_text(&session, "Alpha 1") && has_text(&session, "Beta 0"));

        click_named_native_button(&mut session, window, view, "Remove Beta", 3, now);
        assert!(has_text(&session, "Alpha 1") && !has_text(&session, "Beta 0"));

        click_named_native_button(&mut session, window, view, "Insert Beta", 4, now);
        assert!(has_text(&session, "Alpha 1") && has_text(&session, "Beta 0"));

        click_named_native_button(&mut session, window, view, "Replace Alpha", 5, now);
        assert!(has_text(&session, "Alpha 0") && has_text(&session, "Beta 0"));
        assert!(!has_text(&session, "Alpha 1"));
    }
}

#[test]
fn official_ui_mount_is_vm_jit_protocol_equivalent() {
    let workspace = UiTestWorkspace::create();
    let compiled = workspace.compile();
    let (vm_initial, vm_update) = ui_batches_for(compiled.module.clone(), RunMode::Vm);
    let (jit_initial, jit_update) = ui_batches_for(compiled.module, RunMode::Jit);

    assert_eq!(vm_initial.revision, 1);
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert!(vm_initial.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. } if text == "before"
    )));
    assert_eq!(vm_update.revision, 2);
    assert_eq!(vm_update.mutations, jit_update.mutations);
    assert_eq!(vm_update.mutations.len(), 2);
    assert!(vm_update.mutations.iter().all(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { .. } | vo_ui_protocol::Mutation::SetProperty { .. }
    )));
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. } if text == "after"
    )));
}

#[test]
fn official_ui_native_vm_jit_session_routes_clicks_through_desktop_host() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func App() ui.View {
	count := int64(0)
	return ui.Button("Count " + strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    for mode in [RunMode::Vm, RunMode::Jit] {
        let compiled = workspace.compile();
        let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
        let window = vo_app_protocol::GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let view = vo_app_protocol::GenerationalHandle {
            index: 2,
            generation: 1,
        };
        let now = std::time::Instant::now();
        let (mut session, started) = crate::NativeUiVmSession::start(
            vm,
            window,
            view,
            crate::NativeUiSessionConfig::default(),
            now,
        )
        .unwrap();
        assert_eq!(started.revision, 1);
        let update = click_first_native_button(&mut session, window, view, now);
        assert_eq!(update.revision, 2);
        assert_eq!(update.delivered_events, 1);
        assert_eq!(update.applied_frames, 1);
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "Count 1"));
    }
}

#[test]
fn official_uikit_accessibility_and_paint_goldens_match_vm_and_jit() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/kit"
	"github.com/vo-lang/ui/motion"
)
func App() ui.View {
	theme := kit.LightTheme()
	progress := motion.UseValue(42)
	return kit.Screen(theme, kit.Card(theme, ui.Gap(ui.Column(
		kit.Heading(theme, "Account"),
		kit.ValidatedTextField(theme, "Display name", "Ada", "Your name", true, false, "Name is required", func(event ui.Event) {}),
		kit.ToggleField(theme, "Email updates", true, false, func(event ui.Event) {}),
		kit.MotionProgress(theme, progress, 100),
		kit.FormActions(theme, kit.PrimaryButton(theme, "Save", false, func(event ui.Event) {})),
	), theme.Space * 2)))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let mut snapshots = Vec::new();
    for mode in [RunMode::Vm, RunMode::Jit] {
        let vm = build_native_gui_vm_for_mode(compiled.clone(), mode).unwrap();
        let window = vo_app_protocol::GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let view = vo_app_protocol::GenerationalHandle {
            index: 2,
            generation: 1,
        };
        let (session, _) = crate::NativeUiVmSession::start(
            vm,
            window,
            view,
            crate::NativeUiSessionConfig::default(),
            std::time::Instant::now(),
        )
        .unwrap();
        let host = session.renderer().host();
        let mut measurer = vo_ui_layout::ApproximateTextMeasurer;
        let layout = vo_ui_layout::compute_layout(
            host.tree(),
            vo_ui_layout::Size::new(760.0, 520.0),
            vo_ui_layout::LayoutLimits::default(),
            &mut measurer,
        )
        .unwrap();
        let accessibility = host
            .build_accessibility_tree(&layout, vo_ui_accessibility::AccessibilityLimits::default())
            .unwrap();
        let paint = host
            .build_paint_scene(&layout, vo_ui_paint::PaintLimits::default())
            .unwrap();
        snapshots.push((
            vo_ui_golden::accessibility_snapshot(&accessibility),
            vo_ui_golden::paint_snapshot(&paint),
        ));
    }
    assert_eq!(snapshots[0], snapshots[1]);
    assert_eq!(
        snapshots[0].0,
        include_str!("../../../../ui/testdata/goldens/uikit.accessibility.txt")
    );
    assert_eq!(
        snapshots[0].1,
        include_str!("../../../../ui/testdata/goldens/uikit.paint.txt")
    );
}

#[test]
fn official_uikit_portable_menu_is_vm_jit_protocol_equivalent() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/kit"
)
func App() ui.View {
	theme := kit.LightTheme()
	status := "Ready"
	return ui.Column(
		kit.MenuBar(theme, "Application menu",
			kit.MenuAction(theme, "New", false, func(event ui.Event) { status = "Created" }),
			kit.MenuToggleAction(theme, "Auto save", true, false, func(event ui.Event) {}),
		),
		kit.Body(theme, status),
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let (vm_initial, vm_update) = ui_single_event_batches_for(
        compiled.module.clone(),
        RunMode::Vm,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    let (jit_initial, jit_update) = ui_single_event_batches_for(
        compiled.module,
        RunMode::Jit,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_update.mutations, jit_update.mutations);
    for expected in ["menubar", "menuitem", "menuitemcheckbox"] {
        assert!(vm_initial.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::ROLE
                    && property.value == vo_ui_core::Value::Text(expected.to_string())
        )));
    }
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. } if text == "Created"
    )));
}

#[test]
fn official_ui_system_request_suspends_one_goroutine_and_commits_response() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"github.com/vo-lang/ui"
	uisystem "github.com/vo-lang/ui/system"
)
func App() ui.View {
	status := ui.UseStringState("waiting")
	started := ui.UseBoolState(false)
	if !ui.BoolStateValue(started) {
		ui.SetBoolState(started, true)
		go func() {
			value, present, err := uisystem.ReadClipboardText()
			if err != nil { ui.SetStringState(status, err.Error())
			} else if present { ui.SetStringState(status, value)
			} else { ui.SetStringState(status, "empty") }
			ui.Invalidate()
		}()
	}
	return ui.Text(ui.StringStateValue(status))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
    );
    for mode in [RunMode::Vm, RunMode::Jit] {
        let compiled = workspace.compile();
        let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
        let window = vo_app_protocol::GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let view = vo_app_protocol::GenerationalHandle {
            index: 2,
            generation: 1,
        };
        let now = std::time::Instant::now();
        let (mut session, started) = crate::NativeUiVmSession::start(
            vm,
            window,
            view,
            crate::NativeUiSessionConfig::default(),
            now,
        )
        .unwrap();
        assert_eq!(started.revision, 1);
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "waiting"));

        let requests = session.take_system_requests().unwrap();
        assert_eq!(requests.len(), 1);
        let decoded = vo_ui_system::decode_system_request(
            &requests[0].frame,
            vo_ui_system::SystemLimits::default(),
        )
        .unwrap();
        assert!(matches!(
            decoded.request,
            vo_ui_system::SystemRequest::ReadClipboard(vo_ui_system::ClipboardFormat::Text)
        ));
        let response = vo_ui_system::encode_system_response(
            &vo_ui_system::SystemResponseEnvelope {
                request_id: requests[0].request_id,
                response: vo_ui_system::SystemResponse::Clipboard(Some(
                    vo_ui_system::ClipboardContent::Text("copied".to_string()),
                )),
            },
            vo_ui_system::SystemLimits::default(),
        )
        .unwrap();
        let report = session
            .complete_system_request(&requests[0], response, now)
            .unwrap();
        assert_eq!(report.completed_system_requests, 1);
        assert_eq!(report.revision, 2);
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "copied"));
    }
}

#[test]
fn official_ui_file_drag_suspends_one_goroutine_across_vm_and_jit() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"github.com/vo-lang/ui"
	uisystem "github.com/vo-lang/ui/system"
)
func App() ui.View {
	status := ui.UseStringState("waiting")
	started := ui.UseBoolState(false)
	if !ui.BoolStateValue(started) {
		ui.SetBoolState(started, true)
		go func() {
			err := uisystem.BeginFileDrag(
				[]string{"/tmp/alpha.vo", "/tmp/beta.vo"},
				uisystem.FileDragOptions{Mode: uisystem.FileDragMove, Preview: "/tmp/preview.png"},
			)
			if err != nil { ui.SetStringState(status, err.Error())
			} else { ui.SetStringState(status, "dragging") }
			ui.Invalidate()
		}()
	}
	return ui.Text(ui.StringStateValue(status))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
    );
    for mode in [RunMode::Vm, RunMode::Jit] {
        let compiled = workspace.compile();
        let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
        let window = vo_app_protocol::GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let view = vo_app_protocol::GenerationalHandle {
            index: 2,
            generation: 1,
        };
        let now = std::time::Instant::now();
        let (mut session, started) = crate::NativeUiVmSession::start(
            vm,
            window,
            view,
            crate::NativeUiSessionConfig::default(),
            now,
        )
        .unwrap();
        assert_eq!(started.revision, 1);
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "waiting"));

        let requests = session.take_system_requests().unwrap();
        assert_eq!(requests.len(), 1);
        let decoded = vo_ui_system::decode_system_request(
            &requests[0].frame,
            vo_ui_system::SystemLimits::default(),
        )
        .unwrap();
        let vo_ui_system::SystemRequest::BeginFileDrag(request) = decoded.request else {
            panic!("expected native file drag request");
        };
        assert_eq!(request.paths, ["/tmp/alpha.vo", "/tmp/beta.vo"]);
        assert_eq!(request.preview.as_deref(), Some("/tmp/preview.png"));
        assert_eq!(request.mode, vo_ui_system::FileDragMode::Move);

        let response = vo_ui_system::encode_system_response(
            &vo_ui_system::SystemResponseEnvelope {
                request_id: requests[0].request_id,
                response: vo_ui_system::SystemResponse::Complete,
            },
            vo_ui_system::SystemLimits::default(),
        )
        .unwrap();
        let report = session
            .complete_system_request(&requests[0], response, now)
            .unwrap();
        assert_eq!(report.completed_system_requests, 1);
        assert_eq!(report.revision, 2);
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "dragging"));
    }
}

#[test]
fn official_ui_native_vm_jit_reload_is_stateful_and_transactional() {
    const INITIAL: &str = r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) { count++ })
}
func App() ui.View { return ui.Column(ui.Key(Counter("Count"), "counter")) }
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#;
    const UPDATED: &str = r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) { count++ })
}
func updatedLabel() string { return "Value" }
func App() ui.View { return ui.Column(ui.Key(Counter(updatedLabel()), "counter")) }
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#;
    const PANICKING: &str = r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	panic("candidate failed")
	return ui.Text("unreachable")
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#;
    let workspace = UiTestWorkspace::create_with_main(INITIAL);
    for mode in [RunMode::Vm, RunMode::Jit] {
        std::fs::write(workspace.app().join("main.vo"), INITIAL).unwrap();
        let compiled = workspace.compile();
        let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
        let window = vo_app_protocol::GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let view = vo_app_protocol::GenerationalHandle {
            index: 2,
            generation: 1,
        };
        let now = std::time::Instant::now();
        let (mut session, _) = crate::NativeUiVmSession::start(
            vm,
            window,
            view,
            crate::NativeUiSessionConfig::default(),
            now,
        )
        .unwrap();
        assert_eq!(
            click_first_native_button(&mut session, window, view, now).revision,
            2
        );

        std::fs::write(workspace.app().join("main.vo"), UPDATED).unwrap();
        let updated = workspace.compile();
        let prepared = prepare_native_gui_reload_for_mode(updated, mode).unwrap();
        let previous_epoch = session.renderer().host().session_epoch();
        let reloaded = session.reload(prepared, now).unwrap();
        assert_eq!(reloaded.revision, 1);
        assert!(session.renderer().host().session_epoch() > previous_epoch);
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "Value 1"));

        std::fs::write(workspace.app().join("main.vo"), PANICKING).unwrap();
        let panicking = workspace.compile();
        let prepared = prepare_native_gui_reload_for_mode(panicking, mode).unwrap();
        let failed = session.reload(prepared, now);
        assert!(
            matches!(
                failed,
                Err(crate::NativeUiSessionError::Vm(_))
                    | Err(crate::NativeUiSessionError::Terminal(
                        SchedulingOutcome::Panicked
                    ))
            ),
            "unexpected reload result: {failed:?}"
        );
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "Value 1"));
        assert_eq!(
            click_first_native_button(&mut session, window, view, now).revision,
            2
        );
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "Value 2"));
    }
}

#[test]
fn official_ui_native_vm_jit_session_commits_goroutine_invalidation() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func App() ui.View {
	count := ui.UseIntState(0)
	started := ui.UseBoolState(false)
	if !ui.BoolStateValue(started) {
		ui.SetBoolState(started, true)
		go func() {
			ui.SetIntState(count, ui.IntStateValue(count)+1)
			ui.Invalidate()
		}()
	}
	return ui.Text("Count " + strconv.FormatInt(ui.IntStateValue(count), 10))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    for mode in [RunMode::Vm, RunMode::Jit] {
        let compiled = workspace.compile();
        let vm = build_native_gui_vm_for_mode(compiled, mode).unwrap();
        let window = vo_app_protocol::GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let view = vo_app_protocol::GenerationalHandle {
            index: 2,
            generation: 1,
        };
        let now = std::time::Instant::now();
        let (session, started) = crate::NativeUiVmSession::start(
            vm,
            window,
            view,
            crate::NativeUiSessionConfig::default(),
            now,
        )
        .unwrap();
        assert_eq!(started.pending_timers, 0);
        assert_eq!(started.woken_timers, 0);
        assert_eq!(started.delivered_events, 1);
        assert_eq!(started.applied_frames, 2);
        assert_eq!(started.revision, 2);
        assert!(session
            .renderer()
            .host()
            .tree()
            .nodes()
            .any(|node| node.text == "Count 1"));
    }
}

#[test]
fn official_motion_tween_advances_on_host_timers_in_vm_and_jit() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/motion"
	"time"
)
func App() ui.View {
	value := motion.UseValue(0)
	status := "idle"
	if value.IsRunning() { status = "running" }
	return ui.Column(ui.Text(status), ui.Padding(ui.Button("Animate", func(event ui.Event) {
		if value.IsRunning() { value.Stop() } else {
			value.AnimateTo(80, motion.Tween{Duration: 100 * time.Millisecond, Curve: motion.Linear})
		}
	}), value.Current()))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let mut final_values = Vec::new();
    for (mode, step_millis) in [RunMode::Vm, RunMode::Jit]
        .into_iter()
        .flat_map(|mode| [7_u64, 20, 37].into_iter().map(move |step| (mode, step)))
    {
        engine().ensure_toolchain_host_installed();
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                ..vo_vm::JitConfig::default()
            })
            .expect("JIT should initialize"),
        };
        register_externs(&mut vm, &compiled.module).unwrap();
        let extensions = load_extensions(&compiled.extensions).unwrap();
        vm.load_verified_with_extensions(compiled.module.clone(), extensions)
            .unwrap();
        let clock = vo_runtime::io::ManualClock::new(1_700_000_000_000_000_000);
        vm.set_manual_clock(clock.clone()).unwrap();
        let window = vo_app_protocol::GenerationalHandle {
            index: 1,
            generation: 1,
        };
        let view = vo_app_protocol::GenerationalHandle {
            index: 2,
            generation: 1,
        };
        let started = std::time::Instant::now();
        let (mut session, _) = crate::NativeUiVmSession::start(
            vm,
            window,
            view,
            crate::NativeUiSessionConfig::default(),
            started,
        )
        .unwrap();
        let clicked = click_first_native_button(&mut session, window, view, started);
        assert_eq!(clicked.revision, 2);
        let padding = |session: &crate::NativeUiVmSession| {
            session
                .renderer()
                .host()
                .tree()
                .nodes()
                .find_map(|node| {
                    node.properties
                        .get(&vo_ui_core::PropertyId::PADDING)
                        .cloned()
                })
                .expect("animated padding")
        };
        clock.advance(std::time::Duration::from_millis(20)).unwrap();
        session
            .pump(started + std::time::Duration::from_millis(20))
            .unwrap();
        let intermediate = padding(&session);
        assert!(
            matches!(intermediate, vo_ui_core::Value::Length(vo_ui_core::Length::Px(value)) if value > 0.0 && value < 80.0)
        );
        click_first_native_button(
            &mut session,
            window,
            view,
            started + std::time::Duration::from_millis(20),
        );
        clock
            .advance(std::time::Duration::from_millis(100))
            .unwrap();
        session
            .pump(started + std::time::Duration::from_millis(120))
            .unwrap();
        assert_eq!(
            padding(&session),
            intermediate,
            "cancelled animation must retain its last value"
        );
        click_first_native_button(
            &mut session,
            window,
            view,
            started + std::time::Duration::from_millis(120),
        );
        let mut animation_frames = 0;
        for tick in 1..=240_u64.div_ceil(step_millis) {
            clock
                .advance(std::time::Duration::from_millis(step_millis))
                .unwrap();
            let report = session
                .pump(started + std::time::Duration::from_millis(120 + tick * step_millis))
                .unwrap();
            animation_frames += report.applied_frames;
        }
        assert!(
            animation_frames >= 2,
            "motion worker should publish multiple coalesced frames at {step_millis}ms steps; got {animation_frames}"
        );
        let value = session
            .renderer()
            .host()
            .tree()
            .nodes()
            .find_map(|node| {
                node.properties
                    .get(&vo_ui_core::PropertyId::PADDING)
                    .cloned()
            })
            .expect("animated padding property");
        assert_eq!(
            value,
            vo_ui_core::Value::Length(vo_ui_core::Length::Px(80.0))
        );
        if mode == RunMode::Jit {
            assert!(
                session.vm().jit_execution_stats().function_entries > 0,
                "JIT must enter compiled code"
            );
        }
        final_values.push(value);
    }
    assert!(final_values.windows(2).all(|pair| pair[0] == pair[1]));
}

#[test]
fn official_ui_state_cells_drive_only_dependent_direct_slots() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	name := ui.UseStringState("before")
	locked := ui.UseBoolState(false)
	return ui.Column(
		ui.TextInput(ui.StringStateValue(name), "Type", func(event ui.Event) {
			ui.SetStringState(name, event.Text)
		}),
		ui.Disabled(ui.Button("Locked", func(event ui.Event) {}), ui.BoolStateValue(locked)),
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let artifact = compiled
        .module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .expect("static UI component artifact");
    let component = vo_ui_artifact::decode_component_artifact(
        &artifact.payload,
        vo_ui_artifact::ArtifactLimits::default(),
        vo_ui_plan::PlanLimits::default(),
    )
    .unwrap();
    assert_eq!(component.mode, vo_ui_artifact::ExecutionMode::Direct);
    assert_eq!(component.states.len(), 2);
    assert!(component
        .states
        .iter()
        .all(|state| state.initializer_func.is_some()));
    assert!(component
        .slots
        .iter()
        .all(|binding| binding.evaluator_func.is_some()));

    let (vm_initial, vm_updates) =
        ui_input_batches_for(compiled.module.clone(), RunMode::Vm, &["after", "again"]);
    let vm_profile = vo_ui_vm::reactive_profile();
    let (jit_initial, jit_updates) =
        ui_input_batches_for(compiled.module, RunMode::Jit, &["after", "again"]);
    let jit_profile = vo_ui_vm::reactive_profile();
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_updates.len(), jit_updates.len());
    assert_eq!(vm_profile, jit_profile);
    assert_eq!(vm_profile.changed_state_writes, 2);
    assert_eq!(vm_profile.root_evaluations, 1);
    assert_eq!(vm_profile.direct_update_turns, 2);
    assert_eq!(vm_profile.scheduled_bindings, 2);
    assert_eq!(vm_profile.evaluator_calls, 2);
    assert_eq!(vm_profile.submitted_slots, 2);
    assert_eq!(vm_profile.emitted_revisions, 3);
    assert_eq!(vm_profile.no_op_updates, 0);
    assert_eq!(
        vm_profile.emitted_mutations,
        (vm_initial.mutations.len()
            + vm_updates
                .iter()
                .map(|batch| batch.mutations.len())
                .sum::<usize>()) as u64
    );
    for (vm, jit) in vm_updates.iter().zip(&jit_updates) {
        assert_eq!(vm.revision, jit.revision);
        assert_eq!(vm.mutations, jit.mutations);
    }
    assert_eq!(vm_updates[0].mutations.len(), 1);
    assert!(matches!(
        &vm_updates[0].mutations[0],
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::VALUE
                && property.value == vo_ui_core::Value::Text("after".to_string())
    ));
    assert_eq!(vm_updates[1].revision, 3);
    assert!(matches!(
        &vm_updates[1].mutations[0],
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::VALUE
                && property.value == vo_ui_core::Value::Text("again".to_string())
    ));
}

#[test]
fn official_ui_ordinary_local_state_is_automatically_persistent() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
import "strconv"
func App() ui.View {
	count := int64(0)
	return ui.Button("Count "+strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let artifact = compiled
        .module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .expect("automatic local state should retain a direct component artifact");
    let component = vo_ui_artifact::decode_component_artifact(
        &artifact.payload,
        vo_ui_artifact::ArtifactLimits::default(),
        vo_ui_plan::PlanLimits::default(),
    )
    .unwrap();
    assert_eq!(component.mode, vo_ui_artifact::ExecutionMode::Direct);
    assert_eq!(component.states.len(), 1);

    let (vm_initial, vm_update) = ui_single_event_batches_for(
        compiled.module.clone(),
        RunMode::Vm,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    let (jit_initial, jit_update) = ui_single_event_batches_for(
        compiled.module,
        RunMode::Jit,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_update.mutations, jit_update.mutations);
    assert!(vm_initial.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. } if text == "Count 0"
    )));
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. } if text == "Count 1"
    )));
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::ACCESSIBLE_NAME
                && property.value == vo_ui_core::Value::Text("Count 1".to_string())
    )));
}

#[test]
fn official_ui_automatic_cells_cover_all_scalar_state_kinds() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
import "strconv"
func App() ui.View {
	name := "before"
	locked := false
	count := int64(0)
	width := 1.5
	return ui.Column(
		ui.TextInput(name, "Name", func(event ui.Event) {}),
		ui.Disabled(ui.Button(strconv.FormatInt(count, 10), func(event ui.Event) {
			name = "after"
			locked = true
			count += 2
			width *= 2
		}), locked),
		ui.Width(ui.Box(), width),
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let artifact = compiled
        .module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .unwrap();
    let component = vo_ui_artifact::decode_component_artifact(
        &artifact.payload,
        vo_ui_artifact::ArtifactLimits::default(),
        vo_ui_plan::PlanLimits::default(),
    )
    .unwrap();
    assert_eq!(component.mode, vo_ui_artifact::ExecutionMode::Direct);
    assert_eq!(component.states.len(), 4);

    let (vm_initial, vm_update) = ui_single_event_batches_for(
        compiled.module.clone(),
        RunMode::Vm,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    let (jit_initial, jit_update) = ui_single_event_batches_for(
        compiled.module,
        RunMode::Jit,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_update.mutations, jit_update.mutations);
    for (property_id, expected) in [
        (
            vo_ui_core::PropertyId::VALUE,
            vo_ui_core::Value::Text("after".to_string()),
        ),
        (
            vo_ui_core::PropertyId::DISABLED,
            vo_ui_core::Value::Bool(true),
        ),
        (
            vo_ui_core::PropertyId::WIDTH,
            vo_ui_core::Value::Length(vo_ui_core::Length::Px(3.0)),
        ),
    ] {
        assert!(vm_update.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == property_id && property.value == expected
        )));
    }
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. } if text == "2"
    )));
}

#[test]
fn official_ui_automatic_local_state_survives_library_fallback() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
import "strconv"
func Card(child ui.View) ui.View {
	return ui.Padding(ui.Background(child, 0xffffffff), 12)
}
func App() ui.View {
	count := int64(0)
	return Card(ui.Button("Count "+strconv.FormatInt(count, 10), func(event ui.Event) {
		count++
	}))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .is_none());
    let (vm_initial, vm_update) = ui_single_event_batches_for(
        compiled.module.clone(),
        RunMode::Vm,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    let (jit_initial, jit_update) = ui_single_event_batches_for(
        compiled.module,
        RunMode::Jit,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_update.mutations, jit_update.mutations);
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. } if text == "Count 1"
    )));
}

#[cfg(all(feature = "aot-native", feature = "aot-wasm"))]
#[test]
fn official_ui_and_system_externs_lower_to_native_and_core_wasm_aot() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"github.com/vo-lang/ui"
	uisystem "github.com/vo-lang/ui/system"
)
func App() ui.View {
	label := "before"
	go func() {
		value, present, err := uisystem.ReadClipboardText()
		if err == nil && present && value != "" { label = value }
		_ = uisystem.BeginFileDrag([]string{"/tmp/demo.vo"}, uisystem.DefaultFileDragOptions())
	}()
	return ui.TextInput(label, "Name", func(event ui.Event) { label = event.Text })
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let image = engine().compile_wasm_aot_image(&compiled, &target).unwrap();
    assert!(image.bytes.starts_with(b"\0asm"));
    assert_eq!(
        image.manifest.target_triple,
        vo_engine::WASM32_UNKNOWN_UNKNOWN
    );
    let native_target = vo_engine::TargetSpec::host().unwrap();
    let object = engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap();
    assert!(!object.bytes.is_empty());
    assert_eq!(object.target_triple, native_target.triple());
}

#[cfg(all(feature = "aot-native", feature = "aot-wasm"))]
#[test]
fn component_bundle_evaluators_lower_to_native_and_core_wasm_aot() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"strconv"
	"github.com/vo-lang/ui"
)
func Counter(label string) ui.View {
	count := int64(0)
	return ui.Button(label+" "+strconv.FormatInt(count, 10), func(event ui.Event) { count++ })
}
func App() ui.View { return ui.Column(Counter("A"), ui.Key(Counter("B"), "b")) }
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .module()
        .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
        .is_some());

    let wasm_target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let image = engine()
        .compile_wasm_aot_image(&compiled, &wasm_target)
        .unwrap();
    let artifacts = vo_wasm_aot::decode_wasm_aot_artifacts(&image.bytes).unwrap();
    assert!(artifacts.iter().any(|artifact| {
        artifact.name == vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME
            && artifact.version == vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_VERSION
    }));

    let native_target = vo_engine::TargetSpec::host().unwrap();
    let object = engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap();
    assert!(!object.bytes.is_empty());
    assert!(object.functions.len() >= compiled.module.module().functions.len());
}

#[cfg(all(feature = "aot-native", feature = "aot-wasm"))]
#[test]
fn component_dynamic_scopes_lower_to_native_and_core_wasm_aot() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
func Child(label string) ui.View { count := int64(0); return ui.Button(label, func(event ui.Event) { count++ }) }
func App() ui.View {
	reversed := false
	if reversed { return ui.Column(ui.Key(Child("B"), "b"), ui.Key(Child("A"), "a")) }
	return ui.Column(ui.Key(Child("A"), "a"), ui.Key(Child("B"), "b"))
}
func main() { if err := ui.Mount(App); err != nil { panic(err.Error()) } }
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .module()
        .artifact(vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME)
        .is_none());
    for expected in ["runtimeEnterComponent", "runtimeExitComponent"] {
        assert!(compiled.module.module().externs.iter().any(|external| {
            vo_common_core::extern_key::decode_extern_name(&external.name).is_ok_and(|key| {
                key.package() == "github.com/vo-lang/ui" && key.function() == expected
            })
        }));
    }

    let wasm_target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let image = engine()
        .compile_wasm_aot_image(&compiled, &wasm_target)
        .unwrap();
    assert!(image.bytes.starts_with(b"\0asm"));

    let native_target = vo_engine::TargetSpec::host().unwrap();
    let object = engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap();
    assert!(!object.bytes.is_empty());
    assert!(object.functions.len() >= compiled.module.module().functions.len());
}

#[cfg(all(feature = "aot-native", feature = "aot-wasm"))]
#[test]
fn official_motion_and_gesture_packages_lower_to_native_and_core_wasm_aot() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/gesture"
	"github.com/vo-lang/ui/motion"
)
func App() ui.View {
	value := motion.UseValue(0)
	drag := gesture.UseDrag()
	return gesture.BindDrag(ui.Padding(ui.Box(), value.Current()), drag, gesture.DefaultDragOptions(), func(snapshot gesture.DragSnapshot) {
		if snapshot.Phase == gesture.Ended {
			value.SpringTo(0, motion.DefaultSpring())
		} else {
			value.AnimateTo(snapshot.DeltaX, motion.DefaultTween())
		}
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let wasm_target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let image = engine()
        .compile_wasm_aot_image(&compiled, &wasm_target)
        .unwrap();
    assert!(image.bytes.starts_with(b"\0asm"));
    let native_target = vo_engine::TargetSpec::host().unwrap();
    let object = engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap();
    assert!(!object.bytes.is_empty());
}

fn ui_key_update_for(
    module: Arc<vo_runtime::bytecode::LoadedModule>,
    mode: RunMode,
    key: &str,
    code: &str,
    modifiers: vo_ui_core::EventModifiers,
    repeat: bool,
    composing: bool,
) -> vo_ui_protocol::MutationBatch {
    let mut vm = match mode {
        RunMode::Vm => Vm::new(),
        RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        })
        .unwrap(),
    };
    register_externs(&mut vm, &module).unwrap();
    vm.load_verified(module).unwrap();
    assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
    let limits = vo_ui_protocol::ProtocolLimits::default();
    let initial = vo_ui_protocol::decode_batch(&vm.take_host_output().unwrap(), limits).unwrap();
    let (target, handler) = initial
        .mutations
        .iter()
        .find_map(|mutation| match mutation {
            vo_ui_protocol::Mutation::Listen { id, listener }
                if listener.event == vo_ui_core::EventType::KEY_DOWN =>
            {
                Some((*id, listener.handler))
            }
            _ => None,
        })
        .unwrap();
    let event = vo_ui_protocol::EventEnvelope::new(
        initial.session_epoch,
        vo_ui_core::UiEvent {
            handler,
            event: vo_ui_core::EventType::KEY_DOWN,
            target,
            sequence: 1,
            payload: vo_ui_core::EventPayload::Key(vo_ui_core::KeyEventData {
                key: key.to_string(),
                code: code.to_string(),
                modifiers,
                repeat,
                composing,
            }),
        },
    );
    let pending = vm.take_pending_host_events();
    assert!(vm.wake_host_event_with_data(
        pending[0].key,
        vo_ui_protocol::encode_event(&event, limits).unwrap()
    ));
    assert_eq!(
        vm.run_scheduled().unwrap(),
        SchedulingOutcome::SuspendedForHostEvents
    );
    vo_ui_protocol::decode_batch(&vm.take_host_output().unwrap(), limits).unwrap()
}

#[test]
fn official_ui_key_event_fields_are_vm_jit_equivalent() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
var observed = "waiting"
func App() ui.View {
	return ui.OnKeyDown(
		ui.TextInput(observed, "Key", func(event ui.Event) {}),
		func(event ui.Event) {
			observed = event.Key + ":" + event.Code
			if event.Repeat { observed += ":repeat" }
			if event.Composing { observed += ":ime" }
		},
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let key_modifiers = vo_ui_core::EventModifiers {
        shift: true,
        ..vo_ui_core::EventModifiers::default()
    };
    let vm = ui_key_update_for(
        compiled.module.clone(),
        RunMode::Vm,
        "Enter",
        "NumpadEnter",
        key_modifiers,
        true,
        true,
    );
    let jit = ui_key_update_for(
        compiled.module,
        RunMode::Jit,
        "Enter",
        "NumpadEnter",
        key_modifiers,
        true,
        true,
    );
    assert_eq!(vm.mutations, jit.mutations);
    assert!(vm.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::VALUE
                && property.value
                    == vo_ui_core::Value::Text("Enter:NumpadEnter:repeat:ime".to_string())
    )));
}

#[test]
fn official_ui_command_shortcuts_match_shifted_letter_case_in_vm_and_jit() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import (
	"github.com/vo-lang/ui"
	"github.com/vo-lang/ui/commands"
)
var observed = "waiting"
func App() ui.View {
	input := ui.TextInput(observed, "Command", func(event ui.Event) {})
	return commands.Bind(input, commands.New("palette", "Palette", commands.Key("p", ui.ModifierMeta|ui.ModifierShift), func() {
		observed = "matched"
	}))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let modifiers = vo_ui_core::EventModifiers {
        shift: true,
        meta: true,
        ..vo_ui_core::EventModifiers::default()
    };
    for mode in [RunMode::Vm, RunMode::Jit] {
        let batch = ui_key_update_for(
            compiled.module.clone(),
            mode,
            "P",
            "KeyP",
            modifiers,
            false,
            false,
        );
        assert!(batch.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == vo_ui_core::PropertyId::VALUE
                    && property.value
                        == vo_ui_core::Value::Text("matched".to_string())
        )));
    }
}

fn ui_composition_update_for(
    module: Arc<vo_runtime::bytecode::LoadedModule>,
    mode: RunMode,
) -> vo_ui_protocol::MutationBatch {
    let mut vm = match mode {
        RunMode::Vm => Vm::new(),
        RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
            call_threshold: 1,
            loop_threshold: 1,
            debug_ir: false,
            ..vo_vm::JitConfig::default()
        })
        .unwrap(),
    };
    register_externs(&mut vm, &module).unwrap();
    vm.load_verified(module).unwrap();
    assert_eq!(vm.run().unwrap(), SchedulingOutcome::SuspendedForHostEvents);
    let limits = vo_ui_protocol::ProtocolLimits::default();
    let initial = vo_ui_protocol::decode_batch(&vm.take_host_output().unwrap(), limits).unwrap();
    let (target, handler) = initial
        .mutations
        .iter()
        .find_map(|mutation| match mutation {
            vo_ui_protocol::Mutation::Listen { id, listener }
                if listener.event == vo_ui_core::EventType::COMPOSITION_UPDATE =>
            {
                Some((*id, listener.handler))
            }
            _ => None,
        })
        .unwrap();
    let event = vo_ui_protocol::EventEnvelope::new(
        initial.session_epoch,
        vo_ui_core::UiEvent {
            handler,
            event: vo_ui_core::EventType::COMPOSITION_UPDATE,
            target,
            sequence: 1,
            payload: vo_ui_core::EventPayload::Composition(vo_ui_core::CompositionEventData {
                text: "拼音".to_string(),
                selection_start_utf16: 1,
                selection_length_utf16: 2,
            }),
        },
    );
    let pending = vm.take_pending_host_events();
    assert!(vm.wake_host_event_with_data(
        pending[0].key,
        vo_ui_protocol::encode_event(&event, limits).unwrap()
    ));
    assert_eq!(
        vm.run_scheduled().unwrap(),
        SchedulingOutcome::SuspendedForHostEvents
    );
    vo_ui_protocol::decode_batch(&vm.take_host_output().unwrap(), limits).unwrap()
}

#[test]
fn official_ui_composition_selection_is_vm_jit_equivalent() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
import "strconv"
var observed = "waiting"
func App() ui.View {
	return ui.OnCompositionUpdate(
		ui.TextInput(observed, "IME", func(event ui.Event) {}),
		func(event ui.Event) {
			observed = event.Text + ":" +
				strconv.FormatInt(event.SelectionStartUTF16, 10) + ":" +
				strconv.FormatInt(event.SelectionLengthUTF16, 10)
		},
	)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    let vm = ui_composition_update_for(compiled.module.clone(), RunMode::Vm);
    let jit = ui_composition_update_for(compiled.module, RunMode::Jit);
    assert_eq!(vm.mutations, jit.mutations);
    assert!(vm.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::VALUE
                && property.value
                    == vo_ui_core::Value::Text("拼音:1:2".to_string())
    )));
}

#[test]
fn official_kit_is_vm_jit_equivalent_through_generic_reconciliation() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
import "github.com/vo-lang/ui/kit"
var name = "before"
func App() ui.View {
	theme := kit.LightTheme()
	return kit.Screen(theme, kit.Card(theme,
		ui.SelectionLengthUTF16(ui.SelectionStartUTF16(kit.ValidatedTextField(theme, "Name", name, "Type", true, false, "Name is required", func(event ui.Event) {
			name = event.Text
		}), 1), 2),
	))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .is_none());
    let (vm_initial, vm_update) = ui_batches_for(compiled.module.clone(), RunMode::Vm);
    let (jit_initial, jit_update) = ui_batches_for(compiled.module, RunMode::Jit);
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_update.mutations, jit_update.mutations);
    assert!(vm_initial.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::RADIUS
                && property.value == vo_ui_core::Value::Length(vo_ui_core::Length::Px(10.0))
    )));
    for (id, value) in [
        (
            vo_ui_core::PropertyId::REQUIRED,
            vo_ui_core::Value::Bool(true),
        ),
        (
            vo_ui_core::PropertyId::INVALID,
            vo_ui_core::Value::Bool(true),
        ),
        (
            vo_ui_core::PropertyId::ACCESSIBLE_DESCRIPTION,
            vo_ui_core::Value::Text("Name is required".to_string()),
        ),
        (
            vo_ui_core::PropertyId::SELECTION_START_UTF16,
            vo_ui_core::Value::I64(1),
        ),
        (
            vo_ui_core::PropertyId::SELECTION_LENGTH_UTF16,
            vo_ui_core::Value::I64(2),
        ),
    ] {
        assert!(vm_initial.mutations.iter().any(|mutation| matches!(
            mutation,
            vo_ui_protocol::Mutation::SetProperty { property, .. }
                if property.id == id && property.value == value
        )));
    }
    assert!(vm_initial.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::ROLE
                && property.value == vo_ui_core::Value::Text("alert".to_string())
    )));
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::VALUE
                && property.value == vo_ui_core::Value::Text("after".to_string())
    )));
}

#[test]
fn official_ui_keys_preserve_vm_jit_nodes_across_reordering() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
var flipped = false
func App() ui.View {
	a := ui.Key(ui.Button("A", func(event ui.Event) { flipped = !flipped }), "a")
	b := ui.Key(ui.Button("B", func(event ui.Event) { flipped = !flipped }), "b")
	if flipped { return ui.Column(b, a) }
	return ui.Column(a, b)
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .is_none());
    let (vm_initial, vm_update) = ui_single_event_batches_for(
        compiled.module.clone(),
        RunMode::Vm,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    let (jit_initial, jit_update) = ui_single_event_batches_for(
        compiled.module,
        RunMode::Jit,
        vo_ui_core::EventType::CLICK,
        vo_ui_core::EventPayload::None,
    );
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_update.mutations, jit_update.mutations);

    let buttons = vm_initial
        .mutations
        .iter()
        .filter_map(|mutation| match mutation {
            vo_ui_protocol::Mutation::Create {
                id,
                kind: vo_ui_protocol::NodeKind::Element(vo_ui_core::Primitive::Button),
            } => Some(*id),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(buttons.len(), 2);
    assert!(vm_update.mutations.iter().all(|mutation| !matches!(
        mutation,
        vo_ui_protocol::Mutation::Create { .. } | vo_ui_protocol::Mutation::Delete { .. }
    )));
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::InsertBefore {
            child,
            before: Some(before),
            ..
        } if *child == buttons[1] && *before == buttons[0]
    )));
}

#[test]
fn official_kit_virtual_list_bounds_vm_jit_materialization() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
import "github.com/vo-lang/ui/kit"
import "strconv"
var offset = 0.0
func App() ui.View {
	window := kit.VisibleRange(100, 20, 200, 100, 2)
	if window.Start != 8 || window.End != 18 { panic("invalid virtual range") }
	return kit.VirtualList(100, 20, offset, 100, 2, func(index int64) ui.View {
		return ui.Text("Row "+strconv.FormatInt(index, 10))
	}, func(event ui.Event) { offset = event.Y })
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .is_none());
    let payload = vo_ui_core::EventPayload::Scroll(vo_ui_core::ScrollEventData {
        x: 0.0,
        y: 200.0,
        delta_x: 0.0,
        delta_y: 200.0,
        unit: vo_ui_core::ScrollUnit::Pixel,
        modifiers: vo_ui_core::EventModifiers::default(),
    });
    let (vm_initial, vm_update) = ui_single_event_batches_for(
        compiled.module.clone(),
        RunMode::Vm,
        vo_ui_core::EventType::SCROLL,
        payload.clone(),
    );
    let (jit_initial, jit_update) = ui_single_event_batches_for(
        compiled.module,
        RunMode::Jit,
        vo_ui_core::EventType::SCROLL,
        payload,
    );
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_update.mutations, jit_update.mutations);
    assert_eq!(
        vm_initial
            .mutations
            .iter()
            .filter(|mutation| matches!(mutation, vo_ui_protocol::Mutation::SetText { .. }))
            .count(),
        8
    );
    assert_eq!(
        vm_update
            .mutations
            .iter()
            .filter(|mutation| matches!(mutation, vo_ui_protocol::Mutation::SetText { .. }))
            .count(),
        10
    );
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::SCROLL_Y
                && property.value == vo_ui_core::Value::F64(200.0)
    )));
}

#[cfg(all(feature = "aot-native", feature = "aot-wasm"))]
#[test]
fn official_data_application_compiles_and_bounds_vm_jit_materialization() {
    let workspace = UiTestWorkspace::create_with_main(include_str!(
        "../../../../ui/showcases/data-application/main.vo"
    ));
    let compiled = workspace.compile();
    let vm = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
    let jit = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
    assert_eq!(vm.mutations, jit.mutations);
    let text_count = vm
        .mutations
        .iter()
        .filter(|mutation| matches!(mutation, vo_ui_protocol::Mutation::SetText { .. }))
        .count();
    assert!(
        text_count < 200,
        "virtual data app mounted {text_count} text nodes"
    );
    assert!(vm.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. } if text == "Member 0"
    )));
    assert!(vm.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::ROLE
                && property.value == vo_ui_core::Value::Text("grid".to_string())
    )));
    let wasm_target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let wasm = engine()
        .compile_wasm_aot_image(&compiled, &wasm_target)
        .unwrap();
    assert!(wasm.bytes.starts_with(b"\0asm"));
    let native_target = vo_engine::TargetSpec::host().unwrap();
    let native = engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap();
    assert!(!native.bytes.is_empty());
}

#[test]
fn official_e4_application_models_are_vm_jit_equivalent() {
    let workspace = UiTestWorkspace::create_with_main(include_str!(
        "../../../../ui/tests/application-platform/main.vo"
    ));
    let compiled = workspace.compile();
    for mode in [RunMode::Vm, RunMode::Jit] {
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            })
            .unwrap(),
        };
        register_externs(&mut vm, &compiled.module).unwrap();
        vm.load_verified(compiled.module.clone()).unwrap();
        assert_eq!(
            vm.run().unwrap(),
            SchedulingOutcome::Completed,
            "mode {mode:?}"
        );
    }
}

#[test]
fn official_e5_web_and_desktop_models_are_vm_jit_equivalent() {
    let workspace = UiTestWorkspace::create_with_main(include_str!(
        "../../../../ui/tests/web-desktop-product/main.vo"
    ));
    let compiled = workspace.compile();
    for mode in [RunMode::Vm, RunMode::Jit] {
        let mut vm = match mode {
            RunMode::Vm => Vm::new(),
            RunMode::Jit => Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: 1,
                loop_threshold: 1,
                debug_ir: false,
                ..vo_vm::JitConfig::default()
            })
            .unwrap(),
        };
        register_externs(&mut vm, &compiled.module).unwrap();
        vm.load_verified(compiled.module.clone()).unwrap();
        assert_eq!(
            vm.run().unwrap(),
            SchedulingOutcome::Completed,
            "mode {mode:?}"
        );
    }
}

#[cfg(all(feature = "aot-native", feature = "aot-wasm"))]
#[test]
fn browser_aot_rejects_server_authority() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui/web/server"
func main() { _ = server.NewAuthority(nil, nil, nil) }
"#,
    );
    let compiled = workspace.compile();
    let target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let error = engine()
        .compile_wasm_aot_image(&compiled, &target)
        .unwrap_err();
    assert!(
        error.to_string().contains("web/server authority"),
        "{error}"
    );
    let native_target = vo_engine::TargetSpec::host().unwrap();
    let native = engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap();
    assert!(!native.bytes.is_empty());
}

#[cfg(feature = "aot-wasm")]
#[test]
fn official_content_site_renders_distinct_useful_routes_and_web_aot() {
    let workspace = UiTestWorkspace::create_with_main(include_str!(
        "../../../../ui/showcases/content-site/main.vo"
    ));
    let compiled = workspace.compile();
    let metadata = vo_ui_web::DocumentMetadata {
        language: "en".to_string(),
        direction: "ltr".to_string(),
        title: "Volang Field Notes".to_string(),
        description: "Useful HTML".to_string(),
        ..vo_ui_web::DocumentMetadata::default()
    };
    let home = render_initial_ui_document_at(
        compiled.clone(),
        RunMode::Vm,
        "/",
        &metadata,
        vo_ui_web::SsrLimits::default(),
    )
    .unwrap();
    let article = render_initial_ui_document_at(
        compiled.clone(),
        RunMode::Jit,
        "/articles/wasm-aot",
        &metadata,
        vo_ui_web::SsrLimits::default(),
    )
    .unwrap();
    assert!(home.html.contains("zero JavaScript application code"));
    assert!(article
        .html
        .contains("Wasm AOT without a JavaScript framework"));
    assert!(!article.html.contains("zero JavaScript application code"));
    assert!(!article.activation.is_empty());
    let chunks = vo_ui_web::stream_document(&article, 1024).unwrap();
    assert!(chunks.len() > 1);
    assert_eq!(chunks.concat(), article.html);
    let target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    let image = engine().compile_wasm_aot_image(&compiled, &target).unwrap();
    assert!(image.bytes.starts_with(b"\0asm"));
}

#[cfg(all(feature = "aot-native", feature = "aot-wasm"))]
#[test]
fn official_advanced_packs_are_vm_jit_and_aot_equivalent() {
    let workspace = UiTestWorkspace::create_with_main(include_str!(
        "../../../../ui/tests/advanced-packs/main.vo"
    ));
    let compiled = workspace.compile();
    let vm_initial = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
    let jit_initial = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert!(vm_initial.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::Create {
            kind: vo_ui_protocol::NodeKind::Element(vo_ui_core::Primitive::Canvas),
            ..
        }
    )));
    assert!(vm_initial.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::GRAPHICS_PROGRAM
    )));
    let web_target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    assert!(!engine()
        .compile_wasm_aot_image(&compiled, &web_target)
        .unwrap()
        .bytes
        .is_empty());
    let native_target = vo_engine::TargetSpec::host().unwrap();
    assert!(!engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap()
        .bytes
        .is_empty());
}

#[cfg(feature = "aot-wasm")]
#[test]
fn official_media_and_studio_showcases_use_public_advanced_packs() {
    for (name, source) in [
        (
            "media",
            include_str!("../../../../ui/showcases/media-application/main.vo"),
        ),
        (
            "studio",
            include_str!("../../../../ui/showcases/studio-workbench/main.vo"),
        ),
    ] {
        let workspace = UiTestWorkspace::create_with_main(source);
        let compiled = workspace.compile();
        let vm = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
        let jit = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
        assert_eq!(vm.mutations, jit.mutations, "{name} VM/JIT tree");
        assert!(vm.mutations.len() > 20, "{name} should render useful UI");
        let web_target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
        assert!(!engine()
            .compile_wasm_aot_image(&compiled, &web_target)
            .unwrap()
            .bytes
            .is_empty());
    }
}

#[cfg(feature = "aot-native")]
#[test]
fn official_multi_window_editor_preserves_shared_document_and_native_aot() {
    let workspace = UiTestWorkspace::create_with_main(include_str!(
        "../../../../ui/tests/multi-window-editor/main.vo"
    ));
    let compiled = workspace.compile();
    let vm = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
    let jit = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
    assert_eq!(vm.mutations, jit.mutations);
    let native_target = vo_engine::TargetSpec::host().unwrap();
    assert!(!engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap()
        .bytes
        .is_empty());
}

#[cfg(all(feature = "aot-native", feature = "aot-wasm"))]
#[test]
fn official_ui_testing_and_observability_are_vm_jit_and_aot_equivalent() {
    let workspace = UiTestWorkspace::create_with_main(include_str!(
        "../../../../ui/tests/tooling-resilience/main.vo"
    ));
    let compiled = workspace.compile();
    let vm = ui_initial_batch_for(compiled.module.clone(), RunMode::Vm);
    let jit = ui_initial_batch_for(compiled.module.clone(), RunMode::Jit);
    assert_eq!(vm.mutations, jit.mutations);
    assert!(vm.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetText { text, .. }
            if text == "tooling-resilience: ok"
    )));
    let web_target = vo_engine::TargetSpec::parse(vo_engine::WASM32_UNKNOWN_UNKNOWN).unwrap();
    assert!(!engine()
        .compile_wasm_aot_image(&compiled, &web_target)
        .unwrap()
        .bytes
        .is_empty());
    let native_target = vo_engine::TargetSpec::host().unwrap();
    assert!(!engine()
        .compile_native_aot_object(&compiled, &native_target, false)
        .unwrap()
        .bytes
        .is_empty());
}

#[test]
fn official_typed_state_is_persistent_and_vm_jit_equivalent() {
    let workspace = UiTestWorkspace::create_with_main(
        r#"
package main
import "github.com/vo-lang/ui"
func App() ui.View {
	name := ui.UseStringState("before")
	return ui.TextInput(ui.StringStateValue(name), "Name", func(event ui.Event) {
		ui.SetStringState(name, event.Text)
	})
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#,
    );
    let compiled = workspace.compile();
    assert!(compiled
        .module
        .artifact(vo_ui_artifact::COMPONENT_ARTIFACT_NAME)
        .is_some());
    let (vm_initial, vm_update) = ui_batches_for(compiled.module.clone(), RunMode::Vm);
    let (jit_initial, jit_update) = ui_batches_for(compiled.module, RunMode::Jit);
    assert_eq!(vm_initial.mutations, jit_initial.mutations);
    assert_eq!(vm_update.mutations, jit_update.mutations);
    assert!(vm_update.mutations.iter().any(|mutation| matches!(
        mutation,
        vo_ui_protocol::Mutation::SetProperty { property, .. }
            if property.id == vo_ui_core::PropertyId::VALUE
                && property.value == vo_ui_core::Value::Text("after".to_string())
    )));
}

#[test]
fn ui_artifacts_survive_cache_hits_without_contaminating_plain_language_compilation() {
    let workspace = UiTestWorkspace::create();
    let workfile = workspace.0.join("vo.work");
    #[cfg(not(windows))]
    let workfile = workfile.canonicalize().unwrap();
    let options = vo_module::project::ProjectContextOptions::new(
        vo_module::workspace::WorkspaceDiscovery::Explicit(workfile),
    );
    let path = workspace.app();
    let path = path.to_str().unwrap();
    let ui = engine();
    let first = ui.compile_with_cache_with_options(path, &options).unwrap();
    let plain = vo_engine::Engine::default()
        .compile_with_cache_with_options(path, &options)
        .unwrap();
    let cache_hits = Arc::new(AtomicU64::new(0));
    let observed_hits = Arc::clone(&cache_hits);
    let cached = vo_engine::with_compile_log_sink(
        move |record| {
            if record.code == "compile_cache_hit" {
                observed_hits.fetch_add(1, Ordering::Relaxed);
            }
        },
        || ui.compile_with_cache_with_options(path, &options),
    )
    .unwrap();
    assert_eq!(cache_hits.load(Ordering::Relaxed), 1);
    for name in [
        vo_ui_artifact::COMPONENT_ARTIFACT_NAME,
        vo_ui_artifact::COMPONENT_BUNDLE_ARTIFACT_NAME,
    ] {
        let artifact = first.module.artifact(name).unwrap();
        assert_eq!(cached.module.artifact(name), Some(artifact));
        assert!(plain.module.artifact(name).is_none());
    }
    let rendered = render_initial_ui_document(
        cached,
        RunMode::Vm,
        &DocumentMetadata::default(),
        SsrLimits::default(),
    )
    .unwrap();
    assert!(rendered.html.contains("before"));
}
