use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::{Duration, Instant};

use vo_ui_artifact::{
    BindingDefinition, BindingId, BundleLimits, BundleSourceMetadata, ComponentBundle,
    ComponentCallMode, ComponentCallSite, ComponentCallSiteId, ComponentDefinition,
    ComponentInterface, ComponentTypeId, ExecutionMode, LifecycleDefinition, StateFieldDefinition,
    StateFieldId, StateValueKind, COMPONENT_BUNDLE_ABI_VERSION,
};
use vo_ui_core::{NodeId, Primitive, PropertyId, Value};
use vo_ui_plan::{
    ComponentPlan, LocalNodeId, PlanLimits, SlotId, SlotKind, SlotValue, TemplateNode, UpdateSite,
    ValidatedPlan,
};
use vo_ui_protocol::{MutationBatch, Renderer};
use vo_ui_reactive::{Runtime as ReactiveRuntime, RuntimeConfig};
use vo_ui_runtime::{
    ComponentForestLimits, ComponentSpec, ComponentStateCell, ComponentTemplateRuntime,
    ComponentValue, TemplateRuntime,
};
use vo_ui_scheduler::{SchedulerConfig, ScopedMessage, TaskCompletion};
use vo_ui_session::{SlotWrites, UiSession};

const FRAME_SAMPLES: usize = 600;
const INTERACTION_SAMPLES: usize = 300;
const COMPONENT_SAMPLES: usize = 300;
const COMPONENT_ROWS: usize = 256;
const FRAME_P95_BUDGET: Duration = Duration::from_millis(8);
const INTERACTION_P95_BUDGET: Duration = Duration::from_millis(50);
const COMPONENT_P95_BUDGET: Duration = Duration::from_millis(8);

static TRACK_ALLOCATIONS: AtomicBool = AtomicBool::new(false);
static ALLOCATIONS: AtomicU64 = AtomicU64::new(0);

struct TrackingAllocator;

unsafe impl GlobalAlloc for TrackingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        count_allocation();
        unsafe { System.alloc(layout) }
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        count_allocation();
        unsafe { System.alloc_zeroed(layout) }
    }

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        unsafe { System.dealloc(pointer, layout) }
    }

    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        count_allocation();
        unsafe { System.realloc(pointer, layout, size) }
    }
}

#[global_allocator]
static GLOBAL_ALLOCATOR: TrackingAllocator = TrackingAllocator;

fn count_allocation() {
    if TRACK_ALLOCATIONS.load(Ordering::Relaxed) {
        ALLOCATIONS.fetch_add(1, Ordering::Relaxed);
    }
}

#[derive(Default)]
struct CertificationRenderer {
    revision: u64,
}

impl Renderer for CertificationRenderer {
    type Error = &'static str;

    fn apply(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
        if batch.revision != self.revision.saturating_add(1) {
            return Err("non-monotonic certification revision");
        }
        self.revision = batch.revision;
        Ok(())
    }
}

fn scalar_plan() -> ValidatedPlan {
    let mut plan = ComponentPlan::new(LocalNodeId::new(0));
    plan.slots.push(SlotKind::Property);
    plan.nodes
        .push(TemplateNode::element(LocalNodeId::new(0), Primitive::Box));
    plan.updates.push(UpdateSite::property(
        SlotId::new(0),
        LocalNodeId::new(0),
        PropertyId::CHECKED,
    ));
    plan.validate(PlanLimits::default())
        .expect("certification plan must be valid")
}

fn slot(value: bool) -> SlotValue {
    SlotValue::Property(Value::Bool(value))
}

fn component_type(name: &str) -> ComponentTypeId {
    ComponentTypeId::new("volang.dev/ui-benchmark", name)
}

fn component_bundle() -> ComponentBundle {
    let app_type = component_type("App");
    let row_type = component_type("Row");
    let mut app_plan = ComponentPlan::new(LocalNodeId::new(0));
    app_plan.nodes.push(
        TemplateNode::element(LocalNodeId::new(0), Primitive::Column).child(LocalNodeId::new(1)),
    );
    app_plan.nodes.push(TemplateNode::element(
        LocalNodeId::new(1),
        Primitive::Fragment,
    ));
    let mut row_plan = ComponentPlan::new(LocalNodeId::new(0));
    row_plan.slots.push(SlotKind::Text);
    row_plan
        .nodes
        .push(TemplateNode::text(LocalNodeId::new(0), ""));
    row_plan
        .updates
        .push(UpdateSite::text(SlotId::new(0), LocalNodeId::new(0)));
    let interface = ComponentInterface::empty();
    ComponentBundle {
        abi_version: COMPONENT_BUNDLE_ABI_VERSION,
        module_identity: "volang.dev/ui-benchmark".to_string(),
        root: app_type.clone(),
        linked_modules: Vec::new(),
        definitions: vec![
            ComponentDefinition {
                type_id: app_type,
                display_name: "App".to_string(),
                mode: ExecutionMode::RootFallback,
                interface,
                plan: app_plan
                    .validate(PlanLimits::default())
                    .expect("component benchmark app plan must be valid"),
                call_sites: vec![ComponentCallSite {
                    id: ComponentCallSiteId::new(1),
                    mode: ComponentCallMode::Dynamic,
                    callee: None,
                    mount_parent: LocalNodeId::new(1),
                    mount_before: None,
                    props_bindings: Vec::new(),
                    key_binding: None,
                }],
                states: Vec::new(),
                bindings: Vec::new(),
                handlers: Vec::new(),
                effects: Vec::new(),
                tasks: Vec::new(),
                lifecycle: LifecycleDefinition::empty(),
                reload_schema_fingerprint: 1,
            },
            ComponentDefinition {
                type_id: row_type,
                display_name: "Row".to_string(),
                mode: ExecutionMode::RootFallback,
                interface,
                plan: row_plan
                    .validate(PlanLimits::default())
                    .expect("component benchmark row plan must be valid"),
                call_sites: Vec::new(),
                states: vec![StateFieldDefinition {
                    id: StateFieldId::new(0),
                    key: "count".to_string(),
                    type_fingerprint: 7,
                    value_kind: StateValueKind::Int,
                    has_initializer: true,
                    initializer_func: None,
                    initializer_dependencies: Vec::new(),
                    initializer_props: Vec::new(),
                }],
                bindings: vec![BindingDefinition {
                    id: BindingId::new(0),
                    evaluator_func: None,
                    slots: vec![SlotId::new(0)],
                    dependencies: Vec::new(),
                    prop_dependencies: Vec::new(),
                }],
                handlers: Vec::new(),
                effects: Vec::new(),
                tasks: Vec::new(),
                lifecycle: LifecycleDefinition::empty(),
                reload_schema_fingerprint: 1,
            },
        ],
        imports: Vec::new(),
        capabilities: vec!["ui.component-v2".to_string()],
        source: BundleSourceMetadata {
            source_digest: [1; 32],
            compiler_identity: "vo-ui-benchmark".to_string(),
            reload_schema_version: 1,
        },
    }
}

fn component_rows(changed: bool) -> Vec<ComponentSpec> {
    (0..COMPONENT_ROWS)
        .map(|index| {
            let text = if index == COMPONENT_ROWS / 2 && changed {
                "changed"
            } else {
                "stable"
            };
            ComponentSpec::new(ComponentCallSiteId::new(1), component_type("Row"))
                .keyed(index as i64)
                .state([ComponentStateCell::new(
                    StateFieldId::new(0),
                    "count",
                    7,
                    ComponentValue::Int(index as i64),
                )])
                .slots([SlotValue::Text(text.to_string())])
        })
        .collect()
}

fn measured_allocations(operation: impl FnOnce()) -> u64 {
    ALLOCATIONS.store(0, Ordering::Relaxed);
    TRACK_ALLOCATIONS.store(true, Ordering::SeqCst);
    operation();
    TRACK_ALLOCATIONS.store(false, Ordering::SeqCst);
    ALLOCATIONS.load(Ordering::Relaxed)
}

fn percentile(samples: &mut [Duration], percent: usize) -> Duration {
    assert!((1..=100).contains(&percent));
    samples.sort_unstable();
    let rank = samples.len().saturating_mul(percent).div_ceil(100);
    samples[rank.saturating_sub(1)]
}

fn nanos(duration: Duration) -> u128 {
    duration.as_nanos()
}

fn main() {
    let root = NodeId::new(0, 1);
    let plan = scalar_plan();
    let mut runtime = TemplateRuntime::new(CertificationRenderer::default(), 1, root);
    runtime
        .mount(plan.clone(), vec![slot(false)])
        .expect("certification mount must succeed");
    runtime
        .update_slots_in_place([(SlotId::new(0), slot(true))])
        .expect("warm update must succeed");
    runtime
        .update_slots_in_place([(SlotId::new(0), slot(false))])
        .expect("warm update must succeed");

    let direct_allocations = measured_allocations(|| {
        runtime
            .update_slots_in_place([(SlotId::new(0), slot(true))])
            .expect("measured update must succeed");
    });

    let mut frame_samples = Vec::with_capacity(FRAME_SAMPLES);
    for index in 0..FRAME_SAMPLES {
        let start = Instant::now();
        runtime
            .update_slots_in_place([(SlotId::new(0), slot(index % 2 == 0))])
            .expect("frame update must succeed");
        frame_samples.push(start.elapsed());
    }
    let frame_p50 = percentile(&mut frame_samples, 50);
    let frame_p95 = percentile(&mut frame_samples, 95);
    let frame_p99 = percentile(&mut frame_samples, 99);

    let component_root = NodeId::new(1, 1);
    let mut component_runtime = ComponentTemplateRuntime::new(
        CertificationRenderer::default(),
        3,
        component_root,
        component_bundle(),
        BundleLimits::default(),
        ComponentForestLimits::default(),
    )
    .expect("component benchmark runtime must be created");
    component_runtime
        .mount(Vec::new(), Vec::new(), Vec::new(), component_rows(false))
        .expect("component benchmark must mount");
    let component_instance = component_runtime
        .forest()
        .root()
        .expect("component benchmark root must be mounted");
    let stable_identities = component_runtime
        .forest()
        .get(component_instance)
        .expect("component benchmark root must be live")
        .children
        .clone();
    let mut component_samples = Vec::with_capacity(COMPONENT_SAMPLES);
    for index in 0..COMPONENT_SAMPLES {
        let start = Instant::now();
        let commit = component_runtime
            .update(
                Vec::new(),
                Vec::new(),
                Vec::new(),
                component_rows(index % 2 == 0),
            )
            .expect("component benchmark update must succeed");
        component_samples.push(start.elapsed());
        assert!(commit.forest.created.is_empty());
        assert!(commit.forest.disposed.is_empty());
        assert_eq!(
            component_runtime
                .forest()
                .get(component_instance)
                .expect("component benchmark root must remain live")
                .children,
            stable_identities
        );
    }
    let component_p50 = percentile(&mut component_samples, 50);
    let component_p95 = percentile(&mut component_samples, 95);
    let component_p99 = percentile(&mut component_samples, 99);

    let mut reactive = ReactiveRuntime::new(RuntimeConfig::default());
    let owner = reactive
        .create_scope(reactive.root_scope())
        .expect("certification scope must be created");
    let mut session = UiSession::new(
        CertificationRenderer::default(),
        2,
        root,
        SchedulerConfig::default(),
        |message: ScopedMessage<bool>, writes: &mut SlotWrites| {
            writes.set(SlotId::new(0), slot(message.message));
        },
    )
    .expect("certification session must be created");
    session
        .mount(plan, vec![slot(false)])
        .expect("certification session must mount");
    let mut interaction_samples = Vec::with_capacity(INTERACTION_SAMPLES);
    for index in 0..INTERACTION_SAMPLES {
        let task = session
            .spawn_task(owner)
            .expect("certification task must be created");
        let start = Instant::now();
        session
            .enqueue_completion(TaskCompletion::new(task, index % 2 == 0))
            .expect("certification completion queue must accept the task");
        let turn = session
            .drain_turn()
            .expect("certification interaction turn must succeed");
        if turn.commit.is_none() {
            panic!("certification interaction must commit a changed scalar slot");
        }
        interaction_samples.push(start.elapsed());
    }
    let interaction_p50 = percentile(&mut interaction_samples, 50);
    let interaction_p95 = percentile(&mut interaction_samples, 95);
    let interaction_p99 = percentile(&mut interaction_samples, 99);

    println!(
        concat!(
            "{{\"schema\":1,\"target\":\"{}-{}\",\"profile\":\"{}\",",
            "\"direct_scalar_allocations\":{},",
            "\"frame_p50_ns\":{},\"frame_p95_ns\":{},\"frame_p99_ns\":{},",
            "\"interaction_p50_ns\":{},\"interaction_p95_ns\":{},\"interaction_p99_ns\":{},",
            "\"component_p50_ns\":{},\"component_p95_ns\":{},\"component_p99_ns\":{},",
            "\"frame_samples\":{},\"interaction_samples\":{},",
            "\"component_samples\":{},\"component_rows\":{}}}"
        ),
        std::env::consts::OS,
        std::env::consts::ARCH,
        if cfg!(debug_assertions) {
            "debug"
        } else {
            "release"
        },
        direct_allocations,
        nanos(frame_p50),
        nanos(frame_p95),
        nanos(frame_p99),
        nanos(interaction_p50),
        nanos(interaction_p95),
        nanos(interaction_p99),
        nanos(component_p50),
        nanos(component_p95),
        nanos(component_p99),
        FRAME_SAMPLES,
        INTERACTION_SAMPLES,
        COMPONENT_SAMPLES,
        COMPONENT_ROWS,
    );

    assert_eq!(
        direct_allocations, 0,
        "steady-state scalar slot update allocated host memory"
    );
    assert!(
        frame_p95 <= FRAME_P95_BUDGET,
        "frame p95 {:?} exceeded {:?}",
        frame_p95,
        FRAME_P95_BUDGET
    );
    assert!(
        interaction_p95 <= INTERACTION_P95_BUDGET,
        "interaction p95 {:?} exceeded {:?}",
        interaction_p95,
        INTERACTION_P95_BUDGET
    );
    assert!(
        component_p95 <= COMPONENT_P95_BUDGET,
        "component update p95 {:?} exceeded {:?}",
        component_p95,
        COMPONENT_P95_BUDGET
    );
}
