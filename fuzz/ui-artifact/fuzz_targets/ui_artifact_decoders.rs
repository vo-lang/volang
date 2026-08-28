#![no_main]

use libfuzzer_sys::fuzz_target;
use vo_ui_artifact::{
    decode_component_artifact, decode_component_bundle, encode_component_artifact,
    encode_component_bundle, ArtifactLimits, BundleLimits, BundleSourceMetadata, ComponentArtifact,
    ComponentBundle, ComponentDefinition, ComponentInterface, ComponentTypeId, ExecutionMode,
    LifecycleDefinition,
};
use vo_ui_core::Primitive;
use vo_ui_plan::{ComponentPlan, LocalNodeId, PlanLimits, TemplateNode, ValidatedPlan};

fuzz_target!(|bytes: &[u8]| {
    let artifact_limits = ArtifactLimits {
        max_bytes: 256 * 1024,
        max_identity_bytes: 4 * 1024,
        max_component_name_bytes: 4 * 1024,
        max_state_fields: 1_024,
        max_bindings: 4_096,
        max_state_key_bytes: 4 * 1024,
    };
    let bundle_limits = BundleLimits {
        max_bundle_bytes: 256 * 1024,
        max_identity_bytes: 4 * 1024,
        max_display_name_bytes: 4 * 1024,
        max_state_key_bytes: 4 * 1024,
        max_compiler_identity_bytes: 4 * 1024,
        max_capability_bytes: 512,
        max_definitions: 256,
        max_linked_modules: 128,
        max_imports: 128,
        max_imported_types: 1_024,
        max_capabilities: 256,
        max_call_sites_per_definition: 1_024,
        max_states_per_definition: 1_024,
        max_bindings_per_definition: 4_096,
        max_handlers_per_definition: 4_096,
        max_effects_per_definition: 1_024,
        max_tasks_per_definition: 1_024,
        max_dependencies_per_entry: 1_024,
        max_static_nesting: 64,
    };
    let plan_limits = PlanLimits {
        max_plan_bytes: 128 * 1024,
        max_nodes: 4_096,
        max_slots: 4_096,
        max_updates: 8_192,
        max_children_per_node: 1_024,
        max_properties_per_node: 256,
        max_listeners_per_node: 256,
        max_static_value_bytes: 64 * 1024,
    };

    exercise_all(bytes, artifact_limits, bundle_limits, plan_limits);

    let plan = minimal_plan(plan_limits);
    let artifact = ComponentArtifact {
        identity: "github.com/vo-lang/fuzz::App".into(),
        component_name: "App".into(),
        mode: ExecutionMode::RootFallback,
        plan: plan.clone(),
        states: Vec::new(),
        slots: Vec::new(),
        handlers: Vec::new(),
    };
    let artifact_seed = encode_component_artifact(&artifact, artifact_limits, plan_limits).unwrap();
    exercise_all(
        &mutate_seed(&artifact_seed, bytes),
        artifact_limits,
        bundle_limits,
        plan_limits,
    );

    let root = ComponentTypeId::new("github.com/vo-lang/fuzz", "main.App");
    let bundle = ComponentBundle {
        abi_version: vo_ui_artifact::COMPONENT_BUNDLE_ABI_VERSION,
        module_identity: "github.com/vo-lang/fuzz".into(),
        root: root.clone(),
        linked_modules: Vec::new(),
        definitions: vec![ComponentDefinition {
            type_id: root,
            display_name: "App".into(),
            mode: ExecutionMode::RootFallback,
            interface: ComponentInterface::empty(),
            plan,
            call_sites: Vec::new(),
            states: Vec::new(),
            bindings: Vec::new(),
            handlers: Vec::new(),
            effects: Vec::new(),
            tasks: Vec::new(),
            lifecycle: LifecycleDefinition::empty(),
            reload_schema_fingerprint: 1,
        }],
        imports: Vec::new(),
        capabilities: Vec::new(),
        source: BundleSourceMetadata {
            source_digest: [7; 32],
            compiler_identity: "vo-ui-fuzz/1".into(),
            reload_schema_version: 1,
        },
    };
    let bundle_seed = encode_component_bundle(&bundle, bundle_limits, plan_limits).unwrap();
    exercise_all(
        &mutate_seed(&bundle_seed, bytes),
        artifact_limits,
        bundle_limits,
        plan_limits,
    );
});

fn minimal_plan(limits: PlanLimits) -> ValidatedPlan {
    let mut plan = ComponentPlan::new(LocalNodeId::new(0));
    plan.nodes
        .push(TemplateNode::element(LocalNodeId::new(0), Primitive::Root));
    plan.validate(limits).unwrap()
}

fn exercise_all(
    bytes: &[u8],
    artifact_limits: ArtifactLimits,
    bundle_limits: BundleLimits,
    plan_limits: PlanLimits,
) {
    let _ = decode_component_artifact(bytes, artifact_limits, plan_limits);
    let _ = decode_component_bundle(bytes, bundle_limits, plan_limits);
}

fn mutate_seed(seed: &[u8], data: &[u8]) -> Vec<u8> {
    let mut mutated = seed.to_vec();
    for (index, byte) in data.iter().copied().take(64 * 1024).enumerate() {
        if index < mutated.len() {
            mutated[index] ^= byte;
        } else {
            mutated.push(byte);
        }
    }
    mutated
}
