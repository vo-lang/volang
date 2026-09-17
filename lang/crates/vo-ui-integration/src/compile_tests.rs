use std::{collections::BTreeSet, path::PathBuf};

#[test]
fn analyzed_ui_project_embeds_a_decodable_component_artifact() {
    use vo_analysis::project::PackageIdentity;
    use vo_analysis::vfs::{
        analyze_file_set_with_package_identity, ModSource, PackageResolver, StdSource,
    };
    use vo_common::vfs::{FileSet, MemoryFs};
    use vo_ui_artifact::{
        decode_component_artifact, decode_component_bundle, ArtifactLimits, BundleLimits,
        ExecutionMode, COMPONENT_ARTIFACT_NAME, COMPONENT_ARTIFACT_VERSION,
        COMPONENT_BUNDLE_ARTIFACT_NAME, COMPONENT_BUNDLE_ARTIFACT_VERSION,
    };
    use vo_ui_plan::PlanLimits;

    let module_fs = MemoryFs::new()
        .with_file(
            "github.com/vo-lang/ui/vo.mod",
            include_str!("../../../../ui/vo.mod"),
        )
        .with_file(
            "github.com/vo-lang/ui/ui.vo",
            include_str!("../../../../ui/ui.vo"),
        );
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", "package errors\n")),
        r#mod: ModSource::with_fs(module_fs),
    };
    let mut files = FileSet::new(PathBuf::from("."));
    files.files.insert(
        PathBuf::from("main.vo"),
        r#"package main
import "github.com/vo-lang/ui"
func App() ui.View {
	count := 1
	return ui.Button("Count", func(event ui.Event) { count++ })
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#
        .to_string(),
    );
    let project = analyze_file_set_with_package_identity(
        files,
        resolver,
        MemoryFs::new(),
        PathBuf::from("."),
        Some("github.com/acme/app".to_string()),
        Some(PackageIdentity::new("github.com/acme/app").unwrap()),
    )
    .expect("analyze UI fixture");

    let module = crate::compile_project(&project).expect("compile UI fixture");
    let artifact = module
        .artifact(COMPONENT_ARTIFACT_NAME)
        .expect("embedded UI component artifact");
    assert_eq!(artifact.version, COMPONENT_ARTIFACT_VERSION);
    let component = decode_component_artifact(
        &artifact.payload,
        ArtifactLimits::default(),
        PlanLimits::default(),
    )
    .expect("decode UI component artifact");
    assert_eq!(component.identity, "github.com/acme/app::App");
    assert_eq!(component.mode, ExecutionMode::Direct);
    assert_eq!(component.states.len(), 1);
    assert_eq!(component.handlers.len(), 1);
    assert!(component.handlers[0].evaluator_func.is_some());
    let expected_payload = artifact.payload.clone();
    let bundle_artifact = module
        .artifact(COMPONENT_BUNDLE_ARTIFACT_NAME)
        .expect("embedded UI component bundle");
    assert_eq!(bundle_artifact.version, COMPONENT_BUNDLE_ARTIFACT_VERSION);
    let bundle = decode_component_bundle(
        &bundle_artifact.payload,
        BundleLimits::default(),
        PlanLimits::default(),
    )
    .expect("decode UI component bundle");
    assert_eq!(bundle.root.object(), "App");
    assert_eq!(bundle.definitions.len(), 1);
    assert_eq!(bundle.definitions[0].mode, ExecutionMode::Direct);
    let expected_bundle_payload = bundle_artifact.payload.clone();

    let bytes = module.serialize().expect("serialize UI module");
    let decoded_module =
        vo_common_core::Module::deserialize(&bytes).expect("deserialize UI module");
    assert_eq!(
        decoded_module
            .artifact(COMPONENT_ARTIFACT_NAME)
            .map(|artifact| artifact.payload.as_slice()),
        Some(expected_payload.as_slice())
    );
    assert_eq!(
        decoded_module
            .artifact(COMPONENT_BUNDLE_ARTIFACT_NAME)
            .map(|artifact| artifact.payload.as_slice()),
        Some(expected_bundle_payload.as_slice())
    );
}

#[test]
fn nested_ui_project_emits_bundle_without_publishing_a_partial_vua1_root() {
    use vo_analysis::project::PackageIdentity;
    use vo_analysis::vfs::{
        analyze_file_set_with_package_identity, ModSource, PackageResolver, StdSource,
    };
    use vo_common::vfs::{FileSet, MemoryFs};
    use vo_ui_artifact::{
        decode_component_bundle, BundleLimits, COMPONENT_ARTIFACT_NAME,
        COMPONENT_BUNDLE_ARTIFACT_NAME,
    };
    use vo_ui_plan::PlanLimits;

    let module_fs = MemoryFs::new()
        .with_file(
            "github.com/vo-lang/ui/vo.mod",
            include_str!("../../../../ui/vo.mod"),
        )
        .with_file(
            "github.com/vo-lang/ui/ui.vo",
            include_str!("../../../../ui/ui.vo"),
        )
        .with_file(
            "github.com/acme/components/vo.mod",
            r#"format = 1
module = "github.com/acme/components"
version = "0.1.0"
vo = "0.1.0"
"#,
        )
        .with_file(
            "github.com/acme/components/components.vo",
            r#"package components
import "github.com/vo-lang/ui"
func Child(label string) ui.View { return ui.Text(label) }
"#,
        );
    let resolver = PackageResolver {
        std: StdSource::with_fs(MemoryFs::new().with_file("errors/errors.vo", "package errors\n")),
        r#mod: ModSource::with_fs(module_fs),
    };
    let mut files = FileSet::new(PathBuf::from("."));
    files.files.insert(
        PathBuf::from("main.vo"),
        r#"package main
import "github.com/vo-lang/ui"
import components "github.com/acme/components"
func App() ui.View {
	label := "child"
	return ui.Column(components.Child(label), components.Child("fixed"))
}
func main() {
	if err := ui.Mount(App); err != nil { panic(err.Error()) }
}
"#
        .to_string(),
    );
    let project = analyze_file_set_with_package_identity(
        files,
        resolver,
        MemoryFs::new(),
        PathBuf::from("."),
        Some("github.com/acme/app".to_string()),
        Some(PackageIdentity::new("github.com/acme/app").unwrap()),
    )
    .expect("analyze nested UI fixture");

    let module = crate::compile_project(&project).expect("compile nested UI component graph");
    assert!(module.artifact(COMPONENT_ARTIFACT_NAME).is_none());
    let artifact = module
        .artifact(COMPONENT_BUNDLE_ARTIFACT_NAME)
        .expect("nested component bundle");
    let bundle = decode_component_bundle(
        &artifact.payload,
        BundleLimits::default(),
        PlanLimits::default(),
    )
    .expect("decode nested component bundle");
    assert_eq!(bundle.definitions.len(), 2);
    assert_eq!(bundle.root.object(), "App");
    assert_eq!(
        bundle.linked_modules,
        vec!["github.com/acme/components".to_string()]
    );
    assert_eq!(bundle.definitions[0].call_sites.len(), 2);
    assert_eq!(bundle.definitions[1].interface.props_arity, 1);
    assert!(bundle.definitions[1].call_sites.is_empty());
    let root_evaluators = bundle.definitions[0]
        .bindings
        .iter()
        .filter_map(|binding| binding.evaluator_func)
        .collect::<BTreeSet<_>>();
    let imported_evaluators = bundle.definitions[1]
        .bindings
        .iter()
        .filter_map(|binding| binding.evaluator_func)
        .collect::<BTreeSet<_>>();
    assert!(!root_evaluators.is_empty());
    assert!(!imported_evaluators.is_empty());
    assert!(root_evaluators.is_disjoint(&imported_evaluators));
}
