use super::*;

#[test]
fn vm_jit_callback_boundary_001_source_guards_callback_abi_and_transition_publication() {
    let select_src = production_source_before_test_module_062(include_str!("../../select.rs"));
    let defer_src = production_source_before_test_module_062(include_str!("../../defer.rs"));

    assert!(
        select_src.contains("validate_callback_slot_count") && select_src.contains("case_count"),
        "jit_select_begin must reject oversized case counts before allocating select state"
    );
    assert!(
        defer_src.contains("validate_callback_raw_slot_span") && defer_src.contains("result_ptr"),
        "jit_recover must validate its two-slot out pointer before writing"
    );
    for (name, src) in jit_callback_production_sources_062() {
        assert!(
            !src.contains("apply_runtime_transition"),
            "JIT {name} callbacks must publish RuntimeTransition for the VM boundary applier"
        );
        assert_no_jit_callback_scheduler_mutators_062(&name, &src);
    }
}

#[test]
fn vm_jit_callback_boundary_001_source_set_tracks_callback_exports_062() {
    let callbacks_mod = include_str!("../../mod.rs");
    let (assignment_sources, errors) =
        jit_context_assignment_source_names_062(include_str!("../../../context.rs"), callbacks_mod);
    assert!(
        errors.is_empty(),
        "JitContext callback assignments must resolve to scanned source files:\n{}",
        errors.join("\n")
    );
    let sources = jit_callback_production_sources_062();
    let source_names: std::collections::BTreeSet<_> =
        sources.iter().map(|(name, _)| name.as_str()).collect();
    let expected_names: std::collections::BTreeSet<_> = ["context"]
        .into_iter()
        .chain(assignment_sources.iter().map(String::as_str))
        .collect();

    assert_eq!(
            source_names, expected_names,
            "JIT callback hardening source set must be derived from JitContext function pointer assignments"
        );
}

#[test]
fn vm_jit_callback_boundary_001_source_set_tracks_private_reexport_modules_062() {
    let synthetic_mod = r#"
            mod dangerous;
            pub(crate) use dangerous::jit_new_callback;
            pub mod queue;
            pub use queue::{jit_queue_send, jit_queue_recv};
        "#;
    let modules = callback_source_module_names_from_mod_062(synthetic_mod);

    assert!(
        modules.contains("dangerous"),
        "scanner source set must include private modules that publish JIT callbacks by re-export"
    );
    assert!(modules.contains("queue"));
}

#[test]
fn vm_jit_callback_boundary_001_source_set_rejects_unresolved_context_fn_aliases_062() {
    let synthetic_mod = r#"
            mod dangerous;
            pub(crate) use dangerous::jit_new_callback;
        "#;
    let synthetic_context = r#"
            use super::frame::{jit_push_frame};
            let ctx = JitContext {
                push_frame_fn: Some(jit_push_frame),
                direct_callback_fn: Some(callbacks::jit_new_callback),
                alias_callback_fn: Some(jit_new_callback),
                unknown_abi_fn: Some(super::jit_new_abi),
            };
        "#;

    let (sources, errors) =
        jit_context_assignment_source_names_062(synthetic_context, synthetic_mod);

    assert!(sources.contains("dangerous"));
    assert!(sources.contains("frame"));
    assert!(
        errors
            .iter()
            .any(|error| error.contains("alias_callback_fn") && error.contains("jit_new_callback")),
        "bare callback aliases must be rejected so source ownership stays explicit"
    );
    assert!(
            errors
                .iter()
                .any(|error| error.contains("unknown_abi_fn") && error.contains("super::jit_new_abi")),
            "new non-callback ABI function pointers must be added to the resolver before they can bypass scanning"
        );
}

#[test]
fn vm_jit_callback_boundary_001_source_set_tracks_multiline_context_fn_assignments_062() {
    let synthetic_mod = r#"
            mod dangerous;
            pub(crate) use dangerous::jit_new_callback;
        "#;
    let synthetic_context = r#"
            let ctx = JitContext {
                direct_callback_fn:
                    Some(callbacks::jit_new_callback),
                split_some_callback_fn: Some(
                    callbacks::jit_new_callback
                ),
            };
        "#;

    let (sources, errors) =
        jit_context_assignment_source_names_062(synthetic_context, synthetic_mod);

    assert!(
        errors.is_empty(),
        "multiline callback assignments must resolve without parser errors: {errors:?}"
    );
    assert!(
        sources.contains("dangerous"),
        "multiline JitContext callback assignments must still select their callback source"
    );
}

#[test]
fn vm_jit_callback_boundary_001_source_set_keeps_scanning_after_none_fn_fields_062() {
    let callbacks_mod = include_str!("../../mod.rs");
    let (sources, errors) =
        jit_context_assignment_source_names_062(include_str!("../../../context.rs"), callbacks_mod);

    assert!(
            errors.is_empty(),
            "real JitContext callback assignments must resolve after None-valued cfg branches: {errors:?}"
        );
    for expected in [
        "closure_call",
        "defer",
        "extern_call",
        "frame",
        "goroutine",
        "helpers",
        "island",
        "queue",
        "select",
    ] {
        assert!(
            sources.contains(expected),
            "JitContext callback source set must include {expected} after None-valued fields"
        );
    }
}

#[test]
fn vm_jit_callback_boundary_001_source_set_scans_nested_reexport_modules_062() {
    let root = std::env::temp_dir().join(format!(
        "vo-jit-callback-nested-reexport-{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(root.join("impls")).expect("create path callback module dir");
    std::fs::write(
        root.join("dangerous.rs"),
        r##"
                #[cfg_attr(
                    not(test),
                    path = r#"impls/inner.rs"#,
                )]
                pub(in crate::vm::jit::callbacks) mod inner;
                pub(crate) use inner::jit_new_callback;

                #[path = "impls/self_inner.rs"]
                pub(self) mod self_inner;
                pub(crate) use self_inner::jit_new_callback_with_self_visibility;

                #[path = "impls/mod.rs"]
                pub(crate) mod impls;
                pub(crate) use impls::jit_mod_callback;
            "##,
    )
    .expect("write re-exporting callback module");
    std::fs::write(
        root.join("impls/inner.rs"),
        "fn jit_new_callback() { Scheduler::try_wake_fiber(&mut vm.scheduler, fid); }",
    )
    .expect("write path-attributed callback implementation");
    std::fs::write(
            root.join("impls/self_inner.rs"),
            "fn jit_new_callback_with_self_visibility() { Scheduler::wake_host_event(&mut vm.scheduler, host_key); }",
        )
        .expect("write pub(self) path-attributed callback implementation");
    std::fs::write(
        root.join("impls/mod.rs"),
        "mod deeper; pub(crate) use deeper::jit_mod_callback;",
    )
    .expect("write path-attributed mod.rs callback implementation");
    std::fs::write(
        root.join("impls/deeper.rs"),
        "fn jit_mod_callback() { Scheduler::wake_io(&mut vm.scheduler, io_key); }",
    )
    .expect("write nested default child of path-attributed mod.rs");

    let source = read_jit_source_062(&root, "dangerous.rs");

    assert!(
            source.contains("Scheduler::try_wake_fiber"),
            "callback source-set scans must include nested implementation files re-exported by the scanned module"
        );
    assert!(
        source.contains("Scheduler::wake_host_event"),
        "callback source-set scans must include scoped-visibility nested implementation modules"
    );
    assert!(
            source.contains("Scheduler::wake_io"),
            "callback source-set scans must include default child modules below path-attributed mod.rs implementations"
        );
}

#[test]
fn vm_jit_callback_boundary_001_source_set_scans_sibling_helper_modules_062() {
    let root = std::env::temp_dir().join(format!(
        "vo-jit-callback-sibling-helper-{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).expect("create callback module dir");
    std::fs::write(
            root.join("dangerous.rs"),
            r#"
                use super::{
                    dangerous_helper::wake,
                    r#async::raw_wake,
                };

                pub(crate) fn jit_new_callback() {
                    wake();
                    raw_wake();
                    super::dangerous_qualified::wake();
                    crate::vm::jit::callbacks::dangerous_crate::wake();
                }

                use crate::vm::jit::callbacks::{
                    dangerous_crate_grouped as dcg,
                    dangerous_crate_nested::{nested_wake},
                };
                use crate::vm::jit::callbacks as cb;
                use crate::vm::jit::callbacks as r#raw_cb;
                use crate::vm::jit::callbacks::{self as braced_cb};
                use crate::vm::jit::{callbacks as jit_grouped_cb};
                use crate::vm::{jit::callbacks::dangerous_vm_nested::wake_vm_nested};
                use crate::vm::{jit::callbacks as vm_nested_cb};
                use crate::vm::{jit::{callbacks::dangerous_vm_double_nested::wake_vm_double_nested}};
                use crate::vm::{jit::{callbacks as vm_double_nested_cb}};
                use super as scb;
                use crate::vm::{jit, types};
                use crate::vm::jit as jit_mod;
                use crate::vm as vm_mod;
                use crate::vm::{jit /* trivia */ ::callbacks::dangerous_comment_nested::{comment_nested_wake}};
                use crate::{vm::jit::callbacks::dangerous_root_grouped::root_wake};
                use crate::{vm::{jit::callbacks::dangerous_root_nested::{root_nested_wake}}};
                pub(crate) use super::dangerous_pub_helper::wake_pub;

                pub(crate) fn jit_grouped_callback() {
                    dcg::wake();
                    nested_wake();
                    cb::dangerous_alias::wake();
                    r#raw_cb::dangerous_raw_alias::wake();
                    braced_cb::dangerous_braced_alias::wake();
                    jit_grouped_cb::dangerous_jit_grouped_alias::wake();
                    wake_vm_nested();
                    vm_nested_cb::dangerous_vm_nested_alias::wake();
                    wake_vm_double_nested();
                    vm_double_nested_cb::dangerous_vm_double_nested_alias::wake();
                    scb::dangerous_super_alias::wake();
                    jit::callbacks::dangerous_jit_bare_parent::wake();
                    jit::callbacks::dangerous_jit_grouped_bare_parent::wake();
                    jit_mod::callbacks::dangerous_jit_parent_alias::wake();
                    vm_mod::jit::callbacks::dangerous_vm_parent_alias::wake();
                    comment_nested_wake();
                    root_wake();
                    root_nested_wake();
                    wake_pub();
                }
            "#,
        )
        .expect("write callback module using sibling helper");
    std::fs::write(
        root.join("dangerous_helper.rs"),
        "fn wake() { Scheduler::try_wake_fiber(&mut vm.scheduler, fid); }",
    )
    .expect("write sibling callback helper");
    std::fs::write(
        root.join("async.rs"),
        "fn raw_wake() { Scheduler::wake_queue_waiter(&mut vm.scheduler, waiter); }",
    )
    .expect("write raw-identifier sibling callback helper");
    std::fs::write(
        root.join("dangerous_qualified.rs"),
        "fn wake() { Scheduler::wake_host_event(&mut vm.scheduler, host_key); }",
    )
    .expect("write qualified sibling callback helper");
    std::fs::write(
        root.join("dangerous_crate.rs"),
        "fn wake() { Scheduler::wake_io_token(&mut vm.scheduler, token); }",
    )
    .expect("write crate-qualified sibling callback helper");
    std::fs::write(
        root.join("dangerous_crate_grouped.rs"),
        "fn wake() { Scheduler::wake_io(&mut vm.scheduler, io_key); }",
    )
    .expect("write grouped crate-qualified sibling callback helper");
    std::fs::write(
            root.join("dangerous_crate_nested.rs"),
            "fn nested_wake() { Scheduler::wake_host_event_with_data(&mut vm.scheduler, host_key, data); }",
        )
        .expect("write nested grouped crate-qualified sibling callback helper");
    std::fs::write(
        root.join("dangerous_alias.rs"),
        "fn wake() { Scheduler::schedule_next(&mut vm.scheduler); }",
    )
    .expect("write callbacks-module alias sibling callback helper");
    std::fs::write(
        root.join("dangerous_raw_alias.rs"),
        "fn wake() { Scheduler::wake_raw_callback_alias_probe(&mut vm.scheduler, token); }",
    )
    .expect("write raw callbacks-module alias sibling callback helper");
    std::fs::write(
        root.join("dangerous_braced_alias.rs"),
        "fn wake() { Scheduler::yield_current(&mut vm.scheduler); }",
    )
    .expect("write braced callbacks-module alias sibling callback helper");
    std::fs::write(
        root.join("dangerous_jit_grouped_alias.rs"),
        "fn wake() { Scheduler::block_for_queue(&mut vm.scheduler); }",
    )
    .expect("write jit grouped callbacks-module alias sibling callback helper");
    std::fs::write(
        root.join("dangerous_vm_nested.rs"),
        "fn wake_vm_nested() { Scheduler::wake_vm_nested_probe(&mut vm.scheduler, token); }",
    )
    .expect("write nested vm use-tree callback helper");
    std::fs::write(
        root.join("dangerous_vm_nested_alias.rs"),
        "fn wake() { Scheduler::wake_vm_nested_alias_probe(&mut vm.scheduler, token); }",
    )
    .expect("write nested vm use-tree callback alias helper");
    std::fs::write(
            root.join("dangerous_vm_double_nested.rs"),
            "fn wake_vm_double_nested() { Scheduler::wake_vm_double_nested_probe(&mut vm.scheduler, token); }",
        )
        .expect("write nested braced vm use-tree callback helper");
    std::fs::write(
        root.join("dangerous_vm_double_nested_alias.rs"),
        "fn wake() { Scheduler::wake_vm_double_nested_alias_probe(&mut vm.scheduler, token); }",
    )
    .expect("write nested braced vm use-tree callback alias helper");
    std::fs::write(
        root.join("dangerous_super_alias.rs"),
        "fn wake() { Scheduler::block_for_host_event(&mut vm.scheduler, host_key); }",
    )
    .expect("write super-module alias sibling callback helper");
    std::fs::write(
        root.join("dangerous_jit_bare_parent.rs"),
        "fn wake() { Scheduler::wake_jit_bare_parent_probe(&mut vm.scheduler, token); }",
    )
    .expect("write jit bare parent-module callback helper");
    std::fs::write(
        root.join("dangerous_jit_grouped_bare_parent.rs"),
        "fn wake() { Scheduler::wake_jit_grouped_bare_parent_probe(&mut vm.scheduler, token); }",
    )
    .expect("write grouped jit bare parent-module callback helper");
    std::fs::write(
        root.join("dangerous_jit_parent_alias.rs"),
        "fn wake() { Scheduler::wake_jit_parent_alias_probe(&mut vm.scheduler, token); }",
    )
    .expect("write jit parent-module alias callback helper");
    std::fs::write(
        root.join("dangerous_vm_parent_alias.rs"),
        "fn wake() { Scheduler::wake_vm_parent_alias_probe(&mut vm.scheduler, token); }",
    )
    .expect("write vm parent-module alias callback helper");
    std::fs::write(
            root.join("dangerous_comment_nested.rs"),
            "fn comment_nested_wake() { Scheduler::wake_comment_nested_probe(&mut vm.scheduler, token); }",
        )
        .expect("write comment-trivia nested use-tree callback helper");
    std::fs::write(
            root.join("dangerous_root_grouped.rs"),
            "fn root_wake() { Scheduler::wake_queue_waiter_with_result(&mut vm.scheduler, waiter, result); }",
        )
        .expect("write root-grouped crate callback helper");
    std::fs::write(
        root.join("dangerous_root_nested.rs"),
        "fn root_nested_wake() { Scheduler::wake_root_nested_probe(&mut vm.scheduler, token); }",
    )
    .expect("write root-grouped nested crate callback helper");
    std::fs::write(
        root.join("dangerous_pub_helper.rs"),
        "fn wake_pub() { Scheduler::wake_pub_use_probe(&mut vm.scheduler, token); }",
    )
    .expect("write visibility-qualified pub use callback helper");

    let source = read_jit_source_062(&root, "dangerous.rs");

    assert!(
        source.contains("Scheduler::try_wake_fiber"),
        "callback source-set scans must include sibling helper modules used by callbacks"
    );
    assert!(
        source.contains("Scheduler::wake_queue_waiter"),
        "callback source-set scans must include raw-identifier sibling helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_host_event"),
        "callback source-set scans must include direct super-qualified sibling helpers"
    );
    assert!(
        source.contains("Scheduler::wake_io_token"),
        "callback source-set scans must include crate-qualified callback helpers"
    );
    assert!(
        source.contains("Scheduler::wake_io"),
        "callback source-set scans must include grouped crate-qualified callback helper imports"
    );
    assert!(
            source.contains("Scheduler::wake_host_event_with_data"),
            "callback source-set scans must include nested grouped crate-qualified callback helper imports"
        );
    assert!(
        source.contains("Scheduler::schedule_next"),
        "callback source-set scans must include callbacks-module alias helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_raw_callback_alias_probe"),
        "callback source-set scans must include raw callbacks-module alias helper imports"
    );
    assert!(
        source.contains("Scheduler::yield_current"),
        "callback source-set scans must include braced callbacks-module alias helper imports"
    );
    assert!(
        source.contains("Scheduler::block_for_queue"),
        "callback source-set scans must include grouped jit-module callbacks alias helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_vm_nested_probe"),
        "callback source-set scans must include nested crate::vm use-tree callback helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_vm_nested_alias_probe"),
        "callback source-set scans must include nested crate::vm use-tree callback module aliases"
    );
    assert!(
            source.contains("Scheduler::wake_vm_double_nested_probe"),
            "callback source-set scans must include nested-braced crate::vm use-tree callback helper imports"
        );
    assert!(
            source.contains("Scheduler::wake_vm_double_nested_alias_probe"),
            "callback source-set scans must include nested-braced crate::vm use-tree callback module aliases"
        );
    assert!(
        source.contains("Scheduler::block_for_host_event"),
        "callback source-set scans must include super-module alias helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_jit_bare_parent_probe"),
        "callback source-set scans must include bare jit parent-module helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_jit_grouped_bare_parent_probe"),
        "callback source-set scans must include grouped bare jit parent-module helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_jit_parent_alias_probe"),
        "callback source-set scans must include jit parent-module alias helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_vm_parent_alias_probe"),
        "callback source-set scans must include vm parent-module alias helper imports"
    );
    assert!(
            source.contains("Scheduler::wake_comment_nested_probe"),
            "callback source-set scans must include comment-trivia nested crate::vm use-tree helper imports"
        );
    assert!(
        source.contains("Scheduler::wake_queue_waiter_with_result"),
        "callback source-set scans must include root-grouped crate helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_root_nested_probe"),
        "callback source-set scans must include root-grouped nested crate helper imports"
    );
    assert!(
        source.contains("Scheduler::wake_pub_use_probe"),
        "callback source-set scans must include visibility-qualified helper imports"
    );
}

#[test]
fn vm_jit_callback_boundary_001_source_set_scans_late_root_grouped_parent_modules_062() {
    let root = std::env::temp_dir().join(format!(
        "vo-jit-callback-late-root-parent-{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).expect("create callback module dir");
    std::fs::write(
        root.join("dangerous.rs"),
        r#"
                use crate::{types, vm::{jit}};
                use crate::{types as root_types, vm::{jit as late_jit}};
                use crate::{types as root_types_alias, vm as late_vm};

                pub(crate) fn jit_new_callback() {
                    jit::callbacks::dangerous_late_root_jit_bare::wake();
                    late_jit::callbacks::dangerous_late_root_jit_alias::wake();
                    late_vm::jit::callbacks::dangerous_late_root_vm_alias::wake();
                }
            "#,
    )
    .expect("write callback module using late root grouped parent modules");
    std::fs::write(
        root.join("dangerous_late_root_jit_bare.rs"),
        "fn wake() { Scheduler::wake_late_root_jit_bare_probe(&mut vm.scheduler, token); }",
    )
    .expect("write late root grouped bare jit helper");
    std::fs::write(
        root.join("dangerous_late_root_jit_alias.rs"),
        "fn wake() { Scheduler::wake_late_root_jit_alias_probe(&mut vm.scheduler, token); }",
    )
    .expect("write late root grouped jit alias helper");
    std::fs::write(
        root.join("dangerous_late_root_vm_alias.rs"),
        "fn wake() { Scheduler::wake_late_root_vm_alias_probe(&mut vm.scheduler, token); }",
    )
    .expect("write late root grouped vm alias helper");

    let source = read_jit_source_062(&root, "dangerous.rs");

    assert!(
            source.contains("Scheduler::wake_late_root_jit_bare_probe"),
            "callback source-set scans must include root-grouped bare jit parent imports after earlier grouped items"
        );
    assert!(
            source.contains("Scheduler::wake_late_root_jit_alias_probe"),
            "callback source-set scans must include root-grouped jit parent aliases after earlier grouped items"
        );
    assert!(
            source.contains("Scheduler::wake_late_root_vm_alias_probe"),
            "callback source-set scans must include root-grouped vm parent aliases after earlier grouped items"
        );
}

#[test]
fn vm_jit_callback_boundary_001_source_set_scans_include_implementation_files_062() {
    let root = std::env::temp_dir().join(format!(
        "vo-jit-callback-include-helper-{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).expect("create callback module dir");
    std::fs::write(
            root.join("dangerous.rs"),
            r#"
                include! /* callback include comment */ (
                    /* callback include body comment */ "dangerous_impl.inc"
                );
                include!(concat!("dangerous_concat", ".inc"));
                include!(concat /* callback concat comment */ ! (
                    /* callback concat arg comment */ "dangerous_concat_comment",
                    ".inc"
                ));
                include!(concat! [ "dangerous_concat_bracket", ".inc" ]);
                include!(concat!(env!("CARGO_MANIFEST_DIR"), "/dangerous_env.inc"));
                include!(concat!(env!("CARGO_MANIFEST_DIR", "manifest dir required"), "/dangerous_env_msg.inc"));
                include!(concat!(env!("CARGO_MANIFEST_DIR"), '/', "dangerous_env_char.inc"));
                use std::include as rust_include;
                rust_include!("dangerous_aliased.inc");
                macro_rules! include {
                    ($path:literal) => {};
                }
                std::include!("dangerous_qualified.inc");

                pub(crate) fn jit_new_callback() {
                    wake();
                    wake_concat();
                    wake_concat_comment();
                    wake_concat_bracket();
                    wake_env();
                    wake_env_msg();
                    wake_env_char();
                    wake_aliased();
                    wake_qualified();
                }
            "#,
        )
        .expect("write callback module using include implementation");
    std::fs::write(
        root.join("dangerous_impl.inc"),
        "fn wake() { Scheduler::wake_io_token(&mut vm.scheduler, token); }",
    )
    .expect("write included callback implementation");
    std::fs::write(
            root.join("dangerous_concat.inc"),
            "fn wake_concat() { Scheduler::wake_queue_waiter_with_result(&mut vm.scheduler, fid, result); }",
        )
        .expect("write concat included callback implementation");
    std::fs::write(
            root.join("dangerous_concat_comment.inc"),
            "fn wake_concat_comment() { Scheduler::wake_host_event_with_data(&mut vm.scheduler, key, data); }",
        )
        .expect("write comment concat included callback implementation");
    std::fs::write(
            root.join("dangerous_concat_bracket.inc"),
            "fn wake_concat_bracket() { Scheduler::wake_queue_sender_closed(&mut vm.scheduler, waiter); }",
        )
        .expect("write bracket concat included callback implementation");
    std::fs::write(
        root.join("dangerous_env.inc"),
        "fn wake_env() { Scheduler::wake_io(&mut vm.scheduler, key); }",
    )
    .expect("write env concat included callback implementation");
    std::fs::write(
        root.join("dangerous_env_msg.inc"),
        "fn wake_env_msg() { Scheduler::wake_host_event(&mut vm.scheduler, key); }",
    )
    .expect("write env concat with message callback implementation");
    std::fs::write(
        root.join("dangerous_env_char.inc"),
        "fn wake_env_char() { Scheduler::wake_select_ready(&mut vm.scheduler, key); }",
    )
    .expect("write env concat with char separator callback implementation");
    std::fs::write(
        root.join("dangerous_aliased.inc"),
        "fn wake_aliased() { Scheduler::wake_aliased_include_probe(&mut vm.scheduler, key); }",
    )
    .expect("write aliased include callback implementation");
    std::fs::write(
        root.join("dangerous_qualified.inc"),
        "fn wake_qualified() { Scheduler::wake_qualified_include_probe(&mut vm.scheduler, key); }",
    )
    .expect("write qualified include callback implementation");

    let source = read_jit_source_062(&root, "dangerous.rs");

    assert!(
        source.contains("Scheduler::wake_io_token"),
        "callback source-set scans must include production include! implementation files"
    );
    assert!(
        source.contains("Scheduler::wake_queue_waiter_with_result"),
        "callback source-set scans must include literal concat! include implementation files"
    );
    assert!(
            source.contains("Scheduler::wake_host_event_with_data"),
            "callback source-set scans must include comment-wrapped concat! include implementation files"
        );
    assert!(
            source.contains("Scheduler::wake_queue_sender_closed"),
            "callback source-set scans must include bracket-delimited concat! include implementation files"
        );
    assert!(
            source.contains("Scheduler::wake_io"),
            "callback source-set scans must include env!(CARGO_MANIFEST_DIR) concat! include implementation files"
        );
    assert!(
            source.contains("Scheduler::wake_host_event"),
            "callback source-set scans must include env!(CARGO_MANIFEST_DIR, message) concat! include implementation files"
        );
    assert!(
            source.contains("Scheduler::wake_select_ready"),
            "callback source-set scans must include char literal separator concat! include implementation files"
        );
    assert!(
        source.contains("Scheduler::wake_aliased_include_probe"),
        "callback source-set scans must include std::include aliases"
    );
    assert!(
            source.contains("Scheduler::wake_qualified_include_probe"),
            "callback source-set scans must include path-qualified std::include! under local bare include shadowing"
        );
}

#[test]
fn vm_jit_callback_boundary_001_source_set_ignores_shadowed_include_implementation_files_062() {
    let root = std::env::temp_dir().join(format!(
        "vo-jit-callback-shadowed-include-helper-{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(&root).expect("create callback module dir");
    std::fs::write(
        root.join("dangerous.rs"),
        r#"
                macro_rules! include {
                    ($path:literal) => {};
                }

                include!("shadowed_callback.inc");

                pub(crate) fn jit_new_callback() {}
            "#,
    )
    .expect("write callback module using shadowed include");
    std::fs::write(
        root.join("shadowed_callback.inc"),
        "fn shadowed_wake() { Scheduler::shadowed_include_probe(&mut vm.scheduler, token); }",
    )
    .expect("write shadowed callback include implementation");

    let source = read_jit_source_062(&root, "dangerous.rs");

    assert!(
            !source.contains("Scheduler::shadowed_include_probe"),
            "callback source-set scans must not include files referenced by locally shadowed include! macros"
        );
}

fn jit_callback_production_sources_062() -> Vec<(String, String)> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("src/vm/jit");
    let callbacks_root = root.join("callbacks");
    let callbacks_mod = std::fs::read_to_string(callbacks_root.join("mod.rs"))
        .expect("callbacks/mod.rs should be readable");
    let context_src =
        std::fs::read_to_string(root.join("context.rs")).expect("context.rs should be readable");
    let (source_names, errors) =
        jit_context_assignment_source_names_062(&context_src, &callbacks_mod);
    assert!(
        errors.is_empty(),
        "JitContext callback assignments must resolve to scanned source files:\n{}",
        errors.join("\n")
    );
    let callback_modules = callback_source_module_names_from_mod_062(&callbacks_mod);
    let mut sources = Vec::new();
    sources.push((
        "context".to_string(),
        production_source_before_test_module_062(&context_src),
    ));
    for source_name in source_names {
        let source_root = if callback_modules.contains(&source_name) {
            &callbacks_root
        } else {
            &root
        };
        sources.push((
            source_name.clone(),
            read_jit_source_062(source_root, &format!("{source_name}.rs")),
        ));
    }
    sources
}

fn read_jit_source_062(root: &std::path::Path, file: &str) -> String {
    let mut pending = vec![root.join(file)];
    let mut seen = std::collections::BTreeSet::new();
    let mut production = String::new();
    while let Some(path) = pending.pop() {
        let path = normalize_source_path_062(path);
        if !seen.insert(path.clone()) {
            continue;
        }
        let source = std::fs::read_to_string(&path)
            .unwrap_or_else(|err| panic!("failed to read JIT source {}: {err}", path.display()));
        let source_production = production_source_before_test_module_062(&source);
        if !production.is_empty() {
            production.push('\n');
        }
        production.push_str(&source_production);

        if let Some(stem) = path.file_stem().and_then(|stem| stem.to_str()) {
            let child_dir = if stem == "mod" {
                path.parent().unwrap_or(root).to_path_buf()
            } else {
                path.parent().unwrap_or(root).join(stem)
            };
            if child_dir.is_dir() {
                for (child_path, _child_source) in
                    vo_source_contract::production_sources_without_test_modules(&child_dir)
                {
                    if !jit_callback_production_child_source_path_062(&child_path) {
                        continue;
                    }
                    pending.push(child_path);
                }
            }
        }
        let parent = path.parent().unwrap_or(root);
        pending.extend(
            vo_source_contract::production_include_files_from_production_source(
                &source_production,
                &source,
                parent,
            ),
        );
        pending.extend(production_path_attr_module_files_062(
            &source_production,
            parent,
        ));
        pending.extend(super_module_files_062(&source_production, parent));
    }
    production
}

fn jit_callback_production_child_source_path_062(path: &std::path::Path) -> bool {
    path.file_name() != Some(std::ffi::OsStr::new("tests.rs"))
        && !path
            .components()
            .any(|component| component.as_os_str() == std::ffi::OsStr::new("tests"))
}

fn production_path_attr_module_files_062(
    source: &str,
    parent: &std::path::Path,
) -> Vec<std::path::PathBuf> {
    let mut files = Vec::new();
    let mut pending_path = None;
    let mut pending_attr = None::<String>;
    for line in source.lines().map(str::trim) {
        if let Some(attr) = pending_attr.as_mut() {
            attr.push_str(line);
            if attr.ends_with(']') {
                if let Some(path) = path_attr_literal_062(attr) {
                    pending_path = Some(path);
                }
                pending_attr = None;
            }
            continue;
        }
        if line.starts_with("#[") {
            if line.ends_with(']') {
                if let Some(path) = path_attr_literal_062(line) {
                    pending_path = Some(path);
                }
            } else {
                pending_attr = Some(line.to_string());
            }
            continue;
        }
        let module_line = strip_visibility_prefix_062(line);
        if module_line.starts_with("mod ") && module_line.ends_with(';') {
            if let Some(path) = pending_path.take() {
                files.push(normalize_source_path_062(parent.join(path)));
            }
            continue;
        }
        if !line.is_empty() {
            pending_path = None;
        }
    }
    files
}

fn super_module_files_062(source: &str, parent: &std::path::Path) -> Vec<std::path::PathBuf> {
    let mut modules = std::collections::BTreeSet::new();
    for statement in rust_use_statements_062(source) {
        let rest = statement
            .strip_prefix("use super::")
            .or_else(|| statement.strip_prefix("use crate::vm::jit::callbacks::"));
        if let Some(rest) = rest {
            for module in callback_use_module_names_062(rest) {
                modules.insert(module);
            }
        }
        if let Some(rest) = statement
            .strip_prefix("use crate::vm::{")
            .and_then(|rest| rest.strip_suffix("};"))
        {
            for module in nested_vm_callback_use_module_names_062(rest) {
                modules.insert(module);
            }
        }
        if let Some(rest) = statement
            .strip_prefix("use crate::{")
            .and_then(|rest| rest.strip_suffix("};"))
        {
            for module in nested_vm_callback_use_module_names_062(rest) {
                modules.insert(module);
            }
        }
    }
    modules.extend(qualified_callback_helper_module_names_062(source));

    let mut files = Vec::new();
    for module in modules {
        let path = parent.join(format!("{module}.rs"));
        if path.is_file() {
            files.push(normalize_source_path_062(path));
        }
    }
    files
}

fn rust_use_statements_062(source: &str) -> Vec<String> {
    let mut statements = Vec::new();
    let mut pending = None::<String>;
    for line in source.lines().map(str::trim) {
        if let Some(current) = pending.as_mut() {
            current.push_str(line);
            if line.ends_with(';') {
                statements.push(pending.take().expect("pending use statement"));
            }
            continue;
        }
        let line = strip_visibility_prefix_062(line);
        if !line.starts_with("use ") {
            continue;
        }
        if line.ends_with(';') {
            statements.push(line.to_string());
        } else {
            pending = Some(line.to_string());
        }
    }
    statements
}

fn strip_visibility_prefix_062(mut source: &str) -> &str {
    loop {
        let Some(stripped) = source
            .strip_prefix("pub(crate) ")
            .or_else(|| source.strip_prefix("pub(super) "))
            .or_else(|| source.strip_prefix("pub "))
        else {
            if let Some(rest) = source.strip_prefix("pub(") {
                if let Some(close) = rest.find(')') {
                    if let Some(stripped) = rest[close + 1..].strip_prefix(' ') {
                        source = stripped;
                        continue;
                    }
                }
            }
            return source;
        };
        source = stripped;
    }
}

fn callback_use_module_names_062(rest: &str) -> Vec<String> {
    if let Some(body) = rest
        .strip_prefix('{')
        .and_then(|rest| rest.strip_suffix("};"))
    {
        return body
            .split(',')
            .filter_map(|item| first_path_segment_062(item.trim()))
            .collect();
    }
    first_path_segment_062(rest).into_iter().collect()
}

fn nested_vm_callback_use_module_names_062(rest: &str) -> Vec<String> {
    let (compact, _) = compact_source(rest);
    let compact =
        std::str::from_utf8(&compact).expect("compacted callback use should remain UTF-8");
    let mut modules = std::collections::BTreeSet::new();
    for prefix in ["jit::callbacks::", "jit::{callbacks::"] {
        let mut cursor = compact;
        while let Some(pos) = cursor.find(prefix) {
            let after = &cursor[pos + prefix.len()..];
            if let Some(module) = first_path_segment_062(after) {
                modules.insert(module);
            }
            cursor = &after[1.min(after.len())..];
        }
    }
    modules.into_iter().collect()
}

fn first_path_segment_062(path: &str) -> Option<String> {
    let path = path.trim().strip_prefix("r#").unwrap_or(path.trim());
    let segment: String = path
        .chars()
        .take_while(|ch| ch.is_ascii_alphanumeric() || *ch == '_')
        .collect();
    (!segment.is_empty()).then_some(segment)
}

fn qualified_callback_helper_module_names_062(source: &str) -> Vec<String> {
    let (compact, _) = compact_source(source);
    let mut modules = std::collections::BTreeSet::new();
    let mut prefixes = vec![
        b"super::".to_vec(),
        b"crate::vm::jit::callbacks::".to_vec(),
        b"vm::jit::callbacks::".to_vec(),
    ];
    prefixes.extend(
        callback_module_aliases_062(source)
            .into_iter()
            .map(|alias| {
                let mut prefix = alias.into_bytes();
                prefix.extend_from_slice(b"::");
                prefix
            }),
    );
    prefixes.extend(callback_parent_module_alias_prefixes_062(source));
    for prefix in prefixes {
        let prefix = prefix.as_slice();
        let mut idx = 0usize;
        while idx + prefix.len() < compact.len() {
            if !compact[idx..].starts_with(prefix) {
                idx += 1;
                continue;
            }
            if let Some(module) = first_compact_path_segment(&compact[idx + prefix.len()..]) {
                modules.insert(module);
            }
            idx += prefix.len();
        }
    }
    modules.into_iter().collect()
}

fn callback_module_aliases_062(source: &str) -> std::collections::BTreeSet<String> {
    let (compact, _) = compact_source(source);
    let source =
        std::str::from_utf8(&compact).expect("compacted callback source should remain valid UTF-8");
    let mut aliases = std::collections::BTreeSet::new();
    for prefix in [
        "usecrate::vm::jit::callbacksas",
        "usesuperas",
        "usecrate::{vm::jit::callbacksas",
        "usecrate::vm::{jit::callbacksas",
        "usecrate::vm::{jit::{callbacksas",
        "usecrate::{vm::{jit::callbacksas",
        "usecrate::{vm::{jit::{callbacksas",
    ] {
        collect_ident_after_prefix_062(source, prefix, &mut aliases);
    }
    aliases
}

fn callback_parent_module_alias_prefixes_062(source: &str) -> Vec<Vec<u8>> {
    let (compact, _) = compact_source(source);
    let source =
        std::str::from_utf8(&compact).expect("compacted callback source should remain valid UTF-8");
    let mut jit_aliases = std::collections::BTreeSet::new();
    for prefix in [
        "usecrate::vm::jitas",
        "usecrate::{vm::jitas",
        "usecrate::vm::{jitas",
        "usecrate::{vm::{jitas",
    ] {
        collect_ident_after_prefix_062(source, prefix, &mut jit_aliases);
    }
    collect_compact_grouped_use_aliases(source, "usecrate::vm::{", "jit", &mut jit_aliases);
    collect_compact_grouped_use_nested_aliases(
        source,
        "usecrate::{",
        "vm",
        "jit",
        &mut jit_aliases,
    );
    let mut vm_aliases = std::collections::BTreeSet::new();
    for prefix in ["usecrate::vmas", "usecrate::{vmas"] {
        collect_ident_after_prefix_062(source, prefix, &mut vm_aliases);
    }
    collect_compact_grouped_use_aliases(source, "usecrate::{", "vm", &mut vm_aliases);

    let mut prefixes = Vec::new();
    if ["usecrate::vm::jit;", "usecrate::{vm::jit}"]
        .iter()
        .any(|needle| source.contains(needle))
        || compact_grouped_use_contains_bare_item(source, "usecrate::vm::{", "jit")
        || compact_grouped_use_contains_bare_item(source, "usecrate::{vm::{", "jit")
        || compact_grouped_use_contains_nested_bare_item(source, "usecrate::{", "vm", "jit")
    {
        prefixes.push(b"jit::callbacks::".to_vec());
    }
    if ["usecrate::vm;", "usecrate::{vm}"]
        .iter()
        .any(|needle| source.contains(needle))
        || compact_grouped_use_contains_bare_item(source, "usecrate::{", "vm")
    {
        prefixes.push(b"vm::jit::callbacks::".to_vec());
    }
    prefixes.extend(jit_aliases.into_iter().map(|alias| {
        let mut prefix = alias.into_bytes();
        prefix.extend_from_slice(b"::callbacks::");
        prefix
    }));
    prefixes.extend(vm_aliases.into_iter().map(|alias| {
        let mut prefix = alias.into_bytes();
        prefix.extend_from_slice(b"::jit::callbacks::");
        prefix
    }));
    prefixes
}

fn compact_grouped_use_contains_bare_item(source: &str, prefix: &str, item: &str) -> bool {
    compact_grouped_use_bodies_after_prefix(source, prefix)
        .iter()
        .any(|body| compact_grouped_use_body_contains_bare_item(body, item))
}

fn compact_grouped_use_contains_nested_bare_item(
    source: &str,
    prefix: &str,
    parent: &str,
    item: &str,
) -> bool {
    compact_grouped_use_bodies_after_prefix(source, prefix)
        .iter()
        .flat_map(|body| compact_grouped_use_nested_bodies(body, parent))
        .any(|body| compact_grouped_use_body_contains_bare_item(&body, item))
}

fn collect_compact_grouped_use_aliases(
    source: &str,
    prefix: &str,
    item: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) {
    for body in compact_grouped_use_bodies_after_prefix(source, prefix) {
        collect_compact_grouped_use_body_aliases(&body, item, aliases);
    }
}

fn collect_compact_grouped_use_nested_aliases(
    source: &str,
    prefix: &str,
    parent: &str,
    item: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) {
    for body in compact_grouped_use_bodies_after_prefix(source, prefix) {
        for nested in compact_grouped_use_nested_bodies(&body, parent) {
            collect_compact_grouped_use_body_aliases(&nested, item, aliases);
        }
    }
}

fn compact_grouped_use_bodies_after_prefix(source: &str, prefix: &str) -> Vec<String> {
    let bytes = source.as_bytes();
    let mut bodies = Vec::new();
    let mut cursor = 0usize;
    while let Some(rel_start) = source[cursor..].find(prefix) {
        let body_start = cursor + rel_start + prefix.len();
        let mut idx = body_start;
        let mut depth = 1usize;
        while idx < bytes.len() {
            match bytes[idx] {
                b'{' => depth += 1,
                b'}' => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        break;
                    }
                }
                _ => {}
            }
            idx += 1;
        }
        if depth != 0 {
            break;
        }
        bodies.push(source[body_start..idx].to_string());
        cursor = idx + 1;
    }
    bodies
}

fn compact_grouped_use_body_contains_bare_item(body: &str, item: &str) -> bool {
    compact_grouped_use_body_segments(body)
        .into_iter()
        .any(|segment| compact_grouped_use_segment_is_bare_item(segment, item))
}

fn collect_compact_grouped_use_body_aliases(
    body: &str,
    item: &str,
    aliases: &mut std::collections::BTreeSet<String>,
) {
    for segment in compact_grouped_use_body_segments(body) {
        if let Some(alias) = compact_grouped_use_segment_alias(segment, item) {
            aliases.insert(alias);
        }
    }
}

fn compact_grouped_use_nested_bodies(body: &str, parent: &str) -> Vec<String> {
    compact_grouped_use_body_segments(body)
        .into_iter()
        .filter_map(|segment| compact_grouped_use_segment_nested_body(segment, parent))
        .map(str::to_string)
        .collect()
}

fn compact_grouped_use_body_segments(body: &str) -> Vec<&str> {
    let bytes = body.as_bytes();
    let mut segments = Vec::new();
    let mut start = 0usize;
    let mut depth = 0usize;
    for idx in 0..=bytes.len() {
        if idx == bytes.len() || (bytes[idx] == b',' && depth == 0) {
            segments.push(&body[start..idx]);
            start = idx + 1;
            continue;
        }
        match bytes[idx] {
            b'{' => depth += 1,
            b'}' => depth = depth.saturating_sub(1),
            _ => {}
        }
    }
    segments
}

fn compact_grouped_use_segment_is_bare_item(segment: &str, item: &str) -> bool {
    let Some((ident, len)) = compact_ident_token(segment) else {
        return false;
    };
    ident.strip_prefix("r#").unwrap_or(&ident) == item && len == segment.len()
}

fn compact_grouped_use_segment_alias(segment: &str, item: &str) -> Option<String> {
    for item_token in [item.to_string(), format!("r#{item}")] {
        let Some(rest) = segment
            .strip_prefix(&item_token)
            .and_then(|rest| rest.strip_prefix("as"))
        else {
            continue;
        };
        let (alias, len) = compact_ident_token(rest)?;
        if len == rest.len() && rust_ident_string_062(&alias) {
            return Some(alias);
        }
    }
    None
}

fn compact_grouped_use_segment_nested_body<'a>(segment: &'a str, item: &str) -> Option<&'a str> {
    for item_token in [item.to_string(), format!("r#{item}")] {
        if let Some(body) = segment
            .strip_prefix(&item_token)
            .and_then(|rest| rest.strip_prefix("::{"))
            .and_then(|rest| rest.strip_suffix('}'))
        {
            return Some(body);
        }
    }
    None
}

fn first_compact_path_segment(path: &[u8]) -> Option<String> {
    let (start, raw_prefix) = if path.starts_with(b"r#") {
        (2, true)
    } else {
        (0, false)
    };
    let first = *path.get(start)?;
    if !rust_ident_start_062(first) {
        return None;
    }
    let mut end = start + 1;
    while path
        .get(end)
        .is_some_and(|byte| rust_ident_continue_062(*byte))
    {
        end += 1;
    }
    let ident = std::str::from_utf8(&path[start..end]).ok()?;
    if raw_prefix || !ident.is_empty() {
        Some(ident.to_string())
    } else {
        None
    }
}

fn path_attr_literal_062(line: &str) -> Option<String> {
    let attr = line.strip_prefix("#[")?.strip_suffix(']')?.trim();
    if let Some(path) = direct_path_attr_literal_062(attr) {
        return Some(path);
    }
    let rest = attr.strip_prefix("cfg_attr")?.trim_start();
    let body = rest.strip_prefix('(')?.strip_suffix(')')?;
    let mut args = split_cfg_attr_args_062(body);
    if args.len() < 2 {
        return None;
    }
    let predicate = args.remove(0);
    if !cfg_expr_can_be_true_without_test_062(predicate) {
        return None;
    }
    args.into_iter().find_map(direct_path_attr_literal_062)
}

fn direct_path_attr_literal_062(attr: &str) -> Option<String> {
    let rest = attr.trim().strip_prefix("path")?.trim_start();
    let rest = rest.strip_prefix('=')?.trim_start();
    decode_rust_string_literal_062(rest)
}

fn decode_rust_string_literal_062(source: &str) -> Option<String> {
    let bytes = source.as_bytes();
    if bytes.first() == Some(&b'"') {
        let mut idx = 1usize;
        while idx < bytes.len() {
            if bytes[idx] == b'\\' {
                idx += 2;
                continue;
            }
            if bytes[idx] == b'"' {
                return Some(source[1..idx].replace("\\\"", "\"").replace("\\\\", "\\"));
            }
            idx += 1;
        }
        return None;
    }
    let mut cursor = 0usize;
    if bytes.get(cursor) != Some(&b'r') {
        return None;
    }
    cursor += 1;
    let mut hashes = 0usize;
    while bytes.get(cursor) == Some(&b'#') {
        hashes += 1;
        cursor += 1;
    }
    if bytes.get(cursor) != Some(&b'"') {
        return None;
    }
    let content_start = cursor + 1;
    cursor = content_start;
    while cursor < bytes.len() {
        if bytes.get(cursor) == Some(&b'"')
            && (0..hashes).all(|offset| bytes.get(cursor + 1 + offset) == Some(&b'#'))
        {
            return Some(source[content_start..cursor].to_string());
        }
        cursor += 1;
    }
    None
}

fn split_cfg_attr_args_062(source: &str) -> Vec<&str> {
    let mut args = Vec::new();
    let mut start = 0usize;
    let mut depth = 0usize;
    let mut in_string = false;
    let bytes = source.as_bytes();
    let mut idx = 0usize;
    while idx < bytes.len() {
        if in_string {
            if bytes[idx] == b'\\' {
                idx += 2;
                continue;
            }
            if bytes[idx] == b'"' {
                in_string = false;
            }
            idx += 1;
            continue;
        }
        match bytes[idx] {
            b'"' => in_string = true,
            b'(' => depth += 1,
            b')' => depth = depth.saturating_sub(1),
            b',' if depth == 0 => {
                args.push(source[start..idx].trim());
                start = idx + 1;
            }
            _ => {}
        }
        idx += 1;
    }
    let tail = source[start..].trim();
    if !tail.is_empty() {
        args.push(tail);
    }
    args
}

fn cfg_expr_can_be_true_without_test_062(expr: &str) -> bool {
    let expr = expr.trim();
    if expr == "test" {
        return false;
    }
    if let Some(body) = cfg_call_body_062(expr, "all") {
        return split_cfg_attr_args_062(body)
            .into_iter()
            .all(cfg_expr_can_be_true_without_test_062);
    }
    if let Some(body) = cfg_call_body_062(expr, "any") {
        return split_cfg_attr_args_062(body)
            .into_iter()
            .any(cfg_expr_can_be_true_without_test_062);
    }
    if let Some(body) = cfg_call_body_062(expr, "not") {
        return cfg_expr_can_be_false_without_test_062(body);
    }
    true
}

fn cfg_expr_can_be_false_without_test_062(expr: &str) -> bool {
    let expr = expr.trim();
    if expr == "test" {
        return true;
    }
    if let Some(body) = cfg_call_body_062(expr, "all") {
        return split_cfg_attr_args_062(body)
            .into_iter()
            .any(cfg_expr_can_be_false_without_test_062);
    }
    if let Some(body) = cfg_call_body_062(expr, "any") {
        return split_cfg_attr_args_062(body)
            .into_iter()
            .all(cfg_expr_can_be_false_without_test_062);
    }
    if let Some(body) = cfg_call_body_062(expr, "not") {
        return cfg_expr_can_be_true_without_test_062(body);
    }
    true
}

fn cfg_call_body_062<'a>(expr: &'a str, name: &str) -> Option<&'a str> {
    let rest = expr.trim().strip_prefix(name)?;
    let rest = rest.trim_start();
    rest.strip_prefix('(')?.strip_suffix(')')
}

fn normalize_source_path_062(path: std::path::PathBuf) -> std::path::PathBuf {
    let mut normalized = std::path::PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::CurDir => {}
            std::path::Component::ParentDir => {
                normalized.pop();
            }
            other => normalized.push(other.as_os_str()),
        }
    }
    normalized
}

fn callback_module_names_from_mod_062(source: &str) -> std::collections::BTreeSet<String> {
    source
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            trimmed
                .strip_prefix("pub mod ")
                .and_then(|rest| rest.strip_suffix(';'))
                .map(str::trim)
                .filter(|name| !name.is_empty())
                .map(str::to_string)
        })
        .collect()
}

fn callback_source_module_names_from_mod_062(source: &str) -> std::collections::BTreeSet<String> {
    let mut modules = callback_module_names_from_mod_062(source);
    for line in source.lines().map(str::trim) {
        let Some(rest) = callback_use_rest_062(line) else {
            continue;
        };
        if let Some((module, _)) = rest.split_once("::{") {
            modules.insert(module.trim().to_string());
            continue;
        }
        if let Some((module, _)) = rest.split_once("::") {
            modules.insert(module.trim().to_string());
        }
    }
    modules
}

fn callback_exported_symbol_modules_from_mod_062(
    source: &str,
) -> std::collections::BTreeMap<String, String> {
    let mut symbols = std::collections::BTreeMap::new();
    for line in source.lines().map(str::trim) {
        let Some(rest) = callback_use_rest_062(line) else {
            continue;
        };
        if let Some((_, exported)) = rest.split_once("::{") {
            let Some((module, _)) = rest.split_once("::{") else {
                continue;
            };
            let Some(exported) = exported.strip_suffix("};") else {
                continue;
            };
            for symbol in exported.split(',').map(str::trim) {
                if !symbol.is_empty() {
                    symbols.insert(symbol.to_string(), module.trim().to_string());
                }
            }
            continue;
        }
        if let Some((module, symbol)) = rest.rsplit_once("::") {
            if let Some(symbol) = symbol.strip_suffix(';') {
                symbols.insert(symbol.trim().to_string(), module.trim().to_string());
            }
        }
    }
    symbols
}

fn callback_use_rest_062(line: &str) -> Option<&str> {
    line.strip_prefix("pub use ")
        .or_else(|| line.strip_prefix("pub(crate) use "))
        .or_else(|| line.strip_prefix("pub(super) use "))
}

fn jit_context_assignment_source_names_062(
    context_src: &str,
    callbacks_mod_src: &str,
) -> (std::collections::BTreeSet<String>, Vec<String>) {
    let callback_symbols = callback_exported_symbol_modules_from_mod_062(callbacks_mod_src);
    let frame_symbols = imported_symbols_from_module_use_062(context_src, "super::frame");
    let mut sources = std::collections::BTreeSet::new();
    let mut errors = Vec::new();
    for (field, expr) in jit_context_fn_assignments_062(context_src) {
        if let Some(symbol) = expr.strip_prefix("callbacks::") {
            match callback_symbols.get(symbol) {
                    Some(module) => {
                        sources.insert(module.clone());
                    }
                    None => errors.push(format!(
                        "JitContext field {field} references callbacks::{symbol}, but callbacks/mod.rs does not export it"
                    )),
                }
            continue;
        }
        if frame_symbols.contains(expr.as_str()) {
            sources.insert("frame".to_string());
            continue;
        }
        if expr == "super::jit_call_extern" {
            sources.insert("extern_call".to_string());
            continue;
        }
        errors.push(format!(
            "JitContext field {field} uses unresolved function pointer {expr}"
        ));
    }
    (sources, errors)
}

fn jit_context_fn_assignments_062(source: &str) -> Vec<(String, String)> {
    let mut assignments = Vec::new();
    let mut pending_field = None::<(String, String)>;
    for line in source.lines() {
        let trimmed = line.trim();
        if let Some((field, mut expr_source)) = pending_field.take() {
            expr_source.push_str(trimmed);
            if let Some(expr) = jit_context_some_fn_expr_062(&expr_source) {
                assignments.push((field, expr));
            } else if jit_context_fn_assignment_may_continue_062(&expr_source) {
                pending_field = Some((field, expr_source));
            }
            continue;
        }
        let Some((field, rest)) = trimmed.split_once(':') else {
            continue;
        };
        let field = field.trim();
        if !field.ends_with("_fn") {
            continue;
        }
        if let Some(expr) = jit_context_some_fn_expr_062(rest) {
            assignments.push((field.to_string(), expr));
        } else if jit_context_fn_assignment_may_continue_062(rest) {
            pending_field = Some((field.to_string(), rest.to_string()));
        }
    }
    assignments
}

fn jit_context_fn_assignment_may_continue_062(source: &str) -> bool {
    let trimmed = source.trim();
    trimmed.is_empty()
        || trimmed.starts_with("#[")
        || trimmed.starts_with("//")
        || trimmed.starts_with("Some(")
}

fn jit_context_some_fn_expr_062(source: &str) -> Option<String> {
    let rest = source.trim_start().strip_prefix("Some(")?;
    let close = rest.find(')')?;
    let expr = rest[..close].trim();
    (!expr.is_empty()).then(|| expr.to_string())
}

fn imported_symbols_from_module_use_062(
    source: &str,
    module: &str,
) -> std::collections::BTreeSet<String> {
    let mut symbols = std::collections::BTreeSet::new();
    let prefix = format!("use {module}::{{");
    for line in source.lines().map(str::trim) {
        let Some(rest) = line.strip_prefix(&prefix) else {
            continue;
        };
        let Some(rest) = rest.strip_suffix("};") else {
            continue;
        };
        for symbol in rest.split(',').map(str::trim) {
            if !symbol.is_empty() {
                symbols.insert(symbol.to_string());
            }
        }
    }
    symbols
}
