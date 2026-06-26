use super::{
    compact_delimiter_close, compact_pattern_line_numbers, compact_rust_source_for_contract,
    compact_rust_source_without_non_dominating_blocks_for_contract,
    production_source_without_test_items_preserving_macro_rules_062,
    production_source_without_test_modules, source_line_number,
};

#[test]
fn source_contract_production_view_062_preserves_textual_cfg_test_markers() {
    let probe = r##"
            // #[cfg(test)] in a production comment must not hide later source.
            const TEXT: &str = r#"#[cfg(test)] inside a raw string"#;
            fn production_after_textual_marker() {
                let _ = vm.apply_runtime_transition(None, transition);
            }
        "##;

    let production = production_source_without_test_modules(probe);

    assert!(
        production.contains("vm.apply_runtime_transition"),
        "textual cfg(test) markers must not truncate production source-contract scans"
    );
}

#[test]
fn source_contract_production_view_062_erases_named_test_modules() {
    let probe = r#"
            fn production_before_tests() {
                vm.run_scheduled();
            }

            #[cfg(test)]
            mod command_tests {
                fn test_only() {
                    let _ = vm.apply_runtime_transition(None, transition);
                }
            }

            fn production_after_tests() {
                vm.run_scheduled();
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(production.contains("production_before_tests"));
    assert!(production.contains("production_after_tests"));
    assert!(
        !production.contains("vm.apply_runtime_transition"),
        "real cfg(test) modules must be erased from production source-contract scans"
    );
}

#[test]
fn source_contract_production_view_062_erases_cfg_test_expression_modules() {
    let probe = r#"
            fn production_before_tests() {
                vm.run_scheduled();
            }

            #[cfg(all(test, feature = "jit"))]
            mod jit_tests {
                fn test_only() {
                    let _ = vm.apply_runtime_transition(None, transition);
                }
            }

            #[cfg(feature = "jit")]
            #[cfg(test)]
            pub(crate) mod stacked_tests {
                fn test_only() {
                    let _ = vm.apply_runtime_transition(None, transition);
                }
            }

            fn production_after_tests() {
                vm.run_scheduled();
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(production.contains("production_before_tests"));
    assert!(production.contains("production_after_tests"));
    assert!(
            !production.contains("vm.apply_runtime_transition"),
            "cfg expressions containing test, stacked attrs, and visible test modules must be erased from production scans"
        );
}

#[test]
fn source_contract_production_view_062_erases_cfg_not_not_test_modules() {
    let probe = r#"
            fn production_before_tests() {
                vm.run_scheduled();
            }

            #[cfg(not(not(test)))]
            mod double_negative_tests {
                fn test_only() {
                    let _ = vm.apply_runtime_transition(None, transition);
                }
            }

            fn production_after_tests() {
                vm.run_scheduled();
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(production.contains("production_before_tests"));
    assert!(production.contains("production_after_tests"));
    assert!(
        !production.contains("vm.apply_runtime_transition"),
        "cfg not-not(test) modules must be erased from production source-contract scans"
    );
}

#[test]
fn source_contract_production_view_062_preserves_cfg_not_test_modules() {
    let probe = r#"
            #[cfg(not(test))]
            mod production_only {
                fn production_visible() {
                    let _ = vm.apply_runtime_transition(None, transition);
                }
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(
        production.contains("vm.apply_runtime_transition"),
        "cfg not(test) modules are production-visible and must be preserved"
    );
}

#[test]
fn source_contract_production_view_062_erases_raw_identifier_test_modules() {
    let probe = r#"
            fn production_before_tests() {
                vm.run_scheduled();
            }

            #[cfg(test)]
            mod r#tests {
                fn test_only() {
                    let _ = vm.apply_runtime_transition(None, transition);
                }
            }

            fn production_after_tests() {
                vm.run_scheduled();
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(production.contains("production_before_tests"));
    assert!(production.contains("production_after_tests"));
    assert!(
        !production.contains("vm.apply_runtime_transition"),
        "raw identifier cfg(test) modules must be erased from production scans"
    );
}

#[test]
fn source_contract_production_view_062_erases_cfg_attr_cfg_test_modules() {
    let probe = r#"
            fn production_before_tests() {
                vm.run_scheduled();
            }

            #[cfg_attr(all(), cfg(test))]
            mod cfg_attr_tests {
                fn test_only() {
                    let _ = vm.apply_runtime_transition(None, transition);
                }
            }

            fn production_after_tests() {
                vm.run_scheduled();
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(production.contains("production_before_tests"));
    assert!(production.contains("production_after_tests"));
    assert!(
        !production.contains("vm.apply_runtime_transition"),
        "cfg_attr-generated cfg(test) modules must be erased from production scans"
    );
}

#[test]
fn source_contract_production_view_062_erases_cfg_test_non_module_items() {
    let probe = r#"
            fn production_before_tests() {
                vm.run_scheduled();
            }

            #[cfg(test)]
            fn test_only_fn() {
                let _ = vm.apply_runtime_transition(None, transition);
            }

            #[cfg(test)]
            const TEST_ONLY_CONST: &str = "TEST_ONLY_CONST_SENTINEL";

            #[cfg(test)]
            static TEST_ONLY_STATIC: &str = "TEST_ONLY_STATIC_SENTINEL";

            fn production_after_tests() {
                vm.run_scheduled();
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(production.contains("production_before_tests"));
    assert!(production.contains("production_after_tests"));
    assert!(
        !production.contains("vm.apply_runtime_transition")
            && !production.contains("TEST_ONLY_CONST_SENTINEL")
            && !production.contains("TEST_ONLY_STATIC_SENTINEL"),
        "cfg(test) non-module items must be erased from production scans"
    );
}

#[test]
fn source_contract_compact_non_dominating_view_062_erases_macro_and_closure_bodies() {
    let probe = r#"
            live_before();
            if false { hidden_if_false(); }
            if cfg!(any()) { hidden_cfg_any(); }
            match false { _ => { hidden_match_false(); } }
            async { hidden_async(); };
            async move { hidden_async_move(); };
            macro_rules! hidden_macro {
                () => { hidden_macro_body(); };
            }
            let _plain = || { hidden_plain_closure(); };
            let _typed = |value: usize| -> usize { hidden_typed_closure(value) };
            let _move = move |value: usize| { hidden_move_closure(value) };
            fn hidden_local_fn() { hidden_local_fn_body(); }
            live_after();
        "#;
    let compact = compact_rust_source_for_contract(probe).0;
    let filtered = compact_rust_source_without_non_dominating_blocks_for_contract(&compact);
    let filtered = String::from_utf8(filtered).expect("compact source should stay UTF-8");

    assert!(filtered.contains("live_before"));
    assert!(filtered.contains("live_after"));
    for hidden in [
        "hidden_if_false",
        "hidden_cfg_any",
        "hidden_match_false",
        "hidden_async",
        "hidden_async_move",
        "hidden_macro_body",
        "hidden_plain_closure",
        "hidden_typed_closure",
        "hidden_move_closure",
        "hidden_local_fn_body",
    ] {
        assert!(
            !filtered.contains(hidden),
            "non-dominating compact view must erase {hidden}"
        );
    }
}

#[test]
fn source_contract_compact_helpers_062_report_lines_after_non_ascii_text() {
    let probe = r#"
            // non-ascii comments like café and 茶 must not affect byte indexing.
            const TEXT: &str = "emoji 😀 stays in ignored string text";
            fn target() {}

            fn target() {}
        "#;

    assert_eq!(
        compact_pattern_line_numbers(probe, "fntarget(){}"),
        vec![4, 6],
        "line-number helper must map compact-byte offsets back to source lines"
    );
    let second_target = probe.rfind("fn target").expect("second target probe");
    assert_eq!(source_line_number(probe, second_target), 6);

    let compact = b"outer({inner[0]})tail";
    let close =
        compact_delimiter_close(compact, "outer".len()).expect("outer call delimiter should close");
    assert_eq!(&compact[..=close], b"outer({inner[0]})");
}

#[test]
fn source_contract_production_view_062_erases_conjunctive_test_only_cfg_attrs() {
    let probe = r#"
            #[cfg(any(test, feature = "root_proof_probe"))]
            #[cfg(not(feature = "root_proof_probe"))]
            fn test_only_conjunction() {
                let _ = vm.apply_runtime_transition(None, transition);
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(
        !production.contains("vm.apply_runtime_transition"),
        "multiple cfg attrs whose conjunction is impossible without test must be erased"
    );
}

#[test]
fn source_contract_production_view_062_erases_conditional_cfg_attr_test_only_items() {
    let probe = r#"
            #[cfg(any(test, feature = "root_proof_probe"))]
            #[cfg_attr(feature = "root_proof_probe", cfg(not(feature = "root_proof_probe")))]
            fn test_only_conditional_cfg_attr() {
                let _ = vm.apply_runtime_transition(None, transition);
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(
        !production.contains("vm.apply_runtime_transition"),
        "cfg_attr predicates must participate in impossible-without-test cfg conjunctions"
    );
}

#[test]
fn source_contract_production_view_062_erases_macro_rules_bodies() {
    let probe = r#"
            macro_rules! test_only_contract_probe {
                ($attr:meta) => {
                    #[$attr]
                    mod tests {
                        fn test_only() {
                            let _ = vm.apply_runtime_transition(None, transition);
                        }
                    }
                };
            }

            fn production_after_macro() {
                vm.run_scheduled();
            }
        "#;

    let production = production_source_without_test_modules(probe);

    assert!(production.contains("production_after_macro"));
    assert!(
        !production.contains("vm.apply_runtime_transition"),
        "macro_rules bodies are templates, not production call sites for source-contract scans"
    );
}

#[test]
fn source_contract_macro_preserving_view_062_erases_cfg_test_items_only() {
    let probe = r#"
            #[cfg(test)]
            macro_rules! refetch_after_frame_change {
                () => {{
                    self.mark_gc_fiber_roots_dirty(fiber_id);
                    refetch!();
                }}
            }

            macro_rules! refetch_after_frame_change {
                () => {{
                    self.mark_gc_fiber_roots_dirty(fiber_id);
                    refetch!();
                }}
            }
        "#;

    let production = production_source_without_test_items_preserving_macro_rules_062(probe);

    assert_eq!(
        production
            .matches("macro_rules! refetch_after_frame_change")
            .count(),
        1,
        "cfg-test macro owners must be erased without erasing production macro owners"
    );
    assert!(
            production.contains("mark_gc_fiber_roots_dirty")
                && production.contains("refetch!()"),
            "macro-preserving source view must keep production macro bodies for macro-template contracts"
        );
}

fn write_probe_crate_062(name: &str, source: &str) -> std::path::PathBuf {
    let root =
        std::env::temp_dir().join(format!("vo-source-contract-{name}-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&root);
    std::fs::create_dir_all(root.join("src")).expect("create source-contract probe src dir");
    std::fs::write(root.join("src/lib.rs"), source).expect("write source-contract probe");
    root
}

#[test]
fn source_contract_production_tree_062_excludes_out_of_line_cfg_test_modules() {
    let root = write_probe_crate_062(
        "out-of-line-test-module",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                mod tests;
            "#,
    );
    std::fs::write(
        root.join("src/tests.rs"),
        r#"
                fn test_only_source() {
                    let _ = JitContext::OFFSET_TEST_ONLY_SENTINEL;
                }
            "#,
    )
    .expect("write out-of-line test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_SENTINEL"),
        "out-of-line cfg(test) module files must be excluded from production source-set scans"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_file_module_cfg_test_children() {
    let root = write_probe_crate_062(
        "file-module-test-child",
        r#"
                mod foo;
            "#,
    );
    std::fs::write(
        root.join("src/foo.rs"),
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                mod tests;
            "#,
    )
    .expect("write file module");
    std::fs::create_dir_all(root.join("src/foo")).expect("create file module child dir");
    std::fs::write(
        root.join("src/foo/tests.rs"),
        r#"
                fn test_only_source() {
                    let _ = JitContext::OFFSET_TEST_ONLY_FILE_MODULE_SENTINEL;
                }
            "#,
    )
    .expect("write file module out-of-line test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_FILE_MODULE_SENTINEL"),
        "file-module cfg(test) children must be excluded from production source-set scans"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_path_attr_cfg_test_modules() {
    let root = write_probe_crate_062(
        "path-attr-test-module",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                #[path = "fixtures/root_proof.rs"]
                mod tests;
            "#,
    );
    std::fs::create_dir_all(root.join("src/fixtures")).expect("create path attr fixture dir");
    std::fs::write(
        root.join("src/fixtures/root_proof.rs"),
        r#"
                fn test_only_source() {
                    let _ = JitContext::OFFSET_TEST_ONLY_PATH_ATTR_SENTINEL;
                }
            "#,
    )
    .expect("write path attr test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_PATH_ATTR_SENTINEL"),
        "path-attributed cfg(test) modules must be excluded from production source-set scans"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_path_attr_before_cfg_test_modules() {
    let root = write_probe_crate_062(
        "path-before-cfg-test-module",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[path = "fixtures/root_proof.rs"]
                #[cfg(test)]
                mod tests;
            "#,
    );
    std::fs::create_dir_all(root.join("src/fixtures")).expect("create path attr fixture dir");
    std::fs::write(
        root.join("src/fixtures/root_proof.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_PATH_BEFORE_CFG; }",
    )
    .expect("write path-before-cfg test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_PATH_BEFORE_CFG"),
        "path attrs before cfg(test) must be honored by production source-set scans"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_cfg_attr_path_attr_cfg_test_modules() {
    let root = write_probe_crate_062(
        "cfg-attr-path-test-module",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                #[cfg_attr(test, path = "fixtures/root_proof.rs")]
                mod tests;
            "#,
    );
    std::fs::create_dir_all(root.join("src/fixtures")).expect("create cfg_attr fixture dir");
    std::fs::write(
        root.join("src/fixtures/root_proof.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_CFG_ATTR_PATH; }",
    )
    .expect("write cfg_attr path test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_CFG_ATTR_PATH"),
        "cfg_attr(test, path = ...) modules must be excluded from production source-set scans"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_cfg_attr_cfg_test_modules() {
    let root = write_probe_crate_062(
        "cfg-attr-cfg-test-module",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg_attr(all(), cfg(test))]
                mod tests;
            "#,
    );
    std::fs::write(
        root.join("src/tests.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_CFG_ATTR_CFG; }",
    )
    .expect("write cfg_attr cfg(test) module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
            !production.contains("OFFSET_TEST_ONLY_CFG_ATTR_CFG"),
            "cfg_attr-generated cfg(test) out-of-line modules must be excluded from production source-set scans"
        );
}

#[test]
fn source_contract_production_tree_062_excludes_conjunctive_test_only_out_of_line_modules() {
    let root = write_probe_crate_062(
        "conjunctive-test-only-module",
        r#"
                #[cfg(any(test, feature = "root_proof_probe"))]
                #[cfg(not(feature = "root_proof_probe"))]
                mod tests;
            "#,
    );
    std::fs::write(
        root.join("src/tests.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_CONJUNCTIVE_CFG; }",
    )
    .expect("write conjunctive cfg test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        !production.contains("OFFSET_TEST_ONLY_CONJUNCTIVE_CFG"),
        "out-of-line modules with cfg conjunctions impossible without test must be excluded"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_conditional_cfg_attr_test_only_modules() {
    let root = write_probe_crate_062(
        "conditional-cfg-attr-test-only-module",
        r#"
                #[cfg(any(test, feature = "root_proof_probe"))]
                #[cfg_attr(feature = "root_proof_probe", cfg(not(feature = "root_proof_probe")))]
                mod tests;
            "#,
    );
    std::fs::write(
        root.join("src/tests.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_CONDITIONAL_CFG_ATTR; }",
    )
    .expect("write conditional cfg_attr module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        !production.contains("OFFSET_TEST_ONLY_CONDITIONAL_CFG_ATTR"),
        "out-of-line modules impossible without test through cfg_attr predicates must be excluded"
    );
}

#[test]
fn source_contract_production_tree_062_ignores_macro_rules_cfg_test_modules() {
    let root = write_probe_crate_062(
        "macro-rules-fake-test-module",
        r#"
                macro_rules! fake_test_module {
                    () => {
                        #[cfg(test)]
                        mod gc_proof;
                    };
                }

                mod gc_proof;
            "#,
    );
    std::fs::write(
        root.join("src/gc_proof.rs"),
        "fn production_source() { let _ = JitContext::OFFSET_PRODUCTION_GC_PROOF; }",
    )
    .expect("write production module shadowed by macro template");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
            production.contains("OFFSET_PRODUCTION_GC_PROOF"),
            "macro_rules templates must not declare cfg(test) modules for production source-tree filtering"
        );
}

#[test]
fn source_contract_production_tree_062_includes_production_include_files() {
    let root = write_probe_crate_062(
        "production-include-file",
        r##"
                include!("../generated_contract.inc");
                include!(r#"../raw_generated_contract.inc"#);
                include! { "../brace_generated_contract.inc" }
                include! [ r#"../bracket_generated_contract.inc"# ]
                include! /* production include comment */ (
                    /* production include body comment */ "../comment_generated_contract.inc"
                );
                include!(concat!("../concat_", "generated_contract.inc"));
                include!(concat /* concat macro comment */ ! (
                    /* concat arg comment */ "../concat_comment_",
                    "generated_contract.inc"
                ));
                include!(concat! [ "../concat_bracket_", "generated_contract.inc" ]);
                include!(concat!(env!("CARGO_MANIFEST_DIR"), "/env_generated_contract.inc"));
                include!(concat!(env!("CARGO_MANIFEST_DIR", "manifest dir required"), "/env_msg_generated_contract.inc"));
                include!(concat!(env!("CARGO_MANIFEST_DIR"), '/', "env_char_generated_contract.inc"));
                use std::include as rust_include;
                rust_include!("../aliased_generated_contract.inc");
                macro_rules! include {
                    ($path:literal) => {};
                }
                std::include!("../qualified_generated_contract.inc");
            "##,
    );
    std::fs::write(
        root.join("generated_contract.inc"),
        "fn production_include_source() { let _ = JitContext::OFFSET_PRODUCTION_INCLUDE_FILE; }",
    )
    .expect("write production include file");
    std::fs::write(
            root.join("raw_generated_contract.inc"),
            "fn raw_production_include_source() { let _ = JitContext::OFFSET_RAW_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write raw-string production include file");
    std::fs::write(
            root.join("brace_generated_contract.inc"),
            "fn brace_production_include_source() { let _ = JitContext::OFFSET_BRACE_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write brace-delimited production include file");
    std::fs::write(
            root.join("bracket_generated_contract.inc"),
            "fn bracket_production_include_source() { let _ = JitContext::OFFSET_BRACKET_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write bracket-delimited production include file");
    std::fs::write(
            root.join("comment_generated_contract.inc"),
            "fn comment_production_include_source() { let _ = JitContext::OFFSET_COMMENT_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write comment-wrapped production include file");
    std::fs::write(
            root.join("concat_generated_contract.inc"),
            "fn concat_production_include_source() { let _ = JitContext::OFFSET_CONCAT_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write concat production include file");
    std::fs::write(
            root.join("concat_comment_generated_contract.inc"),
            "fn concat_comment_production_include_source() { let _ = JitContext::OFFSET_CONCAT_COMMENT_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write concat-with-comment production include file");
    std::fs::write(
            root.join("concat_bracket_generated_contract.inc"),
            "fn concat_bracket_production_include_source() { let _ = JitContext::OFFSET_CONCAT_BRACKET_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write bracket concat production include file");
    std::fs::write(
            root.join("env_generated_contract.inc"),
            "fn env_production_include_source() { let _ = JitContext::OFFSET_ENV_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write env concat production include file");
    std::fs::write(
            root.join("env_msg_generated_contract.inc"),
            "fn env_msg_production_include_source() { let _ = JitContext::OFFSET_ENV_MSG_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write env concat with message production include file");
    std::fs::write(
            root.join("env_char_generated_contract.inc"),
            "fn env_char_production_include_source() { let _ = JitContext::OFFSET_ENV_CHAR_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write env concat with char separator production include file");
    std::fs::write(
            root.join("aliased_generated_contract.inc"),
            "fn aliased_production_include_source() { let _ = JitContext::OFFSET_ALIASED_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write aliased production include file");
    std::fs::write(
            root.join("qualified_generated_contract.inc"),
            "fn qualified_production_include_source() { let _ = JitContext::OFFSET_QUALIFIED_PRODUCTION_INCLUDE_FILE; }",
        )
        .expect("write qualified production include file");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        production.contains("OFFSET_PRODUCTION_INCLUDE_FILE"),
        "production include! files must be included in source-set scans"
    );
    assert!(
        production.contains("OFFSET_RAW_PRODUCTION_INCLUDE_FILE"),
        "raw-string production include! files must be included in source-set scans"
    );
    assert!(
        production.contains("OFFSET_BRACE_PRODUCTION_INCLUDE_FILE"),
        "brace-delimited production include! files must be included in source-set scans"
    );
    assert!(
        production.contains("OFFSET_BRACKET_PRODUCTION_INCLUDE_FILE"),
        "bracket-delimited production include! files must be included in source-set scans"
    );
    assert!(
        production.contains("OFFSET_COMMENT_PRODUCTION_INCLUDE_FILE"),
        "comment-wrapped production include! files must be included in source-set scans"
    );
    assert!(
        production.contains("OFFSET_CONCAT_PRODUCTION_INCLUDE_FILE"),
        "literal concat! production include! files must be included in source-set scans"
    );
    assert!(
        production.contains("OFFSET_CONCAT_COMMENT_PRODUCTION_INCLUDE_FILE"),
        "comment-wrapped concat! production include! files must be included in source-set scans"
    );
    assert!(
        production.contains("OFFSET_CONCAT_BRACKET_PRODUCTION_INCLUDE_FILE"),
        "bracket-delimited concat! production include! files must be included in source-set scans"
    );
    assert!(
            production.contains("OFFSET_ENV_PRODUCTION_INCLUDE_FILE"),
            "env!(CARGO_MANIFEST_DIR) concat! production include! files must be included in source-set scans"
        );
    assert!(
            production.contains("OFFSET_ENV_MSG_PRODUCTION_INCLUDE_FILE"),
            "env!(CARGO_MANIFEST_DIR, message) concat! production include! files must be included in source-set scans"
        );
    assert!(
        production.contains("OFFSET_ENV_CHAR_PRODUCTION_INCLUDE_FILE"),
        "char literal separators inside concat! production include! paths must be decoded"
    );
    assert!(
        production.contains("OFFSET_ALIASED_PRODUCTION_INCLUDE_FILE"),
        "std::include aliases must be included in source-set scans"
    );
    assert!(
            production.contains("OFFSET_QUALIFIED_PRODUCTION_INCLUDE_FILE"),
            "path-qualified std::include! must remain builtin even when bare include is locally shadowed"
        );
}

#[test]
fn source_contract_production_tree_062_tracks_std_include_alias_names() {
    let names = super::production_include_macro_names_062(
        r#"
                use std::include as rust_include;
                use core::{include as core_include};
            "#,
    );

    assert!(names.contains("include"));
    assert!(names.contains("rust_include"));
    assert!(names.contains("core_include"));
}

#[test]
fn source_contract_production_tree_062_scans_std_include_alias_invocations() {
    let root = write_probe_crate_062(
        "include-alias-invocation",
        r#"
                use std::include as rust_include;
                rust_include!("../aliased.inc");
            "#,
    );
    std::fs::write(root.join("aliased.inc"), "fn aliased() {}")
        .expect("write aliased include file");
    let source = std::fs::read_to_string(root.join("src/lib.rs")).expect("read probe source");

    let includes = super::production_include_files(&source, &root.join("src"));

    assert!(
        includes.iter().any(|path| path.ends_with("aliased.inc")),
        "std::include aliases must resolve include files"
    );
}

#[test]
fn source_contract_production_tree_062_scans_raw_identifier_include_aliases() {
    let root = write_probe_crate_062(
        "raw-include-alias-invocation",
        r#"
                use r#std::r#include as r#jit_include;
                r#jit_include!("../raw_aliased.inc");
            "#,
    );
    std::fs::write(root.join("raw_aliased.inc"), "fn raw_aliased() {}")
        .expect("write raw aliased include file");
    let source = std::fs::read_to_string(root.join("src/lib.rs")).expect("read probe source");

    let includes = super::production_include_files(&source, &root.join("src"));

    assert!(
        includes
            .iter()
            .any(|path| path.ends_with("raw_aliased.inc")),
        "raw identifier std::include aliases must resolve include files"
    );
}

#[test]
fn source_contract_production_tree_062_scans_known_include_alias_invocations() {
    let root = write_probe_crate_062("known-include-alias-invocation", r#"fn placeholder() {}"#);
    std::fs::write(root.join("aliased.inc"), "fn aliased() {}")
        .expect("write aliased include file");
    let include_names = std::collections::BTreeSet::from(["rust_include".to_string()]);

    let includes = super::production_include_files_with_macro_scope_062(
        r#"rust_include!("../aliased.inc");"#,
        &root.join("src"),
        &[],
        &include_names,
    );

    assert!(
        includes.iter().any(|path| path.ends_with("aliased.inc")),
        "known include aliases must resolve include files"
    );
}

#[test]
fn source_contract_production_tree_062_scans_raw_path_qualified_include_under_shadow() {
    let root = write_probe_crate_062(
        "raw-qualified-include-shadow",
        r#"
                macro_rules! include {
                    ($path:literal) => {};
                }

                std::r#include!("../raw_qualified.inc");
            "#,
    );
    std::fs::write(root.join("raw_qualified.inc"), "fn raw_qualified() {}")
        .expect("write raw qualified include file");
    let source = std::fs::read_to_string(root.join("src/lib.rs")).expect("read probe source");

    let includes = super::production_include_files(&source, &root.join("src"));

    assert!(
        includes
            .iter()
            .any(|path| path.ends_with("raw_qualified.inc")),
        "raw path-qualified std::include! must remain builtin under local shadowing"
    );
}

#[test]
fn source_contract_production_tree_062_ignores_include_tokens_inside_byte_raw_strings() {
    let root = write_probe_crate_062(
        "byte-raw-string-include-shadow",
        r##"
                const BYTES: &[u8] = br#"";
                    include!("../phantom.inc");
                    macro_rules! include {
                        ($path:literal) => {};
                    }
                "#;

                include!("../real.inc");
            "##,
    );
    std::fs::write(root.join("phantom.inc"), "fn phantom() {}")
        .expect("write phantom include file");
    std::fs::write(root.join("real.inc"), "fn real() {}").expect("write real include file");
    let source = std::fs::read_to_string(root.join("src/lib.rs")).expect("read probe source");

    let includes = super::production_include_files(&source, &root.join("src"));

    assert!(
        includes.iter().any(|path| path.ends_with("real.inc")),
        "byte raw string contents must not shadow real production include! calls"
    );
    assert!(
        !includes.iter().any(|path| path.ends_with("phantom.inc")),
        "include! tokens inside byte raw strings must not enter the production include graph"
    );
}

#[test]
fn source_contract_production_tree_062_ignores_shadowed_include_macro_files() {
    let root = write_probe_crate_062(
        "shadowed-include-file",
        r#"
                macro_rules! include {
                    ($path:literal) => {};
                }

                include!("../shadowed_root_proof.inc");
            "#,
    );
    std::fs::write(
        root.join("shadowed_root_proof.inc"),
        "fn shadowed_include_source() { let _ = JitContext::OFFSET_SHADOWED_INCLUDE_FILE; }",
    )
    .expect("write shadowed include file");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        !production.contains("OFFSET_SHADOWED_INCLUDE_FILE"),
        "source-set scans must not include files referenced by locally shadowed include! macros"
    );

    let export_root = write_probe_crate_062(
        "macro-export-shadowed-include-file",
        r#"
                mod exported_shadow {
                    #[macro_export]
                    macro_rules! include {
                        ($path:literal) => {};
                    }
                }

                include!("../macro_export_shadowed_root_proof.inc");
            "#,
    );
    std::fs::write(
            export_root.join("macro_export_shadowed_root_proof.inc"),
            "fn macro_export_shadowed_include_source() { let _ = JitContext::OFFSET_MACRO_EXPORT_SHADOWED_INCLUDE_FILE; }",
        )
        .expect("write macro_export shadowed include file");

    let export_production =
        super::production_sources_without_test_modules(&export_root.join("src"))
            .into_iter()
            .map(|(_path, source)| source)
            .collect::<Vec<_>>()
            .join("\n");

    assert!(
            !export_production.contains("OFFSET_MACRO_EXPORT_SHADOWED_INCLUDE_FILE"),
            "source-set scans must not include files referenced by #[macro_export] shadowed include! macros"
        );

    let export_after_root = write_probe_crate_062(
        "macro-export-after-shadowed-include-file",
        r#"
                include!("../macro_export_after_shadowed_root_proof.inc");

                mod exported_shadow {
                    #[macro_export]
                    macro_rules! include {
                        ($path:literal) => {};
                    }
                }
            "#,
    );
    std::fs::write(
            export_after_root.join("macro_export_after_shadowed_root_proof.inc"),
            "fn macro_export_after_shadowed_include_source() { let _ = JitContext::OFFSET_MACRO_EXPORT_AFTER_SHADOWED_INCLUDE_FILE; }",
        )
        .expect("write post macro_export shadowed include file");

    let export_after_production =
        super::production_sources_without_test_modules(&export_after_root.join("src"))
            .into_iter()
            .map(|(_path, source)| source)
            .collect::<Vec<_>>()
            .join("\n");

    assert!(
            !export_after_production.contains("OFFSET_MACRO_EXPORT_AFTER_SHADOWED_INCLUDE_FILE"),
            "source-set scans must not include files referenced before a later #[macro_export] shadowed include! macro"
        );

    let macro_use_root = write_probe_crate_062(
        "macro-use-shadowed-include-file",
        r#"
                #[macro_use]
                mod imported_shadow {
                    macro_rules! include {
                        ($path:literal) => {};
                    }
                }

                include!("../macro_use_shadowed_root_proof.inc");
            "#,
    );
    std::fs::write(
            macro_use_root.join("macro_use_shadowed_root_proof.inc"),
            "fn macro_use_shadowed_include_source() { let _ = JitContext::OFFSET_MACRO_USE_SHADOWED_INCLUDE_FILE; }",
        )
        .expect("write macro_use shadowed include file");

    let macro_use_production =
        super::production_sources_without_test_modules(&macro_use_root.join("src"))
            .into_iter()
            .map(|(_path, source)| source)
            .collect::<Vec<_>>()
            .join("\n");

    assert!(
            !macro_use_production.contains("OFFSET_MACRO_USE_SHADOWED_INCLUDE_FILE"),
            "source-set scans must not include files referenced by #[macro_use] imported shadowed include! macros"
        );
}

#[test]
fn source_contract_production_tree_062_excludes_cfg_test_include_files() {
    let root = write_probe_crate_062(
        "cfg-test-include-file",
        r#"
                #[cfg(test)]
                mod tests {
                    include!("../test_only_module.inc");
                }

                #[cfg(test)]
                fn test_only_helper() {
                    include!("../test_only_fn.inc");
                }
            "#,
    );
    std::fs::write(
        root.join("test_only_module.inc"),
        "fn test_only_module_include() { let _ = JitContext::OFFSET_TEST_ONLY_MODULE_INCLUDE; }",
    )
    .expect("write cfg-test module include file");
    std::fs::write(
        root.join("test_only_fn.inc"),
        "fn test_only_fn_include() { let _ = JitContext::OFFSET_TEST_ONLY_FN_INCLUDE; }",
    )
    .expect("write cfg-test function include file");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        !production.contains("OFFSET_TEST_ONLY_MODULE_INCLUDE"),
        "cfg-test module include! files must not be treated as production source"
    );
    assert!(
        !production.contains("OFFSET_TEST_ONLY_FN_INCLUDE"),
        "cfg-test function include! files must not be treated as production source"
    );
}

#[test]
fn source_contract_production_tree_062_includes_production_path_attr_modules_outside_src() {
    let root = write_probe_crate_062(
        "production-path-outside-src",
        r#"
                #[path = "../outside.rs"]
                mod outside;
            "#,
    );
    std::fs::write(
        root.join("outside.rs"),
        "fn production_outside_source() { let _ = JitContext::OFFSET_PRODUCTION_OUTSIDE_SRC; }",
    )
    .expect("write production path module outside src");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        production.contains("OFFSET_PRODUCTION_OUTSIDE_SRC"),
        "production #[path] modules outside src must be included in source-set scans"
    );
}

#[test]
fn source_contract_production_tree_062_preserves_production_path_attr_shared_with_cfg_test_alias() {
    let root = write_probe_crate_062(
        "production-path-shared-with-test-alias",
        r#"
                #[path = "../shared.rs"]
                mod production_shared;

                #[cfg(test)]
                #[path = "../shared.rs"]
                mod test_alias;
            "#,
    );
    std::fs::write(
        root.join("shared.rs"),
        "fn production_shared_source() { let _ = JitContext::OFFSET_PRODUCTION_SHARED_ROOT; }",
    )
    .expect("write shared production/test path module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        production.contains("OFFSET_PRODUCTION_SHARED_ROOT"),
        "cfg(test) path aliases must not exclude files that are also production-reachable"
    );
}

#[test]
fn source_contract_production_tree_062_preserves_in_src_production_module_shared_with_cfg_test_alias(
) {
    let root = write_probe_crate_062(
        "production-default-shared-with-test-alias",
        r#"
                mod production_shared;

                #[cfg(test)]
                #[path = "production_shared.rs"]
                mod test_alias;
            "#,
    );
    std::fs::write(
        root.join("src/production_shared.rs"),
        "fn production_shared_source() { let _ = JitContext::OFFSET_PRODUCTION_IN_SRC_ROOT; }",
    )
    .expect("write shared in-src production/test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
            production.contains("OFFSET_PRODUCTION_IN_SRC_ROOT"),
            "cfg(test) aliases must not exclude default-path modules that are also production-reachable"
        );
}

#[test]
fn source_contract_production_tree_062_preserves_in_src_path_module_shared_with_cfg_test_alias() {
    let root = write_probe_crate_062(
        "production-in-src-path-shared-with-test-alias",
        r#"
                #[path = "fixtures/root.rs"]
                mod production_shared;

                #[cfg(test)]
                #[path = "fixtures/root.rs"]
                mod test_alias;
            "#,
    );
    std::fs::create_dir_all(root.join("src/fixtures")).expect("create shared fixture dir");
    std::fs::write(
        root.join("src/fixtures/root.rs"),
        "fn production_shared_source() { let _ = JitContext::OFFSET_PRODUCTION_IN_SRC_PATH_ROOT; }",
    )
    .expect("write shared in-src path module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
            production.contains("OFFSET_PRODUCTION_IN_SRC_PATH_ROOT"),
            "cfg(test) path aliases must not exclude in-src path modules that are also production-reachable"
        );
}

#[test]
fn source_contract_production_tree_062_excludes_in_src_path_modules_reachable_only_from_cfg_test_file(
) {
    let root = write_probe_crate_062(
        "in-src-path-module-only-from-test-file",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                mod tests;
            "#,
    );
    std::fs::write(
        root.join("src/tests.rs"),
        r#"
                #[path = "helper.rs"]
                mod helper;
            "#,
    )
    .expect("write cfg-test module with in-src path helper");
    std::fs::write(
        root.join("src/helper.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_IN_SRC_PATH; }",
    )
    .expect("write in-src test-only path helper");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_IN_SRC_PATH"),
        "in-src path modules reachable only through cfg(test) files must be excluded"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_in_src_path_modules_reachable_only_from_inline_cfg_test(
) {
    let root = write_probe_crate_062(
        "in-src-path-module-only-from-inline-test",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                mod tests {
                    #[path = "../helper.rs"]
                    mod helper;
                }
            "#,
    );
    std::fs::write(
        root.join("src/helper.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_INLINE_IN_SRC_PATH; }",
    )
    .expect("write in-src inline test-only path helper");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_INLINE_IN_SRC_PATH"),
        "in-src path modules reachable only through inline cfg(test) modules must be excluded"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_nested_inline_cfg_test_modules() {
    let root = write_probe_crate_062(
        "nested-inline-test-module",
        r#"
                mod outer {
                    #[cfg(test)]
                    mod tests;
                }
            "#,
    );
    std::fs::create_dir_all(root.join("src/outer")).expect("create nested inline module dir");
    std::fs::write(
        root.join("src/outer/tests.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_NESTED_INLINE; }",
    )
    .expect("write nested inline test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        !production.contains("OFFSET_TEST_ONLY_NESTED_INLINE"),
        "cfg(test) modules inside inline module namespaces must exclude their nested source paths"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_nested_inline_path_attr_cfg_test_modules() {
    let root = write_probe_crate_062(
        "nested-inline-path-attr-test-module",
        r#"
                mod outer {
                    #[cfg(test)]
                    #[path = "fixtures/root_proof.rs"]
                    mod tests;
                }
            "#,
    );
    std::fs::create_dir_all(root.join("src/outer/fixtures"))
        .expect("create nested inline path attr dir");
    std::fs::write(
        root.join("src/outer/fixtures/root_proof.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_NESTED_PATH_ATTR; }",
    )
    .expect("write nested inline path attr test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(
        !production.contains("OFFSET_TEST_ONLY_NESTED_PATH_ATTR"),
        "path attrs inside inline module namespaces must resolve from the inline namespace"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_normalized_path_attr_cfg_test_modules() {
    let root = write_probe_crate_062(
        "normalized-path-attr-test-module",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                #[path = "./fixtures/root_proof.rs"]
                mod tests;
            "#,
    );
    std::fs::create_dir_all(root.join("src/fixtures")).expect("create path attr fixture dir");
    std::fs::write(
        root.join("src/fixtures/root_proof.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_NORMALIZED_PATH; }",
    )
    .expect("write normalized path attr test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_NORMALIZED_PATH"),
        "normalized path attrs must be honored by production source-set scans"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_raw_string_path_attr_cfg_test_modules() {
    let root = write_probe_crate_062(
        "raw-string-path-attr-test-module",
        r##"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                #[path = r#"fixtures/root_proof.rs"#]
                mod tests;
            "##,
    );
    std::fs::create_dir_all(root.join("src/fixtures")).expect("create path attr fixture dir");
    std::fs::write(
        root.join("src/fixtures/root_proof.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_RAW_STRING_PATH; }",
    )
    .expect("write raw string path attr test module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_RAW_STRING_PATH"),
        "raw string path attrs must be honored by production source-set scans"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_path_attr_cfg_test_children() {
    let root = write_probe_crate_062(
        "path-attr-test-children",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                #[path = "fixtures/tests/mod.rs"]
                mod tests;
            "#,
    );
    std::fs::create_dir_all(root.join("src/fixtures/tests")).expect("create path attr child dir");
    std::fs::write(root.join("src/fixtures/tests/mod.rs"), "mod gc_proof;")
        .expect("write path attr module root");
    std::fs::write(
        root.join("src/fixtures/tests/gc_proof.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_PATH_CHILD; }",
    )
    .expect("write path attr test child module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_PATH_CHILD"),
        "children of path-attributed cfg(test) modules must be excluded"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_path_modules_reachable_only_from_cfg_test_file() {
    let root = write_probe_crate_062(
        "path-module-only-from-test-file",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                mod tests;
            "#,
    );
    std::fs::write(
        root.join("src/tests.rs"),
        r#"
                #[path = "../outside_test_only.rs"]
                mod helper;
            "#,
    )
    .expect("write cfg-test module with path helper");
    std::fs::write(
        root.join("outside_test_only.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_OUTSIDE_PATH; }",
    )
    .expect("write outside test-only helper");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_OUTSIDE_PATH"),
        "outside path modules reachable only through cfg(test) files must be excluded"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_path_modules_reachable_only_from_inline_cfg_test() {
    let root = write_probe_crate_062(
        "path-module-only-from-inline-test",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                mod tests {
                    #[path = "../outside_inline_test_only.rs"]
                    mod helper;
                }
            "#,
    );
    std::fs::write(
        root.join("outside_inline_test_only.rs"),
        "fn test_only_source() { let _ = JitContext::OFFSET_TEST_ONLY_INLINE_OUTSIDE_PATH; }",
    )
    .expect("write outside inline test-only helper");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_INLINE_OUTSIDE_PATH"),
        "outside path modules reachable only through inline cfg(test) modules must be excluded"
    );
}

#[test]
fn source_contract_production_tree_062_excludes_inline_cfg_test_child_modules() {
    let root = write_probe_crate_062(
        "inline-test-child",
        r#"
                fn production_source() {
                    vm.run_scheduled();
                }

                #[cfg(test)]
                mod tests {
                    mod gc_proof;
                }
            "#,
    );
    std::fs::create_dir_all(root.join("src/tests")).expect("create inline test child dir");
    std::fs::write(
        root.join("src/tests/gc_proof.rs"),
        r#"
                fn test_only_source() {
                    let _ = JitContext::OFFSET_TEST_ONLY_INLINE_CHILD_SENTINEL;
                }
            "#,
    )
    .expect("write inline test child module");

    let production = super::production_sources_without_test_modules(&root.join("src"))
        .into_iter()
        .map(|(_path, source)| source)
        .collect::<Vec<_>>()
        .join("\n");

    assert!(production.contains("production_source"));
    assert!(
        !production.contains("OFFSET_TEST_ONLY_INLINE_CHILD_SENTINEL"),
        "children of inline cfg(test) modules must be excluded from production source-set scans"
    );
}

fn assert_guard_rejects_062(name: &str, source: &str, reason: &str) {
    let root = write_probe_crate_062(name, source);
    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err(reason);
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("src/lib.rs"), "{msg}");
}

#[test]
fn source_contract_guard_062_rejects_spaced_cfg_test_split_marker() {
    let root = write_probe_crate_062(
        "spaced-split",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split( "#[cfg(test)]" ).next();
                }
            "##,
    );

    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err("spaced textual cfg(test) split must be rejected");
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("src/lib.rs"), "{msg}");
}

#[test]
fn source_contract_guard_062_rejects_cfg_test_marker_constant_split() {
    let root = write_probe_crate_062(
        "const-split",
        r##"
                const TEST_MARKER: &str = "#[cfg(test)]";

                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split(TEST_MARKER).next();
                }
            "##,
    );

    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err("constant textual cfg(test) split must be rejected");
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("src/lib.rs"), "{msg}");
}

#[test]
fn source_contract_guard_062_rejects_contextual_cfg_test_marker_constant_split() {
    let root = write_probe_crate_062(
        "contextual-const-split",
        r##"
                const TEST_MARKER: &str = "\n#[cfg(test)]\nmod tests";

                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split(TEST_MARKER).next();
                }
            "##,
    );

    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err("contextual constant textual cfg(test) split must be rejected");
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("src/lib.rs"), "{msg}");
}

#[test]
fn source_contract_guard_062_rejects_cfg_test_marker_split_once() {
    let root = write_probe_crate_062(
        "split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split_once("#[cfg(test)]");
                }
            "##,
    );

    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err("split_once textual cfg(test) split must be rejected");
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("src/lib.rs"), "{msg}");
}

#[test]
fn source_contract_guard_062_rejects_cfg_test_marker_let_binding_split_once() {
    let root = write_probe_crate_062(
        "let-binding-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = "#[cfg(test)]";
                    let _ = include_str!("probe.rs").split_once(marker);
                }
            "##,
    );

    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err("let-bound textual cfg(test) split_once marker must be rejected");
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("src/lib.rs"), "{msg}");
}

#[test]
fn source_contract_guard_062_rejects_ufcs_cfg_test_marker_split_once() {
    let root = write_probe_crate_062(
        "ufcs-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = str::split_once(include_str!("probe.rs"), "#[cfg(test)]");
                }
            "##,
    );

    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err("UFCS textual cfg(test) split_once marker must be rejected");
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("src/lib.rs"), "{msg}");
}

#[test]
fn source_contract_guard_062_rejects_cfg_test_marker_splitn_second_arg() {
    let root = write_probe_crate_062(
        "splitn-second-arg",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").splitn(2, "#[cfg(test)]").next();
                }
            "##,
    );

    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err("splitn textual cfg(test) second argument marker must be rejected");
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("src/lib.rs"), "{msg}");
}

#[test]
fn source_contract_guard_062_rejects_cfg_test_marker_rsplitn_second_arg() {
    assert_guard_rejects_062(
        "rsplitn-second-arg",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").rsplitn(2, "#[cfg(test)]").next();
                }
            "##,
        "rsplitn textual cfg(test) second argument marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_parenthesized_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "parenthesized-binding-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = "#[cfg(test)]";
                    let _ = include_str!("probe.rs").split_once((marker));
                }
            "##,
        "parenthesized textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_destructured_cfg_test_marker_split_once() {
    assert_guard_rejects_062(
        "destructured-binding-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let (marker,) = ("#[cfg(test)]",);
                    let _ = include_str!("probe.rs").split_once(marker);
                }
            "##,
        "destructured textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_alias_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "alias-binding-split-once",
        r##"
                const TEST_MARKER: &str = "#[cfg(test)]";

                #[test]
                fn bad_source_contract() {
                    let marker = TEST_MARKER;
                    let _ = include_str!("probe.rs").split_once(marker);
                }
            "##,
        "alias textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_module_path_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "module-path-binding-split-once",
        r##"
                mod markers {
                    pub const TEST_MARKER: &str = "#[cfg(test)]";
                }

                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split_once(markers::TEST_MARKER);
                }
            "##,
        "module-path textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_concat_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "concat-binding-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = concat!("#[cfg", "(test)]");
                    let _ = include_str!("probe.rs").split_once(marker);
                }
            "##,
        "concat textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_cfg_test_expression_marker_split() {
    assert_guard_rejects_062(
        "cfg-expression-marker-split",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs")
                        .split("#[cfg(all(test, feature = \"jit\"))]")
                        .next();
                }
            "##,
        "cfg expression textual marker split must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_escaped_cfg_test_marker_split() {
    assert_guard_rejects_062(
        "escaped-marker-split",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split("\x23[cfg(test)]").next();
                }
            "##,
        "escaped textual cfg(test) marker split must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_turbofish_cfg_test_marker_split() {
    assert_guard_rejects_062(
        "turbofish-split",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split::<&str>("#[cfg(test)]").next();
                }
            "##,
        "turbofish textual cfg(test) split marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_ufcs_turbofish_cfg_test_marker_split() {
    assert_guard_rejects_062(
        "ufcs-turbofish-split",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = str::split::<&str>(include_str!("probe.rs"), "#[cfg(test)]").next();
                }
            "##,
        "UFCS turbofish textual cfg(test) split marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_stringify_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "stringify-binding-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = stringify!(#[cfg(test)]);
                    let _ = include_str!("probe.rs").split_once(marker);
                }
            "##,
        "stringify textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_stringify_bracket_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "stringify-bracket-binding-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = stringify![#[cfg(test)]];
                    let _ = include_str!("probe.rs").split_once(marker);
                }
            "##,
        "stringify bracket textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_stringify_brace_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "stringify-brace-binding-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = stringify!{#[cfg(test)]};
                    let _ = include_str!("probe.rs").split_once(marker);
                }
            "##,
        "stringify brace textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_fn_returned_cfg_test_marker_split_once() {
    assert_guard_rejects_062(
        "fn-returned-marker-split-once",
        r##"
                fn marker() -> &'static str {
                    "#[cfg(test)]"
                }

                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split_once(marker());
                }
            "##,
        "function-returned textual cfg(test) marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_raw_identifier_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "raw-ident-binding-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let r#marker = "#[cfg(test)]";
                    let _ = include_str!("probe.rs").split_once(r#marker);
                }
            "##,
        "raw-identifier textual cfg(test) marker binding must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_raw_identifier_fn_returned_cfg_test_marker_split_once() {
    assert_guard_rejects_062(
        "raw-ident-fn-marker-split-once",
        r##"
                fn r#marker() -> &'static str {
                    "#[cfg(test)]"
                }

                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split_once(r#marker());
                }
            "##,
        "raw-identifier function-returned textual cfg(test) marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_marker_binding_method_expr_split_once() {
    assert_guard_rejects_062(
        "method-expr-marker-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = "#[cfg(test)]";
                    let _ = include_str!("probe.rs").split_once(marker.trim());
                }
            "##,
        "method-expression textual cfg(test) marker binding must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_fn_arg_returned_cfg_test_marker_split_once() {
    assert_guard_rejects_062(
        "fn-arg-marker-split-once",
        r##"
                fn marker(_enabled: bool) -> &'static str {
                    "#[cfg(test)]"
                }

                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split_once(marker(true));
                }
            "##,
        "argument-taking function textual cfg(test) marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_block_expr_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "block-expr-marker-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = "#[cfg(test)]";
                    let _ = include_str!("probe.rs").split_once({ marker });
                }
            "##,
        "block-expression textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_if_expr_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "if-expr-marker-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = "#[cfg(test)]";
                    let _ = include_str!("probe.rs")
                        .split_once(if true { marker } else { marker });
                }
            "##,
        "if-expression textual cfg(test) binding marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_format_cfg_test_marker_binding_split_once() {
    assert_guard_rejects_062(
        "format-marker-split-once",
        r##"
                #[test]
                fn bad_source_contract() {
                    let marker = format!("#[cfg({})]", "test");
                    let _ = include_str!("probe.rs").split_once(marker.as_str());
                }
            "##,
        "format-composed textual cfg(test) marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_macro_returned_cfg_test_marker_split_once() {
    assert_guard_rejects_062(
        "macro-marker-split-once",
        r##"
                macro_rules! marker {
                    () => { "#[cfg(test)]" };
                }

                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split_once(marker!());
                }
            "##,
        "macro-returned textual cfg(test) marker must be rejected",
    );
}

#[test]
fn source_contract_guard_062_rejects_cfg_attr_cfg_test_marker_split() {
    assert_guard_rejects_062(
        "cfg-attr-marker-split",
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs")
                        .split("#[cfg_attr(all(), cfg(test))]")
                        .next();
                }
            "##,
        "cfg_attr-generated cfg(test) textual marker split must be rejected",
    );
}

#[test]
fn source_contract_guard_062_scans_production_path_attr_modules_outside_src() {
    let root = write_probe_crate_062(
        "guard-production-path-outside-src",
        r#"
                #[path = "../outside.rs"]
                mod outside;
            "#,
    );
    std::fs::write(
        root.join("outside.rs"),
        r##"
                #[test]
                fn bad_source_contract() {
                    let _ = include_str!("probe.rs").split("#[cfg(test)]").next();
                }
            "##,
    )
    .expect("write production path module outside src");

    let err = std::panic::catch_unwind(|| {
        super::assert_no_textual_cfg_test_splits(root.to_str().unwrap())
    })
    .expect_err("textual guard must scan production path modules outside src");
    let msg = err
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| err.downcast_ref::<&str>().copied())
        .unwrap_or("<non-string panic>");

    assert!(msg.contains("outside.rs"), "{msg}");
}
