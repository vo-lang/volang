pub(crate) fn production_source_without_test_modules(source: &str) -> String {
    vo_source_contract::production_source_without_test_modules(source)
}

pub(crate) fn production_source_without_test_items_preserving_macro_rules(source: &str) -> String {
    vo_source_contract::production_source_without_test_items_preserving_macro_rules(source)
}

#[cfg(test)]
mod tests {
    use super::production_source_without_test_modules;

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
    fn source_contract_tests_do_not_use_textual_cfg_test_truncation_062() {
        vo_source_contract::assert_no_textual_cfg_test_splits(env!("CARGO_MANIFEST_DIR"));
    }
}
