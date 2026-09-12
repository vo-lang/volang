use super::*;
use vo_runtime::gc::{MemoryError, OomPolicy, VmMemoryConfig};

#[test]
fn literal_reuse_preserves_values_and_roots_in_vm_function_jit_and_osr() {
    let source = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../../tests/lang/cases/runtime/core/backend_literal_reuse.vo"
    ));
    let compiled = crate::compile_string(source).unwrap();
    for mode in ["vm", "function", "osr"] {
        let mut vm = if mode == "vm" {
            Vm::new()
        } else {
            Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: if mode == "function" { 1 } else { 1_000_000 },
                loop_threshold: if mode == "osr" { 1 } else { 1_000_000 },
                optimizing_threshold: 16,
                ..Default::default()
            })
            .unwrap()
        };
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load_verified(compiled.module.clone()).unwrap();
        vm.run().unwrap_or_else(|error| panic!("{mode}: {error:?}"));
        assert_eq!(output.take_bytes(), b"literal reuse ok\n");
        assert!(vm.literal_cache_metadata_bytes() > 0);
        let stats = vm.jit_execution_stats();
        match mode {
            "function" => assert!(stats.function_entries > 0, "{stats:?}"),
            "osr" => assert!(stats.loop_entries > 0, "{stats:?}"),
            _ => assert!(!stats.executed_jit_code()),
        }
    }
}

#[test]
fn literal_materialization_failure_stops_vm_and_generated_code_before_later_effects() {
    let compiled = crate::compile_string(
        r#"package main
func value() string { return "allocation check" }
func main() { s := value(); if len(s) != 0 { println("after allocation") } }
"#,
    )
    .unwrap();
    for jit in [false, true] {
        for max_objects in [0, 1] {
            let memory = VmMemoryConfig {
                max_objects: Some(max_objects),
                automatic_gc: false,
                oom_policy: OomPolicy::TerminateIsland,
                ..Default::default()
            };
            let mut vm = if jit {
                Vm::try_with_jit_and_memory_config(
                    vo_vm::JitConfig {
                        call_threshold: 1,
                        loop_threshold: 1_000_000,
                        ..Default::default()
                    },
                    memory,
                )
                .unwrap()
            } else {
                Vm::with_memory_config(memory)
            };
            let output = CaptureSink::new();
            vm.set_output_sink(output.clone());
            vm.load_verified(compiled.module.clone()).unwrap();
            let error = vm
                .run()
                .expect_err("literal construction must respect the object limit");
            assert!(
                matches!(error, VmError::IslandMemory(MemoryError::MetadataExhausted)),
                "{error:?}"
            );
            assert_eq!(output.take_bytes(), b"");
            assert_eq!(vm.memory_stats().allocation_failures, 1);
            assert_eq!(vm.literal_cache_metadata_bytes(), 0);
            assert_eq!(format!("{:?}", vm.run().unwrap_err()), format!("{error:?}"));
            if jit {
                assert!(vm.jit_execution_stats().function_entries > 0);
            }
        }
    }
}
