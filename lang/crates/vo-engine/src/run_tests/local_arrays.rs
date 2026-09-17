use super::*;
use vo_runtime::gc::{MemoryError, OomPolicy, VmMemoryConfig};

fn array_vm(mode: &str, memory: VmMemoryConfig) -> Vm {
    if mode == "vm" {
        Vm::with_memory_config(memory)
    } else {
        Vm::try_with_jit_and_memory_config(
            vo_vm::JitConfig {
                call_threshold: if mode == "osr" { 1_000_000 } else { 1 },
                loop_threshold: if mode == "osr" { 1 } else { 1_000_000 },
                optimizing_threshold: if mode == "optimizing" { 1 } else { 16 },
                ..Default::default()
            },
            memory,
        )
        .unwrap()
    }
}

fn assert_array_mode_entered(vm: &Vm, mode: &str) {
    let stats = vm.jit_execution_stats();
    match mode {
        "function" => assert!(stats.function_entries > 0, "{stats:?}"),
        "optimizing" => assert!(stats.optimizing_functions_executed > 0, "{stats:?}"),
        "osr" => assert!(stats.loop_entries > 0, "{stats:?}"),
        _ => assert!(!stats.executed_jit_code()),
    }
}

#[test]
fn independent_array_locals_run_with_managed_allocation_disabled() {
    let compiled = crate::compile_string(
        r#"package main
func values(x int) int {
    a := [4]int{x, x+1, x+2, x+3}
    b := a
    var c = [8]int{3: x}
    b[0] = 100
    return a[x & 3] + b[0] + c[3]
}
func main() {
    sum := 0
    for i := 0; i < 300; i++ { sum += values(i) }
    println(sum)
}"#,
    )
    .unwrap();
    for mode in ["vm", "function", "optimizing", "osr"] {
        let mut vm = array_vm(
            mode,
            VmMemoryConfig {
                allocation_allowed: false,
                oom_policy: OomPolicy::TerminateIsland,
                ..Default::default()
            },
        );
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load_verified(compiled.module.clone()).unwrap();
        let before = vm.memory_stats();
        vm.run().unwrap_or_else(|error| panic!("{mode}: {error:?}"));
        assert_eq!(output.take_bytes(), b"120150\n");
        let after = vm.memory_stats();
        assert_eq!(after.allocation_bytes_total, before.allocation_bytes_total);
        assert_eq!(after.allocation_failures, 0);
        assert_array_mode_entered(&vm, mode);
    }
}

#[test]
fn canonical_array_admission_still_precedes_element_effects_and_is_sticky() {
    let compiled = crate::compile_string(
        r#"package main
func element() int { println("element evaluated"); return 7 }
func main() {
    a := [2]int{element(), 8}
    view := a[:]
    println(view[0])
}"#,
    )
    .unwrap();
    for mode in ["vm", "function"] {
        let mut vm = array_vm(
            mode,
            VmMemoryConfig {
                allocation_allowed: false,
                oom_policy: OomPolicy::TerminateIsland,
                ..Default::default()
            },
        );
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load_verified(compiled.module.clone()).unwrap();
        let error = vm.run().expect_err("canonical array must allocate");
        assert!(
            matches!(
                error,
                VmError::IslandMemory(MemoryError::AllocationForbidden)
            ),
            "{error:?}"
        );
        assert_eq!(output.take_bytes(), b"");
        assert_eq!(vm.memory_stats().allocation_failures, 1);
        assert_eq!(format!("{:?}", vm.run().unwrap_err()), format!("{error:?}"));
        assert_array_mode_entered(&vm, mode);
    }
}

#[test]
fn local_array_representations_preserve_gc_aliasing_and_failure_semantics() {
    let compiled = crate::compile_string(include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../../tests/lang/cases/runtime/core/backend_local_arrays.vo"
    )))
    .unwrap();
    for mode in ["vm", "function", "optimizing", "osr"] {
        let mut vm = array_vm(mode, VmMemoryConfig::default());
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load_verified(compiled.module.clone()).unwrap();
        vm.run().unwrap_or_else(|error| panic!("{mode}: {error:?}"));
        assert_eq!(output.take_bytes(), b"local arrays ok\n");
        assert_array_mode_entered(&vm, mode);
    }
}
