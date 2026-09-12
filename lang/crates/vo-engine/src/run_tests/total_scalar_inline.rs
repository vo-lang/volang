use super::*;

#[test]
fn scalar_composition_preserves_gc_roots_and_recovery_in_vm_jit_and_osr() {
    let source = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../../tests/lang/cases/runtime/core/backend_scalar_composition.vo"
    ));
    let compiled = crate::compile_string(source).unwrap();
    let module = compiled.module.module();
    let driver = module
        .functions
        .iter()
        .find(|f| f.name == "driver")
        .unwrap();
    for name in ["f7", "narrow", "swap", "named"] {
        let id = module
            .functions
            .iter()
            .position(|f| f.name == name)
            .unwrap() as u32;
        assert!(
            !driver
                .code
                .iter()
                .any(|i| i.opcode() == Opcode::Call && i.static_call_func_id() == id),
            "driver must compose {name}"
        );
    }
    for mode in ["vm", "function", "osr"] {
        let mut vm = if mode == "vm" {
            Vm::new()
        } else {
            Vm::try_with_jit_config(vo_vm::JitConfig {
                call_threshold: if mode == "function" { 1 } else { 1_000_000 },
                loop_threshold: if mode == "osr" { 1 } else { 1_000_000 },
                optimizing_threshold: 16,
                ..vo_vm::JitConfig::default()
            })
            .unwrap()
        };
        let output = CaptureSink::new();
        vm.set_output_sink(output.clone());
        vm.load(module.clone()).unwrap();
        vm.run().unwrap_or_else(|error| panic!("{mode}: {error:?}"));
        assert_eq!(output.take_bytes(), b"scalar composition ok\n");
        let stats = vm.jit_execution_stats();
        match mode {
            "function" => assert!(stats.function_entries > 0, "{stats:?}"),
            "osr" => assert!(stats.loop_entries > 0, "{stats:?}"),
            _ => assert!(!stats.executed_jit_code()),
        }
    }
}
