//! Execute a verified bytecode fixture and retain its error source together
//! with actual native-entry counters, including when execution fails.
use serde_json::json;
use vo_engine::{new_vm_for_mode, CaptureSink, Module, RunMode, VmMemoryConfig};
use vo_vm::vm::VmError;

fn main() {
    let arguments: Vec<_> = std::env::args().collect();
    assert_eq!(
        arguments.len(),
        3,
        "usage: execution_error input.vob vm|jit|osr"
    );
    let mode = match arguments[2].as_str() {
        "vm" => RunMode::Vm,
        "jit" | "osr" => RunMode::Jit,
        _ => panic!("invalid execution mode"),
    };
    // Thresholds are part of the external invocation identity. Keeping them
    // outside the process also avoids mutating the environment after startup.
    let bytes = vo_common_core::serialize::read_vob_file(arguments[1].as_ref()).unwrap();
    let module = Module::deserialize(&bytes).unwrap();
    let debug = module.debug_info.clone();
    let names: Vec<_> = module.functions.iter().map(|f| f.name.clone()).collect();
    let mut vm = new_vm_for_mode(VmMemoryConfig::default(), mode).unwrap();
    let output = CaptureSink::new();
    vm.set_output_sink(output.clone());
    vm.load(module).unwrap();
    let error = vm
        .run()
        .expect_err("fixture must terminate with an execution error");
    let (kind, message, location) = match &error {
        VmError::RuntimeTrap { kind, msg, loc } => (format!("{kind:?}"), msg.clone(), *loc),
        VmError::PanicUnwound { msg, loc } => ("PanicUnwound".to_owned(), format!("{msg:?}"), *loc),
        _ => panic!("unexpected execution error: {error:?}"),
    };
    let source = location.map(|loc| {
        let source = debug.lookup(loc.func_id(), loc.pc());
        json!({
            "func_id":loc.func_id(), "pc":loc.pc(),
            "function":names.get(loc.func_id() as usize),
            "file":source.as_ref().map(|s|s.file.as_str()),
            "line":source.as_ref().map(|s|s.line),
            "column":source.as_ref().map(|s|s.col)
        })
    });
    let stats = vm.jit_execution_stats();
    match arguments[2].as_str() {
        "jit" => assert!(
            stats.function_entries > 0,
            "no function machine code executed"
        ),
        "osr" => assert!(stats.loop_entries > 0, "no OSR machine code executed"),
        _ => assert!(!stats.executed_jit_code()),
    }
    println!(
        "{}",
        json!({
            "mode":arguments[2], "kind":kind, "message":message, "source":source,
            "stdout":String::from_utf8(output.take_bytes()).unwrap(),
            "function_entries":stats.function_entries, "loop_entries":stats.loop_entries,
            "function_compilations":stats.function_compilations,
            "optimizing_compilations":stats.optimizing_compilations,
            "optimizing_functions_executed":stats.optimizing_functions_executed,
            "loop_compilations":stats.loop_compilations,
            "aot_continuation_entries":stats.aot_continuation_entries
        })
    );
}
