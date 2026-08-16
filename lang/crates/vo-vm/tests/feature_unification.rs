#![cfg(not(feature = "std"))]

use vo_vm::bytecode::{
    ExternDef, ExternEffects, FunctionDef, InstructionMetadata, Module, ParamShape, ReturnShape,
};
use vo_vm::instruction::{Instruction, Opcode};
use vo_vm::vm::{Vm, VmError};

fn module_requiring_native_stdin() -> Module {
    let code = vec![Instruction::new(Opcode::Return, 0, 0, 0)];
    let mut module = Module::new("no-native-stdlib".to_string());
    module.functions.push(FunctionDef {
        name: "main".to_string(),
        param_count: 0,
        param_slots: 0,
        local_slots: 0,
        ret_slots: 0,
        ret_slot_types: Vec::new(),
        recv_slots: 0,
        heap_ret_gcref_count: 0,
        heap_ret_gcref_start: 0,
        heap_ret_slots: Vec::new(),
        is_closure: false,
        error_ret_slot: -1,
        has_defer: false,
        has_calls: false,
        has_call_extern: false,
        instruction_metadata: vec![InstructionMetadata::None; code.len()],
        code,
        slot_types: Vec::new(),
        capture_types: Vec::new(),
        capture_slot_types: Vec::new(),
        param_types: Vec::new(),
    });
    module.externs.push(ExternDef::new(
        vo_runtime::vo_extern_name!("fmt", "nativeReadLine").to_string(),
        ParamShape::exact(0),
        ReturnShape::slots(3),
        ExternEffects::NONE,
        Vec::new(),
    ));
    module
}

#[test]
fn alloc_only_vm_does_not_inherit_native_stdlib_providers() {
    let name = vo_runtime::vo_extern_name!("fmt", "nativeReadLine");
    let error = Vm::new()
        .load(module_requiring_native_stdin())
        .expect_err("alloc-only defaults must leave native providers to the embedder");

    match error {
        VmError::Jit(message) => assert!(message.contains(name), "unexpected error: {message}"),
        other => panic!("unexpected load error: {other:?}"),
    }
}
