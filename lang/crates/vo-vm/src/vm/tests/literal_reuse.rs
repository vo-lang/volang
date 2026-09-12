use super::*;
use vo_runtime::objects::string;

fn literal_module(value: &str) -> Module {
    let mut module = malformed_single_instruction_module(
        "literal-load-scope",
        vec![
            Instruction::new(Opcode::StrNew, 0, 0, 0),
            Instruction::new(Opcode::StrNew, 1, 0, 0),
            Instruction::new(Opcode::Return, 0, 0, 0),
        ],
        vec![Constant::String(value.into())],
    );
    module.functions[0].slot_types = vec![
        SlotType::GcBase,
        SlotType::GcBase,
        SlotType::Value,
        SlotType::Value,
    ];
    module
}

fn value(vm: &mut Vm) -> GcRef {
    string::try_from_literal(&mut vm.state.gc, vm.module.as_ref().unwrap(), 0)
        .unwrap()
        .unwrap()
}

#[test]
fn successful_load_binds_literal_reuse_for_interpreter_execution() {
    let mut vm = Vm::with_memory_config(vo_runtime::gc::VmMemoryConfig {
        automatic_gc: false,
        ..Default::default()
    });
    vm.load(literal_module("interpreter reuse")).unwrap();
    assert_eq!(vm.literal_cache_metadata_bytes(), 0);
    vm.run().unwrap();
    assert_eq!(vm.memory_stats().object_count, 2);
    assert!(vm.literal_cache_metadata_bytes() > 0);
}

#[test]
fn rejected_replacement_keeps_the_loaded_literal_scope() {
    let mut vm = Vm::new();
    vm.load(literal_module("old module")).unwrap();
    let old_image = vm.module.as_ref().unwrap().clone();
    let old = value(&mut vm);
    let error = vm.load(literal_module("replacement module")).unwrap_err();
    assert!(format!("{error:?}").contains("cannot replace a loaded or previously run module"));
    assert!(Arc::ptr_eq(vm.module.as_ref().unwrap(), &old_image));
    assert_eq!(value(&mut vm), old);
    assert_eq!(unsafe { string::to_bytes(old) }, b"old module");
}

#[test]
fn failed_initial_load_can_retry_and_bind_the_successful_image() {
    let mut vm = Vm::new();
    let mut invalid = literal_module("rejected module");
    invalid.functions[0].code[0].b = u16::MAX;
    assert!(vm.load(invalid).is_err());
    assert!(vm.module.is_none());
    assert_eq!(vm.literal_cache_metadata_bytes(), 0);
    vm.load(literal_module("accepted module")).unwrap();
    let accepted = value(&mut vm);
    assert_eq!(value(&mut vm), accepted);
    assert_eq!(unsafe { string::to_bytes(accepted) }, b"accepted module");
}

#[cfg(feature = "std")]
#[test]
fn inherited_image_uses_child_owned_literal_storage() {
    let mut parent = Vm::new();
    parent.load(literal_module("shared image")).unwrap();
    let original = value(&mut parent);
    let mut child = Vm::new();
    child
        .load_inherited_module(parent.inherited_program_image().unwrap())
        .unwrap();
    assert!(Arc::ptr_eq(
        parent.module.as_ref().unwrap(),
        child.module.as_ref().unwrap()
    ));
    assert_eq!(child.literal_cache_metadata_bytes(), 0);
    let independent = value(&mut child);
    assert_ne!(independent, original);
    assert_eq!(value(&mut child), independent);
    assert_eq!(value(&mut parent), original);
    assert_eq!(child.memory_stats().object_count, 2);
    assert_eq!(parent.memory_stats().object_count, 2);
    assert_eq!(unsafe { string::to_bytes(independent) }, b"shared image");
}
