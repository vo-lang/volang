//! Authenticated extern lookup and intrinsic selection.
use super::*;

pub(super) fn direct_intrinsic(
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
) -> Option<ExternIntrinsic> {
    let arg_slots = function
        .instruction_metadata
        .get(pc)
        .and_then(InstructionMetadata::call_layout_slots)
        .map(|layout| layout.0)?;
    let resolved = resolved_externs.get(u32::from(instruction.b))?;
    let ExternJitRoute::Intrinsic(intrinsic) = resolved.jit_route else {
        return None;
    };
    matches!(
        intrinsic,
        ExternIntrinsic::Sqrt
            | ExternIntrinsic::Floor
            | ExternIntrinsic::Ceil
            | ExternIntrinsic::Trunc
    )
    .then_some(intrinsic)
    .filter(|_| arg_slots == 1)
}

pub(super) fn core_runtime_extern(
    resolved_externs: &ResolvedExternTable,
    extern_id: u32,
) -> Option<CoreRuntimeExtern> {
    let resolved = resolved_externs.get(extern_id)?;
    if let Ok(key) = vo_common_core::extern_key::decode_extern_name(&resolved.name) {
        let dynamic = match (key.package(), key.function()) {
            ("dyn", "getDynErrors") => Some(CoreRuntimeExtern::DynErrors),
            ("dyn", "GetAttr") => Some(CoreRuntimeExtern::DynGetAttr),
            ("dyn", "GetIndex") => Some(CoreRuntimeExtern::DynGetIndex),
            ("dyn", "SetAttr") => Some(CoreRuntimeExtern::DynSetAttr),
            ("dyn", "SetIndex") => Some(CoreRuntimeExtern::DynSetIndexApi),
            _ => None,
        };
        if dynamic.is_some() {
            return dynamic;
        }
    }
    if resolved.source == RegisteredExternSource::Builtin {
        return match resolved.name.as_str() {
            "vo_copy" => Some(CoreRuntimeExtern::Copy),
            "vo_copy_string" => Some(CoreRuntimeExtern::CopyString),
            "dyn_field" => Some(CoreRuntimeExtern::DynField),
            "dyn_index" => Some(CoreRuntimeExtern::DynIndex),
            "dyn_set_field" => Some(CoreRuntimeExtern::DynSetField),
            "dyn_set_index_unified" => Some(CoreRuntimeExtern::DynSetIndex),
            "dyn_pack_any_slice" => Some(CoreRuntimeExtern::DynPackAnySlice),
            "dyn_call" => Some(CoreRuntimeExtern::DynCall),
            "dyn_method" => Some(CoreRuntimeExtern::DynMethod),
            _ => None,
        };
    }
    if resolved.source != RegisteredExternSource::Stdlib {
        return None;
    }
    let key = vo_common_core::extern_key::decode_extern_name(&resolved.name).ok()?;
    match (key.package(), key.function()) {
        ("errors", "assignTo") if resolved.effective_effects.is_empty() => {
            Some(CoreRuntimeExtern::ErrorsAssignTo)
        }
        ("errors", "identity") if resolved.effective_effects.is_empty() => {
            Some(CoreRuntimeExtern::ErrorsIdentity)
        }
        ("errors", "equal") if resolved.effective_effects.is_empty() => {
            Some(CoreRuntimeExtern::ErrorsEqual)
        }
        _ => None,
    }
}

pub(super) fn extern_requires_host(
    resolved_externs: &ResolvedExternTable,
    function: &FunctionDef,
    pc: usize,
    instruction: &vo_common_core::instruction::Instruction,
) -> bool {
    if direct_intrinsic(resolved_externs, function, pc, instruction).is_some() {
        return false;
    }
    matches!(
        core_runtime_extern(resolved_externs, u32::from(instruction.b)),
        None | Some(CoreRuntimeExtern::Copy | CoreRuntimeExtern::CopyString)
    )
}

pub(super) fn global_slot(module: &ModuleAnalysis<'_>, name: &str) -> Option<u32> {
    let mut slot = 0u32;
    for global in &module.globals {
        if global.name == name {
            return Some(slot);
        }
        slot = slot.checked_add(u32::from(global.slots))?;
    }
    None
}
