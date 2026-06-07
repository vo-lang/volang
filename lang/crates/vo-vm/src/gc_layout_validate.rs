//! Central GC layout validation for module metadata loaded by the VM.

#[cfg(not(feature = "std"))]
use alloc::{format, string::String, string::ToString};

use crate::bytecode::{FunctionDef, Module};
use vo_common_core::types::SlotType;

pub fn validate_module_gc_layout(module: &Module) -> Result<(), String> {
    let mut total_global_slots = 0usize;
    for (idx, global) in module.globals.iter().enumerate() {
        total_global_slots = total_global_slots
            .checked_add(global.slots as usize)
            .ok_or_else(|| "global slot count overflows usize".to_string())?;
        validate_slot_layout(
            &format!("global {idx} ({})", global.name),
            global.slots as usize,
            &global.slot_types,
        )?;
    }
    let _ = total_global_slots;

    for (idx, meta) in module.struct_metas.iter().enumerate() {
        let label = format!("struct_meta {idx}");
        validate_slot_layout(&label, meta.slot_types.len(), &meta.slot_types)?;
        for (field_idx, field) in meta.fields.iter().enumerate() {
            let end = (field.offset as usize)
                .checked_add(field.slot_count as usize)
                .ok_or_else(|| {
                    format!(
                        "{label} field {field_idx} ({}) slot range overflows",
                        field.name
                    )
                })?;
            if end > meta.slot_types.len() {
                return Err(format!(
                    "{label} field {field_idx} ({}) slot range {}..{} exceeds struct slots {}",
                    field.name,
                    field.offset,
                    end,
                    meta.slot_types.len()
                ));
            }
        }
    }

    for (idx, func) in module.functions.iter().enumerate() {
        validate_function_gc_layout(idx, func)?;
    }

    Ok(())
}

pub fn validate_slot_layout(
    label: &str,
    slots: usize,
    slot_types: &[SlotType],
) -> Result<(), String> {
    if slot_types.len() != slots {
        return Err(format!(
            "{label} slot_types len {} does not match slots {}",
            slot_types.len(),
            slots
        ));
    }
    validate_interface_pairs(label, slot_types)
}

pub fn validate_interface_pairs(label: &str, slot_types: &[SlotType]) -> Result<(), String> {
    for (slot_idx, slot_type) in slot_types.iter().enumerate() {
        match slot_type {
            SlotType::Interface0 => {
                if slot_types.get(slot_idx + 1) != Some(&SlotType::Interface1) {
                    return Err(format!(
                        "{label} Interface0 slot {slot_idx} is not followed by Interface1"
                    ));
                }
            }
            SlotType::Interface1 => {
                if slot_idx == 0 || slot_types.get(slot_idx - 1) != Some(&SlotType::Interface0) {
                    return Err(format!(
                        "{label} Interface1 slot {slot_idx} is not preceded by Interface0"
                    ));
                }
            }
            _ => {}
        }
    }
    Ok(())
}

fn validate_function_gc_layout(idx: usize, func: &FunctionDef) -> Result<(), String> {
    let label = format!("function {idx} ({})", func.name);
    let local_slots = func.local_slots as usize;

    if func.slot_types.len() > local_slots {
        return Err(format!(
            "{label} slot_types len {} exceeds local_slots {}",
            func.slot_types.len(),
            func.local_slots
        ));
    }
    validate_interface_pairs(&label, &func.slot_types)?;

    if func.gc_scan_slots as usize > func.slot_types.len() {
        return Err(format!(
            "{label} gc_scan_slots {} exceeds slot_types len {}",
            func.gc_scan_slots,
            func.slot_types.len()
        ));
    }

    let expected_scan_slots = FunctionDef::compute_gc_scan_slots(&func.slot_types);
    if func.gc_scan_slots < expected_scan_slots {
        return Err(format!(
            "{label} gc_scan_slots {} under-scans slot_types; expected at least {}",
            func.gc_scan_slots, expected_scan_slots
        ));
    }

    if !func.borrowed_scan_slots_prefix.is_empty() {
        let expected_prefix = FunctionDef::compute_borrowed_scan_slots_prefix(&func.slot_types);
        if func.borrowed_scan_slots_prefix != expected_prefix {
            return Err(format!(
                "{label} borrowed_scan_slots_prefix does not match slot_types"
            ));
        }
    }

    validate_slot_layout(
        &format!("{label} return slots"),
        func.ret_slots as usize,
        &func.ret_slot_types,
    )?;

    if !func.capture_slot_types.is_empty() {
        validate_interface_pairs(&format!("{label} capture slots"), &func.capture_slot_types)?;
    }

    if !func.heap_ret_slots.is_empty()
        && func.heap_ret_slots.len() != func.heap_ret_gcref_count as usize
    {
        return Err(format!(
            "{label} heap_ret_slots len {} does not match heap_ret_gcref_count {}",
            func.heap_ret_slots.len(),
            func.heap_ret_gcref_count
        ));
    }
    let heap_ret_end = (func.heap_ret_gcref_start as usize)
        .checked_add(func.heap_ret_gcref_count as usize)
        .ok_or_else(|| format!("{label} heap return GcRef range overflows"))?;
    if heap_ret_end > local_slots {
        return Err(format!(
            "{label} heap return GcRef range {}..{} exceeds local_slots {}",
            func.heap_ret_gcref_start, heap_ret_end, func.local_slots
        ));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_runtime::bytecode::{FieldMeta, FunctionDef, StructMeta};
    use vo_runtime::{ValueKind, ValueRttid};

    fn function_with_slot_types(slot_types: Vec<SlotType>) -> FunctionDef {
        FunctionDef {
            name: "f".to_string(),
            param_count: 0,
            param_slots: 0,
            local_slots: slot_types.len() as u16,
            gc_scan_slots: FunctionDef::compute_gc_scan_slots(&slot_types),
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
            code: Vec::new(),
            jit_metadata: Vec::new(),
            borrowed_scan_slots_prefix: FunctionDef::compute_borrowed_scan_slots_prefix(
                &slot_types,
            ),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
            slot_types,
        }
    }

    #[test]
    fn gc_layout_rejects_global_width_mismatch() {
        let mut module = Module::new("test".to_string());
        module.globals.push(vo_runtime::bytecode::GlobalDef {
            name: "g".to_string(),
            slots: 1,
            value_kind: ValueKind::String as u8,
            meta_id: 0,
            slot_types: Vec::new(),
        });

        let err = validate_module_gc_layout(&module).unwrap_err();
        assert!(err.contains("global 0 (g) slot_types len 0 does not match slots 1"));
    }

    #[test]
    fn gc_layout_rejects_struct_field_width_mismatch() {
        let mut module = Module::new("test".to_string());
        module.struct_metas.push(StructMeta {
            slot_types: vec![SlotType::Value],
            fields: vec![FieldMeta {
                name: "field".to_string(),
                offset: 0,
                slot_count: 2,
                type_info: ValueRttid::from_raw(0),
                embedded: false,
                tag: None,
            }],
            field_index: Default::default(),
        });

        let err = validate_module_gc_layout(&module).unwrap_err();
        assert!(err.contains("field 0 (field) slot range 0..2 exceeds struct slots 1"));
    }

    #[test]
    fn gc_layout_rejects_orphan_interface1() {
        let err = validate_interface_pairs("layout", &[SlotType::Interface1]).unwrap_err();
        assert!(err.contains("Interface1 slot 0 is not preceded by Interface0"));
    }

    #[test]
    fn gc_layout_rejects_interface0_without_interface1() {
        let err = validate_interface_pairs("layout", &[SlotType::Interface0]).unwrap_err();
        assert!(err.contains("Interface0 slot 0 is not followed by Interface1"));
    }

    #[test]
    fn gc_layout_rejects_function_gc_scan_under_scan() {
        let mut module = Module::new("test".to_string());
        let mut func = function_with_slot_types(vec![SlotType::GcRef]);
        func.gc_scan_slots = 0;
        module.functions.push(func);

        let err = validate_module_gc_layout(&module).unwrap_err();
        assert!(err.contains("under-scans"));
    }

    #[test]
    fn gc_layout_rejects_capture_interface_pair_drift() {
        let mut module = Module::new("test".to_string());
        let mut func = function_with_slot_types(vec![]);
        func.capture_slot_types = vec![SlotType::Interface0, SlotType::Value];
        module.functions.push(func);

        let err = validate_module_gc_layout(&module).unwrap_err();
        assert!(err.contains("capture slots Interface0 slot 0 is not followed by Interface1"));
    }

    #[test]
    fn gc_layout_rejects_return_slot_width_mismatch() {
        let mut module = Module::new("test".to_string());
        let mut func = function_with_slot_types(vec![SlotType::Value]);
        func.ret_slots = 1;
        func.ret_slot_types = Vec::new();
        module.functions.push(func);

        let err = validate_module_gc_layout(&module).unwrap_err();
        assert!(err.contains("return slots slot_types len 0 does not match slots 1"));
    }
}
