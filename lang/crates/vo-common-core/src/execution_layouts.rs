//! Compact execution layouts derived from verified instruction metadata.
//!
//! Serialized instruction metadata remains the semantic authority. A loaded
//! module derives this fixed-width view once, allowing the interpreter to
//! consume already-validated scalar layout facts without repeating enum,
//! vector-shape, and width validation in every container operation.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

use crate::bytecode::{ElemLayout, Module};

const PRESENT: u8 = 1;

#[repr(C)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
struct CompactElementLayout {
    bytes: u32,
    slots: u16,
    needs_sign_extend: u8,
    present: u8,
}

impl CompactElementLayout {
    fn from_layout(layout: ElemLayout) -> Self {
        Self {
            bytes: u32::try_from(layout.bytes)
                .expect("instruction element byte width originates from u32 metadata"),
            slots: layout.slots,
            needs_sign_extend: u8::from(layout.needs_sign_extend),
            present: PRESENT,
        }
    }

    #[inline]
    fn get(self) -> Option<ElemLayout> {
        (self.present == PRESENT).then_some(ElemLayout {
            bytes: self.bytes as usize,
            slots: self.slots,
            needs_sign_extend: self.needs_sign_extend != 0,
        })
    }
}

/// Fixed-width per-PC element layout facts for one function.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionElementLayouts {
    entries: Vec<CompactElementLayout>,
}

impl FunctionElementLayouts {
    #[inline]
    pub fn get(&self, pc: usize) -> Option<ElemLayout> {
        self.entries.get(pc).copied()?.get()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

/// Element-layout execution facts bound to the exact loaded module image.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ElementLayoutMaps {
    functions: Vec<FunctionElementLayouts>,
}

impl ElementLayoutMaps {
    pub(crate) fn build(module: &Module) -> Self {
        let functions = module
            .functions
            .iter()
            .map(|function| FunctionElementLayouts {
                entries: function
                    .instruction_metadata
                    .iter()
                    .map(|metadata| {
                        metadata
                            .elem_layout()
                            .map(CompactElementLayout::from_layout)
                            .unwrap_or_default()
                    })
                    .collect(),
            })
            .collect();
        Self { functions }
    }

    #[inline]
    pub fn function(&self, func_id: u32) -> Option<&FunctionElementLayouts> {
        self.functions.get(func_id as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::{FunctionDef, InstructionMetadata};
    use crate::SlotType;

    #[test]
    fn maps_valid_layouts_and_rejects_invalid_shapes() {
        let mut module = Module::new("execution-layouts".to_string());
        let function = FunctionDef {
            name: "f".to_string(),
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
            code: Vec::new(),
            instruction_metadata: vec![
                InstructionMetadata::ElemLayout {
                    elem_bytes: 8,
                    needs_sign_extend: false,
                    slot_layout: vec![SlotType::Value],
                },
                InstructionMetadata::ElemLayout {
                    elem_bytes: 9,
                    needs_sign_extend: false,
                    slot_layout: vec![SlotType::Value],
                },
                InstructionMetadata::None,
            ],
            slot_types: Vec::new(),
            capture_types: Vec::new(),
            capture_slot_types: Vec::new(),
            param_types: Vec::new(),
        };
        module.functions.push(function);

        let maps = ElementLayoutMaps::build(&module);
        let layouts = maps.function(0).expect("function layouts");
        assert_eq!(
            layouts.get(0),
            Some(ElemLayout {
                bytes: 8,
                slots: 1,
                needs_sign_extend: false,
            })
        );
        assert_eq!(layouts.get(1), None);
        assert_eq!(layouts.get(2), None);
    }
}
