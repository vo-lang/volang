//! Runtime type metadata.

#[cfg(not(feature = "std"))]
use alloc::{string::String, vec::Vec};

use vo_common_core::types::SlotType;

/// Struct-specific metadata.
#[derive(Debug, Clone)]
pub struct StructMeta {
    pub name: String,
    pub field_names: Vec<String>,
    pub field_offsets: Vec<u16>,
    pub slot_types: Vec<SlotType>,
}

impl StructMeta {
    pub fn slot_count(&self) -> u16 {
        self.slot_types.len() as u16
    }
}

/// Interface-specific metadata.
#[derive(Debug, Clone)]
pub struct InterfaceMeta {
    pub name: String,
    pub method_names: Vec<String>,
}
