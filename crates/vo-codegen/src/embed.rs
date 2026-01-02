//! Embedding path analysis for method resolution.
//!
//! This module provides a unified way to analyze embedding paths for:
//! - Direct method calls (compile_concrete_method)
//! - Itab building (build_itab)
//! - Wrapper generation
//!
//! The key insight is that all method calls through embedding share the same
//! path analysis logic, whether at compile time or for runtime dispatch.

use vo_analysis::objects::{ObjKey, TCObjects, TypeKey};
use vo_analysis::check::type_info as layout;
use vo_analysis::typ::{self, Type};

/// Result of analyzing an embedding path.
#[derive(Debug, Clone)]
pub struct EmbedPathInfo {
    /// Steps in the embedding path (empty = direct method, no embedding)
    pub steps: Vec<EmbedStep>,
    /// Total byte offset to reach the final embedded field
    pub total_offset: u16,
    /// The final type after traversing the path
    pub final_type: TypeKey,
    /// If the path ends at an interface field, this contains the interface info
    pub embedded_iface: Option<EmbeddedIfaceInfo>,
}

/// A single step in an embedding path.
#[derive(Debug, Clone, Copy)]
pub struct EmbedStep {
    /// Field index at this level
    pub field_index: usize,
    /// Byte offset of this field within its parent struct
    pub field_offset: u16,
    /// Type of this field
    pub field_type: TypeKey,
    /// True if this field is a pointer type (*T)
    pub is_pointer: bool,
    /// True if this field is an interface type
    pub is_interface: bool,
}

/// Info about an embedded interface at the end of the path.
#[derive(Debug, Clone, Copy)]
pub struct EmbeddedIfaceInfo {
    /// Offset to the interface field
    pub offset: u16,
    /// The interface type
    pub iface_type: TypeKey,
}

impl EmbedPathInfo {
    /// Returns true if path contains any pointer embeddings.
    pub fn has_pointer_embed(&self) -> bool {
        self.steps.iter().any(|s| s.is_pointer)
    }
    
    /// Returns true if path ends at an embedded interface.
    pub fn ends_at_interface(&self) -> bool {
        self.embedded_iface.is_some()
    }
}

/// Analyze an embedding path from indices (as returned by Selection or lookup).
///
/// The indices array is [field_idx, ..., method_idx] where:
/// - All but the last element are field indices for embedded structs/interfaces
/// - The last element is the method index (ignored in path analysis)
///
/// This function is the single source of truth for embedding path analysis.
pub fn analyze_embed_path(
    recv_type: TypeKey,
    indices: &[usize],
    tc_objs: &TCObjects,
) -> EmbedPathInfo {
    let mut steps = Vec::new();
    let mut total_offset = 0u16;
    let mut current_type = recv_type;
    let mut embedded_iface = None;
    
    // Strip pointer from receiver if needed
    if layout::is_pointer(current_type, tc_objs) {
        let underlying = typ::underlying_type(current_type, tc_objs);
        if let Type::Pointer(p) = &tc_objs.types[underlying] {
            current_type = p.base();
        }
    }
    
    // Walk through all indices except the last one (which is method index)
    let path_indices = if indices.len() > 1 {
        &indices[..indices.len() - 1]
    } else {
        &[]
    };
    
    for &idx in path_indices {
        // Get field info at current level
        let (field_offset, _slots) = layout::struct_field_offset_by_index(current_type, idx, tc_objs);
        let field_type = layout::struct_field_type_by_index(current_type, idx, tc_objs);
        
        let is_pointer = layout::is_pointer(field_type, tc_objs);
        let is_interface = layout::is_interface(field_type, tc_objs);
        
        steps.push(EmbedStep {
            field_index: idx,
            field_offset,
            field_type,
            is_pointer,
            is_interface,
        });
        
        total_offset += field_offset;
        
        // Check if we hit an interface - that's a special case
        if is_interface {
            embedded_iface = Some(EmbeddedIfaceInfo {
                offset: total_offset,
                iface_type: field_type,
            });
            // Don't continue past interface - method comes from the interface
            break;
        }
        
        // Move to next level
        if is_pointer {
            // Pointer embedding: dereference to get the base type
            let underlying = typ::underlying_type(field_type, tc_objs);
            if let Type::Pointer(p) = &tc_objs.types[underlying] {
                current_type = p.base();
            }
            // Note: offset resets after pointer dereference in some sense,
            // but we track the pointer in the step for code gen
        } else {
            current_type = field_type;
        }
    }
    
    EmbedPathInfo {
        steps,
        total_offset,
        final_type: current_type,
        embedded_iface,
    }
}

/// Check if an ObjKey refers to an interface method declaration (no body).
/// Interface method declarations are not registered in objkey_to_iface_func.
pub fn is_interface_method_decl(obj_key: ObjKey, tc_objs: &TCObjects) -> bool {
    let obj = &tc_objs.lobjs[obj_key];
    if !obj.entity_type().is_func() {
        return false;
    }
    
    // Interface method declarations have a signature with no body
    // We can check if the method's receiver type is an interface
    if let Some(method_type) = obj.typ() {
        if let Some(sig) = tc_objs.types[method_type].try_as_signature() {
            if let Some(recv_obj) = *sig.recv() {
                if let Some(recv_type) = tc_objs.lobjs[recv_obj].typ() {
                    return layout::is_interface(recv_type, tc_objs);
                }
            }
        }
    }
    
    false
}

#[cfg(test)]
mod tests {
    // Unit tests would go here
}
