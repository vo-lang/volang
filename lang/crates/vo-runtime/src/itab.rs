//! Interface method table (itab) management.
//!
//! Unified itab table design:
//! - VM init: copy module.itabs to vm.itabs
//! - Runtime: new itabs are appended to vm.itabs
//!
//! NOTE: This is shared between VM and runtime externs, so runtime helpers can
//! construct correct interface values (with non-zero itab_id) when needed.

#[cfg(not(feature = "std"))]
use alloc::vec;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(not(feature = "std"))]
use hashbrown::HashMap;
#[cfg(feature = "std")]
use std::collections::HashMap;

use crate::{RuntimeType, ValueKind, ValueRttid};
use vo_common_core::bytecode::{InterfaceMeta, Itab, Module, NamedTypeMeta};

pub fn expected_interface_itab_methods(
    named_type_id: u32,
    iface_meta_id: u32,
    src_is_pointer: bool,
    named_type_metas: &[NamedTypeMeta],
    interface_metas: &[InterfaceMeta],
) -> Option<Vec<u32>> {
    let named_type = named_type_metas.get(named_type_id as usize)?;
    let iface_meta = interface_metas.get(iface_meta_id as usize)?;

    let mut methods = Vec::with_capacity(iface_meta.methods.len());
    for iface_method in &iface_meta.methods {
        let method_info = named_type.methods.get(&iface_method.name)?;
        // Value types cannot use pointer receiver methods
        if !src_is_pointer && method_info.is_pointer_receiver {
            return None;
        }
        if method_info.signature_rttid != iface_method.signature_rttid {
            return None;
        }
        methods.push(method_info.func_id);
    }
    Some(methods)
}

/// Unified itab table with runtime cache for interface-to-interface assignments.
#[derive(Debug, Default)]
pub struct ItabCache {
    /// Cache for runtime-built itabs: (named_type_id, iface_meta_id, src_is_pointer) -> itab_id
    cache: HashMap<(u32, u32, bool), u32>,
    /// Unified itab table: initialized from module.itabs, runtime itabs appended
    itabs: Vec<Itab>,
}

impl ItabCache {
    /// Create from module's compile-time itabs
    pub fn from_module_itabs(itabs: Vec<Itab>) -> Self {
        let itabs = if itabs.is_empty() {
            vec![Itab::default()]
        } else {
            itabs
        };
        Self {
            cache: HashMap::new(),
            itabs,
        }
    }

    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            itabs: vec![Itab::default()],
        }
    }

    pub fn get_itab(&self, itab_id: u32) -> Option<&Itab> {
        self.itabs.get(itab_id as usize)
    }

    /// Get or create itab for interface-to-interface assignment (runtime).
    /// For concrete type assignments, itab_id is already in the constant.
    /// Panics if named type doesn't implement the interface (compile-time checked).
    ///
    /// `src_is_pointer`: true if source is pointer type (*T), false if value type (T).
    /// Value types cannot use pointer receiver methods.
    pub fn get_or_create(
        &mut self,
        named_type_id: u32,
        iface_meta_id: u32,
        src_is_pointer: bool,
        named_type_metas: &[NamedTypeMeta],
        interface_metas: &[InterfaceMeta],
    ) -> u32 {
        self.try_get_or_create(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            named_type_metas,
            interface_metas,
        )
        .expect("method not found in named type")
    }

    /// Try to get or create itab. Returns None if named type doesn't implement the interface.
    /// Use this for dynamic access where type mismatch should return error, not panic.
    ///
    /// `src_is_pointer`: true if source is pointer type (*T), false if value type (T).
    /// Value types cannot use pointer receiver methods.
    pub fn try_get_or_create(
        &mut self,
        named_type_id: u32,
        iface_meta_id: u32,
        src_is_pointer: bool,
        named_type_metas: &[NamedTypeMeta],
        interface_metas: &[InterfaceMeta],
    ) -> Option<u32> {
        let key = (named_type_id, iface_meta_id, src_is_pointer);

        if let Some(&itab_id) = self.cache.get(&key) {
            return Some(itab_id);
        }

        let itab = Self::try_build_itab(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            named_type_metas,
            interface_metas,
        )?;
        let itab_id = u32::try_from(self.itabs.len()).ok()?;
        self.itabs.push(itab);
        self.cache.insert(key, itab_id);

        Some(itab_id)
    }

    /// Build itab for a named type implementing an interface.
    ///
    /// `src_is_pointer`: true if source is pointer type (*T), false if value type (T).
    /// When src_is_pointer is false, methods with pointer receivers are not accessible.
    fn try_build_itab(
        named_type_id: u32,
        iface_meta_id: u32,
        src_is_pointer: bool,
        named_type_metas: &[NamedTypeMeta],
        interface_metas: &[InterfaceMeta],
    ) -> Option<Itab> {
        let methods = expected_interface_itab_methods(
            named_type_id,
            iface_meta_id,
            src_is_pointer,
            named_type_metas,
            interface_metas,
        )?;

        Some(Itab {
            iface_meta_id,
            methods,
        })
    }

    #[inline]
    pub fn lookup_method(&self, itab_id: u32, method_idx: usize) -> u32 {
        self.itabs[itab_id as usize].methods[method_idx]
    }

    /// Get pointer to itabs slice for JIT context.
    pub fn itabs_ptr(&self) -> *const core::ffi::c_void {
        self.itabs.as_ptr() as *const core::ffi::c_void
    }
}

// =============================================================================
// Interface satisfaction checking
// =============================================================================

/// Check if a source type satisfies a target interface.
///
/// This is the single source of truth for interface satisfaction checking,
/// used by both VM and JIT type assertions.
///
/// - `src_rttid`: Runtime type ID of the source value
/// - `src_vk`: ValueKind of the source value (Pointer vs Struct, etc.)
/// - `target_iface_id`: Interface meta ID of the target interface
/// - `module`: Module containing type metadata
///
/// Returns true if the source type implements all methods of the target interface
/// with matching signatures, respecting pointer receiver rules.
pub fn check_interface_satisfaction(
    src_rttid: u32,
    src_vk: ValueKind,
    target_iface_id: u32,
    module: &Module,
) -> bool {
    let iface_meta = match module.interface_metas.get(target_iface_id as usize) {
        Some(m) => m,
        None => return false,
    };

    if iface_meta.methods.is_empty() {
        return true; // empty interface always satisfied
    }

    let Some(named_type_id) = module.named_type_id_for_rttid(src_rttid) else {
        return false;
    };
    expected_interface_itab_methods(
        named_type_id,
        target_iface_id,
        src_vk == ValueKind::Pointer,
        &module.named_type_metas,
        &module.interface_metas,
    )
    .is_some()
}

fn interface_method_set_includes(source: &InterfaceMeta, target: &InterfaceMeta) -> bool {
    target.methods.iter().all(|target_method| {
        source.methods.iter().any(|source_method| {
            source_method.name == target_method.name
                && source_method.signature_rttid == target_method.signature_rttid
        })
    })
}

/// Apply the typed ordinary-assignment rules available from runtime metadata.
///
/// This is shared by dynamic calls and FFI signature checks so concrete-to-
/// interface assignments, interface method-set inclusion, and named/unnamed
/// identity all use one fail-closed implementation.
pub fn runtime_value_is_assignable(
    source: ValueRttid,
    target: ValueRttid,
    module: &Module,
) -> bool {
    let resolver = module.runtime_type_resolver();
    let Some((source_underlying, source_runtime_type)) = resolver.resolve_value_rttid(source)
    else {
        return false;
    };
    let Some((target_underlying, target_runtime_type)) = resolver.resolve_value_rttid(target)
    else {
        return false;
    };

    if let RuntimeType::Interface {
        meta_id: target_meta_id,
        ..
    } = target_runtime_type
    {
        let Some(target_interface) = module.interface_metas.get(*target_meta_id as usize) else {
            return false;
        };
        if source == target {
            return true;
        }
        if target_interface.methods.is_empty() {
            return true;
        }

        if let RuntimeType::Interface {
            meta_id: source_meta_id,
            ..
        } = source_runtime_type
        {
            let Some(source_interface) = module.interface_metas.get(*source_meta_id as usize)
            else {
                return false;
            };
            return interface_method_set_includes(source_interface, target_interface);
        }

        return check_interface_satisfaction(
            source.rttid(),
            source.value_kind(),
            *target_meta_id,
            module,
        );
    }

    if source == target {
        return true;
    }

    let Some(source_top_level) = module.runtime_types.get(source.rttid() as usize) else {
        return false;
    };
    let Some(target_top_level) = module.runtime_types.get(target.rttid() as usize) else {
        return false;
    };
    if matches!(source_top_level, RuntimeType::Named { .. })
        && matches!(target_top_level, RuntimeType::Named { .. })
    {
        return false;
    }

    source_underlying == target_underlying
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_common_core::bytecode::{InterfaceMethodMeta, MethodInfo};
    use vo_common_core::runtime_type::InterfaceMethod;
    use vo_common_core::types::{ValueMeta, ValueRttid};

    #[test]
    fn itab_cache_reserves_zero_for_no_itab_060() {
        let mut methods = std::collections::BTreeMap::new();
        methods.insert(
            "M".to_string(),
            MethodInfo {
                func_id: 7,
                is_pointer_receiver: false,
                receiver_is_iface_boxed: false,
                signature_rttid: 3,
            },
        );
        let named = vec![NamedTypeMeta {
            name: "T".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
            methods,
        }];
        let interfaces = vec![InterfaceMeta {
            name: "I".to_string(),
            method_names: vec!["M".to_string()],
            methods: vec![InterfaceMethodMeta {
                name: "M".to_string(),
                signature_rttid: 3,
            }],
        }];

        let mut cache = ItabCache::new();
        assert_eq!(cache.get_itab(0).map(|itab| itab.methods.len()), Some(0));
        assert_eq!(
            cache.try_get_or_create(0, 0, false, &named, &interfaces),
            Some(1)
        );

        let mut cache = ItabCache::from_module_itabs(Vec::new());
        assert_eq!(cache.get_itab(0).map(|itab| itab.methods.len()), Some(0));
        assert_eq!(
            cache.try_get_or_create(0, 0, false, &named, &interfaces),
            Some(1)
        );
    }

    #[test]
    fn itab_cache_rejects_signature_mismatch_060() {
        let mut methods = std::collections::BTreeMap::new();
        methods.insert(
            "M".to_string(),
            MethodInfo {
                func_id: 7,
                is_pointer_receiver: false,
                receiver_is_iface_boxed: false,
                signature_rttid: 3,
            },
        );
        let named = vec![NamedTypeMeta {
            name: "T".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: ValueRttid::new(0, ValueKind::Int64),
            methods,
        }];
        let interfaces = vec![InterfaceMeta {
            name: "I".to_string(),
            method_names: vec!["M".to_string()],
            methods: vec![InterfaceMethodMeta {
                name: "M".to_string(),
                signature_rttid: 4,
            }],
        }];

        let mut cache = ItabCache::new();

        assert_eq!(
            cache.try_get_or_create(0, 0, false, &named, &interfaces),
            None
        );
    }

    #[test]
    fn runtime_assignment_checks_concrete_and_interface_method_sets() {
        let mut module = Module::new("runtime-interface-assignability".to_string());
        let int_type = ValueRttid::new(0, ValueKind::Int64);
        let m_signature = ValueRttid::new(1, ValueKind::Closure);
        let n_signature = ValueRttid::new(2, ValueKind::Closure);
        module.runtime_types.extend([
            RuntimeType::Basic(ValueKind::Int64),
            RuntimeType::Func {
                params: Vec::new(),
                results: Vec::new(),
                variadic: false,
            },
            RuntimeType::Func {
                params: Vec::new(),
                results: vec![int_type],
                variadic: false,
            },
        ]);

        let mut methods = std::collections::BTreeMap::new();
        methods.insert(
            "M".to_string(),
            MethodInfo {
                func_id: 7,
                is_pointer_receiver: false,
                receiver_is_iface_boxed: false,
                signature_rttid: m_signature.rttid(),
            },
        );
        module.named_type_metas.push(NamedTypeMeta {
            name: "T".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: int_type,
            methods,
        });
        module.named_type_metas.push(NamedTypeMeta {
            name: "U".to_string(),
            underlying_meta: ValueMeta::new(0, ValueKind::Int64),
            underlying_rttid: int_type,
            methods: Default::default(),
        });
        module.runtime_types.extend([
            RuntimeType::Named {
                id: 0,
                struct_meta_id: None,
            },
            RuntimeType::Named {
                id: 1,
                struct_meta_id: None,
            },
        ]);

        module.interface_metas.extend([
            InterfaceMeta {
                name: "MOnly".to_string(),
                method_names: vec!["M".to_string()],
                methods: vec![InterfaceMethodMeta {
                    name: "M".to_string(),
                    signature_rttid: m_signature.rttid(),
                }],
            },
            InterfaceMeta {
                name: "MN".to_string(),
                method_names: vec!["M".to_string(), "N".to_string()],
                methods: vec![
                    InterfaceMethodMeta {
                        name: "M".to_string(),
                        signature_rttid: m_signature.rttid(),
                    },
                    InterfaceMethodMeta {
                        name: "N".to_string(),
                        signature_rttid: n_signature.rttid(),
                    },
                ],
            },
            InterfaceMeta {
                name: "Any".to_string(),
                method_names: Vec::new(),
                methods: Vec::new(),
            },
        ]);
        module.runtime_types.extend([
            RuntimeType::Interface {
                methods: vec![InterfaceMethod::new("M".to_string(), m_signature)],
                meta_id: 0,
            },
            RuntimeType::Interface {
                methods: vec![
                    InterfaceMethod::new("M".to_string(), m_signature),
                    InterfaceMethod::new("N".to_string(), n_signature),
                ],
                meta_id: 1,
            },
            RuntimeType::Interface {
                methods: Vec::new(),
                meta_id: 2,
            },
        ]);
        let named_t = ValueRttid::new(3, ValueKind::Int64);
        let named_u = ValueRttid::new(4, ValueKind::Int64);
        let m_only = ValueRttid::new(5, ValueKind::Interface);
        let mn = ValueRttid::new(6, ValueKind::Interface);
        let any = ValueRttid::new(7, ValueKind::Interface);
        module.runtime_types.push(RuntimeType::Pointer(named_t));
        let pointer_t = ValueRttid::new(8, ValueKind::Pointer);

        assert!(runtime_value_is_assignable(named_t, m_only, &module));
        assert!(runtime_value_is_assignable(pointer_t, m_only, &module));
        assert!(!runtime_value_is_assignable(named_u, m_only, &module));
        assert!(runtime_value_is_assignable(mn, m_only, &module));
        assert!(!runtime_value_is_assignable(m_only, mn, &module));
        assert!(runtime_value_is_assignable(named_u, any, &module));
        assert!(!runtime_value_is_assignable(any, named_u, &module));
    }
}
