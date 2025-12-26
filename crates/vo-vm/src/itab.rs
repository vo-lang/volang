//! Interface method table (itab) management.
//!
//! Unified itab table design:
//! - VM init: copy module.itabs to vm.itabs
//! - Runtime: new itabs are appended to vm.itabs

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(not(feature = "std"))]
use hashbrown::HashMap;

use crate::bytecode::{InterfaceMeta, Itab, NamedTypeMeta};

/// Unified itab table with runtime cache for interface-to-interface assignments.
#[derive(Debug, Default)]
pub struct ItabCache {
    /// Cache for runtime-built itabs: (named_type_id, iface_meta_id) -> itab_id
    cache: HashMap<(u32, u32), u32>,
    /// Unified itab table: initialized from module.itabs, runtime itabs appended
    itabs: Vec<Itab>,
}

impl ItabCache {
    /// Create from module's compile-time itabs
    pub fn from_module_itabs(itabs: Vec<Itab>) -> Self {
        Self {
            cache: HashMap::new(),
            itabs,
        }
    }

    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            itabs: Vec::new(),
        }
    }

    pub fn get_itab(&self, itab_id: u32) -> Option<&Itab> {
        self.itabs.get(itab_id as usize)
    }

    /// Get or create itab for interface-to-interface assignment (runtime).
    /// For concrete type assignments, itab_id is already in the constant.
    pub fn get_or_create(
        &mut self,
        named_type_id: u32,
        iface_meta_id: u32,
        named_type_metas: &[NamedTypeMeta],
        interface_metas: &[InterfaceMeta],
    ) -> u32 {
        let key = (named_type_id, iface_meta_id);

        if let Some(&itab_id) = self.cache.get(&key) {
            return itab_id;
        }

        let itab = Self::build_itab(named_type_id, iface_meta_id, named_type_metas, interface_metas);
        let itab_id = self.itabs.len() as u32;
        self.itabs.push(itab);
        self.cache.insert(key, itab_id);

        itab_id
    }

    fn build_itab(
        named_type_id: u32,
        iface_meta_id: u32,
        named_type_metas: &[NamedTypeMeta],
        interface_metas: &[InterfaceMeta],
    ) -> Itab {
        let named_type = &named_type_metas[named_type_id as usize];
        let iface_meta = &interface_metas[iface_meta_id as usize];

        // Method set check done at compile time
        let methods: Vec<u32> = iface_meta
            .method_names
            .iter()
            .map(|name| {
                named_type
                    .methods
                    .get(name)
                    .expect("method not found in named type")
                    .func_id
            })
            .collect();

        Itab { methods }
    }

    #[inline]
    pub fn lookup_method(&self, itab_id: u32, method_idx: usize) -> u32 {
        self.itabs[itab_id as usize].methods[method_idx]
    }
}
