//! Interface method table (itab) management.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::collections::HashMap;
#[cfg(not(feature = "std"))]
use hashbrown::HashMap;

use crate::bytecode::{InterfaceMeta, StructMeta};

#[derive(Debug, Clone)]
pub struct Itab {
    pub methods: Vec<u32>,
}

#[derive(Debug, Default)]
pub struct ItabCache {
    cache: HashMap<(u32, u32), u32>,
    itabs: Vec<Itab>,
}

impl ItabCache {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            itabs: Vec::new(),
        }
    }

    pub fn get_itab(&self, itab_id: u32) -> Option<&Itab> {
        self.itabs.get(itab_id as usize)
    }

    pub fn get_or_create(
        &mut self,
        meta_id: u32,
        iface_meta_id: u32,
        struct_metas: &[StructMeta],
        interface_metas: &[InterfaceMeta],
    ) -> u32 {
        let key = (meta_id, iface_meta_id);

        if let Some(&itab_id) = self.cache.get(&key) {
            return itab_id;
        }

        let itab = self.build_itab(meta_id, iface_meta_id, struct_metas, interface_metas);
        let itab_id = self.itabs.len() as u32;
        self.itabs.push(itab);
        self.cache.insert(key, itab_id);

        itab_id
    }

    fn build_itab(
        &self,
        meta_id: u32,
        iface_meta_id: u32,
        struct_metas: &[StructMeta],
        interface_metas: &[InterfaceMeta],
    ) -> Itab {
        let struct_meta = &struct_metas[meta_id as usize];
        let iface_meta = &interface_metas[iface_meta_id as usize];

        let methods: Vec<u32> = iface_meta
            .method_names
            .iter()
            .map(|name| {
                *struct_meta
                    .methods
                    .get(name)
                    .expect("method not found in struct")
            })
            .collect();

        Itab { methods }
    }

    #[inline]
    pub fn lookup_method(&self, itab_id: u32, method_idx: usize) -> u32 {
        self.itabs[itab_id as usize].methods[method_idx]
    }
}
