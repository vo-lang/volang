//! Complete layouts shared by package wrappers within one compilation.
//! Small layouts are returned inline; large cached layouts share immutable
//! storage. Admission is bounded, and uncached results remain ordinary values.

use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;
use vo_analysis::layout::{try_type_slot_types_with_facts, TypeLayoutFacts};
use vo_analysis::objects::{TCObjects, TypeKey};
use vo_common_core::SlotType;

const MAX_ENTRIES: usize = 4096;
const MAX_SHARED_BYTES: usize = 4 * 1024 * 1024;

#[derive(Clone, Debug)]
pub(crate) enum SlotLayout {
    Inline { types: [SlotType; 2], len: u8 },
    Shared(Rc<[SlotType]>),
    Owned(Vec<SlotType>),
}

impl SlotLayout {
    pub(crate) fn single(ty: SlotType) -> Self {
        Self::Inline {
            types: [ty, SlotType::Value],
            len: 1,
        }
    }

    fn from_vec(types: Vec<SlotType>) -> Self {
        if types.len() <= 2 {
            let mut inline = [SlotType::Value; 2];
            inline[..types.len()].copy_from_slice(&types);
            Self::Inline {
                types: inline,
                len: types.len() as u8,
            }
        } else {
            Self::Shared(types.into())
        }
    }
}

impl Deref for SlotLayout {
    type Target = [SlotType];
    fn deref(&self) -> &Self::Target {
        match self {
            Self::Inline { types, len } => &types[..usize::from(*len)],
            Self::Shared(types) => types,
            Self::Owned(types) => types,
        }
    }
}

impl PartialEq for SlotLayout {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}
impl Eq for SlotLayout {}

#[derive(Default)]
struct Cache {
    entries: HashMap<TypeKey, SlotLayout>,
    shared_bytes: usize,
}

impl Cache {
    fn admit(&mut self, key: TypeKey, types: Vec<SlotType>) -> SlotLayout {
        let bytes = if types.len() <= 2 {
            0
        } else {
            types
                .len()
                .saturating_mul(core::mem::size_of::<SlotType>())
                .saturating_add(2 * core::mem::size_of::<usize>())
        };
        if self.entries.len() == MAX_ENTRIES
            || bytes > MAX_SHARED_BYTES.saturating_sub(self.shared_bytes)
            || self.entries.try_reserve(1).is_err()
        {
            return SlotLayout::Owned(types);
        }
        let layout = SlotLayout::from_vec(types);
        self.shared_bytes += bytes;
        self.entries.insert(key, layout.clone());
        layout
    }
}

pub(crate) struct CodegenLayouts {
    facts: TypeLayoutFacts,
    cache: RefCell<Cache>,
}

impl CodegenLayouts {
    pub(crate) fn new(facts: TypeLayoutFacts) -> Self {
        Self {
            facts,
            cache: RefCell::new(Cache::default()),
        }
    }

    pub(crate) fn slots(&self, key: TypeKey, objects: &TCObjects) -> Result<SlotLayout, String> {
        if let Some(layout) = self.cache.borrow().entries.get(&key).cloned() {
            return Ok(layout);
        }
        let types = try_type_slot_types_with_facts(key, objects, &self.facts)
            .map_err(|error| error.to_string())?;
        Ok(self.cache.borrow_mut().admit(key, types))
    }
}

impl Deref for CodegenLayouts {
    type Target = TypeLayoutFacts;
    fn deref(&self) -> &Self::Target {
        &self.facts
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vo_analysis::arena::ArenaKey;
    use vo_analysis::layout::try_all_type_slot_counts;
    use vo_analysis::typ::BasicType;

    #[test]
    fn repeated_layouts_share_storage_and_keep_invalid_widths_fallible() {
        let mut objects = TCObjects::new();
        let int = objects.universe().lookup_type(BasicType::Int).unwrap();
        let array = objects.new_t_array(int, Some(4096));
        let too_wide = objects.new_t_array(int, Some(65536));
        let layouts = CodegenLayouts::new(try_all_type_slot_counts(&objects).unwrap());
        assert_eq!(&*layouts.slots(int, &objects).unwrap(), &[SlotType::Value]);
        let first = layouts.slots(array, &objects).unwrap();
        let next = layouts.slots(array, &objects).unwrap();
        assert_eq!(first.as_ptr(), next.as_ptr());
        assert_eq!(first.len(), 4096);
        assert!(first.iter().all(|ty| *ty == SlotType::Value));
        assert!(layouts.slots(too_wide, &objects).is_err());
        assert_eq!(layouts.cache.borrow().entries.len(), 2);
    }

    #[test]
    fn admission_bounds_keep_existing_entries_and_return_complete_uncached_values() {
        let mut cache = Cache::default();
        for index in 0..MAX_ENTRIES {
            cache.admit(TypeKey::from_usize(index), vec![SlotType::Value]);
        }
        let extra = cache.admit(TypeKey::from_usize(MAX_ENTRIES), vec![SlotType::Float; 8]);
        assert_eq!(&*extra, &[SlotType::Float; 8]);
        assert_eq!(cache.entries.len(), MAX_ENTRIES);
        assert_eq!(cache.shared_bytes, 0);
        let mut cache = Cache {
            shared_bytes: MAX_SHARED_BYTES,
            ..Cache::default()
        };
        let extra = cache.admit(TypeKey::from_usize(0), vec![SlotType::GcRef; 32]);
        assert_eq!(&*extra, &[SlotType::GcRef; 32]);
        assert!(cache.entries.is_empty());
        assert_eq!(cache.shared_bytes, MAX_SHARED_BYTES);
    }
}
