//! Garbage collector core.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::alloc as heap_alloc;
#[cfg(not(feature = "std"))]
use alloc::alloc as heap_alloc;

use vo_common_core::types::{ValueKind, ValueMeta};

/// GC object header - 8 bytes.
/// Layout: [mark:8 | gen:8 | slots:16 | ValueMeta:32]
///
/// ValueMeta contains:
/// - meta_id (24 bits): meaning depends on value_kind
/// - value_kind (8 bits): ValueKind enum
///
/// meta_id meaning depends on kind:
/// - Struct, Pointer: struct_metas[] index
/// - Array: element's meta_id
/// - Interface: interface_metas[] index
/// - Others: 0
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct GcHeader {
    pub mark: u8,
    pub gen: u8,
    pub slots: u16,
    pub value_meta: ValueMeta,
}


impl GcHeader {
    pub const SIZE: usize = 8;

    pub fn new(value_meta: ValueMeta, slots: u16) -> Self {
        Self {
            mark: GcColor::White as u8,
            gen: GcGen::Young as u8,
            slots,
            value_meta,
        }
    }

    #[inline]
    pub fn meta_id(&self) -> u32 {
        self.value_meta.meta_id()
    }

    #[inline]
    pub fn set_meta_id(&mut self, meta_id: u32) {
        self.value_meta = ValueMeta::new(meta_id, self.value_meta.value_kind());
    }

    #[inline]
    pub fn kind(&self) -> ValueKind {
        self.value_meta.value_kind()
    }

    #[inline]
    pub fn value_kind(&self) -> ValueKind {
        self.kind()
    }

    #[inline]
    pub fn value_meta(&self) -> ValueMeta {
        self.value_meta
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcColor {
    White = 0,
    Gray = 1,
    Black = 2,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcGen {
    Young = 0,
    Old = 1,
    Touched = 2,
}

/// GC reference - pointer to GcObject data (after header).
pub type GcRef = *mut u64;

/// Entry in the all_objects list.
struct GcObjectEntry {
    ptr: GcRef,
    size_bytes: usize,
}

/// Garbage collector.
pub struct Gc {
    all_objects: Vec<GcObjectEntry>,
    gray_queue: Vec<GcRef>,
    total_bytes: usize,
    threshold: usize,
}

impl Gc {
    const INITIAL_THRESHOLD: usize = 1024 * 1024;

    pub fn new() -> Self {
        Self {
            all_objects: Vec::new(),
            gray_queue: Vec::new(),
            total_bytes: 0,
            threshold: Self::INITIAL_THRESHOLD,
        }
    }

    /// Allocate a new GC object.
    pub fn alloc(&mut self, value_meta: ValueMeta, slots: u16) -> GcRef {
        self.alloc_inner(value_meta, slots, slots as usize)
    }
    
    /// Allocate a large array. For arrays with total_slots > u16::MAX,
    /// GcHeader.slots is set to 0, and the actual size is read from ArrayHeader.
    pub fn alloc_array(&mut self, value_meta: ValueMeta, total_slots: usize) -> GcRef {
        let header_slots = if total_slots > u16::MAX as usize { 0 } else { total_slots as u16 };
        self.alloc_inner(value_meta, header_slots, total_slots)
    }
    
    fn alloc_inner(&mut self, value_meta: ValueMeta, header_slots: u16, slots: usize) -> GcRef {
        let header_size = GcHeader::SIZE;
        let data_size = slots * 8;
        let total_size = header_size + data_size;

        let layout = core::alloc::Layout::from_size_align(total_size, 8).unwrap();
        let ptr = unsafe { heap_alloc::alloc_zeroed(layout) };

        if ptr.is_null() {
            panic!("GC allocation failed");
        }

        let header = GcHeader::new(value_meta, header_slots);
        unsafe {
            core::ptr::write(ptr as *mut GcHeader, header);
        }

        let data_ptr = unsafe { ptr.add(header_size) as GcRef };

        self.all_objects.push(GcObjectEntry {
            ptr: data_ptr,
            size_bytes: total_size,
        });
        self.total_bytes += total_size;

        data_ptr
    }

    /// Read a slot from a GC object.
    /// # Safety
    /// obj must be a valid GcRef and idx must be within bounds.
    #[inline]
    pub unsafe fn read_slot(obj: GcRef, idx: usize) -> u64 {
        *obj.add(idx)
    }

    /// Write a slot to a GC object.
    /// # Safety
    /// obj must be a valid GcRef and idx must be within bounds.
    #[inline]
    pub unsafe fn write_slot(obj: GcRef, idx: usize, val: u64) {
        *obj.add(idx) = val
    }

    /// Get the header of a GC object.
    #[inline]
    pub fn header(obj: GcRef) -> &'static GcHeader {
        unsafe { &*((obj as *const u8).sub(GcHeader::SIZE) as *const GcHeader) }
    }

    /// Get mutable header of a GC object.
    #[inline]
    pub fn header_mut(obj: GcRef) -> &'static mut GcHeader {
        unsafe { &mut *((obj as *mut u8).sub(GcHeader::SIZE) as *mut GcHeader) }
    }

    /// Mark an object as gray (pending scan).
    pub fn mark_gray(&mut self, obj: GcRef) {
        if obj.is_null() {
            return;
        }
        let header = Self::header_mut(obj);
        if header.mark == GcColor::White as u8 {
            header.mark = GcColor::Gray as u8;
            self.gray_queue.push(obj);
        }
    }

    /// Write barrier for incremental GC.
    pub fn write_barrier(&mut self, _parent: GcRef, child: GcRef) {
        self.mark_gray(child);
    }

    /// Check if GC should run.
    pub fn should_collect(&self) -> bool {
        self.total_bytes >= self.threshold
    }

    /// Run garbage collection.
    /// - `scan_object`: marks children of an object (mark phase)
    /// - `finalize_object`: releases native resources before dealloc (sweep phase)
    pub fn collect<S, F>(&mut self, mut scan_object: S, mut finalize_object: F)
    where
        S: FnMut(&mut Gc, GcRef),
        F: FnMut(GcRef),
    {
        while let Some(obj) = self.gray_queue.pop() {
            let header = Self::header_mut(obj);
            if header.mark == GcColor::Gray as u8 {
                header.mark = GcColor::Black as u8;
                scan_object(self, obj);
            }
        }

        let mut new_objects = Vec::new();
        let mut freed_bytes = 0;

        for entry in self.all_objects.drain(..) {
            let header = Self::header(entry.ptr);
            if header.mark == GcColor::Black as u8 {
                Self::header_mut(entry.ptr).mark = GcColor::White as u8;
                new_objects.push(entry);
            } else {
                finalize_object(entry.ptr);
                freed_bytes += entry.size_bytes;
                let raw_ptr = unsafe { (entry.ptr as *mut u8).sub(GcHeader::SIZE) };
                let layout =
                    core::alloc::Layout::from_size_align(entry.size_bytes, 8).unwrap();
                unsafe { heap_alloc::dealloc(raw_ptr, layout) };
            }
        }

        self.all_objects = new_objects;
        self.total_bytes -= freed_bytes;

        if self.total_bytes > self.threshold / 2 {
            self.threshold *= 2;
        }
    }

    pub fn total_bytes(&self) -> usize {
        self.total_bytes
    }

    pub fn object_count(&self) -> usize {
        self.all_objects.len()
    }

    /// Deep copy (clone) a heap object.
    /// Allocates new object with same value_meta and copies all slots.
    /// Used by PtrClone instruction and interface assignment (value semantics).
    /// # Safety
    /// src must be a valid GcRef or null.
    pub unsafe fn ptr_clone(&mut self, src: GcRef) -> GcRef {
        use crate::objects::array;
        
        if src.is_null() {
            return src;
        }
        let header = Self::header(src);
        let value_meta = header.value_meta;
        
        // For large arrays, slots == 0, read actual size from ArrayHeader
        let actual_slots = if header.slots == 0 {
            if value_meta.value_kind() != ValueKind::Array {
                panic!("slots == 0 but value_kind is not Array");
            }
            array::total_slots(src)
        } else {
            header.slots as usize
        };

        let dst = self.alloc_inner(value_meta, header.slots, actual_slots);

        for i in 0..actual_slots {
            let val = unsafe { Self::read_slot(src, i) };
            unsafe { Self::write_slot(dst, i, val) };
        }

        dst
    }
}

impl Default for Gc {
    fn default() -> Self {
        Self::new()
    }
}
