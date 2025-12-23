//! Garbage collector core.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::alloc as heap_alloc;
#[cfg(not(feature = "std"))]
use alloc::alloc as heap_alloc;

use vo_common_core::types::{ValueKind, META_ID_MASK};

/// GC object header - 8 bytes.
/// Layout: [mark:8 | gen:8 | slots:16 | meta_id:24 | value_kind:8]
///
/// value_kind field:
/// - bit 7 (0x80): is_array flag
///   - 1 = Array object, kind bits are element type
///   - 0 = Normal object, kind bits are object type
/// - bit 0-6: ValueKind value (0-127)
///
/// meta_id meaning depends on kind:
/// - Struct: struct_metas[] index
/// - Interface: interface_metas[] index
/// - Array of Struct/Interface: element's meta_id
/// - Others: 0
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct GcHeader {
    pub mark: u8,
    pub gen: u8,
    pub slots: u16,
    pub meta_id_low: u16,   // lower 16 bits of meta_id
    pub meta_id_high: u8,   // upper 8 bits of meta_id
    pub value_kind: u8,     // [is_array:1 | kind:7]
}

pub const IS_ARRAY_FLAG: u8 = 0x80;
pub const VALUE_KIND_MASK: u8 = 0x7F;

impl GcHeader {
    pub const SIZE: usize = 8;

    pub fn new(value_kind: u8, meta_id: u32, slots: u16) -> Self {
        Self {
            mark: GcColor::White as u8,
            gen: GcGen::Young as u8,
            slots,
            meta_id_low: (meta_id & 0xFFFF) as u16,
            meta_id_high: ((meta_id >> 16) & 0xFF) as u8,
            value_kind,
        }
    }

    #[inline]
    pub fn meta_id(&self) -> u32 {
        (self.meta_id_low as u32) | ((self.meta_id_high as u32) << 16)
    }

    #[inline]
    pub fn set_meta_id(&mut self, meta_id: u32) {
        self.meta_id_low = (meta_id & 0xFFFF) as u16;
        self.meta_id_high = ((meta_id >> 16) & 0xFF) as u8;
    }

    #[inline]
    pub fn is_array(&self) -> bool {
        (self.value_kind & IS_ARRAY_FLAG) != 0
    }

    #[inline]
    pub fn kind(&self) -> ValueKind {
        ValueKind::from_u8(self.value_kind & VALUE_KIND_MASK)
    }

    #[inline]
    pub fn value_kind(&self) -> ValueKind {
        self.kind()
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
    /// value_kind: [is_array:1 | kind:7]
    /// meta_id: 24-bit metadata index
    pub fn alloc(&mut self, value_kind: u8, meta_id: u32, slots: u16) -> GcRef {
        let header_size = GcHeader::SIZE;
        let data_size = (slots as usize) * 8;
        let total_size = header_size + data_size;

        let layout = core::alloc::Layout::from_size_align(total_size, 8).unwrap();
        let ptr = unsafe { heap_alloc::alloc_zeroed(layout) };

        if ptr.is_null() {
            panic!("GC allocation failed");
        }

        let header = GcHeader::new(value_kind, meta_id & META_ID_MASK, slots);
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
    #[inline]
    pub fn read_slot(obj: GcRef, idx: usize) -> u64 {
        unsafe { *obj.add(idx) }
    }

    /// Write a slot to a GC object.
    #[inline]
    pub fn write_slot(obj: GcRef, idx: usize, val: u64) {
        unsafe { *obj.add(idx) = val }
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
    pub fn collect<F>(&mut self, mut scan_object: F)
    where
        F: FnMut(&mut Gc, GcRef),
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
}

impl Default for Gc {
    fn default() -> Self {
        Self::new()
    }
}
