//! Garbage collector core.
#![allow(clippy::items_after_test_module)]

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use core::{cell::Cell, mem};

#[cfg(not(feature = "std"))]
use alloc::alloc as heap_alloc;
#[cfg(feature = "std")]
use std::alloc as heap_alloc;

use crate::slot::{Slot, SLOT_BYTES};
use vo_common_core::types::{ValueKind, ValueMeta};

/// GC object header - 8 bytes.
/// Layout: [marked:8 | reserved:8 | slots:16 | ValueMeta:32]
///
/// marked field bit layout (Lua style):
///   bit 0-2: age (for generational GC)
///   bit 3: WHITE0
///   bit 4: WHITE1
///   bit 5: BLACK
///   bit 6-7: reserved
///
/// ValueMeta contains:
/// - meta_id (24 bits): meaning depends on value_kind
/// - value_kind (8 bits): ValueKind enum
///
/// meta_id meaning depends on kind:
/// - Struct: struct_metas[] index (for field layout / GC scan)
/// - Pointer: struct_metas[] index of the *pointee* struct (for PtrNew-created
///   objects that hold the full struct data). For heap-boxed pointer variables
///   (1-slot GcRef container), meta_id = 0 (ref box).
/// - Array: element's ValueMeta (elem_kind + elem_meta_id)
/// - Interface: interface_metas[] index
/// - Others: 0
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct GcHeader {
    pub marked: u8,
    pub _reserved: u8,
    pub slots: u16,
    pub value_meta: ValueMeta,
}

// Marked field bit positions
const AGE_MASK: u8 = 0x07; // bits 0-2
const WHITE0_BIT: u8 = 1 << 3; // bit 3
const WHITE1_BIT: u8 = 1 << 4; // bit 4
const BLACK_BIT: u8 = 1 << 5; // bit 5
const WHITE_BITS: u8 = WHITE0_BIT | WHITE1_BIT;

// Age values (for generational GC)
pub const G_YOUNG: u8 = 0;
pub const G_SURVIVAL: u8 = 1;
pub const G_OLD: u8 = 2;
pub const G_TOUCHED: u8 = 3;

/// GC state machine states.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcState {
    Pause = 0,     // Idle, waiting for trigger
    Propagate = 1, // Incremental marking (interruptible)
    Atomic = 2,    // Atomic marking (not interruptible)
    Sweep = 3,     // Sweeping dead objects
}

/// Caller-provided root-set freshness for one incremental GC step.
///
/// `Gc::step` always uses `MayHaveChanged`, which is the conservative and safe
/// default. `StableSinceLastScan` may only be used when the caller can prove
/// that no root slot has changed since the previous root scan performed by this
/// GC instance. Heap write barriers and new allocations are still processed
/// through the gray queues; this flag controls only whether sweep must rescan
/// all roots before freeing the next chunk.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcRootState {
    MayHaveChanged = 0,
    StableSinceLastScan = 1,
}

/// Root scan pass currently requested by the collector.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GcRootScanKind {
    StartCycle = 0,
    Atomic = 1,
    Sweep = 2,
}

/// Result of one bounded root scan chunk.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcRootScanChunk {
    pub done: bool,
    pub work_bytes: usize,
}

impl GcRootScanChunk {
    #[inline]
    pub fn complete(work_bytes: usize) -> Self {
        Self {
            done: true,
            work_bytes,
        }
    }

    #[inline]
    pub fn pending(work_bytes: usize) -> Self {
        Self {
            done: false,
            work_bytes,
        }
    }
}

/// Platform-independent telemetry for the most recent incremental GC step.
///
/// Durations are intentionally not recorded here because `vo-runtime` is
/// no_std-capable. Hosts such as the VM, Studio, or the perf harness should
/// measure wall-clock time around `Gc::step`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcStepStats {
    pub phase_before: GcState,
    pub phase_after: GcState,
    pub root_state: GcRootState,
    pub root_scan_calls: usize,
    pub root_scan_skips: usize,
    pub root_scan_work_bytes: usize,
    pub object_scans: usize,
    pub finalized_objects: usize,
    pub sweep_freed_bytes: usize,
    pub propagate_work_bytes: usize,
    pub sweep_work_bytes: usize,
    pub total_work_bytes: usize,
    pub heap_bytes_before: usize,
    pub heap_bytes_after: usize,
    pub debt_before: i64,
    pub debt_after: i64,
    pub gray_len_before: usize,
    pub gray_len_after: usize,
    pub grayagain_len_before: usize,
    pub grayagain_len_after: usize,
    pub cycle_started: bool,
    pub cycle_finished: bool,
}

impl Default for GcStepStats {
    fn default() -> Self {
        Self {
            phase_before: GcState::Pause,
            phase_after: GcState::Pause,
            root_state: GcRootState::MayHaveChanged,
            root_scan_calls: 0,
            root_scan_skips: 0,
            root_scan_work_bytes: 0,
            object_scans: 0,
            finalized_objects: 0,
            sweep_freed_bytes: 0,
            propagate_work_bytes: 0,
            sweep_work_bytes: 0,
            total_work_bytes: 0,
            heap_bytes_before: 0,
            heap_bytes_after: 0,
            debt_before: 0,
            debt_after: 0,
            gray_len_before: 0,
            gray_len_after: 0,
            grayagain_len_before: 0,
            grayagain_len_after: 0,
            cycle_started: false,
            cycle_finished: false,
        }
    }
}

impl GcHeader {
    pub const SIZE: usize = SLOT_BYTES;

    pub fn new(value_meta: ValueMeta, slots: u16) -> Self {
        Self::new_with_white(value_meta, slots, WHITE0_BIT)
    }

    pub fn new_with_white(value_meta: ValueMeta, slots: u16, white_bit: u8) -> Self {
        Self {
            marked: white_bit | G_YOUNG,
            _reserved: 0,
            slots,
            value_meta,
        }
    }

    // ========== Color methods ==========

    #[inline]
    pub fn is_white(&self) -> bool {
        (self.marked & WHITE_BITS) != 0
    }

    #[inline]
    pub fn is_black(&self) -> bool {
        (self.marked & BLACK_BIT) != 0
    }

    #[inline]
    pub fn is_gray(&self) -> bool {
        !self.is_white() && !self.is_black()
    }

    #[inline]
    pub fn set_black(&mut self) {
        self.marked = (self.marked & !(WHITE_BITS)) | BLACK_BIT;
    }

    #[inline]
    pub fn set_white(&mut self, current_white: u8) {
        self.marked = (self.marked & !(WHITE_BITS | BLACK_BIT)) | current_white;
    }

    #[inline]
    pub fn set_gray(&mut self) {
        self.marked &= !(WHITE_BITS | BLACK_BIT);
    }

    // ========== Age methods ==========

    #[inline]
    pub fn age(&self) -> u8 {
        self.marked & AGE_MASK
    }

    #[inline]
    pub fn set_age(&mut self, age: u8) {
        self.marked = (self.marked & !AGE_MASK) | (age & AGE_MASK);
    }

    // ========== ValueMeta methods ==========

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
    pub fn value_meta(&self) -> ValueMeta {
        self.value_meta
    }
}

/// GC reference - pointer to GcObject data (after header).
pub type GcRef = *mut Slot;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct RangeEntry {
    page: usize,
    base: usize,
    next: usize,
}

/// Garbage collector.
#[repr(C)]
pub struct Gc {
    // ========== Object Storage ==========
    all_objects: Vec<GcRef>,
    base_index: Vec<usize>,
    base_index_items: Cell<usize>,
    base_index_tombstones: Cell<usize>,
    range_buckets: Vec<usize>,
    range_entries: Vec<RangeEntry>,
    range_tombstones: Cell<usize>,
    object_index: core::cell::RefCell<Vec<GcRef>>,
    object_index_dirty: Cell<bool>,

    // ========== Mark Queues ==========
    gray: Vec<GcRef>,      // To be scanned
    grayagain: Vec<GcRef>, // Re-scan (barrier triggered)

    // ========== State ==========
    state: GcState,
    current_white: u8,      // Current white bit (WHITE0_BIT or WHITE1_BIT)
    sweep_pos: usize,       // Read position in sweep phase
    sweep_write_pos: usize, // Write position for live objects in sweep phase

    // ========== Memory Stats ==========
    total_bytes: usize, // Total allocated bytes
    estimate: usize,    // Estimated live bytes after last GC
    debt: i64,          // Work debt (triggers GC when > 0)

    // ========== Parameters ==========
    pause: u16,      // Pause multiplier (default 200 = 2x)
    stepmul: u16,    // Step multiplier (default 100)
    stepsize: usize, // Bytes per step (default 8KB)

    // ========== Phase Budget ==========
    /// Fixed per-step budget for sweep phase, snapshotted at sweep start.
    /// Using a constant prevents convergence issues (total_bytes shrinks as dead
    /// objects are freed, causing dynamically-computed limits to shrink too).
    sweep_budget: usize,
    /// In-progress root scan for callers that provide a bounded root scanner.
    pending_root_scan: Option<GcRootScanKind>,

    // ========== Diagnostics ==========
    /// Stress mode for GC correctness testing. When enabled, every scheduler
    /// boundary runs a GC step even when there is no allocation debt, forcing
    /// mark/sweep interleavings that are otherwise rare.
    stress_every_step: bool,
    last_step_stats: GcStepStats,
}

impl Gc {
    const BASE_INDEX_EMPTY: usize = 0;
    const BASE_INDEX_DELETED: usize = usize::MAX;
    const RANGE_INDEX_EMPTY: usize = 0;
    const RANGE_PAGE_SHIFT: usize = 12;
    const NEARBY_BASE_SCAN_SLOTS: usize = 64;
    #[cfg(target_pointer_width = "64")]
    const HASH_MULT: usize = 0x9e37_79b9_7f4a_7c15;
    #[cfg(target_pointer_width = "32")]
    const HASH_MULT: usize = 0x9e37_79b9;

    // Default parameters
    const DEFAULT_PAUSE: u16 = 200; // Trigger at 2x estimated live size
    const DEFAULT_STEPMUL: u16 = 100; // Work multiplier
    const DEFAULT_STEPSIZE: usize = 8192; // 8KB per step

    pub fn new() -> Self {
        Self {
            all_objects: Vec::new(),
            base_index: Vec::new(),
            base_index_items: Cell::new(0),
            base_index_tombstones: Cell::new(0),
            range_buckets: Vec::new(),
            range_entries: Vec::new(),
            range_tombstones: Cell::new(0),
            object_index: core::cell::RefCell::new(Vec::new()),
            object_index_dirty: Cell::new(false),
            gray: Vec::new(),
            grayagain: Vec::new(),
            state: GcState::Pause,
            current_white: WHITE0_BIT,
            sweep_pos: 0,
            sweep_write_pos: 0,
            total_bytes: 0,
            estimate: 0,
            debt: 0,
            pause: Self::DEFAULT_PAUSE,
            stepmul: Self::DEFAULT_STEPMUL,
            stepsize: Self::DEFAULT_STEPSIZE,
            sweep_budget: 0,
            pending_root_scan: None,
            stress_every_step: false,
            last_step_stats: GcStepStats::default(),
        }
    }

    /// Get current GC state.
    #[inline]
    pub fn state(&self) -> GcState {
        self.state
    }

    /// Get current white bit for new allocations.
    #[inline]
    pub fn current_white(&self) -> u8 {
        self.current_white
    }

    /// Telemetry for the most recent incremental GC step.
    #[inline]
    pub fn last_step_stats(&self) -> GcStepStats {
        self.last_step_stats
    }

    /// Get the "other" white bit (for checking dead objects).
    #[inline]
    fn other_white(&self) -> u8 {
        self.current_white ^ WHITE_BITS
    }

    /// Check if object is dead (has the "other" white color).
    #[inline]
    #[allow(dead_code)]
    fn is_dead(&self, obj: GcRef) -> bool {
        let header = Self::header(obj);
        (header.marked & WHITE_BITS) == self.other_white()
    }

    /// Allocate a new GC object.
    pub fn alloc(&mut self, value_meta: ValueMeta, slots: u16) -> GcRef {
        self.alloc_inner(value_meta, slots, slots as usize)
    }

    /// Allocate a large array. For arrays with total_slots > u16::MAX,
    /// GcHeader.slots is set to 0, and the actual size is read from ArrayHeader.
    pub fn alloc_array(&mut self, value_meta: ValueMeta, total_slots: usize) -> GcRef {
        let header_slots = if total_slots > u16::MAX as usize {
            0
        } else {
            total_slots as u16
        };
        self.alloc_inner(value_meta, header_slots, total_slots)
    }

    fn alloc_inner(&mut self, value_meta: ValueMeta, header_slots: u16, slots: usize) -> GcRef {
        let header_size = GcHeader::SIZE;
        let data_size = match slots.checked_mul(SLOT_BYTES) {
            Some(s) => s,
            None => {
                // Overflow - allocation too large
                #[cfg(feature = "std")]
                eprintln!("GC allocation overflow: slots={}", slots);
                return core::ptr::null_mut();
            }
        };
        let total_size = match header_size.checked_add(data_size) {
            Some(s) => s,
            None => {
                #[cfg(feature = "std")]
                eprintln!(
                    "GC allocation overflow: header_size={}, data_size={}",
                    header_size, data_size
                );
                return core::ptr::null_mut();
            }
        };

        let layout = match core::alloc::Layout::from_size_align(total_size, SLOT_BYTES) {
            Ok(l) => l,
            Err(_) => {
                #[cfg(feature = "std")]
                eprintln!(
                    "GC allocation layout error: total_size={}, align={}",
                    total_size, SLOT_BYTES
                );
                return core::ptr::null_mut();
            }
        };
        let ptr = unsafe { heap_alloc::alloc_zeroed(layout) };

        if ptr.is_null() {
            #[cfg(feature = "std")]
            eprintln!("GC allocation failed: out of memory");
            return core::ptr::null_mut();
        }

        // New object gets current white color. During marking, queue it gray so
        // its initialized slots are scanned before the cycle reaches sweep.
        let header = GcHeader::new_with_white(value_meta, header_slots, self.current_white);
        unsafe {
            core::ptr::write(ptr as *mut GcHeader, header);
        }

        let data_ptr = unsafe { ptr.add(header_size) as GcRef };

        self.all_objects.push(data_ptr);
        self.insert_base_index(data_ptr);
        self.insert_range_index(data_ptr, data_size);
        self.object_index_dirty.set(true);
        self.total_bytes += total_size;
        self.debt += total_size as i64;
        if matches!(self.state, GcState::Propagate | GcState::Atomic) {
            Self::header_mut(data_ptr).set_gray();
            self.gray.push(data_ptr);
        }

        #[cfg(feature = "gc-debug")]
        crate::gc_debug::on_alloc(data_ptr);

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

    /// Compute object size in bytes from header.
    /// For large arrays (slots == 0), reads actual size from ArrayHeader.
    #[inline]
    fn object_size_bytes(obj: GcRef) -> usize {
        use crate::objects::array;
        let header = Self::header(obj);
        let slots = if header.slots == 0 && header.kind() == ValueKind::Array {
            array::total_slots(obj)
        } else {
            header.slots as usize
        };
        GcHeader::SIZE + slots * SLOT_BYTES
    }

    #[inline]
    fn object_data_size_bytes(obj: GcRef) -> usize {
        Self::object_size_bytes(obj) - GcHeader::SIZE
    }

    #[cfg(debug_assertions)]
    fn refresh_object_index(&self) {
        if !self.object_index_dirty.get() {
            return;
        }

        let mut index = self.object_index.borrow_mut();
        index.clear();
        index.extend(
            self.all_objects
                .iter()
                .copied()
                .filter(|obj| !obj.is_null()),
        );
        index.sort_unstable_by_key(|&obj| obj as usize);
        index.dedup();
        self.object_index_dirty.set(false);
    }

    #[inline]
    fn hash_base_addr(addr: usize) -> usize {
        (addr >> 3).wrapping_mul(Self::HASH_MULT)
    }

    fn resize_base_index(&mut self, min_items: usize) {
        let mut new_len = 16usize;
        while new_len.saturating_mul(3) / 4 < min_items.max(1) {
            new_len = new_len.saturating_mul(2);
        }

        let old = mem::take(&mut self.base_index);
        self.base_index.resize(new_len, Self::BASE_INDEX_EMPTY);

        let mut items = 0usize;
        for addr in old {
            if addr != Self::BASE_INDEX_EMPTY && addr != Self::BASE_INDEX_DELETED {
                Self::insert_base_index_raw(&mut self.base_index, addr);
                items += 1;
            }
        }
        self.base_index_items.set(items);
        self.base_index_tombstones.set(0);
    }

    fn insert_base_index_raw(table: &mut [usize], addr: usize) {
        debug_assert!(addr != Self::BASE_INDEX_EMPTY && addr != Self::BASE_INDEX_DELETED);
        let mask = table.len() - 1;
        let mut idx = Self::hash_base_addr(addr) & mask;
        loop {
            match table[idx] {
                Self::BASE_INDEX_EMPTY | Self::BASE_INDEX_DELETED => {
                    table[idx] = addr;
                    return;
                }
                existing if existing == addr => return,
                _ => idx = (idx + 1) & mask,
            }
        }
    }

    fn insert_base_index(&mut self, obj: GcRef) {
        let addr = obj as usize;
        let len = self.base_index.len();
        let used = self.base_index_items.get() + self.base_index_tombstones.get() + 1;
        if len == 0 || used.saturating_mul(4) >= len.saturating_mul(3) {
            self.resize_base_index(self.base_index_items.get() + 1);
        }

        let table = &mut self.base_index;
        let mask = table.len() - 1;
        let mut idx = Self::hash_base_addr(addr) & mask;
        let mut first_deleted = None;
        loop {
            match table[idx] {
                Self::BASE_INDEX_EMPTY => {
                    let insert_at = first_deleted.unwrap_or(idx);
                    if first_deleted.is_some() {
                        self.base_index_tombstones
                            .set(self.base_index_tombstones.get().saturating_sub(1));
                    }
                    table[insert_at] = addr;
                    self.base_index_items.set(self.base_index_items.get() + 1);
                    return;
                }
                Self::BASE_INDEX_DELETED => {
                    first_deleted.get_or_insert(idx);
                }
                existing if existing == addr => return,
                _ => {}
            }
            idx = (idx + 1) & mask;
        }
    }

    fn remove_base_index(&mut self, obj: GcRef) {
        let addr = obj as usize;
        let table = &mut self.base_index;
        if table.is_empty() {
            return;
        }

        let mask = table.len() - 1;
        let mut idx = Self::hash_base_addr(addr) & mask;
        loop {
            match table[idx] {
                Self::BASE_INDEX_EMPTY => return,
                existing if existing == addr => {
                    table[idx] = Self::BASE_INDEX_DELETED;
                    self.base_index_items
                        .set(self.base_index_items.get().saturating_sub(1));
                    self.base_index_tombstones
                        .set(self.base_index_tombstones.get() + 1);
                    break;
                }
                _ => idx = (idx + 1) & mask,
            }
        }

        if self.base_index_tombstones.get() > self.base_index_items.get()
            && self.base_index.len() > 16
        {
            self.resize_base_index(self.base_index_items.get());
        }
    }

    fn base_index_contains(&self, addr: usize) -> bool {
        let table = &self.base_index;
        if table.is_empty() {
            return false;
        }

        let mask = table.len() - 1;
        let mut idx = Self::hash_base_addr(addr) & mask;
        loop {
            match table[idx] {
                Self::BASE_INDEX_EMPTY => return false,
                existing if existing == addr => return true,
                _ => idx = (idx + 1) & mask,
            }
        }
    }

    #[inline]
    fn object_page(addr: usize) -> usize {
        addr >> Self::RANGE_PAGE_SHIFT
    }

    #[inline]
    fn hash_page(page: usize) -> usize {
        page.wrapping_mul(Self::HASH_MULT)
    }

    fn insert_range_entry_raw(
        buckets: &mut [usize],
        entries: &mut Vec<RangeEntry>,
        page: usize,
        base: usize,
    ) {
        let bucket = Self::hash_page(page) & (buckets.len() - 1);
        let next = buckets[bucket];
        entries.push(RangeEntry { page, base, next });
        buckets[bucket] = entries.len();
    }

    fn resize_range_index(&mut self, min_entries: usize) {
        let mut new_len = 16usize;
        while new_len < min_entries.max(1).saturating_mul(2) {
            new_len = new_len.saturating_mul(2);
        }

        let old_entries = mem::take(&mut self.range_entries);
        let mut new_buckets = Vec::new();
        new_buckets.resize(new_len, Self::RANGE_INDEX_EMPTY);
        let mut new_entries = Vec::new();

        for entry in old_entries {
            if entry.base != 0 && self.base_index_contains(entry.base) {
                Self::insert_range_entry_raw(
                    &mut new_buckets,
                    &mut new_entries,
                    entry.page,
                    entry.base,
                );
            }
        }

        self.range_buckets = new_buckets;
        self.range_entries = new_entries;
        self.range_tombstones.set(0);
    }

    fn insert_range_index(&mut self, obj: GcRef, data_size: usize) {
        if data_size <= SLOT_BYTES {
            return;
        }

        let base = obj as usize;
        let start_page = Self::object_page(base);
        let end_page = Self::object_page(base + data_size - 1);
        let page_count = end_page - start_page + 1;

        let bucket_len = self.range_buckets.len();
        let entry_count = self.range_entries.len();
        if bucket_len == 0 || (entry_count + page_count).saturating_mul(2) >= bucket_len {
            self.resize_range_index(entry_count + page_count);
        }

        for page in start_page..=end_page {
            Self::insert_range_entry_raw(
                &mut self.range_buckets,
                &mut self.range_entries,
                page,
                base,
            );
        }
    }

    fn remove_range_index(&mut self, obj: GcRef) {
        let data_size = Self::object_data_size_bytes(obj);
        if data_size <= SLOT_BYTES {
            return;
        }

        let base = obj as usize;
        let start_page = Self::object_page(base);
        let end_page = Self::object_page(base + data_size - 1);
        let mut removed = 0usize;

        {
            let buckets = &mut self.range_buckets;
            let entries = &mut self.range_entries;
            if buckets.is_empty() {
                return;
            }

            for page in start_page..=end_page {
                let bucket = Self::hash_page(page) & (buckets.len() - 1);
                let mut link = buckets[bucket];
                while link != Self::RANGE_INDEX_EMPTY {
                    let entry = &mut entries[link - 1];
                    if entry.page == page && entry.base == base {
                        entry.base = 0;
                        removed += 1;
                    }
                    link = entry.next;
                }
            }
        }

        if removed == 0 {
            return;
        }
        self.range_tombstones
            .set(self.range_tombstones.get() + removed);

        let entry_count = self.range_entries.len();
        if entry_count > 32 && self.range_tombstones.get().saturating_mul(2) > entry_count {
            self.resize_range_index(entry_count - self.range_tombstones.get());
        }
    }

    fn range_index_lookup(&self, addr: usize) -> Option<GcRef> {
        let buckets = &self.range_buckets;
        if buckets.is_empty() {
            return None;
        }
        let entries = &self.range_entries;
        let page = Self::object_page(addr);
        let bucket = Self::hash_page(page) & (buckets.len() - 1);
        let mut link = buckets[bucket];
        while link != Self::RANGE_INDEX_EMPTY {
            let entry = entries[link - 1];
            if entry.page == page && entry.base != 0 {
                let base = entry.base as GcRef;
                let base_addr = entry.base;
                let data_end = base_addr + Self::object_data_size_bytes(base);
                if addr >= base_addr && addr < data_end {
                    return Some(base);
                }
            }
            link = entry.next;
        }
        None
    }

    fn nearby_base_lookup(&self, addr: usize) -> Option<GcRef> {
        let lower_bound =
            addr.saturating_sub(Self::NEARBY_BASE_SCAN_SLOTS.saturating_mul(SLOT_BYTES));
        let mut candidate = addr.saturating_sub(SLOT_BYTES);

        while candidate >= lower_bound {
            if self.base_index_contains(candidate) {
                let base = candidate as GcRef;
                let data_end = candidate + Self::object_data_size_bytes(base);
                return (addr < data_end).then_some(base);
            }
            if candidate < SLOT_BYTES {
                break;
            }
            candidate -= SLOT_BYTES;
        }
        None
    }

    pub fn canonicalize_ref(&self, obj: GcRef) -> Option<GcRef> {
        if obj.is_null() {
            return Some(obj);
        }

        let addr = obj as usize;
        if (addr & (SLOT_BYTES - 1)) != 0 || addr < 4096 {
            return None;
        }

        if self.base_index_contains(addr) {
            return Some(obj);
        }

        if let Some(base) = self.nearby_base_lookup(addr) {
            return Some(base);
        }

        self.range_index_lookup(addr)
    }

    #[cfg(debug_assertions)]
    pub fn debug_ref_membership(&self, obj: GcRef) -> (bool, bool, usize) {
        let in_all_objects = self.base_index_contains(obj as usize);
        self.refresh_object_index();
        let index = self.object_index.borrow();
        let in_object_index = index
            .binary_search_by_key(&(obj as usize), |&candidate| candidate as usize)
            .is_ok();
        (in_all_objects, in_object_index, index.len())
    }

    /// Mark an object as gray (pending scan).
    #[inline]
    pub fn mark_gray(&mut self, obj: GcRef) {
        if obj.is_null() {
            return;
        }
        let Some(obj) = self.canonicalize_ref(obj) else {
            self.mark_gray_fail(obj);
        };
        if self.state == GcState::Sweep {
            self.mark_dead_white_gray(obj);
            return;
        }
        let header = Self::header_mut(obj);
        if header.is_white() {
            header.set_gray();
            self.gray.push(obj);
        }
    }

    #[inline]
    fn mark_dead_white_gray(&mut self, obj: GcRef) {
        let dead_white = self.other_white();
        let header = Self::header_mut(obj);
        if header.marked & WHITE_BITS == dead_white {
            header.set_gray();
            self.gray.push(obj);
        }
    }

    /// Queue a fully-initialized object allocated during sweep for scanning.
    ///
    /// Sweep treats current-white objects as live, but a newly allocated wrapper
    /// can contain copied references to old-white objects. Those children must be
    /// traced before sweep can free them. Call this only after the object's slots
    /// have been initialized.
    #[inline]
    #[track_caller]
    pub fn mark_allocated_for_scan(&mut self, obj: GcRef) {
        if self.state != GcState::Sweep || obj.is_null() {
            return;
        }
        let Some(obj) = self.canonicalize_ref(obj) else {
            self.mark_gray_fail(obj);
        };
        let header = Self::header_mut(obj);
        if header.marked & WHITE_BITS == self.current_white {
            header.set_gray();
            self.gray.push(obj);
        }
    }

    #[cold]
    #[track_caller]
    #[inline(never)]
    fn mark_gray_fail(&self, obj: GcRef) -> ! {
        let loc = core::panic::Location::caller();
        panic!(
            "mark_gray: invalid GcRef {:p} (raw={:#x}) — non-GcRef value in GcRef-typed slot caller={}:{}",
            obj,
            obj as usize,
            loc.file(),
            loc.line(),
        );
    }

    /// Write barrier for incremental GC (backward barrier).
    /// Called when a black object writes a white reference.
    #[track_caller]
    pub fn write_barrier(&mut self, parent: GcRef, child: GcRef) {
        #[cfg(feature = "gc-debug")]
        crate::gc_debug::on_barrier(parent, 0, child as u64);

        if !matches!(self.state, GcState::Propagate | GcState::Sweep) {
            return;
        }
        if parent.is_null() || child.is_null() {
            return;
        }
        let Some(parent) = self.canonicalize_ref(parent) else {
            self.write_barrier_parent_fail(parent, child);
        };
        let Some(child) = self.canonicalize_ref(child) else {
            return;
        };
        match self.state {
            GcState::Propagate => {
                let p_header = Self::header(parent);
                let c_header = Self::header(child);
                // Backward barrier: black parent writes white child -> parent becomes gray.
                if p_header.is_black() && c_header.is_white() {
                    self.barrier_back(parent);
                }
            }
            GcState::Sweep => {
                // During sweep an old-white object may become reachable again
                // through a root or heap write. It must be rescanned, not just
                // blackened, because composite values such as strings/slices
                // own backing arrays that would otherwise still be swept.
                self.mark_dead_white_gray(parent);
                self.mark_dead_white_gray(child);
            }
            GcState::Pause | GcState::Atomic => {}
        }
    }

    #[cold]
    #[track_caller]
    #[inline(never)]
    fn write_barrier_parent_fail(&self, parent: GcRef, child: GcRef) -> ! {
        let loc = core::panic::Location::caller();
        panic!(
            "write_barrier: invalid parent {:p} (raw={:#x}) child={:p} child_raw={:#x} state={:?} caller={}:{}",
            parent,
            parent as usize,
            child,
            child as usize,
            self.state,
            loc.file(),
            loc.line(),
        );
    }

    /// Check if object is black (for gc-debug)
    #[inline]
    pub fn is_black(&self, obj: GcRef) -> bool {
        if obj.is_null() {
            return false;
        }
        self.canonicalize_ref(obj)
            .map(|base| !base.is_null() && Self::header(base).is_black())
            .unwrap_or(false)
    }

    /// Check if object is white (for gc-debug)
    #[inline]
    pub fn is_white(&self, obj: GcRef) -> bool {
        if obj.is_null() {
            return false;
        }
        self.canonicalize_ref(obj)
            .map(|base| !base.is_null() && Self::header(base).is_white())
            .unwrap_or(false)
    }

    /// Backward barrier: turn black object back to gray for re-scan.
    fn barrier_back(&mut self, obj: GcRef) {
        let header = Self::header_mut(obj);
        header.set_white(self.current_white);
        self.grayagain.push(obj);
    }

    /// Enable or disable GC stress mode.
    #[inline]
    pub fn set_stress_every_step(&mut self, enabled: bool) {
        self.stress_every_step = enabled;
    }

    /// Returns whether GC stress mode is enabled.
    #[inline]
    pub fn stress_every_step(&self) -> bool {
        self.stress_every_step
    }

    /// Check if GC should run.
    ///
    /// Debt starts a new cycle. Once a cycle has started, keep advancing it at
    /// every scheduler boundary so mark/sweep work is amortized across frames
    /// instead of bunching up behind future allocations. Stress mode forces a
    /// step at every scheduler boundary to expose write-barrier bugs.
    #[inline]
    pub fn should_step(&self) -> bool {
        self.stress_every_step || self.debt > 0 || self.state != GcState::Pause
    }

    /// Incremental GC step. Returns work done (bytes processed).
    /// Call this when should_step() is true, passing scan_roots and scan_object callbacks.
    pub fn step<R, S, F>(&mut self, scan_roots: R, scan_object: S, finalize_object: F) -> usize
    where
        R: FnMut(&mut Gc),
        S: FnMut(&mut Gc, GcRef),
        F: FnMut(GcRef),
    {
        self.step_with_root_state(
            GcRootState::MayHaveChanged,
            scan_roots,
            scan_object,
            finalize_object,
        )
    }

    /// Incremental GC step with an explicit root-set freshness contract.
    ///
    /// The safe default is `GcRootState::MayHaveChanged`. Passing
    /// `StableSinceLastScan` is correct only when the caller controls all roots
    /// and can prove none changed since this collector last ran `scan_roots`.
    pub fn step_with_root_state<R, S, F>(
        &mut self,
        root_state: GcRootState,
        mut scan_roots: R,
        mut scan_object: S,
        mut finalize_object: F,
    ) -> usize
    where
        R: FnMut(&mut Gc),
        S: FnMut(&mut Gc, GcRef),
        F: FnMut(GcRef),
    {
        self.step_with_root_scanner(
            root_state,
            |gc, _kind, _limit| {
                scan_roots(gc);
                GcRootScanChunk::complete(0)
            },
            &mut scan_object,
            &mut finalize_object,
        )
    }

    /// Incremental GC step with a bounded root scanner.
    ///
    /// This API is for hosts with very large root sets. The scanner may process
    /// up to `limit_bytes` worth of root work and return `pending`; the collector
    /// will resume the same `GcRootScanKind` on the next step.
    ///
    /// Correctness contract: while a root scan pass is pending, the caller must
    /// either keep the scanned root set stable or use its own dirty/restart
    /// protocol so roots changed behind the cursor are not lost. `Gc::step` and
    /// `step_with_root_state` remain the conservative default for callers that
    /// cannot provide that proof.
    pub fn step_with_root_scanner<R, S, F>(
        &mut self,
        root_state: GcRootState,
        mut scan_roots: R,
        mut scan_object: S,
        mut finalize_object: F,
    ) -> usize
    where
        R: FnMut(&mut Gc, GcRootScanKind, usize) -> GcRootScanChunk,
        S: FnMut(&mut Gc, GcRef),
        F: FnMut(GcRef),
    {
        let mut work = 0usize;
        let base = self.stepsize * self.stepmul as usize / 100;
        let mut stats = GcStepStats {
            phase_before: self.state,
            root_state,
            heap_bytes_before: self.total_bytes,
            debt_before: self.debt,
            gray_len_before: self.gray.len(),
            grayagain_len_before: self.grayagain.len(),
            ..GcStepStats::default()
        };

        // work_limit: allocation-proportional budget for DEBT TRACKING only.
        // This controls how much debt is repaid per step; it does NOT control
        // how much work a step actually does (that's phase_limit's job).
        let work_limit = base.max(self.debt.max(0) as usize);

        // Target frame count for each GC phase (Propagate/Sweep).
        //
        // Phase acceleration is needed because allocation-proportional debt alone
        // can make large mostly-live heaps take thousands of frames to finish a
        // cycle. For interactive runtimes, though, heap / TARGET_PHASE_FRAMES can mean large
        // of scan/sweep work at one scheduler boundary, which is a visible frame
        // hitch. Keep the phase accelerator, but cap one incremental slice so a
        // single GC step cannot consume a whole frame budget.
        const TARGET_PHASE_FRAMES: usize = 128;
        const MAX_PHASE_STEP_BYTES: usize = 8 * 1024;

        let mut completed_atomic_root_scan = false;
        let mut completed_sweep_root_scan = false;
        loop {
            let phase_limit = (self.total_bytes / TARGET_PHASE_FRAMES)
                .max(base)
                .min(MAX_PHASE_STEP_BYTES);

            if let Some(kind) = self.pending_root_scan {
                let limit = phase_limit.saturating_sub(work).max(SLOT_BYTES);
                stats.root_scan_calls += 1;
                let chunk = scan_roots(self, kind, limit);
                debug_assert!(
                    chunk.done || chunk.work_bytes > 0,
                    "bounded GC root scanner returned pending without progress"
                );
                stats.root_scan_work_bytes += chunk.work_bytes;
                work += chunk.work_bytes;

                if !chunk.done {
                    break;
                }

                self.pending_root_scan = None;
                if kind == GcRootScanKind::Atomic {
                    completed_atomic_root_scan = true;
                }
                if kind == GcRootScanKind::Sweep {
                    completed_sweep_root_scan = true;
                }
                if kind == GcRootScanKind::StartCycle && work >= phase_limit {
                    break;
                }
            }

            match self.state {
                GcState::Pause => {
                    // Start new cycle
                    stats.cycle_started = true;
                    self.current_white ^= WHITE_BITS;
                    self.pending_root_scan = Some(GcRootScanKind::StartCycle);
                    self.state = GcState::Propagate;
                }

                GcState::Propagate => {
                    // phase_limit for Propagate: total_bytes / TARGET.
                    // total_bytes only increases during Propagate (new allocs, no frees),
                    // so this is stable/increasing across steps — no convergence issue.
                    // Use .max(base) NOT .max(work_limit) to avoid first-cycle spikes
                    // where debt = total_bytes would make phase_limit = entire heap.
                    let propagate_work = {
                        let mut counted_scan_object = |gc: &mut Gc, obj: GcRef| {
                            stats.object_scans += 1;
                            scan_object(gc, obj);
                        };
                        self.propagate_step(
                            &mut counted_scan_object,
                            phase_limit.saturating_sub(work),
                        )
                    };
                    stats.propagate_work_bytes += propagate_work;
                    work += propagate_work;

                    if self.gray.is_empty() {
                        self.state = GcState::Atomic;
                        break;
                    } else if work >= phase_limit {
                        break;
                    }
                }

                GcState::Atomic => {
                    // Roots are mutable during incremental marking. A stack slot or global
                    // can start pointing at an old-white object after start_cycle() has
                    // already scanned roots, so rescan roots at the atomic boundary before
                    // finalizing the mark set.
                    if !completed_atomic_root_scan {
                        self.pending_root_scan = Some(GcRootScanKind::Atomic);
                        continue;
                    }
                    {
                        let mut counted_scan_object = |gc: &mut Gc, obj: GcRef| {
                            stats.object_scans += 1;
                            scan_object(gc, obj);
                        };
                        self.atomic_phase(&mut counted_scan_object);
                    }
                    self.state = GcState::Sweep;
                    self.sweep_pos = 0;
                    self.sweep_write_pos = 0;
                    // Snapshot sweep budget at sweep start. total_bytes here includes
                    // all objects (alive + dead). Using a fixed budget prevents the
                    // convergence problem: if we recomputed total_bytes/TARGET each step,
                    // freed dead bytes would shrink total_bytes, shrinking the budget,
                    // causing exponential decay instead of linear progress (99% dead heap
                    // would need ~130 steps instead of 32).
                    self.sweep_budget = (self.total_bytes / TARGET_PHASE_FRAMES)
                        .max(base)
                        .min(MAX_PHASE_STEP_BYTES);
                    break;
                }

                GcState::Sweep => {
                    // Mutator roots can change while sweep is incremental. Rescue
                    // any newly reachable old-white graph before sweeping the next
                    // chunk; mark_gray() is sweep-aware and ignores current-white
                    // objects that have already survived this cycle.
                    if root_state == GcRootState::MayHaveChanged {
                        if !completed_sweep_root_scan {
                            self.pending_root_scan = Some(GcRootScanKind::Sweep);
                            continue;
                        }
                    } else if !completed_sweep_root_scan {
                        stats.root_scan_skips += 1;
                    }
                    {
                        let mut counted_scan_object = |gc: &mut Gc, obj: GcRef| {
                            stats.object_scans += 1;
                            scan_object(gc, obj);
                        };
                        self.atomic_phase(&mut counted_scan_object);
                    }
                    let sweep_work = self.sweep_step_counted(
                        &mut finalize_object,
                        self.sweep_budget.saturating_sub(work),
                        &mut stats,
                    );
                    stats.sweep_work_bytes += sweep_work;
                    work += sweep_work;

                    if self.sweep_pos >= self.all_objects.len() {
                        self.finish_cycle();
                        stats.cycle_finished = true;
                        break;
                    } else if work >= self.sweep_budget {
                        break;
                    }
                }
            }
        }

        // Debt tracks allocation-proportional work. Phase-accelerated work (done to
        // finish within TARGET_PHASE_FRAMES) may far exceed the allocation budget;
        // crediting all of it would make debt hugely negative and delay the next cycle.
        self.debt -= (work as i64).min(work_limit as i64);
        stats.phase_after = self.state;
        stats.total_work_bytes = work;
        stats.heap_bytes_after = self.total_bytes;
        stats.debt_after = self.debt;
        stats.gray_len_after = self.gray.len();
        stats.grayagain_len_after = self.grayagain.len();
        self.last_step_stats = stats;
        work
    }

    /// Propagate marking incrementally. Returns work done.
    fn propagate_step<S: FnMut(&mut Gc, GcRef)>(
        &mut self,
        scan_object: &mut S,
        limit: usize,
    ) -> usize {
        let mut work = 0;

        while let Some(obj) = self.gray.pop() {
            // Validate gray object before processing
            debug_assert!(
                !obj.is_null() && (obj as usize) & (SLOT_BYTES - 1) == 0 && (obj as usize) >= 4096,
                "propagate_step: invalid GcRef {:p} in gray queue",
                obj
            );
            let header = Self::header_mut(obj);
            if !header.is_black() {
                header.set_black();
                scan_object(self, obj);
                work += Self::object_size_bytes(obj);

                if work >= limit {
                    break;
                }
            }
        }

        work
    }

    /// Atomic phase: process grayagain and finalize marking.
    fn atomic_phase<S: FnMut(&mut Gc, GcRef)>(&mut self, scan_object: &mut S) {
        // Process grayagain (objects modified during propagate)
        while let Some(obj) = self.grayagain.pop() {
            let header = Self::header_mut(obj);
            if !header.is_black() {
                header.set_black();
                scan_object(self, obj);
            }
        }

        // Process any new gray objects added during grayagain processing
        while let Some(obj) = self.gray.pop() {
            let header = Self::header_mut(obj);
            if !header.is_black() {
                header.set_black();
                scan_object(self, obj);
            }
        }
    }

    /// Sweep dead objects incrementally. Returns work done.
    #[cfg(test)]
    fn sweep_step<F: FnMut(GcRef)>(&mut self, finalize_object: &mut F, limit: usize) -> usize {
        let mut stats = GcStepStats::default();
        self.sweep_step_counted(finalize_object, limit, &mut stats)
    }

    /// Sweep dead objects incrementally and record telemetry. Returns work done.
    fn sweep_step_counted<F: FnMut(GcRef)>(
        &mut self,
        finalize_object: &mut F,
        limit: usize,
        stats: &mut GcStepStats,
    ) -> usize {
        let mut work = 0;
        let dead_white = self.other_white();

        while self.sweep_pos < self.all_objects.len() && work < limit {
            let obj = self.all_objects[self.sweep_pos];
            let header = Self::header(obj);
            let obj_white = header.marked & WHITE_BITS;

            // Gray objects should never exist during sweep — atomic phase processes all of them.
            // If one leaks here, it would be silently dropped (memory leak). Catch it early.
            debug_assert!(
                header.is_black() || obj_white != 0,
                "sweep_step: gray object {:p} found during sweep (neither white nor black)",
                obj
            );

            // Compute size once — both alive and dead branches need it for work accounting.
            let size_bytes = Self::object_size_bytes(obj);

            if header.is_black() || obj_white == self.current_white {
                // Alive: reset to current white
                Self::header_mut(obj).set_white(self.current_white);
                self.all_objects[self.sweep_write_pos] = obj;
                self.sweep_write_pos += 1;
                // Count alive objects toward work budget. Without this, a mostly-alive
                // heap (e.g. 99% alive) would have work ≈ 0 and the loop would process
                // the entire heap in one call — a 500MB heap causes a 625ms latency spike.
                work += size_bytes;
            } else if obj_white == dead_white {
                // Dead: free it
                #[cfg(feature = "gc-debug")]
                crate::gc_debug::on_free(obj);

                finalize_object(obj);
                stats.finalized_objects += 1;
                stats.sweep_freed_bytes += size_bytes;
                self.remove_base_index(obj);
                self.remove_range_index(obj);
                self.all_objects[self.sweep_pos] = core::ptr::null_mut();
                self.object_index_dirty.set(true);
                self.total_bytes -= size_bytes;
                work += size_bytes;

                let raw_ptr = unsafe { (obj as *mut u8).sub(GcHeader::SIZE) };
                let layout = core::alloc::Layout::from_size_align(size_bytes, 8).unwrap();
                unsafe { heap_alloc::dealloc(raw_ptr, layout) };
            }

            self.sweep_pos += 1;
        }

        // If sweep complete, truncate the vector
        if self.sweep_pos >= self.all_objects.len() {
            self.all_objects.truncate(self.sweep_write_pos);
            self.object_index_dirty.set(true);
        }

        work
    }

    /// Finish GC cycle.
    fn finish_cycle(&mut self) {
        self.estimate = self.total_bytes;
        self.state = GcState::Pause;
        self.pending_root_scan = None;

        // Set the debt threshold for the next cycle. Incremental phase work is
        // deliberately capped when repaying debt, but a cycle can still end with
        // old large negative debt from earlier accounting modes or host-driven
        // full cycles. Carrying that credit forward delays the next cycle and can
        // grow small WASM heaps until allocation fails, so cycle completion resets
        // to the current live-heap threshold instead of preserving excess credit.
        let threshold = (self.estimate as u64 * self.pause as u64 / 100) as i64;
        self.debt = -threshold.max(1024);
    }

    pub fn total_bytes(&self) -> usize {
        self.total_bytes
    }

    pub fn object_count(&self) -> usize {
        self.all_objects.iter().filter(|obj| !obj.is_null()).count()
    }

    pub fn debt(&self) -> i64 {
        self.debt
    }

    pub fn estimate(&self) -> usize {
        self.estimate
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

        self.mark_allocated_for_scan(dst);
        dst
    }
}

impl Default for Gc {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn empty_capture_slot_types(_: u32) -> &'static [crate::SlotType] {
        &[]
    }

    #[test]
    fn test_canonicalize_ref_base_uses_base_index() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let obj = gc.alloc(meta, 2);

        assert!(gc.object_index_dirty.get());
        assert_eq!(gc.canonicalize_ref(obj), Some(obj));
        assert!(gc.object_index_dirty.get());
    }

    #[test]
    fn test_canonicalize_ref_interior_pointer_uses_range_index() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let obj = gc.alloc(meta, 2);
        let interior = unsafe { obj.add(1) };

        assert!(gc.object_index_dirty.get());
        assert_eq!(gc.canonicalize_ref(interior), Some(obj));
        assert!(gc.object_index_dirty.get());
    }

    #[test]
    fn test_canonicalize_ref_large_array_far_interior_pointer() {
        let mut gc = Gc::new();
        let len = u16::MAX as usize + 32;
        let arr =
            crate::objects::array::create(&mut gc, ValueMeta::new(0, ValueKind::Uint64), 8, len);
        assert!(!arr.is_null());
        assert_eq!(Gc::header(arr).slots, 0);

        let far_interior =
            unsafe { crate::objects::array::data_ptr_bytes(arr).add((len - 1) * 8) as GcRef };
        assert_eq!(gc.canonicalize_ref(far_interior), Some(arr));
    }

    #[test]
    fn test_canonicalize_ref_nearby_interior_pointer_uses_base_fast_path() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let obj = gc.alloc(meta, 8);
        let interior = unsafe { obj.add(7) };

        assert_eq!(gc.canonicalize_ref(interior), Some(obj));
        assert!(gc.object_index_dirty.get());
    }

    #[test]
    fn test_canonicalize_ref_forgets_freed_object_during_partial_sweep() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let dead = gc.alloc(meta, 2);
        let live = gc.alloc(meta, 2);
        let dead_interior = unsafe { dead.add(1) };
        let mut finalized = Vec::new();

        assert_eq!(gc.canonicalize_ref(dead_interior), Some(dead));
        assert!(gc.object_index_dirty.get());

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        Gc::header_mut(live).set_black();

        let dead_size = Gc::object_size_bytes(dead);
        let work = gc.sweep_step(&mut |dead| finalized.push(dead), dead_size);

        assert!(work >= dead_size);
        assert_eq!(finalized, vec![dead]);
        assert_eq!(gc.state(), GcState::Sweep);
        assert_eq!(gc.canonicalize_ref(dead_interior), None);
    }

    #[test]
    fn test_sweep_removes_dead_object_from_live_index() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let obj = gc.alloc(meta, 2);
        let mut finalized = Vec::new();

        assert_eq!(gc.canonicalize_ref(obj), Some(obj));
        assert_eq!(gc.object_count(), 1);

        let mut work = 0;
        for _ in 0..8 {
            work += gc.step(|_| {}, |_, _| {}, |dead| finalized.push(dead));
            if gc.state() == GcState::Pause {
                break;
            }
        }

        assert!(work > 0);
        assert_eq!(finalized, vec![obj]);
        assert_eq!(gc.state(), GcState::Pause);
        assert_eq!(gc.object_count(), 0);
        assert_eq!(gc.canonicalize_ref(obj), None);
    }

    #[test]
    fn test_zero_slot_struct_sweeps_as_header_only_object() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let obj = gc.alloc(meta, 0);
        let mut finalized = Vec::new();

        assert_eq!(Gc::object_size_bytes(obj), GcHeader::SIZE);

        for _ in 0..8 {
            gc.step(|_| {}, |_, _| {}, |dead| finalized.push(dead));
            if gc.state() == GcState::Pause {
                break;
            }
        }

        assert_eq!(finalized, vec![obj]);
        assert_eq!(gc.total_bytes(), 0);
        assert_eq!(gc.object_count(), 0);
    }

    #[test]
    fn test_atomic_rescans_roots_added_after_cycle_start() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let keeper = gc.alloc(meta, 0);
        let late_root = gc.alloc(meta, 0);
        let late_root_slot = Cell::new(core::ptr::null_mut::<Slot>());
        let mut finalized = Vec::new();

        let mut work = 0;
        for _ in 0..8 {
            work += gc.step(
                |gc| {
                    gc.mark_gray(keeper);
                    let late = late_root_slot.get();
                    if !late.is_null() {
                        gc.mark_gray(late);
                    }
                },
                |_, obj| {
                    if obj == keeper {
                        late_root_slot.set(late_root);
                    }
                },
                |dead| finalized.push(dead),
            );
            if gc.state() == GcState::Pause {
                break;
            }
        }

        assert!(work > 0);
        assert!(
            !finalized.contains(&late_root),
            "object that became a root during mark must survive the same GC cycle"
        );
        assert_eq!(gc.state(), GcState::Pause);
        assert_eq!(gc.object_count(), 2);
        assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));
    }

    #[test]
    fn test_new_object_allocated_during_mark_scans_old_child() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let keeper = gc.alloc(meta, 0);
        let child = gc.alloc(meta, 0);
        let parent_slot = Cell::new(core::ptr::null_mut::<Slot>());
        let mut finalized = Vec::new();

        let mut work = 0;
        for _ in 0..8 {
            work += gc.step(
                |gc| {
                    gc.mark_gray(keeper);
                },
                |gc, obj| {
                    if obj == keeper && parent_slot.get().is_null() {
                        let parent = gc.alloc(meta, 1);
                        unsafe {
                            Gc::write_slot(parent, 0, child as u64);
                        }
                        gc.write_barrier(parent, child);
                        parent_slot.set(parent);
                    }
                    if obj == parent_slot.get() {
                        let raw_child = unsafe { Gc::read_slot(obj, 0) };
                        if raw_child != 0 {
                            gc.mark_gray(raw_child as GcRef);
                        }
                    }
                },
                |dead| finalized.push(dead),
            );
            if gc.state() == GcState::Pause {
                break;
            }
        }

        assert!(work > 0);
        assert!(
            !finalized.contains(&child),
            "old child stored in a new object allocated during mark must be scanned"
        );
        assert_eq!(gc.state(), GcState::Pause);
        assert_eq!(gc.object_count(), 3);
        assert_eq!(gc.canonicalize_ref(child), Some(child));
    }

    #[test]
    fn test_sweep_write_barrier_rescues_old_white_child() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let parent = gc.alloc(meta, 1);
        let child = gc.alloc(meta, 0);
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        unsafe {
            Gc::write_slot(parent, 0, child as u64);
        }
        Gc::header_mut(parent).set_black();

        assert_eq!(Gc::header(child).marked & WHITE_BITS, gc.other_white());
        gc.write_barrier(parent, child);
        assert!(Gc::header(child).is_gray());

        gc.atomic_phase(&mut |_, _| {});
        assert!(Gc::header(child).is_black());

        let work = gc.sweep_step(&mut |dead| finalized.push(dead), usize::MAX);

        assert!(work > 0);
        assert!(
            !finalized.contains(&child),
            "old child written during sweep must be rescued before sweep reaches it"
        );
        assert_eq!(gc.object_count(), 2);
        assert_eq!(gc.canonicalize_ref(child), Some(child));
    }

    #[test]
    fn test_active_gc_cycle_keeps_stepping_without_new_debt() {
        let mut gc = Gc::new();

        assert!(!gc.should_step());

        gc.state = GcState::Propagate;
        gc.debt = 0;
        assert!(gc.should_step());

        gc.state = GcState::Sweep;
        assert!(gc.should_step());
    }

    #[test]
    fn test_stress_every_step_starts_cycle_without_debt() {
        let mut gc = Gc::new();

        assert!(!gc.should_step());
        assert!(!gc.stress_every_step());

        gc.set_stress_every_step(true);

        assert!(gc.stress_every_step());
        assert!(gc.should_step());

        let work = gc.step(|_| {}, |_, _| {}, |_| {});
        assert_eq!(work, 0);
        assert_eq!(gc.state(), GcState::Atomic);
        assert!(gc.should_step());

        let work = gc.step(|_| {}, |_, _| {}, |_| {});
        assert_eq!(work, 0);
        assert_eq!(gc.state(), GcState::Sweep);
        assert!(gc.should_step());

        let work = gc.step(|_| {}, |_, _| {}, |_| {});
        assert_eq!(work, 0);
        assert_eq!(gc.state(), GcState::Pause);
        assert!(gc.should_step());
    }

    #[test]
    fn test_finish_cycle_resets_excess_negative_debt_to_live_heap_threshold() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let root = gc.alloc(meta, 4);

        gc.mark_gray(root);
        gc.debt = -1_000_000_000;
        gc.finish_cycle();

        let expected_threshold =
            ((gc.total_bytes() as u64 * gc.pause as u64 / 100) as i64).max(1024);
        assert_eq!(gc.debt(), -expected_threshold);
        assert!(gc.debt() > -1_000_000_000);
        assert_eq!(gc.state(), GcState::Pause);
    }

    #[test]
    fn test_step_stats_record_mark_work() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let root = gc.alloc(meta, 0);

        let work = gc.step(|gc| gc.mark_gray(root), |_, _| {}, |_| {});
        let stats = gc.last_step_stats();

        assert_eq!(stats.phase_before, GcState::Pause);
        assert_eq!(stats.phase_after, GcState::Atomic);
        assert_eq!(stats.root_state, GcRootState::MayHaveChanged);
        assert!(stats.cycle_started);
        assert_eq!(stats.root_scan_calls, 1);
        assert_eq!(stats.object_scans, 1);
        assert_eq!(stats.propagate_work_bytes, work);
        assert_eq!(stats.total_work_bytes, work);
        assert_eq!(stats.heap_bytes_before, GcHeader::SIZE);
        assert_eq!(stats.heap_bytes_after, GcHeader::SIZE);
    }

    #[test]
    fn test_step_stats_record_sweep_frees() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let dead = gc.alloc(meta, 0);
        let mut finalized = Vec::new();

        gc.step(|_| {}, |_, _| {}, |_| {});
        gc.step(|_| {}, |_, _| {}, |_| {});
        assert_eq!(gc.state(), GcState::Sweep);

        let work = gc.step(|_| {}, |_, _| {}, |obj| finalized.push(obj));
        let stats = gc.last_step_stats();

        assert_eq!(finalized, vec![dead]);
        assert_eq!(stats.phase_before, GcState::Sweep);
        assert_eq!(stats.phase_after, GcState::Pause);
        assert!(stats.cycle_finished);
        assert_eq!(stats.root_scan_calls, 1);
        assert_eq!(stats.finalized_objects, 1);
        assert_eq!(stats.sweep_freed_bytes, GcHeader::SIZE);
        assert_eq!(stats.sweep_work_bytes, work);
        assert_eq!(stats.total_work_bytes, work);
        assert_eq!(stats.heap_bytes_after, 0);
    }

    #[test]
    fn test_stable_root_state_skips_redundant_sweep_root_scans() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let root = gc.alloc(meta, 0);
        let mut root_scans = 0usize;

        for _ in 0..8 {
            gc.step_with_root_state(
                GcRootState::StableSinceLastScan,
                |gc| {
                    root_scans += 1;
                    gc.mark_gray(root);
                },
                |_, _| {},
                |_| {},
            );
            if gc.state() == GcState::Pause {
                break;
            }
        }

        assert_eq!(gc.state(), GcState::Pause);
        assert_eq!(
            root_scans, 2,
            "stable roots should be scanned only at cycle start and atomic"
        );
        assert_eq!(gc.object_count(), 1);
    }

    #[test]
    fn test_conservative_root_state_rescues_late_sweep_root() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let root = gc.alloc(meta, 0);
        let mut root_scans = 0usize;
        let mut finalized = Vec::new();

        gc.step(|_| {}, |_, _| {}, |_| {});
        gc.step(
            |_| {
                root_scans += 1;
            },
            |_, _| {},
            |_| {},
        );
        assert_eq!(gc.state(), GcState::Sweep);

        gc.step(
            |gc| {
                root_scans += 1;
                gc.mark_gray(root);
            },
            |_, _| {},
            |dead| finalized.push(dead),
        );

        assert!(
            !finalized.contains(&root),
            "default conservative step must rescan roots during sweep"
        );
        assert!(root_scans >= 2);
        assert_eq!(gc.canonicalize_ref(root), Some(root));
    }

    #[test]
    fn test_sweep_write_barrier_rescues_old_white_parent() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let parent = gc.alloc(meta, 1);
        let child = gc.alloc(meta, 0);
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        unsafe {
            Gc::write_slot(parent, 0, child as u64);
        }

        assert_eq!(Gc::header(parent).marked & WHITE_BITS, gc.other_white());
        gc.write_barrier(parent, child);
        assert!(Gc::header(parent).is_gray());

        gc.atomic_phase(&mut |gc, obj| {
            let raw_child = unsafe { Gc::read_slot(obj, 0) };
            if raw_child != 0 {
                gc.mark_gray(raw_child as GcRef);
            }
        });
        assert!(Gc::header(parent).is_black());

        let work = gc.sweep_step(&mut |dead| finalized.push(dead), usize::MAX);

        assert!(work > 0);
        assert!(
            !finalized.contains(&parent),
            "old parent written during sweep must be rescued before sweep reaches it"
        );
        assert_eq!(gc.canonicalize_ref(parent), Some(parent));
    }

    #[test]
    fn test_sweep_write_barrier_rescans_rescued_string_child() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let parent = gc.alloc(meta, 1);
        let child = crate::objects::string::create(&mut gc, b"hello");
        let child_array = crate::objects::slice::array_ref(child);
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        unsafe {
            Gc::write_slot(parent, 0, child as u64);
        }
        Gc::header_mut(parent).set_black();

        gc.write_barrier(parent, child);
        assert!(Gc::header(child).is_gray());

        gc.atomic_phase(&mut |gc, obj| {
            crate::gc_types::scan_object(gc, obj, &[], &empty_capture_slot_types);
        });

        let work = gc.sweep_step(&mut |dead| finalized.push(dead), usize::MAX);

        assert!(work > 0);
        assert!(!finalized.contains(&child));
        assert!(
            !finalized.contains(&child_array),
            "rescued string child must trace and rescue its backing array"
        );
        assert_eq!(gc.canonicalize_ref(child), Some(child));
        assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
    }

    #[test]
    fn test_sweep_rescans_roots_added_after_atomic() {
        let mut gc = Gc::new();
        let late_root = crate::objects::string::create(&mut gc, b"late");
        let late_root_array = crate::objects::slice::array_ref(late_root);
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        gc.sweep_budget = usize::MAX;

        let work = gc.step(
            |gc| gc.mark_gray(late_root),
            |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_capture_slot_types),
            |dead| finalized.push(dead),
        );

        assert!(work > 0);
        assert!(!finalized.contains(&late_root));
        assert!(
            !finalized.contains(&late_root_array),
            "root rescued during sweep must be rescanned before sweeping"
        );
        assert_eq!(gc.state(), GcState::Pause);
        assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));
        assert_eq!(gc.canonicalize_ref(late_root_array), Some(late_root_array));
    }

    #[test]
    fn test_sweep_allocated_clone_scans_copied_old_child() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let source = gc.alloc(meta, 1);
        let child = crate::objects::string::create(&mut gc, b"child");
        let child_array = crate::objects::slice::array_ref(child);
        let cloned_root = Cell::new(core::ptr::null_mut::<Slot>());
        let mut finalized = Vec::new();

        unsafe {
            Gc::write_slot(source, 0, child as u64);
        }

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        gc.sweep_budget = usize::MAX;

        let clone = unsafe { gc.ptr_clone(source) };
        cloned_root.set(clone);
        assert!(Gc::header(clone).is_gray());

        let work = gc.step(
            |gc| gc.mark_gray(cloned_root.get()),
            |gc, obj| {
                if obj == source || obj == clone {
                    let raw_child = unsafe { Gc::read_slot(obj, 0) };
                    if raw_child != 0 {
                        gc.mark_gray(raw_child as GcRef);
                    }
                }
                crate::gc_types::scan_object(gc, obj, &[], &empty_capture_slot_types);
            },
            |dead| finalized.push(dead),
        );

        assert!(work > 0);
        assert!(!finalized.contains(&clone));
        assert!(!finalized.contains(&child));
        assert!(
            !finalized.contains(&child_array),
            "object allocated during sweep must scan copied references"
        );
        assert_eq!(gc.canonicalize_ref(clone), Some(clone));
        assert_eq!(gc.canonicalize_ref(child), Some(child));
        assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
    }

    #[test]
    fn test_sweep_range_barrier_rescues_copied_string_refs() {
        let mut gc = Gc::new();
        let elem_meta = ValueMeta::new(0, ValueKind::String);
        let arr = crate::objects::array::create(&mut gc, elem_meta, SLOT_BYTES, 1);
        let child = crate::objects::string::create(&mut gc, b"child");
        let child_array = crate::objects::slice::array_ref(child);
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        gc.sweep_budget = usize::MAX;
        Gc::header_mut(arr).set_black();

        crate::objects::array::set(arr, 0, child as u64, SLOT_BYTES);
        crate::gc_types::typed_write_barrier_range_by_meta(
            &mut gc,
            arr,
            crate::objects::array::data_ptr_bytes(arr),
            1,
            SLOT_BYTES,
            elem_meta,
            None,
        );
        assert!(Gc::header(child).is_gray());

        let work = gc.step(
            |gc| gc.mark_gray(arr),
            |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_capture_slot_types),
            |dead| finalized.push(dead),
        );

        assert!(work > 0);
        assert!(!finalized.contains(&child));
        assert!(!finalized.contains(&child_array));
        assert_eq!(gc.canonicalize_ref(child), Some(child));
        assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
    }

    #[test]
    fn test_struct_barrier_without_module_is_conservative() {
        let mut gc = Gc::new();
        let parent_meta = ValueMeta::new(1, ValueKind::Struct);
        let struct_meta = ValueMeta::new(123, ValueKind::Struct);
        let parent = gc.alloc(parent_meta, 1);
        let child = crate::objects::string::create(&mut gc, b"struct-child");
        let child_array = crate::objects::slice::array_ref(child);
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        gc.sweep_budget = usize::MAX;
        Gc::header_mut(parent).set_black();

        unsafe {
            Gc::write_slot(parent, 0, child as u64);
        }
        crate::gc_types::typed_write_barrier_by_meta(
            &mut gc,
            parent,
            &[child as u64],
            struct_meta,
            None,
        );
        assert!(Gc::header(child).is_gray());

        let work = gc.step(
            |gc| gc.mark_gray(parent),
            |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_capture_slot_types),
            |dead| finalized.push(dead),
        );

        assert!(work > 0);
        assert!(!finalized.contains(&child));
        assert!(!finalized.contains(&child_array));
        assert_eq!(gc.canonicalize_ref(child), Some(child));
        assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
    }

    #[test]
    fn test_sweep_initialized_array_scans_copied_old_child() {
        let mut gc = Gc::new();
        let elem_meta = ValueMeta::new(0, ValueKind::String);
        let child = crate::objects::string::create(&mut gc, b"child");
        let child_array = crate::objects::slice::array_ref(child);
        let new_arr_root = Cell::new(core::ptr::null_mut::<Slot>());
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        gc.sweep_budget = usize::MAX;

        let new_arr = crate::objects::array::create(&mut gc, elem_meta, SLOT_BYTES, 1);
        crate::objects::array::set(new_arr, 0, child as u64, SLOT_BYTES);
        gc.mark_allocated_for_scan(new_arr);
        new_arr_root.set(new_arr);
        assert!(Gc::header(new_arr).is_gray());

        let work = gc.step(
            |gc| gc.mark_gray(new_arr_root.get()),
            |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_capture_slot_types),
            |dead| finalized.push(dead),
        );

        assert!(work > 0);
        assert!(!finalized.contains(&new_arr));
        assert!(!finalized.contains(&child));
        assert!(!finalized.contains(&child_array));
        assert_eq!(gc.canonicalize_ref(new_arr), Some(new_arr));
        assert_eq!(gc.canonicalize_ref(child), Some(child));
        assert_eq!(gc.canonicalize_ref(child_array), Some(child_array));
    }

    #[test]
    fn test_sweep_initialized_map_scans_copied_old_child() {
        let mut gc = Gc::new();
        let str_meta = ValueMeta::new(0, ValueKind::String);
        let key = crate::objects::string::create(&mut gc, b"key");
        let key_array = crate::objects::slice::array_ref(key);
        let child = crate::objects::string::create(&mut gc, b"child");
        let child_array = crate::objects::slice::array_ref(child);
        let new_map_root = Cell::new(core::ptr::null_mut::<Slot>());
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        gc.sweep_budget = usize::MAX;

        let new_map = crate::objects::map::create(&mut gc, str_meta, str_meta, 1, 1, 0);
        crate::objects::map::set(new_map, &[key as u64], &[child as u64], None);
        gc.mark_allocated_for_scan(new_map);
        new_map_root.set(new_map);
        assert!(Gc::header(new_map).is_gray());

        let work = gc.step(
            |gc| gc.mark_gray(new_map_root.get()),
            |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_capture_slot_types),
            |dead| finalized.push(dead),
        );

        assert!(work > 0);
        assert!(!finalized.contains(&new_map));
        assert!(!finalized.contains(&key));
        assert!(!finalized.contains(&key_array));
        assert!(!finalized.contains(&child));
        assert!(!finalized.contains(&child_array));
        assert_eq!(gc.canonicalize_ref(new_map), Some(new_map));
        assert_eq!(gc.canonicalize_ref(key), Some(key));
        assert_eq!(gc.canonicalize_ref(child), Some(child));
    }

    #[test]
    fn test_object_allocated_after_partial_sweep_survives_as_late_root() {
        let mut gc = Gc::new();
        let meta = ValueMeta::new(1, ValueKind::Struct);
        let _dead_a = gc.alloc(meta, 0);
        let _dead_b = gc.alloc(meta, 0);
        let _dead_c = gc.alloc(meta, 0);
        let mut finalized = Vec::new();

        gc.current_white ^= WHITE_BITS;
        gc.state = GcState::Sweep;
        gc.sweep_pos = 0;
        gc.sweep_write_pos = 0;
        gc.sweep_budget = usize::MAX;

        let partial_work = gc.sweep_step(&mut |dead| finalized.push(dead), GcHeader::SIZE);
        assert!(partial_work > 0);
        assert_eq!(gc.state(), GcState::Sweep);
        assert!(gc.sweep_pos > 0);

        let late_root =
            crate::objects::slice::create(&mut gc, ValueMeta::new(0, ValueKind::Uint8), 1, 16, 16);
        assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));

        let work = gc.step(
            |gc| gc.mark_gray(late_root),
            |gc, obj| crate::gc_types::scan_object(gc, obj, &[], &empty_capture_slot_types),
            |dead| finalized.push(dead),
        );

        assert!(work > 0);
        assert!(
            !finalized.contains(&late_root),
            "object allocated after a partial sweep and then rooted must not be freed"
        );
        assert_eq!(gc.state(), GcState::Pause);
        assert_eq!(gc.canonicalize_ref(late_root), Some(late_root));
    }
}

/// Scan a slice of values using SlotTypes for GC marking.
///
/// This is the unified scanning function used by both VM root scanning
/// and heap object scanning.
#[inline]
pub fn scan_slots_by_types(gc: &mut Gc, slots: &[u64], slot_types: &[crate::SlotType]) {
    use crate::objects::interface;
    use crate::SlotType;

    let mut i = 0;
    while i < slot_types.len() && i < slots.len() {
        match slot_types[i] {
            SlotType::GcRef => {
                if slots[i] != 0 {
                    gc.mark_gray(slots[i] as GcRef);
                }
            }
            SlotType::Interface0 => {
                // Interface header slot - check if data slot contains GcRef
                if i + 1 < slots.len() && interface::data_is_gc_ref(slots[i]) && slots[i + 1] != 0 {
                    gc.mark_gray(slots[i + 1] as GcRef);
                }
                i += 1; // Skip data slot (Interface1)
            }
            _ => {}
        }
        i += 1;
    }
}
