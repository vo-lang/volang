//! Garbage collector core.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

#[cfg(feature = "std")]
use std::alloc as heap_alloc;
#[cfg(not(feature = "std"))]
use alloc::alloc as heap_alloc;

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
const AGE_MASK: u8 = 0x07;      // bits 0-2
const WHITE0_BIT: u8 = 1 << 3;  // bit 3
const WHITE1_BIT: u8 = 1 << 4;  // bit 4
const BLACK_BIT: u8 = 1 << 5;   // bit 5
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
    Pause = 0,      // Idle, waiting for trigger
    Propagate = 1,  // Incremental marking (interruptible)
    Atomic = 2,     // Atomic marking (not interruptible)
    Sweep = 3,      // Sweeping dead objects
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

/// Garbage collector.
pub struct Gc {
    // ========== Object Storage ==========
    all_objects: Vec<GcRef>,
    
    // ========== Mark Queues ==========
    gray: Vec<GcRef>,        // To be scanned
    grayagain: Vec<GcRef>,   // Re-scan (barrier triggered)
    
    // ========== State ==========
    state: GcState,
    current_white: u8,       // Current white bit (WHITE0_BIT or WHITE1_BIT)
    sweep_pos: usize,        // Read position in sweep phase
    sweep_write_pos: usize,  // Write position for live objects in sweep phase
    
    // ========== Memory Stats ==========
    total_bytes: usize,      // Total allocated bytes
    estimate: usize,         // Estimated live bytes after last GC
    debt: i64,               // Work debt (triggers GC when > 0)
    
    // ========== Parameters ==========
    pause: u16,              // Pause multiplier (default 200 = 2x)
    stepmul: u16,            // Step multiplier (default 100)
    stepsize: usize,         // Bytes per step (default 8KB)
    
}

impl Gc {
    // Default parameters
    const DEFAULT_PAUSE: u16 = 200;      // Trigger at 2x estimated live size
    const DEFAULT_STEPMUL: u16 = 100;    // Work multiplier
    const DEFAULT_STEPSIZE: usize = 8192; // 8KB per step

    pub fn new() -> Self {
        Self {
            all_objects: Vec::new(),
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
        let header_slots = if total_slots > u16::MAX as usize { 0 } else { total_slots as u16 };
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
                eprintln!("GC allocation overflow: header_size={}, data_size={}", header_size, data_size);
                return core::ptr::null_mut();
            }
        };

        let layout = match core::alloc::Layout::from_size_align(total_size, SLOT_BYTES) {
            Ok(l) => l,
            Err(_) => {
                #[cfg(feature = "std")]
                eprintln!("GC allocation layout error: total_size={}, align={}", total_size, SLOT_BYTES);
                return core::ptr::null_mut();
            }
        };
        let ptr = unsafe { heap_alloc::alloc_zeroed(layout) };

        if ptr.is_null() {
            #[cfg(feature = "std")]
            eprintln!("GC allocation failed: out of memory");
            return core::ptr::null_mut();
        }

        // New object gets current white color
        let header = GcHeader::new_with_white(value_meta, header_slots, self.current_white);
        unsafe {
            core::ptr::write(ptr as *mut GcHeader, header);
        }

        let data_ptr = unsafe { ptr.add(header_size) as GcRef };

        self.all_objects.push(data_ptr);
        self.total_bytes += total_size;
        self.debt += total_size as i64;

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
        let slots = if header.slots == 0 {
            array::total_slots(obj)
        } else {
            header.slots as usize
        };
        GcHeader::SIZE + slots * SLOT_BYTES
    }

    /// Mark an object as gray (pending scan).
    #[inline]
    pub fn mark_gray(&mut self, obj: GcRef) {
        if obj.is_null() {
            return;
        }
        if (obj as usize) & (SLOT_BYTES - 1) != 0 || (obj as usize) < 4096 {
            self.mark_gray_fail(obj);
        }
        let header = Self::header_mut(obj);
        if header.is_white() {
            header.set_gray();
            self.gray.push(obj);
        }
    }

    #[cold]
    #[inline(never)]
    fn mark_gray_fail(&self, obj: GcRef) -> ! {
        panic!(
            "mark_gray: invalid GcRef {:p} (raw={:#x}) — non-GcRef value in GcRef-typed slot",
            obj, obj as usize
        );
    }

    /// Write barrier for incremental GC (backward barrier).
    /// Called when a black object writes a white reference.
    pub fn write_barrier(&mut self, parent: GcRef, child: GcRef) {
        #[cfg(feature = "gc-debug")]
        crate::gc_debug::on_barrier(parent, 0, child as u64);
        
        if self.state != GcState::Propagate {
            return;
        }
        if parent.is_null() || child.is_null() {
            return;
        }
        let p_header = Self::header(parent);
        let c_header = Self::header(child);
        // Backward barrier: black parent writes white child → parent becomes gray
        if p_header.is_black() && c_header.is_white() {
            self.barrier_back(parent);
        }
    }
    
    /// Check if object is black (for gc-debug)
    #[inline]
    pub fn is_black(&self, obj: GcRef) -> bool {
        if obj.is_null() { return false; }
        Self::header(obj).is_black()
    }
    
    /// Check if object is white (for gc-debug)
    #[inline]
    pub fn is_white(&self, obj: GcRef) -> bool {
        if obj.is_null() { return false; }
        Self::header(obj).is_white()
    }
    
    /// Backward barrier: turn black object back to gray for re-scan.
    fn barrier_back(&mut self, obj: GcRef) {
        let header = Self::header_mut(obj);
        header.set_white(self.current_white);
        self.grayagain.push(obj);
    }

    /// Check if GC should run (debt-based trigger).
    #[inline]
    pub fn should_step(&self) -> bool {
        self.debt > 0
    }

    /// Incremental GC step. Returns work done (bytes processed).
    /// Call this when debt > 0, passing scan_roots and scan_object callbacks.
    pub fn step<R, S, F>(
        &mut self,
        mut scan_roots: R,
        mut scan_object: S,
        mut finalize_object: F,
    ) -> usize
    where
        R: FnMut(&mut Gc),
        S: FnMut(&mut Gc, GcRef),
        F: FnMut(GcRef),
    {
        let mut work = 0usize;
        let work_limit = self.stepsize * self.stepmul as usize / 100;
        
        loop {
            match self.state {
                GcState::Pause => {
                    // Start new cycle
                    self.start_cycle(&mut scan_roots);
                    self.state = GcState::Propagate;
                }
                
                GcState::Propagate => {
                    // Incremental marking
                    work += self.propagate_step(&mut scan_object, work_limit.saturating_sub(work));
                    
                    if self.gray.is_empty() {
                        // Move to atomic phase
                        self.state = GcState::Atomic;
                    } else if work >= work_limit {
                        // Yield - done enough work this step
                        break;
                    }
                }
                
                GcState::Atomic => {
                    // Atomic phase: process grayagain, finalize marking
                    self.atomic_phase(&mut scan_object);
                    self.state = GcState::Sweep;
                    self.sweep_pos = 0;
                    self.sweep_write_pos = 0;
                }
                
                GcState::Sweep => {
                    // Incremental sweeping
                    work += self.sweep_step(&mut finalize_object, work_limit.saturating_sub(work));
                    
                    if self.sweep_pos >= self.all_objects.len() {
                        // Sweep complete
                        self.finish_cycle();
                        break;
                    } else if work >= work_limit {
                        // Yield
                        break;
                    }
                }
            }
        }
        
        self.debt -= work as i64;
        work
    }
    
    /// Start a new GC cycle.
    fn start_cycle<R: FnMut(&mut Gc)>(&mut self, scan_roots: &mut R) {
        // Flip white for this cycle (objects allocated during GC get new white)
        self.current_white ^= WHITE_BITS;
        scan_roots(self);
    }
    
    /// Propagate marking incrementally. Returns work done.
    fn propagate_step<S: FnMut(&mut Gc, GcRef)>(&mut self, scan_object: &mut S, limit: usize) -> usize {
        let mut work = 0;
        
        while let Some(obj) = self.gray.pop() {
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
    fn sweep_step<F: FnMut(GcRef)>(&mut self, finalize_object: &mut F, limit: usize) -> usize {
        let mut work = 0;
        let dead_white = self.other_white();
        
        while self.sweep_pos < self.all_objects.len() && work < limit {
            let obj = self.all_objects[self.sweep_pos];
            let header = Self::header(obj);
            let obj_white = header.marked & WHITE_BITS;
            
            if header.is_black() || obj_white == self.current_white {
                // Alive: reset to current white
                Self::header_mut(obj).set_white(self.current_white);
                self.all_objects[self.sweep_write_pos] = obj;
                self.sweep_write_pos += 1;
            } else if obj_white == dead_white {
                // Dead: free it
                #[cfg(feature = "gc-debug")]
                crate::gc_debug::on_free(obj);
                
                let size_bytes = Self::object_size_bytes(obj);
                finalize_object(obj);
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
        }
        
        work
    }
    
    /// Finish GC cycle.
    fn finish_cycle(&mut self) {
        self.estimate = self.total_bytes;
        self.state = GcState::Pause;
        
        // Set debt threshold for next cycle
        let threshold = (self.estimate as u64 * self.pause as u64 / 100) as i64;
        self.debt = self.debt.min(-(threshold.max(1024) as i64));
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

/// Scan a slice of values using SlotTypes for GC marking.
/// 
/// This is the unified scanning function used by both VM root scanning
/// and heap object scanning.
#[inline]
pub fn scan_slots_by_types(gc: &mut Gc, slots: &[u64], slot_types: &[crate::SlotType]) {
    use crate::SlotType;
    use crate::objects::interface;
    
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
                if i + 1 < slots.len() && interface::data_is_gc_ref(slots[i]) {
                    if slots[i + 1] != 0 {
                        gc.mark_gray(slots[i + 1] as GcRef);
                    }
                }
                i += 1; // Skip data slot (Interface1)
            }
            _ => {}
        }
        i += 1;
    }
}
