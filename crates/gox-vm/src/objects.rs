//! Heap object operations: String, Array, Slice, Map, Channel, Closure.

use crate::gc::{Gc, GcRef};
use crate::types::TypeId;
use indexmap::IndexMap;
use std::collections::VecDeque;

/// String object layout (after GcHeader):
/// - slot 0: ptr to byte array (GcRef)
/// - slot 1: start offset (usize)
/// - slot 2: length (usize)
pub mod string {
    use super::*;
    
    const ARRAY_SLOT: usize = 0;
    const START_SLOT: usize = 1;
    const LEN_SLOT: usize = 2;
    pub const SIZE_SLOTS: usize = 3;
    
    pub fn create(gc: &mut Gc, type_id: TypeId, bytes: &[u8]) -> GcRef {
        // Create byte array
        let array_slots = (bytes.len() + 7) / 8;
        let array = gc.alloc(type_id, 1 + array_slots); // len + data
        Gc::write_slot(array, 0, bytes.len() as u64);
        let data_ptr = unsafe { Gc::get_data_ptr(array).add(1) as *mut u8 };
        unsafe {
            std::ptr::copy_nonoverlapping(bytes.as_ptr(), data_ptr, bytes.len());
        }
        
        // Create string object
        let str_obj = gc.alloc(type_id, SIZE_SLOTS);
        Gc::write_slot(str_obj, ARRAY_SLOT, array as u64);
        Gc::write_slot(str_obj, START_SLOT, 0);
        Gc::write_slot(str_obj, LEN_SLOT, bytes.len() as u64);
        
        str_obj
    }
    
    pub fn from_rust_str(gc: &mut Gc, type_id: TypeId, s: &str) -> GcRef {
        create(gc, type_id, s.as_bytes())
    }
    
    pub fn len(str_ref: GcRef) -> usize {
        Gc::read_slot(str_ref, LEN_SLOT) as usize
    }
    
    pub fn as_bytes(str_ref: GcRef) -> &'static [u8] {
        let array = Gc::read_slot(str_ref, ARRAY_SLOT) as GcRef;
        let start = Gc::read_slot(str_ref, START_SLOT) as usize;
        let len = Gc::read_slot(str_ref, LEN_SLOT) as usize;
        
        let data_ptr = unsafe { Gc::get_data_ptr(array).add(1) as *const u8 };
        unsafe { std::slice::from_raw_parts(data_ptr.add(start), len) }
    }
    
    pub fn as_str(str_ref: GcRef) -> &'static str {
        unsafe { std::str::from_utf8_unchecked(as_bytes(str_ref)) }
    }
    
    pub fn index(str_ref: GcRef, idx: usize) -> u8 {
        as_bytes(str_ref)[idx]
    }
    
    pub fn concat(gc: &mut Gc, type_id: TypeId, a: GcRef, b: GcRef) -> GcRef {
        let a_bytes = as_bytes(a);
        let b_bytes = as_bytes(b);
        let mut combined = Vec::with_capacity(a_bytes.len() + b_bytes.len());
        combined.extend_from_slice(a_bytes);
        combined.extend_from_slice(b_bytes);
        create(gc, type_id, &combined)
    }
    
    pub fn array_ref(str_ref: GcRef) -> GcRef {
        Gc::read_slot(str_ref, ARRAY_SLOT) as GcRef
    }
    
    /// Compare two strings for equality (content comparison).
    pub fn eq(a: GcRef, b: GcRef) -> bool {
        if a == b {
            return true; // Same reference
        }
        if a.is_null() || b.is_null() {
            return a.is_null() && b.is_null();
        }
        as_bytes(a) == as_bytes(b)
    }
    
    /// Compare two strings for inequality (content comparison).
    pub fn ne(a: GcRef, b: GcRef) -> bool {
        !eq(a, b)
    }
}

/// Array object layout (after GcHeader):
/// - slot 0: elem_type (TypeId)
/// - slot 1: elem_size (slots)
/// - slot 2: length
/// - slot 3+: data
pub mod array {
    use super::*;
    
    const ELEM_TYPE_SLOT: usize = 0;
    const ELEM_SIZE_SLOT: usize = 1;
    const LEN_SLOT: usize = 2;
    const DATA_START: usize = 3;
    
    pub fn create(gc: &mut Gc, type_id: TypeId, elem_type: TypeId, elem_size: usize, len: usize) -> GcRef {
        let total_slots = DATA_START + len * elem_size;
        let arr = gc.alloc(type_id, total_slots);
        Gc::write_slot(arr, ELEM_TYPE_SLOT, elem_type as u64);
        Gc::write_slot(arr, ELEM_SIZE_SLOT, elem_size as u64);
        Gc::write_slot(arr, LEN_SLOT, len as u64);
        arr
    }
    
    pub fn len(arr: GcRef) -> usize {
        Gc::read_slot(arr, LEN_SLOT) as usize
    }
    
    pub fn elem_size(arr: GcRef) -> usize {
        Gc::read_slot(arr, ELEM_SIZE_SLOT) as usize
    }
    
    pub fn elem_type(arr: GcRef) -> TypeId {
        Gc::read_slot(arr, ELEM_TYPE_SLOT) as TypeId
    }
    
    pub fn get(arr: GcRef, idx: usize) -> u64 {
        let elem_sz = elem_size(arr);
        debug_assert!(idx < len(arr));
        debug_assert!(elem_sz == 1, "multi-slot get not supported, use get_n");
        Gc::read_slot(arr, DATA_START + idx * elem_sz)
    }
    
    pub fn set(arr: GcRef, idx: usize, val: u64) {
        let elem_sz = elem_size(arr);
        debug_assert!(idx < len(arr));
        debug_assert!(elem_sz == 1, "multi-slot set not supported, use set_n");
        Gc::write_slot(arr, DATA_START + idx * elem_sz, val);
    }
    
    pub fn get_n(arr: GcRef, idx: usize, dest: &mut [u64]) {
        let elem_sz = elem_size(arr);
        debug_assert!(idx < len(arr));
        debug_assert!(dest.len() == elem_sz);
        for i in 0..elem_sz {
            dest[i] = Gc::read_slot(arr, DATA_START + idx * elem_sz + i);
        }
    }
    
    pub fn set_n(arr: GcRef, idx: usize, src: &[u64]) {
        let elem_sz = elem_size(arr);
        debug_assert!(idx < len(arr));
        debug_assert!(src.len() == elem_sz);
        for i in 0..elem_sz {
            Gc::write_slot(arr, DATA_START + idx * elem_sz + i, src[i]);
        }
    }
    
    pub fn data_ptr(arr: GcRef) -> *mut u64 {
        unsafe { Gc::get_data_ptr(arr).add(DATA_START) }
    }
}

/// Slice object layout (after GcHeader):
/// - slot 0: array ref (GcRef)
/// - slot 1: start index
/// - slot 2: length
/// - slot 3: capacity
pub mod slice {
    use super::*;
    
    const ARRAY_SLOT: usize = 0;
    const START_SLOT: usize = 1;
    const LEN_SLOT: usize = 2;
    const CAP_SLOT: usize = 3;
    pub const SIZE_SLOTS: usize = 4;
    
    pub fn create(gc: &mut Gc, type_id: TypeId, array: GcRef, start: usize, len: usize, cap: usize) -> GcRef {
        let slice = gc.alloc(type_id, SIZE_SLOTS);
        Gc::write_slot(slice, ARRAY_SLOT, array as u64);
        Gc::write_slot(slice, START_SLOT, start as u64);
        Gc::write_slot(slice, LEN_SLOT, len as u64);
        Gc::write_slot(slice, CAP_SLOT, cap as u64);
        slice
    }
    
    pub fn from_array(gc: &mut Gc, type_id: TypeId, array: GcRef) -> GcRef {
        let len = array::len(array);
        create(gc, type_id, array, 0, len, len)
    }
    
    pub fn array_ref(slice: GcRef) -> GcRef {
        Gc::read_slot(slice, ARRAY_SLOT) as GcRef
    }
    
    pub fn start(slice: GcRef) -> usize {
        Gc::read_slot(slice, START_SLOT) as usize
    }
    
    pub fn len(slice: GcRef) -> usize {
        Gc::read_slot(slice, LEN_SLOT) as usize
    }
    
    pub fn cap(slice: GcRef) -> usize {
        Gc::read_slot(slice, CAP_SLOT) as usize
    }
    
    pub fn elem_size(slice: GcRef) -> usize {
        array::elem_size(array_ref(slice))
    }
    
    pub fn elem_type(slice: GcRef) -> TypeId {
        array::elem_type(array_ref(slice))
    }
    
    pub fn get(slice: GcRef, idx: usize) -> u64 {
        debug_assert!(idx < len(slice));
        array::get(array_ref(slice), start(slice) + idx)
    }
    
    pub fn set(slice: GcRef, idx: usize, val: u64) {
        debug_assert!(idx < len(slice));
        array::set(array_ref(slice), start(slice) + idx, val);
    }
    
    pub fn get_n(slice: GcRef, idx: usize, dest: &mut [u64]) {
        debug_assert!(idx < len(slice));
        array::get_n(array_ref(slice), start(slice) + idx, dest);
    }
    
    pub fn set_n(slice: GcRef, idx: usize, src: &[u64]) {
        debug_assert!(idx < len(slice));
        array::set_n(array_ref(slice), start(slice) + idx, src);
    }
    
    pub fn slice_of(gc: &mut Gc, type_id: TypeId, slice: GcRef, new_start: usize, new_end: usize) -> GcRef {
        let arr = array_ref(slice);
        let base = start(slice);
        let old_cap = cap(slice);
        
        debug_assert!(new_start <= new_end);
        debug_assert!(new_end <= len(slice));
        
        let new_cap = old_cap - new_start;
        create(gc, type_id, arr, base + new_start, new_end - new_start, new_cap)
    }
    
    pub fn append(gc: &mut Gc, type_id: TypeId, arr_type_id: TypeId, slice: GcRef, val: u64) -> GcRef {
        // Handle nil slice
        if slice.is_null() {
            // Create a new slice with capacity 4
            let elem_sz = 1; // Default element size for nil slice
            let elem_ty = 0; // Default element type
            let new_arr = array::create(gc, arr_type_id, elem_ty, elem_sz, 4);
            array::set(new_arr, 0, val);
            return create(gc, type_id, new_arr, 0, 1, 4);
        }
        
        let current_len = len(slice);
        let current_cap = cap(slice);
        let elem_sz = elem_size(slice);
        let elem_ty = elem_type(slice);
        
        if current_len < current_cap {
            // Has capacity, just extend
            let arr = array_ref(slice);
            array::set(arr, start(slice) + current_len, val);
            Gc::write_slot(slice, LEN_SLOT, (current_len + 1) as u64);
            slice
        } else {
            // Need to grow
            let new_cap = if current_cap == 0 { 4 } else { current_cap * 2 };
            let new_arr = array::create(gc, arr_type_id, elem_ty, elem_sz, new_cap);
            
            // Copy old data
            let old_arr = array_ref(slice);
            let old_start = start(slice);
            for i in 0..current_len {
                let v = array::get(old_arr, old_start + i);
                array::set(new_arr, i, v);
            }
            
            // Add new element
            array::set(new_arr, current_len, val);
            
            create(gc, type_id, new_arr, 0, current_len + 1, new_cap)
        }
    }
}

/// Compute hash for a struct value (based on field values, not pointer)
pub fn struct_hash(obj: GcRef, field_count: usize) -> u64 {
    use std::hash::{Hash, Hasher};
    use std::collections::hash_map::DefaultHasher;
    
    let mut hasher = DefaultHasher::new();
    for i in 0..field_count {
        let val = Gc::read_slot(obj, i);
        val.hash(&mut hasher);
    }
    hasher.finish()
}

/// Map object - uses Rust IndexMap internally.
/// Layout (after GcHeader):
/// - slot 0: Box pointer to IndexMap
/// - slot 1: key_type
/// - slot 2: value_type
pub mod map {
    use super::*;
    
    const MAP_PTR_SLOT: usize = 0;
    const KEY_TYPE_SLOT: usize = 1;
    const VAL_TYPE_SLOT: usize = 2;
    pub const SIZE_SLOTS: usize = 3;
    
    type MapInner = IndexMap<u64, u64>;
    
    pub fn create(gc: &mut Gc, type_id: TypeId, key_type: TypeId, val_type: TypeId) -> GcRef {
        let map_obj = gc.alloc(type_id, SIZE_SLOTS);
        let inner = Box::new(MapInner::new());
        Gc::write_slot(map_obj, MAP_PTR_SLOT, Box::into_raw(inner) as u64);
        Gc::write_slot(map_obj, KEY_TYPE_SLOT, key_type as u64);
        Gc::write_slot(map_obj, VAL_TYPE_SLOT, val_type as u64);
        map_obj
    }
    
    fn get_inner(map: GcRef) -> &'static mut MapInner {
        let ptr = Gc::read_slot(map, MAP_PTR_SLOT) as *mut MapInner;
        unsafe { &mut *ptr }
    }
    
    pub fn len(map: GcRef) -> usize {
        get_inner(map).len()
    }
    
    pub fn get(map: GcRef, key: u64) -> Option<u64> {
        get_inner(map).get(&key).copied()
    }
    
    pub fn set(map: GcRef, key: u64, val: u64) {
        get_inner(map).insert(key, val);
    }
    
    pub fn delete(map: GcRef, key: u64) {
        get_inner(map).swap_remove(&key);
    }
    
    pub fn contains(map: GcRef, key: u64) -> bool {
        get_inner(map).contains_key(&key)
    }
    
    pub fn iter_at(map: GcRef, idx: usize) -> Option<(u64, u64)> {
        get_inner(map).get_index(idx).map(|(&k, &v)| (k, v))
    }
    
    pub fn key_type(map: GcRef) -> TypeId {
        Gc::read_slot(map, KEY_TYPE_SLOT) as TypeId
    }
    
    pub fn val_type(map: GcRef) -> TypeId {
        Gc::read_slot(map, VAL_TYPE_SLOT) as TypeId
    }
    
    /// Drop the internal map (must be called before GC frees the object).
    pub unsafe fn drop_inner(map: GcRef) {
        let ptr = Gc::read_slot(map, MAP_PTR_SLOT) as *mut MapInner;
        if !ptr.is_null() {
            drop(Box::from_raw(ptr));
            Gc::write_slot(map, MAP_PTR_SLOT, 0);
        }
    }
}

/// Channel object layout (after GcHeader):
/// - slot 0: Box pointer to channel state
/// - slot 1: elem_type
/// - slot 2: capacity
pub mod channel {
    use super::*;
    use crate::fiber::FiberId;
    
    const CHAN_PTR_SLOT: usize = 0;
    const ELEM_TYPE_SLOT: usize = 1;
    const CAP_SLOT: usize = 2;
    pub const SIZE_SLOTS: usize = 3;
    
    #[derive(Default)]
    pub struct ChannelState {
        pub buffer: VecDeque<u64>,
        pub closed: bool,
        pub waiting_senders: VecDeque<(FiberId, u64)>,
        pub waiting_receivers: VecDeque<FiberId>,
    }
    
    pub fn create(gc: &mut Gc, type_id: TypeId, elem_type: TypeId, capacity: usize) -> GcRef {
        let chan = gc.alloc(type_id, SIZE_SLOTS);
        let state = Box::new(ChannelState {
            buffer: VecDeque::with_capacity(capacity),
            closed: false,
            waiting_senders: VecDeque::new(),
            waiting_receivers: VecDeque::new(),
        });
        Gc::write_slot(chan, CHAN_PTR_SLOT, Box::into_raw(state) as u64);
        Gc::write_slot(chan, ELEM_TYPE_SLOT, elem_type as u64);
        Gc::write_slot(chan, CAP_SLOT, capacity as u64);
        chan
    }
    
    pub fn get_state(chan: GcRef) -> &'static mut ChannelState {
        let ptr = Gc::read_slot(chan, CHAN_PTR_SLOT) as *mut ChannelState;
        unsafe { &mut *ptr }
    }
    
    pub fn elem_type(chan: GcRef) -> TypeId {
        Gc::read_slot(chan, ELEM_TYPE_SLOT) as TypeId
    }
    
    pub fn capacity(chan: GcRef) -> usize {
        Gc::read_slot(chan, CAP_SLOT) as usize
    }
    
    /// Returns the number of elements currently in the channel buffer.
    pub fn len(chan: GcRef) -> usize {
        get_state(chan).buffer.len()
    }
    
    pub fn is_closed(chan: GcRef) -> bool {
        get_state(chan).closed
    }
    
    pub fn close(chan: GcRef) {
        get_state(chan).closed = true;
    }
    
    /// Try to send a value. Returns Ok(()) if sent, Err(val) if would block.
    pub fn try_send(chan: GcRef, val: u64) -> Result<Option<FiberId>, u64> {
        let state = get_state(chan);
        let cap = capacity(chan);
        
        if state.closed {
            panic!("send on closed channel");
        }
        
        // If there's a waiting receiver, buffer the value and return receiver to unblock
        // The receiver will get the value from the buffer when it retries
        if let Some(receiver_id) = state.waiting_receivers.pop_front() {
            state.buffer.push_back(val);
            return Ok(Some(receiver_id));
        }
        
        // If buffer has space, buffer it
        if state.buffer.len() < cap {
            state.buffer.push_back(val);
            return Ok(None);
        }
        
        // Would block
        Err(val)
    }
    
    /// Try to receive a value. Returns Ok(Some(val)) if received, Ok(None) if closed, Err(()) if would block.
    pub fn try_recv(chan: GcRef) -> Result<Option<u64>, ()> {
        let state = get_state(chan);
        
        // Check buffer first
        if let Some(val) = state.buffer.pop_front() {
            // If there's a waiting sender, move their value to buffer
            if let Some((_, sender_val)) = state.waiting_senders.pop_front() {
                state.buffer.push_back(sender_val);
            }
            return Ok(Some(val));
        }
        
        // Check waiting senders (unbuffered case)
        if let Some((_, val)) = state.waiting_senders.pop_front() {
            return Ok(Some(val));
        }
        
        // Check if closed
        if state.closed {
            return Ok(None);
        }
        
        // Would block
        Err(())
    }
    
    /// Drop the internal state (must be called before GC frees the object).
    pub unsafe fn drop_inner(chan: GcRef) {
        let ptr = Gc::read_slot(chan, CHAN_PTR_SLOT) as *mut ChannelState;
        if !ptr.is_null() {
            drop(Box::from_raw(ptr));
            Gc::write_slot(chan, CHAN_PTR_SLOT, 0);
        }
    }
}

/// Closure object layout (after GcHeader):
/// - slot 0: func_id
/// - slot 1: upvalue_count
/// - slot 2+: upvalues
pub mod closure {
    use super::*;
    
    const FUNC_ID_SLOT: usize = 0;
    const UPVAL_COUNT_SLOT: usize = 1;
    const UPVAL_START: usize = 2;
    
    pub fn create(gc: &mut Gc, type_id: TypeId, func_id: u32, upvalue_count: usize) -> GcRef {
        let total_slots = UPVAL_START + upvalue_count;
        let closure = gc.alloc(type_id, total_slots);
        Gc::write_slot(closure, FUNC_ID_SLOT, func_id as u64);
        Gc::write_slot(closure, UPVAL_COUNT_SLOT, upvalue_count as u64);
        closure
    }
    
    pub fn func_id(closure: GcRef) -> u32 {
        Gc::read_slot(closure, FUNC_ID_SLOT) as u32
    }
    
    pub fn upvalue_count(closure: GcRef) -> usize {
        Gc::read_slot(closure, UPVAL_COUNT_SLOT) as usize
    }
    
    pub fn get_upvalue(closure: GcRef, idx: usize) -> u64 {
        debug_assert!(idx < upvalue_count(closure));
        Gc::read_slot(closure, UPVAL_START + idx)
    }
    
    pub fn set_upvalue(closure: GcRef, idx: usize, val: u64) {
        debug_assert!(idx < upvalue_count(closure));
        Gc::write_slot(closure, UPVAL_START + idx, val);
    }
    
    // === Upval Box (for reference capture semantics) ===
    // A single-slot heap object that holds a captured variable's value.
    // Multiple closures can share the same upval_box to see mutations.
    
    pub fn create_upval_box(gc: &mut Gc, type_id: TypeId) -> GcRef {
        let uv = gc.alloc(type_id, 1);
        Gc::write_slot(uv, 0, 0);
        uv
    }
    
    pub fn get_upval_box(uv: GcRef) -> u64 {
        Gc::read_slot(uv, 0)
    }
    
    pub fn set_upval_box(uv: GcRef, value: u64) {
        Gc::write_slot(uv, 0, value);
    }
}

/// Interface object layout (2 slots inline, not heap allocated):
/// - slot 0: type_id of boxed value
/// - slot 1: data (value or GcRef depending on type)
pub mod interface {
    use super::*;
    
    pub const SIZE_SLOTS: usize = 2;
    
    pub fn box_value(type_id: TypeId, data: u64) -> (u64, u64) {
        (type_id as u64, data)
    }
    
    pub fn unbox_type(slot0: u64) -> TypeId {
        slot0 as TypeId
    }
    
    pub fn unbox_data(slot1: u64) -> u64 {
        slot1
    }
    
    pub fn is_nil(slot0: u64, slot1: u64) -> bool {
        slot0 == 0 && slot1 == 0
    }
}
