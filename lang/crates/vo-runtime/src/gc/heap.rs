//! Island-owned block and span allocator for managed objects.
//!
//! Managed object addresses stay stable for their complete lifetime. Small
//! objects are cells in single-size-class blocks. Large objects occupy a
//! contiguous block run. System allocation only occurs while heap growth is
//! allowed.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec::Vec};
#[cfg(feature = "std")]
use std::{boxed::Box, vec::Vec};

use core::alloc::Layout;

#[cfg(not(feature = "std"))]
use alloc::alloc as heap_alloc;
#[cfg(feature = "std")]
use std::alloc as heap_alloc;

pub const HEAP_BLOCK_SIZE: usize = 64 * 1024;

const MIN_CLASS_SHIFT: usize = 4;
const MAX_CLASS_SHIFT: usize = 15;
const CLASS_COUNT: usize = MAX_CLASS_SHIFT - MIN_CLASS_SHIFT + 1;
pub(crate) const MIN_CELL_SIZE: usize = 1usize << MIN_CLASS_SHIFT;
const MAX_CELLS_PER_BLOCK: usize = HEAP_BLOCK_SIZE / MIN_CELL_SIZE;
const ALLOCATION_WORDS: usize = MAX_CELLS_PER_BLOCK.div_ceil(64);
const MAX_GROWTH_BLOCKS: usize = 256;
const FREE_CELL_NONE: u16 = u16::MAX;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HeapError {
    AllocationForbidden,
    GrowthDisabled,
    HardLimitExceeded,
    SystemAllocationFailed,
    InvalidPointer,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct HeapStats {
    pub committed_bytes: usize,
    pub allocated_span_bytes: usize,
    pub pending_reclaim_bytes: usize,
    pub segment_count: usize,
    pub block_count: usize,
    pub free_blocks: usize,
}

struct SmallBlock {
    class_index: u8,
    bump_cells: u16,
    free_head: u16,
    live_cells: u16,
    allocated: [u64; ALLOCATION_WORDS],
}

fn try_box<T>(value: T) -> Result<Box<T>, HeapError> {
    let layout = Layout::new::<T>();
    let raw = if layout.size() == 0 {
        core::ptr::NonNull::<T>::dangling().as_ptr()
    } else {
        let raw = unsafe { heap_alloc::alloc(layout) }.cast::<T>();
        if raw.is_null() {
            return Err(HeapError::SystemAllocationFailed);
        }
        raw
    };
    unsafe {
        raw.write(value);
        Ok(Box::from_raw(raw))
    }
}

impl SmallBlock {
    fn new(class_index: usize) -> Self {
        Self {
            class_index: class_index as u8,
            bump_cells: 0,
            free_head: FREE_CELL_NONE,
            live_cells: 0,
            allocated: [0; ALLOCATION_WORDS],
        }
    }

    #[inline]
    fn class_size(&self) -> usize {
        1usize << (MIN_CLASS_SHIFT + usize::from(self.class_index))
    }

    #[inline]
    fn cell_count(&self) -> usize {
        HEAP_BLOCK_SIZE / self.class_size()
    }

    #[inline]
    fn has_capacity(&self) -> bool {
        self.free_head != FREE_CELL_NONE || usize::from(self.bump_cells) < self.cell_count()
    }

    #[inline]
    fn is_allocated(&self, cell: usize) -> bool {
        let word = cell / 64;
        let bit = cell % 64;
        self.allocated[word] & (1u64 << bit) != 0
    }

    #[inline]
    fn set_allocated(&mut self, cell: usize, allocated: bool) {
        let word = cell / 64;
        let bit = cell % 64;
        if allocated {
            self.allocated[word] |= 1u64 << bit;
        } else {
            self.allocated[word] &= !(1u64 << bit);
        }
    }
}

enum BlockState {
    Free,
    Small(Box<SmallBlock>),
    LargeHead {
        blocks: u32,
        pending_reclaim: bool,
        reclaim_next: u32,
    },
    LargeTail {
        head: u32,
    },
}

struct HeapSegment {
    base: usize,
    layout: Layout,
    blocks: Box<[BlockState]>,
    free_blocks: usize,
}

impl HeapSegment {
    #[inline]
    fn byte_len(&self) -> usize {
        self.layout.size()
    }

    #[inline]
    fn contains(&self, address: usize) -> bool {
        address >= self.base && address < self.base + self.byte_len()
    }
}

impl Drop for HeapSegment {
    fn drop(&mut self) {
        unsafe {
            heap_alloc::dealloc(self.base as *mut u8, self.layout);
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Allocation {
    pub raw: *mut u8,
    pub capacity: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct LocatedAllocation {
    pub raw: *mut u8,
    pub capacity: usize,
}

pub struct SpanHeap {
    segments: Vec<HeapSegment>,
    /// Stable segment indices sorted by base address. Segments are never
    /// removed, so pointer canonicalization can use binary search without
    /// coupling allocator order to virtual-address order.
    segment_index_by_base: Vec<usize>,
    active_small: [Option<(usize, usize)>; CLASS_COUNT],
    partial_small: [Vec<(usize, usize)>; CLASS_COUNT],
    partial_index_complete: [bool; CLASS_COUNT],
    hard_limit_bytes: Option<usize>,
    growth_allowed: bool,
    allocation_allowed: bool,
    committed_bytes: usize,
    allocated_span_bytes: usize,
    pending_reclaim_bytes: usize,
    free_blocks: usize,
    next_growth_blocks: usize,
    reclaim_segment_cursor: usize,
    reclaim_block_cursor: usize,
    pending_large_spans: usize,
}

impl SpanHeap {
    pub fn new(hard_limit_bytes: Option<usize>) -> Self {
        Self {
            segments: Vec::new(),
            segment_index_by_base: Vec::new(),
            active_small: [None; CLASS_COUNT],
            partial_small: core::array::from_fn(|_| Vec::new()),
            partial_index_complete: [true; CLASS_COUNT],
            hard_limit_bytes,
            growth_allowed: true,
            allocation_allowed: true,
            committed_bytes: 0,
            allocated_span_bytes: 0,
            pending_reclaim_bytes: 0,
            free_blocks: 0,
            next_growth_blocks: 1,
            reclaim_segment_cursor: 0,
            reclaim_block_cursor: 0,
            pending_large_spans: 0,
        }
    }

    #[inline]
    pub fn growth_allowed(&self) -> bool {
        self.growth_allowed
    }

    #[inline]
    pub fn set_growth_allowed(&mut self, allowed: bool) {
        self.growth_allowed = allowed;
    }

    #[inline]
    pub fn allocation_allowed(&self) -> bool {
        self.allocation_allowed
    }

    #[inline]
    pub fn set_allocation_allowed(&mut self, allowed: bool) {
        self.allocation_allowed = allowed;
    }

    #[inline]
    pub fn hard_limit_bytes(&self) -> Option<usize> {
        self.hard_limit_bytes
    }

    /// Number of heap blocks already owned by this allocator.
    #[inline]
    pub fn committed_block_count(&self) -> usize {
        self.committed_bytes / HEAP_BLOCK_SIZE
    }

    /// Conservative upper bound for simultaneous non-empty allocations in the
    /// committed heap. GC objects always contain a header, and therefore every
    /// object consumes at least one minimum-size cell.
    #[inline]
    pub fn max_min_cell_allocations(&self) -> usize {
        self.committed_bytes / MIN_CELL_SIZE
    }

    pub fn set_hard_limit_bytes(&mut self, limit: Option<usize>) -> Result<(), HeapError> {
        if limit.is_some_and(|limit| limit < self.committed_bytes) {
            return Err(HeapError::HardLimitExceeded);
        }
        self.hard_limit_bytes = limit;
        Ok(())
    }

    pub fn reserve(&mut self, bytes: usize) -> Result<usize, HeapError> {
        if bytes == 0 {
            return Ok(self.committed_bytes);
        }
        if !self.growth_allowed {
            return Err(HeapError::GrowthDisabled);
        }
        let blocks = bytes.div_ceil(HEAP_BLOCK_SIZE);
        self.add_segment(blocks)?;
        Ok(self.committed_bytes)
    }

    pub fn allocate(&mut self, size: usize) -> Result<Allocation, HeapError> {
        if !self.allocation_allowed {
            return Err(HeapError::AllocationForbidden);
        }
        if let Some((class_index, class_size)) = allocation_class(size) {
            self.allocate_small(class_index, class_size)
        } else {
            self.allocate_large(size)
        }
    }

    fn allocate_small(
        &mut self,
        class_index: usize,
        class_size: usize,
    ) -> Result<Allocation, HeapError> {
        let (segment_index, block_index) =
            if let Some((segment_index, block_index)) = self.active_small[class_index] {
                let usable = matches!(
                    &self.segments[segment_index].blocks[block_index],
                    BlockState::Small(block) if block.has_capacity()
                );
                if usable {
                    (segment_index, block_index)
                } else {
                    self.active_small[class_index] = None;
                    self.find_or_create_small_block(class_index)?
                }
            } else {
                self.find_or_create_small_block(class_index)?
            };

        let segment_base = self.segments[segment_index].base;
        let block_base = segment_base + block_index * HEAP_BLOCK_SIZE;
        let block = match &mut self.segments[segment_index].blocks[block_index] {
            BlockState::Small(block) => block,
            _ => unreachable!("active small block must retain its size class"),
        };

        let cell = if block.free_head != FREE_CELL_NONE {
            let cell = usize::from(block.free_head);
            let cell_ptr = (block_base + cell * class_size) as *const u16;
            block.free_head = unsafe { cell_ptr.read_unaligned() };
            cell
        } else {
            let cell = usize::from(block.bump_cells);
            block.bump_cells += 1;
            cell
        };

        debug_assert!(!block.is_allocated(cell));
        block.set_allocated(cell, true);
        block.live_cells += 1;
        if !block.has_capacity() {
            self.active_small[class_index] = None;
        }

        let raw = (block_base + cell * class_size) as *mut u8;
        unsafe {
            raw.write_bytes(0, class_size);
        }
        self.allocated_span_bytes += class_size;
        Ok(Allocation {
            raw,
            capacity: class_size,
        })
    }

    fn find_or_create_small_block(
        &mut self,
        class_index: usize,
    ) -> Result<(usize, usize), HeapError> {
        while let Some((segment_index, block_index)) = self.partial_small[class_index].pop() {
            if matches!(
                self.segments
                    .get(segment_index)
                    .and_then(|segment| segment.blocks.get(block_index)),
                Some(BlockState::Small(block))
                    if usize::from(block.class_index) == class_index && block.has_capacity()
            ) {
                self.active_small[class_index] = Some((segment_index, block_index));
                return Ok((segment_index, block_index));
            }
        }
        if !self.partial_index_complete[class_index] {
            for (segment_index, segment) in self.segments.iter().enumerate() {
                for (block_index, state) in segment.blocks.iter().enumerate() {
                    if matches!(
                        state,
                        BlockState::Small(block)
                            if usize::from(block.class_index) == class_index && block.has_capacity()
                    ) {
                        self.active_small[class_index] = Some((segment_index, block_index));
                        return Ok((segment_index, block_index));
                    }
                }
            }
            self.partial_index_complete[class_index] = true;
        }

        let block = try_box(SmallBlock::new(class_index))?;
        let (segment_index, block_index) = self.acquire_free_run(1)?;
        self.free_blocks -= 1;
        self.segments[segment_index].free_blocks -= 1;
        self.segments[segment_index].blocks[block_index] = BlockState::Small(block);
        self.active_small[class_index] = Some((segment_index, block_index));
        Ok((segment_index, block_index))
    }

    fn allocate_large(&mut self, size: usize) -> Result<Allocation, HeapError> {
        let blocks = size.div_ceil(HEAP_BLOCK_SIZE).max(1);
        let (segment_index, head) = self.acquire_free_run(blocks)?;
        self.free_blocks -= blocks;
        self.segments[segment_index].free_blocks -= blocks;
        self.segments[segment_index].blocks[head] = BlockState::LargeHead {
            blocks: blocks as u32,
            pending_reclaim: false,
            reclaim_next: 1,
        };
        for block in 1..blocks {
            self.segments[segment_index].blocks[head + block] =
                BlockState::LargeTail { head: head as u32 };
        }

        let raw = (self.segments[segment_index].base + head * HEAP_BLOCK_SIZE) as *mut u8;
        let capacity = blocks * HEAP_BLOCK_SIZE;
        unsafe {
            raw.write_bytes(0, size);
        }
        self.allocated_span_bytes += capacity;
        Ok(Allocation { raw, capacity })
    }

    fn acquire_free_run(&mut self, blocks: usize) -> Result<(usize, usize), HeapError> {
        if let Some(found) = self.find_free_run(blocks) {
            return Ok(found);
        }
        if !self.growth_allowed {
            return Err(HeapError::GrowthDisabled);
        }
        let growth_blocks = blocks.max(self.next_growth_blocks);
        self.add_segment(growth_blocks)?;
        self.next_growth_blocks = growth_blocks.saturating_mul(2).clamp(1, MAX_GROWTH_BLOCKS);
        self.find_free_run(blocks)
            .ok_or(HeapError::SystemAllocationFailed)
    }

    fn find_free_run(&self, blocks: usize) -> Option<(usize, usize)> {
        for (segment_index, segment) in self.segments.iter().enumerate() {
            if segment.free_blocks < blocks {
                continue;
            }
            let mut run_start = 0usize;
            let mut run_len = 0usize;
            for (block_index, state) in segment.blocks.iter().enumerate() {
                if matches!(state, BlockState::Free) {
                    if run_len == 0 {
                        run_start = block_index;
                    }
                    run_len += 1;
                    if run_len >= blocks {
                        return Some((segment_index, run_start));
                    }
                } else {
                    run_len = 0;
                }
            }
        }
        None
    }

    fn add_segment(&mut self, blocks: usize) -> Result<(), HeapError> {
        let bytes = blocks
            .checked_mul(HEAP_BLOCK_SIZE)
            .ok_or(HeapError::HardLimitExceeded)?;
        let next_committed = self
            .committed_bytes
            .checked_add(bytes)
            .ok_or(HeapError::HardLimitExceeded)?;
        if self
            .hard_limit_bytes
            .is_some_and(|limit| next_committed > limit)
        {
            return Err(HeapError::HardLimitExceeded);
        }
        let layout = Layout::from_size_align(bytes, HEAP_BLOCK_SIZE)
            .map_err(|_| HeapError::SystemAllocationFailed)?;
        self.segments
            .try_reserve(1)
            .map_err(|_| HeapError::SystemAllocationFailed)?;
        self.segment_index_by_base
            .try_reserve(1)
            .map_err(|_| HeapError::SystemAllocationFailed)?;
        let mut block_states = Vec::new();
        block_states
            .try_reserve_exact(blocks)
            .map_err(|_| HeapError::SystemAllocationFailed)?;
        block_states.resize_with(blocks, || BlockState::Free);
        let raw = unsafe { heap_alloc::alloc_zeroed(layout) };
        if raw.is_null() {
            return Err(HeapError::SystemAllocationFailed);
        }
        let base = raw as usize;
        let segment_index = self.segments.len();
        self.segments.push(HeapSegment {
            base,
            layout,
            blocks: block_states.into_boxed_slice(),
            free_blocks: blocks,
        });
        let position = self
            .segment_index_by_base
            .partition_point(|index| self.segments[*index].base < base);
        self.segment_index_by_base.insert(position, segment_index);
        self.committed_bytes = next_committed;
        self.free_blocks += bytes / HEAP_BLOCK_SIZE;
        Ok(())
    }

    pub fn locate(&self, address: usize, header_size: usize) -> Option<LocatedAllocation> {
        let (segment_index, block_index) = self.locate_block(address)?;
        let segment = &self.segments[segment_index];
        let block_base = segment.base + block_index * HEAP_BLOCK_SIZE;
        match &segment.blocks[block_index] {
            BlockState::Free => None,
            BlockState::Small(block) => {
                let class_size = block.class_size();
                let cell = (address - block_base) / class_size;
                if cell >= block.cell_count() || !block.is_allocated(cell) {
                    return None;
                }
                let raw = block_base + cell * class_size;
                (address >= raw + header_size && address < raw + class_size).then_some(
                    LocatedAllocation {
                        raw: raw as *mut u8,
                        capacity: class_size,
                    },
                )
            }
            BlockState::LargeHead {
                blocks,
                pending_reclaim,
                ..
            } => {
                if *pending_reclaim {
                    return None;
                }
                let capacity = *blocks as usize * HEAP_BLOCK_SIZE;
                (address >= block_base + header_size && address < block_base + capacity).then_some(
                    LocatedAllocation {
                        raw: block_base as *mut u8,
                        capacity,
                    },
                )
            }
            BlockState::LargeTail { head } => {
                let head = *head as usize;
                let head_base = segment.base + head * HEAP_BLOCK_SIZE;
                let BlockState::LargeHead {
                    blocks,
                    pending_reclaim,
                    ..
                } = &segment.blocks[head]
                else {
                    return None;
                };
                if *pending_reclaim {
                    return None;
                }
                let capacity = *blocks as usize * HEAP_BLOCK_SIZE;
                (address >= head_base + header_size && address < head_base + capacity).then_some(
                    LocatedAllocation {
                        raw: head_base as *mut u8,
                        capacity,
                    },
                )
            }
        }
    }

    fn locate_block(&self, address: usize) -> Option<(usize, usize)> {
        let position = self
            .segment_index_by_base
            .partition_point(|index| self.segments[*index].base <= address)
            .checked_sub(1)?;
        let segment_index = self.segment_index_by_base[position];
        let segment = &self.segments[segment_index];
        segment
            .contains(address)
            .then(|| (segment_index, (address - segment.base) / HEAP_BLOCK_SIZE))
    }

    pub fn free(&mut self, raw: *mut u8) -> Result<(), HeapError> {
        let address = raw as usize;
        let (segment_index, block_index) = self
            .locate_block(address)
            .ok_or(HeapError::InvalidPointer)?;
        let segment_base = self.segments[segment_index].base;
        let block_base = segment_base + block_index * HEAP_BLOCK_SIZE;

        match &mut self.segments[segment_index].blocks[block_index] {
            BlockState::Small(block) => {
                let class_index = usize::from(block.class_index);
                let class_size = block.class_size();
                let was_full = !block.has_capacity();
                let offset = address
                    .checked_sub(block_base)
                    .ok_or(HeapError::InvalidPointer)?;
                if offset % class_size != 0 {
                    return Err(HeapError::InvalidPointer);
                }
                let cell = offset / class_size;
                if cell >= block.cell_count() || !block.is_allocated(cell) {
                    return Err(HeapError::InvalidPointer);
                }
                block.set_allocated(cell, false);
                block.live_cells -= 1;
                unsafe {
                    (address as *mut u16).write_unaligned(block.free_head);
                }
                block.free_head = cell as u16;
                self.allocated_span_bytes -= class_size;
                if block.live_cells == 0 {
                    self.segments[segment_index].blocks[block_index] = BlockState::Free;
                    self.free_blocks += 1;
                    self.segments[segment_index].free_blocks += 1;
                    if self.active_small[class_index] == Some((segment_index, block_index)) {
                        self.active_small[class_index] = None;
                    }
                } else if self.active_small[class_index].is_none() {
                    self.active_small[class_index] = Some((segment_index, block_index));
                } else if was_full {
                    let partial = &mut self.partial_small[class_index];
                    if partial.try_reserve(1).is_ok() {
                        partial.push((segment_index, block_index));
                    } else {
                        self.partial_index_complete[class_index] = false;
                    }
                }
                Ok(())
            }
            BlockState::LargeHead {
                blocks,
                pending_reclaim,
                reclaim_next,
                ..
            } if address == block_base && !*pending_reclaim => {
                let capacity = *blocks as usize * HEAP_BLOCK_SIZE;
                *pending_reclaim = true;
                *reclaim_next = 1;
                self.pending_large_spans += 1;
                self.pending_reclaim_bytes += capacity;
                self.allocated_span_bytes -= capacity;
                Ok(())
            }
            _ => Err(HeapError::InvalidPointer),
        }
    }

    pub fn reclaim_step(&mut self, max_blocks: usize) -> (usize, bool) {
        if self.pending_large_spans == 0 || max_blocks == 0 {
            return (0, self.pending_large_spans == 0);
        }
        let mut work = 0usize;

        while work < max_blocks && self.pending_large_spans > 0 {
            if self.reclaim_segment_cursor >= self.segments.len() {
                self.reclaim_segment_cursor = 0;
                self.reclaim_block_cursor = 0;
            }
            let segment_index = self.reclaim_segment_cursor;
            if self.reclaim_block_cursor >= self.segments[segment_index].blocks.len() {
                self.reclaim_segment_cursor += 1;
                self.reclaim_block_cursor = 0;
                continue;
            }

            let head = self.reclaim_block_cursor;
            work += 1;
            let pending = matches!(
                self.segments[segment_index].blocks[head],
                BlockState::LargeHead {
                    pending_reclaim: true,
                    ..
                }
            );
            if !pending {
                self.reclaim_block_cursor += 1;
                continue;
            }

            let (blocks, mut reclaim_next) = match self.segments[segment_index].blocks[head] {
                BlockState::LargeHead {
                    blocks,
                    reclaim_next,
                    ..
                } => (blocks as usize, reclaim_next as usize),
                _ => unreachable!(),
            };
            while reclaim_next < blocks && work < max_blocks {
                self.segments[segment_index].blocks[head + reclaim_next] = BlockState::Free;
                self.free_blocks += 1;
                self.segments[segment_index].free_blocks += 1;
                reclaim_next += 1;
                work += 1;
            }
            if reclaim_next == blocks {
                self.segments[segment_index].blocks[head] = BlockState::Free;
                self.free_blocks += 1;
                self.segments[segment_index].free_blocks += 1;
                self.pending_large_spans -= 1;
                self.pending_reclaim_bytes -= blocks * HEAP_BLOCK_SIZE;
                self.reclaim_block_cursor += 1;
            } else if let BlockState::LargeHead {
                reclaim_next: cursor,
                ..
            } = &mut self.segments[segment_index].blocks[head]
            {
                *cursor = reclaim_next as u32;
            }
        }

        (work, self.pending_large_spans == 0)
    }

    pub fn stats(&self) -> HeapStats {
        HeapStats {
            committed_bytes: self.committed_bytes,
            allocated_span_bytes: self.allocated_span_bytes,
            pending_reclaim_bytes: self.pending_reclaim_bytes,
            segment_count: self.segments.len(),
            block_count: self.committed_block_count(),
            free_blocks: self.free_blocks,
        }
    }
}

#[inline]
fn allocation_class(size: usize) -> Option<(usize, usize)> {
    if size == 0 || size > (1usize << MAX_CLASS_SHIFT) {
        return None;
    }
    let class_size = size.max(1usize << MIN_CLASS_SHIFT).next_power_of_two();
    let shift = class_size.trailing_zeros() as usize;
    Some((shift - MIN_CLASS_SHIFT, class_size))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_allocations_reuse_cells_and_keep_stable_addresses() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        heap.reserve(HEAP_BLOCK_SIZE).unwrap();
        heap.set_growth_allowed(false);

        let first = heap.allocate(24).unwrap();
        let second = heap.allocate(24).unwrap();
        assert_ne!(first.raw, second.raw);
        heap.free(first.raw).unwrap();
        let reused = heap.allocate(24).unwrap();
        assert_eq!(reused.raw, first.raw);
        assert_eq!(reused.capacity, 32);
    }

    #[test]
    fn full_small_blocks_reenter_allocation_without_segment_rescan() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE * 2));
        heap.reserve(HEAP_BLOCK_SIZE * 2).unwrap();
        heap.set_growth_allowed(false);
        let cells_per_block = HEAP_BLOCK_SIZE / 1024;
        let allocations: Vec<_> = (0..cells_per_block * 2)
            .map(|_| heap.allocate(1024).expect("fill two small blocks"))
            .collect();

        heap.free(allocations[0].raw).unwrap();
        heap.free(allocations[cells_per_block].raw).unwrap();
        assert_eq!(heap.partial_small[6].len(), 1);

        assert_eq!(
            heap.allocate(1024).expect("reuse active block").raw,
            allocations[0].raw
        );
        assert_eq!(
            heap.allocate(1024)
                .expect("reuse indexed partial block")
                .raw,
            allocations[cells_per_block].raw
        );
        assert!(heap.partial_small[6].is_empty());
    }

    #[test]
    fn incomplete_partial_index_falls_back_without_losing_free_cells() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE * 2));
        heap.reserve(HEAP_BLOCK_SIZE * 2).unwrap();
        heap.set_growth_allowed(false);
        let cells_per_block = HEAP_BLOCK_SIZE / 1024;
        let allocations: Vec<_> = (0..cells_per_block * 2)
            .map(|_| heap.allocate(1024).expect("fill two small blocks"))
            .collect();

        heap.free(allocations[0].raw).unwrap();
        heap.free(allocations[cells_per_block].raw).unwrap();
        heap.partial_small[6].clear();
        heap.partial_index_complete[6] = false;

        assert_eq!(heap.allocate(1024).unwrap().raw, allocations[0].raw);
        assert_eq!(
            heap.allocate(1024)
                .expect("slow fallback must recover an unindexed partial block")
                .raw,
            allocations[cells_per_block].raw
        );
        assert!(!heap.partial_index_complete[6]);
    }

    #[test]
    fn no_growth_and_hard_limit_fail_closed() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        heap.set_growth_allowed(false);
        assert!(matches!(heap.allocate(8), Err(HeapError::GrowthDisabled)));

        heap.set_growth_allowed(true);
        heap.reserve(HEAP_BLOCK_SIZE).unwrap();
        assert_eq!(
            heap.reserve(HEAP_BLOCK_SIZE),
            Err(HeapError::HardLimitExceeded)
        );
    }

    #[test]
    fn sub_block_reserve_reports_every_min_cell_allocation() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        assert_eq!(heap.reserve(1), Ok(HEAP_BLOCK_SIZE));
        assert_eq!(MIN_CELL_SIZE, 16);
        assert_eq!(
            heap.max_min_cell_allocations(),
            HEAP_BLOCK_SIZE / MIN_CELL_SIZE
        );
        heap.set_growth_allowed(false);

        for _ in 0..heap.max_min_cell_allocations() {
            assert_eq!(heap.allocate(8).expect("minimum cell").capacity, 16);
        }
        assert!(matches!(heap.allocate(8), Err(HeapError::GrowthDisabled)));
    }

    #[test]
    fn large_reclaim_is_block_bounded() {
        let mut heap = SpanHeap::new(None);
        let allocation = heap.allocate(HEAP_BLOCK_SIZE * 3).unwrap();
        heap.free(allocation.raw).unwrap();
        assert_eq!(heap.stats().pending_reclaim_bytes, HEAP_BLOCK_SIZE * 3);

        let (first_work, first_done) = heap.reclaim_step(1);
        assert_eq!(first_work, 1);
        assert!(!first_done);
        let (second_work, second_done) = heap.reclaim_step(3);
        assert!(second_work <= 3);
        assert!(second_done);
        assert_eq!(heap.stats().pending_reclaim_bytes, 0);
    }

    #[test]
    fn locate_canonicalizes_small_and_large_interiors() {
        let mut heap = SpanHeap::new(None);
        let small = heap.allocate(64).unwrap();
        let small_located = heap.locate(small.raw as usize + 24, 8).unwrap();
        assert_eq!(small_located.raw, small.raw);

        let large = heap.allocate(HEAP_BLOCK_SIZE + 64).unwrap();
        let large_located = heap
            .locate(large.raw as usize + HEAP_BLOCK_SIZE + 16, 8)
            .unwrap();
        assert_eq!(large_located.raw, large.raw);
    }

    #[test]
    fn segment_address_index_locates_many_independent_segments() {
        let mut heap = SpanHeap::new(None);
        for _ in 0..16 {
            heap.reserve(HEAP_BLOCK_SIZE).unwrap();
        }
        let allocations: Vec<_> = (0..16)
            .map(|_| heap.allocate(HEAP_BLOCK_SIZE).unwrap())
            .collect();

        assert!(heap
            .segment_index_by_base
            .windows(2)
            .all(|pair| { heap.segments[pair[0]].base < heap.segments[pair[1]].base }));
        for allocation in allocations {
            let located = heap
                .locate(allocation.raw as usize + HEAP_BLOCK_SIZE - 1, 8)
                .expect("interior address must resolve through segment index");
            assert_eq!(located.raw, allocation.raw);
        }
    }
}
