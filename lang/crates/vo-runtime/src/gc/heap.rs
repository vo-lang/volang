//! Island-owned block and span allocator for managed objects.
//!
//! Managed object addresses stay stable for their complete lifetime. Small
//! objects are cells in single-size-class blocks. Large objects occupy a
//! contiguous block run. New managed pages are acquired only while heap growth
//! is allowed; block metadata belongs to the host allocation domain.

#[cfg(not(feature = "std"))]
use alloc::{boxed::Box, vec::Vec};
#[cfg(feature = "std")]
use std::{boxed::Box, vec::Vec};

use core::alloc::Layout;
use core::ptr::NonNull;

#[cfg(not(feature = "std"))]
use alloc::alloc as heap_alloc;
#[cfg(feature = "std")]
use std::alloc as heap_alloc;

pub const HEAP_BLOCK_SIZE: usize = 64 * 1024;

const MIN_CLASS_SHIFT: usize = 4;
const MAX_CLASS_SHIFT: usize = 15;
pub(super) const CLASS_COUNT: usize = MAX_CLASS_SHIFT - MIN_CLASS_SHIFT + 1;
pub(crate) const MIN_CELL_SIZE: usize = 1usize << MIN_CLASS_SHIFT;
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
    allocated: Box<[u64]>,
    /// Exact requested bytes per cell. This fixed block-local directory lets
    /// pointer validation reject a corrupted header even when the forged size
    /// still fits inside the cell's size class.
    logical_sizes: Box<[u16]>,
    remembered: Box<[u64]>,
    remembered_cells: u16,
    logical_bytes: usize,
    old_cells: u16,
    finalizable_cells: u16,
    runtime_backing_bytes: usize,
    marked_cycle: u64,
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
    fn try_new(class_index: usize) -> Result<Self, HeapError> {
        let class_size = 1usize << (MIN_CLASS_SHIFT + class_index);
        let cell_count = HEAP_BLOCK_SIZE / class_size;
        let word_count = cell_count.div_ceil(64);
        let mut allocated = Vec::new();
        allocated
            .try_reserve_exact(word_count)
            .map_err(|_| HeapError::SystemAllocationFailed)?;
        allocated.resize(word_count, 0);
        let mut logical_sizes = Vec::new();
        logical_sizes
            .try_reserve_exact(cell_count)
            .map_err(|_| HeapError::SystemAllocationFailed)?;
        logical_sizes.resize(cell_count, 0);
        let mut remembered = Vec::new();
        remembered
            .try_reserve_exact(word_count)
            .map_err(|_| HeapError::SystemAllocationFailed)?;
        remembered.resize(word_count, 0);
        Ok(Self::with_storage(
            class_index as u8,
            allocated.into_boxed_slice(),
            logical_sizes.into_boxed_slice(),
            remembered.into_boxed_slice(),
        ))
    }

    fn with_storage(
        class_index: u8,
        allocated: Box<[u64]>,
        logical_sizes: Box<[u16]>,
        remembered: Box<[u64]>,
    ) -> Self {
        Self {
            class_index,
            bump_cells: 0,
            free_head: FREE_CELL_NONE,
            live_cells: 0,
            allocated,
            logical_sizes,
            remembered,
            remembered_cells: 0,
            logical_bytes: 0,
            old_cells: 0,
            finalizable_cells: 0,
            runtime_backing_bytes: 0,
            marked_cycle: 0,
        }
    }

    fn reset(&mut self) {
        self.allocated.fill(0);
        self.remembered.fill(0);
        // Only allocated cells expose a logical extent. Every ordinary and
        // native-lane allocation publishes a fresh size before it is visible.
        // Stale sizes in unallocated cells therefore need no clearing.
        *self = Self::with_storage(
            self.class_index,
            core::mem::take(&mut self.allocated),
            core::mem::take(&mut self.logical_sizes),
            core::mem::take(&mut self.remembered),
        );
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

    #[inline]
    fn is_remembered(&self, cell: usize) -> bool {
        let word = cell / 64;
        let bit = cell % 64;
        self.remembered[word] & (1u64 << bit) != 0
    }

    #[inline]
    fn set_remembered(&mut self, cell: usize, remembered: bool) {
        let word = cell / 64;
        let bit = cell % 64;
        if remembered {
            self.remembered[word] |= 1u64 << bit;
        } else {
            self.remembered[word] &= !(1u64 << bit);
        }
    }
}

enum BlockState {
    Free,
    Small(Box<SmallBlock>),
    LargeHead {
        blocks: u32,
        logical_bytes: usize,
        pending_reclaim: bool,
        reclaim_next: u32,
        remembered: bool,
    },
    LargeTail {
        head: u32,
    },
}

struct HeapSegment {
    base: usize,
    layout: Layout,
    blocks: Box<[BlockState]>,
    /// One summary bit per heap block. This lets a minor collection find the
    /// sparse set of remembered blocks without walking every object block.
    remembered_blocks: Box<[u64]>,
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
    raw: NonNull<u8>,
    pub capacity: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct LocatedAllocation {
    raw: NonNull<u8>,
    pub capacity: usize,
    pub logical_bytes: usize,
}

// Successful allocations and range lookups always identify live segment
// storage. Carry that fact in the type so Result/Option reuse the null niche
// instead of adding a separate tag and a larger stack return buffer.
impl Allocation {
    #[inline]
    pub fn as_ptr(self) -> *mut u8 {
        self.raw.as_ptr()
    }
}

impl LocatedAllocation {
    #[inline]
    pub fn as_ptr(self) -> *mut u8 {
        self.raw.as_ptr()
    }
}

const _: () = assert!(
    core::mem::size_of::<Result<Allocation, HeapError>>() == 2 * core::mem::size_of::<usize>()
);
const _: () =
    assert!(core::mem::size_of::<Option<LocatedAllocation>>() == 3 * core::mem::size_of::<usize>());

/// Runtime-owned pointers for a single-mutator bump lane. The lane covers
/// fresh cells in one allocation-bitmap word; collection invalidates every
/// lane before it can mutate the referenced block metadata.
#[derive(Debug, Clone, Copy)]
#[cfg(any(test, not(feature = "gc-debug")))]
pub struct HeapBumpLane {
    pub cursor: *mut u8,
    pub limit: *mut u8,
    pub bitmap_word: *mut u64,
    pub logical_size_cursor: *mut u16,
    pub live_cells: *mut u16,
    pub logical_bytes: *mut usize,
    pub class_size: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg(any(test, not(feature = "gc-debug")))]
pub struct BulkReclaim {
    pub object_count: usize,
    pub logical_bytes: usize,
    pub runtime_backing_bytes: usize,
}

/// Persistent, snapshot-bounded cursor over heap allocation metadata.
///
/// A cursor never follows segments added after it was created. Each call to a
/// `walk_*_step` method examines at most one allocation-bitmap word or one
/// block-state record, so collector work remains externally budgetable.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct HeapObjectCursor {
    segment_index: usize,
    block_index: usize,
    // A block has at most HEAP_BLOCK_SIZE / MIN_CELL_SIZE (4096) cells.
    cell_index: u16,
    block_has_object: bool,
    segment_end: usize,
}

// Keep the bounded cell index and first-object flag in the original word so
// adding traversal state does not move generated-code-visible Gc fields.
const _: () =
    assert!(core::mem::size_of::<HeapObjectCursor>() == 4 * core::mem::size_of::<usize>());
const _: () = assert!(HEAP_BLOCK_SIZE / MIN_CELL_SIZE <= u16::MAX as usize);

#[derive(Debug, Clone, Copy)]
struct BlockLocation {
    segment: usize,
    block: usize,
}

/// A heap walk's allocation and already resolved block. Consume it in the same
/// collector step, before any mutator can free or reuse the allocation. No heap
/// metadata pointer is retained across a yield or metadata-vector growth.
#[derive(Debug, Clone, Copy)]
pub struct WalkedAllocation {
    pub allocation: LocatedAllocation,
    first_in_block: bool,
    location: BlockLocation,
}

#[derive(Debug, Clone, Copy)]
pub enum HeapWalkStep {
    Object(WalkedAllocation),
    Metadata,
    Done,
}

#[repr(C)]
pub struct SpanHeap {
    segments: Vec<HeapSegment>,
    /// Stable segment indices sorted by base address. Segments are never
    /// removed, so pointer canonicalization can use binary search without
    /// coupling allocator order to virtual-address order.
    segment_index_by_base: Vec<usize>,
    active_small: [Option<(usize, usize)>; CLASS_COUNT],
    partial_small: [Vec<(usize, usize)>; CLASS_COUNT],
    partial_index_complete: [bool; CLASS_COUNT],
    /// At most one detached metadata allocation per class. No managed block
    /// or object is retained; reuse clears bitmaps outside collector steps.
    spare_small: [Option<Box<SmallBlock>>; CLASS_COUNT],
    hard_limit_bytes: Option<usize>,
    growth_allowed: bool,
    allocation_allowed: bool,
    committed_bytes: usize,
    pub(super) allocated_span_bytes: usize,
    pending_reclaim_bytes: usize,
    free_blocks: usize,
    next_growth_blocks: usize,
    reclaim_segment_cursor: usize,
    reclaim_block_cursor: usize,
    pending_large_spans: usize,
    remembered_objects: usize,
}

impl SpanHeap {
    pub fn new(hard_limit_bytes: Option<usize>) -> Self {
        Self {
            segments: Vec::new(),
            segment_index_by_base: Vec::new(),
            active_small: [None; CLASS_COUNT],
            partial_small: core::array::from_fn(|_| Vec::new()),
            partial_index_complete: [true; CLASS_COUNT],
            spare_small: core::array::from_fn(|_| None),
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
            remembered_objects: 0,
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

    /// Allocate and publish block-local size, finalizer and mark accounting.
    /// The selected block is already known here; callers need no second lookup.
    /// `cycle_id` is the collector's monotonically advancing cycle generation.
    pub fn allocate(
        &mut self,
        size: usize,
        finalizable: bool,
        cycle_id: u64,
    ) -> Result<Allocation, HeapError> {
        if !self.allocation_allowed {
            return Err(HeapError::AllocationForbidden);
        }
        if let Some((class_index, class_size)) = allocation_class(size) {
            self.allocate_small(class_index, class_size, size, finalizable, cycle_id)
        } else {
            self.allocate_large(size)
        }
    }

    /// Reserve fresh, contiguous small cells for inline allocation. The
    /// cells remain absent from the allocation bitmap until generated code
    /// commits each object, so unconsumed lane capacity is never observable as
    /// a managed allocation.
    #[cfg(any(test, not(feature = "gc-debug")))]
    pub fn reserve_bump_lane(
        &mut self,
        size: usize,
        max_cells: usize,
    ) -> Result<Option<HeapBumpLane>, HeapError> {
        if !self.allocation_allowed {
            return Err(HeapError::AllocationForbidden);
        }
        let Some((class_index, class_size)) = allocation_class(size) else {
            return Ok(None);
        };
        if max_cells == 0 {
            return Ok(None);
        }

        let existing = self.active_small[class_index].filter(|(segment_index, block_index)| {
            matches!(
                &self.segments[*segment_index].blocks[*block_index],
                BlockState::Small(block)
                    if usize::from(block.bump_cells) < block.cell_count()
            )
        });
        let (segment_index, block_index) = if let Some(existing) = existing {
            existing
        } else {
            self.create_small_block(class_index)?
        };

        let block_base = self.segments[segment_index].base + block_index * HEAP_BLOCK_SIZE;
        let block = match &mut self.segments[segment_index].blocks[block_index] {
            BlockState::Small(block) => block.as_mut(),
            _ => unreachable!("bump lane must reference a small block"),
        };
        let start = usize::from(block.bump_cells);
        let word_end = ((start / 64) + 1).saturating_mul(64);
        let end = start
            .saturating_add(max_cells)
            .min(word_end)
            .min(block.cell_count());
        if end <= start {
            return Ok(None);
        }
        block.bump_cells = end as u16;
        if !block.has_capacity() {
            self.active_small[class_index] = None;
        }

        let cursor = (block_base + start * class_size) as *mut u8;
        let limit = (block_base + end * class_size) as *mut u8;
        unsafe {
            cursor.write_bytes(0, (end - start) * class_size);
        }
        let word = start / 64;
        Ok(Some(HeapBumpLane {
            cursor,
            limit,
            bitmap_word: unsafe { block.allocated.as_mut_ptr().add(word) },
            logical_size_cursor: unsafe { block.logical_sizes.as_mut_ptr().add(start) },
            live_cells: &mut block.live_cells,
            logical_bytes: &mut block.logical_bytes,
            class_size,
        }))
    }

    pub fn release_bump_lane(&mut self, cursor: *mut u8, limit: *mut u8) {
        if cursor.is_null() || limit.is_null() || cursor >= limit {
            return;
        }
        let Some((segment_index, block_index)) = self.locate_block(cursor as usize) else {
            return;
        };
        let block_base = self.segments[segment_index].base + block_index * HEAP_BLOCK_SIZE;
        let class_index = match &mut self.segments[segment_index].blocks[block_index] {
            BlockState::Small(block) => {
                let block = block.as_mut();
                let class_size = block.class_size();
                let start = (cursor as usize).saturating_sub(block_base) / class_size;
                let end = (limit as usize).saturating_sub(block_base) / class_size;
                if end != usize::from(block.bump_cells)
                    || start > end
                    || (start..end).any(|cell| block.is_allocated(cell))
                {
                    return;
                }
                block.bump_cells = start as u16;
                usize::from(block.class_index)
            }
            _ => return,
        };
        self.active_small[class_index] = Some((segment_index, block_index));
    }

    /// Account cells admitted to an allocation region before the mutator
    /// publishes their bitmap bits. A region is closed before any heap walk;
    /// unused cells are refunded through the matching release method.
    #[cfg(not(feature = "gc-debug"))]
    pub(super) fn admit_region_cells(&mut self, cells: usize, class_size: usize) {
        self.allocated_span_bytes = self
            .allocated_span_bytes
            .saturating_add(cells.saturating_mul(class_size));
    }

    pub(super) fn refund_region_cells(&mut self, cells: usize, class_size: usize) {
        self.allocated_span_bytes = self
            .allocated_span_bytes
            .saturating_sub(cells.saturating_mul(class_size));
    }

    fn allocate_small(
        &mut self,
        class_index: usize,
        class_size: usize,
        logical_bytes: usize,
        finalizable: bool,
        cycle_id: u64,
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
            BlockState::Small(block) => block.as_mut(),
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
        debug_assert!(logical_bytes <= u16::MAX as usize);
        block.set_allocated(cell, true);
        block.logical_sizes[cell] = logical_bytes as u16;
        block.live_cells += 1;
        // Allocations are live in the current cycle. While paused, this is the
        // completed cycle's ID; the next cycle advances it before tracing.
        block.marked_cycle = cycle_id;
        block.logical_bytes = block.logical_bytes.saturating_add(logical_bytes);
        block.finalizable_cells = block
            .finalizable_cells
            .saturating_add(u16::from(finalizable));
        if !block.has_capacity() {
            self.active_small[class_index] = None;
        }

        let raw = (block_base + cell * class_size) as *mut u8;
        // Statically sized clearing keeps the common small cells inline;
        // larger classes retain the target's bulk clearing implementation.
        unsafe {
            match class_size {
                16 => raw.cast::<[u8; 16]>().write([0; 16]),
                32 => raw.cast::<[u8; 32]>().write([0; 32]),
                64 => raw.cast::<[u8; 64]>().write([0; 64]),
                128 => raw.cast::<[u8; 128]>().write([0; 128]),
                _ => raw.write_bytes(0, class_size),
            }
        }
        self.allocated_span_bytes += class_size;
        Ok(Allocation {
            // The cell belongs to a live, non-null heap segment.
            raw: unsafe { NonNull::new_unchecked(raw) },
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

        self.create_small_block(class_index)
    }

    fn create_small_block(&mut self, class_index: usize) -> Result<(usize, usize), HeapError> {
        let block = match self.spare_small[class_index].take() {
            Some(mut block) => {
                block.reset();
                block
            }
            None => try_box(SmallBlock::try_new(class_index)?)?,
        };
        let (segment_index, block_index) = match self.acquire_free_run(1) {
            Ok(location) => location,
            Err(error) => {
                self.spare_small[class_index] = Some(block);
                return Err(error);
            }
        };
        self.free_blocks -= 1;
        self.segments[segment_index].free_blocks -= 1;
        self.segments[segment_index].blocks[block_index] = BlockState::Small(block);
        self.active_small[class_index] = Some((segment_index, block_index));
        Ok((segment_index, block_index))
    }

    // Large spans perform run acquisition and span bookkeeping. Keep those
    // paths out of the small-cell allocator's register and instruction budget.
    #[cold]
    #[inline(never)]
    fn allocate_large(&mut self, size: usize) -> Result<Allocation, HeapError> {
        let blocks = size.div_ceil(HEAP_BLOCK_SIZE).max(1);
        let (segment_index, head) = self.acquire_free_run(blocks)?;
        self.free_blocks -= blocks;
        self.segments[segment_index].free_blocks -= blocks;
        self.segments[segment_index].blocks[head] = BlockState::LargeHead {
            blocks: blocks as u32,
            logical_bytes: size,
            pending_reclaim: false,
            reclaim_next: 1,
            remembered: false,
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
        Ok(Allocation {
            raw: unsafe { NonNull::new_unchecked(raw) },
            capacity,
        })
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
        let remembered_word_count = blocks.div_ceil(64);
        let mut remembered_blocks = Vec::new();
        remembered_blocks
            .try_reserve_exact(remembered_word_count)
            .map_err(|_| HeapError::SystemAllocationFailed)?;
        remembered_blocks.resize(remembered_word_count, 0);
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
            remembered_blocks: remembered_blocks.into_boxed_slice(),
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

    /// Start a bounded walk over the segments that exist at this instant.
    #[inline]
    pub fn object_cursor(&self) -> HeapObjectCursor {
        HeapObjectCursor {
            segment_end: self.segments.len(),
            ..HeapObjectCursor::default()
        }
    }

    /// Advance an allocation walk by one budgetable metadata operation.
    pub fn walk_allocated_step(&self, cursor: &mut HeapObjectCursor) -> HeapWalkStep {
        self.walk_step(cursor, false)
    }

    /// Advance a remembered-object walk by one budgetable metadata operation.
    pub fn walk_remembered_step(&self, cursor: &mut HeapObjectCursor) -> HeapWalkStep {
        self.walk_step(cursor, true)
    }

    fn walk_step(&self, cursor: &mut HeapObjectCursor, remembered_only: bool) -> HeapWalkStep {
        if cursor.segment_index >= cursor.segment_end {
            return HeapWalkStep::Done;
        }
        let Some(segment) = self.segments.get(cursor.segment_index) else {
            cursor.segment_index = cursor.segment_end;
            return HeapWalkStep::Done;
        };
        if cursor.block_index >= segment.blocks.len() {
            cursor.segment_index += 1;
            cursor.block_index = 0;
            cursor.cell_index = 0;
            cursor.block_has_object = false;
            return HeapWalkStep::Metadata;
        }

        if remembered_only {
            let word_index = cursor.block_index / 64;
            let bit_offset = cursor.block_index % 64;
            let candidates = segment.remembered_blocks[word_index] & (u64::MAX << bit_offset);
            if candidates == 0 {
                cursor.block_index = ((word_index + 1) * 64).min(segment.blocks.len());
                cursor.cell_index = 0;
                cursor.block_has_object = false;
                return HeapWalkStep::Metadata;
            }
            let block_index = word_index * 64 + candidates.trailing_zeros() as usize;
            if block_index != cursor.block_index {
                cursor.block_index = block_index;
                cursor.cell_index = 0;
                cursor.block_has_object = false;
                return HeapWalkStep::Metadata;
            }
        }

        let block_index = cursor.block_index;
        let block_base = segment.base + block_index * HEAP_BLOCK_SIZE;
        match &segment.blocks[block_index] {
            BlockState::Free | BlockState::LargeTail { .. } => {
                cursor.block_index += 1;
                cursor.cell_index = 0;
                cursor.block_has_object = false;
                HeapWalkStep::Metadata
            }
            BlockState::LargeHead {
                blocks,
                logical_bytes,
                pending_reclaim,
                remembered,
                ..
            } => {
                cursor.block_index += *blocks as usize;
                cursor.cell_index = 0;
                cursor.block_has_object = false;
                if *pending_reclaim || (remembered_only && !*remembered) {
                    HeapWalkStep::Metadata
                } else {
                    HeapWalkStep::Object(WalkedAllocation {
                        first_in_block: true,
                        location: BlockLocation {
                            segment: cursor.segment_index,
                            block: block_index,
                        },
                        allocation: LocatedAllocation {
                            raw: unsafe { NonNull::new_unchecked(block_base as *mut u8) },
                            capacity: *blocks as usize * HEAP_BLOCK_SIZE,
                            logical_bytes: *logical_bytes,
                        },
                    })
                }
            }
            BlockState::Small(block) => {
                let cell_count = block.cell_count();
                let cell_index = usize::from(cursor.cell_index);
                if cell_index >= cell_count {
                    cursor.block_index += 1;
                    cursor.cell_index = 0;
                    cursor.block_has_object = false;
                    return HeapWalkStep::Metadata;
                }

                let word_index = cell_index / 64;
                let bit_offset = cell_index % 64;
                let mut candidates = block.allocated[word_index];
                if remembered_only {
                    candidates &= block.remembered[word_index];
                }
                candidates &= u64::MAX << bit_offset;
                if candidates == 0 {
                    cursor.cell_index = ((word_index + 1) * 64).min(cell_count) as u16;
                    return HeapWalkStep::Metadata;
                }

                let cell = word_index * 64 + candidates.trailing_zeros() as usize;
                cursor.cell_index = (cell + 1) as u16;
                let first_in_block = !cursor.block_has_object;
                cursor.block_has_object = true;
                HeapWalkStep::Object(WalkedAllocation {
                    first_in_block,
                    location: BlockLocation {
                        segment: cursor.segment_index,
                        block: block_index,
                    },
                    allocation: LocatedAllocation {
                        raw: unsafe {
                            NonNull::new_unchecked(
                                (block_base + cell * block.class_size()) as *mut u8,
                            )
                        },
                        capacity: block.class_size(),
                        logical_bytes: usize::from(block.logical_sizes[cell]),
                    },
                })
            }
        }
    }

    #[inline]
    pub fn remembered_object_count(&self) -> usize {
        self.remembered_objects
    }

    fn walked_location(&self, walked: WalkedAllocation) -> Result<BlockLocation, HeapError> {
        let location = walked.location;
        let segment = self
            .segments
            .get(location.segment)
            .ok_or(HeapError::InvalidPointer)?;
        if location.block >= segment.blocks.len() {
            return Err(HeapError::InvalidPointer);
        }
        let base = segment.base + location.block * HEAP_BLOCK_SIZE;
        if (walked.allocation.as_ptr() as usize).wrapping_sub(base) >= HEAP_BLOCK_SIZE {
            return Err(HeapError::InvalidPointer);
        }
        Ok(location)
    }

    pub fn promote_walked(&mut self, walked: WalkedAllocation) -> Result<(), HeapError> {
        let location = self.walked_location(walked)?;
        self.remember_at(walked.allocation.as_ptr(), location)?;
        if let BlockState::Small(block) =
            &mut self.segments[location.segment].blocks[location.block]
        {
            block.old_cells = block.old_cells.saturating_add(1);
        }
        Ok(())
    }

    #[cfg(test)]
    pub fn record_promoted(&mut self, raw: *mut u8) {
        if let Some((segment_index, block_index)) = self.locate_block(raw as usize) {
            if let BlockState::Small(block) = &mut self.segments[segment_index].blocks[block_index]
            {
                block.old_cells = block.old_cells.saturating_add(1);
            }
        }
    }

    pub fn record_runtime_backing(&mut self, raw: *mut u8, logical_bytes: usize) {
        if let Some((segment_index, block_index)) = self.locate_block(raw as usize) {
            if let BlockState::Small(block) = &mut self.segments[segment_index].blocks[block_index]
            {
                block.runtime_backing_bytes =
                    block.runtime_backing_bytes.saturating_add(logical_bytes);
            }
        }
    }

    pub fn record_marked(&mut self, raw: *mut u8, cycle_id: u64) {
        if let Some((segment_index, block_index)) = self.locate_block(raw as usize) {
            if let BlockState::Small(block) = &mut self.segments[segment_index].blocks[block_index]
            {
                block.marked_cycle = cycle_id;
            }
        }
    }

    /// A completely unmarked young block with no finalizers can be released
    /// from its first allocated cell, including when physical cell zero is free.
    #[cfg(any(test, not(feature = "gc-debug")))]
    pub fn try_reclaim_walked_young_block(
        &mut self,
        walked: WalkedAllocation,
        cycle_id: u64,
    ) -> Option<BulkReclaim> {
        if !walked.first_in_block {
            return None;
        }
        let location = self.walked_location(walked).ok()?;
        self.try_reclaim_young_block_at(location, cycle_id)
    }

    #[cfg(test)]
    pub fn try_reclaim_unmarked_young_block(
        &mut self,
        raw: *mut u8,
        cycle_id: u64,
    ) -> Option<BulkReclaim> {
        let (segment, block) = self.locate_block(raw as usize)?;
        self.try_reclaim_young_block_at(BlockLocation { segment, block }, cycle_id)
    }

    #[cfg(any(test, not(feature = "gc-debug")))]
    fn try_reclaim_young_block_at(
        &mut self,
        location: BlockLocation,
        cycle_id: u64,
    ) -> Option<BulkReclaim> {
        let BlockLocation {
            segment: segment_index,
            block: block_index,
        } = location;
        let block = match &self.segments[segment_index].blocks[block_index] {
            BlockState::Small(block)
                if block.old_cells == 0
                    && block.remembered_cells == 0
                    && block.finalizable_cells == 0
                    && block.marked_cycle != cycle_id =>
            {
                block
            }
            _ => return None,
        };
        let reclaimed = BulkReclaim {
            object_count: usize::from(block.live_cells),
            logical_bytes: block.logical_bytes,
            runtime_backing_bytes: block.runtime_backing_bytes,
        };
        if reclaimed.object_count == 0 {
            return None;
        }
        self.allocated_span_bytes = self
            .allocated_span_bytes
            .saturating_sub(reclaimed.object_count.saturating_mul(block.class_size()));
        self.release_small_block(segment_index, block_index);
        Some(reclaimed)
    }

    /// Called only after all resident objects have been individually freed or
    /// admitted to whole-block reclamation. Detached bitmaps are inaccessible
    /// to the collector and will be reset only when the metadata is reused.
    fn release_small_block(&mut self, segment_index: usize, block_index: usize) {
        let state = core::mem::replace(
            &mut self.segments[segment_index].blocks[block_index],
            BlockState::Free,
        );
        let BlockState::Small(block) = state else {
            unreachable!("small block release requires small metadata");
        };
        let class_index = usize::from(block.class_index);
        self.set_remembered_block(segment_index, block_index, false);
        self.free_blocks += 1;
        self.segments[segment_index].free_blocks += 1;
        if self.active_small[class_index] == Some((segment_index, block_index)) {
            self.active_small[class_index] = None;
        }
        if self.spare_small[class_index].is_none() {
            self.spare_small[class_index] = Some(block);
        }
    }

    #[inline]
    fn set_remembered_block(&mut self, segment_index: usize, block_index: usize, remembered: bool) {
        let word = block_index / 64;
        let bit = 1u64 << (block_index % 64);
        let summary = &mut self.segments[segment_index].remembered_blocks[word];
        if remembered {
            *summary |= bit;
        } else {
            *summary &= !bit;
        }
    }

    /// Record an old parent in heap-local metadata. Returns true on the first
    /// transition so callers can update telemetry without another lookup.
    pub fn remember(&mut self, raw: *mut u8) -> Result<bool, HeapError> {
        let (segment, block) = self
            .locate_block(raw as usize)
            .ok_or(HeapError::InvalidPointer)?;
        self.remember_at(raw, BlockLocation { segment, block })
    }

    fn remember_at(&mut self, raw: *mut u8, location: BlockLocation) -> Result<bool, HeapError> {
        let address = raw as usize;
        let BlockLocation {
            segment: segment_index,
            block: block_index,
        } = location;
        let segment_base = self.segments[segment_index].base;
        let block_base = segment_base + block_index * HEAP_BLOCK_SIZE;
        let changed = match &mut self.segments[segment_index].blocks[block_index] {
            BlockState::Small(block) => {
                let block = block.as_mut();
                let class_size = block.class_size();
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
                if block.is_remembered(cell) {
                    false
                } else {
                    block.set_remembered(cell, true);
                    block.remembered_cells += 1;
                    true
                }
            }
            BlockState::LargeHead {
                pending_reclaim,
                remembered,
                ..
            } if address == block_base && !*pending_reclaim => {
                if *remembered {
                    false
                } else {
                    *remembered = true;
                    true
                }
            }
            _ => return Err(HeapError::InvalidPointer),
        };
        if changed {
            self.remembered_objects += 1;
            self.set_remembered_block(segment_index, block_index, true);
        }
        Ok(changed)
    }

    pub fn forget_remembered(&mut self, raw: *mut u8) -> Result<bool, HeapError> {
        let address = raw as usize;
        let (segment_index, block_index) = self
            .locate_block(address)
            .ok_or(HeapError::InvalidPointer)?;
        let segment_base = self.segments[segment_index].base;
        let block_base = segment_base + block_index * HEAP_BLOCK_SIZE;
        let (changed, block_still_remembered) =
            match &mut self.segments[segment_index].blocks[block_index] {
                BlockState::Small(block) => {
                    let block = block.as_mut();
                    let class_size = block.class_size();
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
                    let was_remembered = block.is_remembered(cell);
                    if was_remembered {
                        block.set_remembered(cell, false);
                        block.remembered_cells -= 1;
                    }
                    (was_remembered, block.remembered_cells != 0)
                }
                BlockState::LargeHead {
                    pending_reclaim,
                    remembered,
                    ..
                } if address == block_base && !*pending_reclaim => {
                    let was_remembered = *remembered;
                    *remembered = false;
                    (was_remembered, false)
                }
                _ => return Err(HeapError::InvalidPointer),
            };
        if changed {
            self.remembered_objects -= 1;
            if !block_still_remembered {
                self.set_remembered_block(segment_index, block_index, false);
            }
        }
        Ok(changed)
    }

    pub fn is_remembered(&self, raw: *mut u8) -> bool {
        let address = raw as usize;
        let Some((segment_index, block_index)) = self.locate_block(address) else {
            return false;
        };
        let segment = &self.segments[segment_index];
        let block_base = segment.base + block_index * HEAP_BLOCK_SIZE;
        match &segment.blocks[block_index] {
            BlockState::Small(block) => {
                let offset = address.saturating_sub(block_base);
                let class_size = block.class_size();
                if !offset.is_multiple_of(class_size) {
                    return false;
                }
                let cell = offset / class_size;
                cell < block.cell_count() && block.is_allocated(cell) && block.is_remembered(cell)
            }
            BlockState::LargeHead {
                pending_reclaim,
                remembered,
                ..
            } => address == block_base && !*pending_reclaim && *remembered,
            _ => false,
        }
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
                        raw: unsafe { NonNull::new_unchecked(raw as *mut u8) },
                        capacity: class_size,
                        logical_bytes: usize::from(block.logical_sizes[cell]),
                    },
                )
            }
            BlockState::LargeHead {
                blocks,
                logical_bytes,
                pending_reclaim,
                ..
            } => {
                if *pending_reclaim {
                    return None;
                }
                let capacity = *blocks as usize * HEAP_BLOCK_SIZE;
                (address >= block_base + header_size && address < block_base + capacity).then_some(
                    LocatedAllocation {
                        raw: unsafe { NonNull::new_unchecked(block_base as *mut u8) },
                        capacity,
                        logical_bytes: *logical_bytes,
                    },
                )
            }
            BlockState::LargeTail { head } => {
                let head = *head as usize;
                let head_base = segment.base + head * HEAP_BLOCK_SIZE;
                let BlockState::LargeHead {
                    blocks,
                    logical_bytes,
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
                        raw: unsafe { NonNull::new_unchecked(head_base as *mut u8) },
                        capacity,
                        logical_bytes: *logical_bytes,
                    },
                )
            }
        }
    }

    pub fn canonicalize_and_record_marked(
        &mut self,
        address: usize,
        header_size: usize,
        cycle_id: u64,
    ) -> Option<*mut u8> {
        let (segment_index, block_index) = self.locate_block(address)?;
        let segment = &mut self.segments[segment_index];
        let block_base = segment.base + block_index * HEAP_BLOCK_SIZE;
        match &mut segment.blocks[block_index] {
            BlockState::Free => None,
            BlockState::Small(block) => {
                let class_size = block.class_size();
                let cell = (address - block_base) / class_size;
                if cell >= block.cell_count() || !block.is_allocated(cell) {
                    return None;
                }
                let raw = block_base + cell * class_size;
                if address < raw + header_size || address >= raw + class_size {
                    return None;
                }
                let data_base = raw + header_size;
                let logical_end = raw.checked_add(usize::from(block.logical_sizes[cell]))?;
                if address != data_base && address >= logical_end {
                    return None;
                }
                block.marked_cycle = cycle_id;
                Some(raw as *mut u8)
            }
            BlockState::LargeHead {
                blocks,
                logical_bytes,
                pending_reclaim,
                ..
            } => {
                if *pending_reclaim {
                    return None;
                }
                let capacity = *blocks as usize * HEAP_BLOCK_SIZE;
                let data_base = block_base + header_size;
                let logical_end = block_base.checked_add(*logical_bytes)?;
                (address >= data_base
                    && address < block_base + capacity
                    && (address == data_base || address < logical_end))
                    .then_some(block_base as *mut u8)
            }
            BlockState::LargeTail { head } => {
                let head = *head as usize;
                let head_base = segment.base + head * HEAP_BLOCK_SIZE;
                let BlockState::LargeHead {
                    blocks,
                    logical_bytes,
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
                let data_base = head_base + header_size;
                let logical_end = head_base.checked_add(*logical_bytes)?;
                (address >= data_base
                    && address < head_base + capacity
                    && (address == data_base || address < logical_end))
                    .then_some(head_base as *mut u8)
            }
        }
    }

    #[inline]
    fn locate_block(&self, address: usize) -> Option<(usize, usize)> {
        if self.segment_index_by_base.len() == 1 {
            let segment_index = self.segment_index_by_base[0];
            let segment = &self.segments[segment_index];
            return segment
                .contains(address)
                .then(|| (segment_index, (address - segment.base) / HEAP_BLOCK_SIZE));
        }
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

    #[cfg(test)]
    pub fn free(&mut self, raw: *mut u8) -> Result<(), HeapError> {
        let (segment, block) = self
            .locate_block(raw as usize)
            .ok_or(HeapError::InvalidPointer)?;
        self.free_at(raw, BlockLocation { segment, block }, None)
    }

    #[cfg(test)]
    pub fn free_recorded(
        &mut self,
        raw: *mut u8,
        logical_bytes: usize,
        was_old: bool,
        finalizable: bool,
        runtime_backing: bool,
    ) -> Result<(), HeapError> {
        let (segment, block) = self
            .locate_block(raw as usize)
            .ok_or(HeapError::InvalidPointer)?;
        self.free_at(
            raw,
            BlockLocation { segment, block },
            Some((logical_bytes, was_old, finalizable, runtime_backing)),
        )
    }

    pub fn free_walked(
        &mut self,
        walked: WalkedAllocation,
        was_old: bool,
        finalizable: bool,
        runtime_backing: bool,
    ) -> Result<(), HeapError> {
        let location = self.walked_location(walked)?;
        self.free_at(
            walked.allocation.as_ptr(),
            location,
            Some((
                walked.allocation.logical_bytes,
                was_old,
                finalizable,
                runtime_backing,
            )),
        )
    }

    fn free_at(
        &mut self,
        raw: *mut u8,
        location: BlockLocation,
        accounting: Option<(usize, bool, bool, bool)>,
    ) -> Result<(), HeapError> {
        let address = raw as usize;
        let BlockLocation {
            segment: segment_index,
            block: block_index,
        } = location;
        let segment_base = self.segments[segment_index].base;
        let block_base = segment_base + block_index * HEAP_BLOCK_SIZE;

        let mut clear_remembered_summary = false;
        let result = match &mut self.segments[segment_index].blocks[block_index] {
            BlockState::Small(block) => {
                let block = block.as_mut();
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
                let (logical_bytes, was_old, finalizable, runtime_backing) = accounting
                    .unwrap_or((usize::from(block.logical_sizes[cell]), false, false, false));
                block.logical_bytes = block.logical_bytes.saturating_sub(logical_bytes);
                if was_old {
                    block.old_cells = block.old_cells.saturating_sub(1);
                }
                if finalizable {
                    block.finalizable_cells = block.finalizable_cells.saturating_sub(1);
                }
                if runtime_backing {
                    block.runtime_backing_bytes =
                        block.runtime_backing_bytes.saturating_sub(logical_bytes);
                }
                if block.is_remembered(cell) {
                    block.set_remembered(cell, false);
                    block.remembered_cells -= 1;
                    self.remembered_objects -= 1;
                    clear_remembered_summary = block.remembered_cells == 0;
                }
                block.set_allocated(cell, false);
                block.logical_sizes[cell] = 0;
                block.live_cells -= 1;
                unsafe {
                    (address as *mut u16).write_unaligned(block.free_head);
                }
                block.free_head = cell as u16;
                self.allocated_span_bytes -= class_size;
                if block.live_cells == 0 {
                    self.release_small_block(segment_index, block_index);
                    clear_remembered_summary = false;
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
                remembered,
                ..
            } if address == block_base && !*pending_reclaim => {
                let capacity = *blocks as usize * HEAP_BLOCK_SIZE;
                if *remembered {
                    *remembered = false;
                    self.remembered_objects -= 1;
                    clear_remembered_summary = true;
                }
                *pending_reclaim = true;
                *reclaim_next = 1;
                self.pending_large_spans += 1;
                self.pending_reclaim_bytes += capacity;
                self.allocated_span_bytes -= capacity;
                Ok(())
            }
            _ => Err(HeapError::InvalidPointer),
        };
        if clear_remembered_summary {
            self.set_remembered_block(segment_index, block_index, false);
        }
        result
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
            let pending = matches!(
                self.segments[segment_index].blocks[head],
                BlockState::LargeHead {
                    pending_reclaim: true,
                    ..
                }
            );
            if !pending {
                work += 1;
                self.reclaim_block_cursor += 1;
                continue;
            }

            // Charge each released block once. Charging the head inspection
            // first would consume every one-unit call without advancing the
            // persistent tail cursor.
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
            if reclaim_next == blocks && work < max_blocks {
                work += 1;
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
pub(super) fn allocation_class(size: usize) -> Option<(usize, usize)> {
    if size == 0 || size > (1usize << MAX_CLASS_SHIFT) {
        return None;
    }
    let class_size = size.max(1usize << MIN_CLASS_SHIFT).next_power_of_two();
    let shift = class_size.trailing_zeros() as usize;
    Some((shift - MIN_CLASS_SHIFT, class_size))
}

#[cfg(any(test, not(feature = "gc-debug")))]
#[inline]
pub(super) fn allocation_bit(cursor: *mut u8, class_size: usize) -> u64 {
    debug_assert!(class_size.is_power_of_two());
    let block_offset = cursor as usize & (HEAP_BLOCK_SIZE - 1);
    let cell_index = block_offset >> class_size.trailing_zeros();
    1_u64 << (cell_index % 64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn small_allocations_reuse_cells_and_keep_stable_addresses() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        heap.reserve(HEAP_BLOCK_SIZE).unwrap();
        heap.set_growth_allowed(false);

        let first = heap.allocate(24, false, 0).unwrap();
        let second = heap.allocate(24, false, 0).unwrap();
        assert_ne!(first.as_ptr(), second.as_ptr());
        heap.free(first.as_ptr()).unwrap();
        let reused = heap.allocate(24, false, 0).unwrap();
        assert_eq!(reused.as_ptr(), first.as_ptr());
        assert_eq!(reused.capacity, 32);
    }

    fn detached_metadata_identity(block: &SmallBlock) -> [usize; 4] {
        [
            block as *const SmallBlock as usize,
            block.allocated.as_ptr() as usize,
            block.logical_sizes.as_ptr() as usize,
            block.remembered.as_ptr() as usize,
        ]
    }

    fn small_metadata(heap: &SpanHeap, raw: *mut u8) -> &SmallBlock {
        let (segment, block) = heap.locate_block(raw as usize).unwrap();
        let BlockState::Small(metadata) = &heap.segments[segment].blocks[block] else {
            panic!("expected small metadata");
        };
        metadata
    }

    #[test]
    fn detached_metadata_reuses_every_class_without_retaining_objects() {
        for class in 0..CLASS_COUNT {
            for bulk in [false, true] {
                let size = 1usize << (MIN_CLASS_SHIFT + class);
                let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
                let first = heap.allocate(size - 1, false, 4).unwrap();
                let second = heap.allocate(size - 1, false, 4).unwrap();
                let identity = detached_metadata_identity(small_metadata(&heap, first.as_ptr()));
                unsafe {
                    first.as_ptr().write_bytes(0xa5, size);
                    second.as_ptr().write_bytes(0x6d, size);
                }
                if bulk {
                    let reclaimed = heap
                        .try_reclaim_unmarked_young_block(first.as_ptr(), 5)
                        .unwrap();
                    assert_eq!(reclaimed.object_count, 2);
                    assert_eq!(reclaimed.logical_bytes, 2 * (size - 1));
                } else {
                    heap.free(first.as_ptr()).unwrap();
                    heap.free(second.as_ptr()).unwrap();
                }
                assert_eq!(heap.stats().free_blocks, 1);
                assert_eq!(heap.stats().allocated_span_bytes, 0);
                assert!(heap.locate(first.as_ptr() as usize, 0).is_none());
                assert!(heap.locate(second.as_ptr() as usize, 0).is_none());
                assert_eq!(
                    detached_metadata_identity(heap.spare_small[class].as_ref().unwrap()),
                    identity
                );
                heap.set_growth_allowed(false);
                let reused = heap.allocate(size - 3, false, 9).unwrap();
                assert_eq!(reused.as_ptr(), first.as_ptr());
                assert_eq!(
                    detached_metadata_identity(small_metadata(&heap, reused.as_ptr())),
                    identity
                );
                assert!(heap.spare_small[class].is_none());
                assert!(heap.locate(second.as_ptr() as usize, 0).is_none());
                assert_eq!(
                    heap.locate(reused.as_ptr() as usize, 0)
                        .unwrap()
                        .logical_bytes,
                    size - 3
                );
                assert!(heap
                    .canonicalize_and_record_marked(reused.as_ptr() as usize + size - 2, 0, 9)
                    .is_none());
                assert!(
                    unsafe { core::slice::from_raw_parts(reused.as_ptr(), size) }
                        .iter()
                        .all(|b| *b == 0)
                );
                let reclaimed = heap
                    .try_reclaim_unmarked_young_block(reused.as_ptr(), 10)
                    .unwrap();
                assert_eq!(reclaimed.object_count, 1);
                assert_eq!(reclaimed.logical_bytes, size - 3);
            }
        }
    }

    #[test]
    fn detached_metadata_retention_is_one_allocation_per_class() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE * 2));
        heap.reserve(HEAP_BLOCK_SIZE * 2).unwrap();
        heap.set_growth_allowed(false);
        for class in 0..CLASS_COUNT {
            let size = 1usize << (MIN_CLASS_SHIFT + class);
            let cells = HEAP_BLOCK_SIZE / size;
            let allocations: Vec<_> = (0..=cells)
                .map(|_| heap.allocate(size, false, 0).unwrap())
                .collect();
            let identity =
                detached_metadata_identity(small_metadata(&heap, allocations[0].as_ptr()));
            heap.try_reclaim_unmarked_young_block(allocations[0].as_ptr(), 1)
                .unwrap();
            heap.try_reclaim_unmarked_young_block(allocations[cells].as_ptr(), 1)
                .unwrap();
            assert_eq!(
                heap.spare_small.iter().filter(|b| b.is_some()).count(),
                class + 1
            );
            assert_eq!(
                detached_metadata_identity(heap.spare_small[class].as_ref().unwrap()),
                identity
            );
            assert_eq!(heap.stats().free_blocks, 2);
            assert_eq!(heap.stats().allocated_span_bytes, 0);
        }
        let retained = core::mem::size_of_val(&heap.spare_small)
            + heap
                .spare_small
                .iter()
                .flatten()
                .map(|b| {
                    core::mem::size_of::<SmallBlock>()
                        + core::mem::size_of_val(b.allocated.as_ref())
                        + core::mem::size_of_val(b.logical_sizes.as_ref())
                        + core::mem::size_of_val(b.remembered.as_ref())
                })
                .sum::<usize>();
        // Includes each metadata box, all three buffers and the fixed cache;
        // allocator overhead is deliberately outside this requested-byte bound.
        assert!(
            retained <= 20 * 1024,
            "retained requested bytes: {retained}"
        );
    }

    #[test]
    fn detached_metadata_does_not_pin_large_span_capacity_or_cross_islands() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE * 3));
        heap.reserve(HEAP_BLOCK_SIZE * 3).unwrap();
        heap.set_growth_allowed(false);
        let first = heap.allocate(24, false, 0).unwrap();
        let identity = detached_metadata_identity(small_metadata(&heap, first.as_ptr()));
        heap.free(first.as_ptr()).unwrap();
        let large = heap.allocate(HEAP_BLOCK_SIZE + 1, false, 0).unwrap();
        assert_eq!(large.as_ptr(), first.as_ptr());
        let reused = heap.allocate(25, false, 0).unwrap();
        assert_ne!(reused.as_ptr(), first.as_ptr());
        assert_eq!(
            detached_metadata_identity(small_metadata(&heap, reused.as_ptr())),
            identity
        );
        let foreign = SpanHeap::new(None);
        assert!(foreign.spare_small.iter().all(Option::is_none));
        assert!(foreign.locate(reused.as_ptr() as usize, 0).is_none());
        heap.free(large.as_ptr()).unwrap();
        assert_eq!(heap.reclaim_step(1), (1, false));
        let mut completed = false;
        for _ in 0..4 {
            let (work, done) = heap.reclaim_step(1);
            assert!(work <= 1);
            if done {
                completed = true;
                break;
            }
        }
        assert!(
            completed,
            "large reclaim must complete within its bounded scan"
        );
        assert_eq!(heap.stats().pending_reclaim_bytes, 0);
        assert_eq!(heap.stats().free_blocks, 2);
        assert!(heap.locate(reused.as_ptr() as usize, 0).is_some());
        assert_eq!(heap.stats().allocated_span_bytes, 32);
    }

    #[test]
    fn failed_small_block_admission_keeps_detached_metadata_available() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        let first = heap.allocate(24, false, 0).unwrap();
        let identity = detached_metadata_identity(small_metadata(&heap, first.as_ptr()));
        heap.free(first.as_ptr()).unwrap();
        let large = heap.allocate(HEAP_BLOCK_SIZE, false, 0).unwrap();
        for (growth, expected) in [
            (false, HeapError::GrowthDisabled),
            (true, HeapError::HardLimitExceeded),
        ] {
            heap.set_growth_allowed(growth);
            assert_eq!(heap.allocate(24, false, 0).unwrap_err(), expected);
            assert_eq!(
                detached_metadata_identity(heap.spare_small[1].as_ref().unwrap()),
                identity
            );
            assert_eq!(heap.stats().allocated_span_bytes, HEAP_BLOCK_SIZE);
        }
        heap.free(large.as_ptr()).unwrap();
        while !heap.reclaim_step(1).1 {}
        heap.set_allocation_allowed(false);
        assert_eq!(
            heap.allocate(24, false, 0).unwrap_err(),
            HeapError::AllocationForbidden
        );
        assert_eq!(
            heap.reserve_bump_lane(24, 4).unwrap_err(),
            HeapError::AllocationForbidden
        );
        assert_eq!(
            detached_metadata_identity(heap.spare_small[1].as_ref().unwrap()),
            identity
        );
        heap.set_allocation_allowed(true);
        let reused = heap.allocate(24, false, 0).unwrap();
        assert_eq!(
            detached_metadata_identity(small_metadata(&heap, reused.as_ptr())),
            identity
        );
    }

    #[test]
    fn detached_metadata_clears_old_finalizer_remembered_and_backing_state() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        let first = heap.allocate(24, true, 77).unwrap();
        heap.record_promoted(first.as_ptr());
        heap.record_runtime_backing(first.as_ptr(), 24);
        assert!(heap.remember(first.as_ptr()).unwrap());
        heap.free_recorded(first.as_ptr(), 24, true, true, true)
            .unwrap();
        assert_eq!(heap.remembered_object_count(), 0);
        let reused = heap.allocate(25, false, 78).unwrap();
        assert!(!heap.is_remembered(reused.as_ptr()));
        let block = small_metadata(&heap, reused.as_ptr());
        assert_eq!(block.old_cells, 0);
        assert_eq!(block.finalizable_cells, 0);
        assert_eq!(block.remembered_cells, 0);
        assert_eq!(block.runtime_backing_bytes, 0);
        assert_eq!(block.marked_cycle, 78);
        let reclaimed = heap
            .try_reclaim_unmarked_young_block(reused.as_ptr(), 79)
            .unwrap();
        assert_eq!(reclaimed.object_count, 1);
        assert_eq!(reclaimed.logical_bytes, 25);
        assert_eq!(reclaimed.runtime_backing_bytes, 0);
    }

    #[test]
    fn native_lane_reuses_detached_metadata_and_publishes_fresh_extents() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        let first = heap.allocate(31, false, 0).unwrap();
        let identity = detached_metadata_identity(small_metadata(&heap, first.as_ptr()));
        heap.try_reclaim_unmarked_young_block(first.as_ptr(), 1)
            .unwrap();
        let lane = heap.reserve_bump_lane(17, 4).unwrap().unwrap();
        assert_eq!(
            detached_metadata_identity(small_metadata(&heap, lane.cursor)),
            identity
        );
        assert!(heap.locate(lane.cursor as usize, 0).is_none());
        assert!(
            unsafe { core::slice::from_raw_parts(lane.cursor, 4 * lane.class_size) }
                .iter()
                .all(|b| *b == 0)
        );
        unsafe {
            *lane.logical_size_cursor = 17;
            *lane.bitmap_word |= 1;
            *lane.live_cells += 1;
            *lane.logical_bytes += 17;
        }
        heap.allocated_span_bytes += lane.class_size;
        heap.release_bump_lane(unsafe { lane.cursor.add(lane.class_size) }, lane.limit);
        assert_eq!(
            heap.locate(lane.cursor as usize, 0).unwrap().logical_bytes,
            17
        );
        assert!(heap
            .canonicalize_and_record_marked(lane.cursor as usize + 18, 0, 2)
            .is_none());
        let reclaimed = heap
            .try_reclaim_unmarked_young_block(lane.cursor, 3)
            .unwrap();
        assert_eq!(reclaimed.object_count, 1);
        assert_eq!(reclaimed.logical_bytes, 17);
    }

    #[test]
    fn reused_small_cells_clear_padding_without_touching_neighbor() {
        for class_shift in MIN_CLASS_SHIFT..=MAX_CLASS_SHIFT {
            let class_size = 1usize << class_shift;
            let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
            let first = heap.allocate(class_size - 3, false, 0).unwrap();
            let neighbor = heap.allocate(class_size - 3, false, 0).unwrap();
            assert_eq!(first.capacity, class_size);
            unsafe {
                first.as_ptr().write_bytes(0xa5, class_size);
                neighbor.as_ptr().write_bytes(0x6d, class_size);
            }
            heap.free(first.as_ptr()).unwrap();
            let reused = heap.allocate(class_size - 3, false, 0).unwrap();
            assert_eq!(reused.as_ptr(), first.as_ptr());
            assert!(
                unsafe { core::slice::from_raw_parts(reused.as_ptr(), class_size) }
                    .iter()
                    .all(|byte| *byte == 0)
            );
            assert!(
                unsafe { core::slice::from_raw_parts(neighbor.as_ptr(), class_size) }
                    .iter()
                    .all(|byte| *byte == 0x6d)
            );
        }
    }

    #[test]
    fn full_small_blocks_reenter_allocation_without_segment_rescan() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE * 2));
        heap.reserve(HEAP_BLOCK_SIZE * 2).unwrap();
        heap.set_growth_allowed(false);
        let cells_per_block = HEAP_BLOCK_SIZE / 1024;
        let allocations: Vec<_> = (0..cells_per_block * 2)
            .map(|_| {
                heap.allocate(1024, false, 0)
                    .expect("fill two small blocks")
            })
            .collect();

        heap.free(allocations[0].as_ptr()).unwrap();
        heap.free(allocations[cells_per_block].as_ptr()).unwrap();
        assert_eq!(heap.partial_small[6].len(), 1);

        assert_eq!(
            heap.allocate(1024, false, 0)
                .expect("reuse active block")
                .as_ptr(),
            allocations[0].as_ptr()
        );
        assert_eq!(
            heap.allocate(1024, false, 0)
                .expect("reuse indexed partial block")
                .as_ptr(),
            allocations[cells_per_block].as_ptr()
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
            .map(|_| {
                heap.allocate(1024, false, 0)
                    .expect("fill two small blocks")
            })
            .collect();

        heap.free(allocations[0].as_ptr()).unwrap();
        heap.free(allocations[cells_per_block].as_ptr()).unwrap();
        heap.partial_small[6].clear();
        heap.partial_index_complete[6] = false;

        assert_eq!(
            heap.allocate(1024, false, 0).unwrap().as_ptr(),
            allocations[0].as_ptr()
        );
        assert_eq!(
            heap.allocate(1024, false, 0)
                .expect("slow fallback must recover an unindexed partial block")
                .as_ptr(),
            allocations[cells_per_block].as_ptr()
        );
        assert!(!heap.partial_index_complete[6]);
    }

    #[test]
    fn no_growth_and_hard_limit_fail_closed() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        heap.set_growth_allowed(false);
        assert!(matches!(
            heap.allocate(8, false, 0),
            Err(HeapError::GrowthDisabled)
        ));

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
            assert_eq!(
                heap.allocate(8, false, 0).expect("minimum cell").capacity,
                16
            );
        }
        assert!(matches!(
            heap.allocate(8, false, 0),
            Err(HeapError::GrowthDisabled)
        ));
    }

    #[test]
    fn large_reclaim_is_block_bounded() {
        let mut heap = SpanHeap::new(None);
        let allocation = heap.allocate(HEAP_BLOCK_SIZE * 3, false, 0).unwrap();
        heap.free(allocation.as_ptr()).unwrap();
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
        let small = heap.allocate(64, false, 0).unwrap();
        let small_located = heap.locate(small.as_ptr() as usize + 24, 8).unwrap();
        assert_eq!(small_located.as_ptr(), small.as_ptr());

        let large = heap.allocate(HEAP_BLOCK_SIZE + 64, false, 0).unwrap();
        let large_located = heap
            .locate(large.as_ptr() as usize + HEAP_BLOCK_SIZE + 16, 8)
            .unwrap();
        assert_eq!(large_located.as_ptr(), large.as_ptr());
    }

    #[test]
    fn segment_address_index_locates_many_independent_segments() {
        let mut heap = SpanHeap::new(None);
        for _ in 0..16 {
            heap.reserve(HEAP_BLOCK_SIZE).unwrap();
        }
        let allocations: Vec<_> = (0..16)
            .map(|_| heap.allocate(HEAP_BLOCK_SIZE, false, 0).unwrap())
            .collect();

        assert!(heap
            .segment_index_by_base
            .windows(2)
            .all(|pair| { heap.segments[pair[0]].base < heap.segments[pair[1]].base }));
        for allocation in allocations {
            let located = heap
                .locate(allocation.as_ptr() as usize + HEAP_BLOCK_SIZE - 1, 8)
                .expect("interior address must resolve through segment index");
            assert_eq!(located.as_ptr(), allocation.as_ptr());
        }
    }

    #[test]
    fn bump_lane_commits_cells_and_returns_unused_tail() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        heap.reserve(HEAP_BLOCK_SIZE).unwrap();
        heap.set_growth_allowed(false);
        let lane = heap
            .reserve_bump_lane(24, 8)
            .unwrap()
            .expect("small allocation lane");

        for cell in 0..3usize {
            unsafe {
                *lane.bitmap_word |= allocation_bit(lane.cursor, lane.class_size) << cell;
                *lane.logical_size_cursor.add(cell) = 24;
                *lane.live_cells += 1;
                *lane.logical_bytes += 24;
            }
            heap.allocated_span_bytes += lane.class_size;
        }
        let unused = unsafe { lane.cursor.add(3 * lane.class_size) };
        heap.release_bump_lane(unused, lane.limit);
        for cell in 0..3usize {
            let object = unsafe { lane.cursor.add(cell * lane.class_size + 8) };
            assert_eq!(heap.locate(object as usize, 8).unwrap().logical_bytes, 24);
        }

        let allocation = heap
            .allocate(24, false, 0)
            .expect("released lane tail is reusable");
        assert_eq!(allocation.as_ptr(), unused);
        assert_eq!(heap.stats().allocated_span_bytes, 4 * lane.class_size);
    }

    #[test]
    fn allocation_walk_and_bulk_reclaim_share_one_bitmap_authority() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        heap.reserve(HEAP_BLOCK_SIZE).unwrap();
        heap.set_growth_allowed(false);
        let first = heap.allocate(24, false, 0).unwrap();
        let second = heap.allocate(24, false, 0).unwrap();

        let mut cursor = heap.object_cursor();
        let mut seen = Vec::new();
        loop {
            match heap.walk_allocated_step(&mut cursor) {
                HeapWalkStep::Object(walked) => seen.push(walked.allocation.as_ptr()),
                HeapWalkStep::Metadata => {}
                HeapWalkStep::Done => break,
            }
        }
        assert_eq!(seen, vec![first.as_ptr(), second.as_ptr()]);

        let reclaimed = heap
            .try_reclaim_unmarked_young_block(first.as_ptr(), 1)
            .expect("unmarked plain young block can be reclaimed atomically");
        assert_eq!(reclaimed.object_count, 2);
        assert_eq!(reclaimed.logical_bytes, 48);
        assert!(heap.locate(first.as_ptr() as usize + 8, 8).is_none());
    }

    #[test]
    fn allocation_accounting_survives_partial_free_and_cell_reuse() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        let first = heap.allocate(24, false, 0).unwrap();
        let second = heap.allocate(24, false, 0).unwrap();
        heap.free_recorded(first.as_ptr(), 24, false, false, false)
            .unwrap();
        let replacement = heap.allocate(31, true, 0).unwrap();
        assert_eq!(replacement.as_ptr(), first.as_ptr());
        assert_eq!(
            heap.try_reclaim_unmarked_young_block(first.as_ptr(), 1),
            None
        );
        heap.free_recorded(replacement.as_ptr(), 31, false, true, false)
            .unwrap();
        let replacement = heap.allocate(25, false, 0).unwrap();
        assert_eq!(replacement.as_ptr(), first.as_ptr());
        let reclaimed = heap
            .try_reclaim_unmarked_young_block(first.as_ptr(), 1)
            .unwrap();
        assert_eq!(reclaimed.object_count, 2);
        assert_eq!(reclaimed.logical_bytes, 25 + 24);
        assert!(heap.locate(second.as_ptr() as usize + 8, 8).is_none());
    }

    #[test]
    fn allocation_protects_current_cycle_and_expires_for_next_cycle() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        let first = heap.allocate(24, false, 0).unwrap();
        let current = heap.allocate(24, false, 7).unwrap();
        assert_eq!(
            heap.try_reclaim_unmarked_young_block(first.as_ptr(), 7),
            None
        );
        assert!(heap.locate(current.as_ptr() as usize + 8, 8).is_some());
        let reclaimed = heap
            .try_reclaim_unmarked_young_block(first.as_ptr(), 8)
            .unwrap();
        assert_eq!(reclaimed.object_count, 2);
        assert_eq!(reclaimed.logical_bytes, 48);
    }

    fn next_walked(heap: &SpanHeap, cursor: &mut HeapObjectCursor) -> WalkedAllocation {
        loop {
            match heap.walk_allocated_step(cursor) {
                HeapWalkStep::Object(walked) => return walked,
                HeapWalkStep::Metadata => {}
                HeapWalkStep::Done => panic!("expected another allocation"),
            }
        }
    }

    #[test]
    fn bulk_reclaim_handles_empty_bitmap_words_before_first_live_cell() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        let objects: Vec<_> = (0..130)
            .map(|_| heap.allocate(24, false, 0).unwrap())
            .collect();
        for allocation in &objects[..129] {
            heap.free(allocation.as_ptr()).unwrap();
        }
        let mut cursor = heap.object_cursor();
        let walked = next_walked(&heap, &mut cursor);
        assert_eq!(walked.allocation.as_ptr(), objects[129].as_ptr());
        assert!(walked.first_in_block);
        let reclaimed = heap.try_reclaim_walked_young_block(walked, 1).unwrap();
        assert_eq!(reclaimed.object_count, 1);
        assert_eq!(reclaimed.logical_bytes, 24);
        assert_eq!(heap.stats().allocated_span_bytes, 0);
        assert!(heap.locate(objects[129].as_ptr() as usize + 8, 8).is_none());
    }

    #[test]
    fn walked_location_rejects_foreign_heap_and_preserves_promotion_cards() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        let first = heap.allocate(24, false, 0).unwrap();
        let second = heap.allocate(24, false, 0).unwrap();
        let mut cursor = heap.object_cursor();
        let walked = next_walked(&heap, &mut cursor);
        let mut foreign = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
        foreign.allocate(24, false, 0).unwrap();
        assert_eq!(
            foreign.free_walked(walked, false, false, false),
            Err(HeapError::InvalidPointer)
        );
        assert_eq!(
            foreign.promote_walked(walked),
            Err(HeapError::InvalidPointer)
        );
        assert!(foreign.try_reclaim_walked_young_block(walked, 1).is_none());
        assert_eq!(foreign.stats().allocated_span_bytes, 32);

        heap.promote_walked(walked).unwrap();
        assert!(heap.is_remembered(first.as_ptr()));
        assert_eq!(heap.remembered_object_count(), 1);
        assert!(heap.try_reclaim_walked_young_block(walked, 1).is_none());
        heap.free_walked(walked, true, false, false).unwrap();
        assert_eq!(heap.remembered_object_count(), 0);
        assert_eq!(
            heap.free_walked(walked, true, false, false),
            Err(HeapError::InvalidPointer)
        );
        let walked_second = next_walked(&heap, &mut cursor);
        assert_eq!(walked_second.allocation.as_ptr(), second.as_ptr());
        assert!(!walked_second.first_in_block);
        heap.free_walked(walked_second, false, false, false)
            .unwrap();
        assert_eq!(heap.stats().allocated_span_bytes, 0);
    }

    #[test]
    fn remembered_walk_uses_block_summary_and_clears_it_precisely() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE * 2));
        let first = heap.allocate(24, false, 0).unwrap();
        let second = heap.allocate(24, false, 0).unwrap();
        assert!(heap.remember(first.as_ptr()).unwrap());
        assert!(heap.remember(second.as_ptr()).unwrap());

        let collect = |heap: &SpanHeap| {
            let mut cursor = heap.object_cursor();
            let mut seen = Vec::new();
            loop {
                match heap.walk_remembered_step(&mut cursor) {
                    HeapWalkStep::Object(walked) => seen.push(walked.allocation.as_ptr()),
                    HeapWalkStep::Metadata => {}
                    HeapWalkStep::Done => break,
                }
            }
            seen
        };
        assert_eq!(collect(&heap), vec![first.as_ptr(), second.as_ptr()]);

        assert!(heap.forget_remembered(first.as_ptr()).unwrap());
        assert_eq!(collect(&heap), vec![second.as_ptr()]);
        assert!(heap.forget_remembered(second.as_ptr()).unwrap());
        assert!(collect(&heap).is_empty());
    }

    #[test]
    fn bulk_reclaim_rejects_survivor_old_and_finalizable_blocks() {
        for guard in ["marked", "old", "finalizable"] {
            let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE));
            heap.reserve(HEAP_BLOCK_SIZE).unwrap();
            let allocation = heap.allocate(24, guard == "finalizable", 0).unwrap();
            if guard == "marked" {
                heap.record_marked(allocation.as_ptr(), 7);
            }
            if guard == "old" {
                heap.record_promoted(allocation.as_ptr());
            }
            assert_eq!(
                heap.try_reclaim_unmarked_young_block(allocation.as_ptr(), 7),
                None,
                "{guard} block must use the per-object sweep path"
            );
        }
    }
    #[test]
    fn one_unit_large_reclaim_makes_progress_without_releasing_live_neighbors() {
        let mut heap = SpanHeap::new(Some(HEAP_BLOCK_SIZE * 4));
        heap.reserve(HEAP_BLOCK_SIZE * 4).unwrap();
        let large = heap.allocate(HEAP_BLOCK_SIZE * 3, false, 0).unwrap();
        let live = heap.allocate(24, false, 0).unwrap();
        heap.free(large.as_ptr()).unwrap();
        assert_eq!(heap.reclaim_step(0), (0, false));
        for step in 0..3 {
            let (work, done) = heap.reclaim_step(1);
            assert_eq!(work, 1);
            assert_eq!(heap.stats().free_blocks, step + 1);
            assert_eq!(done, step == 2);
            assert!(heap.locate(live.as_ptr() as usize, 0).is_some());
            assert!(heap.locate(large.as_ptr() as usize, 0).is_none());
        }
        assert_eq!(heap.stats().pending_reclaim_bytes, 0);
        assert_eq!(heap.stats().allocated_span_bytes, 32);
        assert_eq!(heap.reclaim_step(1), (0, true));
    }
}
