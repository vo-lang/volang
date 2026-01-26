//! VM types and state definitions.

#[cfg(not(feature = "std"))]
use alloc::string::String;
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use vo_runtime::gc::Gc;
use vo_runtime::SentinelErrorCache;

use crate::exec::ExternRegistry;
use vo_runtime::itab::ItabCache;

/// Time slice: number of instructions before forced yield check.
pub const TIME_SLICE: u32 = 1000;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExecResult {
    Continue,
    Return,
    Yield,
    Block,  // Channel blocking - don't re-queue, wait for wake
    Panic,
    Done,
    /// OSR request: (func_id, backedge_pc, loop_header_pc)
    Osr(u32, usize, usize),
}

/// Runtime error location for debug info lookup.
#[derive(Debug, Clone, Copy)]
pub struct ErrorLocation {
    pub func_id: u32,
    pub pc: u32,
}

#[derive(Debug)]
pub enum VmError {
    NoEntryFunction,
    InvalidFunctionId(u32),
    StackOverflow,
    StackUnderflow,
    InvalidOpcode(u8),
    DivisionByZero(Option<ErrorLocation>),
    IndexOutOfBounds(Option<ErrorLocation>),
    NilPointerDereference(Option<ErrorLocation>),
    TypeAssertionFailed(Option<ErrorLocation>),
    PanicUnwound { msg: Option<String>, loc: Option<ErrorLocation> },
    SendOnClosedChannel(Option<ErrorLocation>),
}

/// VM mutable state that can be borrowed independently from scheduler.
pub struct VmState {
    pub gc: Gc,
    pub globals: Vec<u64>,
    pub itab_cache: ItabCache,
    pub extern_registry: ExternRegistry,
    pub program_args: Vec<String>,
    /// Per-VM sentinel error cache (reset on each module load).
    pub sentinel_errors: SentinelErrorCache,
}

impl VmState {
    pub fn new() -> Self {
        Self {
            gc: Gc::new(),
            globals: Vec::new(),
            itab_cache: ItabCache::new(),
            extern_registry: ExternRegistry::new(),
            program_args: Vec::new(),
            sentinel_errors: SentinelErrorCache::new(),
        }
    }
}

impl Default for VmState {
    fn default() -> Self {
        Self::new()
    }
}
