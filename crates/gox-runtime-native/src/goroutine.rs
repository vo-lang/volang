//! Goroutine runtime for AOT-compiled code.
//!
//! This module implements Go-style goroutines using stackful coroutines
//! with M:N scheduling (many goroutines on fewer OS threads).
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Global Scheduler                          │
//! │  ┌─────────┐  ┌─────────┐  ┌─────────┐                      │
//! │  │ Worker  │  │ Worker  │  │ Worker  │  (OS threads)        │
//! │  │ Thread  │  │ Thread  │  │ Thread  │                      │
//! │  └────┬────┘  └────┬────┘  └────┬────┘                      │
//! │       │            │            │                            │
//! │  ┌────▼────┐  ┌────▼────┐  ┌────▼────┐                      │
//! │  │  Local  │  │  Local  │  │  Local  │  (work-stealing)     │
//! │  │  Queue  │  │  Queue  │  │  Queue  │                      │
//! │  └─────────┘  └─────────┘  └─────────┘                      │
//! │       ▲            ▲            ▲                            │
//! │       │    steal   │    steal   │                            │
//! │       └────────────┴────────────┘                            │
//! └─────────────────────────────────────────────────────────────┘
//!
//! Each goroutine has its own stack (via corosensei).
//! Channel operations can suspend/resume goroutines.
//! ```

use std::boxed::Box;
use std::collections::VecDeque;
use std::sync::Arc;
use std::vec::Vec;
use std::cell::UnsafeCell;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};

use corosensei::{Coroutine, CoroutineResult, Yielder};
use crossbeam_deque::{Injector, Stealer, Worker};
use parking_lot::{Condvar, Mutex, RwLock};

use gox_runtime_core::gc::GcRef;

// =============================================================================
// Goroutine
// =============================================================================

/// Goroutine ID.
pub type GoroutineId = u64;

/// Yield reason - why a goroutine suspended.
#[derive(Clone, Copy, Debug)]
pub enum YieldReason {
    /// Voluntary yield (runtime.Gosched).
    Yield,
    /// Blocked on channel send.
    ChanSend(GcRef),
    /// Blocked on channel receive.
    ChanRecv(GcRef),
    /// Goroutine completed.
    Done,
}

/// Input to resume a goroutine.
#[derive(Clone, Copy, Debug)]
pub enum ResumeInput {
    /// Normal resume.
    Continue,
    /// Resume with channel value received.
    ChanValue(u64),
    /// Resume after successful send.
    ChanSendOk,
}

/// A goroutine - a lightweight thread with its own stack.
pub struct Goroutine {
    pub id: GoroutineId,
    coroutine: Option<Coroutine<ResumeInput, YieldReason, ()>>,
    /// For blocked goroutines, store channel value here.
    pub pending_value: Option<u64>,
}

impl Goroutine {
    /// Create a new goroutine that will call the given function.
    pub fn new<F>(id: GoroutineId, f: F) -> Self
    where
        F: FnOnce(&Yielder<ResumeInput, YieldReason>) + Send + 'static,
    {
        let coroutine = Coroutine::new(move |yielder, _input: ResumeInput| {
            f(yielder);
        });
        
        Self {
            id,
            coroutine: Some(coroutine),
            pending_value: None,
        }
    }
    
    /// Resume the goroutine.
    pub fn resume(&mut self, input: ResumeInput) -> YieldReason {
        match self.coroutine.as_mut() {
            Some(coro) => {
                match coro.resume(input) {
                    CoroutineResult::Yield(reason) => reason,
                    CoroutineResult::Return(()) => {
                        self.coroutine = None;
                        YieldReason::Done
                    }
                }
            }
            None => YieldReason::Done,
        }
    }
    
    /// Check if goroutine is done.
    pub fn is_done(&self) -> bool {
        self.coroutine.is_none()
    }
}

// =============================================================================
// Channel
// =============================================================================

/// Channel state.
struct ChannelInner {
    /// Buffered values.
    buffer: VecDeque<u64>,
    /// Buffer capacity (0 = unbuffered).
    capacity: usize,
    /// Is channel closed?
    closed: bool,
    /// Goroutines waiting to send (id, value).
    waiting_senders: VecDeque<(GoroutineId, u64)>,
    /// Goroutines waiting to receive.
    waiting_receivers: VecDeque<GoroutineId>,
}

/// A Go-style channel.
pub struct Channel {
    inner: Mutex<ChannelInner>,
    /// Notify when channel state changes.
    notify: Condvar,
}

impl Channel {
    /// Create a new channel with given capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
            inner: Mutex::new(ChannelInner {
                buffer: VecDeque::with_capacity(capacity),
                capacity,
                closed: false,
                waiting_senders: VecDeque::new(),
                waiting_receivers: VecDeque::new(),
            }),
            notify: Condvar::new(),
        }
    }
    
    /// Try to send without blocking. Returns:
    /// - Ok(Some(receiver_id)) - sent directly to waiting receiver
    /// - Ok(None) - buffered successfully
    /// - Err(()) - would block or channel closed
    pub fn try_send(&self, value: u64) -> Result<Option<GoroutineId>, ()> {
        let mut inner = self.inner.lock();
        
        if inner.closed {
            return Err(());
        }
        
        // Check for waiting receiver first
        if let Some(receiver_id) = inner.waiting_receivers.pop_front() {
            return Ok(Some(receiver_id));
        }
        
        // Try to buffer
        if inner.buffer.len() < inner.capacity {
            inner.buffer.push_back(value);
            return Ok(None);
        }
        
        Err(())
    }
    
    /// Register as waiting sender.
    pub fn register_sender(&self, goroutine_id: GoroutineId, value: u64) {
        let mut inner = self.inner.lock();
        inner.waiting_senders.push_back((goroutine_id, value));
    }
    
    /// Try to receive without blocking. Returns:
    /// - Ok((value, Some(sender_id))) - received from buffer, woke up sender
    /// - Ok((value, None)) - received from buffer
    /// - Err(true) - channel closed
    /// - Err(false) - would block
    pub fn try_recv(&self) -> Result<(u64, Option<GoroutineId>), bool> {
        let mut inner = self.inner.lock();
        
        // Check buffer first
        if let Some(value) = inner.buffer.pop_front() {
            // If there's a waiting sender, move their value to buffer
            let woke_sender = if let Some((sender_id, sender_value)) = inner.waiting_senders.pop_front() {
                inner.buffer.push_back(sender_value);
                Some(sender_id)
            } else {
                None
            };
            return Ok((value, woke_sender));
        }
        
        // Check for waiting sender (unbuffered case)
        if let Some((sender_id, value)) = inner.waiting_senders.pop_front() {
            return Ok((value, Some(sender_id)));
        }
        
        // Channel empty
        if inner.closed {
            Err(true)
        } else {
            Err(false)
        }
    }
    
    /// Register as waiting receiver.
    pub fn register_receiver(&self, goroutine_id: GoroutineId) {
        let mut inner = self.inner.lock();
        inner.waiting_receivers.push_back(goroutine_id);
    }
    
    /// Close the channel.
    pub fn close(&self) {
        let mut inner = self.inner.lock();
        inner.closed = true;
        self.notify.notify_all();
    }
    
    /// Check if closed.
    pub fn is_closed(&self) -> bool {
        self.inner.lock().closed
    }
    
    /// Get all waiting receivers (for waking on close).
    pub fn take_waiting_receivers(&self) -> Vec<GoroutineId> {
        let mut inner = self.inner.lock();
        inner.waiting_receivers.drain(..).collect()
    }
    
    /// Get all waiting senders (for waking on close).
    pub fn take_waiting_senders(&self) -> Vec<(GoroutineId, u64)> {
        let mut inner = self.inner.lock();
        inner.waiting_senders.drain(..).collect()
    }
}

// =============================================================================
// M:N Scheduler
// =============================================================================

/// Task to run.
enum Task {
    /// New goroutine to start.
    New(Box<Goroutine>),
    /// Existing goroutine to resume.
    Resume(Box<Goroutine>, ResumeInput),
}

/// Per-worker state.
#[allow(dead_code)]
struct WorkerState {
    /// Local work queue.
    queue: Worker<Task>,
    /// Stealer handle for other workers.
    stealer: Stealer<Task>,
}

/// Global scheduler state.
pub struct Scheduler {
    /// Global task injector.
    injector: Injector<Task>,
    /// Worker stealers (for work stealing).
    stealers: RwLock<Vec<Stealer<Task>>>,
    /// Next goroutine ID.
    next_id: AtomicU64,
    /// Number of active goroutines.
    active_count: AtomicUsize,
    /// Channels by GcRef.
    channels: RwLock<Vec<(GcRef, Arc<Channel>)>>,
    /// Blocked goroutines by ID.
    blocked: RwLock<Vec<(GoroutineId, Box<Goroutine>)>>,
    /// Shutdown flag.
    shutdown: AtomicBool,
}

impl Scheduler {
    /// Create a new scheduler.
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            injector: Injector::new(),
            stealers: RwLock::new(Vec::new()),
            next_id: AtomicU64::new(1),
            active_count: AtomicUsize::new(0),
            channels: RwLock::new(Vec::new()),
            blocked: RwLock::new(Vec::new()),
            shutdown: AtomicBool::new(false),
        })
    }
    
    /// Allocate a new goroutine ID.
    pub fn next_goroutine_id(&self) -> GoroutineId {
        self.next_id.fetch_add(1, Ordering::Relaxed)
    }
    
    /// Spawn a new goroutine.
    pub fn spawn<F>(&self, f: F) -> GoroutineId
    where
        F: FnOnce(&Yielder<ResumeInput, YieldReason>) + Send + 'static,
    {
        let id = self.next_goroutine_id();
        let goroutine = Goroutine::new(id, f);
        self.active_count.fetch_add(1, Ordering::Relaxed);
        self.injector.push(Task::New(Box::new(goroutine)));
        id
    }
    
    /// Create or get a channel.
    pub fn get_or_create_channel(&self, chan_ref: GcRef, capacity: usize) -> Arc<Channel> {
        {
            let channels = self.channels.read();
            if let Some((_, ch)) = channels.iter().find(|(r, _)| *r == chan_ref) {
                return ch.clone();
            }
        }
        
        let mut channels = self.channels.write();
        // Double-check after acquiring write lock
        if let Some((_, ch)) = channels.iter().find(|(r, _)| *r == chan_ref) {
            return ch.clone();
        }
        
        let ch = Arc::new(Channel::new(capacity));
        channels.push((chan_ref, ch.clone()));
        ch
    }
    
    /// Get channel by ref.
    pub fn get_channel(&self, chan_ref: GcRef) -> Option<Arc<Channel>> {
        let channels = self.channels.read();
        channels.iter().find(|(r, _)| *r == chan_ref).map(|(_, ch)| ch.clone())
    }
    
    /// Block a goroutine.
    pub fn block_goroutine(&self, goroutine: Box<Goroutine>) {
        let mut blocked = self.blocked.write();
        blocked.push((goroutine.id, goroutine));
    }
    
    /// Unblock a goroutine by ID.
    pub fn unblock_goroutine(&self, id: GoroutineId, input: ResumeInput) {
        let mut blocked = self.blocked.write();
        if let Some(pos) = blocked.iter().position(|(gid, _)| *gid == id) {
            let (_, mut goroutine) = blocked.remove(pos);
            if let ResumeInput::ChanValue(v) = input {
                goroutine.pending_value = Some(v);
            }
            drop(blocked);
            self.injector.push(Task::Resume(goroutine, input));
        }
    }
    
    /// Run the scheduler on current thread until all goroutines complete.
    pub fn run_until_complete(self: &Arc<Self>) {
        let worker = Worker::new_fifo();
        {
            let mut stealers = self.stealers.write();
            stealers.push(worker.stealer());
        }
        
        loop {
            // Check shutdown
            if self.shutdown.load(Ordering::Relaxed) {
                break;
            }
            
            // Try to get a task
            let task = worker.pop().or_else(|| {
                // Try global queue
                loop {
                    match self.injector.steal_batch_and_pop(&worker) {
                        crossbeam_deque::Steal::Success(task) => return Some(task),
                        crossbeam_deque::Steal::Empty => break,
                        crossbeam_deque::Steal::Retry => continue,
                    }
                }
                
                // Try stealing from other workers
                let stealers = self.stealers.read();
                for stealer in stealers.iter() {
                    loop {
                        match stealer.steal() {
                            crossbeam_deque::Steal::Success(task) => return Some(task),
                            crossbeam_deque::Steal::Empty => break,
                            crossbeam_deque::Steal::Retry => continue,
                        }
                    }
                }
                None
            });
            
            match task {
                Some(Task::New(mut goroutine)) => {
                    let reason = goroutine.resume(ResumeInput::Continue);
                    self.handle_yield(goroutine, reason, &worker);
                }
                Some(Task::Resume(mut goroutine, input)) => {
                    let reason = goroutine.resume(input);
                    self.handle_yield(goroutine, reason, &worker);
                }
                None => {
                    // No tasks available
                    if self.active_count.load(Ordering::Relaxed) == 0 {
                        break;
                    }
                    // Spin briefly before yielding
                    std::hint::spin_loop();
                }
            }
        }
    }
    
    /// Handle goroutine yield.
    fn handle_yield(&self, goroutine: Box<Goroutine>, reason: YieldReason, worker: &Worker<Task>) {
        match reason {
            YieldReason::Yield => {
                // Re-queue for later execution
                worker.push(Task::Resume(goroutine, ResumeInput::Continue));
            }
            YieldReason::ChanSend(_chan_ref) => {
                // Goroutine is blocked on send
                self.block_goroutine(goroutine);
            }
            YieldReason::ChanRecv(_chan_ref) => {
                // Goroutine is blocked on receive
                self.block_goroutine(goroutine);
            }
            YieldReason::Done => {
                // Goroutine completed
                self.active_count.fetch_sub(1, Ordering::Relaxed);
            }
        }
    }
    
    /// Shutdown the scheduler.
    pub fn shutdown(&self) {
        self.shutdown.store(true, Ordering::Relaxed);
    }
}

impl Default for Scheduler {
    fn default() -> Self {
        Self {
            injector: Injector::new(),
            stealers: RwLock::new(Vec::new()),
            next_id: AtomicU64::new(1),
            active_count: AtomicUsize::new(0),
            channels: RwLock::new(Vec::new()),
            blocked: RwLock::new(Vec::new()),
            shutdown: AtomicBool::new(false),
        }
    }
}

// =============================================================================
// Thread-local Yielder Access
// =============================================================================

thread_local! {
    static CURRENT_YIELDER: UnsafeCell<Option<*const Yielder<ResumeInput, YieldReason>>> = 
        UnsafeCell::new(None);
    static CURRENT_SCHEDULER: UnsafeCell<Option<*const Scheduler>> = 
        UnsafeCell::new(None);
}

/// Set current yielder for this thread.
pub fn set_current_yielder(yielder: &Yielder<ResumeInput, YieldReason>) {
    CURRENT_YIELDER.with(|y| unsafe {
        *y.get() = Some(yielder as *const _);
    });
}

/// Clear current yielder.
pub fn clear_current_yielder() {
    CURRENT_YIELDER.with(|y| unsafe {
        *y.get() = None;
    });
}

/// Get current yielder.
pub fn current_yielder() -> Option<&'static Yielder<ResumeInput, YieldReason>> {
    CURRENT_YIELDER.with(|y| unsafe {
        (*y.get()).map(|p| &*p)
    })
}

/// Set current scheduler.
pub fn set_current_scheduler(scheduler: &Scheduler) {
    CURRENT_SCHEDULER.with(|s| unsafe {
        *s.get() = Some(scheduler as *const _);
    });
}

/// Get current scheduler.
pub fn current_scheduler() -> Option<&'static Scheduler> {
    CURRENT_SCHEDULER.with(|s| unsafe {
        (*s.get()).map(|p| &*p)
    })
}

// =============================================================================
// C ABI Functions for AOT
// =============================================================================

/// Yield current goroutine (runtime.Gosched).
#[no_mangle]
pub extern "C" fn gox_yield() {
    if let Some(yielder) = current_yielder() {
        yielder.suspend(YieldReason::Yield);
    }
}

/// Send value to channel. Returns 1 on success, 0 if channel closed.
#[no_mangle]
pub unsafe extern "C" fn gox_chan_send(chan_ref: GcRef, value: u64) -> u8 {
    let scheduler = match current_scheduler() {
        Some(s) => s,
        None => return 0,
    };
    
    let channel = match scheduler.get_channel(chan_ref) {
        Some(ch) => ch,
        None => return 0,
    };
    
    if channel.is_closed() {
        return 0;
    }
    
    // Try non-blocking send first
    match channel.try_send(value) {
        Ok(Some(receiver_id)) => {
            // Sent directly to waiting receiver
            scheduler.unblock_goroutine(receiver_id, ResumeInput::ChanValue(value));
            return 1;
        }
        Ok(None) => {
            // Buffered successfully
            return 1;
        }
        Err(()) => {
            // Need to block
        }
    }
    
    // Block current goroutine
    if let Some(yielder) = current_yielder() {
        // Register as waiting sender
        // Note: goroutine ID is not directly available here, 
        // but the scheduler handles this via the blocked list
        yielder.suspend(YieldReason::ChanSend(chan_ref));
        1
    } else {
        0
    }
}

/// Receive from channel. Returns value, sets *ok to 1 on success, 0 if closed.
#[no_mangle]
pub unsafe extern "C" fn gox_chan_recv(chan_ref: GcRef, ok: *mut u8) -> u64 {
    let scheduler = match current_scheduler() {
        Some(s) => s,
        None => {
            if !ok.is_null() { *ok = 0; }
            return 0;
        }
    };
    
    let channel = match scheduler.get_channel(chan_ref) {
        Some(ch) => ch,
        None => {
            if !ok.is_null() { *ok = 0; }
            return 0;
        }
    };
    
    // Try non-blocking receive
    match channel.try_recv() {
        Ok((value, woke_sender)) => {
            if let Some(sender_id) = woke_sender {
                scheduler.unblock_goroutine(sender_id, ResumeInput::ChanSendOk);
            }
            if !ok.is_null() { *ok = 1; }
            return value;
        }
        Err(true) => {
            // Channel closed
            if !ok.is_null() { *ok = 0; }
            return 0;
        }
        Err(false) => {
            // Need to block
        }
    }
    
    // Block current goroutine
    if let Some(yielder) = current_yielder() {
        let input = yielder.suspend(YieldReason::ChanRecv(chan_ref));
        if let ResumeInput::ChanValue(v) = input {
            if !ok.is_null() { *ok = 1; }
            return v;
        }
    }
    
    if !ok.is_null() { *ok = 0; }
    0
}

/// Close a channel.
#[no_mangle]
pub unsafe extern "C" fn gox_chan_close(chan_ref: GcRef) {
    let scheduler = match current_scheduler() {
        Some(s) => s,
        None => return,
    };
    
    let channel = match scheduler.get_channel(chan_ref) {
        Some(ch) => ch,
        None => return,
    };
    
    channel.close();
    
    // Wake all waiting receivers with zero value
    for receiver_id in channel.take_waiting_receivers() {
        scheduler.unblock_goroutine(receiver_id, ResumeInput::ChanValue(0));
    }
    
    // Wake all waiting senders (they'll see channel is closed)
    for (sender_id, _) in channel.take_waiting_senders() {
        scheduler.unblock_goroutine(sender_id, ResumeInput::ChanSendOk);
    }
}

/// Spawn a new goroutine with the given function pointer and arguments.
/// 
/// Uses libffi for dynamic function calls - supports unlimited arguments.
/// 
/// # Safety
/// - `func_ptr` must be a valid function pointer
/// - `args_ptr` must point to valid memory containing `arg_count` u64 values
#[no_mangle]
pub unsafe extern "C" fn gox_go_spawn(func_ptr: u64, args_ptr: *const u64, arg_count: u64) {
    let scheduler = match current_scheduler() {
        Some(s) => s,
        None => return,
    };
    
    // Copy arguments
    let args: Vec<u64> = if !args_ptr.is_null() && arg_count > 0 {
        std::slice::from_raw_parts(args_ptr, arg_count as usize).to_vec()
    } else {
        Vec::new()
    };
    
    let func_addr = func_ptr;
    
    scheduler.spawn(move |_yielder| {
        use libffi::low::{ffi_cif, ffi_type, ffi_abi_FFI_DEFAULT_ABI, prep_cif, call, CodePtr};
        
        unsafe {
            let arg_count = args.len();
            
            // Build argument types array (all u64/i64)
            let mut arg_types: Vec<*mut ffi_type> = Vec::with_capacity(arg_count);
            for _ in 0..arg_count {
                arg_types.push(std::ptr::addr_of_mut!(libffi::low::types::sint64));
            }
            
            // Prepare CIF (Call Interface)
            let mut cif: ffi_cif = std::mem::zeroed();
            prep_cif(
                &mut cif,
                ffi_abi_FFI_DEFAULT_ABI,
                arg_count,
                std::ptr::addr_of_mut!(libffi::low::types::void),  // return type: void
                if arg_count > 0 { arg_types.as_mut_ptr() } else { std::ptr::null_mut() },
            ).expect("go: libffi prep_cif failed");
            
            // Build argument values array
            let mut arg_values: Vec<*mut std::ffi::c_void> = Vec::with_capacity(arg_count);
            let mut arg_storage = args.clone();  // Need mutable storage
            for i in 0..arg_count {
                arg_values.push(&mut arg_storage[i] as *mut u64 as *mut _);
            }
            
            // Call the function
            let code_ptr = CodePtr::from_ptr(func_addr as *const _);
            call::<()>(
                &mut cif,
                code_ptr,
                if arg_count > 0 { arg_values.as_mut_ptr() } else { std::ptr::null_mut() },
            );
        }
    });
}

// =============================================================================
// Defer/Panic/Recover
// =============================================================================

/// A deferred function call entry.
#[derive(Clone)]
struct DeferEntry {
    func_ptr: u64,
    args: Vec<u64>,
}

thread_local! {
    /// Stack of deferred function calls for current thread.
    static DEFER_STACK: UnsafeCell<Vec<DeferEntry>> = UnsafeCell::new(Vec::new());
    /// Current panic value (if panicking).
    static PANIC_VALUE: UnsafeCell<Option<u64>> = UnsafeCell::new(None);
    /// Whether recover() was called during panic unwinding.
    static RECOVERING: UnsafeCell<bool> = UnsafeCell::new(false);
}

/// Push a deferred function call.
#[no_mangle]
pub unsafe extern "C" fn gox_defer_push(func_ptr: u64, args_ptr: *const u64, arg_count: u64) {
    let args: Vec<u64> = if !args_ptr.is_null() && arg_count > 0 {
        std::slice::from_raw_parts(args_ptr, arg_count as usize).to_vec()
    } else {
        Vec::new()
    };
    
    DEFER_STACK.with(|stack| {
        (*stack.get()).push(DeferEntry { func_ptr, args });
    });
}

/// Pop and execute all deferred functions (LIFO order).
#[no_mangle]
pub unsafe extern "C" fn gox_defer_pop() {
    // Pop all defers and execute in reverse order
    let defers: Vec<DeferEntry> = DEFER_STACK.with(|stack| {
        std::mem::take(&mut *stack.get())
    });
    
    for entry in defers.into_iter().rev() {
        execute_deferred_call(entry.func_ptr, &entry.args);
    }
}

/// Execute a deferred function call using libffi.
unsafe fn execute_deferred_call(func_ptr: u64, args: &[u64]) {
    use libffi::low::{ffi_cif, ffi_type, ffi_abi_FFI_DEFAULT_ABI, prep_cif, call, CodePtr};
    
    let arg_count = args.len();
    
    // Build argument types array (all u64/i64)
    let mut arg_types: Vec<*mut ffi_type> = Vec::with_capacity(arg_count);
    for _ in 0..arg_count {
        arg_types.push(std::ptr::addr_of_mut!(libffi::low::types::sint64));
    }
    
    // Prepare CIF
    let mut cif: ffi_cif = std::mem::zeroed();
    prep_cif(
        &mut cif,
        ffi_abi_FFI_DEFAULT_ABI,
        arg_count,
        std::ptr::addr_of_mut!(libffi::low::types::void),
        if arg_count > 0 { arg_types.as_mut_ptr() } else { std::ptr::null_mut() },
    ).expect("defer: libffi prep_cif failed");
    
    // Build argument values array
    let mut arg_values: Vec<*mut std::ffi::c_void> = Vec::with_capacity(arg_count);
    let mut arg_storage = args.to_vec();
    for i in 0..arg_count {
        arg_values.push(&mut arg_storage[i] as *mut u64 as *mut _);
    }
    
    // Call the function
    let code_ptr = CodePtr::from_ptr(func_ptr as *const _);
    call::<()>(
        &mut cif,
        code_ptr,
        if arg_count > 0 { arg_values.as_mut_ptr() } else { std::ptr::null_mut() },
    );
}

/// Panic with a value. This executes defers and then aborts if not recovered.
#[no_mangle]
pub unsafe extern "C" fn gox_panic(value: u64) {
    // Set panic value
    PANIC_VALUE.with(|pv| {
        *pv.get() = Some(value);
    });
    
    // Execute all defers
    let defers: Vec<DeferEntry> = DEFER_STACK.with(|stack| {
        std::mem::take(&mut *stack.get())
    });
    
    for entry in defers.into_iter().rev() {
        // Check if recovered
        let recovered = RECOVERING.with(|r| *r.get());
        if recovered {
            RECOVERING.with(|r| *r.get() = false);
            PANIC_VALUE.with(|pv| *pv.get() = None);
            return;
        }
        
        execute_deferred_call(entry.func_ptr, &entry.args);
    }
    
    // Check if recovered after all defers
    let recovered = RECOVERING.with(|r| *r.get());
    if recovered {
        RECOVERING.with(|r| *r.get() = false);
        PANIC_VALUE.with(|pv| *pv.get() = None);
        return;
    }
    
    // Not recovered - abort
    eprintln!("panic: {}", value);
    std::process::abort();
}

/// Recover from panic. Returns panic value if panicking, 0 otherwise.
#[no_mangle]
pub unsafe extern "C" fn gox_recover() -> u64 {
    PANIC_VALUE.with(|pv| {
        if let Some(val) = (*pv.get()).take() {
            RECOVERING.with(|r| *r.get() = true);
            val
        } else {
            0
        }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_goroutine_basic() {
        let scheduler = Scheduler::new();
        let counter = Arc::new(AtomicUsize::new(0));
        let counter_clone = counter.clone();
        
        scheduler.spawn(move |_yielder| {
            counter_clone.fetch_add(1, Ordering::Relaxed);
        });
        
        scheduler.run_until_complete();
        assert_eq!(counter.load(Ordering::Relaxed), 1);
    }
    
    #[test]
    fn test_multiple_goroutines() {
        let scheduler = Scheduler::new();
        let counter = Arc::new(AtomicUsize::new(0));
        
        for _ in 0..10 {
            let counter_clone = counter.clone();
            scheduler.spawn(move |_yielder| {
                counter_clone.fetch_add(1, Ordering::Relaxed);
            });
        }
        
        scheduler.run_until_complete();
        assert_eq!(counter.load(Ordering::Relaxed), 10);
    }
    
    #[test]
    fn test_yield() {
        let scheduler = Scheduler::new();
        let order = Arc::new(Mutex::new(Vec::new()));
        
        let order1 = order.clone();
        scheduler.spawn(move |yielder| {
            order1.lock().push(1);
            yielder.suspend(YieldReason::Yield);
            order1.lock().push(3);
        });
        
        let order2 = order.clone();
        scheduler.spawn(move |yielder| {
            order2.lock().push(2);
            yielder.suspend(YieldReason::Yield);
            order2.lock().push(4);
        });
        
        scheduler.run_until_complete();
        
        let result = order.lock();
        // Both goroutines should interleave
        assert_eq!(result.len(), 4);
    }
    
    #[test]
    fn test_channel_buffered() {
        let channel = Channel::new(2);
        
        // Send to buffered channel (non-blocking)
        assert!(channel.try_send(42).is_ok());
        assert!(channel.try_send(100).is_ok());
        
        // Buffer full, should fail
        assert!(channel.try_send(200).is_err());
        
        // Receive
        let (val, _) = channel.try_recv().unwrap();
        assert_eq!(val, 42);
        
        let (val, _) = channel.try_recv().unwrap();
        assert_eq!(val, 100);
        
        // Empty, should fail
        assert!(channel.try_recv().is_err());
    }
    
    #[test]
    fn test_channel_close() {
        let channel = Channel::new(1);
        
        channel.try_send(42).unwrap();
        channel.close();
        
        // Can still receive from closed channel
        let (val, _) = channel.try_recv().unwrap();
        assert_eq!(val, 42);
        
        // Empty closed channel returns closed error
        assert!(matches!(channel.try_recv(), Err(true)));
        
        // Send to closed channel fails
        assert!(channel.try_send(100).is_err());
    }
    
    #[test]
    fn test_defer_basic() {
        use std::sync::atomic::AtomicU64;
        
        let counter = Arc::new(AtomicU64::new(0));
        let counter_clone = counter.clone();
        
        // Simulate a deferred increment function
        extern "C" fn increment(ptr: u64) {
            unsafe {
                let counter = &*(ptr as *const AtomicU64);
                counter.fetch_add(1, Ordering::SeqCst);
            }
        }
        
        unsafe {
            // Push a defer
            let counter_ptr = Arc::as_ptr(&counter_clone) as u64;
            let args = [counter_ptr];
            gox_defer_push(increment as u64, args.as_ptr(), 1);
            
            // Counter should still be 0
            assert_eq!(counter.load(Ordering::SeqCst), 0);
            
            // Pop defers - should execute increment
            gox_defer_pop();
            
            // Counter should now be 1
            assert_eq!(counter.load(Ordering::SeqCst), 1);
        }
    }
    
    #[test]
    fn test_defer_lifo_order() {
        use std::sync::atomic::AtomicU64;
        
        let value = Arc::new(AtomicU64::new(0));
        
        // Functions that multiply/add to track order
        extern "C" fn mul_10(ptr: u64) {
            unsafe {
                let v = &*(ptr as *const AtomicU64);
                let old = v.load(Ordering::SeqCst);
                v.store(old * 10, Ordering::SeqCst);
            }
        }
        
        extern "C" fn add_1(ptr: u64) {
            unsafe {
                let v = &*(ptr as *const AtomicU64);
                v.fetch_add(1, Ordering::SeqCst);
            }
        }
        
        extern "C" fn add_2(ptr: u64) {
            unsafe {
                let v = &*(ptr as *const AtomicU64);
                v.fetch_add(2, Ordering::SeqCst);
            }
        }
        
        unsafe {
            let ptr = Arc::as_ptr(&value) as u64;
            let args = [ptr];
            
            // Push: mul_10, add_1, add_2
            // LIFO execution: add_2, add_1, mul_10
            // Result: ((0 + 2) + 1) * 10 = 30
            gox_defer_push(mul_10 as u64, args.as_ptr(), 1);
            gox_defer_push(add_1 as u64, args.as_ptr(), 1);
            gox_defer_push(add_2 as u64, args.as_ptr(), 1);
            
            gox_defer_pop();
            
            assert_eq!(value.load(Ordering::SeqCst), 30);
        }
    }
    
    #[test]
    fn test_recover_no_panic() {
        unsafe {
            // Recover when not panicking should return 0
            let val = gox_recover();
            assert_eq!(val, 0);
        }
    }
}
