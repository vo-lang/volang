//! Multi-threaded M:N scheduler for VM fibers.
//!
//! This module provides a work-stealing scheduler that can run many fibers
//! across multiple OS threads.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Global Ready Queue                        │
//! │                    (Injector<FiberId>)                       │
//! └─────────────────────────────────────────────────────────────┘
//!          │              │              │
//!          ▼              ▼              ▼
//! ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
//! │  Worker 0   │ │  Worker 1   │ │  Worker 2   │  (OS threads)
//! │ Local Queue │ │ Local Queue │ │ Local Queue │
//! └─────────────┘ └─────────────┘ └─────────────┘
//!          ↑              ↑              ↑
//!          └──── steal ───┴──── steal ───┘
//! ```

#[cfg(feature = "multithread")]
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
#[cfg(feature = "multithread")]
use std::sync::Arc;

#[cfg(feature = "multithread")]
use crossbeam_deque::{Injector, Stealer, Worker};
#[cfg(feature = "multithread")]
use parking_lot::{Mutex, RwLock};

use crate::fiber::{Fiber, FiberId, FiberStatus};

/// Thread-safe fiber storage.
#[cfg(feature = "multithread")]
pub struct FiberPool {
    /// All fibers, indexed by ID.
    fibers: RwLock<Vec<Mutex<Fiber>>>,
    /// Next fiber ID.
    next_id: AtomicUsize,
}

#[cfg(feature = "multithread")]
impl FiberPool {
    pub fn new() -> Self {
        Self {
            fibers: RwLock::new(Vec::new()),
            next_id: AtomicUsize::new(0),
        }
    }
    
    /// Create a new fiber and return its ID.
    pub fn create(&self, stack_size: usize) -> FiberId {
        let id = self.next_id.fetch_add(1, Ordering::Relaxed) as FiberId;
        let fiber = Fiber::new(id, stack_size);
        
        let mut fibers = self.fibers.write();
        // Ensure we have space
        while fibers.len() <= id as usize {
            // Placeholder - will be replaced
            fibers.push(Mutex::new(Fiber::new(u32::MAX, 0)));
        }
        fibers[id as usize] = Mutex::new(fiber);
        
        id
    }
    
    /// Get a fiber for reading (locks the specific fiber).
    pub fn with_fiber<F, R>(&self, id: FiberId, f: F) -> Option<R>
    where
        F: FnOnce(&Fiber) -> R,
    {
        let fibers = self.fibers.read();
        fibers.get(id as usize).map(|m| f(&m.lock()))
    }
    
    /// Get a fiber for modification (locks the specific fiber).
    pub fn with_fiber_mut<F, R>(&self, id: FiberId, f: F) -> Option<R>
    where
        F: FnOnce(&mut Fiber) -> R,
    {
        let fibers = self.fibers.read();
        fibers.get(id as usize).map(|m| f(&mut m.lock()))
    }
    
    /// Check if all fibers are dead.
    pub fn all_done(&self) -> bool {
        let fibers = self.fibers.read();
        fibers.iter().all(|m| m.lock().status == FiberStatus::Dead)
    }
    
    /// Count active (non-dead) fibers.
    pub fn active_count(&self) -> usize {
        let fibers = self.fibers.read();
        fibers.iter().filter(|m| m.lock().status != FiberStatus::Dead).count()
    }
}

#[cfg(feature = "multithread")]
impl Default for FiberPool {
    fn default() -> Self {
        Self::new()
    }
}

/// Multi-threaded work-stealing scheduler.
#[cfg(feature = "multithread")]
pub struct MtScheduler {
    /// Fiber storage.
    pub fibers: Arc<FiberPool>,
    /// Global ready queue.
    injector: Injector<FiberId>,
    /// Worker stealers (for work stealing).
    stealers: RwLock<Vec<Stealer<FiberId>>>,
    /// Active worker count.
    active_workers: AtomicUsize,
    /// Shutdown flag.
    shutdown: AtomicBool,
}

#[cfg(feature = "multithread")]
impl MtScheduler {
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            fibers: Arc::new(FiberPool::new()),
            injector: Injector::new(),
            stealers: RwLock::new(Vec::new()),
            active_workers: AtomicUsize::new(0),
            shutdown: AtomicBool::new(false),
        })
    }
    
    /// Spawn a new fiber.
    pub fn spawn(&self, stack_size: usize) -> FiberId {
        let id = self.fibers.create(stack_size);
        self.injector.push(id);
        id
    }
    
    /// Add a fiber to the ready queue.
    pub fn ready(&self, id: FiberId) {
        self.fibers.with_fiber_mut(id, |f| {
            if f.status != FiberStatus::Dead {
                f.status = FiberStatus::Ready;
            }
        });
        self.injector.push(id);
    }
    
    /// Mark a fiber as blocked.
    pub fn block(&self, id: FiberId, reason: crate::fiber::BlockReason) {
        self.fibers.with_fiber_mut(id, |f| {
            f.status = FiberStatus::Blocked;
            f.block_reason = reason;
        });
    }
    
    /// Unblock a fiber.
    pub fn unblock(&self, id: FiberId) {
        self.fibers.with_fiber_mut(id, |f| {
            if f.status == FiberStatus::Blocked {
                f.status = FiberStatus::Ready;
                f.block_reason = crate::fiber::BlockReason::None;
            }
        });
        self.injector.push(id);
    }
    
    /// Mark a fiber as dead.
    pub fn kill(&self, id: FiberId) {
        self.fibers.with_fiber_mut(id, |f| {
            f.status = FiberStatus::Dead;
        });
    }
    
    /// Check if all fibers are done.
    pub fn all_done(&self) -> bool {
        self.fibers.all_done()
    }
    
    /// Shutdown the scheduler.
    pub fn shutdown(&self) {
        self.shutdown.store(true, Ordering::SeqCst);
    }
    
    /// Check if shutdown requested.
    pub fn is_shutdown(&self) -> bool {
        self.shutdown.load(Ordering::SeqCst)
    }
    
    /// Create a worker and return its local queue.
    pub fn create_worker(&self) -> Worker<FiberId> {
        let worker = Worker::new_fifo();
        self.stealers.write().push(worker.stealer());
        self.active_workers.fetch_add(1, Ordering::Relaxed);
        worker
    }
    
    /// Try to get a fiber to run.
    pub fn try_steal(&self, local: &Worker<FiberId>) -> Option<FiberId> {
        // Try local queue first
        if let Some(id) = local.pop() {
            return Some(id);
        }
        
        // Try global queue
        loop {
            match self.injector.steal_batch_and_pop(local) {
                crossbeam_deque::Steal::Success(id) => return Some(id),
                crossbeam_deque::Steal::Empty => break,
                crossbeam_deque::Steal::Retry => continue,
            }
        }
        
        // Try stealing from other workers
        let stealers = self.stealers.read();
        for stealer in stealers.iter() {
            loop {
                match stealer.steal() {
                    crossbeam_deque::Steal::Success(id) => return Some(id),
                    crossbeam_deque::Steal::Empty => break,
                    crossbeam_deque::Steal::Retry => continue,
                }
            }
        }
        
        None
    }
    
    /// Put a fiber back to local queue (for time-slicing).
    pub fn requeue_local(&self, local: &Worker<FiberId>, id: FiberId) {
        self.fibers.with_fiber_mut(id, |f| {
            if f.status == FiberStatus::Running {
                f.status = FiberStatus::Ready;
            }
        });
        local.push(id);
    }
}

#[cfg(feature = "multithread")]
impl Default for MtScheduler {
    fn default() -> Self {
        Self {
            fibers: Arc::new(FiberPool::new()),
            injector: Injector::new(),
            stealers: RwLock::new(Vec::new()),
            active_workers: AtomicUsize::new(0),
            shutdown: AtomicBool::new(false),
        }
    }
}

#[cfg(all(test, feature = "multithread"))]
mod tests {
    use super::*;
    
    #[test]
    fn test_fiber_pool() {
        let pool = FiberPool::new();
        let id1 = pool.create(1024);
        let id2 = pool.create(1024);
        
        assert_eq!(id1, 0);
        assert_eq!(id2, 1);
        
        pool.with_fiber_mut(id1, |f| {
            f.status = FiberStatus::Dead;
        });
        
        assert!(!pool.all_done());
        
        pool.with_fiber_mut(id2, |f| {
            f.status = FiberStatus::Dead;
        });
        
        assert!(pool.all_done());
    }
    
    #[test]
    fn test_scheduler_spawn() {
        let sched = MtScheduler::new();
        let id = sched.spawn(1024);
        
        assert_eq!(id, 0);
        assert!(!sched.all_done());
    }
    
    #[test]
    fn test_work_stealing() {
        let sched = MtScheduler::new();
        
        // Spawn some fibers
        for _ in 0..10 {
            sched.spawn(1024);
        }
        
        // Create worker
        let worker = sched.create_worker();
        
        // Should be able to steal all
        let mut count = 0;
        while let Some(_id) = sched.try_steal(&worker) {
            count += 1;
            if count >= 10 { break; }
        }
        
        assert_eq!(count, 10);
    }
}
