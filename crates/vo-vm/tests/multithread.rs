//! Multi-threaded M:N scheduler integration tests.

#![cfg(feature = "multithread")]

use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Instant;

use vo_vm::scheduler::MtScheduler;
use parking_lot::Mutex;

/// Test that multiple threads can process fibers in parallel.
#[test]
fn test_parallel_fiber_execution() {
    let sched = Arc::new(MtScheduler::default());
    let counter = Arc::new(AtomicU64::new(0));
    let thread_ids = Arc::new(Mutex::new(std::collections::HashSet::new()));
    
    const NUM_FIBERS: usize = 1000;
    const NUM_WORKERS: usize = 4;
    
    // Spawn fibers
    for _ in 0..NUM_FIBERS {
        sched.spawn(64);
    }
    
    let start = Instant::now();
    
    // Spawn worker threads
    let handles: Vec<_> = (0..NUM_WORKERS).map(|worker_id| {
        let sched = Arc::clone(&sched);
        let counter = Arc::clone(&counter);
        let thread_ids = Arc::clone(&thread_ids);
        
        thread::spawn(move || {
            let worker = sched.create_worker();
            let my_thread_id = thread::current().id();
            
            // Record this thread participated
            thread_ids.lock().insert(format!("{:?}", my_thread_id));
            
            let mut local_count = 0u64;
            
            loop {
                if sched.is_shutdown() {
                    break;
                }
                
                if let Some(fiber_id) = sched.try_steal(&worker) {
                    // Simulate work - compute something
                    let mut sum = 0u64;
                    for i in 0..1000 {
                        sum = sum.wrapping_add(i);
                    }
                    std::hint::black_box(sum);
                    
                    local_count += 1;
                    let prev = counter.fetch_add(1, Ordering::Relaxed);
                    
                    // Mark fiber as dead
                    sched.kill(fiber_id);
                    
                    // Check if all done
                    if prev + 1 >= NUM_FIBERS as u64 {
                        sched.shutdown();
                        break;
                    }
                } else {
                    // No work available
                    if sched.all_done() {
                        break;
                    }
                    thread::yield_now();
                }
            }
            
            (worker_id, local_count)
        })
    }).collect();
    
    // Collect results
    let mut results = Vec::new();
    for h in handles {
        results.push(h.join().unwrap());
    }
    
    let elapsed = start.elapsed();
    let final_count = counter.load(Ordering::Relaxed);
    let num_threads = thread_ids.lock().len();
    
    println!("\n=== Parallel Execution Test ===");
    println!("Total fibers: {}", NUM_FIBERS);
    println!("Worker threads: {}", NUM_WORKERS);
    println!("Time: {:?}", elapsed);
    println!("Threads participated: {}", num_threads);
    println!("\nPer-worker distribution:");
    for (id, count) in &results {
        let pct = (*count as f64 / NUM_FIBERS as f64) * 100.0;
        println!("  Worker {}: {} fibers ({:.1}%)", id, count, pct);
    }
    
    // Verify
    assert_eq!(final_count, NUM_FIBERS as u64, "All fibers should be processed");
    assert!(num_threads > 1, "Multiple threads should participate");
    
    // Check work distribution (no single thread should do all the work)
    let max_work = results.iter().map(|(_, c)| *c).max().unwrap();
    let min_work = results.iter().map(|(_, c)| *c).min().unwrap();
    
    // Allow some imbalance but not extreme
    assert!(
        max_work < (NUM_FIBERS as u64 * 8 / 10),
        "Work should be distributed (max worker did {} of {})", max_work, NUM_FIBERS
    );
    
    println!("Work balance: min={}, max={}", min_work, max_work);
    println!("✓ Test passed!\n");
}

/// Test work stealing under contention.
#[test]
fn test_work_stealing_contention() {
    let sched = Arc::new(MtScheduler::default());
    
    // Use more fibers and barrier to ensure fair start
    const NUM_FIBERS: usize = 10000;
    const NUM_WORKERS: usize = 4;
    
    let stolen_count = Arc::new(AtomicU64::new(0));
    let ready = Arc::new(AtomicU64::new(0));
    
    // Start workers first, they'll wait for fibers
    let handles: Vec<_> = (0..NUM_WORKERS).map(|_| {
        let sched = Arc::clone(&sched);
        let stolen_count = Arc::clone(&stolen_count);
        let ready = Arc::clone(&ready);
        
        thread::spawn(move || {
            let worker = sched.create_worker();
            
            // Signal ready
            ready.fetch_add(1, Ordering::SeqCst);
            
            // Wait for all workers to be ready
            while ready.load(Ordering::SeqCst) < NUM_WORKERS as u64 + 1 {
                thread::yield_now();
            }
            
            let mut count = 0u64;
            
            loop {
                if sched.is_shutdown() {
                    break;
                }
                
                if let Some(fiber_id) = sched.try_steal(&worker) {
                    count += 1;
                    sched.kill(fiber_id);
                    
                    let total = stolen_count.fetch_add(1, Ordering::Relaxed) + 1;
                    if total >= NUM_FIBERS as u64 {
                        sched.shutdown();
                        break;
                    }
                } else {
                    if stolen_count.load(Ordering::Relaxed) >= NUM_FIBERS as u64 {
                        break;
                    }
                    thread::yield_now();
                }
            }
            
            count
        })
    }).collect();
    
    // Wait for workers to be ready
    while ready.load(Ordering::SeqCst) < NUM_WORKERS as u64 {
        thread::yield_now();
    }
    
    // Now spawn fibers
    for _ in 0..NUM_FIBERS {
        sched.spawn(32);
    }
    
    // Signal workers to start
    ready.fetch_add(1, Ordering::SeqCst);
    
    let results: Vec<u64> = handles.into_iter().map(|h| h.join().unwrap()).collect();
    let total: u64 = results.iter().sum();
    
    println!("\n=== Work Stealing Contention Test ===");
    println!("Fibers: {}, Workers: {}", NUM_FIBERS, NUM_WORKERS);
    println!("Distribution: {:?}", results);
    println!("Total processed: {}", total);
    
    assert_eq!(total, NUM_FIBERS as u64);
    
    // Most workers should have gotten some work
    let active_workers = results.iter().filter(|&&c| c > 0).count();
    println!("Active workers: {}/{}", active_workers, NUM_WORKERS);
    println!("✓ Test passed!\n");
}

/// Test scheduler shutdown.
#[test]
fn test_scheduler_shutdown() {
    let sched = Arc::new(MtScheduler::default());
    let started = Arc::new(AtomicU64::new(0));
    
    // Spawn some fibers
    for _ in 0..10 {
        sched.spawn(32);
    }
    
    let handles: Vec<_> = (0..4).map(|_| {
        let sched = Arc::clone(&sched);
        let started = Arc::clone(&started);
        
        thread::spawn(move || {
            let worker = sched.create_worker();
            started.fetch_add(1, Ordering::SeqCst);
            
            loop {
                if sched.is_shutdown() {
                    return "shutdown";
                }
                
                let _ = sched.try_steal(&worker);
                
                // Small sleep to avoid busy-waiting
                thread::sleep(std::time::Duration::from_micros(100));
            }
        })
    }).collect();
    
    // Wait for all workers to start
    while started.load(Ordering::SeqCst) < 4 {
        thread::yield_now();
    }
    
    // Give workers time to enter their loop
    thread::sleep(std::time::Duration::from_millis(5));
    
    // Shutdown
    sched.shutdown();
    
    // All workers should exit via shutdown
    for h in handles {
        let reason = h.join().unwrap();
        assert_eq!(reason, "shutdown");
    }
    
    println!("\n=== Shutdown Test ===");
    println!("✓ All workers exited cleanly via shutdown\n");
}
