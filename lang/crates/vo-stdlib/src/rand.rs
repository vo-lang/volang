//! math/rand - Pseudo-random number generation
//!
//! Uses xoroshiro128++ algorithm, automatically seeded at startup.

use core::cell::RefCell;
use core::sync::atomic::{AtomicU64, Ordering};

use vo_ffi_macro::vostd_fn;

/// xoroshiro128++ state
struct Rng {
    s0: u64,
    s1: u64,
}

impl Rng {
    fn new(seed: u64) -> Self {
        // Use splitmix64 to expand seed into two state words
        let mut z = seed;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
        let s0 = z ^ (z >> 31);
        
        z = seed.wrapping_add(0x9e3779b97f4a7c15);
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
        let s1 = z ^ (z >> 31);
        
        // Ensure state is not all zeros
        if s0 == 0 && s1 == 0 {
            Self { s0: 1, s1: 1 }
        } else {
            Self { s0, s1 }
        }
    }
    
    #[inline]
    fn next_u64(&mut self) -> u64 {
        let s0 = self.s0;
        let mut s1 = self.s1;
        
        // xoroshiro128++ output function
        let result = s0.wrapping_add(s1).rotate_left(17).wrapping_add(s0);
        
        // State update
        s1 ^= s0;
        self.s0 = s0.rotate_left(49) ^ s1 ^ (s1 << 21);
        self.s1 = s1.rotate_left(28);
        
        result
    }
}

// Global RNG with automatic seeding
thread_local! {
    static RNG: RefCell<Rng> = RefCell::new(Rng::new(auto_seed()));
}

// Counter for uniqueness
static SEED_COUNTER: AtomicU64 = AtomicU64::new(0);

#[cfg(feature = "std")]
fn auto_seed() -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let mut hasher = DefaultHasher::new();
    std::process::id().hash(&mut hasher);
    std::thread::current().id().hash(&mut hasher);
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0)
        .hash(&mut hasher);
    SEED_COUNTER.fetch_add(1, Ordering::Relaxed).hash(&mut hasher);
    hasher.finish()
}

#[cfg(not(feature = "std"))]
fn auto_seed() -> u64 {
    // In no_std, use counter + some constant mixing
    let count = SEED_COUNTER.fetch_add(1, Ordering::Relaxed);
    let mut z = count ^ 0xdeadbeef12345678;
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
    z ^ (z >> 31)
}

#[inline]
fn with_rng<T>(f: impl FnOnce(&mut Rng) -> T) -> T {
    RNG.with(|rng| f(&mut rng.borrow_mut()))
}

/// Generate a random u64. For internal use by other stdlib modules.
pub fn next_u64() -> u64 {
    with_rng(|rng| rng.next_u64())
}

// Generate bounded random integer in [0, n) using Lemire's method
#[inline]
fn bounded_int(n: u64) -> u64 {
    with_rng(|rng| {
        // Fast path for power of 2
        if n.is_power_of_two() {
            return rng.next_u64() & (n - 1);
        }
        
        // Lemire's nearly divisionless method
        let mut x = rng.next_u64();
        let mut m = (x as u128) * (n as u128);
        let mut l = m as u64;
        
        if l < n {
            let threshold = n.wrapping_neg() % n;
            while l < threshold {
                x = rng.next_u64();
                m = (x as u128) * (n as u128);
                l = m as u64;
            }
        }
        
        (m >> 64) as u64
    })
}

#[vostd_fn("math/rand", "Intn")]
fn intn(n: i64) -> i64 {
    if n <= 0 {
        panic!("rand.Intn: invalid argument {}", n);
    }
    bounded_int(n as u64) as i64
}

#[vostd_fn("math/rand", "Int63n")]
fn int63n(n: i64) -> i64 {
    if n <= 0 {
        panic!("rand.Int63n: invalid argument {}", n);
    }
    bounded_int(n as u64) as i64
}

#[vostd_fn("math/rand", "Int")]
fn rand_int() -> i64 {
    with_rng(|rng| (rng.next_u64() >> 1) as i64)
}

#[vostd_fn("math/rand", "Uint64")]
fn uint64() -> u64 {
    with_rng(|rng| rng.next_u64())
}

#[vostd_fn("math/rand", "Uint32")]
fn uint32() -> u64 {
    with_rng(|rng| rng.next_u64() as u32 as u64)
}

#[vostd_fn("math/rand", "Float64")]
fn float64() -> f64 {
    // Use upper 53 bits for double precision
    with_rng(|rng| {
        let bits = rng.next_u64() >> 11;
        (bits as f64) * (1.0 / (1u64 << 53) as f64)
    })
}

#[vostd_fn("math/rand", "Float32")]
fn float32() -> f64 {
    // Use upper 24 bits, return as f64 for ABI compatibility
    with_rng(|rng| {
        let bits = rng.next_u64() >> 40;  // 24 bits
        (bits as f64) * (1.0 / (1u64 << 24) as f64)
    })
}

#[cfg(feature = "std")]
mod read_impl {
    use vo_ffi_macro::vostd_fn;
    use vo_runtime::ffi::{ExternCallContext, ExternResult};
    use vo_runtime::builtins::error_helper::write_nil_error;
    use vo_runtime::objects::slice;
    use super::with_rng;

    #[vostd_fn("math/rand", "Read", std)]
    pub fn read(call: &mut ExternCallContext) -> ExternResult {
        let buf_ref = call.arg_ref(slots::ARG_P);
        let len = slice::len(buf_ref);
        let buf_ptr = slice::data_ptr(buf_ref);
        if buf_ptr.is_null() && len > 0 {
            call.ret_i64(slots::RET_0, 0);
            write_nil_error(call, slots::RET_1);
            return ExternResult::Ok;
        }
        let buf = if len == 0 { &mut [] as &mut [u8] } else { unsafe { std::slice::from_raw_parts_mut(buf_ptr, len) } };
        
        with_rng(|rng| {
            let mut i = 0;
            while i + 8 <= len {
                let val = rng.next_u64();
                buf[i..i+8].copy_from_slice(&val.to_le_bytes());
                i += 8;
            }
            if i < len {
                let val = rng.next_u64();
                let bytes = val.to_le_bytes();
                for j in 0..(len - i) {
                    buf[i + j] = bytes[j];
                }
            }
        });
        
        call.ret_i64(slots::RET_0, len as i64);
        write_nil_error(call, slots::RET_1);
        ExternResult::Ok
    }
}
#[cfg(feature = "std")]
pub use read_impl::*;

#[cfg(feature = "std")]
vo_runtime::stdlib_register!(math_rand:
    Intn, Int63n, Int, Uint64, Uint32, Float64, Float32, Read,
);

#[cfg(not(feature = "std"))]
vo_runtime::stdlib_register!(math_rand:
    Intn, Int63n, Int, Uint64, Uint32, Float64, Float32,
);
