//! math/rand - Pseudo-random number generation
//!
//! Uses xoroshiro128++. Portable providers use one deterministic,
//! lock-protected stream; native providers seed each thread automatically.

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use core::cell::RefCell;
use core::cell::UnsafeCell;
use core::hint::spin_loop;
#[cfg(feature = "std")]
use core::sync::atomic::AtomicU64;
use core::sync::atomic::{AtomicBool, Ordering};

/// xoroshiro128++ state
struct Rng {
    s0: u64,
    s1: u64,
    read_val: u64,
    read_pos: u8,
}

impl Rng {
    const fn new(seed: u64) -> Self {
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
            Self {
                s0: 1,
                s1: 1,
                read_val: 0,
                read_pos: 0,
            }
        } else {
            Self {
                s0,
                s1,
                read_val: 0,
                read_pos: 0,
            }
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

    /// Fill bytes from 63-bit draws, retaining unused bytes across calls.
    /// This makes a sequence of short reads identical to one combined read.
    fn fill_bytes(&mut self, dst: &mut [u8]) {
        for byte in dst {
            if self.read_pos == 0 {
                self.read_val = self.next_u64() >> 1;
                self.read_pos = 7;
            }
            *byte = self.read_val as u8;
            self.read_val >>= 8;
            self.read_pos -= 1;
        }
    }
}

// Native hosts keep independent, automatically seeded streams per thread.
#[cfg(feature = "std")]
thread_local! {
    static NATIVE_RNG: RefCell<Rng> = RefCell::new(Rng::new(auto_seed()));
}

// Counter for uniqueness
#[cfg(feature = "std")]
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
    SEED_COUNTER
        .fetch_add(1, Ordering::Relaxed)
        .hash(&mut hasher);
    hasher.finish()
}

struct LockedRng {
    locked: AtomicBool,
    state: UnsafeCell<Rng>,
}

impl LockedRng {
    const fn new(seed: u64) -> Self {
        Self {
            locked: AtomicBool::new(false),
            state: UnsafeCell::new(Rng::new(seed)),
        }
    }

    fn with<T>(&self, f: impl FnOnce(&mut Rng) -> T) -> T {
        while self
            .locked
            .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            spin_loop();
        }

        struct Unlock<'a>(&'a AtomicBool);
        impl Drop for Unlock<'_> {
            fn drop(&mut self) {
                self.0.store(false, Ordering::Release);
            }
        }

        let _unlock = Unlock(&self.locked);
        // Safety: the acquire/release lock above provides exclusive mutable
        // access for the entire callback, including unwind on supported hosts.
        f(unsafe { &mut *self.state.get() })
    }
}

// Safety: every access to `state` is serialized by `locked`.
unsafe impl Sync for LockedRng {}

const PORTABLE_SEED: u64 = 0x6a09_e667_f3bc_c909;
static PORTABLE_RNG: LockedRng = LockedRng::new(PORTABLE_SEED);

#[cfg(feature = "std")]
#[inline]
fn with_native_rng<T>(f: impl FnOnce(&mut Rng) -> T) -> T {
    NATIVE_RNG.with(|rng| f(&mut rng.borrow_mut()))
}

#[inline]
fn with_portable_rng<T>(f: impl FnOnce(&mut Rng) -> T) -> T {
    PORTABLE_RNG.with(f)
}

#[cfg(feature = "std")]
use self::with_native_rng as with_target_rng;
#[cfg(not(feature = "std"))]
use self::with_portable_rng as with_target_rng;

/// Generate a random u64. For internal use by other stdlib modules.
pub fn next_u64() -> u64 {
    with_target_rng(|rng| rng.next_u64())
}

// Generate bounded random integer in [0, n) using Lemire's method
#[inline]
fn bounded_int(rng: &mut Rng, n: u64) -> u64 {
    if n.is_power_of_two() {
        return rng.next_u64() & (n - 1);
    }

    let mut x = rng.next_u64();
    let mut m = (x as u128) * (n as u128);
    let mut low = m as u64;

    if low < n {
        let threshold = n.wrapping_neg() % n;
        while low < threshold {
            x = rng.next_u64();
            m = (x as u128) * (n as u128);
            low = m as u64;
        }
    }

    (m >> 64) as u64
}

macro_rules! define_rand_provider {
    ($with_rng:ident) => {
        #[vo_ffi_macro::vostd_fn("math/rand", "Intn")]
        fn intn(n: i64) -> i64 {
            if n <= 0 {
                panic!("rand.Intn: invalid argument {}", n);
            }
            $with_rng(|rng| bounded_int(rng, n as u64) as i64)
        }

        #[vo_ffi_macro::vostd_fn("math/rand", "Int63n")]
        fn int63n(n: i64) -> i64 {
            if n <= 0 {
                panic!("rand.Int63n: invalid argument {}", n);
            }
            $with_rng(|rng| bounded_int(rng, n as u64) as i64)
        }

        #[vo_ffi_macro::vostd_fn("math/rand", "Int")]
        fn rand_int() -> i64 {
            $with_rng(|rng| (rng.next_u64() >> 1) as i64)
        }

        #[vo_ffi_macro::vostd_fn("math/rand", "Uint64")]
        fn uint64() -> u64 {
            $with_rng(|rng| rng.next_u64())
        }

        #[vo_ffi_macro::vostd_fn("math/rand", "Uint32")]
        fn uint32() -> u32 {
            $with_rng(|rng| rng.next_u64() as u32)
        }

        #[vo_ffi_macro::vostd_fn("math/rand", "Float64")]
        fn float64() -> f64 {
            $with_rng(|rng| {
                let bits = rng.next_u64() >> 11;
                (bits as f64) * (1.0 / (1u64 << 53) as f64)
            })
        }

        #[vo_ffi_macro::vostd_fn("math/rand", "Float32")]
        fn float32() -> f32 {
            $with_rng(|rng| {
                let bits = rng.next_u64() >> 40;
                (bits as f32) * (1.0 / (1u32 << 24) as f32)
            })
        }

        mod read_impl {
            use super::*;
            use vo_runtime::builtins::error_helper::write_nil_error;
            use vo_runtime::ffi::{ExternCallContext, ExternResult};
            use vo_runtime::objects::slice;

            #[vo_ffi_macro::vostd_fn("math/rand", "Read")]
            pub fn read(call: &mut ExternCallContext) -> ExternResult {
                let buf_ref = call.arg_ref(slots::ARG_P);
                // Safety: `buf_ref` is the rooted []byte argument for this extern call.
                let len = unsafe { slice::len(buf_ref) };
                let mut buf = Vec::new();
                buf.resize(len, 0);

                $with_rng(|rng| rng.fill_bytes(&mut buf));
                unsafe { slice::write_bytes(buf_ref, &buf) };

                call.ret_i64(slots::RET_0, len as i64);
                write_nil_error(call, slots::RET_1);
                ExternResult::Ok
            }
        }
        pub use read_impl::*;

        vo_ffi_macro::vostd_register!("math/rand":
            Intn, Int63n, Int, Uint64, Uint32, Float64, Float32, Read,
        );
    };
}

define_rand_provider!(with_portable_rng);

#[cfg(feature = "std")]
pub(crate) mod native {
    use super::{bounded_int, with_native_rng};

    define_rand_provider!(with_native_rng);
}

#[cfg(test)]
mod tests {
    use super::{LockedRng, Rng, PORTABLE_SEED};

    #[test]
    fn split_reads_preserve_the_byte_stream() {
        let mut split = Rng::new(42);
        let mut combined = Rng::new(42);
        let mut first = [0; 3];
        let mut second = [0; 19];
        let mut all = [0; 22];

        split.fill_bytes(&mut first);
        split.fill_bytes(&mut second);
        combined.fill_bytes(&mut all);

        assert_eq!(&all[..3], first.as_slice());
        assert_eq!(&all[3..], second.as_slice());
    }

    #[test]
    fn locked_rng_preserves_the_underlying_stream() {
        let locked = LockedRng::new(42);
        let mut direct = Rng::new(42);

        for _ in 0..128 {
            assert_eq!(locked.with(Rng::next_u64), direct.next_u64());
        }
    }

    #[test]
    fn root_provider_uses_the_portable_stream() {
        let mut expected = Rng::new(PORTABLE_SEED);
        assert_eq!(super::uint64(), expected.next_u64());
    }

    #[test]
    fn generated_floats_stay_half_open() {
        let mut rng = Rng::new(7);
        for _ in 0..10_000 {
            let f64_value = (rng.next_u64() >> 11) as f64 * (1.0 / (1u64 << 53) as f64);
            let f32_value = (rng.next_u64() >> 40) as f32 * (1.0 / (1u32 << 24) as f32);
            assert!((0.0..1.0).contains(&f64_value));
            assert!((0.0..1.0).contains(&f32_value));
        }
    }

    #[cfg(feature = "std")]
    #[test]
    fn portable_and_native_provider_tables_have_the_same_contracts() {
        let portable = super::__VO_STDLIB_ENTRIES;
        let native = super::native::__VO_STDLIB_ENTRIES;
        assert_eq!(portable.len(), native.len());

        for (portable, native) in portable.iter().zip(native) {
            assert_eq!(portable.name, native.name);
            assert_eq!(portable.effects, native.effects);
        }
    }
}
