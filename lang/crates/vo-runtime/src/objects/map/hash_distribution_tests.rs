//! Regression for correlated numeric keys collapsing into a few probe homes.
use super::*;

#[test]
fn floating_keys_disperse_and_cross_checked_and_scalar_access_after_resize() {
    const ENTRIES: usize = 768;
    for kind in [ValueKind::Float32, ValueKind::Float64] {
        let mut gc = Gc::new();
        let m = create(
            &mut gc,
            ValueMeta::new(0, kind),
            ValueMeta::new(0, ValueKind::Int64),
            1,
            1,
            0,
        );
        let bits = |index: usize| match kind {
            ValueKind::Float32 => u64::from((index as f32 + 0.25).to_bits()),
            _ => (index as f64 + 0.25).to_bits(),
        };
        for index in 0..ENTRIES {
            if index % 2 == 0 {
                unsafe { set_checked(&mut gc, m, &[bits(index)], &[index as u64 + 1], None) }
                    .unwrap();
            } else {
                assert!(matches!(
                    unsafe {
                        set_trusted_scalar_deferred(
                            &mut gc,
                            m,
                            bits(index),
                            index as u64 + 1,
                            None,
                            true,
                        )
                    }
                    .unwrap(),
                    MapSetOutcome::Set
                ));
            }
        }
        let backing = unsafe { backing_ref(m) };
        let capacity = unsafe { backing_capacity(backing) };
        assert_eq!(capacity, 1024, "exercise the three-quarter-full table");
        let mut probes = 0usize;
        for index in 0..capacity {
            if unsafe { bucket_state(m, backing, index) } != BucketState::Occupied {
                continue;
            }
            let home = unsafe { bucket_hash(m, backing, index) } as usize & (capacity - 1);
            probes += index.wrapping_sub(home) & (capacity - 1);
            probes += 1;
        }
        // A loose work bound catches the old hundreds-of-probes pathology
        // without prescribing a particular hash or exact bucket permutation.
        assert!(
            probes <= ENTRIES * 8,
            "{kind:?}: {probes} successful probes for {ENTRIES} keys"
        );
        for index in 0..ENTRIES {
            let key = bits(index);
            let pointer = unsafe { get_trusted_scalar_ptr(m, key) }.unwrap();
            assert!(!pointer.is_null());
            assert_eq!(unsafe { *pointer }, index as u64 + 1);
            assert_eq!(
                unsafe { get_checked(m, &[key], None) }.unwrap().as_deref(),
                Some(&[index as u64 + 1][..])
            );
            if index % 3 == 0 {
                unsafe { delete_trusted_scalar(m, key) }.unwrap();
            }
        }
        for index in 0..ENTRIES {
            let mut value = [u64::MAX];
            let found = unsafe { get_checked_into(m, &[bits(index)], None, &mut value) }.unwrap();
            assert_eq!(found, index % 3 != 0);
            assert_eq!(value[0], if found { index as u64 + 1 } else { 0 });
        }
    }
}
