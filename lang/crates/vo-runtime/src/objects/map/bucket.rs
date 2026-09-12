//! Physical bucket control encoding, shared by lookup and iterator forwarding.

pub(super) const PREFIX_SLOTS: usize = 1;
const TAG_SHIFT: u32 = 62;
const PAYLOAD_MASK: u64 = (1_u64 << TAG_SHIFT) - 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u64)]
pub(super) enum BucketState {
    Empty = 0,
    Tombstone = 1,
    Occupied = 2,
    Forwarded = 3,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct BucketControl(u64);

impl BucketControl {
    pub(super) const EMPTY: Self = Self(0);
    pub(super) const TOMBSTONE: Self = Self((BucketState::Tombstone as u64) << TAG_SHIFT);

    #[inline]
    pub(super) const fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    #[inline]
    pub(super) const fn raw(self) -> u64 {
        self.0
    }

    #[inline]
    pub(super) const fn state(self) -> BucketState {
        match self.0 >> TAG_SHIFT {
            0 => BucketState::Empty,
            1 => BucketState::Tombstone,
            2 => BucketState::Occupied,
            _ => BucketState::Forwarded,
        }
    }

    #[inline]
    pub(super) const fn occupied(hash: u64) -> Self {
        Self(((BucketState::Occupied as u64) << TAG_SHIFT) | (hash & PAYLOAD_MASK))
    }

    #[inline]
    pub(super) const fn hash_matches(self, hash: u64) -> bool {
        self.0 == Self::occupied(hash).0
    }

    #[inline]
    pub(super) fn hash(self) -> u64 {
        debug_assert_eq!(self.state(), BucketState::Occupied);
        self.0 & PAYLOAD_MASK
    }

    #[inline]
    pub(super) fn forwarded(index: usize) -> Self {
        // Backing allocation bounds its byte extent by isize::MAX, so every
        // bucket index fits even for a zero-width key and value (one slot).
        debug_assert!((index as u64) <= PAYLOAD_MASK);
        Self(((BucketState::Forwarded as u64) << TAG_SHIFT) | index as u64)
    }

    #[inline]
    pub(super) fn forwarded_index(self) -> Option<usize> {
        if self.state() == BucketState::Forwarded {
            Some((self.0 & PAYLOAD_MASK) as usize)
        } else {
            None
        }
    }
}

// The low hash bits used for initial probing remain available for every
// representable backing. This also proves forwarding cannot truncate an index.
const _: () = assert!((isize::MAX as u64 / crate::slot::SLOT_BYTES as u64) < PAYLOAD_MASK);

pub(super) const ABI_LAYOUT_WORDS: &[u64] = &[
    PREFIX_SLOTS as u64,
    TAG_SHIFT as u64,
    PAYLOAD_MASK,
    BucketState::Empty as u64,
    BucketState::Tombstone as u64,
    BucketState::Occupied as u64,
    BucketState::Forwarded as u64,
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn hashes_preserve_probe_bits_without_changing_state() {
        for hash in [0, 1, PAYLOAD_MASK, 1 << 62, 1 << 63, u64::MAX] {
            let control = BucketControl::occupied(hash);
            assert_eq!(control.state(), BucketState::Occupied);
            assert_eq!(control.hash(), hash & PAYLOAD_MASK);
            assert!(control.hash_matches(hash));
            assert!(!control.hash_matches(hash ^ 1));
            for bits in 3..usize::BITS - 3 {
                let mask = (1_usize << bits) - 1;
                assert_eq!(hash as usize & mask, control.hash() as usize & mask);
            }
        }
    }

    #[test]
    fn forwarding_preserves_every_allocatable_index() {
        for index in [0, 7, 8, isize::MAX as usize / crate::slot::SLOT_BYTES] {
            let control = BucketControl::forwarded(index);
            assert_eq!(control.state(), BucketState::Forwarded);
            assert_eq!(control.forwarded_index(), Some(index));
            assert!(!control.hash_matches(index as u64));
        }
        for control in [
            BucketControl::EMPTY,
            BucketControl::TOMBSTONE,
            BucketControl::occupied(u64::MAX),
        ] {
            assert_eq!(control.forwarded_index(), None);
        }
    }
}
