//! Physical slot widths shared by semantic analysis and native FFI macros.
//!
//! Frontends describe only the by-value type graph. This traversal owns width
//! arithmetic, empty aggregates, cycle detection, and optional saturation.

use std::collections::HashMap;
use std::hash::Hash;

pub enum SlotLayout<K> {
    Scalar,
    Interface,
    Struct(Vec<K>),
    Tuple(Vec<K>),
    Array { element: K, len: u64 },
    Alias(K),
}

#[derive(Debug, PartialEq, Eq)]
pub enum SlotLayoutError<K, E> {
    Invalid(E),
    Cycle(Vec<K>),
    Overflow(K),
}

enum Task<K> {
    Enter(K),
    Exit(K, SlotLayout<K>),
}

/// Compute widths in O(V + E), without recursion or allocating physical slots.
/// A saturation limit retains an over-wide sentinel for metadata-only types;
/// zero-width arrays remain zero even when a child has saturated.
pub fn slot_counts<K: Copy + Eq + Hash, E>(
    roots: impl IntoIterator<Item = K>,
    mut describe: impl FnMut(K) -> Result<SlotLayout<K>, E>,
    saturation: Option<usize>,
) -> Result<HashMap<K, usize>, SlotLayoutError<K, E>> {
    let mut counts = HashMap::new();
    let mut visiting = HashMap::new();
    let mut path = Vec::new();
    let mut tasks: Vec<_> = roots.into_iter().map(Task::Enter).collect();
    while let Some(task) = tasks.pop() {
        match task {
            Task::Enter(key) => {
                if counts.contains_key(&key) {
                    continue;
                }
                if let Some(&start) = visiting.get(&key) {
                    let mut cycle = path[start..].to_vec();
                    cycle.push(key);
                    return Err(SlotLayoutError::Cycle(cycle));
                }
                let node = describe(key).map_err(SlotLayoutError::Invalid)?;
                visiting.insert(key, path.len());
                path.push(key);
                let children = match &node {
                    SlotLayout::Scalar | SlotLayout::Interface => Vec::new(),
                    SlotLayout::Struct(fields) | SlotLayout::Tuple(fields) => fields.clone(),
                    SlotLayout::Array { element, .. } | SlotLayout::Alias(element) => {
                        vec![*element]
                    }
                };
                tasks.push(Task::Exit(key, node));
                tasks.extend(children.into_iter().rev().map(Task::Enter));
            }
            Task::Exit(key, node) => {
                let add = |a: usize, b: usize| match saturation {
                    Some(limit) => Ok(a.saturating_add(b).min(limit)),
                    None => a.checked_add(b).ok_or(SlotLayoutError::Overflow(key)),
                };
                let count = match node {
                    SlotLayout::Scalar => 1,
                    SlotLayout::Interface => 2,
                    SlotLayout::Alias(child) => counts[&child],
                    SlotLayout::Struct(fields) => fields
                        .iter()
                        .try_fold(0, |total, field| add(total, counts[field]))?
                        .max(1),
                    SlotLayout::Tuple(fields) => fields
                        .iter()
                        .try_fold(0, |total, field| add(total, counts[field]))?,
                    SlotLayout::Array { element, len } => {
                        let width = counts[&element];
                        if width == 0 || len == 0 {
                            0
                        } else if let Some(limit) = saturation {
                            ((width as u128) * u128::from(len)).min(limit as u128) as usize
                        } else {
                            usize::try_from(len)
                                .ok()
                                .and_then(|len| width.checked_mul(len))
                                .ok_or(SlotLayoutError::Overflow(key))?
                        }
                    }
                };
                counts.insert(key, count);
                visiting.remove(&key);
                path.pop();
            }
        }
    }
    Ok(counts)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn physical_widths_preserve_empty_structs_and_zero_width_arrays() {
        for saturation in [None, Some(u16::MAX as usize + 1)] {
            let widths = slot_counts(
                0..6,
                |key| {
                    Ok::<_, ()>(match key {
                        0 => SlotLayout::Scalar,
                        1 => SlotLayout::Struct(vec![]),
                        2 => SlotLayout::Array { element: 0, len: 0 },
                        3 => SlotLayout::Array {
                            element: 2,
                            len: u64::MAX,
                        },
                        4 => SlotLayout::Struct(vec![1, 0]),
                        5 => SlotLayout::Tuple(vec![1, 4, 3]),
                        _ => unreachable!(),
                    })
                },
                saturation,
            )
            .unwrap();
            assert_eq!(
                (widths[&1], widths[&3], widths[&4], widths[&5]),
                (1, 0, 2, 3)
            );
        }
    }

    #[test]
    fn deep_graphs_share_subtrees_and_report_cycles() {
        let widths = slot_counts(
            [10_000],
            |key| {
                Ok::<_, ()>(if key == 0 {
                    SlotLayout::Scalar
                } else {
                    SlotLayout::Alias(key - 1)
                })
            },
            None,
        )
        .unwrap();
        assert_eq!(widths[&10_000], 1);
        assert_eq!(
            slot_counts([0], |key| Ok::<_, ()>(SlotLayout::Alias(1 - key)), None),
            Err(SlotLayoutError::Cycle(vec![0, 1, 0]))
        );
    }

    #[test]
    fn overflow_saturation_and_zero_length_are_distinct() {
        let describe = |key| {
            Ok::<_, ()>(match key {
                0 => SlotLayout::Interface,
                1 => SlotLayout::Array {
                    element: 0,
                    len: u64::MAX,
                },
                2 => SlotLayout::Array { element: 1, len: 0 },
                _ => unreachable!(),
            })
        };
        assert_eq!(
            slot_counts([1], describe, None),
            Err(SlotLayoutError::Overflow(1))
        );
        let widths = slot_counts([2], describe, Some(65_536)).unwrap();
        assert_eq!((widths[&1], widths[&2]), (65_536, 0));
    }
}
