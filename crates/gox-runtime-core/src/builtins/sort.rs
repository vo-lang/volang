//! Sort operations (pure logic).

/// Sort i64 slice in place.
pub fn sort_ints(slice: &mut [i64]) {
    slice.sort();
}

/// Sort f64 slice in place.
pub fn sort_floats(slice: &mut [f64]) {
    slice.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
}

/// Sort string slice in place.
pub fn sort_strings(slice: &mut [String]) {
    slice.sort();
}

/// Check if i64 slice is sorted.
pub fn is_sorted_ints(slice: &[i64]) -> bool {
    slice.windows(2).all(|w| w[0] <= w[1])
}

/// Check if f64 slice is sorted.
pub fn is_sorted_floats(slice: &[f64]) -> bool {
    slice.windows(2).all(|w| w[0] <= w[1])
}

/// Check if string slice is sorted.
pub fn is_sorted_strings(slice: &[String]) -> bool {
    slice.windows(2).all(|w| w[0] <= w[1])
}

/// Binary search in sorted i64 slice.
pub fn search_ints(slice: &[i64], x: i64) -> usize {
    slice.binary_search(&x).unwrap_or_else(|i| i)
}

/// Binary search in sorted f64 slice.
pub fn search_floats(slice: &[f64], x: f64) -> usize {
    slice.binary_search_by(|a| a.partial_cmp(&x).unwrap_or(std::cmp::Ordering::Equal))
        .unwrap_or_else(|i| i)
}

/// Binary search in sorted string slice.
pub fn search_strings(slice: &[String], x: &str) -> usize {
    slice.binary_search_by(|a| a.as_str().cmp(x)).unwrap_or_else(|i| i)
}

/// Reverse slice in place.
pub fn reverse<T>(slice: &mut [T]) {
    slice.reverse();
}
