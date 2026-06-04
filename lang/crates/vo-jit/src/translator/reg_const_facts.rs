use std::collections::HashMap;

mod cfg;
mod effect;
mod fold;
mod merge;

#[cfg(test)]
mod tests;

pub type RegConstFacts = Vec<HashMap<u16, i64>>;

pub use cfg::compute_reg_const_facts_with_context;
