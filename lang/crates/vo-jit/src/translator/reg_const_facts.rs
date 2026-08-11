mod cfg;
mod effect;
mod fold;
mod merge;

#[cfg(test)]
mod tests;

pub type RegConstFact = (u16, i64);
pub type RegConstFacts = Vec<Box<[RegConstFact]>>;

pub use cfg::try_compute_reg_const_facts_with_context;

#[cfg(test)]
pub use cfg::compute_reg_const_facts_with_context;
