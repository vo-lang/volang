#![no_std]

extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet};
use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ReloadLimits {
    pub max_identity_bytes: usize,
    pub max_state_fields: usize,
    pub max_state_key_bytes: usize,
}

impl Default for ReloadLimits {
    fn default() -> Self {
        Self {
            max_identity_bytes: 4 * 1024,
            max_state_fields: 65_536,
            max_state_key_bytes: 4 * 1024,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StateField {
    pub key: String,
    /// Stable compiler-provided fingerprint of the logical Volang type.
    pub type_fingerprint: u64,
}

impl StateField {
    pub fn new(key: impl Into<String>, type_fingerprint: u64) -> Self {
        Self {
            key: key.into(),
            type_fingerprint,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ComponentSchema {
    pub identity: String,
    pub state: Vec<StateField>,
}

impl ComponentSchema {
    pub fn new(identity: impl Into<String>, state: Vec<StateField>) -> Self {
        Self {
            identity: identity.into(),
            state,
        }
    }

    pub fn validate(&self, limits: ReloadLimits) -> Result<(), ReloadError> {
        if self.identity.is_empty() || self.identity.len() > limits.max_identity_bytes {
            return Err(ReloadError::InvalidIdentity);
        }
        if self.state.len() > limits.max_state_fields {
            return Err(ReloadError::StateLimitExceeded);
        }
        let mut keys = BTreeSet::new();
        for field in &self.state {
            if field.key.is_empty() || field.key.len() > limits.max_state_key_bytes {
                return Err(ReloadError::InvalidStateKey(field.key.clone()));
            }
            if !keys.insert(field.key.as_str()) {
                return Err(ReloadError::DuplicateStateKey(field.key.clone()));
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StateAction {
    /// Move the old cell into the new schema without rerunning initialization.
    Preserve { previous_index: u32 },
    /// Evaluate the new declaration's initializer.
    Initialize,
    /// The key survived while its type changed; discard the old value and
    /// evaluate the new initializer.
    Reinitialize { previous_index: u32 },
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ReloadPlan {
    pub actions: Vec<StateAction>,
    pub dropped_previous: Vec<u32>,
}

impl ReloadPlan {
    pub fn preserved_count(&self) -> usize {
        self.actions
            .iter()
            .filter(|action| matches!(action, StateAction::Preserve { .. }))
            .count()
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ReloadError {
    InvalidIdentity,
    IdentityMismatch { previous: String, next: String },
    StateLimitExceeded,
    InvalidStateKey(String),
    DuplicateStateKey(String),
    StateIdentityExhausted,
}

impl fmt::Display for ReloadError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "invalid UI reload schema: {self:?}")
    }
}

/// Matches cells by stable source key and preserves values only when their
/// logical type fingerprint is unchanged. Field order may change freely.
pub fn plan_reload(
    previous: &ComponentSchema,
    next: &ComponentSchema,
    limits: ReloadLimits,
) -> Result<ReloadPlan, ReloadError> {
    previous.validate(limits)?;
    next.validate(limits)?;
    if previous.identity != next.identity {
        return Err(ReloadError::IdentityMismatch {
            previous: previous.identity.clone(),
            next: next.identity.clone(),
        });
    }

    let mut previous_by_key = BTreeMap::new();
    for (index, field) in previous.state.iter().enumerate() {
        let index = u32::try_from(index).map_err(|_| ReloadError::StateIdentityExhausted)?;
        previous_by_key.insert(field.key.as_str(), (index, field.type_fingerprint));
    }
    let mut retained = BTreeSet::new();
    let mut actions = Vec::new();
    actions
        .try_reserve(next.state.len())
        .map_err(|_| ReloadError::StateLimitExceeded)?;
    for field in &next.state {
        let action = match previous_by_key.get(field.key.as_str()).copied() {
            Some((previous_index, fingerprint)) if fingerprint == field.type_fingerprint => {
                retained.insert(previous_index);
                StateAction::Preserve { previous_index }
            }
            Some((previous_index, _)) => {
                retained.insert(previous_index);
                StateAction::Reinitialize { previous_index }
            }
            None => StateAction::Initialize,
        };
        actions.push(action);
    }
    let mut dropped_previous = Vec::new();
    for index in 0..previous.state.len() {
        let index = u32::try_from(index).map_err(|_| ReloadError::StateIdentityExhausted)?;
        if !retained.contains(&index) {
            dropped_previous.push(index);
        }
    }
    Ok(ReloadPlan {
        actions,
        dropped_previous,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn schema(fields: &[(&str, u64)]) -> ComponentSchema {
        ComponentSchema::new(
            "github.com/acme/app::App",
            fields
                .iter()
                .map(|(key, fingerprint)| StateField::new(*key, *fingerprint))
                .collect(),
        )
    }

    #[test]
    fn reorder_preserves_matching_state_by_key_and_type() {
        let previous = schema(&[("count", 1), ("query", 2)]);
        let next = schema(&[("query", 2), ("count", 1)]);
        let plan = plan_reload(&previous, &next, ReloadLimits::default()).unwrap();
        assert_eq!(
            plan.actions,
            alloc::vec![
                StateAction::Preserve { previous_index: 1 },
                StateAction::Preserve { previous_index: 0 },
            ]
        );
        assert_eq!(plan.preserved_count(), 2);
        assert!(plan.dropped_previous.is_empty());
    }

    #[test]
    fn additions_removals_and_type_changes_are_explicit() {
        let previous = schema(&[("count", 1), ("removed", 9)]);
        let next = schema(&[("count", 2), ("added", 3)]);
        let plan = plan_reload(&previous, &next, ReloadLimits::default()).unwrap();
        assert_eq!(
            plan.actions,
            alloc::vec![
                StateAction::Reinitialize { previous_index: 0 },
                StateAction::Initialize,
            ]
        );
        assert_eq!(plan.dropped_previous, alloc::vec![1]);
    }

    #[test]
    fn duplicate_keys_and_component_identity_changes_are_rejected() {
        let duplicate = schema(&[("count", 1), ("count", 1)]);
        assert_eq!(
            duplicate.validate(ReloadLimits::default()),
            Err(ReloadError::DuplicateStateKey("count".into()))
        );

        let previous = schema(&[]);
        let next = ComponentSchema::new("github.com/acme/app::Other", Vec::new());
        assert!(matches!(
            plan_reload(&previous, &next, ReloadLimits::default()),
            Err(ReloadError::IdentityMismatch { .. })
        ));
    }
}
