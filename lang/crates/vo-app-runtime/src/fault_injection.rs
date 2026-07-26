use alloc::collections::BTreeMap;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub enum RuntimeFaultPoint {
    ProtocolDecode,
    EndpointQueue,
    WorkerDispatch,
    ResourceAcquire,
    SurfaceOperation,
    DeviceOperation,
    AudioOperation,
    Shutdown,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuntimeInjectedFault {
    RejectBeforeDispatch,
    FailOwner,
    DropLatestOnly,
    OutcomeUnknown,
    SurfaceLost,
    DeviceLost,
    AudioDeviceLost,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RuntimeFaultRule {
    pub point: RuntimeFaultPoint,
    pub fault: RuntimeInjectedFault,
    pub skip: u64,
    pub every: u64,
    pub remaining: u64,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RuntimeFaultInjectionError {
    InvalidRule,
    RuleCapacity,
    UnknownRule,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct RuntimeFaultInjectionMetrics {
    pub installed_rules: usize,
    pub evaluated: u64,
    pub injected: u64,
    pub exhausted: u64,
}

#[derive(Clone, Copy)]
struct ActiveRule {
    rule: RuntimeFaultRule,
    visits: u64,
}

pub struct RuntimeFaultInjector {
    max_rules: usize,
    rules: BTreeMap<RuntimeFaultPoint, ActiveRule>,
    metrics: RuntimeFaultInjectionMetrics,
}

impl RuntimeFaultInjector {
    pub fn new(max_rules: usize) -> Self {
        Self {
            max_rules: max_rules.max(1),
            rules: BTreeMap::new(),
            metrics: RuntimeFaultInjectionMetrics::default(),
        }
    }

    pub fn replace(&mut self, rule: RuntimeFaultRule) -> Result<(), RuntimeFaultInjectionError> {
        if rule.every == 0 || rule.remaining == 0 {
            return Err(RuntimeFaultInjectionError::InvalidRule);
        }
        if !self.rules.contains_key(&rule.point) && self.rules.len() == self.max_rules {
            return Err(RuntimeFaultInjectionError::RuleCapacity);
        }
        self.rules
            .insert(rule.point, ActiveRule { rule, visits: 0 });
        self.metrics.installed_rules = self.rules.len();
        Ok(())
    }

    pub fn remove(
        &mut self,
        point: RuntimeFaultPoint,
    ) -> Result<RuntimeFaultRule, RuntimeFaultInjectionError> {
        let rule = self
            .rules
            .remove(&point)
            .map(|active| active.rule)
            .ok_or(RuntimeFaultInjectionError::UnknownRule)?;
        self.metrics.installed_rules = self.rules.len();
        Ok(rule)
    }

    pub fn clear(&mut self) -> usize {
        let removed = self.rules.len();
        self.rules.clear();
        self.metrics.installed_rules = 0;
        removed
    }

    pub fn trigger(&mut self, point: RuntimeFaultPoint) -> Option<RuntimeInjectedFault> {
        self.metrics.evaluated = self.metrics.evaluated.saturating_add(1);
        let active = self.rules.get_mut(&point)?;
        active.visits = active.visits.saturating_add(1);
        if active.visits <= active.rule.skip {
            return None;
        }
        let eligible = active.visits - active.rule.skip - 1;
        if eligible % active.rule.every != 0 {
            return None;
        }
        let fault = active.rule.fault;
        active.rule.remaining -= 1;
        self.metrics.injected = self.metrics.injected.saturating_add(1);
        if active.rule.remaining == 0 {
            self.rules.remove(&point);
            self.metrics.installed_rules = self.rules.len();
            self.metrics.exhausted = self.metrics.exhausted.saturating_add(1);
        }
        Some(fault)
    }

    pub const fn metrics(&self) -> RuntimeFaultInjectionMetrics {
        self.metrics
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn deterministic_schedule_skips_repeats_and_exhausts_exactly() {
        let mut injector = RuntimeFaultInjector::new(1);
        injector
            .replace(RuntimeFaultRule {
                point: RuntimeFaultPoint::DeviceOperation,
                fault: RuntimeInjectedFault::DeviceLost,
                skip: 1,
                every: 2,
                remaining: 2,
            })
            .unwrap();

        assert_eq!(injector.trigger(RuntimeFaultPoint::DeviceOperation), None);
        assert_eq!(
            injector.trigger(RuntimeFaultPoint::DeviceOperation),
            Some(RuntimeInjectedFault::DeviceLost)
        );
        assert_eq!(injector.trigger(RuntimeFaultPoint::DeviceOperation), None);
        assert_eq!(
            injector.trigger(RuntimeFaultPoint::DeviceOperation),
            Some(RuntimeInjectedFault::DeviceLost)
        );
        assert_eq!(injector.trigger(RuntimeFaultPoint::DeviceOperation), None);
        assert_eq!(
            injector.metrics(),
            RuntimeFaultInjectionMetrics {
                installed_rules: 0,
                evaluated: 5,
                injected: 2,
                exhausted: 1,
            }
        );
    }

    #[test]
    fn invalid_capacity_and_unknown_removal_fail_without_mutation() {
        let mut injector = RuntimeFaultInjector::new(1);
        assert_eq!(
            injector.replace(RuntimeFaultRule {
                point: RuntimeFaultPoint::ProtocolDecode,
                fault: RuntimeInjectedFault::RejectBeforeDispatch,
                skip: 0,
                every: 0,
                remaining: 1,
            }),
            Err(RuntimeFaultInjectionError::InvalidRule)
        );
        injector
            .replace(RuntimeFaultRule {
                point: RuntimeFaultPoint::ProtocolDecode,
                fault: RuntimeInjectedFault::RejectBeforeDispatch,
                skip: 0,
                every: 1,
                remaining: 1,
            })
            .unwrap();
        assert_eq!(
            injector.replace(RuntimeFaultRule {
                point: RuntimeFaultPoint::Shutdown,
                fault: RuntimeInjectedFault::FailOwner,
                skip: 0,
                every: 1,
                remaining: 1,
            }),
            Err(RuntimeFaultInjectionError::RuleCapacity)
        );
        assert_eq!(
            injector.remove(RuntimeFaultPoint::Shutdown),
            Err(RuntimeFaultInjectionError::UnknownRule)
        );
        assert_eq!(injector.metrics().installed_rules, 1);
    }
}
