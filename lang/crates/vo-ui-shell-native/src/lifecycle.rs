use std::collections::{BTreeMap, BTreeSet, VecDeque};

const MAX_WINDOWS: usize = 256;
const MAX_EVENTS: usize = 16_384;

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct WindowKey(u64);

impl WindowKey {
    pub fn new(index: u32, generation: u32) -> Result<Self, DesktopLifecycleError> {
        if generation == 0 {
            return Err(DesktopLifecycleError::InvalidWindow);
        }
        Ok(Self((u64::from(generation) << 32) | u64::from(index)))
    }

    pub const fn index(self) -> u32 {
        self.0 as u32
    }

    pub const fn generation(self) -> u32 {
        (self.0 >> 32) as u32
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct WindowGeometry {
    pub x: f64,
    pub y: f64,
    pub width: f64,
    pub height: f64,
    pub maximized: bool,
    pub fullscreen: bool,
}

impl WindowGeometry {
    fn valid(self) -> bool {
        [self.x, self.y, self.width, self.height]
            .into_iter()
            .all(f64::is_finite)
            && self.width > 0.0
            && self.height > 0.0
            && self.width <= 100_000.0
            && self.height <= 100_000.0
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MonitorWorkArea {
    pub id: String,
    pub geometry: WindowGeometry,
    pub scale: f64,
    pub primary: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WindowSpec {
    pub key: WindowKey,
    pub owner: Option<WindowKey>,
    pub title: String,
    pub geometry: WindowGeometry,
    pub monitor: Option<String>,
    pub visible: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LifecycleKind {
    Created,
    Activated,
    Deactivated,
    Suspended,
    Resumed,
    CloseRequested,
    Closed,
    ThemeChanged,
    LocaleChanged,
    ScaleChanged,
    MonitorChanged,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LifecycleEvent {
    pub window: WindowKey,
    pub kind: LifecycleKind,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DesktopLifecycleError {
    InvalidWindow,
    DuplicateWindow,
    MissingOwner,
    WindowLimitExceeded,
    EventLimitExceeded,
}

#[derive(Default)]
pub struct DesktopLifecycle {
    windows: BTreeMap<WindowKey, WindowSpec>,
    order: Vec<WindowKey>,
    events: VecDeque<LifecycleEvent>,
}

impl DesktopLifecycle {
    pub fn open(&mut self, spec: WindowSpec) -> Result<(), DesktopLifecycleError> {
        if spec.title.is_empty()
            || !spec.geometry.valid()
            || spec.owner == Some(spec.key)
            || spec
                .owner
                .is_some_and(|owner| !self.windows.contains_key(&owner))
        {
            return Err(
                if spec
                    .owner
                    .is_some_and(|owner| !self.windows.contains_key(&owner))
                {
                    DesktopLifecycleError::MissingOwner
                } else {
                    DesktopLifecycleError::InvalidWindow
                },
            );
        }
        if self.windows.contains_key(&spec.key) {
            return Err(DesktopLifecycleError::DuplicateWindow);
        }
        if self.windows.len() >= MAX_WINDOWS {
            return Err(DesktopLifecycleError::WindowLimitExceeded);
        }
        self.push_event(spec.key, LifecycleKind::Created)?;
        self.order.push(spec.key);
        self.windows.insert(spec.key, spec);
        Ok(())
    }

    pub fn window(&self, key: WindowKey) -> Option<&WindowSpec> {
        self.windows.get(&key)
    }

    pub fn publish(
        &mut self,
        window: WindowKey,
        kind: LifecycleKind,
    ) -> Result<(), DesktopLifecycleError> {
        if !self.windows.contains_key(&window)
            || matches!(kind, LifecycleKind::Created | LifecycleKind::Closed)
        {
            return Err(DesktopLifecycleError::InvalidWindow);
        }
        self.push_event(window, kind)
    }

    pub fn close(&mut self, root: WindowKey) -> Result<Vec<WindowKey>, DesktopLifecycleError> {
        if !self.windows.contains_key(&root) {
            return Err(DesktopLifecycleError::InvalidWindow);
        }
        let mut closing = BTreeSet::from([root]);
        loop {
            let before = closing.len();
            for key in &self.order {
                if self
                    .windows
                    .get(key)
                    .and_then(|window| window.owner)
                    .is_some_and(|owner| closing.contains(&owner))
                {
                    closing.insert(*key);
                }
            }
            if closing.len() == before {
                break;
            }
        }
        if self.events.len().saturating_add(closing.len()) > MAX_EVENTS {
            return Err(DesktopLifecycleError::EventLimitExceeded);
        }
        let closed = self
            .order
            .iter()
            .rev()
            .copied()
            .filter(|key| closing.contains(key))
            .collect::<Vec<_>>();
        for key in &closed {
            self.events.push_back(LifecycleEvent {
                window: *key,
                kind: LifecycleKind::Closed,
            });
            self.windows.remove(key);
        }
        self.order.retain(|key| !closing.contains(key));
        Ok(closed)
    }

    pub fn restore(
        &mut self,
        key: WindowKey,
        monitors: &[MonitorWorkArea],
    ) -> Result<WindowGeometry, DesktopLifecycleError> {
        let window = self
            .windows
            .get_mut(&key)
            .ok_or(DesktopLifecycleError::InvalidWindow)?;
        let selected = window
            .monitor
            .as_deref()
            .and_then(|id| monitors.iter().find(|monitor| monitor.id == id))
            .or_else(|| monitors.iter().find(|monitor| monitor.primary))
            .or_else(|| monitors.first())
            .filter(|monitor| {
                monitor.geometry.valid() && monitor.scale.is_finite() && monitor.scale > 0.0
            })
            .ok_or(DesktopLifecycleError::InvalidWindow)?;
        let work = selected.geometry;
        let mut geometry = window.geometry;
        geometry.width = geometry.width.min(work.width);
        geometry.height = geometry.height.min(work.height);
        geometry.x = geometry
            .x
            .clamp(work.x, work.x + work.width - geometry.width);
        geometry.y = geometry
            .y
            .clamp(work.y, work.y + work.height - geometry.height);
        window.geometry = geometry;
        window.monitor = Some(selected.id.clone());
        Ok(geometry)
    }

    pub fn next_event(&mut self) -> Option<LifecycleEvent> {
        self.events.pop_front()
    }

    fn push_event(
        &mut self,
        window: WindowKey,
        kind: LifecycleKind,
    ) -> Result<(), DesktopLifecycleError> {
        if self.events.len() >= MAX_EVENTS {
            return Err(DesktopLifecycleError::EventLimitExceeded);
        }
        self.events.push_back(LifecycleEvent { window, kind });
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn geometry(x: f64, y: f64, width: f64, height: f64) -> WindowGeometry {
        WindowGeometry {
            x,
            y,
            width,
            height,
            maximized: false,
            fullscreen: false,
        }
    }

    #[test]
    fn ownership_close_and_monitor_restoration_are_deterministic() {
        let main = WindowKey::new(1, 1).unwrap();
        let child = WindowKey::new(2, 1).unwrap();
        let grandchild = WindowKey::new(3, 1).unwrap();
        let mut lifecycle = DesktopLifecycle::default();
        for (key, owner) in [(main, None), (child, Some(main)), (grandchild, Some(child))] {
            lifecycle
                .open(WindowSpec {
                    key,
                    owner,
                    title: format!("Window {}", key.index()),
                    geometry: geometry(-100.0, -100.0, 1_200.0, 900.0),
                    monitor: Some("gone".to_string()),
                    visible: true,
                })
                .unwrap();
        }
        let restored = lifecycle
            .restore(
                main,
                &[MonitorWorkArea {
                    id: "primary".to_string(),
                    geometry: geometry(0.0, 0.0, 800.0, 600.0),
                    scale: 2.0,
                    primary: true,
                }],
            )
            .unwrap();
        assert_eq!(restored, geometry(0.0, 0.0, 800.0, 600.0));
        assert_eq!(
            lifecycle.close(main).unwrap(),
            vec![grandchild, child, main]
        );
        assert!(lifecycle.window(main).is_none());
    }
}
