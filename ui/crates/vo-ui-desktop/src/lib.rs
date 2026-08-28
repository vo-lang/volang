#![no_std]

extern crate alloc;

use core::fmt;
use vo_ui_core::{EventPayload, EventType, NodeId, Primitive, UiEvent};
use vo_ui_protocol::{
    ApplyError, EventEnvelope, MutationBatch, ProtocolLimits, Renderer, TreeMirror,
};

/// Semantic native object requested by a renderer-neutral primitive. The host
/// remains free to draw it with a GPU scene, a platform control, or a hybrid.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DesktopElement {
    Root,
    Fragment,
    Container,
    Scroll,
    Image,
    Button,
    TextInput,
    TextArea,
    Toggle,
    Slider,
    Canvas,
    PlatformView,
    Text,
}

impl DesktopElement {
    pub const fn for_primitive(primitive: Primitive) -> Self {
        match primitive {
            Primitive::Root => Self::Root,
            Primitive::Fragment => Self::Fragment,
            Primitive::Box
            | Primitive::Row
            | Primitive::Column
            | Primitive::Stack
            | Primitive::Grid => Self::Container,
            Primitive::Scroll => Self::Scroll,
            Primitive::Image => Self::Image,
            Primitive::Button => Self::Button,
            Primitive::TextInput => Self::TextInput,
            Primitive::TextArea => Self::TextArea,
            Primitive::Toggle => Self::Toggle,
            Primitive::Slider => Self::Slider,
            Primitive::Canvas => Self::Canvas,
            Primitive::PlatformView => Self::PlatformView,
            Primitive::Text => Self::Text,
        }
    }
}

/// Native hosts apply one complete revision to retained layout, semantics, and
/// presentation state. A rejected call must preserve the prior revision.
pub trait DesktopHost {
    type Error;

    fn apply_atomic(&mut self, batch: &MutationBatch) -> Result<(), Self::Error>;

    fn poll_event(&mut self) -> Result<Option<EventEnvelope>, Self::Error> {
        Ok(None)
    }
}

#[derive(Debug)]
pub enum DesktopRendererError<E> {
    Protocol(ApplyError),
    Host(E),
}

impl<E: fmt::Display> fmt::Display for DesktopRendererError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Protocol(error) => {
                write!(formatter, "desktop protocol validation failed: {error}")
            }
            Self::Host(error) => write!(formatter, "desktop host rejected revision: {error}"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DesktopEventError {
    SessionEpochMismatch,
    SequenceReplay,
    MissingTarget,
    ListenerMismatch,
}

impl fmt::Display for DesktopEventError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "invalid desktop event: {self:?}")
    }
}

#[derive(Debug, PartialEq)]
pub enum DesktopPollError<E> {
    Host(E),
    Invalid(DesktopEventError),
}

impl<E: fmt::Display> fmt::Display for DesktopPollError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Host(error) => write!(formatter, "desktop event polling failed: {error}"),
            Self::Invalid(error) => error.fmt(formatter),
        }
    }
}

/// Renderer-side security and transaction boundary shared by native AOT and
/// VM/JIT desktop development hosts.
pub struct DesktopRenderer<H> {
    host: H,
    tree: TreeMirror,
    session_epoch: u64,
    last_event_sequence: u64,
}

impl<H> DesktopRenderer<H> {
    pub fn new(host: H, session_epoch: u64, root: NodeId, limits: ProtocolLimits) -> Self {
        Self {
            host,
            tree: TreeMirror::new(session_epoch, root, limits),
            session_epoch,
            last_event_sequence: 0,
        }
    }

    pub const fn revision(&self) -> u64 {
        self.tree.revision()
    }

    pub fn host(&self) -> &H {
        &self.host
    }

    pub fn host_mut(&mut self) -> &mut H {
        &mut self.host
    }

    pub fn into_host(self) -> H {
        self.host
    }
}

impl<H: DesktopHost> DesktopRenderer<H> {
    /// Accept only an event targeting the exact listener in the latest
    /// committed desktop tree. Hosts may perform hit testing independently;
    /// they cannot forge a stale or unrelated handler identity.
    pub fn poll_event(&mut self) -> Result<Option<UiEvent>, DesktopPollError<H::Error>> {
        let Some(incoming) = self.host.poll_event().map_err(DesktopPollError::Host)? else {
            return Ok(None);
        };
        if incoming.session_epoch != self.session_epoch {
            return Err(DesktopPollError::Invalid(
                DesktopEventError::SessionEpochMismatch,
            ));
        }
        if incoming.event.sequence <= self.last_event_sequence {
            return Err(DesktopPollError::Invalid(DesktopEventError::SequenceReplay));
        }
        let invalidation = incoming.event.event == EventType::INVALIDATE
            && incoming.event.handler.index() == u32::MAX
            && incoming.event.handler.generation() == 1
            && incoming.event.target == self.tree.root()
            && incoming.event.payload == EventPayload::None;
        if !invalidation {
            let target = self
                .tree
                .node(incoming.event.target)
                .ok_or(DesktopPollError::Invalid(DesktopEventError::MissingTarget))?;
            let listener =
                target
                    .listeners
                    .get(&incoming.event.event)
                    .ok_or(DesktopPollError::Invalid(
                        DesktopEventError::ListenerMismatch,
                    ))?;
            if listener.handler != incoming.event.handler {
                return Err(DesktopPollError::Invalid(
                    DesktopEventError::ListenerMismatch,
                ));
            }
        }
        self.last_event_sequence = incoming.event.sequence;
        Ok(Some(incoming.event))
    }
}

impl<H: DesktopHost> Renderer for DesktopRenderer<H> {
    type Error = DesktopRendererError<H::Error>;

    fn apply(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
        let mut staged = self.tree.clone();
        staged
            .apply(batch)
            .map_err(DesktopRendererError::Protocol)?;
        self.host
            .apply_atomic(batch)
            .map_err(DesktopRendererError::Host)?;
        self.tree = staged;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::collections::VecDeque;
    use alloc::vec::Vec;
    use vo_ui_core::{EventPayload, EventType, HandlerId, Listener};
    use vo_ui_protocol::{Mutation, NodeKind};

    #[derive(Default)]
    struct TestHost {
        batches: Vec<MutationBatch>,
        events: VecDeque<EventEnvelope>,
        reject_next: bool,
    }

    impl DesktopHost for TestHost {
        type Error = &'static str;

        fn apply_atomic(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
            if self.reject_next {
                self.reject_next = false;
                return Err("rejected");
            }
            self.batches.push(batch.clone());
            Ok(())
        }

        fn poll_event(&mut self) -> Result<Option<EventEnvelope>, Self::Error> {
            Ok(self.events.pop_front())
        }
    }

    fn mount_button(renderer: &mut DesktopRenderer<TestHost>) -> (NodeId, HandlerId) {
        let root = NodeId::new(0, 1);
        let button = NodeId::new(1, 1);
        let handler = HandlerId::new(2, 1);
        renderer
            .apply(&MutationBatch::new(
                7,
                1,
                alloc::vec![
                    Mutation::Create {
                        id: button,
                        kind: NodeKind::Element(Primitive::Button),
                    },
                    Mutation::Listen {
                        id: button,
                        listener: Listener::new(EventType::CLICK, handler),
                    },
                    Mutation::InsertBefore {
                        parent: root,
                        child: button,
                        before: None,
                    },
                ],
            ))
            .unwrap();
        (button, handler)
    }

    #[test]
    fn primitive_mapping_keeps_platform_views_explicit() {
        assert_eq!(
            DesktopElement::for_primitive(Primitive::TextInput),
            DesktopElement::TextInput
        );
        assert_eq!(
            DesktopElement::for_primitive(Primitive::PlatformView),
            DesktopElement::PlatformView
        );
    }

    #[test]
    fn host_commit_is_atomic_with_protocol_state() {
        let mut renderer = DesktopRenderer::new(
            TestHost::default(),
            7,
            NodeId::new(0, 1),
            ProtocolLimits::default(),
        );
        mount_button(&mut renderer);
        assert_eq!(renderer.revision(), 1);
        assert_eq!(renderer.host().batches.len(), 1);

        renderer.host_mut().reject_next = true;
        assert!(matches!(
            renderer.apply(&MutationBatch::new(7, 2, Vec::new())),
            Err(DesktopRendererError::Host("rejected"))
        ));
        assert_eq!(renderer.revision(), 1);
    }

    #[test]
    fn reverse_events_require_live_listener_and_monotonic_sequence() {
        let mut renderer = DesktopRenderer::new(
            TestHost::default(),
            7,
            NodeId::new(0, 1),
            ProtocolLimits::default(),
        );
        let (button, handler) = mount_button(&mut renderer);
        let event = UiEvent {
            handler,
            event: EventType::CLICK,
            target: button,
            sequence: 1,
            payload: EventPayload::None,
        };
        renderer
            .host_mut()
            .events
            .push_back(EventEnvelope::new(7, event.clone()));
        assert_eq!(renderer.poll_event().unwrap(), Some(event.clone()));
        renderer
            .host_mut()
            .events
            .push_back(EventEnvelope::new(7, event));
        assert_eq!(
            renderer.poll_event(),
            Err(DesktopPollError::Invalid(DesktopEventError::SequenceReplay))
        );
    }

    #[test]
    fn trusted_invalidation_requires_the_reserved_root_tuple() {
        let mut renderer = DesktopRenderer::new(
            TestHost::default(),
            7,
            NodeId::new(0, 1),
            ProtocolLimits::default(),
        );
        mount_button(&mut renderer);
        let invalidation = UiEvent {
            handler: HandlerId::new(u32::MAX, 1),
            event: EventType::INVALIDATE,
            target: NodeId::new(0, 1),
            sequence: 1,
            payload: EventPayload::None,
        };
        renderer
            .host_mut()
            .events
            .push_back(EventEnvelope::new(7, invalidation.clone()));
        assert_eq!(renderer.poll_event().unwrap(), Some(invalidation));

        renderer.host_mut().events.push_back(EventEnvelope::new(
            7,
            UiEvent {
                handler: HandlerId::new(u32::MAX, 2),
                event: EventType::INVALIDATE,
                target: NodeId::new(0, 1),
                sequence: 2,
                payload: EventPayload::None,
            },
        ));
        assert_eq!(
            renderer.poll_event(),
            Err(DesktopPollError::Invalid(
                DesktopEventError::ListenerMismatch
            ))
        );
    }
}
