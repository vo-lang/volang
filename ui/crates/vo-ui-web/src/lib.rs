#![no_std]

extern crate alloc;

mod ssr;

pub use ssr::{
    render_document, stream_document, ActivationEntry, AssetLink, DocumentMetadata,
    RenderedDocument, SsrError, SsrLimits,
};

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt;
use vo_ui_core::{
    EventType, HandlerId, Listener, NodeId, Primitive, Property, PropertyId, UiEvent,
};
use vo_ui_protocol::{
    decode_event, encode_batch, ApplyError, CodecError, Mutation, MutationBatch, NodeKind,
    ProtocolLimits, Renderer, TreeMirror,
};

pub use vo_ui_protocol::EventEnvelope as DomEvent;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DomElement {
    RootHost,
    FragmentBoundary,
    Div,
    Image,
    Button,
    TextInput,
    TextArea,
    Checkbox,
    RangeInput,
    Canvas,
    PlatformView,
    Text,
}

impl DomElement {
    pub const fn for_primitive(primitive: Primitive) -> Self {
        match primitive {
            Primitive::Root => Self::RootHost,
            Primitive::Fragment => Self::FragmentBoundary,
            Primitive::Box
            | Primitive::Row
            | Primitive::Column
            | Primitive::Stack
            | Primitive::Grid
            | Primitive::Scroll => Self::Div,
            Primitive::Image => Self::Image,
            Primitive::Button => Self::Button,
            Primitive::TextInput => Self::TextInput,
            Primitive::TextArea => Self::TextArea,
            Primitive::Toggle => Self::Checkbox,
            Primitive::Slider => Self::RangeInput,
            Primitive::Canvas => Self::Canvas,
            Primitive::PlatformView => Self::PlatformView,
            Primitive::Text => Self::Text,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DomCommand {
    CreateElement {
        id: NodeId,
        primitive: Primitive,
        element: DomElement,
    },
    CreateText {
        id: NodeId,
    },
    SetText {
        id: NodeId,
        text: String,
    },
    SetProperty {
        id: NodeId,
        property: Property,
    },
    RemoveProperty {
        id: NodeId,
        property: PropertyId,
    },
    Listen {
        id: NodeId,
        listener: Listener,
    },
    Unlisten {
        id: NodeId,
        event: EventType,
        handler: HandlerId,
    },
    InsertBefore {
        parent: NodeId,
        child: NodeId,
        before: Option<NodeId>,
    },
    Remove {
        parent: NodeId,
        child: NodeId,
    },
    Delete {
        id: NodeId,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct DomBatch {
    pub session_epoch: u64,
    pub revision: u64,
    pub commands: Vec<DomCommand>,
}

/// Browser implementations stage every command and publish them together.
/// A failed call must leave the previous DOM revision intact.
pub trait DomHost {
    type Error;

    fn apply_atomic(&mut self, batch: &DomBatch) -> Result<(), Self::Error>;

    fn poll_event(&mut self) -> Result<Option<DomEvent>, Self::Error> {
        Ok(None)
    }
}

#[derive(Debug)]
pub enum DomRendererError<E> {
    Protocol(ApplyError),
    Host(E),
}

impl<E: fmt::Display> fmt::Display for DomRendererError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Protocol(error) => write!(formatter, "DOM protocol validation failed: {error}"),
            Self::Host(error) => write!(formatter, "DOM host rejected revision: {error}"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DomEventError {
    SessionEpochMismatch,
    SequenceReplay,
    MissingTarget,
    ListenerMismatch,
}

impl fmt::Display for DomEventError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "invalid DOM event: {self:?}")
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum DomPollError<E> {
    Host(E),
    Invalid(DomEventError),
}

impl<E: fmt::Display> fmt::Display for DomPollError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Host(error) => write!(formatter, "DOM host event polling failed: {error}"),
            Self::Invalid(error) => error.fmt(formatter),
        }
    }
}

pub struct DomRenderer<H: DomHost> {
    host: H,
    tree: TreeMirror,
    session_epoch: u64,
    last_event_sequence: u64,
}

impl<H: DomHost> DomRenderer<H> {
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

    pub fn tree(&self) -> &TreeMirror {
        &self.tree
    }

    pub fn into_host(self) -> H {
        self.host
    }

    /// Accepts only events targeting the exact live listener identity in the
    /// last committed tree. Replayed or stale-generation events are discarded.
    pub fn poll_event(&mut self) -> Result<Option<UiEvent>, DomPollError<H::Error>> {
        let Some(incoming) = self.host.poll_event().map_err(DomPollError::Host)? else {
            return Ok(None);
        };
        if incoming.session_epoch != self.session_epoch {
            return Err(DomPollError::Invalid(DomEventError::SessionEpochMismatch));
        }
        if incoming.event.sequence <= self.last_event_sequence {
            return Err(DomPollError::Invalid(DomEventError::SequenceReplay));
        }
        let target = self
            .tree
            .node(incoming.event.target)
            .ok_or(DomPollError::Invalid(DomEventError::MissingTarget))?;
        let listener = target
            .listeners
            .get(&incoming.event.event)
            .ok_or(DomPollError::Invalid(DomEventError::ListenerMismatch))?;
        if listener.handler != incoming.event.handler {
            return Err(DomPollError::Invalid(DomEventError::ListenerMismatch));
        }
        self.last_event_sequence = incoming.event.sequence;
        Ok(Some(incoming.event))
    }
}

/// Minimal byte-frame ABI implemented by generated browser glue. Application
/// code and component libraries never import this interface directly.
pub trait WebWire {
    type Error;

    fn apply_mutation_frame(&mut self, frame: &[u8]) -> Result<(), Self::Error>;

    fn poll_event_frame(&mut self, max_bytes: usize) -> Result<Option<Vec<u8>>, Self::Error>;
}

#[derive(Debug)]
pub enum WireDomHostError<E> {
    Codec(CodecError),
    Transport(E),
}

impl<E: fmt::Display> fmt::Display for WireDomHostError<E> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Codec(error) => write!(formatter, "Web UI frame codec failed: {error}"),
            Self::Transport(error) => write!(formatter, "Web UI host transport failed: {error}"),
        }
    }
}

pub struct WireDomHost<W> {
    wire: W,
    limits: ProtocolLimits,
}

impl<W> WireDomHost<W> {
    pub const fn new(wire: W, limits: ProtocolLimits) -> Self {
        Self { wire, limits }
    }

    pub fn wire(&self) -> &W {
        &self.wire
    }

    pub fn wire_mut(&mut self) -> &mut W {
        &mut self.wire
    }

    pub fn into_wire(self) -> W {
        self.wire
    }
}

impl<W: WebWire> DomHost for WireDomHost<W> {
    type Error = WireDomHostError<W::Error>;

    fn apply_atomic(&mut self, batch: &DomBatch) -> Result<(), Self::Error> {
        let protocol = MutationBatch::new(
            batch.session_epoch,
            batch.revision,
            batch.commands.iter().map(command_as_mutation).collect(),
        );
        let frame = encode_batch(&protocol, self.limits).map_err(WireDomHostError::Codec)?;
        self.wire
            .apply_mutation_frame(&frame)
            .map_err(WireDomHostError::Transport)
    }

    fn poll_event(&mut self) -> Result<Option<DomEvent>, Self::Error> {
        let Some(frame) = self
            .wire
            .poll_event_frame(self.limits.max_event_bytes)
            .map_err(WireDomHostError::Transport)?
        else {
            return Ok(None);
        };
        decode_event(&frame, self.limits)
            .map(Some)
            .map_err(WireDomHostError::Codec)
    }
}

fn command_as_mutation(command: &DomCommand) -> Mutation {
    match command {
        DomCommand::CreateElement { id, primitive, .. } => Mutation::Create {
            id: *id,
            kind: NodeKind::Element(*primitive),
        },
        DomCommand::CreateText { id } => Mutation::Create {
            id: *id,
            kind: NodeKind::Text,
        },
        DomCommand::SetText { id, text } => Mutation::SetText {
            id: *id,
            text: text.clone(),
        },
        DomCommand::SetProperty { id, property } => Mutation::SetProperty {
            id: *id,
            property: property.clone(),
        },
        DomCommand::RemoveProperty { id, property } => Mutation::RemoveProperty {
            id: *id,
            property: *property,
        },
        DomCommand::Listen { id, listener } => Mutation::Listen {
            id: *id,
            listener: *listener,
        },
        DomCommand::Unlisten { id, event, handler } => Mutation::Unlisten {
            id: *id,
            event: *event,
            handler: *handler,
        },
        DomCommand::InsertBefore {
            parent,
            child,
            before,
        } => Mutation::InsertBefore {
            parent: *parent,
            child: *child,
            before: *before,
        },
        DomCommand::Remove { parent, child } => Mutation::Remove {
            parent: *parent,
            child: *child,
        },
        DomCommand::Delete { id } => Mutation::Delete { id: *id },
    }
}

#[cfg(target_arch = "wasm32")]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BrowserImportError {
    ApplyRejected(u32),
    EventLengthRejected,
    EventFrameTooLarge,
    EventReadRejected(u32),
}

#[cfg(target_arch = "wasm32")]
impl fmt::Display for BrowserImportError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "browser import failed: {self:?}")
    }
}

/// Production browser transport. The packager supplies these three imports as
/// generated, version-pinned glue below the Volang application boundary.
#[cfg(target_arch = "wasm32")]
#[derive(Clone, Copy, Debug, Default)]
pub struct BrowserImports;

#[cfg(target_arch = "wasm32")]
#[link(wasm_import_module = "volang_ui_web_v1")]
unsafe extern "C" {
    #[link_name = "apply_mutation_frame"]
    fn browser_apply_mutation_frame(ptr: *const u8, len: u32) -> u32;
    #[link_name = "next_event_frame_len"]
    fn browser_next_event_frame_len() -> u32;
    #[link_name = "read_event_frame"]
    fn browser_read_event_frame(ptr: *mut u8, len: u32) -> u32;
}

#[cfg(target_arch = "wasm32")]
impl WebWire for BrowserImports {
    type Error = BrowserImportError;

    fn apply_mutation_frame(&mut self, frame: &[u8]) -> Result<(), Self::Error> {
        let len = u32::try_from(frame.len()).map_err(|_| BrowserImportError::EventFrameTooLarge)?;
        // The import copies the frame during this call and returns zero on an
        // atomically accepted DOM revision.
        let status = unsafe { browser_apply_mutation_frame(frame.as_ptr(), len) };
        if status == 0 {
            Ok(())
        } else {
            Err(BrowserImportError::ApplyRejected(status))
        }
    }

    fn poll_event_frame(&mut self, max_bytes: usize) -> Result<Option<Vec<u8>>, Self::Error> {
        // Zero means that the browser event queue is currently empty; the
        // all-ones sentinel reports an adapter failure.
        let len = unsafe { browser_next_event_frame_len() };
        if len == 0 {
            return Ok(None);
        }
        if len == u32::MAX {
            return Err(BrowserImportError::EventLengthRejected);
        }
        let len = len as usize;
        if len > max_bytes {
            return Err(BrowserImportError::EventFrameTooLarge);
        }
        let mut frame = alloc::vec![0; len];
        // The adapter must copy exactly the length returned by the preceding
        // peek; a non-zero status rejects races and partial reads.
        let status = unsafe { browser_read_event_frame(frame.as_mut_ptr(), len as u32) };
        if status == 0 {
            Ok(Some(frame))
        } else {
            Err(BrowserImportError::EventReadRejected(status))
        }
    }
}

impl<H: DomHost> Renderer for DomRenderer<H> {
    type Error = DomRendererError<H::Error>;

    fn apply(&mut self, batch: &MutationBatch) -> Result<(), Self::Error> {
        let mut staged = self.tree.clone();
        staged.apply(batch).map_err(DomRendererError::Protocol)?;
        let dom_batch = DomBatch {
            session_epoch: batch.session_epoch,
            revision: batch.revision,
            commands: batch.mutations.iter().map(lower_mutation).collect(),
        };
        self.host
            .apply_atomic(&dom_batch)
            .map_err(DomRendererError::Host)?;
        self.tree = staged;
        Ok(())
    }
}

fn lower_mutation(mutation: &Mutation) -> DomCommand {
    match mutation {
        Mutation::Create { id, kind } => match kind {
            NodeKind::Element(primitive) => DomCommand::CreateElement {
                id: *id,
                primitive: *primitive,
                element: DomElement::for_primitive(*primitive),
            },
            NodeKind::Text => DomCommand::CreateText { id: *id },
        },
        Mutation::SetText { id, text } => DomCommand::SetText {
            id: *id,
            text: text.clone(),
        },
        Mutation::SetProperty { id, property } => DomCommand::SetProperty {
            id: *id,
            property: property.clone(),
        },
        Mutation::RemoveProperty { id, property } => DomCommand::RemoveProperty {
            id: *id,
            property: *property,
        },
        Mutation::Listen { id, listener } => DomCommand::Listen {
            id: *id,
            listener: *listener,
        },
        Mutation::Unlisten { id, event, handler } => DomCommand::Unlisten {
            id: *id,
            event: *event,
            handler: *handler,
        },
        Mutation::InsertBefore {
            parent,
            child,
            before,
        } => DomCommand::InsertBefore {
            parent: *parent,
            child: *child,
            before: *before,
        },
        Mutation::Remove { parent, child } => DomCommand::Remove {
            parent: *parent,
            child: *child,
        },
        Mutation::Delete { id } => DomCommand::Delete { id: *id },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::collections::VecDeque;
    use vo_ui_core::{EventPayload, Primitive};

    #[derive(Default)]
    struct TestHost {
        batches: Vec<DomBatch>,
        events: VecDeque<DomEvent>,
        reject_next: bool,
    }

    impl DomHost for TestHost {
        type Error = &'static str;

        fn apply_atomic(&mut self, batch: &DomBatch) -> Result<(), Self::Error> {
            if self.reject_next {
                self.reject_next = false;
                return Err("rejected");
            }
            self.batches.push(batch.clone());
            Ok(())
        }

        fn poll_event(&mut self) -> Result<Option<DomEvent>, Self::Error> {
            Ok(self.events.pop_front())
        }
    }

    fn mount_button(renderer: &mut DomRenderer<TestHost>) -> (NodeId, HandlerId) {
        let root = NodeId::new(0, 1);
        let button = NodeId::new(1, 1);
        let handler = HandlerId::new(3, 1);
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
    fn mutation_batch_lowers_to_one_atomic_dom_commit() {
        let root = NodeId::new(0, 1);
        let mut renderer =
            DomRenderer::new(TestHost::default(), 7, root, ProtocolLimits::default());
        mount_button(&mut renderer);
        assert_eq!(renderer.revision(), 1);
        assert_eq!(renderer.host().batches.len(), 1);
        assert!(matches!(
            renderer.host().batches[0].commands[0],
            DomCommand::CreateElement {
                element: DomElement::Button,
                ..
            }
        ));
    }

    #[test]
    fn host_rejection_keeps_the_previous_dom_revision() {
        let root = NodeId::new(0, 1);
        let mut renderer =
            DomRenderer::new(TestHost::default(), 7, root, ProtocolLimits::default());
        renderer.host_mut().reject_next = true;
        assert!(matches!(
            renderer.apply(&MutationBatch::new(7, 1, Vec::new())),
            Err(DomRendererError::Host("rejected"))
        ));
        assert_eq!(renderer.revision(), 0);
    }

    #[test]
    fn reverse_events_require_live_listener_and_monotonic_sequence() {
        let root = NodeId::new(0, 1);
        let mut renderer =
            DomRenderer::new(TestHost::default(), 7, root, ProtocolLimits::default());
        let (button, handler) = mount_button(&mut renderer);
        let event = UiEvent {
            handler,
            event: EventType::CLICK,
            target: button,
            sequence: 1,
            payload: EventPayload::None,
        };
        renderer.host_mut().events.push_back(DomEvent {
            session_epoch: 7,
            event: event.clone(),
        });
        assert_eq!(renderer.poll_event().unwrap(), Some(event.clone()));

        renderer.host_mut().events.push_back(DomEvent {
            session_epoch: 7,
            event,
        });
        assert_eq!(
            renderer.poll_event(),
            Err(DomPollError::Invalid(DomEventError::SequenceReplay))
        );
    }

    #[derive(Default)]
    struct TestWire {
        mutations: Vec<Vec<u8>>,
        events: VecDeque<Vec<u8>>,
    }

    impl WebWire for TestWire {
        type Error = &'static str;

        fn apply_mutation_frame(&mut self, frame: &[u8]) -> Result<(), Self::Error> {
            self.mutations.push(frame.to_vec());
            Ok(())
        }

        fn poll_event_frame(&mut self, max_bytes: usize) -> Result<Option<Vec<u8>>, Self::Error> {
            let event = self.events.pop_front();
            if event.as_ref().is_some_and(|event| event.len() > max_bytes) {
                return Err("oversized event");
            }
            Ok(event)
        }
    }

    #[test]
    fn wire_host_uses_versioned_frames_in_both_directions() {
        let root = NodeId::new(0, 1);
        let button = NodeId::new(1, 1);
        let handler = HandlerId::new(4, 2);
        let limits = ProtocolLimits::default();
        let host = WireDomHost::new(TestWire::default(), limits);
        let mut renderer = DomRenderer::new(host, 11, root, limits);
        let mutations = MutationBatch::new(
            11,
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
        );
        renderer.apply(&mutations).unwrap();
        let applied = vo_ui_protocol::decode_batch(
            &renderer.host().wire().mutations[0],
            ProtocolLimits::default(),
        )
        .unwrap();
        assert_eq!(applied, mutations);

        let event = UiEvent {
            handler,
            event: EventType::CLICK,
            target: button,
            sequence: 1,
            payload: EventPayload::None,
        };
        let frame = vo_ui_protocol::encode_event(
            &DomEvent::new(11, event.clone()),
            ProtocolLimits::default(),
        )
        .unwrap();
        renderer.host_mut().wire_mut().events.push_back(frame);
        assert_eq!(renderer.poll_event().unwrap(), Some(event));
    }
}
