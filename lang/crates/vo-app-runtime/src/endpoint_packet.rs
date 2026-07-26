use alloc::vec::Vec;

use vo_app_protocol::channel::LaneLimits;
use vo_app_protocol::{ChannelHandle, DecodeError, MessageKind, SessionHandle};
use vo_runtime::host_services_v2::CallerEndpointHandle;

use crate::{BoundedLane, BoundedLaneConfig, BoundedLaneMetrics, LaneAdmission, LanePushError};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EndpointPacket {
    pub caller: CallerEndpointHandle,
    pub channel: ChannelHandle,
    pub channel_epoch: u64,
    pub message_kind: MessageKind,
    pub sequence: u64,
    pub request_id: u64,
    pub bytes: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct EndpointChannelBinding {
    pub session: SessionHandle,
    pub session_epoch: u64,
    pub caller: CallerEndpointHandle,
    pub channel: ChannelHandle,
    pub channel_epoch: u64,
    pub selected_minor: u16,
    pub selected_exact_fingerprint: [u8; 32],
    pub limits: LaneLimits,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EndpointPacketError {
    ChannelNotBound,
    WrongEndpoint,
    MalformedEnvelope(DecodeError),
    PacketTooLarge,
    WouldBlock,
    DuplicateOrOutOfOrder { expected: u64, found: u64 },
    SequenceExhausted,
}

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct EndpointPacketChannelMetrics {
    pub outbound: BoundedLaneMetrics,
    pub inbound: BoundedLaneMetrics,
}

pub(crate) struct EndpointPacketChannel {
    caller: CallerEndpointHandle,
    outbound: BoundedLane<EndpointPacket>,
    inbound: BoundedLane<EndpointPacket>,
    next_outbound_sequence: u64,
    next_inbound_sequence: u64,
    max_packet_bytes: usize,
}

impl EndpointPacketChannel {
    pub(crate) fn new(
        caller: CallerEndpointHandle,
        max_packet_bytes: usize,
        max_messages: usize,
        max_bytes: usize,
    ) -> Self {
        let config = BoundedLaneConfig {
            max_messages,
            max_bytes,
            reserved_messages: 0,
            reserved_bytes: 0,
        };
        Self {
            caller,
            outbound: BoundedLane::new(config)
                .expect("negotiated channel limits are non-zero and internally consistent"),
            inbound: BoundedLane::new(config)
                .expect("negotiated channel limits are non-zero and internally consistent"),
            next_outbound_sequence: 1,
            next_inbound_sequence: 1,
            max_packet_bytes,
        }
    }

    pub(crate) const fn caller(&self) -> CallerEndpointHandle {
        self.caller
    }

    pub(crate) const fn metrics(&self) -> EndpointPacketChannelMetrics {
        EndpointPacketChannelMetrics {
            outbound: self.outbound.metrics(),
            inbound: self.inbound.metrics(),
        }
    }

    pub(crate) fn push_outbound(
        &mut self,
        packet: EndpointPacket,
    ) -> Result<(), EndpointPacketError> {
        push_packet(
            &mut self.outbound,
            &mut self.next_outbound_sequence,
            self.max_packet_bytes,
            packet,
        )
    }

    pub(crate) fn push_inbound(
        &mut self,
        packet: EndpointPacket,
    ) -> Result<(), EndpointPacketError> {
        push_packet(
            &mut self.inbound,
            &mut self.next_inbound_sequence,
            self.max_packet_bytes,
            packet,
        )
    }

    pub(crate) fn push_inbound_batch(
        &mut self,
        packets: Vec<EndpointPacket>,
    ) -> Result<(), EndpointPacketError> {
        if packets.is_empty() {
            return Ok(());
        }
        let mut expected = self.next_inbound_sequence;
        let mut total_bytes = 0_usize;
        for packet in &packets {
            let packet_bytes = packet.bytes.len();
            if packet_bytes > self.max_packet_bytes {
                return Err(EndpointPacketError::PacketTooLarge);
            }
            if packet.sequence != expected {
                return Err(EndpointPacketError::DuplicateOrOutOfOrder {
                    expected,
                    found: packet.sequence,
                });
            }
            expected = expected
                .checked_add(1)
                .ok_or(EndpointPacketError::SequenceExhausted)?;
            total_bytes = total_bytes
                .checked_add(packet_bytes)
                .ok_or(EndpointPacketError::PacketTooLarge)?;
        }
        if !self
            .inbound
            .can_push_batch(packets.len(), total_bytes, LaneAdmission::Normal)
        {
            self.inbound.record_capacity_rejection();
            return Err(EndpointPacketError::WouldBlock);
        }
        for packet in packets {
            let packet_bytes = packet.bytes.len();
            self.inbound
                .try_push(packet, packet_bytes, LaneAdmission::Normal)
                .map_err(|error| match error {
                    LanePushError::ItemTooLarge(_) => EndpointPacketError::PacketTooLarge,
                    LanePushError::WouldBlock(_) => EndpointPacketError::WouldBlock,
                    LanePushError::SequenceExhausted(_) => EndpointPacketError::SequenceExhausted,
                })?;
        }
        self.next_inbound_sequence = expected;
        Ok(())
    }

    pub(crate) fn pop_outbound(&mut self) -> Option<EndpointPacket> {
        self.outbound.pop().map(|item| item.value)
    }

    pub(crate) fn pop_inbound(&mut self) -> Option<EndpointPacket> {
        self.inbound.pop().map(|item| item.value)
    }
}

fn push_packet(
    lane: &mut BoundedLane<EndpointPacket>,
    next_sequence: &mut u64,
    max_packet_bytes: usize,
    packet: EndpointPacket,
) -> Result<(), EndpointPacketError> {
    let packet_bytes = packet.bytes.len();
    if packet_bytes > max_packet_bytes {
        return Err(EndpointPacketError::PacketTooLarge);
    }
    if packet.sequence != *next_sequence {
        return Err(EndpointPacketError::DuplicateOrOutOfOrder {
            expected: *next_sequence,
            found: packet.sequence,
        });
    }
    let next = next_sequence
        .checked_add(1)
        .ok_or(EndpointPacketError::SequenceExhausted)?;
    lane.try_push(packet, packet_bytes, LaneAdmission::Normal)
        .map_err(|error| match error {
            LanePushError::ItemTooLarge(_) => EndpointPacketError::PacketTooLarge,
            LanePushError::WouldBlock(_) => EndpointPacketError::WouldBlock,
            LanePushError::SequenceExhausted(_) => EndpointPacketError::SequenceExhausted,
        })?;
    *next_sequence = next;
    Ok(())
}
