#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(feature = "std")]
use std::vec::Vec;

use crate::vm::RuntimeTrapKind;

use vo_runtime::objects::queue_state::QueueWaiter;

pub enum QueueAction {
    Continue,
    Block,
    ReplayThenBlock,
    Wake(QueueWaiter),
    Trap(RuntimeTrapKind),
    Close {
        waiters: Vec<QueueWaiter>,
        endpoint_id: Option<u64>,
    },
    RemoteSend {
        endpoint_id: u64,
        home_island: u32,
        data: Vec<u8>,
    },
    RemoteRecv {
        endpoint_id: u64,
        home_island: u32,
    },
    RemoteRecvData {
        endpoint_id: u64,
        target_island: u32,
        fiber_id: u64,
        data: Vec<u8>,
    },
    RemoteClose {
        endpoint_id: u64,
        home_island: u32,
    },
}
