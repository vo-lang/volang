//! HostServices V2 primitives for native extensions.
//!
//! Every call uses the VM-authoritative per-call V2 table and hidden
//! CallerEndpoint identity. Calls outside an active extension invocation
//! fail closed.

pub mod capability;
pub mod v2;
