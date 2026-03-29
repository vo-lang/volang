//! Instruction execution modules.

mod array;
mod call;
mod closure;
mod copy;
mod defer;
mod global;
mod goroutine;
mod iface;
mod island;
mod load;
mod map;
mod ptr;
pub mod queue;
mod select;
mod slice;
mod string;
mod transport;
mod unwind;

pub use array::*;
pub use call::*;
pub use closure::*;
pub use copy::*;
pub use defer::*;
pub use global::*;
pub use goroutine::*;
pub use iface::*;
pub use island::*;
pub use load::*;
pub use map::*;
pub use ptr::*;
pub use queue::*;
pub use select::*;
pub use slice::*;
pub use string::*;
pub use transport::*;
pub use unwind::*;
