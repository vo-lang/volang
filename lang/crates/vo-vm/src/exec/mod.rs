//! Instruction execution modules.

mod load;
mod copy;
mod global;
mod ptr;
mod call;
mod string;
mod array;
mod slice;
mod map;
mod channel;
mod closure;
mod iface;
mod defer;
mod select;
mod goroutine;
mod unwind;
mod port;
mod island;

pub use load::*;
pub use copy::*;
pub use global::*;
pub use ptr::*;
pub use call::*;
pub use string::*;
pub use array::*;
pub use slice::*;
pub use map::*;
pub use channel::*;
pub use closure::*;
pub use iface::*;
pub use defer::*;
pub use select::*;
pub use goroutine::*;
pub use unwind::*;
pub use port::*;
pub use island::*;
