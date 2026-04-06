#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

mod app_session;
mod dispatch;
mod effects;
mod guest_runtime_session;
mod gui_session;
#[cfg(feature = "std")]
mod host_output;
mod mailbox;
#[cfg(feature = "std")]
mod native;
#[cfg(feature = "std")]
mod native_event_loop;
pub mod protocol;
mod render_buffer;
mod render_island_session;
mod scheduler;
mod session;
#[cfg(feature = "std")]
mod tick;
#[cfg(feature = "std")]
mod timer;
mod web;

pub use app_session::AppSession;
pub use dispatch::{
    emit_outbound_frames, emit_trimmed_stdout, ignore_not_waiting_for_events, SessionDispatchError,
};
pub use effects::StepResult;
pub use guest_runtime_session::GuestSession;
pub use gui_session::GuiAppSession;
#[cfg(feature = "std")]
pub use host_output::take_captured_stdout;
pub use mailbox::{PendingHostEvent, SessionMailbox};
#[cfg(feature = "std")]
pub use native::NativeGuiRuntime;
#[cfg(feature = "std")]
pub use native_event_loop::{spawn_native_gui, NativeGuestHandle, NativeGuiEventLoopConfig};
pub use render_buffer::RenderBuffer;
#[cfg(feature = "std")]
pub use render_buffer::SyncRenderBuffer;
pub use render_island_session::RenderIslandSession;
pub use scheduler::HostEventScheduler;
pub use session::{
    advance_session, drain_outbound_island_frames, push_targeted_inbound_island_frame,
    replay_event_wait_token, resume_waiting_event, run_inbound_island_command,
    run_inbound_island_frame, validate_scheduling_outcome, SessionError,
};
#[cfg(feature = "std")]
pub use tick::{NativeTickProvider, TickLoopControl};
#[cfg(feature = "std")]
pub(crate) use timer::{NativeTimerProvider, TimerControl};
pub use web::{GuestRuntime, RenderIslandRuntime};
