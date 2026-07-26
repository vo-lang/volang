mod input;
#[cfg(feature = "provider-loading")]
mod provider_loader;
mod route;

pub use input::{
    NativeHostInputError, NativeInputChannel, NativeInputChannelConfig, NativeInputEvent,
    NativeInputKind, NativeInputReceiver, NativeInputStats, NativeModifiers, NativePointerButton,
    NativeScrollUnit,
};
#[cfg(feature = "provider-loading")]
pub use provider_loader::{
    NativeProviderFactory, NativeProviderInstance, NativeProviderLifecycleState,
    NativeProviderLoadError,
};
pub use route::{route_native_input, NativeInputRoute};

#[cfg(feature = "native-wgpu")]
mod wgpu_compositor;
#[cfg(feature = "native-wgpu")]
pub use wgpu_compositor::{WgpuCompositorAdapter, WgpuCompositorConfig};

#[cfg(all(target_os = "macos", feature = "macos-appkit"))]
mod macos;
#[cfg(all(target_os = "macos", feature = "macos-appkit"))]
pub use macos::{MacOsGpuWindow, MacOsGpuWindowConfig, MacOsViewMetrics};
