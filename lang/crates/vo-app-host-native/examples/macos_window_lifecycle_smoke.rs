#[cfg(all(target_os = "macos", feature = "macos-gpu"))]
fn main() {
    use std::thread;
    use std::time::Duration;

    use objc2::MainThreadMarker;
    use objc2_app_kit::NSApplication;
    use vo_app_host_native::{MacOsGpuWindow, MacOsGpuWindowConfig, NativeInputKind};
    use vo_app_protocol::{ViewHandle, WindowHandle};

    fn settle(window: &MacOsGpuWindow) {
        thread::sleep(Duration::from_millis(100));
        window.pump_events(128);
    }

    let mtm = MainThreadMarker::new().expect("run AppKit smoke on the main thread");
    let app = NSApplication::sharedApplication(mtm);
    app.finishLaunching();
    app.activate();

    let window_handle = WindowHandle {
        index: 71,
        generation: 1,
    };
    let view_handle = ViewHandle {
        index: 72,
        generation: 1,
    };
    let mut window = MacOsGpuWindow::new(
        window_handle,
        view_handle,
        MacOsGpuWindowConfig {
            title: String::from("Volang AppKit lifecycle smoke"),
            width_points: 640.0,
            height_points: 360.0,
            ..MacOsGpuWindowConfig::default()
        },
    )
    .expect("create AppKit lifecycle smoke window");

    window.show();
    settle(&window);
    let shown = window.metrics();
    assert!(shown.visible);

    let resized = window
        .resize_content(800.0, 450.0)
        .expect("resize AppKit content");
    settle(&window);
    assert!((resized.width_points - 800.0).abs() < 0.5);
    assert!((resized.height_points - 450.0).abs() < 0.5);

    window.minimize();
    settle(&window);
    assert!(window.is_minimized());
    window.restore();
    settle(&window);
    assert!(!window.is_minimized());
    assert!(window.metrics().visible);

    let lifecycle_events = window
        .drain_input(64)
        .expect("drain AppKit lifecycle events");
    let resize_events = lifecycle_events
        .iter()
        .filter(|event| matches!(event.kind, NativeInputKind::Resized { .. }))
        .count();
    let hidden_events = lifecycle_events
        .iter()
        .filter(|event| matches!(event.kind, NativeInputKind::VisibilityChanged(false)))
        .count();
    let visible_events = lifecycle_events
        .iter()
        .filter(|event| matches!(event.kind, NativeInputKind::VisibilityChanged(true)))
        .count();
    assert!(
        resize_events >= 1,
        "AppKit resize delegate emitted no event"
    );
    assert!(
        hidden_events >= 1,
        "AppKit minimize delegate emitted no hidden event"
    );
    assert!(
        visible_events >= 1,
        "AppKit restore delegate emitted no visible event"
    );

    window.close();
    println!(
        "{{\"passed\":true,\"resize_events\":{resize_events},\"hidden_events\":{hidden_events},\"visible_events\":{visible_events},\"width_points\":{},\"height_points\":{}}}",
        resized.width_points, resized.height_points
    );
}

#[cfg(not(all(target_os = "macos", feature = "macos-gpu")))]
fn main() {
    panic!("macos_window_lifecycle_smoke requires macOS and the macos-gpu feature");
}
