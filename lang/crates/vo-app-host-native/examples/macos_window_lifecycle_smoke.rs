#[cfg(all(target_os = "macos", feature = "macos-gpu"))]
fn main() {
    use std::time::{Duration, Instant};

    use objc2::MainThreadMarker;
    use objc2_app_kit::NSApplication;
    use vo_app_host_native::{
        MacOsGpuWindow, MacOsGpuWindowConfig, NativeInputEvent, NativeInputKind,
    };
    use vo_app_protocol::{ViewHandle, WindowHandle};

    fn wait_for(
        window: &MacOsGpuWindow,
        phase: &str,
        completed: impl Fn(&MacOsGpuWindow, &[NativeInputEvent]) -> bool,
    ) -> Vec<NativeInputEvent> {
        let started = Instant::now();
        let mut events = Vec::new();
        loop {
            window.pump_events(128);
            events.extend(
                window
                    .drain_input(128)
                    .expect("drain AppKit lifecycle events"),
            );
            assert!(
                events.len() <= 1024,
                "AppKit {phase} exceeded its event budget"
            );
            if completed(window, &events) {
                return events;
            }
            assert!(
                started.elapsed() < Duration::from_secs(5),
                "AppKit {phase} did not complete; metrics={:?}, events={events:?}",
                window.metrics()
            );
        }
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
    wait_for(&window, "show", |window, _| window.metrics().visible);
    let shown = window.metrics();
    assert!(shown.visible);

    let resized = window
        .resize_content(800.0, 450.0)
        .expect("resize AppKit content");
    let resize_input = wait_for(&window, "resize", |window, events| {
        let metrics = window.metrics();
        (metrics.width_points - 800.0).abs() < 0.5
            && (metrics.height_points - 450.0).abs() < 0.5
            && events
                .iter()
                .any(|event| matches!(event.kind, NativeInputKind::Resized { .. }))
    });
    assert!((resized.width_points - 800.0).abs() < 0.5);
    assert!((resized.height_points - 450.0).abs() < 0.5);

    window.minimize();
    let hidden_input = wait_for(&window, "minimize", |window, events| {
        window.is_minimized()
            && events
                .iter()
                .any(|event| matches!(event.kind, NativeInputKind::VisibilityChanged(false)))
    });
    assert!(window.is_minimized());
    window.restore();
    let visible_input = wait_for(&window, "restore", |window, events| {
        !window.is_minimized()
            && window.metrics().visible
            && events
                .iter()
                .any(|event| matches!(event.kind, NativeInputKind::VisibilityChanged(true)))
    });
    assert!(!window.is_minimized());
    assert!(window.metrics().visible);

    let resize_events = resize_input
        .iter()
        .filter(|event| matches!(event.kind, NativeInputKind::Resized { .. }))
        .count();
    let hidden_events = hidden_input
        .iter()
        .filter(|event| matches!(event.kind, NativeInputKind::VisibilityChanged(false)))
        .count();
    let visible_events = visible_input
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
    assert!(
        !window.metrics().visible,
        "closed AppKit window remained visible"
    );
    window.close();
    println!(
        "{{\"schema\":\"volang.appkit-lifecycle-result.v1\",\"passed\":true,\"complete\":true,\"checks\":[\"show\",\"resize\",\"minimize\",\"restore\",\"close\"],\"resize_events\":{resize_events},\"hidden_events\":{hidden_events},\"visible_events\":{visible_events},\"width_points\":{},\"height_points\":{}}}",
        resized.width_points, resized.height_points
    );
}

#[cfg(not(all(target_os = "macos", feature = "macos-gpu")))]
fn main() {
    panic!("macos_window_lifecycle_smoke requires macOS and the macos-gpu feature");
}
