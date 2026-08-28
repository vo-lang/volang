use std::{cell::RefCell, ffi::c_void, sync::Arc, time::Instant};

use objc2::{
    class, define_class, msg_send,
    rc::Retained,
    runtime::{AnyObject, NSObject, ProtocolObject, Sel},
    ClassType, DefinedClass, MainThreadMarker, MainThreadOnly,
};
use objc2_app_kit::{
    NSAccessibility, NSApplication, NSBackingStoreType, NSDragOperation, NSDraggingDestination,
    NSDraggingInfo, NSEvent, NSEventMask, NSEventModifierFlags, NSPasteboardTypeFileURL,
    NSTextInputClient, NSView, NSWindow, NSWindowAnimationBehavior, NSWindowDelegate,
    NSWindowStyleMask,
};
use objc2_foundation::{
    NSArray, NSAttributedString, NSAttributedStringKey, NSDate, NSDefaultRunLoopMode,
    NSNotification, NSObjectProtocol, NSPoint, NSRange, NSRangePointer, NSRect, NSRunLoop, NSSize,
    NSString, NSUInteger, NSURL,
};
use vo_app_protocol::{ViewHandle, WindowHandle};

use crate::{
    NativeHostInputError, NativeInputChannel, NativeInputChannelConfig, NativeInputKind,
    NativeInputReceiver, NativeModifiers, NativePointerButton, NativeScrollUnit,
};

#[derive(Clone, Debug, PartialEq)]
pub struct MacOsGpuWindowConfig {
    pub title: String,
    pub width_points: f64,
    pub height_points: f64,
    pub min_width_points: f64,
    pub min_height_points: f64,
    pub input: NativeInputChannelConfig,
}

impl Default for MacOsGpuWindowConfig {
    fn default() -> Self {
        Self {
            title: String::from("Volang"),
            width_points: 1_280.0,
            height_points: 720.0,
            min_width_points: 320.0,
            min_height_points: 200.0,
            input: NativeInputChannelConfig::default(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct MacOsViewMetrics {
    pub width_points: f64,
    pub height_points: f64,
    pub scale_factor: f64,
    pub visible: bool,
}

#[derive(Clone)]
struct NativeClock {
    origin: Instant,
}

impl NativeClock {
    fn now_micros(&self) -> u64 {
        u64::try_from(self.origin.elapsed().as_micros()).unwrap_or(u64::MAX)
    }
}

struct TextInputState {
    marked: String,
    marked_range: NSRange,
    selected_range: NSRange,
    caret_rect: NSRect,
}

impl Default for TextInputState {
    fn default() -> Self {
        Self {
            marked: String::new(),
            marked_range: not_found_range(),
            selected_range: NSRange::new(0, 0),
            caret_rect: NSRect::new(NSPoint::new(0.0, 0.0), NSSize::new(1.0, 1.0)),
        }
    }
}

struct InputViewIvars {
    window: WindowHandle,
    view: ViewHandle,
    channel: NativeInputChannel,
    clock: Arc<NativeClock>,
    text: RefCell<TextInputState>,
}

define_class!(
    #[unsafe(super(NSView))]
    #[name = "VoAppNativeInputView"]
    #[thread_kind = MainThreadOnly]
    #[ivars = InputViewIvars]
    struct MacOsInputView;

    impl MacOsInputView {
        #[unsafe(method(acceptsFirstResponder))]
        fn accepts_first_responder(&self) -> bool {
            true
        }

        #[unsafe(method(isFlipped))]
        fn is_flipped(&self) -> bool {
            true
        }

        #[unsafe(method(mouseDown:))]
        fn mouse_down(&self, event: &NSEvent) {
            self.publish_pointer_button(event, true, NativePointerButton::Primary);
        }

        #[unsafe(method(mouseUp:))]
        fn mouse_up(&self, event: &NSEvent) {
            self.publish_pointer_button(event, false, NativePointerButton::Primary);
        }

        #[unsafe(method(rightMouseDown:))]
        fn right_mouse_down(&self, event: &NSEvent) {
            self.publish_pointer_button(event, true, NativePointerButton::Secondary);
        }

        #[unsafe(method(rightMouseUp:))]
        fn right_mouse_up(&self, event: &NSEvent) {
            self.publish_pointer_button(event, false, NativePointerButton::Secondary);
        }

        #[unsafe(method(otherMouseDown:))]
        fn other_mouse_down(&self, event: &NSEvent) {
            self.publish_pointer_button(event, true, pointer_button(event.buttonNumber()));
        }

        #[unsafe(method(otherMouseUp:))]
        fn other_mouse_up(&self, event: &NSEvent) {
            self.publish_pointer_button(event, false, pointer_button(event.buttonNumber()));
        }

        #[unsafe(method(mouseMoved:))]
        fn mouse_moved(&self, event: &NSEvent) {
            self.publish_pointer_move(event);
        }

        #[unsafe(method(mouseDragged:))]
        fn mouse_dragged(&self, event: &NSEvent) {
            self.publish_pointer_move(event);
        }

        #[unsafe(method(rightMouseDragged:))]
        fn right_mouse_dragged(&self, event: &NSEvent) {
            self.publish_pointer_move(event);
        }

        #[unsafe(method(otherMouseDragged:))]
        fn other_mouse_dragged(&self, event: &NSEvent) {
            self.publish_pointer_move(event);
        }

        #[unsafe(method(scrollWheel:))]
        fn scroll_wheel(&self, event: &NSEvent) {
            // SAFETY: These are CGFloat-returning NSEvent selectors available
            // on every supported macOS target.
            let delta_x: f64 = unsafe { msg_send![event, scrollingDeltaX] };
            let delta_y: f64 = unsafe { msg_send![event, scrollingDeltaY] };
            let point = self.convertPoint_fromView(event.locationInWindow(), None);
            self.publish(NativeInputKind::Wheel {
                device: 1,
                x_milli: milli_i32(point.x),
                y_milli: milli_i32(point.y),
                delta_x_milli: milli_i32(delta_x),
                delta_y_milli: milli_i32(delta_y),
                unit: if event.hasPreciseScrollingDeltas() {
                    NativeScrollUnit::Pixel
                } else {
                    NativeScrollUnit::Line
                },
            });
        }

        #[unsafe(method(keyDown:))]
        fn key_down(&self, event: &NSEvent) {
            self.publish_key(event, true);
            let events = NSArray::from_slice(&[event]);
            self.interpretKeyEvents(&events);
        }

        #[unsafe(method(keyUp:))]
        fn key_up(&self, event: &NSEvent) {
            self.publish_key(event, false);
        }

        #[unsafe(method(flagsChanged:))]
        fn flags_changed(&self, event: &NSEvent) {
            self.publish(NativeInputKind::ModifiersChanged(modifiers(
                event.modifierFlags(),
            )));
        }
    }

    unsafe impl NSTextInputClient for MacOsInputView {
        #[unsafe(method(insertText:replacementRange:))]
        unsafe fn insert_text(&self, string: &AnyObject, replacement_range: NSRange) {
            let text = input_string(string);
            {
                let mut state = self.ivars().text.borrow_mut();
                state.marked.clear();
                state.marked_range = not_found_range();
                state.selected_range =
                    replacement_selection(replacement_range, text.encode_utf16().count());
            }
            if !text.is_empty() {
                self.publish(NativeInputKind::ImeCommitted(text.clone()));
                self.publish(NativeInputKind::Text(text));
            }
        }

        #[unsafe(method(doCommandBySelector:))]
        unsafe fn do_command(&self, _selector: Sel) {}

        #[unsafe(method(setMarkedText:selectedRange:replacementRange:))]
        unsafe fn set_marked_text(
            &self,
            string: &AnyObject,
            selected_range: NSRange,
            _replacement_range: NSRange,
        ) {
            let text = input_string(string);
            let starting = self.ivars().text.borrow().marked.is_empty();
            let text_len = text.encode_utf16().count();
            {
                let mut state = self.ivars().text.borrow_mut();
                state.marked = text.clone();
                state.marked_range = NSRange::new(0, text_len);
                state.selected_range = selected_range;
            }
            if starting {
                self.publish(NativeInputKind::ImeStarted);
            }
            self.publish(NativeInputKind::ImeUpdated {
                text,
                selection_start_utf16: u32::try_from(selected_range.location).unwrap_or(u32::MAX),
                selection_len_utf16: u32::try_from(selected_range.length).unwrap_or(u32::MAX),
            });
        }

        #[unsafe(method(unmarkText))]
        fn unmark_text(&self) {
            let marked = {
                let mut state = self.ivars().text.borrow_mut();
                state.marked_range = not_found_range();
                std::mem::take(&mut state.marked)
            };
            if marked.is_empty() {
                self.publish(NativeInputKind::ImeCancelled);
            } else {
                self.publish(NativeInputKind::ImeCommitted(marked));
            }
        }

        #[unsafe(method(selectedRange))]
        fn selected_range(&self) -> NSRange {
            self.ivars().text.borrow().selected_range
        }

        #[unsafe(method(markedRange))]
        fn marked_range(&self) -> NSRange {
            self.ivars().text.borrow().marked_range
        }

        #[unsafe(method(hasMarkedText))]
        fn has_marked_text(&self) -> bool {
            !self.ivars().text.borrow().marked.is_empty()
        }

        #[unsafe(method_id(attributedSubstringForProposedRange:actualRange:))]
        unsafe fn attributed_substring(
            &self,
            _range: NSRange,
            actual_range: NSRangePointer,
        ) -> Option<Retained<NSAttributedString>> {
            if !actual_range.is_null() {
                // SAFETY: AppKit supplied the out pointer for this method.
                unsafe {
                    *actual_range = not_found_range();
                }
            }
            None
        }

        #[unsafe(method_id(validAttributesForMarkedText))]
        fn valid_attributes(&self) -> Retained<NSArray<NSAttributedStringKey>> {
            NSArray::new()
        }

        #[unsafe(method(firstRectForCharacterRange:actualRange:))]
        unsafe fn first_rect(
            &self,
            range: NSRange,
            actual_range: NSRangePointer,
        ) -> NSRect {
            if !actual_range.is_null() {
                // SAFETY: AppKit supplied the out pointer for this method.
                unsafe {
                    *actual_range = range;
                }
            }
            let rect = self.ivars().text.borrow().caret_rect;
            let in_window = self.convertRect_toView(rect, None);
            self.window()
                .map_or(in_window, |window| window.convertRectToScreen(in_window))
        }

        #[unsafe(method(characterIndexForPoint:))]
        fn character_index(&self, _point: NSPoint) -> NSUInteger {
            self.ivars().text.borrow().selected_range.location
        }
    }

    unsafe impl NSObjectProtocol for MacOsInputView {}

    unsafe impl NSDraggingDestination for MacOsInputView {
        #[unsafe(method(draggingEntered:))]
        fn dragging_entered(
            &self,
            sender: &ProtocolObject<dyn NSDraggingInfo>,
        ) -> NSDragOperation {
            let paths = file_drag_paths(sender);
            if paths.is_empty() {
                return NSDragOperation::None;
            }
            let point = self.convertPoint_fromView(sender.draggingLocation(), None);
            self.publish(NativeInputKind::FileDragEntered {
                x_milli: milli_i32(point.x),
                y_milli: milli_i32(point.y),
                paths,
            });
            NSDragOperation::Copy
        }

        #[unsafe(method(draggingUpdated:))]
        fn dragging_updated(
            &self,
            sender: &ProtocolObject<dyn NSDraggingInfo>,
        ) -> NSDragOperation {
            let point = self.convertPoint_fromView(sender.draggingLocation(), None);
            self.publish(NativeInputKind::FileDragMoved {
                x_milli: milli_i32(point.x),
                y_milli: milli_i32(point.y),
            });
            NSDragOperation::Copy
        }

        #[unsafe(method(draggingExited:))]
        fn dragging_exited(&self, _sender: Option<&ProtocolObject<dyn NSDraggingInfo>>) {
            self.publish(NativeInputKind::FileDragLeft);
        }

        #[unsafe(method(prepareForDragOperation:))]
        fn prepare_for_drag_operation(
            &self,
            sender: &ProtocolObject<dyn NSDraggingInfo>,
        ) -> bool {
            !file_drag_paths(sender).is_empty()
        }

        #[unsafe(method(performDragOperation:))]
        fn perform_drag_operation(
            &self,
            sender: &ProtocolObject<dyn NSDraggingInfo>,
        ) -> bool {
            let paths = file_drag_paths(sender);
            if paths.is_empty() {
                false
            } else {
                let point = self.convertPoint_fromView(sender.draggingLocation(), None);
                self.publish(NativeInputKind::FileDropped {
                    x_milli: milli_i32(point.x),
                    y_milli: milli_i32(point.y),
                    paths,
                });
                true
            }
        }
    }
);

impl MacOsInputView {
    fn new(
        mtm: MainThreadMarker,
        frame: NSRect,
        window: WindowHandle,
        view: ViewHandle,
        channel: NativeInputChannel,
        clock: Arc<NativeClock>,
    ) -> Retained<Self> {
        let this = mtm.alloc().set_ivars(InputViewIvars {
            window,
            view,
            channel,
            clock,
            text: RefCell::new(TextInputState::default()),
        });
        // SAFETY: NSView supports initWithFrame: and the object was allocated
        // on the process main thread.
        unsafe { msg_send![super(this), initWithFrame: frame] }
    }

    fn publish(&self, kind: NativeInputKind) {
        let ivars = self.ivars();
        let _ = ivars
            .channel
            .publish(ivars.clock.now_micros(), ivars.window, ivars.view, kind);
    }

    fn publish_key(&self, event: &NSEvent, pressed: bool) {
        let logical_key = event
            .charactersIgnoringModifiers()
            .map(|value| value.to_string())
            .unwrap_or_default();
        self.publish(NativeInputKind::Key {
            device: 1,
            physical_key: u32::from(event.keyCode()),
            logical_key,
            pressed,
            repeat: pressed && event.isARepeat(),
            modifiers: modifiers(event.modifierFlags()),
        });
    }

    fn publish_pointer_move(&self, event: &NSEvent) {
        let point = self.convertPoint_fromView(event.locationInWindow(), None);
        // SAFETY: CGFloat-returning NSEvent selectors are available on macOS.
        let delta_x: f64 = unsafe { msg_send![event, deltaX] };
        let delta_y: f64 = unsafe { msg_send![event, deltaY] };
        self.publish(NativeInputKind::PointerMoved {
            device: 1,
            x_milli: milli_i32(point.x),
            y_milli: milli_i32(point.y),
            delta_x_milli: milli_i32(delta_x),
            delta_y_milli: milli_i32(delta_y),
            pressure_milli: unit_milli(event.pressure()),
        });
    }

    fn publish_pointer_button(&self, event: &NSEvent, pressed: bool, button: NativePointerButton) {
        let point = self.convertPoint_fromView(event.locationInWindow(), None);
        self.publish(NativeInputKind::PointerButton {
            device: 1,
            button,
            pressed,
            click_count: u16::try_from(event.clickCount()).unwrap_or(u16::MAX),
            x_milli: milli_i32(point.x),
            y_milli: milli_i32(point.y),
        });
    }

    fn set_caret_rect(&self, rect: NSRect, selected_range: NSRange) {
        let mut state = self.ivars().text.borrow_mut();
        state.caret_rect = rect;
        state.selected_range = selected_range;
        drop(state);
        if let Some(context) = self.inputContext() {
            context.invalidateCharacterCoordinates();
        }
    }

    fn cancel_ime(&self) {
        if let Some(context) = self.inputContext() {
            context.discardMarkedText();
        }
        let mut state = self.ivars().text.borrow_mut();
        let had_marked = !state.marked.is_empty();
        state.marked.clear();
        state.marked_range = not_found_range();
        drop(state);
        if had_marked {
            self.publish(NativeInputKind::ImeCancelled);
        }
    }
}

struct WindowDelegateIvars {
    window: WindowHandle,
    view: ViewHandle,
    channel: NativeInputChannel,
    clock: Arc<NativeClock>,
}

define_class!(
    #[unsafe(super(NSObject))]
    #[name = "VoAppNativeWindowDelegate"]
    #[thread_kind = MainThreadOnly]
    #[ivars = WindowDelegateIvars]
    struct MacOsWindowDelegate;

    unsafe impl NSObjectProtocol for MacOsWindowDelegate {}

    unsafe impl NSWindowDelegate for MacOsWindowDelegate {
        #[unsafe(method(windowDidResize:))]
        fn window_did_resize(&self, notification: &NSNotification) {
            self.publish_metrics(notification);
        }

        #[unsafe(method(windowDidChangeBackingProperties:))]
        fn window_did_change_backing(&self, notification: &NSNotification) {
            self.publish_metrics(notification);
        }

        #[unsafe(method(windowDidBecomeKey:))]
        fn window_did_become_key(&self, _notification: &NSNotification) {
            self.publish(NativeInputKind::FocusChanged(true));
        }

        #[unsafe(method(windowDidResignKey:))]
        fn window_did_resign_key(&self, _notification: &NSNotification) {
            self.publish(NativeInputKind::FocusChanged(false));
        }

        #[unsafe(method(windowDidMiniaturize:))]
        fn window_did_miniaturize(&self, _notification: &NSNotification) {
            self.publish(NativeInputKind::VisibilityChanged(false));
        }

        #[unsafe(method(windowDidDeminiaturize:))]
        fn window_did_deminiaturize(&self, _notification: &NSNotification) {
            self.publish(NativeInputKind::VisibilityChanged(true));
        }

        #[unsafe(method(windowWillClose:))]
        fn window_will_close(&self, _notification: &NSNotification) {
            self.publish(NativeInputKind::CloseRequested);
        }
    }
);

impl MacOsWindowDelegate {
    fn new(
        mtm: MainThreadMarker,
        window: WindowHandle,
        view: ViewHandle,
        channel: NativeInputChannel,
        clock: Arc<NativeClock>,
    ) -> Retained<Self> {
        let this = mtm.alloc().set_ivars(WindowDelegateIvars {
            window,
            view,
            channel,
            clock,
        });
        // SAFETY: NSObject has a valid init method.
        unsafe { msg_send![super(this), init] }
    }

    fn publish(&self, kind: NativeInputKind) {
        let ivars = self.ivars();
        let _ = ivars
            .channel
            .publish(ivars.clock.now_micros(), ivars.window, ivars.view, kind);
    }

    fn publish_metrics(&self, notification: &NSNotification) {
        let Some(object) = notification.object() else {
            return;
        };
        let Some(window) = object.downcast_ref::<NSWindow>() else {
            return;
        };
        let Some(content) = window.contentView() else {
            return;
        };
        let bounds = content.bounds();
        self.publish(NativeInputKind::Resized {
            width_milli: milli_u32(bounds.size.width),
            height_milli: milli_u32(bounds.size.height),
            scale_milli: milli_u32(window.backingScaleFactor()),
        });
    }
}

pub struct MacOsGpuWindow {
    window_handle: WindowHandle,
    view_handle: ViewHandle,
    window: Retained<NSWindow>,
    view: Retained<MacOsInputView>,
    delegate: Retained<MacOsWindowDelegate>,
    metal_layer: Retained<AnyObject>,
    input: NativeInputReceiver,
    closed: bool,
}

impl MacOsGpuWindow {
    pub fn new(
        window_handle: WindowHandle,
        view_handle: ViewHandle,
        config: MacOsGpuWindowConfig,
    ) -> Result<Self, NativeHostInputError> {
        if !window_handle.is_valid()
            || !view_handle.is_valid()
            || !valid_size(config.width_points, config.height_points)
            || !valid_size(config.min_width_points, config.min_height_points)
        {
            return Err(NativeHostInputError::InvalidConfig);
        }
        let mtm = MainThreadMarker::new().ok_or(NativeHostInputError::InvalidOwner)?;
        let (channel, input) = NativeInputChannel::bounded(config.input)?;
        let clock = Arc::new(NativeClock {
            origin: Instant::now(),
        });
        let frame = NSRect::new(
            NSPoint::new(0.0, 0.0),
            NSSize::new(config.width_points, config.height_points),
        );
        let view = MacOsInputView::new(
            mtm,
            frame,
            window_handle,
            view_handle,
            channel.clone(),
            Arc::clone(&clock),
        );
        view.setWantsLayer(true);
        // SAFETY: AppKit initializes this immutable process-global pasteboard
        // type before application code runs.
        let file_url_type = unsafe { NSPasteboardTypeFileURL };
        view.registerForDraggedTypes(&NSArray::from_slice(&[file_url_type]));
        // SAFETY: CAMetalLayer is provided by QuartzCore on every supported
        // macOS target. The returned Objective-C object is retained.
        let metal_layer: Retained<AnyObject> = unsafe { msg_send![class!(CAMetalLayer), new] };
        // SAFETY: NSView's layer property accepts a CALayer; CAMetalLayer is a
        // concrete CALayer subclass.
        unsafe {
            let _: () = msg_send![&*view, setLayer: &*metal_layer];
        }
        let style = NSWindowStyleMask::Titled
            | NSWindowStyleMask::Closable
            | NSWindowStyleMask::Miniaturizable
            | NSWindowStyleMask::Resizable;
        // SAFETY: The frame and style mask are valid and this call executes on
        // the process main thread.
        let window = unsafe {
            NSWindow::initWithContentRect_styleMask_backing_defer(
                NSWindow::alloc(mtm),
                frame,
                style,
                NSBackingStoreType::Buffered,
                false,
            )
        };
        // SAFETY: The window is retained by this owner and must not release
        // itself during App Runtime's explicit close sequence.
        unsafe {
            window.setReleasedWhenClosed(false);
        }
        window.setAnimationBehavior(NSWindowAnimationBehavior::None);
        window.setTitle(&NSString::from_str(&config.title));
        window.setContentView(Some(&view));
        window.setAcceptsMouseMovedEvents(true);
        window.setContentMinSize(NSSize::new(
            config.min_width_points,
            config.min_height_points,
        ));
        let delegate =
            MacOsWindowDelegate::new(mtm, window_handle, view_handle, channel, Arc::clone(&clock));
        window.setDelegate(Some(ProtocolObject::<dyn NSWindowDelegate>::from_ref(
            &*delegate,
        )));
        view.setAccessibilityElement(true);
        Ok(Self {
            window_handle,
            view_handle,
            window,
            view,
            delegate,
            metal_layer,
            input,
            closed: false,
        })
    }

    pub const fn window_handle(&self) -> WindowHandle {
        self.window_handle
    }

    pub const fn view_handle(&self) -> ViewHandle {
        self.view_handle
    }

    pub fn show(&self) {
        self.window.center();
        self.window.makeKeyAndOrderFront(None);
        self.window.makeFirstResponder(Some(&self.view));
    }

    pub fn hide(&self) {
        self.window.orderOut(None);
    }

    pub fn resize_content(
        &self,
        width_points: f64,
        height_points: f64,
    ) -> Result<MacOsViewMetrics, NativeHostInputError> {
        if !valid_size(width_points, height_points) {
            return Err(NativeHostInputError::InvalidConfig);
        }
        self.window
            .setContentSize(NSSize::new(width_points, height_points));
        Ok(self.metrics())
    }

    pub fn minimize(&self) {
        self.window.performMiniaturize(None);
    }

    pub fn restore(&self) {
        self.window.deminiaturize(None);
    }

    pub fn is_minimized(&self) -> bool {
        self.window.isMiniaturized()
    }

    pub fn pump_events(&self, max_events: usize) -> usize {
        let Some(mtm) = MainThreadMarker::new() else {
            return 0;
        };
        let app = NSApplication::sharedApplication(mtm);
        NSRunLoop::currentRunLoop().runUntilDate(&NSDate::dateWithTimeIntervalSinceNow(0.01));
        let expiration = NSDate::distantPast();
        let mut processed = 0;
        while processed < max_events {
            let Some(event) = app.nextEventMatchingMask_untilDate_inMode_dequeue(
                NSEventMask::Any,
                Some(&expiration),
                // SAFETY: Foundation initializes this immutable global run-loop mode.
                unsafe { NSDefaultRunLoopMode },
                true,
            ) else {
                break;
            };
            app.sendEvent(&event);
            processed += 1;
        }
        NSRunLoop::currentRunLoop().runUntilDate(&NSDate::dateWithTimeIntervalSinceNow(0.01));
        processed
    }

    pub fn metrics(&self) -> MacOsViewMetrics {
        let bounds = self.view.bounds();
        MacOsViewMetrics {
            width_points: bounds.size.width,
            height_points: bounds.size.height,
            scale_factor: self.window.backingScaleFactor(),
            visible: self.window.isVisible(),
        }
    }

    pub fn drain_input(
        &self,
        max_events: usize,
    ) -> Result<Vec<crate::NativeInputEvent>, NativeHostInputError> {
        self.input.drain(max_events)
    }

    pub fn input_receiver(&self) -> &NativeInputReceiver {
        &self.input
    }

    pub fn set_ime_caret_rect(
        &self,
        x_points: f64,
        y_points: f64,
        width_points: f64,
        height_points: f64,
        selection_start_utf16: usize,
        selection_len_utf16: usize,
    ) -> Result<(), NativeHostInputError> {
        if !x_points.is_finite()
            || !y_points.is_finite()
            || !valid_size(width_points, height_points)
        {
            return Err(NativeHostInputError::InvalidConfig);
        }
        self.view.set_caret_rect(
            NSRect::new(
                NSPoint::new(x_points, y_points),
                NSSize::new(width_points, height_points),
            ),
            NSRange::new(selection_start_utf16, selection_len_utf16),
        );
        Ok(())
    }

    pub fn cancel_ime(&self) {
        self.view.cancel_ime();
    }

    pub fn accessibility_host(&self) -> Retained<AnyObject> {
        self.view.clone().into_super().into_super().into()
    }

    /// Raw NSView pointer accepted by AccessKit's NSAccessibility adapter.
    /// It remains valid until this window owner is dropped.
    pub fn accessibility_view_ptr(&self) -> *mut c_void {
        Retained::as_ptr(&self.view).cast_mut().cast()
    }

    /// Pointer accepted by `wgpu::SurfaceTargetUnsafe::CoreAnimationLayer`.
    ///
    /// It remains valid until this owner is dropped.
    pub fn core_animation_layer_ptr(&self) -> *mut c_void {
        Retained::as_ptr(&self.metal_layer).cast_mut().cast()
    }

    #[cfg(feature = "native-wgpu")]
    pub fn create_wgpu_surface<'window>(
        &'window self,
        instance: &wgpu::Instance,
    ) -> Result<wgpu::Surface<'window>, wgpu::CreateSurfaceError> {
        // SAFETY: The CAMetalLayer is retained by this window owner and the
        // returned Surface lifetime is tied to the same owner borrow.
        unsafe {
            instance.create_surface_unsafe(wgpu::SurfaceTargetUnsafe::CoreAnimationLayer(
                self.core_animation_layer_ptr(),
            ))
        }
    }

    pub fn close(&mut self) {
        if self.closed {
            return;
        }
        self.view.cancel_ime();
        self.window.setDelegate(None);
        self.window.orderOut(None);
        self.window.close();
        self.closed = true;
    }
}

fn file_drag_paths(sender: &ProtocolObject<dyn NSDraggingInfo>) -> Vec<String> {
    let pasteboard = sender.draggingPasteboard();
    let classes = NSArray::from_slice(&[NSURL::class()]);
    // SAFETY: NSURL conforms to NSPasteboardReading and no untyped options are
    // supplied. Every returned object is checked before it is used.
    let Some(objects) = (unsafe { pasteboard.readObjectsForClasses_options(&classes, None) })
    else {
        return Vec::new();
    };
    objects
        .iter()
        .filter_map(|object| {
            let url = object.downcast_ref::<NSURL>()?;
            url.isFileURL()
                .then(|| url.path().map(|path| path.to_string()))
                .flatten()
        })
        .collect()
}

impl Drop for MacOsGpuWindow {
    fn drop(&mut self) {
        self.close();
        let _ = &self.delegate;
    }
}

fn input_string(object: &AnyObject) -> String {
    if let Some(string) = object.downcast_ref::<NSString>() {
        string.to_string()
    } else if let Some(attributed) = object.downcast_ref::<NSAttributedString>() {
        attributed.string().to_string()
    } else {
        String::new()
    }
}

fn modifiers(flags: NSEventModifierFlags) -> NativeModifiers {
    NativeModifiers {
        shift: flags.contains(NSEventModifierFlags::Shift),
        control: flags.contains(NSEventModifierFlags::Control),
        alt: flags.contains(NSEventModifierFlags::Option),
        meta: flags.contains(NSEventModifierFlags::Command),
        caps_lock: flags.contains(NSEventModifierFlags::CapsLock),
        function: flags.contains(NSEventModifierFlags::Function),
    }
}

fn pointer_button(number: isize) -> NativePointerButton {
    match number {
        0 => NativePointerButton::Primary,
        1 => NativePointerButton::Secondary,
        2 => NativePointerButton::Middle,
        other => NativePointerButton::Auxiliary(u16::try_from(other).unwrap_or(u16::MAX)),
    }
}

fn replacement_selection(range: NSRange, inserted_utf16: usize) -> NSRange {
    if range.location == usize::MAX {
        NSRange::new(inserted_utf16, 0)
    } else {
        NSRange::new(range.location.saturating_add(inserted_utf16), 0)
    }
}

const fn not_found_range() -> NSRange {
    NSRange::new(usize::MAX, 0)
}

fn valid_size(width: f64, height: f64) -> bool {
    width.is_finite() && height.is_finite() && width > 0.0 && height > 0.0
}

fn milli_i32(value: f64) -> i32 {
    let scaled = value * 1_000.0;
    if scaled.is_nan() {
        0
    } else if scaled <= f64::from(i32::MIN) {
        i32::MIN
    } else if scaled >= f64::from(i32::MAX) {
        i32::MAX
    } else {
        scaled.round() as i32
    }
}

fn milli_u32(value: f64) -> u32 {
    let scaled = value * 1_000.0;
    if !scaled.is_finite() || scaled <= 0.0 {
        0
    } else if scaled >= f64::from(u32::MAX) {
        u32::MAX
    } else {
        scaled.round() as u32
    }
}

fn unit_milli(value: f32) -> u16 {
    let scaled = f64::from(value.clamp(0.0, 1.0)) * 1_000.0;
    scaled.round() as u16
}
