# Platform capability contract

Volang UI keeps rendering, state, events, goroutines, and application code
portable. Services owned by an operating system follow an explicit capability
contract: a supported request completes with its typed result; an unavailable
request returns the VUS1 `unsupported` failure. Applications can then keep a
portable UIKit path visible.

| Capability | Web | macOS | Windows | Linux |
| --- | --- | --- | --- | --- |
| DOM/native retained rendering | DOM | WGPU | WGPU | WGPU |
| Clipboard text/HTML/image | Browser permission dependent | Yes | Yes | Desktop/session dependent |
| File and message dialogs | Browser API dependent | Yes | Yes | Portal/desktop dependent |
| File-drop destination | Yes | Yes | Yes | Yes |
| Native file-drag source | Typed unsupported | Yes | Yes | Typed unsupported |
| Conventional OS application menu | Typed unsupported | Yes | Yes | Typed unsupported |
| Portable `kit.MenuBar` | Yes | Yes | Yes | Yes |
| Accessibility bridge | ARIA | NSAccessibility | UI Automation | AT-SPI |

Linux desktop environments do not expose one universal application-menu
facility. The official contract therefore uses `kit.MenuBar` as the Linux menu
surface. The current stable winit event boundary accepts file drops on Linux
and has no source-drag entry point; `system.BeginFileDrag` reports
`unsupported`, allowing an application to offer copy, download, or file-dialog
actions from the same command.

Capability failures never invalidate the mounted UI revision. VUS1 completes
only the requesting goroutine, while the UI Island and other resource
goroutines keep processing input and rendering.
