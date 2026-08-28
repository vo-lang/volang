# Portable UIKit menu

This application uses `kit.MenuBar`, `kit.MenuAction`, and
`kit.MenuToggleAction`. The component tree, keyboard semantics, accessibility
roles, and event handlers are shared by Web and desktop renderers.

Applications may additionally install an operating-system menu with
`ui/system.InstallMenu` on macOS and Windows. Linux applications keep this
UIKit menu visible in the window, which gives every desktop environment the
same reachable commands.
