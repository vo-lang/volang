# Headless UIKit behavior

This package contains renderer-neutral interaction and semantic state machines
used by the official UIKit. Applications can replace every visual recipe while
retaining the same keyboard, focus, RTL, portal, and accessibility behavior.

State remains controlled by the caller. A behavior receives the committed
value and emits a typed proposed change; the UI Island remains the only writer.
