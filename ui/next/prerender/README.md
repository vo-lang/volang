# Static page rendering

`prerender.Run` is an optional native entry adapter. Share the initial view with
the browser entry through an ordinary application package:

```vo
package main

import (
    "your.module/app"
    "github.com/vo-lang/ui/next/prerender"
)

func main() {
    prerender.Run(app.View)
}
```

`app.View` has the signature `func(string) ui.View`. The browser uses
`host.RunWithData(app.View)` and development uses `develop.RunWithData(app.View)`.
The starter wires `services.initialData` to the document's inert JSON string.
An application may parse that string into its own typed data model.

The native adapter reads at most 1 MiB of valid UTF-8 from stdin, renders an
isolated root through `host.HTML`, and prints only that HTML. It never commits
client effects. Write diagnostics to stderr so stdout stays a valid root
document fragment. Invalid data and rendering failures terminate the page build.

The project builder compiles this entry once, executes a fresh process per page,
and embeds the same initial data for the browser. Each process has a 30-second
deadline and 16 MiB output limit. At most 256 pages and 64 MiB of generated
document HTML are admitted. Build cancellation terminates the active process;
any failure leaves the previous complete distribution in place. Native bytecode
is temporary and is excluded from the deployed static assets.

See the [application workflow](../README.md#create-an-independent-application)
for `prerenderPages`, page titles/descriptions, document markers and deployment. This adapter provides
build-time pages. Request-time servers and automatic server data loading remain
separate work; data embedded in these static documents is public page content.
