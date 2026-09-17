# Request-time HTML delivery

This optional, native-only package renders a fresh page for each HTTP request.
Application code owns route selection, data loading and response metadata. The
Node adapter owns HTTP, bounded admission, process deadlines and disconnect
cancellation. Each request executes the prepared bytecode in a fresh process;
requests do not share globals, UI roots or data caches. Source compilation happens
at build time.

Add `"serverEntry": "server"` to an existing experimental application's
`ui-next.json`, then create `server/main.vo`:

```vo
package main

import (
    "net/url"
    "local/my-app/app"
    "github.com/vo-lang/ui/next/server"
)

func main() {
    server.Run(func(request server.Request) (server.Page, error) {
        location, err := url.Parse(request.URL)
        if err != nil {
            return server.Page{}, err
        }
        name := location.Query().Get("name")
        if name == "" {
            name = "Visitor"
        }
        return server.Page{
            Render: app.View,
            InitialData: name,
            Title: "Hello, " + name,
            Description: "A page rendered with Volang.",
        }, nil
    })
}
```

Use the project's actual module name in the app import. The shared
`app.View(initial string) ui.View` receives the same initial string in the native
entry and the browser's `host.RunWithData(app.View)`. Encode structured data as
JSON. `Page.Render` executes in a fresh root; client effects start only after
browser activation. The native handler can load data before rendering and encode
successful query results with `data.EncodeInitial`; the shared app decodes those
results into its `data.NewClient` options. Request identity, including the timeout,
must match the client query to avoid a duplicate request.

`Request.URL` is the escaped, origin-relative path and query. `Request.BasePath`
is the deployment prefix, ending with `/`. Headers use lowercase names and
ordered string arrays. The adapter forwards the incoming request directly and
does not infer an origin from proxy headers.

Standard text forms can submit before Wasm loads and with scripts disabled.
Render a native `form` with `method="post"`, an `action` under `Request.BasePath`,
and named controls. Leave the browser's submit default enabled. The handler can
read `request.PostForm()` and return a 303 redirect after accepting the values:

```vo
if request.Method == "POST" {
    fields, err := request.PostForm()
    if err != nil {
        return server.Page{Render: app.View, Status: 400}, nil
    }
    name := fields.Get("name")
    // Validate the submitted fields before applying application changes.
    return server.Page{Status: 303, Headers: map[string][]string{
        "location": {request.BasePath + "profile?name=" + url.QueryEscape(name)},
    }}, nil
}
```

`PostForm` accepts `application/x-www-form-urlencoded`, optionally with
`charset=utf-8`, within 1 MiB and 4,096 field slots. It returns `url.Values` with
ordered repeated fields and empty values. Query parameters remain separate.
Malformed escapes or decoded non-UTF-8 fields return an error with no partial
values. Use a normal page with status 422 and the submitted initial data to show
validation feedback. A successful 303 navigates to GET, so a page refresh does
not submit the form again. These are the browser's [standard form submission](https://developer.mozilla.org/en-US/docs/Web/HTML/Reference/Elements/form)
and [303 redirect](https://developer.mozilla.org/en-US/docs/Web/HTTP/Reference/Status/303) behaviors.

`Request.Body` exposes the original UTF-8 POST body for other text formats; the
application owns their content-type checks and decoding. Compressed bodies,
multipart uploads and binary bodies need a separate adapter. GET and HEAD reject
nonempty or chunked request bodies; static files accept only GET and HEAD.

`Page.Status` defaults to 200 and supports 200–599. Render a normal page with a
404 status for missing routes. Response headers use the same lowercase/array
shape; repeated `set-cookie` values remain separate. A redirect may omit Render
when it has a 3xx status and one nonempty `location`. Statuses 204, 205 and 304
require no Render. Content type and transport/framing headers belong to the
adapter. HTML responses default to `cache-control: no-store`; applications can
explicitly provide another caching policy.

Build and preview using the existing experimental project commands:

```sh
node eng/ui-next/cli.mjs build --project path/to/app
node eng/ui-next/cli.mjs preview --project path/to/app
node eng/ui-next/cli.mjs test --project path/to/app
```

A server build produces `target/ui-next/dist/public/` for browser assets and
`target/ui-next/dist/server/` for the prepared native bytecode, HTML template and
bundled Node entry. Copy the complete distribution into a versioned deployment
directory. With Node 24 and a compatible Volang CLI installed, run:

```sh
node server/entry.mjs
```

`HOST`, `PORT`, `BASE_PATH` and `VO_EXECUTABLE` configure the listener, deployment
prefix and native executable. Defaults are `127.0.0.1`, port 3000, `/` and `vo` on
PATH. To customize admission or deadlines, import the emitted entry's `start`
function and pass `concurrency`, `queued` and `timeoutMilliseconds`. The defaults
are four active requests, sixteen queued requests and a 30-second request
deadline including queue time, upload reading and rendering. The host reads a
body only after admission. Excess requests receive 503; request deadlines
receive 504. Bodies above 1 MiB receive 413; invalid UTF-8 receives 400 and
unsupported content encoding receives 415. Rejected/incomplete uploads close
their connection after the response. Disconnect and application shutdown cancel queued/running work, and
shutdown waits for native processes to exit. Restart the server to adopt a new
build; keep each running deployment directory immutable.

Public files use the build's Brotli/gzip representations when accepted by the
client. Encoding preferences and exclusions follow
[HTTP content negotiation](https://www.rfc-editor.org/rfc/rfc9110.html#section-12.5.3).
`Vary: Accept-Encoding`, representation-specific ETags and Last-Modified support
cache revalidation; matching conditional requests return 304. HEAD reports the
selected representation's length without reading its body. Each file streams
with backpressure and shares the configured request deadline and cancellation
owner, so slow downloads cannot keep a retired development build alive forever.
Abandoning a download releases its file stream. Compression happens during the
build; dynamic native HTML keeps its existing response path and cache policy.

Server request/response protocol 4 adds an explicit client page entry to HTML
responses. JSON and bodyless responses keep that field empty. Upgrade the
experimental UI module and build tooling together and rebuild the complete
distribution, including its private `server/entries.json` manifest. Older native
entries and host bundles cannot be mixed. The DOM wire remains v18.
Requests are bounded to 8 MiB of JSON-encoded input (including body escaping),
with a separate 1 MiB body, 64 KiB header budget and 8 KiB URL. Initial
client data is limited to 1 MiB, native HTML to 16 MiB, JSON bodies to 2 MiB, and the encoded response to
32 MiB. Titles/descriptions and headers have independent limits. Standard output
belongs to the versioned response; application diagnostics must use stderr.

The current adapter serves GET, HEAD, POST, PUT, PATCH, DELETE and OPTIONS.
GET/HEAD reject bodies; other methods share the bounded UTF-8 body reader.
Each application decides which methods its routes accept and supplies 405/Allow
where needed. Static files continue to accept GET and HEAD only. Streaming and
automatic effect-driven prefetch remain separate work. Studio has its own
experimental [production distribution](../../../apps/studio/next/README.md).
Static builds retain their existing layout; `serverEntry` takes
precedence over `prerenderEntry` and cannot be combined with `prerenderPages`.

## JSON endpoints

The same native handler can return `server.JSON(status, value)` with an ordinary
Vo struct, map or other JSON-encodable value. The returned `Page` supports the
existing response `Headers`. The adapter writes `application/json; charset=utf-8`
with `no-store` unless the application supplies its own cache policy. It sends
the encoded JSON directly without a document template or browser assets.

```vo
type Result struct {
    Message string `json:"message"`
}

func handle(request server.Request) (server.Page, error) {
    if request.URL == "/api/profile" && request.Method == "GET" {
        return server.JSON(200, Result{Message: "Ready"})
    }
    return server.Page{Render: app.View}, nil
}
```

Decode a request's `Body` into an application struct with `encoding/json`, using
the request's method and content type to select the route contract. Structured
validation errors can use status 422 and a typed error map. The browser's
[`web.HTTP`](../web/README.md) preserves these statuses and bodies for forms.
JSON responses cannot include a render function, startup data, page metadata or
a bodyless status (204, 205 or 304). HEAD returns the same status and headers
without the body. Every native request keeps its fresh-process isolation;
persistent application data needs an explicitly chosen store.

## Local server development

The normal `dev --project` command recognizes `serverEntry`. It renders requests
through the prepared native handler and serves the development browser entry;
the starter defaults to VM even on deep URLs without a backend parameter.
Shared Vo changes restore compatible UI state. Changes inside the server entry,
configuration or Web bootstrap reload the document to fetch fresh request data.
Stylesheets update without replacing the UI. Adding or removing `serverEntry`
requires restarting development; failures explain that in the existing overlay.

Each successful build publishes a separate revision under a stable local origin.
Source edits received during a build discard that superseded result and rebuild
the latest inputs before publishing a client/server pair.
HTML references assets from its exact revision. In-flight native requests finish
against their previous build; a new request uses the current complete build.
Failed builds and failed installation retain the working revision and show the
compiler diagnostic, including on newly opened pages. A first-build failure
returns a 503 document that reconnects and reloads after the source is fixed.
Standard POST forms use the same native handler during development.

The development host retains eight revisions. Before evicting the oldest it
waits for that revision's active requests to drain within their existing deadline.
All revisions share one admission queue, so rebuilding cannot multiply the
configured request concurrency or waiting budget.
Requests for evicted assets return 410 with a reload diagnostic; missing assets
remain 404 and never invoke the page handler. State/style refreshes explicitly
select the new revision, while ordinary module imports keep the original path.
Closing development cancels remaining work and removes this session's revisions.
The production distribution remains separate and immutable.
