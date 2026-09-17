# Multipart uploads

Import `github.com/vo-lang/ui/next/web/upload` and call
`upload.Multipart(url, data, web.HTTPOptions{})` from a form submit or event handler.
It returns an ordinary scoped `ui.Request`; `WithTimeout` and cancellation keep
their existing behavior. Decode the response with `web.DecodeHTTP`, including
readable 4xx/5xx statuses and application-defined field errors.

`data` is `forms.Data`. Ordered text values and native file selections become one
browser FormData body. Fields are sorted by name, with each field's values/files
in selection order. Empty file selections are omitted. The host uses the actual
native filename, media type and Blob; contents never pass through guest state,
base64 or the UI wire. File references must belong to this root and still be live
when the task enters its provider. Once captured, the upload can outlive the input
under its task's lifetime.

The default method is POST; PUT and PATCH are also supported. Headers and
credentials use the same HTTP policy as `web.HTTP`. Keep `Body` empty and omit
`Content-Type`: Fetch must generate the multipart boundary. One upload admits at
most 4,096 parts, 1 MiB of text/name bytes, 32 files and 128 MiB of total native file
content. Responses retain the HTTP 2 MiB body limit. Native picker `accept` and
file metadata describe the selection; application/server validation owns the
accepted business format.

Choose an endpoint that accepts standard multipart HTTP. The current
`next/server.Request` adapter accepts bounded UTF-8 request bodies and its
`FormValues` helper handles URL-encoded text; multipart server ingestion still
requires an upload-capable endpoint. The executable browser fixture supplies an
independent local HTTP parser and verifies the received text and binary bytes.
Cancelling a client request cannot undo a write already performed by a server.
