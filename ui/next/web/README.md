# Browser requests

This experimental package describes browser work through the existing UI task
owner. Start work from a handler or effect with `ui.Start`, declare reads with
`ui.Resource` or `data.Use`, and submit writes through `forms.Form.Submit` or
`Form.View`. Disposal, replacement and explicit cancellation retire the task;
`WithTimeout` also aborts network work and reports a local error.

```vo
request := web.HTTP("/api/profile", web.HTTPOptions{
    Method: "PATCH",
    Headers: map[string]string{"Content-Type": "application/json"},
    Body: encodedProfile,
}).WithTimeout(5000)

ui.Start(scope, request, func(result ui.Result) {
    response, err := web.DecodeHTTP(result)
    if err != nil {
        // Transport failure, cancellation deadline or response size limit.
        message.Set(err.Error())
        return
    }
    // Decode response.Body into an application struct with encoding/json.
    // response.Status includes 422 and other error statuses; response.OK()
    // identifies 200..299. Field errors can become forms.Outcome.Errors.
    message.Set(response.Body)
})
```

`HTTP` defaults to GET and `Credentials: "same-origin"`. Supported methods are
GET, HEAD, POST, PUT, PATCH, DELETE and OPTIONS. GET and HEAD have no body.
Credentials also accept `omit` and `include`. Browser Fetch owns redirect,
cookie, CORS, forbidden-header and origin behavior; the response reports its
final URL, `Redirected`, readable status and browser-exposed lowercase headers.
See the [Fetch standard](https://fetch.spec.whatwg.org/#fetch-api).

Request bodies are UTF-8 text within 1 MiB; URLs are within 8 KiB. Header names
are case-insensitive HTTP tokens, with at most 128 names and 64 KiB total.
Different casing of the same header in one map is rejected. Names are sorted
and values trimmed/copied when creating the request, giving deterministic
query identities independent of map iteration order or later caller mutation.
Browser restrictions still apply to headers such as Cookie and Content-Length.

The response reader bounds actual decompressed body bytes to 2 MiB and cancels
an oversized stream before it enters the UI event queue. It decodes text using
Fetch's UTF-8 replacement behavior. The worst-case JSON representation still
fits the UI frame limit. Response headers have the same count/byte budget;
HTTP errors preserve their body. `FetchText` uses the same bounded reader and
reports non-2xx statuses as local errors, preserving its convenience behavior.
The 2 MiB reader budget is new in this experimental version.

Use query caching for idempotent reads. Writes should run from events or form
submissions; after success, invalidate affected `data.Client` queries. The
framework does not infer which reads a write changes. Form submissions freeze
their values, retain edits made while saving and attach returned field errors
only to fields that still match the submitted snapshot. A successful submission
advances the dirty baseline to its submitted values.

The native [server package](../server/README.md) provides `server.JSON(status,
value)` for the other end of this contract. Binary/file uploads, streaming
responses and automatic mutation-to-query dependency inference remain outside
this text API.
