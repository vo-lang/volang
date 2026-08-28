# Asynchronous search probe

This E2 contract application certifies the public `ui/task` structured
goroutine API. Workers receive cancellation contexts and publish typed mailbox
messages. The UI Island owns every mounted-state write. A slow `alpha` request
is superseded by a fast `beta` request, so cancellation and generation checks
must prevent `alpha` from replacing the visible `beta` result.
