# Cached resources

This example exercises the bounded pure-Volang resource cache. Two reads of the
same key join one loader goroutine. Switching keys cancels the previous context,
and successful completion wakes one coalesced UI render turn.
