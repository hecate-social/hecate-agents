---
title: "FAQ: Calling reckon-gateway from Go"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Call reckon-gateway from Go?

[Back to FAQ: Calling Event Sourcing from a Non-BEAM App](FAQ_CALL_RECKON_GATEWAY.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

Verified against a real example.

`reckon-go` **v0.9.0**. ⚠ Its module path is
`codeberg.org/reckon-db-org/reckon-go` even though the repo's actual git
`origin` is now GitHub — `go get` resolving that import path depends on
the Codeberg mirror staying alive, which this workspace's own migration
notes say is *not* guaranteed. Verify the module still resolves before
depending on it for anything long-lived.

Real, complete example (`examples/streams-demo/main.go` — note this is
the `reckon-go` wrapper client; the gateway README's own inline "Call
from a client" snippet shows a third, lower-level option: raw generated
protobuf stubs, `streampb.NewStreamServiceClient(conn).AppendEvents(...)`,
a different shape from both this and the REST call in the overview page):

```go
c, _ := reckon.Connect(ctx, endpoint, reckon.Insecure()) // lab gateway: plaintext gRPC
defer c.Close()

s := c.Streams(store)
res, _ := s.Append(ctx, stream, streams.AnyVersion, []streams.ProposedEvent{
    {EventType: "demo_started_v1", Data: []byte(`{"n":1}`)},
})
// res.Version, res.Position, res.Count

events, _ := s.Read(ctx, stream, 0, 100)
for _, e := range events {
    // e.Version, e.EventType, e.Data
}
```

DCB: `c.Dcb(store)` with `dcb.MatchAny`/`dcb.MatchAll`/`dcb.And`/`dcb.Or`
filters (`reckon-go/dcb/dcb.go`), mirroring the `TagFilter` shape from
the overview page.

## See also

- [FAQ: Calling Event Sourcing from a Non-BEAM App](FAQ_CALL_RECKON_GATEWAY.md) — the shared reckon-proto/reckon-gateway contract
- [FAQ: Developing Edge Services in Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md) — the parallel mesh-SDK story for Go
- [C#/F# (.NET)](FAQ_CALL_RECKON_GATEWAY_DOTNET.md) / [Rust](FAQ_CALL_RECKON_GATEWAY_RUST.md) / [PHP](FAQ_CALL_RECKON_GATEWAY_PHP.md)
