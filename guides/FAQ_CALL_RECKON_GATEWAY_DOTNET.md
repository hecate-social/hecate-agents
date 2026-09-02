---
title: "FAQ: Calling reckon-gateway from C#/F# (.NET)"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Call reckon-gateway from C# or F#?

[Back to FAQ: Calling Event Sourcing from a Non-BEAM App](FAQ_CALL_RECKON_GATEWAY.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

Verified against a real example.

`reckon-dotnet`, package **`Reckon.Client` v0.1.0**, multi-targeting
`net8.0;net9.0;net10.0` — broader framework support than `macula-dotnet`
(net10.0-only). **No F# examples exist for this client** (unlike
`macula-dotnet`'s parallel C#/F# example trees) — this is C#-only real
prior art today. ⚠ Published to **Codeberg's own NuGet feed**
(`codeberg.org/api/packages/reckon-db-org/nuget/index.json`), not
nuget.org — the same GitHub-migration fragility as Go's module path,
arguably sharper here since it's the actual package distribution
channel, not just an import string.

Real, complete example (`examples/QuickStart/Program.cs`) — includes a
genuinely good DCB conflict demonstration:

```csharp
var options = new ReckonClientOptions { Insecure = true }; // plaintext, lab gateway
await using var client = await ReckonClient.ConnectAsync(gateway, options, ct);

var streams = client.Streams(store);
var append = await streams.AppendAsync(streamId, StreamState.NoStream, new[]
{
    new ProposedEvent("user_registered_v1", Encoding.UTF8.GetBytes("""{"name":"Ada"}""")),
}, ct);
// append.Count, append.Version

// --- DCB: cross-stream uniqueness ---
var dcb = client.Dcb(store);
var filter = DcbFilter.MatchAny(tag);
var context = await dcb.ReadAsync(filter, cancellationToken: ct);

var first = await dcb.AppendAsync(filter, context.MaxSeq, new[] {
    new ProposedEvent("slot_reserved_v1", "{}"u8.ToArray(), Tags: new[] { tag }),
}, ct);
// first.IsCommitted == true

var second = await dcb.AppendAsync(filter, DcbClient.NothingObserved, new[] {
    new ProposedEvent("slot_reserved_v1", "{}"u8.ToArray(), Tags: new[] { tag }),
}, ct);
// second.IsCommitted == false -- a real, structured conflict, not an exception
```

## See also

- [FAQ: Calling Event Sourcing from a Non-BEAM App](FAQ_CALL_RECKON_GATEWAY.md) — the shared reckon-proto/reckon-gateway contract
- [FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md) — the parallel mesh-SDK story for .NET
- [Go](FAQ_CALL_RECKON_GATEWAY_GO.md) / [Rust](FAQ_CALL_RECKON_GATEWAY_RUST.md) / [PHP](FAQ_CALL_RECKON_GATEWAY_PHP.md)
