---
title: "FAQ: Authorizing with UCAN in C#/F# (.NET)"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Authorize a Procedure with UCAN in C# or F#?

[Back to FAQ: Authorizing with UCAN](FAQ_AUTHORIZE_WITH_UCAN.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

Full runnable example: `macula-dotnet/examples/11_Ucan.cs` (F# mirror:
`examples-fsharp/11_Ucan.fs`).

## C#

```csharp
var token = UcanToken.Create(
    issuer,
    "did:macula:examples.ucan_gated",
    new[] { new UcanToken.Capability("mri:procedure:examples/ucan_gated", "call") },
    callerIdentity);

await providerSession.AdvertiseAsync(new AdvertiseSpec { Realm = realm, Procedure = procedure, Advertiser = providerIdentity.NodeId() });

CallLookup lookup = (_, proc) => proc == procedure ? (payload => Task.FromResult(payload)) : null;
PolicyLookup policy = (_, proc) => proc == procedure ? Policy.Required(callerIdentity.NodeId()) : Policy.Open;

// No token: refused by policy before the handler ever runs.
var unauthorizedTask = providerSession.ServeOneCallGatedAsync(lookup, policy, TimeSpan.FromSeconds(15));
var unauthorized = await callerSession.CallAsync(procedure, realm, Value.Null, deadlineMs, TimeSpan.FromSeconds(10));
await unauthorizedTask;

// Valid token: reaches the handler for real.
var authorizedTask = providerSession.ServeOneCallGatedAsync(lookup, policy, TimeSpan.FromSeconds(15));
var authorized = await callerSession.CallWithUcanAsync(procedure, realm, Value.Text("hello, gated procedure"), deadlineMs, TimeSpan.FromSeconds(10), token);
await authorizedTask;
```

Worth noting from the example's own doc comment: plain
`Session.ServeOneCallAsync` is a zero-behavior-change delegation to
`ServeOneCallGatedAsync` with an always-open policy — ordinary,
ungated procedures are entirely unaffected by UCAN existing in the
library at all.

## F#

A real, complete, independently-maintained mirror, not a thin wrapper
around the C# — same shape, F#'s own idioms (`examples-fsharp/11_Ucan.fs`):

```fsharp
let token =
    UcanToken.Create(
        issuer,
        "did:macula:examples_fsharp.ucan_gated",
        [| UcanToken.Capability("mri:procedure:examples_fsharp/ucan_gated", "call") |],
        callerIdentity)

let lookup =
    CallLookup(fun _realm proc ->
        if proc = procedure then CallHandler(fun payload -> Task.FromResult payload) else null)
let policy =
    PolicyLookup(fun _realm proc ->
        if proc = procedure then Policy.Required(callerIdentity.NodeId()) else Policy.Open)

// First call: no token. Refused by policy before the handler ever runs.
let unauthorizedServeTask = providerSession.ServeOneCallGatedAsync(lookup, policy, TimeSpan.FromSeconds 15.0)
let! unauthorizedResponse = callerSession.CallAsync(procedure, realm, Value.Null, deadlineMs, TimeSpan.FromSeconds 10.0)
do! unauthorizedServeTask

// Second call: valid token. Reaches the handler for real.
let authorizedServeTask = providerSession.ServeOneCallGatedAsync(lookup, policy, TimeSpan.FromSeconds 15.0)
let! authorizedResponse = callerSession.CallWithUcanAsync(procedure, realm, Value.Text "hello, gated procedure", deadlineMs, TimeSpan.FromSeconds 10.0, token)
do! authorizedServeTask
```

## See also

- [FAQ: Authorizing with UCAN](FAQ_AUTHORIZE_WITH_UCAN.md) — the shared model, token format, and `macula-cli` mint/inspect
- [FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md) — the base .NET SDK this builds on
- [FAQ: Authorizing with UCAN in Go](FAQ_AUTHORIZE_WITH_UCAN_GO.md) / [Rust](FAQ_AUTHORIZE_WITH_UCAN_RUST.md) / [PHP](FAQ_AUTHORIZE_WITH_UCAN_PHP.md) / [Erlang/Elixir/Gleam](FAQ_AUTHORIZE_WITH_UCAN_BEAM.md)
