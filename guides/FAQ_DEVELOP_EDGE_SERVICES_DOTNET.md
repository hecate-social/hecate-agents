---
title: "FAQ: Developing Macula Edge Services in C#/F# (.NET)"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Develop Macula Edge Services in C#, F#, or Other .NET Languages?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

For BEAM-native services (Erlang/Elixir/Gleam), see
[FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
instead. This is one of four "leaf client" SDKs — see also
[Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md),
[Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md), and
[PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md). All four connect to a station and
call/serve/publish/subscribe, but don't participate in DHT routing or
station-to-station gossip themselves — that scope boundary is deliberate
and shared across all four, not a gap in this one.

`macula-dotnet` ships a real, runnable examples directory (both C# and
F#) verified against a live station — every snippet below is quoted from
those, not invented.

---

## A gotcha shared by every leaf-client SDK

**Don't let a `Session` drop immediately after a send-then-return call.**
macula-rust's README documents two once-mysterious failures (a call
timeout, a "serve one call" failure) that both traced to the same root
cause: a `Session` dropped right after `serve_one_call`/`publish`/any
send-then-return call can close the underlying QUIC connection before the
in-flight reply or write actually reaches the wire. Keep the session
alive briefly after the call returns, or close it explicitly once you're
sure the write landed. Worth knowing here too — every leaf-client SDK
shares the same wire protocol and relay behavior.

---

## C# / F# (.NET)

**Version note — repo is ahead of what's published:** `Macula.csproj`
carries `0.2.1`, but NuGet only has up to `0.2.0` at time of writing.
`dotnet add package Macula` gets you `0.2.0`, not whatever's on the
`main` branch — check before assuming a fix you see in the repo is
actually installable yet.

```bash
dotnet add package Macula
```

Requires .NET 10.0+. This is a **full native port**, not an FFI binding —
its own CBOR codec, its own Ed25519/Kademlia identity (via
`BouncyCastle.Cryptography`), its own QUIC transport via
`System.Net.Quic` plus the community `Unofficial.MsQuic` NuGet package
(Microsoft's own official MsQuic package still doesn't ship Linux
natives — this is how the library gets Linux QUIC support anyway).

**C#:**
```csharp
var identity = KeyPair.GenerateWithDefaultPuzzle();
var session = await Session.ConnectAsync(host, port, identity, Trust.UseWebPki);
```

**F# is fully first-class**, not an afterthought — `examples-fsharp/`
mirrors every one of the 11 numbered C# examples 1:1, each live-verified
against the real fleet independently:

```fsharp
// examples-fsharp/01_Handshake.fs
task {
    let identity = KeyPair.GenerateWithDefaultPuzzle()
    let! session = Session.ConnectAsync(host, port, identity, Trust.UseWebPki)
    // ...
}
```

Two F#-specific idioms the README calls out explicitly:
1. Prefer `task { }` over `async { } |> Async.AwaitTask` — some methods
   return `ValueTask`, which `Async.AwaitTask` can't await directly.
2. Required-property spec types (`CallSpec`, `SubscribeSpec`, etc.)
   construct with F#'s named-parens syntax, not C#'s `{ }` initializer
   braces:
   ```fsharp
   SubscribeSpec(Topic = topic, Realm = realm, Subscriber = identity.NodeId())
   ```

**Verify without a live station**: `dotnet test --filter "Category!=Live"`
runs pure logic plus 20 golden byte-exact frame vectors, entirely offline.
`dotnet test --filter "Category=Live"` dials the real fleet.

By design (same scope boundary as every sibling SDK here): no real DHT
peer participation, no station-to-station gossip. Every wire primitive
this library does claim to support is built for both caller and provider
roles, and live-verified.

## See also

- [FAQ: Developing Edge Services in Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md)
- [FAQ: Developing Edge Services in Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md)
- [FAQ: Developing Edge Services in PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md)
- [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
- [FAQ: Connecting Blazor to the Mesh](FAQ_CONNECT_BLAZOR.md) — reasoned from this page's API
- [FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md)
