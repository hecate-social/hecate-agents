---
title: "FAQ: Developing Macula Edge Services in Rust"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Develop Macula Edge Services in Rust?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

For BEAM-native services (Erlang/Elixir/Gleam), see
[FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
instead. This is one of four "leaf client" SDKs — see also
[Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md),
[C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md), and
[PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md). All four connect to a station and
call/serve/publish/subscribe, but don't participate in DHT routing or
station-to-station gossip themselves — that scope boundary is deliberate
and shared across all four, not a gap in this one.

`macula-rust` ships a real, runnable `examples/` directory verified
against a live station — every snippet below is quoted from those, not
invented.

---

## A gotcha shared by every leaf-client SDK

**Don't let a `Session` drop immediately after a send-then-return call.**
macula-rust's own README documents two once-mysterious failures (a call
timeout, a "serve one call" failure) that both traced to the same root
cause: a `Session` dropped right after `serve_one_call`/`publish`/any
send-then-return call can close the underlying QUIC connection before the
in-flight reply or write actually reaches the wire. Keep the session
alive briefly after the call returns, or close it explicitly once you're
sure the write landed. Worth knowing before you hit it in Go, .NET, or
PHP too — they share the same wire protocol and relay behavior.

Two more limits documented consistently across Go and Rust specifically
(same underlying station code): `ClientStream` mode's reply path is
currently blocked by a real macula-station-side bug, not a client bug —
tests skip with a diagnostic rather than fail; and the demo fleet's
`station_endpoint` DHT records have a short TTL, so a direct-dial resolve
can intermittently fail — retry, it's fleet state, not your code.

---

## Rust

**Version**: `0.2.1` — [`macula-io/macula-rust`](https://github.com/macula-io/macula-rust)

**Not published on crates.io** — `cargo add macula-rust` will not work.
Add it as a git dependency:

```toml
macula-rust = { git = "https://github.com/macula-io/macula-rust", tag = "v0.2.1" }
```

`call` takes the realm, a millisecond deadline, the signing identity, and
a timeout as real required parameters too, same as every leaf-client SDK
here — not just `(procedure, payload)`:
```rust
let identity = KeyPair::generate_with_default_puzzle();
let session = connection::connect(host, port, Trust::WebPki, &identity).await?;

let deadline_ms = now_ms + 5_000;
let result = session
    .call("io.macula.echo", [0u8; 32], Value::Text("hello".into()), deadline_ms, &identity, Duration::from_secs(5))
    .await?;
```

Run the real example with `cargo run --example quickstart`. There are
also UniFFI mobile bindings (`macula-rust-ffi` workspace member) if you
need to reach macula from a mobile app — see
[FAQ: Building Mobile Applications for the Mesh](FAQ_BUILD_MOBILE_APPS.md).

## See also

- [FAQ: Developing Edge Services in Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md)
- [FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md)
- [FAQ: Developing Edge Services in PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md)
- [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
- [FAQ: Building Mobile Applications for the Mesh](FAQ_BUILD_MOBILE_APPS.md) — this SDK exposed via UniFFI
- [FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md)
