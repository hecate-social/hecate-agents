---
title: "FAQ: Authorizing with UCAN in Rust"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Authorize a Procedure with UCAN in Rust?

[Back to FAQ: Authorizing with UCAN](FAQ_AUTHORIZE_WITH_UCAN.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

Full runnable example: `macula-rust/examples/ucan.rs` —
`cargo run --example ucan`.

```rust
// A real deployment would use a stable, pre-shared authority identity,
// not one minted fresh per run.
let issuer_pub = authority.node_id();

provider.serve_one_call_gated(
    lookup_fn,
    move |_, _| ucan::Policy::required(issuer_pub),
    &provider_id,
    Duration::from_secs(15),
).await?;

// No token: refused before the handler ever runs.
let rejected = caller.call(procedure, realm, Value::Null, 0, &caller_id, Duration::from_secs(5)).await;

// A real token minted by the required authority, then used to call again.
// call_with_ucan takes the token by value (Vec<u8>), not by reference --
// and its own Result is worth inspecting directly rather than `?`-ing it
// away: an authorization rejection comes back as Ok(response) with
// is_error true, not an Err.
let token = ucan::create("did:key:example-issuer", "did:key:example-audience", vec![], &authority, ucan::CreateOpts::default())?;
let granted = caller.call_with_ucan(procedure, realm, Value::Text("...".into()), deadline_ms, &caller_id, Duration::from_secs(5), token).await;
```

The example's own module doc flags the same Session-drop gotcha that's
in every leaf-client SDK FAQ here: keep the provider `Session` alive
briefly after `serve_one_call_gated` returns, since `Session` has no
`Drop` impl and dropping it immediately can close the QUIC connection
before the just-sent reply frame reaches the peer.

## See also

- [FAQ: Authorizing with UCAN](FAQ_AUTHORIZE_WITH_UCAN.md) — the shared model, token format, and `macula-cli` mint/inspect
- [FAQ: Developing Edge Services in Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md) — the base Rust SDK this builds on
- [FAQ: Authorizing with UCAN in Go](FAQ_AUTHORIZE_WITH_UCAN_GO.md) / [C#/F# (.NET)](FAQ_AUTHORIZE_WITH_UCAN_DOTNET.md) / [PHP](FAQ_AUTHORIZE_WITH_UCAN_PHP.md) / [Erlang/Elixir/Gleam](FAQ_AUTHORIZE_WITH_UCAN_BEAM.md)
