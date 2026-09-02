---
title: "FAQ: Calling reckon-gateway from Rust"
layer: guide
audience: [agent, human]
stage: draft
---

# FAQ: How Do I Call reckon-gateway from Rust?

[Back to FAQ: Calling Event Sourcing from a Non-BEAM App](FAQ_CALL_RECKON_GATEWAY.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

**Real protocol, but no working example anywhere in this workspace.**

**No `reckon-rust` package exists**, and `reckon-proto`'s own
`buf.gen.yaml` only configures Go codegen — no Rust plugin. A Rust
caller has to run its own `tonic-build`/`prost-build` against
`reckon-proto`'s raw `.proto` files directly. This is a standard,
well-established pattern in the Rust gRPC ecosystem in general — but
there is **zero project-specific precedent** for it anywhere in this
workspace to verify the resulting code against, unlike Go and .NET.
Calibrate confidence accordingly: the wire contract itself
(`AppendEventsRequest`/`DcbService` etc., see the overview page) is real
and verified; the Rust code you'd write to speak it has not been run
here.

```toml
# Cargo.toml — sketch, not verified
[build-dependencies]
tonic-build = "0.12"
[dependencies]
tonic = "0.12"
prost = "0.13"
```

```rust,ignore
// build.rs — sketch, not verified. Point path at a local checkout or
// vendored copy of reckon-proto's proto/ directory.
tonic_build::configure().compile(&["reckon-proto/proto/reckon_streams.proto"], &["reckon-proto/proto"])?;
```

## See also

- [FAQ: Calling Event Sourcing from a Non-BEAM App](FAQ_CALL_RECKON_GATEWAY.md) — the shared reckon-proto/reckon-gateway contract
- [FAQ: Developing Edge Services in Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md) — the parallel mesh-SDK story for Rust
- [Go](FAQ_CALL_RECKON_GATEWAY_GO.md) / [C#/F# (.NET)](FAQ_CALL_RECKON_GATEWAY_DOTNET.md) / [PHP](FAQ_CALL_RECKON_GATEWAY_PHP.md)
