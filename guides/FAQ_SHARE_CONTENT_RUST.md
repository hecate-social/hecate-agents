---
title: "FAQ: Sharing Content in Rust"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Share Content Over the Mesh in Rust?

[Back to FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

`content::put(session, data, name, identity) -> Result<Mcid, PutError>`,
`content::get(session, mcid, identity) -> Result<Vec<u8>, GetError>` —
`macula-rust/src/content.rs`. No dedicated `examples/` script exists for
content transfer; this is the real call shape from the crate's own live
test suite (`tests/live_station.rs`):

```rust
let mcid = macula_rust::content::put(&mut session, &data, "test-block", &identity)
    .await
    .expect("put should succeed");
let fetched = macula_rust::content::get(&mut session, mcid, &identity)
    .await
    .expect("get should succeed for content this session just put");
```

## See also

- [FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) — the MCID model, `macula-cli`, and the cross-station gotcha
- [FAQ: Developing Edge Services in Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md) — the base Rust SDK this builds on
- [FAQ: Sharing Content in Erlang/Elixir/Gleam](FAQ_SHARE_CONTENT_BEAM.md) / [Go](FAQ_SHARE_CONTENT_GO.md) / [C#/F# (.NET)](FAQ_SHARE_CONTENT_DOTNET.md) / [PHP](FAQ_SHARE_CONTENT_PHP.md)
