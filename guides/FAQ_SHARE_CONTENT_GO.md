---
title: "FAQ: Sharing Content in Go"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Share Content Over the Mesh in Go?

[Back to FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

`content.Put(ctx, session, data, name, id) (manifest.Mcid, error)`,
`content.Get(ctx, session, mcid, id) ([]byte, error)` —
`macula-go/content/content.go`. No dedicated `examples/` script exists
for content transfer; this is the real call shape from the package's
own live test suite (`content/live_test.go`):

```go
mcid, err := content.Put(ctx, session, data, "test-block", identity)
// ...
fetched, err := content.Get(ctx, session, mcid, identity)
```

## See also

- [FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) — the MCID model, `macula-cli`, and the cross-station gotcha
- [FAQ: Developing Edge Services in Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md) — the base Go SDK this builds on
- [FAQ: Sharing Content in Erlang/Elixir/Gleam](FAQ_SHARE_CONTENT_BEAM.md) / [Rust](FAQ_SHARE_CONTENT_RUST.md) / [C#/F# (.NET)](FAQ_SHARE_CONTENT_DOTNET.md) / [PHP](FAQ_SHARE_CONTENT_PHP.md)
