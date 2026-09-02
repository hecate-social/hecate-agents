---
title: "FAQ: Sharing Content in C#/F# (.NET)"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Share Content Over the Mesh in C# or F#?

[Back to FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

## C#

Real, complete, runnable example, `macula-dotnet/examples/04_Content.cs`:

```csharp
var small = "hello from macula-dotnet"u8.ToArray();
var smallMcid = await ContentTransfer.PutAsync(session, small, "greeting.txt", identity);
// mcid={hex}, chunked=False

var fetchedSmall = await ContentTransfer.GetAsync(session, smallMcid, identity);

// Above the 256 KiB chunk size: uploaded sequentially over one
// dedicated stream, then a manifest.
var big = new byte[ManifestBuilder.DefaultChunkSize * 2 + 12_345];
var bigMcid = await ContentTransfer.PutAsync(session, big, "random-blob.bin", identity);
// chunked=True
```

## F#

Mirrors the C# exactly (`examples-fsharp/04_Content.fs`):

```fsharp
let small = Encoding.UTF8.GetBytes "hello from macula-dotnet (F#)"
let! smallMcid = ContentTransfer.PutAsync(session, small, "greeting.txt", identity)
let! fetchedSmall = ContentTransfer.GetAsync(session, smallMcid, identity)

let big = Array.zeroCreate<byte> (ManifestBuilder.DefaultChunkSize * 2 + 12_345)
let! bigMcid = ContentTransfer.PutAsync(session, big, "random-blob.bin", identity)
let! fetchedBig = ContentTransfer.GetAsync(session, bigMcid, identity)
```

## See also

- [FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) — the MCID model, `macula-cli`, and the cross-station gotcha
- [FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md) — the base .NET SDK this builds on
- [FAQ: Sharing Content in Erlang/Elixir/Gleam](FAQ_SHARE_CONTENT_BEAM.md) / [Go](FAQ_SHARE_CONTENT_GO.md) / [Rust](FAQ_SHARE_CONTENT_RUST.md) / [PHP](FAQ_SHARE_CONTENT_PHP.md)
