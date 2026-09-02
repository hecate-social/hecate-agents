---
title: "FAQ: Sharing Content in PHP"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Share Content Over the Mesh in PHP?

[Back to FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

`$session->contentPut(string $data, string $name): string` /
`$session->contentGet(string $mcid): string` (both the raw 34-byte MCID
as a binary string) — `macula-php/src/Session.php`. Real example
`examples/04_content.php`; the README's own documented output:
`put chunked: mcid=...  size=536633  chunked round trip OK`.
Direct-dial variants (`getDirect`/`putDirect`) also present via FFI.

```php
$mcid = $session->contentPut($data, 'greeting.txt');
$fetched = $session->contentGet($mcid);
```

## See also

- [FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) — the MCID model, `macula-cli`, and the cross-station gotcha
- [FAQ: Developing Edge Services in PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md) — the base PHP SDK this builds on
- [FAQ: Sharing Content in Erlang/Elixir/Gleam](FAQ_SHARE_CONTENT_BEAM.md) / [Go](FAQ_SHARE_CONTENT_GO.md) / [Rust](FAQ_SHARE_CONTENT_RUST.md) / [C#/F# (.NET)](FAQ_SHARE_CONTENT_DOTNET.md)
