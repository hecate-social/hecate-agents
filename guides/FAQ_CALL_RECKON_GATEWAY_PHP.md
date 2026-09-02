---
title: "FAQ: Calling reckon-gateway from PHP"
layer: guide
audience: [agent, human]
stage: draft
---

# FAQ: How Do I Call reckon-gateway from PHP?

[Back to FAQ: Calling Event Sourcing from a Non-BEAM App](FAQ_CALL_RECKON_GATEWAY.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

**Zero prior art, draft only.**

**Nothing PHP-shaped exists for reckon anywhere** — no client package,
no example, no PHP codegen target in `reckon-proto`. Unlike Rust (at
least a standard tooling path with no local example), this is a
genuinely clean slate. If you build this, the shape would likely be the
PHP `grpc/grpc` extension plus `protoc --php_out` against
`reckon-proto`'s raw `.proto` files — but treat every claim on this page
as reasoned-not-verified, and please replace it with a real recipe once
someone actually builds it.

## See also

- [FAQ: Calling Event Sourcing from a Non-BEAM App](FAQ_CALL_RECKON_GATEWAY.md) — the shared reckon-proto/reckon-gateway contract
- [FAQ: Developing Edge Services in PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md) — the parallel mesh-SDK story for PHP
- [Go](FAQ_CALL_RECKON_GATEWAY_GO.md) / [C#/F# (.NET)](FAQ_CALL_RECKON_GATEWAY_DOTNET.md) / [Rust](FAQ_CALL_RECKON_GATEWAY_RUST.md)
