---
title: "FAQ: Sharing Content in Erlang, Elixir, and Gleam"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Share Content Over the Mesh in Erlang, Elixir, or Gleam?

[Back to FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

## Erlang — raw primitives and supervised wrappers

Raw:
```erlang
{ok, MCID} = macula:put_content(Pool, Bytes),
{ok, Bytes} = macula:get_content(Pool, MCID).  % or {error, not_found}
```

**Prefer the supervised wrappers for anything beyond a one-shot script**:
`macula_feeder`/`macula_download` (behaviours: `init/1` +
`handle_fed/2`/`handle_downloaded/2`) add a real, peer-visible `cancel/1`
(a QUIC `RESET_STREAM` since 9.11.1, not just "stop reading locally") and
auto-publish `sharing.put_*_v1`/`sharing.get_*_v1` mesh facts so other
peers can observe transfer progress. `macula_pusher`/`macula_upload` push
a file to a known recipient directly instead of content-addressed
discovery (built on `client_stream`) — reach for these when you already
know who should receive the file, not just its hash.

Direct-dial variants skip station-gossip discovery lag entirely:
`macula_direct_dial:get_content(Pool, MCID, TimeoutMs)` and
`put_content(Pool, Station, Bytes, TimeoutMs)`. Discovery itself —
`macula:find_content_providers(Pool, MCID)` — lives on the main `macula`
module, not `macula_direct_dial` (which calls it internally); it
resolves signed `content_announcement` DHT records, signature *and*
signer both verified, not just decoded.

## Elixir

Calls every one of the functions above directly, no wrapper, per this
workspace's own convention:

```elixir
{:ok, mcid} = :macula.put_content(pool, bytes)
{:ok, bytes} = :macula.get_content(pool, mcid)
```

See [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
for the general no-wrapper convention this follows.

## Gleam

No real prior art exists for content sharing specifically — same as the
base primitives — see
[FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)'s
own Gleam section for the general `@external` FFI pattern this would
follow.

## See also

- [FAQ: Sharing Content](FAQ_SHARE_CONTENT.md) — the MCID model, `macula-cli`, and the cross-station gotcha
- [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md) — the base BEAM SDK this builds on
- [FAQ: Sharing Content in Go](FAQ_SHARE_CONTENT_GO.md) / [Rust](FAQ_SHARE_CONTENT_RUST.md) / [C#/F# (.NET)](FAQ_SHARE_CONTENT_DOTNET.md) / [PHP](FAQ_SHARE_CONTENT_PHP.md)
