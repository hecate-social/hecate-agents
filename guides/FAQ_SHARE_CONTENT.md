---
title: "FAQ: Sharing Content (Files/Blobs) Over the Mesh"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Share Content (Files/Blobs) Over the Mesh?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

`content put/get/probe` already get one-line mentions in
[FAQ: How do I run macula-cli?](FAQ_MACULA_CLI.md) and `mesh_put`/`mesh_get`
in [FAQ: How do I run macula-mcp?](FAQ_MACULA_MCP.md) — this is the
dedicated deep-dive, sourced from `macula`'s own
[`docs/guides/content/CONTENT_GUIDE.md`](https://github.com/macula-io/macula/blob/main/docs/guides/content/CONTENT_GUIDE.md).

---

## The model: content-addressed by MCID

An **MCID** is 34 bytes: `<<Version:8, Codec:8, Hash:32/binary>>`.
`Codec` is `0x55` for a single block (content hashed directly with
BLAKE3, ≤256 KiB) or `0x56` for a chunked manifest (BLAKE3 over the
manifest's own metadata — used above 256 KiB, `macula_manifest`'s
default chunk size). Integrity is verified before `get` ever returns
data to you — there's no "got it, but is it corrupt" step to add
yourself.

## Worked examples, one per language

- [Erlang / Elixir / Gleam](FAQ_SHARE_CONTENT_BEAM.md) — the reference implementation: raw primitives, supervised wrappers, direct-dial
- [Go](FAQ_SHARE_CONTENT_GO.md)
- [Rust](FAQ_SHARE_CONTENT_RUST.md)
- [C# / F# (.NET)](FAQ_SHARE_CONTENT_DOTNET.md)
- [PHP](FAQ_SHARE_CONTENT_PHP.md)

## macula-cli

```bash
macula-cli content put --json station-de-frankfurt.macula.io:4433 ./notes.txt
# {"ok": true, "data": {"host": "...", "mcid": "015529...", "size_bytes": 72, "duration_ms": 60}}

macula-cli content get --json --out ./notes-back.txt station-de-frankfurt.macula.io:4433 015529...
# or, without --out, the JSON envelope's "data" carries content_base64 directly

macula-cli content probe station-de-frankfurt.macula.io:4433   # full put/get/verify round trip
```

## The "cross-station reads are unreliable" gotcha, explained

`FAQ_MACULA_MCP.md` already documents this as a known limit
(`mesh_put`/`mesh_get` reliable same-station, best-effort cross-station)
— here's the actual mechanism: `get_content/2` reaches the content via
the connected station's own 1-hop peer relay by default, which is
reliable when both sides are on the same station. Cross-station requires
the `content_announcement` DHT record to have **already propagated**
first; `find_content_providers` returning `[]` means "not yet
replicated," not "doesn't exist" — this is explicit in `CONTENT_GUIDE.md`
itself. Direct-dial's `get_content/3` retries past exactly this lag
instead of depending on gossip having already arrived. Note also: a
single-block put (≤256 KiB) never gets an announcement at all — nothing
to discover later, so a plain same-station or direct-dial get is the
only path for small content regardless.

## See also

- [FAQ: How do I run macula-cli?](FAQ_MACULA_CLI.md) — `content put/get/probe` in the fuller command reference
- [FAQ: How do I run macula-mcp?](FAQ_MACULA_MCP.md) — `mesh_put`/`mesh_get` as MCP tools
- [FAQ: How do I debug a service that isn't reachable on the mesh?](FAQ_DEBUG_MESH_DISCOVERY.md) — the same gossip-propagation-lag root cause shows up for RPC discovery too
