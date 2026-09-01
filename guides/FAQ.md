---
title: "FAQ — Running and Building on the Macula Mesh"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ — Running and Building on the Macula Mesh

[Back to corpus index](../INDEX.md)

Practical "how do I..." answers, as opposed to the philosophy and
architecture doctrine that makes up most of this corpus. Every answer here
is a summary that links to the real, authoritative source (a repo's own
README, HOWTO, or source code) rather than a copy that can drift out of
sync with it — when in doubt, follow the link.

| Question | Answer |
|---|---|
| How do I join the Mesh — run my own station? | [`FAQ_JOIN_THE_MESH.md`](FAQ_JOIN_THE_MESH.md) |
| How do I run `macula-cli`? | [`FAQ_MACULA_CLI.md`](FAQ_MACULA_CLI.md) |
| How do I run `macula-mcp`? | [`FAQ_MACULA_MCP.md`](FAQ_MACULA_MCP.md) |
| How do I develop an edge service in Erlang, Elixir, or Gleam? | [`FAQ_DEVELOP_EDGE_SERVICES_BEAM.md`](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md) |
| How do I develop an edge service in Go? | [`FAQ_DEVELOP_EDGE_SERVICES_GO.md`](FAQ_DEVELOP_EDGE_SERVICES_GO.md) |
| How do I develop an edge service in Rust? | [`FAQ_DEVELOP_EDGE_SERVICES_RUST.md`](FAQ_DEVELOP_EDGE_SERVICES_RUST.md) |
| How do I develop an edge service in C# or F# (.NET)? | [`FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md`](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md) |
| How do I develop an edge service in PHP? | [`FAQ_DEVELOP_EDGE_SERVICES_PHP.md`](FAQ_DEVELOP_EDGE_SERVICES_PHP.md) |
| How do I deploy my own hecate service to the fleet? | [`FAQ_DEPLOY_HECATE_SERVICES.md`](FAQ_DEPLOY_HECATE_SERVICES.md) |
| How do I build a mobile app for the mesh? | [`FAQ_BUILD_MOBILE_APPS.md`](FAQ_BUILD_MOBILE_APPS.md) |
| How do I connect a Phoenix LiveView site to the mesh? | [`FAQ_CONNECT_PHOENIX_LIVEVIEW.md`](FAQ_CONNECT_PHOENIX_LIVEVIEW.md) |
| How do I connect a Blazor site to the mesh? | [`FAQ_CONNECT_BLAZOR.md`](FAQ_CONNECT_BLAZOR.md) — ⚠ draft, no prior art exists yet |

## The shape of the whole stack, in order

If you're starting from zero, the first six answer, in sequence: what you
connect *to* (a station), what you connect *with* (the CLI, or an MCP
server built on it), what you write in (any of eight languages across
two families), and where it actually runs once written. The last three
are consumption patterns for specific client shapes — mobile, and two web
UI frameworks — layered on top of the SDKs above rather than a new stage
of the pipeline.

```
FAQ_JOIN_THE_MESH           — the station you connect to
       │
       ├── FAQ_MACULA_CLI            — the reference client
       │        └── FAQ_MACULA_MCP   — an MCP server built on the CLI
       │
       └── FAQ_DEVELOP_EDGE_SERVICES_{BEAM,GO,RUST,DOTNET,PHP}  — write your own peer
                │
                ├── FAQ_DEPLOY_HECATE_SERVICES         — ship it
                │
                └── client-shape patterns, all built on the SDKs above:
                      FAQ_BUILD_MOBILE_APPS            — Android/iOS via macula-rust's UniFFI
                      FAQ_CONNECT_PHOENIX_LIVEVIEW      — real prior art, Erlang/Elixir SDK
                      FAQ_CONNECT_BLAZOR                — no prior art, reasoned from macula-dotnet
```

Railroad terms used throughout, if unfamiliar: `macula` SDK = the
**track** (QUIC/HTTP-3 transport), `macula-station` = the **station**
(DHT/SWIM/routing), `macula-realm` = the **train company** (identity and
certs), a hecate-service or `hecate-daemon` = a **passenger** (outbound-only,
connects out, never accepts inbound connections directly). See
[`GLOSSARY.md`](../GLOSSARY.md) for the full canonical vocabulary.
