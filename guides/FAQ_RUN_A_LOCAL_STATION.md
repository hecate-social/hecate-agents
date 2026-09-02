---
title: "FAQ: Running a Local Station for Dev and Testing"
layer: guide
audience: [agent, human]
stage: draft
---

# FAQ: How Do I Run a Local Station for Dev/Testing Instead of the Public Fleet?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

**No documented local-dev pattern exists anywhere in this workspace today** —
`macula-station`'s README, `docs/DEPLOYMENT_GUIDE.md`, and `scripts/` cover
fleet deployment only; `deployment/` has an unrelated off-grid Raspberry
Pi recipe. None of the four leaf-client SDKs' CI workflows boot a real
station either: their "non-Live" test categories are pure offline
unit/golden-vector tests, and "Live" tests dial the real public fleet
directly. **Everything below is reasoned from real, verified building
blocks — not a tested recipe** — same honesty register as
[FAQ: Connecting Blazor to the Mesh](FAQ_CONNECT_BLAZOR.md). Please
correct this page once someone actually runs it this way.

---

## The building block that makes this simpler than it looks: `Insecure`/`Unsafe` trust

[FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md) §4 documents a real
two-phase cert dance a **production** station needs — a throwaway
placeholder cert, then deriving a real one from the station's own
identity, because `Trust::Pinned` requires the cert's key to match the
station's identity key. For **local dev**, you likely don't need any of
that: three of the four leaf-client SDKs ship a real, documented
`Insecure` trust mode that skips TLS/cert verification entirely, meant
exactly for this:

- Go: `transport.Insecure{}` — doc comment: *"skips TLS verification
  entirely. Dev/lab only."*
- Rust: `Trust::Insecure`
- .NET: `Trust.Unsafe` (`= new Insecure()`) — doc comment: *"Skips TLS
  verification entirely. Dev/lab only... Never use this against a real
  deployment; a machine-in-the-middle can freely intercept traffic
  before the frame-level check ever runs."*
- **PHP has no equivalent** — only `WebPki`-style connect exists.

This is not a theoretical escape hatch: Go's, Rust's, and .NET's own
**direct-dial** implementations use exactly this trust mode internally,
each with a comment to the effect of "the dial itself uses
insecure/unsafe transport because the frame-level identity check already
verifies who we're talking to regardless of TLS." The application-layer
signed-frame check (every CONNECT/HELLO's `node_id` is verified
regardless of TLS trust mode) still applies — you're skipping the
TLS-layer chain/hostname check, not identity verification altogether.

**Practical implication**: for SDK-based local testing, you can leave a
local station on its Phase-1 throwaway self-signed cert *permanently*
and just connect client-side with `Insecure`/`Unsafe` trust — skipping
real-cert derivation entirely. The same warning the SDKs give applies:
never point this at anything but a station you run yourself.

**`macula-cli` has no such flag** — nothing named `insecure` appears in
its source or `guides/HOWTO.md`; the closest related flag
(`--realm-ca`/`--org`) is about direct-dial cert-chain authorization, a
different concern. CLI-based local testing still needs the real
two-phase cert derivation from `FAQ_JOIN_THE_MESH.md` §4, or a
locally-trusted CA. SDK code is the easier path for pure local dev.

## A minimal local config

Reuse `FAQ_JOIN_THE_MESH.md`'s config.json schema with an empty peer
list — already documented there as "boots successfully but is an island
until either a peer dials it, or you add at least one entry," a
genuinely supported configuration, just not previously called out as a
*deliberate* dev pattern:

```json
{
  "data_dir":      "/var/lib/macula/station",
  "identity_file": "/var/lib/macula/station/identity.erl.bin",
  "bind":          "127.0.0.1",
  "port":          4433,
  "certfile":      "/certs/station.crt",
  "keyfile":       "/certs/station.key",
  "outbound_peers": []
}
```

```bash
docker run -d --network host \
  -v ./config.json:/etc/macula-station/config.json:ro \
  -v ./certs:/certs:ro \
  ghcr.io/macula-io/macula-station:main   # no :latest exists yet, see FAQ_JOIN_THE_MESH.md §2
```

Then, from an SDK, connect with `Insecure`/`Unsafe` trust against
`127.0.0.1:4433` instead of the public fleet — no gossip lag, no shared
demo-fleet state to collide with another test run.

## If you actually build this

Things worth verifying that this page cannot answer without a real
build: whether the station genuinely boots and serves cleanly with zero
`outbound_peers` and no DNS at all (nothing found says it shouldn't, but
it's untested here); whether `macula-cli connect` needs a locally-trusted
CA workaround or whether there's a simpler path not yet discovered;
whether a `docker-compose.yml` for exactly this scenario belongs in
`macula-station` itself rather than being hand-assembled per-project.

## See also

- [FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md) — the production two-phase cert dance this page's dev shortcut avoids
- [FAQ: Developing Edge Services in Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md) / [Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md) / [C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md) / [PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md) — where each SDK's real `Trust`/connect API lives
- [FAQ: How do I debug a service that isn't reachable on the mesh?](FAQ_DEBUG_MESH_DISCOVERY.md) — a local single-node station also sidesteps most gossip-propagation debugging entirely
