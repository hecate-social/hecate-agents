---
title: "FAQ: Authorizing a Procedure or Topic with UCAN"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Authorize a Procedure or Topic with UCAN?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

**What's actually gated today, stated plainly by `macula`'s own
[`AUTHORIZATION_GUIDE.md`](https://github.com/macula-io/macula/blob/main/docs/guides/shared/AUTHORIZATION_GUIDE.md):**
the SDK's only *enforced* authorization point is per-procedure, via
`advertise`'s `auth` option — `open` (default: serve any identified
caller; every QUIC session is already Ed25519 peer-bound, so "open" is
not "anonymous") or `{ucan_required, Issuer}` (a caller must present a
UCAN token that verifies against `Issuer`, a raw 32-byte Ed25519 public
key — not a DID string). There is no automatic DID-namespace-ownership
check on publish/subscribe/call otherwise: DIDs, certs, and UCANs are
primitives you build a stronger policy from, not something the SDK
enforces on its own beyond this one gate. (See
[FAQ: Joining a Realm](FAQ_JOIN_A_REALM.md) for the realm-cert side,
which is separate from this per-procedure token gate.)

**The token itself**: JWT-style (header.payload.signature, base64url), a
UCAN spec draft (`0.10.0`) — deliberately *not* the current 1.0/IPLD
spec, since no ready-made library for that exists; every language hand-
rolls the encoding to match `macula_ucan_nif`'s Rust reference exactly.
This makes it **wire-compatible across every SDK**: a token minted in
Go, Rust, .NET, PHP, or the Erlang reference verifies in every other one
— stated explicitly in `macula-cli`'s own `ucan mint --help` text and
confirmed in each SDK's own source.

Gating happens **before** a handler runs — a rejected caller never
reaches business logic, and an accepted caller's handler never even sees
the raw token; the policy layer already did the only thing that
mattered with it.

---

## Minting and inspecting a token with `macula-cli`

Both subcommands are **local-only — no station, no network** (same
shape as `macula-cli identity`):

```bash
# Mint, signed by the local persisted identity. <issuer>/<audience> are
# opaque DID strings, unvalidated. Repeat -capability for more than one.
macula-cli ucan mint -expires-in 1h \
  -capability "mri:procedure:examples/ucan_gated:call" \
  -out token.ucan did:macula:example-issuer did:macula:example-audience

# Decode WITHOUT verifying the signature -- for inspecting claims, never
# for an authorization decision.
macula-cli ucan inspect token.ucan
```

`ucan mint`'s flags: `-identity <path>` (which local key signs it),
`-expires-in <duration>` (`0` = never expires), `-capability with:can`
(repeatable), `-out <file>` (else prints to stdout), `-json`.
`ucan inspect` takes a token file path or `-` for stdin, and prints
issuer/audience/expired/capabilities.

## Worked examples, one per language

Each of these gates a served procedure and shows both the rejected
(no token) and accepted (valid token) paths against the real fleet:

- [Go](FAQ_AUTHORIZE_WITH_UCAN_GO.md)
- [Rust](FAQ_AUTHORIZE_WITH_UCAN_RUST.md)
- [C# / F# (.NET)](FAQ_AUTHORIZE_WITH_UCAN_DOTNET.md)
- [PHP](FAQ_AUTHORIZE_WITH_UCAN_PHP.md)
- [Erlang / Elixir / Gleam](FAQ_AUTHORIZE_WITH_UCAN_BEAM.md) — thinner: the two real options with no complete worked example script

## See also

- [FAQ: Joining a Realm — Getting Your Service an Identity and Certificate](FAQ_JOIN_A_REALM.md) — the separate realm-cert layer this page's per-procedure gate doesn't replace
- [FAQ: How do I run macula-cli?](FAQ_MACULA_CLI.md) — `ucan mint`/`ucan inspect` in the fuller command reference
- [FAQ: Developing Edge Services in Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md) / [Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md) / [C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md) / [PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md) / [BEAM](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md) — the base SDK APIs these UCAN examples build on
