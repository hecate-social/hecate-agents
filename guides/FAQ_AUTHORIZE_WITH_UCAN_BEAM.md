---
title: "FAQ: Authorizing with UCAN in Erlang, Elixir, and Gleam"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Authorize a Procedure with UCAN in Erlang, Elixir, or Gleam?

[Back to FAQ: Authorizing with UCAN](FAQ_AUTHORIZE_WITH_UCAN.md) ·
[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

Thinner than the four leaf-client SDK pages here — real, but no
complete worked example.

## Erlang

`macula`'s own `AUTHORIZATION_GUIDE.md` documents the gate as two real
options rather than a runnable mint-and-call script:

```erlang
%% Provider: gate the procedure at advertise time.
ok = macula:advertise(Pool, Realm, Procedure, Handler, #{auth => {ucan_required, Issuer}}).

%% Caller: attach a token to the call. TimeoutMs is required and
%% positional BEFORE Opts -- omitting it dispatches to call_station/6
%% instead (no Opts at all), silently making an ungated call.
{ok, Response} = macula:call_station(Pool, Station, Realm, Procedure, Payload, TimeoutMs,
                                      #{ucan_token => Token}).
```

`Issuer` is the same raw 32-byte Ed25519 public key used on every
language's page here — not a DID string. Minting and checking a token
on the BEAM goes through the NIF module directly — this is the reference
implementation every other SDK matches wire-for-wire, and
`AUTHORIZATION_GUIDE.md` documents it as the public surface:

```erlang
{ok, Token}   = macula_ucan_nif:create(IssuerDID, AudienceDID, Capabilities, PrivKey),
{ok, Payload} = macula_ucan_nif:verify(Token, IssuerPubKey),

{ok, Issuer}   = macula_ucan_nif:get_issuer(Token),
{ok, Audience} = macula_ucan_nif:get_audience(Token),
{ok, Caps}     = macula_ucan_nif:get_capabilities(Token),
false          = macula_ucan_nif:is_expired(Token).
```

`macula-cli ucan mint` produces the same token shape if you'd rather not
write Erlang for a one-off test.

## Elixir

Calls these same two functions directly, per this workspace's "no
wrapper" convention:

```elixir
:macula.advertise(pool, realm, procedure, handler, %{auth: {:ucan_required, issuer}})
:macula.call_station(pool, station, realm, procedure, payload, timeout_ms, %{ucan_token: token})
```

See [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
for the general no-wrapper convention this follows.

## Gleam

No real prior art exists for UCAN gating specifically, same as for the
base mesh primitives — see
[FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)'s
own Gleam section for the general `@external` FFI pattern this would
follow (calling `macula:advertise/5`/`macula:call_station/7` exactly as
shown above). This page isn't inventing a separate, UCAN-specific Gleam
example on top of that already-unverified base.

## See also

- [FAQ: Authorizing with UCAN](FAQ_AUTHORIZE_WITH_UCAN.md) — the shared model, token format, and `macula-cli` mint/inspect
- [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md) — the Erlang reference these tokens' encoding matches exactly
- [FAQ: Authorizing with UCAN in Go](FAQ_AUTHORIZE_WITH_UCAN_GO.md) / [Rust](FAQ_AUTHORIZE_WITH_UCAN_RUST.md) / [C#/F# (.NET)](FAQ_AUTHORIZE_WITH_UCAN_DOTNET.md) / [PHP](FAQ_AUTHORIZE_WITH_UCAN_PHP.md)
