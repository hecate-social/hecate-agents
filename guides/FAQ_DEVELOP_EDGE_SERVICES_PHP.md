---
title: "FAQ: Developing Macula Edge Services in PHP"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Develop Macula Edge Services in PHP?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

For BEAM-native services (Erlang/Elixir/Gleam), see
[FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
instead. This is one of four "leaf client" SDKs — see also
[Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md),
[Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md), and
[C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md). All four connect to
a station and call/serve/publish/subscribe, but don't participate in DHT
routing or station-to-station gossip themselves — that scope boundary is
deliberate and shared across all four, not a gap in this one.

`macula-php` ships a real, runnable `examples/` directory verified
against a live station — every snippet below is quoted from those, not
invented.

---

## A gotcha shared by every leaf-client SDK

**Don't let a `Session` drop immediately after a send-then-return call.**
macula-rust's README documents two once-mysterious failures (a call
timeout, a "serve one call" failure) that both traced to the same root
cause: a `Session` dropped right after `serve_one_call`/`publish`/any
send-then-return call can close the underlying QUIC connection before the
in-flight reply or write actually reaches the wire. Keep the session
alive briefly after the call returns, or close it explicitly once you're
sure the write landed. Worth knowing here too — every leaf-client SDK
shares the same wire protocol and relay behavior.

---

## PHP

**Version**: `v0.2.0`, published on Packagist as `macula-io/macula-php` —
[`macula-io/macula-php`](https://github.com/macula-io/macula-php)

This is a thin FFI binding over **macula-go's compiled C ABI**, so
`composer require` alone isn't enough — you also need the native shared
library built first:

```bash
composer require macula-io/macula-php
cd cabi && go build -buildmode=c-shared -o libmacula.so . && cd ..
composer install
```

Needs Go ≥1.25 and a C compiler (cgo) at build time — but you never write
Go yourself; it's purely the build step for the shared library.

**Requirements**: PHP ≥8.1 with `ext-ffi` **and** `ext-sodium`. `ext-ffi`
is often not enabled by default — check with `php -m | grep FFI`. If
missing, PHP needs rebuilding with `--with-ffi` (**not** `--enable-ffi`,
which silently does nothing — the project's own README flags this as
their first real mistake building it).

`call` needs a realm (a 32-byte string — `str_repeat("\x00", 32)` is the
"no realm" sentinel) and the payload wrapped in a `Value` (`Value::text()`,
not a raw scalar), not just `($procedure, $payload)`:
```php
$identity = KeyPair::generate();
$session = Session::connect($host, $port, $identity);

$realm = str_repeat("\x00", 32);
$response = $session->call($procedure, $realm, Value::text('hello'));
```

Run with `php examples/01_handshake.php`.

**One current limitation**: the `Value` type (mirroring macula-go's
`cbor.Value`) only supports `Null`/`Int`/`Bytes`/`Text`/`Float` so far —
no `List`/`Map` yet. A payload that needs structure should be encoded as
`Bytes` for now.

Provider-role examples need **two separate OS processes**, not two
threads in one process (unlike the .NET SDK) — see the "Two-process
pattern for provider-role examples" section of the README, and
`06_run_rpc_provider.sh`/`07_run_stream_provider.sh` for the real
orchestration scripts.

## See also

- [FAQ: Developing Edge Services in Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md) — the C ABI this binding wraps
- [FAQ: Developing Edge Services in Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md)
- [FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md)
- [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
- [FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md)
