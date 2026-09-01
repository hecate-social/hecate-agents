---
title: "FAQ: Developing Macula Edge Services in Go"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Develop Macula Edge Services in Go?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

For BEAM-native services (Erlang/Elixir/Gleam), see
[FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
instead. This is one of four "leaf client" SDKs — see also
[Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md),
[C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md), and
[PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md). All four connect to a station and
call/serve/publish/subscribe, but don't participate in DHT routing or
station-to-station gossip themselves — that scope boundary is deliberate
and shared across all four, not a gap in this one.

`macula-go` ships a real, runnable `examples/` directory verified against
a live station — every snippet below is quoted from those, not invented.

---

## A gotcha shared by every leaf-client SDK

**Don't let a `Session` drop immediately after a send-then-return call.**
macula-rust's README documents two once-mysterious failures (a call
timeout, a "serve one call" failure) that both traced to the same root
cause: a `Session` dropped right after `serve_one_call`/`publish`/any
send-then-return call can close the underlying QUIC connection before the
in-flight reply or write actually reaches the wire. Keep the session
alive briefly after the call returns, or close it explicitly once you're
sure the write landed. Go shares the same wire protocol and relay
behavior, so the same rule applies here even though it was first
documented on the Rust side.

Two more limits documented consistently across Go and Rust specifically
(same underlying station code): `ClientStream` mode's reply path is
currently blocked by a real macula-station-side bug, not a client bug —
tests skip with a diagnostic rather than fail; and the demo fleet's
`station_endpoint` DHT records have a short TTL, so a direct-dial resolve
can intermittently fail — retry, it's fleet state, not your code.

---

## Go

**Version**: `v0.3.1` — [`macula-io/macula-go`](https://github.com/macula-io/macula-go)

```bash
go get github.com/macula-io/macula-go@v0.3.1
```

Caller (`Connect` hands back the `*Session` directly — there's no
separate `.Session()` step — and `Call` takes the realm, a deadline, the
signing identity, and a timeout as real required parameters, not just
`(procedure, payload)`):
```go
id, _ := identity.Generate()
session, _ := connection.Connect(ctx, host, port, transport.WebPKI{}, id)

realm := make([]byte, 32) // 32 zero bytes = the "no realm" sentinel
deadlineMs := time.Now().Add(5 * time.Second).UnixMilli()
result, _ := session.Call(procedure, realm, cbor.Text("hello"), deadlineMs, id, 5*time.Second)
```

Provider — `Advertise` takes a built `frame.AdvertiseSpec`, not a bare
procedure string, and neither it nor `ServeOneCall` takes a `ctx`.
There's no dedicated `examples/serve*` script for the provider role; this
is drawn from the SDK's own live test suite
(`connection/serve_forever_live_test.go`), which is the closest thing to
a canonical shape:
```go
lookup := func(_ []byte, procedure string) (connection.CallHandler, bool) {
    h, ok := handlers[procedure] // caller-owned map of registered handlers
    return h, ok
}
session.Advertise(frame.NewAdvertiseSpec(realm, procedure, id.NodeID()), id)
session.ServeOneCall(lookup, id, timeout)
```

See `examples/quickstart` in the repo for the full runnable version of
the caller side.

## See also

- [FAQ: Developing Edge Services in Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md)
- [FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md)
- [FAQ: Developing Edge Services in PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md) — a thin FFI binding over this SDK's compiled C ABI
- [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)
- [FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md)
