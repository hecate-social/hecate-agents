---
title: "FAQ: Developing Macula Edge Services in Erlang, Elixir, Gleam"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Develop Macula/Hecate Edge Services in BEAM Languages?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

For Go/Rust/C#/F#/PHP, see
[Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md),
[Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md),
[C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md), or
[PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md)
instead.

There are two starting points, and which one you want depends on what
you're building:

- **Raw `macula` SDK** ([`macula-io/macula`](https://github.com/macula-io/macula),
  Erlang, 64 modules) — the low-level primitives: connect, publish,
  subscribe, advertise, call. Use this for a standalone tool or a small
  daemon that only needs one or two mesh operations.
- **`hecate_om_service` behaviour** (`hecate-services/hecate-om`) — the
  scaffold-generated shape every production hecate-service in this
  workspace actually uses. Six callbacks (`info/0`, `start/1`, `stop/1`,
  `health/0`, `capabilities/0`, `identity_spec/0`) get you generic
  capability advertisement, TTL management, org-scoped registration, and
  `/health` wiring for free, centrally maintained — see
  [`skills/antipatterns/structure.md`, Demon 59](../skills/antipatterns/structure.md)
  for exactly what goes wrong when a service reinvents this instead. **This
  is the recommended default** for anything meant to run as a real
  hecate-service, not just a script.

Since Erlang, Elixir, and Gleam all run on the same BEAM VM and share the
same module/function calling convention, all three call the *identical*
Erlang SDK modules — there's no separate Elixir or Gleam package, by
design (see this workspace's own "no Elixir wrappers" rule: call Erlang
directly with `:module.function()` / `module:function()` syntax).

---

## Erlang — raw SDK

PubSub (from `macula`'s own `docs/guides/shared/CONNECTING_GUIDE.md`):

```erlang
Seeds = [<<"quic://relay-1.example.com:4433">>, <<"quic://relay-2.example.com:4433">>],
{ok, Pool} = macula:connect(Seeds, #{}),
ok         = macula:publish(Pool, Realm, Topic, Payload),
{ok, _Sub} = macula:subscribe(Pool, Realm, Topic, self()),
ok = macula:close(Pool).
```

Inbound events arrive as messages: `{macula_event, SubRef, Topic, Payload, Meta}`.

RPC provider (from `macula`'s `docs/guides/rpc/RPC_GUIDE.md`):

```erlang
-module(math_service).
-behaviour(macula_response).
-export([init/1, handle_request/2]).

init(_Args) -> {ok, []}.
handle_request(#{<<"a">> := A, <<"b">> := B}, State) ->
    {reply, A + B, State}.
```
```erlang
{ok, _Sup} = macula_response:advertise(Pool, Realm, Procedure, math_service, []).
```

The SDK's own guides cover every primitive beyond this in depth:
`rpc/RPC_PROTOCOL.md`, `pubsub/PUBSUB_GUIDE.md`, `streaming/STREAMING_GUIDE.md`,
`content/CONTENT_GUIDE.md`, `shared/MRI_GUIDE.md`,
`shared/AUTHORIZATION_GUIDE.md`.

**A genuinely minimal real "hello world"**, if the guides above feel too
low-level to start from: [`hecate-social/hecate-stub`](https://github.com/hecate-social/hecate-stub) —
"Connects to a Macula relay, announces geo identity, and serves a health
endpoint. That's it." Real `Dockerfile`, a real `docker run` one-liner
(`MACULA_RELAYS`, `HECATE_MESH_REALM` default `io.macula`,
`HECATE_GEO_CITY`/`COUNTRY`/`LAT`/`LNG`, `HEALTH_PORT` default `8080`), and
a real `rebar3 shell` local-run path. Small enough to read start to finish
in a sitting — a better first read than the full production
`hecate-daemon`.

## Erlang — `hecate_om_service` (the recommended path for a real service)

```bash
rebar3 new hecate_service
```

scaffolds the standard shape. The shortest real, currently-deployed
example — one capability, no pubsub authority — is
`hecate-services/hecate-stations/apps/hecate_stations/src/hecate_stations_service.erl`:

```erlang
-module(hecate_stations_service).
-behaviour(hecate_om_service).

-export([info/0, start/1, stop/1, health/0, capabilities/0, identity_spec/0]).
%% Two more exports beyond the six required callbacks, for its
%% barrel_docdb read model (opt-in, not part of the behaviour itself):
-export([read_model_id/0, data_dir/0]).

info() ->
    #{name => <<"hecate-stations">>,
      version => <<"0.1.0">>,
      description => <<"Live, filterable directory of macula stations: geo, "
                        "health, and direct-dial IP, so clients never "
                        "hand-maintain a station list">>}.

start(_Opts) -> hecate_stations_sup:start_link().
stop(_State) -> ok.
health() -> ok.

capabilities() ->
    [#{name    => <<"hecate_stations.list_stations">>,
       version => 1,
       handler => {list_stations, []}}].

identity_spec() ->
    #{scope => <<"hecate-stations">>, actions => [], resources => [], ttl_days => 30}.

read_model_id() -> <<"hecate_stations">>.
data_dir() -> os:getenv("HECATE_DATA_DIR", "/var/lib/hecate-stations").
```

Declaring `handler` here is the whole story — `hecate_om:boot/1` handles
wiring the mesh pool, publishing the signed `procedure_advertisement` DHT
record, periodic re-advertisement, and TTL, generically, for every service
that uses this path. See
[FAQ: How do I deploy my own hecate service?](FAQ_DEPLOY_HECATE_SERVICES.md)
for what happens after `rebar3 eunit` passes locally.

## Elixir

Real precedent exists across several current Elixir hecate-services in
this workspace (`hecate-services/hecate-whiteboard`, `macula-realm`,
`macula-portal`) — Elixir calls the Erlang SDK's modules directly, exactly
as the "no wrapper" convention prescribes. Two small, complete, real
examples from `hecate-whiteboard`:

Publisher (`guide_board_lifecycle/lib/guide_board_lifecycle/mesh_publisher.ex`,
the full file):
```elixir
defmodule GuideBoardLifecycle.MeshPublisher do
  # Trivial fire-and-forget :macula_publisher callback shared by every
  # mesh-fact emitter in this app -- none of them need to react to the
  # publish outcome, they just want the supervised pid/mesh-fact
  # machinery macula_publisher already provides around a bare
  # macula:publish/4. Mirrors hecate-tube's tube_mesh_publisher.erl.
  @behaviour :macula_publisher

  @impl true
  def init(_args), do: {:ok, nil}

  @impl true
  def handle_published(result, state) do
    require Logger
    Logger.info("[MeshPublisher] outcome: #{inspect(result)}")
    {:stop, :normal, state}
  end
end
```
called as:
```elixir
:macula_publisher.start_link(GuideBoardLifecycle.MeshPublisher, pool, realm, topic, fact, [])
```

Subscriber, started under a `DynamicSupervisor` — this `spec`/`start_child`
call itself is real, but it's only ever reached from inside a ~60-line
retry-loop `GenServer`
(`track_presence/lib/track_presence/peer_departed_mesh_subscriber_starter.ex`)
that waits for `:hecate_om.mesh_handles()` to succeed first, same reason
as the Phoenix LiveView FAQ's "Starting the subscriber" section — a naive
one-shot call here races the mesh pool's async init and loses:
```elixir
spec = %{
  id: TrackPresence.PeerDepartedMeshSubscriber,
  start: {:macula_subscriber, :start_link,
    [TrackPresence.PeerDepartedMeshSubscriber, pool, realm,
     TrackPresence.PeerDepartedMeshSubscriber.topic(), [], %{}]},
  restart: :permanent
}
DynamicSupervisor.start_child(TrackPresence.MeshSubscriberSupervisor, spec)
```

Both patterns repeat throughout these codebases: an Elixir module
implementing `:macula_publisher`/`:macula_subscriber`'s Erlang behaviour
callbacks (`init/1`, `handle_published/2` or the subscriber equivalent),
started via `:module.start_link(...)` with the pool/realm/topic obtained
from `:hecate_om.mesh_handles()`. `macula-energy-mesh-poc` (an older
proof-of-concept) has its own per-app Elixir wrapper module around the
Erlang client — that predates the current no-wrapper convention; treat it
as historical, not a pattern to copy.

## Gleam

**No precedent exists anywhere in this workspace** — no `.gleam` file, no
mention in `macula`'s own docs. The example below is constructed from
Gleam's own documented, stable `@external` FFI attribute for calling raw
Erlang/OTP functions — a standard, well-established Gleam language
feature, not macula-specific — but it has not been run against this SDK.
Verify it before relying on it:

```gleam
// A minimal binding to macula:connect/2 and macula:publish/4.
// Gleam's dynamic type stands in for whatever shape the Erlang side
// actually expects/returns until real usage proves the binding out.
import gleam/dynamic.{type Dynamic}

@external(erlang, "macula", "connect")
pub fn connect(seeds: List(String), opts: Dynamic) -> Result(Dynamic, Dynamic)

@external(erlang, "macula", "publish")
pub fn publish(pool: Dynamic, realm: Dynamic, topic: String, payload: Dynamic) -> Result(Nil, Dynamic)

pub fn main() {
  let assert Ok(pool) = connect(["quic://relay-1.example.com:4433"], dynamic.from(dynamic.nil()))
  publish(pool, dynamic.from(dynamic.nil()), "some.topic", dynamic.from("hello"))
}
```

If you're the first to actually try this, please report back what needed
correcting — this section should stop being a construction from first
principles the moment someone has a real, tested example to replace it
with.

## See also

- [FAQ: Developing Edge Services in Go](FAQ_DEVELOP_EDGE_SERVICES_GO.md)
- [FAQ: Developing Edge Services in Rust](FAQ_DEVELOP_EDGE_SERVICES_RUST.md)
- [FAQ: Developing Edge Services in C#/F# (.NET)](FAQ_DEVELOP_EDGE_SERVICES_DOTNET.md)
- [FAQ: Developing Edge Services in PHP](FAQ_DEVELOP_EDGE_SERVICES_PHP.md)
- [FAQ: How do I deploy my own hecate service?](FAQ_DEPLOY_HECATE_SERVICES.md)
- [`skills/antipatterns/structure.md`, Demon 59](../skills/antipatterns/structure.md) — why `hecate_om_service.capabilities/0` beats hand-rolling mesh advertisement
