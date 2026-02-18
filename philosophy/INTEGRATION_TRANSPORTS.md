# Integration Transports

How umbrella apps communicate within hecate-daemon, with hecate-web, and across the network.

---

## The Three Integration Layers

| Layer | Transport | Scope | Use Case |
|-------|-----------|-------|----------|
| **Internal** | `pg` (OTP process groups) | Same BEAM VM | CMD → PRJ projections, intra-daemon |
| **Local** | `hecate://` (Unix socket proxy) | Same machine, cross-process | hecate-web → daemon API calls |
| **External** | `mesh` (Macula) | WAN, cross-daemon | Agent-to-agent facts, inter-daemon |

```
┌──────────────────────────────────────────────────────────────────────┐
│              HECATE-DAEMON (Single BEAM VM / Pod)                     │
├──────────────────────────────────────────────────────────────────────┤
│                                                                        │
│   ┌──────────────────────────────────────────┐                        │
│   │         ReckonDB (dev_studio_store)       │                        │
│   │    Events stored via evoq_dispatcher     │                        │
│   └───────────┬──────────┬───────────────────┘                        │
│               │          │                                             │
│          evoq sub   evoq sub                                          │
│          (by type)  (by type)                                         │
│               │          │                                             │
│               ▼          ▼                                             │
│   ┌───────────────┐ ┌────────────┐                                   │
│   │ *_to_pg.erl   │ │*_to_mesh   │                                   │
│   │ (emitter)     │ │(emitter)   │                                   │
│   └───────┬───────┘ └─────┬──────┘                                   │
│           │ pg broadcast   │ mesh publish                              │
│           │                │                                           │
│           ▼                ▼                                           │
│   ┌───────────────┐   Macula Mesh                                    │
│   │ pg listeners  │   (WAN/External)                                 │
│   │ (projections  │                                                    │
│   │  or CMD desks)│                                                    │
│   └───────────────┘                                                    │
│                                                                        │
│   OR: projections subscribe directly via evoq (same division)         │
│                                                                        │
│   ┌──────────────────────────────────────────┐                        │
│   │         ReckonDB (dev_studio_store)       │                        │
│   └───────────┬──────────────────────────────┘                        │
│          evoq sub                                                      │
│          (by type)                                                     │
│               │                                                        │
│               ▼                                                        │
│   ┌───────────────────────────────┐                                   │
│   │ projection (direct subscriber)│                                   │
│   │ -> writes to SQLite read model│                                   │
│   └───────────────────────────────┘                                   │
│                                                                        │
│   ┌──────────────────────────────────────────┐                        │
│   │   Unix Socket API (~/.hecate/*/sockets/) │                        │
│   │   HTTP request/response                  │                        │
│   └───────────┬──────────────────────────────┘                        │
│               │                                                        │
└───────────────┼────────────────────────────────────────────────────────┘
                │ Unix socket
                ▼
┌──────────────────────────────────────────────────────────────────────┐
│              HECATE-WEB (Tauri Desktop App)                           │
├──────────────────────────────────────────────────────────────────────┤
│                                                                        │
│   SvelteKit shell                                                     │
│       │                                                                │
│       │ hecate://{daemon}/path                                        │
│       ▼                                                                │
│   ┌───────────────────────────────┐                                   │
│   │ socket_proxy.rs (Tauri)       │                                   │
│   │ Routes hecate:// requests     │                                   │
│   │ to Unix sockets               │                                   │
│   └───────────┬───────────────────┘                                   │
│               │                                                        │
│   Plugin iframes (SvelteKit SPAs)                                     │
│   also use hecate:// protocol                                         │
│   /plugin/{name}/* routing                                            │
│                                                                        │
└──────────────────────────────────────────────────────────────────────┘

CLI (headless):
  hecate CLI → direct Unix socket → daemon API
  (simple request/response, no streaming)
```

---

## Why `pg` for Internal Integration

1. **Built into OTP** - No dependencies, battle-tested
2. **Zero network overhead** - Direct Erlang message passing
3. **Scales to distributed Erlang** - Works across nodes if needed later
4. **Simple API** - `pg:join/2`, `pg:get_members/1`, `pg:broadcast/2`

**Mesh is wrong for internal integration because:**
- Designed for WAN (QUIC, DHT, NAT traversal)
- Massive overhead inside a single BEAM VM
- Doesn't work well inside container networking

---

## Why `mesh` for External Integration

1. **WAN-capable** - QUIC transport, NAT traversal
2. **Cross-daemon** - Agent-to-agent communication
3. **DHT discovery** - Find capabilities across the network
4. **Realm isolation** - Multi-tenant by design

---

## Why `hecate://` for Local Integration

hecate-web is a Tauri desktop app with a custom `hecate://` URI scheme. All communication between the frontend and daemon(s) flows through this protocol.

1. **Unix socket proxy** -- Tauri's `socket_proxy.rs` intercepts `hecate://` requests and proxies them to Unix sockets on disk
2. **Path-based routing** -- `/plugin/{name}/*` routes to `~/.hecate/hecate-{name}d/sockets/api.sock`, enabling each daemon to own its own socket
3. **Request/response semantics** -- Standard HTTP verbs (GET, POST, PUT, DELETE) over Unix sockets. No SSE, no long-polling
4. **Daemon-as-proxy** -- If no local socket exists for a given daemon, the primary daemon can forward the request to a cluster peer via BEAM distribution
5. **Plugin isolation** -- Plugin frontends (`*w`) are SvelteKit SPAs served by nginx containers, embedded in hecate-web via iframes. They use the same `hecate://` protocol
6. **CLI access** -- The `hecate` CLI provides headless/SSH access by making direct HTTP requests to the daemon's Unix socket. No browser required, no streaming

**For real-time updates:** hecate-web polls or will use WebSocket in the future. The daemon does NOT push events to the frontend.

**Key distinction from `pg` and `mesh`:**
- `pg`: Erlang processes join groups, receive messages directly (intra-BEAM)
- `mesh`: Agents subscribe to topics, receive messages via QUIC (WAN)
- `hecate://`: Frontend makes HTTP requests via Unix socket proxy (same machine, cross-process)

---

## Event Subscription Flow

**Emitters and projections subscribe to the event store via evoq. They are NOT called manually.**

See [EVENT_SUBSCRIPTION_FLOW.md](EVENT_SUBSCRIPTION_FLOW.md) for the full canonical pattern.

```
ReckonDB -> evoq subscription -> emitter (*_to_pg.erl)   -> pg broadcast
ReckonDB -> evoq subscription -> emitter (*_to_mesh.erl)  -> mesh publish
ReckonDB -> evoq subscription -> projection               -> SQLite write
```

**API handlers dispatch commands and return. They do NOT call emitters.**

---

## Domain Events vs Integration Facts

Not every domain event becomes an integration fact.

| Concept | Transport | Decision |
|---------|-----------|----------|
| **Domain Event** | pg | Always publish internally for projections |
| **Integration Fact** | mesh | Selective - only what other agents need |

Emitters decide what to publish. They subscribe via evoq and emit autonomously:

```erlang
%% pg emitter — subscribes in init/1, broadcasts on receive
init([]) ->
    {ok, _} = reckon_evoq_adapter:subscribe(
        dev_studio_store, event_type,
        <<"venture_initiated_v1">>,
        <<"venture_initiated_v1_to_pg">>,
        #{subscriber_pid => self()}),
    {ok, #{}}.

handle_info({events, Events}, State) ->
    lists:foreach(fun(E) ->
        pg:send(pg, venture_initiated_v1, {venture_initiated_v1, E})
    end, Events),
    {noreply, State}.

%% mesh emitter — same subscription, publishes to mesh
handle_info({events, Events}, State) ->
    lists:foreach(fun(E) ->
        Fact = translate_to_fact(E),
        macula:publish(mesh_pid(), <<"venture.initiated">>, Fact)
    end, Events),
    {noreply, State}.
```

---

## Naming Conventions

### Emitters (Publishers)

```
{event}_to_{transport}.erl
```

| Transport | Example |
|-----------|---------|
| pg | `venture_initiated_v1_to_pg.erl` |
| mesh | `capability_announced_v1_to_mesh.erl` |

### Listeners (Subscribers)

**Daemon-side -- CMD desks** (listener triggers a command):
```
on_{event}_from_{transport}_maybe_{command}.erl
```

Example:
```
on_division_discovered_v1_from_pg_maybe_initiate_division.erl
```

**Daemon-side -- PRJ desks** (listener triggers a projection):
```
on_{event}_from_{transport}_project_to_{storage}_{target}.erl
```

Example:
```
on_venture_initiated_v1_from_pg_project_to_sqlite_ventures.erl
```

---

## Listener Placement Rule

> **If a listener's sole purpose is to trigger desk X, it lives IN desk X, supervised by desk X's supervisor.**

Listeners are NOT centralized. There is no `listeners/` directory. Each listener belongs to the desk it triggers.

---

## CMD Desk Structure

```
apps/design_division/src/
└── initiate_division/
    ├── initiate_division_desk_sup.erl
    ├── on_division_discovered_v1_from_pg_maybe_initiate_division.erl
    ├── initiate_division_v1.erl
    ├── division_initiated_v1.erl
    ├── division_initiated_v1_to_pg.erl
    └── maybe_initiate_division.erl
```

**Supervision:**
```erlang
%% initiate_division_desk_sup.erl
init([]) ->
    Children = [
        #{id => pg_listener,
          start => {on_division_discovered_v1_from_pg_maybe_initiate_division, start_link, []},
          restart => permanent,
          type => worker}
    ],
    {ok, {#{strategy => one_for_one}, Children}}.
```

---

## PRJ Desk Structure

```
apps/query_ventures/src/
└── venture_initiated_v1_to_ventures/
    ├── venture_initiated_v1_to_ventures_sup.erl
    ├── on_venture_initiated_v1_from_pg_project_to_sqlite_ventures.erl
    └── venture_initiated_v1_to_sqlite_ventures.erl
```

**Pattern:**
| Component | Naming |
|-----------|--------|
| Directory (desk) | `{event}_to_{target}/` |
| Supervisor | `{event}_to_{target}_sup.erl` |
| Listener | `on_{event}_from_{transport}_project_to_{storage}_{target}.erl` |
| Projection | `{event}_to_{storage}_{target}.erl` |

---

## Supervision Hierarchy

```
query_ventures_sup (domain supervisor)
├── venture_initiated_v1_to_ventures_sup (desk supervisor)
│   └── on_venture_initiated_v1_from_pg_project_to_sqlite_ventures (worker)
├── venture_brief_updated_v1_to_ventures_sup (desk supervisor)
│   └── on_venture_brief_updated_v1_from_pg_project_to_sqlite_ventures (worker)
└── query_ventures_store (SQLite connection worker)
```

```
design_division_sup (domain supervisor)
├── initiate_division_desk_sup (desk supervisor)
│   └── on_division_discovered_v1_from_pg_maybe_initiate_division (worker)
├── complete_division_desk_sup (desk supervisor)
│   └── on_all_desks_implemented_from_pg_maybe_complete_division (worker)
└── design_division_store (ReckonDB store)
```

---

## Implementation Examples

### pg Emitter (subscribes via evoq, broadcasts to pg)

```erlang
%% venture_initiated_v1_to_pg.erl
-module(venture_initiated_v1_to_pg).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(EVENT_TYPE, <<"venture_initiated_v1">>).
-define(PG_GROUP, venture_initiated_v1).
-define(SUB_NAME, <<"venture_initiated_v1_to_pg">>).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _} = reckon_evoq_adapter:subscribe(
        dev_studio_store, event_type, ?EVENT_TYPE, ?SUB_NAME,
        #{subscriber_pid => self()}),
    {ok, #{}}.

handle_info({events, Events}, State) ->
    lists:foreach(fun(E) ->
        pg:send(pg, ?PG_GROUP, {?PG_GROUP, E})
    end, Events),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
```

### pg Listener (CMD desk -- inter-division integration)

```erlang
%% on_division_identified_v1_from_pg_maybe_initiate_division.erl
-module(on_division_identified_v1_from_pg_maybe_initiate_division).
-behaviour(gen_server).

-define(GROUP, division_identified_v1).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = pg:join(pg, ?GROUP, self()),
    {ok, #{}}.

handle_info({division_identified_v1, Fact}, State) ->
    case initiate_division_v1:from_fact(Fact) of
        {ok, Cmd} -> maybe_initiate_division:dispatch(Cmd);
        {error, _Reason} -> ok
    end,
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
```

### Projection via evoq Subscription (same division, direct)

```erlang
%% on_venture_initiated_v1_project_to_sqlite_ventures.erl
-module(on_venture_initiated_v1_project_to_sqlite_ventures).
-behaviour(gen_server).

-define(EVENT_TYPE, <<"venture_initiated_v1">>).
-define(SUB_NAME, <<"venture_initiated_to_sqlite_ventures">>).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _} = reckon_evoq_adapter:subscribe(
        dev_studio_store, event_type, ?EVENT_TYPE, ?SUB_NAME,
        #{subscriber_pid => self()}),
    {ok, #{}}.

handle_info({events, Events}, State) ->
    lists:foreach(fun(E) ->
        venture_initiated_v1_to_sqlite_ventures:project(E)
    end, Events),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
```

### Projection via pg Listener (inter-division)

```erlang
%% on_venture_initiated_v1_from_pg_project_to_sqlite_ventures.erl
-module(on_venture_initiated_v1_from_pg_project_to_sqlite_ventures).
-behaviour(gen_server).

-define(GROUP, venture_initiated_v1).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = pg:join(pg, ?GROUP, self()),
    {ok, #{}}.

handle_info({venture_initiated_v1, Event}, State) ->
    venture_initiated_v1_to_sqlite_ventures:project(Event),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
```

---

## Anti-Patterns

| Anti-Pattern | Why It's Wrong | Correct Approach |
|--------------|----------------|------------------|
| `src/listeners/` directory | Horizontal grouping by technical concern | Listeners live in their desk |
| `*_listeners_sup.erl` | Central supervisor for all listeners | Each desk supervises its own listener |
| mesh for intra-daemon | Massive overhead, wrong tool | Use pg |
| pg for cross-daemon | Doesn't work across network | Use mesh |
| Listener without a desk | Orphan code, unclear ownership | Every listener belongs to a desk |
| SSE streaming to frontends | Complexity for little benefit | Use polling or WebSocket (future) |

---

## Decision Record

| Date | Decision |
|------|----------|
| 2026-02-08 | Use `pg` for internal integration (intra-daemon) |
| 2026-02-08 | Use `mesh` for external integration (WAN/inter-daemon) |
| 2026-02-08 | Listeners live in the desk they trigger |
| 2026-02-08 | Naming: `on_{event}_from_{transport}_maybe_{command}.erl` |
| 2026-02-08 | Naming: `on_{event}_from_{transport}_project_to_{storage}_{target}.erl` |
| 2026-02-08 | PRJ desk directory: `{event}_to_{target}/` |
| 2026-02-13 | Emitters subscribe to ReckonDB via evoq -- not called manually from API handlers |
| 2026-02-13 | Emitters are projections -- same subscription mechanism, different output target |
| 2026-02-13 | QRY projections can subscribe via evoq (same division) OR pg/mesh listeners (inter-division) |
| 2026-02-13 | See [EVENT_SUBSCRIPTION_FLOW.md](EVENT_SUBSCRIPTION_FLOW.md) for canonical pattern |
| 2026-02-18 | TUI transport (`_to_tui.erl`, SSE, FactBus) is DEAD -- replaced by hecate-web `hecate://` protocol |
| 2026-02-18 | Local integration is request/response via Unix socket proxy, not streaming |
| 2026-02-18 | `hecate` CLI provides headless access via direct socket requests (no SSE) |
| 2026-02-18 | Plugin routing: `/plugin/{name}/*` maps to `~/.hecate/hecate-{name}d/sockets/api.sock` |
