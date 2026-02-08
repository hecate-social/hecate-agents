# Integration Transports

How umbrella apps communicate within hecate-daemon and across the network.

---

## The Two Integration Layers

| Layer | Transport | Scope | Use Case |
|-------|-----------|-------|----------|
| **Internal** | `pg` (OTP process groups) | Same BEAM VM | CMD → PRJ projections, intra-daemon |
| **External** | `mesh` (Macula) | WAN, cross-daemon | Agent-to-agent facts, inter-daemon |

```
┌─────────────────────────────────────────────────────────────────┐
│            HECATE-DAEMON (Single BEAM VM / Pod)                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│   manage_torches (CMD)              query_torches (PRJ+QRY)      │
│   ┌─────────────────────┐          ┌─────────────────────────┐  │
│   │ initiate_torch/     │          │ torch_initiated_v1      │  │
│   │                     │    pg    │ _to_torches/            │  │
│   │ torch_initiated_v1  │ ──────►  │  └─ projects to SQLite  │  │
│   │ _to_pg.erl          │          │                         │  │
│   └─────────────────────┘          └─────────────────────────┘  │
│            │                                                      │
│            │ (selective - only external facts)                   │
│            ▼                                                      │
│   ┌─────────────────────┐                                        │
│   │ torch_initiated_v1  │                                        │
│   │ _to_mesh.erl        │ ─────►  Macula Mesh (WAN / External)  │
│   └─────────────────────┘                                        │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
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

## Domain Events vs Integration Facts

Not every domain event becomes an integration fact.

| Concept | Transport | Decision |
|---------|-----------|----------|
| **Domain Event** | pg | Always publish internally for projections |
| **Integration Fact** | mesh | Selective - only what other agents need |

```erlang
%% After event is stored in event store:

%% ALWAYS: publish internally for projections
torch_initiated_v1_to_pg:emit(Event),

%% SELECTIVE: publish externally for other agents
case needs_external_publication(Event) of
    true -> torch_initiated_v1_to_mesh:emit(Event);
    false -> ok
end.
```

---

## Naming Conventions

### Emitters (Publishers)

```
{event}_to_{transport}.erl
```

| Transport | Example |
|-----------|---------|
| pg | `torch_initiated_v1_to_pg.erl` |
| mesh | `capability_announced_v1_to_mesh.erl` |
| nats | `order_placed_v1_to_nats.erl` |

### Listeners (Subscribers)

**For CMD spokes** (listener triggers a command):
```
on_{event}_from_{transport}_maybe_{command}.erl
```

Example:
```
on_cartwheel_identified_v1_from_pg_maybe_initiate_cartwheel.erl
```

**For PRJ spokes** (listener triggers a projection):
```
on_{event}_from_{transport}_project_to_{storage}_{target}.erl
```

Example:
```
on_torch_initiated_v1_from_pg_project_to_sqlite_torches.erl
```

---

## Listener Placement Rule

> **If a listener's sole purpose is to trigger spoke X, it lives IN spoke X, supervised by spoke X's supervisor.**

Listeners are NOT centralized. There is no `listeners/` directory. Each listener belongs to the spoke it triggers.

---

## CMD Spoke Structure

```
apps/manage_cartwheels/src/
└── initiate_cartwheel/
    ├── initiate_cartwheel_sup.erl
    ├── on_cartwheel_identified_v1_from_pg_maybe_initiate_cartwheel.erl
    ├── initiate_cartwheel_v1.erl
    ├── cartwheel_initiated_v1.erl
    ├── cartwheel_initiated_v1_to_pg.erl
    └── maybe_initiate_cartwheel.erl
```

**Supervision:**
```erlang
%% initiate_cartwheel_sup.erl
init([]) ->
    Children = [
        #{id => pg_listener,
          start => {on_cartwheel_identified_v1_from_pg_maybe_initiate_cartwheel, start_link, []},
          restart => permanent,
          type => worker}
    ],
    {ok, {#{strategy => one_for_one}, Children}}.
```

---

## PRJ Spoke Structure

```
apps/query_torches/src/
└── torch_initiated_v1_to_torches/
    ├── torch_initiated_v1_to_torches_sup.erl
    ├── on_torch_initiated_v1_from_pg_project_to_sqlite_torches.erl
    └── torch_initiated_v1_to_sqlite_torches.erl
```

**Pattern:**
| Component | Naming |
|-----------|--------|
| Directory (spoke) | `{event}_to_{target}/` |
| Supervisor | `{event}_to_{target}_sup.erl` |
| Listener | `on_{event}_from_{transport}_project_to_{storage}_{target}.erl` |
| Projection | `{event}_to_{storage}_{target}.erl` |

---

## Supervision Hierarchy

```
query_torches_sup (domain supervisor)
├── torch_initiated_v1_to_torches_sup (spoke supervisor)
│   └── on_torch_initiated_v1_from_pg_project_to_sqlite_torches (worker)
├── torch_brief_updated_v1_to_torches_sup (spoke supervisor)
│   └── on_torch_brief_updated_v1_from_pg_project_to_sqlite_torches (worker)
└── query_torches_store (SQLite connection worker)
```

```
manage_cartwheels_sup (domain supervisor)
├── initiate_cartwheel_sup (spoke supervisor)
│   └── on_cartwheel_identified_v1_from_pg_maybe_initiate_cartwheel (worker)
├── complete_cartwheel_sup (spoke supervisor)
│   └── on_all_spokes_implemented_from_pg_maybe_complete_cartwheel (worker)
└── manage_cartwheels_store (ReckonDB store)
```

---

## Implementation Examples

### pg Emitter

```erlang
%% torch_initiated_v1_to_pg.erl
-module(torch_initiated_v1_to_pg).
-export([emit/1]).

-define(GROUP, torch_initiated_v1).

emit(Event) ->
    pg:broadcast(?GROUP, {torch_initiated_v1, Event}).
```

### pg Listener (CMD spoke)

```erlang
%% on_cartwheel_identified_v1_from_pg_maybe_initiate_cartwheel.erl
-module(on_cartwheel_identified_v1_from_pg_maybe_initiate_cartwheel).
-behaviour(gen_server).

-define(GROUP, cartwheel_identified_v1).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = pg:join(?GROUP, self()),
    {ok, #{}}.

handle_info({cartwheel_identified_v1, Fact}, State) ->
    %% Convert fact to command
    case initiate_cartwheel_v1:from_fact(Fact) of
        {ok, Cmd} ->
            %% Dispatch command
            maybe_initiate_cartwheel:dispatch(Cmd);
        {error, _Reason} ->
            %% Log and skip
            ok
    end,
    {noreply, State}.
```

### pg Listener (PRJ spoke)

```erlang
%% on_torch_initiated_v1_from_pg_project_to_sqlite_torches.erl
-module(on_torch_initiated_v1_from_pg_project_to_sqlite_torches).
-behaviour(gen_server).

-define(GROUP, torch_initiated_v1).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ok = pg:join(?GROUP, self()),
    {ok, #{}}.

handle_info({torch_initiated_v1, Event}, State) ->
    torch_initiated_v1_to_sqlite_torches:project(Event),
    {noreply, State}.
```

---

## Anti-Patterns

| Anti-Pattern | Why It's Wrong | Correct Approach |
|--------------|----------------|------------------|
| `src/listeners/` directory | Horizontal grouping by technical concern | Listeners live in their spoke |
| `*_listeners_sup.erl` | Central supervisor for all listeners | Each spoke supervises its own listener |
| mesh for intra-daemon | Massive overhead, wrong tool | Use pg |
| pg for cross-daemon | Doesn't work across network | Use mesh |
| Listener without a spoke | Orphan code, unclear ownership | Every listener belongs to a spoke |

---

## Decision Record

| Date | Decision |
|------|----------|
| 2026-02-08 | Use `pg` for internal integration (intra-daemon) |
| 2026-02-08 | Use `mesh` for external integration (WAN/inter-daemon) |
| 2026-02-08 | Listeners live in the spoke they trigger |
| 2026-02-08 | Naming: `on_{event}_from_{transport}_maybe_{command}.erl` |
| 2026-02-08 | Naming: `on_{event}_from_{transport}_project_to_{storage}_{target}.erl` |
| 2026-02-08 | PRJ spoke directory: `{event}_to_{target}/` |
