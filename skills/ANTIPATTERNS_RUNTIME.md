# ANTIPATTERNS: Runtime — Event Sourcing & Integration

*Demons about runtime behavior. Event sourcing, integration, and side effects.*

[Back to Index](ANTIPATTERNS.md)

---

## 🔥 Using Mesh for Internal Integration

**Date:** 2026-02-08
**Origin:** hecate-daemon walking skeleton debugging

### The Antipattern

Using Macula mesh (`*_to_mesh.erl` emitters) for communication between umbrella apps within the same BEAM VM.

**Example (WRONG):**
```
setup_venture (CMD app)
    → venture_initiated_v1_to_mesh.erl
    → Macula mesh (QUIC, DHT, NAT traversal)
    → on_venture_initiated_from_mesh.erl
    → query_ventures (PRJ+QRY app)
```

This uses WAN-grade infrastructure for intra-process communication.

### Why It's Wrong

1. **Massive overhead** — QUIC, DHT discovery, NAT traversal for processes in the same VM
2. **QUIC needs addressable URIs** — Doesn't work inside containers (no stable public address)
3. **Adds latency** — Network round-trip for what should be direct message passing

### The Rule

> **Use `pg` (OTP process groups) for internal integration.**
> **Mesh (Macula/QUIC) is ONLY for:**
> - **NAT traversal** — when peers are behind different NATs
> - **Direct Internet** — agent-to-agent over the public Internet
> - **LAN ↔ LAN** — communication between separate physical networks
>
> QUIC requires addressable URIs. Containers don't have them. Mesh does NOT work in K8s.

### The Correct Pattern

```
setup_venture (CMD app)
    → venture_initiated_v1_to_pg.erl    # Internal via pg
    → Direct Erlang message passing
    → on_venture_initiated_v1_from_pg_project_to_sqlite_ventures.erl
    → query_ventures (PRJ+QRY app)
```

### Two Integration Layers

| Layer | Transport | Scope |
|-------|-----------|-------|
| **Internal** | `pg` | Same BEAM VM, intra-daemon, intra-LAN (Erlang VM cluster) |
| **External** | `mesh` | NAT traversal, direct Internet, LAN ↔ LAN (QUIC, addressable URIs required) |

### Naming Convention

| Transport | Emitter | Listener |
|-----------|---------|----------|
| pg | `{event}_to_pg.erl` | `on_{event}_from_pg_*.erl` |
| mesh | `{event}_to_mesh.erl` | `on_{event}_from_mesh_*.erl` |

See [INTEGRATION_TRANSPORTS.md](../philosophy/INTEGRATION_TRANSPORTS.md) for full details.

---

## 🔥 Wrong Aggregate Callback Argument Order

**Date:** 2026-02-09
**Origin:** hecate-daemon division auto-initiation bug

### The Antipattern

Writing aggregate `execute/2` and `apply/2` callbacks with wrong argument order.

**Example (WRONG):**
```erlang
%% WRONG - Payload first, State second
execute(#{command_type := <<"my_command">>} = Payload, State) ->
    do_something(Payload, State).

apply_event(#{event_type := <<"my_event_v1">>} = Event, State) ->
    update_state(Event, State).
```

### The Rule

> **evoq behaviour callbacks expect: State first, then Payload/Event.**

evoq_aggregate.erl calls:
```erlang
Module:execute(AggState, Command#evoq_command.payload)
Module:apply(AccState, Event)
```

### The Correct Implementation

```erlang
-module(my_aggregate).
-behaviour(evoq_aggregate).

%% Behaviour callbacks
-export([init/1, execute/2, apply/2]).

%% init/1 returns {ok, State}
init(_AggregateId) ->
    {ok, initial_state()}.

%% execute/2: State first, Payload second
execute(State, #{command_type := <<"my_command">>} = Payload) ->
    do_something(State, Payload).

%% apply/2: State first, Event second
apply(State, #{event_type := <<"my_event_v1">>} = Event) ->
    update_state(State, Event).
```

### Why This Happens

1. It's natural to write `execute(CommandPayload, State)` — "execute this command on this state"
2. Many examples online show the wrong order
3. Without tests, the bug only appears at runtime

### The Symptom

All commands fail with `{error, unknown_command}` because:
- evoq passes `(State, Payload)`
- Aggregate receives State where it expects a map
- Pattern match `#{command_type := ...}` fails on a record
- Falls through to catch-all: `execute(_State, _Payload) -> {error, unknown_command}`

### Prevention: Always Test Aggregates

```erlang
execute_argument_order_test() ->
    State = my_aggregate:initial_state(),
    Payload = #{command_type => <<"my_command">>, id => <<"test">>},

    %% This test catches wrong argument order immediately
    Result = my_aggregate:execute(State, Payload),
    ?assertMatch({ok, [_]}, Result).
```

### The Lesson

> **Use `-behaviour(evoq_aggregate).`** — The compiler will check callbacks exist.
> **Write aggregate tests before push** — A 10-line test would have caught this bug.

---

## 🔥 Side Effects Based on Hope Acknowledgment

**Date:** 2026-02-09
**Origin:** Vision command architecture review

### The Antipattern

Performing side effects (file I/O, state changes) in a client based on a command's HTTP response rather than on a received event (fact).

**Example (WRONG):**
```javascript
// Client sends command to daemon
const res = await fetch('hecate://localhost/api/ventures/refine-vision', {
    method: 'POST', body: JSON.stringify(params)
});
if (res.ok) {
    // WRONG: treating 200 OK as a fact
    updateVisionDisplay(params);
}
```

The 200 OK means "I received your hope." Not "the vision was refined." Between acknowledgment and event storage, anything can fail.

### The Rule

> **Side effects in external systems (frontends, other agents) must be triggered by received FACTS (events), not by command acknowledgments (hope receipts).**

### The Correct Pattern

```javascript
// Client subscribes to event stream
const events = new EventSource('hecate://localhost/api/ventures/events');
events.addEventListener('vision_refined_v1', (e) => {
    updateVisionDisplay(JSON.parse(e.data));  // NOW it's safe
});

// Client sends hope (fire and forget the response)
fetch('hecate://localhost/api/ventures/refine-vision', {
    method: 'POST', body: JSON.stringify(params)
});  // 202 Accepted — don't act on the response
```

### Why It Matters

- Commands can be rejected by aggregate business rules AFTER acknowledgment
- Event store writes can fail
- Async processing means acknowledgment ≠ completion
- The frontend is an **external system** — it must treat the daemon as eventually consistent

### Reference

See [HOPE_FACT_SIDE_EFFECTS.md](HOPE_FACT_SIDE_EFFECTS.md) for the full architectural pattern.

---

## 🔥 Read-Time Status Enrichment

**Date:** 2026-02-10
**Origin:** hecate-daemon venture/division status handling

### The Antipattern

Computing `status_label` at query time instead of storing it in the read model at projection write time.

**Symptoms:**
```erlang
%% BAD: Query module enriches at read time
list(Opts) ->
    {ok, Rows} = store:query("SELECT * FROM ventures"),
    [enrich_status(row_to_map(R)) || R <- Rows].

enrich_status(#{status := Status} = Row) ->
    Label = evoq_bit_flags:to_string(Status, venture_aggregate:flag_map()),
    Row#{status_label => Label}.
```

**Related violations:**
- Magic numbers: `Status = 3` instead of `evoq_bit_flags:set_all(0, [?VENTURE_INITIATED, ?VENTURE_DNA_ACTIVE])`
- Duplicated flags: `-define(ARCHIVED, 32)` redefined in projections instead of using shared `.hrl`
- Query modules importing aggregate internals (`venture_aggregate:flag_map()`)
- Binary key mismatch: Projections match `#{venture_id := ...}` but events arrive with `<<"venture_id">>` keys

### The Rule

> **Read models store DENORMALIZED data. Compute everything at write time (projection), never at read time (query).**

### The Correct Pattern

**1. Extract flag macros to `.hrl` header in CMD app:**
```erlang
%% apps/setup_venture/include/venture_status.hrl
-define(VENTURE_INITIATED,   1).
-define(VENTURE_DNA_ACTIVE,  2).
-define(VENTURE_ARCHIVED,   32).

-define(VENTURE_FLAG_MAP, #{
    0                    => <<"New">>,
    ?VENTURE_INITIATED   => <<"Initiated">>,
    ?VENTURE_DNA_ACTIVE  => <<"Discovering">>,
    ?VENTURE_ARCHIVED    => <<"Archived">>
}).
```

**2. Projection computes and stores `status_label` at write time:**
```erlang
-include_lib("setup_venture/include/venture_status.hrl").

project(Event) ->
    VentureId = get(venture_id, Event),
    Status = evoq_bit_flags:set_all(0, [?VENTURE_INITIATED, ?VENTURE_DNA_ACTIVE]),
    Label = evoq_bit_flags:to_string(Status, ?VENTURE_FLAG_MAP),
    store:execute(
        "INSERT INTO ventures (venture_id, status, status_label) VALUES (?1, ?2, ?3)",
        [VentureId, Status, Label]).
```

**3. Query module reads `status_label` directly — no enrichment:**
```erlang
list(Opts) ->
    {ok, Rows} = store:query("SELECT venture_id, status, status_label FROM ventures"),
    [row_to_map(R) || R <- Rows].
%% NO enrich_status function at all
```

**4. Handle binary keys from events (events arrive with binary keys from evoq/ReckonDB):**
```erlang
get(Key, Map) when is_atom(Key) ->
    case maps:find(Key, Map) of
        {ok, V} -> V;
        error -> maps:get(atom_to_binary(Key, utf8), Map, undefined)
    end.
```

### Why It Matters

- **CPU waste**: `enrich_status` runs `evoq_bit_flags:to_string/2` on every row, every query, every request
- **Coupling**: Query modules (PRJ app) depend on aggregate internals (CMD app's `flag_map()`)
- **Inconsistency**: API handlers compute their own labels with hardcoded magic numbers
- **Fragility**: If `flag_map()` changes, all cached/stored data still shows old labels until re-queried
- **CQRS violation**: Read models should be pre-computed and ready to serve — no computation at query time

### The Lesson

> **Projections exist to do the heavy lifting. Queries exist to be dumb and fast.**

---

## Demon 15: Consumer-Generated Command IDs for Framework Idempotency

**Date exorcised:** 2026-02-11
**Where it appeared:** All 76 dispatch modules across hecate-daemon CMD apps
**Cost:** 9/9 L4b dispatch tests returning cached first-command results (silent data loss)

### The Lie

"Each dispatch module should generate its own `command_id` for idempotency."

### What Happened

All 76 dispatch modules had identical `generate_command_id(Id, Timestamp)` functions using `hash(AggregateId + Timestamp_ms)`. Two different commands to the same aggregate within 1ms produced identical command IDs, causing the idempotency cache to silently return the first command's cached result — a production-grade silent data loss bug. The codegen template propagated the bug to every module.

### Why It's Wrong

- **76 identical functions = wrong responsibility placement.** If every consumer must implement the same logic, it belongs in the framework.
- **Conflates two concepts.** The design conflates command identification (tracing, unique per invocation) with command deduplication (idempotency, deterministic per intent).
- **`hash(Id + Timestamp)` fails at BOTH:** Not unique within 1ms (commands collide), not deterministic across retries (timestamps differ).

### The Truth

- The **FRAMEWORK** (evoq) should auto-generate `command_id` if not provided (unique per invocation, for tracing)
- True idempotency requires a separate `idempotency_key` field — caller-provided, deterministic, based on business intent
- Dispatch modules should NOT contain `generate_command_id` at all
- These are two separate fields on `#evoq_command{}`: `command_id` (framework-owned) and `idempotency_key` (caller-optional)

### The Fix

evoq v1.3.0+ auto-generates `command_id`. Dispatch modules drop `generate_command_id` entirely.

### The Lesson

> **If every consumer implements the same function, it belongs in the framework.**
> **If one field serves two purposes (identification and deduplication), split it into two fields.**

---

## 🔥 Demon 19: esqlite3 Returns Lists, Not Tuples

**Date exorcised:** 2026-02-12
**Where it appeared:** 12+ `row_to_map/1` functions across all `query_*` apps
**Cost:** Silent pattern match failure — queries returned `{ok, []}` instead of data

### The Lie

"SQLite rows come back as tuples like `{Col1, Col2, Col3}`."

### What Happened

All `row_to_map/1` functions used tuple patterns:
```erlang
%% WRONG — tuples
row_to_map({Id, Name, Status, Label}) ->
    #{id => Id, name => Name, status => Status, label => Label}.
```

But `esqlite3:fetchall/1` returns rows as **lists of lists**, not lists of tuples:
```erlang
{ok, [[<<"id1">>, <<"name1">>, 1, <<"Active">>],
      [<<"id2">>, <<"name2">>, 2, <<"Done">>]]}
```

The tuple pattern `{Id, Name, Status, Label}` never matches `[Id, Name, Status, Label]`, so the list comprehension `[row_to_map(R) || R <- Rows]` silently produces an empty list.

### The Correct Pattern

```erlang
%% CORRECT — lists
row_to_map([Id, Name, Status, Label]) ->
    #{id => Id, name => Name, status => Status, label => Label}.
```

### Why This Happens

1. Many Erlang database libraries (mnesia, ets) return tuples
2. The mental model "row = tuple" is deeply ingrained
3. Without tests that exercise the full query path, the bug only surfaces at runtime
4. The empty-list result looks like "no data" rather than "pattern match failed"

### Prevention

```erlang
%% Test that row_to_map works with list input
row_to_map_test() ->
    Row = [<<"id">>, <<"name">>, 1, <<"Active">>],
    Result = row_to_map(Row),
    ?assertEqual(<<"id">>, maps:get(id, Result)).
```

### The Lesson

> **esqlite3 returns lists. Always. Test your row_to_map with actual list inputs.**
> **This bug was in the BIT_FLAGS_STATUS_PROJECTION.md template — the "correct" example was wrong.**

---

## 🔥 Demon 20: Eager Default in `maps:get/3`

**Date exorcised:** 2026-02-12
**Where it appeared:** 10 command modules (`*_v1.erl`) in `mentor_llms`
**Cost:** `noproc` crash when gen_server not running (e.g., in tests)

### The Lie

"Use `maps:get(key, Map, default_value())` to provide a fallback."

### What Happened

Command modules used function calls as `maps:get/3` defaults:
```erlang
%% WRONG — hecate_identity:agent_id() is ALWAYS called
SubmitterId = maps:get(<<"submitter_id">>, Map, hecate_identity:agent_id()),
```

In Erlang, `maps:get(Key, Map, Default)` evaluates `Default` **before** checking if `Key` exists. When `Default` is a function call to a gen_server (`hecate_identity`), it crashes with `noproc` if that server isn't running — even when the key IS present in the map.

### The Correct Pattern

```erlang
%% CORRECT — lazy evaluation via case
SubmitterId = case maps:find(<<"submitter_id">>, Map) of
    {ok, V} -> V;
    error -> hecate_identity:agent_id()
end,
```

### Why This Happens

1. `maps:get/3` looks like it should be lazy (only use default when key is missing)
2. In many other languages, default values ARE lazy (Python's `dict.get(k, v)` doesn't evaluate `v` eagerly)
3. Erlang evaluates all function arguments before calling the function — there are no lazy arguments
4. The bug only manifests when the gen_server isn't running, which may not happen in production but always happens in unit tests

### When This Matters

| Default Expression | Safe? | Why |
|-------------------|-------|-----|
| `maps:get(k, M, <<>>)` | Yes | Literal — no side effects |
| `maps:get(k, M, undefined)` | Yes | Atom literal |
| `maps:get(k, M, 0)` | Yes | Integer literal |
| `maps:get(k, M, gen_server:call(...))` | **NO** | Evaluated even when key exists |
| `maps:get(k, M, hecate_identity:agent_id())` | **NO** | gen_server call, crashes if down |
| `maps:get(k, M, os:timestamp())` | **NO** | Side effect always runs |

### The Lesson

> **Never use function calls as `maps:get/3` defaults. Use `maps:find/2` + `case` for lazy evaluation.**
> **If the default is a literal, `maps:get/3` is fine. If it's a function call, it's a bug waiting to happen.**

---

## 🔥 Demon 21: esqlite3 Argument Order (Db First)

**Date exorcised:** 2026-02-12
**Where it appeared:** 8 `query_*_store.erl` files
**Cost:** `function_clause` crash on store initialization

### The Lie

"Just call `esqlite3:exec(SQL, Db)` — SQL first, then connection."

### What Happened

Store `init/1` functions had the arguments reversed:
```erlang
%% WRONG — SQL first
esqlite3:exec("PRAGMA journal_mode=WAL", Db),
esqlite3:exec("CREATE TABLE IF NOT EXISTS ...", Db),
```

The esqlite3 API is `esqlite3:exec(Db, SQL)` — connection first:
```erlang
%% CORRECT — Db first
esqlite3:exec(Db, "PRAGMA journal_mode=WAL"),
esqlite3:exec(Db, "CREATE TABLE IF NOT EXISTS ..."),
```

### The API

| Function | Signature | Note |
|----------|-----------|------|
| `esqlite3:open/1` | `open(Path)` | Returns `{ok, Db}` |
| `esqlite3:exec/2` | `exec(Db, SQL)` | **Db first** |
| `esqlite3:q/2` | `q(Db, SQL)` | **Db first** |
| `esqlite3:q/3` | `q(Db, SQL, Params)` | **Db first** |

### Why This Happens

1. Many Erlang database APIs use `Module:exec(SQL, Connection)` — SQL first
2. The "subject.verb(object)" pattern (`SQL.exec_on(Db)`) feels natural
3. esqlite3 follows the C SQLite API convention where the handle comes first
4. Without type specs or dialyzer, the crash only appears at runtime

### The Lesson

> **esqlite3: connection (Db) ALWAYS comes first. `exec(Db, SQL)`, `q(Db, SQL, Params)`.**
> **When in doubt, check the esqlite3 source — don't assume argument order from other libraries.**

---

## 🔥 Demon 22: Manual Event Emission from API Handlers

**Date exorcised:** 2026-02-13
**Where it appeared:** All 17 storm desk API handlers in `guide_venture_lifecycle`
**Cost:** Projections never received events — read models stayed empty

### The Lie

"After dispatch, the API handler should call emitters to broadcast events."

### What Happened

API handlers manually called pg and mesh emitters after dispatching:

```erlang
%% post_event_sticky_api.erl — WRONG
case maybe_post_event_sticky:dispatch(Cmd) of
    {ok, _Version, Events} ->
        lists:foreach(fun(E) ->
            event_sticky_posted_v1_to_pg:emit(E),    %% Manual call
            event_sticky_posted_v1_to_mesh:emit(E)    %% Manual call
        end, Events),
        hecate_api_utils:json_reply(201, Body, Req);
```

The pg emitter had a simple `emit/1` function that broadcast to a pg group:

```erlang
%% event_sticky_posted_v1_to_pg.erl — WRONG
emit(Event) ->
    Members = pg:get_members(pg, event_sticky_posted_v1),
    lists:foreach(fun(Pid) -> Pid ! {event_sticky_posted_v1, Event} end, Members),
    ok.
```

Projections joined pg groups and waited for messages:

```erlang
%% on_event_sticky_posted_v1_from_pg_project_to_sqlite_event_stickies.erl — WRONG
init([]) ->
    ok = pg:join(pg, event_sticky_posted_v1, self()),
    {ok, #{}}.

handle_info({event_sticky_posted_v1, Event}, State) ->
    event_sticky_posted_v1_to_sqlite_event_stickies:project(Event),
    {noreply, State}.
```

### Why It's Wrong

1. **Fragile coupling** — Every API handler must know which emitters to call for which events. Forget one? Silent data loss.
2. **Wrong responsibility** — The API handler's job is request/response. Event delivery is infrastructure.
3. **No replay** — If an emitter wasn't running when the event was stored, the event is lost forever. No catch-up.
4. **Duplicates on retry** — If the HTTP request is retried, the event is emitted twice (even though evoq deduplicates the command).
5. **Testing burden** — Every API handler test must verify emit calls in addition to dispatch.

### The Truth

**Emitters are projections.** They subscribe to the event store via evoq and react autonomously. The API handler dispatches the command and returns. Period.

ReckonDB has a built-in subscription mechanism:
1. Subscriber calls `reckon_evoq_adapter:subscribe(StoreId, event_type, EventType, Name, #{subscriber_pid => self()})`
2. ReckonDB registers a **Khepri trigger** filtered by event type
3. When events are appended, the trigger fires
4. Subscriber receives `{events, [Event]}` messages automatically

### The Correct Pattern

**API handler — dispatch and return:**
```erlang
%% post_event_sticky_api.erl — CORRECT
case maybe_post_event_sticky:dispatch(Cmd) of
    {ok, _Version, Events} ->
        %% Return response. DONE. No emit calls.
        hecate_api_utils:json_reply(201, Body, Req);
    {error, Reason} ->
        hecate_api_utils:json_error(422, Reason, Req)
end.
```

**Emitter — subscribes via evoq at startup:**
```erlang
%% event_sticky_posted_v1_to_pg.erl — CORRECT
-behaviour(gen_server).

init([]) ->
    {ok, _} = reckon_evoq_adapter:subscribe(
        dev_studio_store,
        event_type,
        <<"event_sticky_posted_v1">>,
        <<"event_sticky_posted_v1_to_pg">>,
        #{subscriber_pid => self()}
    ),
    {ok, #{}}.

handle_info({events, Events}, State) ->
    lists:foreach(fun(Event) ->
        pg:send(pg, event_sticky_posted_v1, {event_sticky_posted_v1, Event})
    end, Events),
    {noreply, State}.
```

**Projection — subscribes via evoq OR listens on pg:**
```erlang
%% Direct evoq subscription (same division):
init([]) ->
    {ok, _} = reckon_evoq_adapter:subscribe(
        dev_studio_store, event_type,
        <<"event_sticky_posted_v1">>,
        <<"sticky_posted_to_event_stickies">>,
        #{subscriber_pid => self()}
    ),
    {ok, #{}}.

%% OR pg listener (inter-division):
init([]) ->
    ok = pg:join(pg, event_sticky_posted_v1, self()),
    {ok, #{}}.
```

### The Flow

```
WRONG:  API handler -> manual emit -> pg -> projection
RIGHT:  ReckonDB -> evoq subscription -> emitter -> pg -> listener
        ReckonDB -> evoq subscription -> projection (direct)
```

### Prevention

- API handlers must NEVER import or call `*_to_pg` or `*_to_mesh` modules
- Emitters must be `gen_server`s that subscribe in `init/1`
- If an emitter has an `emit/1` export (not `start_link/0`), it's wrong

### The Lesson

> **The API handler dispatches commands. The event store delivers events. These are separate concerns.**
> **Emitters subscribe to the store — they are NOT called by application code.**
> **See [EVENT_SUBSCRIPTION_FLOW.md](../philosophy/EVENT_SUBSCRIPTION_FLOW.md) for the canonical pattern.**

---

## 🔥 Demon 23: Raw #event{} Records Passed to Projections

**Date exorcised:** 2026-02-13
**Where it appeared:** All 45 projection subscribers across `query_venture_lifecycle` and `query_division_alc`
**Cost:** Every projection crashed on boot — read models permanently empty

### The Lie

"The subscriber receives events and passes them to `project/1`. It just works."

### What Happened

ReckonDB emitters send `{events, [#event{}]}` messages where `#event{}` is a **record** (tuple) from `reckon_gater/include/esdb_gater_types.hrl`. The business payload lives inside `#event.data`.

All 45 projection subscribers passed the raw record to `project/1`:

```erlang
%% WRONG — E is an #event{} record (tuple), not a map
handle_info({events, Events}, State) ->
    lists:foreach(fun(E) ->
        case venture_initiated_v1_to_sqlite_ventures:project(E) of
            ok -> ok;
            {error, Reason} -> logger:warning("failed: ~p", [Reason])
        end
    end, Events),
    {noreply, State}.
```

But `project/1` calls `maps:find(Key, Event)` — which crashes with `{badmap, #event{...}}` because `#event{}` is a tuple, not a map.

The gen_server crashes, the supervisor restarts it (subscribing to ReckonDB again), it crashes again on the first event — infinite restart loop until the supervisor hits its max restart intensity and dies.

**Result:** Events stored correctly in ReckonDB, but SQLite read models permanently empty. POST succeeds but GET returns `[]`.

### Why It's Wrong

1. **Records are tuples.** Erlang records compile to tuples. `maps:find/2` on a tuple raises `{badmap, _}`.
2. **Silent failure.** The supervisor restart loop happens in the background — no visible error to the user, just empty query results.
3. **Template propagation.** The subscriber template had this bug, so every subscriber generated from it inherited it.
4. **No tests caught it.** All existing projection tests passed flat maps directly to `project/1`, never testing with actual `#event{}` records.

### The Truth

Projections need a **flat map** with both envelope fields (event_id, event_type, stream_id, version, timestamp) and business data fields merged at top level.

### The Fix

Created `projection_event:to_map/1` in the `shared` app:

```erlang
-module(projection_event).
-include_lib("reckon_gater/include/esdb_gater_types.hrl").
-export([to_map/1]).

to_map(#event{} = E) ->
    Envelope = #{
        event_id => E#event.event_id,
        event_type => E#event.event_type,
        stream_id => E#event.stream_id,
        version => E#event.version,
        metadata => E#event.metadata,
        timestamp => E#event.timestamp,
        epoch_us => E#event.epoch_us
    },
    case E#event.data of
        Data when is_map(Data) -> maps:merge(Data, Envelope);
        _ -> Envelope
    end;
to_map(Map) when is_map(Map) ->
    Map.  %% Already flat — passthrough for backward compat
```

All 45 subscribers updated:
```erlang
%% CORRECT — convert record to flat map first
case venture_initiated_v1_to_sqlite_ventures:project(projection_event:to_map(E)) of
```

### Prevention

**Always test with the actual input type.** Write at least one test per projection that constructs an `#event{}` record and passes it through the full pipeline:

```erlang
raw_record_crashes_project() ->
    EventRecord = #event{
        event_id = <<"evt-1">>, event_type = <<"venture_initiated_v1">>,
        stream_id = <<"venture_aggregate-v1">>, version = 0,
        data = #{venture_id => <<"v-1">>, name => <<"Test">>},
        metadata = #{}, timestamp = 1000, epoch_us = 1000000
    },
    ?assertError({badmap, _}, my_projection:project(EventRecord)).

record_via_to_map_projects() ->
    EventRecord = ...,  %% same as above
    FlatMap = projection_event:to_map(EventRecord),
    ok = my_projection:project(FlatMap).
```

### The Lesson

> **Records are tuples. Maps are maps. Never assume one works where the other is expected.**
> **If all your tests pass flat maps but production receives records, your tests are lying.**
> **Write a failing test FIRST — then fix the code.**

---

## 🔥 Demon 24: Silent Subscription Pipeline Failures

**Date exorcised:** 2026-02-13
**Where it appeared:** reckon-db subscription delivery pipeline (three bugs: v1.2.4, v1.2.5, v1.2.6)
**Cost:** 7 days of debugging. POST /api/ventures/initiate returns 201, GET /api/ventures returns []. No errors, no crashes, no warnings.

### The Lie

"If commands succeed and events are stored, projections will populate."

### What Happened

Three separate bugs in reckon-db's subscription delivery pipeline each caused **complete silence** — events stored correctly but never delivered to subscribers. Each bug was in a different layer:

| Bug | Layer | What Went Wrong | Symptom |
|-----|-------|-----------------|---------|
| Filter path mismatch | Khepri trigger | `by_stream` stripped category prefix from stream ID → filter path `[streams, <<"delivery-001">>]` never matched storage path `[streams, <<"test$delivery-001">>]` | Trigger never fires |
| Record vs map matching | Khepri trigger | `by_event_type` used map pattern `#{event_type => Type}` but stored data is `#event{}` record (tuple) — maps can't match tuples | Trigger never fires |
| Subscription id undefined | pg group routing | `subscribe/5` never set `#subscription.id` → emitter pool joined pg group `{Store, undefined, emitters}` → trigger broadcast to `{Store, CorrectKey, emitters}` | Broadcast finds no emitters |

**Result:** Commands succeed (201). Events stored in ReckonDB. Triggers registered. But the chain breaks silently at different points — subscribers never receive `{events, [Event]}` messages. SQLite read models stay empty. GET returns `[]`.

### Why This Is the Deadliest Bug Class

1. **No errors.** No crashes, no warnings, no log messages. The system appears healthy.
2. **Partial success misleads.** POST returns the event data. The developer thinks "it worked."
3. **Blame diffusion.** Is it the projection? The emitter? The subscription? The trigger? The filter? The pg group? Each component appears correct in isolation.
4. **Unit tests pass.** Each layer's unit tests passed. No integration test exercised the full pipeline.
5. **The gap is invisible.** The distance between "event stored" and "event delivered to subscriber" is a black box.

### The Pipeline

```
append(Event)
  → Khepri put (stored)              ← Events live here
  → Khepri trigger fires             ← Bug 1, 2: filter doesn't match
  → ProcFun executes
  → reckon_db_emitter_group:broadcast(StoreId, SubKey, Event)
  → pg:get_members(scope, {StoreId, SubKey, emitters})  ← Bug 3: wrong key
  → emitter receives
  → subscriber receives {events, [Event]}  ← Never reached
```

### Three Rules That Would Have Prevented This

**Rule 1: Integration-test the full delivery pipeline.**

Before trusting subscriptions, write a CT test that:
1. Creates a store
2. Subscribes a process
3. Starts an emitter for that subscription
4. Appends an event
5. Asserts the subscriber receives it within N seconds

```erlang
%% This test caught ALL THREE bugs
subscribe_then_append_delivers_event(Config) ->
    StoreId = proplists:get_value(store_id, Config),
    {ok, SubKey} = reckon_db_subscriptions:subscribe(
        StoreId, stream, <<"test$stream-1">>, <<"test_sub">>,
        #{subscriber => self()}),
    EmitterName = reckon_db_emitter_group:emitter_name(StoreId, SubKey),
    {ok, _} = reckon_db_emitter:start_link(StoreId, SubKey, self(), EmitterName),
    {ok, _} = reckon_db_streams:append(StoreId, <<"test$stream-1">>, -2,
        [#{event_type => <<"thing_happened_v1">>, data => #{}}]),
    receive
        {events, [E]} -> ?assertEqual(<<"thing_happened_v1">>, E#event.event_type)
    after 5000 -> ct:fail("Event not delivered")
    end.
```

**This test did not exist until after 7 days of debugging.**

**Rule 2: Computed identifiers must travel with their records.**

When you compute a key/id from a record's fields, set it ON the record immediately:

```erlang
%% WRONG — id computed but not set on record
Key = subscriptions_store:key(Subscription),
notify_created(StoreId, subscriptions, Subscription),  %% id = undefined!

%% CORRECT — id travels with the record
Key = subscriptions_store:key(Subscription),
SubWithId = Subscription#subscription{id = Key},
notify_created(StoreId, subscriptions, SubWithId),  %% id = Key ✓
```

If downstream code needs `Record.id`, the code that computes the id must set it. Don't hope someone else will.

**Rule 3: When storage paths and filter paths must match, test that they match.**

Khepri triggers use path-based event filters. If the filter path doesn't **exactly** match the storage path, the trigger silently never fires. There's no "filter didn't match" warning.

```
Storage path:  [streams, <<"test$delivery-001">>, <<"000000000">>]
Filter path:   [streams, <<"delivery-001">>, #if_path_matches{...}]
                         ^^^^^^^^^^^^^^^^^
                         WRONG — stripped the category prefix
```

This is a general rule for any trigger/filter/subscription system: **the selector must mirror the storage path exactly.** Write a test that verifies this by storing data and checking the trigger fires.

### The Meta-Lesson

> **Event sourcing's biggest vulnerability is the invisible gap between "stored" and "delivered."**
> **If you can't prove delivery with an integration test, you can't trust it.**
> **Silent failure + no integration tests = days of debugging that one CT test would have prevented.**

### For Hecate Agents Specifically

When generating code that uses ReckonDB subscriptions:

1. **Always generate a delivery smoke test** alongside subscription code
2. **Never assume subscriptions "just work"** — verify with `receive ... after` in a test
3. **If GET returns empty but POST succeeds** — the subscription pipeline is broken, not the projection logic
4. **Check these layers in order:** filter match → trigger firing → pg group membership → emitter running → subscriber PID alive

### See Also

- [reckon-db CHANGELOG](https://github.com/reckon-db-org/reckon-db/blob/main/CHANGELOG.md) — bugs documented in v1.2.4, v1.2.5, v1.2.6
- Demon #22 (Manual Event Emission) — the application-level version of this infrastructure-level problem
- Demon #23 (Raw Records in Projections) — another "silent failure" in the event delivery chain
- [SESSION_LEVEL_CONSISTENCY.md](../philosophy/SESSION_LEVEL_CONSISTENCY.md) — the mitigation pattern: return aggregate state from commands so callers don't depend on projection timing

---

## 🔥 Demon 26: Calling PG Emitters "Dead Code" Without Subscribers

**Date exorcised:** 2026-02-23
**Where it appeared:** hecate-app-appstored audit — 6 `*_to_pg.erl` emitters
**Cost:** Nearly deleted working infrastructure that enables inter-domain integration

### The Lie

"These `*_to_pg.erl` emitters have no subscribers — they're dead code and should be removed."

### What Happened

During an audit of `hecate-app-appstored`, all 6 PG emitters were flagged as "dead code" because no process currently calls `pg:join/3` on their topics. The reasoning was:

1. No module subscribes to these pg groups → nobody receives the broadcasts → the emitters do nothing
2. Therefore they are dead code and should be removed to reduce complexity

This reasoning is **fundamentally wrong** and misunderstands two things:

### Two Misconceptions Corrected

**Misconception 1: "No current subscribers = dead code"**

PG emitters are **pub/sub publishers**. In pub/sub, the publisher does NOT need to know about consumers. The publisher's job is to PUBLISH. Consumers arrive when they need the data — possibly in a different app, possibly months later, possibly in a process manager that doesn't exist yet.

Calling a publisher "dead" because it has no current subscribers is like calling a radio tower "dead" because nobody is tuned in right now. The tower's job is to broadcast. Listeners come and go.

**Misconception 2: "ReckonDB is just an Event Store"**

ReckonDB (like any Event Store) serves **two roles**:

1. **Event Store** — durable, ordered storage of domain events
2. **Event Bus** — processes subscribe via `reckon_evoq_adapter:subscribe/5` and receive events as they're appended

The `*_to_pg.erl` emitters bridge from the Event Bus to OTP `pg` groups. This serves a different audience than direct evoq subscriptions:

| Channel | Audience | Scope |
|---------|----------|-------|
| Direct evoq subscription | Projections within the same division | Intra-domain |
| PG emitter → pg group | Process managers, listeners in OTHER divisions | Inter-domain |

Both channels are valid. They serve different integration needs.

### The Architecture

```
ReckonDB (Store + Bus)
  ↓ reckon_evoq_adapter:subscribe/5
  ├── Projections → SQLite (read models, same domain)
  └── PG Emitters → pg groups (inter-domain integration)
                      └── Future consumers join when needed
```

### Why It's Dangerous to Remove

1. **Silent breakage** — A future process manager that `pg:join`s an emitter's topic will receive nothing if the emitter was deleted
2. **No error signal** — pg groups with no publishers don't error — they just never deliver messages
3. **Architectural intent lost** — The emitter documents that "this event is important enough to broadcast inter-domain"
4. **Rebuilding is expensive** — Re-creating the emitter, its supervisor child spec, and its evoq subscription is significant work

### The Rule

> **Never remove a pub/sub publisher because it has no current subscribers.**
> **PG emitters are inter-domain integration infrastructure — they exist to PUBLISH, not to serve known consumers.**
> **An Event Store is also an Event Bus. Emitters bridge from store-bus to pg-bus for a different audience.**

### Prevention

Before calling any emitter "dead code," ask:

1. Is it a pub/sub publisher? → Publishers don't need subscribers to be valid
2. Does it bridge between integration layers? → It's infrastructure
3. Was it intentionally created as part of a vertical slice? → It documents intent
4. Could a future consumer need this topic? → Leave it alone

### The Lesson

> **Pub/sub publishers are infrastructure. Infrastructure exists before its consumers.**
> **ReckonDB = Event Store + Event Bus. PG emitters bridge the bus to pg groups for inter-domain use.**
> **"No subscribers" ≠ "dead code." It means "no consumers yet."**

---

## 🔥 Demon 27: Hardcoded User/Submitter IDs

**Date exorcised:** 2026-02-23
**Where it appeared:** hecate-app-appstored command modules
**Cost:** Every event in the store records the wrong actor — audit trail is useless

### The Lie

"Just use `<<"system">>` or a placeholder for the user ID — we'll fix it later."

### What Happened

Command modules hardcoded the submitter identity:

```erlang
%% WRONG — who actually did this?
Cmd = buy_license_v1:new(#{
    license_id => LicenseId,
    plugin_id => PluginId,
    user_id => <<"system">>   %% Hardcoded placeholder
}),
```

Every event stored in ReckonDB records `<<"system">>` as the actor. The audit trail — "who did what, when" — is destroyed. In an event-sourced system, events are immutable. You cannot retroactively fix the actor identity.

### Why It's Wrong

1. **Audit trail destroyed** — Event sourcing's primary value is a complete, truthful history. Hardcoded actors make the history a lie.
2. **Immutable damage** — Events cannot be amended. Once stored with `<<"system">>`, that event will always say "system did it."
3. **Security blind spot** — No way to trace actions back to actual users for access control, debugging, or compliance.
4. **Multi-user broken** — When two users buy licenses, both events say "system" — indistinguishable.

### The Rule

> **Commands MUST carry the real actor identity. The API handler extracts the user from the request context and passes it through to the command.**

### The Correct Pattern

```erlang
%% API handler extracts identity from request
handle_post(Req0, State) ->
    UserId = extract_user_id(Req0),  %% From auth token, session, etc.
    {ok, Params, Req1} = app_api_utils:read_json_body(Req0),
    Cmd = buy_license_v1:new(#{
        license_id => maps:get(<<"license_id">>, Params),
        plugin_id => maps:get(<<"plugin_id">>, Params),
        user_id => UserId   %% Real actor identity
    }),
    ...
```

For commands triggered by Policies or Listeners (no HTTP request), the actor is the **system process** that initiated the action — record it explicitly:

```erlang
%% Policy — actor is the policy itself
Cmd = remove_plugin_v1:new(#{
    license_id => LicenseId,
    initiated_by => <<"policy:on_license_revoked_v1_maybe_remove_plugin">>
}),
```

### Prevention

- Every command struct MUST have a `submitter_id` or `initiated_by` field
- API handlers MUST extract identity from request context
- Policies/Listeners MUST identify themselves as the actor
- Code review: reject any command with hardcoded `<<"system">>`, `<<"admin">>`, or `<<>>`

### The Lesson

> **Events are immutable history. Hardcoded actor IDs destroy that history permanently.**
> **The identity flows from the entry point (API, Policy, Listener) into the command. No exceptions.**

---

## 🔥 Demon 28: No Tests on Event-Sourced Domains

**Date exorcised:** 2026-02-23
**Where it appeared:** hecate-app-appstored — 0 tests across 6 CMD desks, 5 projections, 4 query handlers, 4 policies
**Cost:** Bugs found only by dialyzer or at runtime — no safety net for refactoring

### The Lie

"Dialyzer catches type errors, so we don't need tests."

### What Happened

The appstore daemon shipped with zero tests. Dialyzer caught the `row_to_map` tuple/list bug (Demon #19), but only because it was a type mismatch. Business logic bugs — wrong aggregate guards, incorrect projection SQL, broken policy chains — are invisible to dialyzer.

### What Dialyzer Cannot Catch

| Bug Type | Dialyzer? | Unit Test? |
|----------|-----------|------------|
| Wrong function argument types | Yes | Yes |
| Dead code branches | Yes | Yes |
| Wrong aggregate business rule (rejects valid command) | **No** | Yes |
| Projection writes wrong column value | **No** | Yes |
| Policy dispatches wrong command | **No** | Yes |
| Command validation too permissive | **No** | Yes |
| Event missing required field | **No** | Yes |
| Bit flag combination produces wrong status_label | **No** | Yes |

Dialyzer proves types align. Tests prove behavior is correct. Both are needed.

### Minimum Test Coverage for Event-Sourced Domains

| Component | What to Test | Priority |
|-----------|-------------|----------|
| **Aggregate** | Every command + every business rule guard | Critical |
| **Projection** | Each event type with real `#event{}` record input (Demon #23) | Critical |
| **Command struct** | `new/1` produces valid command, required fields enforced | High |
| **Event struct** | `new/N` produces valid event, `to_map/1` round-trips | High |
| **Policy** | Receives event, dispatches correct command | High |
| **Query API** | `row_to_map` works with actual esqlite3 output format | Medium |

### The Rule

> **Every event-sourced domain needs tests BEFORE it ships. Dialyzer is a type checker, not a behavior checker.**

### Minimum Viable Test Suite

For an aggregate with N commands:

```erlang
%% 1. Each command produces the right event
initiate_test() ->
    {ok, State} = my_aggregate:init(<<"agg-1">>),
    Cmd = #{command_type => <<"initiate_thing_v1">>, id => <<"t-1">>},
    {ok, [Event]} = my_aggregate:execute(State, Cmd),
    ?assertEqual(<<"thing_initiated_v1">>, maps:get(event_type, Event)).

%% 2. Business rules reject invalid commands
cannot_archive_already_archived_test() ->
    State = state_with_flags(?ARCHIVED),
    Cmd = #{command_type => <<"archive_thing_v1">>, id => <<"t-1">>},
    ?assertMatch({error, already_archived}, my_aggregate:execute(State, Cmd)).

%% 3. Projection handles real #event{} records
projection_with_record_test() ->
    Event = #event{
        event_type = <<"thing_initiated_v1">>,
        data = #{id => <<"t-1">>, name => <<"Test">>},
        stream_id = <<"thing-t-1">>, version = 0,
        event_id = <<"evt-1">>, metadata = #{},
        timestamp = 1000, epoch_us = 1000000
    },
    FlatMap = projection_event:to_map(Event),
    ok = thing_initiated_v1_to_sqlite_things:project(FlatMap).
```

### The Lesson

> **Dialyzer catches type bugs. Tests catch logic bugs. An event-sourced domain without tests is a domain you can't safely refactor.**
> **The appstore's tuple/list bug (Demon #19) was found by dialyzer. The next bug won't be.**

---

## 🔥 Missing `/ui/[...]` Cowboy Route in Plugin Daemon

**Demon #29** — 2026-02-24

### What Happened

The snake-duel daemon had a working `/manifest` endpoint and a healthy Unix socket. Its Dockerfile correctly built the SvelteKit frontend and copied `dist/` into `priv/static/`. But the plugin never appeared in hecate-web.

### The Bug

The cowboy route list in the daemon's `_app.erl` had no `/ui/[...]` route. The frontend assets were sitting in `priv/static/component.js` but cowboy never served them. When hecate-web fetched `/ui/component.js` through the Tauri socket proxy, it got a 404. The plugin loading code treats a 404 on the custom element as "plugin doesn't exist" and silently drops it.

### Why It's Insidious

- The daemon was running fine (health OK, manifest OK)
- The socket existed and responded to API calls
- The Dockerfile built and copied the frontend correctly
- The plugin discovery scan found the socket
- Zero errors in logs — the failure is a silent 404 in the browser

### The Fix

Every plugin daemon MUST include this route in its cowboy dispatch:

```erlang
{"/ui/[...]", cowboy_static, {dir, static_dir(), [{mimetypes, cow_mimetypes, all}]}}
```

With the helper:

```erlang
static_dir() ->
    PrivDir = code:priv_dir(my_plugin_app),
    filename:join(PrivDir, "static").
```

### Plugin Daemon Required Endpoints Checklist

| Endpoint | Purpose | Without it |
|----------|---------|-----------|
| `GET /health` | Health check | Plugin marked unhealthy |
| `GET /manifest` | Plugin metadata | Discovery fails with error |
| `GET /ui/[...]` | Frontend custom element | **Plugin silently invisible** |

### The Lesson

> **A plugin with a working daemon and manifest but no `/ui/[...]` route is invisible to hecate-web. The failure is completely silent. Always verify all three required endpoints when creating a new plugin daemon.**

---

*We burned these demons so you don't have to. Keep the fire going.* 🔥🗝️🔥
