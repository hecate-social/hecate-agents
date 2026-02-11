# ANTIPATTERNS.md — Demons We've Exorcised

*Mistakes we've made and corrected. Read this. Don't repeat them.*

---

## 🔥 Technical Names Don't Scream

**Date:** 2026-02-04  
**Origin:** hecate-daemon apprentice

### The Antipattern

Slice names that describe **HOW** (technical implementation) instead of **WHAT** (business intent):

| Technical Name (BAD) | What It Actually Does |
|---------------------|----------------------|
| `llm_model_poller` | Detects what models Ollama has |
| `llm_rpc_listener` | Handles incoming LLM requests from mesh |
| `*_listener` | Technical concern, not business intent |
| `*_poller` | Technical concern, not business intent |
| `*_handler` | Technical concern, not business intent |
| `*_worker` | Technical concern, not business intent |

These names are **horizontal thinking in disguise**. They describe the mechanism, not the meaning.

### The Rule

> **Slice names must be VERB PHRASES that scream WHAT the slice does, not HOW it does it.**

| ❌ Technical (HOW) | ✅ Screaming (WHAT) |
|-------------------|---------------------|
| `poll_llm_models/` | `detect_llm_models/` |
| `handle_llm_rpc/` | `listen_for_llm_request/` |
| `capability_listener/` | `discover_remote_capability/` |
| `event_handler/` | `on_capability_announced_update_index/` |

### Why It Matters

A stranger reading your folder structure should understand **what your system does**, not **what frameworks you used**.

```
# BAD — I see plumbing
apps/serve_llm/src/
├── poll_llm_models/
├── handle_llm_rpc/
└── emit_llm_events/

# GOOD — I see capabilities  
apps/serve_llm/src/
├── detect_llm_models/
├── listen_for_llm_request/
└── announce_llm_availability/
```

### The Test

Read your slice name aloud. If it sounds like infrastructure, rename it.

- "This slice *polls models*" → Infrastructure. ❌
- "This slice *detects available LLM models*" → Business. ✅

---

## 🔥 Parallel Domain Infrastructure

**Date:** 2026-02-04  
**Origin:** hecate-daemon apprentice

### The Antipattern

Creating duplicate command/event/emitter infrastructure in a new domain when an existing domain already handles that concept.

**Example:** `serve_llm` created:
- `announce_llm_capability_v1` command
- `llm_capability_announced_v1` event  
- `llm_capability_announced_v1_to_mesh` emitter
- `hecate.llm.announced` mesh topic

But `manage_capabilities` already has:
- `announce_capability_v1` command
- `capability_announced_v1` event
- `capability_announced_v1_to_mesh` emitter
- `hecate.capability.announced` mesh topic

LLM capabilities ARE just capabilities with `type = <<"llm">>`.

### The Rule

> **Don't duplicate domain concepts. Extend existing domains or use Process Managers to integrate.**

### The Solution: Process Managers

When Domain A needs to trigger behavior in Domain B:

```
Domain A (serve_llm)
    ↓ emits internal event (llm_model_detected_v1)

Process Manager (on_llm_model_detected_announce_capability)
    ↓ subscribes to Domain A events
    ↓ dispatches command to Domain B

Domain B (manage_capabilities)
    ↓ handles command normally
    ↓ existing infrastructure does the rest
```

**Loose coupling. Single source of truth. No duplication.**

---

## 🔥 Incomplete Spokes / Flat Workers

**Date:** 2026-02-04  
**Origin:** hecate-daemon architecture review

### The Antipattern

CMD slices that are missing components or have workers directly supervised by the domain supervisor.

**Symptoms:**
```erlang
%% BAD: Domain sup directly supervises workers
manage_capabilities_sup
├── capability_announced_v1_to_mesh   % Worker — WRONG LEVEL
├── remote_capabilities_listener      % Worker — WRONG LEVEL
└── ...
```

**Missing pieces:**
- No spoke supervisor (`*_spoke_sup.erl`)
- No responder (`*_responder_v1.erl`) — can't receive HOPEs from mesh
- Emitters exist but float orphaned at domain level

### The Rule

> **Domain supervisors ONLY start spoke supervisors + shared infra.**
> **Spoke supervisors start all workers for that spoke.**

```erlang
%% GOOD: Domain sup → Spoke sups → Workers
manage_capabilities_sup
├── manage_capabilities_store         % Shared infra (OK at domain level)
├── announce_capability_spoke_sup     % Supervisor
│   ├── announce_capability_responder_v1    % Worker
│   └── capability_announced_v1_to_mesh     % Worker
├── update_capability_spoke_sup       % Supervisor
│   └── ...
└── retract_capability_spoke_sup      % Supervisor
    └── ...
```

### Complete Spoke Checklist

Every CMD spoke MUST have:
- [ ] `*_spoke_sup.erl` — Spoke supervisor
- [ ] `*_v1.erl` — Command record
- [ ] `*_v1.erl` — Event record  
- [ ] `maybe_*.erl` — Handler
- [ ] `*_responder_v1.erl` — HOPE → Command (mesh inbound)
- [ ] `*_to_mesh.erl` — Event → FACT emitter (mesh outbound)

Optional:
- [ ] `on_{event}_maybe_*.erl` — Policy/PM for cross-domain

### Why It Matters

Without responders, your domain can emit but not receive. You have a mouth but no ears.

Without spoke supervisors, your supervision tree is flat and you lose fault isolation per feature.

See `~/work/github.com/CARTWHEEL.md` for the complete canonical structure.

---

## 🔥 Missing or Wrong "Birth" Event

**Date:** 2026-02-08
**Origin:** Torch/Cartwheel architecture discussion

### The Antipattern

Process/domain aggregates that don't have a clear "birth" event, or use wrong verbs:

| ❌ Wrong | Why |
|----------|-----|
| `project_created_v1` | CRUD — "created" says nothing about business intent |
| `cartwheel_started_v1` | Ambiguous — "started" could mean resumed, begun, etc. |
| `order_made_v1` | Weak — doesn't convey the initiation of a process |
| No birth event at all | Aggregate appears from nowhere |

### The Rule

> **Every process/domain aggregate MUST begin with `{aggregate_singular}_initiated_v{N}`**

This event is the "birth" of the aggregate — it marks the moment the process began.

| ✅ Correct | Aggregate |
|-----------|-----------|
| `torch_initiated_v1` | Torch |
| `cartwheel_initiated_v1` | Cartwheel |
| `order_initiated_v1` | Order |
| `claim_initiated_v1` | Insurance Claim |
| `project_initiated_v1` | Project |

### Why "Initiated"?

- **Business verb** — "We initiated a new project" is natural language
- **Process-oriented** — signals the START of a lifecycle, not just creation of data
- **Consistent** — one word for all aggregates, easy to search/grep
- **Not CRUD** — avoids the banned "created/updated/deleted" vocabulary

### The Pattern

```erlang
%% First event in any aggregate stream
#{
    event_type => <<"torch_initiated_v1">>,
    stream_id => <<"torch-abc123">>,
    data => #{
        torch_id => <<"abc123">>,
        name => <<"macula-geo">>,
        brief => <<"Geo-restriction for compliance">>,
        initiated_by => <<"human:rl">>,
        initiated_at => 1707350400000
    }
}
```

### Checklist

When creating a new aggregate:
- [ ] First event is `{aggregate}_initiated_v1`
- [ ] Event includes `initiated_by` (who/what started it)
- [ ] Event includes `initiated_at` (timestamp)
- [ ] Handler validates "not already initiated" before accepting

---

## 🔥 Auto-Creating Child Aggregates on Parent Initiation

**Date:** 2026-02-08
**Origin:** Torch → Cartwheel architecture discussion

### The Antipattern

Assuming that when a parent aggregate is initiated, child aggregates should be automatically created.

**Example (WRONG):**
```
torch_initiated_v1
    → listener subscribes
    → automatically creates cartwheel
```

This assumes:
- Every torch needs exactly one cartwheel
- The relationship is 1:1 and automatic
- No human/agent decision is needed

**Reality:** A torch might need 0, 1, 5, or 10 cartwheels. This is a deliberate decision, not an automatic consequence.

### The Rule

> **Parent aggregates IDENTIFY children. Child aggregates INITIATE themselves.**

The parent owns the "what exists" decision. The child owns its lifecycle.

### The Correct Flow

```
1. torch_initiated_v1           # Torch exists
2. cartwheel_identified_v1      # Torch decides "I need a cartwheel called X"
   → emitted to mesh
3. cartwheel_initiated_v1       # Cartwheel service starts X's lifecycle
```

### Semantic Distinction

| Action | Owner | Meaning |
|--------|-------|---------|
| **Identify** | Parent (torch) | "This parent will have a child called X" |
| **Initiate** | Child (cartwheel) | "Start the lifecycle of X" |

### Why It Matters

- **Flexibility** — Parent can have 0..N children
- **Explicit decisions** — Humans/agents choose what children exist
- **Proper DDD** — Parent aggregate owns its children's identities
- **Separation of concerns** — Identity vs lifecycle are different responsibilities

### The Lesson

> **Skip DnA phase → Make wrong assumptions → Build wrong architecture → Waste time fixing**

This mistake happened because we jumped to implementation without understanding the domain.

---

## 🔥 Listeners as Separate Spokes

**Date:** 2026-02-08
**Origin:** Cartwheel listener architecture discussion

### The Antipattern

Creating a listener as its own spoke when it only serves one other spoke.

**Example (WRONG):**
```
manage_cartwheels/src/
├── subscribe_to_cartwheel_identified/     # Separate spoke
│   ├── subscribe_to_cartwheel_identified.erl
│   └── subscribe_to_cartwheel_identified_sup.erl
│
└── initiate_cartwheel/                    # The spoke it triggers
    ├── initiate_cartwheel_v1.erl
    └── maybe_initiate_cartwheel.erl
```

This is **horizontal thinking in disguise** — grouping by "listeners" vs "commands".

### The Rule

> **If a listener's sole purpose is to trigger spoke X, it lives IN spoke X.**

### The Correct Structure

```
manage_cartwheels/src/
└── initiate_cartwheel/
    ├── initiate_cartwheel_v1.erl
    ├── cartwheel_initiated_v1.erl
    ├── maybe_initiate_cartwheel.erl
    ├── subscribe_to_cartwheel_identified.erl           # Lives here
    └── on_cartwheel_identified_maybe_initiate_cartwheel.erl  # Lives here
```

The spoke owns **everything needed to initiate cartwheels** — including how it gets triggered.

### When Listeners CAN Be Separate

A listener MAY be its own spoke when:
- It triggers **multiple** different spokes based on message content
- It's truly general-purpose infrastructure (rare)
- It serves a query service, not a command service

### The Test

Ask: "Does this listener exist ONLY to trigger spoke X?"
- **Yes** → Put it in spoke X
- **No** → Consider a separate spoke (but think hard)

---

## 🔥 Using Mesh for Internal Integration

**Date:** 2026-02-08
**Origin:** hecate-daemon walking skeleton debugging

### The Antipattern

Using Macula mesh (`*_to_mesh.erl` emitters) for communication between umbrella apps within the same BEAM VM.

**Example (WRONG):**
```
manage_torches (CMD app)
    → torch_initiated_v1_to_mesh.erl
    → Macula mesh (QUIC, DHT, NAT traversal)
    → on_torch_initiated_from_mesh.erl
    → query_torches (PRJ+QRY app)
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
manage_torches (CMD app)
    → torch_initiated_v1_to_pg.erl    # Internal via pg
    → Direct Erlang message passing
    → on_torch_initiated_v1_from_pg_project_to_sqlite_torches.erl
    → query_torches (PRJ+QRY app)
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

## 🔥 Centralized Listener Supervisors

**Date:** 2026-02-08
**Origin:** hecate-daemon architecture refinement

### The Antipattern

Creating a central supervisor for all listeners across domains.

**Example (WRONG):**
```
apps/hecate_listeners/src/
├── hecate_listeners_sup.erl          # Central supervisor
├── torch_initiated_listener.erl
├── cartwheel_identified_listener.erl
└── capability_announced_listener.erl
```

Or within a domain:
```
apps/manage_cartwheels/src/
├── manage_cartwheels_listeners_sup.erl   # Still wrong!
├── listeners/                             # Horizontal directory
│   ├── cartwheel_identified_listener.erl
│   └── ...
```

### The Rule

> **Each listener belongs to the spoke it triggers, supervised by that spoke's supervisor.**

### The Correct Structure

```
apps/manage_cartwheels/src/
├── initiate_cartwheel/
│   ├── initiate_cartwheel_sup.erl                              # Spoke supervisor
│   └── on_cartwheel_identified_v1_from_pg_maybe_initiate_cartwheel.erl
│
└── complete_cartwheel/
    ├── complete_cartwheel_sup.erl                              # Spoke supervisor
    └── on_all_spokes_implemented_v1_from_pg_maybe_complete_cartwheel.erl
```

### Why It Matters

- **Fault isolation** — Listener crash only affects its spoke
- **Discoverability** — To understand spoke X, look only in `X/`
- **No orphans** — Every listener has a clear owner
- **Vertical slicing** — No horizontal grouping by technical concern

See [INTEGRATION_TRANSPORTS.md](../philosophy/INTEGRATION_TRANSPORTS.md) for spoke structures.

---

## 🔥 Direct Creation Endpoints for Child Aggregates

**Date:** 2026-02-09
**Origin:** Cartwheel API architecture review

### The Antipattern

Exposing a direct "create" or "initiate" API endpoint for child aggregates that should only exist through their parent.

**Example (WRONG):**
```
POST /api/cartwheels/initiate
{
  "name": "My Cartwheel",
  "description": "..."
}
```

This bypasses the domain flow:
- Cartwheels should only exist because a torch identified them
- Direct creation allows orphan cartwheels (no parent torch)
- Violates the parent-child aggregate pattern

### The Rule

> **Child aggregates should NOT have direct creation endpoints. They're created through parent identification.**

### The Correct Flow

1. **Parent identifies child**: `POST /api/torches/:torch_id/cartwheels/identify`
2. **Event emitted**: `cartwheel_identified_v1` to pg (internal) + mesh (external)
3. **Listener receives**: `subscribe_to_cartwheel_identified` in manage_cartwheels
4. **Policy decides**: `on_cartwheel_identified_maybe_initiate_cartwheel`
5. **Child initiated**: `cartwheel_initiated_v1` event created

### When to Use Which

| Aggregate Type | Creation Endpoint? | Why |
|----------------|-------------------|-----|
| **Root aggregate** | ✅ Yes | Torch, Order, User — top-level entities |
| **Child aggregate** | ❌ No | Cartwheel, OrderLine — created via parent |

### API Design Pattern

```erlang
%% GOOD: Parent creates children through relationship
POST /api/torches/:torch_id/cartwheels/identify

%% BAD: Direct creation of child
POST /api/cartwheels/initiate    % REMOVE THIS
```

### Testing Implications

When testing the walking skeleton:
- **Respect the domain flow** — Don't bypass event-driven creation with direct APIs
- **Test the full path** — Parent → Event → Listener → Child
- **If it doesn't work** — Fix the event flow, don't add a shortcut API

### The Lesson

> **API endpoints should reflect domain operations, not CRUD convenience.**

If you need to create test data, use the proper flow or seed the event store directly.

---

## 🔥 Wrong Aggregate Callback Argument Order

**Date:** 2026-02-09
**Origin:** hecate-daemon cartwheel auto-initiation bug

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
**Origin:** TUI vision command architecture review

### The Antipattern

Performing side effects (file I/O, state changes) in the TUI based on a command's HTTP response rather than on a received event (fact).

**Example (WRONG):**
```go
// TUI sends command to daemon
err := client.RefineVision(torchID, params)
if err == nil {
    // WRONG: treating 200 OK as a fact
    writeVisionToDisk()
}
```

The 200 OK means "I received your hope." Not "the vision was refined." Between acknowledgment and event storage, anything can fail.

### The Rule

> **Side effects in external systems (TUI, other agents) must be triggered by received FACTS (events), not by command acknowledgments (hope receipts).**

### The Correct Pattern

```go
// TUI subscribes to event stream
events := client.EventStream(ctx, torchID)

// TUI sends hope (fire and forget the response)
client.RefineVision(torchID, params)  // 202 Accepted

// TUI reacts to fact
for event := range events {
    if event.Type == "vision_refined_v1" {
        writeVisionToDisk()  // NOW it's safe
    }
}
```

### Why It Matters

- Commands can be rejected by aggregate business rules AFTER acknowledgment
- Event store writes can fail
- Async processing means acknowledgment ≠ completion
- The TUI is an **external system** — it must treat the daemon as eventually consistent

### Reference

See [HOPE_FACT_SIDE_EFFECTS.md](HOPE_FACT_SIDE_EFFECTS.md) for the full architectural pattern.

---

## 🔥 Read-Time Status Enrichment

**Date:** 2026-02-10
**Origin:** hecate-daemon torch/cartwheel status handling

### The Antipattern

Computing `status_label` at query time instead of storing it in the read model at projection write time.

**Symptoms:**
```erlang
%% BAD: Query module enriches at read time
list(Opts) ->
    {ok, Rows} = store:query("SELECT * FROM torches"),
    [enrich_status(row_to_map(R)) || R <- Rows].

enrich_status(#{status := Status} = Row) ->
    Label = evoq_bit_flags:to_string(Status, torch_aggregate:flag_map()),
    Row#{status_label => Label}.
```

**Related violations:**
- Magic numbers: `Status = 3` instead of `evoq_bit_flags:set_all(0, [?TORCH_INITIATED, ?TORCH_DNA_ACTIVE])`
- Duplicated flags: `-define(ARCHIVED, 32)` redefined in projections instead of using shared `.hrl`
- Query modules importing aggregate internals (`torch_aggregate:flag_map()`)
- Binary key mismatch: Projections match `#{torch_id := ...}` but events arrive with `<<"torch_id">>` keys

### The Rule

> **Read models store DENORMALIZED data. Compute everything at write time (projection), never at read time (query).**

### The Correct Pattern

**1. Extract flag macros to `.hrl` header in CMD app:**
```erlang
%% apps/manage_torches/include/torch_status.hrl
-define(TORCH_INITIATED,   1).
-define(TORCH_DNA_ACTIVE,  2).
-define(TORCH_ARCHIVED,   32).

-define(TORCH_FLAG_MAP, #{
    0                  => <<"New">>,
    ?TORCH_INITIATED   => <<"Initiated">>,
    ?TORCH_DNA_ACTIVE  => <<"Discovering">>,
    ?TORCH_ARCHIVED    => <<"Archived">>
}).
```

**2. Projection computes and stores `status_label` at write time:**
```erlang
-include_lib("manage_torches/include/torch_status.hrl").

project(Event) ->
    TorchId = get(torch_id, Event),
    Status = evoq_bit_flags:set_all(0, [?TORCH_INITIATED, ?TORCH_DNA_ACTIVE]),
    Label = evoq_bit_flags:to_string(Status, ?TORCH_FLAG_MAP),
    store:execute(
        "INSERT INTO torches (torch_id, status, status_label) VALUES (?1, ?2, ?3)",
        [TorchId, Status, Label]).
```

**3. Query module reads `status_label` directly — no enrichment:**
```erlang
list(Opts) ->
    {ok, Rows} = store:query("SELECT torch_id, status, status_label FROM torches"),
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

## 🔥 Ambiguous Query Module Names

**Date:** 2026-02-10
**Origin:** hecate-daemon get_torch/list_torches rename

### The Antipattern

Query modules with vague names that don't scream their intent or hide scaling dangers.

**Symptoms:**
```
apps/query_torches/src/
├── get_torch/          # Get by what? ID? Name? Status?
├── list_torches/       # Returns ALL torches? Unbounded!
└── get_all_cartwheels/ # "All" is a scaling time bomb
```

**Related violations:**
- `get_{aggregate}` without specifying lookup strategy
- `list_{aggregates}` / `get_all_{aggregates}` returning unbounded result sets
- No pagination in list queries — works in dev, crashes in production

### The Rule

> **1. Single lookups MUST specify the strategy: `get_{aggregate}_by_id`, `get_{aggregate}_by_name`**
> **2. List queries MUST be paged: `get_{aggregates}_page` — NEVER `list_{aggregates}`**

### The Correct Names

| Anti-Pattern | Correct | Why |
|-------------|---------|-----|
| `get_torch` | `get_torch_by_id` | Specifies lookup strategy |
| `list_torches` | `get_torches_page` | "page" enforces bounded results |
| `get_all_cartwheels` | `get_cartwheels_page` | No unbounded queries |
| `find_torches` | `search_torches` or `get_torches_page` | "find" is vague |

### The Correct Structure

```
apps/query_torches/src/
├── get_torch_by_id/        # One torch by primary key
├── get_active_torch/       # The currently active torch
├── get_torches_page/       # Bounded page of torches
└── search_torches/         # Full-text search (also paged)
```

### Why It Matters

- **Scaling**: `list_torches` returning 10,000 rows will kill the daemon
- **Screaming architecture**: A stranger knows exactly what each module does
- **Extensibility**: Adding `get_torch_by_name` later doesn't conflict
- **Client expectations**: "page" in the name tells clients to expect pagination metadata

### The Lesson

> **Query names ARE the API contract. If the name doesn't scream "bounded" and "specific", the query is dangerous.**

Reference: `skills/codegen/erlang/CODEGEN_ERLANG_NAMING.md`

---

## Demon 14: God Module API Handlers

**Date exorcised:** 2026-02-10
**Where it appeared:** `apps/hecate_api/src/hecate_api_*.erl`
**Cost:** 137-file refactoring to fix

### The Demon

Putting all API endpoints for a domain in a single file with multiple `init/2` clauses:

```erlang
❌ WRONG: God module with 16 init/2 clauses
-module(hecate_api_mentors).
-export([init/2]).

init(Req0, [submit]) -> handle_submit(Req0);
init(Req0, [list_learnings]) -> handle_list_learnings(Req0);
init(Req0, [get_learning]) -> handle_get_learning(Req0);
init(Req0, [validate]) -> handle_validate(Req0);
init(Req0, [reject]) -> handle_reject(Req0);
init(Req0, [endorse]) -> handle_endorse(Req0);
%% ... 10 more clauses, 289 lines total
```

### Why It's Wrong

- **Horizontal grouping** — groups by "all mentors HTTP stuff" instead of by business operation
- **Violates vertical slicing** — the API handler is separated from the command/event/handler it serves
- **Growing forever** — every new endpoint adds to the same file
- **Hard to find** — `handle_validate` could be anything; you must read the whole file
- **Duplicated helpers** — each god module reinvents `dispatch_result/3`, `error_response/3`

### The Correct Pattern

Each spoke owns its API handler:

```erlang
✅ CORRECT: Handler lives in its spoke
apps/mentor_agents/src/validate_learning/
├── validate_learning_v1.erl
├── learning_validated_v1.erl
├── maybe_validate_learning.erl
└── validate_learning_api.erl    # ~30 lines, single-purpose
```

### The Lesson

> **API handlers are part of the spoke, not part of the API app.**
> One endpoint = one `*_api.erl` file in the spoke directory.
> The routes file (`hecate_api_routes.erl`) just maps URLs to spoke handlers.

### How This Was Fixed

Replaced 11 god modules (1,700+ lines) with 50 spoke-based handlers (~30-50 lines each).
All handlers use `hecate_api_utils` from the `shared` app for response helpers.
Routes standardized under `/api/` prefix.

Reference: `skills/codegen/erlang/CODEGEN_ERLANG_TEMPLATES.md` → API Handler Templates

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

*Add more demons as we exorcise them.* 🔥🗝️🔥
