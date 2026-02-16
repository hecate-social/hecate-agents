# Example: Process Manager Patterns

*Canonical example: Cross-domain coordination without tight coupling*

> **Note:** This document has been updated to use current terminology (2026-02-10).
> `torch` -> `venture`, `cartwheel` -> `division`, `spoke` -> `desk`.

---

## The Pattern

A **Process Manager** (also called Policy or Saga) coordinates actions across domain boundaries:

1. **Subscribes** to events from a source domain
2. **Makes decisions** about what should happen next
3. **Dispatches commands** to a target domain

This enables:
- **Loose coupling** between domains
- **Explicit integration points** (easy to find and reason about)
- **Testable domains** (each domain can be tested in isolation)

---

## The Problem

When Domain A needs to trigger actions in Domain B, the naive approach is:

```
Domain A event → Domain A handler → DIRECT CALL to Domain B
```

This creates **tight coupling**. Domain A must know:
- Domain B's command structure
- Domain B's aggregate interface
- How to construct valid commands for Domain B

---

## Wrong Way (Direct Cross-Domain Calls)

```erlang
%% In discover_divisions/src/discover_division/maybe_discover_division.erl
%% ❌ WRONG: Handler directly calls another domain

handle(Cmd) ->
    %% Handle our domain event
    Event = division_discovered_v1:new(Cmd),
    ok = store_event(Event),

    %% ❌ WRONG: Direct call to another domain!
    DivisionCmd = initiate_division_v1:new(#{
        division_id => Event#division_discovered_v1.division_id,
        venture_id => Event#division_discovered_v1.venture_id
    }),
    maybe_initiate_division:handle(DivisionCmd),  %% ❌ TIGHT COUPLING

    {ok, [Event]}.
```

**Problems:**

| Issue | Impact |
|-------|--------|
| Domain A knows Domain B's API | Changes to B require changes to A |
| Can't test A without B | Slower, more complex tests |
| Circular dependencies possible | Compilation errors, confusion |
| Hidden integration points | Hard to understand data flow |
| No policy decisions | Always creates division, no conditions |

---

## Correct Way (Process Manager)

```
Domain A (discover_divisions)
    ↓ stores division_discovered_v1 event
    ↓ emitter publishes fact to mesh (optional)

Process Manager (on_division_discovered_maybe_initiate_division)
    ↓ subscribes to division_discovered facts
    ↓ policy: decides IF and HOW to initiate
    ↓ creates initiate_division_v1 command
    ↓ dispatches to target aggregate

Domain B (design_division)
    ↓ receives command
    ↓ stores division_initiated_v1 event
```

**Benefits:**

| Benefit | Why |
|---------|-----|
| Loose coupling | Domains don't know about each other |
| Clear integration points | PM is the only connection |
| Testable in isolation | Mock the PM's dependencies |
| Policy decisions explicit | "maybe" in the name = conditional logic |
| Easy to find | Naming convention reveals purpose |

---

## Naming Convention

```
on_{source_event}_{action}_{target}
```

| Component | Meaning | Example |
|-----------|---------|---------|
| `on_` | Triggered by | Prefix |
| `{source_event}` | What happened | `division_discovered` |
| `{action}` | What we do | `maybe_initiate` |
| `{target}` | What we affect | `division` |

**Examples:**

| Name | Source | Action | Target |
|------|--------|--------|--------|
| `on_division_discovered_maybe_initiate_division` | division_discovered | maybe_initiate | division |
| `on_user_registered_send_welcome_email` | user_registered | send | welcome_email |
| `on_order_placed_reserve_inventory` | order_placed | reserve | inventory |
| `on_payment_received_fulfill_order` | payment_received | fulfill | order |
| `on_capability_announced_update_registry` | capability_announced | update | registry |

**"Maybe" indicates policy decision:** The process manager may choose NOT to act based on conditions.

---

## Location Rule

**Process managers live in the TARGET domain** (the domain receiving the command).

```
design_division/src/              # TARGET domain
└── initiate_division/            # Desk for this operation
    ├── initiate_division_v1.erl
    ├── division_initiated_v1.erl
    ├── maybe_initiate_division.erl
    │
    │ # Process manager lives HERE (in target desk)
    ├── initiate_division_desk_sup.erl
    ├── subscribe_to_division_discovered.erl          # Listener
    └── on_division_discovered_maybe_initiate_division.erl  # PM
```

**Why target domain?**
- PM needs to know how to construct target commands
- PM is a consumer of source events, producer of target commands
- Keeps target domain self-contained

---

## Complete Code Example

### 1. The Listener (Subscribes to Source Events)

```erlang
%%% @doc Listener: Subscribe to division_discovered facts from mesh
%%%
%%% Subscribes to mesh topic `hecate.venture.division_discovered`.
%%% When a venture discovers a division, this listener receives the fact
%%% and forwards it to the policy for processing.
%%%
%%% Flow: Mesh FACT -> Listener -> Policy -> Command -> Aggregate
%%% @end
-module(subscribe_to_division_discovered).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TOPIC, <<"hecate.venture.division_discovered">>).

-record(state, {
    subscription :: reference() | undefined
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    self() ! subscribe,
    logger:info("[listener] Starting for topic ~s", [?TOPIC]),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Subscribe to mesh topic on startup
handle_info(subscribe, State) ->
    SubRef = subscribe_to_topic(?TOPIC),
    {noreply, State#state{subscription = SubRef}};

%% Receive fact from mesh and forward to process manager
handle_info({mesh_fact, ?TOPIC, FactData}, State) ->
    on_division_discovered_maybe_initiate_division:handle(FactData),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{subscription = SubRef}) ->
    unsubscribe(SubRef),
    ok.

subscribe_to_topic(Topic) ->
    case hecate_mesh_client:subscribe(Topic, self()) of
        {ok, SubRef} ->
            logger:info("[listener] Subscribed to ~s", [Topic]),
            SubRef;
        {error, not_connected} ->
            erlang:send_after(5000, self(), subscribe),
            undefined;
        {error, Reason} ->
            logger:error("[listener] Subscribe failed: ~p", [Reason]),
            undefined
    end.

unsubscribe(undefined) -> ok;
unsubscribe(SubRef) -> hecate_mesh_client:unsubscribe(SubRef).
```

### 2. The Process Manager (Makes Decisions, Dispatches Commands)

```erlang
%%% @doc Policy/Process Manager: React to division_discovered facts
%%%
%%% When a venture discovers a division (bounded context), this policy
%%% decides whether and how to initiate that division.
%%%
%%% Naming convention: on_{source_event}_{action}_{target}
%%% - Source: division_discovered (from discover_divisions)
%%% - Action: maybe_initiate (policy decision)
%%% - Target: division (in design_division)
%%%
%%% Current policy: Always initiate a division with the same name.
%%% Future policies may:
%%% - Skip initiation based on division type
%%% - Create with different configurations
%%% - Wait for additional conditions
%%%
%%% @end
-module(on_division_discovered_maybe_initiate_division).

-export([handle/1]).

%% @doc Handle a division_discovered fact and potentially initiate the division
-spec handle(map()) -> ok | {error, term()}.
handle(FactData) ->
    VentureId = get_field(venture_id, FactData),
    DivisionId = get_field(division_id, FactData),
    ContextName = get_field(context_name, FactData),
    Description = get_field(description, FactData),

    logger:debug("[policy] Processing division ~s (~s) from venture ~s",
                [DivisionId, ContextName, VentureId]),

    %% Policy: Always initiate (future: conditional)
    do_initiate(#{
        division_id => DivisionId,
        venture_id => VentureId,
        context_name => ContextName,
        description => Description
    }).

%% @doc Create and dispatch the initiate_division command
-spec do_initiate(map()) -> ok | {error, term()}.
do_initiate(Params) ->
    Result = create_and_dispatch(Params),
    log_result(maps:get(division_id, Params), Result),
    Result.

create_and_dispatch(Params) ->
    with_command(initiate_division_v1:new(Params)).

with_command({ok, Cmd}) ->
    dispatch(maybe_initiate_division:dispatch(Cmd));
with_command({error, _} = Error) ->
    Error.

dispatch({ok, _Version, _Events}) -> ok;
dispatch({error, _} = Error) -> Error.

log_result(DivisionId, ok) ->
    logger:info("[policy] Division ~s initiated", [DivisionId]);
log_result(DivisionId, {error, Reason}) ->
    logger:error("[policy] Failed to initiate division ~s: ~p", [DivisionId, Reason]).

%% @private Get field from map supporting both atom and binary keys
get_field(Key, Map) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    maps:get(Key, Map, maps:get(BinKey, Map, undefined)).
```

### 3. The Desk Supervisor (Owns Listener + PM)

```erlang
%%% @doc Desk supervisor for initiate_division
%%%
%%% Supervises:
%%% - subscribe_to_division_discovered (listener)
%%% - Workers as needed
%%%
%%% The process manager module doesn't need supervision as it's
%%% stateless - called synchronously by the listener.
%%% @end
-module(initiate_division_desk_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{
            id => subscribe_to_division_discovered,
            start => {subscribe_to_division_discovered, start_link, []},
            restart => permanent,
            type => worker
        }
    ],
    {ok, {{one_for_one, 5, 10}, Children}}.
```

---

## Process Manager with Conditional Logic

```erlang
%% Example: Only initiate for certain division types

handle(FactData) ->
    ContextName = get_field(context_name, FactData),
    case should_auto_initiate(ContextName) of
        true ->
            do_initiate(extract_params(FactData));
        false ->
            logger:info("[policy] Skipping auto-initiation for ~s", [ContextName]),
            ok
    end.

should_auto_initiate(<<"authentication", _/binary>>) -> true;
should_auto_initiate(<<"core_", _/binary>>) -> true;
should_auto_initiate(_) -> false.  %% Manual initiation required
```

---

## Anti-Patterns to Avoid

| Anti-Pattern | Why It's Wrong | Correct Approach |
|--------------|----------------|------------------|
| Handler calls other domain | Tight coupling | Use Process Manager |
| PM in source domain | Source knows too much about target | PM in target domain |
| PM without "maybe" in name | Hides conditional nature | Include "maybe" if conditional |
| Direct event passing | Bypasses command validation | Create proper command |
| Global event bus subscription | Hidden dependencies | Explicit listener in desk |

---

## Supervision Hierarchy

```
design_division_sup (domain supervisor)
├── initiate_division_desk_sup
│   └── subscribe_to_division_discovered (listener worker)
│       └── calls on_division_discovered_maybe_initiate_division:handle/1
├── another_desk_sup
│   └── ...
└── ...
```

**The PM module is stateless** - it's called synchronously by the listener and doesn't need its own process.

---

## Flow Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                   DOMAIN A (discover_divisions)                  │
│                                                                 │
│  Command: discover_division_v1                                  │
│      ↓                                                          │
│  Handler: maybe_discover_division                               │
│      ↓                                                          │
│  Event: division_discovered_v1 → stored in Venture stream       │
│      ↓                                                          │
│  Emitter: division_discovered_v1_to_mesh → publishes FACT       │
└─────────────────────────────────────────────────────────────────┘
                                ↓
                    ════════════════════════════
                         MESH (loose coupling)
                         Topic: hecate.venture.division_discovered
                    ════════════════════════════
                                ↓
┌─────────────────────────────────────────────────────────────────┐
│                   DOMAIN B (design_division)                    │
│                                                                 │
│  Listener: subscribe_to_division_discovered                     │
│      ↓ receives FACT                                            │
│  Process Manager: on_division_discovered_maybe_initiate         │
│      ↓ policy decision + creates command                        │
│  Command: initiate_division_v1                                  │
│      ↓                                                          │
│  Handler: maybe_initiate_division                               │
│      ↓                                                          │
│  Event: division_initiated_v1 → stored in Division stream       │
└─────────────────────────────────────────────────────────────────┘
```

---

## Key Takeaways

1. **Domains don't call each other directly** - Process managers bridge the gap
2. **PM lives in TARGET domain** - It knows how to construct target commands
3. **Naming convention reveals purpose** - `on_{event}_{action}_{target}`
4. **"Maybe" indicates policy** - The PM can choose not to act
5. **Listener + PM in same desk** - Vertical slicing applies
6. **PM is stateless** - Called by listener, no process needed
7. **Loose coupling enables testing** - Each domain testable in isolation

---

## When to Use Process Managers

| Scenario | Use PM? | Reason |
|----------|---------|--------|
| Domain A event triggers Domain B | Yes | Cross-domain coordination |
| Same-domain event triggers action | No | Handler can call directly |
| Complex multi-step workflow | Yes | Saga pattern |
| Conditional cross-domain action | Yes | Policy decisions |
| Simple 1:1 mapping | Yes | Still provides loose coupling |

---

## Training Note

This example teaches:
- Process Manager / Policy / Saga pattern
- Cross-domain coordination without coupling
- Naming conventions for process managers
- Location rule (PM in target domain)
- Vertical slicing of PM + Listener
- Flow from source event to target command

*Date: 2026-02-08*
*Origin: Hecate Venture → Division integration*
