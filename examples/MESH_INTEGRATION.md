# Example: Mesh Integration Patterns

*Canonical example: FACTS vs EVENTS, Emitters and Listeners*

---

## The Pattern

The mesh is a distributed communication layer between agents. It is **NOT** an event bus.

| Term | What It Is | Where It Lives |
|------|-----------|----------------|
| **FACT** | External truth published to mesh | Between agents |
| **EVENT** | Internal domain event (what happened) | Within an agent's event store |
| **COMMAND** | Intention (what should happen) | Within agent |

**Critical distinction:**

- **Events** are internal. They belong to an aggregate. They are stored in your event store.
- **Facts** are external. They are published to topics. Other agents receive them.

Treating mesh facts as events (or vice versa) breaks your architecture.

---

## Wrong Way: Bypassing the Command Layer

```
Mesh FACT → Subscriber → Projection (WRONG!)
```

This antipattern:
- Treats external facts as internal events
- Bypasses aggregate validation
- No command audit trail
- Projection state can diverge from event store
- Loses single source of truth

```erlang
%% ❌ WRONG: Direct projection from mesh message
handle_info({mesh_fact, Topic, FactData}, State) ->
    %% NO! Don't project directly from mesh facts!
    capability_projection:project(FactData),
    {noreply, State}.
```

---

## Correct Way: Full LISTENER → COMMAND → AGGREGATE Flow

### Receiving Facts (LISTENER)

```
Mesh FACT → LISTENER → converts to → COMMAND → AGGREGATE → DOMAIN EVENT → stored → projected
```

The listener's ONLY job is to convert external facts into internal commands.

### Publishing Facts (EMITTER)

```
DOMAIN EVENT → EMITTER → converts to → FACT → Mesh
```

The emitter's ONLY job is to convert internal events into external facts.

---

## Emitter Code Example

From `apps/manage_torches/src/identify_cartwheel/cartwheel_identified_v1_to_mesh.erl`:

```erlang
%%% @doc Emitter: Publish cartwheel_identified_v1 events to mesh
%%%
%%% When a cartwheel is identified within a torch, this emitter publishes
%%% the fact to mesh topic `hecate.torch.cartwheel_identified`.
%%%
%%% The manage_cartwheels service subscribes to this topic and initiates
%%% the cartwheel's lifecycle.
%%%
%%% Flow: cartwheel_identified_v1 (event) -> emitter -> mesh fact
%%% @end
-module(cartwheel_identified_v1_to_mesh).
-behaviour(gen_server).

-export([start_link/0, emit/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TOPIC, <<"hecate.torch.cartwheel_identified">>).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Emit a cartwheel_identified event to mesh
-spec emit(map() | cartwheel_identified_v1:cartwheel_identified_v1()) -> ok.
emit(Event) when is_map(Event) ->
    gen_server:cast(?MODULE, {emit, Event});
emit(Event) ->
    gen_server:cast(?MODULE, {emit, cartwheel_identified_v1:to_map(Event)}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    logger:info("[cartwheel_identified_v1_to_mesh] Starting emitter for topic ~s", [?TOPIC]),
    {ok, #state{}}.

handle_cast({emit, EventData}, State) ->
    do_emit(EventData),
    {noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

do_emit(EventData) ->
    TorchId = maps:get(<<"torch_id">>, EventData, maps:get(torch_id, EventData, undefined)),
    CartwheelId = maps:get(<<"cartwheel_id">>, EventData, maps:get(cartwheel_id, EventData, undefined)),

    logger:debug("[emitter] Publishing cartwheel ~s for torch ~s", [CartwheelId, TorchId]),

    %% Publish FACT to mesh
    case hecate_mesh_client:publish(?TOPIC, EventData) of
        ok ->
            logger:info("[emitter] Published to ~s: cartwheel=~s", [?TOPIC, CartwheelId]);
        {error, not_connected} ->
            logger:warning("[emitter] Mesh not connected, fact not published");
        {error, Reason} ->
            logger:error("[emitter] Failed to publish: ~p", [Reason])
    end.
```

**Key points:**
- Emitter converts domain events to mesh facts
- Uses topic naming: `hecate.{domain}.{event_name}`
- Handles connection failures gracefully
- Does NOT modify the event data (just publishes)

---

## Listener Code Example

From `apps/manage_cartwheels/src/initiate_cartwheel/subscribe_to_cartwheel_identified.erl`:

```erlang
%%% @doc Listener: Subscribe to cartwheel_identified facts from mesh
%%%
%%% Subscribes to mesh topic `hecate.torch.cartwheel_identified`.
%%% When a torch identifies a cartwheel, this listener receives the fact
%%% and forwards it to the policy for processing.
%%%
%%% This listener lives in the initiate_cartwheel spoke because its
%%% sole purpose is to trigger cartwheel initiation.
%%%
%%% Flow: Mesh FACT -> Listener -> Policy -> Command -> Aggregate
%%% @end
-module(subscribe_to_cartwheel_identified).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TOPIC, <<"hecate.torch.cartwheel_identified">>).

-record(state, {
    subscription :: reference() | undefined
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    self() ! subscribe,
    logger:info("[listener] Starting for topic ~s", [?TOPIC]),
    {ok, #state{}}.

handle_info(subscribe, State) ->
    SubRef = subscribe_to_topic(?TOPIC),
    {noreply, State#state{subscription = SubRef}};

handle_info({mesh_fact, ?TOPIC, FactData}, State) ->
    %% Forward to POLICY for processing
    %% Policy will create COMMAND and dispatch to AGGREGATE
    on_cartwheel_identified_maybe_initiate_cartwheel:handle(FactData),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{subscription = SubRef}) ->
    unsubscribe(SubRef),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

subscribe_to_topic(Topic) ->
    case hecate_mesh_client:subscribe(Topic, self()) of
        {ok, SubRef} ->
            logger:info("[listener] Subscribed to ~s", [Topic]),
            SubRef;
        {error, not_connected} ->
            %% Retry subscription in 5 seconds
            logger:warning("[listener] Mesh not connected, retrying in 5s"),
            erlang:send_after(5000, self(), subscribe),
            undefined;
        {error, Reason} ->
            logger:error("[listener] Failed to subscribe: ~p", [Reason]),
            undefined
    end.

unsubscribe(undefined) -> ok;
unsubscribe(SubRef) -> hecate_mesh_client:unsubscribe(SubRef).
```

**Key points:**
- Listener subscribes to mesh topics
- Forwards facts to POLICY (not projection!)
- Lives in the spoke it triggers (vertical slicing)
- Handles reconnection gracefully

---

## Policy Code Example

The policy decides what to do with the fact:

```erlang
%%% @doc Policy: React to cartwheel_identified facts
%%%
%%% Naming convention: on_{source_event}_{action}_{target}
%%% - Source: cartwheel_identified (from manage_torches)
%%% - Action: maybe_initiate (policy decision)
%%% - Target: cartwheel (in manage_cartwheels)
%%% @end
-module(on_cartwheel_identified_maybe_initiate_cartwheel).

-export([handle/1]).

%% @doc Handle a cartwheel_identified fact and potentially initiate the cartwheel
-spec handle(map()) -> ok | {error, term()}.
handle(FactData) ->
    CartwheelId = get_field(cartwheel_id, FactData),
    TorchId = get_field(torch_id, FactData),
    ContextName = get_field(context_name, FactData),
    Description = get_field(description, FactData),

    logger:debug("[policy] Processing cartwheel ~s (~s) from torch ~s",
                [CartwheelId, ContextName, TorchId]),

    %% Policy: Always initiate (future: conditional logic here)
    do_initiate(#{
        cartwheel_id => CartwheelId,
        torch_id => TorchId,
        context_name => ContextName,
        description => Description
    }).

%% @doc Create and dispatch the initiate_cartwheel command
do_initiate(Params) ->
    case initiate_cartwheel_v1:new(Params) of
        {ok, Cmd} ->
            %% Dispatch to handler (which dispatches to aggregate)
            case maybe_initiate_cartwheel:dispatch(Cmd) of
                {ok, _Version, _Events} ->
                    logger:info("[policy] Cartwheel ~s initiated",
                               [maps:get(cartwheel_id, Params)]),
                    ok;
                {error, Reason} = Error ->
                    logger:error("[policy] Failed to initiate: ~p", [Reason]),
                    Error
            end;
        {error, Reason} = Error ->
            logger:error("[policy] Invalid command: ~p", [Reason]),
            Error
    end.
```

---

## Complete Flow Diagram

```
Agent A (manage_torches)                     Agent B (manage_cartwheels)
────────────────────────                     ──────────────────────────

1. User: POST /api/torches/:id/cartwheels/identify
   ↓
2. identify_cartwheel_v1 (COMMAND)
   ↓
3. maybe_identify_cartwheel:handle/1
   ↓
4. cartwheel_identified_v1 (DOMAIN EVENT)
   ↓
5. Stored in Torch's event stream
   ↓
6. cartwheel_identified_v1_to_mesh:emit/1 (EMITTER)
   ↓
═══════════════════════════════════════════════════════════════
                      MESH (topic: hecate.torch.cartwheel_identified)
═══════════════════════════════════════════════════════════════
                                                     ↓
7. subscribe_to_cartwheel_identified (LISTENER)
   ↓
8. on_cartwheel_identified_maybe_initiate_cartwheel:handle/1 (POLICY)
   ↓
9. initiate_cartwheel_v1 (COMMAND)
   ↓
10. maybe_initiate_cartwheel:dispatch/1
    ↓
11. cartwheel_initiated_v1 (DOMAIN EVENT)
    ↓
12. Stored in Cartwheel's event stream
    ↓
13. Projected to read model
```

---

## Spoke Structure

Both emitter and listener live in their respective spokes:

```
manage_torches/src/
└── identify_cartwheel/                     # Spoke owns emitter
    ├── identify_cartwheel_v1.erl           # Command
    ├── cartwheel_identified_v1.erl         # Event
    ├── maybe_identify_cartwheel.erl        # Handler
    └── cartwheel_identified_v1_to_mesh.erl # EMITTER

manage_cartwheels/src/
└── initiate_cartwheel/                     # Spoke owns listener
    ├── initiate_cartwheel_v1.erl           # Command
    ├── cartwheel_initiated_v1.erl          # Event
    ├── maybe_initiate_cartwheel.erl        # Handler
    ├── initiate_cartwheel_spoke_sup.erl    # Supervisor
    ├── subscribe_to_cartwheel_identified.erl                  # LISTENER
    └── on_cartwheel_identified_maybe_initiate_cartwheel.erl   # POLICY
```

---

## Naming Conventions

| Component | Pattern | Example |
|-----------|---------|---------|
| **Topic** | `{namespace}.{domain}.{fact_name}` | `hecate.torch.cartwheel_identified` |
| **Emitter** | `{event}_to_mesh` | `cartwheel_identified_v1_to_mesh` |
| **Listener** | `subscribe_to_{fact}` | `subscribe_to_cartwheel_identified` |
| **Policy** | `on_{fact}_{action}_{target}` | `on_cartwheel_identified_maybe_initiate_cartwheel` |

---

## What NOT To Do

| Antipattern | Why It's Wrong | Correct Approach |
|-------------|----------------|------------------|
| Mesh fact → Projection | Bypasses aggregate | Fact → Command → Event → Projection |
| Domain event → Mesh directly | No transform layer | Event → Emitter → Fact → Mesh |
| Listener in `listeners/` folder | Horizontal thinking | Listener in spoke it triggers |
| Central mesh subscriber | God module | Each domain owns its listeners |
| Treating facts as events | Different concepts | Facts external, events internal |

---

## Key Takeaways

1. **FACTS are NOT EVENTS** - External communication uses facts, internal storage uses events
2. **Emitters convert events to facts** - Controlled publication to mesh
3. **Listeners convert facts to commands** - Entry point to command layer
4. **Policy contains the "maybe"** - Business logic for conditional actions
5. **Single source of truth** - Projections read from event store, not mesh
6. **Vertical slicing** - Emitters and listeners live in their spokes
7. **Graceful degradation** - Handle mesh disconnection without crashing

---

## Training Note

This example teaches:
- The fundamental distinction between FACTS and EVENTS
- How EMITTERS publish internal events as external facts
- How LISTENERS receive external facts and dispatch internal commands
- Why bypassing the command layer is an antipattern
- Correct placement of mesh integration components (vertical slicing)
- Policy/process manager patterns for cross-domain integration

*Date: 2026-02-08*
*Origin: Hecate daemon mesh integration architecture*
