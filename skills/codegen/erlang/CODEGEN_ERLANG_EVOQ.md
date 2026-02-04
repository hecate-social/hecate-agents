# CODEGEN.md — Deterministic Code Generation Templates

_Strict templates for generating Cartwheel architecture code. No AI creativity needed._

**Target:** Erlang/OTP with `reckon_evoq`

---

## Variables

Templates use these placeholders:

| Variable          | Example                | Description                    |
| ----------------- | ---------------------- | ------------------------------ |
| `{domain}`        | `manage_capabilities`  | Domain app name                |
| `{command}`       | `announce_capability`  | Command/spoke name (verb_noun) |
| `{event}`         | `capability_announced` | Event name (noun_past_verb)    |
| `{read_store}`    | `capabilities`         | Read model read_store name     |
| `{query}`         | `find_capability`      | Query name                     |
| `{trigger_event}` | `llm_model_detected`   | Event that triggers a policy   |

---

## Directory Structure

### CMD Domain App (WRITE EVENTS)

```
apps/{domain}/
├── src/
│   ├── {domain}_app.erl
│   ├── {domain}_sup.erl
│   ├── {domain}_store.erl              # ReckonDB instance
│   │
│   └── {command}/                      # SPOKE directory
│       ├── {command}_spoke_sup.erl
│       ├── {command}_v1.erl
│       ├── {event}_v1.erl
│       ├── maybe_{command}.erl
│       ├── {command}_responder_v1.erl
│       ├── {event}_to_mesh.erl
│       └── on_{trigger_event}_maybe_{command}.erl  # (optional PM)
│
└── rebar.config
```

### PRJ Domain App

```
apps/query_{domain_noun}/
├── src/
│   ├── query_{domain_noun}_app.erl
│   ├── query_{domain_noun}_sup.erl
│   ├── query_{domain_noun}_store.erl   # SQLite instance
│   │
│   └── {event}_to_{read_store}/             # PRJ SPOKE directory
│       ├── {event}_to_{read_store}_sup.erl
│       └── {event}_to_{read_store}.erl
│
└── rebar.config
```

### QRY (inside PRJ app)

```
apps/query_{domain_noun}/
└── src/
    ├── query_{domain_noun}.erl         # Provider (public API)
    │
    └── {query}/                        # QRY SPOKE directory
        └── {query}.erl
```

---

## CMD Templates

### {domain}\_app.erl

```erlang
-module({domain}_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {domain}_sup:start_link().

stop(_State) ->
    ok.
```

### {domain}\_sup.erl

```erlang
-module({domain}_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        %% Shared infrastructure (optional)
        % #{id => {domain}_store,
        %   start => {{domain}_store, start_link, []},
        %   type => worker},

        %% Spokes (ADD SPOKE SUPERVISORS HERE)
        #{id => {command}_spoke_sup,
          start => {{command}_spoke_sup, start_link, []},
          type => supervisor}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

### {command}\_spoke_sup.erl

```erlang
-module({command}_spoke_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        %% Frontdesk: HOPE responder
        #{id => {command}_responder_v1,
          start => {{command}_responder_v1, start_link, []},
          type => worker},

        %% Backoffice: Emitter (PRJ filer to mesh)
        #{id => {event}_to_mesh,
          start => {{event}_to_mesh, start_link, []},
          type => worker}

        %% Optional: Policy/PM workers
        % #{id => on_{trigger_event}_maybe_{command},
        %   start => {on_{trigger_event}_maybe_{command}, start_link, []},
        %   type => worker}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

### {command}\_v1.erl (Command Record)

```erlang
-module({command}_v1).

-export([new/1, new/2, to_map/1, from_map/1]).
-export([stream_id/1]).
%% Getters
-export([{field1}/1, {field2}/1]).

-record({command}_v1, {
    {field1} :: binary(),
    {field2} :: binary(),
    metadata = #{} :: map()
}).

-opaque t() :: #{{command}_v1{}}.
-export_type([t/0]).

%% Constructor
new(#{field1} := Field1, {field2} := Field2}) ->
    #{{command}_v1{
        {field1} = Field1,
        {field2} = Field2
    }}.

new(Field1, Field2) ->
    #{{command}_v1{
        {field1} = Field1,
        {field2} = Field2
    }}.

%% Stream ID for this dossier
stream_id(#{{command}_v1{{field1} = Field1}}) ->
    <<"{domain_noun}-", Field1/binary>>.

%% Serialization
to_map(#{{command}_v1{} = Cmd) ->
    #{
        {field1} => Cmd#{{command}_v1.{field1},
        {field2} => Cmd#{{command}_v1.{field2},
        metadata => Cmd#{{command}_v1.metadata
    }.

from_map(#{<<"{field1}">> := Field1, <<"{field2}">> := Field2} = Map) ->
    #{{command}_v1{
        {field1} = Field1,
        {field2} = Field2,
        metadata = maps:get(<<"metadata">>, Map, #{})
    }}.

%% Getters
{field1}(#{{command}_v1{{field1} = V}}) -> V.
{field2}(#{{command}_v1{{field2} = V}}) -> V.
```

### {event}\_v1.erl (Event Record)

```erlang
-module({event}_v1).

-export([new/1, from_command/1, to_map/1, from_map/1]).
-export([event_type/0]).
%% Getters
-export([{field1}/1, {field2}/1, timestamp/1]).

-record({event}_v1, {
    {field1} :: binary(),
    {field2} :: binary(),
    timestamp :: integer()
}).

-opaque t() :: #{{event}_v1{}}.
-export_type([t/0]).

%% Event type identifier
event_type() -> <<"{event}_v1">>.

%% Constructor from command
from_command(Cmd) ->
    #{{event}_v1{
        {field1} = {command}_v1:{field1}(Cmd),
        {field2} = {command}_v1:{field2}(Cmd),
        timestamp = erlang:system_time(millisecond)
    }}.

new(#{field1} := Field1, {field2} := Field2}) ->
    #{{event}_v1{
        {field1} = Field1,
        {field2} = Field2,
        timestamp = erlang:system_time(millisecond)
    }}.

%% Serialization
to_map(#{{event}_v1{} = E) ->
    #{
        {field1} => E#{{event}_v1.{field1},
        {field2} => E#{{event}_v1.{field2},
        timestamp => E#{{event}_v1.timestamp
    }.

from_map(#{<<"{field1}">> := Field1, <<"{field2}">> := Field2} = Map) ->
    #{{event}_v1{
        {field1} = Field1,
        {field2} = Field2,
        timestamp = maps:get(<<"timestamp">>, Map, 0)
    }}.

%% Getters
{field1}(#{{event}_v1{{field1} = V}}) -> V.
{field2}(#{{event}_v1{{field2} = V}}) -> V.
timestamp(#{{event}_v1{timestamp = V}}) -> V.
```

### maybe\_{command}.erl (Handler)

```erlang
-module(maybe_{command}).

-export([handle/1, handle/2, dispatch/1]).

-include_lib("kernel/include/logger.hrl").

%% Entry point: dispatch command through evoq
dispatch(Cmd) ->
    StreamId = {command}_v1:stream_id(Cmd),
    case handle(Cmd) of
        {ok, Event} ->
            %% Persist event to store
            EventMap = {event}_v1:to_map(Event),
            EventType = {event}_v1:event_type(),
            ok = reckon_evoq:append({domain}_store, StreamId, EventType, EventMap),
            {ok, Event};
        {error, _} = Error ->
            Error
    end.

%% Handle command (stateless validation)
handle(Cmd) ->
    case validate(Cmd) of
        ok ->
            Event = {event}_v1:from_command(Cmd),
            {ok, Event};
        {error, _} = Error ->
            Error
    end.

%% Handle with aggregate state (for invariant checks)
handle(Cmd, AggregateState) ->
    case can_execute(Cmd, AggregateState) of
        true ->
            handle(Cmd);
        {false, Reason} ->
            {error, Reason}
    end.

%% Validation
validate(Cmd) ->
    Field1 = {command}_v1:{field1}(Cmd),
    case byte_size(Field1) > 0 of
        true -> ok;
        false -> {error, {field1}_required}
    end.

%% Business rule check against current state
can_execute(_Cmd, _State) ->
    %% TODO: Add business rules
    true.
```

### {command}\_responder_v1.erl (Frontdesk - HOPE inbox)

```erlang
-module({command}_responder_v1).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(TOPIC, <<"hecate.{domain_noun}.{command}">>).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Subscribe to HOPE topic on mesh
    ok = hecate_mesh:subscribe(?TOPIC),
    ?LOG_INFO("[~s] Responder started, subscribed to ~s", [?MODULE, ?TOPIC]),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({mesh_hope, Topic, Hope, ReplyTo}, State) ->
    ?LOG_DEBUG("[~s] Received HOPE on ~s", [?MODULE, Topic]),
    Result = handle_hope(Hope),
    %% Send FEEDBACK
    Feedback = case Result of
        {ok, Event} ->
            #{ok => true, event => {event}_v1:to_map(Event)};
        {error, Reason} ->
            #{ok => false, error => Reason}
    end,
    ok = hecate_mesh:publish(ReplyTo, Feedback),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal
%%====================================================================

handle_hope(Hope) ->
    %% Translate HOPE to Command
    Cmd = hope_to_command(Hope),
    %% Dispatch through normal command flow
    maybe_{command}:dispatch(Cmd).

hope_to_command(Hope) ->
    {command}_v1:from_map(Hope).
```

### {event}\_to_mesh.erl (Emitter - PRJ filer to mesh)

```erlang
-module({event}_to_mesh).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(TOPIC, <<"hecate.{domain_noun}.{event}">>).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Subscribe to domain events from store
    ok = reckon_evoq:subscribe({domain}_store, self(), #{
        event_types => [<<"{event}_v1">>]
    }),
    ?LOG_INFO("[~s] Emitter started, publishing to ~s", [?MODULE, ?TOPIC]),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({evoq_event, _StreamId, EventType, EventData, _Position}, State)
  when EventType =:= <<"{event}_v1">> ->
    %% Transform EVENT to FACT (may have different structure)
    Fact = event_to_fact(EventData),
    %% Publish to mesh
    ok = hecate_mesh:publish(?TOPIC, Fact),
    ?LOG_DEBUG("[~s] Published FACT to ~s", [?MODULE, ?TOPIC]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal
%%====================================================================

%% Transform internal EVENT to external FACT
%% FACT is a PUBLIC CONTRACT - may differ from EVENT structure
event_to_fact(EventData) ->
    #{
        {field1} => maps:get(<<"{field1}">>, EventData),
        {field2} => maps:get(<<"{field2}">>, EventData),
        published_at => erlang:system_time(millisecond)
    }.
```

### on*{trigger_event}\_maybe*{command}.erl (Policy/Process Manager)

```erlang
-module(on_{trigger_event}_maybe_{command}).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Subscribe to trigger events from another domain
    ok = reckon_evoq:subscribe({trigger_domain}_store, self(), #{
        event_types => [<<"{trigger_event}_v1">>]
    }),
    ?LOG_INFO("[~s] Policy started", [?MODULE]),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({evoq_event, _StreamId, EventType, EventData, _Position}, State)
  when EventType =:= <<"{trigger_event}_v1">> ->
    %% Transform trigger event to command
    case should_trigger(EventData) of
        true ->
            Cmd = event_to_command(EventData),
            case maybe_{command}:dispatch(Cmd) of
                {ok, _Event} ->
                    ?LOG_DEBUG("[~s] Triggered {command} from {trigger_event}", [?MODULE]);
                {error, Reason} ->
                    ?LOG_WARNING("[~s] Failed to trigger: ~p", [?MODULE, Reason])
            end;
        false ->
            ok
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal
%%====================================================================

should_trigger(_EventData) ->
    %% TODO: Add policy logic
    true.

event_to_command(EventData) ->
    %% Transform trigger event data to command
    {command}_v1:new(#{
        {field1} => maps:get(<<"{trigger_field}">>, EventData),
        {field2} => maps:get(<<"{other_field}">>, EventData, <<>>)
    }).
```

---

## PRJ Templates

### query\_{domain_noun}\_sup.erl

```erlang
-module(query_{domain_noun}_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        %% Store (SQLite)
        #{id => query_{domain_noun}_store,
          start => {query_{domain_noun}_store, start_link, []},
          type => worker},

        %% PRJ Spokes (ADD PROJECTION SUPERVISORS HERE)
        #{id => {event}_to_{read_store}_sup,
          start => {{event}_to_{read_store}_sup, start_link, []},
          type => supervisor}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

### {event}_to_{read_store}\_sup.erl

```erlang
-module({event}_to_{read_store}_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{id => {event}_to_{read_store},
          start => {{event}_to_{read_store}, start_link, []},
          type => worker}
    ],
    {ok, {#{strategy => one_for_one, intensity => 5, period => 10}, Children}}.
```

### {event}_to_{read_store}.erl (Projection)

```erlang
-module({event}_to_{read_store}).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Subscribe to events from CMD domain store
    ok = reckon_evoq:subscribe({domain}_store, self(), #{
        event_types => [<<"{event}_v1">>]
    }),
    ?LOG_INFO("[~s] Projection started", [?MODULE]),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({evoq_event, StreamId, EventType, EventData, Position}, State)
  when EventType =:= <<"{event}_v1">> ->
    ok = project(EventData),
    %% Checkpoint position for recovery
    ok = reckon_evoq:ack({domain}_store, Position),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal
%%====================================================================

project(EventData) ->
    Row = #{
        {pk_field} => maps:get(<<"{pk_field}">>, EventData),
        {field1} => maps:get(<<"{field1}">>, EventData),
        {field2} => maps:get(<<"{field2}">>, EventData),
        updated_at => erlang:system_time(millisecond)
    },
    %% Upsert for idempotency
    query_{domain_noun}_store:upsert({read_store}, Row).
```

---

## QRY Templates

### query\_{domain_noun}.erl (Provider)

```erlang
-module(query_{domain_noun}).

%% Public Query API
-export([get/1, find/1, list_all/0, list_all/1]).

%% Get single record by primary key
get(Id) ->
    query_{domain_noun}_store:get({read_store}, Id).

%% Find records matching criteria
find(Criteria) ->
    query_{domain_noun}_store:find({read_store}, Criteria).

%% List all records
list_all() ->
    list_all(#{limit => 100, offset => 0}).

list_all(Opts) ->
    query_{domain_noun}_store:list({read_store}, Opts).
```

### {query}/{query}.erl (Query Slice)

```erlang
-module({query}).

-export([execute/1]).

%% Execute query with parameters
execute(Params) ->
    %% Extract parameters
    {pk_field} = maps:get({pk_field}, Params),

    %% Query store
    case query_{domain_noun}_store:get({read_store}, {pk_field}) of
        {ok, Row} ->
            {ok, row_to_result(Row)};
        {error, not_found} = Error ->
            Error
    end.

row_to_result(Row) ->
    %% Transform row to API result
    Row.
```

---

## rebar.config Template

```erlang
{erl_opts, [debug_info]}.

{deps, [
    {reckon_evoq, {git, "https://github.com/reckon-db-org/reckon_evoq.git", {branch, "main"}}}
]}.

%% Include spoke directories
{src_dirs, [
    "src",
    "src/{command1}",
    "src/{command2}",
    "src/{event1}_to_{read_store1}",
    "src/{query1}"
]}.
```

---

## Generation Checklist

### New CMD Spoke

Given: `domain=manage_capabilities`, `command=announce_capability`, `event=capability_announced`

Generate:

- [ ] `src/announce_capability/announce_capability_spoke_sup.erl`
- [ ] `src/announce_capability/announce_capability_v1.erl`
- [ ] `src/announce_capability/capability_announced_v1.erl`
- [ ] `src/announce_capability/maybe_announce_capability.erl`
- [ ] `src/announce_capability/announce_capability_responder_v1.erl`
- [ ] `src/announce_capability/capability_announced_to_mesh.erl`
- [ ] Update `manage_capabilities_sup.erl` to include spoke supervisor
- [ ] Update `rebar.config` src_dirs

### New PRJ Spoke

Given: `event=capability_announced`, `read_store=capabilities`

Generate:

- [ ] `src/capability_announced_to_capabilities/capability_announced_to_capabilities_sup.erl`
- [ ] `src/capability_announced_to_capabilities/capability_announced_to_capabilities.erl`
- [ ] Update `query_capabilities_sup.erl` to include spoke supervisor
- [ ] Update `rebar.config` src_dirs

### New Policy/PM

Given: `trigger_event=llm_model_detected`, `command=announce_capability`

Generate:

- [ ] `src/announce_capability/on_llm_model_detected_maybe_announce_capability.erl`
- [ ] Update `announce_capability_spoke_sup.erl` to include PM worker

---

## Naming Rules

| Component  | Pattern                      | Example                                     |
| ---------- | ---------------------------- | ------------------------------------------- |
| Domain app | `{verb}_{noun}`              | `manage_capabilities`                       |
| Query app  | `query_{noun}`               | `query_capabilities`                        |
| Spoke dir  | `{command}/`                 | `announce_capability/`                      |
| Spoke sup  | `{command}_spoke_sup`        | `announce_capability_spoke_sup`             |
| Command    | `{command}_v1`               | `announce_capability_v1`                    |
| Event      | `{noun}_{past_verb}_v1`      | `capability_announced_v1`                   |
| Handler    | `maybe_{command}`            | `maybe_announce_capability`                 |
| Responder  | `{command}_responder_v1`     | `announce_capability_responder_v1`          |
| Emitter    | `{event}_to_mesh`            | `capability_announced_to_mesh`              |
| Projection | `{event}_to_{read_store}`    | `capability_announced_to_capabilities`      |
| Policy/PM  | `on_{event}_maybe_{command}` | `on_llm_detected_maybe_announce_capability` |
| Query      | `{verb}_{noun}`              | `find_capability`                           |

---

_Templates are deterministic. Fill in variables. Generate code. No creativity required._ 🗝️
