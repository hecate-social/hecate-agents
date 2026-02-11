# Testing Erlang Templates (TnI Guidelines)

Parameterized test templates for Hecate venture lifecycle apps.
Proven through `setup_venture` + `query_ventures` pilot (100 tests, 4 layers).

---

## 4 Testing Layers

| Layer | What | Scope | Framework | I/O |
|-------|------|-------|-----------|-----|
| **L1 Dossier** | Aggregate state reconstruction | `apply_event/2` chains | EUnit | None |
| **L2 Domain** | Commands, handlers, execute/2 state machine | Business rules | EUnit | None |
| **L3 Integration** | CMD to PRJ flow | Projection + Query | EUnit + SQLite | SQLite (temp) |
| **L4 Side Effects** | pg broadcast, projection rows, status labels | Observable effects | EUnit + pg + SQLite | pg scope, SQLite |

---

## Layer 1: Dossier Tests

**File:** `apps/{CmdApp}/test/{Aggregate}_tests.erl`

**What:** Pure functions only. No processes. Test that `apply_event/2` chains reconstruct state correctly.

### Template Variables

| Variable | Description |
|----------|-------------|
| `{Aggregate}` | Aggregate module (e.g., `setup_aggregate`) |
| `{StatusHrl}` | Status header include (e.g., `setup_venture/include/venture_status.hrl`) |
| `{StateRecord}` | Record name (e.g., `setup_state`) |
| `{Events}` | List of `{event_type_binary, flag_bit}` pairs |
| `{FlagMap}` | Macro for the flag map (e.g., `?VENTURE_FLAG_MAP`) |

### Template

```erlang
-module({Aggregate}_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("{StatusHrl}").

%% Record field positions (record tag is element 1)
-define(F_STATUS, {StatusFieldPosition}).  %% Adjust per record

%% --- Fixtures ---
%% One fixture function per event type returning a map with binary keys

%% --- Tests ---

initial_state_test() ->
    S = {Aggregate}:initial_state(),
    ?assertEqual(0, element(?F_STATUS, S)).

init_callback_test() ->
    {ok, S} = {Aggregate}:init(<<"any-id">>),
    ?assertEqual(0, element(?F_STATUS, S)).

%% One test per event: apply event, verify status bit + fields
apply_{event_name}_test() ->
    S0 = {Aggregate}:initial_state(),
    S1 = {Aggregate}:apply_event({event_fixture}(), S0),
    ?assertNotEqual(0, element(?F_STATUS, S1) band ?{FLAG_BIT}).

%% Full lifecycle: chain all events, verify final status = sum of all bits
full_lifecycle_state_test() ->
    S0 = {Aggregate}:initial_state(),
    %% Chain: S0 -> S1 -> S2 -> ... -> SN
    ?assertEqual({ExpectedFinalStatus}, element(?F_STATUS, SN)).

%% Unknown event leaves state unchanged
apply_unknown_event_test() ->
    S0 = {Aggregate}:initial_state(),
    S1 = {Aggregate}:apply_event({first_event}(), S0),
    S2 = {Aggregate}:apply_event(#{<<"event_type">> => <<"unknown">>}, S1),
    ?assertEqual(S1, S2).

%% Atom keys work identically to binary keys
apply_with_atom_keys_test() ->
    %% Use atom key variant of first event, verify same result

%% flag_map/0 returns expected labels
flag_map_test() ->
    Map = {Aggregate}:flag_map(),
    ?assertEqual(<<"New">>, maps:get(0, Map)).

%% apply/2 callback order: (State, Event) - state first
apply_callback_order_test() ->
    S0 = {Aggregate}:initial_state(),
    S1 = {Aggregate}:apply(S0, {first_event}()),
    ?assertNotEqual(0, element(?F_STATUS, S1) band ?{FIRST_FLAG}).
```

### Key Patterns

- **Record field access:** Define `?F_*` macros for element positions. Count from 2 (element 1 is record tag).
- **State verification via execute/2:** If record is opaque, verify state through `execute/2` behavior instead of direct field access.
- **Partial event application:** Test events with optional fields omitted to verify defaults.

---

## Layer 2: Domain Internal Tests

### 2a. Command Tests

**File:** `apps/{CmdApp}/test/{CmdApp}_command_tests.erl`

```erlang
%% Per command module: {CmdModule}

{cmd_name}_new_valid_test() ->
    {ok, Cmd} = {CmdModule}:new(#{required_field => <<"value">>}),
    ?assertEqual(<<"value">>, {CmdModule}:get_required_field(Cmd)).

{cmd_name}_new_missing_fields_test() ->
    ?assertEqual({error, missing_required_fields}, {CmdModule}:new(#{})).

{cmd_name}_from_map_binary_keys_test() ->
    {ok, Cmd} = {CmdModule}:from_map(#{<<"required">> => <<"val">>}),
    ?assertEqual(<<"val">>, {CmdModule}:get_required(Cmd)).

{cmd_name}_to_map_round_trip_test() ->
    {ok, Cmd} = {CmdModule}:new(#{...}),
    Map = {CmdModule}:to_map(Cmd),
    ?assertEqual(<<"{cmd_type}">>, maps:get(<<"command_type">>, Map)),
    {ok, Cmd2} = {CmdModule}:from_map(Map),
    ?assertEqual({CmdModule}:get_id(Cmd), {CmdModule}:get_id(Cmd2)).
```

### 2b. Handler Tests

**File:** Same file or `apps/{CmdApp}/test/{CmdApp}_handler_tests.erl`

```erlang
%% Per handler: {HandlerModule} handling {CmdModule} -> {EventModule}

handle_valid_{operation}_test() ->
    {ok, Cmd} = {CmdModule}:new(#{...}),
    {ok, [Event]} = {HandlerModule}:handle(Cmd),
    Map = {EventModule}:to_map(Event),
    ?assertEqual(<<"{event_type}">>, maps:get(<<"event_type">>, Map)).
```

### 2c. Aggregate execute/2 Tests

**File:** Same file as handlers.

Template per command type — test success case + all error paths:

```erlang
%% Helpers: build state from event chains
fresh_state() -> {Aggregate}:initial_state().
{state_name}() -> {Aggregate}:apply_event({event}(), fresh_state()).

%% Success case
execute_{cmd}_on_{valid_state}_test() ->
    Cmd = #{<<"command_type">> => <<"{cmd_type}">>, ...},
    {ok, [EventMap]} = {Aggregate}:execute({valid_state}(), Cmd),
    ?assertEqual(<<"{event_type}">>, maps:get(<<"event_type">>, EventMap)).

%% Error cases (one per guard clause in the aggregate)
execute_{cmd}_{error_condition}_test() ->
    Cmd = #{<<"command_type">> => <<"{cmd_type}">>, ...},
    ?assertEqual({error, {error_atom}}, {Aggregate}:execute({invalid_state}(), Cmd)).

%% Unknown command
execute_unknown_command_test() ->
    ?assertEqual({error, unknown_command},
                 {Aggregate}:execute(fresh_state(), #{<<"command_type">> => <<"unknown">>})).

%% Callback order verification
execute_evoq_callback_order_test() ->
    {ok, _} = {Aggregate}:execute(fresh_state(), {valid_cmd_map}).
```

### 2d. Event Serialization Tests

**File:** `apps/{CmdApp}/test/{event_name}_tests.erl`

```erlang
{event}_new_test() ->
    E = {EventModule}:new(#{...}),
    ?assert(is_integer({EventModule}:get_timestamp(E))).

{event}_to_map_includes_event_type_test() ->
    E = {EventModule}:new(#{...}),
    Map = {EventModule}:to_map(E),
    ?assertEqual(<<"{event_type}">>, maps:get(<<"event_type">>, Map)).

{event}_round_trip_test() ->
    E = {EventModule}:new(#{...}),
    Map = {EventModule}:to_map(E),
    {ok, E2} = {EventModule}:from_map(Map),
    ?assertEqual({EventModule}:get_id(E), {EventModule}:get_id(E2)).

{event}_from_map_invalid_test() ->
    ?assertEqual({error, invalid_event}, {EventModule}:from_map(#{})).
```

---

## Layer 3: Integration Tests (CMD to PRJ)

**File:** `apps/{QryApp}/test/{qry_name}_cqrs_integration_tests.erl`

**Requires:** `test_store_proxy.erl` in the same test directory.

### test_store_proxy Pattern

A gen_server that:
1. Opens a temp SQLite file in `/tmp/`
2. Registers as `{store_module}` (e.g., `query_ventures_store`)
3. Implements the same `execute/1,2` and `query/1,2` API
4. Cleans up the temp file on terminate

```erlang
%% Setup/teardown pattern
cqrs_integration_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [fun test_case_1/1, fun test_case_2/1, ...]}.

setup() ->
    application:ensure_all_started(esqlite),
    DbPath = "/tmp/test_{table}_" ++
             integer_to_list(erlang:unique_integer([positive])) ++ ".db",
    {ok, _} = test_store_proxy:start(DbPath),
    #{db_path => DbPath}.

teardown(#{}) ->
    test_store_proxy:stop(),
    ok.
```

### Standard Test Cases

| Test | What It Verifies |
|------|-----------------|
| `setup_projects_to_sqlite` | Projection inserts a row |
| `query_by_id_found` | Query returns projected data |
| `query_by_id_not_found` | Query returns `{error, not_found}` |
| `archive_updates_status` | Archive projection sets bit flag |
| `page_returns_list` | Page query returns multiple results |
| `page_excludes_archived` | Default filter hides archived |
| `page_includes_archived` | Filter override shows archived |
| `page_limit_offset` | Pagination works correctly |
| `projection_idempotency` | Same event projected twice = no duplicate |
| `full_cqrs_flow` | Command -> handler -> event -> projection -> query |

---

## Layer 4: Side Effect Tests

### 4a. pg Emission Tests

**File:** `apps/{CmdApp}/test/{CmdApp}_side_effects_tests.erl`

```erlang
pg_emission_test_() ->
    {setup,
     fun start_pg/0,
     fun stop_pg/1,
     [fun emit_{event}_reaches_members/0, ...]}.

start_pg() ->
    case pg:start(pg) of
        {ok, Pid} -> {started, Pid};
        {error, {already_started, Pid}} -> {existing, Pid}
    end.

stop_pg({started, Pid}) -> gen_server:stop(Pid);
stop_pg({existing, _}) -> ok.

emit_{event}_reaches_members() ->
    Self = self(),
    Pid = spawn_link(fun() ->
        pg:join(pg, {group_atom}, self()),
        receive Msg -> Self ! {got, Msg} end
    end),
    timer:sleep(10),
    Event = #{<<"event_type">> => <<"{event_type}">>, ...},
    ok = {EmitterModule}:emit(Event),
    receive
        {got, {{group_atom}, E}} -> ?assertEqual(Event, E)
    after 1000 -> ?assert(false)
    end.

emit_with_no_members() ->
    ?assertEqual(ok, {EmitterModule}:emit(#{...})).

emit_reaches_multiple_members() ->
    %% Spawn N processes, join group, emit, verify all received
```

### 4b. Projection Row Verification Tests

**File:** `apps/{QryApp}/test/{table}_projection_tests.erl`

```erlang
%% Uses test_store_proxy, same setup/teardown as Layer 3

projection_creates_correct_row(#{}) ->
    fun() ->
        %% Project event, then SELECT raw row, verify columns
        {ok, [Row]} = {Store}:query("SELECT ... FROM {table} WHERE ...", [...]),
        Values = to_list(Row),  %% Handle tuple or list format
        %% Assert each column value
    end.

projection_status_label_correct(#{}) ->
    fun() ->
        %% Verify status_label matches evoq_bit_flags:to_string output
        ExpectedLabel = evoq_bit_flags:to_string(Status, ?FLAG_MAP),
        ?assertEqual(ExpectedLabel, ActualLabel)
    end.

projection_json_fields_encode(#{}) ->
    fun() ->
        %% Verify JSON fields decode correctly
        ?assertEqual([<<"r1">>], json:decode(ReposJson))
    end.

%% Helper for esqlite3 format flexibility
to_list(T) when is_tuple(T) -> tuple_to_list(T);
to_list(L) when is_list(L) -> L.
```

---

## Running Tests

```bash
# Layer 1+2 (pure functions, auto-discovered)
rebar3 eunit --app={CmdApp}

# Layer 1+2 (standalone test modules)
rebar3 eunit --dir=apps/{CmdApp}/test

# Layer 3+4 (integration, needs SQLite)
rebar3 eunit --dir=apps/{QryApp}/test

# All tests in the daemon
rebar3 eunit

# Specific module
rebar3 eunit --module={test_module}
```

**Note:** `rebar3 eunit --app=X` only auto-discovers `{module}_tests` modules. Use `--dir=apps/X/test` to run standalone test modules.

---

## Template Variable Reference (per CMD app)

| Variable | setup_venture | design_division |
|----------|--------------|----------------|
| `{CmdApp}` | `setup_venture` | `design_division` |
| `{Aggregate}` | `setup_aggregate` | `design_aggregate` |
| `{StatusHrl}` | `setup_venture/include/venture_status.hrl` | `design_division/include/design_status.hrl` |
| `{StateRecord}` | `setup_state` | `design_state` |
| `{InitEvent}` | `venture_setup_v1` | `design_started_v1` |
| `{ArchiveEvent}` | `venture_archived_v1` | `design_archived_v1` |

## Template Variable Reference (per QRY app)

| Variable | query_ventures | query_designs |
|----------|---------------|--------------|
| `{QryApp}` | `query_ventures` | `query_designs` |
| `{Store}` | `query_ventures_store` | `query_designs_store` |
| `{Table}` | `ventures` | `designs` |

---

## Naming Conventions for Test Files

| Layer | Pattern | Example |
|-------|---------|---------|
| L1 Dossier | `{aggregate}_tests.erl` | `setup_aggregate_tests.erl` |
| L2 Commands | `{cmd_app}_command_tests.erl` | `setup_venture_command_tests.erl` |
| L2 Events | `{subject}_event_tests.erl` | `venture_event_tests.erl` |
| L2 Handlers | `{cmd_app}_handler_tests.erl` | `setup_venture_handler_tests.erl` |
| L3 Integration | `{subject}_cqrs_integration_tests.erl` | `venture_cqrs_integration_tests.erl` |
| L4 Side Effects | `{cmd_app}_side_effects_tests.erl` | `setup_venture_side_effects_tests.erl` |
| L4 Projections | `{subject}_projection_tests.erl` | `venture_projection_tests.erl` |
| Helper | `test_store_proxy.erl` | `test_store_proxy.erl` |

---

## Bug Found During Testing

**Archive projection list/tuple mismatch:** `esqlite3:fetchall` can return rows as lists `[[val]]` or tuples `[{val}]`. The archive projection only matched tuples. All code reading from esqlite3 must handle both formats.

Pattern to avoid:
```erlang
%% BAD: only handles tuple format
case query(Sql, Params) of
    {ok, [{Value}]} -> ...
```

Pattern to use:
```erlang
%% GOOD: handles both formats
case query(Sql, Params) of
    {ok, [{Value}]} -> ...;
    {ok, [[Value]]} -> ...
```

Or extract with a helper:
```erlang
to_list(T) when is_tuple(T) -> tuple_to_list(T);
to_list(L) when is_list(L) -> L.
```
