# Example: Vertical API Handlers

*Canonical example: API handlers live in their spokes, not in a monolithic API app*

---

## The Pattern

Instead of grouping API handlers by domain in large files:

```
❌ WRONG: Monolithic API handlers (god modules)
hecate_api/src/
├── hecate_api_mentors.erl      # 289 lines, 16 init/2 clauses
├── hecate_api_capabilities.erl # 215 lines, 5 init/2 clauses
├── hecate_api_social.erl       # 136 lines, 8 init/2 clauses
└── hecate_api_connectors.erl   # 149 lines, 4 init/2 clauses
```

Put each API handler in its spoke:

```
✅ CORRECT: Vertical API handlers
manage_torches/src/
├── initiate_torch/
│   ├── initiate_torch_v1.erl
│   ├── torch_initiated_v1.erl
│   ├── maybe_initiate_torch.erl
│   └── initiate_torch_api.erl     # ~50 lines

query_torches/src/
├── get_torch/
│   ├── get_torch.erl
│   └── get_torch_api.erl          # ~25 lines
```

---

## Why This Works

| Monolithic | Vertical |
|------------|----------|
| Find endpoint in 500-line file | Find endpoint = find spoke |
| Change touches many endpoints | Change is isolated to spoke |
| Hard to understand full feature | Spoke shows full feature |
| Validation duplicated | Handler + API share validation |

---

## Dependency Graph

```
shared/                         # Bottom - no hecate deps
├── hecate_api_utils.erl       # json_response, format_error, etc.

manage_torches/                 # Depends on: shared
├── initiate_torch/
│   └── initiate_torch_api.erl # Uses hecate_api_utils

query_torches/                  # Depends on: shared
├── get_torch/
│   └── get_torch_api.erl      # Uses hecate_api_utils

hecate_api/                     # Depends on: manage_*, query_*, shared
├── hecate_api_routes.erl      # References spoke handlers
└── hecate_api_sup.erl         # Cowboy setup only
```

**Key insight:** `shared` app at the bottom prevents circular dependencies.

---

## Shared Utilities

```erlang
%% shared/src/hecate_api_utils.erl
-module(hecate_api_utils).

-export([json_ok/2, json_ok/3, json_error/3]).
-export([bad_request/2, not_found/1, method_not_allowed/1]).
-export([read_json_body/1, get_field/2, get_field/3]).

%% Send 200 OK with result
json_ok(Result, Req) ->
    json_response(200, maps:merge(#{ok => true}, Result), Req).

%% Send error response
json_error(StatusCode, Reason, Req) ->
    json_response(StatusCode, #{ok => false, error => format_error(Reason)}, Req).

%% Read and decode JSON body
read_json_body(Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    try
        {ok, json:decode(Body), Req1}
    catch
        _:_ -> {error, invalid_json, Req1}
    end.

%% Get field supporting both atom and binary keys
get_field(Key, Map, Default) when is_atom(Key) ->
    BinKey = atom_to_binary(Key, utf8),
    maps:get(Key, Map, maps:get(BinKey, Map, Default)).
```

---

## Spoke API Handler Template

```erlang
%%% @doc API handler: POST /api/torch/initiate
%%%
%%% Initiates a new torch.
%%% Lives in the initiate_torch spoke for vertical slicing.
%%% @end
-module(initiate_torch_api).

-export([init/2]).

%% Cowboy handler entry point
init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"POST">> -> handle_post(Req0, State);
        _ -> hecate_api_utils:method_not_allowed(Req0)
    end.

%% Read JSON body
handle_post(Req0, _State) ->
    case hecate_api_utils:read_json_body(Req0) of
        {ok, Params, Req1} -> do_initiate(Params, Req1);
        {error, invalid_json, Req1} -> hecate_api_utils:bad_request(<<"Invalid JSON">>, Req1)
    end.

%% Validate and dispatch
do_initiate(Params, Req) ->
    Name = hecate_api_utils:get_field(name, Params),
    case validate(Name) of
        ok -> create_torch(Name, Params, Req);
        {error, Reason} -> hecate_api_utils:bad_request(Reason, Req)
    end.

%% Validation
validate(undefined) -> {error, <<"name is required">>};
validate(Name) when not is_binary(Name) -> {error, <<"name must be string">>};
validate(_) -> ok.

%% Create command and dispatch
create_torch(Name, Params, Req) ->
    CmdParams = #{
        name => Name,
        brief => hecate_api_utils:get_field(brief, Params)
    },
    case initiate_torch_v1:new(CmdParams) of
        {ok, Cmd} -> dispatch(Cmd, Req);
        {error, Reason} -> hecate_api_utils:bad_request(Reason, Req)
    end.

%% Dispatch to handler
dispatch(Cmd, Req) ->
    case maybe_initiate_torch:handle(Cmd) of
        {ok, Events} ->
            EventMaps = [torch_initiated_v1:to_map(E) || E <- Events],
            hecate_api_utils:json_ok(201, #{
                torch_id => initiate_torch_v1:get_torch_id(Cmd),
                events => EventMaps
            }, Req);
        {error, Reason} ->
            hecate_api_utils:bad_request(Reason, Req)
    end.
```

---

## Query Handler Templates

### By-ID Query Handler

```erlang
%%% @doc API handler: GET /api/torches/:torch_id
-module(get_torch_by_id_api).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> hecate_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    TorchId = cowboy_req:binding(torch_id, Req0),
    case get_torch_by_id:execute(TorchId) of
        {ok, Torch} ->
            hecate_api_utils:json_ok(#{torch => Torch}, Req0);
        {error, not_found} ->
            hecate_api_utils:not_found(Req0);
        {error, Reason} ->
            hecate_api_utils:json_error(500, Reason, Req0)
    end.
```

### Paged List Query Handler

```erlang
%%% @doc API handler: GET /api/torches
-module(get_torches_page_api).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> hecate_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    QS = cowboy_req:parse_qs(Req0),
    Filters = build_filters(QS),
    case get_torches_page:execute(Filters) of
        {ok, Result} ->
            hecate_api_utils:json_ok(#{torches => Result}, Req0);
        {error, Reason} ->
            hecate_api_utils:json_error(500, Reason, Req0)
    end.

build_filters(QS) ->
    lists:foldl(fun({K, V}, Acc) ->
        case K of
            <<"limit">> -> safe_int(V, limit, Acc);
            <<"offset">> -> safe_int(V, offset, Acc);
            _ -> Acc
        end
    end, #{}, QS).

safe_int(V, Key, Acc) ->
    case catch binary_to_integer(V) of
        I when is_integer(I) -> Acc#{Key => I};
        _ -> Acc
    end.
```

---

## Routes Configuration

All routes MUST use the `/api/` prefix. Routes reference spoke handlers directly — no `[action]` state args.

```erlang
%% hecate_api/src/hecate_api_routes.erl
-module(hecate_api_routes).
-export([compile/0]).

compile() ->
    cowboy_router:compile([{'_', routes()}]).

routes() ->
    torch_routes() ++ cartwheel_routes() ++ llm_routes().

%% Routes reference spoke handlers directly
torch_routes() ->
    [
        {"/api/torch", get_active_torch_api, []},
        {"/api/torch/initiate", initiate_torch_api, []},
        {"/api/torches", get_torches_page_api, []},
        {"/api/torches/:torch_id", get_torch_by_id_api, []},
        {"/api/torches/:torch_id/archive", archive_torch_api, []},
        {"/api/torches/:torch_id/cartwheels/identify", identify_cartwheel_api, []}
    ].

%% Cartwheel routes - organized by ALC phase
cartwheel_routes() ->
    [
        %% Core routes
        {"/api/cartwheel", get_active_cartwheel_api, []},
        {"/api/cartwheels", get_cartwheels_page_api, []},
        {"/api/cartwheels/:cartwheel_id", get_cartwheel_by_id_api, []},
        {"/api/cartwheels/:cartwheel_id/transition", transition_phase_api, []},
        %% Discovery & Analysis phase
        {"/api/cartwheels/:cartwheel_id/discovery/start", start_discovery_api, []},
        {"/api/cartwheels/:cartwheel_id/discovery/findings", get_findings_page_api, []},
        {"/api/cartwheels/:cartwheel_id/discovery/findings/record", record_finding_api, []},
        %% ... (similar for architecture, testing, deployment phases)
    ].
```

**Route conventions:**
- `POST /api/{domain}/{verb}` — commands (e.g., `/api/social/follow`)
- `GET /api/{domain}` — paged list (e.g., `/api/capabilities`)
- `GET /api/{domain}/:id` — single by-id lookup (e.g., `/api/torches/:torch_id`)
- `POST /api/{domain}/:id/{verb}` — action on existing aggregate (e.g., `/api/cartwheels/:id/transition`)
- No `[action]` state args — each route maps to a dedicated handler module

---

## Complete Spoke Structure

A fully vertical spoke contains:

```
initiate_torch/
├── initiate_torch_v1.erl           # Command
├── torch_initiated_v1.erl          # Event
├── maybe_initiate_torch.erl        # Handler
├── torch_initiated_v1_to_mesh.erl  # Emitter (optional)
└── initiate_torch_api.erl          # API handler
```

**The spoke owns EVERYTHING for that operation.**

---

## Migration Strategy

When splitting a monolithic handler:

1. **Create `shared/` app** with common utilities
2. **Pick one endpoint** to migrate first
3. **Create API handler in spoke** using the template
4. **Update routes** to reference new handler
5. **Repeat** for remaining endpoints
6. **Delete monolithic handler** when empty

---

## Checklist

Before creating an API handler:

- [ ] Does the spoke exist? (cmd + event + handler)
- [ ] Is `shared/` app in dependencies?
- [ ] Is handler using `hecate_api_utils`?
- [ ] Is handler ~50 lines or less?
- [ ] Does routes file reference the spoke handler?

---

## Key Takeaways

1. **API handlers live in spokes** - not in hecate_api
2. **`shared/` app prevents circular deps** - utilities at bottom
3. **~50 lines per handler** - easy to read
4. **Routes just dispatch** - no logic in routes file
5. **Spoke owns full vertical slice** - cmd, event, handler, emitter, API

---

## Training Note

This example teaches:
- Vertical slicing for API layer
- Dependency management with shared utilities
- Handler template patterns
- Migration from monolithic to vertical

*Date: 2026-02-10*
*Origin: Hecate daemon god module refactoring (137 files, 50 spoke handlers)*
