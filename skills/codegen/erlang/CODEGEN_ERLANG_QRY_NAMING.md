# CODEGEN_ERLANG_QRY_NAMING.md — Query Module Naming Conventions

_How to name query modules so the architecture screams intent._

**Target:** Erlang/OTP Hecate runtimes (hecate-daemon, future runtimes)

---

## Overview

Query modules live in `apps/query_{aggregates}/src/`. Their names MUST scream what they do.
Two critical rules prevent silent scaling problems and ambiguous intent.

---

## Rule 1: No Raw `get_{aggregate}` — Use `get_{aggregate}_by_id`

A module named `get_torch` is ambiguous. Get it how? By name? By status? By position?

**The name MUST specify the lookup strategy.**

| Module Name | What It Screams |
|-------------|----------------|
| `get_torch_by_id` | "I fetch one torch by its primary key" |
| `get_torch_by_name` | "I fetch one torch by its unique name" |
| `get_active_torch` | "I fetch the currently active torch" |

### Why This Matters

- **Discoverability**: When you have 5 get-by-X modules, their names tell you the query vocabulary
- **Screaming architecture**: A stranger reads `src/get_torch_by_id/` and knows exactly what it does
- **Extensibility**: Adding `get_torch_by_name` doesn't conflict or confuse
- **API alignment**: The module name maps directly to the HTTP endpoint semantics

### Directory Structure

```
apps/query_torches/src/
├── get_torch_by_id/
│   ├── get_torch_by_id.erl          # Query module
│   └── get_torch_by_id_api.erl      # API handler
├── get_active_torch/
│   ├── get_active_torch.erl
│   └── get_active_torch_api.erl
└── ...
```

### Code Template

```erlang
%%% @doc Query: get a {aggregate} by ID
-module(get_{aggregate}_by_id).

-export([execute/1]).

-spec execute(binary()) -> {ok, map()} | {error, not_found | term()}.
execute({Aggregate}Id) ->
    Sql = "SELECT {aggregate}_id, name, status, status_label "
          "FROM {aggregates} WHERE {aggregate}_id = ?1",
    case query_{aggregates}_store:query(Sql, [{Aggregate}Id]) of
        {ok, [Row]} ->
            {ok, row_to_map(Row)};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.
```

---

## Rule 2: No Raw `list_{aggregates}` — Use `get_{aggregates}_page`

A module named `list_torches` sounds like it returns ALL torches. That's a scaling time bomb.

**Every list query MUST be paged by default.**

| Module Name | What It Screams |
|-------------|----------------|
| `get_torches_page` | "I return one page of torches (bounded)" |
| `get_cartwheels_page` | "I return one page of cartwheels (bounded)" |

### Why This Matters

- **No unbounded queries**: `list_torches` returning 10,000 rows kills the daemon
- **Name enforces behavior**: "page" in the name means pagination is mandatory
- **Client expectations**: TUI/API clients know to expect `{items, total, page, page_size}`
- **SQLite friendly**: Paged queries use `LIMIT ?1 OFFSET ?2` — always bounded

### Naming Comparison

| Name | Screams | Danger |
|------|---------|--------|
| `list_torches` | "Give me everything" | Unbounded. Will crash at scale. |
| `get_all_torches` | "Give me literally all of them" | Even worse. Explicit unbounded intent. |
| `get_torches_page` | "Give me one page" | Bounded. Safe. Correct. |

### Code Template

```erlang
%%% @doc Query: get a page of {aggregates}
-module(get_{aggregates}_page).

-export([execute/1]).

-define(DEFAULT_PAGE_SIZE, 50).
-define(MAX_PAGE_SIZE, 200).

-spec execute(map()) -> {ok, map()} | {error, term()}.
execute(Opts) ->
    Page = maps:get(page, Opts, 1),
    PageSize = min(maps:get(page_size, Opts, ?DEFAULT_PAGE_SIZE), ?MAX_PAGE_SIZE),
    Offset = (Page - 1) * PageSize,

    %% Count total (for pagination metadata)
    {ok, [[Total]]} = query_{aggregates}_store:query(
        "SELECT COUNT(*) FROM {aggregates} WHERE (status & ?1) = 0",
        [?ARCHIVED]),

    %% Fetch page
    Sql = "SELECT {aggregate}_id, name, status, status_label, initiated_at "
          "FROM {aggregates} "
          "WHERE (status & ?1) = 0 "
          "ORDER BY initiated_at DESC "
          "LIMIT ?2 OFFSET ?3",
    case query_{aggregates}_store:query(Sql, [?ARCHIVED, PageSize, Offset]) of
        {ok, Rows} ->
            {ok, #{
                items => [row_to_map(R) || R <- Rows],
                total => Total,
                page => Page,
                page_size => PageSize
            }};
        {error, Reason} ->
            {error, Reason}
    end.
```

### API Response Shape

```json
{
  "ok": true,
  "items": [...],
  "total": 127,
  "page": 1,
  "page_size": 50
}
```

### API Handler Template

```erlang
-module(get_{aggregates}_page_api).

-export([init/2]).

init(Req0, State) ->
    case cowboy_req:method(Req0) of
        <<"GET">> -> handle_get(Req0, State);
        _ -> hecate_api_utils:method_not_allowed(Req0)
    end.

handle_get(Req0, _State) ->
    QS = cowboy_req:parse_qs(Req0),
    Opts = parse_pagination(QS),
    case get_{aggregates}_page:execute(Opts) of
        {ok, Result} ->
            hecate_api_utils:json_ok(Result, Req0);
        {error, Reason} ->
            hecate_api_utils:json_error(500, Reason, Req0)
    end.

parse_pagination(QS) ->
    lists:foldl(fun({K, V}, Acc) ->
        case K of
            <<"page">> ->
                case catch binary_to_integer(V) of
                    I when is_integer(I), I > 0 -> Acc#{page => I};
                    _ -> Acc
                end;
            <<"page_size">> ->
                case catch binary_to_integer(V) of
                    I when is_integer(I), I > 0 -> Acc#{page_size => I};
                    _ -> Acc
                end;
            _ -> Acc
        end
    end, #{}, QS).
```

---

## Rule 3: Filtered Queries Include the Filter in the Name

When a query filters by a specific field, name it explicitly.

| Module Name | What It Screams |
|-------------|----------------|
| `get_cartwheels_by_torch` | "I return cartwheels belonging to a specific torch" |
| `get_findings_by_cartwheel` | "I return findings for a specific cartwheel" |
| `get_torches_by_status` | "I return torches matching a status filter" |

These filtered queries SHOULD also be paged when the result set could be large.

---

## Complete QRY Naming Convention

| Pattern | Module Name | Intent |
|---------|-------------|--------|
| Single by PK | `get_{aggregate}_by_id` | One record by primary key |
| Single by unique field | `get_{aggregate}_by_{field}` | One record by unique constraint |
| Single active/current | `get_active_{aggregate}` | The one currently active |
| Paged list | `get_{aggregates}_page` | Bounded page of records |
| Filtered paged list | `get_{aggregates}_by_{field}` | Filtered + paged |
| Search | `search_{aggregates}` | Full-text search (paged) |
| Count/stats | `count_{aggregates}` | Aggregate statistics |

---

## Migration: Renaming Existing Modules

When renaming a query module (e.g., `get_torch` → `get_torch_by_id`):

### Checklist

1. **Rename directory**: `src/get_torch/` → `src/get_torch_by_id/`
2. **Create new module files** with updated `-module()` declarations
3. **Delete old files** from the renamed directory
4. **Update `rebar.config`**: `src_dirs` entry
5. **Update callers**:
   - Route in `hecate_api_routes.erl`
   - Any API handler calling `old_module:execute/1`
   - Any internal caller (e.g., `get_active_*_api.erl`)
6. **Delete stale beam files**: `_build/default/lib/*/ebin/old_module.beam`
7. **Compile + test**: `rebar3 compile && rebar3 eunit`

---

## Anti-Patterns

| Anti-Pattern | Correct | Why |
|--------------|---------|-----|
| `get_torch` | `get_torch_by_id` | "get how?" is ambiguous |
| `list_torches` | `get_torches_page` | Unbounded result set |
| `get_all_torches` | `get_torches_page` | "all" is a scaling time bomb |
| `find_torches` | `get_torches_page` or `search_torches` | "find" is vague |
| `query_torches` | Specific module per query | Provider module, not a spoke |

---

## Fine-Tuning Examples

### Example 1: Wrong (Ambiguous Get)

**Input:** "Create a query to fetch a torch"

**Wrong Output:**
```erlang
-module(get_torch).
execute(TorchId) -> ...
```

**Why wrong:** Ambiguous. Get by what? Name? ID? Status?

### Example 2: Correct (Screaming Get)

**Input:** "Create a query to fetch a torch by its ID"

**Correct Output:**
```erlang
-module(get_torch_by_id).
execute(TorchId) -> ...
```

### Example 3: Wrong (Unbounded List)

**Input:** "Create a query to list cartwheels"

**Wrong Output:**
```erlang
-module(list_cartwheels).
execute(Opts) ->
    Sql = "SELECT * FROM cartwheels",
    {ok, Rows} = store:query(Sql, []),
    {ok, [row_to_map(R) || R <- Rows]}.
```

**Why wrong:** Returns ALL cartwheels. No pagination. Will crash at scale.

### Example 4: Correct (Paged List)

**Input:** "Create a query to list cartwheels"

**Correct Output:**
```erlang
-module(get_cartwheels_page).
execute(Opts) ->
    Page = maps:get(page, Opts, 1),
    PageSize = min(maps:get(page_size, Opts, 50), 200),
    Offset = (Page - 1) * PageSize,
    Sql = "SELECT cartwheel_id, ... FROM cartwheels "
          "WHERE (status & ?1) = 0 "
          "ORDER BY initiated_at DESC "
          "LIMIT ?2 OFFSET ?3",
    case query_cartwheels_store:query(Sql, [?CW_ARCHIVED, PageSize, Offset]) of
        {ok, Rows} ->
            {ok, #{items => [row_to_map(R) || R <- Rows],
                   page => Page, page_size => PageSize}};
        {error, Reason} ->
            {error, Reason}
    end.
```

---

_Query names scream intent. Pages prevent disasters. Be explicit or be sorry._
