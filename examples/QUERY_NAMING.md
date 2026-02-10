# Query Module Naming — Examples

*Canonical examples for query module naming in Hecate runtimes.*

**Date:** 2026-02-10
**Origin:** hecate-daemon torch/cartwheel query rename

---

## The Pattern

Query module names MUST scream their intent. Two rules:

1. **No raw `get_{aggregate}`** — specify the lookup strategy: `get_{aggregate}_by_id`
2. **No raw `list_{aggregates}`** — use paged queries: `get_{aggregates}_page`

---

## Wrong Way: Ambiguous Names

```
apps/query_torches/src/
├── get_torch/                    # Get by what?
│   ├── get_torch.erl
│   └── get_torch_api.erl
├── list_torches/                 # Returns ALL torches? Unbounded!
│   ├── list_torches.erl
│   └── list_torches_api.erl
└── ...
```

**Problems:**
- `get_torch` doesn't tell you the lookup strategy
- `list_torches` sounds like it returns everything — a scaling time bomb
- Adding `get_torch_by_name` later creates naming confusion
- A stranger reading the directory can't distinguish lookup strategies

---

## Correct Way: Screaming Names

```
apps/query_torches/src/
├── get_torch_by_id/              # One torch, by primary key
│   ├── get_torch_by_id.erl
│   └── get_torch_by_id_api.erl
├── get_active_torch/             # The currently active torch
│   ├── get_active_torch.erl
│   └── get_active_torch_api.erl
├── get_torches_page/             # Bounded page of torches
│   ├── get_torches_page.erl
│   └── get_torches_page_api.erl
└── ...
```

**Benefits:**
- Every name screams exactly what it does
- No ambiguity about lookup strategy
- "page" in the name enforces pagination
- New queries don't conflict

---

## Code Examples

### get_torch_by_id.erl (Single Lookup)

```erlang
-module(get_torch_by_id).

-export([execute/1]).

-spec execute(binary()) -> {ok, map()} | {error, not_found | term()}.
execute(TorchId) ->
    Sql = "SELECT torch_id, name, brief, status, status_label, "
          "initiated_at, initiated_by "
          "FROM torches WHERE torch_id = ?1",
    case query_torches_store:query(Sql, [TorchId]) of
        {ok, [Row]} ->
            {ok, row_to_map(Row)};
        {ok, []} ->
            {error, not_found};
        {error, Reason} ->
            {error, Reason}
    end.
```

### get_torches_page.erl (Paged List)

```erlang
-module(get_torches_page).

-include_lib("manage_torches/include/torch_status.hrl").

-export([execute/1]).

-define(DEFAULT_PAGE_SIZE, 50).
-define(MAX_PAGE_SIZE, 200).

-spec execute(map()) -> {ok, map()} | {error, term()}.
execute(Opts) ->
    Page = maps:get(page, Opts, 1),
    PageSize = min(maps:get(page_size, Opts, ?DEFAULT_PAGE_SIZE), ?MAX_PAGE_SIZE),
    Offset = (Page - 1) * PageSize,

    CountSql = "SELECT COUNT(*) FROM torches WHERE (status & ?1) = 0",
    {ok, [[Total]]} = query_torches_store:query(CountSql, [?TORCH_ARCHIVED]),

    Sql = "SELECT torch_id, name, brief, status, status_label, initiated_at "
          "FROM torches "
          "WHERE (status & ?1) = 0 "
          "ORDER BY initiated_at DESC "
          "LIMIT ?2 OFFSET ?3",
    case query_torches_store:query(Sql, [?TORCH_ARCHIVED, PageSize, Offset]) of
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

### get_cartwheels_by_torch.erl (Filtered + Paged)

```erlang
-module(get_cartwheels_by_torch).

-include_lib("manage_cartwheels/include/cartwheel_status.hrl").

-export([execute/2]).

-define(DEFAULT_PAGE_SIZE, 50).

-spec execute(binary(), map()) -> {ok, map()} | {error, term()}.
execute(TorchId, Opts) ->
    PageSize = min(maps:get(page_size, Opts, ?DEFAULT_PAGE_SIZE), 200),
    Page = maps:get(page, Opts, 1),
    Offset = (Page - 1) * PageSize,

    Sql = "SELECT cartwheel_id, torch_id, context_name, status, status_label "
          "FROM cartwheels "
          "WHERE torch_id = ?1 AND (status & ?2) = 0 "
          "ORDER BY initiated_at DESC "
          "LIMIT ?3 OFFSET ?4",
    case query_cartwheels_store:query(Sql, [TorchId, ?CW_ARCHIVED, PageSize, Offset]) of
        {ok, Rows} ->
            {ok, #{items => [row_to_map(R) || R <- Rows],
                   page => Page, page_size => PageSize}};
        {error, Reason} ->
            {error, Reason}
    end.
```

---

## Route Naming Alignment

Query module names should align with API routes:

| Module | Route | HTTP |
|--------|-------|------|
| `get_torch_by_id_api` | `/api/torches/:torch_id` | GET |
| `get_active_torch_api` | `/api/torch` | GET |
| `get_torches_page_api` | `/api/torches` | GET |
| `get_cartwheel_by_id_api` | `/api/cartwheels/:cartwheel_id` | GET |
| `get_cartwheels_page_api` | `/api/cartwheels` | GET |

---

## Complete Naming Table

| Query Type | Module Pattern | Example |
|------------|---------------|---------|
| Single by PK | `get_{agg}_by_id` | `get_torch_by_id` |
| Single by unique field | `get_{agg}_by_{field}` | `get_torch_by_name` |
| Single special | `get_active_{agg}` | `get_active_torch` |
| Paged list | `get_{aggs}_page` | `get_torches_page` |
| Filtered paged | `get_{aggs}_by_{field}` | `get_cartwheels_by_torch` |
| Full-text search | `search_{aggs}` | `search_torches` |
| Count/stats | `count_{aggs}` | `count_cartwheels` |

---

## Key Takeaways

1. **Names scream intent** — A stranger knows what the module does from its name
2. **No unbounded queries** — "page" in the name enforces pagination at the naming level
3. **Specify lookup strategy** — "by_id", "by_name", "by_torch" eliminates ambiguity
4. **Page size has a ceiling** — `MAX_PAGE_SIZE` prevents clients from requesting 10,000 rows
5. **Archived excluded by default** — Paged queries filter `(status & ?ARCHIVED) = 0`

---

## Training Note

When asked to create a query module:
- If it fetches ONE record → `get_{aggregate}_by_{lookup_field}`
- If it fetches MANY records → `get_{aggregates}_page` (paged, bounded)
- NEVER create `list_{aggregates}` or `get_all_{aggregates}`
- ALWAYS include pagination with a max page size cap

---

*Maintained by: Hecate Team*
*Last updated: 2026-02-10*
