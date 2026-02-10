# CODEGEN_ERLANG_NAMING.md — Erlang Code Naming Conventions

_Strict naming rules for all components in the Cartwheel architecture._

**Target:** Erlang/OTP with `reckon_evoq`

**Related files:**
- [CODEGEN_ERLANG_TEMPLATES.md](CODEGEN_ERLANG_TEMPLATES.md) — Code templates
- [CODEGEN_ERLANG_CHECKLISTS.md](CODEGEN_ERLANG_CHECKLISTS.md) — Generation checklists
- [../../NAMING_CONVENTIONS.md](../../NAMING_CONVENTIONS.md) — Consolidated quick-reference

---

## Master Naming Table

| Component      | Pattern                      | Example                                     |
| -------------- | ---------------------------- | ------------------------------------------- |
| Domain app     | `{verb}_{noun}`              | `manage_capabilities`                       |
| Query app      | `query_{noun}`               | `query_capabilities`                        |
| Spoke dir      | `{command}/`                 | `announce_capability/`                      |
| Spoke sup      | `{command}_spoke_sup`        | `announce_capability_spoke_sup`             |
| Command        | `{command}_v1`               | `announce_capability_v1`                    |
| Event          | `{noun}_{past_verb}_v1`      | `capability_announced_v1`                   |
| Handler        | `maybe_{command}`            | `maybe_announce_capability`                 |
| CMD API        | `{command}_api`              | `announce_capability_api`                   |
| Responder      | `{command}_responder_v1`     | `announce_capability_responder_v1`          |
| Emitter (mesh) | `{event}_to_mesh`            | `capability_announced_to_mesh`              |
| Emitter (pg)   | `{event}_to_pg`              | `capability_announced_to_pg`                |
| Aggregate      | `{noun}_aggregate`           | `capability_aggregate`                      |
| Projection     | `{event}_to_{read_store}`    | `capability_announced_to_capabilities`      |
| Policy/PM      | `on_{event}_maybe_{command}` | `on_llm_detected_maybe_announce_capability` |
| Query (by PK)  | `get_{noun}_by_id`           | `get_capability_by_id`                      |
| QRY by-ID API  | `get_{noun}_by_id_api`       | `get_capability_by_id_api`                  |
| Query (paged)  | `get_{nouns}_page`           | `get_capabilities_page`                     |
| QRY paged API  | `get_{nouns}_page_api`       | `get_capabilities_page_api`                 |
| Tests          | `{module}_tests`             | `capability_aggregate_tests`                |

---

## Query Module Naming (Critical)

### Rule 1: No Raw `get_{aggregate}` — Use `get_{aggregate}_by_id`

A module named `get_torch` is ambiguous. Get it how? By name? By status?

**The name MUST specify the lookup strategy.**

| Module Name | What It Screams |
|-------------|----------------|
| `get_torch_by_id` | "I fetch one torch by its primary key" |
| `get_torch_by_name` | "I fetch one torch by its unique name" |
| `get_active_torch` | "I fetch the currently active torch" |

### Rule 2: No Raw `list_{aggregates}` — Use `get_{aggregates}_page`

A module named `list_torches` sounds like it returns ALL torches. That's a scaling time bomb.

**Every list query MUST be paged by default.**

| Name | Screams | Danger |
|------|---------|--------|
| `list_torches` | "Give me everything" | Unbounded. Will crash at scale. |
| `get_all_torches` | "Give me literally all of them" | Even worse. Explicit unbounded intent. |
| `get_torches_page` | "Give me one page" | Bounded. Safe. Correct. |

### Rule 3: Filtered Queries Include the Filter in the Name

| Module Name | What It Screams |
|-------------|----------------|
| `get_cartwheels_by_torch` | "I return cartwheels belonging to a specific torch" |
| `get_findings_by_cartwheel` | "I return findings for a specific cartwheel" |
| `get_torches_by_status` | "I return torches matching a status filter" |

### Complete QRY Naming Convention

| Pattern | Module Name | Intent |
|---------|-------------|--------|
| Single by PK | `get_{aggregate}_by_id` | One record by primary key |
| Single by unique field | `get_{aggregate}_by_{field}` | One record by unique constraint |
| Single active/current | `get_active_{aggregate}` | The one currently active |
| Paged list | `get_{aggregates}_page` | Bounded page of records |
| Filtered paged list | `get_{aggregates}_by_{field}` | Filtered + paged |
| Search | `search_{aggregates}` | Full-text search (paged) |
| Count/stats | `count_{aggregates}` | Aggregate statistics |

### Route Alignment

| Module | Route | HTTP |
|--------|-------|------|
| `get_torch_by_id_api` | `/api/torches/:torch_id` | GET |
| `get_active_torch_api` | `/api/torch` | GET |
| `get_torches_page_api` | `/api/torches` | GET |
| `get_cartwheel_by_id_api` | `/api/cartwheels/:cartwheel_id` | GET |
| `get_cartwheels_page_api` | `/api/cartwheels` | GET |

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

## Migration: Renaming Existing Modules

When renaming a query module (e.g., `get_torch` -> `get_torch_by_id`):

### Checklist

1. **Rename directory**: `src/get_torch/` -> `src/get_torch_by_id/`
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

## Banned Suffixes (Technical Noise)

These suffixes reveal implementation, not intent:

- `*_handler`, `*_manager`, `*_processor`, `*_worker`, `*_service`, `*_helper`, `*_util`, `*_impl`

## Allowed Suffixes (With Meaning)

- `*_v1` (Version)
- `*_spoke_sup` (Spoke supervisor)
- `*_responder_v1` (HOPE receiver)
- `*_to_mesh` (Emitter to mesh)
- `*_to_pg` (Emitter to pg)
- `*_to_{table}` (Projection)
- `*_store` (Storage accessor)
- `*_api` (API handler)

---

_Names scream intent. Pages prevent disasters. Be explicit or be sorry._
