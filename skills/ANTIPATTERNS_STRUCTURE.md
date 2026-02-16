# ANTIPATTERNS: Structure — Code Organization Violations

*Demons about code structure. Vertical slicing, not horizontal layers.*

[Back to Index](ANTIPATTERNS.md)

---

## 🔥 Incomplete Desks / Flat Workers

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
- No desk supervisor (`*_desk_sup.erl`)
- No responder (`*_responder_v1.erl`) — can't receive HOPEs from mesh
- Emitters exist but float orphaned at domain level

### The Rule

> **Domain supervisors ONLY start desk supervisors + shared infra.**
> **Desk supervisors start all workers for that desk.**

```erlang
%% GOOD: Domain sup → Desk sups → Workers
manage_capabilities_sup
├── manage_capabilities_store         % Shared infra (OK at domain level)
├── announce_capability_desk_sup      % Supervisor
│   ├── announce_capability_responder_v1    % Worker
│   └── capability_announced_v1_to_mesh     % Worker
├── update_capability_desk_sup        % Supervisor
│   └── ...
└── retract_capability_desk_sup       % Supervisor
    └── ...
```

### Complete Desk Checklist

Every CMD desk MUST have:
- [ ] `*_desk_sup.erl` — Desk supervisor
- [ ] `*_v1.erl` — Command record
- [ ] `*_v1.erl` — Event record
- [ ] `maybe_*.erl` — Handler
- [ ] `*_responder_v1.erl` — HOPE → Command (mesh inbound)
- [ ] `*_to_mesh.erl` — Event → FACT emitter (mesh outbound)

Optional:
- [ ] `on_{event}_maybe_*.erl` — Policy/PM for cross-domain

### Why It Matters

Without responders, your domain can emit but not receive. You have a mouth but no ears.

Without desk supervisors, your supervision tree is flat and you lose fault isolation per feature.

See [CARTWHEEL.md](../philosophy/CARTWHEEL.md) for the complete canonical structure.

---

## 🔥 Listeners as Separate Desks

**Date:** 2026-02-08
**Origin:** Division listener architecture discussion

### The Antipattern

Creating a listener as its own desk when it only serves one other desk.

**Example (WRONG):**
```
design_division/src/
├── subscribe_to_division_discovered/      # Separate desk
│   ├── subscribe_to_division_discovered.erl
│   └── subscribe_to_division_discovered_sup.erl
│
└── initiate_division/                     # The desk it triggers
    ├── initiate_division_v1.erl
    └── maybe_initiate_division.erl
```

This is **horizontal thinking in disguise** — grouping by "listeners" vs "commands".

### The Rule

> **If a listener's sole purpose is to trigger desk X, it lives IN desk X.**

### The Correct Structure

```
design_division/src/
└── initiate_division/
    ├── initiate_division_v1.erl
    ├── division_initiated_v1.erl
    ├── maybe_initiate_division.erl
    ├── subscribe_to_division_discovered.erl            # Lives here
    └── on_division_discovered_maybe_initiate_division.erl  # Lives here
```

The desk owns **everything needed to initiate divisions** — including how it gets triggered.

### When Listeners CAN Be Separate

A listener MAY be its own desk when:
- It triggers **multiple** different desks based on message content
- It's truly general-purpose infrastructure (rare)
- It serves a query service, not a command service

### The Test

Ask: "Does this listener exist ONLY to trigger desk X?"
- **Yes** → Put it in desk X
- **No** → Consider a separate desk (but think hard)

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
├── venture_initiated_listener.erl
├── division_discovered_listener.erl
└── capability_announced_listener.erl
```

Or within a domain:
```
apps/design_division/src/
├── design_division_listeners_sup.erl     # Still wrong!
├── listeners/                             # Horizontal directory
│   ├── division_discovered_listener.erl
│   └── ...
```

### The Rule

> **Each listener belongs to the desk it triggers, supervised by that desk's supervisor.**

### The Correct Structure

```
apps/design_division/src/
├── initiate_division/
│   ├── initiate_division_desk_sup.erl                          # Desk supervisor
│   └── on_division_discovered_v1_from_pg_maybe_initiate_division.erl
│
└── complete_division/
    ├── complete_division_desk_sup.erl                          # Desk supervisor
    └── on_all_desks_implemented_v1_from_pg_maybe_complete_division.erl
```

### Why It Matters

- **Fault isolation** — Listener crash only affects its desk
- **Discoverability** — To understand desk X, look only in `X/`
- **No orphans** — Every listener has a clear owner
- **Vertical slicing** — No horizontal grouping by technical concern

See [INTEGRATION_TRANSPORTS.md](../philosophy/INTEGRATION_TRANSPORTS.md) for desk structures.

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

Each desk owns its API handler:

```erlang
✅ CORRECT: Handler lives in its desk
apps/mentor_agents/src/validate_learning/
├── validate_learning_v1.erl
├── learning_validated_v1.erl
├── maybe_validate_learning.erl
└── validate_learning_api.erl    # ~30 lines, single-purpose
```

### The Lesson

> **API handlers are part of the desk, not part of the API app.**
> One endpoint = one `*_api.erl` file in the desk directory.
> Each handler exports `routes/0` — the central aggregator discovers them automatically.
> See Demon #25 for why centralized route files are wrong.

### How This Was Fixed

Replaced 11 god modules (1,700+ lines) with 50 desk-based handlers (~30-50 lines each).
All handlers use `hecate_api_utils` from the `shared` app for response helpers.
Routes standardized under `/api/` prefix.

Reference: `skills/codegen/erlang/CODEGEN_ERLANG_TEMPLATES.md` → API Handler Templates

---

## 🔥 Demon 18: Process Managers as Separate Slices

**Date exorcised:** 2026-02-12
**Where it appeared:** `guide_node_lifecycle` — `on_llm_detected_announce_capability/`, `on_llm_removed_retract_capability/`, `on_llm_status_reported_update_capability/`
**Cost:** PMs scattered as standalone slices instead of being part of the desk they serve

### The Lie

"A process manager is a first-class architectural element. It deserves its own slice directory."

### Why It's Wrong

A PM has ONE job: react to an event and dispatch a command to a specific desk. That makes it a **policy** of that desk — part of the desk's capability (Inbox → Policy → Command). Giving it a separate slice:

1. **Scatters the desk's logic** — to understand `announce_capability`, you must also find `on_llm_detected_announce_capability/` elsewhere
2. **Creates false peers** — the PM looks like a sibling of `announce_capability` when it's actually a subordinate
3. **Horizontal in disguise** — a `on_*` directory is just a `process_managers/` folder with extra steps

### The Rule

> **A PM is a policy of the desk it triggers. It lives INSIDE that desk's directory.**

```
announce_capability/
├── announce_capability_v1.erl                    # Command
├── capability_announced_v1.erl                   # Event
├── maybe_announce_capability.erl                 # Handler
├── announce_capability_dispatch.erl              # Dispatch
└── on_llm_detected_announce_capability.erl       # Policy (inbox + decision)
```

### The Desk Capability Model

Every desk is a complete capability with three aspects:

| Aspect | What It Is | Lives In |
|--------|-----------|----------|
| **Inboxes** | Topics this desk subscribes to (pg + mesh) | Policy modules inside the desk |
| **Policies** | Decision rules: event arrives → dispatch command? | Policy modules inside the desk |
| **Emitters** | Facts published to mesh after success | Emitter modules inside the desk |

### The Test

> "If I delete this desk directory, does everything related to this capability disappear?"
>
> If no — some logic is scattered in a separate PM slice — that PM belongs inside the desk.

### The Lesson

> **There is no such thing as a "PM slice." A PM is a policy. Policies belong to desks.**

---

---

## 🔥 Demon 25: Centralized Route Registration Files

**Date exorcised:** 2026-02-16
**Where it appeared:** 15 `*_routes.erl` files across all hecate apps
**Cost:** 15 centralized files deleted, ~102 handlers updated

### The Demon

One file per OTP app that lists all routes for that app's handlers:

```erlang
❌ WRONG: Centralized route file knows about all handlers
-module(breed_snake_gladiators_routes).
-export([routes/0]).

routes() ->
    [{"/api/arcade/gladiators/stables", initiate_stable_api, []},
     {"/api/arcade/gladiators/stables/:stable_id", get_stable_api, []},
     {"/api/arcade/gladiators/stables/:stable_id/champion/duel", start_champion_duel_api, []},
     {"/api/arcade/gladiators/heroes", heroes_api, []},
     {"/api/arcade/gladiators/heroes/:hero_id", get_hero_api, []},
     {"/api/arcade/gladiators/heroes/:hero_id/duel", start_hero_duel_api, []}].
```

### Why It's Wrong

- **Horizontal grouping** — routes are grouped by app, not by the handler that serves them
- **Every new handler requires editing TWO files** — the handler AND the route file
- **Route file knows too much** — it must import or reference every handler module
- **Merge conflicts** — multiple feature branches touching the same routes file
- **Violates screaming architecture** — a handler's URL path is part of its identity, not a separate concern
- **Same demon as #14** (God Module API Handlers) applied to routing

### The Correct Pattern

Each Cowboy handler exports `routes/0` declaring its own routes:

```erlang
✅ CORRECT: Handler owns its route
-module(initiate_stable_api).
-export([init/2, routes/0]).

routes() ->
    [{"/api/arcade/gladiators/stables", ?MODULE, []}].

init(Req0, State) ->
    %% ...
```

A single central aggregator discovers all handlers via OTP module introspection:

```erlang
✅ CORRECT: Auto-discovery aggregator (the ONLY central file)
-module(hecate_api_routes).
-export([compile/0]).

-define(HECATE_APPS, [hecate_api, guide_venture_lifecycle, ...]).

compile() ->
    cowboy_router:compile([{'_', discover_routes()}]).

discover_routes() ->
    lists:flatmap(fun collect_app_routes/1, ?HECATE_APPS).

collect_app_routes(App) ->
    Mods = app_modules(App),
    Handlers = [M || M <- Mods, M =/= ?MODULE, exports_routes(M)],
    lists:flatmap(fun(M) -> M:routes() end, Handlers).

app_modules(App) ->
    case application:get_key(App, modules) of
        {ok, Mods} -> Mods;
        _ -> []
    end.

exports_routes(Mod) ->
    code:ensure_loaded(Mod),
    erlang:function_exported(Mod, routes, 0).
```

### The Key Insight

Adding a new API endpoint requires touching exactly ONE file — the handler itself. The aggregator discovers it automatically because it exports `routes/0`.

### The Test

> "Can I add a new API endpoint by creating a single file?"
>
> **Yes** → routes/0 auto-discovery is working.
> **No, I also need to edit a routes file** → you have a centralized route file demon.

### The Lesson

> **Route ownership follows handler ownership.**
> The handler IS the route. The handler declares its path.
> The aggregator discovers — it never enumerates.

*Add more demons as we exorcise them.* 🔥🗝️🔥
