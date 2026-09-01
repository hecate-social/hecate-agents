---
title: "ANTIPATTERNS: Erlang/OTP Gotchas"
layer: skill
audience: [agent, human]
stage: stable
---

# ANTIPATTERNS: Erlang/OTP Gotchas

*Demons about Erlang-specific pitfalls. esqlite3, maps, gen_server, and OTP.*

[Back to Index](INDEX.md)

---

## 🔥 Demon 19: esqlite3 Returns Lists, Not Tuples

**Date exorcised:** 2026-02-12
**Where it appeared:** 12+ `row_to_map/1` functions across all `query_*` apps
**Cost:** Silent pattern match failure — queries returned `{ok, []}` instead of data

### The Lie

"SQLite rows come back as tuples like `{Col1, Col2, Col3}`."

### What Happened

All `row_to_map/1` functions used tuple patterns:
```erlang
%% WRONG — tuples
row_to_map({Id, Name, Status, Label}) ->
    #{id => Id, name => Name, status => Status, label => Label}.
```

But `esqlite3:fetchall/1` returns rows as **lists of lists**, not lists of tuples:
```erlang
{ok, [[<<"id1">>, <<"name1">>, 1, <<"Active">>],
      [<<"id2">>, <<"name2">>, 2, <<"Done">>]]}
```

The tuple pattern `{Id, Name, Status, Label}` never matches `[Id, Name, Status, Label]`, so the list comprehension `[row_to_map(R) || R <- Rows]` silently produces an empty list.

### The Correct Pattern

```erlang
%% CORRECT — lists
row_to_map([Id, Name, Status, Label]) ->
    #{id => Id, name => Name, status => Status, label => Label}.
```

### Why This Happens

1. Many Erlang database libraries (mnesia, ets) return tuples
2. The mental model "row = tuple" is deeply ingrained
3. Without tests that exercise the full query path, the bug only surfaces at runtime
4. The empty-list result looks like "no data" rather than "pattern match failed"

### Prevention

```erlang
%% Test that row_to_map works with list input
row_to_map_test() ->
    Row = [<<"id">>, <<"name">>, 1, <<"Active">>],
    Result = row_to_map(Row),
    ?assertEqual(<<"id">>, maps:get(id, Result)).
```

### The Lesson

> **esqlite3 returns lists. Always. Test your row_to_map with actual list inputs.**
> **This bug was in the BIT_FLAGS_STATUS_PROJECTION.md template — the "correct" example was wrong.**

---

## 🔥 Demon 20: Eager Default in `maps:get/3`

**Date exorcised:** 2026-02-12
**Where it appeared:** 10 command modules (`*_v1.erl`) in `mentor_llms`
**Cost:** `noproc` crash when gen_server not running (e.g., in tests)

### The Lie

"Use `maps:get(key, Map, default_value())` to provide a fallback."

### What Happened

Command modules used function calls as `maps:get/3` defaults:
```erlang
%% WRONG — hecate_identity:agent_id() is ALWAYS called
SubmitterId = maps:get(<<"submitter_id">>, Map, hecate_identity:agent_id()),
```

In Erlang, `maps:get(Key, Map, Default)` evaluates `Default` **before** checking if `Key` exists. When `Default` is a function call to a gen_server (`hecate_identity`), it crashes with `noproc` if that server isn't running — even when the key IS present in the map.

### The Correct Pattern

```erlang
%% CORRECT — lazy evaluation via case
SubmitterId = case maps:find(<<"submitter_id">>, Map) of
    {ok, V} -> V;
    error -> hecate_identity:agent_id()
end,
```

### Why This Happens

1. `maps:get/3` looks like it should be lazy (only use default when key is missing)
2. In many other languages, default values ARE lazy (Python's `dict.get(k, v)` doesn't evaluate `v` eagerly)
3. Erlang evaluates all function arguments before calling the function — there are no lazy arguments
4. The bug only manifests when the gen_server isn't running, which may not happen in production but always happens in unit tests

### When This Matters

| Default Expression | Safe? | Why |
|-------------------|-------|-----|
| `maps:get(k, M, <<>>)` | Yes | Literal — no side effects |
| `maps:get(k, M, undefined)` | Yes | Atom literal |
| `maps:get(k, M, 0)` | Yes | Integer literal |
| `maps:get(k, M, gen_server:call(...))` | **NO** | Evaluated even when key exists |
| `maps:get(k, M, hecate_identity:agent_id())` | **NO** | gen_server call, crashes if down |
| `maps:get(k, M, os:timestamp())` | **NO** | Side effect always runs |

### The Lesson

> **Never use function calls as `maps:get/3` defaults. Use `maps:find/2` + `case` for lazy evaluation.**
> **If the default is a literal, `maps:get/3` is fine. If it's a function call, it's a bug waiting to happen.**

---

## 🔥 Demon 21: esqlite3 Argument Order (Db First)

**Date exorcised:** 2026-02-12
**Where it appeared:** 8 `query_*_store.erl` files
**Cost:** `function_clause` crash on store initialization

### The Lie

"Just call `esqlite3:exec(SQL, Db)` — SQL first, then connection."

### What Happened

Store `init/1` functions had the arguments reversed:
```erlang
%% WRONG — SQL first
esqlite3:exec("PRAGMA journal_mode=WAL", Db),
esqlite3:exec("CREATE TABLE IF NOT EXISTS ...", Db),
```

The esqlite3 API is `esqlite3:exec(Db, SQL)` — connection first:
```erlang
%% CORRECT — Db first
esqlite3:exec(Db, "PRAGMA journal_mode=WAL"),
esqlite3:exec(Db, "CREATE TABLE IF NOT EXISTS ..."),
```

### The API

| Function | Signature | Note |
|----------|-----------|------|
| `esqlite3:open/1` | `open(Path)` | Returns `{ok, Db}` |
| `esqlite3:exec/2` | `exec(Db, SQL)` | **Db first** |
| `esqlite3:q/2` | `q(Db, SQL)` | **Db first** |
| `esqlite3:q/3` | `q(Db, SQL, Params)` | **Db first** |

### Why This Happens

1. Many Erlang database APIs use `Module:exec(SQL, Connection)` — SQL first
2. The "subject.verb(object)" pattern (`SQL.exec_on(Db)`) feels natural
3. esqlite3 follows the C SQLite API convention where the handle comes first
4. Without type specs or dialyzer, the crash only appears at runtime

### The Lesson

> **esqlite3: connection (Db) ALWAYS comes first. `exec(Db, SQL)`, `q(Db, SQL, Params)`.**
> **When in doubt, check the esqlite3 source — don't assume argument order from other libraries.**

---

## 🔥 gen_server Self-Call Deadlock

**Date:** 2026-03-05
**Origin:** reckon_db emitter pool fix deadlocked on `is_active/1`

### The Antipattern

Calling a gen_server from within its own process via a function that does `gen_server:call`:

```erlang
%% In reckon_db_leader handle_cast({activate, StoreId}, State):
%%   → save_default_subscriptions(StoreId)
%%     → subscribe/5
%%       → setup_event_notification
%%         → reckon_db_leader:is_active(StoreId)  %% gen_server:call back to self!
%%           → DEADLOCK
```

### Why It's Wrong

`gen_server:call` sends a message and waits for a reply. If the target is the calling process itself, the process is already handling a message and can't process the call. Erlang raises `{calling_self, {gen_server, call, [...]}}`.

### The Correct Pattern

Use a non-blocking check that doesn't require the gen_server to respond:

```erlang
%% CORRECT — check if the supervisor process exists via whereis/1
SupName = reckon_db_naming:emitter_sup_name(StoreId),
maybe_start_emitter_pool(StoreId, Key, Sub, whereis(SupName)).

maybe_start_emitter_pool(_StoreId, _Key, _Sub, undefined) -> ok;
maybe_start_emitter_pool(StoreId, Key, Sub, _SupPid) ->
    case reckon_db_emitter_pool:start_emitter(StoreId, Sub) of
        {ok, _Pid} ->
            logger:info("Started emitter pool for ~s (store: ~p)", [Key, StoreId]);
        {error, {already_started, _}} -> ok;
        {error, _} -> ok
    end.
```

**Rule:** If a function might be called from inside a gen_server, never use `gen_server:call` to query that same server. Use `whereis/1`, ETS lookups, or process dictionary reads instead.

---

## 🔥 Demon 38: Emoji Literals in esqlite3 SQL Strings

**Date exorcised:** 2026-03-06
**Where it appeared:** `project_launcher_store:create_tables/1` and 3 projection modules
**Cost:** `badarg` crash on startup — daemon crash-loops, never boots

### The Lie

"Just put the emoji in the SQL DEFAULT value: `DEFAULT '🔌'`."

### What Happened

The SQLite CREATE TABLE statement had emoji literals in DEFAULT clauses:
```erlang
%% WRONG — emoji literal in SQL string
esqlite3:exec(Db,
    "CREATE TABLE IF NOT EXISTS launcher_entries ("
    "  icon TEXT NOT NULL DEFAULT '🔌'"
    ");").
```

`esqlite3_nif:exec/2` received a charlist containing codepoints > 127 (the plug emoji U+1F50C is 4 bytes in UTF-8: `\xF0\x9F\x94\x8C`). The NIF expects iodata but chokes on the high codepoints, crashing with `badarg`. The error is buried in a kernel pid termination message — no clear "emoji bad" error.

### The Correct Pattern

Use UTF-8 byte escapes in Erlang binaries, passed as bind parameters:
```erlang
%% CORRECT — byte escapes in binary, passed as bind parameter
project_launcher_store:execute(
    "INSERT INTO launcher_groups (name, icon) VALUES (?1, ?2)",
    [GroupName, <<"\xF0\x9F\x93\x81">>]).

%% CORRECT — empty default in DDL, real icon comes from event data
"icon TEXT NOT NULL DEFAULT ''"
```

For SQL DDL (CREATE TABLE), use empty string defaults — the actual emoji values come from event data via bind parameters, which handle UTF-8 correctly.

### Common Emoji Byte Sequences

| Emoji | UTF-8 Bytes | Erlang Binary |
|-------|-------------|---------------|
| 📁 (folder) | F0 9F 93 81 | `<<"\xF0\x9F\x93\x81">>` |
| 🔌 (plug) | F0 9F 94 8C | `<<"\xF0\x9F\x94\x8C">>` |
| ⚙️ (gear) | E2 9A 99 EF B8 8F | `<<"\xE2\x9A\x99\xEF\xB8\x8F">>` |

### Why This Happens

1. Erlang strings are charlists — `"🔌"` is `[128268]`, a single codepoint > 127
2. esqlite3's NIF expects iodata (binaries or byte-range charlists)
3. High codepoints in charlists are NOT valid iodata
4. The crash message is opaque: `{badarg, [{esqlite3_nif, exec, [#Ref<...>, [67,82,69,65,...]]}]}` — just raw codepoints
5. Binaries with `\xNN` escapes ARE valid UTF-8 iodata and pass through cleanly

### The Lesson

> **Never put emoji (or any non-ASCII) directly in esqlite3 SQL strings.**
> **Use `<<"\xF0\x9F\x...">>` binaries as bind parameters. Use empty defaults in DDL.**
> **This applies to all esqlite3 functions: `exec/2`, `prepare/2`, etc.**

---

## 🔥 Demon 60: Hard Binary-Key Matching on Mesh RPC Payloads

**Date exorcised:** 2026-09-01
**Where it appeared:** 12+ entry points across `hecate-rag` (`route/2` clauses, every `*_v1.erl` command's `from_map/1`, `search_chunks_semantic`, `list_chunks_by_source`, `list_sources_page`, `maybe_classify_topics`) — `hecate_om_wire.erl`'s own moduledoc names `hecate-dns`/`-git`/`-llm`/`-rag` as the same "zero tolerance" victims
**Cost:** Most of `hecate-rag`'s mesh RPC surface silently broken for real callers — three failure shapes depending on where the mismatch landed, none of them looking like "this request was well-formed and still failed"

### The Lie

"The caller sent JSON with string keys, so the payload map I receive will have binary keys — I can pattern-match `#{<<"field">> := V}` directly."

### What Happened

macula's frame decoder round-trips an inbound RPC payload's keys through `binary_to_existing_atom/1` on the way in. A caller that sends `{"corpus_id": "x"}` over the wire gets it delivered server-side as `#{corpus_id => <<"x">>}` — an **atom**-keyed map — not `#{<<"corpus_id">> => <<"x">>}`. Every handler in `hecate-rag` was written assuming the opposite, and this produced three different-looking failures depending on exactly where the assumption lived:

```erlang
%% route/2 destructuring the key directly (get_document_verbatim,
%% get_chunk_by_id, get_source_by_id) -- the clause's OWN pattern fails
%% to match, Erlang falls through every other clause, and lands on the
%% generic catch-all:
route(<<"hecate-rag.get_document_verbatim">>, #{<<"source_path">> := Path}) ->
    get_document_verbatim:handle(Path);
...
route(Other, _P) ->
    {error, {unknown_method, Other}}.
%% Result: a real, deployed, correctly-advertised procedure comes back
%% "unknown_method" -- indistinguishable from a typo'd procedure name
%% or a station that never learned the route. A multi-hour investigation
%% chased tombstone races, gossip propagation lag, and ADVERTISE-burst
%% rate limits before finding this -- all plausible, all wrong, because
%% the actual symptom (unknown_method) looks EXACTLY like a routing
%% failure, not an argument-shape failure.

%% A command's from_map/1 destructuring the key one level down
%% (detect_corpus_change_v1, schedule_reembed_v1, and 7 others) --
%% route/2 itself matches fine (it just passes the whole map through),
%% but the command's own hard pattern fails and falls through to ITS
%% catch-all:
from_map(#{<<"corpus_id">> := Id} = Map) -> {ok, #detect_corpus_change_v1{...}};
from_map(_) -> {error, missing_aggregate_id}.
%% Result: a call with a genuinely present, correctly-typed corpus_id
%% comes back "missing_aggregate_id" -- a BELIEVABLE, domain-sounding
%% validation error that is not what actually happened. This is the
%% dangerous one: it doesn't crash, doesn't route-fail, just quietly
%% tells the caller they forgot a field they didn't forget. A live test
%% that "successfully" reached this exact error was wrongly read as
%% proof the call worked, sending the same investigation down a wrong
%% path for over an hour before the real mechanism was found.

%% search_chunks_semantic's dual-shape dispatch -- neither shape
%% matches, falls through to the map-shaped catch-all:
handle(#{<<"query_vector">> := V} = P) when is_list(V) -> ...;
handle(#{<<"query_text">> := T} = P) when is_binary(T) -> ...;
handle(Params) when is_map(Params) -> {error, query_text_or_vector_required}.
%% Result: a caller who DID supply query_text gets told they supplied
%% neither query_text nor query_vector.
```

### The Correct Pattern

Use `hecate_om_wire:field/2,3` (ships with `hecate_om`) everywhere a payload's own field is read — it tries the atom form first, then the binary form, so it's correct for BOTH a mesh-delivered (atom-keyed) payload and an HTTP/jsx-decoded (binary-keyed only) payload hitting the same code path:

```erlang
%% route/2 -- pass the whole map through instead of destructuring in
%% the clause head, and look the field up where it's actually needed:
route(<<"hecate-rag.get_document_verbatim">>, P) ->
    get_document_verbatim:handle(hecate_om_wire:field(<<"source_path">>, P));

%% from_map/1 -- dispatch on the field's presence via a second function
%% clause (idiomatic, no case/if) instead of pattern-matching it in the
%% head:
from_map(Map) when is_map(Map) ->
    from_map_(hecate_om_wire:field(<<"corpus_id">>, Map), Map);
from_map(_) ->
    {error, missing_aggregate_id}.

from_map_(undefined, _Map) -> {error, missing_aggregate_id};
from_map_(Id, Map) ->
    {ok, #detect_corpus_change_v1{
        corpus_id = Id,
        source_path = hecate_om_wire:field(<<"source_path">>, Map),
        ...
    }}.
```

A payload's own nested VALUES (e.g. a list of hit-maps the caller round-trips back from a prior response, or a stored chunk's own metadata map read back from the store) are a different, unconfirmed hazard — they weren't touched by this fix. Only TOP-LEVEL keys of the payload macula's decoder itself atomizes are the confirmed, in-scope mechanism here.

### Why This Happens

1. `from_map/1` *reads* as "decode this map from the wire," so writing it as "the wire always sends binary keys" feels self-evidently true — it's exactly backwards for macula specifically.
2. The three failure shapes above (`unknown_method`, a believable domain error, a believable "you forgot a field" error) are all indistinguishable from a genuinely different, unrelated bug — none of them look like an argument-decoding problem, so debugging effort gets spent everywhere except the actual mechanism.
3. `hecate_om_wire.erl` — the fix already existed, ships with `hecate_om`, and its own moduledoc names this exact codebase as a known victim — but nothing forced any of the affected `route/2`/`from_map/1` call sites to actually adopt it. A correct library sitting unused fixes nothing.
4. A test that reaches a downstream, domain-shaped error looks like a passing test at a glance. `detect_corpus_change_from_map_does_not_silently_lose_a_real_corpus_id_test` — asserting the result is NOT `{error, missing_aggregate_id}` given a real corpus_id — is the shape of test this bug needs; a test that only checks "does this return an error tuple of SOME kind" would pass on both the broken and fixed code.

### The Lesson

> **Any function reading a field out of a payload that arrived over the mesh — `route/2` clauses, every `from_map/1`, every `handle/1` — must use `hecate_om_wire:field/2,3`, never a hard `#{<<"key">> := V}` pattern or `maps:get(<<"key">>, Map, Default)` literal. If `hecate_om_wire.erl`'s own moduledoc names your service, grep your `route/2` and every `from_map/1` for `#{<<"` in a function head — don't wait to find each one live.**
> **When a fresh finding overturns your own recent conclusion mid-investigation, say so plainly and explain the mechanism precisely, rather than quietly folding it in as if it were expected.**

---

*We burned these demons so you don't have to. Keep the fire going.*
