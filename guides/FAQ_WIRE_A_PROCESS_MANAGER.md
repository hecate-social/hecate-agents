---
title: "FAQ: Wiring a Process Manager for Cross-Domain Integration"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Wire a Process Manager for Cross-Domain Integration?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

[FAQ: Connecting Phoenix LiveView to the Mesh](FAQ_CONNECT_PHOENIX_LIVEVIEW.md)
already shows one real Process Manager whose job is "publish this event
to the mesh." This page is the general-purpose companion: wiring a PM
for genuine cross-*domain* integration — Domain A's event triggers a
real command in Domain B — using the exact same mechanism.

**A note before the recipe**: this corpus's own
[`philosophy/PROCESS_MANAGERS.md`](../philosophy/PROCESS_MANAGERS.md)
illustrates a PM as a hand-rolled `gen_server` doing `pg:join(Scope,
Topic, self())` and receiving raw `{evoq_event, ...}` messages. That
example's own app names (`discover_divisions`, `design_division`) don't
correspond to any real repo in this workspace — it reads as an
illustrative doctrine example, not verified running code. **Every real
PM that actually exists in this workspace instead implements the plain
`evoq_event_handler` behaviour** — the same shape already documented in
the Phoenix LiveView FAQ — because `evoq_event_handler` subscribes by
event type globally, not by a per-domain `pg` scope. This page describes
what's actually shipped; treat the philosophy doc as design intent, not
as the wiring recipe to copy.

---

## The real contract

```erlang
%% evoq_event_handler behaviour
interested_in() -> [binary()].                          % required
init(Config) -> {ok, State} | {error, Reason}.           % required
handle_event(EventType, Event, Metadata, State) ->
    {ok, NewState} | {error, Reason}.                    % required
on_error(Error, Event, FailureContext, State) -> ...      % optional
```

Started via the library's own generic starter —
`evoq_event_handler:start_link(CallbackModule, Config)` — under your
supervision tree. No hand-rolled `pg:join`, no raw message receiving: the
generic `evoq_event_handler` gen_server does all subscription plumbing;
your callback module is a plain, focused piece of business logic.

## A real, cross-domain example (not just "publish to mesh")

`hecate-daemon` (since removed) had a genuine cross-app PM, kept here
as a worked illustration of the correct wiring shape:
`offering_terms_accepted_v1` was emitted by `guide_license_lifecycle`;
the PM reacting to it lived in **`guide_procurement_lifecycle`** — the
target domain, per this workspace's own naming convention — and
dispatched a real command into that domain's own aggregate, not a mesh
publish:

```erlang
-module(on_offering_terms_accepted_initiate_procurement).
-behaviour(evoq_event_handler).
-export([interested_in/0, init/1, handle_event/4]).

interested_in() -> [<<"offering_terms_accepted_v1">>].
init(_Config) -> {ok, #{}}.

handle_event(_EventType, Event, _Metadata, State) ->
    Data = maps:get(data, Event),
    LicenseId = hecate_api_utils:get_field(license_id, Data),
    ConsumerId = hecate_api_utils:get_field(consumer_id, Data),
    OfferingId = hecate_api_utils:get_field(offering_id, Data),
    PluginId = hecate_api_utils:get_field(plugin_id, Data),
    AuthorId = hecate_api_utils:get_field(author_id, Data),
    logger:info("[PM] Offering terms accepted for license ~s, initiating procurement", [LicenseId]),
    case initiate_procurement_v1:new(#{
        consumer_id => ConsumerId,
        offering_id => OfferingId,
        plugin_id => PluginId,
        author_id => AuthorId
    }) of
        {ok, Cmd} ->
            case maybe_initiate_procurement:dispatch(Cmd) of
                {ok, _, _} ->
                    logger:info("[PM] Procurement initiated for consumer ~s, plugin ~s", [ConsumerId, PluginId]);
                {error, Reason} ->
                    logger:error("[PM] Failed to initiate procurement for license ~s: ~p", [LicenseId, Reason])
            end;
        {error, Reason} ->
            logger:error("[PM] Failed to create procurement command for license ~s: ~p", [LicenseId, Reason])
    end,
    {ok, State}.
```

Wired into the target app's own supervisor — a one-line child spec, not
a bespoke supervisor of its own:

```erlang
init([]) ->
    Children = [
        emitter(procurement_initiated_v1_to_pg),
        emitter(procurement_archived_v1_to_pg),
        emitter(on_offering_terms_accepted_initiate_procurement)  %% cross-context process managers
    ],
    {ok, {SupFlags, Children}}.

emitter(Mod) ->
    #{id => Mod, start => {evoq_event_handler, start_link, [Mod, #{}]},
      restart => permanent, type => worker}.
```

This is a materially simpler wiring story than the philosophy doc's
hand-rolled supervisor-plus-`pg:join` illustration — the generic
`evoq_event_handler:start_link/2` is the entire supervision-tree
integration; your module is nothing but `interested_in/0` +
`handle_event/4`.

**Don't confuse this with `_to_pg` emitters** — `procurement_initiated_v1_to_pg`
in the same supervisor above is a *different* tool: same
`evoq_event_handler` shape, but its job is fanning an event out to local
same-node subscribers via `pg:get_members/2`, not dispatching a
cross-domain command. Both are real and both matter, but they answer
different questions ("who else in this BEAM node cares" vs. "what other
domain needs to react").

## Naming and location

`on_{source_event}_{action}_{target}`, living in the **target** domain
(the one whose command it dispatches) — this convention is genuinely
followed at scale: dozens of real `on_*` directories exist across
`hecate-tube`, `hecate-marketplace`, `hecate-victron`, and
`hecate-app-martha` alone. This isn't a one-off pattern from a single
example.

## See also

- [`philosophy/PROCESS_MANAGERS.md`](../philosophy/PROCESS_MANAGERS.md) — the conceptual doctrine; its worked example doesn't match real shipped code, see the note above
- [FAQ: Connecting Phoenix LiveView to the Mesh](FAQ_CONNECT_PHOENIX_LIVEVIEW.md) — the same `evoq_event_handler` shape used for "publish to the mesh" instead of cross-domain dispatch
- [FAQ: How do I add event sourcing to a new hecate service?](FAQ_ADD_EVENT_SOURCING.md) — the aggregate/command side a PM ultimately dispatches into
