---
title: "ANTIPATTERNS: Verification — What We Wrote Down vs What We Built"
layer: skill
audience: [agent, human]
stage: stable
---

# ANTIPATTERNS: Verification — What We Wrote Down vs What We Built

*Demons about the gap between a repository's prose and its behaviour. Every other
file here catalogues a technical mistake. This one catalogues the mistake of
believing the technical mistakes were caught.*

[Back to Index](INDEX.md)

---

## 🔥🔥🔥 Demon 53: Comments That State Intent as Fact

**Date exorcised:** 2026-08-07
**Where it appeared:** `hecate-dronex` (four files in one day), `beam-campus-net/CLAUDE.md`
**Cost:** Every island in an archipelago restarted its population from seed on
every deploy, for weeks, while a module header explained in detail how it did not.

### The Lie

"The comment above this function describes what the function does."

### What Happened

Four in a single day, all in code written by the same author, all landing in the
same commit as the code they misdescribe:

| Prose | Code |
|---|---|
| "restore reads BACKWARD to the most recent snapshot and then forward from there" | `stream_forward(StoreId, Stream, 0, 5000)` — forward from the beginning, capped |
| "counters live on the island so they survive a deploy, because the roster snapshot that survives a restart IS the island" | the snapshot stored `entries` and `capacity`; every counter reset on every restart |
| "`max_ticks` is 1200 at 20 Hz" beside a call timeout justified as "longer than an engagement can take" | the fight had moved off the call months earlier |
| "`mix ecto.create` needs Postgres" | `adapter: Ecto.Adapters.SQLite3` |

None of these were lies about somebody else's code. In each case the author wrote
down the design, then wrote something else, and the design is what got preserved
**in the present tense**.

### Why It Survives

The house prose style is emphatic: capitals, ⚠ markers, a paragraph of hard-won
reasoning. That register is correct for **why** and actively harmful for **what**,
because a sentence that reads as settled does not get re-checked. In all four
cases the comment is the reason nobody looked. A hedged sentence would have
survived less well and cost less.

### The Rule

**A comment that describes BEHAVIOUR must have a test named after it.**

If the header says "reads backward to the newest snapshot", there is a test called
`reads_backward_to_the_newest_snapshot`. If that test cannot be written, the
sentence is a PLAN and must be marked as one:

```erlang
%% INTENDED: restore should read backward to the newest snapshot.
%% ACTUAL: reads forward from 0, capped at 5000. See TODO-nnn.
```

Reasons are durable and belong in comments. Descriptions are verifiable and belong
in tests. When in doubt, write less prose: **a shorter comment cannot be as wrong.**

---

## 🔥 Demon 54: A Test for the Name and None for the Fold

**Date exorcised:** 2026-08-07
**Where it appeared:** `hecate-dronex/apps/hecate_dronex/src/breed_a_roster/roster_log.erl`
**Cost:** the same as Demon 53 — every lineage, every deploy, for weeks

### The Lie

"This module is tested."

### What Happened

`roster_log` had exactly one test: that its stream id was a shape reckon-db would
accept. That was the part the author had recently argued with themselves about,
because a bad stream id had once wedged an island for four minutes.

The **fold** — the function that turns stored events back into a roster, the whole
reason the module exists — had no test at all. It raised `badmap` on the first
event of every restore it ever attempted.

### The Signature

**The untested part is the part you were confident about.** Attention went to the
recently painful thing; the rest was written on autopilot and shipped unexamined.
Look for a module whose tests cluster around one concern and are silent about its
main verb.

A second signature, from the same day: every fault sat at a **seam**. Record
versus map, ETS versus disk, colocated hook versus bundler path, a CSS margin
versus text content, a hex string versus a lightness band. Unit tests inside each
module were fine. Nothing crossed a join.

### The Rule

**Every seam gets one test built from the real shape, produced by the real
library.** Never a hand-written map standing in for a record:

```erlang
%% WRONG — tests the invention, and the invention is the bug
Ev = #{event_type => <<"roster_snapshotted">>, data => #{entries => []}},

%% RIGHT — the library's own header, so a shape change fails the test
-include_lib("reckon_gater/include/reckon_gater_types.hrl").
Ev = #event{event_type = <<"roster_snapshotted">>, data = #{entries => []}},
```

A test that invents a convenient shape tests the invention. That is precisely the
mistake the code made, so the test passes cheerfully beside the bug.

---

## 🔥🔥🔥 Demon 55: Believing That Writing It Down Prevents It

**Date exorcised:** 2026-08-07
**Where it appeared:** this file's own index
**Cost:** Demon 23, repeated in full, five months and three weeks later

### The Lie

"It's in the antipatterns index, so we won't do it again."

### What Happened

**Demon 23, exorcised 2026-02-13:** *Raw `#event{}` Records Passed to Projections.
ReckonDB emitters send records; the consumer called map functions on a tuple; read
models were permanently empty and nothing errored.*

**2026-08-07, `hecate-dronex`:** `roster_log` read events from the same library,
called `maps:find/2` on the same record, restored nothing, and reported nothing.
A different repository, a different author-session, an identical bug.

Between those dates the demon sat in `INDEX.md`, correctly described, numbered and
dated. It changed nothing, because **the index is prose too**, and this whole file
is about prose not biting.

### The Rule

A demon is only exorcised when something MECHANICAL will refuse it. Ranked by how
much they actually bite:

1. **A type or a compile error.** Dialyzer caught three unreachable clauses in the
   same session that four paragraphs of comment had not.
2. **A test that fails without the fix.** Verify it RED before believing the green.
   Ten of eleven new tests went red against the reverted reader, with the
   production `badmap` in the output. That is what makes the green mean something.
3. **A lint rule.** Elvis, a custom check, a CI grep.
4. **A line in a document.** Nearly worthless on its own, and worth writing anyway
   for the reasoning it carries. Never mistake it for a guard.

**When adding a demon here, name the mechanism that will refuse it.** A demon with
no mechanism is a demon with a scheduled return date.

---

## 🔥 Demon 56: Correct Behaviour With No Reporting

**Date exorcised:** 2026-08-07
**Where it appeared:** `island_server:kept/2`; `Dronex` board (ETS with no read model)
**Cost:** weeks of silent lineage loss, and a fleet of exhibits resetting unnoticed

### The Lie

"It degrades gracefully."

### What Happened

```erlang
kept(Island, {ok, R})     -> island:with_roster(Island, R);
kept(Island, {error, _Why}) -> Island.       %% correct, and silent
```

An island that cannot read its log **must** still start. That behaviour is right.
The reporting was absent, so the failure had no voice for weeks.

What made it invisible was not the missing log alone. It was that **the only
published evidence agreed with both outcomes**: roster depth. A restored lineage
and a fresh island filling up from seed both show a number that climbs, and there
is no depth at which one looks wrong.

The same shape appeared the same day on the site: the /dronex board held every
raid in ETS and nowhere else, so each deploy emptied it, and a board filling up
again looks exactly like a board that was never empty.

### The Rule

Two questions, both required, whenever a fallback is written:

1. **Does it say so?** Every `rescue`, catch-all clause and error branch that
   returns a default logs once, at a level someone reads. See also Demons 24, 42
   and 48 — this family has now cost four separate outages.
2. **Could an observer tell?** If the healthy state and the failed state produce
   the same published numbers, add a field that distinguishes them. "Restored from
   the log" and "seeded fresh" must not render identically.

---

## The Session This File Came From

2026-08-07, one working day, one author. Findings: an archipelago whose attack
graph was decided by `hd/1` over a sorted flatmap; a lineage that had never once
been restored; counters that reset on every deploy under a comment promising they
would not; an unjustified physics constant deciding every long fight; a public
exhibit holding its entire dataset in memory; four documents describing designs
the code did not implement; and a table column that asserted a ceiling the caption
beside it denied.

**Every one was found by measuring. Every one had been hidden by prose.**

The good moments were all instruments: a probe against a live node, a query against
the running board, a palette validator, a dry-run of the updater, an anonymous
registry pull. The bad ones were all assertions from memory, written confidently.

That is the whole of this file. Measure the thing. Then write down only what the
measurement said.
