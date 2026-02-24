# ANTIPATTERNS.md — Demons We've Exorcised

*Mistakes we've made and corrected. Read this. Don't repeat them.*

---

## Demon Index

| # | Name | One-Line Summary | Date |
|---|------|-----------------|------|
| 1 | Technical Names Don't Scream | Slice names describe HOW not WHAT | 2026-02-04 |
| 2 | Parallel Domain Infrastructure | Duplicate infrastructure across domains | 2026-02-04 |
| 3 | Incomplete Desks / Flat Workers | Missing desk supervisors and components | 2026-02-04 |
| 4 | Missing or Wrong "Birth" Event | Must use `{agg}_initiated_v1` | 2026-02-08 |
| 5 | Auto-Creating Child Aggregates | Parent IDENTIFIES, child INITIATES | 2026-02-08 |
| 6 | Listeners as Separate Desks | Listener for desk X belongs IN desk X | 2026-02-08 |
| 7 | Using Mesh for Internal Integration | Use pg internally, mesh for WAN only | 2026-02-08 |
| 8 | Centralized Listener Supervisors | No central supervisor for all listeners | 2026-02-08 |
| 9 | Direct Creation Endpoints for Child Aggregates | Children created through parents only | 2026-02-09 |
| 10 | Wrong Aggregate Callback Argument Order | State first, Payload second | 2026-02-09 |
| 11 | Side Effects Based on Hope Acknowledgment | React to facts, not acknowledgments | 2026-02-09 |
| 12 | Read-Time Status Enrichment | Compute at projection time, not query time | 2026-02-10 |
| 13 | Ambiguous Query Module Names | Vague query names hide scaling dangers | 2026-02-10 |
| 14 | God Module API Handlers | All endpoints in one file | 2026-02-10 |
| 15 | Consumer-Generated Command IDs | Framework owns command_id generation | 2026-02-11 |
| 16 | Parameterized Phase Lifecycle | Generic `start_phase/pause_phase` hides intent | 2026-02-12 |
| 17 | CRUD Verbs in Event-Sourced Commands | "update" is the U in CRUD | 2026-02-12 |
| 18 | Process Managers as Separate Slices | PMs are policies inside the target desk | 2026-02-12 |
| 19 | esqlite3 Returns Lists, Not Tuples | `row_to_map({A,B})` silently fails — use `[A,B]` | 2026-02-12 |
| 20 | Eager Default in `maps:get/3` | `maps:get(k, M, f())` evaluates `f()` even when key exists | 2026-02-12 |
| 21 | esqlite3 Argument Order (Db First) | `esqlite3:exec(Db, SQL)` not `exec(SQL, Db)` | 2026-02-12 |
| 22 | Manual Event Emission from API Handlers | Emitters subscribe via evoq, not called from handlers | 2026-02-13 |
| 23 | Raw #event{} Records in Projections | Projections receive records, call maps:find on tuples | 2026-02-13 |
| 24 | Silent Subscription Pipeline Failures | POST succeeds, GET returns [] — three bugs, zero errors | 2026-02-13 |
| 25 | Centralized Route Registration Files | One file per app listing all routes — horizontal grouping | 2026-02-16 |
| 26 | PG Emitters "Dead Code" Without Subscribers | Pub/sub publishers are infrastructure — no subscribers ≠ dead code | 2026-02-23 |
| 27 | Hardcoded User/Submitter IDs | Commands must carry the real actor identity, not placeholders | 2026-02-23 |
| 28 | No Tests on Event-Sourced Domains | Aggregates, projections, and policies need tests — dialyzer alone isn't enough | 2026-02-23 |
| 29 | Missing `/ui/[...]` Cowboy Route in Plugin Daemon | Plugin has manifest + socket but no static file route — hecate-web silently drops it | 2026-02-24 |

---

## Debugging Rule

**In case of a bug: write a test BEFORE going wild in unguided reasoning.**

1. Reproduce the bug in a test (make it fail)
2. Fix the code (make the test pass)
3. THEN explore if more instances exist

Unguided runtime debugging (checking processes, logs, curl) without a failing test is junior behavior. Tests are deterministic, reproducible, and document the bug for posterity.

---

## Detail Files by Topic

### [ANTIPATTERNS_NAMING.md](ANTIPATTERNS_NAMING.md) — Names That Don't Scream

Demons #1, #13, #16, #17. Naming violations where module, event, or command names fail to communicate business intent.

### [ANTIPATTERNS_STRUCTURE.md](ANTIPATTERNS_STRUCTURE.md) — Code Organization Violations

Demons #3, #6, #8, #14, #18, #25. Structural mistakes where code is organized by technical concern instead of business capability.

### [ANTIPATTERNS_DOMAIN.md](ANTIPATTERNS_DOMAIN.md) — Domain Modeling Mistakes

Demons #2, #4, #5, #9. Errors in modeling aggregate lifecycles, parent-child relationships, and domain boundaries.

### [ANTIPATTERNS_RUNTIME.md](ANTIPATTERNS_RUNTIME.md) — Event Sourcing & Integration Runtime

Demons #7, #10, #11, #12, #15, #19, #20, #21, #22, #23, #24, #26, #27, #28, #29. Runtime mistakes in event handling, callback signatures, integration channels, projection timing, subscription flow, pub/sub infrastructure, identity propagation, test coverage, plugin discovery, and Erlang/esqlite3 gotchas.

---

*We burned these demons so you don't have to. Keep the fire going.*
