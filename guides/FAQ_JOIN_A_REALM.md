---
title: "FAQ: Joining a Realm — Getting Your Service an Identity and Certificate"
layer: guide
audience: [agent, human]
stage: stable
---

# FAQ: How Do I Join a Realm and Get My Service an Identity/Certificate?

[Back to FAQ index](FAQ.md) · [Back to corpus index](../INDEX.md)

[FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md) covers running a
**station** — realm-agnostic routing infrastructure that never joins a
realm itself. This page is the other half: how a **service** actually
gets a realm-signed identity so it can be authorized to do something on
the mesh.

This is a genuinely two-layer situation and this page keeps the layers
separate rather than blending them: a real, working cert-issuance flow
you can use today, and a broader identity/authorization model that is an
active, dated design-in-progress — not yet built.

---

## 1. The real, working flow today: service-principal certs from `macula-portal`

**`macula-portal` owns PKI/cert issuance. `macula-realm` (a separate,
more recently extracted repo) owns mesh membership/gossip/topology
only** — its own README describes "realm lifecycle, realm-member
admission, station links," with no cert-issuance code in it. Don't
confuse the two despite the similar names; this page is entirely about
`macula-portal`.

Every hecate-service running on realm infrastructure carries its own
realm-signed credential — an institution of the realm, not a user's
identity (`hecate-corpus/philosophy/HECATE_TIER_MODEL.md`'s
citizens-vs-institutions framing). The real endpoint
(`macula-portal`'s `ServicePrincipalIssuanceController`):

```
POST /api/v1/services/provision
Authorization: Bearer mrt_…
Content-Type: application/json

{
  "public_key":   "base64-encoded-ed25519-public-key",
  "service_name": "hecate-rag",
  "node_name":    "beam00"
}
```

→ `201`:

```json
{
  "org_identity":  "mri:org:io.macula/rgfaber",
  "service_mri":   "mri:app:io.macula/rgfaber/_service-hecate-rag-beam00",
  "service_name":  "hecate-rag",
  "node_name":     "beam00",
  "cert_pem":      "-----BEGIN CERTIFICATE-----…",
  "org_ca_pem":    "-----BEGIN CERTIFICATE-----…",
  "ca_chain_pem":  "-----BEGIN CERTIFICATE-----…"
}
```

**The flow** (operator, or a gitops reconciler, does this at install
time — not automated by anything running on a schedule):

1. Generate a fresh Ed25519 keypair for the service.
2. POST the pubkey + `service_name` + `node_name` to the endpoint above,
   presenting a refresh token as bearer auth. `service_name` must match
   `^hecate-[a-z][a-z0-9-]{0,55}$`.
3. Write the response's `cert_pem`/private key to
   `/etc/hecate/secrets/<service-name>/service-cert.pem` (+
   `service-key.pem`) on the target node.
4. The Quadlet/compose unit mounts that directory read-only into the
   container at `/etc/hecate/secrets/service-cert.pem`, which
   `hecate_om_identity` (hecate-services/hecate-om) reads at boot.

`cert_pem` is the leaf; `org_ca_pem` is the intermediate org CA that
issued it; `ca_chain_pem` is the realm CA (the trust anchor) — a service
embeds `cert_pem ++ org_ca_pem` in its `procedure_advertisement` so a
direct-dial consumer can verify it to the realm CA offline. **The
response never includes the private key** — the service generates its
own keypair and keeps the private half; the realm only ever sees and
signs the public half.

This is explicitly labeled **v1** in `hecate_om_identity`'s own doc
comment: "long-lived realm-signed cert provisioned out-of-band by a
realm-admin script." A stated **v2** exists only on paper so far:
"short-lived UCAN auto-rotated from a realm HTTP endpoint" — the module
is written so that swap-in lands without touching consumers, but it
hasn't landed.

**Known v1 gap, from the controller's own doc comment:** no CRL beyond
cert TTL expiry (rotation happens by re-POST), and **no per-service
authorization policy yet — every refresh-token holder can provision any
service-principal.** Phase 2 is meant to add a `services` claim to the
refresh token plus an admin policy file; not built yet.

---

## 2. `identity_spec/0` is a declared manifest today, not (yet) enforcement

Every `hecate_om_service` implementation exports an `identity_spec/0`
callback (see
[FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md)) —
the scaffold's own template comment calls it "THE AUTHORITY THIS SERVICE
ASKS THE REALM FOR." It reads like a real authorization request. **It
currently is not one**, verified by reading `hecate_om:boot/2`
(hecate-services/hecate-om) end to end: it calls a service's
`capabilities/0` and `start/1`, registers with `hecate_om_capabilities`
— and never calls `identity_spec/0` anywhere in the runtime boot path.

It's still real and required — a compile-time-enforced `-callback`, and
the scaffold's own generated test suite shape-checks it (added
specifically after an earlier template version shipped an
`identity_spec` claiming authority over resources the generated service
could not actually touch, with nothing checking it). But today it's a
**self-declared intent manifest**, not wired to any enforcement — don't
write one assuming a realm somewhere is reading and gating on it yet.

---

## 3. The bigger picture is an active, dated design — not shipped

`macula-io/macula-architecture/plans/PLAN_CITIZEN_IDENTITY_AUTHN_AUTHZ.md`
(Status: Draft v1.0, created **2026-09-01**) opens by naming the actual
state of things plainly: "Three pieces of this were already designed,
separately, and never reconciled" — a Hanko-based identity layer cake
that scoped authorization out as future work, a provisional-realm trust
ladder for zero-human bootstrap, and the UCAN capability-token guide
above (real and implemented, but opt-in per procedure, never used as a
general policy). A fourth piece — `macula-realm`'s original
membership model, keyed on a device's mesh pubkey with no representation
of the human behind it — was designed, then explicitly paused
2026-08-28 as wrong on its own terms.

**Treat this plan document as exactly that: an active reconciliation
effort, not settled architecture.** The service-principal cert flow in
§1 is real and usable right now; the citizen/UCAN/realm-membership model
this plan is reconciling is not built yet. If you're integrating
identity/authorization into a new service, use §1's flow for what a
service concretely needs today, and check the plan doc directly for
anything beyond that rather than trusting a description of it here that
would go stale the moment the plan moves.

---

## See also

- [FAQ: How do I join the Mesh?](FAQ_JOIN_THE_MESH.md) — the realm-agnostic station layer this page's realm layer sits above
- [FAQ: How do I authorize a procedure or topic with UCAN?](FAQ_AUTHORIZE_WITH_UCAN.md) — the one authorization primitive that IS enforced by the SDK today
- [FAQ: Developing Edge Services in BEAM Languages](FAQ_DEVELOP_EDGE_SERVICES_BEAM.md) — `identity_spec/0` and the `hecate_om_service` behaviour
- [FAQ: How do I deploy my own hecate service?](FAQ_DEPLOY_HECATE_SERVICES.md) — where the resulting cert actually gets mounted into a running container
