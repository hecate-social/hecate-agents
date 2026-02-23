# Appstore Plugin License Lifecycle

_Reference architecture for the hecate-app-appstore plugin._

---

## Overview

The appstore manages plugin licenses through a full seller/buyer lifecycle.
The catalog is populated by events — NOT hardcoded seed data.

**hecate-app-appstore replaces hecate-marketplace.** The `hecate-app-*` convention is the standard for all plugins.

---

## Aggregate: plugin_license_aggregate

### Full Event Lifecycle

```
SELLER SIDE:
  initiate_plugin_license  → plugin_license_initiated_v1   (birth event)
  announce_plugin_license  → plugin_license_announced_v1   (pre-publish, marketing)
  publish_plugin_license   → plugin_license_published_v1   (available for purchase)

BUYER SIDE:
  buy_license              → license_bought_v1
  install_plugin           → plugin_installed_v1
  upgrade_plugin           → plugin_upgraded_v1
  remove_plugin            → plugin_removed_v1
  revoke_license           → license_revoked_v1
  archive_license          → license_archived_v1
```

### Seller Desks (CMD)

| Desk | Command | Event | Entry Point |
|------|---------|-------|-------------|
| `initiate_plugin_license/` | `initiate_plugin_license_v1` | `plugin_license_initiated_v1` | API (form: github repo, name, description, icon, selling formula) |
| `announce_plugin_license/` | `announce_plugin_license_v1` | `plugin_license_announced_v1` | API (pre-publishing, marketing) |
| `publish_plugin_license/` | `publish_plugin_license_v1` | `plugin_license_published_v1` | API + Mesh Emitter |

### Buyer Desks (CMD)

| Desk | Command | Event | Entry Point |
|------|---------|-------|-------------|
| `buy_license/` | `buy_license_v1` | `license_bought_v1` | API |
| `install_plugin/` | `install_plugin_v1` | `plugin_installed_v1` | Policy (on_license_bought_v1) |
| `upgrade_plugin/` | `upgrade_plugin_v1` | `plugin_upgraded_v1` | API |
| `remove_plugin/` | `remove_plugin_v1` | `plugin_removed_v1` | Policy (on_license_revoked_v1) |
| `revoke_license/` | `revoke_license_v1` | `license_revoked_v1` | API |
| `archive_license/` | `archive_license_v1` | `license_archived_v1` | API |

---

## Catalog Population — Two Sources

The catalog read model is populated from two entry points:

### 1. Local Seller Publishes

```
Seller fills form → initiate_plugin_license → ... → publish_plugin_license
  → plugin_license_published_v1 (event in local store)
  → Projection writes to catalog SQLite
  → Mesh Emitter publishes FACT to mesh
```

### 2. Remote Seller Publishes (via Mesh)

```
FACT arrives from mesh (plugin_license_published)
  → Listener: on_plugin_license_published_maybe_register_catalog_entry
  → Command: register_catalog_entry_v1
  → Event: catalog_entry_registered_v1
  → Projection writes to catalog SQLite
```

Both paths produce events in the local store, projected to the same catalog read model.
The catalog does not distinguish local vs remote — all published plugins appear the same.

### Mesh Integration Flow

```
Node A (seller):                          Node B (buyer):
  publish_plugin_license                    ← FACT from mesh
  → plugin_license_published_v1             → on_plugin_license_published_
  → plugin_license_published_v1_to_mesh       maybe_register_catalog_entry
  → FACT on mesh ─────────────────────────→ → register_catalog_entry_v1
                                            → catalog_entry_registered_v1
                                            → projection → catalog SQLite
```

---

## App Structure

```
hecate-app-appstored/
├── apps/
│   ├── guide_plugin_lifecycle/         # CMD — seller + buyer desks
│   │   └── src/
│   │       ├── plugin_license_aggregate.erl
│   │       ├── initiate_plugin_license/    # Seller
│   │       ├── announce_plugin_license/    # Seller
│   │       ├── publish_plugin_license/     # Seller + Mesh Emitter
│   │       ├── buy_license/                # Buyer
│   │       ├── install_plugin/             # Buyer + Policy
│   │       ├── upgrade_plugin/             # Buyer
│   │       ├── remove_plugin/              # Buyer + Policy
│   │       ├── revoke_license/             # Buyer
│   │       └── archive_license/            # Buyer
│   │
│   ├── project_appstore/               # PRJ — projections + SQLite store
│   │   └── src/
│   │       ├── project_appstore_store.erl
│   │       ├── plugin_license_published_v1_to_catalog/
│   │       ├── catalog_entry_registered_v1_to_catalog/
│   │       ├── license_bought_v1_to_licenses/
│   │       ├── plugin_installed_v1_to_licenses/
│   │       └── ...
│   │
│   └── query_appstore/                 # QRY — API handlers, reads only
│       └── src/
│           ├── browse_catalog/
│           ├── get_plugin_details/
│           ├── list_installed/
│           └── list_licenses/
```

---

_The appstore is a full marketplace — sellers publish, the mesh distributes, buyers purchase and install._
