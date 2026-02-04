# HECATE_DoO — Deployment & Operations

*Ship it and keep it running.*

**Phase 4 of [HECATE_ALC](HECATE_ALC.md)**

---

## Purpose

Get the software into production and keep it healthy:
- Deploy releases safely
- Monitor system health
- Respond to incidents
- Collect feedback for the next cycle

**DoO is where software meets reality.**

---

## Mindset

```
"I deploy small and often.
 I observe before assuming.
 I respond quickly to issues.
 I learn from production."
```

---

## Activities

### 1. Release Preparation

**Before deploying:**

- [ ] Version number assigned (semver)
- [ ] CHANGELOG updated
- [ ] Release notes written
- [ ] Breaking changes documented
- [ ] Rollback plan ready

**Release checklist:**

```markdown
## Release v{X.Y.Z}

### Changes
- feat: ...
- fix: ...

### Breaking Changes
- None / List them

### Migration Steps
- None / List them

### Rollback Plan
- Revert to v{previous}
- Data migration rollback: ...
```

---

### 2. Deployment

**GitOps flow:**

```
1. Merge to main
2. CI builds and tags image
3. Update image tag in GitOps repo
4. ArgoCD syncs to cluster
5. Kubernetes rolls out new pods
6. Health checks pass
7. ✓ Deployed
```

**Deployment strategies:**

| Strategy | Use When | Risk |
|----------|----------|------|
| **Rolling** | Standard deploys | Low |
| **Blue/Green** | Zero-downtime critical | Medium |
| **Canary** | High-risk changes | Low (gradual) |
| **Recreate** | Breaking changes | High (downtime) |

---

### 3. Smoke Testing

**Immediately after deployment:**

```bash
# Health check
curl https://prod.example.com/health
# → {"status": "ok", "version": "1.2.3"}

# Basic functionality
curl https://prod.example.com/api/{resource}
# → 200 OK

# Critical path test
# → Whatever is most important works
```

**Automated smoke tests should run on every deployment.**

---

### 4. Monitoring Setup

**The four golden signals:**

| Signal | What to Monitor | Alert When |
|--------|-----------------|------------|
| **Latency** | Response time p50, p95, p99 | > threshold |
| **Traffic** | Requests per second | Unusual spike/drop |
| **Errors** | Error rate, error types | > threshold |
| **Saturation** | CPU, memory, connections | > 80% |

**Dashboards:**

- System health overview
- Request/response metrics
- Error breakdown
- Resource utilization

**Alerts:**

- Error rate spike
- Latency degradation
- Resource exhaustion
- Health check failures

---

### 5. Logging

**Structured logging:**

```erlang
?LOG_INFO(#{
    event => capability_announced,
    capability_mri => MRI,
    agent_id => AgentId,
    duration_ms => Duration
})
```

**Log levels:**

| Level | Use For |
|-------|---------|
| `debug` | Detailed troubleshooting |
| `info` | Normal operations |
| `warning` | Unexpected but handled |
| `error` | Failures requiring attention |

**Correlation IDs:** Trace requests across services.

---

### 6. Incident Response

**When something goes wrong:**

```
1. DETECT   — Alert fires or user reports
2. TRIAGE   — Assess severity and impact
3. MITIGATE — Stop the bleeding (rollback, scale, disable)
4. DIAGNOSE — Find root cause
5. RESOLVE  — Fix the issue
6. REVIEW   — Post-incident analysis
```

**Severity levels:**

| Severity | Impact | Response |
|----------|--------|----------|
| **SEV1** | System down | All hands, immediate |
| **SEV2** | Major feature broken | Team, < 1 hour |
| **SEV3** | Minor issue | Next business day |
| **SEV4** | Cosmetic/low impact | Backlog |

---

### 7. Feedback Collection

**Sources of feedback:**

| Source | What You Learn |
|--------|----------------|
| Metrics | System behavior, performance |
| Logs | Errors, unusual patterns |
| User reports | Pain points, bugs |
| Support tickets | Common issues |
| Usage analytics | Feature adoption |

**Questions to answer:**

- Is it performing as expected?
- Are users successful?
- What errors are occurring?
- What's confusing or broken?

---

### 8. Iteration Planning

**Feed back into AnD:**

```
Production feedback
    ↓
Prioritize issues/improvements
    ↓
Add to backlog
    ↓
Next AnD cycle
```

**Categories:**

- **Bugs:** Things that don't work → Fix in current cycle
- **Improvements:** Things that could be better → Next cycle
- **Features:** New capabilities needed → Backlog
- **Tech debt:** Internal quality → Scheduled maintenance

---

## Outputs

### Required

- [ ] **Deployed Release** — Running in production
- [ ] **Monitoring** — Dashboards and alerts configured
- [ ] **Runbook** — How to operate the system

### Recommended

- [ ] **Incident Reports** — Post-mortems for issues
- [ ] **Feedback Log** — Collected feedback organized
- [ ] **Performance Baseline** — Normal metrics documented

---

## Runbook Template

```markdown
# Runbook: {System Name}

## Overview
{What this system does}

## Architecture
{Key components, dependencies}

## Common Operations

### Restart the service
```bash
kubectl rollout restart deployment/{name}
```

### Check logs
```bash
kubectl logs -f deployment/{name}
```

### Scale up/down
```bash
kubectl scale deployment/{name} --replicas=N
```

## Troubleshooting

### High latency
1. Check resource utilization
2. Check dependent services
3. Check recent deployments

### Error rate spike
1. Check logs for error patterns
2. Check recent deployments
3. Consider rollback

## Rollback Procedure
1. ...
2. ...

## Contacts
- Team: ...
- Escalation: ...
```

---

## Checklists

### Before Deployment

- [ ] CI/CD green
- [ ] STAGING verified
- [ ] Release notes ready
- [ ] Rollback plan documented
- [ ] Team notified

### After Deployment

- [ ] Health checks passing
- [ ] Smoke tests passing
- [ ] Monitoring shows healthy
- [ ] No error spikes
- [ ] Users unaffected or notified

### Ongoing Operations

- [ ] Alerts configured
- [ ] Dashboards reviewed regularly
- [ ] Incidents documented
- [ ] Feedback collected
- [ ] Backlog updated

---

## Anti-Patterns

| Anti-Pattern | Problem | Instead |
|--------------|---------|---------|
| **Deploy and forget** | Issues go unnoticed | Monitor actively |
| **No rollback plan** | Stuck when things break | Always have a way back |
| **Alert fatigue** | Real issues ignored | Tune alerts, reduce noise |
| **Manual deployments** | Inconsistent, error-prone | GitOps always |
| **Blame culture** | People hide mistakes | Blameless post-mortems |
| **Ignoring feedback** | Same issues recur | Feed back into planning |

---

## Transition to AnD (Next Cycle)

After DoO stabilizes:

1. Production is stable and monitored
2. Feedback collected and organized
3. Issues triaged and prioritized
4. Next iteration scope identified
5. Return to [HECATE_AnD](HECATE_AnD.md)

---

*Ship it. Watch it. Learn from it. Improve it.* 🗝️
