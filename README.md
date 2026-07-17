# soma 杣

**Forestry / logging robotics — directional felling + bucking + extraction.**
Tier-B actor · ADR-2606142010 · 🟡 R0 (design + sim) · Clojure-first.

soma ("杣" = the marked/worked forest, and the woodsman who works it) is the logging body
the robotics remote-work survey (ADR-2606073001 §4) reserved — **伐採**, repeatedly named one
of the deadliest jobs. It is **selective + regenerative only**: it directionally fells marked
trees away from people, bucks the stems to maximise value, and extracts the logs without
rutting the soil. A protected/old-growth tree refuses felling; an unsafe fall line refuses too.

It is a sibling of **kuramori 倉守** (ADR-2606142000) in the **Clojure-first GAP-actor wave** —
methods authored directly in babashka-runnable Clojure (pure, no deps → also kotoba-pywasm-portable).

## Run

```bash
bb --classpath 20-actors 20-actors/soma/methods/test_soma.clj   # 16 tests / 67 assertions
bb --classpath 20-actors -m soma.methods.analyze                # → forestry-stand R0 report
bb --classpath 20-actors -m soma.methods.datom-emit             # → kotoba EAVT Datom log
```

## What it does

| Method | Role |
|---|---|
| `fell_plan.clj`  | directional fell mechanics — predict fall azimuth (notch aim biased by lean, perturbed by wind), hinge holding-wood width, 1.5×-height fall-zone sector; **`safe-fell?` / `plan-fell` RAISE** on an exclusion in the fall zone (G5) or a protected/no-cut tree (G7) |
| `harvester.clj`  | cut-to-length bucking value DP (sawlog>pulp, unbounded rod-cutting) + grapple/boom reach feasibility (G8) |
| `extraction.clj` | forwarder/skidder route — slope gate + ground-impact (soil bearing) gate; **`plan-route` RAISES** on over-grade or over-pressure/protected soil (G2 regenerative-only) |
| `analyze.clj`    | end-to-end: load seed → per-tree fell (aim into a clear lane, refuse protected/unsafe) → buck → extraction → report |
| `datom_emit.clj` | kotoba EAVT projection (`:soma.*` GROUND + `:felled`/`:refused-protected` 縁 + `:bond/*` DERIVED transient) |

## Gates

R0 design+sim only (G1, no-server-key) · selective + regenerative only / no clear-cut /
slope+soil limits (G2) · no worker surveillance (G3) · Displacement-Dividend-coupled (G4) ·
**exclusion-zone fell safety — raises (G5)** · Murakumo-only (G6) · **protected-species /
no-cut refusal — raises (G7)** · tazuna-teleoperable (G8). See `CLAUDE.md` for full text.

Apache 2.0 + etzhayyim Charter Compliance Rider v3.1.
