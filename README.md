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
bb run_tests.clj                                                # 40 tests / 150 assertions
bb --classpath . -m soma.methods.analyze                        # → forestry-stand R0 report
bb --classpath . -m soma.methods.datom-emit                     # → kotoba EAVT Datom log
bb bin/verify_authorities.clj                                   # citations vs. the live e-Gov API
```

`verify_authorities.clj` exits **0** verified · **1** a citation does not match its source ·
**2** the API was unreachable so nothing was checked. It needs network and is deliberately not
part of `bb run_tests.clj`, which stays green offline.

## What it does

| Method | Role |
|---|---|
| `fell_plan.clj`  | directional fell mechanics — predict fall azimuth (notch aim biased by lean, perturbed by wind), hinge holding-wood width, **statutory 2×-height keep-out circle** (労働安全衛生規則 第481条第2項) + 1.5×-height directional fall sector; **`safe-fell?` / `plan-fell` RAISE** on a person inside the circle (G5), an exclusion in the fall sector (G5), or a protected/no-cut tree (G7) |
| `harvester.clj`  | cut-to-length bucking value DP (sawlog>pulp, unbounded rod-cutting) + grapple/boom reach feasibility (G8) |
| `extraction.clj` | forwarder/skidder route — slope gate + ground-impact (soil bearing) gate; **`plan-route` RAISES** on over-grade or over-pressure/protected soil (G2 regenerative-only) |
| `analyze.clj`    | end-to-end: load seed → per-tree fell (aim into a clear lane, refuse protected/unsafe) → buck → extraction → report |
| `datom_emit.clj` | kotoba EAVT projection (`:soma.*` GROUND + `:felled`/`:refused-protected` 縁 + `:bond/*` DERIVED transient) |

## Gates

R0 design+sim only (G1, no-server-key) · selective + regenerative only / no clear-cut /
slope+soil limits (G2) · no worker surveillance (G3) · Displacement-Dividend-coupled (G4) ·
**exclusion-zone fell safety — raises (G5)** · Murakumo-only (G6) · **protected-species /
no-cut refusal — raises (G7)** · tazuna-teleoperable (G8). See `CLAUDE.md` for full text.

## Where the numbers come from

`data/authorities.edn` carries the primary source behind each safety threshold — the quoted
provision, the e-Gov Laws API URL it was fetched from, and a conformance verdict. Four of the
seven cited obligations are marked `:cited-not-implemented`: soma does **not** yet model the
notch depth (第477条第1項第三号), the bad-weather stop (第483条), the pre-selected retreat
position (第477条第1項第一号), or the signal-and-confirm-evacuation sequence (第479条第2項).
That file is the honest inventory, not a compliance claim.

Verify a citation against `https://laws.e-gov.go.jp/api/1/lawdata/<id>` — **not** against
`https://laws.e-gov.go.jp/law/<id>`, which returns 200 for law IDs that do not exist.

Apache 2.0 + etzhayyim Charter Compliance Rider v3.1.
