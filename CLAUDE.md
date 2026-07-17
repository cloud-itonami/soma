# soma 杣 — forestry / logging robotics (felling · bucking · extraction)

**ADR**: 2606142010 · **depends**: 2606073001 (robotics remote-work survey — §4 reserves
`soma 杣` for 伐採, "one of the deadliest jobs") · 2606032100 (labor-liberation OSS-robotics
wave — sanae/hataori/kiyome pattern) · 2606032130 (Displacement Dividend) · 2606042100
(tazuna — teleop substrate) · 2605215000 (Murakumo-only) · 2605312345 (Datom = canonical
state). **related**: 2606142000 (kuramori — the Clojure-first reference actor this mirrors).
**Status**: 🟡 R0 design + sim only.

soma ("杣" = the marked/worked forest + the woodsman) is the **logging body** the robotics
remote-work survey reserved. Logging (伐採) is repeatedly the deadliest civilian occupation;
soma's whole reason to exist is to take the human out of the fall zone. It is **selective +
regenerative only** — it never clear-cuts and never fells protected/old-growth trees.

**Clojure-first.** soma is a sibling of kuramori 倉守 in the Clojure-first GAP-actor wave —
methods authored directly in babashka-runnable Clojure, pure (no deps) so they run under both
`bb` and the kotoba pywasm runtime.

## Hard gates (constitutional — read before any change)

- **G1 — design + sim only.** R0 is pure planning compute; it moves no real machine. Real
  actuation is Council Lv6+/operator-gated R1 (no-server-key). The methods never touch a
  network or a device.
- **G2 — selective + regenerative only.** Clear-cut and old-growth/protected-species felling
  are unrepresentable. `extraction/plan-route` **RAISES** on a segment over the machine's max
  grade, or on over-ground-pressure / protected soil — no rutting or compaction beyond the
  soil's bearing limit.
- **G3 — no worker surveillance.** KPI is m³/hour + value/volume (*equipment* metrics), never
  a per-worker pace ranking or biometric.
- **G4 — dividend-coupled.** Forestry-labour displacement is coupled to a funded
  Displacement-Dividend cohort (ADR-2606032130 G2). No live displacement without it.
- **G5 — exclusion-zone fell safety (the headline gate).** `fell_plan/safe-fell?` is false and
  `plan-fell` **RAISES** when the predicted fall zone (a ≈1.5× tree-height sector around the
  fall line) overlaps ANY exclusion point — human, road, or watercourse. Fall fatality is the
  #1 logging hazard; an unsafe fell must surface, never be silently planned.
- **G6 — Murakumo-only** narration/inference (ADR-2605215000).
- **G7 — protected-species / no-cut refusal.** A protected species or a no-cut flag
  (old-growth / seed-tree) on a tree makes `fell_plan/plan-fell` **RAISE** — felling is
  refused, never silently forced.
- **G8 — tazuna-operated.** Remote operation/teleop is via tazuna 手綱 (ADR-2606042100);
  weaponizable use is unrepresentable.

## Layout

```
20-actors/soma/
├── CLAUDE.md                       # this file
├── manifest.edn                    # actor manifest (5 cells, 8 gates, Clojure methods)
├── data/
│   └── stand.edn                   # reference selective-harvest stand seed (:representative)
├── methods/                        # pure Clojure → bb-runnable AND kotoba-pywasm-portable
│   ├── fell_plan.clj               # directional felling: fall prediction + hinge + fall-zone safety (G5/G7 raise)
│   ├── harvester.clj               # cut-to-length bucking value DP + grapple reach (G8)
│   ├── extraction.clj              # forwarder slope + ground-impact gates (G2 raise)
│   ├── analyze.clj                 # end-to-end R0 orchestrator
│   ├── datom_emit.clj              # kotoba EAVT Datom-log emitter (canonical state)
│   └── test_soma.clj               # 16 tests / 67 assertions (clojure.test)
└── lex/
    └── fellAttestation.edn         # per-tree felling/bucking/extraction attestation lexicon
```

## Run

```bash
# from repo root (classpath = 20-actors, ns = soma.methods.*)
bb --classpath 20-actors 20-actors/soma/methods/test_soma.clj   # 16 green
bb --classpath 20-actors -m soma.methods.analyze                # → report
bb --classpath 20-actors -m soma.methods.datom-emit             # → EAVT Datom log
```

## Why the raises matter

The two `RAISE` gates are not error-handling — they are the actor's reason to exist. Logging
kills because trees fall the wrong way and because machines slide on steep wet ground.
`fell_plan/plan-fell` refusing an unsafe fall line (G5) and a protected tree (G7), and
`extraction/plan-route` refusing an over-grade / over-pressure route (G2), encode those two
fatality classes as structural impossibilities, not as warnings a planner can override. Mirror
kuramori's `assign-slot!` StowError discipline: an infeasible/unsafe request surfaces.
