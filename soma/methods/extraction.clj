;; soma 杣 — forwarder/skidder extraction: slope + low-ground-impact gates.
;;
;; After bucking, logs are EXTRACTED (forwarded/skidded) from stump to the
;; landing. Two hard limits govern an extraction route:
;;   * SLOPE — a route segment steeper than the machine's max grade is unsafe to
;;     traverse loaded (rollover / runaway) and is REFUSED;
;;   * GROUND IMPACT — operating on wet/protected soil beyond its bearing limit
;;     compacts and ruts it; soma is regenerative-only (G2), so an over-pressure
;;     or protected-soil pass is REFUSED.
;;
;; This is the planning core behind the `extract` cell. It moves no real machine —
;; pure planning compute (G1 no-server-key / R0 design+sim).
;;
;; An over-grade or over-pressure / protected-soil route RAISES (ex-info), never
;; silently extracts (G2 regenerative-only gate). Pure Clojure, no deps →
;; babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0). Clojure-first (the GAP-actor wave).
(ns soma.methods.extraction)

;; ── slope gate (safety) ───────────────────────────────────────────────────────
(defn grade-ok?
  "True iff `grade-pct` is within the forwarder's `:max-grade-pct`. A steeper grade
   is refused (rollover/runaway risk when loaded)."
  [forwarder grade-pct]
  (<= (Math/abs (double grade-pct)) (:max-grade-pct forwarder)))

;; ── ground-impact gate (G2 regenerative-only) ────────────────────────────────
(defn bearing-limit-kpa
  "Soil bearing limit (kPa) for a soil class. Wet/sensitive soil bears far less
   than firm; protected soil is 0 (no operation permitted at all)."
  [forwarder soil]
  (case soil
    :firm      (:bearing-firm-kpa forwarder 80.0)
    :wet       (:bearing-wet-kpa forwarder 30.0)
    :protected 0.0
    ;; unknown soil → conservative: treat as wet
    (:bearing-wet-kpa forwarder 30.0)))

(defn ground-ok?
  "True iff the forwarder's ground pressure is within the soil's bearing limit AND
   the soil is not :protected. Regenerative-only (G2) — no compaction beyond the
   soil's tolerance, no operation on protected ground."
  [forwarder soil]
  (and (not= soil :protected)
       (<= (:ground-pressure-kpa forwarder 45.0) (bearing-limit-kpa forwarder soil))))

;; ── the route-segment gate ────────────────────────────────────────────────────
(defn segment-feasible?
  "True iff the forwarder may traverse this segment: within grade AND within
   ground-impact limits. Pure predicate; never throws (use `plan-route` to raise)."
  [forwarder soil segment]
  (and (grade-ok? forwarder (:grade-pct segment 0.0))
       (ground-ok? forwarder soil)))

(defn plan-route
  "Plan an extraction route over `segments` on a stand of soil class `soil`.
   RAISES (ex-info) on the FIRST segment that exceeds the machine's max grade,
   or whenever the soil is over-bearing / protected — an over-grade or
   over-impact extraction must SURFACE, never be silently run (G2). Returns a
   plan map {:soil :n-segments :total-length-m :max-grade-pct :feasible true}."
  [forwarder soil segments]
  (when-not (ground-ok? forwarder soil)
    (throw (ex-info "soil over-bearing / protected — extraction refused (G2 regenerative-only)"
                    {:soil soil
                     :ground-pressure-kpa (:ground-pressure-kpa forwarder)
                     :bearing-limit-kpa (bearing-limit-kpa forwarder soil)})))
  (doseq [seg segments]
    (when-not (grade-ok? forwarder (:grade-pct seg 0.0))
      (throw (ex-info "route segment exceeds max grade — extraction refused (slope safety)"
                      {:segment (select-keys seg [:from :to :grade-pct])
                       :max-grade-pct (:max-grade-pct forwarder)}))))
  {:soil soil
   :n-segments (count segments)
   :total-length-m (reduce + 0.0 (map #(double (:length-m % 0.0)) segments))
   :max-grade-pct (reduce max 0.0 (map #(Math/abs (double (:grade-pct % 0.0))) segments))
   :feasible true})
