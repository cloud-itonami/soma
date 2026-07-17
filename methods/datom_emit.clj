;; soma 杣 — kotoba Datom-log emitter (canonical EAVT state, ADR-2605312345).
;;
;; Projects the forest-stand graph into append-only kotoba Datoms [e a v tx op].
;;   GROUND (op :add, durable) — stand / tree / exclusion node datoms + felled 縁.
;;     This IS the Datom log.
;;   DERIVED (op :derived, transient :bond/is-transient true) — bucked total value,
;;     extraction max-grade/feasibility, refusal/unsafe counts; computed on READ,
;;     NOT persisted (N1/G2 pattern, mirrors asobi/kuramori datom_emit).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0).
(ns soma.methods.datom-emit
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [soma.methods.analyze :as az]))

(defn fmt
  "Format a value as an EDN Datom field: keywords bare, strings quoted, bools/nil literal."
  [v]
  (cond
    (true? v) "true"
    (false? v) "false"
    (nil? v) "nil"
    (keyword? v) (str v)
    (string? v) (if (str/starts-with? v ":")
                  v
                  (str \" (str/escape v {\\ "\\\\" \" "\\\""}) \"))
    (float? v) (format "%g" v)
    :else (str v)))

(defn datom [e a v tx op]
  (str "[" (fmt e) " " (str a) " " (fmt v) " " tx " " (str op) "]"))

(defn- present
  "Treat a run-day stage value as present only when it is an actual artifact —
   `:skipped` (no seed fixture) and nil count as absent (guard against NPE)."
  [v]
  (when (and (some? v) (not= v :skipped)) v))

(defn- base-lines
  "Inner GROUND + DERIVED datom lines for the base `run` result (no header/brackets).
   Byte-identical to the original emit body — `emit` output is unchanged."
  [seed res tx]
  (let [L (atom [])
        add! (fn [s] (swap! L conj s))
        felled? (set (map :tree (:fells res)))
        refused? (set (map :tree (:refused res)))]
    ;; GROUND — stand
    (let [st (:stand seed)]
      (add! (datom (:id st) :soma.stand/slope-pct (double (:slope-pct st 0.0)) tx :add))
      (add! (datom (:id st) :soma.stand/soil (:soil st :firm) tx :add)))
    ;; GROUND — trees (+ protected / no-cut flags)
    (doseq [t (:trees seed)]
      (add! (datom (:id t) :soma.tree/species (:species t) tx :add))
      (add! (datom (:id t) :soma.tree/diameter-m (double (:diameter-m t 0.0)) tx :add))
      (add! (datom (:id t) :soma.tree/height-m (double (:height-m t 0.0)) tx :add))
      (add! (datom (:id t) :soma.tree/lean-deg (double (:lean-deg t 0.0)) tx :add))
      (add! (datom (:id t) :soma.tree/protected (boolean (or (:protected t) (:no-cut t))) tx :add)))
    ;; GROUND — exclusions (humans/road/watercourse, the fall-zone keep-outs)
    (doseq [x (:exclusions seed)]
      (add! (datom (:id x) :soma.exclusion/kind (:kind x) tx :add)))
    ;; GROUND — forwarder
    (let [f (:forwarder seed)]
      (add! (datom (:id f) :soma.forwarder/max-grade-pct (double (:max-grade-pct f 0.0)) tx :add)))
    ;; GROUND — felled 縁 (tree → log, with fall azimuth + hinge width)
    (doseq [fl (:fells res)]
      (let [en (str "en." (:tree fl) ".felled")]
        (add! (datom en :en/from (:tree fl) tx :add))
        (add! (datom en :en/kind :felled tx :add))
        (add! (datom en :soma.log/fall-az (double (:fall-az fl)) tx :add))
        (add! (datom en :soma.log/hinge-m (double (:hinge-m fl)) tx :add))
        (add! (datom en :soma.log/value (double (get-in fl [:buck :value] 0.0)) tx :add))))
    ;; GROUND — refusal 縁 (protected/no-cut trees, G7 — recorded, never felled)
    (doseq [r (:refused res)]
      (let [en (str "en." (:tree r) ".refused")]
        (add! (datom en :en/from (:tree r) tx :add))
        (add! (datom en :en/kind :refused-protected tx :add))))
    ;; DERIVED — transient readouts (computed on read; not durable)
    (add! ";; ── DERIVED readouts (transient; computed on read) ──")
    (let [sid (get-in seed [:stand :id])]
      (add! (datom sid :bond/total-value (double (:total-value res)) tx :derived))
      (add! (datom sid :bond/n-felled (count felled?) tx :derived))
      (add! (datom sid :bond/n-refused (count refused?) tx :derived))
      (add! (datom sid :bond/n-unsafe (count (:unsafe res)) tx :derived))
      (add! (datom sid :bond/extraction-feasible
                   (boolean (get-in res [:extraction :feasible])) tx :derived))
      (add! (datom sid :bond/extraction-max-grade
                   (double (get-in res [:extraction :max-grade-pct] 0.0)) tx :derived)))
    @L))

(defn- day-lines
  "Inner datom lines for the FULL run-day pipeline artifacts — so the canonical log
   captures the whole forestry DAY (timber-supply handoffs, load-out, replant,
   road), not just the base fell/buck/extract. GROUND :add for operations;
   DERIVED for metrics. Every :day key is guarded with `present` so a `:skipped`
   stage (no seed fixture) never NPEs."
  [res tx]
  (let [day (:day res)]
    (concat
     [";; ── run-day operations (GROUND) ──"]
     ;; timber-supply handoff 縁 (soma → tatekata) — the chain edge (mirrors kuramori)
     (mapcat (fn [h]
               (let [e (str "en.handoff." (:from-actor h) "." (:to-actor h) "." (:id h))]
                 [(datom e :handoff/from-actor (:from-actor h) tx :add)
                  (datom e :handoff/to-actor (:to-actor h) tx :add)
                  (datom e :handoff/kind (:kind h) tx :add)]))
             (present (:handoff day)))
     ;; load-out — each loaded log id rides the haul truck (GROUND op)
     (when-let [lo (present (:loadout day))]
       (for [log-id (:loaded lo)]
         (datom (str "load." log-id) :soma.loadout/loaded true tx :add)))
     ;; replant — the regenerated cohort (GROUND: seedling count on the prepared area)
     (when-let [rp (present (:replant day))]
       [(datom (str "replant." (name (:species rp :stand)))
               :soma.replant/seedling-count (:seedling-count rp) tx :add)])
     ;; road / skid-trail — the planned route length + crossings (GROUND op)
     (when-let [rd (present (:road day))]
       [(datom "road.landing-to-stand" :soma.road/length-m (double (:total-length-m rd)) tx :add)
        (datom "road.landing-to-stand" :soma.road/crossings (:crossings rd) tx :add)])
     ;; DERIVED day metrics
     [";; ── run-day metrics (DERIVED) ──"]
     (when-let [lo (present (:loadout day))]
       [(datom "facility" :bond/loadout-util (double (:weight-util lo)) tx :derived)])
     (when-let [rp (present (:replant day))]
       [(datom "facility" :bond/replant-seedlings (:seedling-count rp) tx :derived)]))))

(defn- wrap [lines]
  (str ";; soma 杣 — GENERATED kotoba Datom log (ADR-2606142010). DO NOT hand-edit.\n"
       ";; Canonical EAVT state (ADR-2605312345). [e a v tx op].\n"
       ";; GROUND op :add = durable. DERIVED :bond/is-transient = computed on read (N1/G2).\n"
       "[\n" (str/join "\n" (remove nil? lines)) "\n]\n"))

(defn emit
  "Emit the base forest-stand Datom log (fell/buck/extract) as an EDN string.
   `seed` is the loaded map, `res` the analyze/run result, `tx` the transaction
   number. Output is byte-identical to the R0 emitter."
  [seed res tx]
  (wrap (base-lines seed res tx)))

(defn emit-day
  "Emit the FULL day Datom log — base GROUND/DERIVED PLUS the run-day pipeline
   artifacts (timber-supply handoffs, load-out, replant, road). `day-res` =
   az/run-day. A strict superset of `emit`."
  [seed day-res tx]
  (wrap (concat (base-lines seed day-res tx) (day-lines day-res tx))))

(defn -main [& args]
  (let [path (or (first args) "20-actors/soma/data/stand.edn")
        seed (az/load-seed path)
        day-res (az/run-day seed)]
    (print (emit-day seed day-res 1))
    (flush)))
