;; soma 杣 — log load-out onto a haul truck.
;;
;; Once a stem is bucked + delimbed into graded log assortments, the logs are
;; loaded onto a haul truck (the load-out / haul leg) for transport off the
;; landing. The load-out planning problem is the classic bin-packing variant:
;;   - the truck has a finite payload (:max-weight-kg) and a bunk/log-length
;;     limit (:max-length-m — a log longer than the bunks cannot ride),
;;   - pack the graded assortments by First-Fit-Decreasing (heaviest first) so
;;     the high-mass logs claim payload first and we maximise weight utilisation,
;;   - logs that don't fit the remaining payload stay :remaining (wait for the
;;     next truck) — never silently dropped,
;;   - a single log that CANNOT be hauled at all (longer than the bunks, or
;;     heavier than the whole truck's payload) RAISES — it is infeasible and
;;     must surface, never be forced onto a truck that physically cannot carry
;;     it. This mirrors soma's raising-gate discipline (cf. delimb oversize,
;;     extraction over-grade): an infeasible request surfaces, never silently
;;     forced through.
;;
;; This is the planning core behind the load-out / haul leg — pure planning
;; compute, moves no real truck (G1 no-server-key / R0 design+sim).
;;
;; KPI is weight-util / payload (an EQUIPMENT metric), never a per-worker pace (G3).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0). Clojure-first (the GAP-actor wave).
(ns soma.methods.loadout)

;; ── single-log feasibility (the raising gate) ────────────────────────────────
(defn- assert-haulable!
  "A single log MUST be physically haulable on this truck: its :length-m must
   fit the bunks (≤ :max-length-m) and its :weight-kg alone must not exceed the
   whole truck's payload (≤ :max-weight-kg). Otherwise RAISES — an un-haulable
   log surfaces, never silently forced onto a truck that cannot carry it."
  [{:keys [log-id length-m weight-kg] :as _log}
   {:keys [max-weight-kg max-length-m] :as _truck}]
  (when (> (double length-m) (double max-length-m))
    (throw (ex-info "log length exceeds truck bunks — cannot be hauled"
                    {:log-id log-id :length-m length-m :max-length-m max-length-m})))
  (when (> (double weight-kg) (double max-weight-kg))
    (throw (ex-info "log weight exceeds truck payload — cannot be hauled"
                    {:log-id log-id :weight-kg weight-kg :max-weight-kg max-weight-kg}))))

;; ── load-out: First-Fit-Decreasing (heaviest first) ─────────────────────────
(defn load-truck
  "Load graded `assortments` (a seq of {:log-id :length-m :weight-kg :grade})
   onto a haul `truck` {:max-weight-kg :max-length-m :bunk-count} by
   First-Fit-Decreasing (heaviest log first): each log is loaded iff the running
   total weight + its :weight-kg ≤ :max-weight-kg AND its :length-m ≤
   :max-length-m. Logs that don't fit the remaining payload stay :remaining
   (they ride the next truck) — never silently dropped.

   A single log that CANNOT be hauled at all (longer than the bunks, or heavier
   than the whole payload) RAISES (via assert-haulable!).

   Returns
     {:loaded      [<log-id> …]   ;; in load order (heaviest first)
      :remaining   [<log-id> …]
      :weight-util <0..1>}        ;; loaded weight / max-weight."
  [assortments {:keys [max-weight-kg max-length-m] :as truck}]
  (when (or (nil? max-weight-kg) (not (pos? max-weight-kg)))
    (throw (ex-info "truck max-weight-kg must be positive" {:max-weight-kg max-weight-kg})))
  (when (or (nil? max-length-m) (not (pos? max-length-m)))
    (throw (ex-info "truck max-length-m must be positive" {:max-length-m max-length-m})))
  ;; every log must be individually haulable — surface the infeasible first.
  (doseq [log assortments] (assert-haulable! log truck))
  (let [;; FFD — heaviest log claims payload first.
        sorted (sort-by #(- (double (:weight-kg %))) assortments)
        {:keys [loaded remaining used]}
        (reduce
         (fn [{:keys [used] :as acc} {:keys [log-id length-m weight-kg]}]
           (if (and (<= (+ used (double weight-kg)) (double max-weight-kg))
                    (<= (double length-m) (double max-length-m)))
             (-> acc
                 (update :loaded conj log-id)
                 (assoc :used (+ used (double weight-kg))))
             (update acc :remaining conj log-id)))
         {:loaded [] :remaining [] :used 0.0}
         sorted)]
    {:loaded loaded
     :remaining remaining
     :weight-util (/ used (double max-weight-kg))}))

(defn -main [& _args]
  (let [truck {:max-weight-kg 24000.0 :max-length-m 13.0 :bunk-count 4}
        logs [{:log-id "l-1" :length-m 5.0 :weight-kg 9000.0 :grade :sawlog}
              {:log-id "l-2" :length-m 4.0 :weight-kg 8000.0 :grade :sawlog}
              {:log-id "l-3" :length-m 3.0 :weight-kg 7000.0 :grade :pulp}
              {:log-id "l-4" :length-m 3.0 :weight-kg 6000.0 :grade :pulp}]
        plan (load-truck logs truck)]
    (println "soma 杣 — log load-out onto a haul truck (R0 design+sim)")
    (println (format "  truck: payload %.0f kg, bunks %.1f m × %d"
                     (:max-weight-kg truck) (:max-length-m truck) (:bunk-count truck)))
    (println (str "  loaded    (heaviest-first): " (:loaded plan)))
    (println (str "  remaining (next truck):     " (:remaining plan)))
    (println (format "  weight-util: %.1f%%" (* 100.0 (:weight-util plan))))
    ;; demo the un-haulable RAISE (G-style refusal): an over-length log on the bunks
    (println "  over-length check (15 m log on 13 m bunks):")
    (try
      (load-truck [{:log-id "l-long" :length-m 15.0 :weight-kg 5000.0 :grade :sawlog}] truck)
      (println "    (no raise — UNEXPECTED)")
      (catch clojure.lang.ExceptionInfo e
        (println (str "    RAISED — " (.getMessage e)))))))
