;; soma 杣 — harvester processing-head delimbing pass.
;;
;; A single-grip harvester head, having felled a stem, draws it through the
;; delimbing knives: the head feeds the stem axially while the knives shear off
;; the branch whorls. The delimbing planning problem is:
;;   - how many branch whorls the head removes over the stem length
;;     (≈ stem length / whorl spacing — branches grow in whorls at the nodes),
;;   - how long the feed pass takes (stem length / feed rate), and
;;   - whether the head can physically GRIP the stem at all: a stem whose butt
;;     diameter EXCEEDS the head's max-diameter cannot be processed — the knives
;;     and feed rollers cannot close around it. That RAISES (matches soma's
;;     raising-gate discipline: an infeasible request surfaces, never silently
;;     forced through an oversize stem).
;;
;; This is the planning core behind the harvester-head delimb pass — pure
;; planning compute, moves no real head (G1 no-server-key / R0 design+sim).
;;
;; KPI is whorls/stem + pass-time (an EQUIPMENT metric), never a per-worker pace (G3).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0). Clojure-first (the GAP-actor wave).
(ns soma.methods.delimb)

;; ── single-stem delimbing pass ───────────────────────────────────────────────
(defn delimb-pass
  "Plan one delimbing pass of a felled `stem`
     {:stem-id :length-m :diameter-cm :whorl-spacing-m}
   through a processing `head` {:max-diameter-cm :feed-rate-mps}.

   The head GRIPS the stem and feeds it axially through the delimbing knives.
   A stem whose butt `:diameter-cm` EXCEEDS the head's `:max-diameter-cm` cannot
   be gripped → RAISES (oversize is infeasible, never silently forced).

   Returns
     {:stem-id <id>
      :branches-removed <≈ length / whorl-spacing, whole whorls>
      :pass-time-s <length / feed-rate>
      :clean? true}."
  [{:keys [stem-id length-m diameter-cm whorl-spacing-m] :as _stem}
   {:keys [max-diameter-cm feed-rate-mps] :as _head}]
  (when (or (nil? length-m) (neg? length-m))
    (throw (ex-info "stem length must be non-negative" {:stem-id stem-id :length-m length-m})))
  (when (or (nil? whorl-spacing-m) (not (pos? whorl-spacing-m)))
    (throw (ex-info "whorl spacing must be positive" {:stem-id stem-id :whorl-spacing-m whorl-spacing-m})))
  (when (or (nil? feed-rate-mps) (not (pos? feed-rate-mps)))
    (throw (ex-info "feed rate must be positive" {:stem-id stem-id :feed-rate-mps feed-rate-mps})))
  (when (> (double diameter-cm) (double max-diameter-cm))
    (throw (ex-info "stem diameter exceeds head max — cannot process oversize stem"
                    {:stem-id stem-id
                     :diameter-cm diameter-cm
                     :max-diameter-cm max-diameter-cm})))
  {:stem-id stem-id
   :branches-removed (long (Math/floor (/ (double length-m) (double whorl-spacing-m))))
   :pass-time-s (/ (double length-m) (double feed-rate-mps))
   :clean? true})

;; ── multi-stem delimbing plan ────────────────────────────────────────────────
(defn process-stems
  "Run `delimb-pass` over a seq of `stems` through one `head`, summing the total
   pass time + total branches removed. A plan must be FEASIBLE: the first oversize
   stem RAISES (propagated from `delimb-pass`) — we do not silently skip an
   un-processable stem, because that stem still has to be dealt with on the ground.

   Returns
     {:n-stems <count>
      :total-pass-time-s <Σ pass-time-s>
      :total-branches-removed <Σ branches-removed>
      :passes [<delimb-pass result> …]}."
  [stems head]
  (let [passes (mapv #(delimb-pass % head) stems)]
    {:n-stems (count passes)
     :total-pass-time-s (reduce + 0.0 (map :pass-time-s passes))
     :total-branches-removed (reduce + 0 (map :branches-removed passes))
     :passes passes}))

(defn -main [& _args]
  (let [head {:max-diameter-cm 60.0 :feed-rate-mps 5.0}
        stems [{:stem-id "s-1" :length-m 24.0 :diameter-cm 42.0 :whorl-spacing-m 0.6}
               {:stem-id "s-2" :length-m 18.0 :diameter-cm 35.0 :whorl-spacing-m 0.5}]
        plan (process-stems stems head)]
    (println "soma 杣 — harvester delimbing pass (R0 design+sim)")
    (println (str "  head: max-diameter " (:max-diameter-cm head) " cm, feed "
                  (:feed-rate-mps head) " m/s"))
    (doseq [{:keys [stem-id branches-removed pass-time-s clean?]} (:passes plan)]
      (println (format "    %-5s  %2d whorls removed  %.1fs pass  clean?=%s"
                       stem-id branches-removed pass-time-s (str clean?))))
    (println (format "  TOTAL: %d stems  %d branches  %.1fs"
                     (:n-stems plan)
                     (:total-branches-removed plan)
                     (:total-pass-time-s plan)))
    ;; demo the oversize RAISE (G-style refusal): a 72 cm stem on a 60 cm head
    (println "  oversize check (72 cm stem on a 60 cm head):")
    (try
      (delimb-pass {:stem-id "s-big" :length-m 20.0 :diameter-cm 72.0 :whorl-spacing-m 0.6} head)
      (println "    (no raise — UNEXPECTED)")
      (catch clojure.lang.ExceptionInfo e
        (println (str "    RAISED — " (.getMessage e)))))))
