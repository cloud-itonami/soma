;; soma 杣 — site preparation + regeneration (replanting).
;;
;; After a selective harvest, the worked site is PREPARED for the next cohort of
;; trees and REPLANTED — soma is regenerative-only (G2), so the forest is renewed,
;; never clear-cut and abandoned. The replanting problem is:
;;   - given a harvested :area-ha and a :target-stems-per-ha density, compute the
;;     seedling count needed (area × density),
;;   - choose a site-prep / ground-disturbance method by soil type (scarification
;;     on firm mineral soil; mounding on wet/compacted soil to lift seedling roots
;;     clear of standing water),
;;   - REFUSE over-planting: a target density beyond the soil's max sustainable
;;     stocking compacts the stand, starves the regeneration of light/nutrients,
;;     and is NOT regenerative — it RAISES, never silently over-plants (G2),
;;   - REFUSE a clear-cut: a `:clear-cut?` flag is unrepresentable — soma is
;;     selective + regenerative only; clear-cut site-prep RAISES (G2).
;;
;; This is the planning core behind the site-prep / replant leg — pure planning
;; compute, moves no real machine (G1 no-server-key / R0 design+sim).
;;
;; KPI is seedlings/ha + survival stocking (an ECOLOGICAL metric), never a
;; per-worker pace (G3).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0). Clojure-first (the GAP-actor wave).
(ns soma.methods.siteprep)

;; ── max sustainable stocking (G2 regenerative-only) ──────────────────────────
(def ^:const default-max-stems-per-ha
  "Conservative upper bound on sustainable planting density (stems/ha). A target
   beyond this over-stocks the stand (light/nutrient starvation, compaction) and
   is NOT regenerative — it is refused, never silently planted."
  3000.0)

;; ── site-prep method choice by soil type ─────────────────────────────────────
(defn prep-method
  "Choose a site-preparation / ground-disturbance method by soil class:
     :firm / :mineral → :scarification (expose mineral soil, firm seedbed),
     :wet / :compacted → :mounding (raise seedling roots above standing water),
   unknown soil → conservative :mounding (assume wet)."
  [soil]
  (case soil
    (:firm :mineral) :scarification
    (:wet :compacted) :mounding
    :mounding))

(defn replant-plan
  "Plan the replant of a harvested site. Given
     {:area-ha :species :target-stems-per-ha [:soil] [:max-stems-per-ha] [:clear-cut?]}
   computes the seedling count (area × density) and the site-prep method by soil.

   RAISES (ex-info) when:
     - `:clear-cut?` is truthy — clear-cut is unrepresentable (G2 selective +
       regenerative only); soma never clear-cuts,
     - `:target-stems-per-ha` exceeds the max sustainable density — over-planting
       is not regenerative and is refused (G2),
     - `:area-ha` or `:target-stems-per-ha` is non-positive.

   Returns a plan map
     {:species :area-ha :target-stems-per-ha :seedling-count
      :soil :prep-method :regenerative true}."
  [{:keys [area-ha species target-stems-per-ha soil clear-cut? max-stems-per-ha]
    :or   {soil :firm}}]
  (when clear-cut?
    (throw (ex-info "clear-cut is unrepresentable — soma is selective + regenerative only (G2)"
                    {:clear-cut? clear-cut? :species species})))
  (when (or (nil? area-ha) (not (pos? (double area-ha))))
    (throw (ex-info "area-ha must be positive" {:area-ha area-ha})))
  (when (or (nil? target-stems-per-ha) (not (pos? (double target-stems-per-ha))))
    (throw (ex-info "target-stems-per-ha must be positive" {:target-stems-per-ha target-stems-per-ha})))
  (let [max-density (double (or max-stems-per-ha default-max-stems-per-ha))]
    (when (> (double target-stems-per-ha) max-density)
      (throw (ex-info "target density exceeds max sustainable stocking — over-planting refused (G2 regenerative-only)"
                      {:target-stems-per-ha target-stems-per-ha
                       :max-stems-per-ha max-density
                       :species species})))
    {:species species
     :area-ha (double area-ha)
     :target-stems-per-ha (double target-stems-per-ha)
     :seedling-count (long (Math/round (* (double area-ha) (double target-stems-per-ha))))
     :soil soil
     :prep-method (prep-method soil)
     :regenerative true}))

(defn -main [& _args]
  (let [plan (replant-plan {:area-ha 12.0 :species :cryptomeria
                            :target-stems-per-ha 2500.0 :soil :wet})]
    (println "soma 杣 — site preparation + replant (R0 design+sim, regenerative-only)")
    (println (format "  species: %s   area: %.1f ha   density: %.0f stems/ha"
                     (name (:species plan)) (:area-ha plan) (:target-stems-per-ha plan)))
    (println (format "  seedlings needed: %d" (:seedling-count plan)))
    (println (str "  soil: " (name (:soil plan)) "  → site-prep method: " (name (:prep-method plan))))
    ;; demo the over-planting RAISE (G2): a density beyond max sustainable stocking
    (println "  over-density check (5000 stems/ha):")
    (try
      (replant-plan {:area-ha 12.0 :species :cryptomeria :target-stems-per-ha 5000.0})
      (println "    (no raise — UNEXPECTED)")
      (catch clojure.lang.ExceptionInfo e
        (println (str "    RAISED — " (.getMessage e)))))
    ;; demo the clear-cut RAISE (G2): clear-cut is unrepresentable
    (println "  clear-cut check:")
    (try
      (replant-plan {:area-ha 12.0 :species :cryptomeria :target-stems-per-ha 2500.0 :clear-cut? true})
      (println "    (no raise — UNEXPECTED)")
      (catch clojure.lang.ExceptionInfo e
        (println (str "    RAISED — " (.getMessage e)))))))
