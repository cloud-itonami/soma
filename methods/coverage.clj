;; soma 杣 — occupation sub-task coverage map (HONEST; asobi/shiori pattern).
;;
;; An honest map of the forestry/logging (伐採) occupation's sub-tasks against
;; what soma's methods actually implement. G5 sourcing-honesty: we MEASURE
;; coverage rather than overclaim — a sub-task is :covered? true ONLY if a real
;; existing method implements it (fell_plan, harvester, delimb, extraction,
;; siteprep, road, handoff, loadout). With siteprep + road landed the full
;; felling→regeneration cycle is covered (9/9).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0).
(ns soma.methods.coverage
  (:require [clojure.string :as str]))

(def sub-tasks
  "The forestry/logging occupation decomposed into sub-tasks, each marked
   covered? only when a real existing soma method implements it."
  [{:id :felling
    :desc "directional tree felling (notch+hinge, fall-azimuth)"
    :covered? true  :method "fell_plan"}
   {:id :bucking
    :desc "cut-to-length bucking value optimization"
    :covered? true  :method "harvester"}
   {:id :grading
    :desc "log assortment grading/sorting"
    :covered? true  :method "harvester"}
   {:id :extraction
    :desc "forwarder/skidder extraction (slope/soil-limited)"
    :covered? true  :method "extraction"}
   {:id :timber-supply
    :desc "graded-timber handoff to tatekata"
    :covered? true  :method "handoff"}
   {:id :delimbing
    :desc "limbing/delimbing the felled stem"
    :covered? true  :method "delimb"}
   {:id :site-prep
    :desc "site preparation / replanting / regeneration"
    :covered? true  :method "siteprep"}
   {:id :forest-road
    :desc "forest-road / skid-trail planning"
    :covered? true  :method "road"}
   {:id :load-out
    :desc "log load-out + haul transport"
    :covered? true  :method "loadout"}])

(defn report
  "Honest coverage summary: total, covered count, coverage fraction, and the
   uncovered sub-tasks (the gaps)."
  []
  (let [total   (count sub-tasks)
        covered (filterv :covered? sub-tasks)
        gaps    (filterv (complement :covered?) sub-tasks)]
    {:total    total
     :covered  (count covered)
     :coverage (/ (double (count covered)) total)
     :gaps     gaps}))

(defn report-str
  "Human-readable coverage report with an honest gap bullet list."
  []
  (let [{:keys [total covered coverage gaps]} (report)
        pct (format "%.1f%%" (* 100.0 coverage))]
    (str/join
     "\n"
     (concat
      [(str "soma 杣 — occupation sub-task coverage (HONEST; "
            (if (seq gaps) "partial by design" "full — felling→regeneration cycle complete") ")")
       (str "  coverage: " pct "  (" covered "/" total " sub-tasks)")
       (str "  gaps (" (count gaps) " uncovered):")]
      (if (seq gaps)
        (map #(str "    - " (name (:id %)) " — " (:desc %)) gaps)
        ["    (none — full coverage)"])))))

(defn -main [& _args]
  (println (report-str)))
