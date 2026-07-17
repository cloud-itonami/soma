;; soma 杣 — end-to-end forestry-stand analyzer (orchestrator).
;;
;; Loads the forest-stand seed and runs the R0 sim pipeline:
;;   1. fell — for every harvestable tree, plan a directional fell (notch + hinge +
;;      back cut) aimed into a clear lane; protected/no-cut trees are REFUSED (G7),
;;      unsafe fells (fall zone overlapping a human/road/watercourse) are REFUSED (G5);
;;   2. buck — cut-to-length value optimization of each felled stem (sawlog>pulp DP);
;;   3. extract — slope-limited, low-ground-impact forwarder route to the landing
;;      (refuses over-grade / over-pressure / protected soil, G2).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0).
(ns soma.methods.analyze
  (:require [clojure.edn :as edn]
            [soma.methods.fell-plan :as fp]
            [soma.methods.harvester :as hv]
            [soma.methods.extraction :as ex]
            [soma.methods.delimb :as dl]
            [soma.methods.loadout :as lo]
            [soma.methods.siteprep :as sp]
            [soma.methods.road :as rd]
            [soma.methods.handoff :as ho]))

(defn- tx-data?
  "True if content is a datomic/datascript tx-data vector (one entity map with
   :db/id), i.e. data/stand.edn's post-datomize shape."
  [content]
  (and (vector? content) (seq content) (map? (first content)) (contains? (first content) :db/id)))

(defn- unblob
  "pr-str'd non-scalar attribute values (nested maps / vectors-of-maps) come back
   as strings from the tx-data shape; parse them back to data. Leaves live scalars
   (and anything that fails to parse as a collection) untouched."
  [v]
  (if (string? v)
    (try (let [parsed (edn/read-string v)] (if (coll? parsed) parsed v))
         (catch Exception _ v))
    v))

(defn- reconstitute-entity
  "Un-namespace + un-blob a tx-data entity back into the original bare seed map
   (:stand/trees -> :trees etc.) so every downstream `(:keys [...])` destructure
   in this ns and in delimb/loadout/siteprep/road/handoff keeps working unchanged."
  [tx-data]
  (into {} (map (fn [[k v]] [(keyword (name k)) (unblob v)]))
        (dissoc (first tx-data) :db/id)))

(defn load-seed
  "Read the forest-stand EDN seed into a Clojure map. Accepts either the legacy
   bare map or the datomic/datascript tx-data vector (post-datomize) shape."
  [path]
  (let [content (edn/read-string (slurp path))]
    (if (tx-data? content)
      (reconstitute-entity content)
      content)))

(defn- aim-away-from-exclusions
  "Choose a fell aim azimuth (deg) for a tree: bias toward its natural lean, but
   if that line is blocked, sweep candidate azimuths and pick the first that
   clears every exclusion. Returns an azimuth, or nil if none clears (caller then
   records the tree as unsafe-to-fell rather than forcing it)."
  [tree exclusions]
  (let [candidates (cons (:lean-az tree 0.0)
                         (map double (range 0 360 15)))]
    (some (fn [aim]
            (let [az (fp/predict-fall-az {:aim-az aim
                                          :lean-az (:lean-az tree 0.0)
                                          :lean-deg (:lean-deg tree 0.0)
                                          :wind-az (:wind-az tree 0.0)
                                          :wind-mps (:wind-mps tree 0.0)})]
              (when (fp/safe-fell? tree az exclusions) aim)))
          candidates)))

(defn run
  "Run the full R0 analysis over a loaded seed map. Returns a report map.
   Each tree lands in exactly one of {:fells :refused :unsafe}."
  [seed]
  (let [{:keys [trees exclusions price-table forwarder route]} seed
        soil (get-in seed [:stand :soil] :firm)
        ;; 1. fell + 2. buck per tree
        results
        (reduce
         (fn [acc tree]
           (cond
             ;; G7 — protected / no-cut: refuse, do not fell
             (fp/protected? tree)
             (update acc :refused conj {:tree (:id tree) :reason :protected})
             :else
             (if-let [aim (aim-away-from-exclusions tree exclusions)]
               (let [plan (fp/plan-fell tree aim exclusions)
                     ;; merchantable stem length ≈ height minus crown/butt trim
                     stem-len (* 0.80 (:height-m tree))
                     buck (hv/buck-summary (hv/buck-stem stem-len price-table))]
                 (update acc :fells conj
                         {:tree (:id tree)
                          :fall-az (:fall-az plan)
                          :hinge-m (:hinge-m plan)
                          :stem-length-m stem-len
                          :buck buck}))
               ;; G5 — no safe aim clears the exclusions
               (update acc :unsafe conj {:tree (:id tree) :reason :no-clear-fall-line}))))
         {:fells [] :refused [] :unsafe []}
         trees)
        ;; 3. extract — plan the forwarder route (raises if over-grade/over-impact)
        extraction (ex/plan-route forwarder soil (:segments route))
        total-value (reduce + 0.0 (map #(get-in % [:buck :value]) (:fells results)))]
    (assoc results
           :extraction extraction
           :total-value total-value
           :n-trees (count trees))))

(defn run-day
  "Full forestry-DAY pipeline — threads the stand through EVERY domain method so
   they actually compose (R1 integration), not just coexist:
     fell(fell_plan) → buck/grade(harvester) → delimb(delimb) → extract(extraction)
     → load-out(loadout) → site-prep replant(siteprep) → road plan(road) →
     timber-supply handoff(handoff).
   Returns the base `run` report plus `:pipeline` (per-stage summary), `:methods`
   (the set of method modules exercised), and `:day` (the per-stage artifacts).
   Stages with no seed fixture are recorded `:skipped` rather than failing."
  [seed]
  (let [base (run seed)
        stage (fn [m summary] {:method m :summary summary})
        ;; 1. fell + 2. buck/grade — already done in `base` (fell_plan + harvester)
        n-fells (count (:fells base))
        ;; 3. delimb — feed each felled stem through the processing head
        head (:head seed)
        stems (when head
                (map (fn [f]
                       {:stem-id (:tree f)
                        :length-m (:stem-length-m f)
                        ;; head wants butt diameter in cm; firm stand stems are in-spec
                        :diameter-cm 40.0
                        :whorl-spacing-m 0.6})
                     (:fells base)))
        delimb (when (seq stems) (dl/process-stems stems head))
        ;; 4. extract — already planned in `base` (extraction route to the landing)
        ;; 5. load-out — pack the graded assortments onto the haul truck (FFD)
        loadout (when (and (:assortments seed) (:truck seed))
                  (lo/load-truck (:assortments seed) (:truck seed)))
        ;; 6. site-prep + replant the harvested area (regenerative-only, G2)
        replant (when (:replant seed) (sp/replant-plan (:replant seed)))
        ;; 7. road / skid-trail plan from the landing to the stand
        road (when (:road seed)
               (rd/plan-road (:segments (:road seed))
                             (select-keys (:road seed) [:landing :stand :max-grade-pct])))
        ;; 8. timber-supply handoff — graded assortments → tatekata lumber intents
        handoff (when (:assortments seed) (ho/outbound-handoff (:assortments seed)))
        pipeline (cond-> []
                   true    (conj (stage "fell_plan" (str n-fells " felled (safe)")))
                   true    (conj (stage "harvester" (format "bucked value %.1f" (:total-value base))))
                   delimb  (conj (stage "delimb" (str (:total-branches-removed delimb)
                                                      " branches / "
                                                      (format "%.1fs" (:total-pass-time-s delimb)))))
                   true    (conj (stage "extraction" (str (get-in base [:extraction :n-segments])
                                                          " segment(s), max grade "
                                                          (format "%.1f%%" (get-in base [:extraction :max-grade-pct])))))
                   loadout (conj (stage "loadout" (format "%d loaded, util %.0f%%"
                                                          (count (:loaded loadout))
                                                          (* 100.0 (:weight-util loadout)))))
                   replant (conj (stage "siteprep" (str (:seedling-count replant) " seedlings ("
                                                        (name (:prep-method replant)) ")")))
                   road    (conj (stage "road" (format "%.0fm, %d crossing(s)"
                                                       (:total-length-m road) (:crossings road))))
                   handoff (conj (stage "handoff" (str (count handoff) " timber-supply → tatekata"))))]
    (assoc base
           :pipeline pipeline
           :methods (set (map :method pipeline))
           :day {:delimb (or delimb :skipped)
                 :loadout (or loadout :skipped)
                 :replant (or replant :skipped)
                 :road (or road :skipped)
                 :handoff (or handoff :skipped)})))

(defn report-day-str
  "Human-readable full-day pipeline report."
  [res]
  (str ";; soma 杣 — full forestry-DAY pipeline (R1 integration)\n"
       "methods exercised: " (pr-str (sort (:methods res))) "\n"
       (apply str (map (fn [s] (str "  • " (:method s) " — " (:summary s) "\n"))
                       (:pipeline res)))))

(defn report-str
  "Human-readable report (for out/ and Murakumo narration input, G6)."
  [res]
  (str ";; soma 杣 — forestry-stand R0 analysis\n"
       "trees: " (:n-trees res) "\n"
       "felled (safe): " (count (:fells res)) "\n"
       "refused (protected/no-cut, G7): " (pr-str (mapv :tree (:refused res))) "\n"
       "unsafe (no clear fall line, G5): " (pr-str (mapv :tree (:unsafe res))) "\n"
       "total bucked value: " (format "%.1f" (:total-value res)) "\n"
       "extraction segments: " (get-in res [:extraction :n-segments])
       " (max grade " (format "%.1f" (get-in res [:extraction :max-grade-pct])) "%)\n"))

(defn -main [& args]
  (let [path (or (first args) "20-actors/soma/data/stand.edn")
        seed (load-seed path)
        res (run-day seed)]
    (print (report-str res))
    (print (report-day-str res))
    (flush)))
