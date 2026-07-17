;; soma 杣 — skid-trail / forest-road planning.
;;
;; To extract logs, soma plans a route from the LANDING to a STAND over a network
;; of terrain segments {:from :to :grade-pct :length-m :stream-crossing? :culvert?}.
;; Two hard limits govern a feasible road/skid-trail:
;;   - GRADE — a segment steeper than the machine's max road grade (default 20%)
;;     is unsafe to traverse loaded and is REFUSED (slope safety, mirrors the
;;     extraction slope gate),
;;   - WATERCOURSE — a stream crossing WITHOUT a `:culvert?` ruts the bed, silts
;;     the watercourse, and is NOT regenerative; an unmitigated crossing is
;;     REFUSED (water-protection, matches the actor's G2 soil/eco discipline).
;;
;; The router finds the shortest feasible path from landing → stand through the
;; segment graph (Dijkstra over :length-m); a single infeasible segment (over
;; grade OR an unmitigated stream crossing) RAISES — an infeasible road must
;; surface, never be silently built (G2). If `:from`/`:to` are omitted the
;; segments are treated as a simple ordered list (each still gated).
;;
;; This is the planning core behind the road / skid-trail leg — pure planning
;; compute, builds no real road (G1 no-server-key / R0 design+sim).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0). Clojure-first (the GAP-actor wave).
(ns soma.methods.road)

(def ^:const default-max-grade-pct
  "Max sustainable forest-road / skid-trail grade (%). Steeper segments are
   unsafe to traverse loaded (runaway / loss of traction) and are refused."
  20.0)

;; ── per-segment feasibility (the raising gate, G2) ───────────────────────────
(defn- assert-segment!
  "A terrain segment MUST be feasible: its :grade-pct within `max-grade-pct`, and
   any :stream-crossing? must carry a :culvert?. Otherwise RAISES — an over-grade
   segment or an unmitigated watercourse crossing surfaces, never silently built."
  [{:keys [from to grade-pct stream-crossing? culvert?] :as _seg} max-grade-pct]
  (when (> (Math/abs (double (or grade-pct 0.0))) (double max-grade-pct))
    (throw (ex-info "road segment exceeds max grade — refused (slope safety)"
                    {:segment {:from from :to to :grade-pct grade-pct}
                     :max-grade-pct max-grade-pct})))
  (when (and stream-crossing? (not culvert?))
    (throw (ex-info "stream crossing without a culvert — refused (water-protection, G2)"
                    {:segment {:from from :to to :stream-crossing? stream-crossing?}}))))

;; ── shortest feasible path (Dijkstra over :length-m) ─────────────────────────
(defn- shortest-path
  "Dijkstra from `landing` to `stand` over the (already feasibility-checked)
   directed `segments`. Returns {:path [node …] :length-m total} or nil if no
   path connects landing → stand."
  [landing stand segments]
  (let [adj (reduce (fn [m {:keys [from to length-m] :as s}]
                      (update m from (fnil conj []) (assoc s :w (double (or length-m 0.0)))))
                    {} segments)]
    (loop [dist {landing 0.0}
           prev {}
           ;; frontier = set of unsettled nodes we have a tentative distance for
           frontier #{landing}
           settled #{}]
      (if (empty? frontier)
        ;; reconstruct if reachable
        (when (contains? dist stand)
          (let [path (loop [n stand acc (list stand)]
                       (if-let [p (prev n)]
                         (recur p (conj acc p))
                         (vec acc)))]
            {:path path :length-m (dist stand)}))
        (let [u (apply min-key dist frontier)
              frontier' (disj frontier u)
              settled' (conj settled u)
              [dist' prev' frontier'']
              (reduce
               (fn [[d pv fr] {:keys [to w]}]
                 (if (settled' to)
                   [d pv fr]
                   (let [nd (+ (d u) w)]
                     (if (< nd (get d to Double/POSITIVE_INFINITY))
                       [(assoc d to nd) (assoc pv to u) (conj fr to)]
                       [d pv (conj fr to)]))))
               [dist prev frontier']
               (get adj u []))]
          (recur dist' prev' frontier'' settled'))))))

(defn plan-road
  "Plan a skid-trail / forest-road route from `landing` to `stand` over terrain
   `segments` (each {:from :to :grade-pct :length-m [:stream-crossing?] [:culvert?]}).

   Every segment is gated FIRST: a segment over `:max-grade-pct` (default 20%) or
   a stream crossing without a `:culvert?` RAISES (slope safety + water-protection,
   G2) — an infeasible road surfaces, never silently built.

   Routing: when `landing`/`stand` are supplied, returns the shortest feasible
   path through the graph; otherwise the segments are taken as a simple ordered
   list and the route is their endpoints in order. Returns
     {:route [node …] :total-length-m <m> :crossings <n> :max-grade-pct <%>
      :feasible true}."
  ([segments] (plan-road segments nil))
  ([segments {:keys [landing stand max-grade-pct]
              :or   {max-grade-pct default-max-grade-pct}}]
   (when (empty? segments)
     (throw (ex-info "no segments to plan a road over" {:segments segments})))
   ;; gate every segment first — surface the infeasible before routing.
   (doseq [seg segments] (assert-segment! seg max-grade-pct))
   (let [crossings (count (filter :stream-crossing? segments))
         max-grade (reduce max 0.0 (map #(Math/abs (double (or (:grade-pct %) 0.0))) segments))]
     (if (and landing stand)
       (let [sp (shortest-path landing stand segments)]
         (when-not sp
           (throw (ex-info "no feasible path connects landing → stand"
                           {:landing landing :stand stand})))
         {:route (:path sp)
          :total-length-m (:length-m sp)
          :crossings crossings
          :max-grade-pct max-grade
          :feasible true})
       ;; simple ordered-list mode: endpoints in order, total = Σ lengths
       {:route (into (vec (map :from segments)) [(:to (last segments))])
        :total-length-m (reduce + 0.0 (map #(double (or (:length-m %) 0.0)) segments))
        :crossings crossings
        :max-grade-pct max-grade
        :feasible true}))))

(defn -main [& _args]
  (let [segs [{:from "landing" :to "a" :grade-pct 8.0 :length-m 120.0}
              {:from "a" :to "stand" :grade-pct 12.0 :length-m 80.0
               :stream-crossing? true :culvert? true}
              {:from "landing" :to "stand" :grade-pct 6.0 :length-m 260.0}]
        plan (plan-road segs {:landing "landing" :stand "stand"})]
    (println "soma 杣 — skid-trail / forest-road planning (R0 design+sim, water-protected)")
    (println (str "  route: " (:route plan)))
    (println (format "  total length: %.0f m   crossings: %d   max grade: %.1f%%"
                     (:total-length-m plan) (:crossings plan) (:max-grade-pct plan)))
    ;; demo the over-grade RAISE (slope safety)
    (println "  over-grade check (25% segment):")
    (try
      (plan-road [{:from "x" :to "y" :grade-pct 25.0 :length-m 50.0}])
      (println "    (no raise — UNEXPECTED)")
      (catch clojure.lang.ExceptionInfo e
        (println (str "    RAISED — " (.getMessage e)))))
    ;; demo the unmitigated-stream RAISE (water-protection, G2)
    (println "  uncrossable-stream check (no culvert):")
    (try
      (plan-road [{:from "x" :to "y" :grade-pct 5.0 :length-m 40.0 :stream-crossing? true}])
      (println "    (no raise — UNEXPECTED)")
      (catch clojure.lang.ExceptionInfo e
        (println (str "    RAISED — " (.getMessage e)))))))
