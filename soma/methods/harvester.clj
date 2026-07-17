;; soma 杣 — cut-to-length bucking optimization + grapple-reach feasibility.
;;
;; A felled stem is "bucked" (cross-cut) into log assortments. Each cut length
;; belongs to a price class (sawlog pays more than pulp); the bucking problem is:
;; given a stem of length L and a price-by-length-class table, choose cuts that
;; MAXIMISE total value (the classic unbounded cut/rod-cutting DP). Plus a
;; grapple/boom reach feasibility check — the harvester head must physically
;; reach the stem before it can buck it.
;;
;; This is the planning core behind the `buck` cell. It moves no real head —
;; pure planning compute (G1 no-server-key / R0 design+sim).
;;
;; KPI is value/volume per stem — an EQUIPMENT metric, never a per-worker pace (G3).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0). Clojure-first (the GAP-actor wave).
(ns soma.methods.harvester)

;; ── grapple/boom reach (G8 tazuna-operated head) ─────────────────────────────
(defn reachable?
  "True iff the harvester head (at `boom` coord, with `reach-m` boom reach) can
   reach the stem at `stem` coord. A stem out of reach cannot be bucked here."
  [boom-coord reach-m stem-coord]
  (let [[bx by] boom-coord [sx sy] stem-coord
        d (Math/sqrt (+ (* (- bx sx) (- bx sx)) (* (- by sy) (- by sy))))]
    (<= d reach-m)))

;; ── cut-to-length bucking value DP (unbounded; the rod-cutting DP) ───────────
(defn- discretize
  "Quantise `length-m` to a centimetre grid so the DP table is integral.
   Returns the number of 1-cm units."
  [length-m]
  (long (Math/round (* 100.0 (double length-m)))))

(defn buck-stem
  "Maximise total value bucking a stem of `stem-length-m` using `price-table`
   (a seq of {:class :length-m :price}). `:price` is per CUT of that length
   (value yielded by one log of that class/length). Unbounded — a length class
   may be used any number of times. Returns
     {:value <max total> :cuts [{:class :length-m :price} …] :waste-m <remainder>}.
   This is the cut-to-length optimization the harvester head executes per stem."
  [stem-length-m price-table]
  (when (neg? stem-length-m) (throw (ex-info "stem length must be non-negative" {:l stem-length-m})))
  (let [N (discretize stem-length-m)
        ;; each option as integral units + its price
        opts (->> price-table
                  (map (fn [o] (assoc o :units (discretize (:length-m o)))))
                  (filter #(pos? (:units %)))
                  vec)
        ;; dp[i] = best value usable within i units; choice[i] = option index taken
        dp (long-array (inc N) 0)
        choice (int-array (inc N) -1)]
    (doseq [i (range 1 (inc N))]
      (doseq [oi (range (count opts))]
        (let [o (opts oi) u (:units o)]
          (when (<= u i)
            (let [cand (+ (aget dp (- i u)) (long (Math/round (* 100.0 (double (:price o))))))]
              (when (> cand (aget dp i))
                (aset dp i cand)
                (aset choice i oi)))))))
    ;; reconstruct the cut list from the best prefix (allow leftover waste at the tip)
    (let [best-i (apply max-key #(aget dp %) (range (inc N)))
          cuts (loop [i best-i acc []]
                 (let [c (aget choice i)]
                   (if (neg? c)
                     acc
                     (let [o (opts c)]
                       (recur (- i (:units o))
                              (conj acc (select-keys o [:class :length-m :price])))))))]
      {:value (/ (double (aget dp best-i)) 100.0)
       :cuts (vec (reverse cuts))
       :waste-m (/ (double (- N best-i)) 100.0)})))

(defn buck-summary
  "Roll up a bucking result into class counts + total value, for reporting.
   Returns {:value v :by-class {class count} :n-logs n :waste-m w}."
  [{:keys [value cuts waste-m]}]
  {:value value
   :by-class (frequencies (map :class cuts))
   :n-logs (count cuts)
   :waste-m waste-m})
