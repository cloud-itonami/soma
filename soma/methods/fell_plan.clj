;; soma 杣 — directional tree-felling mechanics (notch + hinge + back cut).
;;
;; Directional felling is the headline forestry-robotics safety problem: a felled
;; tree must drop where the planner intends, into a clear fall zone. The hinge
;; (holding wood left between the notch face and the back cut) steers the fall;
;; the predicted fall direction is the tree's natural lean BIASED by the cut's
;; aim and PERTURBED by wind. The fall ZONE is a sector around that line out to
;; ≈1.5× tree height; it must contain NO human/road/watercourse exclusion point.
;;
;; This is the planning core behind the `fell` cell. It moves no real saw —
;; pure planning compute (G1 no-server-key / R0 design+sim).
;;
;; UNSAFE / protected fells RAISE (ex-info), never silently plan (G5 fall-fatality
;; gate + G7 protected-species/no-cut refusal). Felling is the #1 logging hazard.
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0). Clojure-first (the GAP-actor wave).
(ns soma.methods.fell-plan)

;; ── trig helpers (degrees) ────────────────────────────────────────────────────
(defn- deg->rad [d] (* d (/ Math/PI 180.0)))
(defn- rad->deg [r] (* r (/ 180.0 Math/PI)))

(defn norm-az
  "Normalise an azimuth (deg) into [0, 360)."
  [az]
  (let [m (mod az 360.0)] (if (neg? m) (+ m 360.0) m)))

(defn ang-diff
  "Smallest absolute angular difference (deg) between two azimuths, in [0, 180]."
  [a b]
  (let [d (Math/abs (- (norm-az a) (norm-az b)))]
    (if (> d 180.0) (- 360.0 d) d)))

;; ── hinge (holding-wood) geometry ─────────────────────────────────────────────
(def ^:const hinge-ratio
  "Hinge (holding-wood) thickness as a fraction of stem diameter — the felling-saw
   rule of thumb ≈ 10% of DBH. Per ADR-2606142010 G5; not tunable down by a planner
   (too thin = barber-chair / loss of steering)."
  0.10)

(defn hinge-width-m
  "Holding-wood width (m) left for a tree of given diameter. The hinge is what
   steers the fall — thinner than this and directional control is lost."
  [diameter-m]
  (when (not (pos? diameter-m)) (throw (ex-info "diameter must be positive" {:d diameter-m})))
  (* hinge-ratio diameter-m))

;; ── predicted fall direction ──────────────────────────────────────────────────
(def ^:const wind-bias-per-mps
  "Degrees the fall line is pulled toward the wind azimuth per m/s of wind,
   when the wind blows across the intended aim. A modest perturbation; a strong
   cross-wind makes a fell unsafe to attempt (caller's gate)."
  2.0)

(defn predict-fall-az
  "Predict the fall azimuth (deg) of a notch/hinge cut.

   The notch face is cut to AIM the tree at `aim-az`; the hinge holds the fall
   toward that aim. Two physical pulls perturb it:
     * natural LEAN — the crown's weight pulls toward `lean-az`, weighted by the
       lean angle (a steeper lean overrides the aim more);
     * WIND — a cross-wind nudges the line toward the wind azimuth.

   Returns the resultant azimuth in [0,360). Pure geometry; no actuation."
  [{:keys [aim-az lean-az lean-deg wind-az wind-mps]
    :or {lean-deg 0.0 wind-mps 0.0 wind-az 0.0}}]
  ;; lean weight grows with lean angle (cap influence at a hard lean ~15°)
  (let [lean-w (min 1.0 (/ (double lean-deg) 15.0))
        ;; resolve aim vs lean as a weighted unit-vector sum
        a (deg->rad aim-az)
        l (deg->rad lean-az)
        x (+ (* (- 1.0 lean-w) (Math/cos a)) (* lean-w (Math/cos l)))
        y (+ (* (- 1.0 lean-w) (Math/sin a)) (* lean-w (Math/sin l)))
        base (rad->deg (Math/atan2 y x))
        ;; wind nudge: signed toward the wind azimuth, scaled by speed
        wind-pull (* wind-bias-per-mps (double wind-mps))
        ;; sign: rotate base toward wind-az along the short arc
        delta (let [raw (- (norm-az wind-az) (norm-az base))
                    raw (cond (> raw 180.0) (- raw 360.0)
                              (< raw -180.0) (+ raw 360.0)
                              :else raw)]
                (* (Math/signum (double raw)) (min wind-pull (Math/abs raw))))]
    (norm-az (+ base delta))))

;; ── fall zone + exclusion safety (G5) ────────────────────────────────────────
(def ^:const fall-zone-radius-factor
  "Fall-zone radius as a multiple of tree height. A tree can throw debris well
   past its own length; 1.5× height is the keep-out radius. Per ADR-2606142010 G5."
  1.5)

(def ^:const fall-zone-half-angle-deg
  "Half-angle (deg) of the fall sector around the predicted fall line. Anything
   inside ±this of the fall azimuth, within the radius, is in the danger sector."
  35.0)

(defn- dist [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2)))))

(defn- bearing-deg
  "Azimuth (deg) from point `from` to point `to`."
  [[x1 y1] [x2 y2]]
  (norm-az (rad->deg (Math/atan2 (- y2 y1) (- x2 x1)))))

(defn in-fall-zone?
  "True iff exclusion point `ex` lies inside the fall sector of a tree felled
   from `tree-coord` along `fall-az`, given tree height. Within 1.5× height AND
   within ±half-angle of the fall line."
  [tree-coord fall-az height-m ex-coord]
  (let [r (* fall-zone-radius-factor height-m)
        d (dist tree-coord ex-coord)]
    (and (<= d r)
         (<= (ang-diff fall-az (bearing-deg tree-coord ex-coord))
             fall-zone-half-angle-deg))))

(defn fall-zone-intrusions
  "All exclusion points that fall inside the tree's fall zone. Each exclusion is
   a map with :coord (and typically :id / :kind)."
  [tree fall-az exclusions]
  (filter #(in-fall-zone? (:coord tree) fall-az (:height-m tree) (:coord %)) exclusions))

;; ── the planning gate ─────────────────────────────────────────────────────────
(defn protected?
  "True iff the tree is constitutionally un-fellable: a protected species or a
   no-cut flag (old-growth / seed-tree). Per ADR-2606142010 G7."
  [tree]
  (boolean (or (:protected tree) (:no-cut tree))))

(defn safe-fell?
  "True iff felling `tree` along `fall-az` is safe AND permitted:
     * G7 — the tree is not protected / not no-cut;
     * G5 — the fall zone contains NO exclusion point.
   Pure predicate; never throws (use `plan-fell` for the raising variant)."
  [tree fall-az exclusions]
  (and (not (protected? tree))
       (empty? (fall-zone-intrusions tree fall-az exclusions))))

(defn plan-fell
  "Plan a directional fell of `tree` aimed at `aim-az`, against the stand's wind
   and `exclusions`. Returns a plan map {:tree :fall-az :hinge-m :fall-zone-r
   :exclusions-clear}. RAISES (ex-info) when the tree is protected (G7) or when
   the fall zone overlaps any exclusion/human point (G5) — an unsafe or forbidden
   fell must SURFACE, never be silently planned. Felling is the #1 logging hazard."
  [tree aim-az exclusions]
  (when (protected? tree)
    (throw (ex-info "tree is protected / no-cut — felling refused (G7)"
                    {:tree (:id tree) :protected (:protected tree) :no-cut (:no-cut tree)})))
  (let [fall-az (predict-fall-az {:aim-az aim-az
                                  :lean-az (:lean-az tree 0.0)
                                  :lean-deg (:lean-deg tree 0.0)
                                  :wind-az (:wind-az tree 0.0)
                                  :wind-mps (:wind-mps tree 0.0)})
        intrusions (fall-zone-intrusions tree fall-az exclusions)]
    (when (seq intrusions)
      (throw (ex-info "fall zone overlaps an exclusion/human point — felling refused (G5)"
                      {:tree (:id tree)
                       :fall-az fall-az
                       :intrusions (mapv :id intrusions)})))
    {:tree (:id tree)
     :fall-az fall-az
     :hinge-m (hinge-width-m (:diameter-m tree))
     :fall-zone-r (* fall-zone-radius-factor (:height-m tree))
     :exclusions-clear true}))
