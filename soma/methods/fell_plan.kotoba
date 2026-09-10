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
;; TWO keep-outs, and they answer different questions:
;;   * the STATUTORY circle — 労働安全衛生規則 第481条第2項 puts every OTHER WORKER
;;     outside a full circle of radius 2x tree height. Direction is irrelevant: a
;;     worker behind the sawyer is as excluded as one on the fall line.
;;   * the directional FALL SECTOR — where the stem is predicted to land. This is
;;     the right model for a road or a watercourse, which the ordinance does not
;;     speak to, and it is soma's own model, not law.
;; Both gate. See data/authorities.edn :anei-481-2 for the quoted provision and
;; for what this code did wrong before 2026-08-30.
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

(def ^:const statutory-keepout-radius-factor
  "Radius of the statutory keep-out circle, as a multiple of tree height.

   NOT soma's number: 労働安全衛生規則 第481条第2項 requires that other workers be
   kept outside \"当該立木の高さの二倍に相当する距離を半径とする円形\" — a circle of
   radius 2x the tree's height, centred on the stem being felled. It is a CIRCLE,
   so `fall-zone-half-angle-deg` does not apply to it, and it is 2.0, not the 1.5
   this file used for its own sector.

   Cited in data/authorities.edn :anei-481-2 (e-Gov law 347M50002000032)."
  2.0)

(defn person?
  "True iff this exclusion point is a person. 第481条第2項 protects 他の作業従事者 —
   people — so the statutory circle applies to these and not to a road or a
   watercourse, for which the directional fall sector is the right model."
  [ex]
  (= :human (:kind ex)))

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

(defn keepout-radius-m
  "Statutory keep-out radius (m) for a tree — 2x its height (第481条第2項)."
  [tree]
  (* statutory-keepout-radius-factor (:height-m tree)))

(defn in-keepout-circle?
  "True iff `ex-coord` lies inside the statutory keep-out circle of a tree of
   `height-m` standing at `tree-coord`. A CIRCLE — no azimuth is consulted,
   because 第481条第2項 does not consult one."
  [tree-coord height-m ex-coord]
  (<= (dist tree-coord ex-coord) (* statutory-keepout-radius-factor height-m)))

(defn keepout-intrusions
  "Every PERSON inside the statutory keep-out circle (第481条第2項).

   Independent of the fall azimuth on purpose. The predecessor of this function
   was `fall-zone-intrusions` alone, which asked only whether a person was within
   +/-35deg of the predicted fall line out to 1.5x height — so a worker standing
   off the line but well inside the statutory circle read as clear. Measured on
   data/stand.edn: tree t-3 (h=27m, circle 54m) with x-crew at 36.1m passed for
   59 of 72 aim azimuths."
  [tree exclusions]
  (filter #(and (person? %)
                (in-keepout-circle? (:coord tree) (:height-m tree) (:coord %)))
          exclusions))

;; ── the planning gate ─────────────────────────────────────────────────────────
(defn protected?
  "True iff the tree is constitutionally un-fellable: a protected species or a
   no-cut flag (old-growth / seed-tree). Per ADR-2606142010 G7."
  [tree]
  (boolean (or (:protected tree) (:no-cut tree))))

(defn safe-fell?
  "True iff felling `tree` along `fall-az` is safe AND permitted:
     * G7 — the tree is not protected / not no-cut;
     * G5 — no person inside the statutory 2x-height keep-out CIRCLE
       (労働安全衛生規則 第481条第2項), and
     * G5 — the directional fall SECTOR contains no exclusion point of any kind.
   The circle is not implied by the sector: it is bigger (2x vs 1.5x height) and
   it ignores direction, so it refuses fells the sector alone allows.
   Pure predicate; never throws (use `plan-fell` for the raising variant)."
  [tree fall-az exclusions]
  (and (not (protected? tree))
       (empty? (keepout-intrusions tree exclusions))
       (empty? (fall-zone-intrusions tree fall-az exclusions))))

(defn plan-fell
  "Plan a directional fell of `tree` aimed at `aim-az`, against the stand's wind
   and `exclusions`. Returns a plan map {:tree :fall-az :hinge-m :fall-zone-r
   :keepout-r :exclusions-clear}. RAISES (ex-info) when the tree is protected
   (G7), when a person is inside the statutory 2x-height keep-out circle
   (G5, 労働安全衛生規則 第481条第2項), or when the fall zone overlaps any
   exclusion point (G5) — an unsafe or forbidden fell must SURFACE, never be
   silently planned. Felling is the #1 logging hazard."
  [tree aim-az exclusions]
  (when (protected? tree)
    (throw (ex-info "tree is protected / no-cut — felling refused (G7)"
                    {:tree (:id tree) :protected (:protected tree) :no-cut (:no-cut tree)})))
  ;; The statutory circle is checked BEFORE the fall line is even predicted:
  ;; 第481条第2項 does not depend on where the tree is aimed, so no choice of aim
  ;; can clear it. Refusing here says so — re-aiming is not a remedy.
  (let [in-circle (keepout-intrusions tree exclusions)]
    (when (seq in-circle)
      (throw (ex-info (str "a person is inside the statutory keep-out circle "
                           "(2x tree height) — felling refused (G5, 労働安全衛生規則 第481条第2項)")
                      {:tree (:id tree)
                       :keepout-r (keepout-radius-m tree)
                       :persons (mapv :id in-circle)
                       :authority :anei-481-2}))))
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
     :keepout-r (keepout-radius-m tree)
     :exclusions-clear true}))
