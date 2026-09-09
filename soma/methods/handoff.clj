;; soma 杣 — cross-actor handoff edges in the kotoba Datom log.
;;
;; The forest stand is a SEAM in the timber chain, not an island:
;;     soma 杣 (fell + buck graded assortments) → tatekata 建方 (construction)
;; soma produces graded cut-to-length timber assortments; tatekata consumes structural
;; lumber. This module makes that chain queryable as append-only `:handoff/*` 縁 — the
;; R0→R1 integration step that lets one actor's output (a bucked-log assortment) become
;; the next actor's input (a lumber-supply intent) on the shared Datom log, with source
;; provenance on every edge.
;;
;;   * outbound — a bucked-log/assortment record {:log-id :grade :length-m :volume-m3}
;;                becomes a tatekata lumber-supply intent (grade + volume on the payload);
;;   * a provenance gate — every handoff must name both a source and a destination actor;
;;     an orphan handoff RAISES (no unattributed chain edge).
;;
;; Pure Clojure, no deps → babashka-runnable AND kotoba-pywasm-portable.
;; Per ADR-2606142010 (soma R0→R1 integration; sibling of kuramori ADR-2606142000).
(ns soma.methods.handoff
  (:require [clojure.string :as str]))

(defn assert-handoff!
  "RAISE if a handoff lacks a source or destination actor (no unattributed chain
   edge — provenance is structural)."
  [{:keys [from-actor to-actor] :as h}]
  (when (or (str/blank? (str from-actor)) (str/blank? (str to-actor)))
    (throw (ex-info "handoff missing source/destination actor" {:handoff h})))
  h)

(defn supply-handoff
  "Map a bucked-log/assortment record → a tatekata 建方 lumber-supply intent.
   Each assortment: {:log-id :grade :length-m :volume-m3}. Source actor = soma,
   destination actor = tatekata."
  [a]
  (assert-handoff!
   {:id (str "sup." (:log-id a))
    :from-actor "soma" :to-actor "tatekata" :kind :timber-supply
    :payload {:log-id (:log-id a) :grade (:grade a)
              :length-m (:length-m a) :volume-m3 (:volume-m3 a)}}))

(defn outbound-handoff
  "Map a sequence of bucked-log assortments → tatekata lumber-supply handoffs."
  [assortments]
  (mapv supply-handoff assortments))

(defn- fmt [v]
  (cond (keyword? v) (str v)
        (string? v) (if (str/starts-with? v ":") v (str \" v \"))
        :else (str v)))

(defn emit
  "EAVT handoff datoms (GROUND :add). Each handoff is an edge entity
   en.handoff.<from>.<to>.<id> carrying actor provenance + payload kind."
  [handoffs tx]
  (let [lines
        (mapcat
         (fn [h]
           (let [eid (str "en.handoff." (:from-actor h) "." (:to-actor h) "." (:id h))]
             [(str "[" (fmt eid) " :handoff/from-actor " (fmt (:from-actor h)) " " tx " :add]")
              (str "[" (fmt eid) " :handoff/to-actor " (fmt (:to-actor h)) " " tx " :add]")
              (str "[" (fmt eid) " :handoff/kind " (fmt (:kind h)) " " tx " :add]")]))
         handoffs)]
    (str ";; soma 杣 — GENERATED handoff 縁 (chain: soma→tatekata timber supply). ADR-2606142010.\n"
         "[\n" (str/join "\n" lines) "\n]\n")))

(defn -main [& _]
  (let [assortments [{:log-id "l1" :grade :sawlog :length-m 5.0 :volume-m3 0.42}
                     {:log-id "l2" :grade :sawlog :length-m 4.0 :volume-m3 0.31}]
        out (outbound-handoff assortments)]
    (print (emit out 1))
    (flush)))
