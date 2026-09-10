#!/usr/bin/env bb
;; soma 杣 — verify data/authorities.edn against the live e-Gov Laws API.
;;
;;   bb bin/verify_authorities.clj
;;
;; Exit codes are three-valued ON PURPOSE:
;;
;;   0  every citation resolved AND its quoted provision was found in the payload
;;   1  a citation is WRONG — 200 but the quote is not in the document, or non-200
;;   2  UNDETERMINED — the network/API could not be reached, so nothing was checked
;;
;; 2 exists because a verifier that returns "fine" when it could not look is the
;; failure this repo's citations are meant to prevent. "Not checked" must not be
;; spelled the same way as "checked and clean".
;;
;; This is deliberately NOT part of `bb run_tests.clj`: the suite must stay green
;; offline. The suite checks the corpus's SHAPE (see test_soma.clj); this checks
;; that the shape still corresponds to the world.
(require '[clojure.edn :as edn]
         '[clojure.string :as str]
         '[babashka.process :as p])

(def ^:private corpus (edn/read-string (slurp "data/authorities.edn")))

(defn- fetch
  "GET `u` into a temp file. Returns {:status \"200\" :body \"…\"} or nil when the
   request could not be made at all (which is UNDETERMINED, not a failure)."
  [u]
  (let [f (str "/tmp/soma-cite-" (Math/abs (hash u)) ".xml")
        r (try (p/shell {:out :string :err :string :continue true}
                        "curl" "-sS" "-o" f "-w" "%{http_code}" "--max-time" "90" u)
               (catch Exception _ nil))]
    (when (and r (zero? (:exit r)) (.exists (java.io.File. f)))
      {:status (str/trim (:out r)) :body (slurp f)})))

(defn- doc-text
  "All character data of the XML with markup removed, then whitespace collapsed.

   Stripping tags is the whole point: e-Gov splits a provision across <Sentence>
   elements, so a two-sentence quote is NOT a substring of the raw markup even
   though it IS the provision. Matching against the raw XML produced a false
   NEGATIVE on 第477条第1項第三号 while the citation was correct."
  [xml]
  (-> xml (str/replace #"<[^>]*>" "") (str/replace #"\s+" "")))

(defn- normalise
  "Collapse whitespace and drop a leading item/paragraph marker (三, ２, …) — the
   markers live in the markup structure, not in the sentence text."
  [q]
  (-> q (str/replace #"\s+" "")
        (str/replace #"^[一二三四五六七八九十０-９２]+" "")))

(let [cache (atom {})
      rows  (doall
             (for [a (:corpus/authorities corpus)]
               (let [u (:authority/source-url a)
                     r (or (@cache u) (let [v (fetch u)] (swap! cache assoc u v) v))]
                 (cond
                   (nil? r)                {:id (:authority/id a) :verdict :unreachable}
                   (not= "200" (:status r)) {:id (:authority/id a) :verdict :bad-status
                                             :status (:status r)}
                   :else
                   {:id (:authority/id a)
                    :verdict (if (str/includes? (doc-text (:body r))
                                                (normalise (:authority/quote a)))
                               :ok :quote-absent)}))))]
  (doseq [{:keys [id verdict status]} rows]
    (println (format "%-16s %s%s" (name id) (name verdict) (if status (str " " status) ""))))
  (println)
  (let [n (count rows)
        ok (count (filter #(= :ok (:verdict %)) rows))
        unreachable (count (filter #(= :unreachable (:verdict %)) rows))]
    (println (format "%d/%d citations verified against the live source" ok n))
    (cond
      (pos? unreachable)
      (do (println (format "UNDETERMINED — %d citation(s) could not be fetched; nothing was verified."
                           unreachable))
          (System/exit 2))
      (= ok n) (do (println "OK") (System/exit 0))
      :else    (do (println "FAILED — a citation does not match its source.") (System/exit 1)))))
