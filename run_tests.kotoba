#!/usr/bin/env bb
;; soma — bb-native test runner (Clojure / babashka; no shell). Auto-generated
;; (repo-wide rule, root CLAUDE.md §"Operational code = clj/bb"): first-party tooling is
;; clj/bb, not shell. Discovers every test_*.{cljc,clj} namespace already authored under
;; this actor's tree and runs it via clojure.test — this actor had test suites but no
;; runner wired up (vitals reflex was reading as :absent).
;;
;;   bb run_tests.clj
(require '[babashka.classpath :as cp]
         '[babashka.fs :as fs]
         '[clojure.test :as t])

;; The standalone repository root is the namespace classpath root.
(cp/add-classpath (str (fs/parent (fs/absolutize *file*))))

(def suites
  '[soma.methods.test-soma])

(apply require suites)

(let [{:keys [fail error]} (apply t/run-tests suites)]
  (if (zero? (+ fail error))
    (println "-- soma: ALL suites green --")
    (do (println "-- soma: FAILURES above --")
        (System/exit 1))))
