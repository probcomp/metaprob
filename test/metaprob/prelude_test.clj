(ns metaprob.prelude-test
  (:require [clojure.test :refer :all :exclude [function?]]
            [metaprob.syntax :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :as prelude])
  (:refer-clojure :exclude [assoc dissoc]))

(deftest smoke-1
  (testing "Prelude smoke test"
    ;; These tests have to run after the call to sp
    (is (= (ns-resolve 'metaprob.prelude 'v) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'metaprob.prelude) 'v))
        "namespacing sanity check 2")))

;; ------------------------------------------------------------------

(def this-map prelude/map)

(deftest map-1
  (testing "map smoke test"
    (is (nth (this-map (gen [x] (+ x 1))
                               (list 4 5 6))
                     1)
        6)
    ;; These tests have to run after the call to map
    (is (= (ns-resolve 'metaprob.prelude 'value) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'metaprob.prelude) 'value))
        "namespacing sanity check 2")))

;; I'm sort of tired of this and don't anticipate problems, so
;; not putting more work into tests at this time.

(deftest map-1a
  (testing "Map over a clojure list"
    (let [start (list 6 7 8)
          foo (this-map (fn [x] (+ x 1))
                        start)]
      (is (count foo) 3)
      (is (= (nth foo 0) 7))
      (is (= (nth foo 1) 8))
      (is (= (nth foo 2) 9)))))

(deftest map-2
  (testing "Map over a different list"
    (is (= (first
            (rest
             (this-map (fn [x] (+ x 1))
                       (list 6 7 8))))
           8))))
