(ns metaprob.prelude-test
  (:require [clojure.test :refer :all :exclude [function?]]
            [metaprob.syntax :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.builtin-impl :refer :all]
            [metaprob.builtin :as builtin]
            [metaprob.prelude :as prelude])
  (:refer-clojure :exclude [assoc dissoc]))

;;  (:refer metaprob.builtin
;;          :exclude [map reverse zipmap iterate concat drop replicate filter repeat])
;;  (:refer metaprob.prelude
;;          :exclude [not assert pprint and or
;;                    list first rest last nth range])

(deftest smoke-1
  (testing "Prelude smoke test"
    ;; These tests have to run after the call to sp
    (is (= (ns-resolve 'metaprob.prelude 'v) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'metaprob.prelude) 'v))
        "namespacing sanity check 2")))

(deftest name-1
  (testing "see if a procedure has the right name"
    (is (.contains (procedure-name prelude/opaque) "opaque"))))

(deftest apply-1
  (testing "apply smoke test"
    (is (= (prelude/apply builtin/- [3 2]) 1))
    (is (= (prelude/apply builtin/- (list 3 2)) 1))
    (is (= (prelude/apply prelude/apply (list builtin/- (list 3 2))) 1))))

;; ------------------------------------------------------------------

(def this-map prelude/map)

(deftest map-1
  (testing "map smoke test"
    (is (builtin/nth (this-map (gen [x] (builtin/+ x 1))
                               (builtin/list 4 5 6))
                     1)
        6)
    ;; These tests have to run after the call to map
    (is (= (ns-resolve 'metaprob.prelude 'val) nil)
        "namespacing sanity check 1")
    (is (not (contains? (ns-publics 'metaprob.prelude) 'val))
        "namespacing sanity check 2")))

;; I'm sort of tired of this and don't anticipate problems, so
;; not putting more work into tests at this time.

(deftest map-1a
  (testing "Map over a clojure list"
    (let [start (builtin/list 6 7 8)
          foo (this-map (fn [x] (+ x 1))
                        start)]
      (is (builtin/count foo) 3)
      (is (= (builtin/nth foo 0) 7))
      (is (= (builtin/nth foo 1) 8))
      (is (= (builtin/nth foo 2) 9)))))

(deftest map-2
  (testing "Map over a different list"
    (is (= (builtin/first
            (builtin/rest
             (this-map (fn [x] (+ x 1))
                       (builtin/list 6 7 8))))
           8))))
