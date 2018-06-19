(ns metaprob.prelude-test
  (:require [clojure.test :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.builtin-impl :refer :all])
  (:require [metaprob.builtin :as builtin])
  (:require [metaprob.prelude :as prelude]))

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

(deftest reverse-1
  (testing "reverse tests"
    (is (builtin/immutable-trace? (list)))
    (let [tr (prelude/reverse (list))]
      (is (builtin/immutable-trace? tr))
      (is (= tr (list))))
    (let [tr (prelude/reverse (list 1))]
      (is (builtin/immutable-trace? tr))
      (is (= tr (list 1))))
    (let [tr (prelude/reverse (list 1 2 3))]
      (is (builtin/immutable-trace? tr))
      (is (= tr (list 3 2 1))))))

(deftest reverse-2
  (testing "reverse mutability tests"
    (let [tr (prelude/reverse (builtin/to-seq (list 1 2 3)))]
      (is (builtin/immutable-trace? tr))
      (is (= tr (list 3 2 1))))
    ;; This is a problem.
    (let [tr (prelude/reverse (trace-copy (list 1 2 3)))]
      (is (builtin/mutable-trace? tr))
      (is (= (builtin/first tr) 1)))))

(deftest map-1
  (testing "map smoke test"
    (is (builtin/nth (prelude/map (gen [x] (builtin/add x 1))
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
    (let [foo (prelude/map (fn [x] (+ x 1))
                           (builtin/list 6 7 8))]
      (is (builtin/length foo) 3)
      (is (= (builtin/nth foo 0) 7))
      (is (= (builtin/nth foo 1) 8))
      (is (= (builtin/nth foo 2) 9)))))

(deftest map-2
  (testing "Map over a metaprob list"
    (is (= (builtin/first
            (builtin/rest
             (prelude/map (fn [x] (+ x 1))
                          (builtin/pair 6 (builtin/pair 7 (builtin/pair 8 (builtin/empty-trace)))))))
           8))))

(deftest map-3
  (testing "Map over a metaprob tuple"
    (is (= (builtin/trace-get (prelude/map (fn [x] (+ x 1))
                                           (builtin/tuple 6 7 8))
                              '(1))
           8))))

(deftest name-1
  (testing "see if a procedure has the right name"
    (is (.contains (procedure-name prelude/drop) "drop"))))

