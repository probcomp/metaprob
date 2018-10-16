(ns metaprob.mem-test
  (:require [clojure.test :refer :all]
            [metaprob.trace :refer :all]
            [metaprob.sequence :refer :all]
            [metaprob.syntax :refer :all]
            [metaprob.builtin-impl :as impl]
            [metaprob.distributions :refer :all]
            [metaprob.interpreters :refer :all]
            [metaprob.mem :refer :all]))

;; ---- Null case: deterministic function

(deftest mem-value
  (testing "see whether memoized function gives the right values"
    (define fun (gen [x] (+ x 2)))
    (is (= (with-memoized fun
             (gen [mfun]
               [(mfun 1) (mfun 4) (mfun 1)]))
           [3 6 3]))))

;; ---- Deterministic function, check to make sure values get cached

(define detector (mutable-trace))
(define fun
  (gen [x]
    (trace-set! detector (+ (trace-get detector) 1))
    (+ x 2)))

(deftest mem-cache-value
  (testing "see whether memoized function caches the computation"
    (trace-set! detector 0)
    (is (= (with-memoized fun
             (gen [mfun]
               [(mfun 1) (mfun 4) (mfun 1) (mfun 4)]))
           [3 6 3 6]))
    (is (= (trace-get detector) 2))))

;; ---- Nondeterministic function interpreted using infer

(define unit-uniform (gen [arg] (uniform 0 1)))
(define doit
  (gen []
    (with-memoized unit-uniform
      (gen [uu]
        [(uu 1) (uu 1) (uu 1) (uu 2)]))))

(deftest mem-cache-value-2
  (testing "basic test of mem functionality, calling infer"
    (define [result _ _]
      (infer :procedure doit
             :inputs []
             ;; Force use of meta-circular interpreter
             :output-trace? true))
    (is (= (nth result 0) (nth result 1)))
    (is (not (= (nth result 2) (nth result 3))))))

;; If there are four samples, we expect the score to be... on average

(deftest mem-cache-score
  (testing "basic test of mem functionality, calling infer, checking score"
    (define [result output score]
      (infer :procedure doit
             :inputs []
             ;; Force use of meta-circular interpreter
             :output-trace? true))
    (is (> score -5))                   ;Very occasional test failures
    (define cache (trace-subtrace output (list "with-memoized" traces-stored-under-key)))
    (is (= (trace-count cache) 2))
    (is (> (trace-get cache (list 2 "uniform")) 0))
    (metaprob-pprint output)))

;; Test ability to intervene

(deftest mem-intervene
  (testing "can we intervene on a memoized function?"
    (define cache-before (trace 1 (** (trace "uniform" 0.4))))
    (define intervene
      (trace "with-memoized"
             (** (trace traces-stored-under-key
                        (** cache-before)))))
             
    (define [result output score]
      (infer :procedure doit
             :inputs []
             :intervention-trace intervene))

    (define cache-after (trace-subtrace output (list "with-memoized" traces-stored-under-key)))

    (is (= (trace-get cache-after (list 1 "uniform"))
           (trace-get cache-before (list 1 "uniform"))))))

