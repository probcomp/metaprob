;; This file was automatically generated

(ns metaprob.examples.coin-ex2.example
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  flip_coins
  constrain_coin_flipper_trace
  extract_weight
  analytic_sampler)

(define
  flip_coins
  (program
    [n]
    (define weight (beta 2 2))
    (replicate n (program [] (flip weight)))))

(define
  constrain_coin_flipper_trace
  (program
    [n]
    (define t1 (mk_nil))
    (for_each
      (range n)
      (program
        [k]
        (block
          (trace_set
            (lookup
              t1
              (list
                1
                (prob_prog_name replicate)
                k
                (lookup flip_coins (list "source" "body" 1 2))
                "flip"))
            true))))
    t1))

(define
  extract_weight
  (program [state] (trace_get (lookup state (list 0 "weight" "beta")))))

(define analytic_sampler (program [] (beta 7 2)))

