;; This file was automatically generated

(ns metaprob.examples.coin-ex4.example
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare flip_coins constrain_coin_flipper_trace extract_weight)

(define
  flip_coins
  (program
    [n]
    (define tricky (flip 0.5))
    (define
      weight
      (if tricky (block (beta 3 3)) (block (exactly 0.5))))
    (replicate n (program [] (flip weight)))))

(define
  constrain_coin_flipper_trace
  (program
    [n]
    (define t1 (mk_nil))
    (define coin_trace (lookup t1 (list 2 2)))
    (for_each
      (range n)
      (program
        [k]
        (block
          (trace_set
            (lookup
              t1
              (list
                2
                (prob_prog_name replicate)
                k
                (lookup flip_coins (list "source" "body" 2 2))
                "flip"))
            (if (lt k 3) true false)))))
    t1))

(define
  extract_weight
  (program
    [state]
    (define site1 (list 1 "weight" "then" 0 "beta"))
    (if (trace_has (lookup state site1))
      (trace_get (lookup state site1))
      (block
        (define site2 (list 1 "weight" "else" 0 "exactly"))
        (trace_get (lookup state site2))))))

