;; This file was automatically generated, then edited

(clojure.core/ns metaprob.examples.coin-ex3.example
  (:refer-clojure :only [declare ])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            ;; Added manually
            [metaprob.metacirc.trace-choices :refer [trace_choices]]
            [metaprob.lightweight :refer [single_site_metropolis_hastings_step]]))

(declare
  flip_coins
  constrain_coin_flipper_trace
  extract_weight
  draw_plots
  transcript1
  transcript2)

(define
  flip_coins
  (program
    [n]
    (define root_addr this)
    (print "yow")
    (define tricky (flip 0.1))
    (pprint tricky)
    (define weight (if tricky (block (uniform 0 1)) (block 0.5)))
    (pprint weight)
    (map
      (program
        [i]
        (with-address (list root_addr "datum" i) (flip weight)))
      (range n))))

(define
  constrain_coin_flipper_trace
  (program
    [n]
    (define t1 (mk_nil))
    (for_each
      (range n)
      (program
        [k]
        (block (trace_set (lookup t1 (list "datum" k "flip")) true))))
    t1))

(define
  extract_weight
  (program
    [state]
    (define site1 (list 2 "weight" "then" 0 "uniform"))
    (if (trace_has (lookup state site1))
      (block (trace_get (lookup state site1)))
      (block
        (define site2 (list "source" "body" 2 "weight" "else" 0))
        (trace_get
          (lookup (lookup flip_coins site2) (list "value")))))))

(define
  transcript1
  (program
    [do_print]
    (define a_trace (mk_nil))
    (trace_choices flip_coins (tuple 5) (mk_nil) a_trace)
    (if do_print
      (block
        (pprint a_trace)
        (print (trace_get (lookup a_trace (list 1 "tricky" "flip")))))
      "ok")
    a_trace))

(define
  transcript2
  (program
    []
    (define a_trace (transcript1 false))
    (trace_set (lookup a_trace (list "datum" 0 "flip")) true)
    (trace_set (lookup a_trace (list "datum" 1 "flip")) true)
    (trace_set (lookup a_trace (list "datum" 2 "flip")) true)
    (trace_set (lookup a_trace (list "datum" 3 "flip")) true)
    (trace_set (lookup a_trace (list "datum" 4 "flip")) true)
    (define
      approximate_inference_update
      (program
        []
        (block
          (single_site_metropolis_hastings_step
            flip_coins
            (tuple 5)
            a_trace
            (set_difference
              (addresses_of a_trace)
              (tuple
                (list 1 "tricky" "flip")
                (list 2 "weight" "then" 0 "uniform")))))))
    (repeat 20 approximate_inference_update)
    (print (trace_get (lookup a_trace (list 1 "tricky" "flip"))))))

