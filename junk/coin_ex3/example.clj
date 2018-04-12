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
  (gen
    [n]
    (define root_addr this)
    (define tricky (flip 0.1))
    (define weight (if tricky (block (uniform 0 1)) (block 0.5)))
    (map
      (gen
        [i]
        (with-address (addr root_addr "datum" i) (flip weight)))
      (range n))))

(define constrain_coin_flipper_trace
  (gen [n]
    (define t1 (empty-trace))
    (for_each
      (range n)
      (gen [k]
        (block (trace-set t1 (addr "datum" k "flip") true))))
    t1))

(define extract_weight
  (gen [state]
    (define site1 (addr 2 "weight" "then" 0 "uniform"))
    (if (trace-has? (lookup state site1))
      (trace-get state site1)
      (block
        (define site2 (addr "native-generate" "body" 2 "weight" "else" 0))
        (trace_get (lookup flip_coins site2) "value")))))

(define transcript1
  (gen [do_print]
    (define a_trace (empty-trace))
    (trace_choices flip_coins (tuple 5) (empty-trace) a_trace)
    (if do_print
      (block
        (pprint a_trace)
        (print (trace_get a_trace (addr 1 "tricky" "flip"))))
      "ok")
    a_trace))

(define transcript2
  (gen []
    (define a_trace (transcript1 false))
    (trace-set a_trace (addr "datum" 0 "flip") true)
    (trace-set a_trace (addr "datum" 1 "flip") true)
    (trace-set a_trace (addr "datum" 2 "flip") true)
    (trace-set a_trace (addr "datum" 3 "flip") true)
    (trace-set a_trace (addr "datum" 4 "flip") true)
    (define approximate_inference_update
      (gen []
        (single_site_metropolis_hastings_step
            flip_coins
            (tuple 5)
            a_trace
            (set-difference
              (addresses_of a_trace)
              (tuple
                (addr 1 "tricky" "flip")
                (addr 2 "weight" "then" 0 "uniform"))))))
    (repeat 20 approximate_inference_update)
    (print (trace-get a_trace (addr 1 "tricky" "flip")))))

