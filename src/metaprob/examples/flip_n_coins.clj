(ns metaprob.examples.flip-n-coins
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.infer :refer [map]]
            [metaprob.interpreters :refer :all]))

;; Define a probabilistic model for n flips of a coin
;; with a custom address name for each coin flip

(define flip-n-coins
  (gen [n] 
    (define root-addr (&this))
    (define tricky (flip 0.1))    ;unlikely to be true
    (define weight (if tricky (uniform 0 1) 0.5))
    (define datum (map (gen [i] (flip weight))
                       (range n)))
    datum))

(define coin-flips-demo-n-flips
  (gen [n]
    (define trace-with-n-flips (empty-trace))
    (infer :procedure flip-n-coins
           :inputs [n]
           :output-trace trace-with-n-flips)
    (pprint trace-with-n-flips)
    ;; (*@\textit{=> ( ... )}@*)

    (pprint (infer :procedure flip-n-coins
                   :inputs [n]
                   :target-trace trace-with-n-flips))
    ;;  => value:score:
    ))

;; make a partial trace that intervenes on flip-coins
;; to ensure the coin is tricky and the weight is 0.99
;; but the fourth flip comes up false

(define ensure-tricky-and-biased (empty-trace))
(trace-set! ensure-tricky-and-biased (addr 1 "tricky" "flip") true)
(trace-set! ensure-tricky-and-biased (addr 2 "weight" "then" "uniform") 0.99)
(trace-set! ensure-tricky-and-biased (addr "datum" 3 "flip") false)

(define coin-flips-demo-biased
  (gen [n]

    (print "--ensure-tricky-and-biased intervention trace--")
    (pprint ensure-tricky-and-biased)

    (define output (empty-trace))
    ;; run  the  program  subject  to  these  interventions
    (pprint (infer :procedure flip-n-coins
                   :inputs [n]
                   :intervention-trace ensure-tricky-and-biased
                   :output-trace output))
    (print "--output trace--")
    (pprint output)
    ;;  => (true true true false true true true true true true)
    ))
