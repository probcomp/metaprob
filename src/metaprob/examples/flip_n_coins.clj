(ns metaprob.examples.flip-n-coins
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.interpreters :refer :all]))

;; Define a probabilistic model for n flips of a coin
;; with a custom address name for each coin flip

(define flip-n-coins
  (gen [n]
    (define tricky (flip 0.1))    ;unlikely to be true
    (define weight (if tricky (uniform 0 1) 0.5))
    (define datum (map (gen [i] (flip weight))
                       (range n)))
    datum))

(define coin-flips-demo-n-flips
  (gen [n]
    (define [_ trace-with-n-flips _]
      (infer :procedure flip-n-coins
             :inputs [n]))
    (pprint trace-with-n-flips)
    (infer :procedure flip-n-coins
           :inputs [n]
           :target-trace trace-with-n-flips
           :output-trace? false)))
    ;;  => value:score:

;; make a partial trace that intervenes on flip-coins
;; to ensure the coin is tricky and the weight is 0.99
;; but the fourth flip comes up false

(define ensure-tricky-and-biased
  {0 {"tricky" {"flip" {:value true}}},
   1 {"weight" {"then" {"uniform" {:value 0.99}}}},
   2 {"datum"  {"map"  {3  {"flip" {:value false}}}}}})

(define coin-flips-demo-biased
  (gen [n]
    (print "--ensure-tricky-and-biased intervention trace--")
    (pprint ensure-tricky-and-biased)

    ;; run  the  program  subject  to  these  interventions
    (define [value output _]
      (infer :procedure flip-n-coins
             :inputs [n]
             :intervention-trace ensure-tricky-and-biased))
    (pprint value)
    (print "--output trace--")
    (pprint output)))
    ;;  => (true true true false true true true true true true)
