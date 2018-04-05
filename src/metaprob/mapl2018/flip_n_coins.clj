(ns metaprob.mapl2018.flip-n-coins
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.mapl2018.interpreters :refer :all]))

;; Define a probabilistic model for n flips of a coin
;; with a custom address name for each coin flip

(define flip-n-coins
  (gen [n] 
    (define root-addr (&this))
    (define tricky (flip 0.1))
    (define weight (if tricky (uniform 0 1) 0.5))
    (map (gen [i] (with-address (pair root-addr (addr "datum" i)) ;foo
                    (flip weight)))
         (range n))))

;; make a partial  trace  that  intervenes  on flip -coins
;; to  ensure  the  coin is  tricky  and  the  weight  is 0.99
;; but  the  fourth  flip  comes  up  false

(define ensure-tricky-and-biased (empty-trace))
(trace-set ensure-tricky-and-biased (addr 1 "tricky" "flip") true)
(trace-set ensure-tricky-and-biased (addr 2 "weight" "then" 0 "uniform") 0.99)
(trace-set ensure-tricky-and-biased (addr "datum" 3 "flip") false)

(define run
  (gen []
    (define trace-with-2-flips (empty-trace))
    (infer :procedure flip-n-coins :inputs (tuple 2)
           :output-trace trace-with-2-flips)
    (pprint trace-with-2-flips)
    ;; (*@\textit{=> ( ... )}@*)

    (pprint (infer :procedure flip-n-coins :inputs (tuple 2)
                   :target-trace  trace-with-2-flips))
    ;;  => value:score:

    (print "--ensure-tricky-and-biased--")
    (pprint ensure-tricky-and-biased)

    (define output (empty-trace))
    ;; run  the  program  subject  to  these  interventions
    (pprint (infer :procedure flip-n-coins :inputs (tuple 10)
                   :intervention-trace ensure-tricky-and-biased
                   :output-trace output))
    (print "--output--")
    (pprint output)
    ;;  => (true  true  true  false  true  true  true  true  true  true)
    ))
