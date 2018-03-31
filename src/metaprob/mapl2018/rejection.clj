;; 4.

(ns metaprob.mapl2018.rejection
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.mapl2018.interpreters :refer :all]))

(define rejection-sampling
  (gen [model-procedure inputs target-trace log-bound]
    (define candidate-trace (empty-trace))
    (define
      [_ score]
      (query
        :procedure model-procedure
        :inputs inputs
        :intervention-trace (empty-trace)
	:target-trace       target-trace
	:output-trace       candidate-trace))
    (if (lt (log (uniform 0 1)) (sub score log-bound))
      candidate-trace
      (rejection-sampling
	model-procedure inputs target-trace log-bound)) ))
