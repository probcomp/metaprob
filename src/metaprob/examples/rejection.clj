;; 4.

(ns metaprob.examples.rejection
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]
            [metaprob.examples.interpreters :refer :all]))

(define rejection-sampling
  (gen [model-procedure inputs target-trace log-bound]
    (define candidate-trace (empty-trace))
    (define
      [value score]
      (infer :procedure model-procedure
             :inputs inputs
             :intervention-trace (empty-trace)
             :target-trace       target-trace
             :output-trace       candidate-trace))
    (if (lt (log (uniform 0 1)) (sub score log-bound))
      candidate-trace
      (rejection-sampling
	model-procedure inputs target-trace log-bound)) ))
