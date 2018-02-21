;; 3.

(ns metaprob.mapl2018.importance
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.mapl2018.interpreters :refer :all]))

(define importance-resampling
  (probprog [model-probprog inputs target-trace N]

    ;; generate N candidate traces, called particles, each
    ;; with a score
    
    (define particles
    	    (replicate N
	      (probprog []
                (define candidate-trace (empty-trace))
                (propose-and-trace-choices
                  :probprog model-probprog
                  :inputs inputs
                  :intervention-trace (empty-trace)
	          :target-trace       target-trace
	          :output-trace       candidate-trace))))
    (define traces 
      (map (probprog [p] (trace_get (lookup p (addr 0)))) particles))
    (define scores
      (map (probprog [p] (trace_get (lookup p (addr 1)))) particles))

    ;; return a trace with probability proportional to (exp score)
    (log_categorical scores traces)))
