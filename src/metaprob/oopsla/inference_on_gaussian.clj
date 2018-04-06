;; 5.

(ns metaprob.oopsla.inference-on-gaussian
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.oopsla.gaussian :refer [gaussian score-gaussian two-variable-gaussian-model]]
            [metaprob.oopsla.rejection :refer [rejection-sampling]]
            [metaprob.oopsla.importance :refer [importance-resampling]]
            [metaprob.oopsla.metropolis-hastings-step :refer [lightweight-single-site-MH-sampling]]
            [metaprob.oopsla.interpreters :refer :all]))

(define number-of-runs 3)

(define prior-density
  (gen [x]
    (exp (score-gaussian x (tuple 0 1)))))

(define target-density
  (gen [x]
    (exp (score-gaussian x (tuple 1.5 (div 1.0 (sqrt 2.0)))))))

(define get-samples
  (gen [number-of-runs]
    (binned-histogram
      :name    "samples from the prior"
      :samples (replicate number-of-runs
                          two-variable-gaussian-model)
      :overlay-densities (list (tuple "prior" prior-density)
                               (tuple "target" target-density)))))

(define target-trace (empty-trace))
(trace-set target-trace (addr 1 "y" "gaussian") 3.0)

(define rejection-assay
  (gen [number-of-runs]
    (binned-histogram
      :name    "samples from the target"
      :samples (replicate
                 number-of-runs
                 (gen []
                   (print "rejection sample")
                   (define tr
                     (rejection-sampling
                      two-variable-gaussian-model  ; :model-procedure 
                      (tuple)  ; :inputs 
                      target-trace  ; :target-trace 
                      0.5))   ; :log-bound 
                   (trace-get tr (addr 0 "x" "gaussian"))))
      :overlay-densities (list (tuple "prior" prior-density)
                               (tuple "target" target-density)))))

(define importance-assay
  (gen [number-of-runs]
    (binned-histogram
      :name    "samples from importance sampling with 20 particles"
      :samples (replicate
                 number-of-runs
                 (gen []
                   (define tr
                     (importance-resampling
                      two-variable-gaussian-model  ; :model-procedure 
                      (tuple)  ; :inputs 
                      target-trace  ; :target-trace 
                      20))
                   (trace-get tr (addr 0 "x" "gaussian"))))  ; :N 
      :overlay-densities (list (tuple "prior" prior-density)
                               (tuple "target" target-density)))))

(define MH-assay
  (gen [number-of-runs]
    (binned-histogram
      :name    "samples from lightweight single-site MH with 20 iterations"
      :samples (replicate
                 number-of-runs
                 (gen []    ;added by JAR
                   (define tr
                     (lightweight-single-site-MH-sampling 20
                                                          two-variable-gaussian-model
                                                          target-trace))
                   (trace-get tr (addr 0 "x" "gaussian"))))
      :overlay-densities (list (tuple "prior" prior-density)
                               (tuple "target" target-density)))))
