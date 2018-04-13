;; 5.


(ns metaprob.examples.inference-on-gaussian
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all])
  (:require [metaprob.builtin :refer :all])
  (:require [metaprob.prelude :refer :all])
  (:require [metaprob.infer :refer :all])
  (:require [metaprob.examples.gaussian :refer [gaussian score-gaussian two-variable-gaussian-model]])
  (:require [metaprob.examples.rejection :refer [rejection-sampling]])
  (:require [metaprob.examples.importance :refer [importance-resampling]])
  (:require [metaprob.examples.metropolis-hastings-step :refer [lightweight-single-site-MH-sampling]])
  (:require [metaprob.examples.interpreters :refer :all]))

(define prior-density
  (gen [x]
    (exp (score-gaussian x (tuple 0 1)))))

(define target-density
  (gen [x]
    (exp (score-gaussian x (tuple 1.5 (div 1.0 (sqrt 2.0)))))))

;; Each sample is an output trace.

(define peak
  (gen [samples]
    (define so (sort samples))
    (define window (add 1 (clojure.core/quot (length so) 10)))
    (define nthcdr (gen [x i] (if (eq i 0) x (nthcdr (rest x) (sub i 1)))))
    (define lead (nthcdr so window))
    (nth (first (sort (clojure.core/map (gen [x y] [(sub y x) (div (add x y) 2)])
                                        so
                                        lead)))
         1)))

(define analyze
  (gen [samples]
    (print ["average:" (div (apply clojure.core/+ samples) (length samples))
            "peak:" (peak samples)])
    samples))

(define get-samples
  (gen [number-of-runs]
    (binned-histogram
      :name    "samples from the prior"
      :samples (analyze
               (replicate number-of-runs
                          two-variable-gaussian-model))
      :overlay-densities (list (tuple "prior" prior-density)
                               (tuple "target" target-density)))))

(define target-trace (empty-trace))
(trace-set target-trace (addr 1 "y" "gaussian") 3.0)

(define rejection-assay
  (gen [number-of-runs]
    (binned-histogram
      :name    "samples from the target"
      :samples (analyze
               (replicate
                 number-of-runs
                 (gen []
                   (print "rejection sample")
                   (define tr
                     (rejection-sampling
                      two-variable-gaussian-model  ; :model-procedure 
                      (tuple)  ; :inputs 
                      target-trace  ; :target-trace 
                      0.5))   ; :log-bound 
                   (trace-get tr))))
      :overlay-densities (list (tuple "prior" prior-density)
                               (tuple "target" target-density)))))

(define importance-assay
  (gen [number-of-runs]
    (binned-histogram
      :name    "samples from importance sampling with 20 particles"
      :samples (analyze
               (replicate
                 number-of-runs
                 (gen []
                   (define tr
                     (importance-resampling
                      two-variable-gaussian-model  ; :model-procedure 
                      (tuple)  ; :inputs 
                      target-trace  ; :target-trace 
                      20))
                   (trace-get tr))))  ; :N 
      :overlay-densities (list (tuple "prior" prior-density)
                               (tuple "target" target-density)))))

(define MH-assay
  (gen [number-of-runs]
    (binned-histogram
      :name    "samples from lightweight single-site MH with 20 iterations"
      :samples (analyze
               (replicate
                 number-of-runs
                 (gen []    ;added by JAR
                   (define tr
                     (lightweight-single-site-MH-sampling 20
                                                          two-variable-gaussian-model
                                                          target-trace))
                   ;; was (trace-get tr (addr 0 "x" "gaussian"))
                   (trace-get tr))))
      :overlay-densities (list (tuple "prior" prior-density)
                               (tuple "target" target-density)))))
