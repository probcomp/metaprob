;; 5.

(ns metaprob.mapl2018.inference-on-gaussian
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.mapl2018.gaussian :refer [gaussian two-variable-gaussian-model]]
            [metaprob.mapl2018.interpreters :refer :all]))

(define prior-density
  (probprog [x]
    (exp ((trace-get gaussian
                     (addr "log-output-probability-density"))
          (tuple x 0 1))) ))

(define target-density
  (probprog [x]
    (exp ((trace-get gaussian
                     (addr "log-output-probability-density"))
          (tuple x 1.5 (div 1.0 (sqrt 2.0))))) ))    ;was /

(binned-histogram
  :name    "samples from the prior"
  :samples (replicate 1000
  	   	      two-variable-gaussian-model)
  :overlay-densities (list (tuple "prior" prior-density)
  		     	   (tuple "target" target-density)))

(define target-trace (empty-trace))
;; Attn VKM: This makes no sense (only one arg to lookup)
(trace-set target-trace (lookup (addr 1 "y" "gaussian")) 3.0)

(binned-histogram
  :name    "samples from the target"
  :samples (replicate
  	     1000
  	     (probprog
	       []
	       (rejection-sampling
	         :model-probprog two-variable-gaussian-model
		 :inputs (tuple)
		 :target-trace target-trace
	         :log-bound 0.5)))
  :overlay-densities (list (tuple "prior" prior-density)
  		     	   (tuple "target" target-density)))

(binned-histogram
  :name    "samples from importance sampling with 20 particles"
  :samples (replicate
  	     1000
  	     (probprog
	       []
	       (importance-resampling
	         :model-probprog two-variable-gaussian-model
		 :inputs (tuple)
		 :target-trace target-trace
		 :N 20)))
  :overlay-densities (list (tuple "prior" prior-density)
  		     	   (tuple "target" target-density)))

(binned-histogram
  :name    "samples from lightweight single-site MH with 20 iterations"
  :samples (replicate
  	     1000
	     (lightweight-single-site-MH-sampling 20 target-trace))
  :overlay-densities (list (tuple "prior" prior-density)
  		     	   (tuple "target" target-density)))
