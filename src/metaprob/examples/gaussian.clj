;; 2.

(ns metaprob.examples.gaussian
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.distributions :refer :all]))

;; sampling from a Gaussian with user-specified mean
;; and standard deviation, by rescaling one output from the Box-Muller
;; algorithm for sampling from a Gaussian with zero mean
;; and unit variance

(define generate-gaussian
  (gen [mu sigma]
    (define u1 (uniform 0 1))
    (define u2 (uniform 0 1))
    (define answer
     (+ mu (* (* (sqrt (* (- 0 2) (log u1)))    ;CHECK THIS
    	              (cos (* (* 2 3.14159265) u2)))
		      sigma)))
    answer))

(define standard-gaussian-log-density
  (gen [x]
    (- (* (- 0 0.5) (log (* 2 3.14159265)))
       (* (* 0.5 x) x))))

(define score-gaussian
  (gen [x params]
    (define [mu sigma] params)
    (- (standard-gaussian-log-density
         (/ (- x mu) sigma))
       (log sigma))))

(define gaussian
  (make-inference-procedure-from-sampler-and-scorer
			"gaussian"
      generate-gaussian
      score-gaussian))

;; defining a latent variable model using this new
;; primitive probability distribution

(define two-variable-gaussian-model
  (gen []
    (define x (gaussian 0 1))
    (define y (gaussian x 1))
    x))
