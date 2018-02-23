;; 2.

(ns metaprob.mapl2018.gaussian
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.mapl2018.interpreters :refer :all]))

;; sampling from a Gaussian with user-specified mean
;; and variance, by rescaling one output from the Box-Muller
;; algorithm for sampling from a Gaussian with zero mean
;; and unit variance

(define gaussian
  (probprog [mu sigma]
    (define u1 (uniform 0 1))
    (define u2 (uniform 0 1))
    (define answer
     (add mu (mul (mul (sqrt (mul (sub 0 2) (log u1)))    ;CHECK THIS
    	              (cos (mul (mul 2 3.14159265) u2)))
		      sigma)))
    answer))

;; associating the gaussian probabilistic program with a
;; meta-program for evaluating its density

(trace-set
  (lookup gaussian (addr "log-output-probability-density"))
  (probprog [x mu sigma]
    (define standard-gaussian-log-density
      (probprog [x]
        (sub (mul (sub 0 0.5) (log (mul 2 3.14159265)))
           (mul (mul 0.5 x) x))))
    (sub (standard-gaussian-log-density
           (div (sub x mu) sigma)) (log sigma)) ))

;; associating invocations of the Gaussian with
;; user-interpretable names

(trace-set
  (lookup gaussian (addr "name")) "gaussian")

;; defining a latent variable model using this new
;; primitive probability distribution

(define two-variable-gaussian-model
  (probprog []
    (define x (gaussian 0 1))
    (define y (gaussian x 1))
    x))
