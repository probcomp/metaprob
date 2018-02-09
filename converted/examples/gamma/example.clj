;; This file was automatically generated

(ns metaprob.examples.gamma.example
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare std_gamma doit)

(define
  std_gamma
  (program
    [alpha]
    (define d (sub alpha (div 1.0 3)))
    (define c (div 1.0 (sqrt (mul 9 d))))
    (define x (normal 0 1))
    (define v (pow (add 1 (mul c x)) 3))
    (if (lte v 0)
      (block (std_gamma alpha))
      (block
        (define
          log_bound
          (add (mul (mul 0.5 x) x) (mul d (add (sub 1 v) (log v)))))
        (define u (uniform 0 1))
        (if (gte (log u) log_bound)
          (block (std_gamma alpha))
          (block (mul d v)))))))

(trace_set
  (lookup std_gamma (list "log_output_probability_density"))
  (program
    [x alpha]
    (block
      (sub
        (sub (mul (sub alpha 1) (log x)) x)
        (log_gamma_function alpha)))))

(define
  doit
  (program
    [filename]
    (block
      (binned_histogram
        (replicate 200 (program [] (block (std_gamma 0.5))))
        (program
          [x]
          (block
            (exp
              ((trace_get
                 (lookup
                   std_gamma
                   (list "log_output_probability_density")))
                x
                0.5))))
        "Forward sampling a gamma distribution"
        filename))))

