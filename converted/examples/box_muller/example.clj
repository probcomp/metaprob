;; This file was automatically generated

(ns metaprob.examples.box-muller.example
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  std_normal
  std_normal_density
  gaussian
  with_standard_proposer
  normal_normal
  prior_density
  analytic_density
  prior_hist
  rejection_hist
  importance_hist
  mcmc_hist)

(define
  std_normal
  (program
    []
    (define u1 (uniform 0 1))
    (define u2 (uniform 0 1))
    (mul
      (sqrt (mul (sub 0 2) (log u1)))
      (cos (mul (mul 2 3.14159265) u2)))))

(define
  std_normal_density
  (program
    [x]
    (block
      (sub
        (mul (sub 0 0.5) (log (mul 2 3.14159265)))
        (mul (mul 0.5 x) x)))))

(define
  gaussian
  (program [mu sigma] (block (add mu (mul (std_normal) sigma)))))

(trace_set
  (lookup gaussian (list "log_output_probability_density"))
  (program
    [x mu sigma]
    (block
      (sub (std_normal_density (div (sub x mu) sigma)) (log sigma)))))

(trace_set (lookup gaussian (list "name")) "gaussian")

(define
  with_standard_proposer
  (program
    [prob_prog]
    (assert
      (trace_has_key prob_prog "log_output_probability_density")
      "No density given")
    (define
      proposer
      (program
        [args intervene target output]
        (define [a1 a2] args)
        (if (trace_has target)
          (block
            (define answer (trace_get target))
            (define
              score
              ((trace_get
                 (lookup
                   prob_prog
                   (list "log_output_probability_density")))
                answer
                a1
                a2)))
          (block
            (if (trace_has intervene)
              (block (define answer (trace_get intervene)))
              (block (define answer (prob_prog a1 a2))))
            (define score 0.0)))
        (trace_set output answer)
        (tuple answer score)))
    (define
      ans
      (sp (trace_get (lookup prob_prog (list "name"))) proposer))
    (trace_set
      (lookup ans (list "log_output_probability_density"))
      (trace_get
        (lookup prob_prog (list "log_output_probability_density"))))
    ans))

(define gaussian (with_standard_proposer gaussian))

(define
  normal_normal
  (program [] (define x (gaussian 0 1)) (define y (gaussian x 1)) x))

(define target (mk_nil))

(trace_set (lookup target (list 1 "y" "gaussian")) 3)

(define
  prior_density
  (program
    [x]
    (block
      (exp
        ((trace_get
           (lookup gaussian (list "log_output_probability_density")))
          x
          0
          1)))))

(define
  analytic_density
  (program
    [x]
    (block
      (exp
        ((trace_get
           (lookup gaussian (list "log_output_probability_density")))
          x
          1.5
          (div 1 (sqrt 2)))))))

(define
  prior_hist
  (program
    [filename]
    (define
      prior_sample
      (program
        []
        (define res (mk_nil))
        (trace_choices normal_normal (tuple) (mk_nil) res)
        (trace_get (lookup res (list 0 "x" "gaussian")))))
    (binned_histogram
      (replicate 20 prior_sample)
      (block
        (define __trace_0__ (mk_nil))
        (trace_set (lookup __trace_0__ (list "prior")) prior_density)
        (trace_set
          (lookup __trace_0__ (list "posterior"))
          analytic_density)
        __trace_0__)
      filename
      "Prior sampling")))

(define
  rejection_hist
  (program
    [filename]
    (define
      rejection_sample
      (program
        []
        (define
          res
          (rejection_sampling normal_normal (tuple) target (log 0.5)))
        (trace_get (lookup res (list 0 "x" "gaussian")))))
    (binned_histogram
      (replicate 20 rejection_sample)
      (block
        (define __trace_1__ (mk_nil))
        (trace_set (lookup __trace_1__ (list "prior")) prior_density)
        (trace_set
          (lookup __trace_1__ (list "posterior"))
          analytic_density)
        __trace_1__)
      filename
      "Rejection sampling")))

(define
  importance_hist
  (program
    [filename p]
    (define
      importance_sample
      (program
        []
        (define trace_scores (mk_nil))
        (define traces (mk_nil))
        (for_each
          (range p)
          (program
            [i]
            (define candidate_trace (mk_nil))
            (define
              [_ score]
              (propose_and_trace_choices
                normal_normal
                (tuple)
                (mk_nil)
                target
                candidate_trace))
            (trace_set (lookup trace_scores (list i)) score)
            (trace_set (lookup traces (list i)) candidate_trace)))
        (define
          result
          (sample_discrete_random_variate traces trace_scores))
        (trace_get (lookup result (list 0 "x" "gaussian")))))
    (binned_histogram
      (replicate 20 importance_sample)
      (block
        (define __trace_2__ (mk_nil))
        (trace_set (lookup __trace_2__ (list "prior")) prior_density)
        (trace_set
          (lookup __trace_2__ (list "posterior"))
          analytic_density)
        __trace_2__)
      filename
      (add (add "Importance sampling (" (string p)) " particles)"))))

(define
  mcmc_hist
  (program
    [filename s]
    (define
      mcmc_sample
      (program
        []
        (define result (mk_nil))
        (define constraints (addresses_of target))
        (trace_choices normal_normal args target result)
        (repeat
          s
          (program
            []
            (block
              (single_site_metropolis_hastings_step
                normal_normal
                args
                result
                constraints))))
        (trace_get (lookup result (list 0 "x" "gaussian")))))
    (binned_histogram
      (replicate 20 mcmc_sample)
      (block
        (define __trace_3__ (mk_nil))
        (trace_set (lookup __trace_3__ (list "prior")) prior_density)
        (trace_set
          (lookup __trace_3__ (list "posterior"))
          analytic_density)
        __trace_3__)
      filename
      (add (add "MCMC (" (string s)) " resimulation steps)"))))

