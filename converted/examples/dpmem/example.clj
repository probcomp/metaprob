;; This file was automatically generated

(ns metaprob.examples.dpmem.example
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  dpmem
  draw_dpmmog
  constrain_dpmog_trace
  posterior_predictive
  constrain_assignments
  particle_samples
  dump_particle_samples
  mcmc_samples
  dump_mcmc_samples
  lightweight_samples
  dump_lightweight_samples)

(define
  dpmem
  (program
    [alpha]
    (program
      [thunk]
      (define assign (mk_crp alpha))
      (define sample (mem (program [i] (thunk))))
      (program [] (sample (assign))))))

(define
  draw_dpmmog
  (program
    [n]
    (define alpha (gamma 1.0 1.0))
    (define
      cmpt
      (program
        []
        (define sigma (sqrt (inverse_gamma 1.0 1.0)))
        (define mean (normal 0 (mul 100 sigma)))
        (tuple mean sigma)))
    (define memoized ((dpmem alpha) cmpt))
    (define
      sampler
      (program
        []
        (define [mean sigma] (memoized))
        (normal mean sigma)))
    (replicate n sampler)))

(define
  constrain_dpmog_trace
  (program
    [data]
    (define t1 (mk_nil))
    (define
      sampler
      (lookup draw_dpmmog (list "source" "body" 3 "sampler")))
    (imap
      (program
        [i datum]
        (block
          (trace_set
            (lookup
              t1
              (list 4 (prob_prog_name replicate) i sampler 1 "normal"))
            datum)))
      data)
    t1))

(define
  data
  (array_to_list
    (tuple
      100
      101
      (sub 0 99)
      (sub 0 100)
      99
      (sub 0 101)
      100.5
      99.5
      (sub 0 100.5)
      (sub 0 99.5))))

(define correct_assignments (array_to_list (tuple 1 1 2 2 1 2 1 1 2 2)))

(define
  posterior_predictive
  (program
    [n]
    (program
      [state]
      (define
        [vals _]
        (propose draw_dpmmog (tuple (add n 1)) (mk_nil) state))
      (nth vals n))))

(define
  constrain_assignments
  (program
    [trace correct]
    (define
      sampler
      (lookup draw_dpmmog (list "source" "body" 3 "sampler")))
    (define memoized (lookup dpmem (list "source" "body" "body" 2)))
    (imap
      (program
        [i datum]
        (block
          (trace_set
            (lookup
              trace
              (list
                4
                (prob_prog_name replicate)
                i
                sampler
                0
                "definiens"
                memoized
                1
                "crp"))
            datum)))
      correct)
    trace))

(define
  particle_samples
  (program
    [num_replicates num_p]
    (replicate
      num_replicates
      (program
        []
        (infer_rolling_resample
          draw_dpmmog
          (tuple (length data))
          (constrain_assignments
            (constrain_dpmog_trace data)
            correct_assignments)
          num_p
          (posterior_predictive (length data)))))))

(define
  dump_particle_samples
  (program
    [num_replicates num_p outfile]
    (dump_py_data (particle_samples num_replicates num_p) outfile)))

(define
  mcmc_samples
  (program
    [num_replicates num_p]
    (replicate
      num_replicates
      (program
        []
        (infer_mcmc
          draw_dpmmog
          (tuple (length data))
          (constrain_dpmog_trace data)
          num_p
          (posterior_predictive (length data)))))))

(define
  dump_mcmc_samples
  (program
    [num_replicates num_p outfile]
    (dump_py_data (mcmc_samples num_replicates num_p) outfile)))

(define
  lightweight_samples
  (program
    [num_replicates num_t]
    (replicate
      num_replicates
      (program
        []
        (infer_lightweight_mcmc
          draw_dpmmog
          (tuple (length data))
          (constrain_dpmog_trace data)
          num_t
          (posterior_predictive (length data)))))))

(define
  dump_lightweight_samples
  (program
    [num_replicates num_p outfile]
    (dump_py_data (lightweight_samples num_replicates num_p) outfile)))

