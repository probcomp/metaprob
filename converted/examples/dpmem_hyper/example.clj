;; This file was automatically generated

(ns metaprob.examples.dpmem-hyper.example
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  dpmem
  generate_from_dirichlet_process_mixture
  constrain_dpmog_trace
  posterior_predictive
  log_score
  assignment_addresses
  constrain_assignments
  obtain_assignments
  query
  particle_samples
  dump_particle_samples
  mcmc_samples
  dump_mcmc_samples
  lightweight_samples
  dump_lightweight_samples)

(define
  dpmem
  (program
    [alpha root cmpt]
    (define assign (mk_crp alpha))
    (define sample (mem (program [cluster] (cmpt))))
    (program
      [i]
      (sample
        (apply_with_address (list root "assign" i) assign (tuple))))))

(define
  generate_from_dirichlet_process_mixture
  (program
    [num_datapoints]
    (define root_addr this)
    (define alpha (gamma 1.0 1.0))
    (define sample_assignment (make_chinese_restaurant_sampler alpha))
    (define
      get_cluster
      (mem (program [i] (block (sample_assignment)))))
    (define a (inverse_gamma 3.0 10.0))
    (define b (uniform 0.0 10.0))
    (define mu (uniform (sub 0 150.0) 150.0))
    (define V (inverse_gamma 2 100))
    (define
      get_mu
      (mem
        (program
          [cluster]
          (block (normal mu (mul V (get_sigma cluster)))))))
    (define
      get_sigma
      (mem (program [cluster] (block (sqrt (inverse_gamma a b))))))
    (define
      get_cluster
      (mem
        (program
          [i]
          (block
            (apply_with_address
              (list root_addr "assign" i)
              sample_assignment
              (tuple))))))
    (define
      get_datapoint
      (mem
        (program
          [i]
          (define cluster (get_cluster i))
          (with-address
            (list root_addr "datum" i)
            (normal (get_mu cluster) (get_sigma cluster))))))
    (map get_datapoint (range num_datapoints))))

(define draw_dpmmog generate_from_dirichlet_process_mixture)

(define
  constrain_dpmog_trace
  (program
    [data]
    (define t1 (mk_nil))
    (imap
      (program
        [i datum]
        (block
          (trace_set (lookup t1 (list "datum" i "normal")) datum)))
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
  log_score
  (program
    [n state]
    (propose1 draw_dpmmog (tuple n) (mk_nil) state (mk_nil))))

(define
  assignment_addresses
  (program [trace n] (map (program [i] (list "assign" i)) (range n))))

(define
  constrain_assignments
  (program
    [trace correct]
    (zipmap
      (program
        [addr datum]
        (block (trace_set (lookup trace addr) datum)))
      (assignment_addresses trace (length correct))
      correct)
    trace))

(define
  obtain_assignments
  (program
    [trace n]
    (map
      (program [addr] (trace_get (lookup trace addr)))
      (assignment_addresses trace n))))

(define
  query
  (program
    [state]
    (define alpha (trace_get (lookup state (list 1 "alpha" "gamma"))))
    (define a (trace_get (lookup state (list 4 "a" "inverse_gamma"))))
    (define b (trace_get (lookup state (list 5 "b" "uniform"))))
    (define mu (trace_get (lookup state (list 6 "mu" "uniform"))))
    (define V (trace_get (lookup state (list 7 "V" "inverse_gamma"))))
    (define pred ((posterior_predictive (length data)) state))
    (define score (log_score (length data) state))
    (define assignments (obtain_assignments state (length data)))
    (tuple alpha a b mu V pred score assignments)))

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
          (constrain_dpmog_trace data)
          num_p
          query)))))

(define
  dump_particle_samples
  (program
    [num_replicates num_p outfile]
    (dump_py_data (particle_samples num_replicates num_p) outfile)))

(define
  mcmc_samples
  (program
    [num_replicates num_t]
    (replicate
      num_replicates
      (program
        []
        (infer_mcmc
          draw_dpmmog
          (tuple (length data))
          (constrain_dpmog_trace data)
          num_t
          query)))))

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
          query)))))

(define
  dump_lightweight_samples
  (program
    [num_replicates num_p outfile]
    (dump_py_data (lightweight_samples num_replicates num_p) outfile)))

