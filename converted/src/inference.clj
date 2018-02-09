;; This file was automatically generated

(ns metaprob.src.inference
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  rejection_sampling
  infer_resampling
  sample_discrete_random_variate
  ssrmh_step
  infer_ssrmh
  infer_mcmc
  infer_rolling_resample
  joint_enumerate
  enumerate_executions)

(define
  rejection_sampling
  (program
    [program-noncolliding inputs target log_bound]
    (define candidate_trace (mk_nil))
    (define
      [_ score]
      (propose_and_trace_choices
        program-noncolliding
        inputs
        (mk_nil)
        target
        candidate_trace))
    (if (lt (log (uniform 0 1)) (sub score log_bound))
      (block candidate_trace)
      (block
        (rejection_sampling
          program-noncolliding
          inputs
          target
          log_bound)))))

(define
  infer_resampling
  (program
    [program-noncolliding inputs target num_p]
    (define
      particles
      (replicate
        num_p
        (program
          []
          (define candidate_trace (mk_nil))
          (define
            score
            (propose1
              program-noncolliding
              inputs
              (mk_nil)
              target
              candidate_trace))
          (tuple score candidate_trace))))
    (define
      trace_scores
      (map (program [p] (trace_get (lookup p (list 0)))) particles))
    (define
      traces
      (map (program [p] (trace_get (lookup p (list 1)))) particles))
    (log_categorical trace_scores traces)))

(define
  sample_discrete_random_variate
  (program
    [outputs log_weights]
    (block (log_categorical log_weights outputs))))

(define
  ssrmh_step
  (program
    [sp args input_trace constraint_addresses]
    (define sites (filter random_output (trace_sites input_trace)))
    (define candidates (set_difference sites constraint_addresses))
    (define site (uniform_categorical candidates))
    (define old_correction (sub 0 (log (length candidates))))
    (define old_val (trace_get (lookup input_trace site)))
    (trace_clear (lookup input_trace site))
    (define
      old_score
      (propose1
        sp
        args
        (block
          (define __trace_0__ (mk_nil))
          (trace_set (lookup __trace_0__ site) old_val)
          __trace_0__)
        input_trace
        (mk_nil)))
    (define new_t (mk_nil))
    (define new_score (propose1 sp args (mk_nil) input_trace new_t))
    (define new_sites (filter random_output (trace_sites new_t)))
    (define
      new_candidates
      (set_difference new_sites constraint_addresses))
    (define new_correction (sub 0 (log (length new_candidates))))
    (define
      log_accept
      (add
        (sub (sub new_score old_score) old_correction)
        new_correction))
    (if (lt (log (uniform 0 1)) log_accept)
      (block new_t)
      (block
        (trace_set (lookup input_trace site) old_val)
        input_trace))))

(define
  infer_ssrmh
  (program
    [sp args target num_steps]
    (define candidate (mk_nil))
    (define constraints (trace_sites target))
    (trace_choices sp args target candidate)
    (define step (program [t] (ssrmh_step sp args t constraints)))
    (iterate num_steps step candidate)))

(define
  infer_mcmc
  (program
    [sp args target num_steps query]
    (define
      t_box
      (block
        (define __trace_1__ (mk_nil))
        (trace_set __trace_1__ (mk_nil))
        __trace_1__))
    (define constraints (trace_sites target))
    (trace_choices sp args target (trace_get t_box))
    (define first_val (query (trace_get t_box)))
    (define
      step
      (program
        [t]
        (define
          new_t
          (ssrmh_step sp args (trace_get t_box) constraints))
        (trace_set t_box new_t)
        (query (trace_get t_box))))
    (pair first_val (map step (range num_steps)))))

(define
  infer_rolling_resample
  (program
    [sp args target num_p query]
    (define
      particle
      (program
        []
        (define candidate_trace (mk_nil))
        (define
          score
          (propose1 sp args (mk_nil) target candidate_trace))
        (tuple score candidate_trace)))
    (define neg_inf (div (sub 0 1) 0))
    (define
      current
      (block
        (define __trace_2__ (mk_nil))
        (trace_set (lookup __trace_2__ (list "score")) neg_inf)
        (trace_set (lookup __trace_2__ (list "t")) (mk_nil))
        __trace_2__))
    (define
      step
      (program
        []
        (define [score t] (particle))
        (define
          weights
          (tuple (trace_get (lookup current (list "score"))) score))
        (trace_set
          (lookup current (list "t"))
          (log_categorical
            weights
            (tuple (trace_get (lookup current (list "t"))) t)))
        (trace_set (lookup current (list "score")) (logsumexp weights))
        (query (trace_get (lookup current (list "t"))))))
    (replicate num_p step)))

(define
  joint_enumerate
  (program
    [sites]
    (if (is_pair sites)
      (block
        (define others (joint_enumerate (rest sites)))
        (define oper_name (last (first sites)))
        (define oper (toplevel_lookup oper_name))
        (define
          value_candidates
          (trace_get (lookup oper (list "support"))))
        (define
          trace_lists
          (map
            (program
              [value]
              (block
                (map
                  (program
                    [t]
                    (define t1 (trace_copy t))
                    (trace_set (lookup t1 (first sites)) value)
                    t1)
                  others)))
            value_candidates))
        (concat trace_lists))
      (block (pair (mk_nil) (mk_nil))))))

(define
  enumerate_executions
  (program
    [program-noncolliding inputs intervention_trace target_trace]
    (define one_run (mk_nil))
    (trace_choices
      program-noncolliding
      inputs
      intervention_trace
      one_run)
    (define all_sites (addresses_of one_run))
    (define
      free_sites
      (set_difference
        (set_difference all_sites (addresses_of intervention_trace))
        (addresses_of target_trace)))
    (define candidates (joint_enumerate free_sites))
    (map
      (program
        [candidate]
        (trace_update candidate target_trace)
        (define t (mk_nil))
        (define
          [_ score]
          (propose_and_trace_choices
            program-noncolliding
            inputs
            intervention_trace
            candidate
            t))
        (tuple t score))
      candidates)))

