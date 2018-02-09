;; This file was automatically generated

(ns metaprob.examples.earthquake.example
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  earthquake_bayesian_network
  styles
  query
  prior_samples
  particle_samples
  cond_samples
  inter_samples
  prior_exact
  cond_exact
  inter_exact
  prior_samples_hist
  inter_samples_hist
  cond_samples_hist
  prior_exact_hist
  inter_exact_hist
  cond_exact_hist)

(define
  earthquake_bayesian_network
  (program
    []
    (define earthquake (flip 0.1))
    (define burglary (flip 0.1))
    (define
      p_alarm
      (if (and burglary earthquake)
        0.9
        (if burglary 0.85 (if earthquake 0.2 0.05))))
    (define alarm (flip p_alarm))
    (define p_john_call (if alarm 0.8 0.1))
    (define john_call (flip p_john_call))
    (define p_mary_call (if alarm 0.9 0.4))
    (define mary_call (flip p_mary_call))
    "ok"))

(define alarm_went_off (mk_nil))

(trace_set (lookup alarm_went_off (list 3 "alarm" "flip")) true)

(define
  styles
  (program
    []
    (propose
      earthquake_bayesian_network
      (tuple)
      (mk_nil)
      (mk_nil)
      (mk_nil))
    (propose
      earthquake_bayesian_network
      (tuple)
      alarm_went_off
      (mk_nil)
      (mk_nil))
    (propose
      earthquake_bayesian_network
      (tuple)
      (mk_nil)
      alarm_went_off
      (mk_nil))
    "ok"))

(define
  query
  (program
    [state]
    (define
      earthquake
      (trace_get (lookup state (list 0 "earthquake" "flip"))))
    (define
      burglary
      (trace_get (lookup state (list 1 "burglary" "flip"))))
    (define alarm (trace_get (lookup state (list 3 "alarm" "flip"))))
    (define
      john_call
      (trace_get (lookup state (list 5 "john_call" "flip"))))
    (define
      mary_call
      (trace_get (lookup state (list 7 "mary_call" "flip"))))
    (tuple earthquake burglary alarm john_call mary_call)))

(define
  prior_samples
  (program
    [num_replicates]
    (replicate
      num_replicates
      (program
        []
        (define
          [score t]
          (trace_of earthquake_bayesian_network (tuple)))
        (tuple score (query t))))))

(define
  particle_samples
  (program
    [num_replicates num_p]
    (replicate
      num_replicates
      (program
        []
        (query
          (infer_resampling
            earthquake_bayesian_network
            (tuple)
            alarm_went_off
            num_p))))))

(define
  cond_samples
  (program
    [num_replicates]
    (replicate
      num_replicates
      (program
        []
        (define t (mk_nil))
        (define
          score
          (propose1
            earthquake_bayesian_network
            (tuple)
            (mk_nil)
            alarm_went_off
            t))
        (tuple score (query t))))))

(define
  inter_samples
  (program
    [num_replicates]
    (replicate
      num_replicates
      (program
        []
        (define t (mk_nil))
        (define
          score
          (propose1
            earthquake_bayesian_network
            (tuple)
            alarm_went_off
            (mk_nil)
            t))
        (tuple score (query t))))))

(define
  prior_exact
  (program
    []
    (block
      (map
        (program
          [pair]
          (define [t score] pair)
          (tuple score (query t)))
        (enumerate_executions
          earthquake_bayesian_network
          (tuple)
          (mk_nil)
          (mk_nil))))))

(define
  cond_exact
  (program
    []
    (block
      (map
        (program
          [pair]
          (define [t score] pair)
          (tuple score (query t)))
        (enumerate_executions
          earthquake_bayesian_network
          (tuple)
          (mk_nil)
          (block
            (define __trace_0__ (mk_nil))
            (trace_set
              (lookup __trace_0__ (list 3 "alarm" "flip"))
              true)
            __trace_0__))))))

(define
  inter_exact
  (program
    []
    (block
      (map
        (program
          [pair]
          (define [t score] pair)
          (tuple score (query t)))
        (enumerate_executions
          earthquake_bayesian_network
          (tuple)
          (block
            (define __trace_1__ (mk_nil))
            (trace_set
              (lookup __trace_1__ (list 3 "alarm" "flip"))
              true)
            __trace_1__)
          (mk_nil))))))

(define
  prior_samples_hist
  (program
    [n_samples savfile filename save]
    (block
      (if save
        (block
          (define
            prior_trace
            (program
              []
              (define t (mk_nil))
              (trace_choices
                earthquake_bayesian_network
                (tuple)
                (mk_nil)
                t)
              t))
          (define prior_traces (replicate n_samples prior_trace))
          (dump_py_data (map query prior_traces) savfile))
        (block
          (define prior_traces (load_py_data savfile))
          (discrete_histogram
            prior_traces
            query
            filename
            "Earthquake prior (samples)"))))))

(define
  inter_samples_hist
  (program
    [n_samples savfile filename save]
    (block
      (if save
        (block
          (define
            inter_trace
            (program
              []
              (define t (mk_nil))
              (trace_choices
                earthquake_bayesian_network
                (tuple)
                alarm_went_off
                t)
              t))
          (define inter_traces (replicate n_samples inter_trace))
          (dump_py_data (map query inter_traces) savfile))
        (block
          (define inter_traces (load_py_data savfile))
          (discrete_histogram
            inter_traces
            query
            filename
            "Alarm intervened (samples)"))))))

(define
  cond_samples_hist
  (program
    [n_samples savfile filename save]
    (block
      (if save
        (block
          (define
            cond_trace
            (program
              []
              (block
                (rejection_sampling
                  earthquake_bayesian_network
                  (tuple)
                  alarm_went_off
                  0))))
          (define cond_traces (replicate n_samples cond_trace))
          (dump_py_data (map query cond_traces) savfile))
        (block
          (define cond_traces (load_py_data savfile))
          (discrete_histogram
            cond_traces
            query
            filename
            "Alarm observed true (samples)"))))))

(define
  prior_exact_hist
  (program
    [filename]
    (define
      prior_traces
      (enumerate_executions
        earthquake_bayesian_network
        (tuple)
        (mk_nil)
        (mk_nil)))
    (define
      prior_traces
      (map
        (program
          [pair]
          (define [t score] pair)
          (tuple (query t) score))
        prior_traces))
    (discrete_weighted_histogram
      prior_traces
      query
      filename
      "Earthquake prior (enumeration)")))

(define
  inter_exact_hist
  (program
    [filename]
    (define
      inter_traces
      (enumerate_executions
        earthquake_bayesian_network
        (tuple)
        alarm_went_off
        (mk_nil)))
    (define
      inter_traces
      (map
        (program
          [pair]
          (define [t score] pair)
          (tuple (query t) score))
        inter_traces))
    (discrete_weighted_histogram
      inter_traces
      query
      filename
      "Alarm intervened (enumerated)")))

(define
  cond_exact_hist
  (program
    [filename]
    (define
      cond_traces
      (enumerate_executions
        earthquake_bayesian_network
        (tuple)
        (mk_nil)
        alarm_went_off))
    (define
      cond_traces
      (map
        (program
          [pair]
          (define [t score] pair)
          (tuple (query t) score))
        cond_traces))
    (discrete_weighted_histogram
      cond_traces
      query
      filename
      "Alarm observed true (enumerated)")))

