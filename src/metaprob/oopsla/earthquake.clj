;; This file was automatically generated

(ns metaprob.oopsla.earthquake
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            [metaprob.infer :refer :all]
            [metaprob.distributions :refer :all]))

(declare
  propose1
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

(define earthquake_bayesian_network
  (gen []
    (define earthquake (flip 0.1))
    (define burglary (flip 0.1))
    (define p_alarm
      (if (and burglary earthquake)
        0.9
        (if burglary 0.85 (if earthquake 0.2 0.05))))
    (define alarm (flip p_alarm))
    (define p_john_call (if alarm 0.8 0.1))
    (define john_call (flip p_john_call))
    (define p_mary_call (if alarm 0.9 0.4))
    (define mary_call (flip p_mary_call))
    "ok"))

(define alarm_went_off (empty-trace))

(trace-set (lookup alarm_went_off (list 3 "alarm" "flip")) true)

(define styles
  (gen []
    (infer-apply                        ;Prior
      earthquake_bayesian_network
      (tuple)
      nil
      nil
      nil)
    (infer-apply                        ;Set alarm
      earthquake_bayesian_network
      (tuple)
      alarm_went_off
      nil
      nil)
    (infer-apply                        ;Observe alarm
      earthquake_bayesian_network
      (tuple)
      nil
      alarm_went_off
      nil)
    "ok"))

(define query
  (gen [state]
    (define earthquake
      (trace-get (lookup state (list 0 "earthquake" "flip"))))
    (define burglary
      (trace-get (lookup state (list 1 "burglary" "flip"))))
    (define alarm (trace-get (lookup state (list 3 "alarm" "flip"))))
    (define john_call
      (trace-get (lookup state (list 5 "john_call" "flip"))))
    (define mary_call
      (trace-get (lookup state (list 7 "mary_call" "flip"))))
    (tuple earthquake burglary alarm john_call mary_call)))

;; See prelude.vnts
(define trace-of
  (gen [proc inputs]
    (define output (empty-trace))
    [(propose1 proc inputs nil nil output) output]))

;; See prelude.vnts
(define propose1
  (gen [prog inputs intervene target output]
    (define [_ score] (infer-apply prog inputs intervene target output))
    score))

(define prior_samples
  (gen [num_replicates]
    (replicate
      num_replicates
      (gen []
        (define [score t]
          (trace-of earthquake_bayesian_network (tuple)))
        (tuple score (query t))))))

(define particle_samples
  (gen [num_replicates num_p]
    (replicate
      num_replicates
      (gen []
        (query
          (infer_resampling             ;importance (re)sampling
            earthquake_bayesian_network
            (tuple)
            alarm_went_off
            num_p))))))

(define cond_samples
  (gen [num_replicates]
    (replicate
      num_replicates
      (gen []
        (define output (empty-trace))
        (define score
          (propose1
            earthquake_bayesian_network
            (tuple)
            nil
            alarm_went_off
            output))
        (tuple score (query output))))))

(define inter_samples
  (gen [num_replicates]
    (replicate
      num_replicates
      (gen []
        (define output (empty-trace))
        (define
          score
          (propose1
            earthquake_bayesian_network
            (tuple)
            alarm_went_off
            nil                         ;no target
            output))
        (tuple score (query output))))))

(define prior_exact
  (gen []
    (block
      (map
        (gen
          [pair]
          (define [output score] pair)
          (tuple score (query output)))
        (enumerate_executions
          earthquake_bayesian_network
          (tuple)
          nil
          nil)))))

(define cond_exact
  (gen []
    (block
      (map
        (gen
          [pair]
          (define [output score] pair)
          (tuple score (query output)))
        (enumerate_executions
          earthquake_bayesian_network
          (tuple)
          nil
          (block
            (define __trace_0__ (empty-trace))
            (trace_set
              (lookup __trace_0__ (list 3 "alarm" "flip"))
              true)
            __trace_0__))))))

(define inter_exact
  (gen []
    (block
      (map
        (gen
          [pair]
          (define [output score] pair)
          (tuple score (query output)))
        (enumerate_executions
          earthquake_bayesian_network
          (tuple)
          (block
            (define __trace_1__ (empty-trace))
            (trace_set
              (lookup __trace_1__ (list 3 "alarm" "flip"))
              true)
            __trace_1__)
          nil)))))

(define prior_samples_hist
  (gen [n_samples savfile filename save]
    (block
      (if save
        (block
          (define
            prior_trace
            (gen
              []
              (define output (empty-trace))
              (trace_choices
                earthquake_bayesian_network
                (tuple)
                nil
                output)
              output))
          (define prior_traces (replicate n_samples prior_trace))
          (dump_py_data (map query prior_traces) savfile))
        (block
          (define prior_traces (load_py_data savfile))
          (discrete_histogram
            prior_traces
            query
            filename
            "Earthquake prior (samples)"))))))

(define inter_samples_hist
  (gen [n_samples savfile filename save]
    (block
      (if save
        (block
          (define
            inter_trace
            (gen
              []
              (define output (empty-trace))
              (trace_choices
                earthquake_bayesian_network
                (tuple)
                alarm_went_off
                output)
              output))
          (define inter_traces (replicate n_samples inter_trace))
          (dump_py_data (map query inter_traces) savfile))
        (block
          (define inter_traces (load_py_data savfile))
          (discrete_histogram
            inter_traces
            query
            filename
            "Alarm intervened (samples)"))))))

(define cond_samples_hist
  (gen [n_samples savfile filename save]
    (block
      (if save
        (block
          (define
            cond_trace
            (gen
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

(define prior_exact_hist
  (gen [filename]
    (define
      prior_traces
      (enumerate_executions
        earthquake_bayesian_network
        (tuple)
        nil
        nil))
    (define
      prior_traces
      (map
        (gen
          [pair]
          (define [output score] pair)
          (tuple (query output) score))
        prior_traces))
    (discrete_weighted_histogram
      prior_traces
      query
      filename
      "Earthquake prior (enumeration)")))

(define inter_exact_hist
  (gen [filename]
    (define
      inter_traces
      (enumerate_executions
        earthquake_bayesian_network
        (tuple)
        alarm_went_off
        nil))
    (define
      inter_traces
      (map
        (gen
          [pair]
          (define [output score] pair)
          (tuple (query output) score))
        inter_traces))
    (discrete_weighted_histogram
      inter_traces
      query
      filename
      "Alarm intervened (enumerated)")))

(define cond_exact_hist
  (gen [filename]
    (define
      cond_traces
      (enumerate_executions
        earthquake_bayesian_network
        (tuple)
        nil
        alarm_went_off))
    (define
      cond_traces
      (map
        (gen [pair]
          (define [output score] pair)
          (tuple (query output) score))
        cond_traces))
    (discrete_weighted_histogram
      cond_traces
      query
      filename
      "Alarm observed true (enumerated)")))

