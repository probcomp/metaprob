;; This file was automatically generated

(ns metaprob.src.crp
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  new_crp_state
  crp_sample
  crp_log_density
  crp_incorporate
  make_chinese_restaurant_sampler)

(define
  new_crp_state
  (program
    []
    (block
      (block
        (define __trace_0__ (mk_nil))
        (trace_set_subtrace_at __trace_0__ (list "tables") (mk_nil))
        (trace_set (lookup __trace_0__ (list "next_table")) 1)
        (trace_set (lookup __trace_0__ (list "num_customers")) 0)
        __trace_0__))))

(define
  crp_sample
  (program
    [state alpha]
    (define
      old_tables
      (trace_subkeys (lookup state (list "table_counts"))))
    (define
      old_counts
      (map
        (program
          [k]
          (trace_get (lookup state (list "table_counts" k))))
        old_tables))
    (define weights (pair alpha old_counts))
    (define
      candidates
      (pair (trace_get (lookup state (list "next_table"))) old_tables))
    (categorical (normalize weights) candidates)))

(define
  crp_log_density
  (program
    [state alpha new_table]
    (define
      prob_num
      (if (trace_has (lookup state (list "table_counts" new_table)))
        (block
          (log
            (trace_get
              (lookup state (list "table_counts" new_table)))))
        (if (eq
              new_table
              (trace_get (lookup state (list "next_table"))))
          (block (log alpha))
          (block (div (sub 0 1) 0)))))
    (sub
      prob_num
      (log
        (add
          alpha
          (trace_get (lookup state (list "num_customers"))))))))

(define
  crp_incorporate
  (program
    [state new_table]
    (if (gte new_table (trace_get (lookup state (list "next_table"))))
      (block
        (trace_set
          (lookup state (list "next_table"))
          (add new_table 1)))
      "ok")
    (if (trace_has (lookup state (list "table_counts" new_table)))
      (block
        (trace_set
          (lookup state (list "table_counts" new_table))
          (add
            (trace_get (lookup state (list "table_counts" new_table)))
            1)))
      (block
        (trace_set (lookup state (list "table_counts" new_table)) 1)))
    (trace_set
      (lookup state (list "num_customers"))
      (add (trace_get (lookup state (list "num_customers"))) 1))))

(define
  make_chinese_restaurant_sampler
  (program
    [alpha]
    (define state (new_crp_state))
    (define
      tracing_proposer
      (program
        [args intervention target output]
        (block
          (if (trace_has target)
            (block
              (define new_table (trace_get target))
              (define prob (crp_log_density state alpha new_table))
              (crp_incorporate state new_table)
              (trace_set output new_table)
              (tuple new_table prob))
            (block
              (if (trace_has intervention)
                (block
                  (crp_incorporate state (trace_get intervention))
                  (tuple (trace_get intervention) 0.0))
                (block
                  (define ans (crp_sample state alpha))
                  (crp_incorporate state ans)
                  (trace_set output ans)
                  (tuple ans 0.0))))))))
    (block
      (define __trace_1__ (mk_nil))
      (trace_set __trace_1__ "prob prog")
      (trace_set (lookup __trace_1__ (list "name")) "crp")
      (trace_set
        (lookup __trace_1__ (list "custom_interpreter"))
        (program
          [args intervene]
          (define
            [v _]
            (tracing_proposer args intervene (mk_nil) (mk_nil)))
          v))
      (trace_set
        (lookup __trace_1__ (list "custom_choice_tracer"))
        (program
          [args intervene output]
          (define
            [v _]
            (tracing_proposer args intervene (mk_nil) output))
          v))
      (trace_set
        (lookup __trace_1__ (list "custom_proposer"))
        (program
          [args intervene target]
          (block (tracing_proposer args intervene target (mk_nil)))))
      (trace_set
        (lookup __trace_1__ (list "custom_choice_tracing_proposer"))
        tracing_proposer)
      __trace_1__)))

(define mk_crp make_chinese_restaurant_sampler)

