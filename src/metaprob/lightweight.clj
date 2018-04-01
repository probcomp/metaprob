;; This file was automatically generated, then edited

(clojure.core/ns metaprob.lightweight
  (:refer-clojure :only [declare ])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            ;; Added
            [metaprob.trace]
            [metaprob.infer :refer [infer]]
            [metaprob.distributions :refer [infer]]
            [metaprob.metacirc.propose :refer [propose]]
            [metaprob.metacirc.trace-choices :refer [trace_choices]]))

(declare
  single_site_metropolis_hastings_step
  infer_lightweight_chain
  infer_lightweight_mcmc)

(define single_site_metropolis_hastings_step
  (gen [proc inputs trace constraint_addresses]
    (define choice_addresses (addresses_of trace))
    (define
      candidates
      (set-difference choice_addresses constraint_addresses))
    (define target_address (uniform-sample candidates))
    (define initial_value (trace-get trace target_address))
    (define initial_num_choices (length candidates))
    (trace-clear (lookup trace target_address))
    (define new_trace (empty-trace))
    (define [_ forward_score]
      (infer proc
             inputs
             (empty-trace)
             trace
             new_trace))
    (define new_value (trace-get new_trace target_address))
    (define new_choice_addresses (addresses_of new_trace))
    (define new_candidates
      (set-difference new_choice_addresses constraint_addresses))
    (define new_num_choices (length new_candidates))
    (define
      restoring_trace
      (block
        (define __trace_0__ (empty-trace))
        (trace-set __trace_0__ target_address initial_value)
        __trace_0__))
    (for_each
      (set-difference choice_addresses new_choice_addresses)
      (gen [addr]
        (block
          (trace-set restoring_trace
                     addr
                     (trace-get trace addr)))))
    (trace-clear (lookup new_trace target_address))
    (define [_ reverse_score]
      (propose proc inputs restoring_trace new_trace))
    (trace-set new_trace target_address new_value)
    (define
      log_p_accept
      (add
        (sub
          (sub forward_score reverse_score)
          (log initial_num_choices))
        (log new_num_choices)))
    (if (lt (log (uniform 0 1)) log_p_accept)
      (block
        (for_each
          (set-difference choice_addresses new_choice_addresses)
          (gen [addr] (trace-clear (lookup trace addr))))
        (for_each
          new_choice_addresses
          (gen [addr]
            (trace-set trace
                       addr
                       (trace-get new_trace addr)))))
      (block (trace-set trace target_address initial_value)))))

(define infer_lightweight_chain
  (gen [sp args target num_steps]
    (define candidate (empty-trace))
    (define constraints (addresses_of target))
    (trace_choices sp args target candidate)
    (repeat
      num_steps
      (gen
        []
        (single_site_metropolis_hastings_step
          sp
          args
          candidate
          constraints)))
    candidate))

(define infer_lightweight_mcmc
  (gen [sp args target num_steps query]
    (define state (empty-trace))
    (define constraints (addresses-of target))
    (trace_choices sp args target state)
    (define first_val (query state))
    (define step
      (gen [i]
        (single_site_metropolis_hastings_step
          sp
          args
          state
          constraints)
        (query state)))
    (pair first_val (map step (range num_steps)))))

