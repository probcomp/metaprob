;; This file was automatically generated, then edited

(clojure.core/ns metaprob.lightweight
  (:refer-clojure :only [declare ])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]
            ;; Added
            [metaprob.trace]
            [metaprob.metacirc.propose-and-trace-choices :refer [propose_and_trace_choices]]
            [metaprob.metacirc.propose :refer [propose]]
            [metaprob.metacirc.trace-choices :refer [trace_choices]]))

(declare
  single_site_metropolis_hastings_step
  infer_lightweight_chain
  infer_lightweight_mcmc)

(define
  single_site_metropolis_hastings_step
  (program
    [program-noncolliding inputs trace constraint_addresses]
    (define choice_addresses (addresses_of trace))
    (define
      candidates
      (set_difference choice_addresses constraint_addresses))
    (define target_address (uniform_sample candidates))
    (define initial_value (trace_get (lookup trace target_address)))
    (define initial_num_choices (length candidates))
    (trace_clear (lookup trace target_address))
    (define new_trace (mk_nil))
    (define
      [_ forward_score]
      (propose_and_trace_choices
        program-noncolliding
        inputs
        (mk_nil)
        trace
        new_trace))
    (define new_value (trace_get (lookup new_trace target_address)))
    (define new_choice_addresses (addresses_of new_trace))
    (define
      new_candidates
      (set_difference new_choice_addresses constraint_addresses))
    (define new_num_choices (length new_candidates))
    (define
      restoring_trace
      (block
        (define __trace_0__ (mk_nil))
        (trace_set (lookup __trace_0__ target_address) initial_value)
        __trace_0__))
    (for_each
      (set_difference choice_addresses new_choice_addresses)
      (program
        [addr]
        (block
          (trace_set
            (lookup restoring_trace addr)
            (trace_get (lookup trace addr))))))
    (trace_clear (lookup new_trace target_address))
    (define
      [_ reverse_score]
      (propose program-noncolliding inputs restoring_trace new_trace))
    (trace_set (lookup new_trace target_address) new_value)
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
          (set_difference choice_addresses new_choice_addresses)
          (program [addr] (block (trace_clear (lookup trace addr)))))
        (for_each
          new_choice_addresses
          (program
            [addr]
            (block
              (trace_set
                (lookup trace addr)
                (trace_get (lookup new_trace addr)))))))
      (block (trace_set (lookup trace target_address) initial_value)))))

(define
  infer_lightweight_chain
  (program
    [sp args target num_steps]
    (define candidate (mk_nil))
    (define constraints (addresses_of target))
    (trace_choices sp args target candidate)
    (repeat
      num_steps
      (program
        []
        (single_site_metropolis_hastings_step
          sp
          args
          candidate
          constraints)))
    candidate))

(define
  infer_lightweight_mcmc
  (program
    [sp args target num_steps query]
    (define state (mk_nil))
    (define constraints (trace_sites target))
    (trace_choices sp args target state)
    (define first_val (query state))
    (define
      step
      (program
        [i]
        (single_site_metropolis_hastings_step
          sp
          args
          state
          constraints)
        (query state)))
    (pair first_val (map step (range num_steps)))))

