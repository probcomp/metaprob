;; This file was automatically generated

(ns metaprob.examples.dpmem-hyper.infer-transcript
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare infer_transcript)

(define
  infer_transcript
  (program
    [num_steps]
    (define target_trace (mk_nil))
    (define
      data
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
        (sub 0 99.5)))
    (for_each
      (range (length data))
      (program
        [i]
        (block
          (trace_set
            (lookup target_trace (list "datum" i "normal"))
            (trace_get (lookup data (list i)))))))
    (define data_addresses (addresses_of target_trace))
    (define markov_chain_state (mk_nil))
    (trace_choices
      generate_from_dirichlet_process_mixture
      (tuple (length data))
      target_trace
      markov_chain_state)
    (define
      approximate_inference_update
      (program
        []
        (block
          (single_site_metropolis_hastings_step
            generate_from_dirichlet_process_mixture
            (tuple (length data))
            markov_chain_state
            data_addresses))))
    (repeat num_steps approximate_inference_update)
    (define
      all_data
      (interpret
        generate_from_dirichlet_process_mixture
        (tuple (add (length data) 100))
        markov_chain_state))
    (define predictive_data (drop all_data (length data)))
    predictive_data))

