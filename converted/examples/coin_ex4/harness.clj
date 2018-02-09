;; This file was automatically generated

(ns metaprob.examples.coin-ex4.harness
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  dump_rejection_samples
  dump_mcmc_samples
  dump_lightweight_samples)

(define
  dump_rejection_samples
  (program
    [num_samples _fuel outfile]
    (dump_py_data
      (replicate
        num_samples
        (program
          []
          (pair
            (extract_weight
              (rejection_sampling
                flip_coins
                (tuple 5)
                (constrain_coin_flipper_trace 5)
                0))
            (mk_nil))))
      outfile)))

(define
  dump_mcmc_samples
  (program
    [num_replicates num_steps outfile]
    (dump_py_data
      (replicate
        num_replicates
        (program
          []
          (infer_mcmc
            flip_coins
            (tuple 5)
            (constrain_coin_flipper_trace 5)
            num_steps
            extract_weight)))
      outfile)))

(define
  dump_lightweight_samples
  (program
    [num_replicates num_steps outfile]
    (dump_py_data
      (replicate
        num_replicates
        (program
          []
          (infer_lightweight_mcmc
            flip_coins
            (tuple 5)
            (constrain_coin_flipper_trace 5)
            num_steps
            extract_weight)))
      outfile)))

