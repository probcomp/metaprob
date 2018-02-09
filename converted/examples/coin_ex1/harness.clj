;; This file was automatically generated

(ns metaprob.examples.coin-ex1.harness
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  particle_sampler
  rejection_sampler
  ssrmh_sampler
  lightweight_sampler
  rejection_p_p_plot
  particles_p_p_plot
  ssrmh_p_p_plot
  dump_analytic_samples
  dump_rejection_samples
  dump_particle_samples
  dump_mcmc_samples)

(define
  particle_sampler
  (program
    [num_p]
    (program
      []
      (extract_weight
        (infer_resampling
          flip_coins
          (tuple 5)
          (constrain_coin_flipper_trace 5)
          num_p)))))

(define
  rejection_sampler
  (program
    []
    (extract_weight
      (rejection_sampling
        flip_coins
        (tuple 5)
        (constrain_coin_flipper_trace 5)
        0))))

(define
  ssrmh_sampler
  (program
    [num_steps]
    (program
      []
      (extract_weight
        (infer_ssrmh
          flip_coins
          (tuple 5)
          (constrain_coin_flipper_trace 5)
          num_steps)))))

(define
  lightweight_sampler
  (program
    [num_steps]
    (program
      []
      (extract_weight
        (infer_lightweight_chain
          flip_coins
          (tuple 5)
          (constrain_coin_flipper_trace 5)
          num_steps)))))

(define
  rejection_p_p_plot
  (program
    [num_samples filename]
    (p_p_plot_2samp_to_file
      filename
      (replicate num_samples rejection_sampler)
      (replicate (mul 10 num_samples) analytic_sampler))))

(define
  particles_p_p_plot
  (program
    [num_samples num_particles filename]
    (p_p_plot_2samp_to_file
      filename
      (replicate num_samples (particle_sampler num_particles))
      (replicate (mul 10 num_samples) analytic_sampler))))

(define
  ssrmh_p_p_plot
  (program
    [num_samples num_steps filename]
    (p_p_plot_2samp_to_file
      filename
      (replicate num_samples (ssrmh_sampler num_steps))
      (replicate (mul 10 num_samples) analytic_sampler))))

(define
  dump_analytic_samples
  (program
    [num_samples]
    (replicate
      num_samples
      (program [] (print (list (analytic_sampler)))))))

(define
  dump_rejection_samples
  (program
    [num_samples]
    (replicate
      num_samples
      (program [] (print (list (rejection_sampler)))))))

(define
  dump_particle_samples
  (program
    [num_replicates num_p]
    (replicate
      num_replicates
      (program
        []
        (print
          (infer_rolling_resample
            flip_coins
            (tuple 5)
            (constrain_coin_flipper_trace 5)
            num_p
            extract_weight))))))

(define
  dump_mcmc_samples
  (program
    [num_replicates num_steps]
    (replicate
      num_replicates
      (program
        []
        (print
          (infer_mcmc
            flip_coins
            (tuple 5)
            (constrain_coin_flipper_trace 5)
            num_steps
            extract_weight))))))

