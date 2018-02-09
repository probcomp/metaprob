;; This file was automatically generated

(ns metaprob.examples.gmm.gmm
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare
  draw_gmm
  constrain_gmm_trace
  extract_clusters
  particle_samples
  dump_particle_samples
  mh_samples
  dump_mh_samples
  lightweight_samples
  dump_lightweight_samples)

(define
  draw_gmm
  (program
    [n]
    (define cluster (mem (program [id] (uniform_discrete 0 2))))
    (define make_mean (mem (program [cl] (normal 0 100))))
    (define point (program [id] (normal (make_mean (cluster id)) 1)))
    (map point (range n))))

(define
  constrain_gmm_trace
  (program
    [data]
    (define t1 (mk_nil))
    (imap
      (program
        [i datum]
        (block
          (trace_set
            (lookup
              t1
              (list
                3
                (prob_prog_name map)
                i
                (lookup draw_gmm (list "source" "body" 2 "point"))
                "normal"))
            datum)))
      data)
    t1))

(define
  extract_clusters
  (program
    [n]
    (program
      [state]
      (define
        get_it
        (program
          [k]
          (define
            site
            (list
              0
              "cluster"
              (prob_prog_name mem)
              "cache"
              k
              (lookup draw_gmm (list "source" "body" 0 "cluster" 1))
              "uniform_discrete"))
          (trace_get (lookup state site))))
      (map get_it (range n)))))

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

(define
  particle_samples
  (program
    [num_replicates num_p]
    (replicate
      num_replicates
      (program
        []
        (infer_rolling_resample
          draw_gmm
          (tuple (length data))
          (constrain_gmm_trace data)
          num_p
          (extract_clusters (length data)))))))

(define
  dump_particle_samples
  (program
    [num_replicates num_p outfile]
    (dump_py_data (particle_samples num_replicates num_p) outfile)))

(define
  mh_samples
  (program
    [num_replicates num_steps]
    (replicate
      num_replicates
      (program
        []
        (infer_mcmc
          draw_gmm
          (tuple (length data))
          (constrain_gmm_trace data)
          num_steps
          (extract_clusters (length data)))))))

(define
  dump_mh_samples
  (program
    [num_replicates num_steps outfile]
    (dump_py_data (mh_samples num_replicates num_steps) outfile)))

(define
  lightweight_samples
  (program
    [num_replicates num_steps]
    (replicate
      num_replicates
      (program
        []
        (infer_lightweight_mcmc
          draw_gmm
          (tuple (length data))
          (constrain_gmm_trace data)
          num_steps
          (extract_clusters (length data)))))))

(define
  dump_lightweight_samples
  (program
    [num_replicates num_steps outfile]
    (dump_py_data
      (lightweight_samples num_replicates num_steps)
      outfile)))

