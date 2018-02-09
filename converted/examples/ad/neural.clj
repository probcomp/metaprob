;; This file was automatically generated

(ns metaprob.examples.ad.neural
  (:refer-clojure :only [ns declare])
  (:require [metaprob.syntax :refer :all]
            [metaprob.builtin :refer :all]
            [metaprob.prelude :refer :all]))

(declare train predictive_accuracy multilayer_perceptron init_mlp doit)

(define
  train
  (program
    [init run examples answers iters]
    (define
      state
      (block
        (define __trace_0__ (mk_nil))
        (trace_set __trace_0__ (list_to_vlist (init)))
        __trace_0__))
    (replicate
      iters
      (program
        []
        (for_each2
          (program
            [example answer]
            (define run_t (mk_nil))
            (define
              val
              (trace_choices
                run
                (tuple (trace_get state) example)
                (mk_nil)
                run_t))
            (define direction (sub answer val))
            (define
              [dstate dexample]
              ((compound_backpropagator (lookup run (list "source")))
                run_t
                direction))
            (trace_set state (add (trace_get state) dstate)))
          examples
          answers)))
    (trace_get state)))

(define
  predictive_accuracy
  (program
    [init run examples answers train_iters test test_answers]
    (define trained (train init run examples answers train_iters))
    (zipmap
      (program
        [example answer]
        (sub answer (lookup (run trained example) 0)))
      test
      test_answers)))

(define
  multilayer_perceptron
  (program
    [state input]
    (if (is_vpair state)
      (block
        (define input_weights (lookup (vfirst state) 0))
        (define biases (lookup (vfirst state) 1))
        (define unit_inputs (matrix_times_vector input_weights input))
        (define unit_outputs (logisticv (sub unit_inputs biases)))
        (multilayer_perceptron (vrest state) unit_outputs))
      (block (exactly input)))))

(define
  init_mlp
  (program
    [n_layers input_width hidden_width output_width]
    (program
      []
      (if (eq n_layers 1)
        (block
          (array_to_list
            (tuple
              (array_to_varray
                (tuple
                  (zero_matrix output_width input_width)
                  (fill output_width 0))))))
        (block
          (pair
            (array_to_varray
              (tuple
                (zero_matrix hidden_width input_width)
                (fill hidden_width 0)))
            ((init_mlp
               (sub n_layers 1)
               hidden_width
               hidden_width
               output_width))))))))

(define
  little_test_input
  (map
    array_to_vlist
    (array_to_list
      (tuple (tuple 0 0 1) (tuple 1 1 1) (tuple 1 0 1) (tuple 0 1 1)))))

(define little_test_output (array_to_list (tuple 0.0 1.0 1.0 0.0)))

(define
  doit
  (program
    [n_layers hidden_width train_iters]
    (define mlp_state (init_mlp n_layers 3 hidden_width 1))
    (predictive_accuracy
      mlp_state
      multilayer_perceptron
      little_test_input
      little_test_output
      train_iters
      little_test_input
      little_test_output)))

